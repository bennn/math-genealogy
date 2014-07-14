open Core.Std
open Async.Std

let cBASE_URL = "http://genealogy.math.ndsu.nodak.edu"
let cOUTPUT = "./crawl.log"
let debug = false

let rec contains_str_aux s t =
  begin match String.index s t.[0] with
    | Some i ->
       begin
         try
           let s' = String.slice s i (i + String.length t) in
           s' = t || contains_str_aux (snd (String.lsplit2_exn s ~on:(t.[0]))) t
         with _ -> false (* reached end of string *)
       end
    | None   -> false
  end

(* check if [s] contains substring [t] *)
let contains_str s t : bool =
  if (0 = String.length t)
  then true
  else if (0 = String.length s)
  then false
  else contains_str_aux s t

(* TODO need xpath help. this is soooooo shitty right now. *)
(* pull from the pipe [p] until we get a name from it. Return the name and the pipe. *)
let rec name_of_raw_html (body : string list) : string =
  begin match body with
    | []    -> "<no-name>"
    | x::xs ->
       if String.is_suffix x ~suffix:" </h2>"
       then String.slice x 0 ((String.length x) - 6)
       else name_of_raw_html xs
  end

let uri_of_href (href : string) : Uri.t =
  Uri.of_string (cBASE_URL ^ "/" ^ href)

(* brute-force take the href and text from a string *)
let advisors_aux (s : string) (i : int) : string * string =
  (* remove the beginning slice *)
  let s' = String.slice s (i+9) ((String.length s)-1) in
  (* take until first quote character. That's the href *)
  let i' = String.index_exn s' '"' in
  let href = String.slice s' 0 i' in
  let s'' = String.slice s' (i'+2) ((String.length s')-1) in
  (* take until first <. That's the name *)
  let i'' = String.index_exn s'' '<' in
  let name = String.slice s'' 0 i'' in
  href, name

(* get href and text out of html *)
let advisors_of_str (s : string) : (string * Uri.t) list =
  let () = if debug then print_endline ("[advisors_of_str] " ^ s) in
  let regex = Str.regexp "<a href=\"\\(.*\\)\">\\(.*\\)</a>" in
  let match1 = Str.search_forward regex s 0 in
  let href1, name1 = advisors_aux s match1 in
  let match2 = Str.search_backward regex s ((String.length s)-1) in
  let href2, name2 = advisors_aux s match2 in
  if name1 = name2
  then [(name1, uri_of_href href1)]
  else [(name1, uri_of_href href1); (name2, uri_of_href href2)]

(* get a list of advisors from html *)
let rec advisors_of_raw_html (body : string list) : (string * Uri.t) list =
  begin match body with
    | []    -> []
    | x::xs ->
       if contains_str x "Advisor"
       then (try (advisors_of_str x) with _ -> [])
       else advisors_of_raw_html xs
  end

let write_output (s : string) : unit =
  (* let () = if debug then print_endline ("[write_output] " ^ s) in *)
  (* Out_channel.write_lines cOUTPUT [s] *)
  print_endline s

(* let get_search_url (name : string) : Uri.t = *)
(*   let base  = Uri.of_string "http://api.duckduckgo.com/?format=json" in *)
(*   let query = Format.sprintf "%s math genealogy" name in *)
(*   Uri.add_query_param base ("q", [query]) *)

(* (\* return the result url from math geneology if any, else nothing *\) *)
(* let get_url_from_json (raw_json : string) : Uri.t option = *)
(*   begin match Yojson.Safe.from_string raw_json with *)
(*     | `Assoc kv_list -> *)
(*        printf "result keys: %s\n" (String.concat ~sep:"; " (List.map kv_list ~f:fst)); *)
(*        None *)
(*     | _ -> None *)
(*   end *)

(* (\* take name of scientist, get uri *\) *)
(* let url_of_name (name : string) : (Uri.t option) Deferred.t = *)
(*   let search_url = get_search_url name in *)
(*   Cohttp_async.Client.get search_url *)
(*   >>= (fun (_, body) -> *)
(*        Pipe.read body *)
(*        >>= (function *)
(*              | `Eof         -> return None *)
(*              | `Ok raw_json -> return (get_url_from_json raw_json) *)
(*            ) *)
(*       ) *)

(* TODO string list Deferred.t, print all the names (deduplicated) at the end *)
(* visit uri, get name, get ancestor, call recursively on ancestor *)
let rec ancestors_of_url (uri : Uri.t) : unit Deferred.t =
  (* returns response and pipe reader. response doesn't make sense, ignoring
     https://github.com/mirage/ocaml-cohttp/blob/master/async/cohttp_async.mli
   *)
  Cohttp_async.Client.get uri
  >>= (fun (_, body) ->
       Pipe.read body
       >>= (function
             | `Eof         -> return ()
             | `Ok raw_html ->
                let lines    = String.split raw_html ~on:'\n' in
                let stu_name = name_of_raw_html lines in
                let advisors = advisors_of_raw_html lines in
                (* for each advisor uri, send out a new request *)
                Deferred.all_unit ( (* unit deferred list. Making this string instead of unit, we can hold the whole tree here. *)
                    List.map advisors ~f:(fun (adv_name, url) ->
                                          let () = write_output (Format.sprintf "%s\t%s" stu_name adv_name) in
                                          ancestors_of_url url))
           )
      )

let () =
  Command.async_basic
  ~summary:"Print all ancestors of a mathematician, starting from the given URL."
  Command.Spec.(
    empty
    +> anon ("url" %: string)
  )
  (fun raw_url () ->
   ancestors_of_url (Uri.of_string raw_url)
   (* >>= (function *)
   (*       | Some url -> ancestors_of_url url *)
   (*       | None     -> return (printf "Error: could not find math genealogy page for '%s'. Try giving an exact url with '-url'." name) *)
   (*     ) *)
  )
  |> Command.run
