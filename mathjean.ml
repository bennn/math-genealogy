open Core.Std
open Async.Std

module AdvisorSet =
  Set.Make(struct
            type t        = string
            let compare   = Pervasives.compare
            let t_of_sexp = Sexp.to_string
            let sexp_of_t = Sexp.of_string
          end)

let cBASE_URL = "http://genealogy.math.ndsu.nodak.edu"
let cDOWNLOAD_DELAY = 1.0
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

(* let write_output (s : string) : unit = *)
(*   (\* let () = if debug then print_endline ("[write_output] " ^ s) in *\) *)
(*   (\* Out_channel.write_lines cOUTPUT [s] *\) *)
(*   print_endline s *)

(* TODO string list Deferred.t, print all the names (deduplicated) at the end *)
(* visit uri, get name, get ancestor, call recursively on ancestor *)
let rec ancestors_of_url (uri : Uri.t) =
  (* returns response and pipe reader. response doesn't make sense, ignoring
     https://github.com/mirage/ocaml-cohttp/blob/master/async/cohttp_async.mli
   *)
  after (Time.Span.of_sec cDOWNLOAD_DELAY)
  >>= (fun () ->
       Cohttp_async.Client.get uri
       >>= (fun (_, body) ->
            Pipe.read body
            >>= (function
                  | `Eof         -> return AdvisorSet.empty
                  | `Ok raw_html ->
                     let lines    = String.split raw_html ~on:'\n' in
                     (* let stu_name = name_of_raw_html lines in *)
                     let advisors = advisors_of_raw_html lines in
                     (* for each advisor uri, send out a new request *)
                     Deferred.all (
                         List.map advisors ~f:(fun (_, url) ->
                                               ancestors_of_url url))
                     >>= (fun advs_list ->
                          let init =
                            List.fold
                              advisors
                              ~init:AdvisorSet.empty
                              ~f:(fun acc (name,_) -> AdvisorSet.add acc name)
                          in
                          return (List.fold advs_list ~init:init ~f:AdvisorSet.union))
                )
           )
      )

(* construct a math genealogy url from a math genealogy id *)
let uri_of_id (id : int) : Uri.t =
  Uri.of_string (Format.sprintf "%s/id.php?id=%d" cBASE_URL id)

let () =
  Command.async_basic
  ~summary:"Print all ancestors of a mathematician, starting from the given ID."
  Command.Spec.(
    empty
    +> anon ("id" %: int)
  )
  (fun id () ->
   ancestors_of_url (uri_of_id id)
   >>| (fun advs -> printf "Ancestors:\n==========\n%s\n" (String.concat ~sep:"\n" (AdvisorSet.to_list advs)))
  )
  |> Command.run
