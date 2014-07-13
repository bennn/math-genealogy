open Core.Std
open Async.Std

let cBASE_URL = "http://genealogy.math.ndsu.nodak.edu"
let cOUTPUT = "./crawl.log"

(* TODO need xpath help. this is soooooo shitty right now. *)
(* pull from the pipe [p] until we get a name from it. Return the name and the pipe. *)
let name_of_raw_html (body : string list) : string =
  begin match List.nth body 195 with
    | Some n when String.is_suffix n ~suffix:" </h2>" ->  String.slice n 0 ((String.length n) - 6)
    | Some _ | None   -> "<no-name>"
  end

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

let uri_of_href (href : string) : Uri.t =
  Uri.of_string (cBASE_URL ^ "/" ^ href)

(* get href and text out of html *)
let advisors_of_str (s : string) : (string * Uri.t) list =
  let regex = Str.regexp "<a href=\"\\(.*\\)\">\\(.*\\)</a>" in
  let match1 = Str.search_forward regex s 0 in
  let href1 = Str.matched_group 0 s in
  let name1 = Str.matched_group 1 s in
  let match2 = Str.search_backward regex s ((String.length s)-1) in
  let href2 = Str.matched_group 0 s in
  let name2 = Str.matched_group 1 s in
  if href1 = href2
  then [(name1, uri_of_href href1)]
  else [(name1, uri_of_href href1); (name2, uri_of_href href2)]

(* get a list of advisors from html *)
let rec advisors_of_raw_html (body : string list) : (string * Uri.t) list =
  begin match body with
    | []    -> []
    | x::xs ->
       if contains_str x "Advisor"
       then advisors_of_str x
       else advisors_of_raw_html xs
  end

(* visit uri, get name, get ancestor, call recursively on ancestor *)
let ancestors_of_url (uri : Uri.t) : unit Deferred.t =
  (* returns response and pipe reader. response doesn't make sense, ignoring
     https://github.com/mirage/ocaml-cohttp/blob/master/async/cohttp_async.mli
   *)
  Cohttp_async.Client.get uri
  >>= (fun (_, body) ->
       Pipe.to_list body
       >>= (fun body ->
            let name = name_of_raw_html body in
            let advisors = advisors_of_raw_html body in
            (* for each advisor uri, send out a new request *)
            printf "name = %s\nadvisors = %s\n" name (String.concat ~sep:"; " (List.map advisors ~f:fst));
            return ()
           )
      )


let () =
  Command.async_basic
  ~summary:"Print all ancestors of a mathematician, starting from the given URL."
  Command.Spec.(
    empty
    +> anon ("url" %: string)
  )
  (fun raw_url () -> ancestors_of_url (Uri.of_string raw_url))
  |> Command.run
