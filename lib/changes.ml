(*
 * Copyright (c) 2016 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Result

module Change = struct
  type t = {
    description : string;
  }

  let default_bullet = '*'

  let to_string { description } =
    let lines = Astring.String.fields ~is_sep:((=) '\n') description in
    let rev_indented_lines = List.fold_left (fun lines line -> match lines with
      | [] -> [line]
      | _::_ -> match line with
        | "" -> ""::lines
        | line -> ("  "^line)::lines
    ) [] lines in
    let description = String.concat "\n" (List.rev rev_indented_lines) in
    Printf.sprintf "%c %s" default_bullet description
end

module Section = struct
  type t = {
    title : string;
    changes : Change.t list;
  }

  let to_string = function
    | { title = ""; changes } ->
      (String.concat "\n" (List.map Change.to_string changes))
    | { title; changes } ->
      Printf.sprintf "%s:\n%s"
        title
        (String.concat "\n" (List.map Change.to_string changes))
end

module Release = struct
  type t = {
    version  : string;
    date     : (int * int * int, string) result;
    sections : Section.t list;
  }

  let default_date_sep = '-'

  let string_of_date = function
    | Ok (y,m,d) ->
      Printf.sprintf "%d%c%02d%c%02d" y default_date_sep m default_date_sep d
    | Error branch -> branch

  let to_string { version; date; sections } =
    Printf.sprintf "%s (%s):\n%s\n"
      version (string_of_date date)
      (String.concat "\n\n" (List.map Section.to_string sections))
end

type t = Release.t list

let ok x = Ok x

let error x = Error x

module Parser = struct
  open MParser

  type t = {
    date_sep      : char option;
    change_bullet : char option;
    cur_change_d  : int;
  }

  let (<.<) x y = x >>= (>>$) y

  let (>.>) x y = x >>= fun _ -> y

  let rec skip_upto_count k p = match k with
    | 0 -> return ()
    | k ->
      (attempt (skip p) >>$ true) <|> (return false)
      >>= function
      | true  -> skip_upto_count (k - 1) p
      | false -> return ()

  let blanks = skip_many_chars blank

  let line = many_chars (not_followed_by newline "" >.> any_char)

  let printable_char_no_space =
    any_of (String.init (126 - 33 + 1) (fun x -> Char.chr (x + 33)))
  let version_char = printable_char_no_space
  let version = many1_chars (not_followed_by blank "" >.> version_char)

  let decimal =
    many1_chars digit
    >>= fun digits ->
    try return (int_of_string digits)
    with Failure "int_of_string" ->
      message ("couldn't create integer from "^digits)

  let date_sep =
    get_user_state
    >>= function
    | { date_sep = None } as state ->
      char '-' <|> char '/'
      >>= fun sep ->
      set_user_state { state with date_sep = Some sep } >>$ sep
    | { date_sep = Some sep } -> char sep

  let date =
    decimal <.< skip date_sep
    >>= fun year ->
    decimal <.< skip date_sep
    >>= fun month ->
    decimal
    >>= fun day ->
    return (year, month, day)

  let date_or_branch =
    let maybe_date = attempt (date |>> ok) in
    let branch = many_chars_until any_char (look_ahead (char ')')) |>> error in
    maybe_date <|> branch

  let change_bullet =
    get_user_state
    >>= function
    | { change_bullet = None } as state ->
      char '*' <|> char '-' <|> char '+'
      >>= fun bullet ->
      set_user_state { state with change_bullet = Some bullet } >>$ bullet
    | { change_bullet = Some bullet } -> char bullet

  let change_start =
    blanks >.> skip change_bullet >.> blanks
    >.> get_pos
    >>= fun (_,_,col) ->
    get_user_state
    >>= fun state ->
    set_user_state { state with cur_change_d = col - 1 }

  let blank_line = newline >.> skip_many1_chars newline

  let rec continue_change d prev_lines =
    begin
      (followed_by (newline >.> change_start) "next line not new change"
       <|> followed_by (blank_line >.> not_followed_by (skip_count d blank) "")
         "next line not new release"
       <|> followed_by (optional newline >.> eof) "next line not eof"
      )
      |>> fun () ->
      let description = String.concat "\n" (List.rev prev_lines) in
      { Change.description }
    end
    <|> (newline >.> (skip_upto_count d blank) >.> line
         >>= fun next_line ->
         continue_change d (next_line :: prev_lines)
        )

  let change =
    change_start >.>
    get_user_state
    >>= fun { cur_change_d = d } ->
    line
    >>= fun description -> continue_change d [description]

  let rec changes prev_changes =
    change
    >>= fun delta ->
    begin
      followed_by blank_line "not next release"
      <|> followed_by (optional newline >.> eof) "not eof 2"
      |>> fun () ->
      List.rev (delta :: prev_changes)
    end
    <|> (newline >.> changes (delta :: prev_changes))

  let section =
    begin
      followed_by change_start "not change start"
      >>= fun () ->
      changes []
      |>> fun changes ->
      { Section.title = ""; changes }
    end
    <|> begin
      let end_of_title = char ':' >.> newline in
      many_chars (not_followed_by end_of_title "" >.> any_char)
      >>= fun title ->
      skip end_of_title >.> optional newline >.>
      changes []
      |>> fun changes ->
      { Section.title; changes }
    end

  let release_header =
    skip_many (char '#') >.> blanks >.>
    (version <?> "a non-empty, non-blank version string") <.< blanks
    >>= fun version ->
    (between
       (char '(')
       (char ')' >.> optional (char ':'))
       date_or_branch)
    <?> "date or branch/release name (in parentheses)"
    <.< skip_many1_chars newline
    |>> fun date -> (version, date)

  let rec sections prev_sections =
    section
    >>= fun section ->
    begin
      followed_by (blank_line >.> release_header) "not release header"
      <|> followed_by (optional newline >.> eof) "not eof 1"
      |>> fun () ->
      List.rev (section :: prev_sections)
    end
    <|> (blank_line >.> sections (section :: prev_sections))

  let release =
    release_header
    >>= fun (version, date) ->
    sections []
    >>= fun sections ->
    return Release.{ version; date; sections; }

  let rec releases prev_releases =
    release
    >>= fun release ->
    begin
      followed_by (optional newline >.> eof) "not eof 0"
      |>> fun () ->
      List.rev (release :: prev_releases)
    end
    <|> (blank_line >.> releases (release :: prev_releases))

  let v = releases []
end

module Error = struct

end

let of_ parse changelog =
  let ps = {
    Parser.date_sep = None;
    change_bullet = None;
    cur_change_d = 1;
  } in
  match parse Parser.v changelog ps with
  | MParser.Success e            -> Ok e
  | MParser.Failed (msg, _error) -> Error msg

let of_string = of_ MParser.parse_string

let of_channel = of_ MParser.parse_channel

let to_string changelog =
  String.concat "\n" (List.map Release.to_string changelog)
