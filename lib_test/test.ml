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

let (/) = Filename.concat

let read_ic ic =
  let rec loop prev =
    match try Some (input_line ic) with End_of_file -> None with
    | Some line -> loop (line::prev)
    | None -> List.rev prev
  in
  String.concat "\n" (loop [])

let read_file path =
  let ic = open_in path in
  let contents = read_ic ic in
  close_in ic;
  contents

let read_tests f =
  let cases = "cases" in
  let contents = Array.to_list (Sys.readdir cases) in
  let case_list =
    List.filter (fun c -> Sys.is_directory (cases / c)) contents
  in
  List.map (fun dir -> cases / dir, `Quick, f (cases / dir)) case_list

let check_diff dir s =
  let expected = dir / "expected" in
  let output = dir / "output" in
  let oc = open_out output in
  output_string oc s;
  close_out oc;
  let diff = Printf.sprintf "diff -u %s %s" expected output in
  let diff_out, diff_in = Unix.open_process diff in
  let diff_output = read_ic diff_out in
  match Unix.close_process (diff_out, diff_in) with
  | Unix.WEXITED 0 -> ()
    | Unix.WEXITED x ->
      Alcotest.fail ("diff failed "^string_of_int x^":\n"^diff_output)
    | _ -> Alcotest.fail "diff failed unexpectedly"

(* parse, print *)
module Parse = struct
  let test dir () =
    let ic = open_in (dir / "CHANGES") in
    match Changes.of_channel ic with
    | Result.Error message -> check_diff dir message
    | Result.Ok changes -> check_diff dir (Changes.to_string changes)

  let tests = read_tests test
end

let date =
  let module M = struct
    type t = int * int * int
    let pp fmt (y,m,d) = Format.fprintf fmt "%d-%02d-%02d" y m d
    let equal = (=)
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let diff expected output =
  let open Changes in
  Alcotest.(check int) "number of releases"
    (List.length expected)
    (List.length output);
  List.iter2 (fun expected output ->
    let open Release in
    let version = expected.version in
    Alcotest.(check string) "version" expected.version output.version;
    Alcotest.(check (result date string)) "date" expected.date output.date;
    Alcotest.(check int) ("number of sections in "^expected.version)
      (List.length expected.sections)
      (List.length output.sections);
    List.iter2 (fun expected output ->
      let open Section in
      let title = version ^ "/" ^ expected.title in
      Alcotest.(check string) ("section title in "^version)
        expected.title output.title;
      Alcotest.(check int) ("number of changes in "^title)
        (List.length expected.changes)
        (List.length output.changes);
      List.iter2 (fun expected output ->
        let open Change in
        Alcotest.(check string) ("change description of "^title)
          expected.description output.description
      ) expected.changes output.changes
    ) expected.sections output.sections
  ) expected output

(* parse, print, parse *)
module Roundtrip = struct
  let test dir () =
    let ic = open_in (dir / "CHANGES") in
    match Changes.of_channel ic with
    | Result.Error message -> check_diff dir message
    | Result.Ok changes ->
      match Changes.(of_string (to_string changes)) with
      | Result.Error message ->
        Alcotest.fail ("Roundtrip parsing failed with: "^message)
      | Result.Ok rt -> diff changes rt

  let tests = read_tests test
end

(* parse, print, parse, print *)
module Fixpoint = struct
  let test dir () =
    let ic = open_in (dir / "CHANGES") in
    match Changes.of_channel ic with
    | Result.Error message -> check_diff dir message
    | Result.Ok changes ->
      match Changes.(of_string (to_string changes)) with
      | Result.Error message ->
        Alcotest.fail ("Fixpoint parsing failed with: "^message)
      | Result.Ok rt ->
        check_diff dir (Changes.to_string rt)

  let tests = read_tests test
end

let tests = [
  "parse_print", Parse.tests;
  "roundtrip", Roundtrip.tests;
  "fixpoint", Fixpoint.tests;
]

;;
Alcotest.run "Changes" tests
