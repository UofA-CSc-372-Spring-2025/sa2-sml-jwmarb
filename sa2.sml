(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Joseph Marbella *)
(* Time spent on HW6:
*)

(* Collaborators and references:
  Deepseek R1 (671B)
  Athene v2 Chat (72B)
  Fuse O1 (32B)
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

(* fun mynull []       = true
  | mynull (_::_)   = false *)

(* 
  Explicit type definitions are a must to prevent runtime bugs!
*)
fun mynull ([]: 'a list): bool = true
  | mynull _ = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

val () =
    Unit.checkExpectWith Bool.toString 
    "mynull [1] should be false"
    (fn () => mynull [1]) (* So this is equivalent to declaring a lambda in python or a closure in javascript *)
    false

(*
Equivalent declaration in Java:

Unit.checkExpectWith(Bool::toString, "mynull [1] should be false", () -> mynull(new int[] { 1 }), false);
*)

(* 
Unit.checkExpectWith is a function that accepts 3 parameters
*)
val () = Unit.checkExpectWith Bool.toString "mynull [1, 2, 3] should be false" (fn () => mynull [1, 2, 3]) false


(**** Problem B ****)
fun firstVowel ([]: char list): bool = false
  | firstVowel (#"a"::_) = true
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel (_) = false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'a' should be true"
    (fn () => firstVowel [#"a", #"b"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'x' should be false"
    (fn () => firstVowel [#"x", #"a"])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel empty list should be false"
    (fn () => firstVowel [])
    false
(*
fun firstVowel _ = false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true
*)
(**** Problem C ****)
fun reverse (xs: 'a list): 'a list =  List.foldl (fn (x, acc) => x::acc) [] xs

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [] should be []"
  (fn () => reverse [])
  []

(*
fun reverse xs = xs

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]
*)
(**** Problem D ****)
(*
fun minlist _ = 0

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0
*)
(**** Problem E ****)
(*
exception Mismatch

fun zip _ = []
*)
(**** Problem F ****)
(*
fun concat xs = xs
*)
(**** Problem G ****)
(*
fun isDigit _    = false;
*)
(**** Problem H ****)
(*
fun isAlpha c = false
*)
(**** Problem I ****)
(*
fun svgCircle (cx, cy, r, fill) = "NOT IMPLEMENTED YET"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";
*)
(**** Problem J ****)
(*
fun partition p (x :: xs) = ([],[])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
*)

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
