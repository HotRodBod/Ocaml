(* Rodney Wotton 
I pledge my honor that I have abided by the Stevens Honor System - Rodney Wotton*)


(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
start = "q0";
tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
start = "q0";
tf = [("q0",'a',"q1"); ("q1",'b',"q1")
; ("q1",'c',"q2");  ("q3",'a',"q4")];
final= ["q2"]
}
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
let rec exp i l =
if i < 0 then l else exp (i - 1) (s.[i] :: l) in
exp (String.length s - 1) []


let rec final s fa =
match s with
| [] -> true
| x::t ->
if List.mem x fa.states
then final t fa
else false

let rec nextAgain tf state = 
match tf with 
|[]-> []
|(a,b,c)::t ->
if a=state 
then c::nextAgain t state
else nextAgain t state

let rec reach fa seen all = 
match seen with 
| []-> all
| x::t -> 
if List.mem x all 
then reach fa t all 
else reach fa (List.append t (nextAgain fa.tf x)) (x::all)

let rec del_tf tf reachable =
match tf with
| []->[]
| (a,b,c)::t ->
if not(List.mem a reachable) || not(List.mem c reachable)
then del_tf t reachable
else (a,b,c)::del_tf t reachable


let rec delstates finals reachable =
match finals with
| []->[]
| x::t ->
if List.mem x reachable 
then x::delstates t reachable
else delstates t reachable


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

let rec apply_transition_function tf state symbol =
match tf with
| [] -> None
| (a,b,c)::t ->
if a=state && b=symbol
then Some c
else apply_transition_function t state symbol

let rec accept fa input = 
let rec acceptmyhelp state input = 
match input with 
| [] -> state
| a::t -> 
match state with 
| Some w -> acceptmyhelp (apply_transition_function fa.tf w a) t
| None -> None
in let rec acceptor s = 
match s with
|None-> false
|Some m -> 
if List.mem m fa.final
then true
else false
in acceptor (acceptmyhelp (Some fa.start) input)


let rec next tf state symbol =
match tf with
| [] -> []
| (a,b,c)::t ->
if a=state && b=symbol
then c::next t state symbol
else next t state symbol


let deterministic fa =
let rec dethelp tf =
match tf with
| [] -> true
| (a,b,c)::t ->
if List.length (next fa.tf a b) > 1
then false
else dethelp t
in dethelp fa.tf


let valid fa = 
List.mem fa.start fa.states && deterministic fa && final fa.final fa

let reachable fa = 
let rec sorting states sstates = 
match states with 
|[] -> []
| x::t -> 
if List.mem x sstates 
then x::sorting t sstates
else sorting t sstates
in sorting fa.states (reach fa (nextAgain fa.tf fa.start) [fa.start])


let remove_dead_states fa =
{
states = reachable fa;
start = fa.start;
tf = del_tf fa.tf (reachable fa);
final = delstates fa.final (reachable fa);
}
























