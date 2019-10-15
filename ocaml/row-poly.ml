(* Typescript's structural subtyping corresponds to OCaml's row polymorphism for objects and polymorphic variants? *)
(* Typescript:
// We can have unions (reported types are the same as on the right):
type a = string | number
type b = { foo: string, bar: number } | { foo: string, bar: string, baz: string } // does not get reduced
type bf = [b['foo'], b['bar']] // [string, a], Property 'baz' does not exist on type 'b'
// type is closed, no supertypes allowed:
const x5: b = { foo: '', bar: 0, boo: ''} // ... 'boo' does not exist in type 'b'
// but union is propagated down on fields of the same name (bar: number | string):
const x6: b = { foo: '', bar: 0, baz: ''}

// And intersections (types in comment, 'same' if not reduced):
type c = string & number // never
type d = { foo: string, bar: number } & { foo: string, bar: string, baz: string } // same
type df = [d['foo'], d['bar'], d['baz']] // [string, never, string], accessing d['no'] is a type-error ('Property 'no' does not exist on type 'd')
*)

(* union *)
(* polymorphic variants *)
type a = [ `String of string | `Int of int ]
type b1 = [ `Foo of string | `Bar of int ]
type b2 = [ `Foo of string | `Bar of string | `Baz of string]
type b = [ b1 | b2 ] (* b2: This variant type contains a constructor [ `Bar of string ] which should be [ `Bar of int ] *)
(* not surprising that this does not work since we don't have implicit union of int and string *)
type b1 = [ `Foo of string | `Bar of a ]
type b2 = [ `Foo of string | `Bar of a | `Baz of string]
type b = [ b1 | b2 ] (* like this it works *)
(* result: *)
type b =
    [ `Bar of [ `Int of int | `String of string ]
    | `Baz of string
    | `Foo of string ]

type a = [ `A | `B ]
type b = [ `A | `C ]
type c = [ a | b ] (* type c = [ `A | `B | `C ] *)
type 'a c = 'a constraint 'a = [> a] constraint 'a = [> b] (* type 'a c = 'a constraint 'a = [> `A | `B | `C ] *)


(* intersection *)
(* type c = a & b (* Syntax error *) *)
(* type c = [ a & b ] (* Syntax error after unclosed [, expecting `|' *) *)
(* type c = [ `C of a & b ] (* The present constructor C has a conjunctive type *) *)
type 'a c = 'a constraint 'a = [< a] constraint 'a = [< b] (* type 'a c = 'a constraint 'a = [< `A ] *)

(* the above works on single values, to simulate Typescript objects, we'd need to put them into lists: *)
let a = [`A; `B] (* [> `A | `B ] list *)
let b = [`A; `C] (* [> `A | `C ] list *)
(* However for polymorphic variants, we can't force at least some values: *)
let a: [`A | `B] list = [`A]
let a: [> `A | `B] list = [`A] (* at least `A is at least `A | `B *)
let a: [< `A | `B] list = [`A]
let a: [> `A | `B] list = []
let a: [< `A | `B] list = [`C] (* error, we can only check at most for list *)
let a: [`A | `B] = `A
let a: [> `A | `B] = `A
let a: [< `A | `B] = `A
let a = (`A :> [`A | `B])

(* To enforce a minimum of fields we need objects: *)
type a = < a: unit; b: unit >
type b = < a: unit; c: unit >
type c = <a; b> (* type c = < a : unit; b : unit; c : unit > *)
(* with subtyping on fields: *)
type 'a a = < a: [> `A|`B] as 'a; b: unit >
type 'a b = < a: [> `A|`C] as 'a; c: unit >
type 'a c = <'a a; 'a b> (* type 'a c = < a : [> `A | `B | `C ]; b : unit; c : unit > constraint 'a = [> `A | `B | `C ] *)
type 'a a = < a: [< `A|`B] as 'a; b: unit >
type 'a b = < a: [< `A|`C] as 'a; c: unit >
type 'a c = <'a a; 'a b> (* type 'a c = < a : [< `A ]; b : unit; c : unit > constraint 'a = [< `A ] *)
let add_c o = object method c = o#a + o#b end (* can't inherit/extend o *)
type 'a add_c = (< a: int; b: int; ..> as 'a) -> < c: int; 'a > (* Illegal open object type *)
type ('a,'b) add_c = (< a: int; b: int; ..> as 'a) -> (< c: int; .. > as 'b)
(* result is: *)
type ('a, 'b) add_c = 'a -> 'b constraint 'a = < a : int; b : int; .. > constraint 'b = < c : int; .. >
type ('a, 'b) add_c = 'a -> 'b constraint 'a = < a : int; b : int; .. > constraint 'b = < c : int; 'a > (* Error: The type 'a is not an object type *)
(* Can't construct a value but do the .. mean the same implicit 'a? *)
let f: < a: int; .. > -> < a: int; b: int; .. > = fun _ -> failwith ""
