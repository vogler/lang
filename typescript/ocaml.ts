// # literal types
// Typescript has literal types which is pretty nice (OCaml does not), but they don't get propagated precisely...
const l1/*: 12 */= 12 // `let l1(*: int *) = 12`
const l2/*: "foo" */= "foo" // `let l2(*: string *) = 12`
const l3/*: "foo" | "bar" */= true ? "foo" : "bar" // why is the type not just `"foo"`?
const l4/*: 0 | 2 */= 1 && 2 // type should just be 2
// function arguments always need a type annotation, otherwise they're `any`?
const l5/*: (x: any) => 1 | 2 */= x => x == 1 && x == 2 ? 1 : 2 // why can it not infer that x is type `number` instead of `any`? Also, return type should be `2` since it's already syntactically clear that the condition is false (no matter what x's type is).
// const l6 = (x:number) => x == 1 && x == 2 ? 1 : 2 // error on `x == 2`: This condition will always return `false` since the types `1` and `2` have no overlap.

// # product types / tuples
// OCaml needs no type annotation for tuples: `let p1(*: int * string *)= 12, "foo" in let p1a, p1b = p1`.
// In Typescript, the syntax for tuple values is shared with lists.
// So we get a list with the sum of its element types if we don't annotate a type:
const p1/*: (string | number)[] */= [12, "foo"]
const [p1a, p1b] = p1 // both p1a and p1b have type `string | number` instead of `string` and `number` respectively
const p2: [number, string] = [12, "foo"]
const [p2a, p2b] = p2 // now it works

// # coproduct / sum types
// As already seen above for lists, Typescript implicitly takes the sum/union of different types.
// In OCaml we can't just define a heterogenous list like `[12, "foo"]`, but we need to define a type with constructors for the different element types.

// # enums / (polymorphic) variants
// In OCaml we have variants which need to be declared:
// type t1 = A | B;;
// let e1(*: t1 list *)= [A; B];;
// In Typescript we can do the same as enums:
enum t1 { A, B }
const e1/*: t1[] */= [t1.A, t1.B]
// However, in OCaml variants can be parametric:
// type t2 = C of int | D of string;;
// let e2(*: t2 list *)= [C 12; D "foo"]
// In Typescript there is no syntax for it and we can only achieve something similar via so-called discriminated (tagged) unions, i.e. use a field of an object as a tag.
// https://www.typescriptlang.org/docs/handbook/advanced-types.html#discriminated-unions
const e2f/*: ({
  kind: string;
  content: number;
} | {
  kind: string;
  content: string;
})[] */= [{kind: "C", content: 12}, {kind: "D", content: "foo"}] // `kind` is not a string literal type here :(
// So, we also need to declare types first:
interface C {
  kind: "C",
  content: number,
}
interface D {
  kind: "D",
  content: string,
}
type t2 = C | D
const e2: t2[] = [{kind: "C", content: 12}, {kind: "D", content: "foo"}] // note that ': t2[]' is required and that we can't just assign e2f.
// This is very verbose compared to OCaml's `type t2 = C of int | D of string;; [C 12; D "foo"]`
// Luckily we can just flatten it to the following (interface creates a name, type does not, see https://www.typescriptlang.org/docs/handbook/advanced-types.html#interfaces-vs-type-aliases):
type t3 = { kind: "C", content: number } | { kind: "D", content: string }
const e3: t3[] = [{kind: "C", content: 12}, {kind: "D", content: "foo"}]
// We could also pull out the kind field into a tuple:
type t4 = ["C", { content: number }] | ["D", { content: string }]
const e4: t4[] = [["C", {content: 12}], ["D", {content: "foo"}]]
// Seems like the best option to me (closest to OCaml, clearer what we descriminate by, no extra field that shouldn't be part of the object since it only represents its type (e.g. relevant for generic print))
