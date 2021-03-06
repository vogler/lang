// structural subtyping on objects means any function on objects is polymorphic, however arguments are upcast to the specified supertype and type information is lost
const id = (x: {}) => x
const x1/*: {} */ = id({foo: 'foo'})

// with generics we can return the original type
const id_gen = <T>(x: T) => x
const x2/*: {foo: string} */ = id_gen({foo: 'foo'})
// and also put constraints on it
const id_gen_ext = <T extends {}>(x: T) => x
const x3/*: {foo: string} */ = id_gen_ext({foo: 'foo'})

// we can also splice the argument and thereby extend the object
const add_duration/*: .. => T & {duration: number} */ = <T extends {start: number, stop: number}>(x: T) => ({...x, duration: x.stop-x.start})

// we can think of it as an extensible record of type S = { | R }
const id_row = <R, S extends R>(x: S) => x
const x4 = id_row({foo: 'foo', bar: ''})

// We can have unions (reported types are the same as on the right):
type a = string | number
type b = { foo: string, bar: number } | { foo: string, bar: string, baz: string } // does not get reduced
type bf = [b['foo'], b['bar']] // [string, a], Property 'baz' does not exist on type 'b'
// type is closed, no supertypes allowed:
const x5: b = { foo: '', bar: 0, boo: ''} // ... 'boo' does not exist in type 'b'
// but union is propagated down on fields of the same name (bar: number | string):
const x6: b = { foo: '', bar: 0, baz: ''} // note that bar: number is only in the first choice for b and baz only in the second; also baz is ok here, but it does not exist in bf!? Shouldn't `bf['baz']` be `string?` (`string | undefined`)?

// And intersections (types in comment, 'same' if not reduced):
type c = string & number // never
type d = { foo: string, bar: number } & { foo: string, bar: string, baz: string } // same
type df = [d['foo'], d['bar'], d['baz']] // [string, never, string], accessing d['no'] is a type-error ('Property 'no' does not exist on type 'd')

// For union (|) and intersection (&) on object types a and b, the operation is done on the intersection of field names of a and b.
// Fields `x:t` that are only present in either a or b are treated a bit counter-intuitively:
// For `type c = a | b`, those fields will not be visible in c but still be there as `x:t?` so that the type can be narrowed again (needed for discriminated unions).
// For `type c = a & b`, those fields will be visible in c as `x:t`.
