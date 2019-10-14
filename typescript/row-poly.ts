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
