// need to specify types for function arguments :(
const id = (x) => x // any => any
// let id x = x is 'a -> 'a, but any is top!
const _1 = [id(1), id('x')] // any[]
const id_gen = <a> (x:a) => x // <a>(x: a) => a
// the following even shows literal types 1 and 'x' as result on hover, but the result is just a list of (string | number) instead of a tuple [1, 'x']
const _2 = [id_gen(1), id_gen('x')] // (string | number)[]
const _3 = [1, 'x'] // (string | number)[]
const _4: [1, 'x'] = [1, 'x'] // guess tuples always need a type annotation, otherwise it's inferred as a list

const _5/*: 1 */ = 1 // 1
const _6/*: number */ = 1+1 // operations on literal types widen
const _7/*: string */ = 1+'1' // JS coercions to string

// optional arguments have to be filled from left to right, no implicit union
const opt_args = (a?: number, b?: string) => [a, b]
opt_args(12) // [ 12, undefined ]
// opt_args('a') // Argument of type '"a"' is not assignable to parameter of type 'number'
