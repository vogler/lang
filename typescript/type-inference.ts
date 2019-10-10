// need to specify types for function arguments :(
const id = (x) => x // any => any
// let id x = x is 'a -> 'a, but any is top!
const _1 = [id(1), id('x')] // any[]
const id_gen = <a> (x:a) => x // <a>(x: a) => a
// the following even shows literal types 1 and 'x' as result on hover, but the result is just a list of (string | number) instead of a tuple [1, 'x']
const _2 = [id_gen(1), id_gen('x')] // (string | number)[]
const _3 = [1, 'x'] // (string | number)[]
const _4: [1, 'x'] = [1, 'x'] // guess tuples always need a type annotation, otherwise it's inferred as a list
