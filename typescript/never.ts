type a = { id: number, name: string, foo: string }
type b = { id: string, name: string, bar: string }
type c = a | b
type d = a & b
type id = d['id'] // number & string = never (bot)

const va: a = {id: 1, name: 'ha', foo: 'foo'}
const vb: b = {id: '1', name: 'ha', bar: 'bar'}
const vc1: c = va
const vc2: c = vb
const vc3/*: c*/ = { ...va, ...vb }
const vd: d = vc3 // error message could be better; there is no value satisfying d since id: never
