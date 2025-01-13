// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

struct S<each T> {}
protocol P {}
func open<T: P>(_: Int, _: T.Type, _: S<T>? = nil) {}
let meta: any P.Type
open(0, meta)
// expected-warning@-1:9 {{calling 'open' with an existential value is not supported; call it with a generic value instead; this will be an error in a future Swift release}}
