// RUN: not %target-swift-frontend %s -typecheck

protocol A {}
protocol B {}

struct X : A {}
struct Y : B {}

struct G<T:A> { var v : T }
struct G<T:B> { var v : T }

let a = G(v: X())
let b = G(v: Y())
