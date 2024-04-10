// RUN: %target-typecheck-verify-swift

@_marker protocol Q {}
struct G<T: Q> {}

typealias A1 = G<any Q>

//

class C: Q {}
typealias A2 = G<C>

//

protocol Q2: Q {}
typealias A3 = G<any Q2>

//

protocol P1: C {}
protocol P2 where Self: C {}

typealias A4 = G<any P1>
typealias A5 = G<any P2>
