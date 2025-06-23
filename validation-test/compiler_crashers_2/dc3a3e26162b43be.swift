// {"signature":"(anonymous namespace)::SetExprTypes::walkToExprPost(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a<b, c> = c
struct d < each b {
  typealias e< c > =
  (repeat a< each b, c >)
  typealias f< each c > = (repeat e< each c >
  func 1 {
    typealias g = h
    (g<
    i >
    typealias g< each c > = f< repeat each c >
