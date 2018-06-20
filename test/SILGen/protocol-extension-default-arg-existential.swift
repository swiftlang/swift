// RUN: %target-swift-frontend -emit-sil -verify %s

//
// Make sure neither rdar://problem/37031037 (default arguments on protocol extension methods
// depend on Self and normally get evaluated before an existential self value
// gets opened) nor rdar://problem/39524104 (if you open the existential self
// earlier in an attempt to fix this, then you get undesirable exclusivity
// conflicts on constructs like `existential.x = existential.y`) regress.
//

protocol MethodWithDefaultArgGenerator {
  var a: Int { get set }
  var b: Int { get nonmutating set }

  mutating func mutate(_ x: inout Int)
}

protocol P { static var value: Self { get } }
extension Int: P { static var value: Int { return 0 } }

extension MethodWithDefaultArgGenerator {
  mutating func foo(_ x: Int = 0) {}

  mutating func reabstracted<T>(_ x: T.Type = T.self) -> T { fatalError() }
  mutating func indirected<T: P>(_ x: T = T.value) -> T { fatalError() }

  mutating func exploded(x y: (Int, String) = (0, "foo")) {}
}
func invokeMethodsWithDefaultArgs(x: inout MethodWithDefaultArgGenerator) {
  x.foo()
  _ = x.reabstracted() as Int
  _ = x.indirected() as Int
  x.exploded()
}
func checkAgainstExclusivityViolations(x: inout MethodWithDefaultArgGenerator) {
  x.a = x.a
  x.mutate(&x.b)
}

