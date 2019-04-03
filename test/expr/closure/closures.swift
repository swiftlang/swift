// RUN: %target-typecheck-verify-swift

func doStuff(_ fn : @escaping () -> Int) {}
func doVoidStuff(_ fn : @escaping () -> ()) {}

// <rdar://problem/16193162> Require specifying self for locations in code where strong reference cycles are likely
class ExplicitSelfRequiredTest {
  var x = 42
  func method() -> Int {
    // explicit closure requires an explicit "self." base.
    doVoidStuff({ [self] in x += 1 })
    doStuff({ x+1 })    // expected-error {{reference to property 'x' in closure requires explicit 'self.' to make capture semantics explicit}} {{15-15=self.}}

    return 42
  }
}
