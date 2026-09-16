// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking)
// RUN: %target-run-simple-swift(-O -Xfrontend -disable-availability-checking)

// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

// Regression test for a miscompile where TBAA failed to recurse into
// `Builtin.FixedArray`'s element type, allowing LICM to hoist a load out
// of a loop past an aliasing store. The symptom was that a loop calling
// a `mutating` method which wrote to an `InlineArray` element via
// subscript left the effect of only one iteration in the final state.

struct Container {
  var values: InlineArray<3, Int>
  mutating func step() {
    values[0] &+= 1
  }
}

var c = Container(values: [0, 0, 0])

for _ in 0 ..< 10 {
  c.step()
}

precondition(c.values[0] == 10)
precondition(c.values[1] == 0)
precondition(c.values[2] == 0)
