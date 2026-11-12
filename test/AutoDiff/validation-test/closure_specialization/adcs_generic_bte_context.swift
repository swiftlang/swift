// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -o %t/none.out
// RUN: %target-build-swift %s -O -o %t/opt.out
// RUN: %target-run %t/none.out
// RUN: %target-run %t/opt.out

import _Differentiation
import StdlibUnittest

// The VJP is specialized for a concrete T before ADCS. Its BTE declaration is
// still generic, but the instantiated BTE in its SIL contains that concrete T.
@differentiable(reverse, wrt: x)
@inline(never)
func branch<T>(_ x: Float, _ type: T.Type) -> Float {
  if x > 0 { return x * x }
  return x + x
}

// The multiply closure is replaced by its Float captures, including when the
// enclosing VJP no longer has a generic signature.
// CHECK: enum _AD__{{.*}}branch{{.*}}_spec{{.*}} {
// CHECK: case bb1((predecessor: {{.*}}, (Float, Float)))

var tests = TestSuite("ADCSGenericBTEContext")
tests.test("InstantiatedBranchTracingEnums") {
  expectEqual(Float(6), gradient(at: Float(3), of: { branch($0, Int.self) }))
  expectEqual(Float(2), gradient(at: Float(-3), of: { branch($0, Int.self) }))
  expectEqual(Float(8), gradient(at: Float(4), of: { branch($0, String.self) }))
  expectEqual(Float(2), gradient(at: Float(-4), of: { branch($0, String.self) }))
}
runAllTests()
