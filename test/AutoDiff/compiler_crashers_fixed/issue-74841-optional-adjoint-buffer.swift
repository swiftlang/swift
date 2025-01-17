// RUN: %target-swift-frontend -emit-sil -verify %s

// https://github.com/swiftlang/swift/issues/74841
// We used to create invalid adjoint buffer for Optional<T>
// if this buffer originated from unchecked_take_enum_data_addr
// instruction
import _Differentiation;

struct F<I> {subscript(_ i: Int) -> S<I>? {get {nil} set {}}}
struct S<I> {subscript(_ i: Int) -> I?    {get {nil} set {}}}
extension F: Differentiable {}
extension S: Differentiable {}
struct A{@differentiable(reverse) func b(c: inout F<Double>, d: S<Double>) {c[0]![0] = 0}}
