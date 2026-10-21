// Test that an extended existential shape's requirement signature leaves out
// the requirements of its generalization signature.

// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -target %target-swift-5.7-abi-triple | %IRGenFileCheck %s

public protocol Source<Element> {
  associatedtype Element
}

public class Base<Value: Hashable> {}

// The requirement signature has 3 parameters, 3 requirements (`τ_0_1 ==
// Self.Element`, `Self: Base<τ_0_0>`, `Self: Source`), and 4 key arguments.
// The generalization signature has 2 parameters, 1 requirement (`τ_0_0:
// Hashable`), and 3 key arguments.
// CHECK-LABEL: @"$sSHRzr0_l{{.*}}6Source_{{.*}}4BaseCyxGXcq_7ElementRts_XPXGMq" = linkonce_odr hidden constant
// CHECK-SAME:    i32 6401,
// CHECK-SAME:    i16 3, i16 3, i16 4, i16 0,
// CHECK-SAME:    i16 2, i16 1, i16 3, i16 0,
public func make() -> Any.Type {
  return (any Base<Int> & Source<Int>).self
}

public class NoncopyableArgBase<Value: ~Copyable> {}

// Neither signature has any inverse requirements when `Self` is copyable.
// CHECK-LABEL: @"$sr0_l{{.*}}6Source_{{.*}}18NoncopyableArgBaseCyxGXcq_7ElementRts_XPXGMq" = linkonce_odr hidden constant
// CHECK-SAME:    i32 6401,
// CHECK-SAME:    i16 3, i16 3, i16 4, i16 0,
// CHECK-SAME:    i16 2, i16 0, i16 2, i16 0,
public func makeNoncopyableArg() -> Any.Type {
  return (any NoncopyableArgBase<Int> & Source<Int>).self
}
