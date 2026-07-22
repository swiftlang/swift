// RUN: %target-swift-emit-ir %s | %FileCheck %s

// https://github.com/swiftlang/swift/issues/87030
//
// When a non-throwing closure literal `{ $0 }` is stored into a property of
// type `(T) throws(E) -> T` where `E` is a nested error type of a generic
// container, SILGen emits the closure with a non-throwing SIL function type
// (its body doesn't throw) and wraps it in an outer `convert_function` that
// adds the error result. But SILGen's prolog used to emit the artificial
// `debug_value undef, name "$error"` because the *abstracted* closure type
// throws. IRGen's handling of that `$error` debug var calls
// `funcTy->getErrorResult()` unconditionally — which asserted because the
// closure's own SIL function type has no error result.
//
// Fix: only emit the `$error` debug placeholder when the closure's own SIL
// function type actually has an error result. Also related: issue #73641
// (same pattern with a generic class).
//
// Regression: both reproducers must compile cleanly through IRGen.

// --- Issue #87030: generic struct -----------------------------------------

public struct Box<T> {
    public var f: (T) throws(Error) -> T
    public enum Error: Swift.Error { case e }
}

public enum E { case a }

extension Box where T == E {
    public static var id: Self { .init(f: { $0 }) }
}

// The static `id` getter and its non-throwing closure literal must both be
// emitted. The closure's IR signature has no swifterror register — it
// returns void and writes to its sret, matching the non-throwing SIL type.
// CHECK-LABEL: define {{.*}} @"$s39typed_throws_nested_error_closure_870303BoxVA2A1EORszlE2idACyAEGvgZ"
// CHECK-LABEL: define internal swiftcc void @"$s39typed_throws_nested_error_closure_870303BoxVA2A1EORszlE2idACyAEGvgZA2EcfU_"(ptr noalias sret(%T39typed_throws_nested_error_closure_870301EO) captures(none) {{%.*}}, ptr noalias captures(none) {{%.*}})

// --- Issue #73641 sibling: generic class ----------------------------------

public class Container<T> {
  public enum Err: Error { case bad }
  public var action: () throws(Err) -> T
  public init(_ a: @escaping () throws(Err) -> T) { self.action = a }
}

extension Container where T == Int {
  public static var noop: Container<Int> { Container { 0 } }
}

// CHECK-LABEL: define {{.*}} @"$s39typed_throws_nested_error_closure_870309ContainerCAASiRszlE4noopACySiGvgZ"
// CHECK-LABEL: define internal swiftcc void @"$s39typed_throws_nested_error_closure_870309ContainerCAASiRszlE4noopACySiGvgZSiycfU_"(ptr noalias sret(%TSi) captures(none) {{%.*}})
