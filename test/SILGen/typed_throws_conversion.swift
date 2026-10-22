// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -swift-version 6 %s | %FileCheck %s

// https://github.com/swiftlang/swift/issues/87893
// rdar://172703741
// SIL verification failure when a static method with typed throws is passed
// as a function reference to a generic parameter expecting untyped throws.

struct E: Error {}
struct S {
  static func src(_ x: Int) throws(E) -> Int { x }
}

func sink<T: Equatable>(_ fn: (T) throws -> T, _ v: [T]) throws {
  for x in v { _ = try fn(x) }
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}test
// CHECK:         function_ref @$s{{.*}}test{{.*}}fu_
// CHECK:         // function_ref thunk for
// CHECK:         function_ref @$s{{.*}}_TR
// CHECK:         partial_apply
func test() throws {
  try sink(S.src, [1])
}

// Verify that an explicit closure literal with typed throws assigned to an
// untyped-throws context is emitted directly without a thunk.
// CHECK-LABEL: sil private {{.*}} @$s{{.*}}testExplicitClosure{{.*}}fU_ :
// CHECK-SAME:    -> @error any Error
func testExplicitClosure() {
    var callback: (() throws -> Void)?
    callback = { () throws(E) in }
}

// A non-throwing closure literal converted to a typed-throws type whose error
// type depends on an outer generic parameter is emitted at its own type and
// then converted.
struct Tree<Key> {
  enum KeyError: Error { case missing }

  func takesNonEscaping(_ fn: () throws(KeyError) -> Void) {}
  func takesEscaping(_ fn: @escaping () throws(KeyError) -> Void) {}

  // CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testNoGenericCaptureyyF :
  // CHECK:         [[FN:%.*]] = function_ref @$s{{.*}}testNoGenericCaptureyyFyyXEfU_ : $@convention(thin) () -> ()
  // CHECK:         [[THICK:%.*]] = thin_to_thick_function [[FN]]
  // CHECK:         convert_function [[THICK]] to $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error Tree<τ_0_0>.KeyError for <Key>
  func testNoGenericCapture() {
    takesNonEscaping {}
  }

  // CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testGenericCaptureyyF :
  // CHECK:         [[FN:%.*]] = function_ref @$s{{.*}}testGenericCaptureyyFyyXEfU_ : $@convention(thin) <τ_0_0> () -> ()
  // CHECK:         [[PA:%.*]] = partial_apply [callee_guaranteed] [[FN]]<Key>()
  // CHECK:         [[NOESC:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PA]]
  // CHECK:         convert_function [[NOESC]] to $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error Tree<τ_0_0>.KeyError for <Key>
  func testGenericCapture() {
    takesNonEscaping { _ = Key.self }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testEscapingyyF :
  // CHECK:         [[FN:%.*]] = function_ref @$s{{.*}}testEscapingyyFyycfU_ : $@convention(thin) () -> ()
  // CHECK:         [[THICK:%.*]] = thin_to_thick_function [[FN]]
  // CHECK:         convert_function [[THICK]] to $@callee_guaranteed @substituted <τ_0_0> () -> @error Tree<τ_0_0>.KeyError for <Key>
  func testEscaping() {
    takesEscaping {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}testGenericErroryyxms0F0RzlF :
// CHECK:         [[FN:%.*]] = function_ref @$s{{.*}}testGenericErroryyxms0F0RzlFyyXEfU_ : $@convention(thin) () -> ()
// CHECK:         [[THICK:%.*]] = thin_to_thick_function [[FN]]
// CHECK:         [[THUNK:%.*]] = function_ref @$sIg_xIegzr_s5ErrorRzlTR
// CHECK:         [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]<Err>([[THICK]])
// CHECK:         convert_function [[PA]] to $@callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <Err>
func testGenericError<Err: Error>(_: Err.Type) {
  func take(_ fn: () throws(Err) -> Void) {}
  take {}
}
