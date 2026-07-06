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
