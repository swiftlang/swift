// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen %s -experimental-skip-non-inlinable-function-bodies | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -experimental-skip-non-inlinable-function-bodies-without-types | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -import-objc-header %S/Inputs/open_enum.h -debug-forbid-typecheck-prefix SKIP_ALL_NO_TYPECHECK -experimental-skip-all-function-bodies | %FileCheck %s --check-prefix=CHECK-SKIP-ALL

public protocol P {
  @_borrowed var x: Int { get }
}

public struct S: P {
  /// Since x implements a `@_borrowed` requirement of `P` the synthesized
  /// `_read` accessor has forced static dispatch and is a serialized function
  /// even though it is not `@_transparent`.
  public var x: Int

  // CHECK-SKIP-ALL-NOT: s4main1SV1xSivr
  // CHECK: sil shared [serialized]{{.*}} @$s4main1SV1xSivr : $@yield_once @convention(method) (S) -> @yields Int {
  // CHECK:   yield
  // CHECK: } // end sil function '$s4main1SV1xSivr'
}
