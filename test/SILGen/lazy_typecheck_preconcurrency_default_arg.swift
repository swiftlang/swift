// RUN: %target-swift-frontend -emit-silgen %s -swift-version 5 -parse-as-library -enable-library-evolution -module-name Test | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -swift-version 5 -parse-as-library -enable-library-evolution -module-name Test -experimental-lazy-typecheck | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -swift-version 6 -parse-as-library -enable-library-evolution -module-name Test -experimental-lazy-typecheck | %FileCheck %s

// rdar://188639324
//
// An inferred @preconcurrency property is always picked up by mangling under
// lazy type checking, so Sendable is stripped.
@MainActor @preconcurrency
public protocol P {}

extension P {
  // `f` inherits `@MainActor @preconcurrency` from `P`.

  // CHECK-LABEL: sil non_abi [serialized]{{.*}} @$s4Test1PPAAE1f_1xyqd__m_SitlFfA0_ :
  // CHECK-LABEL: sil{{.*}} @$s4Test1PPAAE1f_1xyqd__m_SitlF :
  public func f<T: Sendable>(_: T.Type, x: Int = 0) {}
}
