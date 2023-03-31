// RUN: %target-swift-frontend -parse-as-library -module-name Test -validate-tbd-against-ir=missing %s -emit-ir | %FileCheck %s --check-prefixes=CHECK,CHECK-NO-STRIP

// RUN: %target-swift-frontend -parse-as-library -module-name Test -validate-tbd-against-ir=missing -unavailable-decl-optimization=complete %s -emit-ir | %FileCheck %s --check-prefixes=CHECK,CHECK-STRIP

// CHECK-NO-STRIP: s4Test14globalConstantSbvp
// CHECK-NO-STRIP: s4Test14globalConstantSbvau
// CHECK-STRIP-NOT: s4Test14globalConstantSbvp
// CHECK-STRIP-NOT: s4Test14globalConstantSbvau
@available(*, unavailable)
public let globalConstant = true

// CHECK-NO-STRIP: s4Test15unavailableFuncyyF
// CHECK-STRIP-NOT: s4Test15unavailableFuncyyF
@available(*, unavailable)
public func unavailableFunc() {}

@available(*, unavailable)
public struct UnavailableStruct<T> {
  // CHECK-NO-STRIP: s4Test17UnavailableStructV8propertyxvg
  // CHECK-NO-STRIP: s4Test17UnavailableStructV8propertyxvs
  // CHECK-NO-STRIP: s4Test17UnavailableStructV8propertyxvM
  // CHECK-STRIP-NOT: s4Test17UnavailableStructV8propertyxvg
  // CHECK-STRIP-NOT: s4Test17UnavailableStructV8propertyxvs
  // CHECK-STRIP-NOT: s4Test17UnavailableStructV8propertyxvM
  public var property: T

  // CHECK-NO-STRIP: s4Test17UnavailableStructVyACyxGxcfC
  // CHECK-NO-STRIP: s4Test17UnavailableStructVMa
  // CHECK-STRIP-NOT: s4Test17UnavailableStructVyACyxGxcfC
  // CHECK-STRIP-NOT: s4Test17UnavailableStructVMa
  public init(_ t: T) {
    self.property = t
  }

  // CHECK-NO-STRIP: s4Test17UnavailableStructV6methodyyF
  // CHECK-STRIP-NOT: s4Test17UnavailableStructV6methodyyF
  public func method() {}
}

@available(*, unavailable)
extension UnavailableStruct {
  // CHECK-NO-STRIP: s4Test17UnavailableStructV15extensionMethodyyF
  // CHECK-STRIP-NOT: s4Test17UnavailableStructV15extensionMethodyyF
  public func extensionMethod() {}
}

public struct S<T> {
  // CHECK-NO-STRIP: s4Test1SV19unavailablePropertyxvg
  // CHECK-NO-STRIP: s4Test1SV19unavailablePropertyxvs
  // CHECK-NO-STRIP: s4Test1SV19unavailablePropertyxvM
  // CHECK-STRIP-NOT: s4Test1SV19unavailablePropertyxvg
  // CHECK-STRIP-NOT: s4Test1SV19unavailablePropertyxvs
  // CHECK-STRIP-NOT: s4Test1SV19unavailablePropertyxvM
  @available(*, unavailable)
  public var unavailableProperty: T

  // CHECK-NO-STRIP: s4Test1SVyACyxGxcfC
  // CHECK-STRIP-NOT: s4Test1SVyACyxGxcfC
  @available(*, unavailable)
  public init(_ t: T) { fatalError() }

  // CHECK-NO-STRIP: s4Test1SV17unavailableMethodyyF
  // CHECK-STRIP-NOT: s4Test1SV17unavailableMethodyyF
  @available(*, unavailable)
  public func unavailableMethod() {}
}

@available(*, unavailable)
extension S {
  // CHECK-NO-STRIP: s4Test1SV28methodInUnavailableExtensionyyF
  // CHECK-STRIP-NOT: s4Test1SV28methodInUnavailableExtensionyyF
  public func methodInUnavailableExtension() {}
}
