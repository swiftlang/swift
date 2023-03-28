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

@available(*, unavailable)
public enum UnavailableEnum {
  case a, b

  // CHECK-NO-STRIP: s4Test15UnavailableEnumO6methodyyF
  // CHECK-STRIP-NOT: s4Test15UnavailableEnumO6methodyyF
  public func method() {}

  // CHECK-NO-STRIP: s4Test15UnavailableEnumO21__derived_enum_equalsySbAC_ACtFZ
  // CHECK-NO-STRIP: s4Test15UnavailableEnumO4hash4intoys6HasherVz_tF
  // CHECK-NO-STRIP: s4Test15UnavailableEnumO9hashValueSivg
  // CHECK-STRIP-NOT: s4Test15UnavailableEnumO21__derived_enum_equalsySbAC_ACtFZ
  // CHECK-STRIP-NOT: s4Test15UnavailableEnumO4hash4intoys6HasherVz_tF
  // CHECK-STRIP-NOT: s4Test15UnavailableEnumO9hashValueSivg
}

@available(*, unavailable)
public class UnavailableClass<T> {
  // CHECK-NO-STRIP: s4Test16UnavailableClassC8propertyxvg
  // CHECK-NO-STRIP: s4Test16UnavailableClassC8propertyxvs
  // CHECK-NO-STRIP: s4Test16UnavailableClassC8propertyxvM
  // CHECK-STRIP-NOT: s4Test16UnavailableClassC8propertyxvg
  // CHECK-STRIP-NOT: s4Test16UnavailableClassC8propertyxvs
  // CHECK-STRIP-NOT: s4Test16UnavailableClassC8propertyxvM
  public var property: T

  // CHECK-NO-STRIP: s4Test16UnavailableClassCyACyxGxcfC
  // CHECK-NO-STRIP: s4Test16UnavailableClassCyACyxGxcfc
  // CHECK-STRIP-NOT: s4Test16UnavailableClassCyACyxGxcfC
  // CHECK-STRIP-NOT: s4Test16UnavailableClassCyACyxGxcfc
  public init(_ t: T) {
    self.property = t
  }

  // CHECK-NO-STRIP: s4Test16UnavailableClassCfd
  // CHECK-NO-STRIP: s4Test16UnavailableClassCfD
  // CHECK-STRIP-NOT: s4Test16UnavailableClassCfd
  // CHECK-STRIP-NOT: s4Test16UnavailableClassCfD
  deinit {}
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

public enum E {
  case a

  @available(*, unavailable)
  case b

  // CHECK-NO-STRIP: s4Test1EO17unavailableMethodyyF
  // CHECK-STRIP-NOT: s4Test1EO17unavailableMethodyyF
  @available(*, unavailable)
  public func unavailableMethod() {}
}

public class C<T> {
  // CHECK-NO-STRIP: s4Test1CC19unavailablePropertyxvg
  // CHECK-NO-STRIP: s4Test1CC19unavailablePropertyxvs
  // CHECK-NO-STRIP: s4Test1CC19unavailablePropertyxvM
  // CHECK-STRIP-NOT: s4Test1CC19unavailablePropertyxvg
  // CHECK-STRIP-NOT: s4Test1CC19unavailablePropertyxvs
  // CHECK-STRIP-NOT: s4Test1CC19unavailablePropertyxvM
  @available(*, unavailable)
  public var unavailableProperty: T

  // CHECK-NO-STRIP: s4Test1CCyACyxGxcfC
  // CHECK-NO-STRIP: s4Test1CCyACyxGxcfc
  // CHECK-STRIP-NOT: s4Test1CCyACyxGxcfC
  // CHECK-STRIP-NOT: s4Test1CCyACyxGxcfc
  @available(*, unavailable)
  public init(_ t: T) { fatalError() }

  // CHECK: s4Test1CCfd
  // CHECK: s4Test1CCfD
  deinit {}
}

public protocol P {
  func requirement()
}

@available(*, unavailable)
extension S: P {
  // CHECK-NO-STRIP: s4Test1SV11requirementyyF
  // CHECK-STRIP-NOT: s4Test1SV11requirementyyF
  public func requirement() {}
}

// CHECK-NO-STRIP: s4Test29unavailableFuncWithNestedTypeyyF
// CHECK-STRIP-NOT: s4Test29unavailableFuncWithNestedTypeyyF
@available(*, unavailable)
public func unavailableFuncWithNestedType() {
  struct Nested {
    // s4Test29unavailableFuncWithNestedTypeyyF0E0L_V6methodyyF
    public func method() {}
  }
}

// MARK: -

// MARK: UnavailableEnum

// CHECK-NO-STRIP: s4Test15UnavailableEnumOwug
// CHECK-STRIP-NOT: s4Test15UnavailableEnumOwug

// CHECK-NO-STRIP: s4Test15UnavailableEnumOMa
// CHECK-STRIP-NOT: s4Test15UnavailableEnumOMa

// MARK: UnavailableClass

// CHECK-NO-STRIP: s4Test16UnavailableClassCMa
// CHECK-STRIP-NOT: s4Test16UnavailableClassCMa

// MARK: E

// CHECK: s4Test1EOwug
// CHECK: s4Test1EOMa

// MARK: unavailableFuncWithNestedType().Nested

// CHECK-NO-STRIP: s4Test29unavailableFuncWithNestedTypeyyF0E0L_VMa
// CHECK-STRIP-NOT: s4Test29unavailableFuncWithNestedTypeyyF0E0L_VMa
