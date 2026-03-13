// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Library.swift -parse-as-library -module-name Library -enable-library-evolution -emit-module-path %t/Library.swiftmodule
// RUN: %target-swift-frontend -emit-silgen -primary-file %t/Primary.swift %t/Other.swift -parse-as-library -module-name Test -I %t | %FileCheck %s --check-prefixes CHECK,CHECK-PRIMARY,CHECK-COMMON
// RUN: %target-swift-frontend -emit-silgen %t/Primary.swift %t/Other.swift -parse-as-library -module-name Test -I %t | %FileCheck %s --check-prefixes CHECK,CHECK-WHOLE-MODULE,CHECK-COMMON
// RUN: %target-swift-frontend -emit-silgen -primary-file %t/Primary.swift %t/Other.swift -parse-as-library -module-name Test -I %t -experimental-lazy-typecheck | %FileCheck %s --check-prefixes CHECK,CHECK-PRIMARY,CHECK-COMMON
// RUN: %target-swift-frontend -emit-silgen -primary-file %t/Primary.swift %t/Other.swift -parse-as-library -module-name Test -I %t -experimental-skip-non-inlinable-function-bodies | %FileCheck %s --check-prefixes CHECK-SKIP,CHECK-COMMON

//--- Library.swift

public protocol P {}

@usableFromInline struct LibraryStruct: P {
  @usableFromInline init() {}
}

@available(SwiftStdlib 5.5, *)
public func returnsLibraryStruct() -> some P {
  return LibraryStruct()
}

@available(SwiftStdlib 5.5, *)
@inlinable public func inlinableReturnsLibraryStruct() -> some P {
  return LibraryStruct()
}

//--- Other.swift

import Library

struct OtherStruct: P {}

@available(SwiftStdlib 5.5, *)
public func returnsOtherStruct() -> some P {
  return OtherStruct()
}

//--- Primary.swift

import Library

public struct PrimaryStruct: P {
  public init() {}
}

// CHECK-LABEL: sil{{.*}} @$s4Test20returnsPrimaryStructQryF : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <@_opaqueReturnTypeOf("$s4Test20returnsPrimaryStructQryF", 0) __> {
// CHECK:       bb0(%0 : $*PrimaryStruct):
// CHECK:       } // end sil function '$s4Test20returnsPrimaryStructQryF'
@available(SwiftStdlib 5.5, *)
public func returnsPrimaryStruct() -> some P {
  return PrimaryStruct()
}

// CHECK-LABEL: sil{{.*}} @$s4Test39globalComputedVarReturningPrimaryStructQrvg : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <@_opaqueReturnTypeOf("$s4Test39globalComputedVarReturningPrimaryStructQrvp", 0) __> {
// CHECK:        bb0(%0 : $*PrimaryStruct):
// CHECK:        } // end sil function '$s4Test39globalComputedVarReturningPrimaryStructQrvg'
@available(SwiftStdlib 5.5, *)
public var globalComputedVarReturningPrimaryStruct: some P {
  return PrimaryStruct()
}

@available(SwiftStdlib 5.5, *)
public struct S {
  private var privatePrimaryStruct: PrimaryStruct

  public var computedVarReturningPrimaryStruct: some P {
    // CHECK-LABEL: sil{{.*}} @$s4Test1SV33computedVarReturningPrimaryStructQrvg : $@convention(method) (S) -> @out @_opaqueReturnTypeOf("$s4Test1SV33computedVarReturningPrimaryStructQrvp", 0) __ {
    // CHECK:       bb0(%0 : $*PrimaryStruct, %1 : $S):
    // CHECK:       } // end sil function '$s4Test1SV33computedVarReturningPrimaryStructQrvg'
    get { privatePrimaryStruct }

    // CHECK-LABEL: sil{{.*}} @$s4Test1SV33computedVarReturningPrimaryStructQrvs : $@convention(method) (@in @_opaqueReturnTypeOf("$s4Test1SV33computedVarReturningPrimaryStructQrvp", 0) __, @inout S) -> () {
    // CHECK:       bb0(%0 : $*PrimaryStruct, %1 : $*S):
    // CHECK:       } // end sil function '$s4Test1SV33computedVarReturningPrimaryStructQrvs'
    set {}
  }

  public subscript(subscriptReturningPrimaryStruct: Void) -> some P {
    // CHECK-LABEL: sil{{.*}} @$s4Test1SVyQryt_tcig : $@convention(method) (S) -> @out @_opaqueReturnTypeOf("$s4Test1SVyQryt_tcip", 0) __ {
    // CHECK:       bb0(%0 : $*PrimaryStruct, %1 : $S):
    // CHECK:       } // end sil function '$s4Test1SVyQryt_tcig'
    get { privatePrimaryStruct }
    // CHECK-LABEL: sil{{.*}} @$s4Test1SVyQryt_tcis : $@convention(method) (@in @_opaqueReturnTypeOf("$s4Test1SVyQryt_tcip", 0) __, @inout S) -> () {
    // CHECK:       bb0(%0 : $*PrimaryStruct, %1 : $*S):
    // CHECK:       } // end sil function '$s4Test1SVyQryt_tcis'
    set {}
  }
}

// CHECK-COMMON-LABEL:  sil{{.*}} @$s4Test024inlinableReturnsResultOfC13PrimaryStructQryF : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <@_opaqueReturnTypeOf("$s4Test024inlinableReturnsResultOfC13PrimaryStructQryF", 0) __> {
// CHECK:               bb0(%0 : $*PrimaryStruct):
// CHECK-SKIP:          bb0(%0 : $*@_opaqueReturnTypeOf("$s4Test20returnsPrimaryStructQryF", 0) __):
// CHECK-COMMON:        } // end sil function '$s4Test024inlinableReturnsResultOfC13PrimaryStructQryF'
@available(SwiftStdlib 5.5, *)
@inlinable public func inlinableReturnsResultOfReturnsPrimaryStruct() -> some P {
  return returnsPrimaryStruct()
}

// CHECK-LABEL: sil{{.*}} @$s4Test19returnsNestedStructQryF : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <@_opaqueReturnTypeOf("$s4Test19returnsNestedStructQryF", 0) __> {
// CHECK:       bb0(%0 : $*NestedStruct):
// CHECK:       } // end sil function '$s4Test19returnsNestedStructQryF'
@available(SwiftStdlib 5.5, *)
public func returnsNestedStruct() -> some P {
  struct NestedStruct: P {}
  return NestedStruct()
}

// CHECK-LABEL: sil{{.*}} @$s4Test34returnsResultOfReturnsNestedStructQryF : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <@_opaqueReturnTypeOf("$s4Test34returnsResultOfReturnsNestedStructQryF", 0) __> {
// CHECK:       bb0(%0 : $*NestedStruct):
// CHECK:       } // end sil function '$s4Test34returnsResultOfReturnsNestedStructQryF'
@available(SwiftStdlib 5.5, *)
public func returnsResultOfReturnsNestedStruct() -> some P {
  return returnsNestedStruct()
}

// CHECK-LABEL:         sil{{.*}} @$s4Test33returnsResultOfReturnsOtherStructQryF : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <@_opaqueReturnTypeOf("$s4Test33returnsResultOfReturnsOtherStructQryF", 0) __> {
// CHECK-PRIMARY:       bb0(%0 : $*@_opaqueReturnTypeOf("$s4Test18returnsOtherStructQryF", 0) __):
// CHECK-WHOLE-MODULE:  bb0(%0 : $*OtherStruct):
// CHECK:               } // end sil function '$s4Test33returnsResultOfReturnsOtherStructQryF'
@available(SwiftStdlib 5.5, *)
public func returnsResultOfReturnsOtherStruct() -> some P {
  return returnsOtherStruct()
}

// CHECK-LABEL: sil{{.*}} @$s4Test35returnsResultOfReturnsLibraryStructQryF : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <@_opaqueReturnTypeOf("$s4Test35returnsResultOfReturnsLibraryStructQryF", 0) __> {
// CHECK:       bb0(%0 : $*@_opaqueReturnTypeOf("$s7Library07returnsA6StructQryF", 0) __):
// CHECK:       } // end sil function '$s4Test35returnsResultOfReturnsLibraryStructQryF'
@available(SwiftStdlib 5.5, *)
public func returnsResultOfReturnsLibraryStruct() -> some P {
  return returnsLibraryStruct()
}

// CHECK-LABEL: sil{{.*}}  @$s4Test44returnsResulfOfInlinableReturnsLibraryStructQryF : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <@_opaqueReturnTypeOf("$s4Test44returnsResulfOfInlinableReturnsLibraryStructQryF", 0) __> {
// CHECK:       bb0(%0 : $*LibraryStruct):
// CHECK:       } // end sil function '$s4Test44returnsResulfOfInlinableReturnsLibraryStructQryF'
@available(SwiftStdlib 5.5, *)
public func returnsResulfOfInlinableReturnsLibraryStruct() -> some P {
  return inlinableReturnsLibraryStruct()
}
