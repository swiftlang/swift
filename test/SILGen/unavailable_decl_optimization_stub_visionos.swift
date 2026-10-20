// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub -target %target-cpu-apple-xros1.0 | %FileCheck %s --check-prefixes=CHECK,CHECK-NO-EXTENSION
// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub -target %target-cpu-apple-xros1.0 -application-extension | %FileCheck %s --check-prefixes=CHECK,CHECK-EXTENSION

// REQUIRES: OS=xros

// The RUN lines override the target triple but keep the SDK that lit selected,
// so the two can disagree. That mismatch is not relevant to what this test checks.
// expected-warning@<unknown> * {{using sysroot for }}

public struct S {}

// CHECK-LABEL: sil{{.*}}@$s4Test19visionOSUnavailableAA1SVyF
// CHECK-NOT:     _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test19visionOSUnavailableAA1SVyF'
@available(visionOS, unavailable)
public func visionOSUnavailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test14iOSUnavailableAA1SVyF
// CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:       } // end sil function '$s4Test14iOSUnavailableAA1SVyF'
@available(iOS, unavailable)
public func iOSUnavailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test16macOSUnavailableAA1SVyF
// CHECK-NOT:     _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test16macOSUnavailableAA1SVyF'
@available(macOS, unavailable)
public func macOSUnavailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test31iOSUnavailableVisionOSAvailableAA1SVyF
// CHECK-NOT:     _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test31iOSUnavailableVisionOSAvailableAA1SVyF'
@available(iOS, unavailable)
@available(visionOS, introduced: 1.0)
public func iOSUnavailableVisionOSAvailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test25iOSAndVisionOSUnavailableAA1SVyF
// CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:       } // end sil function '$s4Test25iOSAndVisionOSUnavailableAA1SVyF'
@available(iOS, unavailable)
@available(visionOS, unavailable)
public func iOSAndVisionOSUnavailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test20iOSAppExtensionsOnlyAA1SVyF
// CHECK-NO-EXTENSION:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
// CHECK-NO-EXTENSION-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK-EXTENSION-NOT:        _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test20iOSAppExtensionsOnlyAA1SVyF'
@available(iOS, unavailable)
@available(visionOS, unavailable)
@available(iOSApplicationExtension, introduced: 1.0)
public func iOSAppExtensionsOnly() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test25visionOSAppExtensionsOnlyAA1SVyF
// CHECK-NO-EXTENSION:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
// CHECK-NO-EXTENSION-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK-EXTENSION-NOT:        _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test25visionOSAppExtensionsOnlyAA1SVyF'
@available(iOS, unavailable)
@available(visionOS, unavailable)
@available(visionOSApplicationExtension, introduced: 1.0)
public func visionOSAppExtensionsOnly() -> S {
  return S()
}

@available(visionOS, unavailable)
public struct UnavailableOnVisionOS {
  // CHECK-LABEL: sil{{.*}}@$s4Test21UnavailableOnVisionOSV14noAvailabilityAA1SVyF
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test21UnavailableOnVisionOSV14noAvailabilityAA1SVyF'
  public func noAvailability() -> S {
    return S()
  }

  // CHECK-LABEL: sil{{.*}}@$s4Test21UnavailableOnVisionOSV022iOSUnavailableInheritsdF0AA1SVyF
  // CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
  // CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
  // CHECK:       } // end sil function '$s4Test21UnavailableOnVisionOSV022iOSUnavailableInheritsdF0AA1SVyF'
  @available(iOS, unavailable)
  public func iOSUnavailableInheritsVisionOSUnavailable() -> S {
    return S()
  }
}

// CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test37iOSUnavailableVisionOSAvailableGlobal_WZ
// CHECK-NOT:     _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test37iOSUnavailableVisionOSAvailableGlobal_WZ'
// CHECK-LABEL: sil [global_init]{{.*}}@$s4Test37iOSUnavailableVisionOSAvailableGlobalAA1SVvau
// CHECK-NOT:     _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test37iOSUnavailableVisionOSAvailableGlobalAA1SVvau'
@available(iOS, unavailable)
@available(visionOS, introduced: 1.0)
public let iOSUnavailableVisionOSAvailableGlobal = S()

// CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test31iOSAndVisionOSUnavailableGlobal_WZ
// CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:       } // end sil function '$s4Test31iOSAndVisionOSUnavailableGlobal_WZ'
@available(iOS, unavailable)
@available(visionOS, unavailable)
public let iOSAndVisionOSUnavailableGlobal = S()

public struct HasStaticProperties {
  // CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test19HasStaticPropertiesV031iOSUnavailableVisionOSAvailableC3Let_WZ
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test19HasStaticPropertiesV031iOSUnavailableVisionOSAvailableC3Let_WZ'
  // CHECK-LABEL: sil [global_init]{{.*}}@$s4Test19HasStaticPropertiesV031iOSUnavailableVisionOSAvailableC3LetAA1SVvau
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test19HasStaticPropertiesV031iOSUnavailableVisionOSAvailableC3LetAA1SVvau'
  @available(iOS, unavailable)
  @available(visionOS, introduced: 1.0)
  public static let iOSUnavailableVisionOSAvailableStaticLet = S()

  // CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test19HasStaticPropertiesV031iOSUnavailableVisionOSAvailableC3Var_WZ
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test19HasStaticPropertiesV031iOSUnavailableVisionOSAvailableC3Var_WZ'
  // CHECK-LABEL: sil [global_init]{{.*}}@$s4Test19HasStaticPropertiesV031iOSUnavailableVisionOSAvailableC3VarAA1SVvau
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test19HasStaticPropertiesV031iOSUnavailableVisionOSAvailableC3VarAA1SVvau'
  @available(iOS, unavailable)
  @available(visionOS, introduced: 1.0)
  public static var iOSUnavailableVisionOSAvailableStaticVar = S()

  // CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test19HasStaticPropertiesV025iOSAndVisionOSUnavailableC3Let_WZ
  // CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyF : $@convention(thin) () -> Never
  // CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
  // CHECK:       } // end sil function '$s4Test19HasStaticPropertiesV025iOSAndVisionOSUnavailableC3Let_WZ'
  @available(iOS, unavailable)
  @available(visionOS, unavailable)
  public static let iOSAndVisionOSUnavailableStaticLet = S()
}

@available(iOS, unavailable)
@available(visionOS, introduced: 1.0)
public struct IOSUnavailableVisionOSAvailable {
  // CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test31IOSUnavailableVisionOSAvailableV9staticLet_WZ
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test31IOSUnavailableVisionOSAvailableV9staticLet_WZ'
  // CHECK-LABEL: sil [global_init]{{.*}}@$s4Test31IOSUnavailableVisionOSAvailableV9staticLetAA1SVvau
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test31IOSUnavailableVisionOSAvailableV9staticLetAA1SVvau'
  public static let staticLet = S()
}
