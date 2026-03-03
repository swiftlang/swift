// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub -target %target-cpu-apple-xros1.0 | %FileCheck %s --check-prefixes=CHECK,CHECK-NO-EXTENSION
// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub -target %target-cpu-apple-xros1.0 -application-extension | %FileCheck %s --check-prefixes=CHECK,CHECK-EXTENSION

// REQUIRES: OS=xros

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
