// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -target arm64-apple-xros1.0 | %FileCheck %s --check-prefixes=CHECK
// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=none -target arm64-apple-xros1.0 | %FileCheck %s --check-prefixes=CHECK

// REQUIRES: OS=xros

public struct S {}

// CHECK-LABEL: sil{{.*}}@$s4Test19visionOSUnavailableAA1SVyF
// CHECK-NOT:     ss31_diagnoseUnavailableCodeReacheds5NeverOyF
// CHECK:       } // end sil function '$s4Test19visionOSUnavailableAA1SVyF'
@available(visionOS, unavailable)
public func visionOSUnavailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test14iOSUnavailableAA1SVyF
// CHECK-NOT:     ss31_diagnoseUnavailableCodeReacheds5NeverOyF
// CHECK:       } // end sil function '$s4Test14iOSUnavailableAA1SVyF'
@available(iOS, unavailable)
public func iOSUnavailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test16macOSUnavailableAA1SVyF
// CHECK-NOT:     ss31_diagnoseUnavailableCodeReacheds5NeverOyF
// CHECK:       } // end sil function '$s4Test16macOSUnavailableAA1SVyF'
@available(macOS, unavailable)
public func macOSUnavailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test31iOSUnavailableVisionOSAvailableAA1SVyF
// CHECK-NOT:     ss31_diagnoseUnavailableCodeReacheds5NeverOyF
// CHECK:       } // end sil function '$s4Test31iOSUnavailableVisionOSAvailableAA1SVyF'
@available(iOS, unavailable)
@available(visionOS, introduced: 1.0)
public func iOSUnavailableVisionOSAvailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test25iOSAndVisionOSUnavailableAA1SVyF
// CHECK-NOT:     ss31_diagnoseUnavailableCodeReacheds5NeverOyF
// CHECK:       } // end sil function '$s4Test25iOSAndVisionOSUnavailableAA1SVyF'
@available(iOS, unavailable)
@available(visionOS, unavailable)
public func iOSAndVisionOSUnavailable() -> S {
  // FIXME: This function should be optimized (rdar://116742214)
  return S()
}

@available(visionOS, unavailable)
public struct UnavailableOnVisionOS {
  // CHECK-LABEL: sil{{.*}}@$s4Test21UnavailableOnVisionOSV14noAvailabilityAA1SVyF
  // CHECK-NOT:     ss31_diagnoseUnavailableCodeReacheds5NeverOyF
  // CHECK:       } // end sil function '$s4Test21UnavailableOnVisionOSV14noAvailabilityAA1SVyF'
  public func noAvailability() -> S {
    return S()
  }

  // CHECK-LABEL: sil{{.*}}@$s4Test21UnavailableOnVisionOSV022iOSUnavailableInheritsdF0AA1SVyF
  // CHECK-NOT:     ss31_diagnoseUnavailableCodeReacheds5NeverOyF
  // CHECK:       } // end sil function '$s4Test21UnavailableOnVisionOSV022iOSUnavailableInheritsdF0AA1SVyF'
  @available(iOS, unavailable)
  public func iOSUnavailableInheritsVisionOSUnavailable() -> S {
    // FIXME: This function should be optimized (rdar://116742214)
    return S()
  }
}
