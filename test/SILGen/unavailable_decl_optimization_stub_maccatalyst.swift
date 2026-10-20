// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub -target %target-cpu-apple-ios13.1-macabi | %FileCheck %s --check-prefixes=CHECK,CHECK-NO-EXTENSION
// RUN: %target-swift-emit-silgen -module-name Test -parse-as-library %s -verify -unavailable-decl-optimization=stub -target %target-cpu-apple-ios13.1-macabi -application-extension | %FileCheck %s --check-prefixes=CHECK,CHECK-EXTENSION

// REQUIRES: OS=macosx || OS=maccatalyst

public struct S {}

// CHECK-LABEL: sil{{.*}}@$s4Test22macCatalystUnavailableAA1SVyF
// CHECK-NOT:     _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test22macCatalystUnavailableAA1SVyF'
@available(macCatalyst, unavailable)
public func macCatalystUnavailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test14iOSUnavailableAA1SVyF
// CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
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

// CHECK-LABEL: sil{{.*}}@$s4Test34iOSUnavailableMacCatalystAvailableAA1SVyF
// CHECK-NOT:     _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test34iOSUnavailableMacCatalystAvailableAA1SVyF'
@available(iOS, unavailable)
@available(macCatalyst, introduced: 1.0)
public func iOSUnavailableMacCatalystAvailable() -> S {
  return S()
}

// CHECK-LABEL: sil{{.*}}@$s4Test28iOSAndMacCatalystUnavailableAA1SVyF
// CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:       } // end sil function '$s4Test28iOSAndMacCatalystUnavailableAA1SVyF'
@available(iOS, unavailable)
@available(macCatalyst, unavailable)
public func iOSAndMacCatalystUnavailable() -> S {
  return S()
}

// CHECK-LABEL:              sil{{.*}}@$s4Test20iOSAppExtensionsOnlyAA1SVyF
// CHECK-NO-EXTENSION:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NO-EXTENSION-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK-EXTENSION-NOT:        _diagnoseUnavailableCodeReached
// CHECK:                    } // end sil function '$s4Test20iOSAppExtensionsOnlyAA1SVyF'
@available(iOS, unavailable)
@available(macCatalyst, unavailable)
@available(iOSApplicationExtension, introduced: 1.0)
public func iOSAppExtensionsOnly() -> S {
  return S()
}

// CHECK-LABEL:              sil{{.*}}@$s4Test28macCatalystAppExtensionsOnlyAA1SVyF
// CHECK-NO-EXTENSION:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NO-EXTENSION-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK-EXTENSION-NOT:        _diagnoseUnavailableCodeReached
// CHECK:                    } // end sil function '$s4Test28macCatalystAppExtensionsOnlyAA1SVyF'
@available(iOS, unavailable)
@available(macCatalyst, unavailable)
@available(macCatalystApplicationExtension, introduced: 1.0)
public func macCatalystAppExtensionsOnly() -> S {
  return S()
}

@available(macCatalyst, unavailable)
public struct UnavailableOnMacCatalyst {
  // CHECK-LABEL: sil{{.*}}@$s4Test24UnavailableOnMacCatalystV14noAvailabilityAA1SVyF
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test24UnavailableOnMacCatalystV14noAvailabilityAA1SVyF'
  public func noAvailability() -> S {
    return S()
  }

  // CHECK-LABEL: sil{{.*}}@$s4Test24UnavailableOnMacCatalystV022iOSUnavailableInheritsdeB0AA1SVyF
  // CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
  // CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
  // CHECK:       } // end sil function '$s4Test24UnavailableOnMacCatalystV022iOSUnavailableInheritsdeB0AA1SVyF'
  @available(iOS, unavailable)
  public func iOSUnavailableInheritsMacCatalystUnavailable() -> S {
    return S()
  }
}

// CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test40iOSUnavailableMacCatalystAvailableGlobal_WZ
// CHECK-NOT:     _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test40iOSUnavailableMacCatalystAvailableGlobal_WZ'
// CHECK-LABEL: sil [global_init]{{.*}}@$s4Test40iOSUnavailableMacCatalystAvailableGlobalAA1SVvau
// CHECK-NOT:     _diagnoseUnavailableCodeReached
// CHECK:       } // end sil function '$s4Test40iOSUnavailableMacCatalystAvailableGlobalAA1SVvau'
@available(iOS, unavailable)
@available(macCatalyst, introduced: 1.0)
public let iOSUnavailableMacCatalystAvailableGlobal = S()

// CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test34iOSAndMacCatalystUnavailableGlobal_WZ
// CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
// CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
// CHECK:       } // end sil function '$s4Test34iOSAndMacCatalystUnavailableGlobal_WZ'
@available(iOS, unavailable)
@available(macCatalyst, unavailable)
public let iOSAndMacCatalystUnavailableGlobal = S()

public struct HasStaticProperties {
  // CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test19HasStaticPropertiesV034iOSUnavailableMacCatalystAvailableC3Let_WZ
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test19HasStaticPropertiesV034iOSUnavailableMacCatalystAvailableC3Let_WZ'
  // CHECK-LABEL: sil [global_init]{{.*}}@$s4Test19HasStaticPropertiesV034iOSUnavailableMacCatalystAvailableC3LetAA1SVvau
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test19HasStaticPropertiesV034iOSUnavailableMacCatalystAvailableC3LetAA1SVvau'
  @available(iOS, unavailable)
  @available(macCatalyst, introduced: 1.0)
  public static let iOSUnavailableMacCatalystAvailableStaticLet = S()

  // CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test19HasStaticPropertiesV034iOSUnavailableMacCatalystAvailableC3Var_WZ
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test19HasStaticPropertiesV034iOSUnavailableMacCatalystAvailableC3Var_WZ'
  // CHECK-LABEL: sil [global_init]{{.*}}@$s4Test19HasStaticPropertiesV034iOSUnavailableMacCatalystAvailableC3VarAA1SVvau
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test19HasStaticPropertiesV034iOSUnavailableMacCatalystAvailableC3VarAA1SVvau'
  @available(iOS, unavailable)
  @available(macCatalyst, introduced: 1.0)
  public static var iOSUnavailableMacCatalystAvailableStaticVar = S()

  // CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test19HasStaticPropertiesV028iOSAndMacCatalystUnavailableC3Let_WZ
  // CHECK:         [[FNREF:%.*]] = function_ref @$ss31_diagnoseUnavailableCodeReacheds5NeverOyFTwb : $@convention(thin) () -> Never
  // CHECK-NEXT:    [[APPLY:%.*]] = apply [[FNREF]]()
  // CHECK:       } // end sil function '$s4Test19HasStaticPropertiesV028iOSAndMacCatalystUnavailableC3Let_WZ'
  @available(iOS, unavailable)
  @available(macCatalyst, unavailable)
  public static let iOSAndMacCatalystUnavailableStaticLet = S()
}

@available(iOS, unavailable)
@available(macCatalyst, introduced: 1.0)
public struct IOSUnavailableMacCatalystAvailable {
  // CHECK-LABEL: sil private [global_init_once_fn]{{.*}}@$s4Test34IOSUnavailableMacCatalystAvailableV9staticLet_WZ
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test34IOSUnavailableMacCatalystAvailableV9staticLet_WZ'
  // CHECK-LABEL: sil [global_init]{{.*}}@$s4Test34IOSUnavailableMacCatalystAvailableV9staticLetAA1SVvau
  // CHECK-NOT:     _diagnoseUnavailableCodeReached
  // CHECK:       } // end sil function '$s4Test34IOSUnavailableMacCatalystAvailableV9staticLetAA1SVvau'
  public static let staticLet = S()
}
