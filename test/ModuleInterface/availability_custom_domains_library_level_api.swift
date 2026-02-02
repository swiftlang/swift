// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -experimental-spi-only-imports \
// RUN:   -enable-library-evolution -swift-version 5 -library-level api \
// RUN:   -package-name TestPackage -module-name Test \
// RUN:   -emit-module-interface-path %t/Test.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Test.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Test.package.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -module-name Test

// RUN: %target-swift-typecheck-module-from-interface(%t/Test.private.swiftinterface) \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -module-name Test

// RUN: %target-swift-typecheck-module-from-interface(%t/Test.package.swiftinterface) \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -module-name Test

// RUN: %FileCheck %s --check-prefixes=CHECK --input-file %t/Test.swiftinterface
// RUN: %FileCheck %s --check-prefixes=CHECK-PUBLIC --input-file %t/Test.swiftinterface
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-NONPUBLIC --input-file %t/Test.private.swiftinterface
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-NONPUBLIC --input-file %t/Test.package.swiftinterface

// REQUIRES: swift_feature_CustomAvailability

import Lakes
@_spiOnly import Seas

// CHECK:                 @available(Salt)
// CHECK:                 public func availableInPublicEnabledDomain()
@available(Salt)
public func availableInPublicEnabledDomain() { }

// CHECK:                 @available(Erie)
// CHECK:                 public func availableInPublicDisabledDomain()
@available(Erie)
public func availableInPublicDisabledDomain() { }

// CHECK:                 @available(Huron)
// CHECK:                 public func availableInPublicDynamicDomain()
@available(Huron)
public func availableInPublicDynamicDomain() { }

// CHECK:                 @available(Salt, unavailable)
// CHECK:                 public func unavailableInPublicEnabledDomain()
@available(Salt, unavailable)
public func unavailableInPublicEnabledDomain() { }

// CHECK:                 @available(Erie, unavailable)
// CHECK:                 public func unavailableInPublicDisabledDomain()
@available(Erie, unavailable)
public func unavailableInPublicDisabledDomain() { }

// CHECK:                 @available(Huron, unavailable)
// CHECK:                 public func unavailableInPublicDynamicDomain()
@available(Huron, unavailable)
public func unavailableInPublicDynamicDomain() { }

// CHECK-NONPUBLIC:       @available(Baltic)
// CHECK-PUBLIC-NOT:      Baltic
// CHECK-LABEL:           public func availableInSPIOnlyEnabledDomain()
@available(Baltic)
public func availableInSPIOnlyEnabledDomain() { }

// CHECK-NONPUBLIC:       @available(Mediterranean)
// CHECK-NONPUBLIC-LABEL: public func availableInSPIOnlyDisabledDomain_Secret()
// CHECK-PUBLIC-NOT:      Mediterranean
// CHECK-PUBLIC-NOT:      availableInSPIOnlyDisabledDomain_Secret
@available(Mediterranean)
public func availableInSPIOnlyDisabledDomain_Secret() { }

// CHECK-NONPUBLIC:       @available(Aegean)
// CHECK-PUBLIC-NOT:      Aegean
// CHECK-LABEL:           public func availableInSPIOnlyDynamicDomain()
@available(Aegean)
public func availableInSPIOnlyDynamicDomain() { }

// CHECK-NONPUBLIC:       @available(Baltic, unavailable)
// CHECK-NONPUBLIC-LABEL: public func unavailableInSPIOnlyEnabledDomain_Secret()
// CHECK-PUBLIC-NOT:      Baltic
// CHECK-PUBLIC-NOT:      unavailableInSPIOnlyEnabledDomain_Secret
@available(Baltic, unavailable)
public func unavailableInSPIOnlyEnabledDomain_Secret() { }

// CHECK-NONPUBLIC:       @available(Mediterranean, unavailable)
// CHECK-PUBLIC-NOT:      Mediterranean
// CHECK-LABEL:           public func unavailableInSPIOnlyDisabledDomain()
@available(Mediterranean, unavailable)
public func unavailableInSPIOnlyDisabledDomain() { }

// CHECK:                 @available(*, unavailable)
// CHECK-NONPUBLIC:       @available(Baltic)
// CHECK-PUBLIC-NOT:      Baltic
// CHECK-LABEL:           public func availableInSPIOnlyEnabledDomainAndUnavailableUniversally()
@available(*, unavailable)
@available(Baltic)
public func availableInSPIOnlyEnabledDomainAndUnavailableUniversally() { }

// CHECK-NONPUBLIC:       @available(*, unavailable)
// CHECK-NONPUBLIC:       @available(Mediterranean)
// CHECK-NONPUBLIC-LABEL: public func availableInSPIOnlyDisabledDomainAndUnavailableUniversally_Secret()
// CHECK-PUBLIC-NOT:      Mediterranean
// CHECK-PUBLIC-NOT:      availableInSPIOnlyDisabledDomainAndUnavailableUniversally_Secret
@available(*, unavailable)
@available(Mediterranean)
public func availableInSPIOnlyDisabledDomainAndUnavailableUniversally_Secret() { }

// CHECK:                 @available(*, unavailable)
// CHECK-NONPUBLIC:       @available(Aegean)
// CHECK-PUBLIC-NOT:      Aegean
// CHECK-LABEL:           public func availableInSPIOnlyDynamicDomainAndUnavailableUniversally()
@available(*, unavailable)
@available(Aegean)
public func availableInSPIOnlyDynamicDomainAndUnavailableUniversally() { }
