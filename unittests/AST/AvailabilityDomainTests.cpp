//===--- AvailabilityDomainTests.cpp - Tests for AvailabilityDomain -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/AvailabilityDomain.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

class AvailabilityDomainLattice : public ::testing::Test {
public:
  AvailabilityDomain domainForPlatform(PlatformKind kind) {
    return AvailabilityDomain::forPlatform(kind);
  }
  AvailabilityDomain Universal = AvailabilityDomain::forUniversal();
  AvailabilityDomain Swift = AvailabilityDomain::forSwiftLanguageMode();
  AvailabilityDomain Package = AvailabilityDomain::forPackageDescription();
  AvailabilityDomain Embedded = AvailabilityDomain::forEmbedded();
  AvailabilityDomain macOS = domainForPlatform(PlatformKind::macOS);
  AvailabilityDomain macOSAppExt =
      domainForPlatform(PlatformKind::macOSApplicationExtension);
  AvailabilityDomain iOS = domainForPlatform(PlatformKind::iOS);
  AvailabilityDomain iOSAppExt =
      domainForPlatform(PlatformKind::iOSApplicationExtension);
  AvailabilityDomain macCatalyst = domainForPlatform(PlatformKind::macCatalyst);
  AvailabilityDomain macCatalystAppExt =
      domainForPlatform(PlatformKind::macCatalystApplicationExtension);
  AvailabilityDomain visionOS = domainForPlatform(PlatformKind::visionOS);
  AvailabilityDomain visionOSAppExt =
      domainForPlatform(PlatformKind::visionOSApplicationExtension);
  AvailabilityDomain Windows = domainForPlatform(PlatformKind::Windows);
  AvailabilityDomain Android = domainForPlatform(PlatformKind::Android);

  std::vector<AvailabilityDomain> all() const {
    return {Universal,   Swift,
            Package,     Embedded,
            macOS,       macOSAppExt,
            iOS,         iOSAppExt,
            macCatalyst, macCatalystAppExt,
            visionOS,    visionOSAppExt,
            Windows,     Android};
  }
};

TEST_F(AvailabilityDomainLattice, Containment) {
  auto independentDomains = {Universal, Swift,   Package,
                             Embedded,  Windows, Android};

  // The independent domains only contain themselves.
  for (auto const &anyDomain : all()) {
    for (auto const &domain : independentDomains) {
      EXPECT_EQ(domain.contains(anyDomain), anyDomain == domain);
      EXPECT_FALSE(domain.isSupersetOf(anyDomain));
      EXPECT_EQ(domain.isRelated(anyDomain), anyDomain == domain);
    }
  }

  // Platform kind domains form their own lattice in which app extension domains
  // are contained within the domain of the same platform.
  EXPECT_TRUE(macOS.contains(macOS));
  EXPECT_FALSE(macOS.isSupersetOf(macOS));
  EXPECT_TRUE(macOS.isRelated(macOS));

  EXPECT_FALSE(macOS.contains(iOS));
  EXPECT_FALSE(macOS.isSupersetOf(iOS));
  EXPECT_FALSE(macOS.isRelated(iOS));

  EXPECT_TRUE(macOS.contains(macOSAppExt));
  EXPECT_TRUE(macOS.isSupersetOf(macOSAppExt));
  EXPECT_TRUE(macOS.isRelated(macOSAppExt));

  EXPECT_FALSE(macOSAppExt.contains(macOS));
  EXPECT_FALSE(macOSAppExt.isSupersetOf(macOS));
  EXPECT_TRUE(macOSAppExt.isRelated(macOS));

  EXPECT_FALSE(macOS.contains(macCatalyst));
  EXPECT_FALSE(macOS.isSupersetOf(macCatalyst));
  EXPECT_FALSE(macOS.isRelated(macCatalyst));

  EXPECT_FALSE(macCatalyst.contains(macOS));
  EXPECT_FALSE(macCatalyst.isSupersetOf(macOS));
  EXPECT_FALSE(macCatalyst.isRelated(macOS));

  // iOS is the ABI platform for macCatalyst and visionOS, so it contain those
  // domains.
  EXPECT_TRUE(iOS.contains(iOS));
  EXPECT_FALSE(iOS.isSupersetOf(iOS));
  EXPECT_TRUE(iOS.isRelated(iOS));

  EXPECT_TRUE(iOS.contains(iOSAppExt));
  EXPECT_TRUE(iOS.isSupersetOf(iOSAppExt));
  EXPECT_TRUE(iOS.isRelated(iOSAppExt));

  EXPECT_TRUE(iOS.contains(macCatalyst));
  EXPECT_TRUE(iOS.isSupersetOf(macCatalyst));
  EXPECT_TRUE(iOS.isRelated(macCatalyst));

  EXPECT_TRUE(iOS.contains(macCatalystAppExt));
  EXPECT_TRUE(iOS.isSupersetOf(macCatalystAppExt));
  EXPECT_TRUE(iOS.isRelated(macCatalystAppExt));

  EXPECT_TRUE(iOS.contains(visionOS));
  EXPECT_TRUE(iOS.isSupersetOf(visionOS));
  EXPECT_TRUE(iOS.isRelated(visionOS));

  EXPECT_TRUE(iOS.contains(visionOSAppExt));
  EXPECT_TRUE(iOS.isSupersetOf(visionOSAppExt));
  EXPECT_TRUE(iOS.isRelated(visionOSAppExt));

  EXPECT_FALSE(iOS.contains(macOS));
  EXPECT_FALSE(iOS.isSupersetOf(macOS));
  EXPECT_FALSE(iOS.isRelated(macOS));

  EXPECT_FALSE(iOS.contains(macOSAppExt));
  EXPECT_FALSE(iOS.isSupersetOf(macOSAppExt));
  EXPECT_FALSE(iOS.isRelated(macOSAppExt));

  // iOSApplicationExtension
  EXPECT_TRUE(iOSAppExt.contains(iOSAppExt));
  EXPECT_FALSE(iOSAppExt.isSupersetOf(iOSAppExt));
  EXPECT_TRUE(iOSAppExt.isRelated(iOSAppExt));

  EXPECT_FALSE(iOSAppExt.contains(iOS));
  EXPECT_FALSE(iOSAppExt.isSupersetOf(iOS));
  EXPECT_TRUE(iOSAppExt.isRelated(iOS));

  EXPECT_FALSE(iOSAppExt.contains(macCatalyst));
  EXPECT_FALSE(iOSAppExt.isSupersetOf(macCatalyst));
  EXPECT_FALSE(iOSAppExt.isRelated(macCatalyst));

  EXPECT_TRUE(iOSAppExt.contains(macCatalystAppExt));
  EXPECT_TRUE(iOSAppExt.isSupersetOf(macCatalystAppExt));
  EXPECT_TRUE(iOSAppExt.isRelated(macCatalystAppExt));

  EXPECT_FALSE(iOSAppExt.contains(visionOS));
  EXPECT_FALSE(iOSAppExt.isSupersetOf(visionOS));
  EXPECT_FALSE(iOSAppExt.isRelated(visionOS));

  EXPECT_TRUE(iOSAppExt.contains(visionOSAppExt));
  EXPECT_TRUE(iOSAppExt.isSupersetOf(visionOSAppExt));
  EXPECT_TRUE(iOSAppExt.isRelated(visionOSAppExt));

  EXPECT_FALSE(iOSAppExt.contains(macOS));
  EXPECT_FALSE(iOSAppExt.isSupersetOf(macOS));
  EXPECT_FALSE(iOSAppExt.isRelated(macOS));

  EXPECT_FALSE(iOSAppExt.contains(macOSAppExt));
  EXPECT_FALSE(iOSAppExt.isSupersetOf(macOSAppExt));
  EXPECT_FALSE(iOSAppExt.isRelated(macOSAppExt));

  // macCatalyst
  EXPECT_TRUE(macCatalyst.contains(macCatalyst));
  EXPECT_FALSE(macCatalyst.isSupersetOf(macCatalyst));
  EXPECT_TRUE(macCatalyst.isRelated(macCatalyst));

  EXPECT_TRUE(macCatalyst.contains(macCatalystAppExt));
  EXPECT_TRUE(macCatalyst.isSupersetOf(macCatalystAppExt));
  EXPECT_TRUE(macCatalyst.isRelated(macCatalystAppExt));

  EXPECT_FALSE(macCatalyst.contains(iOS));
  EXPECT_FALSE(macCatalyst.isSupersetOf(iOS));
  EXPECT_TRUE(macCatalyst.isRelated(iOS));

  EXPECT_FALSE(macCatalyst.contains(iOSAppExt));
  EXPECT_FALSE(macCatalyst.isSupersetOf(iOSAppExt));
  EXPECT_FALSE(macCatalyst.isRelated(iOSAppExt));

  EXPECT_FALSE(macCatalyst.contains(visionOS));
  EXPECT_FALSE(macCatalyst.isSupersetOf(visionOS));
  EXPECT_FALSE(macCatalyst.isRelated(visionOS));

  EXPECT_FALSE(macCatalyst.contains(visionOSAppExt));
  EXPECT_FALSE(macCatalyst.isSupersetOf(visionOSAppExt));
  EXPECT_FALSE(macCatalyst.isRelated(visionOSAppExt));

  EXPECT_FALSE(macCatalyst.contains(macOS));
  EXPECT_FALSE(macCatalyst.isSupersetOf(macOS));
  EXPECT_FALSE(macCatalyst.isRelated(macOS));

  EXPECT_FALSE(macCatalyst.contains(macOSAppExt));
  EXPECT_FALSE(macCatalyst.isSupersetOf(macOSAppExt));
  EXPECT_FALSE(macCatalyst.isRelated(macOSAppExt));

  // macCatalystApplicationExtension
  EXPECT_TRUE(macCatalystAppExt.contains(macCatalystAppExt));
  EXPECT_FALSE(macCatalystAppExt.isSupersetOf(macCatalystAppExt));
  EXPECT_TRUE(macCatalystAppExt.isRelated(macCatalystAppExt));

  EXPECT_FALSE(macCatalystAppExt.contains(macCatalyst));
  EXPECT_FALSE(macCatalystAppExt.isSupersetOf(macCatalyst));
  EXPECT_TRUE(macCatalystAppExt.isRelated(macCatalyst));

  EXPECT_FALSE(macCatalystAppExt.contains(iOS));
  EXPECT_FALSE(macCatalystAppExt.isSupersetOf(iOS));
  EXPECT_TRUE(macCatalystAppExt.isRelated(iOS));

  EXPECT_FALSE(macCatalystAppExt.contains(iOSAppExt));
  EXPECT_FALSE(macCatalystAppExt.isSupersetOf(iOSAppExt));
  EXPECT_TRUE(macCatalystAppExt.isRelated(iOSAppExt));

  EXPECT_FALSE(macCatalystAppExt.contains(visionOS));
  EXPECT_FALSE(macCatalystAppExt.isSupersetOf(visionOS));
  EXPECT_FALSE(macCatalystAppExt.isRelated(visionOS));

  EXPECT_FALSE(macCatalystAppExt.contains(visionOSAppExt));
  EXPECT_FALSE(macCatalystAppExt.isSupersetOf(visionOSAppExt));
  EXPECT_FALSE(macCatalystAppExt.isRelated(visionOSAppExt));

  EXPECT_FALSE(macCatalystAppExt.contains(macOS));
  EXPECT_FALSE(macCatalystAppExt.isSupersetOf(macOS));
  EXPECT_FALSE(macCatalystAppExt.isRelated(macOS));

  EXPECT_FALSE(macCatalystAppExt.contains(macOSAppExt));
  EXPECT_FALSE(macCatalystAppExt.isSupersetOf(macOSAppExt));
  EXPECT_FALSE(macCatalystAppExt.isRelated(macOSAppExt));

  // visionOS
  EXPECT_TRUE(visionOS.contains(visionOS));
  EXPECT_FALSE(visionOS.isSupersetOf(visionOS));
  EXPECT_TRUE(visionOS.isRelated(visionOS));

  EXPECT_TRUE(visionOS.contains(visionOSAppExt));
  EXPECT_TRUE(visionOS.isSupersetOf(visionOSAppExt));
  EXPECT_TRUE(visionOS.isRelated(visionOSAppExt));

  EXPECT_FALSE(visionOS.contains(iOS));
  EXPECT_FALSE(visionOS.isSupersetOf(iOS));
  EXPECT_TRUE(visionOS.isRelated(iOS));

  EXPECT_FALSE(visionOS.contains(iOSAppExt));
  EXPECT_FALSE(visionOS.isSupersetOf(iOSAppExt));
  EXPECT_FALSE(visionOS.isRelated(iOSAppExt));

  EXPECT_FALSE(visionOS.contains(macCatalyst));
  EXPECT_FALSE(visionOS.isSupersetOf(macCatalyst));
  EXPECT_FALSE(visionOS.isRelated(macCatalyst));

  EXPECT_FALSE(visionOS.contains(macCatalystAppExt));
  EXPECT_FALSE(visionOS.isSupersetOf(macCatalystAppExt));
  EXPECT_FALSE(visionOS.isRelated(macCatalystAppExt));

  EXPECT_FALSE(visionOS.contains(macOS));
  EXPECT_FALSE(visionOS.isSupersetOf(macOS));
  EXPECT_FALSE(visionOS.isRelated(macOS));

  EXPECT_FALSE(visionOS.contains(macOSAppExt));
  EXPECT_FALSE(visionOS.isSupersetOf(macOSAppExt));
  EXPECT_FALSE(visionOS.isRelated(macOSAppExt));

  // visionOSApplicationExtension
  EXPECT_TRUE(visionOSAppExt.contains(visionOSAppExt));
  EXPECT_FALSE(visionOSAppExt.isSupersetOf(visionOSAppExt));
  EXPECT_TRUE(visionOSAppExt.isRelated(visionOSAppExt));

  EXPECT_FALSE(visionOSAppExt.contains(visionOS));
  EXPECT_FALSE(visionOSAppExt.isSupersetOf(visionOS));
  EXPECT_TRUE(visionOSAppExt.isRelated(visionOS));

  EXPECT_FALSE(visionOSAppExt.contains(iOS));
  EXPECT_FALSE(visionOSAppExt.isSupersetOf(iOS));
  EXPECT_TRUE(visionOSAppExt.isRelated(iOS));

  EXPECT_FALSE(visionOSAppExt.contains(iOSAppExt));
  EXPECT_FALSE(visionOSAppExt.isSupersetOf(iOSAppExt));
  EXPECT_TRUE(visionOSAppExt.isRelated(iOSAppExt));

  EXPECT_FALSE(visionOSAppExt.contains(macCatalyst));
  EXPECT_FALSE(visionOSAppExt.isSupersetOf(macCatalyst));
  EXPECT_FALSE(visionOSAppExt.isRelated(macCatalyst));

  EXPECT_FALSE(visionOSAppExt.contains(macCatalystAppExt));
  EXPECT_FALSE(visionOSAppExt.isSupersetOf(macCatalystAppExt));
  EXPECT_FALSE(visionOSAppExt.isRelated(macCatalystAppExt));

  EXPECT_FALSE(visionOSAppExt.contains(macOS));
  EXPECT_FALSE(visionOSAppExt.isSupersetOf(macOS));
  EXPECT_FALSE(visionOSAppExt.isRelated(macOS));

  EXPECT_FALSE(visionOSAppExt.contains(macOSAppExt));
  EXPECT_FALSE(visionOSAppExt.isSupersetOf(macOSAppExt));
  EXPECT_FALSE(visionOSAppExt.isRelated(macOSAppExt));
}

TEST_F(AvailabilityDomainLattice, RootDomain) {
  EXPECT_EQ(Universal.getRootDomain(), Universal);
  EXPECT_TRUE(Universal.isRoot());
  EXPECT_EQ(Swift.getRootDomain(), Swift);
  EXPECT_TRUE(Swift.isRoot());
  EXPECT_EQ(Package.getRootDomain(), Package);
  EXPECT_TRUE(Package.isRoot());
  EXPECT_EQ(Embedded.getRootDomain(), Embedded);
  EXPECT_TRUE(Embedded.isRoot());
  EXPECT_EQ(macOS.getRootDomain(), macOS);
  EXPECT_TRUE(macOS.isRoot());
  EXPECT_EQ(macOSAppExt.getRootDomain(), macOS);
  EXPECT_FALSE(macOSAppExt.isRoot());
  EXPECT_EQ(iOS.getRootDomain(), iOS);
  EXPECT_TRUE(iOS.isRoot());
  EXPECT_EQ(iOSAppExt.getRootDomain(), iOS);
  EXPECT_FALSE(iOSAppExt.isRoot());
  EXPECT_EQ(macCatalyst.getRootDomain(), iOS);
  EXPECT_FALSE(macCatalyst.isRoot());
  EXPECT_EQ(macCatalystAppExt.getRootDomain(), iOS);
  EXPECT_FALSE(macCatalystAppExt.isRoot());
  EXPECT_EQ(visionOS.getRootDomain(), iOS);
  EXPECT_FALSE(visionOS.isRoot());
  EXPECT_EQ(visionOSAppExt.getRootDomain(), iOS);
  EXPECT_FALSE(visionOSAppExt.isRoot());
}
