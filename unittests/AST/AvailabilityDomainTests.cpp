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
  AvailabilityDomain Swift = AvailabilityDomain::forSwiftLanguage();
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

  std::vector<AvailabilityDomain> all() const {
    return {
        Universal,   Swift,         Package,   Embedded,    macOS,
        macOSAppExt, iOS,           iOSAppExt, macCatalyst, macCatalystAppExt,
        visionOS,    visionOSAppExt};
  }
};

TEST_F(AvailabilityDomainLattice, Contains) {
  for (auto const &domain : all()) {
    // The universal domain is the bottom domain and contains all others.
    EXPECT_TRUE(Universal.contains(domain));

    // These domains only contain themselves.
    EXPECT_EQ(Swift.contains(domain), domain.isSwiftLanguage());
    EXPECT_EQ(Package.contains(domain), domain.isPackageDescription());
    EXPECT_EQ(Embedded.contains(domain), domain.isEmbedded());
  }

  // Platform kind domains form their own lattice in which app extension domains
  // are contained within the domain of the same platform.
  EXPECT_TRUE(macOS.contains(macOS));
  EXPECT_FALSE(macOS.contains(iOS));
  EXPECT_TRUE(macOS.contains(macOSAppExt));
  EXPECT_FALSE(macOSAppExt.contains(macOS));
  EXPECT_FALSE(macOS.contains(macCatalyst));
  EXPECT_FALSE(macCatalyst.contains(macOS));

  // iOS is the ABI platform for macCatalyst and visionOS, so it contain those
  // domains.
  EXPECT_TRUE(iOS.contains(iOS));
  EXPECT_TRUE(iOS.contains(iOSAppExt));
  EXPECT_TRUE(iOS.contains(macCatalyst));
  EXPECT_TRUE(iOS.contains(macCatalystAppExt));
  EXPECT_TRUE(iOS.contains(visionOS));
  EXPECT_TRUE(iOS.contains(visionOSAppExt));
  EXPECT_FALSE(iOS.contains(macOS));
  EXPECT_FALSE(iOS.contains(macOSAppExt));

  EXPECT_TRUE(iOSAppExt.contains(iOSAppExt));
  EXPECT_FALSE(iOSAppExt.contains(iOS));
  EXPECT_FALSE(iOSAppExt.contains(macCatalyst));
  EXPECT_TRUE(iOSAppExt.contains(macCatalystAppExt));
  EXPECT_FALSE(iOSAppExt.contains(visionOS));
  EXPECT_TRUE(iOSAppExt.contains(visionOSAppExt));
  EXPECT_FALSE(iOSAppExt.contains(macOS));
  EXPECT_FALSE(iOSAppExt.contains(macOSAppExt));

  EXPECT_TRUE(macCatalyst.contains(macCatalyst));
  EXPECT_TRUE(macCatalyst.contains(macCatalystAppExt));
  EXPECT_FALSE(macCatalyst.contains(iOS));
  EXPECT_FALSE(macCatalyst.contains(iOSAppExt));
  EXPECT_FALSE(macCatalyst.contains(visionOS));
  EXPECT_FALSE(macCatalyst.contains(visionOSAppExt));
  EXPECT_FALSE(macCatalyst.contains(macOS));
  EXPECT_FALSE(macCatalyst.contains(macOSAppExt));

  EXPECT_TRUE(macCatalystAppExt.contains(macCatalystAppExt));
  EXPECT_FALSE(macCatalystAppExt.contains(macCatalyst));
  EXPECT_FALSE(macCatalystAppExt.contains(iOS));
  EXPECT_FALSE(macCatalystAppExt.contains(iOSAppExt));
  EXPECT_FALSE(macCatalystAppExt.contains(visionOS));
  EXPECT_FALSE(macCatalystAppExt.contains(visionOSAppExt));
  EXPECT_FALSE(macCatalystAppExt.contains(macOS));
  EXPECT_FALSE(macCatalystAppExt.contains(macOSAppExt));

  EXPECT_TRUE(visionOS.contains(visionOS));
  EXPECT_TRUE(visionOS.contains(visionOSAppExt));
  EXPECT_FALSE(visionOS.contains(iOS));
  EXPECT_FALSE(visionOS.contains(iOSAppExt));
  EXPECT_FALSE(visionOS.contains(macCatalyst));
  EXPECT_FALSE(visionOS.contains(macCatalystAppExt));
  EXPECT_FALSE(visionOS.contains(macOS));
  EXPECT_FALSE(visionOS.contains(macOSAppExt));

  EXPECT_TRUE(visionOSAppExt.contains(visionOSAppExt));
  EXPECT_FALSE(visionOSAppExt.contains(visionOS));
  EXPECT_FALSE(visionOSAppExt.contains(iOS));
  EXPECT_FALSE(visionOSAppExt.contains(iOSAppExt));
  EXPECT_FALSE(visionOSAppExt.contains(macCatalyst));
  EXPECT_FALSE(visionOSAppExt.contains(macCatalystAppExt));
  EXPECT_FALSE(visionOSAppExt.contains(macOS));
  EXPECT_FALSE(visionOSAppExt.contains(macOSAppExt));
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
