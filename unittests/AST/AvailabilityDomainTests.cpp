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

    // FIXME: [availability] The following assertions should change when
    // AvailabilityContext can support multiple simultaneous unavailable
    // domains.

    // The Swift domain is second from the bottom.
    EXPECT_EQ(Swift.contains(domain), !domain.isUniversal());

    // Package and Embedded are both third from the bottom.
    EXPECT_EQ(Package.contains(domain), !domain.isUniversal() && !domain.isSwiftLanguage());
    EXPECT_EQ(Embedded.contains(domain), !domain.isUniversal() && !domain.isSwiftLanguage());
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

TEST_F(AvailabilityDomainLattice, ABICompatibilityDomain) {
  EXPECT_EQ(Universal.getABICompatibilityDomain(), Universal);
  EXPECT_EQ(Swift.getABICompatibilityDomain(), Swift);
  EXPECT_EQ(Package.getABICompatibilityDomain(), Package);
  EXPECT_EQ(Embedded.getABICompatibilityDomain(), Embedded);
  EXPECT_EQ(macOS.getABICompatibilityDomain(), macOS);
  EXPECT_EQ(macOSAppExt.getABICompatibilityDomain(), macOS);
  EXPECT_EQ(iOS.getABICompatibilityDomain(), iOS);
  EXPECT_EQ(iOSAppExt.getABICompatibilityDomain(), iOS);
  EXPECT_EQ(macCatalyst.getABICompatibilityDomain(), iOS);
  EXPECT_EQ(macCatalystAppExt.getABICompatibilityDomain(), iOS);
  EXPECT_EQ(visionOS.getABICompatibilityDomain(), iOS);
  EXPECT_EQ(visionOSAppExt.getABICompatibilityDomain(), iOS);
}

TEST(AvailabilityDomain, TargetPlatform) {
  using namespace llvm;

  struct TargetToPlatformKind {
    Triple target;
    PlatformKind platformKind;
  };
  TargetToPlatformKind tests[] = {
      {Triple("x86_64", "apple", "macosx10.15"), PlatformKind::macOS},
      {Triple("arm64", "apple", "ios13"), PlatformKind::iOS},
      {Triple("arm64_32", "apple", "watchos8"), PlatformKind::watchOS},
      {Triple("x86_64", "apple", "ios14", "macabi"), PlatformKind::macCatalyst},
      {Triple("x86_64", "unknown", "windows", "msvc"), PlatformKind::none},
      {Triple("x86_64", "unknown", "linux", "gnu"), PlatformKind::none},
  };

  for (TargetToPlatformKind test : tests) {
    TestContext context{test.target};
    auto domain = AvailabilityDomain::forTargetPlatform(context.Ctx);
    if (test.platformKind != PlatformKind::none) {
      EXPECT_TRUE(domain);
      if (domain)
        EXPECT_TRUE(domain->getPlatformKind() == test.platformKind);
    } else {
      EXPECT_FALSE(domain);
    }
  }
}
