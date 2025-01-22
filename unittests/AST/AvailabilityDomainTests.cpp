#include "swift/AST/AvailabilityDomain.h"
#include "gtest/gtest.h"

using namespace swift;

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

  // Additionally, iOS is the ABI platform for both macCatalyst and visionOS and
  // thus the iOS domain contains those domains.
  EXPECT_TRUE(iOS.contains(iOS));
  EXPECT_TRUE(iOS.contains(iOSAppExt));
  EXPECT_FALSE(iOSAppExt.contains(iOS));
  EXPECT_TRUE(iOS.contains(macCatalyst));
  EXPECT_FALSE(macCatalyst.contains(iOS));
  EXPECT_TRUE(iOS.contains(macCatalystAppExt));
  EXPECT_FALSE(macCatalystAppExt.contains(iOS));
  EXPECT_TRUE(iOS.contains(visionOS));
  EXPECT_FALSE(visionOS.contains(iOS));
  EXPECT_TRUE(iOS.contains(visionOSAppExt));
  EXPECT_FALSE(visionOSAppExt.contains(iOS));
  EXPECT_FALSE(iOS.contains(macOS));
  EXPECT_FALSE(iOS.contains(macOSAppExt));
}
