//===--- Platform.cpp - Implement platform-related helpers ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Assertions.h"
#include "swift/Basic/Pack.h"
#include "swift/Basic/Platform.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Support/VersionTuple.h"

using namespace swift;

bool swift::tripleIsiOSSimulator(const llvm::Triple &triple) {
  return (triple.isiOS() &&
          !tripleIsMacCatalystEnvironment(triple) &&
          triple.isSimulatorEnvironment());
}

bool swift::tripleIsAppleTVSimulator(const llvm::Triple &triple) {
  return (triple.isTvOS() && triple.isSimulatorEnvironment());
}

bool swift::tripleIsWatchSimulator(const llvm::Triple &triple) {
  return (triple.isWatchOS() && triple.isSimulatorEnvironment());
}

bool swift::tripleIsMacCatalystEnvironment(const llvm::Triple &triple) {
  return triple.isiOS() && !triple.isTvOS() &&
      triple.getEnvironment() == llvm::Triple::MacABI;
}

bool swift::tripleIsVisionSimulator(const llvm::Triple &triple) {
  return triple.isXROS() && triple.isSimulatorEnvironment();
}

bool swift::tripleInfersSimulatorEnvironment(const llvm::Triple &triple) {
  switch (triple.getOS()) {
  case llvm::Triple::IOS:
  case llvm::Triple::TvOS:
  case llvm::Triple::WatchOS:
    return !triple.hasEnvironment() &&
        (triple.getArch() == llvm::Triple::x86 ||
         triple.getArch() == llvm::Triple::x86_64) &&
        !tripleIsMacCatalystEnvironment(triple);

  default:
    return false;
  }
}

bool swift::triplesAreValidForZippering(const llvm::Triple &target,
                                        const llvm::Triple &targetVariant) {
  // The arch and vendor must match.
  if (target.getArchName() != targetVariant.getArchName() ||
      target.getArch() != targetVariant.getArch() ||
      target.getSubArch() != targetVariant.getSubArch() ||
      target.getVendor() != targetVariant.getVendor()) {
    return false;
  }

  // Allow a macOS target and an iOS-macabi target variant
  // This is typically the case when zippering a library originally
  // developed for macOS.
  if (target.isMacOSX() && tripleIsMacCatalystEnvironment(targetVariant)) {
    return true;
  }

  // Allow an iOS-macabi target and a macOS target variant. This would
  // be the case when zippering a library originally developed for
  // iOS.
  if (targetVariant.isMacOSX() && tripleIsMacCatalystEnvironment(target)) {
    return true;
  }

  return false;
}

const std::optional<llvm::VersionTuple>
swift::minimumAvailableOSVersionForTriple(const llvm::Triple &triple) {
  if (triple.isMacOSX())
    return llvm::VersionTuple(10, 10, 0);

  // Mac Catalyst was introduced with an iOS deployment target of 13.1.
  if (tripleIsMacCatalystEnvironment(triple))
    return llvm::VersionTuple(13, 1);
  
  // Note: this must come before checking iOS since that returns true for
  // both iOS and tvOS.
  if (triple.isTvOS())
    return llvm::VersionTuple(9, 0);

  if (triple.isiOS())
    return llvm::VersionTuple(8, 0);

  if (triple.isWatchOS())
    return llvm::VersionTuple(2, 0);

  if (triple.isXROS())
    return llvm::VersionTuple(1, 0);

  return std::nullopt;
}

bool swift::tripleRequiresRPathForSwiftLibrariesInOS(
    const llvm::Triple &triple) {
  if (triple.isMacOSX()) {
    // macOS versions before 10.14.4 don't have Swift in the OS
    // (the linker still uses an rpath-based install name until 10.15).
    // macOS versions before 12.0 don't have _Concurrency in the OS.
    return triple.isMacOSXVersionLT(12, 0);
  }

  if (triple.isiOS()) {
    // iOS versions before 12.2 don't have Swift in the OS.
    // iOS versions before 15.0 don't have _Concurrency in the OS.
    return triple.isOSVersionLT(15, 0);
  }

  if (triple.isWatchOS()) {
    // watchOS versions before 5.2 don't have Swift in the OS.
    // watchOS versions before 8.0 don't have _Concurrency in the OS.
    return triple.isOSVersionLT(8, 0);
  }

  if (triple.isXROS()) {
    return triple.isOSVersionLT(1, 0);
  }

  // Other platforms don't have Swift installed as part of the OS by default.
  return false;
}

bool swift::tripleBTCFIByDefaultInOpenBSD(const llvm::Triple &triple) {
  return triple.isOSOpenBSD() && triple.getArch() == llvm::Triple::aarch64;
}

DarwinPlatformKind swift::getDarwinPlatformKind(const llvm::Triple &triple) {
  if (triple.isiOS()) {
    if (triple.isTvOS()) {
      if (tripleIsAppleTVSimulator(triple))
        return DarwinPlatformKind::TvOSSimulator;
      return DarwinPlatformKind::TvOS;
    }

    if (tripleIsiOSSimulator(triple))
      return DarwinPlatformKind::IPhoneOSSimulator;

    return DarwinPlatformKind::IPhoneOS;
  }

  if (triple.isWatchOS()) {
    if (tripleIsWatchSimulator(triple))
      return DarwinPlatformKind::WatchOSSimulator;
    return DarwinPlatformKind::WatchOS;
  }

  if (triple.isMacOSX())
    return DarwinPlatformKind::MacOS;

  if (triple.isXROS()) {
    if (tripleIsVisionSimulator(triple))
      return DarwinPlatformKind::VisionOSSimulator;
    return DarwinPlatformKind::VisionOS;
  }

  llvm_unreachable("Unsupported Darwin platform");
}

static StringRef getPlatformNameForDarwin(const DarwinPlatformKind platform) {
  switch (platform) {
  case DarwinPlatformKind::MacOS:
    return "macosx";
  case DarwinPlatformKind::IPhoneOS:
    return "iphoneos";
  case DarwinPlatformKind::IPhoneOSSimulator:
    return "iphonesimulator";
  case DarwinPlatformKind::TvOS:
    return "appletvos";
  case DarwinPlatformKind::TvOSSimulator:
    return "appletvsimulator";
  case DarwinPlatformKind::WatchOS:
    return "watchos";
  case DarwinPlatformKind::WatchOSSimulator:
    return "watchsimulator";
  case DarwinPlatformKind::VisionOS:
    return "xros";
  case DarwinPlatformKind::VisionOSSimulator:
    return "xrsimulator";
  }
  llvm_unreachable("Unsupported Darwin platform");
}

StringRef swift::getPlatformNameForTriple(const llvm::Triple &triple) {
  switch (triple.getOS()) {
  case llvm::Triple::AIX:
  case llvm::Triple::AMDHSA:
  case llvm::Triple::AMDPAL:
  case llvm::Triple::BridgeOS:
  case llvm::Triple::CUDA:
  case llvm::Triple::DragonFly:
  case llvm::Triple::DriverKit:
  case llvm::Triple::ELFIAMCU:
  case llvm::Triple::Emscripten:
  case llvm::Triple::Fuchsia:
  case llvm::Triple::HermitCore:
  case llvm::Triple::Hurd:
  case llvm::Triple::KFreeBSD:
  case llvm::Triple::Lv2:
  case llvm::Triple::Mesa3D:
  case llvm::Triple::NaCl:
  case llvm::Triple::NetBSD:
  case llvm::Triple::NVCL:
  case llvm::Triple::PS5:
  case llvm::Triple::RTEMS:
  case llvm::Triple::Serenity:
  case llvm::Triple::ShaderModel:
  case llvm::Triple::Solaris:
  case llvm::Triple::Vulkan:
  case llvm::Triple::ZOS:
    return "";
  case llvm::Triple::Darwin:
  case llvm::Triple::MacOSX:
  case llvm::Triple::IOS:
  case llvm::Triple::TvOS:
  case llvm::Triple::WatchOS:
  case llvm::Triple::XROS:
    return getPlatformNameForDarwin(getDarwinPlatformKind(triple));
  case llvm::Triple::Linux:
    if (triple.isAndroid())
      return "android";
    else if (triple.isMusl()) {
      // The triple for linux-static is <arch>-swift-linux-musl, to distinguish
      // it from a "normal" musl set-up (ala Alpine).
      if (triple.getVendor() == llvm::Triple::Swift)
        return "linux-static";
      else
        return "musl";
    } else
      return "linux";
  case llvm::Triple::FreeBSD:
    return "freebsd";
  case llvm::Triple::OpenBSD:
    return "openbsd";
  case llvm::Triple::Win32:
    switch (triple.getEnvironment()) {
    case llvm::Triple::Cygnus:
      return "cygwin";
    case llvm::Triple::GNU:
      return "mingw";
    case llvm::Triple::MSVC:
    case llvm::Triple::Itanium:
      return "windows";
    default:
      return "none";
    }
  case llvm::Triple::PS4:
    return "ps4";
  case llvm::Triple::Haiku:
    return "haiku";
  case llvm::Triple::WASI:
    return "wasi";
  case llvm::Triple::UnknownOS:
    return "none";
  case llvm::Triple::UEFI:
  case llvm::Triple::LiteOS:
    llvm_unreachable("unsupported OS");
  }
  llvm_unreachable("unsupported OS");
}

llvm::VersionTuple swift::getVersionForTriple(const llvm::Triple &triple) {
  if (triple.isMacOSX()) {
    llvm::VersionTuple OSVersion;
    triple.getMacOSXVersion(OSVersion);
    return OSVersion;
  } else if (triple.isiOS()) {
    return triple.getiOSVersion();
  } else if (triple.isWatchOS()) {
    return triple.getOSVersion();
  } else if (triple.isXROS()) {
    return triple.getOSVersion();
  } else if (triple.isOSWindows()) {
    return triple.getOSVersion();
  }
  return llvm::VersionTuple(/*Major=*/0, /*Minor=*/0, /*Subminor=*/0);
}

StringRef swift::getMajorArchitectureName(const llvm::Triple &Triple) {
  if (Triple.isOSLinux()) {
    switch (Triple.getSubArch()) {
    case llvm::Triple::SubArchType::ARMSubArch_v7:
      return "armv7";
    case llvm::Triple::SubArchType::ARMSubArch_v6:
      return "armv6";
    case llvm::Triple::SubArchType::ARMSubArch_v5:
      return "armv5";
    default:
      break;
    }
  }

  if (Triple.isOSOpenBSD()) {
    if (Triple.getArchName() == "amd64") {
      return "x86_64";
    }
  }

  return Triple.getArchName();
}

// The code below is responsible for normalizing target triples into the form
// used to name target-specific swiftmodule, swiftinterface, and swiftdoc files.
// If two triples have incompatible ABIs or can be distinguished by Swift #if
// declarations, they should normalize to different values.
//
// This code is only really used on platforms with toolchains supporting fat
// binaries (a single binary containing multiple architectures). On these
// platforms, this code should strip unnecessary details from target triple
// components and map synonyms to canonical values. Even values which don't need
// any special canonicalization should be documented here as comments.
//
// (Fallback behavior does not belong here; it should be implemented in code
// that calls this function, most importantly in SerializedModuleLoaderBase.)
//
// If you're trying to refer to this code to understand how Swift behaves and
// you're unfamiliar with LLVM internals, here's a cheat sheet for reading it:
//
// * llvm::Triple is the type for a target name. It's a bit of a misnomer,
//   because it can contain three or four values: arch-vendor-os[-environment].
//
// * In .Cases and .Case, the last argument is the value the arguments before it
//   map to. That is, `.Cases("bar", "baz", "foo")` will return "foo" if it sees
//   "bar" or "baz".
//
// * std::optional is similar to a Swift Optional: it either contains a value
//   or represents the absence of one. `None` is equivalent to `nil`; leading
//   `*` is equivalent to trailing `!`; conversion to `bool` is a not-`None`
//   check.

static StringRef
getArchForAppleTargetSpecificModuleTriple(const llvm::Triple &triple) {
  auto tripleArchName = triple.getArchName();

  return llvm::StringSwitch<StringRef>(tripleArchName)
              .Cases("arm64", "aarch64", "arm64")
              .Cases("arm64_32", "aarch64_32", "arm64_32")
              .Cases("x86_64", "amd64", "x86_64")
              .Cases("i386", "i486", "i586", "i686", "i786", "i886", "i986",
                     "i386")
              .Cases("unknown", "", "unknown")
  // These values are also supported, but are handled by the default case below:
  //          .Case ("armv7s", "armv7s")
  //          .Case ("armv7k", "armv7k")
  //          .Case ("armv7", "armv7")
  //          .Case ("arm64e", "arm64e")
              .Default(tripleArchName);
}

static StringRef
getVendorForAppleTargetSpecificModuleTriple(const llvm::Triple &triple) {
  // We unconditionally normalize to "apple" because it's relatively common for
  // build systems to omit the vendor name or use an incorrect one like
  // "unknown". Most parts of the compiler ignore the vendor, so you might not
  // notice such a mistake.
  //
  // Please don't depend on this behavior--specify 'apple' if you're building
  // for an Apple platform.

  assert(triple.isOSDarwin() &&
         "shouldn't normalize non-Darwin triple to 'apple'");

  return "apple";
}

static StringRef
getOSForAppleTargetSpecificModuleTriple(const llvm::Triple &triple) {
  auto tripleOSName = triple.getOSName();

  // Truncate the OS name before the first digit. "Digit" here is ASCII '0'-'9'.
  auto tripleOSNameNoVersion = tripleOSName.take_until(llvm::isDigit);

  return llvm::StringSwitch<StringRef>(tripleOSNameNoVersion)
              .Cases("macos", "macosx", "darwin", "macos")
              .Cases("unknown", "", "unknown")
  // These values are also supported, but are handled by the default case below:
  //          .Case ("ios", "ios")
  //          .Case ("tvos", "tvos")
  //          .Case ("watchos", "watchos")
              .Default(tripleOSNameNoVersion);
}

static std::optional<StringRef>
getEnvironmentForAppleTargetSpecificModuleTriple(const llvm::Triple &triple) {
  auto tripleEnvironment = triple.getEnvironmentName();
  return llvm::StringSwitch<std::optional<StringRef>>(tripleEnvironment)
      .Cases("unknown", "", std::nullopt)
      // These values are also supported, but are handled by the default case
      // below:
      //          .Case ("simulator", StringRef("simulator"))
      //          .Case ("macabi", StringRef("macabi"))
      .Default(tripleEnvironment);
}

llvm::Triple swift::getTargetSpecificModuleTriple(const llvm::Triple &triple) {
  // isOSDarwin() returns true for all Darwin-style OSes, including macOS, iOS,
  // etc.
  if (triple.isOSDarwin()) {
    StringRef newArch = getArchForAppleTargetSpecificModuleTriple(triple);

    StringRef newVendor = getVendorForAppleTargetSpecificModuleTriple(triple);

    StringRef newOS = getOSForAppleTargetSpecificModuleTriple(triple);

    std::optional<StringRef> newEnvironment =
        getEnvironmentForAppleTargetSpecificModuleTriple(triple);

    if (!newEnvironment)
      // Generate an arch-vendor-os triple.
      return llvm::Triple(newArch, newVendor, newOS);

    // Generate an arch-vendor-os-environment triple.
    return llvm::Triple(newArch, newVendor, newOS, *newEnvironment);
  }

  // android - drop the API level.  That is not pertinent to the module; the API
  // availability is handled by the clang importer.
  if (triple.isAndroid()) {
    StringRef environment =
        llvm::Triple::getEnvironmentTypeName(triple.getEnvironment());

    return llvm::Triple(triple.getArchName(), triple.getVendorName(),
                        triple.getOSName(), environment);
  }

  if (triple.isOSFreeBSD()) {
    return swift::getUnversionedTriple(triple);
  }

  if (triple.isOSOpenBSD()) {
    StringRef arch = swift::getMajorArchitectureName(triple);
    return llvm::Triple(arch, triple.getVendorName(), triple.getOSName());
  }

  // Other platforms get no normalization.
  return triple;
}

llvm::Triple swift::getUnversionedTriple(const llvm::Triple &triple) {
  StringRef unversionedOSName = triple.getOSName().take_until(llvm::isDigit);
  if (triple.getEnvironment()) {
    StringRef environment =
        llvm::Triple::getEnvironmentTypeName(triple.getEnvironment());

    return llvm::Triple(triple.getArchName(), triple.getVendorName(),
                        unversionedOSName, environment);
  }

  return llvm::Triple(triple.getArchName(), triple.getVendorName(),
                      unversionedOSName);
}

namespace {

// Here, we statically reflect the entire contents of RuntimeVersions.def
// into the template-argument structure of the type AllStaticSwiftReleases.
// We then use template metaprogramming on this type to synthesize arrays
// of PlatformSwiftRelease for each of the target platforms with
// deployment restrictions. This would be much simpler with the recent
// generalizations of constexpr and non-type template parameters, but
// those remain above our baseline for now, so we have to do this the
// old way.

/// A specific release of a platform that provides a specific Swift
/// runtime version. Ultimately, all the variadic goop below is just
/// building an array of these for each platform, which is what we'll
/// use at runtime.
struct PlatformSwiftRelease {
  llvm::VersionTuple swiftVersion;
  llvm::VersionTuple platformVersion;
};

/// A deployment-restricted platform.
enum class PlatformKind {
  macOS,
  iOS,
  watchOS,
  visionOS
};

/// A template which statically reflects a version tuple. Generalized
/// template parameters would theoretically let us just use
/// llvm::VersionTuple.
template <unsigned... Components>
struct StaticVersion;

/// A template which statically reflects a single PLATFORM in
/// RuntimeVersions.def.
template <PlatformKind Kind, class Version>
struct StaticPlatformRelease;

/// A template which statically reflects a single RUNTIME_VERSION in
/// RuntimeVersions.def.
template <class SwiftVersion, class PlatformReleases>
struct StaticSwiftRelease;

/// In the assumed context of a particular platform, the release
/// of the platform that first provided a particular Swift version.
template <class SwiftVersion, class PlatformVersion>
struct StaticPlatformSwiftRelease;

// C++ does not allow template argument lists to have trailing commas,
// so to make the macro metaprogramming side of this work, we have to
// include an extra type here (and special-case it in the transforms
// below) for the sole purpose of terminating the list without a comma.
struct Terminal;

#define UNPARENTHESIZE(...) __VA_ARGS__

using AllStaticSwiftReleases =
  packs::Pack<
#define PLATFORM(NAME, VERSION)                                    \
      StaticPlatformRelease<                                       \
        PlatformKind::NAME,                                        \
        StaticVersion<UNPARENTHESIZE VERSION>                      \
      >,
#define FUTURE
#define RUNTIME_VERSION(SWIFT_TUPLE, PROVIDERS)                    \
    StaticSwiftRelease<StaticVersion<UNPARENTHESIZE SWIFT_TUPLE>,  \
                       packs::Pack<PROVIDERS Terminal>>,
#include "swift/AST/RuntimeVersions.def"
    Terminal
  >;

#undef UNPARENTHESIZE

/// A template for comparing two StaticVersion type values.
template <class A, class B>
struct StaticVersionGT;
// 0.0 is not strictly greater than any version.
template <class Second>
struct StaticVersionGT<
    StaticVersion<>,
    Second
  > {
  static constexpr bool value = false;
};
// A version is strictly greater than 0.0 if it has any nonzero component.
template <unsigned FirstHead, unsigned... FirstTail>
struct StaticVersionGT<
    StaticVersion<FirstHead, FirstTail...>,
    StaticVersion<>
  > {
  static constexpr bool value =
    (FirstHead > 0) ? true :
    StaticVersionGT<StaticVersion<FirstTail...>,
                    StaticVersion<>>::value;
};
// a.b is strictly greater than c.d if (a > c || (a == c && b > d)).
template <unsigned FirstHead, unsigned... FirstTail,
          unsigned SecondHead, unsigned... SecondTail>
struct StaticVersionGT<
    StaticVersion<FirstHead, FirstTail...>,
    StaticVersion<SecondHead, SecondTail...>
  > {
  static constexpr bool value =
    (FirstHead > SecondHead) ? true :
    (FirstHead < SecondHead) ? false :
    StaticVersionGT<StaticVersion<FirstTail...>,
                    StaticVersion<SecondTail...>>::value;
};

/// A template for turning a StaticVersion into an llvm::VersionTuple.
template <class>
struct BuildVersionTuple;

template <unsigned... Components>
struct BuildVersionTuple<StaticVersion<Components...>> {
  static constexpr llvm::VersionTuple get() {
    return llvm::VersionTuple(Components...);
  }
};

/// A transform that takes a StaticPlatformRelease, checks if it
/// matches the given platform, and turns it into a
/// StaticPlatformSwiftRelease if so. The result is returned as an
/// optional pack which will be empty if the release is for a different
/// platform.
template <class, class>
struct BuildStaticPlatformSwiftReleaseHelper;
template <PlatformKind Platform, class SwiftVersion>
struct BuildStaticPlatformSwiftReleaseHelper_Arg;

// Matching case.
template <PlatformKind Platform, class SwiftVersion, class PlatformVersion>
struct BuildStaticPlatformSwiftReleaseHelper<
         BuildStaticPlatformSwiftReleaseHelper_Arg<Platform, SwiftVersion>,
         StaticPlatformRelease<Platform, PlatformVersion>> {
  using result = packs::Pack<
    StaticPlatformSwiftRelease<SwiftVersion, PlatformVersion>
  >;
};

// Non-matching case.
template <PlatformKind Platform, class SwiftVersion,
          PlatformKind OtherPlatform, class PlatformVersion>
struct BuildStaticPlatformSwiftReleaseHelper<
         BuildStaticPlatformSwiftReleaseHelper_Arg<Platform, SwiftVersion>,
         StaticPlatformRelease<OtherPlatform, PlatformVersion>> {
  using result = packs::Pack<>;
};

// Terminal case (see above).
template <PlatformKind Platform, class SwiftVersion>
struct BuildStaticPlatformSwiftReleaseHelper<
         BuildStaticPlatformSwiftReleaseHelper_Arg<Platform, SwiftVersion>,
         Terminal> {
  using result = packs::Pack<>;
};


/// A transform that takes a StaticSwiftRelease, finds the platform
/// release in it that matches the given platform, and turns it into
/// StaticPlatformSwiftRelease. The result is returned as an optional
/// pack which will be empty if there is no release for the given
/// platform in this SSR.
template <class, class>
struct BuildStaticPlatformSwiftRelease;
template <PlatformKind Platform>
struct BuildStaticPlatformSwiftRelease_Arg;

// Main case: destructure the arguments, then flat-map our helper
// transform above. Note that we assume that there aren't multiple
// entries for the same platform in the platform releases of a given
// Swift release.
template <PlatformKind Platform, class SwiftVersion,
          class StaticPlatformReleases>
struct BuildStaticPlatformSwiftRelease<
         BuildStaticPlatformSwiftRelease_Arg<Platform>,
         StaticSwiftRelease<SwiftVersion, StaticPlatformReleases>>
  : packs::PackFlatMap<
      BuildStaticPlatformSwiftReleaseHelper,
      BuildStaticPlatformSwiftReleaseHelper_Arg<Platform, SwiftVersion>,
      StaticPlatformReleases> {};

// Terminal case (see above).
template <PlatformKind Platform>
struct BuildStaticPlatformSwiftRelease<
         BuildStaticPlatformSwiftRelease_Arg<Platform>,
         Terminal> {
  using result = packs::Pack<>;
};

/// A template for generating a PlatformSwiftRelease array element
/// from a StaticPlatformSwiftRelease type value.
template <class>
struct BuildPlatformSwiftRelease;

template <class SwiftVersion, class PlatformVersion>
struct BuildPlatformSwiftRelease<
         StaticPlatformSwiftRelease<SwiftVersion, PlatformVersion>
       > {
  static constexpr PlatformSwiftRelease get() {
    return { BuildVersionTuple<SwiftVersion>::get(),
             BuildVersionTuple<PlatformVersion>::get() };
  }
};

/// A template for comparing two StaticPlatformSwiftRelease type values,
/// for the purposes of a well-ordered assertion we want to make:
/// We don't call this GT because it's not really a general-purpose
/// comparison.
template <class, class>
struct StaticPlatformSwiftReleaseStrictlyDescend;

template <class FirstSwift, class FirstPlatform,
          class SecondSwift, class SecondPlatform>
struct StaticPlatformSwiftReleaseStrictlyDescend<
    StaticPlatformSwiftRelease<FirstSwift, FirstPlatform>,
    StaticPlatformSwiftRelease<SecondSwift, SecondPlatform>
  > {
  static constexpr bool value =
    StaticVersionGT<FirstSwift, SecondSwift>::value &&
    StaticVersionGT<FirstPlatform, SecondPlatform>::value;
};

/// A helper template for BuildPlatformSwiftReleaseArray, below.
template <class P>
struct BuildPlatformSwiftReleaseArrayHelper;

template <class... StaticPlatformSwiftReleases>
struct BuildPlatformSwiftReleaseArrayHelper<
         packs::Pack<StaticPlatformSwiftReleases...>
       > {
  // After we reverse the entries, we expect them to strictly
  // descend in both the Swift version and the platform version.
  static_assert(packs::PackComponentsAreOrdered<
                  StaticPlatformSwiftReleaseStrictlyDescend,
                  StaticPlatformSwiftReleases...
                >::value,
                "RuntimeVersions.def is not properly ordered?");
  static constexpr PlatformSwiftRelease releases[] = {
    BuildPlatformSwiftRelease<StaticPlatformSwiftReleases>::get()...
  };
};

/// Build a static constexpr array of PlatformRelease objects matching
/// the given platform.
template <PlatformKind Platform>
struct BuildPlatformSwiftReleaseArray
  : BuildPlatformSwiftReleaseArrayHelper<
      // Turn each entry in AllStaticSwiftReleases into an optional
      // StaticPlatformSwiftRelease representing whether there is a
      // platform release providing that Swift release for the given
      // platform. Flatten that pack, then reverse it so that it's in
      // order of descending release versions. Finally, build an array
      // of PlatformRelease objects for the remaining values.
      typename packs::PackReverse<
        typename packs::PackFlatMap<
          BuildStaticPlatformSwiftRelease,
          BuildStaticPlatformSwiftRelease_Arg<Platform>,
          AllStaticSwiftReleases
        >::result
      >::result
    > {};

} // end anonymous namespace

static std::optional<llvm::VersionTuple>
findSwiftRuntimeVersionHelper(llvm::VersionTuple targetPlatformVersion,
                              llvm::VersionTuple minimumSwiftVersion,
                              ArrayRef<PlatformSwiftRelease> allReleases) {
  #define MAX(a, b) ((a) > (b) ? (a) : (b))

  // Scan forward in our filtered platform release array for the given
  // platform.
  for (auto &release : allReleases) {
    // If the provider version is <= the deployment target, then
    // the deployment target includes support for the given Swift
    // release. Since we're scanning in reverse order of Swift
    // releases (because of the order of entries in RuntimeVersions.def),
    // this must be the highest supported Swift release.
    if (release.platformVersion <= targetPlatformVersion) {
      return std::max(release.swiftVersion, minimumSwiftVersion);
    }
  }

  // If we didn't find anything, but the target release is at least the
  // notional future-release version, return that we aren't
  // deployment-limited.
  if (targetPlatformVersion >= llvm::VersionTuple(99, 99))
    return std::nullopt;

  // Otherwise, return the minimum Swift version.
  return minimumSwiftVersion;
}

/// Return the highest Swift release that matches the given platform and
/// has a version no greater than the target version. Don't return a version
/// older that the minimum. Returns null if the target version matches the
/// notional future release version.
template <PlatformKind TargetPlatform>
static std::optional<llvm::VersionTuple>
findSwiftRuntimeVersion(llvm::VersionTuple targetPlatformVersion,
                        llvm::VersionTuple minimumSwiftVersion) {
  auto &allReleases =
    BuildPlatformSwiftReleaseArray<TargetPlatform>::releases;

  return findSwiftRuntimeVersionHelper(targetPlatformVersion,
                                       minimumSwiftVersion,
                                       allReleases);
}

std::optional<llvm::VersionTuple>
swift::getSwiftRuntimeCompatibilityVersionForTarget(
    const llvm::Triple &Triple) {

  if (Triple.isMacOSX()) {
    llvm::VersionTuple OSVersion;
    Triple.getMacOSXVersion(OSVersion);

    // macOS releases predate the stable ABI, so use Swift 5.0 as our base.
    auto baseRelease = llvm::VersionTuple(5, 0);

    // macOS got its first arm64(e) support in 11.0, which included Swift 5.3.
    if (Triple.isAArch64())
      baseRelease = llvm::VersionTuple(5, 3);

    return findSwiftRuntimeVersion<PlatformKind::macOS>(OSVersion, baseRelease);

  } else if (Triple.isiOS()) { // includes tvOS
    llvm::VersionTuple OSVersion = Triple.getiOSVersion();

    // iOS releases predate the stable ABI, so use Swift 5.0 as our base.
    auto baseRelease = llvm::VersionTuple(5, 0);

    // arm64 simulators and macCatalyst were introduced in iOS 14.0/tvOS 14.0,
    // which included Swift 5.3.
    if (Triple.isAArch64() &&
        (Triple.isSimulatorEnvironment() ||
         Triple.isMacCatalystEnvironment()))
      baseRelease = llvm::VersionTuple(5, 3);

    // iOS first got arm64e support in 12.0, which did not yet support the
    // Swift stable ABI, so it does not provide a useful version bump.

    return findSwiftRuntimeVersion<PlatformKind::iOS>(OSVersion, baseRelease);

  } else if (Triple.isWatchOS()) {
    llvm::VersionTuple OSVersion = Triple.getWatchOSVersion();

    // watchOS releases predate the stable ABI, so use Swift 5.0 as our base.
    auto baseRelease = llvm::VersionTuple(5, 0);

    // 64-bit watchOS was first supported by watchOS 7, which provided
    // Swift 5.3.
    if (Triple.isArch64Bit())
      baseRelease = llvm::VersionTuple(5, 3);

    return findSwiftRuntimeVersion<PlatformKind::watchOS>(OSVersion, baseRelease);

  } else if (Triple.isXROS()) {
    llvm::VersionTuple OSVersion = Triple.getOSVersion();

    // visionOS 1.0 provided Swift 5.9.
    auto baseRelease = llvm::VersionTuple(5, 9);

    return findSwiftRuntimeVersion<PlatformKind::visionOS>(OSVersion, baseRelease);
  }

  return std::nullopt;
}

static const llvm::VersionTuple minimumMacCatalystDeploymentTarget() {
  return llvm::VersionTuple(13, 1);
}

llvm::VersionTuple swift::getTargetSDKVersion(clang::DarwinSDKInfo &SDKInfo,
                                              const llvm::Triple &triple) {
  // Retrieve the SDK version.
  auto SDKVersion = SDKInfo.getVersion();

  // For the Mac Catalyst environment, we have a macOS SDK with a macOS
  // SDK version. Map that to the corresponding iOS version number to pass
  // down to the linker.
  if (tripleIsMacCatalystEnvironment(triple)) {
    if (const auto *MacOStoMacCatalystMapping = SDKInfo.getVersionMapping(
            clang::DarwinSDKInfo::OSEnvPair::macOStoMacCatalystPair())) {
      return MacOStoMacCatalystMapping
          ->map(SDKVersion, minimumMacCatalystDeploymentTarget(), std::nullopt)
          .value_or(llvm::VersionTuple(0, 0, 0));
    }
    return llvm::VersionTuple(0, 0, 0);
  }

  return SDKVersion;
}

std::optional<llvm::Triple>
swift::getCanonicalTriple(const llvm::Triple &triple) {
  llvm::Triple Result = triple;
  // Non-darwin targets do not require canonicalization.
  if (!triple.isOSDarwin())
    return Result;

  // If the OS versions stay the same, return back the same triple.
  const llvm::VersionTuple inputOSVersion = triple.getOSVersion();
  const bool isOSVersionInValidRange =
      llvm::Triple::isValidVersionForOS(triple.getOS(), inputOSVersion);
  const llvm::VersionTuple canonicalVersion =
      llvm::Triple::getCanonicalVersionForOS(
          triple.getOS(), triple.getOSVersion(), isOSVersionInValidRange);
  if (canonicalVersion == triple.getOSVersion())
    return Result;

  const std::string inputOSName = triple.getOSName().str();
  const std::string inputOSVersionAsStr = inputOSVersion.getAsString();
  const int platformNameLength =
      inputOSName.size() - inputOSVersionAsStr.size();
  if (!StringRef(inputOSName).ends_with(inputOSVersionAsStr) ||
      (platformNameLength <= 0))
    return std::nullopt;

  llvm::SmallString<64> buffer(inputOSName.substr(0, platformNameLength));
  buffer.append(canonicalVersion.getAsString());
  Result.setOSName(buffer.str());
  return Result;
}

static std::string getPlistEntry(const llvm::Twine &Path, StringRef KeyName) {
  auto BufOrErr = llvm::MemoryBuffer::getFile(Path);
  if (!BufOrErr) {
    // FIXME: diagnose properly
    return {};
  }

  std::string Key = "<key>";
  Key += KeyName;
  Key += "</key>";

  StringRef Lines = BufOrErr.get()->getBuffer();
  while (!Lines.empty()) {
    StringRef CurLine;
    std::tie(CurLine, Lines) = Lines.split('\n');
    if (CurLine.find(Key) != StringRef::npos) {
      std::tie(CurLine, Lines) = Lines.split('\n');
      unsigned Begin = CurLine.find("<string>") + strlen("<string>");
      unsigned End = CurLine.find("</string>");
      return CurLine.substr(Begin, End - Begin).str();
    }
  }

  return {};
}

std::string swift::getSDKBuildVersionFromPlist(StringRef Path) {
  return getPlistEntry(Path, "ProductBuildVersion");
}

std::string swift::getSDKBuildVersion(StringRef Path) {
  return getSDKBuildVersionFromPlist((llvm::Twine(Path) +
    "/System/Library/CoreServices/SystemVersion.plist").str());
}

std::string swift::getSDKName(StringRef Path) {
  std::string Name = getPlistEntry(llvm::Twine(Path)+"/SDKSettings.plist",
                                   "CanonicalName");
  if (Name.empty() && Path.ends_with(".sdk")) {
    Name = llvm::sys::path::filename(Path).drop_back(strlen(".sdk")).str();
  }
  return Name;
}
