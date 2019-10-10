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

#include "swift/Basic/Platform.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/VersionTuple.h"

using namespace swift;

bool swift::tripleIsiOSSimulator(const llvm::Triple &triple) {
  llvm::Triple::ArchType arch = triple.getArch();
  return (triple.isiOS() &&
          // FIXME: transitional, this should eventually stop testing arch, and
          // switch to only checking the -environment field.
          (triple.isSimulatorEnvironment() ||
           arch == llvm::Triple::x86 || arch == llvm::Triple::x86_64));
}

bool swift::tripleIsAppleTVSimulator(const llvm::Triple &triple) {
  llvm::Triple::ArchType arch = triple.getArch();
  return (triple.isTvOS() &&
          // FIXME: transitional, this should eventually stop testing arch, and
          // switch to only checking the -environment field.
          (triple.isSimulatorEnvironment() ||
           arch == llvm::Triple::x86 || arch == llvm::Triple::x86_64));
}

bool swift::tripleIsWatchSimulator(const llvm::Triple &triple) {
  llvm::Triple::ArchType arch = triple.getArch();
  return (triple.isWatchOS() &&
          // FIXME: transitional, this should eventually stop testing arch, and
          // switch to only checking the -environment field.
          (triple.isSimulatorEnvironment() ||
           arch == llvm::Triple::x86 || arch == llvm::Triple::x86_64));
}

bool swift::tripleIsAnySimulator(const llvm::Triple &triple) {
  // FIXME: transitional, this should eventually just use the -environment
  // field.
  return triple.isSimulatorEnvironment() ||
    tripleIsiOSSimulator(triple) ||
    tripleIsWatchSimulator(triple) ||
    tripleIsAppleTVSimulator(triple);
}


bool swift::tripleRequiresRPathForSwiftInOS(const llvm::Triple &triple) {
  if (triple.isMacOSX()) {
    // macOS 10.14.4 contains a copy of Swift, but the linker will still use an
    // rpath-based install name until 10.15.
    return triple.isMacOSXVersionLT(10, 15);

  } else if (triple.isiOS()) {
    return triple.isOSVersionLT(12, 2);

  } else if (triple.isWatchOS()) {
    return triple.isOSVersionLT(5, 2);
  }

  // Other platforms don't have Swift installed as part of the OS by default.
  return false;
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

  llvm_unreachable("Unsupported Darwin platform");
}

DarwinPlatformKind swift::getNonSimulatorPlatform(DarwinPlatformKind platform) {
  switch (platform) {
  case DarwinPlatformKind::MacOS:
    return DarwinPlatformKind::MacOS;
  case DarwinPlatformKind::IPhoneOS:
  case DarwinPlatformKind::IPhoneOSSimulator:
    return DarwinPlatformKind::IPhoneOS;
  case DarwinPlatformKind::TvOS:
  case DarwinPlatformKind::TvOSSimulator:
    return DarwinPlatformKind::TvOS;
  case DarwinPlatformKind::WatchOS:
  case DarwinPlatformKind::WatchOSSimulator:
    return DarwinPlatformKind::WatchOS;
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
  }
  llvm_unreachable("Unsupported Darwin platform");
}

StringRef swift::getPlatformNameForTriple(const llvm::Triple &triple) {
  switch (triple.getOS()) {
  case llvm::Triple::UnknownOS:
    llvm_unreachable("unknown OS");
  case llvm::Triple::Ananas:
  case llvm::Triple::CloudABI:
  case llvm::Triple::DragonFly:
  case llvm::Triple::Emscripten:
  case llvm::Triple::Fuchsia:
  case llvm::Triple::KFreeBSD:
  case llvm::Triple::Lv2:
  case llvm::Triple::NetBSD:
  case llvm::Triple::OpenBSD:
  case llvm::Triple::Solaris:
  case llvm::Triple::Minix:
  case llvm::Triple::RTEMS:
  case llvm::Triple::NaCl:
  case llvm::Triple::CNK:
  case llvm::Triple::AIX:
  case llvm::Triple::CUDA:
  case llvm::Triple::NVCL:
  case llvm::Triple::AMDHSA:
  case llvm::Triple::ELFIAMCU:
  case llvm::Triple::Mesa3D:
  case llvm::Triple::Contiki:
  case llvm::Triple::AMDPAL:
  case llvm::Triple::HermitCore:
  case llvm::Triple::Hurd:
  case llvm::Triple::WASI:
    return "";
  case llvm::Triple::Darwin:
  case llvm::Triple::MacOSX:
  case llvm::Triple::IOS:
  case llvm::Triple::TvOS:
  case llvm::Triple::WatchOS:
    return getPlatformNameForDarwin(getDarwinPlatformKind(triple));
  case llvm::Triple::Linux:
    return triple.isAndroid() ? "android" : "linux";
  case llvm::Triple::FreeBSD:
    return "freebsd";
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
      llvm_unreachable("unsupported Windows environment");
    }
  case llvm::Triple::PS4:
    return "ps4";
  case llvm::Triple::Haiku:
    return "haiku";
  }
  llvm_unreachable("unsupported OS");
}

StringRef swift::getMajorArchitectureName(const llvm::Triple &Triple) {
  if (Triple.isOSLinux()) {
    switch(Triple.getSubArch()) {
    default:
      return Triple.getArchName();
      break;
    case llvm::Triple::SubArchType::ARMSubArch_v7:
      return "armv7";
      break;
    case llvm::Triple::SubArchType::ARMSubArch_v6:
      return "armv6";
      break;
    }
  } else {
    return Triple.getArchName();
  }
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
// * llvm::Optional is similar to a Swift Optional: it either contains a value
//   or represents the absence of one. `None` is equivalent to `nil`; leading
//   `*` is equivalent to trailing `!`; conversion to `bool` is a not-`None`
//   check.

static StringRef
getArchForAppleTargetSpecificModuleTriple(const llvm::Triple &triple) {
  auto tripleArchName = triple.getArchName();

  return llvm::StringSwitch<StringRef>(tripleArchName)
              .Cases("arm64", "aarch64", "arm64")
              .Cases("x86_64", "amd64", "x86_64")
              .Cases("i386", "i486", "i586", "i686", "i786", "i886", "i986",
                     "i386")
              .Cases("unknown", "", "unknown")
  // These values are also supported, but are handled by the default case below:
  //          .Case ("armv7s", "armv7s")
  //          .Case ("armv7k", "armv7k")
  //          .Case ("armv7", "armv7")
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

static Optional<StringRef>
getEnvironmentForAppleTargetSpecificModuleTriple(const llvm::Triple &triple) {
  auto tripleEnvironment = triple.getEnvironmentName();

  // If the environment is empty, infer a "simulator" environment based on the
  // OS and architecture combination. This feature is deprecated and exists for
  // backwards compatibility only; build systems should pass the "simulator"
  // environment explicitly if they know they're building for a simulator.
  if (tripleEnvironment == "" && swift::tripleIsAnySimulator(triple))
    return StringRef("simulator");

  return llvm::StringSwitch<Optional<StringRef>>(tripleEnvironment)
              .Cases("unknown", "", None)
  // These values are also supported, but are handled by the default case below:
  //          .Case ("simulator", StringRef("simulator"))
              .Default(tripleEnvironment);
}

llvm::Triple swift::getTargetSpecificModuleTriple(const llvm::Triple &triple) {
  // isOSDarwin() returns true for all Darwin-style OSes, including macOS, iOS,
  // etc.
  if (triple.isOSDarwin()) {
    StringRef newArch = getArchForAppleTargetSpecificModuleTriple(triple);

    StringRef newVendor = getVendorForAppleTargetSpecificModuleTriple(triple);

    StringRef newOS = getOSForAppleTargetSpecificModuleTriple(triple);

    Optional<StringRef> newEnvironment =
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

  // Other platforms get no normalization.
  return triple;
}

Optional<llvm::VersionTuple>
swift::getSwiftRuntimeCompatibilityVersionForTarget(const llvm::Triple &Triple){
  unsigned Major, Minor, Micro;
  
  if (Triple.isMacOSX()) {
    Triple.getMacOSXVersion(Major, Minor, Micro);
    if (Major == 10) {
      if (Minor <= 14) {
        return llvm::VersionTuple(5, 0);
      } else if (Minor <= 15) {
        return llvm::VersionTuple(5, 1);
      } else {
        return None;
      }
    } else {
      return None;
    }
  } else if (Triple.isiOS()) { // includes tvOS
    Triple.getiOSVersion(Major, Minor, Micro);
    if (Major <= 12) {
      return llvm::VersionTuple(5, 0);
    } else if (Major <= 13) {
      return llvm::VersionTuple(5, 1);
    } else {
      return None;
    }
  } else if (Triple.isWatchOS()) {
    Triple.getWatchOSVersion(Major, Minor, Micro);
    if (Major <= 5) {
      return llvm::VersionTuple(5, 0);
    } else if (Major <= 6) {
      return llvm::VersionTuple(5, 1);
    } else {
      return None;
    }
  } else {
    return None;
  }
}

