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
#include "llvm/ADT/Triple.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/StringExtras.h"

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

static StringRef
getArchForAppleTargetSpecificModuleTriple(const llvm::Triple &triple) {
  auto tripleArchName = triple.getArchName();

  return llvm::StringSwitch<StringRef>(tripleArchName)
              .Cases("arm64", "aarch64", "arm64")
              .Case ("armv7s", "armv7s")
              .Case ("armv7k", "armv7k")
              .Case ("armv7", "armv7")
              .Case ("x86_64h", "x86_64h")
              .Cases("x86_64", "amd64", "x86_64")
              .Cases("i386", "i486", "i586", "i686", "i786", "i886", "i986",
                     "i386")
              .Cases("unknown", "", "unknown")
              .Default(tripleArchName);
}

static StringRef
getVendorForAppleTargetSpecificModuleTriple(const llvm::Triple &triple) {
  return "apple";
}

static StringRef
getOSForAppleTargetSpecificModuleTriple(const llvm::Triple &triple) {
  auto tripleOSName = triple.getOSName();

  // Truncate the OS name before the first digit.
  auto tripleOSNameNoVersion = tripleOSName.take_until(llvm::isDigit);

  return llvm::StringSwitch<StringRef>(tripleOSNameNoVersion)
              .Cases("macos", "macosx", "darwin", "macos")
              .Case ("ios", "ios")
              .Case ("tvos", "tvos")
              .Cases("unknown", "", "unknown")
              .Default(tripleOSNameNoVersion);
}

static Optional<StringRef>
getEnvironmentForAppleTargetSpecificModuleTriple(const llvm::Triple &triple) {
  auto tripleEnvironment = triple.getEnvironmentName();

  if (tripleEnvironment == "") {
    if (swift::tripleIsAnySimulator(triple))
      return StringRef("simulator");
    else
      return None;
  }

  return llvm::StringSwitch<Optional<StringRef>>(tripleEnvironment)
              .Case("simulator", StringRef("simulator"))
              .Case("unknown", None)
              .Default(tripleEnvironment);
}

llvm::Triple swift::getTargetSpecificModuleTriple(const llvm::Triple &triple) {
  if (triple.isOSDarwin()) {
    StringRef newArch = getArchForAppleTargetSpecificModuleTriple(triple);

    StringRef newVendor = getVendorForAppleTargetSpecificModuleTriple(triple);

    StringRef newOS = getOSForAppleTargetSpecificModuleTriple(triple);

    Optional<StringRef> newEnvironment =
        getEnvironmentForAppleTargetSpecificModuleTriple(triple);

    if (newEnvironment)
      return llvm::Triple(newArch, newVendor, newOS, *newEnvironment);
    else
      return llvm::Triple(newArch, newVendor, newOS);
  } else {
    return triple;
  }
}

