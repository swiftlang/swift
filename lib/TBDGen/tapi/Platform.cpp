//===- tapi/Core/Platform.cpp - Platform ------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Implements platform specific helper functions.
///
//===----------------------------------------------------------------------===//

#include "Platform.h"
#include "LLVM.h"
#include "clang/Basic/Diagnostic.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

TAPI_NAMESPACE_INTERNAL_BEGIN

Platform mapToSim(Platform platform, bool wantSim) {
  switch (platform) {
  default:
    return platform;
  case Platform::iOS:
    return wantSim ? Platform::iOSSimulator : Platform::iOS;
  case Platform::tvOS:
    return wantSim ? Platform::tvOSSimulator : Platform::tvOS;
  case Platform::watchOS:
    return wantSim ? Platform::watchOSSimulator : Platform::watchOS;
  }
}

static Platform mapToPlatform(const Triple &target) {
  switch (target.getOS()) {
  default:
    return Platform::unknown;
  case Triple::MacOSX:
    return Platform::macOS;
  case Triple::IOS:
    if (target.isSimulatorEnvironment())
      return Platform::iOSSimulator;
    return Platform::iOS;
  case Triple::TvOS:
    return target.isSimulatorEnvironment() ? Platform::tvOSSimulator
                                           : Platform::tvOS;
  case Triple::WatchOS:
    return target.isSimulatorEnvironment() ? Platform::watchOSSimulator
                                           : Platform::watchOS;
    /*case Triple::BridgeOS:
      return Platform::bridgeOS;*/
  }
}

Platform mapToSinglePlatform(ArrayRef<Triple> targets) {
  auto result = Platform::unknown;
  for (const auto &target : targets) {
    auto p = mapToPlatform(target);
    if (p == Platform::unknown)
      return Platform::unknown;
    if (result == Platform::unknown) {
      result = p;
      continue;
    }
    if (result != p)
      return Platform::unknown;
  }
  return result;
}

StringRef getPlatformName(Platform platform) {
  switch (platform) {
  case Platform::unknown:
    return "unknown";
  case Platform::macOS:
    return "macOS";
  case Platform::iOS:
    return "iOS";
  case Platform::iOSSimulator:
    return "iOSSimulator";
  case Platform::watchOS:
    return "watchOS";
  case Platform::watchOSSimulator:
    return "watchOSSimulator";
  case Platform::tvOS:
    return "tvOS";
  case Platform::tvOSSimulator:
    return "tvOSSimulator";
  case Platform::bridgeOS:
    return "bridgeOS";
  }
  llvm_unreachable("unknown platform");
}

std::string getOSAndEnvironmentName(Platform platform, std::string version) {
  switch (platform) {
  case Platform::unknown:
    return "darwin" + version;
  case Platform::macOS:
    return "macos" + version;
  case Platform::iOS:
    return "ios" + version;
  case Platform::iOSSimulator:
    return "ios" + version + "-simulator";
  case Platform::watchOS:
    return "watchos" + version;
  case Platform::watchOSSimulator:
    return "watchos" + version + "-simulator";
  case Platform::tvOS:
    return "tvos" + version;
  case Platform::tvOSSimulator:
    return "tvos" + version + "-simulator";
  case Platform::bridgeOS:
    return "bridgeos" + version;
  }
  llvm_unreachable("unknown platform");
}

raw_ostream &operator<<(raw_ostream &os, Platform platform) {
  os << getPlatformName(platform);
  return os;
}

const DiagnosticBuilder &operator<<(const DiagnosticBuilder &db,
                                    Platform platform) {
  db.AddString(getPlatformName(platform));
  return db;
}

TAPI_NAMESPACE_INTERNAL_END
