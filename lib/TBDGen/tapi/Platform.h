//===- tapi/Core/Platform.h - Platform --------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines the platform enum.
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_PLATFORM_H
#define TAPI_CORE_PLATFORM_H

#include "LLVM.h"
#include "Defines.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include <string>

TAPI_NAMESPACE_INTERNAL_BEGIN

enum class Platform : uint8_t {
  unknown,
  macOS,
  iOS,
  iOSSimulator,
  tvOS,
  tvOSSimulator,
  watchOS,
  watchOSSimulator,
  bridgeOS,
};

Platform mapToSim(Platform platform, bool wantSim);
Platform mapToSinglePlatform(ArrayRef<llvm::Triple> targets);
StringRef getPlatformName(Platform platform);
std::string getOSAndEnvironmentName(Platform platform,
                                    std::string version = "");

raw_ostream &operator<<(raw_ostream &os, Platform platform);

const DiagnosticBuilder &operator<<(const DiagnosticBuilder &db,
                                    Platform platform);

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_PLATFORM_H
