//===--- SwiftDemangle.cpp - Public demangling interface ------------------===//
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
//
// Functions in the libswiftDemangle library, which provides external
// access to Swift's demangler.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/DemangleWrappers.h"
#include "swift/Basic/ManglingMacros.h"
#include "swift/SwiftDemangle/SwiftDemangle.h"

/// \returns true if \p MangledName starts with Swift prefix, "_T".
static bool isSwiftPrefixed(const char *MangledName) {
  return MangledName[0] == '_' &&
         (MangledName[1] == 'T' || MangledName[1] == MANGLING_PREFIX_STR[1]);
}

static size_t swift_demangle_getDemangledName_Options(const char *MangledName,
    char *OutputBuffer, size_t Length,
    swift::Demangle::DemangleOptions DemangleOptions) {
  assert(MangledName != nullptr && "null input");
  assert(OutputBuffer != nullptr || Length == 0);

  if (!isSwiftPrefixed(MangledName))
    return 0; // Not a mangled name

  std::string Result = swift::demangle_wrappers::demangleSymbolAsString(
      MangledName, DemangleOptions);

  if (Result == MangledName)
    return 0; // Not a mangled name

  // Copy the result to an output buffer.
  return strlcpy(OutputBuffer, Result.c_str(), Length);
}

size_t swift_demangle_getDemangledName(const char *MangledName,
                                       char *OutputBuffer,
                                       size_t Length) {
  swift::Demangle::DemangleOptions DemangleOptions;
  DemangleOptions.SynthesizeSugarOnTypes = true;
  return swift_demangle_getDemangledName_Options(MangledName, OutputBuffer,
                                                 Length, DemangleOptions);
}

size_t swift_demangle_getSimplifiedDemangledName(const char *MangledName,
                                                 char *OutputBuffer,
                                                 size_t Length) {
  auto Opts = swift::Demangle::DemangleOptions::SimplifiedUIDemangleOptions();
  return swift_demangle_getDemangledName_Options(MangledName, OutputBuffer,
                                                 Length, Opts);
}

size_t fnd_get_demangled_name(const char *MangledName, char *OutputBuffer,
                              size_t Length) {
  return swift_demangle_getDemangledName(MangledName, OutputBuffer, Length);
}

