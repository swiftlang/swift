//===-- FunctionNameDemangle.cpp - Public demangling interface-------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Functions in the libfunctionNameDemangle library, which provides external
// access to Swift's internal demangling functionality.
//
//===----------------------------------------------------------------------===//

#include <swift/Basic/Demangle.h>
#include <swift/FunctionNameDemangle/FunctionNameDemangle.h>

/// \returns true if \p MangledName starts with Swift prefix, "_T".
static bool isSwiftPrefixed(const char *MangledName) {
  return (MangledName[0] == '_' && MangledName[1] == 'T');
}

size_t fnd_get_demangled_name(const char *MangledName, char *OutputBuffer,
                              size_t Length) {
  assert(MangledName != nullptr && "null input");
  assert(OutputBuffer != nullptr || Length == 0);

  if (!isSwiftPrefixed(MangledName))
    return 0; // Not a mangled name

  swift::Demangle::DemangleOptions DemangleOptions;
  DemangleOptions.SynthesizeSugarOnTypes = true;

  std::string Result =
      swift::Demangle::demangleSymbolAsString(MangledName, DemangleOptions);

  if (Result == MangledName)
    return 0; // Not a mangled name

  // Copy the result to an output buffer.
  return strlcpy(OutputBuffer, Result.c_str(), Length);
}

