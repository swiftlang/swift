//===--- Version.cpp - Swift Version Number -------------------------------===//
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
// This file defines several version-related utility functions for Swift.
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/CharInfo.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallString.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Version.h"

#include <vector>

#define TOSTR2(X) #X
#define TOSTR(X) TOSTR2(X)

#ifdef SWIFT_VERSION_PATCHLEVEL
/// Helper macro for SWIFT_VERSION_STRING.
#define SWIFT_MAKE_VERSION_STRING(X, Y, Z) TOSTR(X) "." TOSTR(Y) "." TOSTR(Z)

/// A string that describes the Swift version number, e.g., "1.0".
#define SWIFT_VERSION_STRING                                                   \
  SWIFT_MAKE_VERSION_STRING(SWIFT_VERSION_MAJOR, SWIFT_VERSION_MINOR,          \
                            SWIFT_VERSION_PATCHLEVEL)
#else
/// Helper macro for SWIFT_VERSION_STRING.
#define SWIFT_MAKE_VERSION_STRING(X, Y) TOSTR(X) "." TOSTR(Y)

/// A string that describes the Swift version number, e.g., "1.0".
#define SWIFT_VERSION_STRING                                                   \
  SWIFT_MAKE_VERSION_STRING(SWIFT_VERSION_MAJOR, SWIFT_VERSION_MINOR)
#endif

namespace swift {
namespace version {

void parseVersionString(StringRef VersionString,
                        SmallVectorImpl<unsigned> &Components,
                        SourceLoc Loc,
                        DiagnosticEngine *Diags) {
  SmallString<16> digits;
  llvm::raw_svector_ostream OS(digits);
  unsigned Component;
  for (auto c : VersionString) {
    if (Loc.isValid())
      Loc = Loc.getAdvancedLoc(1);
    if (clang::isDigit(c)) {
      OS << c;
    } else if (c == '.' && digits.str().size()) {
      OS.str().getAsInteger(10, Component);
      Components.push_back(Component);
      digits.clear();
    } else {
      Components.clear();
      if (Diags)
        Diags->diagnose(Loc, diag::invalid_character_in_compiler_version);
      else
        llvm_unreachable("Invalid character in _compiler_version build configuration");
    }
  }
  if (digits.str().size()) {
    OS.str().getAsInteger(10, Component);
    Components.push_back(Component);
  }
}

CompilerVersion::CompilerVersion(const StringRef VersionString,
                                 SourceLoc Loc, DiagnosticEngine *Diags) {
  parseVersionString(VersionString, Components, Loc, Diags);
}

#ifdef SWIFT_COMPILER_VERSION

CompilerVersion::CompilerVersion() {
  parseVersionString(TOSTR(SWIFT_COMPILER_VERSION), Components, SourceLoc(),
                     nullptr);
}
#endif

std::string CompilerVersion::str() const {
  std::string VersionString;
  llvm::raw_string_ostream OS(VersionString);
  for (auto i = Components.begin(); i != Components.end(); i++) {
    OS << *i;
    if (i != Components.end() - 1)
      OS << '.';
  }
  return OS.str();
}

bool operator>=(const class CompilerVersion &lhs,
                const class CompilerVersion &rhs) {

  // The empty compiler version represents the latest possible version,
  // usually built from the source repository.
  if (lhs.empty())
    return true;

  auto n = std::min(lhs.size(), rhs.size());

  for (size_t i = 0; i < n; ++i) {
    // Skip the second component for comparison - this is just an internal
    // compiler variant.
    if (i == 1)
      continue;

    if (lhs[i] < rhs[i])
      return false;
    else if (lhs[i] > rhs[i])
      return true;
  }
  return lhs.size() >= rhs.size();
}

std::pair<unsigned, unsigned> getSwiftNumericVersion() {
  return { SWIFT_VERSION_MAJOR, SWIFT_VERSION_MINOR };
}

std::string getSwiftFullVersion() {
  std::string buf;
  llvm::raw_string_ostream OS(buf);
#ifdef SWIFT_VENDOR
  OS << SWIFT_VENDOR " ";
#endif
  OS << "Swift version " SWIFT_VERSION_STRING;
#ifdef SWIFT_COMPILER_VERSION
  OS << " (" TOSTR(SWIFT_COMPILER_VERSION) ")";
#endif
  return OS.str();
}

} // end namespace version
} // end namespace swift

