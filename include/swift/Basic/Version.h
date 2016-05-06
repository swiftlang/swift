//===--- Version.h - Swift Version Number -----------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines version macros and version-related utility functions
/// for Swift.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_VERSION_H
#define SWIFT_BASIC_VERSION_H


#include "swift/Basic/LLVM.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include <string>

namespace swift {

class DiagnosticEngine;
class SourceLoc;

namespace version {

/// Represents an internal compiler version, represented as a tuple of
/// integers, or "version components".
///
/// For comparison, if a `CompilerVersion` contains more than one
/// version component, the second one is ignored for comparison,
/// as it represents a compiler variant with no defined ordering.
///
/// A CompilerVersion must have no more than five components and must fit in a
/// 64-bit unsigned integer representation.
///
/// Assuming a maximal version component layout of X.Y.Z.a.b,
/// X, Y, Z, a, b are integers with the following (inclusive) ranges:
/// X: [0 - 9223371]
/// Y: [0 - 999]
/// Z: [0 - 999]
/// a: [0 - 999]
/// b: [0 - 999]
class Version {
  SmallVector<uint64_t, 5> Components;
public:
  /// Create the empty compiler version - this always compares greater
  /// or equal to any other CompilerVersion, as in the case of building Swift
  /// from latest sources outside of a build/integration/release context.
  Version() = default;

  /// Create a version from a string in source code.
  ///
  /// Must include only groups of digits separated by a dot.
  Version(StringRef VersionString, SourceLoc Loc, DiagnosticEngine *Diags);

  /// Return a string to be used as an internal preprocessor define.
  ///
  /// Assuming the project version is at most X.Y.Z.a.b, the integral constant
  /// representing the version is:
  ///
  /// X*1000*1000*1000 + Z*1000*1000 + a*1000 + b
  ///
  /// The second version component is not used.
  std::string preprocessorDefinition() const;

  /// Return the ith version component.
  unsigned operator[](size_t i) const {
    return Components[i];
  }

  /// Return the number of version components.
  size_t size() const {
    return Components.size();
  }

  bool empty() const {
    return Components.empty();
  }

  /// Parse a version in the form used by the _compiler_version \#if condition.
  static Version parseCompilerVersionString(StringRef VersionString,
                                            SourceLoc Loc,
                                            DiagnosticEngine *Diags);

  /// Parse a generic version string of the format [0-9]+(.[0-9]+)*
  ///
  /// Version components can be any unsigned 64-bit number.
  static Optional<Version> parseVersionString(StringRef VersionString,
                                              SourceLoc Loc,
                                              DiagnosticEngine *Diags);

  /// Returns a version from the currently defined SWIFT_COMPILER_VERSION.
  ///
  /// If SWIFT_COMPILER_VERSION is undefined, this will return the empty
  /// compiler version.
  static Version getCurrentCompilerVersion();

  /// Returns a version from the currently defined SWIFT_VERSION_MAJOR and
  /// SWIFT_VERSION_MINOR.
  static Version getCurrentLanguageVersion();
};

bool operator>=(const Version &lhs, const Version &rhs);

raw_ostream &operator<<(raw_ostream &os, const Version &version);

/// Retrieves the numeric {major, minor} Swift version.
std::pair<unsigned, unsigned> getSwiftNumericVersion();

/// Retrieves a string representing the complete Swift version, which includes
/// the Swift version number, the repository version, and the vendor tag.
std::string getSwiftFullVersion();

} // end namespace version
} // end namespace swift

#endif // SWIFT_BASIC_VERSION_H
