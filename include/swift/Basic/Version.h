//===--- Version.h - Swift Version Number -----------------------*- C++ -*-===//
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
#include "clang/Basic/VersionTuple.h"
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
  SmallVector<unsigned, 5> Components;
public:
  /// Create the empty compiler version - this always compares greater
  /// or equal to any other CompilerVersion, as in the case of building Swift
  /// from latest sources outside of a build/integration/release context.
  Version() = default;

  /// Create a literal version from a list of components.
  Version(std::initializer_list<unsigned> Values) : Components(Values) {}

  /// Create a version from a string in source code.
  ///
  /// Must include only groups of digits separated by a dot.
  Version(StringRef VersionString, SourceLoc Loc, DiagnosticEngine *Diags);

  /// Return a string to be used as an internal preprocessor define.
  ///
  /// The components of the version are multiplied element-wise by
  /// \p componentWeights, then added together (a dot product operation).
  /// If either array is longer than the other, the missing elements are
  /// treated as zero.
  ///
  /// The resulting string will have the form "-DMACRO_NAME=XYYZZ".
  /// The combined value must fit in a uint64_t.
  std::string preprocessorDefinition(StringRef macroName,
                                     ArrayRef<uint64_t> componentWeights) const;

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

  /// Convert to a (maximum-4-element) clang::VersionTuple, truncating
  /// away any 5th component that might be in this version.
  operator clang::VersionTuple() const;

  /// Returns the concrete version to use when \e this version is provided as
  /// an argument to -swift-version.
  ///
  /// This is not the same as the set of Swift versions that have ever existed,
  /// just those that we are attempting to maintain backward-compatibility
  /// support for. It's also common for valid versions to produce a different
  /// result; for example "-swift-version 3" at one point instructed the
  /// compiler to act as if it is version 3.1.
  Optional<Version> getEffectiveLanguageVersion() const;

  /// Whether this version is in the Swift 3 family
  bool isVersion3() const { return !empty() && Components[0] == 3; }

  /// Return this Version struct with minor and sub-minor components stripped
  Version asMajorVersion() const;

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

  // Whitelist of backward-compatibility versions that we permit passing as
  // -swift-version <vers>
  static std::array<StringRef, 2> getValidEffectiveVersions() {
    return {{"3", "4"}};
  };
};

bool operator>=(const Version &lhs, const Version &rhs);
bool operator==(const Version &lhs, const Version &rhs);

raw_ostream &operator<<(raw_ostream &os, const Version &version);

/// Retrieves the numeric {major, minor} Swift version.
///
/// Note that this is the underlying version of the language, ignoring any
/// -swift-version flags that may have been used in a particular invocation of
/// the compiler.
std::pair<unsigned, unsigned> getSwiftNumericVersion();

/// Retrieves a string representing the complete Swift version, which includes
/// the Swift supported and effective version numbers, the repository version,
/// and the vendor tag.
std::string getSwiftFullVersion(Version effectiveLanguageVersion =
                                Version::getCurrentLanguageVersion());

/// Retrieves the repository revision number (or identifier) from which
/// this Swift was built.
std::string getSwiftRevision();

} // end namespace version
} // end namespace swift

#endif // SWIFT_BASIC_VERSION_H
