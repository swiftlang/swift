//===--- Version.cpp - Swift Version Number -------------------------------===//
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

#include "LLVMRevision.inc"
#include "ClangRevision.inc"
#include "SwiftRevision.inc"

namespace swift {
namespace version {

/// Print a string of the form "LLVM xxxxx, Clang yyyyy, Swift zzzzz",
/// where each placeholder is the revision for the associated repository.
static void printFullRevisionString(raw_ostream &out) {
  // Arbitrarily truncate to 10 characters. This should be enough to unique
  // Git hashes for the time being, and certainly enough for SVN revisions,
  // while keeping the version string from being ridiculously long.
#if defined(LLVM_REVISION)
  out << "LLVM " << StringRef(LLVM_REVISION).slice(0, 10);
# if defined(CLANG_REVISION) || defined(SWIFT_REVISION)
  out << ", ";
# endif
#endif

#if defined(CLANG_REVISION)
  out << "Clang " << StringRef(CLANG_REVISION).slice(0, 10);
# if defined(SWIFT_REVISION)
  out << ", ";
# endif
#endif

#if defined(SWIFT_REVISION)
  out << "Swift " << StringRef(SWIFT_REVISION).slice(0, 10);
#endif
}

void splitVersionComponents(
  SmallVectorImpl<std::pair<StringRef, SourceRange>> &SplitComponents,
                            StringRef &VersionString, SourceLoc Loc,
                            DiagnosticEngine *Diags, bool skipQuote = false) {
  SourceLoc Start = (Loc.isValid() && skipQuote) ? Loc.getAdvancedLoc(1) : Loc;
  SourceLoc End = Start;

  // Split the version string into tokens separated by the '.' character.
  while (!VersionString.empty()) {
    StringRef SplitComponent, Rest;
    std::tie(SplitComponent, Rest) = VersionString.split('.');

    if (Loc.isValid()) {
      End = End.getAdvancedLoc(SplitComponent.size());
    }
    auto Range = Loc.isValid() ? SourceRange(Start, End) : SourceRange();
    if (Loc.isValid())
      End = End.getAdvancedLoc(1);
    Start = End;
    SplitComponents.push_back({SplitComponent, Range});
    VersionString = Rest;
  }
}

Version Version::parseCompilerVersionString(
  StringRef VersionString, SourceLoc Loc, DiagnosticEngine *Diags) {

  Version CV;
  SmallString<16> digits;
  llvm::raw_svector_ostream OS(digits);
  SmallVector<std::pair<StringRef, SourceRange>, 5> SplitComponents;

  auto checkVersionComponent = [&](unsigned Component, SourceRange Range) {
    unsigned limit = CV.Components.size() == 0 ? 9223371 : 999;

    if (Component > limit) {
      if (Diags)
        Diags->diagnose(Range.Start,
                        diag::compiler_version_component_out_of_range, limit);
      else
        llvm_unreachable("Compiler version component out of range");
    }
  };

  splitVersionComponents(SplitComponents, VersionString, Loc, Diags,
                         /*skipQuote=*/true);

  uint64_t ComponentNumber;

  for (size_t i = 0; i < SplitComponents.size(); ++i) {
    StringRef SplitComponent;
    SourceRange Range;
    std::tie(SplitComponent, Range) = SplitComponents[i];

    // Version components can't be empty.
    if (SplitComponent.empty()) {
      if (Diags)
        Diags->diagnose(Range.Start, diag::empty_version_component);
      else
        llvm_unreachable("Found empty compiler version component");
      continue;
    }

    // The second version component isn't used for comparison.
    if (i == 1) {
      if (!SplitComponent.equals("*")) {
        if (Diags) {
          Diags->diagnose(Range.Start, diag::unused_compiler_version_component)
          .fixItReplaceChars(Range.Start, Range.End, "*");
        } else {
          llvm_unreachable("Expected * for second compiler version component");
        }
      }

      CV.Components.push_back(0);
      continue;
    }

    // All other version components must be numbers.
    if (!SplitComponent.getAsInteger(10, ComponentNumber)) {
      checkVersionComponent(ComponentNumber, Range);
      CV.Components.push_back(ComponentNumber);
      continue;
    } else if (Diags) {
      Diags->diagnose(Range.Start,
                      diag::version_component_not_number);
    } else {
      llvm_unreachable("Invalid character in _compiler_version condition");
    }
  }

  if (CV.Components.size() > 5) {
    if (Diags) {
      Diags->diagnose(Loc, diag::compiler_version_too_many_components);
    } else {
      llvm_unreachable("Compiler version must not have more than 5 components");
    }
  }

  return CV;
}

Optional<Version> Version::parseVersionString(StringRef VersionString,
                                              SourceLoc Loc,
                                              DiagnosticEngine *Diags) {
  Version TheVersion;
  SmallString<16> digits;
  llvm::raw_svector_ostream OS(digits);
  SmallVector<std::pair<StringRef, SourceRange>, 5> SplitComponents;
  // Skip over quote character in string literal.

  if (VersionString.empty()) {
    if (Diags)
      Diags->diagnose(Loc, diag::empty_version_string);
    return None;
  }

  splitVersionComponents(SplitComponents, VersionString, Loc, Diags);

  uint64_t ComponentNumber;
  bool isValidVersion = true;

  for (size_t i = 0; i < SplitComponents.size(); ++i) {
    StringRef SplitComponent;
    SourceRange Range;
    std::tie(SplitComponent, Range) = SplitComponents[i];

    // Version components can't be empty.
    if (SplitComponent.empty()) {
      if (Diags)
        Diags->diagnose(Range.Start, diag::empty_version_component);

      isValidVersion = false;
      continue;
    }

    // All other version components must be numbers.
    if (!SplitComponent.getAsInteger(10, ComponentNumber)) {
      TheVersion.Components.push_back(ComponentNumber);
      continue;
    } else {
      if (Diags)
        Diags->diagnose(Range.Start,
                        diag::version_component_not_number);
      isValidVersion = false;
    }
  }

  return isValidVersion ? Optional<Version>(TheVersion) : None;
}


Version Version::getCurrentCompilerVersion() {
#ifdef SWIFT_COMPILER_VERSION
  auto currentVersion = Version::parseVersionString(
    SWIFT_COMPILER_VERSION, SourceLoc(), nullptr);
  assert(currentVersion.hasValue() &&
         "Embedded Swift language version couldn't be parsed: '"
         SWIFT_COMPILER_VERSION
         "'");
  return currentVersion.getValue();
#else
  return Version();
#endif
}

Version Version::getCurrentLanguageVersion() {
#ifndef SWIFT_VERSION_STRING
#error Swift language version is not set!
#endif
  auto currentVersion = Version::parseVersionString(
    SWIFT_VERSION_STRING, SourceLoc(), nullptr);
  assert(currentVersion.hasValue() &&
         "Embedded Swift language version couldn't be parsed: '"
         SWIFT_VERSION_STRING
         "'");
  return currentVersion.getValue();
}

raw_ostream &operator<<(raw_ostream &os, const Version &version) {
  if (version.empty())
    return os;
  os << version[0];
  for (size_t i = 1, e = version.size(); i != e; ++i)
    os << '.' << version[i];
  return os;
}

std::string
Version::preprocessorDefinition(StringRef macroName,
                                ArrayRef<uint64_t> componentWeights) const {
  uint64_t versionConstant = 0;

  for (size_t i = 0, e = std::min(componentWeights.size(), Components.size());
       i < e; ++i) {
    versionConstant += componentWeights[i] * Components[i];
  }

  std::string define("-D");
  llvm::raw_string_ostream(define) << macroName << '=' << versionConstant;
  // This isn't using stream.str() so that we get move semantics.
  return define;
}

Version::operator clang::VersionTuple() const
{
  switch (Components.size()) {
 case 0:
   return clang::VersionTuple();
 case 1:
   return clang::VersionTuple((unsigned)Components[0]);
 case 2:
   return clang::VersionTuple((unsigned)Components[0],
                              (unsigned)Components[1]);
 case 3:
   return clang::VersionTuple((unsigned)Components[0],
                              (unsigned)Components[1],
                              (unsigned)Components[2]);
 case 4:
 case 5:
   return clang::VersionTuple((unsigned)Components[0],
                              (unsigned)Components[1],
                              (unsigned)Components[2],
                              (unsigned)Components[3]);
 default:
   llvm_unreachable("swift::version::Version with 6 or more components");
  }
}

bool Version::isValidEffectiveLanguageVersion() const {
  for (auto verStr : getValidEffectiveVersions()) {
    auto v = parseVersionString(verStr, SourceLoc(), nullptr);
    assert(v.hasValue());
    // In this case, use logical-equality _and_ precision-equality. We do not
    // want to permit users requesting effective language versions more precise
    // than our whitelist (eg. we permit 3 but not 3.0 or 3.0.0), since
    // accepting such an argument promises more than we're able to deliver.
    if (v == *this && v.getValue().size() == size())
      return true;
  }
  return false;
}

Version Version::asMajorVersion() const {
  if (empty())
    return {};
  Version res;
  res.Components.push_back(Components[0]);
  return res;
}

bool operator>=(const class Version &lhs,
                const class Version &rhs) {

  // The empty compiler version represents the latest possible version,
  // usually built from the source repository.
  if (lhs.empty())
    return true;

  auto n = std::max(lhs.size(), rhs.size());

  for (size_t i = 0; i < n; ++i) {
    auto lv = i < lhs.size() ? lhs[i] : 0;
    auto rv = i < rhs.size() ? rhs[i] : 0;
    if (lv < rv)
      return false;
    else if (lv > rv)
      return true;
  }
  // Equality
  return true;
}

bool operator==(const class Version &lhs,
                const class Version &rhs) {
  auto n = std::max(lhs.size(), rhs.size());
  for (size_t i = 0; i < n; ++i) {
    auto lv = i < lhs.size() ? lhs[i] : 0;
    auto rv = i < rhs.size() ? rhs[i] : 0;
    if (lv != rv)
      return false;
  }
  return true;
}

std::pair<unsigned, unsigned> getSwiftNumericVersion() {
  return { SWIFT_VERSION_MAJOR, SWIFT_VERSION_MINOR };
}

std::string getSwiftFullVersion(Version effectiveVersion) {
  std::string buf;
  llvm::raw_string_ostream OS(buf);

#ifdef SWIFT_VENDOR
  OS << SWIFT_VENDOR " ";
#endif

  OS << "Swift version " SWIFT_VERSION_STRING;
#ifndef SWIFT_COMPILER_VERSION
  OS << "-dev";
#endif

  if (!(effectiveVersion == Version::getCurrentLanguageVersion())) {
    OS << " effective-" << effectiveVersion;
  }

#if defined(SWIFT_COMPILER_VERSION)
  OS << " (swiftlang-" SWIFT_COMPILER_VERSION;
#if defined(CLANG_COMPILER_VERSION)
  OS << " clang-" CLANG_COMPILER_VERSION;
#endif
  OS << ")";
#elif defined(LLVM_REVISION) || defined(CLANG_REVISION) || \
      defined(SWIFT_REVISION)
  OS << " (";
  printFullRevisionString(OS);
  OS << ")";
#endif

  // Suppress unused function warning
  (void)&printFullRevisionString;

  return OS.str();
}

std::string getSwiftRevision() {
#ifdef SWIFT_REVISION
  return SWIFT_REVISION;
#else
  return "";
#endif
}

} // end namespace version
} // end namespace swift

