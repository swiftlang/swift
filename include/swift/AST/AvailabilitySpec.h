//===--- AvailabilitySpec.h - Swift Availability Spec ASTs ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the availability specification AST classes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AVAILABILITY_SPEC_H
#define SWIFT_AST_AVAILABILITY_SPEC_H

#include "swift/AST/ASTAllocated.h"
#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/PlatformKindUtils.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/VersionTuple.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
class ASTContext;
class SemanticAvailabilitySpec;

/// The root class for specifications of API availability in availability
/// queries.
class AvailabilitySpec : public ASTAllocated<AvailabilitySpec> {
  AvailabilityDomainOrIdentifier DomainOrIdentifier;

  /// The range of the entire spec, including the version if there is one.
  SourceRange SrcRange;

  /// The version (may be empty if there was no version specified).
  llvm::VersionTuple Version;

  /// If there is a version specified, this is its start location within the
  /// overall source range.
  SourceLoc VersionStartLoc;

  // Location of the availability macro expanded to create this spec.
  SourceLoc MacroLoc;

  unsigned IsInvalid : 1;

  AvailabilitySpec(AvailabilityDomainOrIdentifier DomainOrIdentifier,
                   SourceRange SrcRange, llvm::VersionTuple Version,
                   SourceLoc VersionStartLoc)
      : DomainOrIdentifier(DomainOrIdentifier), SrcRange(SrcRange),
        Version(Version), VersionStartLoc(VersionStartLoc), IsInvalid(false) {}

public:
  /// Creates a wildcard availability specification that guards execution
  /// by checking that the run-time version is greater than the minimum
  /// deployment target. This specification is designed to ease porting
  /// to new platforms. Because new platforms typically branch from
  /// existing platforms, the wildcard allows an #available() check to do the
  /// "right" thing (executing the guarded branch) on the new platform without
  /// requiring a modification to every availability guard in the program. Note
  /// that we still do compile-time availability checking with '*', so the
  /// compiler will still catch references to potentially unavailable symbols.
  static AvailabilitySpec *createWildcard(ASTContext &ctx, SourceLoc starLoc);

  /// Creates an availability specification that requires a minimum version of
  /// some availability domain, e.g., macOS >= 10.10 or swift >= 3.0.1.
  static AvailabilitySpec *
  createForDomain(ASTContext &ctx, AvailabilityDomain domain, SourceLoc loc,
                  llvm::VersionTuple version, SourceRange versionRange);

  /// Creates an availability specification that requires a minimum version of
  /// some availability domain which has not yet been resolved.
  static AvailabilitySpec *
  createForDomainIdentifier(ASTContext &ctx, Identifier domainIdentifier,
                            SourceLoc loc, llvm::VersionTuple version,
                            SourceRange versionRange);

  AvailabilitySpec *clone(ASTContext &ctx) const;

  /// Returns a type-checked representation of the spec, or `std::nullopt` if
  /// the spec is invalid.
  std::optional<SemanticAvailabilitySpec>
  getSemanticAvailabilitySpec(const DeclContext *declContext) const;

  SourceRange getSourceRange() const { return SrcRange; }
  SourceLoc getStartLoc() const { return SrcRange.Start; }

  /// Returns true if this spec did not type-check successfully.
  bool isInvalid() const { return IsInvalid; }

  AvailabilityDomainOrIdentifier getDomainOrIdentifier() const {
    return DomainOrIdentifier;
  }

  bool isWildcard() const {
    if (auto domain = getDomainOrIdentifier().getAsDomain())
      return domain->isUniversal();
    return false;
  }

  // The version tuple that was written in source.
  llvm::VersionTuple getRawVersion() const { return Version; }

  SourceRange getVersionSrcRange() const {
    if (!VersionStartLoc)
      return SourceRange();
    return SourceRange(VersionStartLoc, SrcRange.End);
  }

  // Location of the macro expanded to create this spec.
  SourceLoc getMacroLoc() const { return MacroLoc; }
  void setMacroLoc(SourceLoc loc) { MacroLoc = loc; }

  void print(llvm::raw_ostream &os) const;

private:
  friend class SemanticAvailabilitySpecRequest;

  std::optional<AvailabilityDomain>
  resolveInDeclContext(const DeclContext *declContext);

  void setInvalid() { IsInvalid = true; }
};

inline void simple_display(llvm::raw_ostream &os,
                           const AvailabilitySpec *spec) {
  spec->print(os);
}

/// The type-checked representation of `AvailabilitySpec` which guaranatees that
/// the spec has a valid `AvailabilityDomain`.
class SemanticAvailabilitySpec {
  const AvailabilitySpec *spec;

public:
  SemanticAvailabilitySpec(const AvailabilitySpec *spec) : spec(spec) {
    // The domain must be resolved in order to wrap it in a semantic spec.
    bool hasDomain = spec->getDomainOrIdentifier().isDomain();
    ASSERT(hasDomain);
  }

  const AvailabilitySpec *getParsedSpec() const { return spec; }

  AvailabilityDomain getDomain() const {
    return spec->getDomainOrIdentifier().getAsDomain().value();
  }

  bool isWildcard() const { return spec->isWildcard(); }

  // The platform version to compare against.
  llvm::VersionTuple getVersion() const;

  // The version to be used in codegen for version comparisons at run time.
  // This is required to support beta versions of macOS Big Sur that
  // report 10.16 at run time.
  llvm::VersionTuple getRuntimeVersion() const { return spec->getRawVersion(); }

  void print(llvm::raw_ostream &os) const { spec->print(os); }
};

/// Wraps an array of availability specs and provides an iterator for their
/// semantic representations.
class SemanticAvailabilitySpecs {
public:
  class Filter final {
    const DeclContext *declContext;

  public:
    Filter(const DeclContext *declContext) : declContext(declContext) {}

    std::optional<SemanticAvailabilitySpec>
    operator()(const AvailabilitySpec *spec) const;
  };

  using Range = OptionalTransformRange<
      iterator_range<ArrayRef<AvailabilitySpec *>::const_iterator>, Filter>;

private:
  Range specRange;

public:
  SemanticAvailabilitySpecs(ArrayRef<AvailabilitySpec *> specs,
                            const DeclContext *declContext)
      : specRange(llvm::make_range(specs.begin(), specs.end()),
                  Filter(declContext)) {}

  Range::iterator begin() const { return specRange.begin(); }
  Range::iterator end() const { return specRange.end(); }
  bool empty() const { return specRange.empty(); }
};

/// Maps of macro name and version to availability specifications.
/// Organized as two nested \c DenseMap keyed first on the macro name then
/// the macro version. This structure allows to peek at macro names before
/// parsing a version tuple.
class AvailabilityMacroMap {
public:
  typedef llvm::DenseMap<llvm::VersionTuple, SmallVector<AvailabilitySpec *, 4>>
      VersionEntry;
  llvm::StringMap<VersionEntry> Impl;

  bool hasMacroName(StringRef name) const {
    return Impl.find(name) != Impl.end();
  }

  bool hasMacroNameVersion(StringRef name, llvm::VersionTuple version) const {
    auto entry = Impl.find(name);
    if (entry == Impl.end()) {
      return false;
    }
    return entry->second.find(version) != entry->second.end();
  }

  void addEntry(StringRef name, llvm::VersionTuple version,
                ArrayRef<AvailabilitySpec *> specs) {
    assert(!hasMacroNameVersion(name, version));
    Impl[name][version].assign(specs.begin(), specs.end());
  }

  ArrayRef<AvailabilitySpec *> getEntry(StringRef name,
                                        llvm::VersionTuple version) const {
    auto versions = Impl.find(name);
    if (versions == Impl.end()) {
      return {};
    }
    auto entry = versions->second.find(version);
    if (entry == versions->second.end()) {
      return {};
    }
    return entry->second;
  }
};

} // end namespace swift

namespace llvm {

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const swift::AvailabilitySpec *spec) {
  spec->print(os);
  return os;
}

inline llvm::raw_ostream &
operator<<(llvm::raw_ostream &os, const swift::SemanticAvailabilitySpec &spec) {
  spec.print(os);
  return os;
}

} // end namespace llvm

#endif
