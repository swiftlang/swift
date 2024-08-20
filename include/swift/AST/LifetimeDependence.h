//===--- LifetimeDependenceSpecifiers.h ------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines types and utilities related to Lifetime Dependence
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LIFETIMEDEPENDENCE_H
#define SWIFT_AST_LIFETIMEDEPENDENCE_H

#include "swift/AST/DeclContext.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/IndexSubset.h"
#include "swift/AST/Ownership.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/SourceLoc.h"

#include "llvm/ADT/ArrayRef.h"

namespace swift {

class AbstractFunctionDecl;
class FunctionTypeRepr;
class LifetimeDependentTypeRepr;
class SILParameterInfo;
class SILResultInfo;

enum class ParsedLifetimeDependenceKind : uint8_t {
  Default = 0,
  Scope,
  Inherit // Only used with deserialized decls
};

enum class LifetimeDependenceKind : uint8_t { Inherit = 0, Scope };

class LifetimeDependenceSpecifier {
public:
  enum class SpecifierKind { Named, Ordered, Self, Immortal };

private:
  SourceLoc loc;
  SpecifierKind specifierKind;
  ParsedLifetimeDependenceKind parsedLifetimeDependenceKind;
  union Value {
    struct {
      Identifier name;
    } Named;
    struct {
      unsigned index;
    } Ordered;
    struct {
    } self;
    Value(Identifier name) : Named({name}) {}
    Value(unsigned index) : Ordered({index}) {}
    Value() {}
  } value;

  LifetimeDependenceSpecifier(
      SourceLoc loc, SpecifierKind specifierKind,
      ParsedLifetimeDependenceKind parsedLifetimeDependenceKind, Value value)
      : loc(loc), specifierKind(specifierKind),
        parsedLifetimeDependenceKind(parsedLifetimeDependenceKind),
        value(value) {}

public:
  static LifetimeDependenceSpecifier getNamedLifetimeDependenceSpecifier(
      SourceLoc loc, ParsedLifetimeDependenceKind kind, Identifier name) {
    return {loc, SpecifierKind::Named, kind, name};
  }

  static LifetimeDependenceSpecifier
  getImmortalLifetimeDependenceSpecifier(SourceLoc loc) {
    return {loc, SpecifierKind::Immortal, {}, {}};
  }

  static LifetimeDependenceSpecifier getOrderedLifetimeDependenceSpecifier(
      SourceLoc loc, ParsedLifetimeDependenceKind kind, unsigned index) {
    return {loc, SpecifierKind::Ordered, kind, index};
  }

  static LifetimeDependenceSpecifier
  getSelfLifetimeDependenceSpecifier(SourceLoc loc,
                                     ParsedLifetimeDependenceKind kind) {
    return {loc, SpecifierKind::Self, kind, {}};
  }

  SourceLoc getLoc() const { return loc; }

  SpecifierKind getSpecifierKind() const { return specifierKind; }

  ParsedLifetimeDependenceKind getParsedLifetimeDependenceKind() const {
    return parsedLifetimeDependenceKind;
  }

  Identifier getName() const {
    assert(specifierKind == SpecifierKind::Named);
    return value.Named.name;
  }

  unsigned getIndex() const {
    assert(specifierKind == SpecifierKind::Ordered);
    return value.Ordered.index;
  }

  std::string getParamString() const {
    switch (specifierKind) {
    case SpecifierKind::Named:
      return value.Named.name.str().str();
    case SpecifierKind::Self:
      return "self";
    case SpecifierKind::Ordered:
      return std::to_string(value.Ordered.index);
    case SpecifierKind::Immortal:
      return "immortal";
    }
    llvm_unreachable("Invalid LifetimeDependenceSpecifier::SpecifierKind");
  }

  std::string getLifetimeDependenceSpecifierString() const {
    switch (parsedLifetimeDependenceKind) {
    case ParsedLifetimeDependenceKind::Default:
      return "dependsOn(" + getParamString() + ")";
    case ParsedLifetimeDependenceKind::Scope:
      return "dependsOn(scoped " + getParamString() + ")";
    case ParsedLifetimeDependenceKind::Inherit:
      return "dependsOn(inherited " + getParamString() + ")";
    }
    llvm_unreachable(
        "Invalid LifetimeDependenceSpecifier::ParsedLifetimeDependenceKind");
  }
};

class LifetimeDependenceInfo {
  IndexSubset *inheritLifetimeParamIndices;
  IndexSubset *scopeLifetimeParamIndices;
  unsigned targetIndex;
  bool immortal;

  static LifetimeDependenceInfo getForIndex(AbstractFunctionDecl *afd,
                                            unsigned targetIndex,
                                            unsigned sourceIndex,
                                            LifetimeDependenceKind kind);

  /// Builds LifetimeDependenceInfo on a result or parameter from a swift decl
  static std::optional<LifetimeDependenceInfo>
  fromTypeRepr(AbstractFunctionDecl *afd, LifetimeDependentTypeRepr *typeRepr,
               unsigned targetIndex);

  /// Infer LifetimeDependenceInfo on result
  static std::optional<LifetimeDependenceInfo> infer(AbstractFunctionDecl *afd);

  /// Infer LifetimeDependenceInfo on setter
  static std::optional<LifetimeDependenceInfo>
  inferSetter(AbstractFunctionDecl *afd);

  /// Infer LifetimeDependenceInfo on mutating self
  static std::optional<LifetimeDependenceInfo>
  inferMutatingSelf(AbstractFunctionDecl *afd);

  /// Builds LifetimeDependenceInfo from SIL function type
  static std::optional<LifetimeDependenceInfo>
  fromTypeRepr(LifetimeDependentTypeRepr *lifetimeDependentRepr,
               unsigned targetIndex, ArrayRef<SILParameterInfo> params,
               DeclContext *dc);

public:
  LifetimeDependenceInfo(IndexSubset *inheritLifetimeParamIndices,
                         IndexSubset *scopeLifetimeParamIndices,
                         unsigned targetIndex, bool isImmortal)
      : inheritLifetimeParamIndices(inheritLifetimeParamIndices),
        scopeLifetimeParamIndices(scopeLifetimeParamIndices),
        targetIndex(targetIndex), immortal(isImmortal) {
    assert(isImmortal || inheritLifetimeParamIndices ||
           scopeLifetimeParamIndices);
    assert(!inheritLifetimeParamIndices ||
           !inheritLifetimeParamIndices->isEmpty());
    assert(!scopeLifetimeParamIndices || !scopeLifetimeParamIndices->isEmpty());
  }

  operator bool() const { return !empty(); }

  bool empty() const {
    return !immortal && inheritLifetimeParamIndices == nullptr &&
           scopeLifetimeParamIndices == nullptr;
  }

  bool isImmortal() const { return immortal; }

  unsigned getTargetIndex() const { return targetIndex; }

  bool hasInheritLifetimeParamIndices() const {
    return inheritLifetimeParamIndices != nullptr;
  }
  bool hasScopeLifetimeParamIndices() const {
    return scopeLifetimeParamIndices != nullptr;
  }

  IndexSubset *getInheritIndices() const { return inheritLifetimeParamIndices; }

  IndexSubset *getScopeIndices() const { return scopeLifetimeParamIndices; }

  bool checkInherit(int index) const {
    return inheritLifetimeParamIndices
      && inheritLifetimeParamIndices->contains(index);
  }

  bool checkScope(int index) const {
    return scopeLifetimeParamIndices
      && scopeLifetimeParamIndices->contains(index);
  }

  std::string getString() const;
  void Profile(llvm::FoldingSetNodeID &ID) const;
  void getConcatenatedData(SmallVectorImpl<bool> &concatenatedData) const;

  /// Builds LifetimeDependenceInfo from a swift decl, either from the explicit
  /// lifetime dependence specifiers or by inference based on types and
  /// ownership modifiers.
  static std::optional<ArrayRef<LifetimeDependenceInfo>>
  get(AbstractFunctionDecl *decl);

  /// Builds LifetimeDependenceInfo from the bitvectors passes as parameters.
  static LifetimeDependenceInfo
  get(ASTContext &ctx, const SmallBitVector &inheritLifetimeIndices,
      const SmallBitVector &scopeLifetimeIndices);

  /// Builds LifetimeDependenceInfo from SIL
  static std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
  get(FunctionTypeRepr *funcRepr, ArrayRef<SILParameterInfo> params,
      ArrayRef<SILResultInfo> results, DeclContext *dc);

  bool operator==(const LifetimeDependenceInfo &other) const {
    return this->isImmortal() == other.isImmortal() &&
           this->getTargetIndex() == other.getTargetIndex() &&
           this->getInheritIndices() == other.getInheritIndices() &&
           this->getScopeIndices() == other.getScopeIndices();
  }

  bool operator!=(const LifetimeDependenceInfo &other) const {
    return this->isImmortal() != other.isImmortal() &&
           this->getTargetIndex() != other.getTargetIndex() &&
           this->getInheritIndices() != other.getInheritIndices() &&
           this->getScopeIndices() != other.getScopeIndices();
  }
};

std::optional<LifetimeDependenceInfo>
getLifetimeDependenceFor(ArrayRef<LifetimeDependenceInfo> lifetimeDependencies,
                         unsigned index);
} // namespace swift

#endif
