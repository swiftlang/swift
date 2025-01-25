//===--- LifetimeDependence.h ---------------------------------*- C++ -*-===//
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
#include "llvm/Support/TrailingObjects.h"

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

struct LifetimeDescriptor {
  union Value {
    struct {
      StringRef name;
    } Named;
    struct {
      unsigned index;
    } Ordered;
    struct {
    } Self;
    Value(StringRef name) : Named({name}) {}
    Value(unsigned index) : Ordered({index}) {}
    Value() : Self() {}
  } value;

  enum class DescriptorKind { Named, Ordered, Self } kind;

  ParsedLifetimeDependenceKind parsedLifetimeDependenceKind;

  SourceLoc loc;

private:
  LifetimeDescriptor(StringRef name,
                     ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
                     SourceLoc loc)
      : value{name}, kind(DescriptorKind::Named),
        parsedLifetimeDependenceKind(parsedLifetimeDependenceKind), loc(loc) {}
  LifetimeDescriptor(unsigned index,
                     ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
                     SourceLoc loc)
      : value{index}, kind(DescriptorKind::Ordered),
        parsedLifetimeDependenceKind(parsedLifetimeDependenceKind), loc(loc) {}
  LifetimeDescriptor(ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
                     SourceLoc loc)
      : value{}, kind(DescriptorKind::Self),
        parsedLifetimeDependenceKind(parsedLifetimeDependenceKind), loc(loc) {}

public:
  static LifetimeDescriptor
  forNamed(StringRef name,
           ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
           SourceLoc loc) {
    return {name, parsedLifetimeDependenceKind, loc};
  }
  static LifetimeDescriptor
  forOrdered(unsigned index,
             ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
             SourceLoc loc) {
    return {index, parsedLifetimeDependenceKind, loc};
  }
  static LifetimeDescriptor
  forSelf(ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
          SourceLoc loc) {
    return {parsedLifetimeDependenceKind, loc};
  }

  ParsedLifetimeDependenceKind getParsedLifetimeDependenceKind() const {
    return parsedLifetimeDependenceKind;
  }

  StringRef getName() const {
    assert(kind == DescriptorKind::Named);
    return value.Named.name;
  }

  unsigned getIndex() const {
    assert(kind == DescriptorKind::Ordered);
    return value.Ordered.index;
  }

  DescriptorKind getDescriptorKind() const { return kind; }

  SourceLoc getLoc() const { return loc; }

  bool isImmortal() const {
    if (getDescriptorKind() != LifetimeDescriptor::DescriptorKind::Named) {
      return false;
    }
    return getName() == "immortal";
  }

  std::string getString() const {
    switch (kind) {
    case DescriptorKind::Named:
      return getName().str();
    case DescriptorKind::Ordered:
      return std::to_string(getIndex());
    case DescriptorKind::Self:
      return "self";
    }
    llvm_unreachable("Invalid DescriptorKind");
  }
};

class LifetimeEntry final
    : private llvm::TrailingObjects<LifetimeEntry, LifetimeDescriptor> {
  friend TrailingObjects;

private:
  SourceLoc startLoc, endLoc;
  unsigned numSources;
  std::optional<LifetimeDescriptor> targetDescriptor;

  LifetimeEntry(
      SourceLoc startLoc, SourceLoc endLoc,
      ArrayRef<LifetimeDescriptor> sources,
      std::optional<LifetimeDescriptor> targetDescriptor = std::nullopt)
      : startLoc(startLoc), endLoc(endLoc), numSources(sources.size()),
        targetDescriptor(targetDescriptor) {
    std::uninitialized_copy(sources.begin(), sources.end(),
                            getTrailingObjects<LifetimeDescriptor>());
  }

  size_t numTrailingObjects(OverloadToken<LifetimeDescriptor>) const {
    return numSources;
  }

public:
  static LifetimeEntry *
  create(const ASTContext &ctx, SourceLoc startLoc, SourceLoc endLoc,
         ArrayRef<LifetimeDescriptor> sources,
         std::optional<LifetimeDescriptor> targetDescriptor = std::nullopt);

  SourceLoc getLoc() const { return startLoc; }
  SourceLoc getStartLoc() const { return startLoc; }
  SourceLoc getEndLoc() const { return endLoc; }

  ArrayRef<LifetimeDescriptor> getSources() const {
    return {getTrailingObjects<LifetimeDescriptor>(), numSources};
  }

  std::optional<LifetimeDescriptor> getTargetDescriptor() const {
    return targetDescriptor;
  }

  std::string getString() const {
    std::string result = "@lifetime(";
    if (targetDescriptor.has_value()) {
      result += targetDescriptor->getString();
      result += ": ";
    }

    bool firstElem = true;
    for (auto source : getSources()) {
      if (!firstElem) {
        result += ", ";
      }
      if (source.getParsedLifetimeDependenceKind() ==
          ParsedLifetimeDependenceKind::Scope) {
        result += "borrow ";
      }
      result += source.getString();
      firstElem = false;
    }
    result += ")";
    return result;
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

  /// Builds LifetimeDependenceInfo from @lifetime attribute
  static std::optional<ArrayRef<LifetimeDependenceInfo>>
  fromLifetimeAttribute(AbstractFunctionDecl *afd);

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
  fromDependsOn(LifetimeDependentTypeRepr *lifetimeDependentRepr,
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
