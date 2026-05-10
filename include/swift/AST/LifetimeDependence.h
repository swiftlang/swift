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
#include "swift/AST/Type.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/FlagSet.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/SourceLoc.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {

class AbstractFunctionDecl;
class AnyFunctionType;
class FunctionTypeRepr;
class LifetimeDependentTypeRepr;
class LifetimeTypeAttr;
class SILParameterInfo;
class SILResultInfo;

enum class ParsedLifetimeDependenceKind : uint8_t {
  Default = 0,
  Borrow,
  Inherit, // Only used with deserialized decls
  Inout
};

enum class LifetimeDependenceKind : uint8_t { Inherit = 0, Scope };

struct LifetimeDescriptor {
  enum IsAddressable_t {
    IsNotAddressable,
    IsConditionallyAddressable,
    IsAddressable,
  };

  union Value {
    struct {
      Identifier name;
    } Named;
    struct {
      unsigned index;
      IsAddressable_t isAddress;
    } Ordered;
    struct {
    } Self;
    Value(Identifier name) : Named({name}) {}
    Value(unsigned index, IsAddressable_t isAddress)
      : Ordered({index, isAddress}) {}
    Value() : Self() {}
  } value;

  enum class DescriptorKind { Named, Ordered, Self } kind;

  ParsedLifetimeDependenceKind parsedLifetimeDependenceKind;

  SourceLoc loc;

private:
  LifetimeDescriptor(Identifier name,
                     ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
                     SourceLoc loc)
      : value{name}, kind(DescriptorKind::Named),
        parsedLifetimeDependenceKind(parsedLifetimeDependenceKind), loc(loc) {}
  LifetimeDescriptor(unsigned index, IsAddressable_t isAddress,
                     ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
                     SourceLoc loc)
      : value{index, isAddress}, kind(DescriptorKind::Ordered),
        parsedLifetimeDependenceKind(parsedLifetimeDependenceKind), loc(loc) {}
  LifetimeDescriptor(ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
                     SourceLoc loc)
      : value{}, kind(DescriptorKind::Self),
        parsedLifetimeDependenceKind(parsedLifetimeDependenceKind), loc(loc) {}

public:
  static LifetimeDescriptor
  forNamed(Identifier name,
           ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
           SourceLoc loc) {
    return {name, parsedLifetimeDependenceKind, loc};
  }
  static LifetimeDescriptor
  forOrdered(unsigned index,
             ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
             SourceLoc loc,
             IsAddressable_t isAddress = IsNotAddressable) {
    return {index, isAddress, parsedLifetimeDependenceKind, loc};
  }
  static LifetimeDescriptor
  forSelf(ParsedLifetimeDependenceKind parsedLifetimeDependenceKind,
          SourceLoc loc) {
    return {parsedLifetimeDependenceKind, loc};
  }

  ParsedLifetimeDependenceKind getParsedLifetimeDependenceKind() const {
    return parsedLifetimeDependenceKind;
  }

  Identifier getName() const {
    assert(kind == DescriptorKind::Named);
    return value.Named.name;
  }

  unsigned getIndex() const {
    assert(kind == DescriptorKind::Ordered);
    return value.Ordered.index;
  }
  
  IsAddressable_t isAddressable() const {
    return kind == DescriptorKind::Ordered
      ? value.Ordered.isAddress
      : IsNotAddressable;
  }

  DescriptorKind getDescriptorKind() const { return kind; }

  SourceLoc getLoc() const { return loc; }

  bool isImmortalSpecifier() const {
    if (getDescriptorKind() != LifetimeDescriptor::DescriptorKind::Named) {
      return false;
    }
    return getName().str() == "immortal";
  }

  /// A specifier indicating that the target depends on variables in the
  /// captured context.
  static constexpr StringRef CapturesContextSpecifier = "captures";

  bool isCapturesSpecifier() const {
    if (getDescriptorKind() != LifetimeDescriptor::DescriptorKind::Named) {
      return false;
    }
    return getName().str() == CapturesContextSpecifier;
  }

  std::string getString() const;
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
                            getTrailingObjects());
  }

  size_t numTrailingObjects(OverloadToken<LifetimeDescriptor>) const {
    return numSources;
  }

public:
  static LifetimeEntry *
  create(const ASTContext &ctx, SourceLoc startLoc, SourceLoc endLoc,
         ArrayRef<LifetimeDescriptor> sources,
         std::optional<LifetimeDescriptor> targetDescriptor = std::nullopt);

  std::string getString() const;
  SourceLoc getLoc() const { return startLoc; }
  SourceLoc getStartLoc() const { return startLoc; }
  SourceLoc getEndLoc() const { return endLoc; }

  ArrayRef<LifetimeDescriptor> getSources() const {
    return getTrailingObjects(numSources);
  }

  std::optional<LifetimeDescriptor> getTargetDescriptor() const {
    return targetDescriptor;
  }
};

class LifetimeFlags : public FlagSet<uint8_t> {
public:
  enum {
    /// Whether the lifetime dependence info entry has an immortal specifier.
    HasImmortalSpecifier = 0,
    /// Whether the lifetime dependence info entry originated from an
    /// annotation.
    IsFromAnnotation = 1,
    /// Whether the LifetimeDependenceInfo entry includes a dependence on the
    /// closure context.
    Captures = 2,
  };

  explicit constexpr LifetimeFlags(uint8_t bits) : FlagSet(bits) {}
  constexpr LifetimeFlags() {}

  FLAGSET_DEFINE_FLAG_ACCESSORS_AND_BUILDER(HasImmortalSpecifier,
                                            hasImmortalSpecifier,
                                            setImmortalSpecifier,
                                            withImmortalSpecifier)
  FLAGSET_DEFINE_FLAG_ACCESSORS_AND_BUILDER(IsFromAnnotation, isFromAnnotation,
                                            setAnnotated, withAnnotated)
  FLAGSET_DEFINE_FLAG_ACCESSORS_AND_BUILDER(Captures, hasCaptures, setCaptures,
                                            withCaptures)
};

class LifetimeDependenceInfo {
  IndexSubset *inheritLifetimeParamIndices;
  IndexSubset *scopeLifetimeParamIndices;
  IndexSubset *addressableParamIndices;
  IndexSubset *conditionallyAddressableParamIndices;

  unsigned targetIndex;
  const LifetimeFlags flags;

  static unsigned numParams(IndexSubset *paramIndices) {
    return paramIndices ? paramIndices->getCapacity() : 0;
  }

  bool hasDependencySource() const {
    return inheritLifetimeParamIndices || scopeLifetimeParamIndices;
  };

public:
  /// Fully-initialized dependence info.
  LifetimeDependenceInfo(IndexSubset *inheritLifetimeParamIndices,
                         IndexSubset *scopeLifetimeParamIndices,
                         unsigned targetIndex,
                         IndexSubset *addressableParamIndices,
                         IndexSubset *conditionallyAddressableParamIndices,
                         LifetimeFlags flags)
      : inheritLifetimeParamIndices(inheritLifetimeParamIndices),
        scopeLifetimeParamIndices(scopeLifetimeParamIndices),
        addressableParamIndices(addressableParamIndices),
        conditionallyAddressableParamIndices(
            conditionallyAddressableParamIndices),
        targetIndex(targetIndex), flags(flags) {
    ASSERT(!empty());
    ASSERT(!inheritLifetimeParamIndices ||
           !inheritLifetimeParamIndices->isEmpty());
    ASSERT(!scopeLifetimeParamIndices || !scopeLifetimeParamIndices->isEmpty());
    assert((!addressableParamIndices
            || !conditionallyAddressableParamIndices
            || conditionallyAddressableParamIndices->isDisjointWith(
              addressableParamIndices)));

    if (CONDITIONAL_ASSERT_enabled()) {
      // Ensure inherit/scope/addressable param indices are of the same length
      // or 0.
      unsigned paramIndicesLength = numParams(inheritLifetimeParamIndices);
      if (scopeLifetimeParamIndices) {
        unsigned scopeIndicesLength = numParams(scopeLifetimeParamIndices);
        if (paramIndicesLength == 0) {
          paramIndicesLength = scopeIndicesLength;
        } else {
          ASSERT(paramIndicesLength == scopeIndicesLength);
        }
      }
      if (addressableParamIndices) {
        unsigned addressableIndicesLength = numParams(addressableParamIndices);
        if (paramIndicesLength == 0) {
          paramIndicesLength = addressableIndicesLength;
        } else {
          ASSERT(paramIndicesLength == addressableIndicesLength);
        }
      }
    }
  }

  /// Partially-initialized dependence info, with addressable & conditionally
  /// addressable parameter indices unset for now.
  LifetimeDependenceInfo(IndexSubset *inheritLifetimeParamIndices,
                         IndexSubset *scopeLifetimeParamIndices,
                         unsigned targetIndex, LifetimeFlags flags)
      : LifetimeDependenceInfo(inheritLifetimeParamIndices,
                               scopeLifetimeParamIndices, targetIndex,
                               // set during SIL type lowering
                               nullptr, nullptr, flags) {}

  operator bool() const { return !empty(); }

  bool empty() const {
    return !hasImmortalSpecifier() && inheritLifetimeParamIndices == nullptr &&
           !hasCaptures() && scopeLifetimeParamIndices == nullptr;
  }

  LifetimeFlags getFlags() const { return flags; }

  bool hasImmortalSpecifier() const { return flags.hasImmortalSpecifier(); }

  bool isImmortal() const {
    return hasImmortalSpecifier() && !hasDependencySource();
  }

  /// Whether this lifetime dependence corresponds to a @lifetime annotation in
  /// the source program (Swift or SIL). Such dependencies are likely to differ
  /// from the default (inferred) ones, so they must be included when printing a
  /// Swift function's type.
  bool isFromAnnotation() const { return flags.isFromAnnotation(); }

  /// Whether the target depends on the closure context.
  /// This is implicitly true for any function type.
  bool hasCaptures() const { return flags.hasCaptures(); }

  unsigned getTargetIndex() const { return targetIndex; }

  bool hasInheritLifetimeParamIndices() const {
    return inheritLifetimeParamIndices != nullptr;
  }
  bool hasScopeLifetimeParamIndices() const {
    return scopeLifetimeParamIndices != nullptr;
  }
  bool hasAddressableParamIndices() const {
    return addressableParamIndices != nullptr;
  }

  unsigned getParamIndicesLength() const {
    if (hasInheritLifetimeParamIndices()) {
      return getInheritIndices()->getCapacity();
    }
    if (hasScopeLifetimeParamIndices()) {
      return getScopeIndices()->getCapacity();
    }
    if (hasAddressableParamIndices()) {
      return getAddressableIndices()->getCapacity();
    }
    return 0;
  }

  IndexSubset *getInheritIndices() const { return inheritLifetimeParamIndices; }

  IndexSubset *getScopeIndices() const { return scopeLifetimeParamIndices; }

  /// Return the set of parameters which have addressable dependencies.
  ///
  /// This indicates that any dependency on the parameter value is dependent
  /// not only on the value, but the memory location of a particular instance
  /// of the value.
  IndexSubset *getAddressableIndices() const { return addressableParamIndices; }
  /// Return the set of parameters which may have addressable dependencies
  /// depending on the type of the parameter.
  ///
  /// Generic parameters need to be conservatively treated as addressable in
  /// situations where the substituted type may end up being addressable-for-
  /// dependencies. If substitution at a call site or specialization results
  /// in the type becoming concretely non-addressable-for-dependencies,
  /// then the lifetime dependency can be considered a normal value
  /// dependency.
  IndexSubset *getConditionallyAddressableIndices() const {
    return conditionallyAddressableParamIndices;
  }

  bool checkInherit(int index) const {
    return inheritLifetimeParamIndices
      && inheritLifetimeParamIndices->contains(index);
  }

  bool checkScope(int index) const {
    return scopeLifetimeParamIndices
      && scopeLifetimeParamIndices->contains(index);
  }

  /// Get a string representation of this LifetimeDependenceInfo suitable for
  /// printing in SIL. The target is not included, since this is determined by
  /// the position of the attribute in SIL.
  ///
  /// For printing function type lifetimes in Swift, see
  /// ASTPrinter::printSwiftLifetimeDependence.
  std::string getString() const;
  void Profile(llvm::FoldingSetNodeID &ID) const;
  void getConcatenatedData(SmallVectorImpl<bool> &concatenatedData) const;

  /// Builds LifetimeDependenceInfo from a swift decl, either from the explicit
  /// lifetime dependence specifiers or by inference based on types and
  /// ownership modifiers.
  static std::optional<ArrayRef<LifetimeDependenceInfo>> get(ValueDecl *decl);

  /// Builds LifetimeDependenceInfo from SIL
  static std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
  getFromSIL(FunctionTypeRepr *funcRepr, ArrayRef<SILParameterInfo> params,
             ArrayRef<SILResultInfo> results, DeclContext *dc);

  /// Builds LifetimeDependenceInfo from a function type.
  static std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
  getFromAST(FunctionTypeRepr *funcRepr, AnyFunctionType *funcType,
             ArrayRef<LifetimeTypeAttr *> lifetimeAttributes, DeclContext *dc,
             GenericEnvironment *env);

  /// Compute a LifetimeDependenceInfo list for the uncurried form of a curried
  /// function whose inner type has LifetimeDependenceInfo list
  /// 'inner'.
  ///
  /// TODO: Add support for merging with lifetime dependencies from the outer
  /// type. The outer type's result's dependencies should be copied to any
  /// inner-type dependencies that include closure context dependencies,
  /// replacing the closure context dependence for those entries.
  ///
  /// numInnerParams: number of parameters of the inner function type
  /// numOuterParams: number of parameters of the outer function type
  static ArrayRef<LifetimeDependenceInfo>
  uncurry(ASTContext &ctx, ArrayRef<LifetimeDependenceInfo> inner,
          unsigned numInnerParams, unsigned numOuterParams);

  bool operator==(const LifetimeDependenceInfo &other) const {
    return this->hasImmortalSpecifier() == other.hasImmortalSpecifier() &&
           this->hasCaptures() == other.hasCaptures() &&
           this->getTargetIndex() == other.getTargetIndex() &&
           this->getInheritIndices() == other.getInheritIndices() &&
           this->getAddressableIndices() == other.getAddressableIndices() &&
           this->getScopeIndices() == other.getScopeIndices();
  }

  bool operator!=(const LifetimeDependenceInfo &other) const {
    return !(*this == other);
  }
  
  SWIFT_DEBUG_DUMPER(dump());
};

/// The interface of a function or method with lifetime dependencies.
///
/// This type abstracts over inconsistencies in the representation of function
/// type lifetime dependencies.
///
/// The lifetime dependencies of normal function types are attached directly
/// to the function type:
///
///     @_lifetime(...) (Params...) -> Result
///
/// Instance methods have curried function types. Since lifetime dependencies
/// can involve the self parameter, they are attached to the outer type:
///
///     @_lifetime(...) (Self) -> (Params...) -> Result
///
/// Static methods cannot have lifetime dependencies involving self, so their
/// lifetime dependencies are attached to the actual interface type.
///
///     (Self.Type) -> @_lifetime(...) (Params...) -> Result
class LifetimeDependentInterface {
  /// (Params...) -> Result: The function's interface type.
  AnyFunctionType *interface;
  /// Self: The type of self, if it exists.
  std::optional<Type> implicitSelfType;
  /// @_lifetime(...): The lifetime dependencies of the interface.
  ArrayRef<LifetimeDependenceInfo> lifetimes;

public:
  /// Construct a lifetime-dependent interface for a function declaration with
  /// the specified interface type.
  ///
  /// For methods, the interface type should be the "inner" function type.
  /// I.e. (Params...) -> Result
  LifetimeDependentInterface(AbstractFunctionDecl *afd,
                             AnyFunctionType *interface);

  /// Construct a lifetime-dependent interface for a function type.
  LifetimeDependentInterface(AnyFunctionType *type);

  /// Construct a LifetimeDependentInterface from a ValueDecl and interface
  /// type. If the ValueDecl is an instance of AbstractFunctionDecl, call the
  /// (AbstractFunctionDecl*, AnyFunctionType*) constructor. Otherwise, call the
  /// (AnyFunctionType*) constructor with 'interface'.
  static LifetimeDependentInterface fromValueDecl(ValueDecl *decl,
                                                  AnyFunctionType *interface);

  ArrayRef<LifetimeDependenceInfo> getLifetimeDependencies() const {
    return lifetimes;
  }

  /// Get the type of the lifetime source or target with the given index.
  Type getSourceOrTargetType(unsigned index) const;

  /// Determine whether this interface's lifetime dependencies
  /// are compatible with those specified by 'to'. If they are, and all
  /// other aspects of the types are compatible, a function with this interface
  /// can be cast to type 'to'.
  ///
  /// This is determined according to the Lifetime Subtyping rules in
  /// ‎docs/ReferenceGuides/LifetimeAnnotation.md.
  ///
  /// Examples:
  ///   struct NE: ~Escapable {}
  ///   @_lifetime(copy ne0, copy ne1)
  ///   func copying01(ne0: NE, ne1: NE) -> NE {
  ///     return ne1
  ///   }
  ///   @_lifetime(copy ne0)
  ///   func copying0(ne0: NE, ne1: NE) -> NE {
  ///     return ne0
  ///   }
  ///   func takeCopying01(
  ///     f: @_lifetime(copy ne0, copy ne1)
  ///       (_ ne0: NE, _ ne1: NE) -> NE) {}
  ///   func takeCopying0(
  ///     f: @_lifetime(copy ne0)
  ///       (_ ne0: NE, _ ne1: NE) -> NE) {}
  ///
  ///   takeCopying0(f: copying0)    // OK: Dependencies match exactly.
  ///   takeCopying01(f: copying01)  // OK: Dependencies match exactly.
  ///   takeCopying01(f: copying0)   // OK: No dependencies are dropped.
  ///   takeCopying0(f: copying01)   // Error: The dependence on the second
  ///   parameter is dropped.
  bool canConvertTo(const LifetimeDependentInterface &to) const;

  /// Whether the lifetime dependencies 'fromDeps' are convertible to those
  /// of the target with the same index in 'to'.
  /// See canConvertTo.
  ///
  /// 'fromDeps' is assumed to be a member of this->lifetimes.
  bool canConvertTargetTo(const LifetimeDependenceInfo &fromDeps,
                          const LifetimeDependentInterface &to) const;

  /// Whether this interface has a lifetime dependence target with the given
  /// index.
  bool hasTarget(unsigned targetIndex) const;
};

std::optional<LifetimeDependenceInfo>
getLifetimeDependenceFor(ArrayRef<LifetimeDependenceInfo> lifetimeDependencies,
                         unsigned index);

/// Helper to remove lifetime dependencies whose target references an
/// Escapable type.
///
/// Returns true if the set of output lifetime dependencies is different from
/// the set of input lifetime dependencies, false if there was no change.
bool
filterEscapableLifetimeDependencies(GenericSignature sig,
        ArrayRef<LifetimeDependenceInfo> inputs,
        SmallVectorImpl<LifetimeDependenceInfo> &outputs,
        llvm::function_ref<Type (unsigned targetIndex)> getSubstTargetType);

StringRef
getNameForParsedLifetimeDependenceKind(ParsedLifetimeDependenceKind kind);

} // namespace swift

#endif
