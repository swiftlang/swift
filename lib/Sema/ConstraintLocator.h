//===--- ConstraintLocator.h - Constraint Locator ---------------*- C++ -*-===//
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
// This file provides the \c ConstraintLocator class and its related types,
// which is used by the constraint-based type checker to describe how
// a particular constraint was derived.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CONSTRAINTLOCATOR_H
#define SWIFT_SEMA_CONSTRAINTLOCATOR_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/ErrorHandling.h"
#include <utility>

namespace swift {

class Expr;
class SourceManager;

namespace constraints {
  class ConstraintSystem;

/// \brief Locates a given constraint within the expression being
/// type-checked, which may refer down into subexpressions and parts of
/// the types of those subexpressions.
///
/// Each locator as anchored at some expression, e.g., (3, (x, 3.14)),
/// and contains a path that digs further into the type of that expression.
/// For example, the path "tuple element #1" -> "tuple element #0" with the
/// above expression would refer to 'x'. If 'x' had function type, the
/// path could be further extended with either "-> argument" or "-> result",
/// to indicate constraints on its argument or result type.
class ConstraintLocator : public llvm::FoldingSetNode {
public:
  /// \brief Describes the kind of a particular path element, e.g.,
  /// "tuple element", "call result", "base of member lookup", etc.
  enum PathElementKind : unsigned char {
    /// \brief The argument of function application.
    ApplyArgument,
    /// \brief The function being applied.
    ApplyFunction,
    /// Matching an argument to a parameter.
    ApplyArgToParam,
    /// \brief An archetype being opened.
    ///
    /// Also contains the archetype itself.
    Archetype,
    /// An associated type reference.
    ///
    /// Contains the associated type itself.
    AssociatedType,
    /// \brief The argument type of a function.
    FunctionArgument,
    /// \brief The result type of a function.
    FunctionResult,
    /// \brief A tuple element referenced by position.
    TupleElement,
    /// \brief A tuple element referenced by name.
    NamedTupleElement,
    /// \brief An optional payload.
    OptionalPayload,
    /// \brief A generic argument.
    /// FIXME: Add support for named generic arguments?
    GenericArgument,
    /// \brief A member.
    /// FIXME: Do we need the actual member name here?
    Member,
    /// \brief An unresolved member.
    UnresolvedMember,
    /// \brief The base of a member expression.
    MemberRefBase,
    /// \brief The lookup for a subscript member.
    SubscriptMember,
    /// \brief The index of a subscript expression.
    SubscriptIndex,
    /// \brief The result of a subscript expression.
    SubscriptResult,
    /// \brief The lookup for a constructor member.
    ConstructorMember,
    /// \brief Rvalue adjustment.
    RvalueAdjustment,
    /// \brief The result of a closure.
    ClosureResult,
    /// \brief The parent of a nested type.
    ParentType,
    /// \brief The instance of a metatype type.
    InstanceType,
    /// \brief The generic type of a sequence.
    SequenceIteratorProtocol,
    /// \brief The element type of a generator.
    GeneratorElementType,
    /// \brief The element of an array type.
    ArrayElementType,
    /// \brief The scalar type of a tuple type.
    ScalarToTuple,
    /// \brief The load of an lvalue.
    Load,
    /// The requirement that we're matching during protocol conformance
    /// checking.
    Requirement,
    /// The candidate witness during protocol conformance checking.
    Witness,
    /// This is referring to a type produced by opening a generic type at the
    /// base of the locator.
    OpenedGeneric,
  };

  /// \brief Determine the number of numeric values used for the given path
  /// element kind.
  static unsigned numNumericValuesInPathElement(PathElementKind kind) {
    switch (kind) {
    case ApplyArgument:
    case ApplyFunction:
    case Archetype:
    case AssociatedType:
    case FunctionArgument:
    case FunctionResult:
    case OptionalPayload:
    case Member:
    case MemberRefBase:
    case UnresolvedMember:
    case SubscriptIndex:
    case SubscriptMember:
    case SubscriptResult:
    case ConstructorMember:
    case RvalueAdjustment:
    case ClosureResult:
    case ParentType:
    case InstanceType:
    case SequenceIteratorProtocol:
    case GeneratorElementType:
    case ArrayElementType:
    case ScalarToTuple:
    case Load:
    case Requirement:
    case Witness:
    case OpenedGeneric:
      return 0;

    case GenericArgument:
    case NamedTupleElement:
    case TupleElement:
      return 1;

    case ApplyArgToParam:
      return 2;
    }

    llvm_unreachable("Unhandled PathElementKind in switch.");
  }

  /// Flags for efficiently recording certain information about a path.
  /// All of this information should be re-derivable from the path.
  ///
  /// Values are chosen so that an empty path has value 0 and the
  /// flags for a concatenated paths is simply the bitwise-or of the
  /// flags of the component paths.
  enum Flag : unsigned {
    /// Does this path involve a function conversion, i.e. a
    /// FunctionArgument or FunctionResult node?
    IsFunctionConversion = 0x1,
  };

  static unsigned getSummaryFlagsForPathElement(PathElementKind kind) {
    switch (kind) {
    case ApplyArgument:
    case ApplyFunction:
    case ApplyArgToParam:
    case SequenceIteratorProtocol:
    case GeneratorElementType:
    case ArrayElementType:
    case ClosureResult:
    case ConstructorMember:
    case InstanceType:
    case Load:
    case OptionalPayload:
    case Member:
    case MemberRefBase:
    case UnresolvedMember:
    case ParentType:
    case RvalueAdjustment:
    case ScalarToTuple:
    case SubscriptIndex:
    case SubscriptMember:
    case SubscriptResult:
    case OpenedGeneric:
    case Archetype:
    case AssociatedType:
    case GenericArgument:
    case NamedTupleElement:
    case TupleElement:
    case Requirement:
    case Witness:
      return 0;

    case FunctionArgument:
    case FunctionResult:
      return IsFunctionConversion;
    }

    llvm_unreachable("Unhandled PathElementKind in switch.");
  }

  template<unsigned N> struct incomplete;
  
  /// \brief One element in the path of a locator, which can include both
  /// a kind (PathElementKind) and a value used to describe specific
  /// kinds further (e.g., the position of a tuple element).
  class PathElement {
    /// \brief Describes the kind of data stored here.
    enum StoredKind : unsigned char {
      StoredArchetype,
      StoredRequirement,
      StoredWitness,
      StoredKindAndValue
    };

    /// \brief The actual storage for the path element, which involves both a
    /// kind and (potentially) a value.
    ///
    /// The current storage involves a two-bit "storage kind", which selects
    /// among the possible value stores. The value stores can either be an
    /// archetype (for archetype path elements) or an unsigned value that
    /// stores both the specific kind and the (optional) numeric value of that
    /// kind. Use \c encodeStorage and \c decodeStorage to work with this value.
    ///
    /// \note The "storage kind" is stored in the  \c storedKind field.
    uint64_t storage : 62;

    /// \brief The kind of value stored in \c storage. Valid values are those
    /// from the StoredKind enum.
    uint64_t storedKind : 2;

    /// \brief Encode a path element kind and a value into the storage format.
    static uint64_t encodeStorage(PathElementKind kind, unsigned value) {
      return ((uint64_t)value << 8) | kind;
    }

    /// \brief Decode a storage value into path element kind and value.
    static std::pair<PathElementKind, unsigned>
    decodeStorage(uint64_t storage) {
      return { (PathElementKind)((unsigned)storage & 0xFF), storage >> 8 };
    }

    PathElement(PathElementKind kind, unsigned value)
      : storage(encodeStorage(kind, value)), storedKind(StoredKindAndValue)
    {
      assert(numNumericValuesInPathElement(kind) == 1 &&
             "Path element kind does not require 1 value");
    }

    PathElement(PathElementKind kind, unsigned value1, unsigned value2)
      : storage(encodeStorage(kind, value1 << 16 | value2)),
        storedKind(StoredKindAndValue)
    {
      assert(numNumericValuesInPathElement(kind) == 2 &&
             "Path element kind does not require 2 values");
    }

    friend class ConstraintLocator;

  public:
    PathElement(PathElementKind kind)
      : storage(encodeStorage(kind, 0)), storedKind(StoredKindAndValue)
    {
      assert(numNumericValuesInPathElement(kind) == 0 &&
             "Path element requires value");
    }

    PathElement(ArchetypeType *archetype)
      : storage((reinterpret_cast<uintptr_t>(archetype) >> 2)),
        storedKind(StoredArchetype)
    {
      static_assert(alignof(ArchetypeType) >= 4,
                    "archetypes insufficiently aligned");
      assert(getArchetype() == archetype);
    }

    PathElement(PathElementKind kind, ValueDecl *decl)
      : storage((reinterpret_cast<uintptr_t>(decl) >> 2)),
        storedKind(kind == Witness ? StoredWitness : StoredRequirement)
    {
      assert((kind == Witness || kind == Requirement) &&
             "Not a witness element");
      assert(((kind == Requirement && getRequirement() == decl) ||
              (kind == Witness && getWitness() == decl)));
    }

    PathElement(AssociatedTypeDecl *decl)
      : storage((reinterpret_cast<uintptr_t>(decl) >> 2)),
        storedKind(StoredRequirement)
    {
      assert(getAssociatedType() == decl);
    }

    /// \brief Retrieve a path element for a tuple element referred to by
    /// its position.
    static PathElement getTupleElement(unsigned position) {
      return PathElement(TupleElement, position);
    }

    /// \brief Retrieve a path element for a tuple element referred to by
    /// its name.
    static PathElement getNamedTupleElement(unsigned position) {
      return PathElement(NamedTupleElement, position);
    }

    /// Retrieve a patch element for an argument/parameter comparison in a
    /// function application.
    static PathElement getApplyArgToParam(unsigned argIdx, unsigned paramIdx) {
      return PathElement(ApplyArgToParam, argIdx, paramIdx);
    }

    /// \brief Retrieve a path element for a generic argument referred to by
    /// its position.
    static PathElement getGenericArgument(unsigned position) {
      return PathElement(GenericArgument, position);
    }

    /// \brief Retrieve the kind of path element.
    PathElementKind getKind() const {
      switch (static_cast<StoredKind>(storedKind)) {
      case StoredArchetype:
        return Archetype;

      case StoredRequirement:
        return isa<AssociatedTypeDecl>(getRequirement()) ? AssociatedType
                                                         : Requirement;

      case StoredWitness:
        return Witness;

      case StoredKindAndValue:
        return decodeStorage(storage).first;
      }

      llvm_unreachable("Unhandled StoredKind in switch.");
    }

    /// \brief Retrieve the value associated with this path element,
    /// if it has one.
    unsigned getValue() const {
      unsigned numValues = numNumericValuesInPathElement(getKind());
      assert(numValues > 0 && "No value in path element!");

      auto value = decodeStorage(storage).second;
      if (numValues == 1) {
        return value;
      }

      return value >> 16;
    }

    /// \brief Retrieve the second value associated with this path element,
    /// if it has one.
    unsigned getValue2() const {
      unsigned numValues = numNumericValuesInPathElement(getKind());
      (void)numValues;
      assert(numValues == 2 && "No second value in path element!");

      auto value = decodeStorage(storage).second;
      return value & 0x00FFFF;
    }

    /// Retrieve the declaration for a witness path element.
    ValueDecl *getWitness() const {
      assert(getKind() == Witness && "Is not a witness");
      return reinterpret_cast<ValueDecl *>(storage << 2);
    }

    /// \brief Retrieve the actual archetype for an archetype path element.
    ArchetypeType *getArchetype() const {
      assert(getKind() == Archetype && "Not an archetype path element");
      return reinterpret_cast<ArchetypeType *>(storage << 2);
    }

    /// Retrieve the declaration for a requirement path element.
    ValueDecl *getRequirement() const {
      assert((static_cast<StoredKind>(storedKind) == StoredRequirement) &&
             "Is not a requirement");
      return reinterpret_cast<ValueDecl *>(storage << 2);
    }

    /// Retrieve the declaration for an associated type path element.
    AssociatedTypeDecl *getAssociatedType() const {
      assert(getKind() == AssociatedType && "Is not an associated type");
      return reinterpret_cast<AssociatedTypeDecl *>(storage << 2);
    }

    /// \brief Return the summary flags for this particular element.
    unsigned getNewSummaryFlags() const {
      return getSummaryFlagsForPathElement(getKind());
    }
  };

  /// Return the summary flags for an entire path.
  static unsigned getSummaryFlagsForPath(ArrayRef<PathElement> path) {
    unsigned flags = 0;
    for (auto &elt : path) flags |= elt.getNewSummaryFlags();
    return flags;
  }

  /// \brief Retrieve the expression that anchors this locator.
  Expr *getAnchor() const { return anchor; }
  
  /// \brief Retrieve the path that extends from the anchor to a specific
  /// subcomponent.
  ArrayRef<PathElement> getPath() const {
    // FIXME: Alignment.
    return llvm::makeArrayRef(reinterpret_cast<const PathElement *>(this + 1),
                              numPathElements);
  }

  unsigned getSummaryFlags() const { return summaryFlags; }

  /// \brief Determines whether this locator is part of a function
  /// conversion.
  bool isFunctionConversion() const {
    return (getSummaryFlags() & IsFunctionConversion);
  }

  /// \brief Produce a profile of this locator, for use in a folding set.
  static void Profile(llvm::FoldingSetNodeID &id, Expr *anchor,
                      ArrayRef<PathElement> path);
  
  /// \brief Produce a profile of this locator, for use in a folding set.
  void Profile(llvm::FoldingSetNodeID &id) {
    Profile(id, anchor, getPath());
  }
  
  /// \brief Produce a debugging dump of this locator.
  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(SourceManager *SM) LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");
  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(ConstraintSystem *CS) LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");

  void dump(SourceManager *SM, raw_ostream &OS) LLVM_ATTRIBUTE_USED;

private:
  /// \brief Initialize a constraint locator with an anchor and a path.
  ConstraintLocator(Expr *anchor, ArrayRef<PathElement> path,
                    unsigned flags)
    : anchor(anchor), numPathElements(path.size()), summaryFlags(flags)
  {
    // FIXME: Alignment.
    std::copy(path.begin(), path.end(),
              reinterpret_cast<PathElement *>(this + 1));
  }

  /// \brief Create a new locator from an anchor and an array of path
  /// elements.
  ///
  /// Note that this routine only handles the allocation and initialization
  /// of the locator. The ConstraintSystem object is responsible for
  /// uniquing via the FoldingSet.
  static ConstraintLocator *create(llvm::BumpPtrAllocator &allocator,
                                   Expr *anchor,
                                   ArrayRef<PathElement> path,
                                   unsigned flags) {
    // FIXME: Alignment.
    unsigned size = sizeof(ConstraintLocator)
                  + path.size() * sizeof(PathElement);
    void *mem = allocator.Allocate(size, alignof(ConstraintLocator));
    return new (mem) ConstraintLocator(anchor, path, flags);
  }

  /// \brief The expression at which this locator is anchored.
  Expr *anchor;

  /// \brief The number of path elements in this locator.
  ///
  /// The actual path elements are stored after the locator.
  unsigned numPathElements : 24;

  /// \brief A set of flags summarizing interesting properties of the path.
  unsigned summaryFlags : 7;
  
  friend class ConstraintSystem;
};

typedef ConstraintLocator::PathElement LocatorPathElt;

/// \brief A simple stack-only builder object that constructs a
/// constraint locator without allocating memory.
///
/// Use this object to build a path when passing components down the
/// stack, e.g., when recursively breaking apart types as in \c matchTypes().
class ConstraintLocatorBuilder {
  /// \brief The constraint locator that this builder extends or the
  /// previous builder in the chain.
  llvm::PointerUnion<ConstraintLocator *, ConstraintLocatorBuilder *>
    previous;

  /// \brief The current path element, if there is one.
  Optional<LocatorPathElt> element;

  /// \brief The current set of flags.
  unsigned summaryFlags;

  ConstraintLocatorBuilder(llvm::PointerUnion<ConstraintLocator *,
                                              ConstraintLocatorBuilder *>
                             previous,
                           LocatorPathElt element,
                           unsigned flags)
    : previous(previous), element(element), summaryFlags(flags) { }

public:
  ConstraintLocatorBuilder(ConstraintLocator *locator)
    : previous(locator), element(),
      summaryFlags(locator ? locator->getSummaryFlags() : 0) { }

  /// \brief Retrieve a new path with the given path element added to it.
  ConstraintLocatorBuilder withPathElement(LocatorPathElt newElt) {
    unsigned newFlags = summaryFlags | newElt.getNewSummaryFlags();
    if (!element)
      return ConstraintLocatorBuilder(previous, newElt, newFlags);

    return ConstraintLocatorBuilder(this, newElt, newFlags);
  }

  /// \brief Determine whether this builder has an empty path.
  bool hasEmptyPath() const {
    return !element;
  }

  /// \brief Return the set of flags that summarize this path.
  unsigned getSummaryFlags() const {
    return summaryFlags;
  }

  bool isFunctionConversion() const {
    return (getSummaryFlags() & ConstraintLocator::IsFunctionConversion);
  }

  /// \brief Retrieve the base constraint locator, on which this builder's
  /// path is based.
  ConstraintLocator *getBaseLocator() const {
    for (auto prev = this;
         prev;
         prev = prev->previous.dyn_cast<ConstraintLocatorBuilder *>()) {
      if (auto locator = prev->previous.dyn_cast<ConstraintLocator *>())
        return locator;
    }

    return nullptr;
  }

  /// \brief Retrieve the components of the complete locator, which includes
  /// the anchor expression and the path.
  Expr *getLocatorParts(SmallVectorImpl<LocatorPathElt> &path) const {
    for (auto prev = this;
         prev;
         prev = prev->previous.dyn_cast<ConstraintLocatorBuilder *>()) {
      // If there is an element at this level, add it.
      if (prev->element)
        path.push_back(*prev->element);

      if (auto locator = prev->previous.dyn_cast<ConstraintLocator *>()) {
        // We found the end of the chain. Reverse the path we've built up,
        // then prepend the locator's path.
        std::reverse(path.begin(), path.end());
        path.insert(path.begin(),
                    locator->getPath().begin(),
                    locator->getPath().end());
        return locator->getAnchor();
      }
    }

    // There was no locator. Just reverse the path.
    std::reverse(path.begin(), path.end());
    return nullptr;
  }

  /// Attempt to simplify this locator to a single expression.
  Expr *trySimplifyToExpr() const;

  /// Retrieve the last element in the path, if there is one.
  Optional<LocatorPathElt> last() const {
    // If we stored a path element here, grab it.
    if (element) return *element;

    // Otherwise, look in the previous builder if there is one.
    if (auto prevBuilder = previous.dyn_cast<ConstraintLocatorBuilder *>())
      return prevBuilder->last();

    // Next, check the constraint locator itself.
    if (auto locator = previous.dyn_cast<ConstraintLocator *>()) {
      auto path = locator->getPath();
      if (path.empty()) return None;
      return path.back();
    }

    return None;
  }
};

} // end namespace constraints
} // end namespace swift

#endif // LLVM_SWIFT_SEMA_CONSTRAINTLOCATOR_H
