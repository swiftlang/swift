//===--- ConstraintLocator.h - Constraint Locator ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
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

#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/AST/ASTNode.h"
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
class TypeLoc;
class VarDecl;
class Pattern;
class SourceManager;

namespace constraints {

class ConstraintSystem;

/// Locates a given constraint within the expression being
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
  /// Describes the kind of a particular path element, e.g.,
  /// "tuple element", "call result", "base of member lookup", etc.
  enum PathElementKind : unsigned char {
#define LOCATOR_PATH_ELT(Name) Name,
#define ABSTRACT_LOCATOR_PATH_ELT(Name)
#include "ConstraintLocatorPathElts.def"
  };

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

    /// Does this path involve an argument being applied to a non-ephemeral
    /// parameter?
    IsNonEphemeralParam = 0x2,
  };

  /// One element in the path of a locator, which can include both
  /// a kind (PathElementKind) and a value used to describe specific
  /// kinds further (e.g., the position of a tuple element).
  class PathElement {
    PathElementKind kind;

    /// The storage for the path element value. The value stores can either
    /// be a pointer or an unsigned int. Only custom path elements store values.
    uint64_t storage;

  public:
#define LOCATOR_PATH_ELT(Name) class Name;
#include "ConstraintLocatorPathElts.def"

    PathElement(PathElementKind kind, uint64_t storage = 0)
      : kind(kind), storage(storage) {}

    /// Retrieve the kind of path element.
    PathElementKind getKind() const { return kind; }

    /// Retrieve the raw storage value.
    uint64_t getRawStorage() const { return storage; }

    /// Attempts to cast the path element to a specific \c LocatorPathElt
    /// subclass, returning \c None if unsuccessful.
    template <class T>
    Optional<T> getAs() const {
      if (auto *result = dyn_cast<T>(this))
        return *result;
      return None;
    }

    /// Cast the path element to a specific \c LocatorPathElt subclass.
    template <class T>
    T castTo() const { return *cast<T>(this); }

    /// Checks whether the path element is a specific \c LocatorPathElt
    /// subclass.
    template <class T>
    bool is() const { return isa<T>(this); }

    /// Return the summary flags for this particular element.
    unsigned getNewSummaryFlags() const;

    bool isConditionalRequirement() const {
      return getKind() == PathElementKind::ConditionalRequirement;
    }

    bool isKeyPathDynamicMember() const {
      return getKind() == PathElementKind::KeyPathDynamicMember;
    }

    bool isKeyPathComponent() const {
      return getKind() == PathElementKind::KeyPathComponent;
    }

    bool isClosureResult() const {
      return getKind() == PathElementKind::ClosureResult;
    }

    /// Determine whether this element points to the contextual type
    /// associated with result of a single expression function.
    bool isResultOfSingleExprFunction() const;
  };

  /// Return the summary flags for an entire path.
  static unsigned getSummaryFlagsForPath(ArrayRef<PathElement> path) {
    unsigned flags = 0;
    for (auto &elt : path) flags |= elt.getNewSummaryFlags();
    return flags;
  }

  /// Retrieve the expression that anchors this locator.
  ASTNode getAnchor() const { return anchor; }

  /// Retrieve the path that extends from the anchor to a specific
  /// subcomponent.
  ArrayRef<PathElement> getPath() const {
    // FIXME: Alignment.
    return llvm::makeArrayRef(reinterpret_cast<const PathElement *>(this + 1),
                              numPathElements);
  }

  unsigned getSummaryFlags() const { return summaryFlags; }

  /// Determines whether this locator is part of a function
  /// conversion.
  bool isFunctionConversion() const {
    return (getSummaryFlags() & IsFunctionConversion);
  }

  /// Checks whether this locator is describing an argument application for a
  /// non-ephemeral parameter.
  bool isNonEphemeralParameterApplication() const {
    return (getSummaryFlags() & IsNonEphemeralParam);
  }

  /// Determine whether given locator points to the subscript reference
  /// e.g. `foo[0]` or `\Foo.[0]`
  bool isSubscriptMemberRef() const;

  /// Determine whether give locator points to the type of the
  /// key path expression.
  bool isKeyPathType() const;

  /// Determine whether given locator points to the keypath root
  bool isKeyPathRoot() const;

  /// Determine whether given locator points to the keypath value
  bool isKeyPathValue() const;
  
  /// Determine whether given locator points to the choice picked as
  /// as result of the key path dynamic member lookup operation.
  bool isResultOfKeyPathDynamicMemberLookup() const;

  /// Determine whether this locator points to a subscript component
  /// of the key path at some index.
  bool isKeyPathSubscriptComponent() const;

  /// Determine whether this locator points to the member found
  /// via key path dynamic member lookup.
  bool isForKeyPathDynamicMemberLookup() const;

  /// Determine whether this locator points to one of the key path
  /// components.
  bool isForKeyPathComponent() const;

  /// Determine whether this locator points to the generic parameter.
  bool isForGenericParameter() const;

  /// Determine whether this locator points to the element type of a
  /// sequence in a for ... in ... loop.
  bool isForSequenceElementType() const;

  /// Determine whether this locator points to the contextual type.
  bool isForContextualType() const;

  /// Determine whether this locator points to the assignment expression.
  bool isForAssignment() const;

  /// Determine whether this locator points to the coercion expression.
  bool isForCoercion() const;

  /// Determine whether this locator points to the `try?` expression.
  bool isForOptionalTry() const;

  /// Determine whether this locator is for a function builder body result type.
  bool isForFunctionBuilderBodyResult() const;

  /// Determine whether this locator points directly to a given expression.
  template <typename E> bool directlyAt() const {
    if (auto *expr = getAnchor().dyn_cast<Expr *>())
      return isa<E>(expr) && getPath().empty();
    return false;
  }

  /// Check whether the first element in the path of this locator (if any)
  /// is a given \c LocatorPathElt subclass.
  template <class T>
  bool isFirstElement() const {
    auto path = getPath();
    return !path.empty() && path.front().is<T>();
  }

  /// Attempts to cast the first path element of the locator to a specific
  /// \c LocatorPathElt subclass, returning \c None if either unsuccessful or
  /// the locator has no path elements.
  template <class T>
  Optional<T> getFirstElementAs() const {
    auto path = getPath();
    if (path.empty())
      return None;

    return path[0].getAs<T>();
  }

  /// Casts the first path element of the locator to a specific
  /// \c LocatorPathElt subclass, asserting that it has at least one element.
  template <class T>
  T castFirstElementTo() const {
    auto path = getPath();
    assert(!path.empty() && "Expected at least one path element!");
    return path[0].castTo<T>();
  }

  /// Check whether the last element in the path of this locator (if any)
  /// is a given \c LocatorPathElt subclass.
  template <class T>
  bool isLastElement() const {
    auto path = getPath();
    return !path.empty() && path.back().is<T>();
  }

  /// Attempts to cast the last path element of the locator to a specific
  /// \c LocatorPathElt subclass, returning \c None if either unsuccessful or
  /// the locator has no path elements.
  template <class T>
  Optional<T> getLastElementAs() const {
    auto path = getPath();
    if (path.empty())
      return None;

    return path.back().getAs<T>();
  }

  /// Casts the last path element of the locator to a specific \c LocatorPathElt
  /// subclass, asserting that it has at least one element.
  template <class T>
  T castLastElementTo() const {
    auto path = getPath();
    assert(!path.empty() && "Expected at least one path element!");
    return path.back().castTo<T>();
  }

  using PathIterator = ArrayRef<PathElement>::iterator;
  using PathReverseIterator = ArrayRef<PathElement>::reverse_iterator;

  /// Attempts to find the first element in the locator's path that is a
  /// specific \c LocatorPathElt subclass, returning \c None if no such element
  /// exists.
  ///
  /// \param iter A reference to an iterator which will be used to iterate
  /// over the locator's path.
  template <class T>
  Optional<T> findFirst(PathIterator &iter) const {
    auto path = getPath();
    auto end = path.end();
    assert(iter >= path.begin() && iter <= end);

    for (; iter != end; ++iter)
      if (auto elt = iter->getAs<T>())
        return elt;
    return None;
  }

  /// Attempts to find the first element in the locator's path that is a
  /// specific \c LocatorPathElt subclass, returning \c None if no such element
  /// exists.
  template <class T>
  Optional<T> findFirst() const {
    auto iter = getPath().begin();
    return findFirst<T>(iter);
  }

  /// Attempts to find the last element in the locator's path that is a
  /// specific \c LocatorPathElt subclass, returning \c None if no such element
  /// exists.
  ///
  /// \param iter A reference to a reverse iterator which will be used to
  /// iterate over the locator's path.
  template <class T>
  Optional<T> findLast(PathReverseIterator &iter) const {
    auto path = getPath();
    auto end = path.rend();
    assert(iter >= path.rbegin() && iter <= end);

    for (; iter != end; ++iter)
      if (auto elt = iter->getAs<T>())
        return elt;
    return None;
  }

  /// Attempts to find the last element in the locator's path that is a
  /// specific \c LocatorPathElt subclass, returning \c None if no such element
  /// exists.
  template <class T>
  Optional<T> findLast() const {
    auto iter = getPath().rbegin();
    return findLast<T>(iter);
  }

  /// If this locator points to generic parameter return its type.
  GenericTypeParamType *getGenericParameter() const;

  /// Produce a profile of this locator, for use in a folding set.
  static void Profile(llvm::FoldingSetNodeID &id, ASTNode anchor,
                      ArrayRef<PathElement> path);

  /// Produce a profile of this locator, for use in a folding set.
  void Profile(llvm::FoldingSetNodeID &id) {
    Profile(id, anchor, getPath());
  }
  
  /// Produce a debugging dump of this locator.
  SWIFT_DEBUG_DUMPER(dump(SourceManager *SM));
  SWIFT_DEBUG_DUMPER(dump(ConstraintSystem *CS));

  void dump(SourceManager *SM, raw_ostream &OS) const LLVM_ATTRIBUTE_USED;

private:
  /// Initialize a constraint locator with an anchor and a path.
  ConstraintLocator(ASTNode anchor, ArrayRef<PathElement> path, unsigned flags)
      : anchor(anchor), numPathElements(path.size()), summaryFlags(flags) {
    // FIXME: Alignment.
    std::copy(path.begin(), path.end(),
              reinterpret_cast<PathElement *>(this + 1));
  }

  /// Create a new locator from an anchor and an array of path
  /// elements.
  ///
  /// Note that this routine only handles the allocation and initialization
  /// of the locator. The ConstraintSystem object is responsible for
  /// uniquing via the FoldingSet.
  static ConstraintLocator *create(llvm::BumpPtrAllocator &allocator,
                                   ASTNode anchor, ArrayRef<PathElement> path,
                                   unsigned flags) {
    // FIXME: Alignment.
    unsigned size = sizeof(ConstraintLocator)
                  + path.size() * sizeof(PathElement);
    void *mem = allocator.Allocate(size, alignof(ConstraintLocator));
    return new (mem) ConstraintLocator(anchor, path, flags);
  }

  /// The expression at which this locator is anchored.
  ASTNode anchor;

  /// The number of path elements in this locator.
  ///
  /// The actual path elements are stored after the locator.
  unsigned numPathElements : 24;

  /// A set of flags summarizing interesting properties of the path.
  unsigned summaryFlags : 7;
  
  friend class ConstraintSystem;
};

using LocatorPathElt = ConstraintLocator::PathElement;

// Disallow direct uses of isa/cast/dyn_cast on LocatorPathElt in favor of using
// is/castTo/getAs. This allows us to work with Optional<T> rather than pointers
// for getAs, and maintains consistency with ConstraintLocator's
// isLastElement/castLastElementTo/getLastElementAs members.
template <class X>
inline bool
isa(const LocatorPathElt &) = delete; // Use LocatorPathElt::is instead.

template <class X>
inline typename llvm::cast_retty<X, LocatorPathElt>::ret_type
cast(const LocatorPathElt &) = delete; // Use LocatorPathElt::castTo instead.

template <class X>
inline typename llvm::cast_retty<X, LocatorPathElt>::ret_type
dyn_cast(const LocatorPathElt &) = delete; // Use LocatorPathElt::getAs instead.

#define SIMPLE_LOCATOR_PATH_ELT(Name) \
class LocatorPathElt:: Name final : public LocatorPathElt { \
public: \
  Name () : LocatorPathElt(ConstraintLocator:: Name) {} \
                                                        \
  static bool classof(const LocatorPathElt *elt) { \
    return elt->getKind() == ConstraintLocator:: Name; \
  } \
};
#include "ConstraintLocatorPathElts.def"

/// A base class for custom path elements that store numeric values.
template <unsigned NumValues>
class StoredIntegerElement: public LocatorPathElt {
public:
  template <unsigned NumNumericInputs = NumValues,
            typename = typename std::enable_if<NumNumericInputs == 1>::type>
  StoredIntegerElement(ConstraintLocator::PathElementKind kind, unsigned value)
    : LocatorPathElt(kind, value) {
    assert(value == getValue<0>() && "value truncated");
  }

  template <unsigned NumNumericInputs = NumValues,
            typename = typename std::enable_if<NumNumericInputs == 2>::type>
  StoredIntegerElement(ConstraintLocator::PathElementKind kind, unsigned value0, unsigned value1)
    : LocatorPathElt(kind, (value0 << 16 | value1)) {
    assert(value0 == getValue<0>() && "value0 truncated");
    assert(value1 == getValue<1>() && "value1 truncated");
  }

  template <unsigned NumNumericInputs = NumValues,
            typename = typename std::enable_if<NumNumericInputs == 3>::type>
  StoredIntegerElement(ConstraintLocator::PathElementKind kind, unsigned value0,
                       unsigned value1, unsigned value2)
      : LocatorPathElt(kind, uint64_t(value0) << 32 | uint64_t(value1) << 16 | uint64_t(value2)) {
    assert(value0 == getValue<0>() && "value0 truncated");
    assert(value1 == getValue<1>() && "value1 truncated");
    assert(value2 == getValue<2>() && "value2 truncated");
  }

  /// Retrieve a value associated with the path element.
  template <unsigned Index = 0,
            typename = typename std::enable_if<(Index < NumValues)>::type>
  unsigned getValue() const {
    // We pack values into 16 bit components of the storage, with value0
    // being stored in the upper bits, valueN in the lower bits. Therefore we
    // need to shift out any extra values in the lower bits.
    auto extraValues = NumValues - Index - 1;
    auto value = getRawStorage() >> (extraValues * 16);
    return value & 0xFFFF;
  }
};

/// A base class for custom path elements that store a pointer.
template <typename T>
class StoredPointerElement: public LocatorPathElt {
public:
  StoredPointerElement(ConstraintLocator::PathElementKind kind, const T *ptr)
      : LocatorPathElt(kind, reinterpret_cast<uintptr_t>(ptr)) {
    assert(ptr == getStoredPointer());
  }

  /// Retrieve the associated pointer for the element.
  T *getStoredPointer() const { return reinterpret_cast<T *>(getRawStorage()); }
};

// The following LocatorPathElt subclasses are used to expose accessors for
// specific path element information. They shouldn't introduce additional
// storage, as LocatorPathElt gets passed about by value. Custom path elements
// should subclass StoredIntegerElement to store unsigned values, or StoredPointerElement
// to store pointer values.

class LocatorPathElt::ApplyArgToParam final : public StoredIntegerElement<3> {
public:
  ApplyArgToParam(unsigned argIdx, unsigned paramIdx, ParameterTypeFlags flags)
      : StoredIntegerElement(ConstraintLocator::ApplyArgToParam, argIdx, paramIdx, flags.toRaw()) {}

  unsigned getArgIdx() const { return getValue<0>(); }
  unsigned getParamIdx() const { return getValue<1>(); }
  ParameterTypeFlags getParameterFlags() const {
    return ParameterTypeFlags::fromRaw(getValue<2>());
  }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::ApplyArgToParam;
  }
};

class LocatorPathElt::SynthesizedArgument final : public StoredIntegerElement<1> {
public:
  SynthesizedArgument(unsigned index)
      : StoredIntegerElement(ConstraintLocator::SynthesizedArgument, index) {}

  unsigned getIndex() const { return getValue(); }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::SynthesizedArgument;
  }
};

/// Abstract superclass for any kind of tuple element.
class LocatorPathElt::AnyTupleElement : public StoredIntegerElement<1> {
protected:
  AnyTupleElement(PathElementKind kind, unsigned index)
      : StoredIntegerElement(kind, index) {
    assert(classof(this) && "classof needs updating");
  }

public:
  unsigned getIndex() const { return getValue(); }

  static bool classof(const LocatorPathElt *elt) {
    return elt->is<LocatorPathElt::TupleElement>() ||
           elt->is<LocatorPathElt::NamedTupleElement>();
  }
};

class LocatorPathElt::TupleElement final
    : public LocatorPathElt::AnyTupleElement {
public:
  TupleElement(unsigned index)
      : AnyTupleElement(ConstraintLocator::TupleElement, index) {}

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::TupleElement;
  }
};

class LocatorPathElt::NamedTupleElement final
    : public LocatorPathElt::AnyTupleElement {
public:
  NamedTupleElement(unsigned index)
      : AnyTupleElement(ConstraintLocator::NamedTupleElement, index) {}

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::NamedTupleElement;
  }
};

class LocatorPathElt::KeyPathComponent final : public StoredIntegerElement<1> {
public:
  KeyPathComponent(unsigned index)
      : StoredIntegerElement(ConstraintLocator::KeyPathComponent, index) {}

  unsigned getIndex() const { return getValue(); }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::KeyPathComponent;
  }
};

class LocatorPathElt::GenericArgument final : public StoredIntegerElement<1> {
public:
  GenericArgument(unsigned index)
      : StoredIntegerElement(ConstraintLocator::GenericArgument, index) {}

  unsigned getIndex() const { return getValue(); }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::GenericArgument;
  }
};

/// Abstract superclass for any kind of element that describes a requirement
/// placed on a type within a requirements clause.
class LocatorPathElt::AnyRequirement : public StoredIntegerElement<2> {
protected:
  AnyRequirement(PathElementKind kind, unsigned index, RequirementKind reqKind)
      : StoredIntegerElement(kind, index, static_cast<unsigned>(reqKind)) {
    assert(classof(this) && "classof needs updating");
  }

public:
  unsigned getIndex() const { return getValue<0>(); }
  RequirementKind getRequirementKind() const {
    return static_cast<RequirementKind>(getValue<1>());
  }

  static bool classof(const LocatorPathElt *elt) {
    return elt->is<LocatorPathElt::ConditionalRequirement>() ||
           elt->is<LocatorPathElt::TypeParameterRequirement>();
  }
};

class LocatorPathElt::ConditionalRequirement final
    : public LocatorPathElt::AnyRequirement {
public:
  ConditionalRequirement(unsigned index, RequirementKind reqKind)
      : AnyRequirement(ConstraintLocator::ConditionalRequirement, index,
                       reqKind) {}

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::ConditionalRequirement;
  }
};

class LocatorPathElt::TypeParameterRequirement final
    : public LocatorPathElt::AnyRequirement {
public:
  TypeParameterRequirement(unsigned index, RequirementKind reqKind)
      : AnyRequirement(ConstraintLocator::TypeParameterRequirement, index,
                       reqKind) {}

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::TypeParameterRequirement;
  }
};

class LocatorPathElt::ClosureBody final : public StoredIntegerElement<1> {
  public:
  ClosureBody(bool hasExplicitReturn = false)
    : StoredIntegerElement(ConstraintLocator::ClosureBody, hasExplicitReturn) {}

  /// Indicates whether body of the closure has any `return` statements.
  bool hasExplicitReturn() const { return bool(getValue()); }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::ClosureBody;
  }
};

class LocatorPathElt::ContextualType final : public StoredIntegerElement<1> {
public:
  ContextualType(bool isForSingleExprFunction = false)
      : StoredIntegerElement(ConstraintLocator::ContextualType, isForSingleExprFunction) {}

  /// Whether this element points to the contextual type associated with the
  /// result of a single expression function.
  bool isForSingleExprFunction() const { return bool(getValue()); }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::ContextualType;
  }
};

class LocatorPathElt::Witness final : public StoredPointerElement<ValueDecl> {
public:
  Witness(ValueDecl *witness)
      : StoredPointerElement(PathElementKind::Witness, witness) {}

  ValueDecl *getDecl() const { return getStoredPointer(); }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == PathElementKind::Witness;
  }
};

class LocatorPathElt::ProtocolRequirement final : public StoredPointerElement<ValueDecl> {
public:
  ProtocolRequirement(ValueDecl *decl)
      : StoredPointerElement(PathElementKind::ProtocolRequirement, decl) {}

  ValueDecl *getDecl() const { return getStoredPointer(); }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == PathElementKind::ProtocolRequirement;
  }
};

class LocatorPathElt::GenericParameter final : public StoredPointerElement<GenericTypeParamType> {
public:
  GenericParameter(GenericTypeParamType *type)
      : StoredPointerElement(PathElementKind::GenericParameter, type) {
    static_assert(alignof(GenericTypeParamType) >= 4,
                  "archetypes insufficiently aligned");
  }

  GenericTypeParamType *getType() const {
    return getStoredPointer();
  }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == PathElementKind::GenericParameter;
  }
};

class LocatorPathElt::OpenedGeneric final : public StoredPointerElement<GenericSignatureImpl> {
public:
  OpenedGeneric(GenericSignature sig)
      : StoredPointerElement(PathElementKind::OpenedGeneric, sig.getPointer()) {}

  GenericSignature getSignature() const {
    return getStoredPointer();
  }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::OpenedGeneric;
  }
};

class LocatorPathElt::KeyPathDynamicMember final : public StoredPointerElement<NominalTypeDecl> {
public:
  KeyPathDynamicMember(NominalTypeDecl *keyPathDecl)
      : StoredPointerElement(PathElementKind::KeyPathDynamicMember, keyPathDecl) {}

  NominalTypeDecl *getKeyPathDecl() const {
    return getStoredPointer();
  }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::KeyPathDynamicMember;
  }
};

class LocatorPathElt::TernaryBranch final : public StoredIntegerElement<1> {
public:
  TernaryBranch(bool side)
      : StoredIntegerElement(ConstraintLocator::TernaryBranch, side) {}

  bool forThen() const { return bool(getValue()); }

  bool forElse() const { return !bool(getValue()); }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::TernaryBranch;
  }
};

class LocatorPathElt::PatternMatch final : public StoredPointerElement<Pattern> {
public:
  PatternMatch(Pattern *pattern)
      : StoredPointerElement(PathElementKind::PatternMatch, pattern) {}

  Pattern *getPattern() const { return getStoredPointer(); }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::PatternMatch;
  }
};

class LocatorPathElt::ArgumentAttribute final : public StoredIntegerElement<1> {
public:
  enum Attribute : uint8_t { InOut, Escaping };

private:
  ArgumentAttribute(Attribute attr)
      : StoredIntegerElement(ConstraintLocator::ArgumentAttribute, static_cast<uint8_t>(attr)) {}

public:
  Attribute getAttr() const { return static_cast<Attribute>(getValue()); }

  static ArgumentAttribute forInOut() {
    return ArgumentAttribute(Attribute::InOut);
  }

  static ArgumentAttribute forEscaping() {
    return ArgumentAttribute(Attribute::Escaping);
  }

  static bool classof(const LocatorPathElt *elt) {
    return elt->getKind() == ConstraintLocator::ArgumentAttribute;
  }
};

namespace details {
  template <typename CustomPathElement>
  class PathElement {
    static constexpr bool hasStoredValueImpl(...) { return false; }

    template <unsigned N>
    static constexpr bool hasStoredValueImpl(StoredIntegerElement<N> *) { return true; }

    template <typename T>
    static constexpr bool hasStoredValueImpl(StoredPointerElement<T> *) { return true; }

  public:
    static constexpr bool hasStoredValue() {
      return hasStoredValueImpl(static_cast<CustomPathElement *>(nullptr));
    }
  };
}

template <typename CustomPathElement>
constexpr bool isValidCustomPathElement() {
  return details::PathElement<CustomPathElement>::hasStoredValue();
}

// All custom path element classes must inherit from StoredIntegerElement or StoredPointerElement
#define CUSTOM_LOCATOR_PATH_ELT(Name) \
static_assert(isValidCustomPathElement<LocatorPathElt::Name>(), \
    "Custom path elements must inherit from StoredIntegerElement or StoredPointerElement");
#include "ConstraintLocatorPathElts.def"


/// A simple stack-only builder object that constructs a
/// constraint locator without allocating memory.
///
/// Use this object to build a path when passing components down the
/// stack, e.g., when recursively breaking apart types as in \c matchTypes().
class ConstraintLocatorBuilder {
  /// The constraint locator that this builder extends or the
  /// previous builder in the chain.
  llvm::PointerUnion<ConstraintLocator *, ConstraintLocatorBuilder *>
    previous;

  /// The current path element, if there is one.
  Optional<LocatorPathElt> element;

  /// The current set of flags.
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

  /// Retrieve a new path with the given path element added to it. Note that
  /// the produced locator stores a reference to this locator, and therefore
  /// must not outlive it.
  ConstraintLocatorBuilder withPathElement(LocatorPathElt newElt) & {
    unsigned newFlags = summaryFlags | newElt.getNewSummaryFlags();
    if (!element)
      return ConstraintLocatorBuilder(previous, newElt, newFlags);

    return ConstraintLocatorBuilder(this, newElt, newFlags);
  }

  /// Determine whether this builder has an empty path.
  bool hasEmptyPath() const {
    return !element;
  }

  /// Return the set of flags that summarize this path.
  unsigned getSummaryFlags() const {
    return summaryFlags;
  }

  bool isFunctionConversion() const {
    return (getSummaryFlags() & ConstraintLocator::IsFunctionConversion);
  }

  bool isForAutoclosureResult() const {
    SmallVector<LocatorPathElt, 4> path;
    getLocatorParts(path);

    auto last = std::find_if(
        path.rbegin(), path.rend(), [](LocatorPathElt &elt) -> bool {
          return elt.getKind() != ConstraintLocator::OptionalPayload &&
                 elt.getKind() != ConstraintLocator::GenericArgument;
        });

    if (last != path.rend())
      return last->getKind() == ConstraintLocator::AutoclosureResult;

    return false;
  }

  /// Checks whether this locator is describing an argument application for a
  /// non-ephemeral parameter.
  bool isNonEphemeralParameterApplication() const {
    return (getSummaryFlags() & ConstraintLocator::IsNonEphemeralParam);
  }

  /// Retrieve the base constraint locator, on which this builder's
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

  /// Get anchor expression associated with this locator builder.
  ASTNode getAnchor() const {
    for (auto prev = this; prev;
         prev = prev->previous.dyn_cast<ConstraintLocatorBuilder *>()) {
      if (auto *locator = prev->previous.dyn_cast<ConstraintLocator *>())
        return locator->getAnchor();
    }

    return {};
  }

  /// Retrieve the components of the complete locator, which includes
  /// the anchor expression and the path.
  ASTNode getLocatorParts(SmallVectorImpl<LocatorPathElt> &path) const {
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
