//===--- AbstractionPattern.h - SIL type abstraction pattersn ---*- C++ -*-===//
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
// This file defines the AbstractionPattern class, which is used to
// lower formal AST types into their SIL lowerings.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_ABSTRACTIONPATTERN_H
#define SWIFT_SIL_ABSTRACTIONPATTERN_H

#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"

namespace llvm {
  template <class T> class function_ref;
}

namespace clang {
  class Type;
  class ObjCMethodDecl;
}

namespace swift {
namespace Lowering {

/// A pattern for the abstraction of a value.  See the large comment
/// in SILGenPoly.cpp.
///
/// An abstraction pattern is represented with an original,
/// unsubstituted type.  The archetypes or generic parameters
/// naturally fall at exactly the specified abstraction points.
class AbstractionPattern {
  enum class Kind {
    /// A type reference.  OrigType is valid.
    Type,
    /// An invalid pattern.
    Invalid,
    /// A completely opaque abstraction pattern.
    Opaque,
    /// An open-coded tuple pattern.  OrigTupleElements is valid.
    /// OtherData is the number of tuple elements.
    Tuple,
    /// A type reference with a Clang type.  OrigType and ClangType are valid.
    ClangType,
    /// A reference to the parameters of a Clang function type,
    /// imported as a tuple type.  OrigType is valid and is a tuple
    /// type.  ClangType is valid and is a function type, a function
    /// pointer type, or a block pointer type.
    ClangFunctionParamTupleType,
    /// A type reference with an Objective-C method.  OrigType is
    /// valid and is a function type.  ObjCMethod is valid.
    ObjCMethodType,
    /// A reference to the uncurried parameters of a Clang Objective-C
    /// method type, imported as a tuple type.  OrigType is valid and
    /// is a tuple type with two elements.  ObjCMethod is valid.
    ObjCMethodParamTupleType,
    /// A reference to the formal parameters of a Clang Objective-C
    /// method type, imported as a tuple type.  OrigType is valid and
    /// is a tuple type.  ObjCMethod is valid.
    ObjCMethodFormalParamTupleType,
  };

  unsigned TheKind : 4;
  unsigned OtherData : 28;
  CanType OrigType;
  union {
    const clang::Type *ClangType;
    const clang::ObjCMethodDecl *ObjCMethod;
    const AbstractionPattern *OrigTupleElements;
  };

  static bool isOpaqueType(CanType type) {
    if (auto arch = dyn_cast<ArchetypeType>(type))
      return !arch->requiresClass();
    // FIXME: Check class constraint of dependent types in their originating
    // context
    if (isa<GenericTypeParamType>(type))
      return true;
    if (isa<DependentMemberType>(type))
      return true;
    return false;
  }

  Kind getKind() const { return Kind(TheKind); }

  unsigned getNumTupleElements_Stored() const {
    assert(getKind() == Kind::Tuple);
    return OtherData;
  }

  bool hasStoredClangType() const {
    return (getKind() == Kind::ClangType ||
            getKind() == Kind::ClangFunctionParamTupleType);
  }

  bool hasStoredObjCMethod() const {
    return (getKind() == Kind::ObjCMethodType ||
            getKind() == Kind::ObjCMethodParamTupleType ||
            getKind() == Kind::ObjCMethodFormalParamTupleType);
  }

  void initSwiftType(CanType origType) {
    if (isOpaqueType(origType)) {
      TheKind = unsigned(Kind::Opaque);
    } else {
      TheKind = unsigned(Kind::Type);
      OrigType = origType;
    }
  }

  void initClangType(CanType origType, const clang::Type *clangType,
                     Kind kind = Kind::ClangType) {
    assert(!isOpaqueType(origType));
    TheKind = unsigned(kind);
    OrigType = origType;
    ClangType = clangType;
  }

  void initObjCMethod(CanType origType, const clang::ObjCMethodDecl *method,
                      Kind kind) {
    assert(!isOpaqueType(origType));
    TheKind = unsigned(kind);
    OrigType = origType;
    ObjCMethod = method;
  }

  AbstractionPattern() {}
  explicit AbstractionPattern(Kind kind) : TheKind(unsigned(kind)) {}

public:
  explicit AbstractionPattern(Type origType)
    : AbstractionPattern(origType->getCanonicalType()) {}
  explicit AbstractionPattern(CanType origType) {
    initSwiftType(origType);
  }
  explicit AbstractionPattern(CanType origType, const clang::Type *clangType) {
    initClangType(origType, clangType);
  }

  static AbstractionPattern getOpaque() {
    return AbstractionPattern(Kind::Opaque);
  }

  static AbstractionPattern getInvalid() {
    return AbstractionPattern(Kind::Invalid);
  }

  /// Return an open-coded abstraction pattern for a tuple.  The
  /// caller is responsible for ensuring that the storage for the
  /// tuple elements is valid for as long as the abstraction pattern is.
  static AbstractionPattern getTuple(ArrayRef<AbstractionPattern> tuple) {
    AbstractionPattern pattern(Kind::Tuple);
    pattern.OtherData = tuple.size();
    pattern.OrigTupleElements = tuple.data();
    return pattern;
  }

  /// Return an abstraction pattern for a tuple representing all the
  /// parameters to a C or block function.
  static AbstractionPattern getClangFunctionParamTuple(CanType origType,
                                               const clang::Type *clangType) {
    assert(isa<TupleType>(origType));
    AbstractionPattern pattern;
    pattern.initClangType(origType, clangType, Kind::ClangFunctionParamTupleType);
    return pattern;
  }

  /// Return an abstraction pattern for the type of an Objective-C method.
  static AbstractionPattern getObjCMethod(CanType origType,
                                          const clang::ObjCMethodDecl *method) {
    assert(isa<AnyFunctionType>(origType));
    AbstractionPattern pattern;
    pattern.initObjCMethod(origType, method, Kind::ObjCMethodType);
    return pattern;
  }

  /// Return an abstraction pattern for a tuple representing the
  /// uncurried parameter clauses of an Objective-C method.
  static AbstractionPattern getObjCMethodParamTuple(CanType origType,
                                          const clang::ObjCMethodDecl *method) {
    assert(isa<TupleType>(origType));
    assert(cast<TupleType>(origType)->getNumElements() == 2);
    AbstractionPattern pattern;
    pattern.initObjCMethod(origType, method, Kind::ObjCMethodParamTupleType);
    return pattern;
  }

  /// Return an abstraction pattern for a tuple representing the
  /// formal parameters to an Objective-C method.
  static AbstractionPattern getObjCMethodFormalParamTuple(CanType origType,
                                          const clang::ObjCMethodDecl *method) {
    assert(isa<TupleType>(origType));
    AbstractionPattern pattern;
    pattern.initObjCMethod(origType, method,
                           Kind::ObjCMethodFormalParamTupleType);
    return pattern;
  }

  /// Return an abstraction pattern with an added level of optionality.
  ///
  /// The based abstraction pattern must be either opaque or based on
  /// a Clang or Swift type.  That is, it cannot be a tuple or an ObjC
  /// method type.
  static AbstractionPattern getOptional(AbstractionPattern objectPattern,
                                        OptionalTypeKind optionalKind);

  /// Does this abstraction pattern have something that can be used as a key?
  bool hasCachingKey() const {
    // Only the simplest Kind::Type pattern has a caching key; we
    // don't want to try to unique by Clang node.
    return getKind() == Kind::Type || getKind() == Kind::Opaque;
  }
  using CachingKey = CanType;
  CachingKey getCachingKey() const {
    assert(hasCachingKey());
    return OrigType;
  }

  bool isValid() const {
    return getKind() != Kind::Invalid;
  }

  /// Is this abstraction pattern fully opaque?
  bool isOpaque() const {
    assert(isValid());
    return getKind() == Kind::Opaque;
  }

  /// Return the Swift type which provides structure for this
  /// abstraction pattern.
  ///
  /// This is always valid unless the pattern is opaque or an
  /// open-coded tuple.  However, it does not always fully describe
  /// the abstraction pattern.
  CanType getType() const {
    switch (getKind()) {
    case Kind::Invalid:
      llvm_unreachable("querying invalid abstraction pattern!");
    case Kind::Opaque:
      llvm_unreachable("opaque pattern has no type");
    case Kind::Tuple:
      llvm_unreachable("open-coded tuple pattern has no type");
    case Kind::ClangType:
    case Kind::ClangFunctionParamTupleType:
    case Kind::ObjCMethodType:
    case Kind::ObjCMethodParamTupleType:
    case Kind::ObjCMethodFormalParamTupleType:
    case Kind::Type:
      return OrigType;
    }
    llvm_unreachable("bad kind");
  }

  /// Return whether this abstraction pattern contains foreign type
  /// information.
  ///
  /// In general, after eliminating tuples, a foreign abstraction
  /// pattern will satisfy either isClangType() or isObjCMethod().
  bool isForeign() const {
    switch (getKind()) {
    case Kind::Invalid:
      llvm_unreachable("querying invalid abstraction pattern!");
    case Kind::Opaque:
    case Kind::Tuple:
    case Kind::Type:
      return false;
    case Kind::ClangType:
    case Kind::ClangFunctionParamTupleType:
    case Kind::ObjCMethodType:
    case Kind::ObjCMethodParamTupleType:
    case Kind::ObjCMethodFormalParamTupleType:
      return true;
    }
    llvm_unreachable("bad kind");
  }

  /// Return whether this abstraction pattern represents a Clang type.
  /// If so, it is legal to return getClangType().
  bool isClangType() const {
    return (getKind() == Kind::ClangType);
  }

  const clang::Type *getClangType() const {
    assert(hasStoredClangType());
    return ClangType;
  }

  /// Return whether this abstraction pattern represents an
  /// Objective-C method.  If so, it is legal to call getObjCMethod().
  bool isObjCMethod() const {
    return (getKind() == Kind::ObjCMethodType);
  }

  const clang::ObjCMethodDecl *getObjCMethod() const {
    assert(hasStoredObjCMethod());
    return ObjCMethod;
  }

  template<typename TYPE>
  typename CanTypeWrapperTraits<TYPE>::type
  getAs() const {
    switch (getKind()) {
    case Kind::Invalid:
      llvm_unreachable("querying invalid abstraction pattern!");
    case Kind::Opaque:
      return typename CanTypeWrapperTraits<TYPE>::type();
    case Kind::Tuple:
      return typename CanTypeWrapperTraits<TYPE>::type();
    case Kind::ClangType:
    case Kind::ClangFunctionParamTupleType:
    case Kind::ObjCMethodType:
    case Kind::ObjCMethodParamTupleType:
    case Kind::ObjCMethodFormalParamTupleType:
    case Kind::Type:
      return dyn_cast<TYPE>(getType());
    }
    llvm_unreachable("bad kind");
  }

  /// Is this pattern the exact given type?
  ///
  /// This is only useful for avoiding redundant work at compile time;
  /// code should be prepared to do the right thing in the face of a slight
  /// mismatch.  This may happen for any number of reasons.
  bool isExactType(CanType type) const {
    switch (getKind()) {
    case Kind::Invalid:
      llvm_unreachable("querying invalid abstraction pattern!");
    case Kind::Opaque:
    case Kind::Tuple:
    case Kind::ClangType:
    case Kind::ClangFunctionParamTupleType:
    case Kind::ObjCMethodType:
    case Kind::ObjCMethodParamTupleType:
    case Kind::ObjCMethodFormalParamTupleType:
      // We assume that the Clang type might provide additional structure.
      return false;
    case Kind::Type:
      return getType() == type;
    }
    llvm_unreachable("bad kind");
  }

  AbstractionPattern transformType(llvm::function_ref<CanType(CanType)>) const;

  /// Is the given tuple type a valid substitution of this abstraction
  /// pattern?
  bool matchesTuple(CanTupleType substType);

  bool isTuple() {
    switch (getKind()) {
    case Kind::Invalid:
      llvm_unreachable("querying invalid abstraction pattern!");
    case Kind::Opaque:
    case Kind::ObjCMethodType:
      return false;
    case Kind::Tuple:
    case Kind::ClangFunctionParamTupleType:
    case Kind::ObjCMethodParamTupleType:
    case Kind::ObjCMethodFormalParamTupleType:
      return true;
    case Kind::Type:
    case Kind::ClangType:
      return isa<TupleType>(getType());
    }
    llvm_unreachable("bad kind");
  }

  size_t getNumTupleElements() const {
    switch (getKind()) {
    case Kind::Invalid:
      llvm_unreachable("querying invalid abstraction pattern!");
    case Kind::Opaque:
    case Kind::ObjCMethodType:
      llvm_unreachable("pattern is not a tuple");      
    case Kind::Tuple:
      return getNumTupleElements_Stored();
    case Kind::Type:
    case Kind::ClangType:
    case Kind::ClangFunctionParamTupleType:
    case Kind::ObjCMethodParamTupleType:
    case Kind::ObjCMethodFormalParamTupleType:
      return cast<TupleType>(getType())->getNumElements();
    }
    llvm_unreachable("bad kind");
  }

  AbstractionPattern dropLastTupleElement() const;

  class TupleElementRange;

  /// Return a range over the tuple elements.
  TupleElementRange getTupleElements() const;

  /// Given that the value being abstracted is a tuple type, return
  /// the abstraction pattern for its object type.
  AbstractionPattern getTupleElementType(unsigned index) const;

  /// Given that the value being abstracted is an l-value type, return
  /// the abstraction pattern for its object type.
  AbstractionPattern getLValueObjectType() const;

  /// Given that the value being abstracted is a function, return the
  /// abstraction pattern for its result type.
  AbstractionPattern getFunctionResultType() const;

  /// Given that the value being abstracted is a function, return the
  /// abstraction pattern for its input type.
  AbstractionPattern getFunctionInputType() const;

  /// If this pattern refers to a reference storage type, look through
  /// it.
  AbstractionPattern getReferenceStorageReferentType() const;

  void dump() const LLVM_ATTRIBUTE_USED;
  void print(raw_ostream &OS) const;
};

/// A range of abstraction patterns for the tuple elements of an
/// abstraction pattern.
class AbstractionPattern::TupleElementRange {
  AbstractionPattern Parent;
  unsigned NumElements;
public:
  TupleElementRange(AbstractionPattern parent, unsigned numElements)
    : Parent(parent), NumElements(numElements) {}

  struct iterator {
    const TupleElementRange *Range;
    unsigned Index;

    AbstractionPattern operator*() const {
      return Range->Parent.getTupleElementType(Index);
    }

    iterator &operator++() {
      assert(Index < Range->NumElements);
      Index++;
      return *this;
    }
    iterator operator++(int _) {
      iterator saved = *this;
      operator++();
      return saved;
    }

    bool operator==(const iterator &other) const {
      assert(Range == other.Range);
      return Index == other.Index;
    }
    bool operator!=(const iterator &other) const {
      return !operator==(other);
    }
  };

  iterator begin() const { return { this, 0 }; }
  iterator end() const { return { this, NumElements }; }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                     const AbstractionPattern &pattern) {
  pattern.print(out);
  return out;
}

inline AbstractionPattern::TupleElementRange
AbstractionPattern::getTupleElements() const {
  return TupleElementRange(*this, getNumTupleElements());
}

}
}

#endif
