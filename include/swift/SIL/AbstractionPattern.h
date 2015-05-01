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
  class ObjCMethodDecl;
  class Type;
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
    /// The curried imported type of an Objective-C method (that is,
    /// 'Self -> Input -> Result').  OrigType is valid and is a function
    /// type.  ObjCMethod is valid.  OtherData is an encoded foreign
    /// error index.
    CurriedObjCMethodType,
    /// The partially-applied curried imported type of an Objective-C
    /// method (that is, 'Input -> Result').  OrigType is valid and is a
    /// function type.  ObjCMethod is valid.  OtherData is an encoded
    /// foreign error index.
    PartialCurriedObjCMethodType,
    /// The uncurried imported type of an Objective-C method (that is,
    /// '(Input, Self) -> Result').  OrigType is valid and is a function
    /// type.  ObjCMethod is valid.  OtherData is an encoded foreign
    /// error index.
    ObjCMethodType,
    /// A reference to the uncurried parameters of a Clang Objective-C
    /// method type, imported as a tuple type (that is, '(Input,
    /// Self').  OrigType is valid and is a tuple type with two
    /// elements.  ObjCMethod is valid.  OtherData is an encoded
    /// foreign error index.
    ObjCMethodParamTupleType,
    /// A reference to the formal parameters of a Clang Objective-C
    /// method type when they were imported as a tuple type (that is,
    /// 'Input', if it's a tuple type).  OrigType is valid and is a
    /// tuple type.  ObjCMethod is valid.  OtherData is an encoded
    /// foreign error index.
    ObjCMethodFormalParamTupleType,
  };

  class EncodedForeignErrorInfo {
    unsigned Value;

  public:
    EncodedForeignErrorInfo() : Value(0) {}
    EncodedForeignErrorInfo(unsigned errorParameterIndex,
                            bool stripsResultOptionality)
      : Value(1 + unsigned(stripsResultOptionality) +
              (errorParameterIndex << 1)) {}

    bool hasValue() const { return Value != 0; }
    bool hasErrorParameter() const { return hasValue(); }
    bool stripsResultOptionality() const {
      assert(hasValue());
      return (Value - 1) & 1;
    }
    unsigned getErrorParameterIndex() const {
      assert(hasValue());
      return (Value - 1) >> 1;
    }

    unsigned getOpaqueValue() const { return Value; }
    static EncodedForeignErrorInfo fromOpaqueValue(unsigned value) {
      EncodedForeignErrorInfo result;
      result.Value = value;
      return result;
    }
  };

  unsigned TheKind : 4;
  unsigned OtherData : 28;
  CanType OrigType;
  union {
    const clang::Type *ClangType;
    const clang::ObjCMethodDecl *ObjCMethod;
    const AbstractionPattern *OrigTupleElements;
  };
  CanGenericSignature GenericSig;

  static bool isOpaqueType(CanGenericSignature signature, CanType type) {
    assert(signature || !type->isDependentType());
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

  CanGenericSignature getGenericSignature() const {
    assert(getKind() == Kind::Type ||
           hasStoredClangType() ||
           hasStoredObjCMethod());
    return CanGenericSignature(GenericSig);
  }

  CanGenericSignature getGenericSignatureForFunctionComponent() const {
    if (auto genericFn = dyn_cast<GenericFunctionType>(getType())) {
      return genericFn.getGenericSignature();
    } else {
      return getGenericSignature();
    }
  }

  unsigned getNumTupleElements_Stored() const {
    assert(getKind() == Kind::Tuple);
    return OtherData;
  }

  bool hasStoredClangType() const {
    return (getKind() == Kind::ClangType ||
            getKind() == Kind::ClangFunctionParamTupleType);
  }

  bool hasStoredObjCMethod() const {
    return (getKind() == Kind::CurriedObjCMethodType ||
            getKind() == Kind::PartialCurriedObjCMethodType ||
            getKind() == Kind::ObjCMethodType ||
            getKind() == Kind::ObjCMethodParamTupleType ||
            getKind() == Kind::ObjCMethodFormalParamTupleType);
  }

  bool hasStoredForeignErrorInfo() const {
    return hasStoredObjCMethod();
  }

  void initSwiftType(CanGenericSignature signature, CanType origType) {
    assert(signature || !origType->isDependentType());
    if (isOpaqueType(signature, origType)) {
      TheKind = unsigned(Kind::Opaque);
    } else {
      TheKind = unsigned(Kind::Type);
      OrigType = origType;
      GenericSig = signature;
    }
  }

  void initClangType(CanGenericSignature signature,
                     CanType origType, const clang::Type *clangType,
                     Kind kind = Kind::ClangType) {
    assert(!isOpaqueType(signature, origType));
    TheKind = unsigned(kind);
    OrigType = origType;
    ClangType = clangType;
    GenericSig = signature;
  }

  void initObjCMethod(CanGenericSignature signature,
                      CanType origType, const clang::ObjCMethodDecl *method,
                      Kind kind, EncodedForeignErrorInfo errorInfo) {
    assert(!isOpaqueType(signature, origType));
    TheKind = unsigned(kind);
    OrigType = origType;
    ObjCMethod = method;
    OtherData = errorInfo.getOpaqueValue();
    GenericSig = signature;
  }

  AbstractionPattern() {}
  explicit AbstractionPattern(Kind kind) : TheKind(unsigned(kind)) {}

public:
  explicit AbstractionPattern(Type origType)
    : AbstractionPattern(origType->getCanonicalType()) {}
  explicit AbstractionPattern(CanType origType)
    : AbstractionPattern(nullptr, origType) {}
  explicit AbstractionPattern(CanGenericSignature signature, CanType origType) {
    initSwiftType(signature, origType);
  }
  explicit AbstractionPattern(CanType origType, const clang::Type *clangType)
    : AbstractionPattern(nullptr, origType, clangType) {}
  explicit AbstractionPattern(CanGenericSignature signature, CanType origType,
                              const clang::Type *clangType) {
    initClangType(signature, origType, clangType);
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

private:
  /// Return an abstraction pattern for a tuple representing all the
  /// parameters to a C or block function.
  static AbstractionPattern
  getClangFunctionParamTuple(CanGenericSignature signature, CanType origType,
                             const clang::Type *clangType) {
    assert(isa<TupleType>(origType));
    AbstractionPattern pattern;
    pattern.initClangType(signature, origType, clangType,
                          Kind::ClangFunctionParamTupleType);
    return pattern;
  }

public:
  /// Return an abstraction pattern for the curried type of an
  /// Objective-C method.
  static AbstractionPattern
  getCurriedObjCMethod(CanType origType, const clang::ObjCMethodDecl *method,
                       const Optional<ForeignErrorConvention> &foreignError);

private:
  /// Return an abstraction pattern for the curried type of an
  /// Objective-C method.
  static AbstractionPattern
  getCurriedObjCMethod(CanType origType, const clang::ObjCMethodDecl *method,
                       EncodedForeignErrorInfo errorInfo) {
    assert(isa<AnyFunctionType>(origType));
    AbstractionPattern pattern;
    pattern.initObjCMethod(nullptr, origType, method,
                           Kind::CurriedObjCMethodType, errorInfo);
    return pattern;
  }

  /// Return an abstraction pattern for the partially-applied curried
  /// type of an Objective-C method.
  static AbstractionPattern
  getPartialCurriedObjCMethod(CanGenericSignature signature,
                              CanType origType,
                              const clang::ObjCMethodDecl *method,
                              EncodedForeignErrorInfo errorInfo) {
    assert(isa<AnyFunctionType>(origType));
    AbstractionPattern pattern;
    pattern.initObjCMethod(signature, origType, method,
                           Kind::PartialCurriedObjCMethodType, errorInfo);
    return pattern;
  }

public:
  /// Return an abstraction pattern for the type of an Objective-C method.
  static AbstractionPattern
  getObjCMethod(CanType origType, const clang::ObjCMethodDecl *method,
                const Optional<ForeignErrorConvention> &foreignError);

private:
  /// Return an abstraction pattern for the uncurried type of an
  /// Objective-C method.
  static AbstractionPattern
  getObjCMethod(CanType origType, const clang::ObjCMethodDecl *method,
                EncodedForeignErrorInfo errorInfo) {
    assert(isa<AnyFunctionType>(origType));
    AbstractionPattern pattern;
    pattern.initObjCMethod(nullptr, origType, method, Kind::ObjCMethodType,
                           errorInfo);
    return pattern;
  }

  /// Return an abstraction pattern for a tuple representing the
  /// uncurried parameter clauses of an Objective-C method.
  static AbstractionPattern
  getObjCMethodParamTuple(CanGenericSignature signature, CanType origType,
                          const clang::ObjCMethodDecl *method,
                          EncodedForeignErrorInfo errorInfo) {
    assert(isa<TupleType>(origType));
    assert(cast<TupleType>(origType)->getNumElements() == 2);
    AbstractionPattern pattern;
    pattern.initObjCMethod(signature, origType, method,
                           Kind::ObjCMethodParamTupleType, errorInfo);
    return pattern;
  }

  /// Return a pattern correspond to the 'self' parameter of the
  /// current Objective-C method.
  AbstractionPattern getObjCMethodSelfPattern(CanType paramType) const;

  /// Return a pattern correspond to the formal parameters of the
  /// current Objective-C method.
  AbstractionPattern getObjCMethodFormalParamPattern(CanType paramType) const;

  /// Return an abstraction pattern for a tuple representing the
  /// formal parameters to an Objective-C method.
  static AbstractionPattern
  getObjCMethodFormalParamTuple(CanGenericSignature signature, CanType origType,
                                const clang::ObjCMethodDecl *method,
                                EncodedForeignErrorInfo errorInfo) {
    assert(isa<TupleType>(origType));
    AbstractionPattern pattern;
    pattern.initObjCMethod(signature, origType, method,
                           Kind::ObjCMethodFormalParamTupleType, errorInfo);
    return pattern;
  }

public:
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
    case Kind::CurriedObjCMethodType:
    case Kind::PartialCurriedObjCMethodType:
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
    case Kind::PartialCurriedObjCMethodType:
    case Kind::CurriedObjCMethodType:
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
    return (getKind() == Kind::ObjCMethodType ||
            getKind() == Kind::CurriedObjCMethodType);
  }

  const clang::ObjCMethodDecl *getObjCMethod() const {
    assert(hasStoredObjCMethod());
    return ObjCMethod;
  }

  EncodedForeignErrorInfo getEncodedForeignErrorInfo() const {
    assert(hasStoredForeignErrorInfo());
    return EncodedForeignErrorInfo::fromOpaqueValue(OtherData);
  }

  bool hasForeignErrorStrippingResultOptionality() const {
    switch (getKind()) {
    case Kind::Invalid:
      llvm_unreachable("querying invalid abstraction pattern!");
    case Kind::Tuple:
    case Kind::ClangFunctionParamTupleType:
    case Kind::ObjCMethodParamTupleType:
    case Kind::ObjCMethodFormalParamTupleType:
      llvm_unreachable("querying foreign-error bits on non-function pattern");

    case Kind::Opaque:
    case Kind::ClangType:
    case Kind::Type:
      return false;
    case Kind::PartialCurriedObjCMethodType:
    case Kind::CurriedObjCMethodType:
    case Kind::ObjCMethodType: {
      auto errorInfo = getEncodedForeignErrorInfo();
      return (errorInfo.hasValue() && errorInfo.stripsResultOptionality());
    }
    }
    llvm_unreachable("bad kind");
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
    case Kind::PartialCurriedObjCMethodType:
    case Kind::CurriedObjCMethodType:
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
    case Kind::PartialCurriedObjCMethodType:
    case Kind::CurriedObjCMethodType:
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
    case Kind::PartialCurriedObjCMethodType:
    case Kind::CurriedObjCMethodType:
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
    case Kind::PartialCurriedObjCMethodType:
    case Kind::CurriedObjCMethodType:
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
