//===--- AbstractionPattern.h - SIL type abstraction patterns ---*- C++ -*-===//
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
  class ValueDecl;
  class ObjCMethodDecl;
  class Type;
}

namespace swift {
namespace Lowering {

/// A pattern for the abstraction of a value.  See the large comment
/// in SILGenPoly.cpp.
///
/// The representation of values in Swift can vary according to how
/// their type is abstracted: which is to say, according to the pattern
/// of opaque type variables within their type.  The main motivation
/// here is performance: it would be far easier for types to adopt a
/// single representation regardless of their abstraction, but this
/// would force Swift to adopt a very inefficient representation for
/// abstractable values.
///
/// For example, consider the comparison function on Int:
///   func <(lhs : Int, rhs : Int) -> Bool
///
/// This function can be used as an opaque value of type
/// (Int,Int)->Bool.  An optimal representation of values of that type
/// (ignoring context parameters for the moment) would be a pointer to
/// a function that takes these two arguments directly in registers and
/// returns the result directly in a register.
///
/// (It's important to remember throughout this discussion that we're
/// talking about abstract values.  There's absolutely nothing that
/// requires direct uses of the function to follow the same conventions
/// as abstract uses!  A direct use of a declaration --- even one that
/// implies an indirect call, like a class's instance method ---
/// provides a concrete specification for exactly how to interact with
/// value.)
///
/// However, that representation is problematic in the presence of
/// generics.  This function could be passed off to any of the following
/// generic functions:
///   func foo<T>(f : (T, Int) -> Bool)
///   func bar<U,V>(f : (U, V) -> Bool)
///   func baz<W>(f : (Int, Int) -> W)
///
/// These generic functions all need to be able to call 'f'.  But in
/// Swift's implementation model, these functions don't have to be
/// instantiated for different parameter types, which means that (e.g.)
/// the same 'baz' implementation needs to also be able to work when
/// W=String.  But the optimal way to pass an Int to a function might
/// well be different from the optimal way to pass a String.
///
/// And this runs in both directions: a generic function might return
/// a function that the caller would like to use as an (Int,Int)->Bool:
///   func getFalseFunction<T>() -> (T,T)->Bool
///
/// There are three ways we can deal with this:
///
/// 1. Give all types in Swift a common representation.  The generic
/// implementation can work with both W=String and W=Int because
/// both of those types have the same (direct) storage representation.
/// That's pretty clearly not an acceptable sacrifice.
///
/// 2. Adopt a most-general representation of function types that is
/// used for opaque values; for example, all parameters and results
/// could be passed indirectly.  Concrete values must be coerced to
/// this representation when made abstract.  Unfortunately, there
/// are a lot of obvious situations where this is sub-optimal:
/// for example, in totally non-generic code that just passes around
/// a value of type (Int,Int)->Bool.  It's particularly bad because
/// Swift functions take multiple arguments as just a tuple, and that
/// tuple is usually abstractable: e.g., '<' above could also be
/// passed to this:
///   func fred<T>(f : T -> Bool)
///
/// 3. Permit the representation of values to vary by abstraction.
/// Values require coercion when changing abstraction patterns.
/// For example, the argument to 'fred' would be expected to return
/// its Bool result directly but take a single T parameter indirectly.
/// When '<' is passed to this, what must actually be passed is a
/// thunk that expects a tuple of type (Int,Int) to be stored at
/// the input address.
///
/// There is one major risk with (3): naively implemented, a single
/// function value which undergoes many coercions could build up a
/// linear number of re-abstraction thunks.  However, this can be
/// solved dynamically by applying thunks with a runtime function that
/// can recognize and bypass its own previous handiwork.
///
/// There is one major exception to what sub-expressions in a type
/// expression can be abstracted with type variables: a type substitution
/// must always be materializable.  For example:
///   func f(inout Int, Int) -> Bool
/// 'f' cannot be passed to 'foo' above: T=inout Int is not a legal
/// substitution.  Nor can it be passed to 'fred'.
///
/// In general, abstraction patterns are derived from some explicit
/// type expression, such as the written type of a variable or
/// parameter.  This works whenever the expression directly provides
/// structure for the type in question; for example, when the original
/// type is (T,Int)->Bool and we are working with an (Int,Int)->Bool
/// substitution.  However, it is inadequate when the expression does
/// not provide structure at the appropriate level, i.e. when that
/// level is substituted in: when the original type is merely T.  In
/// these cases, we must devolve to a representation which all legal
/// substitutors will agree upon.  In general, this is the
/// representation of the type which replaces all materializable
/// sub-expressions with a fresh type variable.
///
/// For example, when applying the substitution
///   T=(Int,Int)->Bool
/// values of T are abstracted as if they were of type U->V, i.e.
/// taking one indirect parameter and returning one indirect result.
///
/// But under the substitution
///   T=(inout Int,Int)->Bool
/// values of T are abstracted as if they were of type (inout U,V)->W,
/// i.e. taking one parameter inout, another indirectly, and returning
/// one indirect result.
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
    /// A discarded value. OrigType is valid.
    Discard,
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
    /// The curried imported type of a C function imported as a method.
    /// OrigType is valid and is a function type. ClangType is valid and is
    /// a function type. OtherData is the offset of the parameter imported as
    /// `self`, or -1 if the function was imported as a static method.
    CurriedCFunctionAsMethodType,
    /// The partially-applied curried imported type of a C function imported as
    /// a method.
    /// OrigType is valid and is a function type. ClangType is valid and is
    /// a function type. OtherData is the offset of the parameter imported as
    /// `self`, or ~0 if the function was imported as a static method.
    PartialCurriedCFunctionAsMethodType,
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
    /// A reference to the formal method parameters of a C function that was
    /// imported as a method.
    /// OrigType is valid and is a tuple type. ClangType is valid and is
    /// a function type. OtherData is the offset of the parameter imported as
    /// `self`, or ~0 if the function was imported as a static method.
    CFunctionAsMethodFormalParamTupleType,
  };

  class EncodedForeignErrorInfo {
    unsigned Value;

  public:
    EncodedForeignErrorInfo() : Value(0) {}
    EncodedForeignErrorInfo(unsigned errorParameterIndex,
                            bool replaceParamWithVoid,
                            bool stripsResultOptionality)
      : Value(1 +
              (unsigned(stripsResultOptionality)) +
              (unsigned(replaceParamWithVoid) << 1) +
              (errorParameterIndex << 2)) {}

    static EncodedForeignErrorInfo
    encode(const Optional<ForeignErrorConvention> &foreignError);

    bool hasValue() const { return Value != 0; }
    bool hasErrorParameter() const { return hasValue(); }
    bool hasUnreplacedErrorParameter() const {
      return hasValue() && !isErrorParameterReplacedWithVoid();
    }

    bool stripsResultOptionality() const {
      assert(hasValue());
      return (Value - 1) & 1;
    }

    bool isErrorParameterReplacedWithVoid() const {
      assert(hasValue());
      return (Value - 1) & 2;
    }

    unsigned getErrorParameterIndex() const {
      assert(hasValue());
      return (Value - 1) >> 2;
    }

    unsigned getOpaqueValue() const { return Value; }
    static EncodedForeignErrorInfo fromOpaqueValue(unsigned value) {
      EncodedForeignErrorInfo result;
      result.Value = value;
      return result;
    }
  };

  static constexpr const unsigned NumOtherDataBits = 28;
  static constexpr const unsigned MaxOtherData = (1 << NumOtherDataBits) - 1;

  unsigned TheKind : 32 - NumOtherDataBits;
  unsigned OtherData : NumOtherDataBits;
  CanType OrigType;
  union {
    const clang::Type *ClangType;
    const clang::ObjCMethodDecl *ObjCMethod;
    const AbstractionPattern *OrigTupleElements;
  };
  CanGenericSignature GenericSig;

  Kind getKind() const { return Kind(TheKind); }

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
            getKind() == Kind::ClangFunctionParamTupleType ||
            getKind() == Kind::CurriedCFunctionAsMethodType ||
            getKind() == Kind::PartialCurriedCFunctionAsMethodType ||
            getKind() == Kind::CFunctionAsMethodFormalParamTupleType);
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

  bool hasImportAsMemberStatus() const {
    return getKind() == Kind::CurriedCFunctionAsMethodType ||
           getKind() == Kind::PartialCurriedCFunctionAsMethodType ||
           getKind() == Kind::CFunctionAsMethodFormalParamTupleType;
  }
  
  void initSwiftType(CanGenericSignature signature, CanType origType,
                     Kind kind = Kind::Type) {
    assert(signature || !origType->hasTypeParameter());
    TheKind = unsigned(kind);
    OrigType = origType;
    GenericSig = CanGenericSignature();
    if (origType->hasTypeParameter())
      GenericSig = signature;
  }

  void initClangType(CanGenericSignature signature,
                     CanType origType, const clang::Type *clangType,
                     Kind kind = Kind::ClangType) {
    initSwiftType(signature, origType, kind);
    ClangType = clangType;
  }

  void initObjCMethod(CanGenericSignature signature,
                      CanType origType, const clang::ObjCMethodDecl *method,
                      Kind kind, EncodedForeignErrorInfo errorInfo) {
    initSwiftType(signature, origType, kind);
    ObjCMethod = method;
    OtherData = errorInfo.getOpaqueValue();
  }

  void initCFunctionAsMethod(CanGenericSignature signature,
                             CanType origType, const clang::Type *clangType,
                             Kind kind,
                             ImportAsMemberStatus memberStatus) {
    initClangType(signature, origType, clangType, kind);
    OtherData = memberStatus.rawValue;
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

  bool hasGenericSignature() const {
    return (getKind() == Kind::Type ||
            getKind() == Kind::Discard ||
            hasStoredClangType() ||
            hasStoredObjCMethod());
  }

  CanGenericSignature getGenericSignature() const {
    assert(getKind() == Kind::Type ||
           getKind() == Kind::Discard ||
           hasStoredClangType() ||
           hasStoredObjCMethod());
    return CanGenericSignature(GenericSig);
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

  /// Return an abstraction pattern for the curried type of a
  /// C function imported as a method.
  static AbstractionPattern
  getCurriedCFunctionAsMethod(CanType origType,
                              const AbstractFunctionDecl *function);

  /// For a C-function-as-method pattern,
  /// get the index of the C function parameter that was imported as the
  /// `self` parameter of the imported method, or None if this is a static
  /// method with no `self` parameter.
  ImportAsMemberStatus getImportAsMemberStatus() const {
    assert(hasImportAsMemberStatus());
    ImportAsMemberStatus status;
    status.rawValue = OtherData;
    return status;
  }
  
  /// Return an abstraction pattern for a value that is discarded after being
  /// evaluated.
  static AbstractionPattern
  getDiscard(CanGenericSignature signature, CanType origType) {
    AbstractionPattern pattern;
    pattern.initSwiftType(signature, origType, Kind::Discard);
    return pattern;
  }
  
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
  
  static AbstractionPattern
  getCurriedCFunctionAsMethod(CanType origType,
                              const clang::Type *clangType,
                              ImportAsMemberStatus memberStatus) {
    assert(isa<AnyFunctionType>(origType));
    AbstractionPattern pattern;
    pattern.initCFunctionAsMethod(nullptr, origType, clangType,
                                  Kind::CurriedCFunctionAsMethodType,
                                  memberStatus);
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

  static AbstractionPattern
  getPartialCurriedCFunctionAsMethod(CanGenericSignature signature,
                                     CanType origType,
                                     const clang::Type *clangType,
                                     ImportAsMemberStatus memberStatus) {
    assert(isa<AnyFunctionType>(origType));
    AbstractionPattern pattern;
    pattern.initCFunctionAsMethod(signature, origType, clangType,
                                  Kind::PartialCurriedCFunctionAsMethodType,
                                  memberStatus);
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

  /// Return a pattern corresponding to the 'self' parameter of the
  /// current Objective-C method.
  AbstractionPattern getObjCMethodSelfPattern(CanType paramType) const;

  /// Return a pattern corresponding to the formal parameters of the
  /// current Objective-C method.
  AbstractionPattern getObjCMethodFormalParamPattern(CanType paramType) const;
  
  /// Return a pattern corresponding to the 'self' parameter of the
  /// current C function imported as a method.
  AbstractionPattern getCFunctionAsMethodSelfPattern(CanType paramType) const;
  
  /// Return a pattern corresponding to the formal parameters of the
  /// current C function imported as a method.
  AbstractionPattern getCFunctionAsMethodFormalParamPattern(CanType paramType)
    const;

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

  /// Return an abstraction pattern for a tuple representing the
  /// formal method parameters to a C function imported as a method.
  static AbstractionPattern
  getCFunctionAsMethodFormalParamTuple(CanGenericSignature signature,
                                       CanType origType,
                                       const clang::Type *type,
                                       ImportAsMemberStatus memberStatus) {
    assert(isa<TupleType>(origType));
    AbstractionPattern pattern;
    pattern.initCFunctionAsMethod(signature, origType, type,
                                  Kind::CFunctionAsMethodFormalParamTupleType,
                                  memberStatus);
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
    return getKind() == Kind::Type || getKind() == Kind::Opaque
        || getKind() == Kind::Discard;
  }
  using CachingKey = CanType;
  CachingKey getCachingKey() const {
    assert(hasCachingKey());
    return OrigType;
  }

  bool isValid() const {
    return getKind() != Kind::Invalid;
  }

  bool isTypeParameter() const {
    switch (getKind()) {
    case Kind::Opaque:
      return true;
    case Kind::Type:
    case Kind::Discard: {
      auto type = getType();
      if (isa<ArchetypeType>(type) ||
          isa<DependentMemberType>(type) ||
          isa<GenericTypeParamType>(type)) {
        return true;
      }
      return false;
    }
    default:
      return false;
    }
  }

  /// Is this an interface type that is subject to a concrete
  /// same-type constraint?
  bool isConcreteType(ModuleDecl &module) const {
    assert(isTypeParameter());
    return (getKind() != Kind::Opaque &&
            GenericSig != nullptr &&
            GenericSig->isConcreteType(getType(), module));
  }

  bool requiresClass(ModuleDecl &module) {
    switch (getKind()) {
    case Kind::Opaque:
      return false;
    case Kind::Type:
    case Kind::Discard: {
      auto type = getType();
      if (auto archetype = dyn_cast<ArchetypeType>(type))
        return archetype->requiresClass();
      else if (isa<DependentMemberType>(type) ||
               isa<GenericTypeParamType>(type)) {
        assert(GenericSig &&
               "Dependent type in pattern without generic signature?");
        return GenericSig->requiresClass(type, module);
      }
      return false;
    }
    default:
      return false;
    }
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
    case Kind::CurriedCFunctionAsMethodType:
    case Kind::PartialCurriedCFunctionAsMethodType:
    case Kind::CFunctionAsMethodFormalParamTupleType:
    case Kind::Type:
    case Kind::Discard:
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
    case Kind::Discard:
      return false;
    case Kind::ClangType:
    case Kind::ClangFunctionParamTupleType:
    case Kind::PartialCurriedObjCMethodType:
    case Kind::CurriedObjCMethodType:
    case Kind::ObjCMethodType:
    case Kind::ObjCMethodParamTupleType:
    case Kind::ObjCMethodFormalParamTupleType:
    case Kind::CurriedCFunctionAsMethodType:
    case Kind::PartialCurriedCFunctionAsMethodType:
    case Kind::CFunctionAsMethodFormalParamTupleType:
      return true;
    }
    llvm_unreachable("bad kind");
  }

  /// True if the value is discarded.
  bool isDiscarded() const {
    return getKind() == Kind::Discard;
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
    case Kind::Discard:
    case Kind::CurriedCFunctionAsMethodType:
    case Kind::PartialCurriedCFunctionAsMethodType:
    case Kind::CFunctionAsMethodFormalParamTupleType:
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
    case Kind::CurriedCFunctionAsMethodType:
    case Kind::PartialCurriedCFunctionAsMethodType:
    case Kind::CFunctionAsMethodFormalParamTupleType:
    case Kind::Type:
    case Kind::Discard:
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
    case Kind::CurriedCFunctionAsMethodType:
    case Kind::PartialCurriedCFunctionAsMethodType:
    case Kind::CFunctionAsMethodFormalParamTupleType:
      // We assume that the Clang type might provide additional structure.
      return false;
    case Kind::Type:
    case Kind::Discard:
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
    case Kind::CurriedCFunctionAsMethodType:
    case Kind::PartialCurriedCFunctionAsMethodType:
    case Kind::ObjCMethodType:
      return false;
    case Kind::Tuple:
    case Kind::ClangFunctionParamTupleType:
    case Kind::ObjCMethodParamTupleType:
    case Kind::ObjCMethodFormalParamTupleType:
    case Kind::CFunctionAsMethodFormalParamTupleType:
      return true;
    case Kind::Type:
    case Kind::Discard:
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
    case Kind::CurriedCFunctionAsMethodType:
    case Kind::PartialCurriedCFunctionAsMethodType:
    case Kind::ObjCMethodType:
      llvm_unreachable("pattern is not a tuple");      
    case Kind::Tuple:
      return getNumTupleElements_Stored();
    case Kind::Type:
    case Kind::Discard:
    case Kind::ClangType:
    case Kind::ClangFunctionParamTupleType:
    case Kind::ObjCMethodParamTupleType:
    case Kind::ObjCMethodFormalParamTupleType:
    case Kind::CFunctionAsMethodFormalParamTupleType:
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

  /// Given that the value being abstracted is optional, return the
  /// abstraction pattern for its object type.
  AbstractionPattern getAnyOptionalObjectType() const;

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
