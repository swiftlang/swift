//===--- AutoDiff.h - Swift Automatic Differentiation ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines AST support for automatic differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AUTODIFF_H
#define SWIFT_AST_AUTODIFF_H

#include <cstdint>

#include "swift/AST/Identifier.h"
#include "swift/AST/IndexSubset.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/Range.h"

namespace swift {

enum class DifferentiabilityKind : uint8_t {
  NonDifferentiable = 0,
  Normal = 1,
  Linear = 2
};

/// The kind of an linear map.
struct AutoDiffLinearMapKind {
  enum innerty : uint8_t {
    // The differential function.
    Differential = 0,
    // The pullback function.
    Pullback = 1
  } rawValue;

  AutoDiffLinearMapKind() = default;
  AutoDiffLinearMapKind(innerty rawValue) : rawValue(rawValue) {}
  operator innerty() const { return rawValue; }
};

/// The kind of a derivative function.
struct AutoDiffDerivativeFunctionKind {
  enum innerty : uint8_t {
    // The Jacobian-vector products function.
    JVP = 0,
    // The vector-Jacobian products function.
    VJP = 1
  } rawValue;

  AutoDiffDerivativeFunctionKind() = default;
  AutoDiffDerivativeFunctionKind(innerty rawValue) : rawValue(rawValue) {}
  AutoDiffDerivativeFunctionKind(AutoDiffLinearMapKind linMapKind)
      : rawValue(static_cast<innerty>(linMapKind.rawValue)) {}
  explicit AutoDiffDerivativeFunctionKind(StringRef string);
  operator innerty() const { return rawValue; }
  AutoDiffLinearMapKind getLinearMapKind() {
    return (AutoDiffLinearMapKind::innerty)rawValue;
  }
};

class ParsedAutoDiffParameter {
public:
  enum class Kind { Named, Ordered, Self };

private:
  SourceLoc loc;
  Kind kind;
  union Value {
    struct { Identifier name; } Named;
    struct { unsigned index; } Ordered;
    struct {} self;
    Value(Identifier name) : Named({name}) {}
    Value(unsigned index) : Ordered({index}) {}
    Value() {}
  } value;

public:
  ParsedAutoDiffParameter(SourceLoc loc, Kind kind, Value value)
    : loc(loc), kind(kind), value(value) {}

  ParsedAutoDiffParameter(SourceLoc loc, Kind kind, unsigned index)
    : loc(loc), kind(kind), value(index) {}

  static ParsedAutoDiffParameter getNamedParameter(SourceLoc loc,
                                                   Identifier name) {
    return { loc, Kind::Named, name };
  }

  static ParsedAutoDiffParameter getOrderedParameter(SourceLoc loc,
                                                     unsigned index) {
    return { loc, Kind::Ordered, index };
  }

  static ParsedAutoDiffParameter getSelfParameter(SourceLoc loc) {
    return { loc, Kind::Self, {} };
  }

  Identifier getName() const {
    assert(kind == Kind::Named);
    return value.Named.name;
  }

  unsigned getIndex() const {
    return value.Ordered.index;
  }

  Kind getKind() const {
    return kind;
  }

  SourceLoc getLoc() const {
    return loc;
  }

  bool isEqual(const ParsedAutoDiffParameter &other) const {
    if (getKind() != other.getKind())
      return false;
    if (getKind() == Kind::Named)
      return getName() == other.getName();
    return getKind() == Kind::Self;
  }
};

} // end namespace swift

// SWIFT_ENABLE_TENSORFLOW
// Not-yet-upstreamed additions on `tensorflow` branch is below.
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/IndexSubset.h"
#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class ASTContext;
class AnyFunctionType;
class SILFunctionType;
typedef CanTypeWrapper<SILFunctionType> CanSILFunctionType;
enum class SILLinkage : uint8_t;

/// The kind of a differentiability witness function.
struct DifferentiabilityWitnessFunctionKind {
  enum innerty : uint8_t {
    // The Jacobian-vector products function.
    JVP = 0,
    // The vector-Jacobian products function.
    VJP = 1,
    // The transpose function.
    Transpose = 2
  } rawValue;

  DifferentiabilityWitnessFunctionKind() = default;
  DifferentiabilityWitnessFunctionKind(innerty rawValue) : rawValue(rawValue) {}
  explicit DifferentiabilityWitnessFunctionKind(unsigned rawValue)
      : rawValue(static_cast<innerty>(rawValue)) {}
  explicit DifferentiabilityWitnessFunctionKind(StringRef name);
  operator innerty() const { return rawValue; }

  Optional<AutoDiffDerivativeFunctionKind> getAsDerivativeFunctionKind() const;
};

struct NormalDifferentiableFunctionTypeComponent {
  enum innerty : unsigned {
    Original = 0,
    JVP = 1,
    VJP = 2
  } rawValue;

  NormalDifferentiableFunctionTypeComponent() = default;
  NormalDifferentiableFunctionTypeComponent(innerty rawValue)
      : rawValue(rawValue) {}
  NormalDifferentiableFunctionTypeComponent(
      AutoDiffDerivativeFunctionKind kind);
  explicit NormalDifferentiableFunctionTypeComponent(unsigned rawValue)
      : NormalDifferentiableFunctionTypeComponent((innerty)rawValue) {}
  explicit NormalDifferentiableFunctionTypeComponent(StringRef name);
  operator innerty() const { return rawValue; }

  Optional<AutoDiffDerivativeFunctionKind> getAsDerivativeFunctionKind() const;
};

struct LinearDifferentiableFunctionTypeComponent {
  enum innerty : unsigned {
    Original = 0,
    Transpose = 1,
  } rawValue;

  LinearDifferentiableFunctionTypeComponent() = default;
  LinearDifferentiableFunctionTypeComponent(innerty rawValue)
      : rawValue(rawValue) {}
  explicit LinearDifferentiableFunctionTypeComponent(unsigned rawValue)
      : LinearDifferentiableFunctionTypeComponent((innerty)rawValue) {}
  explicit LinearDifferentiableFunctionTypeComponent(StringRef name);
  operator innerty() const { return rawValue; }
};

/// SIL-level automatic differentiation indices. Consists of a source index,
/// i.e. index of the dependent result to differentiate from, and parameter
/// indices, i.e. index of independent parameters to differentiate with
/// respect to.
///
/// When a function is curried, parameter indices can refer to parameters from
/// all parameter lists. When differentiating such functions, we treat them as
/// fully uncurried.
struct SILAutoDiffIndices {
  /// The index of the dependent result to differentiate from.
  unsigned source;
  /// Independent parameters to differentiate with respect to. The bits
  /// correspond to the function's parameters in order. For example,
  ///
  ///   Function type: (A, B, C) -> R
  ///   Bits: [A][B][C]
  ///
  /// When the function is curried, the bits for the first parameter list come
  /// last. For example,
  ///
  ///   Function type: (A, B) -> (C, D) -> R
  ///   Bits: [C][D][A][B]
  ///
  IndexSubset *parameters;

  /// Creates a set of AD indices from the given source index and a bit vector
  /// representing parameter indices.
  /*implicit*/ SILAutoDiffIndices(unsigned source,
                                  IndexSubset *parameters)
      : source(source), parameters(parameters) {}

  bool operator==(const SILAutoDiffIndices &other) const;

  bool operator!=(const SILAutoDiffIndices &other) const {
    return !(*this == other);
  };

  /// Queries whether the function's parameter with index `parameterIndex` is
  /// one of the parameters to differentiate with respect to.
  bool isWrtParameter(unsigned parameterIndex) const {
    return parameterIndex < parameters->getCapacity() &&
           parameters->contains(parameterIndex);
  }

  void print(llvm::raw_ostream &s = llvm::outs()) const;
  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");

  std::string mangle() const {
    std::string result = "src_" + llvm::utostr(source) + "_wrt_";
    interleave(parameters->getIndices(),
               [&](unsigned idx) { result += llvm::utostr(idx); },
               [&] { result += '_'; });
    return result;
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &s,
                                     const SILAutoDiffIndices &indices) {
  indices.print(s);
  return s;
}

/// Identifies an autodiff derivative function configuration:
/// - Parameter indices.
/// - Result indices.
/// - Derivative generic signature (optional).
struct AutoDiffConfig {
  IndexSubset *parameterIndices;
  IndexSubset *resultIndices;
  GenericSignature derivativeGenericSignature;

  /*implicit*/ AutoDiffConfig(IndexSubset *parameterIndices,
                              IndexSubset *resultIndices,
                              GenericSignature derivativeGenericSignature)
      : parameterIndices(parameterIndices), resultIndices(resultIndices),
        derivativeGenericSignature(derivativeGenericSignature) {}

  /// Returns the `SILAutoDiffIndices` corresponding to this config's indices.
  // TODO(TF-893): This is a temporary shim for incremental removal of
  // `SILAutoDiffIndices`. Eventually remove this.
  SILAutoDiffIndices getSILAutoDiffIndices() const;

  void print(llvm::raw_ostream &s = llvm::outs()) const;
  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");
};

/// In conjunction with the original function declaration, identifies an
/// autodiff derivative function.
///
/// Is uniquely allocated within an ASTContext so that it can be hashed and
/// compared by opaque pointer value.
class AutoDiffDerivativeFunctionIdentifier : public llvm::FoldingSetNode {
  const AutoDiffDerivativeFunctionKind kind;
  IndexSubset *const parameterIndices;
  GenericSignature derivativeGenericSignature;

  AutoDiffDerivativeFunctionIdentifier(
      AutoDiffDerivativeFunctionKind kind, IndexSubset *parameterIndices,
      GenericSignature derivativeGenericSignature)
      : kind(kind), parameterIndices(parameterIndices),
        derivativeGenericSignature(derivativeGenericSignature) {}

public:
  AutoDiffDerivativeFunctionKind getKind() const { return kind; }
  IndexSubset *getParameterIndices() const {
    return parameterIndices;
  }
  GenericSignature getDerivativeGenericSignature() const {
    return derivativeGenericSignature;
  }

  static AutoDiffDerivativeFunctionIdentifier *
  get(AutoDiffDerivativeFunctionKind kind, IndexSubset *parameterIndices,
      GenericSignature derivativeGenericSignature, ASTContext &C);

  void Profile(llvm::FoldingSetNodeID &ID) {
    ID.AddInteger(kind);
    ID.AddPointer(parameterIndices);
    CanGenericSignature derivativeCanGenSig;
    if (derivativeGenericSignature)
      derivativeCanGenSig = derivativeGenericSignature->getCanonicalSignature();
    ID.AddPointer(derivativeCanGenSig.getPointer());
  }
};

/// The key type used for uniquing `SILDifferentiabilityWitness` in
/// `SILModule`: original function name, parameter indices, result indices, and
/// derivative generic signature.
using SILDifferentiabilityWitnessKey = std::pair<StringRef, AutoDiffConfig>;

/// Automatic differentiation utility namespace.
namespace autodiff {
/// Appends the subset's parameter's types to `result`, in the order in
/// which they appear in the function type.
void getSubsetParameterTypes(IndexSubset *indices,
                             AnyFunctionType *type,
                             SmallVectorImpl<Type> &result,
                             bool reverseCurryLevels = false);

/// Returns an index subset for the SIL function parameters corresponding to the
/// parameters in this subset. In particular, this explodes tuples. For example,
///
///   functionType = (A, B, C) -> R
///   if "A" and "C" are in the set,
///   ==> returns 101
///   (because the lowered SIL type is (A, B, C) -> R)
///
///   functionType = (Self) -> (A, B, C) -> R
///   if "Self" and "C" are in the set,
///   ==> returns 0011
///   (because the lowered SIL type is (A, B, C, Self) -> R)
///
///   functionType = (A, (B, C), D) -> R
///   if "A" and "(B, C)" are in the set,
///   ==> returns 1110
///   (because the lowered SIL type is (A, B, C, D) -> R)
///
/// Note:
/// - The function must not be curried unless it's a method. Otherwise, the
///   behavior is undefined.
/// - For methods, whether the self parameter is set is represented by the
///   inclusion of the `0` index in `indices`.
IndexSubset *getLoweredParameterIndices(IndexSubset *indices,
                                        AnyFunctionType *type);

/// Retrieve config from the function name of a variant of
/// `Builtin.applyDerivative`, e.g. `Builtin.applyDerivative_jvp_arity2`.
/// Returns true if the function name is parsed successfully.
bool getBuiltinApplyDerivativeConfig(
    StringRef operationName, AutoDiffDerivativeFunctionKind &kind,
    unsigned &arity, bool &rethrows);

/// Retrieve config from the function name of a variant of
/// `Builtin.applyTranspose`, e.g. `Builtin.applyTranspose_arity2`.
/// Returns true if the function name is parsed successfully.
bool getBuiltinApplyTransposeConfig(
  StringRef operationName, unsigned &arity, bool &rethrows);

/// Retrieve config from the function name of a variant of
/// `Builtin.differentiableFunction` or `Builtin.linearFunction`, e.g.
/// `Builtin.differentiableFunction_arity1_throws`.
/// Returns true if the function name is parsed successfully.
bool getBuiltinDifferentiableOrLinearFunctionConfig(
    StringRef operationName, unsigned &arity, bool &throws);

/// Retrieve config from the function name of a variant of
/// `Builtin.differentiableFunction` or `Builtin.linearFunction`, e.g.
/// `Builtin.differentiableFunction_arity1_throws`.
/// Returns true if the function name is parsed successfully.
bool getBuiltinDifferentiableOrLinearFunctionConfig(
    StringRef operationName, unsigned &arity, bool &throws);

} // end namespace autodiff

class BuiltinFloatType;
class NominalOrBoundGenericNominalType;
class TupleType;

/// A type that represents a vector space.
class VectorSpace {
public:
  /// A tangent space kind.
  enum class Kind {
    /// A type that conforms to `AdditiveArithmetic`.
    Vector,
    /// A product of vector spaces as a tuple.
    Tuple,
    /// A function type whose innermost result conforms to `AdditiveArithmetic`.
    Function
  };

private:
  Kind kind;
  union Value {
    // Vector
    Type vectorType;
    // Tuple
    TupleType *tupleType;
    // Function
    AnyFunctionType *functionType;

    Value(Type vectorType) : vectorType(vectorType) {}
    Value(TupleType *tupleType) : tupleType(tupleType) {}
    Value(AnyFunctionType *functionType) : functionType(functionType) {}
  } value;

  VectorSpace(Kind kind, Value value)
      : kind(kind), value(value) {}

public:
  VectorSpace() = delete;

  static VectorSpace getVector(Type vectorType) {
    return {Kind::Vector, vectorType};
  }
  static VectorSpace getTuple(TupleType *tupleTy) {
    return {Kind::Tuple, tupleTy};
  }
  static VectorSpace getFunction(AnyFunctionType *fnTy) {
    return {Kind::Function, fnTy};
  }

  bool isVector() const { return kind == Kind::Vector; }
  bool isTuple() const { return kind == Kind::Tuple; }

  Kind getKind() const { return kind; }
  Type getVector() const {
    assert(kind == Kind::Vector);
    return value.vectorType;
  }
  TupleType *getTuple() const {
    assert(kind == Kind::Tuple);
    return value.tupleType;
  }
  AnyFunctionType *getFunction() const {
    assert(kind == Kind::Function);
    return value.functionType;
  }

  Type getType() const;
  CanType getCanonicalType() const;
  NominalTypeDecl *getNominal() const;
};

} // end namespace swift

namespace llvm {

using swift::AutoDiffConfig;
using swift::AutoDiffDerivativeFunctionKind;
using swift::GenericSignature;
using swift::IndexSubset;
using swift::SILAutoDiffIndices;

template<typename T> struct DenseMapInfo;

template<> struct DenseMapInfo<AutoDiffConfig> {
  static AutoDiffConfig getEmptyKey() {
    auto *ptr = llvm::DenseMapInfo<void *>::getEmptyKey();
    return {static_cast<IndexSubset *>(ptr), static_cast<IndexSubset *>(ptr),
            DenseMapInfo<GenericSignature>::getEmptyKey()};
  }

  static AutoDiffConfig getTombstoneKey() {
    auto *ptr = llvm::DenseMapInfo<void *>::getTombstoneKey();
    return {static_cast<IndexSubset *>(ptr), static_cast<IndexSubset *>(ptr),
            DenseMapInfo<GenericSignature>::getTombstoneKey()};
  }

  static unsigned getHashValue(const AutoDiffConfig &Val) {
    unsigned combinedHash = hash_combine(
        ~1U, DenseMapInfo<void *>::getHashValue(Val.parameterIndices),
        DenseMapInfo<void *>::getHashValue(Val.resultIndices),
        DenseMapInfo<GenericSignature>::getHashValue(
            Val.derivativeGenericSignature));
    return combinedHash;
  }

  static bool isEqual(const AutoDiffConfig &LHS, const AutoDiffConfig &RHS) {
    return LHS.parameterIndices == RHS.parameterIndices &&
        LHS.resultIndices == RHS.resultIndices &&
        DenseMapInfo<GenericSignature>::isEqual(LHS.derivativeGenericSignature,
                                                RHS.derivativeGenericSignature);
  }
};

template<> struct DenseMapInfo<AutoDiffDerivativeFunctionKind> {
  static AutoDiffDerivativeFunctionKind getEmptyKey() {
    return static_cast<AutoDiffDerivativeFunctionKind::innerty>(
        DenseMapInfo<unsigned>::getEmptyKey());
  }

  static AutoDiffDerivativeFunctionKind getTombstoneKey() {
    return static_cast<AutoDiffDerivativeFunctionKind::innerty>(
        DenseMapInfo<unsigned>::getTombstoneKey());
  }

  static unsigned getHashValue(const AutoDiffDerivativeFunctionKind &Val) {
    return DenseMapInfo<unsigned>::getHashValue(Val);
  }

  static bool isEqual(const AutoDiffDerivativeFunctionKind &LHS,
                      const AutoDiffDerivativeFunctionKind &RHS) {
    return LHS == RHS;
  }
};

template<> struct DenseMapInfo<SILAutoDiffIndices> {
  static SILAutoDiffIndices getEmptyKey() {
    return { DenseMapInfo<unsigned>::getEmptyKey(), nullptr };
  }

  static SILAutoDiffIndices getTombstoneKey() {
    return { DenseMapInfo<unsigned>::getTombstoneKey(), nullptr };
  }

  static unsigned getHashValue(const SILAutoDiffIndices &Val) {
    unsigned combinedHash =
      hash_combine(~1U, DenseMapInfo<unsigned>::getHashValue(Val.source),
                   hash_combine_range(Val.parameters->begin(),
                                      Val.parameters->end()));
    return combinedHash;
  }

  static bool isEqual(const SILAutoDiffIndices &LHS,
                      const SILAutoDiffIndices &RHS) {
    return LHS == RHS;
  }
};

} // end namespace llvm
// SWIFT_ENABLE_TENSORFLOW END

#endif // SWIFT_AST_AUTODIFF_H
