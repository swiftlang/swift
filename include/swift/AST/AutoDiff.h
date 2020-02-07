//===--- AutoDiff.h - Swift automatic differentiation utilities -----------===//
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
//  This file defines utilities for automatic differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AUTODIFF_H
#define SWIFT_AST_AUTODIFF_H

#include <cstdint>

#include "swift/AST/GenericSignature.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/IndexSubset.h"
#include "swift/AST/Type.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {

class AnyFunctionType;
class SILFunctionType;
class TupleType;
struct SILAutoDiffIndices;
class SILFunctionType;

/// A function type differentiability kind.
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

  // SWIFT_ENABLE_TENSORFLOW
  /// Returns the `SILAutoDiffIndices` corresponding to this config's indices.
  // TODO(TF-893): This is a temporary shim for incremental removal of
  // `SILAutoDiffIndices`. Eventually remove this.
  SILAutoDiffIndices getSILAutoDiffIndices() const;
  // SWIFT_ENABLE_TENSORFLOW END

  void print(llvm::raw_ostream &s = llvm::outs()) const;
  SWIFT_DEBUG_DUMP;
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

/// The tangent space of a type.
///
/// For `Differentiable`-conforming types:
/// - The tangent space is the `TangentVector` associated type.
///
/// For tuple types:
/// - The tangent space is a tuple of the elements' tangent space types, for the
///   elements that have a tangent space.
///
/// Other types have no tangent space.
class TangentSpace {
public:
  /// A tangent space kind.
  enum class Kind {
    /// The `TangentVector` associated type of a `Differentiable`-conforming
    /// type.
    TangentVector,
    /// A product of tangent spaces as a tuple.
    Tuple
  };

private:
  Kind kind;
  union Value {
    // TangentVector
    Type tangentVectorType;
    // Tuple
    TupleType *tupleType;

    Value(Type tangentVectorType) : tangentVectorType(tangentVectorType) {}
    Value(TupleType *tupleType) : tupleType(tupleType) {}
  } value;

  TangentSpace(Kind kind, Value value) : kind(kind), value(value) {}

public:
  TangentSpace() = delete;

  static TangentSpace getTangentVector(Type tangentVectorType) {
    return {Kind::TangentVector, tangentVectorType};
  }
  static TangentSpace getTuple(TupleType *tupleTy) {
    return {Kind::Tuple, tupleTy};
  }

  bool isTangentVector() const { return kind == Kind::TangentVector; }
  bool isTuple() const { return kind == Kind::Tuple; }

  Kind getKind() const { return kind; }
  Type getTangentVector() const {
    assert(kind == Kind::TangentVector);
    return value.tangentVectorType;
  }
  TupleType *getTuple() const {
    assert(kind == Kind::Tuple);
    return value.tupleType;
  }

  /// Get the tangent space type.
  Type getType() const;

  /// Get the tangent space canonical type.
  CanType getCanonicalType() const;

  /// Get the underlying nominal type declaration of the tangent space type.
  NominalTypeDecl *getNominal() const;
};

/// The key type used for uniquing `SILDifferentiabilityWitness` in
/// `SILModule`: original function name, parameter indices, result indices, and
/// derivative generic signature.
using SILDifferentiabilityWitnessKey = std::pair<StringRef, AutoDiffConfig>;

/// Automatic differentiation utility namespace.
namespace autodiff {

/// Appends the subset's parameter's types to `results`, in the order in
/// which they appear in the function type.
void getSubsetParameterTypes(IndexSubset *indices, AnyFunctionType *type,
                             SmallVectorImpl<Type> &results,
                             bool reverseCurryLevels = false);

/// "Constrained" derivative generic signatures require all differentiability
/// parameters to conform to the `Differentiable` protocol.
///
/// Returns the "constrained" derivative generic signature given:
/// - An original SIL function type.
/// - Differentiability parameter indices.
/// - A possibly "unconstrained" derivative generic signature.
GenericSignature
getConstrainedDerivativeGenericSignature(SILFunctionType *originalFnTy,
                                         IndexSubset *diffParamIndices,
                                         GenericSignature derivativeGenSig);

} // end namespace autodiff

} // end namespace swift

namespace llvm {

using swift::AutoDiffConfig;
using swift::AutoDiffDerivativeFunctionKind;
using swift::GenericSignature;
using swift::IndexSubset;

template <typename T> struct DenseMapInfo;

template <> struct DenseMapInfo<AutoDiffConfig> {
  static AutoDiffConfig getEmptyKey() {
    auto *ptr = llvm::DenseMapInfo<void *>::getEmptyKey();
    // The `derivativeGenericSignature` component must be `nullptr` so that
    // `getHashValue` and `isEqual` do not try to call
    // `GenericSignatureImpl::getCanonicalSignature()` on an invalid pointer.
    return {static_cast<IndexSubset *>(ptr), static_cast<IndexSubset *>(ptr),
            nullptr};
  }

  static AutoDiffConfig getTombstoneKey() {
    auto *ptr = llvm::DenseMapInfo<void *>::getTombstoneKey();
    // The `derivativeGenericSignature` component must be `nullptr` so that
    // `getHashValue` and `isEqual` do not try to call
    // `GenericSignatureImpl::getCanonicalSignature()` on an invalid pointer.
    return {static_cast<IndexSubset *>(ptr), static_cast<IndexSubset *>(ptr),
            nullptr};
  }

  static unsigned getHashValue(const AutoDiffConfig &Val) {
    auto canGenSig =
        Val.derivativeGenericSignature
            ? Val.derivativeGenericSignature->getCanonicalSignature()
            : nullptr;
    unsigned combinedHash = hash_combine(
        ~1U, DenseMapInfo<void *>::getHashValue(Val.parameterIndices),
        DenseMapInfo<void *>::getHashValue(Val.resultIndices),
        DenseMapInfo<GenericSignature>::getHashValue(canGenSig));
    return combinedHash;
  }

  static bool isEqual(const AutoDiffConfig &LHS, const AutoDiffConfig &RHS) {
    auto lhsCanGenSig =
        LHS.derivativeGenericSignature
            ? LHS.derivativeGenericSignature->getCanonicalSignature()
            : nullptr;
    auto rhsCanGenSig =
        RHS.derivativeGenericSignature
            ? RHS.derivativeGenericSignature->getCanonicalSignature()
            : nullptr;
    return LHS.parameterIndices == RHS.parameterIndices &&
           LHS.resultIndices == RHS.resultIndices &&
           DenseMapInfo<GenericSignature>::isEqual(lhsCanGenSig, rhsCanGenSig);
  }
};

template <> struct DenseMapInfo<AutoDiffDerivativeFunctionKind> {
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
    return static_cast<AutoDiffDerivativeFunctionKind::innerty>(LHS) ==
        static_cast<AutoDiffDerivativeFunctionKind::innerty>(RHS);
  }
};

} // end namespace llvm

// SWIFT_ENABLE_TENSORFLOW
// Not-yet-upstreamed `tensorflow` branch additions are below.
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

struct SILAutoDiffDerivativeFunctionKey {
  SILFunctionType *originalType;
  IndexSubset *parameterIndices;
  IndexSubset *resultIndices;
  AutoDiffDerivativeFunctionKind kind;
  CanGenericSignature derivativeFnGenSig;
  bool isReabstractionThunk;
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

} // end namespace swift

namespace llvm {

using swift::SILAutoDiffIndices;

template <> struct DenseMapInfo<SILAutoDiffIndices> {
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

using swift::SILAutoDiffDerivativeFunctionKey;
using swift::SILFunctionType;
using swift::IndexSubset;
using swift::AutoDiffDerivativeFunctionKind;
using swift::GenericSignature;
using swift::CanGenericSignature;

template <> struct DenseMapInfo<SILAutoDiffDerivativeFunctionKey> {

  static bool isEqual(const SILAutoDiffDerivativeFunctionKey lhs,
                      const SILAutoDiffDerivativeFunctionKey rhs) {
    return lhs.originalType == rhs.originalType &&
        lhs.parameterIndices == rhs.parameterIndices &&
        lhs.resultIndices == rhs.resultIndices &&
        lhs.kind.rawValue == rhs.kind.rawValue &&
        lhs.derivativeFnGenSig == rhs.derivativeFnGenSig &&
        lhs.isReabstractionThunk == rhs.isReabstractionThunk;
  }

  static inline SILAutoDiffDerivativeFunctionKey getEmptyKey() {
    return {
        DenseMapInfo<SILFunctionType *>::getEmptyKey(),
        DenseMapInfo<IndexSubset *>::getEmptyKey(),
        DenseMapInfo<IndexSubset *>::getEmptyKey(),
        AutoDiffDerivativeFunctionKind::innerty(
            DenseMapInfo<unsigned>::getEmptyKey()),
        CanGenericSignature(DenseMapInfo<GenericSignature>::getEmptyKey()),
        (bool)DenseMapInfo<unsigned>::getEmptyKey()};
  }

  static inline SILAutoDiffDerivativeFunctionKey getTombstoneKey() {
    return {
        DenseMapInfo<SILFunctionType *>::getTombstoneKey(),
        DenseMapInfo<IndexSubset *>::getTombstoneKey(),
        DenseMapInfo<IndexSubset *>::getTombstoneKey(),
        AutoDiffDerivativeFunctionKind::innerty(
            DenseMapInfo<unsigned>::getTombstoneKey()),
        CanGenericSignature(DenseMapInfo<GenericSignature>::getTombstoneKey()),
        (bool)DenseMapInfo<unsigned>::getTombstoneKey()};
  }

  static unsigned getHashValue(const SILAutoDiffDerivativeFunctionKey &Val) {
    return hash_combine(
        DenseMapInfo<SILFunctionType *>::getHashValue(Val.originalType),
        DenseMapInfo<IndexSubset *>::getHashValue(Val.parameterIndices),
        DenseMapInfo<IndexSubset *>::getHashValue(Val.resultIndices),
        DenseMapInfo<unsigned>::getHashValue((unsigned)Val.kind.rawValue),
        DenseMapInfo<GenericSignature>::getHashValue(Val.derivativeFnGenSig),
        DenseMapInfo<unsigned>::getHashValue(
            (unsigned)Val.isReabstractionThunk));
  }
};

} // namespace llvm
// SWIFT_ENABLE_TENSORFLOW END

#endif // SWIFT_AST_AUTODIFF_H
