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
#include "swift/AST/TypeAlignments.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/StringExtras.h"

namespace swift {

class AnyFunctionType;
class SILFunctionType;
class TupleType;

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

/// A component of a SIL `@differentiable` function-typed value.
struct NormalDifferentiableFunctionTypeComponent {
  enum innerty : unsigned { Original = 0, JVP = 1, VJP = 2 } rawValue;

  NormalDifferentiableFunctionTypeComponent() = default;
  NormalDifferentiableFunctionTypeComponent(innerty rawValue)
      : rawValue(rawValue) {}
  NormalDifferentiableFunctionTypeComponent(
      AutoDiffDerivativeFunctionKind kind);
  explicit NormalDifferentiableFunctionTypeComponent(unsigned rawValue)
      : NormalDifferentiableFunctionTypeComponent((innerty)rawValue) {}
  explicit NormalDifferentiableFunctionTypeComponent(StringRef name);
  operator innerty() const { return rawValue; }

  /// Returns the derivative function kind, if the component is a derivative
  /// function.
  Optional<AutoDiffDerivativeFunctionKind> getAsDerivativeFunctionKind() const;
};

/// A component of a SIL `@differentiable(linear)` function-typed value.
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

/// A derivative function configuration, uniqued in `ASTContext`.
/// Identifies a specific derivative function given an original function.
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
  IndexSubset *getParameterIndices() const { return parameterIndices; }
  GenericSignature getDerivativeGenericSignature() const {
    return derivativeGenericSignature;
  }

  static AutoDiffDerivativeFunctionIdentifier *
  get(AutoDiffDerivativeFunctionKind kind, IndexSubset *parameterIndices,
      GenericSignature derivativeGenericSignature, ASTContext &C);

  void Profile(llvm::FoldingSetNodeID &ID) {
    ID.AddInteger(kind);
    ID.AddPointer(parameterIndices);
    auto derivativeCanGenSig =
        derivativeGenericSignature.getCanonicalSignature();
    ID.AddPointer(derivativeCanGenSig.getPointer());
  }
};

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

/// SIL-level automatic differentiation indices. Consists of:
/// - Parameter indices: indices of parameters to differentiate with respect to.
/// - Result index: index of the result to differentiate from.
// TODO(TF-913): Remove `SILAutoDiffIndices` in favor of `AutoDiffConfig`.
// `AutoDiffConfig` supports multiple result indices.
struct SILAutoDiffIndices {
  /// The index of the dependent result to differentiate from.
  unsigned source;
  /// The indices for independent parameters to differentiate with respect to.
  IndexSubset *parameters;

  /*implicit*/ SILAutoDiffIndices(unsigned source, IndexSubset *parameters)
      : source(source), parameters(parameters) {}

  bool operator==(const SILAutoDiffIndices &other) const;

  bool operator!=(const SILAutoDiffIndices &other) const {
    return !(*this == other);
  };

  /// Returns true if `parameterIndex` is a differentiability parameter index.
  bool isWrtParameter(unsigned parameterIndex) const {
    return parameterIndex < parameters->getCapacity() &&
           parameters->contains(parameterIndex);
  }

  void print(llvm::raw_ostream &s = llvm::outs()) const;
  SWIFT_DEBUG_DUMP;

  std::string mangle() const {
    std::string result = "src_" + llvm::utostr(source) + "_wrt_";
    interleave(
        parameters->getIndices(),
        [&](unsigned idx) { result += llvm::utostr(idx); },
        [&] { result += '_'; });
    return result;
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

  /// Returns the `SILAutoDiffIndices` corresponding to this config's indices.
  // TODO(TF-913): This is a temporary shim for incremental removal of
  // `SILAutoDiffIndices`. Eventually remove this.
  SILAutoDiffIndices getSILAutoDiffIndices() const;

  void print(llvm::raw_ostream &s = llvm::outs()) const;
  SWIFT_DEBUG_DUMP;
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &s,
                                     const SILAutoDiffIndices &indices) {
  indices.print(s);
  return s;
}

/// A semantic function result type: either a formal function result type or
/// an `inout` parameter type. Used in derivative function type calculation.
struct AutoDiffSemanticFunctionResultType {
  Type type;
  bool isInout;
};

/// Key for caching SIL derivative function types.
struct SILAutoDiffDerivativeFunctionKey {
  SILFunctionType *originalType;
  IndexSubset *parameterIndices;
  IndexSubset *resultIndices;
  AutoDiffDerivativeFunctionKind kind;
  CanGenericSignature derivativeFnGenSig;
  bool isReabstractionThunk;
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

/// Given a function type, collects its semantic result types in type order
/// into `result`: first, the formal result type (if non-`Void`), followed by
/// `inout` parameter types.
///
/// The function type may have at most two parameter lists.
///
/// Remaps the original semantic result using `genericEnv`, if specified.
void getFunctionSemanticResultTypes(
    AnyFunctionType *functionType,
    SmallVectorImpl<AutoDiffSemanticFunctionResultType> &result,
    GenericEnvironment *genericEnv = nullptr);

/// Returns the lowered SIL parameter indices for the given AST parameter
/// indices and `AnyfunctionType`.
///
/// Notable lowering-related changes:
/// - AST tuple parameter types are exploded when lowered to SIL.
/// - AST curried `Self` parameter types become the last parameter when lowered
///   to SIL.
///
/// Examples:
///
///   AST function type: (A, B, C) -> R
///   AST parameter indices: 101, {A, C}
///   Lowered SIL function type: $(A, B, C) -> R
///   Lowered SIL parameter indices: 101
///
///   AST function type: (Self) -> (A, B, C) -> R
///   AST parameter indices: 1010, {Self, B}
///   Lowered SIL function type: $(A, B, C, Self) -> R
///   Lowered SIL parameter indices: 0101
///
///   AST function type: (A, (B, C), D) -> R
///   AST parameter indices: 110, {A, (B, C)}
///   Lowered SIL function type: $(A, B, C, D) -> R
///   Lowered SIL parameter indices: 1110
///
/// Note:
/// - The AST function type must not be curried unless it is a method.
///   Otherwise, the behavior is undefined.
IndexSubset *getLoweredParameterIndices(IndexSubset *astParameterIndices,
                                        AnyFunctionType *functionType);

/// "Constrained" derivative generic signatures require all differentiability
/// parameters to conform to the `Differentiable` protocol.
///
/// "Constrained" transpose generic signatures additionally require all
/// linearity parameters to satisfy `Self == Self.TangentVector`.
///
/// Returns the "constrained" derivative/transpose generic signature given:
/// - An original SIL function type.
/// - Differentiability parameter indices.
/// - A possibly "unconstrained" derivative generic signature.
GenericSignature getConstrainedDerivativeGenericSignature(
    SILFunctionType *originalFnTy, IndexSubset *diffParamIndices,
    GenericSignature derivativeGenSig, LookupConformanceFn lookupConformance,
    bool isTranspose = false);

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

} // end namespace swift

namespace llvm {

using swift::AutoDiffConfig;
using swift::AutoDiffDerivativeFunctionKind;
using swift::CanGenericSignature;
using swift::GenericSignature;
using swift::IndexSubset;
using swift::SILAutoDiffDerivativeFunctionKey;
using swift::SILFunctionType;

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
    return {DenseMapInfo<SILFunctionType *>::getEmptyKey(),
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

} // end namespace llvm

#endif // SWIFT_AST_AUTODIFF_H
