//===--- ExtInfo.h - Extended information for function types ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines the ASTExtInfo and SILExtInfo classes, which are used to store
// the calling convention and related information for function types in the AST
// and SIL respectively. These types are lightweight and immutable; they are
// constructed using builder-pattern style APIs to enforce invariants.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_EXTINFO_H
#define SWIFT_EXTINFO_H

#include "swift/AST/AutoDiff.h"
#include "swift/AST/LifetimeDependence.h"

#include "llvm/Support/raw_ostream.h"
#include <optional>

#include <utility>

namespace clang {
class Type;
class ASTContext;
} // namespace clang

namespace swift {
class AnyFunctionType;
class ASTExtInfo;
class ASTExtInfoBuilder;
class ClangModuleLoader;
class FunctionType;
class SILExtInfo;
class SILExtInfoBuilder;
class SILFunctionType;
enum class SILFunctionTypeRepresentation : uint8_t;
} // namespace swift

namespace swift {

/// The formal isolation of a function type.
class FunctionTypeIsolation {
public:
  enum class Kind : uint8_t {
    /// The function is not isolated.
    NonIsolated,

    /// The function is isolated to a global actor.
    GlobalActor,

    /// The function has an isolated parameter; which one is indicated in
    /// the parameter list.
    Parameter,

    /// The function's isolation is statically erased with @isolated(any).
    Erased,
  };

  static constexpr size_t NumBits = 3; // future-proof this slightly
  static constexpr size_t Mask = (1 << NumBits) - 1;

private:
  llvm::PointerIntPair<Type, NumBits, Kind> value;

  FunctionTypeIsolation(Kind kind, Type type = Type()) : value(type, kind) {}

public:
  static FunctionTypeIsolation forNonIsolated() {
    return { Kind::NonIsolated };
  }
  static FunctionTypeIsolation forGlobalActor(Type type) {
    assert(type && "creating global actor isolation without an actor type");
    return { Kind::GlobalActor, type };
  }
  static FunctionTypeIsolation forParameter() {
    return { Kind::Parameter };
  }
  static FunctionTypeIsolation forErased() {
    return { Kind::Erased };
  }

  Kind getKind() const { return value.getInt(); }
  bool isNonIsolated() const {
    return getKind() == Kind::NonIsolated;
  }
  bool isGlobalActor() const {
    return getKind() == Kind::GlobalActor;
  }
  Type getGlobalActorType() const {
    assert(getKind() == Kind::GlobalActor);
    return value.getPointer();
  }
  bool isParameter() const {
    return getKind() == Kind::Parameter;
  }
  bool isErased() const {
    return getKind() == Kind::Erased;
  }

  // The opaque accessors below are just for the benefit of ExtInfoBuilder,
  // which finds it convenient to break down the type separately.  Normal
  // clients should use the accessors above.

  Type getOpaqueType() const {
    return value.getPointer();
  }

  static FunctionTypeIsolation fromOpaqueValues(Kind kind, Type type) {
    return FunctionTypeIsolation(kind, type);
  }
};

/// For now, the kinds of isolation we carry on SIL function types
/// are significantly reduced compared to AST function types.
/// Isolation is not part of the SIL function model after the
/// early portion of the pipeline.
enum class SILFunctionTypeIsolation {
  /// We don't normally record isolation in SIL function types,
  /// so the empty case here is "unknown".
  Unknown,

  /// The isolation of the function has been statically erased.
  /// This corresponds to @isolated(any).
  Erased,
};

// MARK: - ClangTypeInfo
/// Wrapper class for storing a clang::Type in an (AST|SIL)ExtInfo.
class ClangTypeInfo {
  friend AnyFunctionType;
  friend FunctionType;
  friend SILFunctionType;
  friend ASTExtInfoBuilder;
  friend SILExtInfoBuilder;

  // [NOTE: ClangTypeInfo-contents]
  // We preserve a full clang::Type *, not a clang::FunctionType * as:
  // 1. We need to keep sugar in case we need to present an error to the user
  //    (for AnyFunctionType).
  // 2. The actual type being stored is [ignoring sugar] either a
  //    clang::PointerType, a clang::BlockPointerType, or a
  //    clang::ReferenceType which points to a clang::FunctionType.
  //
  // When used as a part of SILFunctionType, the type is canonical.
  const clang::Type *type;

  constexpr ClangTypeInfo() : type(nullptr) {}
  constexpr ClangTypeInfo(const clang::Type *type) : type(type) {}

  friend bool operator==(ClangTypeInfo lhs, ClangTypeInfo rhs);
  friend bool operator!=(ClangTypeInfo lhs, ClangTypeInfo rhs);
  ClangTypeInfo getCanonical() const;

public:
  constexpr const clang::Type *getType() const { return type; }

  constexpr bool empty() const { return !type; }

  /// Use the ClangModuleLoader to print the Clang type as a string.
  void printType(ClangModuleLoader *cml, llvm::raw_ostream &os) const;

  void dump(llvm::raw_ostream &os, const clang::ASTContext &ctx) const;
};

// MARK: - UnexpectedClangTypeError
/// Potential errors when trying to store a Clang type in an ExtInfo.
struct UnexpectedClangTypeError {
  enum class Kind {
    NullForCOrBlock,
    NonnullForNonCOrBlock,
    NotBlockPointer,
    NotFunctionPointerOrReference,
    NonCanonical,
  };
  const Kind errorKind;
  const clang::Type *type;

  static std::optional<UnexpectedClangTypeError>
  checkClangType(SILFunctionTypeRepresentation fnRep, const clang::Type *type,
                 bool expectNonnullForCOrBlock, bool expectCanonical);

  void dump();
};

// MARK: - FunctionTypeRepresentation
/// The representation form of a function.
enum class FunctionTypeRepresentation : uint8_t {
  /// A "thick" function that carries a context pointer to reference captured
  /// state. The default native function representation.
  Swift = 0,

  /// A thick function that is represented as an Objective-C block.
  Block,

  /// A "thin" function that needs no context.
  Thin,

  /// A C function pointer (or reference), which is thin and also uses the C
  /// calling convention.
  CFunctionPointer,

  /// The value of the greatest AST function representation.
  Last = CFunctionPointer,
};

// MARK: - SILFunctionTypeRepresentation

/// The representation form of a SIL function.
///
/// This is a superset of FunctionTypeRepresentation. The common representations
/// must share an enum value.
///
/// TODO: The overlap of SILFunctionTypeRepresentation and
/// FunctionTypeRepresentation is a total hack necessitated by the way SIL
/// TypeLowering is currently written. We ought to refactor TypeLowering so that
/// it is not necessary to distinguish these cases.
enum class SILFunctionTypeRepresentation : uint8_t {
  /// A freestanding thick function.
  Thick = uint8_t(FunctionTypeRepresentation::Swift),

  /// A thick function that is represented as an Objective-C block.
  Block = uint8_t(FunctionTypeRepresentation::Block),

  /// A freestanding thin function that needs no context.
  Thin = uint8_t(FunctionTypeRepresentation::Thin),

  /// A C function pointer, which is thin and also uses the C calling
  /// convention.
  CFunctionPointer = uint8_t(FunctionTypeRepresentation::CFunctionPointer),

  /// The value of the greatest AST function representation.
  LastAST = CFunctionPointer,

  /// The value of the least SIL-only function representation.
  FirstSIL = 8,

  /// A Swift instance method.
  Method = FirstSIL,

  /// An Objective-C method.
  ObjCMethod,

  /// A Swift protocol witness.
  WitnessMethod,

  /// A closure invocation function that has not been bound to a context.
  Closure,

  /// A C++ method that takes a "this" argument (not a static C++ method or
  /// constructor). Except for
  /// handling the "this" argument, has the same behavior as "CFunctionPointer".
  CXXMethod,

  /// A KeyPath accessor function, which is thin and also uses the variadic
  /// length generic components serialization in trailing buffer.
  /// Each representation has a different convention for which parameters
  /// have serialized generic type info.
  KeyPathAccessorGetter,
  KeyPathAccessorSetter,
  KeyPathAccessorEquals,
  KeyPathAccessorHash,
};

/// Returns true if the function with this convention doesn't carry a context.
constexpr bool
isThinRepresentation(FunctionTypeRepresentation rep) {
  switch (rep) {
    case FunctionTypeRepresentation::Swift:
    case FunctionTypeRepresentation::Block:
      return false;
    case FunctionTypeRepresentation::Thin:
    case FunctionTypeRepresentation::CFunctionPointer:
      return true;
  }
  llvm_unreachable("Unhandled FunctionTypeRepresentation in switch.");
}

/// Returns true if the function with this convention doesn't carry a context.
constexpr bool
isThinRepresentation(SILFunctionTypeRepresentation rep) {
  switch (rep) {
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Block:
    return false;
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::CXXMethod:
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
  case SILFunctionTypeRepresentation::KeyPathAccessorHash:
    return true;
  }
  llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
}

/// Returns true if the function with this convention carries a context.
template <typename Repr>
constexpr bool
isThickRepresentation(Repr repr) {
  return !isThinRepresentation(repr);
}

/// Returns true if the function with this convention receives generic arguments
/// from KeyPath argument buffer.
constexpr bool
isKeyPathAccessorRepresentation(SILFunctionTypeRepresentation rep) {
  switch (rep) {
    case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
    case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
    case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
    case SILFunctionTypeRepresentation::KeyPathAccessorHash:
      return true;
    case SILFunctionTypeRepresentation::Thick:
    case SILFunctionTypeRepresentation::Block:
    case SILFunctionTypeRepresentation::Thin:
    case SILFunctionTypeRepresentation::Method:
    case SILFunctionTypeRepresentation::ObjCMethod:
    case SILFunctionTypeRepresentation::WitnessMethod:
    case SILFunctionTypeRepresentation::CFunctionPointer:
    case SILFunctionTypeRepresentation::Closure:
    case SILFunctionTypeRepresentation::CXXMethod:
      return false;
  }
  llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
}


constexpr SILFunctionTypeRepresentation
convertRepresentation(FunctionTypeRepresentation rep) {
  switch (rep) {
  case FunctionTypeRepresentation::Swift:
    return SILFunctionTypeRepresentation::Thick;
  case FunctionTypeRepresentation::Block:
    return SILFunctionTypeRepresentation::Block;
  case FunctionTypeRepresentation::Thin:
    return SILFunctionTypeRepresentation::Thin;
  case FunctionTypeRepresentation::CFunctionPointer:
    return SILFunctionTypeRepresentation::CFunctionPointer;
  }
  llvm_unreachable("Unhandled FunctionTypeRepresentation!");
}

inline std::optional<FunctionTypeRepresentation>
convertRepresentation(SILFunctionTypeRepresentation rep) {
  switch (rep) {
  case SILFunctionTypeRepresentation::Thick:
    return {FunctionTypeRepresentation::Swift};
  case SILFunctionTypeRepresentation::Block:
    return {FunctionTypeRepresentation::Block};
  case SILFunctionTypeRepresentation::Thin:
    return {FunctionTypeRepresentation::Thin};
  case SILFunctionTypeRepresentation::CXXMethod:
  case SILFunctionTypeRepresentation::CFunctionPointer:
    return {FunctionTypeRepresentation::CFunctionPointer};
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
  case SILFunctionTypeRepresentation::KeyPathAccessorHash:
    return std::nullopt;
  }
  llvm_unreachable("Unhandled SILFunctionTypeRepresentation!");
}

/// Can this calling convention result in a function being called indirectly
/// through the runtime.
constexpr bool canBeCalledIndirectly(SILFunctionTypeRepresentation rep) {
  switch (rep) {
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::CXXMethod:
    return false;
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
  case SILFunctionTypeRepresentation::KeyPathAccessorHash:
    return true;
  }

  llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
}

template <typename Repr> constexpr bool shouldStoreClangType(Repr repr) {
  static_assert(std::is_same<Repr, FunctionTypeRepresentation>::value ||
                    std::is_same<Repr, SILFunctionTypeRepresentation>::value,
                "Expected a Representation type as the argument type.");
  switch (static_cast<SILFunctionTypeRepresentation>(repr)) {
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
  case SILFunctionTypeRepresentation::CXXMethod:
    return true;
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
  case SILFunctionTypeRepresentation::KeyPathAccessorHash:
    return false;
  }
  llvm_unreachable("Unhandled SILFunctionTypeRepresentation.");
}

// MARK: - ASTExtInfoBuilder
/// A builder type for creating an \c ASTExtInfo.
///
/// The main API public includes the \c withXYZ and \p build() methods.
class ASTExtInfoBuilder {
  friend AnyFunctionType;
  friend ASTExtInfo;

  // If bits are added or removed, then TypeBase::NumAFTExtInfoBits
  // and NumMaskBits must be updated, and they must match.
  //
  //   |representation|noEscape|concurrent|async|throws|isolation|differentiability| SendingResult |
  //   |    0 .. 3    |    4   |    5     |  6  |   7  | 8 .. 10 |     11 .. 13    |         14    |
  //
  enum : unsigned {
    RepresentationMask = 0xF << 0,
    NoEscapeMask = 1 << 4,
    SendableMask = 1 << 5,
    AsyncMask = 1 << 6,
    ThrowsMask = 1 << 7,
    IsolationMaskOffset = 8,
    IsolationMask = 0x7 << IsolationMaskOffset,
    DifferentiabilityMaskOffset = 11,
    DifferentiabilityMask = 0x7 << DifferentiabilityMaskOffset,
    SendingResultMask = 1 << 14,
    NumMaskBits = 15
  };

  static_assert(FunctionTypeIsolation::Mask == 0x7, "update mask manually");

  unsigned bits; // Naturally sized for speed.

  ClangTypeInfo clangTypeInfo;

  Type globalActor;
  Type thrownError;

  ArrayRef<LifetimeDependenceInfo> lifetimeDependencies;

  using Representation = FunctionTypeRepresentation;

  ASTExtInfoBuilder(unsigned bits, ClangTypeInfo clangTypeInfo,
                    Type globalActor, Type thrownError,
                    ArrayRef<LifetimeDependenceInfo> lifetimeDependencies)
      : bits(bits), clangTypeInfo(clangTypeInfo), globalActor(globalActor),
        thrownError(thrownError), lifetimeDependencies(lifetimeDependencies) {
    assert(isThrowing() || !thrownError);
    assert(hasGlobalActorFromBits(bits) == !globalActor.isNull());
  }

public:
  /// An ExtInfoBuilder for a typical Swift function: @convention(swift),
  /// @escaping, non-throwing, non-differentiable.
  ASTExtInfoBuilder()
      : ASTExtInfoBuilder(Representation::Swift, false, false, Type(),
                          DifferentiabilityKind::NonDifferentiable, nullptr,
                          FunctionTypeIsolation::forNonIsolated(),
                          std::nullopt /* LifetimeDependenceInfo */,
                          false /*sendingResult*/) {}

  // Constructor for polymorphic type.
  ASTExtInfoBuilder(Representation rep, bool throws, Type thrownError)
      : ASTExtInfoBuilder(rep, false, throws, thrownError,
                          DifferentiabilityKind::NonDifferentiable, nullptr,
                          FunctionTypeIsolation::forNonIsolated(),
                          std::nullopt /* LifetimeDependenceInfo */,
                          false /*sendingResult*/) {}

  // Constructor with no defaults.
  ASTExtInfoBuilder(Representation rep, bool isNoEscape, bool throws,
                    Type thrownError, DifferentiabilityKind diffKind,
                    const clang::Type *type, FunctionTypeIsolation isolation,
                    ArrayRef<LifetimeDependenceInfo> lifetimeDependencies,
                    bool sendingResult)
      : ASTExtInfoBuilder(
            ((unsigned)rep) | (isNoEscape ? NoEscapeMask : 0) |
                (throws ? ThrowsMask : 0) |
                (((unsigned)diffKind << DifferentiabilityMaskOffset) &
                 DifferentiabilityMask) |
                (unsigned(isolation.getKind()) << IsolationMaskOffset) |
                (sendingResult ? SendingResultMask : 0),
            ClangTypeInfo(type), isolation.getOpaqueType(), thrownError,
            lifetimeDependencies) {}

  void checkInvariants() const;

  /// Check if \c this is well-formed and create an ExtInfo.
  ASTExtInfo build() const;

  constexpr Representation getRepresentation() const {
    unsigned rawRep = bits & RepresentationMask;
    return Representation(rawRep);
  }

  constexpr bool isNoEscape() const { return bits & NoEscapeMask; }

  constexpr bool isSendable() const { return bits & SendableMask; }

  constexpr bool isAsync() const { return bits & AsyncMask; }

  constexpr bool isThrowing() const { return bits & ThrowsMask; }

  constexpr bool hasSendingResult() const { return bits & SendingResultMask; }

  constexpr DifferentiabilityKind getDifferentiabilityKind() const {
    return DifferentiabilityKind((bits & DifferentiabilityMask) >>
                                 DifferentiabilityMaskOffset);
  }

  constexpr bool isDifferentiable() const {
    return getDifferentiabilityKind() >
           DifferentiabilityKind::NonDifferentiable;
  }

  ClangTypeInfo getClangTypeInfo() const { return clangTypeInfo; }

  constexpr SILFunctionTypeRepresentation getSILRepresentation() const {
    unsigned rawRep = bits & RepresentationMask;
    return SILFunctionTypeRepresentation(rawRep);
  }

  Type getGlobalActor() const { return globalActor; }
  Type getThrownError() const { return thrownError; }

  ArrayRef<LifetimeDependenceInfo> getLifetimeDependencies() const {
    return lifetimeDependencies;
  }

  FunctionTypeIsolation::Kind getIsolationKind() const {
    return getIsolationKindFromBits(bits);
  }
  static FunctionTypeIsolation::Kind getIsolationKindFromBits(unsigned bits) {
    return FunctionTypeIsolation::Kind(
             (bits & IsolationMask) >> IsolationMaskOffset);
  }
  bool isIsolationStaticallyErased() const {
    return getIsolationKind() == FunctionTypeIsolation::Kind::Erased;
  }
  static bool hasGlobalActorFromBits(unsigned bits) {
    return getIsolationKindFromBits(bits)
             == FunctionTypeIsolation::Kind::GlobalActor;
  }

  FunctionTypeIsolation getIsolation() const {
    return FunctionTypeIsolation::fromOpaqueValues(getIsolationKind(),
                                                   globalActor);
  }

  constexpr bool hasSelfParam() const {
    switch (getSILRepresentation()) {
    case SILFunctionTypeRepresentation::Thick:
    case SILFunctionTypeRepresentation::Block:
    case SILFunctionTypeRepresentation::Thin:
    case SILFunctionTypeRepresentation::CFunctionPointer:
    case SILFunctionTypeRepresentation::Closure:
    case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
    case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
    case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
    case SILFunctionTypeRepresentation::KeyPathAccessorHash:
      return false;
    case SILFunctionTypeRepresentation::ObjCMethod:
    case SILFunctionTypeRepresentation::Method:
    case SILFunctionTypeRepresentation::WitnessMethod:
    case SILFunctionTypeRepresentation::CXXMethod:
      return true;
    }
    llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
  }

  /// True if the function representation carries context.
  constexpr bool hasContext() const {
    return isThickRepresentation(getSILRepresentation());
  }

  // Note that we don't have setters. That is by design, use
  // the following with methods instead of mutating these objects.
  [[nodiscard]]
  ASTExtInfoBuilder withRepresentation(Representation rep) const {
    return ASTExtInfoBuilder((bits & ~RepresentationMask) | (unsigned)rep,
                             shouldStoreClangType(rep) ? clangTypeInfo
                                                       : ClangTypeInfo(),
                             globalActor, thrownError, lifetimeDependencies);
  }
  [[nodiscard]]
  ASTExtInfoBuilder withNoEscape(bool noEscape = true) const {
    return ASTExtInfoBuilder(
        noEscape ? (bits | NoEscapeMask) : (bits & ~NoEscapeMask),
        clangTypeInfo, globalActor, thrownError, lifetimeDependencies);
  }
  [[nodiscard]]
  ASTExtInfoBuilder withSendable(bool concurrent = true) const {
    return ASTExtInfoBuilder(
        concurrent ? (bits | SendableMask) : (bits & ~SendableMask),
        clangTypeInfo, globalActor, thrownError, lifetimeDependencies);
  }
  [[nodiscard]]
  ASTExtInfoBuilder withAsync(bool async = true) const {
    return ASTExtInfoBuilder(async ? (bits | AsyncMask) : (bits & ~AsyncMask),
                             clangTypeInfo, globalActor, thrownError,
                             lifetimeDependencies);
  }
  [[nodiscard]]
  ASTExtInfoBuilder withThrows(bool throws, Type thrownError) const {
    assert(throws || !thrownError);
    return ASTExtInfoBuilder(
        throws ? (bits | ThrowsMask) : (bits & ~ThrowsMask), clangTypeInfo,
        globalActor, thrownError, lifetimeDependencies);
  }

  [[nodiscard]]
  ASTExtInfoBuilder withThrows() const {
    return withThrows(true, Type());
  }

  [[nodiscard]] ASTExtInfoBuilder withSendingResult(bool sending = true) const {
    return ASTExtInfoBuilder(
        sending ? (bits | SendingResultMask) : (bits & ~SendingResultMask),
        clangTypeInfo, globalActor, thrownError, lifetimeDependencies);
  }

  [[nodiscard]]
  ASTExtInfoBuilder
  withDifferentiabilityKind(DifferentiabilityKind differentiability) const {
    return ASTExtInfoBuilder(
        (bits & ~DifferentiabilityMask) |
            ((unsigned)differentiability << DifferentiabilityMaskOffset),
        clangTypeInfo, globalActor, thrownError, lifetimeDependencies);
  }
  [[nodiscard]]
  ASTExtInfoBuilder withClangFunctionType(const clang::Type *type) const {
    return ASTExtInfoBuilder(bits, ClangTypeInfo(type), globalActor,
                             thrownError, lifetimeDependencies);
  }

  /// Put a SIL representation in the ExtInfo.
  ///
  /// SIL type lowering transiently generates AST function types with SIL
  /// representations. However, they shouldn't persist in the AST, and
  /// don't need to be parsed, printed, or serialized.
  [[nodiscard]]
  ASTExtInfoBuilder
  withSILRepresentation(SILFunctionTypeRepresentation rep) const {
    return ASTExtInfoBuilder((bits & ~RepresentationMask) | (unsigned)rep,
                             shouldStoreClangType(rep) ? clangTypeInfo
                                                       : ClangTypeInfo(),
                             globalActor, thrownError, lifetimeDependencies);
  }

  [[nodiscard]] ASTExtInfoBuilder withLifetimeDependencies(
      llvm::ArrayRef<LifetimeDependenceInfo> lifetimeDependencies) const {
    return ASTExtInfoBuilder(bits, clangTypeInfo, globalActor, thrownError,
                             lifetimeDependencies);
  }

  [[nodiscard]]
  ASTExtInfoBuilder withIsolation(FunctionTypeIsolation isolation) const {
    return ASTExtInfoBuilder(
        (bits & ~IsolationMask) |
            (unsigned(isolation.getKind()) << IsolationMaskOffset),
        clangTypeInfo, isolation.getOpaqueType(), thrownError,
        lifetimeDependencies);
  }

  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.AddInteger(bits);
    ID.AddPointer(clangTypeInfo.getType());
    ID.AddPointer(globalActor.getPointer());
    ID.AddPointer(thrownError.getPointer());
    for (auto info : lifetimeDependencies) {
      info.Profile(ID);
    }
  }

  bool isEqualTo(ASTExtInfoBuilder other, bool useClangTypes) const {
    return bits == other.bits &&
           (useClangTypes ? (clangTypeInfo == other.clangTypeInfo) : true) &&
           globalActor.getPointer() == other.globalActor.getPointer() &&
           thrownError.getPointer() == other.thrownError.getPointer() &&
           lifetimeDependencies == other.lifetimeDependencies;
  }
}; // end ASTExtInfoBuilder

// MARK: - ASTExtInfo
/// Calling convention and related information for AnyFunctionType + subclasses.
///
/// New instances can be made from existing instances via \c ASTExtInfoBuilder,
/// typically using a code pattern like:
/// \code
/// extInfo.intoBuilder().withX(x).withY(y).build()
/// \endcode
class ASTExtInfo {
  friend ASTExtInfoBuilder;
  friend AnyFunctionType;

  ASTExtInfoBuilder builder;

  // Only for use by ASTExtInfoBuilder::build. Don't use it elsewhere!
  ASTExtInfo(ASTExtInfoBuilder builder) : builder(builder) {}

  ASTExtInfo(unsigned bits, ClangTypeInfo clangTypeInfo, Type globalActor,
             Type thrownError,
             llvm::ArrayRef<LifetimeDependenceInfo> lifetimeDependenceInfo)
      : builder(bits, clangTypeInfo, globalActor, thrownError,
                lifetimeDependenceInfo) {
    builder.checkInvariants();
  };

public:
  /// An ExtInfo for a typical Swift function: @convention(swift), @escaping,
  /// non-throwing, non-differentiable.
  ASTExtInfo() : builder() { builder.checkInvariants(); };

  /// Create a builder with the same state as \c this.
  ASTExtInfoBuilder intoBuilder() const { return builder; }

private:
  constexpr unsigned getBits() const { return builder.bits; }

public:
  constexpr FunctionTypeRepresentation getRepresentation() const {
    return builder.getRepresentation();
  }

  constexpr SILFunctionTypeRepresentation getSILRepresentation() const {
    return builder.getSILRepresentation();
  }

  constexpr bool isNoEscape() const { return builder.isNoEscape(); }

  constexpr bool isSendable() const { return builder.isSendable(); }

  constexpr bool isAsync() const { return builder.isAsync(); }

  constexpr bool isThrowing() const { return builder.isThrowing(); }

  constexpr bool hasSendingResult() const { return builder.hasSendingResult(); }

  constexpr DifferentiabilityKind getDifferentiabilityKind() const {
    return builder.getDifferentiabilityKind();
  }

  constexpr bool isDifferentiable() const { return builder.isDifferentiable(); }

  ClangTypeInfo getClangTypeInfo() const { return builder.getClangTypeInfo(); }

  constexpr bool hasSelfParam() const { return builder.hasSelfParam(); }

  constexpr bool hasContext() const { return builder.hasContext(); }

  Type getGlobalActor() const { return builder.getGlobalActor(); }
  Type getThrownError() const { return builder.getThrownError(); }

  ArrayRef<LifetimeDependenceInfo> getLifetimeDependencies() const {
    return builder.getLifetimeDependencies();
  }

  FunctionTypeIsolation getIsolation() const { return builder.getIsolation(); }

  /// Helper method for changing the representation.
  ///
  /// Prefer using \c ASTExtInfoBuilder::withRepresentation for chaining.
  [[nodiscard]]
  ASTExtInfo withRepresentation(ASTExtInfoBuilder::Representation rep) const {
    return builder.withRepresentation(rep).build();
  }

  /// Helper method for changing only the noEscape field.
  ///
  /// Prefer using \c ASTExtInfoBuilder::withNoEscape for chaining.
  [[nodiscard]]
  ASTExtInfo withNoEscape(bool noEscape = true) const {
    return builder.withNoEscape(noEscape).build();
  }

  /// Helper method for changing only the concurrent field.
  ///
  /// Prefer using \c ASTExtInfoBuilder::withSendable for chaining.
  [[nodiscard]]
  ASTExtInfo withSendable(bool isSendable = true) const {
    return builder.withSendable(isSendable).build();
  }

  /// Helper method for changing only the throws field.
  ///
  /// Prefer using \c ASTExtInfoBuilder::withThrows for chaining.
  [[nodiscard]]
  ASTExtInfo withThrows(bool throws, Type thrownError) const {
    return builder.withThrows(throws, thrownError).build();
  }

  /// Helper method for changing only the throws field.
  ///
  /// Prefer using \c ASTExtInfoBuilder::withThrows for chaining.
  [[nodiscard]]
  ASTExtInfo withThrows() const {
    return builder.withThrows(true, Type()).build();
  }

  /// Helper method for changing only the async field.
  ///
  /// Prefer using \c ASTExtInfoBuilder::withAsync for chaining.
  [[nodiscard]]
  ASTExtInfo withAsync(bool async = true) const {
    return builder.withAsync(async).build();
  }

  [[nodiscard]] ASTExtInfo withSendingResult(bool sending = true) const {
    return builder.withSendingResult(sending).build();
  }

  [[nodiscard]]
  ASTExtInfo withIsolation(FunctionTypeIsolation isolation) const {
    return builder.withIsolation(isolation).build();
  }

  [[nodiscard]]
  ASTExtInfo withoutIsolation() const {
    return builder.withIsolation(FunctionTypeIsolation::forNonIsolated())
      .build();
  }

  [[nodiscard]]
  ASTExtInfo withGlobalActor(Type globalActor) const {
    return builder.withIsolation(
             FunctionTypeIsolation::forGlobalActor(globalActor))
      .build();
  }

  [[nodiscard]] ASTExtInfo withLifetimeDependencies(
      ArrayRef<LifetimeDependenceInfo> lifetimeDependencies) const {
    return builder.withLifetimeDependencies(lifetimeDependencies).build();
  }

  void Profile(llvm::FoldingSetNodeID &ID) const { builder.Profile(ID); }

  bool isEqualTo(ASTExtInfo other, bool useClangTypes) const {
    return builder.isEqualTo(other.builder, useClangTypes);
  }
}; // end ASTExtInfo

// MARK: - SILFunctionLanguage

/// A language-level calling convention.
enum class SILFunctionLanguage : uint8_t {
  /// A variation of the Swift calling convention.
  Swift = 0,

  /// A variation of the C calling convention.
  C,
};

/// Map a SIL function representation to the base language calling convention
/// it uses.
constexpr
SILFunctionLanguage getSILFunctionLanguage(SILFunctionTypeRepresentation rep) {
  switch (rep) {
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
  case SILFunctionTypeRepresentation::CXXMethod:
    return SILFunctionLanguage::C;
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::Closure:
  case SILFunctionTypeRepresentation::KeyPathAccessorGetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorSetter:
  case SILFunctionTypeRepresentation::KeyPathAccessorEquals:
  case SILFunctionTypeRepresentation::KeyPathAccessorHash:
    return SILFunctionLanguage::Swift;
  }

  llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
}

// MARK: - SILExtInfoBuilder
/// A builder type for creating an \c SILExtInfo.
///
/// The main API public includes the \c withXYZ and \p build() methods.
class SILExtInfoBuilder {
  friend SILExtInfo;
  friend SILFunctionType;

  // If bits are added or removed, then TypeBase::SILFunctionTypeBits
  // and NumMaskBits must be updated, and they must match.

  //   |representation|pseudogeneric| noescape | concurrent | async
  //   |    0 .. 4    |      5      |     6    |     7      |   8
  //   |differentiability|unimplementable|
  //   |     9 .. 11     |      12       |
  //
  enum : unsigned {
    RepresentationMask = 0x1F << 0,
    PseudogenericMask = 1 << 5,
    NoEscapeMask = 1 << 6,
    SendableMask = 1 << 7,
    AsyncMask = 1 << 8,
    DifferentiabilityMaskOffset = 9,
    DifferentiabilityMask = 0x7 << DifferentiabilityMaskOffset,
    UnimplementableMask = 1 << 12,
    ErasedIsolationMask = 1 << 13,
    NumMaskBits = 14
  };

  unsigned bits; // Naturally sized for speed.

  ClangTypeInfo clangTypeInfo;

  ArrayRef<LifetimeDependenceInfo> lifetimeDependencies;

  using Language = SILFunctionLanguage;
  using Representation = SILFunctionTypeRepresentation;

  SILExtInfoBuilder(unsigned bits, ClangTypeInfo clangTypeInfo,
                    ArrayRef<LifetimeDependenceInfo> lifetimeDependencies)
      : bits(bits), clangTypeInfo(clangTypeInfo.getCanonical()),
        lifetimeDependencies(lifetimeDependencies) {}

  static constexpr unsigned makeBits(Representation rep, bool isPseudogeneric,
                                     bool isNoEscape, bool isSendable,
                                     bool isAsync, bool isUnimplementable,
                                     SILFunctionTypeIsolation isolation,
                                     DifferentiabilityKind diffKind) {
    return ((unsigned)rep) | (isPseudogeneric ? PseudogenericMask : 0) |
           (isNoEscape ? NoEscapeMask : 0) | (isSendable ? SendableMask : 0) |
           (isAsync ? AsyncMask : 0) |
           (isUnimplementable ? UnimplementableMask : 0) |
           (isolation == SILFunctionTypeIsolation::Erased ? ErasedIsolationMask
                                                          : 0) |
           (((unsigned)diffKind << DifferentiabilityMaskOffset) &
            DifferentiabilityMask);
  }

public:
  /// An ExtInfoBuilder for a typical Swift function: thick, @escaping,
  /// non-pseudogeneric, non-differentiable.
  SILExtInfoBuilder()
      : SILExtInfoBuilder(
            makeBits(SILFunctionTypeRepresentation::Thick, false, false, false,
                     false, false, SILFunctionTypeIsolation::Unknown,
                     DifferentiabilityKind::NonDifferentiable),
            ClangTypeInfo(nullptr), /*LifetimeDependenceInfo*/ std::nullopt) {}

  SILExtInfoBuilder(Representation rep, bool isPseudogeneric, bool isNoEscape,
                    bool isSendable, bool isAsync, bool isUnimplementable,
                    SILFunctionTypeIsolation isolation,
                    DifferentiabilityKind diffKind, const clang::Type *type,
                    ArrayRef<LifetimeDependenceInfo> lifetimeDependenceInfo)
      : SILExtInfoBuilder(makeBits(rep, isPseudogeneric, isNoEscape, isSendable,
                                   isAsync, isUnimplementable, isolation,
                                   diffKind),
                          ClangTypeInfo(type), lifetimeDependenceInfo) {}

  // Constructor for polymorphic type.
  SILExtInfoBuilder(ASTExtInfoBuilder info, bool isPseudogeneric)
      : SILExtInfoBuilder(makeBits(info.getSILRepresentation(), isPseudogeneric,
                                   info.isNoEscape(), info.isSendable(),
                                   info.isAsync(), /*unimplementable*/ false,
                                   info.getIsolation().isErased()
                                       ? SILFunctionTypeIsolation::Erased
                                       : SILFunctionTypeIsolation::Unknown,
                                   info.getDifferentiabilityKind()),
                          info.getClangTypeInfo(),
                          info.getLifetimeDependencies()) {}

  void checkInvariants() const;

  /// Check if \c this is well-formed and create an ExtInfo.
  SILExtInfo build() const;

  /// What is the abstract representation of this function value?
  constexpr Representation getRepresentation() const {
    return Representation(bits & RepresentationMask);
  }

  constexpr Language getLanguage() const {
    return getSILFunctionLanguage(getRepresentation());
  }

  /// Is this function pseudo-generic?  A pseudo-generic function
  /// is not permitted to dynamically depend on its type arguments.
  constexpr bool isPseudogeneric() const { return bits & PseudogenericMask; }

  // Is this function guaranteed to be no-escape by the type system?
  constexpr bool isNoEscape() const { return bits & NoEscapeMask; }

  constexpr bool isSendable() const { return bits & SendableMask; }

  constexpr bool isAsync() const { return bits & AsyncMask; }

  constexpr DifferentiabilityKind getDifferentiabilityKind() const {
    return DifferentiabilityKind((bits & DifferentiabilityMask) >>
                                 DifferentiabilityMaskOffset);
  }

  constexpr bool isDifferentiable() const {
    return getDifferentiabilityKind() !=
           DifferentiabilityKind::NonDifferentiable;
  }

  constexpr bool isUnimplementable() const {
    return bits & UnimplementableMask;
  }

  /// Does this function type have erased isolation (i.e. is it the
  /// lowering of an @isolated(any) function type)?
  constexpr bool hasErasedIsolation() const {
    return bits & ErasedIsolationMask;
  }

  constexpr SILFunctionTypeIsolation getIsolation() const {
    return hasErasedIsolation()
              ? SILFunctionTypeIsolation::Erased
              : SILFunctionTypeIsolation::Unknown;
  }

  /// Get the underlying ClangTypeInfo value.
  ClangTypeInfo getClangTypeInfo() const { return clangTypeInfo; }

  ArrayRef<LifetimeDependenceInfo> getLifetimeDependencies() const {
    return lifetimeDependencies;
  }

  constexpr bool hasSelfParam() const {
    switch (getRepresentation()) {
    case Representation::Thick:
    case Representation::Block:
    case Representation::Thin:
    case Representation::CFunctionPointer:
    case Representation::Closure:
    case Representation::KeyPathAccessorGetter:
    case Representation::KeyPathAccessorSetter:
    case Representation::KeyPathAccessorEquals:
    case Representation::KeyPathAccessorHash:
      return false;
    case Representation::ObjCMethod:
    case Representation::Method:
    case Representation::WitnessMethod:
    case SILFunctionTypeRepresentation::CXXMethod:
      return true;
    }
    llvm_unreachable("Unhandled Representation in switch.");
  }

  /// True if the function representation carries context.
  constexpr bool hasContext() const {
    switch (getRepresentation()) {
    case Representation::Thick:
    case Representation::Block:
      return true;
    case Representation::Thin:
    case Representation::CFunctionPointer:
    case Representation::ObjCMethod:
    case Representation::Method:
    case Representation::WitnessMethod:
    case Representation::Closure:
    case SILFunctionTypeRepresentation::CXXMethod:
    case Representation::KeyPathAccessorGetter:
    case Representation::KeyPathAccessorSetter:
    case Representation::KeyPathAccessorEquals:
    case Representation::KeyPathAccessorHash:
      return false;
    }
    llvm_unreachable("Unhandled Representation in switch.");
  }

  // Note that we don't have setters. That is by design, use
  // the following with methods instead of mutating these objects.
  [[nodiscard]]
  SILExtInfoBuilder withRepresentation(Representation rep) const {
    return SILExtInfoBuilder((bits & ~RepresentationMask) | (unsigned)rep,
                             shouldStoreClangType(rep) ? clangTypeInfo
                                                       : ClangTypeInfo(),
                             lifetimeDependencies);
  }
  [[nodiscard]]
  SILExtInfoBuilder withIsPseudogeneric(bool isPseudogeneric = true) const {
    return SILExtInfoBuilder(isPseudogeneric ? (bits | PseudogenericMask)
                                             : (bits & ~PseudogenericMask),
                             clangTypeInfo, lifetimeDependencies);
  }
  [[nodiscard]]
  SILExtInfoBuilder withNoEscape(bool noEscape = true) const {
    return SILExtInfoBuilder(noEscape ? (bits | NoEscapeMask)
                                      : (bits & ~NoEscapeMask),
                             clangTypeInfo, lifetimeDependencies);
  }
  [[nodiscard]]
  SILExtInfoBuilder withSendable(bool isSendable = true) const {
    return SILExtInfoBuilder(isSendable ? (bits | SendableMask)
                                        : (bits & ~SendableMask),
                             clangTypeInfo, lifetimeDependencies);
  }

  [[nodiscard]]
  SILExtInfoBuilder withAsync(bool isAsync = true) const {
    return SILExtInfoBuilder(isAsync ? (bits | AsyncMask) : (bits & ~AsyncMask),
                             clangTypeInfo, lifetimeDependencies);
  }

  [[nodiscard]]
  SILExtInfoBuilder withErasedIsolation(bool erased = true) const {
    return SILExtInfoBuilder(erased ? (bits | ErasedIsolationMask)
                                    : (bits & ~ErasedIsolationMask),
                             clangTypeInfo, lifetimeDependencies);
  }
  [[nodiscard]]
  SILExtInfoBuilder withIsolation(SILFunctionTypeIsolation isolation) const {
    switch (isolation) {
    case SILFunctionTypeIsolation::Unknown:
      return *this;
    case SILFunctionTypeIsolation::Erased:
      return withErasedIsolation(true);
    }
    llvm_unreachable("bad kind");
  }
  [[nodiscard]]
  SILExtInfoBuilder withUnimplementable(bool isUnimplementable = true) const {
    return SILExtInfoBuilder(isUnimplementable ? (bits | UnimplementableMask)
                                               : (bits & ~UnimplementableMask),
                             clangTypeInfo, lifetimeDependencies);
  }

  [[nodiscard]]
  SILExtInfoBuilder
  withDifferentiabilityKind(DifferentiabilityKind differentiability) const {
    return SILExtInfoBuilder(
        (bits & ~DifferentiabilityMask) |
            ((unsigned)differentiability << DifferentiabilityMaskOffset),
        clangTypeInfo, lifetimeDependencies);
  }
  [[nodiscard]]
  SILExtInfoBuilder withClangFunctionType(const clang::Type *type) const {
    return SILExtInfoBuilder(bits, ClangTypeInfo(type).getCanonical(),
                             lifetimeDependencies);
  }
  [[nodiscard]] SILExtInfoBuilder withLifetimeDependencies(
      ArrayRef<LifetimeDependenceInfo> lifetimeDependenceInfo) const {
    return SILExtInfoBuilder(bits, clangTypeInfo, lifetimeDependenceInfo);
  }

  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.AddInteger(bits);
    ID.AddPointer(clangTypeInfo.getType());
    for (auto info : lifetimeDependencies) {
      info.Profile(ID);
    }
  }

  bool isEqualTo(SILExtInfoBuilder other, bool useClangTypes) const {
    return bits == other.bits &&
           (useClangTypes ? (clangTypeInfo == other.clangTypeInfo) : true);
  }
}; // end SILExtInfoBuilder

// MARK: - SILExtInfo
/// Calling convention information for SILFunctionType.
///
/// New instances can be made from existing instances via \c SILExtInfoBuilder,
/// typically using a code pattern like:
/// \code
/// extInfo.intoBuilder().withX(x).withY(y).build()
/// \endcode
class SILExtInfo {
  friend SILExtInfoBuilder;
  friend SILFunctionType;

  SILExtInfoBuilder builder;

  // Only for use by SILExtInfoBuilder::build. Don't use it elsewhere!
  SILExtInfo(SILExtInfoBuilder builder) : builder(builder) {}

  SILExtInfo(unsigned bits, ClangTypeInfo clangTypeInfo,
             llvm::ArrayRef<LifetimeDependenceInfo> lifetimeDependencies)
      : builder(bits, clangTypeInfo, lifetimeDependencies) {
    builder.checkInvariants();
  };

public:
  /// An ExtInfo for a typical Swift function: thick, @escaping,
  /// non-pseudogeneric, non-differentiable.
  SILExtInfo() : builder() { builder.checkInvariants(); };

  SILExtInfo(ASTExtInfo info, bool isPseudogeneric)
      : builder(info.intoBuilder(), isPseudogeneric) {
    builder.checkInvariants();
  }

  /// A default ExtInfo but with a Thin convention.
  static SILExtInfo getThin() {
    return SILExtInfoBuilder(
               SILExtInfoBuilder::Representation::Thin, false, false, false,
               false, false, SILFunctionTypeIsolation::Unknown,
               DifferentiabilityKind::NonDifferentiable, nullptr, {})
        .build();
  }

  /// Create a builder with the same state as \c this.
  SILExtInfoBuilder intoBuilder() const { return builder; }

private:
  constexpr unsigned getBits() const { return builder.bits; }

public:
  constexpr SILFunctionTypeRepresentation getRepresentation() const {
    return builder.getRepresentation();
  }

  constexpr SILFunctionLanguage getLanguage() const {
    return builder.getLanguage();
  }

  constexpr bool isPseudogeneric() const { return builder.isPseudogeneric(); }

  constexpr bool isNoEscape() const { return builder.isNoEscape(); }

  constexpr bool isSendable() const { return builder.isSendable(); }

  constexpr bool isAsync() const { return builder.isAsync(); }

  constexpr bool isUnimplementable() const {
    return builder.isUnimplementable();
  }

  constexpr bool hasErasedIsolation() const {
    return builder.hasErasedIsolation();
  }
  constexpr SILFunctionTypeIsolation getIsolation() const {
    return builder.getIsolation();
  }

  constexpr DifferentiabilityKind getDifferentiabilityKind() const {
    return builder.getDifferentiabilityKind();
  }

  constexpr bool isDifferentiable() const { return builder.isDifferentiable(); }

  ClangTypeInfo getClangTypeInfo() const { return builder.getClangTypeInfo(); }

  ArrayRef<LifetimeDependenceInfo> getLifetimeDependencies() const {
    return builder.getLifetimeDependencies();
  }

  constexpr bool hasSelfParam() const { return builder.hasSelfParam(); }

  constexpr bool hasContext() const { return builder.hasContext(); }

  /// Helper method for changing the Representation.
  ///
  /// Prefer using \c SILExtInfoBuilder::withRepresentation for chaining.
  SILExtInfo withRepresentation(SILExtInfoBuilder::Representation rep) const {
    return builder.withRepresentation(rep).build();
  }

  /// Helper method for changing only the NoEscape field.
  ///
  /// Prefer using \c SILExtInfoBuilder::withNoEscape for chaining.
  SILExtInfo withNoEscape(bool noEscape = true) const {
    return builder.withNoEscape(noEscape).build();
  }
  
  SILExtInfo withSendable(bool isSendable = true) const {
    return builder.withSendable(isSendable).build();
  }

  SILExtInfo withAsync(bool isAsync = true) const {
    return builder.withAsync(isAsync).build();
  }

  SILExtInfo withErasedIsolation(bool erased = true) const {
    return builder.withErasedIsolation(erased).build();
  }

  SILExtInfo withUnimplementable(bool isUnimplementable = true) const {
    return builder.withUnimplementable(isUnimplementable).build();
  }

  SILExtInfo withLifetimeDependencies(
      ArrayRef<LifetimeDependenceInfo> lifetimeDependencies) const {
    return builder.withLifetimeDependencies(lifetimeDependencies);
  }

  void Profile(llvm::FoldingSetNodeID &ID) const { builder.Profile(ID); }

  bool isEqualTo(SILExtInfo other, bool useClangTypes) const {
    return builder.isEqualTo(other.builder, useClangTypes);
  }

  std::optional<UnexpectedClangTypeError> checkClangType() const;
};

/// Helper function to obtain the useClangTypes parameter for checking equality
/// of ExtInfos.
///
/// Typically, the argument will be a function type which was used to obtain one
/// of the ExtInfos.
template <typename HasContext> bool useClangTypes(HasContext hasContext) {
  return hasContext->getASTContext().LangOpts.UseClangFunctionTypes;
}

} // end namespace swift

#endif // SWIFT_EXTINFO_H
