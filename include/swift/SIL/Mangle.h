//===--- Mangle.h - Interface to SIL specific symbol mangling ---*- C++ -*-===//
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

#ifndef SWIFT_SIL_MANGLE_H
#define SWIFT_SIL_MANGLE_H

#include "swift/Basic/Demangle.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {

class AbstractClosureExpr;

enum class SpecializationKind : uint8_t {
  Generic,
  NotReAbstractedGeneric,
  Partial,
  NotReAbstractedPartial,
  FunctionSignature,
};

/// Inject SpecializationPass into the Mangle namespace.
using SpecializationPass = Demangle::SpecializationPass;

class SpecializationManglerBase {
protected:
  SpecializationKind Kind;
  SpecializationPass Pass;
  Mangle::Mangler &M;
  IsFragile_t Fragile;
  SILFunction *Function;

public:
  ~SpecializationManglerBase() = default;

  /// These should never be copied.
  SpecializationManglerBase(const SpecializationManglerBase &) = delete;

  /// These should never be moved.
  SpecializationManglerBase(SpecializationManglerBase &&) = delete;

  template <typename T>
  friend class SpecializationMangler;

protected:
  SpecializationManglerBase(SpecializationKind K, SpecializationPass P,
                            Mangle::Mangler &M, IsFragile_t Fragile,
                            SILFunction *F)
      : Kind(K), Pass(P), M(M), Fragile(Fragile), Function(F) {}

  SILFunction *getFunction() const { return Function; }
  Mangle::Mangler &getMangler() const { return M; }

  void mangleKind() {
    switch (Kind) {
    case SpecializationKind::Generic:
      M.append("g");
      break;
    case SpecializationKind::NotReAbstractedGeneric:
      M.append("r");
      break;
    case SpecializationKind::Partial:
      M.append("p");
      break;
    case SpecializationKind::NotReAbstractedPartial:
      M.append("P");
      break;
    case SpecializationKind::FunctionSignature:
      M.append("f");
      break;
    }
  }

  void manglePass() {
    M.append(encodeSpecializationPass(Pass));
  }

  void mangleSpecializationPrefix() {
    M.append("_TTS");
  }

  void mangleFragile() {
    if (Fragile)
      M.append("q");
  }

  void mangleFunctionName() {
    M.append("_");
    M.appendSymbol(Function->getName());
  }
};

/// A class that describes the mangling that should be produced for a
/// specific specialization kind.
template <typename SubType>
class SpecializationMangler : public SpecializationManglerBase {
  SubType *asImpl() { return static_cast<SubType *>(this); }
public:
  Mangle::Mangler &getMangler() const { return M; }

  ~SpecializationMangler() = default;

  /// This should never be copied.
  SpecializationMangler(const SpecializationMangler &) = delete;

  /// This should never be moved.
  SpecializationMangler(SpecializationMangler &&) = delete;

  void mangle() {
    mangleSpecializationPrefix();
    mangleKind();
    mangleFragile();
    manglePass();
    asImpl()->mangleSpecialization();
    mangleFunctionName();
  }

protected:
  SpecializationMangler(SpecializationKind K, SpecializationPass P,
                        Mangle::Mangler &M, IsFragile_t Fragile,
                        SILFunction *F)
      : SpecializationManglerBase(K, P, M, Fragile, F) {}
};

class GenericSpecializationMangler :
  public SpecializationMangler<GenericSpecializationMangler> {

  friend class SpecializationMangler<GenericSpecializationMangler>;

  SubstitutionList Subs;

public:

  enum ReAbstractionMode {
    ReAbstracted,
    NotReabstracted
  };

  GenericSpecializationMangler(Mangle::Mangler &M, SILFunction *F,
                               SubstitutionList Subs, IsFragile_t Fragile,
                               ReAbstractionMode isReAbstracted = ReAbstracted)
      : SpecializationMangler(isReAbstracted == ReAbstracted
                                  ? SpecializationKind::Generic
                                  : SpecializationKind::NotReAbstractedGeneric,
                              SpecializationPass::GenericSpecializer, M,
                              Fragile, F),
        Subs(Subs) {
  }

private:
  void mangleSpecialization();
};

class PartialSpecializationMangler :
  public SpecializationMangler<PartialSpecializationMangler> {

  friend class SpecializationMangler<PartialSpecializationMangler>;

  CanSILFunctionType SpecializedFnTy;
public:

  enum ReAbstractionMode {
    ReAbstracted,
    NotReabstracted
  };

  PartialSpecializationMangler(Mangle::Mangler &M,
                               SILFunction *F,
                               CanSILFunctionType SpecializedFnTy,
                               IsFragile_t Fragile,
                               ReAbstractionMode isReAbstracted = ReAbstracted)
    : SpecializationMangler(isReAbstracted == ReAbstracted ?
                              SpecializationKind::Partial :
                              SpecializationKind::NotReAbstractedPartial,
                            SpecializationPass::GenericSpecializer,
                            M, Fragile, F), SpecializedFnTy(SpecializedFnTy) {
    }

private:
  void mangleSpecialization();
};

class FunctionSignatureSpecializationMangler
  : public SpecializationMangler<FunctionSignatureSpecializationMangler> {

  friend class SpecializationMangler<FunctionSignatureSpecializationMangler>;

  using ReturnValueModifierIntBase = uint16_t;
  enum class ReturnValueModifier : ReturnValueModifierIntBase {
    // Option Space 4 bits (i.e. 16 options).
    Unmodified=0,
    First_Option=0, Last_Option=31,

    // Option Set Space. 12 bits (i.e. 12 option).
    Dead=32,
    OwnedToUnowned=64,
    First_OptionSetEntry=32, LastOptionSetEntry=32768,
  };

  // We use this private typealias to make it easy to expand ArgumentModifier's
  // size if we need to.
  using ArgumentModifierIntBase = uint16_t;
  enum class ArgumentModifier : ArgumentModifierIntBase {
    // Option Space 4 bits (i.e. 16 options).
    Unmodified=0,
    ConstantProp=1,
    ClosureProp=2,
    BoxToValue=3,
    BoxToStack=4,
    First_Option=0, Last_Option=31,

    // Option Set Space. 12 bits (i.e. 12 option).
    Dead=32,
    OwnedToGuaranteed=64,
    SROA=128,
    First_OptionSetEntry=32, LastOptionSetEntry=32768,
  };

  using ArgInfo = std::pair<ArgumentModifierIntBase,
                            NullablePtr<SILInstruction>>;
  llvm::SmallVector<ArgInfo, 8> Args;

  ReturnValueModifierIntBase ReturnValue;

public:
  FunctionSignatureSpecializationMangler(SpecializationPass Pass,
                                         Mangle::Mangler &M,
                                         IsFragile_t Fragile,
                                         SILFunction *F);
  void setArgumentConstantProp(unsigned ArgNo, LiteralInst *LI);
  void setArgumentClosureProp(unsigned ArgNo, PartialApplyInst *PAI);
  void setArgumentClosureProp(unsigned ArgNo, ThinToThickFunctionInst *TTTFI);
  void setArgumentDead(unsigned ArgNo);
  void setArgumentOwnedToGuaranteed(unsigned ArgNo);
  void setArgumentSROA(unsigned ArgNo);
  void setArgumentBoxToValue(unsigned ArgNo);
  void setArgumentBoxToStack(unsigned ArgNo);
  void setReturnValueOwnedToUnowned();

private:
  void mangleSpecialization();
  void mangleConstantProp(LiteralInst *LI);
  void mangleClosureProp(PartialApplyInst *PAI);
  void mangleClosureProp(ThinToThickFunctionInst *TTTFI);
  void mangleArgument(ArgumentModifierIntBase ArgMod,
                      NullablePtr<SILInstruction> Inst);
  void mangleReturnValue(ReturnValueModifierIntBase RetMod);
};

} // end namespace swift

#endif
