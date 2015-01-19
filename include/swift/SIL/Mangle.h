//===--- Mangle.h - Interface to SIL specific symbol mangling ---*- C++ -*-===//
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

#ifndef SWIFT_SIL_MANGLE_H
#define SWIFT_SIL_MANGLE_H

#include "llvm/ADT/DenseMap.h"
#include "swift/Basic/Demangle.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILFunction.h"

namespace swift {

class AbstractClosureExpr;

namespace Mangle {

enum class SpecializationKind : uint8_t {
  Generic,
  FunctionSignature,
};

/// Inject SpecializationPass into the Mangle namespace.
using SpecializationPass = Demangle::SpecializationPass;

class SpecializationManglerBase {
protected:
  SpecializationKind Kind;
  SpecializationPass Pass;
  Mangler &M;
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
                            Mangler &M, SILFunction *F)
      : Kind(K), Pass(P), M(M), Function(F) {}

  llvm::raw_ostream &getBuffer() { return M.Buffer; }
  SILFunction *getFunction() const { return Function; }
  Mangler &getMangler() const { return M; }

  void mangleKind() {
    switch (Kind) {
    case SpecializationKind::Generic:
      M.Buffer << "g";
      break;
    case SpecializationKind::FunctionSignature:
      M.Buffer << "f";
      break;
    }
  }

  void manglePass() {
    M.Buffer << encodeSpecializationPass(Pass);
  }

  void mangleSpecializationPrefix() {
    M.Buffer << "_TTS";
  }

  void mangleFunctionName() {
    M.Buffer << "_" << Function->getName();
  }
};

/// A class that describes the mangling that should be produced for a
/// specific specialization kind.
template <typename SubType>
class SpecializationMangler : public SpecializationManglerBase {
  SubType *asImpl() { return reinterpret_cast<SubType *>(this); }
public:

  ~SpecializationMangler() = default;

  /// This should never be copied.
  SpecializationMangler(const SpecializationMangler &) = delete;

  /// This should never be moved.
  SpecializationMangler(SpecializationMangler &&) = delete;

  void mangle() {
    mangleSpecializationPrefix();
    mangleKind();
    manglePass();
    asImpl()->mangleSpecialization();
    mangleFunctionName();
  }

protected:
  SpecializationMangler(SpecializationKind K, SpecializationPass P, Mangler &M,
                        SILFunction *F)
      : SpecializationManglerBase(K, P, M, F) {}
};

class GenericSpecializationMangler :
  public SpecializationMangler<GenericSpecializationMangler> {

  friend class SpecializationMangler<GenericSpecializationMangler>;

  ArrayRef<Substitution> Subs;

public:
  GenericSpecializationMangler(Mangler &M, SILFunction *F,
                               ArrayRef<Substitution> Subs)
    : SpecializationMangler(SpecializationKind::Generic,
                            SpecializationPass::GenericSpecializer,
                            M, F), Subs(Subs) {}

private:
  void mangleSpecialization();
};

class FunctionSignatureSpecializationMangler
  : public SpecializationMangler<FunctionSignatureSpecializationMangler> {

  friend class SpecializationMangler<FunctionSignatureSpecializationMangler>;

  // We use this private typealias to make it easy to expand ArgumentModifier's
  // size if we need to.
  using ArgumentModifierIntBase = uint16_t;
  enum class ArgumentModifier : ArgumentModifierIntBase {
    // Option Space 4 bits (i.e. 16 options).
    Unmodified=0,
    Dead=1,
    ConstantProp=2,
    ClosureProp=3,
    InOutToValue=4,
    First_Option=0, Last_Option=31,

    // Option Set Space. 12 bits (i.e. 12 option).
    OwnedToGuaranteed=32,
    SROA=64,
    First_OptionSetEntry=32, LastOptionSetEntry=32768,
  };

  using ArgInfo = std::pair<ArgumentModifierIntBase,
                            NullablePtr<SILInstruction>>;
  llvm::SmallVector<ArgInfo, 8> Args;

public:
  FunctionSignatureSpecializationMangler(SpecializationPass Pass,
                                         Mangler &M, SILFunction *F);
  void setArgumentConstantProp(unsigned ArgNo, LiteralInst *LI);
  void setArgumentClosureProp(unsigned ArgNo, PartialApplyInst *PAI);
  void setArgumentDead(unsigned ArgNo);
  void setArgumentOwnedToGuaranteed(unsigned ArgNo);
  void setArgumentSROA(unsigned ArgNo);
  void setArgumentInOutToValue(unsigned ArgNo);

private:
  void mangleSpecialization();
  void mangleConstantProp(LiteralInst *LI);
  void mangleClosureProp(PartialApplyInst *PAI);
  void mangleArgument(ArgumentModifierIntBase ArgMod,
                      NullablePtr<SILInstruction> Inst);
};

} // end namespace Mangle
} // end namespace swift

#endif
