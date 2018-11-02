//===--- SpecializationMangler.h - mangling of specializations --*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_UTILS_SPECIALIZATIONMANGLER_H
#define SWIFT_SILOPTIMIZER_UTILS_SPECIALIZATIONMANGLER_H

#include "swift/Demangling/Demangler.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/AST/ASTMangler.h"
#include "swift/SIL/SILLinkage.h"
#include "swift/SIL/SILFunction.h"

namespace swift {
namespace Mangle {

enum class SpecializationKind : uint8_t {
  Generic,
  NotReAbstractedGeneric,
  FunctionSignature,
};
  
/// Inject SpecializationPass into the Mangle namespace.
using SpecializationPass = Demangle::SpecializationPass;

/// The base class for specialization mangles.
class SpecializationMangler : public Mangle::ASTMangler {
protected:
  /// The specialization pass.
  SpecializationPass Pass;

  IsSerialized_t Serialized;

  /// The original function which is specialized.
  SILFunction *Function;

  llvm::SmallVector<char, 32> ArgOpStorage;
  llvm::raw_svector_ostream ArgOpBuffer;

protected:
  SpecializationMangler(SpecializationPass P, IsSerialized_t Serialized,
                        SILFunction *F)
      : Pass(P), Serialized(Serialized), Function(F), ArgOpBuffer(ArgOpStorage) {}

  SILFunction *getFunction() const { return Function; }

  void beginMangling();

  /// Finish the mangling of the symbol and return the mangled name.
  std::string finalize();

  void appendSpecializationOperator(StringRef Op) {
    appendOperator(Op, StringRef(ArgOpStorage.data(), ArgOpStorage.size()));
  }
};

// The mangler for specialized generic functions.
class GenericSpecializationMangler : public SpecializationMangler {

  SubstitutionList Subs;
  bool isReAbstracted;

public:

  GenericSpecializationMangler(SILFunction *F,
                               SubstitutionList Subs,
                               IsSerialized_t Serialized,
                               bool isReAbstracted)
    : SpecializationMangler(SpecializationPass::GenericSpecializer, Serialized, F),
      Subs(Subs), isReAbstracted(isReAbstracted) {}

  std::string mangle();
};

class PartialSpecializationMangler : public SpecializationMangler {

  CanSILFunctionType SpecializedFnTy;
  bool isReAbstracted;
  
public:
  PartialSpecializationMangler(SILFunction *F,
                               CanSILFunctionType SpecializedFnTy,
                               IsSerialized_t Serialized,
                               bool isReAbstracted)
    : SpecializationMangler(SpecializationPass::GenericSpecializer, Serialized, F),
      SpecializedFnTy(SpecializedFnTy), isReAbstracted(isReAbstracted) {}

  std::string mangle();
};

// The mangler for functions where arguments are specialized.
class FunctionSignatureSpecializationMangler : public SpecializationMangler {

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
    GuaranteedToOwned=256,
    First_OptionSetEntry=32, LastOptionSetEntry=32768,
  };

  using ArgInfo = std::pair<ArgumentModifierIntBase,
                            NullablePtr<SILInstruction>>;
  // Information for each SIL argument in the original function before
  // specialization. This includes SIL indirect result argument required for
  // the original function type at the current stage of compilation.
  llvm::SmallVector<ArgInfo, 8> OrigArgs;

  ReturnValueModifierIntBase ReturnValue;

public:
  FunctionSignatureSpecializationMangler(SpecializationPass Pass,
                                         IsSerialized_t Serialized,
                                         SILFunction *F);
  void setArgumentConstantProp(unsigned OrigArgIdx, LiteralInst *LI);
  void setArgumentClosureProp(unsigned OrigArgIdx, PartialApplyInst *PAI);
  void setArgumentClosureProp(unsigned OrigArgIdx,
                              ThinToThickFunctionInst *TTTFI);
  void setArgumentDead(unsigned OrigArgIdx);
  void setArgumentOwnedToGuaranteed(unsigned OrigArgIdx);
  void setArgumentGuaranteedToOwned(unsigned OrigArgIdx);
  void setArgumentSROA(unsigned OrigArgIdx);
  void setArgumentBoxToValue(unsigned OrigArgIdx);
  void setArgumentBoxToStack(unsigned OrigArgIdx);
  void setReturnValueOwnedToUnowned();

  std::string mangle(int UniqueID = 0);
  
private:
  void mangleConstantProp(LiteralInst *LI);
  void mangleClosureProp(SILInstruction *Inst);
  void mangleArgument(ArgumentModifierIntBase ArgMod,
                      NullablePtr<SILInstruction> Inst);
  void mangleReturnValue(ReturnValueModifierIntBase RetMod);
};

} // end namespace Mangle
} // end namespace swift

#endif
