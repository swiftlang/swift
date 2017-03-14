//===--- Mangle.cpp - SIL specific name Mangling --------------------------===//
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
//  This file implements declaration specialized name mangling for SIL.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/Mangle.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Demangling/Punycode.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/CharInfo.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MD5.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace Mangle;

//===----------------------------------------------------------------------===//
//                           Generic Specialization
//===----------------------------------------------------------------------===//

void GenericSpecializationMangler::mangleSpecialization() {
  Mangler &M = getMangler();
  // This is a full specialization.
  SILFunctionType *FTy = Function->getLoweredFunctionType();
  CanGenericSignature Sig = FTy->getGenericSignature();
  auto SubMap = Sig->getSubstitutionMap(Subs);
  Sig->enumeratePairedRequirements(
    [&](Type depTy, ArrayRef<Requirement> reqts) {
      if (!depTy->is<GenericTypeParamType>())
        return false;

      M.mangleType(depTy.subst(SubMap)->getCanonicalType(), 0);

      for (auto reqt : reqts) {
        auto conformance = SubMap.lookupConformance(
            depTy->getCanonicalType(),
            reqt.getSecondType()->castTo<ProtocolType>()->getDecl());
        if (conformance && conformance->isConcrete())
          M.mangleProtocolConformance(conformance->getConcrete());
      }
      M.append('_');

      return false;
    });
}

void PartialSpecializationMangler::mangleSpecialization() {
  Mangler &M = getMangler();
  // If the only change to the generic signature during specialization is
  // addition of new same-type requirements, which happens in case of a
  // full specialization, it would be enough to mangle only the substitutions.
  //
  // If the types of function arguments have not changed, but some new
  // conformances were added to the generic parameters, e.g. in case of
  // a pre-specialization, then it would be enough to mangle only the new
  // generic signature.
  //
  // If the types of function arguments have changed as a result of a partial
  // specialization, we need to mangle the entire new function type.

  // This is a partial specialization.
  M.mangleType(SpecializedFnTy, 0);
  M.append("_");
}

//===----------------------------------------------------------------------===//
//                      Function Signature Optimizations
//===----------------------------------------------------------------------===//

FunctionSignatureSpecializationMangler::
FunctionSignatureSpecializationMangler(SpecializationPass P, Mangler &M,
                                       IsFragile_t Fragile, SILFunction *F)
  : SpecializationMangler(SpecializationKind::FunctionSignature, P, M, Fragile, F) {
  for (unsigned i = 0, e = F->getConventions().getNumSILArguments(); i != e;
       ++i) {
    (void)i;
    Args.push_back({ArgumentModifierIntBase(ArgumentModifier::Unmodified), nullptr});
  }
  ReturnValue = ReturnValueModifierIntBase(ReturnValueModifier::Unmodified);
}

void
FunctionSignatureSpecializationMangler::
setArgumentDead(unsigned ArgNo) {
  Args[ArgNo].first |= ArgumentModifierIntBase(ArgumentModifier::Dead);
}

void
FunctionSignatureSpecializationMangler::
setArgumentClosureProp(unsigned ArgNo, PartialApplyInst *PAI) {
  auto &Info = Args[ArgNo];
  Info.first = ArgumentModifierIntBase(ArgumentModifier::ClosureProp);
  Info.second = PAI;
}

void
FunctionSignatureSpecializationMangler::
setArgumentClosureProp(unsigned ArgNo, ThinToThickFunctionInst *TTTFI) {
  auto &Info = Args[ArgNo];
  Info.first = ArgumentModifierIntBase(ArgumentModifier::ClosureProp);
  Info.second = TTTFI;
}

void
FunctionSignatureSpecializationMangler::
setArgumentConstantProp(unsigned ArgNo, LiteralInst *LI) {
  auto &Info = Args[ArgNo];
  Info.first = ArgumentModifierIntBase(ArgumentModifier::ConstantProp);
  Info.second = LI;
}

void
FunctionSignatureSpecializationMangler::
setArgumentOwnedToGuaranteed(unsigned ArgNo) {
  Args[ArgNo].first |= ArgumentModifierIntBase(ArgumentModifier::OwnedToGuaranteed);
}

void
FunctionSignatureSpecializationMangler::
setArgumentSROA(unsigned ArgNo) {
  Args[ArgNo].first |= ArgumentModifierIntBase(ArgumentModifier::SROA);
}

void
FunctionSignatureSpecializationMangler::
setArgumentBoxToValue(unsigned ArgNo) {
  Args[ArgNo].first = ArgumentModifierIntBase(ArgumentModifier::BoxToValue);
}

void
FunctionSignatureSpecializationMangler::
setArgumentBoxToStack(unsigned ArgNo) {
  Args[ArgNo].first = ArgumentModifierIntBase(ArgumentModifier::BoxToStack);
}

void
FunctionSignatureSpecializationMangler::
setReturnValueOwnedToUnowned() {
  ReturnValue |= ReturnValueModifierIntBase(ReturnValueModifier::OwnedToUnowned);
}

void
FunctionSignatureSpecializationMangler::mangleConstantProp(LiteralInst *LI) {
  Mangler &M = getMangler();

  // Append the prefix for constant propagation 'cp'.
  M.append("cp");

  // Then append the unique identifier of our literal.
  switch (LI->getKind()) {
  default:
    llvm_unreachable("unknown literal");
  case ValueKind::FunctionRefInst: {
    SILFunction *F = cast<FunctionRefInst>(LI)->getReferencedFunction();
    M.append("fr");
    M.mangleIdentifierSymbol(F->getName());
    break;
  }
  case ValueKind::GlobalAddrInst: {
    SILGlobalVariable *G = cast<GlobalAddrInst>(LI)->getReferencedGlobal();
    M.append("g");
    M.mangleIdentifierSymbol(G->getName());
    break;
  }
  case ValueKind::IntegerLiteralInst: {
    APInt apint = cast<IntegerLiteralInst>(LI)->getValue();
    M.append("i");
    M.mangleNatural(apint);
    break;
  }
  case ValueKind::FloatLiteralInst: {
    APInt apint = cast<FloatLiteralInst>(LI)->getBits();
    M.append("fl");
    M.mangleNatural(apint);
    break;
  }
  case ValueKind::StringLiteralInst: {
    StringLiteralInst *SLI = cast<StringLiteralInst>(LI);
    StringRef V = SLI->getValue();

    assert(V.size() <= 32 && "Cannot encode string of length > 32");

    llvm::SmallString<33> Str;
    Str += "u";
    Str += V;
    M.append("se");
    M.mangleNatural(APInt(32, unsigned(SLI->getEncoding())));
    M.append("v");
    M.mangleIdentifier(Str);
    break;
  }
  }
}

void
FunctionSignatureSpecializationMangler::
mangleClosureProp(PartialApplyInst *PAI) {
  Mangler &M = getMangler();
  M.append("cl");

  // Add in the partial applies function name if we can find one. Assert
  // otherwise. The reason why this is ok to do is currently we only perform
  // closure specialization if we know the function_ref in question. When this
  // restriction is removed, the assert here will fire.
  auto *FRI = cast<FunctionRefInst>(PAI->getCallee());
  M.mangleIdentifierSymbol(FRI->getReferencedFunction()->getName());

  // Then we mangle the types of the arguments that the partial apply is
  // specializing.
  for (auto &Op : PAI->getArgumentOperands()) {
    SILType Ty = Op.get()->getType();
    M.mangleType(Ty.getSwiftRValueType(), 0);
  }
}

void FunctionSignatureSpecializationMangler::mangleClosureProp(
    ThinToThickFunctionInst *TTTFI) {
  Mangler &M = getMangler();
  M.append("cl");

  // Add in the partial applies function name if we can find one. Assert
  // otherwise. The reason why this is ok to do is currently we only perform
  // closure specialization if we know the function_ref in question. When this
  // restriction is removed, the assert here will fire.
  auto *FRI = cast<FunctionRefInst>(TTTFI->getCallee());
  M.mangleIdentifierSymbol(FRI->getReferencedFunction()->getName());
}

void FunctionSignatureSpecializationMangler::mangleArgument(
    ArgumentModifierIntBase ArgMod, NullablePtr<SILInstruction> Inst) {
  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::ConstantProp)) {
    mangleConstantProp(cast<LiteralInst>(Inst.get()));
    return;
  }

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::ClosureProp)) {
    if (auto *PAI = dyn_cast<PartialApplyInst>(Inst.get())) {
      mangleClosureProp(PAI);
      return;
    }

    auto *TTTFI = cast<ThinToThickFunctionInst>(Inst.get());
    mangleClosureProp(TTTFI);
    return;
  }

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::Unmodified)) {
    M.append("n");
    return;
  }

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::BoxToValue)) {
    M.append("i");
    return;
  }

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::BoxToStack)) {
    M.append("k");
    return;
  }

  bool hasSomeMod = false;
  if (ArgMod & ArgumentModifierIntBase(ArgumentModifier::Dead)) {
    M.append("d");
    hasSomeMod = true;
  }

  if (ArgMod & ArgumentModifierIntBase(ArgumentModifier::OwnedToGuaranteed)) {
    M.append("g");
    hasSomeMod = true;
  }
  if (ArgMod & ArgumentModifierIntBase(ArgumentModifier::SROA)) {
    M.append("s");
    hasSomeMod = true;
  }

  assert(hasSomeMod && "Unknown modifier");
}

void FunctionSignatureSpecializationMangler::
mangleReturnValue(ReturnValueModifierIntBase RetMod) {
  if (RetMod == ReturnValueModifierIntBase(ReturnValueModifier::Unmodified)) {
    return;
  }

  if (RetMod & ReturnValueModifierIntBase(ReturnValueModifier::Dead)) {
    M.append("d");
  }

  if (RetMod & ReturnValueModifierIntBase(ReturnValueModifier::OwnedToUnowned)) {
    M.append("g");
  }
}

void FunctionSignatureSpecializationMangler::mangleSpecialization() {

  for (unsigned i : indices(Args)) {
    ArgumentModifierIntBase ArgMod;
    NullablePtr<SILInstruction> Inst;
    std::tie(ArgMod, Inst) = Args[i];
    mangleArgument(ArgMod, Inst);
    M.append("_");
  }

  mangleReturnValue(ReturnValue);
}
