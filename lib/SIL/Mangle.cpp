//===--- Mangle.cpp - SIL specific name Mangling --------------------------===//
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
//  This file implements declaration specialized name mangling for SIL.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/Mangle.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Punycode.h"
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

static void mangleSubstitution(Mangler &M, Substitution Sub) {
  M.mangleType(Sub.getReplacement()->getCanonicalType(),
               ResilienceExpansion::Minimal, 0);
  for (auto C : Sub.getConformances()) {
    if (!C)
      return;
    M.mangleProtocolConformance(C);
  }
}

void GenericSpecializationMangler::mangleSpecialization() {
  Mangler &M = getMangler();
  llvm::raw_ostream &Buf = getBuffer();

  for (auto &Sub : Subs) {
    mangleSubstitution(M, Sub);
    Buf << '_';
  }
}

//===----------------------------------------------------------------------===//
//                      Function Signature Optimizations
//===----------------------------------------------------------------------===//

FunctionSignatureSpecializationMangler::
FunctionSignatureSpecializationMangler(SpecializationPass P, Mangler &M,
                                       SILFunction *F)
  : SpecializationMangler(SpecializationKind::FunctionSignature, P, M, F) {
  for (unsigned i : indices(F->getLoweredFunctionType()->getParameters())) {
    (void)i;
    Args.push_back({ArgumentModifierIntBase(ArgumentModifier::Unmodified), nullptr});
  }
}

void
FunctionSignatureSpecializationMangler::
setArgumentDead(unsigned ArgNo) {
  Args[ArgNo].first = ArgumentModifierIntBase(ArgumentModifier::Dead);
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
setArgumentInOutToValue(unsigned ArgNo) {
  Args[ArgNo].first = ArgumentModifierIntBase(ArgumentModifier::InOutToValue);
}

void
FunctionSignatureSpecializationMangler::mangleConstantProp(LiteralInst *LI) {
  Mangler &M = getMangler();
  llvm::raw_ostream &os = getBuffer();

  // Append the prefix for constant propagation 'cp'.
  os << "cp";

  // Then append the unique identifier of our literal.
  switch (LI->getKind()) {
  default:
    llvm_unreachable("unknown literal");
  case ValueKind::FunctionRefInst: {
    SILFunction *F = cast<FunctionRefInst>(LI)->getReferencedFunction();
    os << "fr";
    M.mangleIdentifier(F->getName());
    break;
  }
  case ValueKind::GlobalAddrInst: {
    SILGlobalVariable *G = cast<GlobalAddrInst>(LI)->getReferencedGlobal();
    os << "g";
    M.mangleIdentifier(G->getName());
    break;
  }
  case ValueKind::IntegerLiteralInst: {
    APInt apint = cast<IntegerLiteralInst>(LI)->getValue();
    os << "i" << apint;
    break;
  }
  case ValueKind::FloatLiteralInst: {
    APInt apint = cast<FloatLiteralInst>(LI)->getBits();
    os << "fl" << apint;
    break;
  }
  case ValueKind::StringLiteralInst: {
    StringLiteralInst *SLI = cast<StringLiteralInst>(LI);
    StringRef V = SLI->getValue();

    assert(V.size() <= 32 && "Can not encode string of length > 32");

    llvm::SmallString<33> Str;
    Str += "u";
    Str += V;
    os << "se" << unsigned(SLI->getEncoding()) << "v";
    M.mangleIdentifier(Str);
    break;
  }
  }
}

void
FunctionSignatureSpecializationMangler::
mangleClosureProp(PartialApplyInst *PAI) {
  Mangler &M = getMangler();
  llvm::raw_ostream &os = getBuffer();

  os << "cl";

  // Add in the partial applies function name if we can find one. Assert
  // otherwise. The reason why this is ok to do is currently we only perform
  // closure specialization if we know the function_ref in question. When this
  // restriction is removed, the assert here will fire.
  auto *FRI = cast<FunctionRefInst>(PAI->getCallee());
  M.mangleIdentifier(FRI->getReferencedFunction()->getName());

  // Then we mangle the types of the arguments that the partial apply is
  // specializing.
  for (auto &Op : PAI->getArgumentOperands()) {
    SILType Ty = Op.get().getType();
    M.mangleType(Ty.getSwiftRValueType(), ResilienceExpansion::Minimal, 0);
  }
}

void FunctionSignatureSpecializationMangler::mangleClosureProp(
    ThinToThickFunctionInst *TTTFI) {
  Mangler &M = getMangler();
  llvm::raw_ostream &os = getBuffer();

  os << "cl";

  // Add in the partial applies function name if we can find one. Assert
  // otherwise. The reason why this is ok to do is currently we only perform
  // closure specialization if we know the function_ref in question. When this
  // restriction is removed, the assert here will fire.
  auto *FRI = cast<FunctionRefInst>(TTTFI->getCallee());
  M.mangleIdentifier(FRI->getReferencedFunction()->getName());
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

  llvm::raw_ostream &os = getBuffer();

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::Unmodified)) {
    os << "n";
    return;
  }

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::Dead)) {
    os << "d";
    return;
  }

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::InOutToValue)) {
    os << "i";
    return;
  }

  bool hasSomeMod = false;
  if (ArgMod & ArgumentModifierIntBase(ArgumentModifier::OwnedToGuaranteed)) {
    os << "g";
    hasSomeMod = true;
  }
  if (ArgMod & ArgumentModifierIntBase(ArgumentModifier::SROA)) {
    os << "s";
    hasSomeMod = true;
  }

  assert(hasSomeMod && "Unknown modifier");
}

void FunctionSignatureSpecializationMangler::mangleSpecialization() {
  llvm::raw_ostream &os = getBuffer();

  for (unsigned i : indices(Args)) {
    ArgumentModifierIntBase ArgMod;
    NullablePtr<SILInstruction> Inst;
    std::tie(ArgMod, Inst) = Args[i];
    mangleArgument(ArgMod, Inst);
    os << "_";
  }
}
