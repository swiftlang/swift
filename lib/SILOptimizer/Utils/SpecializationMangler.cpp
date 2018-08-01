//===--- SpecializationMangler.cpp - mangling of specializations ----------===//
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

#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Demangling/ManglingMacros.h"

using namespace swift;
using namespace Mangle;

void SpecializationMangler::beginMangling() {
  ASTMangler::beginManglingWithoutPrefix();
  if (Serialized)
    ArgOpBuffer << 'q';
  ArgOpBuffer << char(uint8_t(Pass) + '0');
}

namespace {

/// Utility class for demangling specialization attributes.
class AttributeDemangler : public Demangle::Demangler {
public:
  void demangleAndAddAsChildren(StringRef MangledSpecialization,
                                NodePointer Parent) {
    init(MangledSpecialization);
    if (!parseAndPushNodes()) {
      llvm::errs() << "Can't demangle: " << MangledSpecialization << '\n';
      abort();
    }
    for (Node *Nd : NodeStack) {
      addChild(Parent, Nd);
    }
  }
};

} // namespace

std::string SpecializationMangler::finalize() {
  StringRef MangledSpecialization(Storage.data(), Storage.size());
  AttributeDemangler D;
  NodePointer TopLevel = D.createNode(Node::Kind::Global);
  D.demangleAndAddAsChildren(MangledSpecialization, TopLevel);

  StringRef FuncName = Function->getName();
  NodePointer FuncTopLevel = nullptr;
  if (FuncName.startswith(MANGLING_PREFIX_STR)) {
    FuncTopLevel = D.demangleSymbol(FuncName);
    assert(FuncTopLevel);
  }
  if (!FuncTopLevel) {
    FuncTopLevel = D.createNode(Node::Kind::Global);
    FuncTopLevel->addChild(D.createNode(Node::Kind::Identifier, FuncName), D);
  }
  for (NodePointer FuncChild : *FuncTopLevel) {
    TopLevel->addChild(FuncChild, D);
  }
  std::string mangledName = Demangle::mangleNode(TopLevel);
  verify(mangledName);
  return mangledName;
}

//===----------------------------------------------------------------------===//
//                           Generic Specialization
//===----------------------------------------------------------------------===//

std::string GenericSpecializationMangler::mangle(GenericSignature *Sig) {
  beginMangling();

  if (!Sig) {
    SILFunctionType *FTy = Function->getLoweredFunctionType();
    Sig = FTy->getGenericSignature();
  }

  bool First = true;
  for (auto ParamType : Sig->getSubstitutableParams()) {
    appendType(Type(ParamType).subst(SubMap)->getCanonicalType());
    appendListSeparator(First);
  }
  assert(!First && "no generic substitutions");

  if (isInlined)
    appendSpecializationOperator("Ti");
  else
    appendSpecializationOperator(isReAbstracted ? "Tg" : "TG");
  return finalize();
}

//===----------------------------------------------------------------------===//
//                         Partial Generic Specialization
//===----------------------------------------------------------------------===//

std::string PartialSpecializationMangler::mangle() {
  beginMangling();
  appendType(SpecializedFnTy);
  appendSpecializationOperator(isReAbstracted ? "Tp" : "TP");
  return finalize();
}

//===----------------------------------------------------------------------===//
//                      Function Signature Optimizations
//===----------------------------------------------------------------------===//

FunctionSignatureSpecializationMangler::
FunctionSignatureSpecializationMangler(Demangle::SpecializationPass P,
                                       IsSerialized_t Serialized, SILFunction *F)
  : SpecializationMangler(P, Serialized, F) {
  for (unsigned i = 0, e = F->getConventions().getNumSILArguments(); i != e;
       ++i) {
    (void)i;
    OrigArgs.push_back(
        {ArgumentModifierIntBase(ArgumentModifier::Unmodified), nullptr});
  }
  ReturnValue = ReturnValueModifierIntBase(ReturnValueModifier::Unmodified);
}

void FunctionSignatureSpecializationMangler::setArgumentDead(
    unsigned OrigArgIdx) {
  OrigArgs[OrigArgIdx].first |= ArgumentModifierIntBase(ArgumentModifier::Dead);
}

void FunctionSignatureSpecializationMangler::setArgumentClosureProp(
    unsigned OrigArgIdx, PartialApplyInst *PAI) {
  auto &Info = OrigArgs[OrigArgIdx];
  Info.first = ArgumentModifierIntBase(ArgumentModifier::ClosureProp);
  Info.second = PAI;
}

void FunctionSignatureSpecializationMangler::setArgumentClosureProp(
    unsigned OrigArgIdx, ThinToThickFunctionInst *TTTFI) {
  auto &Info = OrigArgs[OrigArgIdx];
  Info.first = ArgumentModifierIntBase(ArgumentModifier::ClosureProp);
  Info.second = TTTFI;
}

void FunctionSignatureSpecializationMangler::setArgumentConstantProp(
    unsigned OrigArgIdx, LiteralInst *LI) {
  auto &Info = OrigArgs[OrigArgIdx];
  Info.first = ArgumentModifierIntBase(ArgumentModifier::ConstantProp);
  Info.second = LI;
}

void FunctionSignatureSpecializationMangler::setArgumentOwnedToGuaranteed(
    unsigned OrigArgIdx) {
  OrigArgs[OrigArgIdx].first |=
      ArgumentModifierIntBase(ArgumentModifier::OwnedToGuaranteed);
}

void FunctionSignatureSpecializationMangler::setArgumentSROA(
    unsigned OrigArgIdx) {
  OrigArgs[OrigArgIdx].first |= ArgumentModifierIntBase(ArgumentModifier::SROA);
}

void FunctionSignatureSpecializationMangler::setArgumentGuaranteedToOwned(
    unsigned OrigArgIdx) {
  OrigArgs[OrigArgIdx].first |=
      ArgumentModifierIntBase(ArgumentModifier::GuaranteedToOwned);
}

void FunctionSignatureSpecializationMangler::setArgumentExistentialToGeneric(
    unsigned OrigArgIdx) {
  OrigArgs[OrigArgIdx].first |=
      ArgumentModifierIntBase(ArgumentModifier::ExistentialToGeneric);
}

void FunctionSignatureSpecializationMangler::setArgumentBoxToValue(
    unsigned OrigArgIdx) {
  OrigArgs[OrigArgIdx].first =
      ArgumentModifierIntBase(ArgumentModifier::BoxToValue);
}

void FunctionSignatureSpecializationMangler::setArgumentBoxToStack(
    unsigned OrigArgIdx) {
  OrigArgs[OrigArgIdx].first =
      ArgumentModifierIntBase(ArgumentModifier::BoxToStack);
}

void
FunctionSignatureSpecializationMangler::
setReturnValueOwnedToUnowned() {
  ReturnValue |= ReturnValueModifierIntBase(ReturnValueModifier::OwnedToUnowned);
}

void
FunctionSignatureSpecializationMangler::mangleConstantProp(LiteralInst *LI) {
  // Append the prefix for constant propagation 'p'.
  ArgOpBuffer << 'p';

  // Then append the unique identifier of our literal.
  switch (LI->getKind()) {
  default:
    llvm_unreachable("unknown literal");
  case SILInstructionKind::FunctionRefInst: {
    SILFunction *F = cast<FunctionRefInst>(LI)->getReferencedFunction();
    ArgOpBuffer << 'f';
    appendIdentifier(F->getName());
    break;
  }
  case SILInstructionKind::GlobalAddrInst: {
    SILGlobalVariable *G = cast<GlobalAddrInst>(LI)->getReferencedGlobal();
    ArgOpBuffer << 'g';
    appendIdentifier(G->getName());
    break;
  }
  case SILInstructionKind::IntegerLiteralInst: {
    APInt apint = cast<IntegerLiteralInst>(LI)->getValue();
    ArgOpBuffer << 'i' << apint;
    break;
  }
  case SILInstructionKind::FloatLiteralInst: {
    APInt apint = cast<FloatLiteralInst>(LI)->getBits();
    ArgOpBuffer << 'd' << apint;
    break;
  }
  case SILInstructionKind::StringLiteralInst: {
    StringLiteralInst *SLI = cast<StringLiteralInst>(LI);
    StringRef V = SLI->getValue();
    assert(V.size() <= 32 && "Cannot encode string of length > 32");
    std::string VBuffer;
    if (!V.empty() && (isDigit(V[0]) || V[0] == '_')) {
      VBuffer = "_";
      VBuffer.append(V.data(), V.size());
      V = VBuffer;
    }
    appendIdentifier(V);

    ArgOpBuffer << 's';
    switch (SLI->getEncoding()) {
      case StringLiteralInst::Encoding::UTF8: ArgOpBuffer << 'b'; break;
      case StringLiteralInst::Encoding::UTF16: ArgOpBuffer << 'w'; break;
      case StringLiteralInst::Encoding::ObjCSelector: ArgOpBuffer << 'c'; break;
    }
    break;
  }
  }
}

void
FunctionSignatureSpecializationMangler::mangleClosureProp(SILInstruction *Inst) {
  ArgOpBuffer << 'c';

  // Add in the partial applies function name if we can find one. Assert
  // otherwise. The reason why this is ok to do is currently we only perform
  // closure specialization if we know the function_ref in question. When this
  // restriction is removed, the assert here will fire.
  if (auto *TTTFI = dyn_cast<ThinToThickFunctionInst>(Inst)) {
    auto *FRI = cast<FunctionRefInst>(TTTFI->getCallee());
    appendIdentifier(FRI->getReferencedFunction()->getName());
    return;
  }
  auto *PAI = cast<PartialApplyInst>(Inst);
  auto *FRI = cast<FunctionRefInst>(PAI->getCallee());
  appendIdentifier(FRI->getReferencedFunction()->getName());

  // Then we mangle the types of the arguments that the partial apply is
  // specializing.
  for (auto &Op : PAI->getArgumentOperands()) {
    SILType Ty = Op.get()->getType();
    appendType(Ty.getASTType());
  }
}

void FunctionSignatureSpecializationMangler::mangleArgument(
    ArgumentModifierIntBase ArgMod, NullablePtr<SILInstruction> Inst) {
  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::ConstantProp)) {
    mangleConstantProp(cast<LiteralInst>(Inst.get()));
    return;
  }

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::ClosureProp)) {
    mangleClosureProp(Inst.get());
    return;
  }

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::Unmodified)) {
    ArgOpBuffer << 'n';
    return;
  }

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::BoxToValue)) {
    ArgOpBuffer << 'i';
    return;
  }

  if (ArgMod == ArgumentModifierIntBase(ArgumentModifier::BoxToStack)) {
    ArgOpBuffer << 's';
    return;
  }

  bool hasSomeMod = false;
  if (ArgMod & ArgumentModifierIntBase(ArgumentModifier::ExistentialToGeneric)) {
    ArgOpBuffer << 'e';
    hasSomeMod = true;
  }

  if (ArgMod & ArgumentModifierIntBase(ArgumentModifier::Dead)) {
    ArgOpBuffer << 'd';
    hasSomeMod = true;
  }

  if (ArgMod & ArgumentModifierIntBase(ArgumentModifier::OwnedToGuaranteed)) {
    ArgOpBuffer << (hasSomeMod ? 'G' : 'g');
    hasSomeMod = true;
  }

  if (ArgMod & ArgumentModifierIntBase(ArgumentModifier::GuaranteedToOwned)) {
    ArgOpBuffer << (hasSomeMod ? 'O' : 'o');
    hasSomeMod = true;
  }

  if (ArgMod & ArgumentModifierIntBase(ArgumentModifier::SROA)) {
    ArgOpBuffer << (hasSomeMod ? 'X' : 'x');
    hasSomeMod = true;
  }

  assert(hasSomeMod && "Unknown modifier");
}

void FunctionSignatureSpecializationMangler::
mangleReturnValue(ReturnValueModifierIntBase RetMod) {
  if (RetMod == ReturnValueModifierIntBase(ReturnValueModifier::Unmodified)) {
    ArgOpBuffer << 'n';
    return;
  }

  bool hasSomeMode = false;
  if (RetMod & ReturnValueModifierIntBase(ReturnValueModifier::Dead)) {
    ArgOpBuffer << 'd';
    hasSomeMode = true;
  }

  if (RetMod & ReturnValueModifierIntBase(ReturnValueModifier::OwnedToUnowned)) {
    ArgOpBuffer << (hasSomeMode ? 'G' : 'g');
  }
}

std::string FunctionSignatureSpecializationMangler::mangle() {
  ArgOpStorage.clear();
  beginMangling();

  for (unsigned i : indices(OrigArgs)) {
    ArgumentModifierIntBase ArgMod;
    NullablePtr<SILInstruction> Inst;
    std::tie(ArgMod, Inst) = OrigArgs[i];
    mangleArgument(ArgMod, Inst);
  }
  ArgOpBuffer << '_';
  mangleReturnValue(ReturnValue);

  appendSpecializationOperator("Tf");
  return finalize();
}
