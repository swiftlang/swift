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
#include "llvm/Support/ErrorHandling.h"
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
FunctionSignatureSpecializationMangler(Mangler &M, SILFunction *F)
  : SpecializationMangler(SpecializationSourceKind::FunctionSignature, M, F) {
  for (unsigned i : indices(F->getLoweredFunctionType()->getParameters())) {
    (void)i;
    Args.push_back({uint8_t(ArgumentModifier::Unmodified), nullptr});
  }
}

void
FunctionSignatureSpecializationMangler::
setArgumentClosureProp(SILArgument *Arg, PartialApplyInst *PAI) {
  assert(Arg->getFunction() == getFunction() && "Arg not from function");
  auto &Info = Args[Arg->getIndex()];
  Info.first |= uint8_t(ArgumentModifier::ClosureProp);
  Info.second = PAI;
}

void FunctionSignatureSpecializationMangler::mangleSpecialization() {
  Mangler &M = getMangler();
  llvm::raw_ostream &os = getBuffer();

  for (unsigned i : indices(Args)) {
    uint8_t ArgMod;
    NullablePtr<SILInstruction> Inst;

    std::tie(ArgMod, Inst) = Args[i];
    if (ArgMod == uint8_t(ArgumentModifier::Unmodified)) {
      os << "n_";
      continue;
    }

    if (ArgMod & uint8_t(ArgumentModifier::ClosureProp)) {
      os << "cl";
      auto *PAI = cast<PartialApplyInst>(Inst.get());

      // Add in the partial applies function name if we can find one otherwise,
      // we use a unique UUID since we need *some* name. Using the UUID means
      // that the function with the specialized partial apply will be unique so
      // we will have code size increases potentially, but it will be *correct*.
      if (auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee())) {
        os << FRI->getReferencedFunction()->getName();
      } else {
        llvm::SmallString<UUID::StringBufferSize> Str;
        UUID::fromTime().toString(Str);
        os << "uu" << Str;
      }

      // Then we mangle the types of the arguments that the partial apply is
      // specializing.
      for (auto &Op : PAI->getArgumentOperands()) {
        SILType Ty = Op.get().getType();
        M.mangleType(Ty.getSwiftRValueType(), ResilienceExpansion::Minimal, 0);
      }

      os << "_";
      continue;
    }

    llvm_unreachable("unknown arg type");
  }
}
