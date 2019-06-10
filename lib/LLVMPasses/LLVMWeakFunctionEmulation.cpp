//===--- LLVMWeakFunctionEmulation.cpp - Swift LLVM IR Generation ---------===//
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

#include "swift/LLVMPasses/PassesFwd.h"

#include "llvm/ADT/Triple.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/Pass.h"

#define DEBUG_TYPE "swift-weak-function-emulation"

using namespace llvm;

namespace {
class SwiftWeakFunctionEmulationPass : public ModulePass {
public:
  static char ID;

  SwiftWeakFunctionEmulationPass() : ModulePass(ID) {}

  bool runOnModule(Module &M) override;
};

char SwiftWeakFunctionEmulationPass::ID = 0;

bool emulate(Module &M, Function &F) {
  std::string Var = (".weak." + F.getName()).str();
  std::string InitVar = (".init.weak." + F.getName()).str();
  std::string InitVarPtr = ("p.init.weak." + F.getName()).str();

  auto *FPtrTy = F.getType()->getPointerTo();
  auto *G = M.getOrInsertGlobal(Var, FPtrTy);

  auto *FTy = FunctionType::get(Type::getVoidTy(M.getContext()), {});
  auto *I = Function::Create(FTy, GlobalValue::InternalLinkage, InitVar, M);
  IRBuilder<> IRB(BasicBlock::Create(M.getContext(), "entry", I));
  // HMODULE hModule = LoadLibraryW(L"<DLL>");
  // if (hModule == nullptr)
  //   return;
  // *G = GetProcAddress(hModule, "<function>");
  IRB.CreateAlignedStore(ConstantPointerNull::get(cast<PointerType>(FPtrTy)), G,
                         G->getPointerAlignment(M.getDataLayout()));
  IRB.CreateRetVoid();

  // const void (__declspec(section(".CRT$XCLe")) *p.init.weak.<function>)(void)
  //    = &.init.weak.<function>;
  auto *IV = new GlobalVariable(M, I->getType(), /*isConstant=*/true,
                                GlobalValue::InternalLinkage, I, InitVarPtr);
  IV->setSection(".CRT$XCLe");
  appendToUsed(M, {IV});

  return true;
}

bool SwiftWeakFunctionEmulationPass::runOnModule(Module &M) {
  if (!Triple(M.getTargetTriple()).isOSWindows())
    return false;

  bool Changed = false;
  for (auto &F : M.functions())
    if (F.hasExternalWeakLinkage())
      Changed |= emulate(M, F);

  return Changed;
}
}

INITIALIZE_PASS_BEGIN(SwiftWeakFunctionEmulationPass,
                      "swift-weak-function-emulation",
                      "Swift weak function emulation pass", false, false)
INITIALIZE_PASS_END(SwiftWeakFunctionEmulationPass,
                    "swift-weak-function-emulation",
                    "Swift weak function emulation pass", false, false)

namespace swift {
llvm::ModulePass *createSwiftWeakFunctionEmulationPass() {
  initializeSwiftWeakFunctionEmulationPassPass(*PassRegistry::getPassRegistry());
  return new SwiftWeakFunctionEmulationPass();
}
}
