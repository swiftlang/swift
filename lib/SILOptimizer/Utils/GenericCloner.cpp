//===--- GenericCloner.cpp - Specializes generic functions  ---------------===//
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

#include "swift/SILOptimizer/Utils/GenericCloner.h"

#include "swift/AST/Type.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

using namespace swift;

/// Create a new empty function with the correct arguments and a unique name.
SILFunction *GenericCloner::initCloned(SILFunction *Orig,
                                       IsFragile_t Fragile,
                                       const ReabstractionInfo &ReInfo,
                                       StringRef NewName) {
  assert((!Fragile || Orig->isFragile())
         && "Specialization cannot make body more resilient");
  assert((Orig->isTransparent() || Orig->isBare() || Orig->getLocation())
         && "SILFunction missing location");
  assert((Orig->isTransparent() || Orig->isBare() || Orig->getDebugScope())
         && "SILFunction missing DebugScope");
  assert(!Orig->isGlobalInit() && "Global initializer cannot be cloned");

  // Create a new empty function.
  SILFunction *NewF = Orig->getModule().createFunction(
      getSpecializedLinkage(Orig, Orig->getLinkage()), NewName,
      ReInfo.getSpecializedType(), ReInfo.getSpecializedGenericEnvironment(),
      Orig->getLocation(), Orig->isBare(), Orig->isTransparent(),
      Fragile, Orig->isThunk(), Orig->getClassVisibility(),
      Orig->getInlineStrategy(), Orig->getEffectsKind(), Orig,
      Orig->getDebugScope());
  for (auto &Attr : Orig->getSemanticsAttrs()) {
    NewF->addSemanticsAttr(Attr);
  }
  if (Orig->hasUnqualifiedOwnership()) {
    NewF->setUnqualifiedOwnership();
  }
  return NewF;
}

void GenericCloner::populateCloned() {
  SILFunction *Cloned = getCloned();

  // Create arguments for the entry block.
  SILBasicBlock *OrigEntryBB = &*Original.begin();
  SILBasicBlock *ClonedEntryBB = Cloned->createBasicBlock();
  getBuilder().setInsertionPoint(ClonedEntryBB);

  llvm::SmallVector<AllocStackInst *, 8> AllocStacks;
  AllocStackInst *ReturnValueAddr = nullptr;

  // Create the entry basic block with the function arguments.
  auto origConv = Original.getConventions();
  unsigned ArgIdx = 0;
  for (auto &OrigArg : OrigEntryBB->getArguments()) {
    RegularLocation Loc((Decl *)OrigArg->getDecl());
    AllocStackInst *ASI = nullptr;
    SILType mappedType = remapType(OrigArg->getType());

    auto createAllocStack = [&]() {
      // We need an alloc_stack as a replacement for the indirect parameter.
      assert(mappedType.isAddress());
      mappedType = mappedType.getObjectType();
      ASI = getBuilder().createAllocStack(Loc, mappedType);
      ValueMap[OrigArg] = ASI;
      AllocStacks.push_back(ASI);
    };
    auto handleConversion = [&]() {
      if (!origConv.useLoweredAddresses())
        return false;

      if (ArgIdx < origConv.getSILArgIndexOfFirstParam()) {
        // Handle result arguments.
        unsigned formalIdx =
            origConv.getIndirectFormalResultIndexForSILArg(ArgIdx);
        if (ReInfo.isFormalResultConverted(formalIdx)) {
          // This result is converted from indirect to direct. The return inst
          // needs to load the value from the alloc_stack. See below.
          createAllocStack();
          assert(!ReturnValueAddr);
          ReturnValueAddr = ASI;
          return true;
        }
      } else {
        // Handle arguments for formal parameters.
        unsigned paramIdx = ArgIdx - origConv.getSILArgIndexOfFirstParam();
        if (ReInfo.isParamConverted(paramIdx)) {
          // Store the new direct parameter to the alloc_stack.
          createAllocStack();
          auto *NewArg = ClonedEntryBB->createFunctionArgument(
              mappedType, OrigArg->getDecl());
          getBuilder().createStore(Loc, NewArg, ASI,
                                   StoreOwnershipQualifier::Unqualified);

          // Try to create a new debug_value from an existing debug_value_addr.
          for (Operand *ArgUse : OrigArg->getUses()) {
            if (auto *DVAI = dyn_cast<DebugValueAddrInst>(ArgUse->getUser())) {
              getBuilder().setCurrentDebugScope(remapScope(DVAI->getDebugScope()));
              getBuilder().createDebugValue(DVAI->getLoc(), NewArg,
                                            DVAI->getVarInfo());
              getBuilder().setCurrentDebugScope(nullptr);
              break;
            }
          }
          return true;
        }
      }
      return false; // No conversion.
    };
    if (!handleConversion()) {
      auto *NewArg =
          ClonedEntryBB->createFunctionArgument(mappedType, OrigArg->getDecl());
      ValueMap[OrigArg] = NewArg;
    }
    ++ArgIdx;
  }

  BBMap.insert(std::make_pair(OrigEntryBB, ClonedEntryBB));
  // Recursively visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(OrigEntryBB);

  // Now iterate over the BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);
    TermInst *OrigTermInst = BI->first->getTerminator();
    if (auto *RI = dyn_cast<ReturnInst>(OrigTermInst)) {
      SILValue ReturnValue;
      if (ReturnValueAddr) {
        // The result is converted from indirect to direct. We have to load the
        // returned value from the alloc_stack.
        ReturnValue =
            getBuilder().createLoad(ReturnValueAddr->getLoc(), ReturnValueAddr,
                                    LoadOwnershipQualifier::Unqualified);
      }
      for (AllocStackInst *ASI : reverse(AllocStacks)) {
        getBuilder().createDeallocStack(ASI->getLoc(), ASI);
      }
      if (ReturnValue) {
        getBuilder().createReturn(RI->getLoc(), ReturnValue);
        continue;
      }
    } else if (isa<ThrowInst>(OrigTermInst)) {
      for (AllocStackInst *ASI : reverse(AllocStacks)) {
        getBuilder().createDeallocStack(ASI->getLoc(), ASI);
      }
    }
    visit(BI->first->getTerminator());
  }
}
