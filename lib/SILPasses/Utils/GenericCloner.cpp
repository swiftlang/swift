//===--------- GenericCloner.cpp - Specializes generic functions  ---------===//
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

#include "swift/SILPasses/Utils/GenericCloner.h"

#include "swift/AST/Type.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

using namespace swift;

/// Create a new empty function with the correct arguments and a unique name.
SILFunction *GenericCloner::initCloned(SILFunction *Orig,
                                       TypeSubstitutionMap &InterfaceSubs,
                                       StringRef NewName) {
  SILModule &M = Orig->getModule();
  Module *SM = M.getSwiftModule();

  CanSILFunctionType FTy =
    SILType::substFuncType(M, SM, InterfaceSubs,
                           Orig->getLoweredFunctionType(),
                           /*dropGenerics = */ true);

  assert((Orig->isTransparent() || Orig->isBare() || Orig->getLocation())
         && "SILFunction missing location");
  assert((Orig->isTransparent() || Orig->isBare() || Orig->getDebugScope())
         && "SILFunction missing DebugScope");
  assert(!Orig->isGlobalInit() && "Global initializer cannot be cloned");

  // Create a new empty function.
  SILFunction *NewF = SILFunction::create(M,
                                      getSpecializedLinkage(Orig->getLinkage()),
                                          NewName, FTy, nullptr,
                                          Orig->getLocation(), Orig->isBare(),
                                          Orig->isTransparent(),
                                          Orig->isFragile(), Orig->isThunk(),
                                          Orig->getClassVisibility(),
                                          Orig->getInlineStrategy(),
                                          Orig->getEffectsKind(), Orig,
                                          Orig->getDebugScope(),
                                          Orig->getDeclContext());
  NewF->setSemanticsAttr(Orig->getSemanticsAttr());
  return NewF;
}

void GenericCloner::populateCloned() {
  SILFunction *Cloned = getCloned();
  SILModule &M = Cloned->getModule();

  // Create arguments for the entry block.
  SILBasicBlock *OrigEntryBB = Original.begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(Cloned);

  // Create the entry basic block with the function arguments.
  auto I = OrigEntryBB->bbarg_begin(), E = OrigEntryBB->bbarg_end();
  while (I != E) {
    SILValue MappedValue =
      new (M) SILArgument(ClonedEntryBB, remapType((*I)->getType()),
                          (*I)->getDecl());
    ValueMap.insert(std::make_pair(*I, MappedValue));
    ++I;
  }

  getBuilder().setInsertionPoint(ClonedEntryBB);
  BBMap.insert(std::make_pair(OrigEntryBB, ClonedEntryBB));
  // Recursively visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(OrigEntryBB);

  // Now iterate over the BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);
    visit(BI->first->getTerminator());
  }
}

void dumpTypeSubstitutionMap(const TypeSubstitutionMap &map) {
  llvm::errs() << "{\n";
  for (auto &kv : map) {
    llvm::errs() << "  ";
    kv.first->print(llvm::errs());
    llvm::errs() << " => ";
    kv.second->print(llvm::errs());
    llvm::errs() << "\n";
  }
  llvm::errs() << "}\n";
}
