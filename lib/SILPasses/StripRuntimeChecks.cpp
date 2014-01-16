//===-------- StripRuntimeChecks.cpp - Strip runtime checks from SIL ------===//
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

#include "swift/AST/Builtins.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"

using namespace swift;

static void convertBinaryOverflowToUncheckedBuiltin(SILBasicBlock *BB,
                                              BuiltinFunctionRefInst *BRI,
                                              StringRef name,
                                              StringRef uncheckedNameRoot) {
  SILBuilder B(BB, SILBasicBlock::iterator(BRI));

  // Get the name of the unchecked builtin.
  assert(BRI->getName().str().startswith(name));
  llvm::SmallString<16> uncheckedName(uncheckedNameRoot);
  uncheckedName += BRI->getName().str().slice(name.size(),
                                              BRI->getName().str().size());

  // Get its type.
  // (T, T, Int1) -> (T, Int1) to (T, T) -> T
  auto type = BRI->getType().castTo<SILFunctionType>();
  CanType uncheckedResult
    = cast<TupleType>(type->getResult().getType()).getElementType(0);
  SILType uncheckedSILResult
    = SILType::getPrimitiveObjectType(uncheckedResult);
  auto uncheckedType = SILFunctionType::get(nullptr,
            type->getExtInfo(),
            type->getCalleeConvention(),
            type->getParameters().slice(0, type->getParameters().size() - 1),
            SILResultInfo(uncheckedResult, type->getResult().getConvention()),
            type->getASTContext());
  SILType uncheckedSILType = SILType::getPrimitiveObjectType(uncheckedType);
  
  // Build the reference to the unchecked builtin.
  auto uncheckedBRI = B.createBuiltinFunctionRef(BRI->getLoc(),
                             uncheckedName,
                             uncheckedSILType);
  // Create a literal Int1 '0' we can use to replace the 'did overflow' bit.
  auto zero = B.createIntegerLiteral(BRI->getLoc(),
                     SILType::getBuiltinIntegerType(1, type->getASTContext()),
                     0);
  
  // Update the applications of the builtin.
  for (auto use : BRI->getUses()) {
    // The only supported use of a builtin ref is as the callee of an
    // application.
    auto app = cast<ApplyInst>(use->getUser());
    assert(app->getCallee().getDef() == BRI);
    
    B.setInsertionPoint(app->getParent(), app);
    
    // Apply the unchecked builtin, dropping the reportOverflow arg.
    auto uncheckedArgRange
      = app->getArguments().slice(0, app->getArguments().size() - 1);
    SmallVector<SILValue, 2> uncheckedArgs;
    for (auto arg : uncheckedArgRange)
      uncheckedArgs.push_back(arg);
    
    auto uncheckedApp = B.createApply(app->getLoc(),
                                      uncheckedBRI,
                                      uncheckedSILType,
                                      uncheckedSILResult,
                                      {},
                                      uncheckedArgs);
    
    // Build a tuple with the unchecked result and our zero to replace the
    // overflow bit.
    SILValue elts[] = {SILValue(uncheckedApp, 0), zero};
    auto tuple = B.createTuple(app->getLoc(), app->getType(), elts);
    
    // Replace references to the checked application with references to the
    // tuple.
    app->replaceAllUsesWith(tuple);
    app->eraseFromParent();
  }
  
  BRI->eraseFromParent();
}

static void processBuiltinFunctionRef(SILBasicBlock *BB,
                                      BuiltinFunctionRefInst *BRI) {
  auto &builtinInfo = BRI->getBuiltinInfo();
  
  // Reduce overflow intrinsics to their unchecked forms.
#define BUILTIN(...)
#define BUILTIN_BINARY_OPERATION_WITH_OVERFLOW(Id, Name, UncheckedName, _, __) \
  if (builtinInfo.ID == BuiltinValueKind::Id) { \
    return convertBinaryOverflowToUncheckedBuiltin(BB, BRI, \
                                                   Name, UncheckedName); \
  }
#include "swift/AST/Builtins.def"
  
  // TODO: Reduce checked conversions to their non-checked forms.
  // TODO: Turn traps into unreachables?
}

static void processFunction(SILFunction &F) {
  for (auto &BB : F) {
    // NB: Mutates the basic block list.
    for (auto II = BB.begin(); II != BB.end();) {
      SILInstruction *I = &*II;
      ++II;
      
      // Consider builtins for translation.
      if (auto BRI = dyn_cast<BuiltinFunctionRefInst>(I)) {
        processBuiltinFunctionRef(&BB, BRI);
        continue;
      }
      
      // Zap condfails.
      if (isa<CondFailInst>(I)) {
        I->eraseFromParent();
        continue;
      }
    }
  }
}

void swift::performSILStripRuntimeChecks(SILModule *M) {
  for (SILFunction &F : *M) {
    if (F.empty())
      continue;
    
    processFunction(F);
  }
}
