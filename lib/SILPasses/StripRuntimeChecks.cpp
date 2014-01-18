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

static SILInstruction *convertBinaryOverflowToUncheckedBuiltin(SILBasicBlock *BB,
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
  // (T, T, Int1) -> (T, Int1) becomes (T, T) -> T
  auto type = BRI->getType().castTo<SILFunctionType>();
  CanType uncheckedResult
    = cast<TupleType>(type->getResult().getType()).getElementType(0);
  auto uncheckedType = SILFunctionType::get(nullptr, nullptr,
            type->getExtInfo(),
            type->getCalleeConvention(),
            type->getParameters().slice(0, type->getParameters().size() - 1),
            SILResultInfo(uncheckedResult, type->getResult().getConvention()),
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
  
  auto next = BRI->getNextNode();
  BRI->eraseFromParent();
  return next;;
}

static SILInstruction *convertCheckedTruncToUncheckedBuiltin(SILBasicBlock *BB,
                                                  BuiltinFunctionRefInst *BRI) {
  ASTContext &C = BRI->getType().getSwiftRValueType()->getASTContext();
  SILBuilder B(BB, SILBasicBlock::iterator(BRI));
 
  // Get the name of the unchecked builtin.
  // We should always be going from '?_to_?_checked_trunc_*' to 'trunc_*'.
  auto name = BRI->getName().str();
  assert(name.slice(1, 5).equals("_to_")
         && name.slice(6, 20).equals("_checked_trunc"));
  
  llvm::SmallString<16> uncheckedName("trunc");
  uncheckedName += name.slice(20, name.size());
  
  // Get its type.
  // T -> (U, Int1) becomes T -> U
  auto type = BRI->getType().castTo<SILFunctionType>();
  CanType uncheckedResult
    = cast<TupleType>(type->getResult().getType()).getElementType(0);
  auto uncheckedType = SILFunctionType::get(nullptr, nullptr,
              type->getExtInfo(),
              type->getCalleeConvention(),
              type->getParameters(),
              SILResultInfo(uncheckedResult, type->getResult().getConvention()),
              type->getParameters(),
              SILResultInfo(uncheckedResult, type->getResult().getConvention()),
              C);
  SILType uncheckedSILType = SILType::getPrimitiveObjectType(uncheckedType);

  // Build the reference to the unchecked builtin.
  auto uncheckedBRI = B.createBuiltinFunctionRef(BRI->getLoc(),
                             uncheckedName,
                             uncheckedSILType);
  // Create a literal Int1 '0' we can use to replace the 'did overflow' bit.
  auto zero = B.createIntegerLiteral(BRI->getLoc(),
                     SILType::getBuiltinIntegerType(1, C),
                     0);
  
  // Update the applications of the builtin.
  for (auto use : BRI->getUses()) {
    // The only supported use of a builtin ref is as the callee of an
    // application.
    auto app = cast<ApplyInst>(use->getUser());
    assert(app->getCallee().getDef() == BRI);

    B.setInsertionPoint(app->getParent(), app);

    // Apply the unchecked builtin.
    assert(app->getArguments().size() == 1);
    auto uncheckedApp = B.createApply(app->getLoc(),
                                      uncheckedBRI,
                                      app->getArguments()[0]);
    
    // Build a tuple with the unchecked result and our zero to replace the
    // overflow bit.
    SILValue elts[] = {SILValue(uncheckedApp, 0), zero};
    auto tuple = B.createTuple(app->getLoc(), app->getType(), elts);

    // Replace references to the checked application with references to the
    // tuple.
    app->replaceAllUsesWith(tuple);
    app->eraseFromParent();
  }
  
  auto next = BRI->getNextNode();
  BRI->eraseFromParent();
  return next;
}

static SILInstruction *convertCheckedConversionToUncheckedBuiltin(
                                                 SILBasicBlock *BB,
                                                 BuiltinFunctionRefInst *BRI) {
  ASTContext &C = BRI->getType().getSwiftRValueType()->getASTContext();
  SILBuilder B(BB, SILBasicBlock::iterator(BRI));

  // Create a literal Int1 '0' we can use to replace the 'did overflow' bit.
  auto zero = B.createIntegerLiteral(BRI->getLoc(),
                     SILType::getBuiltinIntegerType(1, C),
                     0);

  // Update applications of the builtin to refer to their operand directly.
  for (auto use : BRI->getUses()) {
    // The only supported use of a builtin ref is as the callee of an
    // application.
    auto app = cast<ApplyInst>(use->getUser());
    assert(app->getCallee().getDef() == BRI);

    // Wrap the operand in a tuple with 0 for the overflow bit.
    B.setInsertionPoint(app->getParent(), app);
    assert(app->getArguments().size() == 1);
    SILValue val = app->getArguments()[0];
    
    SILValue elts[] = {val, zero};
    auto tuple = B.createTuple(app->getLoc(), app->getType(), elts);
    
    // Replace references to the application with references to the tuple.
    app->replaceAllUsesWith(tuple);
    app->eraseFromParent();
  }
  
  auto next = BRI->getNextNode();
  BRI->eraseFromParent();
  return next;
}

static SILInstruction *processBuiltinFunctionRef(SILBasicBlock *BB,
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
  
  // Reduce checked truncation intrinsics unchecked 'trunc'.
  if (builtinInfo.ID == BuiltinValueKind::UToSCheckedTrunc
      || builtinInfo.ID == BuiltinValueKind::SToSCheckedTrunc
      || builtinInfo.ID == BuiltinValueKind::SToUCheckedTrunc
      || builtinInfo.ID == BuiltinValueKind::UToUCheckedTrunc) {
    return convertCheckedTruncToUncheckedBuiltin(BB, BRI);
  }
  
  // Reduce signed<->unsigned checked conversion intrinsics to no-ops.
  if (builtinInfo.ID == BuiltinValueKind::SUCheckedConversion
      || builtinInfo.ID == BuiltinValueKind::USCheckedConversion) {
    return convertCheckedConversionToUncheckedBuiltin(BB, BRI);
  }
  
  // TODO: Turn traps into unreachables?
  
  return BRI->getNextNode();
}

static void processFunction(SILFunction &F) {
  for (auto &BB : F) {
    // NB: Mutates the basic block list.
    for (auto II = BB.begin(); II != BB.end();) {
      SILInstruction *I = &*II;
      
      // Consider builtins for translation.
      if (auto BRI = dyn_cast<BuiltinFunctionRefInst>(I)) {
        II = processBuiltinFunctionRef(&BB, BRI);
        continue;
      }
      
      ++II;

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
