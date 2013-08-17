//===--- ConstantPropagation.cpp - Promote alloc_box to alloc_stack ------===//
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

#define DEBUG_TYPE "constant-propagation"

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/Subsystems.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumInstFolded, "Number of constant folded instructions");

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

static SILInstruction *constantFoldInstruction(SILInstruction &I,
                                               SILModule &M) {
  // Constant fold function calls.
  if (ApplyInst *AI = dyn_cast<ApplyInst>(&I)) {

    // Constant fold calls to builtins.
    if (BuiltinFunctionRefInst *FR =
        dyn_cast<BuiltinFunctionRefInst>(AI->getCallee().getDef())) {
      llvm::Intrinsic::ID ID = FR->getIntrinsicID();
      
      if (ID == llvm::Intrinsic::sadd_with_overflow) {

        // Check if both arguments are literals.
        OperandValueArrayRef Args = AI->getArguments();
        if (IntegerLiteralInst *Op1 = dyn_cast<IntegerLiteralInst>(Args[0]))
          if (IntegerLiteralInst *Op2 = dyn_cast<IntegerLiteralInst>(Args[1])) {

            // Calculate the result.
            APInt Res;
            bool Overflow;
            Res = Op1->getValue().sadd_ov(Op2->getValue(), Overflow);

            // Diagnose the overflow.
            // FIXME: This might need special handling if we are inside an
            // inlined function.
            if (Overflow) {
              diagnose(M.getASTContext(),
                       AI->getLoc().getSourceLoc(),
                       diag::arithmetic_operation_overflow);
              return nullptr;
            }

            // Get the SIL subtypes of the returned tuple type.
            SILType FuncResType = AI->getFunctionTypeInfo(M)->getResultType();
            TupleType *T = FuncResType.castTo<TupleType>();
            assert(T->getNumElements() == 2);
            SILType ResTy1 =
              SILType::getPrimitiveType(CanType(T->getElementType(0)),
                                        SILValueCategory::Object);
            SILType ResTy2 =
              SILType::getPrimitiveType(CanType(T->getElementType(1)),
                                        SILValueCategory::Object);

            // Construct the folded instruction - a tuple of two literals, the
            // result and overflow.
            SILBuilder B(&I);
            SILValue Result[] = {
              B.createIntegerLiteral(AI->getLoc(), ResTy1, Res),
              B.createIntegerLiteral(AI->getLoc(), ResTy2, Overflow)
            };
            return B.createTuple(AI->getLoc(), FuncResType, Result);
          }
      }
    }
  }
  return nullptr;
}

static bool CCPFunctionBody(SILFunction &F, SILModule &M) {
  // Initialize the worklist to all of the instructions ready to process...
  std::set<SILInstruction*> WorkList;
  for (auto &BB : F) {
    for(auto &I : BB) {
      WorkList.insert(&I);
    }
  }

  // Try to fold instructions in the list one by one.
  bool Folded = false;
  while (!WorkList.empty()) {
    SILInstruction *I = *WorkList.begin();
    WorkList.erase(WorkList.begin());

    if (!I->use_empty())

      // Try to fold the instruction.
      if (SILInstruction *C = constantFoldInstruction(*I, M)) {
        // We were able to fold, so all users should use the new folded value.
        assert(I->getTypes().size() == 1 &&
               "Currently, we only support single result instructions.");
        SILValue(I).replaceAllUsesWith(C);

        // The users could be constant propagatable now.
        for (auto UseI = I->use_begin(),
                  UseE = I->use_end(); UseI != UseE; ++UseI) {
          WorkList.insert(cast<SILInstruction>(UseI.getUser()));
        }

        // Remove the unused instruction.
        WorkList.erase(I);

        // Eagerly DCE.
        recursivelyDeleteTriviallyDeadInstructions(I);

        Folded = true;
        ++NumInstFolded;
      }
  }

  return false;
}

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//
void swift::performSILConstantPropagation(SILModule *M) {
  for (auto &Fn : *M) {
    CCPFunctionBody(Fn, *M);
  }
}
