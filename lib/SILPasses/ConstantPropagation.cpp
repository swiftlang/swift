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

#include "swift/AST/Builtins.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Type.h"
#include "swift/Subsystems.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumInstFolded, "Number of constant folded instructions");

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

static SILInstruction *constantFoldIntrinsic(ApplyInst *AI,
                                             llvm::Intrinsic::ID ID,
                                             SILModule &M) {
  OperandValueArrayRef Args = AI->getArguments();

  if (Args.size() == 2) {
    // Check if both arguments are literals.
    if (IntegerLiteralInst *Op1 = dyn_cast<IntegerLiteralInst>(Args[0])) {
      if (IntegerLiteralInst *Op2 = dyn_cast<IntegerLiteralInst>(Args[1])) {

        switch (ID) {
        default: break;
        case llvm::Intrinsic::sadd_with_overflow:
        case llvm::Intrinsic::uadd_with_overflow:
        case llvm::Intrinsic::ssub_with_overflow:
        case llvm::Intrinsic::usub_with_overflow:
        case llvm::Intrinsic::smul_with_overflow:
        case llvm::Intrinsic::umul_with_overflow: {
          // Handle arithmetic intrinsics with overflow.
          // First, Calculate the result.
          APInt Res;
          bool Overflow;
          switch (ID) {
          default: llvm_unreachable("Invalid case");
          case llvm::Intrinsic::sadd_with_overflow:
            Res = Op1->getValue().sadd_ov(Op2->getValue(), Overflow);
            break;
          case llvm::Intrinsic::uadd_with_overflow:
            Res = Op1->getValue().uadd_ov(Op2->getValue(), Overflow);
            break;
          case llvm::Intrinsic::ssub_with_overflow:
            Res = Op1->getValue().ssub_ov(Op2->getValue(), Overflow);
            break;
          case llvm::Intrinsic::usub_with_overflow:
            Res = Op1->getValue().usub_ov(Op2->getValue(), Overflow);
            break;
          case llvm::Intrinsic::smul_with_overflow:
            Res = Op1->getValue().smul_ov(Op2->getValue(), Overflow);
            break;
          case llvm::Intrinsic::umul_with_overflow:
            Res = Op1->getValue().umul_ov(Op2->getValue(), Overflow);
            break;
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
          SILBuilder B(AI);
          SILValue Result[] = {
            B.createIntegerLiteral(AI->getLoc(), ResTy1, Res),
            B.createIntegerLiteral(AI->getLoc(), ResTy2, Overflow)
          };
          return B.createTuple(AI->getLoc(), FuncResType, Result);
        }
        } // end of switch(ID)
      }
    }
  }
  return nullptr;
}

static SILInstruction *constantFoldBuiltin(ApplyInst *AI,
                                           BuiltinFunctionRefInst *FR,
                                           SILModule &M) {
  const IntrinsicInfo &Intrinsic = FR->getIntrinsicInfo();

  // If it's an llvm intrinsic, fold the intrinsic.
  if (Intrinsic.ID != llvm::Intrinsic::not_intrinsic) {
    return constantFoldIntrinsic(AI, Intrinsic.ID, M);
  }

  // Otherwise, it should be one of the builin functions.
  OperandValueArrayRef Args = AI->getArguments();
  const BuiltinInfo &Builtin = M.getBuiltinInfo(FR->getReferencedFunction());

  switch (Builtin.ID) {
    default: break;
    case BuiltinValueKind::Trunc:
    case BuiltinValueKind::ZExt:
    case BuiltinValueKind::SExt: {

      // We can fold if the value being cast is a constant.
      IntegerLiteralInst *V = dyn_cast<IntegerLiteralInst>(Args[0]);
      if (!V)
        return nullptr;

      // Get the cast result.
      APInt CastResV;
      Type DestTy = Builtin.Types.size() == 2 ? Builtin.Types[1] : Type();
      uint32_t DestBitWidth =
        DestTy->castTo<BuiltinIntegerType>()->getBitWidth();
      switch (Builtin.ID) {
        default : llvm_unreachable("Invalid case.");
        case BuiltinValueKind::Trunc:
          CastResV = V->getValue().trunc(DestBitWidth);
          break;
        case BuiltinValueKind::ZExt:
          CastResV = V->getValue().zext(DestBitWidth);
          break;
        case BuiltinValueKind::SExt:
          CastResV = V->getValue().sext(DestBitWidth);
          break;
      }

      // Add the literal instruction to represnet the result of the cast.
      SILBuilder B(AI);
      return B.createIntegerLiteral(AI->getLoc(),
                                    SILType::getPrimitiveType(CanType(DestTy),
                                                      SILValueCategory::Object),
                                    CastResV);
    }
    }
  return nullptr;
}

static SILInstruction *constantFoldInstruction(SILInstruction &I,
                                               SILModule &M) {
  // Constant fold function calls.
  if (ApplyInst *AI = dyn_cast<ApplyInst>(&I)) {
    // Constant fold calls to builtins.
    if (BuiltinFunctionRefInst *FR =
        dyn_cast<BuiltinFunctionRefInst>(AI->getCallee().getDef())) {
      return constantFoldBuiltin(AI, FR, M);
    }
    return nullptr;
  }

  // Constant fold extraction of a constant element.
  if (TupleExtractInst *TEI = dyn_cast<TupleExtractInst>(&I)) {
    if (TupleInst *TheTuple = dyn_cast<TupleInst>(TEI->getOperand().getDef())) {
      unsigned FieldNo = TEI->getFieldNo();
      ValueBase *Elem = TheTuple->getElements()[FieldNo].getDef();
      if (SILInstruction *SILInstElem = dyn_cast<SILInstruction>(Elem)) {
        return SILInstElem;
      }
    }
  }

  // Constant fold extraction of a constant element.
  if (StructExtractInst *SEI = dyn_cast<StructExtractInst>(&I)) {
    if (StructInst *Struct = dyn_cast<StructInst>(SEI->getOperand().getDef())) {
      // Find the Field number corresponding to the FieldDecl.
      unsigned FieldNo = 0;
      StructDecl *SD =
        cast<StructDecl>(Struct->getType().getSwiftType()->getAnyNominal());

      for (auto MD : SD->getStoredProperties()) {
        if (MD == SEI->getField()) {
          ValueBase *E = Struct->getElements()[FieldNo].getDef();
          // If the element the struct_extract is extracting is const, fold it.
          if (SILInstruction *SILInstElem = dyn_cast<SILInstruction>(E)) {
            return SILInstElem;
          }
          break;
        }
        ++FieldNo;
      }
    }
  }

  return nullptr;
}

static bool CCPFunctionBody(SILFunction &F, SILModule &M) {
  DEBUG(llvm::errs() << "*** ConstPropagation processing: " << F.getName()
        << "\n");

  // Initialize the worklist to all of the instructions ready to process...
  llvm::SetVector<SILInstruction*> WorkList;
  for (auto &BB : F) {
    for(auto &I : BB) {
      WorkList.insert(&I);
    }
  }

  // Try to fold instructions in the list one by one.
  bool Folded = false;
  while (!WorkList.empty()) {
    SILInstruction *I = *WorkList.begin();
    WorkList.remove(I);

    if (!I->use_empty())

      // Try to fold the instruction.
      if (SILInstruction *C = constantFoldInstruction(*I, M)) {
        // The users could be constant propagatable now.
        for (auto UseI = I->use_begin(),
                  UseE = I->use_end(); UseI != UseE; ++UseI) {
          SILInstruction *User = cast<SILInstruction>(UseI.getUser());
          WorkList.insert(User);

          // Some constant users may indirectly cause folding of their users.
          if (isa<StructInst>(User) || isa<TupleInst>(User)) {
            for (auto UseUseI = User->use_begin(),
                 UseUseE = User->use_end(); UseUseI != UseUseE; ++UseUseI) {
              WorkList.insert(cast<SILInstruction>(UseUseI.getUser()));

            }
          }
        }

        // We were able to fold, so all users should use the new folded value.
        assert(I->getTypes().size() == 1 &&
               "Currently, we only support single result instructions.");
        SILValue(I).replaceAllUsesWith(C);

        // Remove the unused instruction.
        WorkList.remove(I);

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
