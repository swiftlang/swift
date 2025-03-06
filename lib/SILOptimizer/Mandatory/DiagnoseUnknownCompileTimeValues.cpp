//===--- DiagnoseUnknownCompileTimeValues.cpp
//------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements a diagnostic pass to diagnose compile-time values
// that were not simplified to be known at compile-time
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-diagnose-unknown-compiletime-values"

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SemanticAttrs.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/ConstExpr.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

class DiagnoseUnknownCompileTimeValues : public SILModuleTransform {
private:
  SymbolicValueBumpAllocator Allocator;
  ConstExprEvaluator ConstantEvaluator;
  unsigned int NumEvaluatedSILInstructions = 0;
  ConstExprFunctionState ConstExprState;

public:
  DiagnoseUnknownCompileTimeValues()
      : Allocator(), ConstantEvaluator(Allocator, 0),
        ConstExprState(ConstantEvaluator, nullptr, {},
                       NumEvaluatedSILInstructions, true) {}

private:
  void run() override {
    // Verify all const globals to be initialized with
    // compile-time known values
    verifyGlobals();

    // Verify @const lets appearing as local variables
    verifyLocals();

    // For each function call, ensure arguments to @const parameters
    // are all compile-time known values
    verifyCallArguments();
  }

  static void printSymbolicValueValue(SymbolicValue value,
                                      SymbolicValueBumpAllocator &Allocator) {
    switch (value.getKind()) {
    case swift::SymbolicValue::Integer:
      llvm::dbgs() << value.getIntegerValue() << "\n";
      break;
    case swift::SymbolicValue::FloatingPoint: {
      SmallVector<char, 0> stringFloatRepr;
      value.getFloatValue().toString(stringFloatRepr);
      llvm::dbgs() << stringFloatRepr << "\n";
    } break;
    case swift::SymbolicValue::String:
      llvm::dbgs() << "\"" << value.getStringValue().str() << "\"\n";
      break;
    case swift::SymbolicValue::Aggregate: {
      ArrayRef<SymbolicValue> Members = value.getAggregateMembers();
      for (auto T : Members)
        printSymbolicValueValue(T, Allocator);
    } break;
    default:
      value.dump();
      break;
    }
  }

  void verifyInitializeOnceGlobal(ConstExprFunctionState &ConstExprState,
                                  SILGlobalVariable &Global, VarDecl *Decl) {
    // TODO: Determine cases if/where this is necessary to perform,
    // i.e. where we currently fail to simplify to a statically-initialized
    // value.
    LLVM_DEBUG(llvm::dbgs()
                   << "@const [init_once] let " << Decl->getName().str().str()
                   << ": " << Decl->getTypeInContext().getString() << " = ";);
    SILModule *M = getModule();
    for (SILFunction &Fn : *M) {
      if (getVariableOfGlobalInit(&Fn) == &Global) {
        BuiltinInst *CallToOnce;
        if (auto *InitF = findInitializer(&Fn, CallToOnce)) {

          // Find the store to the global_addr and attempt to
          // compute its value
          for (SILBasicBlock &BB : *InitF) {
            for (SILInstruction &I : BB) {
              if (auto *GlobalAddr = dyn_cast<GlobalAddrInst>(&I)) {
                if (GlobalAddr->getReferencedGlobal() == &Global) {
                  if (auto SingleUse = GlobalAddr->getSingleUse()) {
                    auto SoleUseUser = SingleUse->getUser();
                    assert(isa<StoreInst>(SoleUseUser));
                    auto src = dyn_cast<StoreInst>(SoleUseUser)->getSrc();
                    auto Value = ConstExprState.getConstantValue(src);
                    if (Value.isConstant()) {
                      LLVM_DEBUG(printSymbolicValueValue(Value, Allocator););
                      return;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    LLVM_DEBUG(llvm::dbgs() << "Unknown\n";);
    Decl->diagnose(diag::require_const_initializer_for_const);
  }

  void verifyGlobals() {
    SILModule *M = getModule();
    // Collect all `const` global decls and ensure they are initialized
    // statically with compile-time known values
    for (SILGlobalVariable &G : M->getSILGlobals()) {
      if (auto Decl = G.getDecl()) {
        if (Decl->isConstVal()) {
          if (G.getStaticInitializerValue()) {
            // Presence of a static initializer alone confirms
            // this to be a constant value.
            LLVM_DEBUG(
                llvm::dbgs()
                    << "@const static let " << Decl->getName().str().str()
                    << ": " << Decl->getTypeInContext().getString() << " = ";
                auto StaticInitializerValue = G.getStaticInitializerValue();
                assert(StaticInitializerValue &&
                       "Expected a static initializer");
                if (auto *SI = dyn_cast<StructInst>(StaticInitializerValue)) {
                  for (auto &SIO : SI->getAllOperands()) {
                    if (!ConstExprState.getConstantValue(SIO.get())
                             .containsOnlyConstants())
                      LLVM_DEBUG(llvm::dbgs()
                                     << "Unknown to the Constant Evaluator\n";);
                    else
                      LLVM_DEBUG(printSymbolicValueValue(
                          ConstExprState.getConstantValue(SIO.get()),
                          Allocator));
                  }
                });
          } else
            verifyInitializeOnceGlobal(ConstExprState, G, Decl);
        }
      }
    }
  }

  void verifyLocal(DebugValueInst *DBI) {
    auto Decl = DBI->getDecl();
    if (!Decl || !isa<VarDecl>(Decl) || isa<ParamDecl>(Decl) ||
        !Decl->isConstVal())
      return;

    auto Value = ConstExprState.getConstantValue(DBI->getOperand());
    LLVM_DEBUG(llvm::dbgs()
                   << "@const let " << Decl->getName().str().str() << ": "
                   << Decl->getTypeInContext().getString() << " = ";);
    LLVM_DEBUG(printSymbolicValueValue(Value, Allocator););
    if (!Value.isConstant()) {
      getModule()->getASTContext().Diags.diagnose(
          Decl->getParentInitializer()->getStartLoc(),
          diag::require_const_initializer_for_const);
    }
  }

  void verifyLocals() {
    for (auto &F : *getModule())
      for (auto &BB : F)
        for (auto &I : BB)
          if (auto *DBI = dyn_cast<DebugValueInst>(&I))
            verifyLocal(DBI);
  }

  void verifyCallArguments() {
    // Find all calls to functions which have @const parameters
    for (SILFunction &Fn : *getModule())
      for (SILBasicBlock &BB : Fn)
        for (SILInstruction &I : BB)
          if (auto Apply = dyn_cast<ApplyInst>(&I))
            verifyCallArguments(Apply);
  }

  void verifyCallArguments(ApplyInst *Apply) {
    auto CalleeFn = Apply->getCalleeFunction();
    if (!CalleeFn)
      return;

    auto CalleeDecl = CalleeFn->getLocation().getAsASTNode<FuncDecl>();
    if (!CalleeDecl)
      return;

    auto CalleeParameters = CalleeDecl->getParameters();
    auto ApplyArgRefs = Apply->getArguments();

    // (AC) TODO: Needs work to correctly match params to args
    bool hasConst = false;
    for (size_t i = 0; i < CalleeParameters->size(); ++i)
      if (CalleeParameters->get(i)->isConstVal())
        hasConst = true;

    if (hasConst) {
      for (size_t i = 0; i < CalleeParameters->size(); ++i) {
        const auto &CorrespondingArg = ApplyArgRefs[i];
        const auto &CorrespondingParameter = CalleeParameters->get(i);
        if (CorrespondingParameter->isConstVal()) {
          LLVM_DEBUG({
            llvm::dbgs() << "Argument of fn{" << CalleeDecl->getNameStr()
                         << "} ";
            llvm::dbgs() << CorrespondingParameter->getNameStr() << ": ";
            std::string typeName;
            llvm::raw_string_ostream out(typeName);
            CorrespondingParameter->getTypeRepr()->print(out);
            auto Value = ConstExprState.getConstantValue(CorrespondingArg);
            llvm::dbgs() << typeName << " = ";
            printSymbolicValueValue(Value, Allocator);
          });
          if (!ConstExprState.getConstantValue(CorrespondingArg).isConstant()) {
            // FIXME: Is there a way to get this source loc without going
            // throuh the ApplyExpr?
            auto ArgLocation = Apply->getLoc().getSourceLoc();
            if (auto ApplyExprNode = Apply->getLoc().getAsASTNode<ApplyExpr>())
              ArgLocation = ApplyExprNode->getArgs()[i].getLoc();
            getModule()->getASTContext().Diags.diagnose(
                ArgLocation, diag::require_const_arg_for_parameter);
            getModule()->getASTContext().Diags.diagnose(
                CorrespondingParameter->getLoc(),
                diag::kind_declname_declared_here, DescriptiveDeclKind::Param,
                CorrespondingParameter->getName());
          }
        }
      }
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createDiagnoseUnknownCompileTimeValues() {
  return new DiagnoseUnknownCompileTimeValues();
}
