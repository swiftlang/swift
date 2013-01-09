//===--- SILGenStmt.cpp - Implements Lowering of ASTs -> SIL for Stmts ----===//
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

#include "SILGen.h"
#include "Scope.h"
#include "Condition.h"
#include "Initialization.h"
#include "LValue.h"
#include "ManagedValue.h"
#include "swift/AST/AST.h"
#include "llvm/ADT/OwningPtr.h"

using namespace swift;
using namespace Lowering;

//===--------------------------------------------------------------------===//
// SILGenFunction visit*Stmt implementation
//===--------------------------------------------------------------------===//

/// emitOrDeleteBlock - If there are branches to the specified basic block,
/// emit it per emitBlock.  If there aren't, then just delete the block - it
/// turns out to have not been needed.
static void emitOrDeleteBlock(SILBuilder &B, BasicBlock *BB) {
  if (BB->pred_empty()) {
    // If the block is unused, we don't need it; just delete it.
    BB->eraseFromParent();
  } else {
    // Otherwise, continue emitting code in BB.
    B.emitBlock(BB);
  }
}

/// emitCondition - Emit a boolean expression as a control-flow condition.
///
/// \param TheStmt - The statement being lowered, for source information on the
///        branch.
/// \param E - The expression to be evaluated as a condition.
/// \param hasFalseCode - true if the false branch doesn't just lead
///        to the fallthrough.
/// \param invertValue - true if this routine should invert the value before
///        testing true/false.
Condition SILGenFunction::emitCondition(Stmt *TheStmt, Expr *E,
                                bool hasFalseCode, bool invertValue) {
  assert(B.hasValidInsertionPoint() &&
         "emitting condition at unreachable point");
  
  // Sema forces conditions to have Builtin.i1 type, which guarantees this.
  Value V;
  {
    FullExpr Scope(Cleanups);
    V = visit(E).forward(*this);
  }
  assert(V.getType().castTo<BuiltinIntegerType>()->getBitWidth() == 1);
  
  BasicBlock *ContBB = new BasicBlock(&F, "condition.cont");
  BasicBlock *TrueBB = new BasicBlock(&F, "if.true");
  
  BasicBlock *FalseBB, *FalseDestBB;
  if (hasFalseCode) {
    FalseBB = FalseDestBB = new BasicBlock(&F, "if.false");
  } else {
    FalseBB = nullptr;
    FalseDestBB = ContBB;
  }
  
  if (invertValue)
    B.createCondBranch(TheStmt, V, FalseDestBB, TrueBB);
  else
    B.createCondBranch(TheStmt, V, TrueBB, FalseDestBB);
  
  return Condition(TrueBB, FalseBB, ContBB);
}

void SILGenFunction::visitBraceStmt(BraceStmt *S, SGFContext C) {
  // Enter a new scope.
  Scope BraceScope(Cleanups);
  
  for (auto &ESD : S->getElements()) {
    assert(B.hasValidInsertionPoint());
    
    if (Stmt *S = ESD.dyn_cast<Stmt*>()) {
      visit(S);
      
      // If we ever reach an unreachable point, stop emitting statements.
      // This will need revision if we ever add goto.
      if (!B.hasValidInsertionPoint()) return;
    } else if (Expr *E = ESD.dyn_cast<Expr*>()) {
      FullExpr scope(Cleanups);
      visit(E);
    } else
      visit(ESD.get<Decl*>());
  }
}

/// emitAssignStmtRecursive - Used to destructure (potentially) recursive
/// assignments into tuple expressions down to their scalar stores.
static void emitAssignStmtRecursive(AssignStmt *S, ManagedValue Src, Expr *Dest,
                                    SILGenFunction &Gen) {
  // If the destination is a tuple, recursively destructure.
  if (TupleExpr *TE = dyn_cast<TupleExpr>(Dest)) {
    Value SrcV = Src.forward(Gen);
    unsigned EltNo = 0;
    for (Expr *DestElem : TE->getElements()) {
      SILType elemType = Gen.getLoweredType(
                                          DestElem->getType()->getRValueType());
      Value SrcVal = Gen.B.createExtract(SILLocation(), SrcV,
                                         EltNo++,
                                         elemType);
      emitAssignStmtRecursive(S,
                              Gen.emitManagedRValueWithCleanup(SrcVal),
                              DestElem, Gen);
    }
    return;
  }
  
  // Otherwise, emit the scalar assignment.
  LValue DstLV = SILGenLValue(Gen).visit(Dest);
  Gen.emitAssignToLValue(S, Src, DstLV);
}


void SILGenFunction::visitAssignStmt(AssignStmt *S, SGFContext C) {
  FullExpr scope(Cleanups);
  ManagedValue SrcV = visit(S->getSrc());

  // Handle tuple destinations by destructuring them if present.
  return emitAssignStmtRecursive(S, SrcV, S->getDest(), *this);
}

namespace {

/// IndirectReturnInitialization - represents initializing an indirect return
/// value.
class IndirectReturnInitialization : public SingleInitializationBase {
  Value address;
public:
  IndirectReturnInitialization(Value address) : address(address) {}
  
  Value getAddressOrNull() override { return address; }
};

} // end anonymous namespace

void SILGenFunction::visitReturnStmt(ReturnStmt *S, SGFContext C) {
  Value ArgV;
  if (IndirectReturnAddress) {
    // Indirect return of an address-only value.
    FullExpr scope(Cleanups);
    llvm::OwningPtr<Initialization> returnInit(
                       new IndirectReturnInitialization(IndirectReturnAddress));
    emitExprInto(S->getResult(), returnInit.get());
    ArgV = B.createEmptyTuple(S);
  } else if (S->hasResult()) {
    // Value return.
    FullExpr scope(Cleanups);
    ArgV = visit(S->getResult()).forward(*this);
  } else {
    // Void return.
    ArgV = B.createEmptyTuple(S);
  }
  Cleanups.emitReturnAndCleanups(S, ArgV);
}

void SILGenFunction::visitIfStmt(IfStmt *S, SGFContext C) {
  Condition Cond = emitCondition(S, S->getCond(), S->getElseStmt() != nullptr);
  
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    visit(S->getThenStmt());
    Cond.exitTrue(B);
  }
  
  if (Cond.hasFalse()) {
    assert(S->getElseStmt());
    Cond.enterFalse(B);
    visit(S->getElseStmt());
    Cond.exitFalse(B);
  }
  
  Cond.complete(B);
}

void SILGenFunction::visitWhileStmt(WhileStmt *S, SGFContext C) {
  // Create a new basic block and jump into it.
  BasicBlock *LoopBB = new (F.getModule()) BasicBlock(&F, "while");
  B.emitBlock(LoopBB);
  
  // Set the destinations for 'break' and 'continue'
  BasicBlock *EndBB = new (F.getModule()) BasicBlock(&F, "while.end");
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(LoopBB, getCleanupsDepth());
  
  // Evaluate the condition with the false edge leading directly
  // to the continuation block.
  Condition Cond = emitCondition(S, S->getCond(), /*hasFalseCode*/ false);
  
  // If there's a true edge, emit the body in it.
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    visit(S->getBody());
    if (B.hasValidInsertionPoint())
      B.createBranch(LoopBB);
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void SILGenFunction::visitDoWhileStmt(DoWhileStmt *S, SGFContext C) {
  // Create a new basic block and jump into it.
  BasicBlock *LoopBB = new (F.getModule()) BasicBlock(&F, "dowhile");
  B.emitBlock(LoopBB);
  
  // Set the destinations for 'break' and 'continue'
  BasicBlock *EndBB = new (F.getModule()) BasicBlock(&F, "dowhile.end");
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(LoopBB, getCleanupsDepth());
  
  // Emit the body, which is always evaluated the first time around.
  visit(S->getBody());
  
  if (B.hasValidInsertionPoint()) {
    // Evaluate the condition with the false edge leading directly
    // to the continuation block.
    Condition Cond = emitCondition(S, S->getCond(), /*hasFalseCode*/ false);
    
    Cond.enterTrue(B);
    if (B.hasValidInsertionPoint())
      B.createBranch(LoopBB);
    
    Cond.exitTrue(B);
    // Complete the conditional execution.
    Cond.complete(B);
  }
  
  emitOrDeleteBlock(B, EndBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void SILGenFunction::visitForStmt(ForStmt *S, SGFContext C) {
  // Enter a new scope.
  Scope ForScope(Cleanups);
  
  // Emit any local 'var' variables declared in the initializer.
  for (auto D : S->getInitializerVarDecls())
    visit(D);
  
  if (Expr *E = S->getInitializer().dyn_cast<Expr*>()) {
    FullExpr Scope(Cleanups);
    visit(E);
  } else if (Stmt *AS = S->getInitializer().dyn_cast<AssignStmt*>()) {
    visit(AS);
  }
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!B.hasValidInsertionPoint()) return;
  
  // Create a new basic block and jump into it.
  BasicBlock *LoopBB = new (F.getModule()) BasicBlock(&F, "for.condition");
  B.emitBlock(LoopBB);
  
  // Set the destinations for 'break' and 'continue'
  BasicBlock *IncBB = new (F.getModule()) BasicBlock(&F, "for.inc");
  BasicBlock *EndBB = new (F.getModule()) BasicBlock(&F, "for.end");
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(IncBB, getCleanupsDepth());
  
  // Evaluate the condition with the false edge leading directly
  // to the continuation block.
  Condition Cond = S->getCond().isNonNull() ?
  emitCondition(S, S->getCond().get(), /*hasFalseCode*/ false) :
  Condition(LoopBB, 0, 0);  // Infinite loop.
  
  // If there's a true edge, emit the body in it.
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    visit(S->getBody());
    
    emitOrDeleteBlock(B, IncBB);
    
    if (B.hasValidInsertionPoint() && !S->getIncrement().isNull()) {
      if (Expr *E = S->getIncrement().dyn_cast<Expr*>()) {
        FullExpr Scope(Cleanups);
        visit(E);
      } else if (Stmt *AS = S->getIncrement().dyn_cast<AssignStmt*>()) {
        visit(AS);
      }
    }
    
    if (B.hasValidInsertionPoint())
      B.createBranch(LoopBB);
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void SILGenFunction::visitForEachStmt(ForEachStmt *S, SGFContext C) {
  // Emit the 'range' variable that we'll be using for iteration.
  Scope OuterForScope(Cleanups);
  visitPatternBindingDecl(S->getRange(), SGFContext());
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!B.hasValidInsertionPoint()) return;
  
  // Create a new basic block and jump into it.
  BasicBlock *LoopBB = new (F.getModule()) BasicBlock(&F, "foreach.cond");
  B.emitBlock(LoopBB);
  
  // Set the destinations for 'break' and 'continue'
  BasicBlock *EndBB = new (F.getModule()) BasicBlock(&F, "foreach.end");
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(LoopBB, getCleanupsDepth());
  
  Condition Cond = emitCondition(S, S->getRangeEmpty(), /*hasFalseCode=*/false,
                                 /*invertValue=*/true);
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    
    // Emit the loop body.
    // The declared variable(s) for the current element are destroyed
    // at the end of each loop iteration.
    {
      Scope InnerForScope(Cleanups);
      visitPatternBindingDecl(S->getElementInit(), SGFContext());
      visit(S->getBody());
    }
    
    // Loop back to the header.
    if (B.hasValidInsertionPoint())
      B.createBranch(LoopBB);
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void SILGenFunction::visitBreakStmt(BreakStmt *S, SGFContext C) {
  Cleanups.emitBranchAndCleanups(BreakDestStack.back());
}

void SILGenFunction::visitContinueStmt(ContinueStmt *S, SGFContext C) {
  Cleanups.emitBranchAndCleanups(ContinueDestStack.back());
}

ManagedValue SILGenFunction::emitMaterializedLoadFromLValue(SILLocation loc,
                                                           LValue const &src) {
  Value addr;
  
  assert(src.begin() != src.end() && "lvalue must have at least one component");
  for (auto &component : src) {
    if (component.isPhysical()) {
      addr = component.asPhysical().offset(*this, loc, addr);
    } else {
      addr = component.asLogical()
        .loadAndMaterialize(*this, loc, addr)
        .address;
    }
    assert((addr.getType().isAddress() ||
            addr.getType().hasReferenceSemantics()) &&
           "resolving lvalue component did not give an address "
           "or reference type");
  }
  assert(addr.getType().isAddress() &&
         "resolving lvalue did not give an address");
  return ManagedValue(addr);
}

static void assignPhysicalAddress(SILGenFunction &gen,
                                  SILLocation loc,
                                  ManagedValue src,
                                  Value addr) {
  SILType srcTy = src.getType();
  
  if (srcTy.isAddressOnly()) {
    src.forwardInto(gen, loc, addr, /*isInitialize=*/false);
  } else {
    // src is a loadable type; release the old value if necessary and store
    // the new.
    assert(!srcTy.isAddress() &&
           "can't assign loadable type from address");
    TypeLoweringInfo const &ti = gen.getTypeLoweringInfo(srcTy.getSwiftRValueType());
    
    Value old;
    
    if (!ti.isTrivial()) {
      old = gen.B.createLoad(loc, addr);
    }
    
    gen.B.createStore(loc, src.forward(gen), addr);
    if (old)
      gen.emitReleaseRValue(loc, old);
  }
}

void SILGenFunction::emitAssignToLValue(SILLocation loc,
                                        ManagedValue src, LValue const &dest) {
  struct StoreWriteback {
    Value base;
    Materialize member;
    LogicalPathComponent const *component;
  };
  Value destAddr;
  SmallVector<StoreWriteback, 4> writebacks;

  assert(dest.begin() != dest.end() &&
         "lvalue must have at least one component");
  
  auto component = dest.begin(), next = dest.begin(), end = dest.end();
  ++next;
  
  // Resolve all components up to the last, keeping track of value-type logical
  // properties we need to write back to.
  for (; next != end; component = next, ++next) {
    if (component->isPhysical()) {
      destAddr = component->asPhysical().offset(*this, loc, destAddr);
    } else {
      LogicalPathComponent const &lcomponent = component->asLogical();
      Materialize newDest = lcomponent
        .loadAndMaterialize(*this, loc, destAddr);
      if (!newDest.address.getType().hasReferenceSemantics())
        writebacks.push_back({destAddr, newDest, &lcomponent});
      destAddr = newDest.address;
    }
    
    if (destAddr.getType().hasReferenceSemantics())
      writebacks.clear();
  }
  
  // Write to the tail component.
  if (component->isPhysical()) {
    Value finalDestAddr = component->asPhysical().offset(*this, loc, destAddr);
    assignPhysicalAddress(*this, loc, src, finalDestAddr);
  } else {
    component->asLogical().storeRValue(*this, loc,
                                       src, destAddr);
  }
  
  // Write back through value-type logical properties.
  for (auto wb = writebacks.rbegin(), wend = writebacks.rend();
       wb != wend; ++wb) {
    // FIXME: address-only writeback
    ManagedValue wbValue = wb->member.consume(*this, loc);
    wb->component->storeRValue(*this, loc,
                               wbValue, wb->base);
  }
}

//===--------------------------------------------------------------------===//
// SILGenModule visitTopLevelCodeDecl implementation
//===--------------------------------------------------------------------===//

void SILGenModule::visitTopLevelCodeDecl(TopLevelCodeDecl *td) {
  // Emit top-level statements and expressions into the toplevel function until
  // we hit an unreachable point.
  assert(TopLevelSGF && "top-level code in a non-main module!");
  if (!TopLevelSGF->B.hasValidInsertionPoint())
    return;
  
  auto body = td->getBody();
  if (Expr *e = body.dyn_cast<Expr*>()) {
    FullExpr scope(TopLevelSGF->Cleanups);
    TopLevelSGF->visit(e);
  } else if (Stmt *s = body.dyn_cast<Stmt*>()) {
    TopLevelSGF->visit(s);
  } else {
    llvm_unreachable("unexpected toplevel decl");
  }
}
