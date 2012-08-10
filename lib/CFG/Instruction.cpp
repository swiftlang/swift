//===--- Instruction.cpp - Instructions for high-level CFGs ------*- C++ -*-==//
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
// This file defines the high-level Instruction classes used for Swift CFGs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AST.h"
#include "swift/CFG/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/FoldingSet.h"
#include <algorithm>

using namespace swift;

Instruction::Instruction(BasicBlock *B, Kind K)
  : kind(K), basicBlock(B) {
  B->instructions.push_back(this);
}

static raw_ostream &printID(raw_ostream &OS, const BasicBlock *Block) {
  static const BasicBlock *lastQueried = 0;
  static unsigned lastCalculated = 0;

  unsigned count = 1;

  if (lastQueried == Block) {
    count = lastCalculated;
  }
  else {
    CFG *C = Block->cfg;
    for (BasicBlock &B : C->blocks) {
      if (&B == Block)
        break;
      ++count;
    }
    lastQueried = Block;
    lastCalculated = count;
  }

  OS << count;
  return OS;
}

static raw_ostream &printID(raw_ostream &OS, const Instruction *Inst) {
  BasicBlock *Block = Inst->basicBlock;
  unsigned count = 1;
  for (const Instruction &I : Block->instructions) {
    if (&I == Inst)
      break;
    ++count;
  }
  printID(OS, Block) << '.' << count;
  return OS;
}

static raw_ostream &printID(raw_ostream &OS, const CFGConstant *Const) {
  OS << "Constant (unsupported)\n";
  return OS;
}

static raw_ostream &printID(raw_ostream &OS, const BasicBlockArg *BBArg) {
  OS << "BBArg (unsupported)\n";
  return OS;
}

static raw_ostream &printID(raw_ostream &OS, const CFGValue &Val) {
  if (const Instruction *Inst = Val.dyn_cast<Instruction*>())
    printID(OS, Inst);
  else if (const CFGConstant *Const = Val.dyn_cast<CFGConstant*>())
    printID(OS, Const);
  else
    printID(OS, Val.get<BasicBlockArg*>());
  return OS;
}

void Instruction::print(raw_ostream &OS) const {
  printID(OS, this) << ": (";

  switch (kind) {
    case Invalid:
      OS << "Invalid";
      break;
    case Call: {
      const CallInst &CE = *cast<CallInst>(this);
      OS << "Call fn=";
      printID(OS, CE.function);
      auto args = CE.arguments();
      if (!args.empty()) {
        OS << " args={";
        for (auto arg : args) {
          OS << ' ';
          printID(OS, arg);
        }
        OS << '}';
      }
      break;
    }
    case DeclRef: {
      const DeclRefInst &DI = *cast<DeclRefInst>(this);
      OS << "DeclRef decl=" << DI.expr->getDecl()->getName();
      break;
    }
    case ThisApply: {
      const ThisApplyInst &TAI = *cast<ThisApplyInst>(this);
      OS << "ThisApply fn=";
      printID(OS, TAI.function);
      OS << " arg=";
      printID(OS, TAI.argument);
      break;
    }
    case TypeOf: {
      const TypeOfInst &TOI = *cast<TypeOfInst>(this);
      OS << "TypeOf type=" << TOI.expr->getType().getString();
      break;
    }
    case UncondBranch: {
      const UncondBranchInst &UBI = *cast<UncondBranchInst>(this);
      OS << "br " << UBI.targetBlock();
      const UncondBranchInst::ArgsTy Args = UBI.blockArgs();
      if (!Args.empty()) {
        OS << '(';
        for (auto Arg : Args) { OS << "%" << Arg; }
        OS << ')';
      }
      break;
    }
  }
  OS << ")\n";
}

void Instruction::dump() const { print(llvm::errs()); }

void Instruction::validateNonTerm() const {
  assert(basicBlock->instructions.size() > 1);
  assert(&*basicBlock->instructions.rbegin() != this &&
         "Non-terminator Instructions cannot be the last in a block");
}

void Instruction::validate() const {
  if (kind > Invalid && kind < TERM_INST_BEGIN)
    validateNonTerm();

  switch (kind) {
    case Invalid:
    case Call:
    case DeclRef:
    case ThisApply:
    case TypeOf:
      return;
    case UncondBranch: {
      const UncondBranchInst &UBI = *cast<UncondBranchInst>(this);
      assert(!basicBlock->instructions.empty() &&
             &*basicBlock->instructions.rbegin() == this &&
             "UncondBranchInst must appear at end of BasicBlock");
      const BasicBlock &targetBlock = UBI.targetBlock();
      assert(std::find(targetBlock.preds().begin(), targetBlock.preds().end(),
                       basicBlock) &&
             "BasicBlock of UncondBranchInst must be a predecessor of target");
      (void)targetBlock;
    }
  }
}

TermInst::Successors TermInst::successors() {
  switch (kind) {
    case Invalid:
    case Call:
    case DeclRef:
    case ThisApply:
    case TypeOf:
      llvm_unreachable("Only TermInst's are allowed");
    case UncondBranch: {
      UncondBranchInst &UBI = *cast<UncondBranchInst>(this);
      return Successors(&UBI.targetBlock());
    }
  }
}

CallInst *CallInst::create(CallExpr *expr,
                           BasicBlock *B,
                           CFGValue function,
                           ArrayRef<CFGValue> args) {
  CFG &cfg = *B->cfg;
  void *Buffer = cfg.allocate(sizeof(CallInst) +
                              args.size() * sizeof(CFGValue),
                              llvm::AlignOf<CallInst>::Alignment);
  return ::new(Buffer) CallInst(expr, B, function, args);
}

CallInst::CallInst(CallExpr *expr, BasicBlock *B,
                   CFGValue function,
                   ArrayRef<CFGValue> args)
  : Instruction(B, Call), NumArgs(args.size()), expr(expr),
    function(function) {
  memcpy(getArgsStorage(), args.data(), args.size() * sizeof(CFGValue));
}

void UncondBranchInst::unregisterTarget() {
  if (!TargetBlock)
    return;

}

void UncondBranchInst::setTarget(BasicBlock *NewTarget, const ArgsTy BlockArgs){
  if (TargetBlock != NewTarget) {
    unregisterTarget();
    TargetBlock = NewTarget;
    TargetBlock->addPred(basicBlock);
  }

  // FIXME: check that TargetBlock's # args agrees with BlockArgs.

  if (BlockArgs.empty())
    return;

  // Copy the arguments over to our holding buffer.
  NumArgs = BlockArgs.size();
  Args = new (basicBlock->cfg) unsigned[NumArgs];
  ArgsTy(Args, NumArgs) = BlockArgs;
}
