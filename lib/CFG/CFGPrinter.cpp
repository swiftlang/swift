//===--- CFGPrinter.cpp - Pretty-printing of CFGs --------------------------==//
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
// This file defines the logic to pretty-print CFGs, Instructions, etc.
//
//===----------------------------------------------------------------------===//

#include "swift/CFG/CFGVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/OwningPtr.h"
using namespace swift;

struct ID {
  enum {
    BasicBlock, SSAValue
  } Kind;
  unsigned Number;
};

raw_ostream &operator<<(raw_ostream &OS, ID i) {
  switch (i.Kind) {
  case ID::BasicBlock: OS << "bb"; break;
  case ID::SSAValue: OS << '%'; break;
  }
  return OS << i.Number;
}


namespace {
/// CFGPrinter class - This is the internal implementation details of printing
/// for CFG structures.
class CFGPrinter : public CFGVisitor<CFGPrinter> {
  raw_ostream &OS;

  llvm::DenseMap<const BasicBlock *, unsigned> BlocksToIDMap;
  ID getID(const BasicBlock *B);

  llvm::DenseMap<const Instruction*, unsigned> InstructionToIDMap;
  ID getID(const Instruction *I);
  ID getID(const BasicBlockArg *BBarg);
  ID getID(CFGValue V);

public:
  CFGPrinter(raw_ostream &OS) : OS(OS) {
  }

  void print(const BasicBlock *BB) {
    OS << getID(BB) << ":\t";

    OS << " ; Preds:";
    for (auto BBI = BB->pred_begin(), E = BB->pred_end(); BBI != E; ++BBI)
      OS << ' ' << getID(*BBI);
    OS << '\n';

    for (const Instruction &I : *BB)
      print(&I);

    OS << '\n';
  }

  //===--------------------------------------------------------------------===//
  // Instruction Printing Logic

  void print(const Instruction *I) {
    OS << "  " << getID(I) << " = ";
    visit(const_cast<Instruction*>(I));
    OS << '\n';
  }

  void visitInstruction(Instruction *I) {
    assert(0 && "CFGPrinter not implemented for this instruction!");
  }

  void visitCallInst(CallInst *CI) {
    OS << "call " << getID(CI->function) << '(';
    bool first = true;
    for (auto arg : CI->arguments()) {
      if (first)
        first = false;
      else
        OS << ", ";
      OS << getID(arg);
    }
    OS << ')';
  }

  void visitDeclRefInst(DeclRefInst *DRI) {
    OS << "declref " << DRI->expr->getDecl()->getName()
       << ", type=" << DRI->expr->getDecl()->getType().getString();
  }
  void visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
    const auto &lit = ILI->literal->getValue();
    OS << "integerliteral " << lit << ", width=" << lit.getBitWidth();
  }
  void visitLoadInst(LoadInst *LI) {
    OS << "load " << getID(LI->lvalue);
  }
  void visitThisApplyInst(ThisApplyInst *TAI) {
    OS << "thisapply "<< getID(TAI->function) << '('
       << getID(TAI->argument) << ')';
  }
  void visitTupleInst(TupleInst *TI) {
    OS << "tuple (";
    bool isFirst = true;
    for (const auto &Elem : TI->elements()) {
      if (isFirst)
        isFirst = false;
      else
        OS << ", ";
      OS << getID(Elem);
    }
    OS << ')';
  }
  void visitTypeOfInst(TypeOfInst *TOI) {
    OS << "typeof " << TOI->Expr->getType().getString();
  }

  void visitReturnInst(ReturnInst *RI) {
    OS << "return ";
    if (RI->returnValue)
      OS << '(' << getID(RI->returnValue) << ')';
  }

  void visitBranchInst(BranchInst *UBI) {
    OS << "br " << getID(UBI->getDestBB());
    const BranchInst::ArgsTy Args = UBI->blockArgs();

    // FIXME: Args should move to terminator generic stuff.
    if (!Args.empty()) {
      OS << '(';
      for (auto Arg : Args) { OS << "%" << Arg; }
      OS << ')';
    }
  }

  void visitCondBranchInst(CondBranchInst *CBI) {
    OS << "condbranch " << /*getID(CBI->condition) <<*/ "???, "
       << getID(CBI->getTrueBB()) << ',' << getID(CBI->getFalseBB());
  }
};
} // end anonymous namespace

ID CFGPrinter::getID(const BasicBlock *Block) {
  // Lazily initialize the Blocks-to-IDs mapping.
  if (BlocksToIDMap.empty()) {
    unsigned idx = 0;
    for (const BasicBlock &B : *Block->getParent())
      BlocksToIDMap[&B] = idx++;
  }

  ID R = { ID::BasicBlock, BlocksToIDMap[Block] };
  return R;
}

ID CFGPrinter::getID(const Instruction *Inst) {
  // Lazily initialize the instruction -> ID mapping.
  if (InstructionToIDMap.empty()) {
    unsigned idx = 0;
    for (auto &BB : *Inst->getParent()->getParent())
      for (auto &I : BB)
        InstructionToIDMap[&I] = idx++;
  }

  ID R = { ID::SSAValue, InstructionToIDMap[Inst] };
  return R;
}

ID CFGPrinter::getID(const BasicBlockArg *BBArg) {
  // FIXME: Not implemented yet.
  ID R = { ID::SSAValue, ~0U };
  return R;
}

ID CFGPrinter::getID(CFGValue Val) {
  if (const Instruction *Inst = Val.dyn_cast<Instruction*>())
    return getID(Inst);
  return getID(Val.get<BasicBlockArg*>());
}

//===----------------------------------------------------------------------===//
// Printing for Instruction, BasicBlock, and CFG
//===----------------------------------------------------------------------===//

void Instruction::dump() const {
  print(llvm::errs());
}

void Instruction::print(raw_ostream &OS) const {
  CFGPrinter(OS).print(this);
}

void BasicBlock::dump() const {
  print(llvm::errs());
}

/// Pretty-print the BasicBlock with the designated stream.
void BasicBlock::print(raw_ostream &OS) const {
  CFGPrinter(OS).print(this);
}

/// Pretty-print the basic block.
void CFG::dump() const {
  print(llvm::errs());
}

/// Pretty-print the basi block with the designated stream.
void CFG::print(llvm::raw_ostream &OS) const {
  CFGPrinter Printer(OS);
  for (const BasicBlock &B : *this)
    Printer.print(&B);
}

