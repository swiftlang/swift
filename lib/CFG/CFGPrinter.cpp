//===--- CFGPrinter.cpp - Pretty-printing of CFGs ----------------*- C++ -*-==//
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
    BasicBlock, LocalVal
  } Kind;
  unsigned Number;
};

raw_ostream &operator<<(raw_ostream &OS, ID i) {
  switch (i.Kind) {
  case ID::BasicBlock: OS << "b"; break;
  case ID::LocalVal: OS << '%'; break;
  }
  return OS << i.Number;
}


namespace {
/// CFGPrinter class - This is the internal implementation details of printing
/// for CFG structures.
class CFGPrinter : public CFGVisitor<CFGPrinter> {
  raw_ostream &OS;

  llvm::DenseMap<const BasicBlock *, unsigned> BlocksToIDsMap;
  ID getID(const BasicBlock *B);

  raw_ostream &printID(const Instruction *I, bool includeBBPrefix = true);
  raw_ostream &printID(const BasicBlockArg *BBarg);
  raw_ostream &printID(const CFGValue &V);

public:
  CFGPrinter(raw_ostream &OS) : OS(OS) {
  }

  void print(const BasicBlock *BB) {
    OS << getID(BB) << ":\n";

    for (const Instruction &I : *BB)
      print(&I);

    OS << "  Preds:";
    for (const BasicBlock *B : BB->getPreds())
      OS << ' ' << getID(B);
    OS << "\n  Succs:";
    for (const BasicBlock *B : BB->getSuccs())
      OS << ' ' << getID(B);
    OS << '\n';
  }

  //===--------------------------------------------------------------------===//
  // Instruction Printing Logic

  void print(const Instruction *I) {
    OS << "  ";
    printID(I, false) << " = ";
    visit(const_cast<Instruction*>(I));
    OS << '\n';
  }

  void visitInstruction(Instruction *I) {
    assert(0 && "CFGPrinter not implemented for this instruction!");
  }

  void visitCallInst(CallInst *CI) {
    OS << "Call(fn=";
    printID(CI->function);
    auto args = CI->arguments();
    if (!args.empty()) {
      bool first = true;
      OS << ",args=(";
      for (auto arg : args) {
        if (first)
          first = false;
        else
          OS << ' ';
        printID(arg);
      }
      OS << ')';
    }
    OS << ')';
  }

  void visitDeclRefInst(DeclRefInst *DRI) {
    OS << "DeclRef(decl=" << DRI->expr->getDecl()->getName() << ')';
  }
  void visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
    const auto &lit = ILI->literal->getValue();
    OS << "Integer(val=" << lit << ",width=" << lit.getBitWidth() << ')';
  }
  void visitLoadInst(LoadInst *LI) {
    OS << "Load(lvalue=";
    printID(LI->lvalue);
    OS << ')';
  }
  void visitThisApplyInst(ThisApplyInst *TAI) {
    OS << "ThisApply(fn=";
    printID(TAI->function);
    OS << ",arg=";
    printID(TAI->argument);
    OS << ')';
  }
  void visitTupleInst(TupleInst *TI) {
    OS << "Tuple(";
    bool isFirst = true;
    for (const auto &Elem : TI->elements()) {
      if (isFirst)
        isFirst = false;
      else
        OS << ',';
      printID(Elem);
    }
    OS << ')';
  }
  void visitTypeOfInst(TypeOfInst *TOI) {
    OS << "TypeOf(type=" << TOI->Expr->getType().getString() << ')';
  }

  void visitReturnInst(ReturnInst *RI) {
    OS << "Return";
    if (RI->returnValue) {
      OS << '(';
      printID(RI->returnValue);
      OS << ')';
    }
  }

  void visitUncondBranchInst(UncondBranchInst *UBI) {
    OS << "br " << getID(UBI->targetBlock());
    const UncondBranchInst::ArgsTy Args = UBI->blockArgs();
    if (!Args.empty()) {
      OS << '(';
      for (auto Arg : Args) { OS << "%" << Arg; }
      OS << ')';
    }
  }

  void visitCondBranchInst(CondBranchInst *CBI) {
    OS << "cond_br(cond=";
    OS << "?";
    //      printID(BI.condition);
    OS << ",branches=(" << getID(CBI->branches()[0]);
    OS << ',' << getID(CBI->branches()[1]);
    OS << "))";
  }
};
} // end anonymous namespace

ID CFGPrinter::getID(const BasicBlock *Block) {
  // Lazily initialize the Blocks-to-IDs mapping.
  if (BlocksToIDsMap.empty()) {
    unsigned idx = 0;
    for (const BasicBlock &B : *Block->getParent())
      BlocksToIDsMap[&B] = idx++;
  }

  ID R = { ID::BasicBlock, BlocksToIDsMap[Block] };
  return R;
}

raw_ostream &CFGPrinter::printID(const Instruction *Inst,
                                 bool includeBBPrefix) {
  const BasicBlock *Block = Inst->getParent();
  unsigned count = 1;
  for (const Instruction &I : *Block) {
    if (&I == Inst)
      break;
    ++count;
  }
  OS << '%';
  if (includeBBPrefix)
    OS << getID(Block) << '.';
  OS << "i" << count;
  return OS;
}

raw_ostream &CFGPrinter::printID(const BasicBlockArg *BBArg) {
  OS << "BBArg (unsupported)\n";
  return OS;
}

raw_ostream &CFGPrinter::printID(const CFGValue &Val) {
  if (const Instruction *Inst = Val.dyn_cast<Instruction*>())
    printID(Inst);
  else
    printID(Val.get<BasicBlockArg*>());
  return OS;
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

