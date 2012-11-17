//===--- SILPrinter.cpp - Pretty-printing of SIL Code ---------------------===//
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
// This file defines the logic to pretty-print SIL, Instructions, etc.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

struct ID {
  enum {
    BasicBlock, SSAValue
  } Kind;
  unsigned Number;
  int ResultNumber;
};

raw_ostream &operator<<(raw_ostream &OS, ID i) {
  switch (i.Kind) {
  case ID::BasicBlock: OS << "bb"; break;
  case ID::SSAValue: OS << '%'; break;
  }
  OS << i.Number;

  if (i.ResultNumber != -1)
    OS << '#' << i.ResultNumber;
  return OS;
}


namespace {
/// SILPrinter class - This is the internal implementation details of printing
/// for SIL structures.
class SILPrinter : public SILVisitor<SILPrinter> {
  raw_ostream &OS;

  llvm::DenseMap<const BasicBlock *, unsigned> BlocksToIDMap;
  ID getID(const BasicBlock *B);

  llvm::DenseMap<const ValueBase*, unsigned> ValueToIDMap;
  ID getID(Value V);

public:
  SILPrinter(raw_ostream &OS) : OS(OS) {
  }

  void print(const BasicBlock *BB) {
    OS << getID(BB);

    if (!BB->bbarg_empty()) {
      OS << '(';
      for (auto I = BB->bbarg_begin(), E = BB->bbarg_end(); I != E; ++I) {
        if (I != BB->bbarg_begin()) OS << ", ";
        OS << getID(*I) << " : " << (*I)->getType().getString();
      }
      OS << ')';
    }

    OS << ":\t";

    if (!BB->pred_empty()) {
      OS << " ; Preds:";
      for (auto BBI = BB->pred_begin(), E = BB->pred_end(); BBI != E; ++BBI)
        OS << ' ' << getID(*BBI);
    }
    OS << '\n';

    for (const Instruction &I : *BB)
      print(&I);

    OS << '\n';
  }

  //===--------------------------------------------------------------------===//
  // Instruction Printing Logic

  void print(Value V) {
    ID Name = getID(V);
    Name.ResultNumber = -1;  // Don't print subresult number.
    OS << "  " << Name << " = ";
    visit(V);
    OS << '\n';
  }
  void visitInstruction(Instruction *I) {
    assert(0 && "SILPrinter not implemented for this instruction!");
  }

  void visitAllocVarInst(AllocVarInst *AVI) {
    OS << "alloc_var " << AVI->getDecl()->getName()
       << ", type=" << AVI->getType().getString();

  }

  void visitAllocTmpInst(AllocTmpInst *ATI) {
    OS << "alloc_tmp type=" << ATI->getType().getString();
  }

  void visitAllocBoxInst(AllocBoxInst *ABI) {
    OS << "alloc_box $" << ABI->getElementType().getString();
  }

  void visitAllocArrayInst(AllocArrayInst *AAI) {
    OS << "alloc_array $" << AAI->getElementType().getString()
       << ", " << getID(AAI->getNumElements());
  }

  void visitApplyInst(ApplyInst *AI) {
    OS << "apply " << getID(AI->getCallee()) << '(';
    bool first = true;
    for (auto arg : AI->getArguments()) {
      if (first)
        first = false;
      else
        OS << ", ";
      OS << getID(arg);
    }
    OS << ')';
  }

  void visitConstantRefInst(ConstantRefInst *DRI) {
    OS << "constantref " << DRI->getDecl()->getName()
    << ", type=" << DRI->getDecl()->getType().getString();
  }

  void visitZeroValueInst(ZeroValueInst *ZVI) {
    OS << "zerovalueinst " << ZVI->getType().getString();
  }

  void visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
    const auto &lit = ILI->getValue();
    OS << "integerliteral " << lit << ", width=" << lit.getBitWidth();
  }
  void visitFloatLiteralInst(FloatLiteralInst *FLI) {
    SmallVector<char, 12> Buffer;
    FLI->getValue().toString(Buffer);
    OS << "floatliteral " << StringRef(Buffer.data(), Buffer.size());
  }
  void visitStringLiteralInst(StringLiteralInst *SLI) {
    OS << "stringliteral \"" << SLI->getValue() << "\"";
  }
  void visitLoadInst(LoadInst *LI) {
    OS << "load " << getID(LI->getLValue());
  }
  void visitStoreInst(StoreInst *SI) {
    OS << "store " << getID(SI->getSrc()) << " to " << getID(SI->getDest());
  }
  void visitCopyInst(CopyInst *CI) {
    OS << "copy " << getID(CI->getSrc());
    if (CI->isTakeOfSrc())
      OS << " [take]";
    OS << " to " << getID(CI->getDest());
    if (CI->isInitializationOfDest())
      OS << " [initialization]";
  }
  void visitSpecializeInst(SpecializeInst *SI) {
    OS << "specialize " << getID(SI->getOperand()) << ", $"
       << SI->getType().getString();
  }
  void visitConvertInst(ConvertInst *CI) {
    OS << "convert " << getID(CI->getOperand()) << ", $"
       << CI->getType().getString();
  }
  void visitTupleInst(TupleInst *TI) {
    OS << "tuple (";
    bool isFirst = true;
    for (const auto &Elem : TI->getElements()) {
      if (isFirst)
        isFirst = false;
      else
        OS << ", ";
      OS << getID(Elem);
    }
    OS << ')';
  }
  void visitTupleElementInst(TupleElementInst *TEI) {
    OS << "tupleelement " << getID(TEI->getOperand()) << ", "
       << TEI->getFieldNo();
  }
  void visitMetatypeInst(MetatypeInst *MI) {
    OS << "metatype " << MI->getMetaType().getString();
  }
  
  void visitRetainInst(RetainInst *RI) {
    OS << "retain " << getID(RI->getOperand());
  }
  void visitReleaseInst(ReleaseInst *RI) {
    OS << "release " << getID(RI->getOperand());
  }
  void visitDeallocInst(DeallocInst *DI) {
    OS << "dealloc " << getID(DI->getOperand());
  }
  void visitDestroyInst(DestroyInst *DI) {
    OS << "destroy " << getID(DI->getOperand());
  }
  
  void visitIndexAddrInst(IndexAddrInst *IAI) {
    OS << "index_addr " << getID(IAI->getOperand()) << ", " <<IAI->getIndex();
  }

  void visitIntegerValueInst(IntegerValueInst *IVI) {
    OS << "integer_value " << IVI->getValue() << ", "
       << IVI->getType().getString();
  }

  void visitUnreachableInst(UnreachableInst *UI) {
    OS << "unreachable";
  }

  void visitReturnInst(ReturnInst *RI) {
    OS << "return " << '(' << getID(RI->getReturnValue()) << ')';
  }

  void visitBranchInst(BranchInst *UBI) {
    OS << "br " << getID(UBI->getDestBB());
  }

  void visitCondBranchInst(CondBranchInst *CBI) {
    OS << "condbranch " << getID(CBI->getCondition()) << ", "
       << getID(CBI->getTrueBB()) << ',' << getID(CBI->getFalseBB());
  }
};
} // end anonymous namespace

ID SILPrinter::getID(const BasicBlock *Block) {
  // Lazily initialize the Blocks-to-IDs mapping.
  if (BlocksToIDMap.empty()) {
    unsigned idx = 0;
    for (const BasicBlock &B : *Block->getParent())
      BlocksToIDMap[&B] = idx++;
  }

  ID R = { ID::BasicBlock, BlocksToIDMap[Block], -1 };
  return R;
}

ID SILPrinter::getID(Value V) {
  // Lazily initialize the instruction -> ID mapping.
  if (ValueToIDMap.empty()) {
    const BasicBlock *ParentBB;
    if (const Instruction *I = dyn_cast<Instruction>(V))
      ParentBB = I->getParent();
    else
      ParentBB = cast<BBArgument>(V)->getParent();

    unsigned idx = 0;
    for (auto &BB : *ParentBB->getParent()) {
      for (auto I = BB.bbarg_begin(), E = BB.bbarg_end(); I != E; ++I)
        ValueToIDMap[*I] = idx++;

      for (auto &I : BB)
        ValueToIDMap[&I] = idx++;
    }
  }

  int ResultNumber = -1;
  if (V.getDef()->getTypes().size() > 1)
    ResultNumber = V.getResultNumber();

  ID R = { ID::SSAValue, ValueToIDMap[V.getDef()], ResultNumber };
  return R;
}

//===----------------------------------------------------------------------===//
// Printing for Instruction, BasicBlock, and Function
//===----------------------------------------------------------------------===//

void ValueBase::dump() const {
  print(llvm::errs());
}

void ValueBase::print(raw_ostream &OS) const {
  SILPrinter(OS).print(this);
}

void BasicBlock::dump() const {
  print(llvm::errs());
}

/// Pretty-print the BasicBlock with the designated stream.
void BasicBlock::print(raw_ostream &OS) const {
  SILPrinter(OS).print(this);
}

/// Pretty-print the basic block.
void Function::dump() const {
  print(llvm::errs());
}

/// Pretty-print the basi block with the designated stream.
void Function::print(llvm::raw_ostream &OS) const {
  SILPrinter Printer(OS);
  for (const BasicBlock &B : *this)
    Printer.print(&B);
}

