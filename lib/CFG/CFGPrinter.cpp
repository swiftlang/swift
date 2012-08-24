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
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/OwningPtr.h"

namespace swift {
class CFGPrintContext {
  typedef llvm::DenseMap<const BasicBlock *, unsigned> BlocksToIdsTy;
  llvm::OwningPtr<BlocksToIdsTy> BlocksToIDs;
public:
  raw_ostream &printID(raw_ostream &OS, const Instruction *I,
                       bool includeBBPrefix = true);
  raw_ostream &printID(raw_ostream &OS, const BasicBlock *B);
  raw_ostream &printID(raw_ostream &OS, const BasicBlockArg *BBarg);
  raw_ostream &printID(raw_ostream &OS, const CFGValue &V);

};
}

using namespace swift;

raw_ostream &CFGPrintContext::printID(raw_ostream &OS,
                                      const BasicBlock *Block) {
  // Lazily initialize the Blocks-to-IDs mapping.
  if (!BlocksToIDs) {
    BlocksToIDs.reset(new BlocksToIdsTy());
    BlocksToIdsTy &Map = *BlocksToIDs;
    unsigned idx = 0;
    for (const BasicBlock &B : *Block->getParent())
      Map[&B] = idx++;
  }

  BlocksToIdsTy &Map = *BlocksToIDs;
  auto I = Map.find(Block);
  assert(I != Map.end());

  OS << "b" << I->second;
  return OS;
}

raw_ostream &CFGPrintContext::printID(raw_ostream &OS,
                                      const Instruction *Inst,
                                      bool includeBBPrefix) {
  const BasicBlock *Block = Inst->getParent();
  unsigned count = 1;
  for (const Instruction &I : *Block) {
    if (&I == Inst)
      break;
    ++count;
  }
  OS << '%';
  if (includeBBPrefix) {
    printID(OS, Block);
    OS << '.';
  }
  OS << "i" << count;
  return OS;
}

raw_ostream &CFGPrintContext::printID(raw_ostream &OS,
                                      const BasicBlockArg *BBArg) {
  OS << "BBArg (unsupported)\n";
  return OS;
}

raw_ostream &CFGPrintContext::printID(raw_ostream &OS,
                                      const CFGValue &Val) {
  if (const Instruction *Inst = Val.dyn_cast<Instruction*>())
    printID(OS, Inst);
  else
    printID(OS, Val.get<BasicBlockArg*>());
  return OS;
}

//===----------------------------------------------------------------------===//
// Pretty-printing for Instructions.
//===----------------------------------------------------------------------===//

class CFGPrinter : public CFGVisitor<CFGPrinter> {
  raw_ostream &OS;
  CFGPrintContext &PC;
public:
  CFGPrinter(raw_ostream &OS, CFGPrintContext &PC) : OS(OS), PC(PC) {
  }

  void visitInstruction(Instruction *I) {
    assert(0 && "CFGPrinter not implemented for this instruction!");
  }

  void visitCallInst(CallInst *CI) {
    OS << "Call(fn=";
    PC.printID(OS, CI->function);
    auto args = CI->arguments();
    if (!args.empty()) {
      bool first = true;
      OS << ",args=(";
      for (auto arg : args) {
        if (first)
          first = false;
        else
          OS << ' ';
        PC.printID(OS, arg);
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
    PC.printID(OS, LI->lvalue);
    OS << ')';
  }
  void visitThisApplyInst(ThisApplyInst *TAI) {
    OS << "ThisApply(fn=";
    PC.printID(OS, TAI->function);
    OS << ",arg=";
    PC.printID(OS, TAI->argument);
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
      PC.printID(OS, Elem);
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
      PC.printID(OS, RI->returnValue);
      OS << ')';
    }
  }

  void visitUncondBranchInst(UncondBranchInst *UBI) {
    OS << "br ";
    PC.printID(OS, UBI->targetBlock());
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
    //      PC.printID(OS, BI.condition);
    OS << ",branches=(";
    PC.printID(OS, CBI->branches()[0]);
    OS << ',';
    PC.printID(OS, CBI->branches()[1]);
    OS << "))";
  }
};


void Instruction::print(raw_ostream &OS, CFGPrintContext &PC,
                        unsigned Indent) const {
  OS.indent(Indent);
  PC.printID(OS, this, false) << " = ";
  CFGPrinter(OS, PC).visit(const_cast<Instruction*>(this));
  OS << '\n';
}

void Instruction::dump() const {
  CFGPrintContext PC;
  print(llvm::errs(), PC);
}

//===----------------------------------------------------------------------===//
// Pretty-printing for BasicBlocks.
//===----------------------------------------------------------------------===//

/// Pretty-print the BasicBlock.
void BasicBlock::dump() const {
  CFGPrintContext PC;
  print(llvm::errs(), PC);
}

/// Pretty-print the BasicBlock with the designated stream.
void BasicBlock::print(raw_ostream &OS, CFGPrintContext &PC,
                       unsigned Indent) const {
  OS.indent(Indent);
  PC.printID(OS, this) << ":\n";
  Indent += 2;

  for (const Instruction &I : *this)
    I.print(OS, PC, Indent);
  OS.indent(Indent) << "Preds:";
  for (const BasicBlock *B : getPreds()) {
    OS << ' ';
    PC.printID(OS, B);
  }
  OS << '\n';
  OS.indent(Indent) << "Succs:";
  for (const BasicBlock *B : getSuccs()) {
    OS << ' ';
    PC.printID(OS, B);
  }
  OS << '\n';
}

namespace llvm {
raw_ostream &operator<<(raw_ostream &OS, const ::swift::BasicBlock &B) {
  OS << 'B' << (void*) &B;
  return OS;
}
} // end namespace llvm

//===----------------------------------------------------------------------===//
// CFG pretty-printing.
//===----------------------------------------------------------------------===//

namespace {
class DumpVisitor : public ASTVisitor<DumpVisitor> {
public:
  DumpVisitor(llvm::raw_ostream &OS, CFGPrintContext &PC) : OS(OS), PC(PC) {}

  raw_ostream &OS;
  CFGPrintContext &PC;

  void visitFuncDecl(FuncDecl *FD) {
    FuncExpr *FE = FD->getBody();
    llvm::OwningPtr<CFG> C(CFG::constructCFG(FE->getBody()));

    if (!C)
      return;

    OS << "func_decl " << FD->getName() << '\n';
    C->print(OS, PC, 2);
    OS << "\n";
  }
};
}

void CFG::dump(TranslationUnit *TU) {
  for (Decl *D : TU->Decls) {
    CFGPrintContext PC;
    DumpVisitor(llvm::errs(), PC).visit(D);
  }
}

/// Pretty-print the basic block.
void CFG::dump() const {
  CFGPrintContext PC;
  print(llvm::errs(), PC);
}

/// Pretty-print the basi block with the designated stream.
void CFG::print(llvm::raw_ostream &OS, CFGPrintContext &PC,
                unsigned Indent) const {
  for (const BasicBlock &B : *this)
    B.print(OS, PC, Indent);
}

