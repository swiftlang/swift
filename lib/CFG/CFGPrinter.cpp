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

#include "swift/CFG/CFG.h"
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
    for (const BasicBlock &B : Block->cfg->blocks) { Map[&B] = idx++; }
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
  BasicBlock *Block = Inst->basicBlock;
  unsigned count = 1;
  for (const Instruction &I : Block->instructions) {
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

void Instruction::print(raw_ostream &OS, CFGPrintContext &PC,
                        unsigned Indent) const {
  OS.indent(Indent);
  PC.printID(OS, this, false) << " = ";

  switch (kind) {
    case Call: {
      const CallInst &CE = *cast<CallInst>(this);
      OS << "Call(fn=";
      PC.printID(OS, CE.function);
      auto args = CE.arguments();
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
      break;
    }
    case CondBranch: {
      const CondBranchInst &BI = *cast<CondBranchInst>(this);
      OS << "cond_br(cond=";
      OS << "?";
      //      PC.printID(OS, BI.condition);
      OS << ",branches=(";
      PC.printID(OS,BI.branches()[0]);
      OS << ',';
      PC.printID(OS,BI.branches()[1]);
      OS << "))";
      break;
    }
    case DeclRef: {
      const DeclRefInst &DI = *cast<DeclRefInst>(this);
      OS << "DeclRef(decl=" << DI.expr->getDecl()->getName() << ')';
      break;
    }
    case IntegerLit: {
      const IntegerLiteralInst &ILE = *cast<IntegerLiteralInst>(this);
      const auto &lit = ILE.literal->getValue();
      OS << "Integer(val=" << lit << ",width=" << lit.getBitWidth() << ')';
      break;
    }
    case Load: {
      const LoadInst &LI = *cast<LoadInst>(this);
      OS << "Load(lvalue=";
      PC.printID(OS, LI.lvalue);
      OS << ')';
      break;
    }
    case Return: {
      const ReturnInst &RI = *cast<ReturnInst>(this);
      OS << "Return";
      if (RI.returnValue) {
        OS << '(';
        PC.printID(OS, RI.returnValue);
        OS << ')';
      }
      break;
    };
    case ThisApply: {
      const ThisApplyInst &TAI = *cast<ThisApplyInst>(this);
      OS << "ThisApply(fn=";
      PC.printID(OS, TAI.function);
      OS << ",arg=";
      PC.printID(OS, TAI.argument);
      OS << ')';
      break;
    }
    case Tuple: {
      const TupleInst &TI = *cast<TupleInst>(this);
      OS << "Tuple(";
      bool isFirst = true;
      for (const auto &Elem : TI.elements()) {
        if (isFirst)
          isFirst = false;
        else
          OS << ',';
        PC.printID(OS, Elem);
      }
      OS << ')';
      break;
    }
    case TypeOf: {
      const TypeOfInst &TOI = *cast<TypeOfInst>(this);
      OS << "TypeOf(type=" << TOI.expr->getType().getString() << ')';
      break;
    }
    case UncondBranch: {
      const UncondBranchInst &UBI = *cast<UncondBranchInst>(this);
      OS << "br ";
      PC.printID(OS, UBI.targetBlock());
      const UncondBranchInst::ArgsTy Args = UBI.blockArgs();
      if (!Args.empty()) {
        OS << '(';
        for (auto Arg : Args) { OS << "%" << Arg; }
        OS << ')';
      }
      break;
    }
  }
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

  for (const Instruction &I : instructions)
    I.print(OS, PC, Indent);
  OS.indent(Indent) << "Preds:";
  for (const BasicBlock *B : preds()) {
    OS << ' ';
    PC.printID(OS, B);
  }
  OS << '\n';
  OS.indent(Indent) << "Succs:";
  for (const BasicBlock *B : succs()) {
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
  for (const BasicBlock &B : blocks)
    B.print(OS, PC, Indent);
}

