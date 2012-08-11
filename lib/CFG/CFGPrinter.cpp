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

#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/CFG/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/OwningPtr.h"

namespace swift {
class CFGPrintContext {
public:
  raw_ostream &printID(raw_ostream &OS, const Instruction *I);
  raw_ostream &printID(raw_ostream &OS, const BasicBlock *B);
  raw_ostream &printID(raw_ostream &OS, const BasicBlockArg *BBarg);
  raw_ostream &printID(raw_ostream &OS, const CFGConstant *C);
  raw_ostream &printID(raw_ostream &OS, const CFGValue &V);

};
}

using namespace swift;

raw_ostream &CFGPrintContext::printID(raw_ostream &OS,
                                      const BasicBlock *Block)
{
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

raw_ostream &CFGPrintContext::printID(raw_ostream &OS,
                                      const Instruction *Inst)
{
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

raw_ostream &CFGPrintContext::printID(raw_ostream &OS,
                                      const CFGConstant *Const)
{
  OS << "Constant (unsupported)\n";
  return OS;
}

raw_ostream &CFGPrintContext::printID(raw_ostream &OS,
                                      const BasicBlockArg *BBArg)
{
  OS << "BBArg (unsupported)\n";
  return OS;
}

raw_ostream &CFGPrintContext::printID(raw_ostream &OS,
                                      const CFGValue &Val)
{
  if (const Instruction *Inst = Val.dyn_cast<Instruction*>())
    printID(OS, Inst);
  else if (const CFGConstant *Const = Val.dyn_cast<CFGConstant*>())
    printID(OS, Const);
  else
    printID(OS, Val.get<BasicBlockArg*>());
  return OS;
}

//===----------------------------------------------------------------------===//
// Pretty-printing for Instructions.
//===----------------------------------------------------------------------===//

void Instruction::print(raw_ostream &OS, CFGPrintContext &PC) const {
  PC.printID(OS, this) << ": (";

  switch (kind) {
    case Invalid:
      OS << "Invalid";
      break;
    case Call: {
      const CallInst &CE = *cast<CallInst>(this);
      OS << "Call fn=";
      PC.printID(OS, CE.function);
      auto args = CE.arguments();
      if (!args.empty()) {
        OS << " args={";
        for (auto arg : args) {
          OS << ' ';
          PC.printID(OS, arg);
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
      PC.printID(OS, TAI.function);
      OS << " arg=";
      PC.printID(OS, TAI.argument);
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
void BasicBlock::print(raw_ostream &OS, CFGPrintContext &PC) const {
  OS << "[Block " << (void*) this << "]\n";
  for (const Instruction &I : instructions)
    I.print(OS, PC);
  OS << "Preds:";
  for (const BasicBlock *B : preds())
    OS << ' ' << (void*) B;
  OS << "\nSuccs:";
  for (const BasicBlock *B : succs())
    OS << ' ' << (void*) B;
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

    OS << "(func_decl " << FD->getName() << '\n';
    C->print(OS, PC);
    OS << ")\n";
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
void CFG::print(llvm::raw_ostream &OS, CFGPrintContext &PC) const {
  for (const BasicBlock &B : blocks) {
    B.print(OS, PC);
  }
}

