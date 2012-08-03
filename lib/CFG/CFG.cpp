//===--- CFG.cpp - Defines the CFG data structure ----------------*- C++ -*-==//
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

#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/CFG/CFG.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

CFG::CFG() :
  EntryBlock(0),
  ExitBlock(0) {}

CFG::~CFG() {
  // FIXME: if all parts of BasicBlock are BumpPtrAllocated, this shouldn't
  // eventually be needed.
  for(BasicBlock *B : *this) { B->~BasicBlock(); }
}

/// Pretty-print the basic block.
void CFG::dump() const { print(llvm::errs()); }

/// Pretty-print the basi block with the designated stream.
void CFG::print(llvm::raw_ostream &OS) const {
  for (const BasicBlock *B : Blks) {
    OS << "[Block " << B->blockID << "]\n";
    B->print(OS);
  }
}

CFG *CFG::constructCFG(const Stmt *S) {
  // FIXME: implement CFG construction.
  CFG *C = new CFG();
  unsigned blockID = 0;

  BasicBlock *ExitBlock = new (*C, 8) BasicBlock(C, blockID);
  BasicBlock *EntryBlock = new (*C, 8) BasicBlock(C, blockID);
  ExitBlock->addPred(EntryBlock);
  EntryBlock->setSucc(ExitBlock);

  (void) ExitBlock;
  (void) EntryBlock;

  return new CFG();
}

namespace {
class DumpVisitor : public ASTVisitor<DumpVisitor> {
public:
  DumpVisitor(llvm::raw_ostream &OS) : OS(OS) {}

  llvm::raw_ostream &OS;

  void visitFuncDecl(FuncDecl *FD) {
    OS << "(func_decl " << FD->getName() << '\n';
    FuncExpr *FE = FD->getBody();
    llvm::OwningPtr<CFG> C(CFG::constructCFG(FE->getBody()));
    C->print(OS);
    OS << ")\n";
  }
};
}

void CFG::dump(TranslationUnit *TU) {
  for (Decl *D : TU->Decls) { DumpVisitor(llvm::errs()).visit(D); }
}
