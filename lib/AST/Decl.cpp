//===--- Decl.cpp - Swift Language Decl ASTs ------------------------------===//
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
//  This file implements the Decl class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;
using llvm::cast;

// Only allow allocation of Decls using the allocator in ASTContext.
void *Decl::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) throw() {
  return C.Allocate(Bytes, Alignment);
}

void Decl::dump() const { print(llvm::errs()); llvm::errs() << '\n'; }

void Decl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  switch (getKind()) {
  case VarDeclKind:  return cast<VarDecl>(this)->print(OS, Indent);
  case FuncDeclKind: return cast<FuncDecl>(this)->print(OS, Indent);
  case AnonDeclKind: return cast<AnonDecl>(this)->print(OS, Indent);
  case ElementRefDeclKind: return cast<ElementRefDecl>(this)->print(OS, Indent);
  }
}


llvm::SMLoc NamedDecl::getLocStart() const {
  switch (getKind()) {
  case VarDeclKind:  return cast<VarDecl>(this)->VarLoc;
  case FuncDeclKind: return cast<FuncDecl>(this)->FuncLoc;
  case AnonDeclKind: return cast<AnonDecl>(this)->UseLoc;
  case ElementRefDeclKind: return cast<ElementRefDecl>(this)->NameLoc;
  }

  assert(0 && "Unknown decl kind");
  return llvm::SMLoc();
}


void NamedDecl::printCommon(llvm::raw_ostream &OS, unsigned Indent) const {
  OS << "'" << Name << "' type='";
  Ty->print(OS);
  OS << "'";
  
  if (Init) {
    OS << '\n';
    Init->print(OS, Indent+1);
  }
  OS << ')';
}


void VarDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(vardecl ";
  printCommon(OS, Indent);
}

void FuncDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(funcdecl ";
  printCommon(OS, Indent);
}

void AnonDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(anondecl ";
  printCommon(OS, Indent);
}

void ElementRefDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(elementrefdecl ";
  printCommon(OS, Indent);
}

