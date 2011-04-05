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
#include "swift/AST/Types.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;
using llvm::cast;

// Only allow allocation of Decls using the allocator in ASTContext.
void *DeclVarName::operator new(size_t Bytes, ASTContext &C,
                                unsigned Alignment) throw() {
  return C.Allocate(Bytes, Alignment);
}

// Only allow allocation of Decls using the allocator in ASTContext.
void *Decl::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) throw() {
  return C.Allocate(Bytes, Alignment);
}

llvm::SMLoc TranslationUnitDecl::getLocStart() const {
  return Body ? Body->getLocStart() : llvm::SMLoc();
}


llvm::SMLoc Decl::getLocStart() const {
  switch (getKind()) {
  case TranslationUnitDeclKind:
    return cast<TranslationUnitDecl>(this)->getLocStart();
  case ImportDeclKind: return cast<ImportDecl>(this)->getLocStart();
  case TypeAliasDeclKind:  return cast<TypeAliasDecl>(this)->getLocStart(); 
  case VarDeclKind:        return cast<VarDecl>(this)->getLocStart();
  case FuncDeclKind:       return cast<FuncDecl>(this)->getLocStart();
  case OneOfElementDeclKind:return cast<OneOfElementDecl>(this)->getLocStart();
  case ArgDeclKind:        return cast<ArgDecl>(this)->getLocStart();
  case ElementRefDeclKind: return cast<ElementRefDecl>(this)->getLocStart();
  }

  assert(0 && "Unknown decl kind");
  return llvm::SMLoc();
}

/// getAliasType - Return the sugared version of this decl as a Type.
NameAliasType *TypeAliasDecl::getAliasType(ASTContext &C) const {
  // Lazily create AliasTy. 
  if (AliasTy == 0)
    AliasTy = new (C) NameAliasType(const_cast<TypeAliasDecl*>(this));
   
  return AliasTy;
}

/// getTypeForPath - Given a type and an access path into it, return the
/// referenced element type.  If the access path is invalid for the specified
/// type, this returns null.
Type ElementRefDecl::getTypeForPath(Type InTy, llvm::ArrayRef<unsigned> Path) {
  assert(!InTy.isNull() && "getTypeForPath() doesn't allow a null type!");
  
  if (Path.empty())
    return InTy;
  
  TypeBase *Ty = InTy->getDesugaredType();
  
  // If we reach a dependent type, just return it.
  if (llvm::isa<DependentType>(Ty))
    return Ty;
  
  // If we have a single-element oneof (like a struct) then we allow matching
  // the struct elements with the tuple syntax.
  if (OneOfType *OOT = Ty->getAs<OneOfType>())
    if (OOT->hasSingleElement())
      Ty = OOT->getElement(0)->ArgumentType->getDesugaredType();
  
  // Right now, you can only dive into syntactic tuples.  Eventually this should 
  // handle oneof's etc.
  if (TupleType *TT = llvm::dyn_cast<TupleType>(Ty)) {
    // Reject invalid indices.
    if (Path[0] >= TT->Fields.size())
      return 0;
  
    return getTypeForPath(TT->getElementType(Path[0]), Path.slice(1));
  }
  
  return 0;
}


//===----------------------------------------------------------------------===//
//  Decl printing.
//===----------------------------------------------------------------------===//

void Decl::dump() const { print(llvm::errs()); llvm::errs() << '\n'; }

void Decl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  switch (getKind()) {
  case TranslationUnitDeclKind:
    return cast<TranslationUnitDecl>(this)->print(OS, Indent);
  case ImportDeclKind:     return cast<ImportDecl>(this)->print(OS, Indent);
  case TypeAliasDeclKind:  return cast<TypeAliasDecl>(this)->print(OS, Indent);
  case VarDeclKind:        return cast<VarDecl>(this)->print(OS, Indent);
  case FuncDeclKind:       return cast<FuncDecl>(this)->print(OS, Indent);
  case OneOfElementDeclKind:
    return cast<OneOfElementDecl>(this)->print(OS,Indent);
  case ArgDeclKind:        return cast<ArgDecl>(this)->print(OS, Indent);
  case ElementRefDeclKind: return cast<ElementRefDecl>(this)->print(OS, Indent);
  }
}

void TranslationUnitDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(translation_unit\n";
  if (Body)
    Body->print(OS, Indent+2);
  else
    OS.indent(Indent+2) << "(null body!)";
  OS << ')';
}

void ImportDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(import_decl '" << AccessPath[0].first;
  for (unsigned i = 1, e = AccessPath.size(); i != e; ++i)
    OS << "." << AccessPath[i].first;
  OS << "')";
}

static void PrintDeclName(const NamedDecl *D, llvm::raw_ostream &OS) {
  if (D->Name.get())
    OS << '\'' << D->Name << '\'';
  else
    OS << "'anonname=" << (const void*)D << '\'';
}

void NamedDecl::printCommon(llvm::raw_ostream &OS, unsigned Indent) const {
  PrintDeclName(this, OS);
}

void TypeAliasDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(typealias ";
  printCommon(OS, Indent);
  OS << " type='";
  UnderlyingTy->print(OS);
  OS << "')";
}


void ValueDecl::printCommon(llvm::raw_ostream &OS, unsigned Indent) const {
  NamedDecl::printCommon(OS, Indent);
  OS << " type='";
  Ty->print(OS);
  OS << "'";
  
  if (Init) {
    OS << '\n';
    Init->print(OS, Indent+2);
  }
}

void VarDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(vardecl ";
  printCommon(OS, Indent);
  OS << ')';
}

void FuncDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(funcdecl ";
  printCommon(OS, Indent);
  OS << ')';
}

void OneOfElementDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(oneofelementdecl ";
  printCommon(OS, Indent);
  OS << ')';
}

void ArgDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(argdecl ";
  printCommon(OS, Indent);
  OS << ')';
}

void ElementRefDecl::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(elementrefdecl ";
  printCommon(OS, Indent);
  OS << '\n';
  OS.indent(Indent+2);
  OS << "(accesspath ";
  PrintDeclName(VD, OS);
  for (unsigned i = 0, e = AccessPath.size(); i != e; ++i)
    OS << ", " << AccessPath[i];
  
  OS << "))";
}

