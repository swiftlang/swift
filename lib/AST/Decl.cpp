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
#include "swift/AST/Module.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Types.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

/// getASTContext - Return the ASTContext for a specified DeclContetx by
/// walking up to the translation unit and returning its ASTContext.
ASTContext &DeclContext::getASTContext() {
  if (Module *M = dyn_cast<Module>(this))
    return M->Ctx;
  
  return getParent()->getASTContext();
}

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

// Only allow allocation of Modules using the allocator in ASTContext.
void *Module::operator new(size_t Bytes, ASTContext &C,
                           unsigned Alignment) throw() {
  return C.Allocate(Bytes, Alignment);
}


SourceLoc Decl::getLocStart() const {
  switch (Kind) {
  case DeclKind::Import: return cast<ImportDecl>(this)->getLocStart();
  case DeclKind::TypeAlias:  return cast<TypeAliasDecl>(this)->getLocStart(); 
  case DeclKind::Var:        return cast<VarDecl>(this)->getLocStart();
  case DeclKind::Func:       return cast<FuncDecl>(this)->getLocStart();
  case DeclKind::OneOfElement:
    return cast<OneOfElementDecl>(this)->getLocStart();
  case DeclKind::Arg:        return cast<ArgDecl>(this)->getLocStart();
  case DeclKind::ElementRef: return cast<ElementRefDecl>(this)->getLocStart();
  }

  assert(0 && "Unknown decl kind");
  return SourceLoc();
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
Type ElementRefDecl::getTypeForPath(Type InTy, ArrayRef<unsigned> Path) {
  assert(!InTy.isNull() && "getTypeForPath() doesn't allow a null type!");
  
  if (Path.empty())
    return InTy;
  
  TypeBase *Ty = InTy->getDesugaredType();
  
  // If we reach a dependent type, just return it.
  if (isa<DependentType>(Ty))
    return Ty;
  
  // If we have a single-element oneof (like a struct) then we allow matching
  // the struct elements with the tuple syntax.
  if (OneOfType *OOT = Ty->getAs<OneOfType>())
    if (OOT->hasSingleElement())
      Ty = OOT->getElement(0)->ArgumentType->getDesugaredType();
  
  // Right now, you can only dive into syntactic tuples.  Eventually this should 
  // handle oneof's etc.
  if (TupleType *TT = dyn_cast<TupleType>(Ty)) {
    // Reject invalid indices.
    if (Path[0] >= TT->Fields.size())
      return 0;
  
    return getTypeForPath(TT->getElementType(Path[0]), Path.slice(1));
  }
  
  return 0;
}

/// getTypeJudgement - Return the full type judgement for a non-member
/// reference to this value.
TypeJudgement ValueDecl::getTypeJudgement() const {
  switch (Kind) {
  case DeclKind::Import:
  case DeclKind::TypeAlias:
    llvm_unreachable("non-value decls don't have type judgements");

  case DeclKind::Var:
  case DeclKind::Arg:
  case DeclKind::ElementRef:
    return TypeJudgement(Ty, ValueKind::LValue);

  case DeclKind::Func:
  case DeclKind::OneOfElement:
    return TypeJudgement(Ty, ValueKind::RValue);
  }
}


//===----------------------------------------------------------------------===//
//  Decl printing.
//===----------------------------------------------------------------------===//

namespace {
  /// PrintDecl - Visitor implementation of Decl::print.
  class PrintDecl : public DeclVisitor<PrintDecl> {
  public:
    raw_ostream &OS;
    unsigned Indent;
    
    PrintDecl(raw_ostream &os, unsigned indent) : OS(os), Indent(indent) {
    }
    
    void printRec(Expr *E) { E->print(OS, Indent+2); }
    void printRec(Stmt *S) { S->print(OS, Indent+2); }

    void printCommon(Decl *D, const char *Name) {
      OS.indent(Indent) << "(" << Name;
    }

    void visitImportDecl(ImportDecl *ID) {
      printCommon(ID, "import_decl");
      OS << " '" << ID->AccessPath[0].first;
      for (unsigned i = 1, e = ID->AccessPath.size(); i != e; ++i)
        OS << "." << ID->AccessPath[i].first;
      OS << "')";
    }

    void printDeclName(NamedDecl *D) {
      if (D->Name.get())
        OS << '\'' << D->Name << '\'';
      else
        OS << "'anonname=" << (const void*)D << '\'';
    }
    
    void printCommon(NamedDecl *D, const char *Name) {
      printCommon((Decl*)D, Name);
      OS << ' ';
      printDeclName(D);
    }
    
    void visitTypeAliasDecl(TypeAliasDecl *TAD) {
      printCommon(TAD, "typealias");
      OS << " type='";
      TAD->UnderlyingTy->print(OS);
      OS << "')";
    }

    void printCommon(ValueDecl *VD, const char *Name) {
      printCommon((NamedDecl*)VD, Name);
      OS << " type='";
      VD->Ty->print(OS);
      OS << "'";
      
      if (VD->Init) {
        OS << '\n';
        printRec(VD->Init);
      }
    }


    void visitTranslationUnit(const TranslationUnit *TU) {
      OS.indent(Indent) << "(translation_unit\n";
      if (TU->Body)
        printRec(TU->Body);
      else
        OS.indent(Indent+2) << "(null body!)";
      OS << ')';
    }
    
    
    void visitVarDecl(VarDecl *VD) {
      printCommon(VD, "var_decl");
      OS << ')';
    }
    
    void visitFuncDecl(FuncDecl *FD) {
      printCommon(FD, "func_decl");
      OS << ')';
    }
    
    void visitOneOfElementDecl(OneOfElementDecl *OOED) {
      printCommon(OOED, "oneof_element_decl");
      OS << ')';
    }
    
    void visitArgDecl(ArgDecl *AD) {
      printCommon(AD, "arg_decl");
      OS << ')';
    }

    void visitElementRefDecl(ElementRefDecl *ERD) {
      printCommon(ERD, "element_ref_decl");
      OS << '\n';
      OS.indent(Indent+2);
      OS << "(accesspath ";
      printDeclName(ERD->VD);
      for (unsigned i = 0, e = ERD->AccessPath.size(); i != e; ++i)
        OS << ", " << ERD->AccessPath[i];
      
      OS << "))";
    }
  };
} // end anonymous namespace.



void Decl::print(raw_ostream &OS, unsigned Indent) const {
  PrintDecl(OS, Indent).visit(const_cast<Decl*>(this));
}

void Decl::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void TranslationUnit::dump() const {
  PrintDecl(llvm::errs(), 0).visitTranslationUnit(this);
  llvm::errs() << '\n';
}
