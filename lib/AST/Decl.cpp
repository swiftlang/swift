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
#include "swift/AST/AST.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

/// getASTContext - Return the ASTContext for a specified DeclContetx by
/// walking up to the translation unit and returning its ASTContext.
ASTContext &DeclContext::getASTContext() {
  if (Module *M = dyn_cast<Module>(this))
    return M->Ctx;
  
  return getParent()->getASTContext();
}

Type DeclContext::getDeclaredTypeOfContext() const {
  switch (getContextKind()) {
  case DeclContextKind::BuiltinModule:
  case DeclContextKind::CapturingExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::TranslationUnit:
  case DeclContextKind::ConstructorDecl:
    return Type();
    
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(this)->getExtendedType();
    
  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(this)->getDeclaredType();
  }
}

// Only allow allocation of Decls using the allocator in ASTContext.
void *Decl::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

// Only allow allocation of Modules using the allocator in ASTContext.
void *Module::operator new(size_t Bytes, ASTContext &C,
                           unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

// Helper functions to verify statically whether source-location
// functions have been overridden.
typedef const char (&TwoChars)[2];
template<typename Class> 
inline char checkSourceLocType(SourceLoc (Class::*)() const);
inline TwoChars checkSourceLocType(SourceLoc (Decl::*)() const);


SourceLoc Decl::getStartLoc() const {
  switch (getKind()) {
#define DECL(ID, PARENT) \
static_assert(sizeof(checkSourceLocType(&ID##Decl::getStartLoc)) == 1, \
              #ID "Decl is missing getStartLoc()"); \
case DeclKind::ID: return cast<ID##Decl>(this)->getStartLoc();
#include "swift/AST/DeclNodes.def"
  }

  llvm_unreachable("Unknown decl kind");
}

SourceLoc Decl::getLoc() const {
  switch (getKind()) {
#define DECL(ID, X) \
static_assert(sizeof(checkSourceLocType(&ID##Decl::getLoc)) == 1, \
              #ID "Decl is missing getLoc()"); \
case DeclKind::ID: return cast<ID##Decl>(this)->getLoc();
#include "swift/AST/DeclNodes.def"
  }

  llvm_unreachable("Unknown decl kind");
}

/// getAliasType - Return the sugared version of this decl as a Type.
NameAliasType *TypeAliasDecl::getAliasType() const {
  // Lazily create AliasTy. 
  if (AliasTy == 0)
    AliasTy = new (getASTContext()) NameAliasType(
                                            const_cast<TypeAliasDecl*>(this));
   
  return AliasTy;
}

ImportDecl *ImportDecl::create(ASTContext &Ctx, DeclContext *DC,
                               SourceLoc ImportLoc,
                               ArrayRef<AccessPathElement> Path) {
  void *buffer = Ctx.Allocate(sizeof(ImportDecl) +
                              Path.size() * sizeof(AccessPathElement),
                              Decl::Alignment);
  return new (buffer) ImportDecl(DC, ImportLoc, Path);
}

ImportDecl::ImportDecl(DeclContext *DC, SourceLoc ImportLoc,
                       ArrayRef<AccessPathElement> Path)
  : Decl(DeclKind::Import, DC), ImportLoc(ImportLoc),
    NumPathElements(Path.size()) {
  memcpy(getPathBuffer(), Path.data(), Path.size() * sizeof(AccessPathElement));
}

SourceLoc TopLevelCodeDecl::getStartLoc() const {
  if (Body.is<Expr*>())
    return Body.get<Expr*>()->getStartLoc();
  return Body.get<Stmt*>()->getStartLoc();
}

/// getTypeOfReference - Return the full type judgement for a non-member
/// reference to this value.
Type ValueDecl::getTypeOfReference() const {
  if (isReferencedAsLValue()) {
    if (LValueType *LVT = Ty->getAs<LValueType>())
      return LValueType::get(LVT->getObjectType(),
                             LValueType::Qual::DefaultForVar, getASTContext());
    return LValueType::get(Ty, LValueType::Qual::DefaultForVar, getASTContext());
  } else {
    return Ty;
  }
}

/// isDefinition - Return true if this is a definition of a decl, not a
/// forward declaration (e.g. of a function) that is implemented outside of
/// the swift code.
bool ValueDecl::isDefinition() const {
  switch (getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::Subscript:
  case DeclKind::TopLevelCode:
  case DeclKind::Constructor:
    llvm_unreachable("non-value decls shouldn't get here");
      
  case DeclKind::Func:
    return cast<FuncDecl>(this)->getBody() != 0;

  case DeclKind::Var:
  case DeclKind::OneOf:
  case DeclKind::OneOfElement:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::TypeAlias:
  case DeclKind::Protocol:
    return true;
  }
}

bool ValueDecl::isInstanceMember() const {
  DeclContext *DC = getDeclContext();
  if (!DC->isTypeContext())
    return false;
  
  // Variables in oneofs/extensions/protocols are instance members.
  // FIXME: If we ever end up with static variables, we'll have to check for
  // them here.
  if (isa<VarDecl>(this))
    return true;
  
  // Non-static methods are instance members.
  if (const FuncDecl *Func = dyn_cast<FuncDecl>(this))
    return !Func->isStatic();
  
  return false;
}

bool ValueDecl::needsCapture() const {
  // We don't need to capture anything from non-local contexts.
  if (!getDeclContext()->isLocalContext())
    return false;
  // We don't need to capture types.
  if (isa<TypeDecl>(this))
    return false;
  return true;
}

Type TypeDecl::getDeclaredType() const {
  if (auto TAD = dyn_cast<TypeAliasDecl>(this))
    return TAD->getAliasType();
  if (auto SD = dyn_cast<StructDecl>(this))
    return SD->getDeclaredType();
  if (auto PD = dyn_cast<ProtocolDecl>(this))
    return PD->getDeclaredType();
  if (auto CD = dyn_cast<ClassDecl>(this))
    return CD->getDeclaredType();
  assert(isa<OneOfDecl>(this));
  return cast<OneOfDecl>(this)->getDeclaredType();
}

TypeAliasDecl::TypeAliasDecl(SourceLoc TypeAliasLoc, Identifier Name,
                             SourceLoc NameLoc, Type Underlyingty,
                             DeclContext *DC, MutableArrayRef<Type> Inherited)
  : TypeDecl(DeclKind::TypeAlias, DC, Name, Inherited, Type()),
    AliasTy(0), TypeAliasLoc(TypeAliasLoc), NameLoc(NameLoc),
    UnderlyingTy(Underlyingty)
{
  // Set the type of the TypeAlias to the right MetaTypeType.
  // FIXME: Is this the right thing to do?
  setType(MetaTypeType::get(this));
}

OneOfDecl::OneOfDecl(SourceLoc OneOfLoc, Identifier Name, SourceLoc NameLoc,
                     MutableArrayRef<Type> Inherited, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::OneOf, Parent, Name, Inherited, Type()),
    OneOfLoc(OneOfLoc), NameLoc(NameLoc) {
  // Set the type of the OneOfDecl to the right MetaTypeType.
  setType(MetaTypeType::get(this));
  // Compute the associated type for this OneOfDecl.
  OneOfTy = new (Parent->getASTContext()) OneOfType(this, Parent->getASTContext());
}

StructDecl::StructDecl(SourceLoc StructLoc, Identifier Name, SourceLoc NameLoc,
                       MutableArrayRef<Type> Inherited, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Struct, Parent, Name, Inherited, Type()),
    StructLoc(StructLoc), NameLoc(NameLoc) {
  // Set the type of the OneOfDecl to the right MetaTypeType.
  setType(MetaTypeType::get(this));
  // Compute the associated type for this StructDecl.
  StructTy = new (Parent->getASTContext()) StructType(this, Parent->getASTContext());
}

ClassDecl::ClassDecl(SourceLoc ClassLoc, Identifier Name, SourceLoc NameLoc,
                     MutableArrayRef<Type> Inherited, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Class, Parent, Name, Inherited, Type()),
    ClassLoc(ClassLoc), NameLoc(NameLoc) {
  // Set the type of the OneOfDecl to the right MetaTypeType.
  setType(MetaTypeType::get(this));
  // Compute the associated type for this ClassDecl.
  ClassTy = new (Parent->getASTContext()) ClassType(this, Parent->getASTContext());
}


OneOfElementDecl *OneOfDecl::getElement(Identifier Name) const {
  // FIXME: Linear search is not great for large oneof decls.
  for (Decl *D : getMembers())
    if (OneOfElementDecl *Elt = dyn_cast<OneOfElementDecl>(D))
      if (Elt->getName() == Name)
        return Elt;
  return 0;
}

bool ProtocolDecl::inheritsFrom(const ProtocolDecl *Super) const {
  if (this == Super)
    return false;
  
  llvm::SmallPtrSet<const ProtocolDecl *, 4> Visited;
  SmallVector<const ProtocolDecl *, 4> Stack;
  
  Stack.push_back(this);
  Visited.insert(this);
  while (!Stack.empty()) {
    const ProtocolDecl *Current = Stack.back();
    Stack.pop_back();
    
    for (auto Inherited : Current->getInherited()) {
      SmallVector<ProtocolDecl *, 4> InheritedDecls;
      if (Inherited->isExistentialType(InheritedDecls)) {
        for (auto InheritedProto : InheritedDecls) {
          if (InheritedProto == Super)
            return true;
            
          else if (Visited.insert(InheritedProto))
            Stack.push_back(InheritedProto);
        }
      }
    }
  }
  
  return false;
}

void ProtocolDecl::collectInherited(
       llvm::SmallPtrSet<ProtocolDecl *, 4> &Inherited) {
  SmallVector<const ProtocolDecl *, 4> Stack;
  
  Stack.push_back(this);
  while (!Stack.empty()) {
    const ProtocolDecl *Current = Stack.back();
    Stack.pop_back();
    
    for (auto IType : Current->getInherited()) {
      SmallVector<ProtocolDecl *, 4> InheritedDecls;
      if (IType->isExistentialType(InheritedDecls)) {
        for (auto InheritedProto : InheritedDecls) {
          if (Inherited.insert(InheritedProto))
            Stack.push_back(InheritedProto);
        }
      }
    }
  }
}

void VarDecl::setProperty(ASTContext &Context, SourceLoc LBraceLoc,
                          FuncDecl *Get, FuncDecl *Set, SourceLoc RBraceLoc) {
  assert(!GetSet && "Variable is already a property?");
  void *Mem = Context.Allocate(sizeof(GetSetRecord), alignof(GetSetRecord));
  GetSet = new (Mem) GetSetRecord;
  GetSet->Braces = SourceRange(LBraceLoc, RBraceLoc);
  GetSet->Get = Get;
  GetSet->Set = Set;
  
  if (Get)
    Get->makeGetter(this);
  if (Set)
    Set->makeSetter(this);
}

/// getExtensionType - If this is a method in a type extension for some type,
/// return that type, otherwise return Type().
Type FuncDecl::getExtensionType() const {
  DeclContext *DC = getDeclContext();
  switch (DC->getContextKind()) {
  case DeclContextKind::TranslationUnit:
  case DeclContextKind::BuiltinModule:
  case DeclContextKind::CapturingExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::ConstructorDecl:
    return Type();

  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(DC)->getDeclaredType();
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(DC)->getExtendedType();
  }
  llvm_unreachable("bad context kind");
}


/// computeThisType - If this is a method in a type extension for some type,
/// compute and return the type to be used for the 'this' argument of the
/// type (which varies based on whether the extended type is a reference type
/// or not), or an empty Type() if no 'this' argument should exist.  This can
/// only be used after name binding has resolved types.
Type FuncDecl::computeThisType() const {
  // 'static' functions have no 'this' argument.
  if (isStatic()) return Type();
  
  Type ContainerType = getExtensionType();
  if (ContainerType.isNull()) return ContainerType;
  
  
  if (ContainerType->hasReferenceSemantics())
    return ContainerType;
  
  // 'this' is accepts implicit l-values and doesn't force them to the heap.
  return LValueType::get(ContainerType, LValueType::Qual::NonHeap,
                         getASTContext());
}

/// getImplicitThisDecl - If this FuncDecl is a non-static method in an
/// extension context, it will have a 'this' argument.  This method returns it
/// if present, or returns null if not.
VarDecl *FuncDecl::getImplicitThisDecl() {
  if (isStatic()) return 0;
  
  if (Body->getParamPatterns().empty()) return 0;
  
  // "this" is represented as (typed_pattern (named_pattern (var_decl 'this')).
  TypedPattern *TP = dyn_cast<TypedPattern>(Body->getParamPatterns()[0]);
  if (TP == 0) return 0;
  
  // The decl should be named 'this' and have no location information.
  NamedPattern *NP = dyn_cast<NamedPattern>(TP->getSubPattern());
  if (NP && NP->getBoundName().str() == "this" && !NP->getLoc().isValid())
    return NP->getDecl();
  return 0;
}

SourceLoc SubscriptDecl::getLoc() const {
  return Indices->getStartLoc();
}

SourceLoc ConstructorDecl::getLoc() const {
  return Arguments->getStartLoc();
}

Type ConstructorDecl::computeThisType() const {
  Type ContainerType = getDeclContext()->getDeclaredTypeOfContext();

  if (ContainerType->hasReferenceSemantics())
    return ContainerType;
  
  // 'this' is accepts implicit l-values and doesn't force them to the heap.
  return LValueType::get(ContainerType, LValueType::Qual::NonHeap,
                         getParent()->getASTContext());
}

Type ConstructorDecl::getArgumentType() const {
  Type ArgTy = getType();
  ArgTy = ArgTy->castTo<FunctionType>()->getResult();
  ArgTy = ArgTy->castTo<FunctionType>()->getInput();
  return ArgTy;
}


//===----------------------------------------------------------------------===//
//  Decl printing.
//===----------------------------------------------------------------------===//

namespace {
  class PrintPattern : public PatternVisitor<PrintPattern> {
  public:
    raw_ostream &OS;
    PrintPattern(raw_ostream &os) : OS(os) {}

    void visitParenPattern(ParenPattern *P) {
      OS << '(';
      visit(P->getSubPattern());
      OS << ')';
    }
    void visitTuplePattern(TuplePattern *P) {
      OS << '(';
      for (unsigned i = 0, e = P->getNumFields(); i != e; ++i) {
        visit(P->getFields()[i].getPattern());
        if (i + 1 != e)
          OS << ", ";
      }
      OS << ')';
    }
    void visitNamedPattern(NamedPattern *P) {
      OS << P->getBoundName().str();
    }
    void visitAnyPattern(AnyPattern *P) {
      OS << '_';
    }
    void visitTypedPattern(TypedPattern *P) {
      visit(P->getSubPattern());
      OS << " : ";
      P->getType()->print(OS);
    }
  };

  /// PrintDecl - Visitor implementation of Decl::print.
  class PrintDecl : public DeclVisitor<PrintDecl> {
  public:
    raw_ostream &OS;
    unsigned Indent;
    
    PrintDecl(raw_ostream &os, unsigned indent) : OS(os), Indent(indent) {
    }
    
    void printRec(Decl *D) { D->print(OS, Indent+2); }
    void printRec(Expr *E) { E->print(OS, Indent+2); }
    void printRec(Stmt *S) { S->print(OS, Indent+2); }

    void printCommon(Decl *D, const char *Name) {
      OS.indent(Indent) << "(" << Name;
    }

    void printInherited(ArrayRef<Type> Inherited) {
      if (Inherited.empty())
        return;
      OS << "inherits: ";
      bool First = true;
      for (auto Super : Inherited) {
        if (First)
          First = false;
        else
          OS << ", ";
        
        Super->print(OS);
      }
    }
    
    void visitImportDecl(ImportDecl *ID) {
      printCommon(ID, "import_decl");
      OS << " '" << ID->getAccessPath()[0].first;
      for (unsigned i = 1, e = ID->getAccessPath().size(); i != e; ++i)
        OS << "." << ID->getAccessPath()[i].first;
      OS << "')";
    }

    void visitExtensionDecl(ExtensionDecl *ED) {
      printCommon(ED, "extension_decl");
      OS << ' ';
      ED->getExtendedType()->print(OS);
      printInherited(ED->getInherited());
      for (Decl *Member : ED->getMembers()) {
        OS << '\n';
        printRec(Member);
      }
      OS << ")";
    }

    void printDeclName(ValueDecl *D) {
      if (D->getName().get())
        OS << '\'' << D->getName() << '\'';
      else
        OS << "'anonname=" << (const void*)D << '\'';
    }
    
    void visitTypeAliasDecl(TypeAliasDecl *TAD) {
      printCommon(TAD, "typealias");
      OS << " type='";
      TAD->getUnderlyingType()->print(OS);
      printInherited(TAD->getInherited());
      OS << "')";
    }

    void visitProtocolDecl(ProtocolDecl *PD) {
      printCommon(PD, "protocol");
      printInherited(PD->getInherited());
      for (auto VD : PD->getMembers()) {
        OS << '\n';
        printRec(VD);
      }
      OS << ")";
    }
    
    void printCommon(ValueDecl *VD, const char *Name) {
      printCommon((Decl*)VD, Name);
      OS << ' ';
      printDeclName(VD);
      OS << " type='";
      if (VD->hasType())
        VD->getType()->print(OS);
      else
        OS << "<null type>";
      OS << '\'';

      if (VD->hasFixedLifetime()) OS << " hasFixedLifetime=true";
      if (VD->isNeverUsedAsLValue()) OS << " neverUsedAsLValue=true";
    }

    void visitTranslationUnit(const TranslationUnit *TU) {
      OS.indent(Indent) << "(translation_unit";
      for (Decl *D : TU->Decls) {
        OS << '\n';
        printRec(D);
      }
      OS << ')';
    }

    void visitVarDecl(VarDecl *VD) {
      printCommon(VD, "var_decl");
      if (VD->isProperty()) {
        if (FuncDecl *Get = VD->getGetter()) {
          OS << "\n";
          OS.indent(Indent + 2);
          OS << "get = ";
          printRec(Get);
        }
        if (FuncDecl *Set = VD->getSetter()) {
          OS << "\n";
          OS.indent(Indent + 2);
          OS << "set = ";
          printRec(Set);
        }
      }
      OS << ')';
    }
    
    void visitFuncDecl(FuncDecl *FD) {
      printCommon(FD, "func_decl");
      if (FuncExpr *E = FD->getBody()) {
        OS << '\n';
        printRec(E);
      }
      OS << ')';
    }

    void visitOneOfDecl(OneOfDecl *OOD) {
      printCommon(OOD, "oneof_decl");
      printInherited(OOD->getInherited());
      for (Decl *D : OOD->getMembers()) {
        OS << '\n';
        printRec(D);
      }
      OS << ')';
    }

    void visitOneOfElementDecl(OneOfElementDecl *OOED) {
      printCommon(OOED, "oneof_element_decl");
      OS << ')';
    }

    void visitStructDecl(StructDecl *SD) {
      printCommon(SD, "struct_decl");
      printInherited(SD->getInherited());
      for (Decl *D : SD->getMembers()) {
        OS << '\n';
        printRec(D);
      }
      OS << "')";
    }

    void visitClassDecl(ClassDecl *CD) {
      printCommon(CD, "class_decl");
      printInherited(CD->getInherited());
      for (Decl *D : CD->getMembers()) {
        OS << '\n';
        printRec(D);
      }
      OS << "')";
    }

    void visitPatternBindingDecl(PatternBindingDecl *PBD) {
      printCommon(PBD, "pattern_binding_decl");
      OS << " pattern='";
      PrintPattern(OS).visit(PBD->getPattern());
      OS << '\'';
      if (PBD->getInit()) {
        OS << '\n';
        printRec(PBD->getInit());
      }
      OS << ')';
    }

    void visitSubscriptDecl(SubscriptDecl *SD) {
      printCommon(SD, "subscript_decl");
      if (FuncDecl *Get = SD->getGetter()) {
        OS << "\n";
        OS.indent(Indent + 2);
        OS << "get = ";
        printRec(Get);
      }
      if (FuncDecl *Set = SD->getSetter()) {
        OS << "\n";
        OS.indent(Indent + 2);
        OS << "set = ";
        printRec(Set);
      }
      OS << ')';
    }

    void visitConstructorDecl(ConstructorDecl *CD) {
      printCommon(CD, "constructor_decl");
      OS << '\n';
      printRec(CD->getBody());
      OS << ')';
    }

    void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
      printCommon(TLCD, "top_level_code_decl");
      auto Body = TLCD->getBody();
      if (!Body.isNull()) {
        OS << "\n";
        if (Body.is<Expr*>())
          printRec(Body.get<Expr*>());
        else
          printRec(Body.get<Stmt*>());
      }
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
