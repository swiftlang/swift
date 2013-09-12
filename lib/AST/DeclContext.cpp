//===--- DeclContext.cpp - DeclContext implementation ---------------------===//
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

#include "swift/AST/DeclContext.h"
#include "swift/AST/AST.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

// Only allow allocation of DeclContext using the allocator in ASTContext.
void *DeclContext::operator new(size_t Bytes, ASTContext &C,
                                unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

/// getASTContext - Return the ASTContext for a specified DeclContetx by
/// walking up to the translation unit and returning its ASTContext.
ASTContext &DeclContext::getASTContext() {
  if (Module *M = dyn_cast<Module>(this))
    return M->Ctx;
  
  return getParent()->getASTContext();
}

Type DeclContext::getDeclaredTypeOfContext() const {
  switch (getContextKind()) {
  case DeclContextKind::Module:
  case DeclContextKind::FuncExpr:
  case DeclContextKind::PipeClosureExpr:
  case DeclContextKind::ClosureExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::ConstructorDecl:
  case DeclContextKind::DestructorDecl:
    return Type();
    
  case DeclContextKind::ExtensionDecl: {
    auto type = cast<ExtensionDecl>(this)->getExtendedType();
    return type->getNominalOrBoundGenericNominal()->getDeclaredType();
  }
    
  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(this)->getDeclaredType();
  }
}

Type DeclContext::getDeclaredTypeInContext() {
  switch (getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::FuncExpr:
    case DeclContextKind::PipeClosureExpr:
    case DeclContextKind::ClosureExpr:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::ConstructorDecl:
    case DeclContextKind::DestructorDecl:
      return Type();

    case DeclContextKind::ExtensionDecl:
      return cast<ExtensionDecl>(this)->getExtendedType();
    case DeclContextKind::NominalTypeDecl:
      return cast<NominalTypeDecl>(this)->getDeclaredTypeInContext();
  }

}

GenericParamList *DeclContext::getGenericParamsOfContext() const {
  switch (getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::TopLevelCodeDecl:
      return nullptr;

    case DeclContextKind::FuncExpr: {
      auto *FD = cast<FuncExpr>(this)->getDecl();
      if (auto GP = FD->getGenericParams())
        return GP;

      return FD->getDeclContext()->getGenericParamsOfContext();
    }

    case DeclContextKind::PipeClosureExpr:
    case DeclContextKind::ClosureExpr:
      return nullptr;

    case DeclContextKind::ConstructorDecl: {
      auto constructor = cast<ConstructorDecl>(this);
      if (auto gp = constructor->getGenericParams())
        return gp;

      return constructor->getDeclContext()->getGenericParamsOfContext();
    }

    case DeclContextKind::DestructorDecl:
      return cast<DestructorDecl>(this)->getDeclContext()
               ->getGenericParamsOfContext();

    case DeclContextKind::NominalTypeDecl: {
      auto nominal = cast<NominalTypeDecl>(this);
      if (auto gp = nominal->getGenericParams())
        return gp;

      return nominal->getDeclContext()->getGenericParamsOfContext();
    }

    case DeclContextKind::ExtensionDecl: {
      auto extension = cast<ExtensionDecl>(this);
      auto extendedType = extension->getExtendedType();
      if (auto bound = extendedType->getAs<BoundGenericType>()) {
        return bound->getDecl()->getGenericParams();
      }
      if (auto nominalTy = extendedType->getAs<NominalType>()) {
        auto nominalDecl = nominalTy->getDecl();
        return nominalDecl->getDeclContext()->getGenericParamsOfContext();
      }
      return nullptr;
    }
  }

  llvm_unreachable("Unhandled declaration context kind");
}

Module *DeclContext::getParentModule() const {
  const DeclContext *DC = this;
  while (!DC->isModuleContext())
    DC = DC->getParent();
  return const_cast<Module *>(cast<Module>(DC));
}

/// Determine whether the given context is generic at any level.
bool DeclContext::isGenericContext() const {
  for (const DeclContext *dc = this; ; dc = dc->getParent() ) {
    switch (dc->getContextKind()) {
    case DeclContextKind::Module:
      return false;

    case DeclContextKind::FuncExpr:
      if (cast<FuncExpr>(dc)->getDecl()->getGenericParams())
        return true;
      break;

    case DeclContextKind::PipeClosureExpr:
    case DeclContextKind::ClosureExpr:
      // Check parent context.
      break;

    case DeclContextKind::ConstructorDecl:
      if (cast<ConstructorDecl>(dc)->getGenericParams())
        return true;
      break;

    case DeclContextKind::DestructorDecl:
    case DeclContextKind::TopLevelCodeDecl:
      // Check parent context.
      break;

    case DeclContextKind::ExtensionDecl:
    case DeclContextKind::NominalTypeDecl:
      if (dc->getDeclaredTypeOfContext()->getAnyNominal()->getGenericParams())
        return true;
      break;
    }
  }
}

void DeclContext::dumpContext() const {
  printContext(llvm::outs());
}
unsigned DeclContext::printContext(raw_ostream &OS) const {
  unsigned Depth = 0;
  if (auto *P = getParent())
    Depth = P->printContext(OS);

  const char *Kind;
  switch (getContextKind()) {
  case DeclContextKind::Module:           Kind = "Module"; break;
  case DeclContextKind::FuncExpr:         Kind = "FuncExpr"; break;
  case DeclContextKind::PipeClosureExpr:  Kind = "PipeClosureExpr"; break;
  case DeclContextKind::ClosureExpr:      Kind = "ClosureExpr"; break;
  case DeclContextKind::NominalTypeDecl:  Kind = "NominalTypeDecl"; break;
  case DeclContextKind::ExtensionDecl:    Kind = "ExtensionDecl"; break;
  case DeclContextKind::TopLevelCodeDecl: Kind = "TopLevelCodeDecl"; break;
  case DeclContextKind::ConstructorDecl:  Kind = "ConstructorDecl"; break;
  case DeclContextKind::DestructorDecl:   Kind = "DestructorDecl"; break;
  }
  OS.indent(Depth*2) << "0x" << (void*)this << " " << Kind;

  if (auto *FE = dyn_cast<FuncExpr>(this)) {
    auto *FD = FE->getDecl();
    OS << " FuncExpr=" << FD->getName()
       << ": " << FD->getType().getString();
  }
  if (auto *CE = dyn_cast<PipeClosureExpr>(this)) {
    OS << ": " << CE->getType().getString();
  }
  if (auto *CE = dyn_cast<ClosureExpr>(this)) {
    OS << ": " << CE->getType().getString();
  }

  if (auto *NTD = dyn_cast<NominalTypeDecl>(this))
    OS << " decl=" << NTD->getName();

  OS << "\n";
  return Depth + 1;
}

