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
#include "swift/AST/ASTWalker.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
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
  case DeclContextKind::FileUnit:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractFunctionDecl:
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
  case DeclContextKind::FileUnit:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractFunctionDecl:
    return Type();

  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(this)->getExtendedType();

  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(this)->getDeclaredTypeInContext();
  }
}

Type DeclContext::getSelfTypeInContext(bool isStatic,
                                       bool isConstructor,
                                       GenericParamList **outerGenericParams) {
  if (outerGenericParams)
    *outerGenericParams = nullptr;

  // Determine the type of the container.
  Type containerTy = getDeclaredTypeInContext();
  if (!containerTy)
    return nullptr;

  // For a protocol, the type of 'self' is the parameter type 'Self', not
  // the protocol itself.
  if (auto proto = containerTy->getAs<ProtocolType>()) {
    auto self = proto->getDecl()->getSelf();
    assert(self && "Missing 'Self' type in protocol");
    containerTy = self->getArchetype();
  }

  // Capture the generic parameters, if requested.
  if (outerGenericParams)
    *outerGenericParams = getGenericParamsOfContext();

  // 'static' functions have 'self' of type metatype<T>.
  if (isStatic)
    return MetaTypeType::get(containerTy, getASTContext());

  // Reference types have 'self' of type T.
  //
  // FIXME: Constructor 'self' values are never @inout, which is weird.
  if (containerTy->hasReferenceSemantics() || isConstructor)
    return containerTy;

  // All other types have 'self' of @inout T.
  return LValueType::get(containerTy,
                         LValueType::Qual::DefaultForInOutSelf,
                         getASTContext());
}

GenericParamList *DeclContext::getGenericParamsOfContext() const {
  switch (getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
    case DeclContextKind::TopLevelCodeDecl:
      return nullptr;

    case DeclContextKind::AbstractClosureExpr:
      return nullptr;

    case DeclContextKind::AbstractFunctionDecl: {
      auto *AFD = cast<AbstractFunctionDecl>(this);
      if (auto GP = AFD->getGenericParams())
        return GP;

      return AFD->getDeclContext()->getGenericParamsOfContext();
    }

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

DeclContext *DeclContext::getLocalContext() {
  if (isLocalContext())
    return this;
  if (isModuleContext() || isExtensionContext())
    return nullptr;
  return getParent()->getLocalContext();
}

Module *DeclContext::getParentModule() const {
  const DeclContext *DC = this;
  while (!DC->isModuleContext())
    DC = DC->getParent();
  return const_cast<Module *>(cast<Module>(DC));
}

SourceFile *DeclContext::getParentSourceFile() const {
  const DeclContext *DC = this;
  while (!DC->isModuleScopeContext())
    DC = DC->getParent();
  return const_cast<SourceFile *>(dyn_cast<SourceFile>(DC));
}

DeclContext *DeclContext::getModuleScopeContext() const {
  const DeclContext *DC = this;
  while (true) {
    switch (DC->getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
      return const_cast<DeclContext*>(DC);
    default:
      break;
    }
    DC = DC->getParent();
  }
}

/// Determine whether the given context is generic at any level.
bool DeclContext::isGenericContext() const {
  for (const DeclContext *dc = this; ; dc = dc->getParent() ) {
    switch (dc->getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
      return false;

    case DeclContextKind::AbstractClosureExpr:
      // Check parent context.
      break;

    case DeclContextKind::AbstractFunctionDecl:
      if (cast<AbstractFunctionDecl>(dc)->getGenericParams())
        return true;
      break;

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
  llvm_unreachable("illegal declcontext hierarchy");
}

/// Determine whether the innermost context is generic.
bool DeclContext::isInnermostContextGeneric() const {
  switch (getContextKind()) {
    case DeclContextKind::AbstractFunctionDecl:
      if (cast<AbstractFunctionDecl>(this)->getGenericParams())
        return true;
      return false;

    case DeclContextKind::ExtensionDecl:
    case DeclContextKind::NominalTypeDecl:
      if (getDeclaredTypeOfContext()->getAnyNominal()->getGenericParams())
        return true;
      return false;
  default:
    return false;
  }
}


bool DeclContext::walkContext(ASTWalker &Walker) {
  switch (getContextKind()) {
  case DeclContextKind::Module: {
    Module *Mod = cast<Module>(this);
    if (TranslationUnit *TU = dyn_cast<TranslationUnit>(Mod))
      return TU->walk(Walker);

    SmallVector<Decl *, 64> Decls;
    Mod->getTopLevelDecls(Decls);
    llvm::SaveAndRestore<ASTWalker::ParentTy> SAR(Walker.Parent, Mod);
    for (Decl *D : Decls) {
      if (D->walk(Walker))
        return true;
    }
    return false;
  }
  case DeclContextKind::FileUnit:
    return cast<FileUnit>(this)->walk(Walker);
  case DeclContextKind::AbstractClosureExpr:
    return cast<AbstractClosureExpr>(this)->walk(Walker);
  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(this)->walk(Walker);
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(this)->walk(Walker);
  case DeclContextKind::TopLevelCodeDecl:
    return cast<TopLevelCodeDecl>(this)->walk(Walker);
  case DeclContextKind::AbstractFunctionDecl:
    return cast<AbstractFunctionDecl>(this)->walk(Walker);
  }
}

void DeclContext::dumpContext() const {
  printContext(llvm::outs());
}

template <typename DCType>
static unsigned getLineNumber(DCType *DC) {
  SourceLoc loc = DC->getLoc();
  const ASTContext &ctx = static_cast<const DeclContext *>(DC)->getASTContext();
  return ctx.SourceMgr.getLineAndColumn(loc).first;
}

unsigned DeclContext::printContext(raw_ostream &OS) const {
  unsigned Depth = 0;
  if (auto *P = getParent())
    Depth = P->printContext(OS);

  const char *Kind;
  switch (getContextKind()) {
  case DeclContextKind::Module:           Kind = "Module"; break;
  case DeclContextKind::FileUnit:         Kind = "FileUnit"; break;
  case DeclContextKind::AbstractClosureExpr:
    Kind = "AbstractClosureExpr";
    break;
  case DeclContextKind::NominalTypeDecl:  Kind = "NominalTypeDecl"; break;
  case DeclContextKind::ExtensionDecl:    Kind = "ExtensionDecl"; break;
  case DeclContextKind::TopLevelCodeDecl: Kind = "TopLevelCodeDecl"; break;
  case DeclContextKind::AbstractFunctionDecl:
    Kind = "AbstractFunctionDecl";
    break;
  }
  OS.indent(Depth*2) << "0x" << (void*)this << " " << Kind;

  switch (getContextKind()) {
  case DeclContextKind::Module:
    OS << " name=" << cast<Module>(this)->Name;
    break;
  case DeclContextKind::FileUnit:
    switch (cast<FileUnit>(this)->getKind()) {
    case FileUnitKind::Builtin:
      OS << " Builtin";
      break;
    case FileUnitKind::Source:
      OS << " file=\"" << cast<SourceFile>(this)->getFilename() << "\"";
      break;
    case FileUnitKind::Loaded:
      OS << " file=\"" << cast<LoadedFile>(this)->getFilename() << "\"";
    }
    break;
  case DeclContextKind::AbstractClosureExpr:
    OS << " line=" << getLineNumber(cast<AbstractClosureExpr>(this));
    OS << " : " << cast<AbstractClosureExpr>(this)->getType();
    break;
  case DeclContextKind::NominalTypeDecl:
    OS << " name=" << cast<NominalTypeDecl>(this)->getName();
    break;
  case DeclContextKind::ExtensionDecl:
    OS << " line=" << getLineNumber(cast<ExtensionDecl>(this));
    OS << " base=" << cast<ExtensionDecl>(this)->getExtendedType();
    break;
  case DeclContextKind::TopLevelCodeDecl:
    OS << " line=" << getLineNumber(cast<TopLevelCodeDecl>(this));
    break;
  case DeclContextKind::AbstractFunctionDecl:
    OS << " name=" << cast<AbstractFunctionDecl>(this)->getName();
    OS << " : " << cast<AbstractClosureExpr>(this)->getType();
    break;
  }

  OS << "\n";
  return Depth + 1;
}

