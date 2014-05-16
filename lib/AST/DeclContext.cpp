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

#include "swift/AST/AST.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;

#define DEBUG_TYPE "Name lookup"

STATISTIC(NumLazyIterableDeclContexts,
          "# of serialized iterable declaration contexts");
STATISTIC(NumUnloadedLazyIterableDeclContexts,
          "# of serialized iterable declaration contexts never loaded");

CanType DeclContext::getExtendedType(const ExtensionDecl *ED) {
  CanType ExtendedTy = ED->getExtendedType()->getCanonicalType();
  
  // FIXME: we should require generic parameter clauses here
  if (auto unbound = dyn_cast<UnboundGenericType>(ExtendedTy)) {
    auto boundType = unbound->getDecl()->getDeclaredTypeInContext();
    auto mutableED = const_cast<ExtensionDecl *>(ED);
    mutableED->getExtendedTypeLoc().setType(boundType, true);
    ExtendedTy = boundType->getCanonicalType();
  }
  return ExtendedTy;
}

// Only allow allocation of DeclContext using the allocator in ASTContext.
void *DeclContext::operator new(size_t Bytes, ASTContext &C,
                                unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

ASTContext &DeclContext::getASTContext() const {
  return getParentModule()->Ctx;
}

/// If this DeclContext is a class, or an extension on a class, return the
/// ClassDecl, otherwise return null.
ClassDecl *DeclContext::isClassOrClassExtensionContext() const {
  if (auto ctx = getDeclaredTypeInContext())
    return ctx->getClassOrBoundGenericClass();
  return nullptr;
}

Type DeclContext::getDeclaredTypeOfContext() const {
  switch (getContextKind()) {
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::Initializer:
    return Type();

  case DeclContextKind::ExtensionDecl: {
    auto ED = cast<ExtensionDecl>(this);
    auto type = ED->getExtendedType();
    
    if (auto ND = type->getNominalOrBoundGenericNominal())
      return ND->getDeclaredType();
    
    if (isa<UnboundGenericType>(type.getPointer()))
      return getExtendedType(const_cast<ExtensionDecl*>(ED));
    return Type();
  }

  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(this)->getDeclaredType();
  }
}

Type DeclContext::getDeclaredTypeInContext() const {
  switch (getContextKind()) {
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::Initializer:
    return Type();

  case DeclContextKind::ExtensionDecl:
    return getExtendedType(cast<ExtensionDecl>(this));

  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(this)->getDeclaredTypeInContext();
  }
}

Type DeclContext::getDeclaredInterfaceType() const {
  switch (getContextKind()) {
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::Initializer:
    return Type();

  case DeclContextKind::ExtensionDecl:
    // FIXME: Need a sugar-preserving getExtendedInterfaceType for extensions
    if (auto nominal = getDeclaredTypeOfContext()->getAnyNominal())
      return nominal->getDeclaredInterfaceType();
    return Type();

  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(this)->getDeclaredInterfaceType();
  }
}



GenericParamList *DeclContext::getGenericParamsOfContext() const {
  switch (getContextKind()) {
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
  case DeclContextKind::TopLevelCodeDecl:
    return nullptr;

  case DeclContextKind::Initializer:
  case DeclContextKind::AbstractClosureExpr:
    // Closures and initializers can't themselves be generic, but they
    // can occur in generic contexts.
    return getParent()->getGenericParamsOfContext();

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
    if (auto gp = extension->getGenericParams())
      return gp;
    return nullptr;
  }
  }
}

GenericSignature *DeclContext::getGenericSignatureOfContext() const {
  switch (getContextKind()) {
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::Initializer:
    return nullptr;

  case DeclContextKind::AbstractFunctionDecl: {
    auto *AFD = cast<AbstractFunctionDecl>(this);
    if (auto GFT = AFD->getInterfaceType()->getAs<GenericFunctionType>())
      return GFT->getGenericSignature();
    return AFD->getDeclContext()->getGenericSignatureOfContext();
  }

  case DeclContextKind::NominalTypeDecl: {
    auto nominal = cast<NominalTypeDecl>(this);
    if (auto genericSig = nominal->getGenericSignature())
      return genericSig;
    return nominal->getDeclContext()->getGenericSignatureOfContext();
  }

  case DeclContextKind::ExtensionDecl: {
    auto extension = cast<ExtensionDecl>(this);
    auto extendedType = extension->getExtendedType();
    // FIXME: What if the extended type is bound, or the extension has
    // constraints?
    auto nomDecl = extendedType->getNominalOrBoundGenericNominal();
    return nomDecl->getGenericSignatureOfContext();
  }
  }
}

DeclContext *DeclContext::getLocalContext() {
  if (isLocalContext())
    return this;
  if (isModuleContext() || isExtensionContext())
    return nullptr;
  return getParent()->getLocalContext();
}

AbstractFunctionDecl *DeclContext::getInnermostMethodContext() {
  DeclContext *result = this;
  while (true) {
    switch (result->getContextKind()) {
    case DeclContextKind::AbstractClosureExpr:
    case DeclContextKind::Initializer:
      // Look through closures, initial values.
      result = result->getParent();
      continue;

    case DeclContextKind::AbstractFunctionDecl: {
      // If this function is a method, we found our result.
      auto func = dyn_cast<AbstractFunctionDecl>(result);
      if (func->getDeclContext()->isTypeContext())
        return func;

      // This function isn't a method; look through it.
      result = func->getDeclContext();
      continue;
    }

    case DeclContextKind::ExtensionDecl:
    case DeclContextKind::FileUnit:
    case DeclContextKind::Module:
    case DeclContextKind::NominalTypeDecl:
    case DeclContextKind::TopLevelCodeDecl:
      // Not in a method context.
      return nullptr;
    }
  }
}

DeclContext *DeclContext::getInnermostTypeContext() {
  DeclContext *Result = this;
  while (true) {
    switch (Result->getContextKind()) {
    case DeclContextKind::AbstractClosureExpr:
    case DeclContextKind::Initializer:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::AbstractFunctionDecl:
      Result = Result->getParent();
      continue;

    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
      return nullptr;

    case DeclContextKind::ExtensionDecl:
    case DeclContextKind::NominalTypeDecl:
      return Result;
    }
  }
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

    case DeclContextKind::Initializer:
    case DeclContextKind::AbstractClosureExpr:
      // Check parent context.
      continue;

    case DeclContextKind::AbstractFunctionDecl:
      if (cast<AbstractFunctionDecl>(dc)->getGenericParams())
        return true;
      continue;

    case DeclContextKind::TopLevelCodeDecl:
      // Check parent context.
      continue;

    case DeclContextKind::NominalTypeDecl:
      if (cast<NominalTypeDecl>(dc)->getGenericParams())
        return true;
      continue;

    case DeclContextKind::ExtensionDecl:
      if (cast<ExtensionDecl>(dc)->getGenericParams())
        return true;
      continue;
    }
    llvm_unreachable("bad decl context kind");
  }
  llvm_unreachable("illegal declcontext hierarchy");
}

/// Determine whether the innermost context is generic.
bool DeclContext::isInnermostContextGeneric() const {
  switch (getContextKind()) {
  case DeclContextKind::AbstractFunctionDecl:
    return (cast<AbstractFunctionDecl>(this)->getGenericParams() != nullptr);
  case DeclContextKind::ExtensionDecl:
    return (cast<ExtensionDecl>(this)->getGenericParams() != nullptr);
  case DeclContextKind::NominalTypeDecl:
    return (cast<NominalTypeDecl>(this)->getGenericParams() != nullptr);
  default:
    return false;
  }
}


bool DeclContext::walkContext(ASTWalker &Walker) {
  switch (getContextKind()) {
  case DeclContextKind::Module:
    return cast<Module>(this)->walk(Walker);
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
  case DeclContextKind::Initializer:
    // Is there any point in trying to walk the expression?
    return false;
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
  case DeclContextKind::Initializer:      Kind = "Initializer"; break;
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
    case FileUnitKind::Derived:
      OS << " derived";
      break;
    case FileUnitKind::Source:
      OS << " file=\"" << cast<SourceFile>(this)->getFilename() << "\"";
      break;
    case FileUnitKind::SerializedAST:
    case FileUnitKind::ClangModule:
      OS << " file=\"" << cast<LoadedFile>(this)->getFilename() << "\"";
      break;
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
  case DeclContextKind::AbstractFunctionDecl: {
    auto *AFD = cast<AbstractFunctionDecl>(this);
    OS << " name=" << AFD->getName();
    if (AFD->hasType())
      OS << " : " << AFD->getType();
    else
      OS << " : (no type set)";
    break;
  }
  case DeclContextKind::Initializer:
    switch (cast<Initializer>(this)->getInitializerKind()) {
    case InitializerKind::PatternBinding: {
      auto init = cast<PatternBindingInitializer>(this);
      OS << " PatternBinding 0x" << (void*) init->getBinding();
      break;
    }
    case InitializerKind::DefaultArgument: {
      auto init = cast<DefaultArgumentInitializer>(this);
      OS << " DefaultArgument index=" << init->getIndex();
      break;
    }
    }
  }

  OS << "\n";
  return Depth + 1;
}

DeclRange IterableDeclContext::getMembers() const {
  loadAllMembers();

  return DeclRange(FirstDecl, nullptr);
}

/// Add a member to this context.
void IterableDeclContext::addMember(Decl *member) {
  // Add the member to the list of declarations without notification.
  addMemberSilently(member);

  // Notify our parent declaration that we have added the member, which can
  // be used to update the lookup tables.
  // FIXME: If only we had the notion of a "searchable" declaration
  // context...
  switch (getIterableContextKind()) {
  case IterableDeclContextKind::NominalTypeDecl: {
    auto nominal = cast<NominalTypeDecl>(this);
    nominal->addedMember(member);
    assert(member->getDeclContext() == nominal &&
           "Added member to the wrong context");
    break;
  }

  case IterableDeclContextKind::ExtensionDecl: {
    auto ext = cast<ExtensionDecl>(this);
    ext->addedMember(member);
    assert(member->getDeclContext() == ext &&
           "Added member to the wrong context");
    break;
  }
  }
}

void IterableDeclContext::addMemberSilently(Decl *member) const {
  assert(!member->NextDecl && "Already added to a container");
  if (auto last = LastDeclAndKind.getPointer()) {
    last->NextDecl = member;
  } else {
    FirstDecl = member;
  }
  LastDeclAndKind.setPointer(member);
}

void IterableDeclContext::setLoader(LazyMemberLoader *loader, 
                                    uint64_t contextData) {
  LazyLoader = loader;
  LazyLoaderContextData = contextData;

  ++NumLazyIterableDeclContexts;
  ++NumUnloadedLazyIterableDeclContexts;
}

void IterableDeclContext::loadAllMembers() const {
  if (!isLazy())
    return;

  // Don't try to load all members re-entrant-ly.
  auto resolver = getLoader();
  auto contextData = getLoaderContextData();
  LazyLoader = nullptr;

  const Decl *container = nullptr;
  switch (getIterableContextKind()) {
  case IterableDeclContextKind::NominalTypeDecl:
    container = cast<NominalTypeDecl>(this);
    break;

  case IterableDeclContextKind::ExtensionDecl:
    container = cast<ExtensionDecl>(this);
    break;
  }

  for (auto member : resolver->loadAllMembers(container, contextData)) {
    const_cast<IterableDeclContext *>(this)->addMember(member);
  }

  --NumUnloadedLazyIterableDeclContexts;
}

