//===--- DeclContext.cpp - DeclContext implementation ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AST.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;

#define DEBUG_TYPE "Name lookup"

STATISTIC(NumLazyIterableDeclContexts,
          "# of serialized iterable declaration contexts");
STATISTIC(NumUnloadedLazyIterableDeclContexts,
          "# of serialized iterable declaration contexts never loaded");

// Only allow allocation of DeclContext using the allocator in ASTContext.
void *DeclContext::operator new(size_t Bytes, ASTContext &C,
                                unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

ASTContext &DeclContext::getASTContext() const {
  return getParentModule()->getASTContext();
}

GenericTypeDecl *
DeclContext::getAsTypeOrTypeExtensionContext() const {
  switch (getContextKind()) {
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::SubscriptDecl:
  case DeclContextKind::Initializer:
  case DeclContextKind::SerializedLocal:
    return nullptr;

  case DeclContextKind::ExtensionDecl: {
    auto ED = cast<ExtensionDecl>(this);
    auto type = ED->getExtendedType();

    if (type.isNull() || type->hasError())
      return nullptr;

    if (auto ND = type->getNominalOrBoundGenericNominal())
      return ND;

    if (auto unbound = dyn_cast<UnboundGenericType>(type.getPointer())) {
      return unbound->getDecl();
    }

    return nullptr;
  }

  case DeclContextKind::GenericTypeDecl:
    return const_cast<GenericTypeDecl*>(cast<GenericTypeDecl>(this));
  }

  llvm_unreachable("Unhandled DeclContextKind in switch.");
}

/// If this DeclContext is a NominalType declaration or an
/// extension thereof, return the NominalTypeDecl.
NominalTypeDecl *DeclContext::
getAsNominalTypeOrNominalTypeExtensionContext() const {
  auto decl = getAsTypeOrTypeExtensionContext();
  return dyn_cast_or_null<NominalTypeDecl>(decl);
}


ClassDecl *DeclContext::getAsClassOrClassExtensionContext() const {
  return dyn_cast_or_null<ClassDecl>(getAsTypeOrTypeExtensionContext());
}

EnumDecl *DeclContext::getAsEnumOrEnumExtensionContext() const {
  return dyn_cast_or_null<EnumDecl>(getAsTypeOrTypeExtensionContext());
}

ProtocolDecl *DeclContext::getAsProtocolOrProtocolExtensionContext() const {
  return dyn_cast_or_null<ProtocolDecl>(getAsTypeOrTypeExtensionContext());
}

ProtocolDecl *DeclContext::getAsProtocolExtensionContext() const {
  if (getContextKind() != DeclContextKind::ExtensionDecl)
    return nullptr;

  return dyn_cast_or_null<ProtocolDecl>(getAsTypeOrTypeExtensionContext());
}

GenericTypeParamType *DeclContext::getProtocolSelfType() const {
  assert(getAsProtocolOrProtocolExtensionContext() && "not a protocol");

  // FIXME: This comes up when the extension didn't resolve,
  // and we have a protocol nested inside that extension
  // (which is not allowed in the first place).
  //
  // Handle this more systematically elsewhere.
  if (!isInnermostContextGeneric())
    return nullptr;

  return getGenericParamsOfContext()->getParams().front()
      ->getDeclaredInterfaceType()
      ->castTo<GenericTypeParamType>();
}

enum class DeclTypeKind : unsigned {
  DeclaredType,
  DeclaredTypeInContext,
  DeclaredInterfaceType
};

static Type computeExtensionType(const ExtensionDecl *ED, DeclTypeKind kind) {
  auto type = ED->getExtendedType();
  if (!type) {
    if (ED->isInvalid())
      return ErrorType::get(ED->getASTContext());
    return Type();
  }

  if (type->is<UnboundGenericType>()) {
    ED->getASTContext().getLazyResolver()->resolveExtension(
      const_cast<ExtensionDecl *>(ED));
    type = ED->getExtendedType();
  }

  if (type->hasError())
    return type;

  switch (kind) {
  case DeclTypeKind::DeclaredType:
    return type->getAnyNominal()->getDeclaredType();
  case DeclTypeKind::DeclaredTypeInContext:
    return type;
  case DeclTypeKind::DeclaredInterfaceType:
    // FIXME: Need a sugar-preserving getExtendedInterfaceType for extensions
    return type->getAnyNominal()->getDeclaredInterfaceType();
  }

  llvm_unreachable("Unhandled DeclTypeKind in switch.");
}

Type DeclContext::getDeclaredTypeOfContext() const {
  if (auto *ED = dyn_cast<ExtensionDecl>(this))
    return computeExtensionType(ED, DeclTypeKind::DeclaredType);
  if (auto *NTD = dyn_cast<NominalTypeDecl>(this))
    return NTD->getDeclaredType();
  return Type();
}

Type DeclContext::getDeclaredTypeInContext() const {
  if (auto *ED = dyn_cast<ExtensionDecl>(this))
    return computeExtensionType(ED, DeclTypeKind::DeclaredTypeInContext);
  if (auto *NTD = dyn_cast<NominalTypeDecl>(this))
    return NTD->getDeclaredTypeInContext();
  return Type();
}

Type DeclContext::getDeclaredInterfaceType() const {
  if (auto *ED = dyn_cast<ExtensionDecl>(this))
    return computeExtensionType(ED, DeclTypeKind::DeclaredInterfaceType);
  if (auto *NTD = dyn_cast<NominalTypeDecl>(this))
    return NTD->getDeclaredInterfaceType();
  return Type();
}

GenericParamList *DeclContext::getGenericParamsOfContext() const {
  for (const DeclContext *dc = this; ; dc = dc->getParent()) {
    switch (dc->getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
    case DeclContextKind::TopLevelCodeDecl:
      return nullptr;

    case DeclContextKind::SerializedLocal:
    case DeclContextKind::Initializer:
    case DeclContextKind::AbstractClosureExpr:
      // Closures and initializers can't themselves be generic, but they
      // can occur in generic contexts.
      continue;

    case DeclContextKind::SubscriptDecl:
      if (auto GP = cast<SubscriptDecl>(dc)->getGenericParams())
        return GP;
      continue;

    case DeclContextKind::AbstractFunctionDecl:
      if (auto GP = cast<AbstractFunctionDecl>(dc)->getGenericParams())
        return GP;
      continue;

    case DeclContextKind::GenericTypeDecl:
      if (auto GP = cast<GenericTypeDecl>(dc)->getGenericParams())
        return GP;
      continue;

    case DeclContextKind::ExtensionDecl:
      // Extensions do not capture outer generic parameters.
      return cast<ExtensionDecl>(dc)->getGenericParams();
    }
    llvm_unreachable("bad DeclContextKind");
  }
  llvm_unreachable("unknown parent");
}

GenericSignature *DeclContext::getGenericSignatureOfContext() const {
  for (const DeclContext *dc = this; ; dc = dc->getParent()) {
    switch (dc->getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
    case DeclContextKind::TopLevelCodeDecl:
      return nullptr;

    case DeclContextKind::Initializer:
    case DeclContextKind::SerializedLocal:
    case DeclContextKind::AbstractClosureExpr:
      // Closures and initializers can't themselves be generic, but they
      // can occur in generic contexts.
      continue;

    case DeclContextKind::SubscriptDecl:
      return cast<SubscriptDecl>(dc)->getGenericSignature();

    case DeclContextKind::AbstractFunctionDecl:
      return cast<AbstractFunctionDecl>(dc)->getGenericSignature();

    case DeclContextKind::GenericTypeDecl:
      return cast<GenericTypeDecl>(dc)->getGenericSignature();

    case DeclContextKind::ExtensionDecl:
      return cast<ExtensionDecl>(dc)->getGenericSignature();
    }
    llvm_unreachable("bad DeclContextKind");
  }
}

GenericEnvironment *DeclContext::getGenericEnvironmentOfContext() const {
  for (const DeclContext *dc = this; ; dc = dc->getParent()) {
    switch (dc->getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
    case DeclContextKind::TopLevelCodeDecl:
      return nullptr;

    case DeclContextKind::Initializer:
    case DeclContextKind::SerializedLocal:
    case DeclContextKind::AbstractClosureExpr:
      // Closures and initializers can't themselves be generic, but they
      // can occur in generic contexts.
      continue;

    case DeclContextKind::SubscriptDecl:
      return cast<SubscriptDecl>(dc)->getGenericEnvironment();

    case DeclContextKind::AbstractFunctionDecl:
      return cast<AbstractFunctionDecl>(dc)->getGenericEnvironment();

    case DeclContextKind::GenericTypeDecl:
      return cast<GenericTypeDecl>(dc)->getGenericEnvironment();

    case DeclContextKind::ExtensionDecl:
      return cast<ExtensionDecl>(dc)->getGenericEnvironment();
    }
    llvm_unreachable("bad DeclContextKind");
  }
}

Type DeclContext::mapTypeIntoContext(Type type) const {
  return GenericEnvironment::mapTypeIntoContext(
      getGenericEnvironmentOfContext(), type);
}

Type DeclContext::mapTypeOutOfContext(Type type) const {
  return GenericEnvironment::mapTypeOutOfContext(
      getGenericEnvironmentOfContext(), type);
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
    case DeclContextKind::SerializedLocal:
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
    case DeclContextKind::GenericTypeDecl:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::SubscriptDecl:
      // Not in a method context.
      return nullptr;
    }
  }
}

bool DeclContext::isTypeContext() const {
  return isa<NominalTypeDecl>(this) ||
         getContextKind() == DeclContextKind::ExtensionDecl;
}

DeclContext *DeclContext::getInnermostTypeContext() {
  DeclContext *Result = this;
  while (true) {
    switch (Result->getContextKind()) {
    case DeclContextKind::AbstractClosureExpr:
    case DeclContextKind::Initializer:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::AbstractFunctionDecl:
    case DeclContextKind::SubscriptDecl:
    case DeclContextKind::SerializedLocal:
      Result = Result->getParent();
      continue;

    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
      return nullptr;

    case DeclContextKind::GenericTypeDecl:
      if (isa<TypeAliasDecl>(Result)) {
        Result = Result->getParent();
        continue;
      }
      return Result;

    case DeclContextKind::ExtensionDecl:
      return Result;
    }
  }
}

Decl *DeclContext::getInnermostDeclarationDeclContext() {
  DeclContext *DC = this;
  while (DC) {
    switch (DC->getContextKind()) {
    case DeclContextKind::AbstractClosureExpr:
    case DeclContextKind::Initializer:
    case DeclContextKind::SerializedLocal:
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
      break;

    case DeclContextKind::TopLevelCodeDecl:
      return cast<TopLevelCodeDecl>(DC);

    case DeclContextKind::AbstractFunctionDecl:
      return cast<AbstractFunctionDecl>(DC);

    case DeclContextKind::SubscriptDecl:
      return cast<SubscriptDecl>(DC);

    case DeclContextKind::GenericTypeDecl:
      return cast<GenericTypeDecl>(DC);

    case DeclContextKind::ExtensionDecl:
      return cast<ExtensionDecl>(DC);
    }

    DC = DC->getParent();
  }

  return nullptr;
}

ModuleDecl *DeclContext::getParentModule() const {
  const DeclContext *DC = this;
  while (!DC->isModuleContext())
    DC = DC->getParent();
  return const_cast<ModuleDecl *>(cast<ModuleDecl>(DC));
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
  for (const DeclContext *dc = this; ; dc = dc->getParent()) {
    switch (dc->getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
    case DeclContextKind::TopLevelCodeDecl:
      return false;

    case DeclContextKind::Initializer:
    case DeclContextKind::AbstractClosureExpr:
    case DeclContextKind::SerializedLocal:
      // Check parent context.
      continue;

    case DeclContextKind::SubscriptDecl:
      if (cast<SubscriptDecl>(dc)->getGenericParams())
        return true;
      // Check parent context.
      continue;

    case DeclContextKind::AbstractFunctionDecl:
      if (cast<AbstractFunctionDecl>(dc)->getGenericParams())
        return true;
      // Check parent context.
      continue;

    case DeclContextKind::GenericTypeDecl:
      if (cast<GenericTypeDecl>(dc)->getGenericParams())
        return true;
      // Check parent context.
      continue;

    case DeclContextKind::ExtensionDecl:
      if (cast<ExtensionDecl>(dc)->getGenericParams())
        return true;
      // Extensions do not capture outer generic parameters.
      return false;
    }
    llvm_unreachable("bad decl context kind");
  }
  llvm_unreachable("illegal declcontext hierarchy");
}

/// Get the most optimal resilience expansion for the body of this function.
/// If the body is able to be inlined into functions in other resilience
/// domains, this ensures that only sufficiently-conservative access patterns
/// are used.
ResilienceExpansion DeclContext::getResilienceExpansion() const {
  for (const auto *dc = this; dc->isLocalContext(); dc = dc->getParent()) {
    // Default argument initializer contexts have their resilience expansion
    // set when they're type checked.
    if (auto *DAI = dyn_cast<DefaultArgumentInitializer>(dc))
      return DAI->getResilienceExpansion();

    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(dc)) {
      // If the function is a nested function, we will serialize its body if
      // we serialize the parent's body.
      if (AFD->getDeclContext()->isLocalContext())
        continue;

      // FIXME: Make sure this method is never called on decls that have not
      // been fully validated.
      if (!AFD->hasAccessibility())
        break;

      // If the function is not externally visible, we will not be serializing
      // its body.
      if (AFD->getEffectiveAccess() < Accessibility::Public)
        break;

      // Bodies of public transparent and always-inline functions are
      // serialized, so use conservative access patterns.
      if (AFD->isTransparent())
        return ResilienceExpansion::Minimal;

      if (AFD->getAttrs().hasAttribute<InlineableAttr>())
        return ResilienceExpansion::Minimal;

      if (auto attr = AFD->getAttrs().getAttribute<InlineAttr>())
        if (attr->getKind() == InlineKind::Always)
          return ResilienceExpansion::Minimal;

      // If a property or subscript is @_inlineable, the accessors are
      // @_inlineable also.
      if (auto FD = dyn_cast<FuncDecl>(AFD))
        if (auto *ASD = FD->getAccessorStorageDecl())
          if (ASD->getAttrs().getAttribute<InlineableAttr>())
            return ResilienceExpansion::Minimal;
    }
  }

  return ResilienceExpansion::Maximal;
}

/// Determine whether the innermost context is generic.
bool DeclContext::isInnermostContextGeneric() const {
  switch (getContextKind()) {
  case DeclContextKind::SubscriptDecl:
    return cast<SubscriptDecl>(this)->isGeneric();
  case DeclContextKind::AbstractFunctionDecl:
    return cast<AbstractFunctionDecl>(this)->isGeneric();
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(this)->isGeneric();
  case DeclContextKind::GenericTypeDecl:
    return cast<GenericTypeDecl>(this)->isGeneric();
  default:
    return false;
  }
  llvm_unreachable("bad DeclContextKind");
}

bool
DeclContext::isCascadingContextForLookup(bool functionsAreNonCascading) const {
  // FIXME: This is explicitly checking for attributes in some cases because
  // it can be called before accessibility is computed.
  switch (getContextKind()) {
  case DeclContextKind::AbstractClosureExpr:
    break;

  case DeclContextKind::SerializedLocal:
    llvm_unreachable("should not perform lookups in deserialized contexts");

  case DeclContextKind::Initializer:
    // Default arguments still require a type.
    if (isa<DefaultArgumentInitializer>(this))
      return false;
    break;

  case DeclContextKind::TopLevelCodeDecl:
    // FIXME: Pattern initializers at top-level scope end up here.
    return true;

  case DeclContextKind::AbstractFunctionDecl: {
    if (functionsAreNonCascading)
      return false;
    auto *AFD = cast<AbstractFunctionDecl>(this);
    if (AFD->hasAccessibility())
      return AFD->getFormalAccess() > Accessibility::FilePrivate;
    break;
  }

  case DeclContextKind::SubscriptDecl: {
    auto *SD = cast<SubscriptDecl>(this);
    if (SD->hasAccessibility())
      return SD->getFormalAccess() > Accessibility::FilePrivate;
    break;
  }
      
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
    return true;

  case DeclContextKind::GenericTypeDecl: {
    auto *nominal = cast<GenericTypeDecl>(this);
    if (nominal->hasAccessibility())
      return nominal->getFormalAccess() > Accessibility::FilePrivate;
    break;
  }

  case DeclContextKind::ExtensionDecl: {
    auto *extension = cast<ExtensionDecl>(this);
    if (extension->hasDefaultAccessibility())
      return extension->getDefaultAccessibility() > Accessibility::FilePrivate;
    // FIXME: duplicated from computeDefaultAccessibility in TypeCheckDecl.cpp.
    if (auto *AA = extension->getAttrs().getAttribute<AccessibilityAttr>())
      return AA->getAccess() > Accessibility::FilePrivate;
    if (Type extendedTy = extension->getExtendedType()) {

      // Need to check if extendedTy is ErrorType
      if (extendedTy->getAnyNominal())
        return extendedTy->getAnyNominal()->isCascadingContextForLookup(true);
    }
    break;
  }
  }

  return getParent()->isCascadingContextForLookup(true);
}

bool DeclContext::walkContext(ASTWalker &Walker) {
  switch (getContextKind()) {
  case DeclContextKind::Module:
    return cast<ModuleDecl>(this)->walk(Walker);
  case DeclContextKind::FileUnit:
    return cast<FileUnit>(this)->walk(Walker);
  case DeclContextKind::AbstractClosureExpr:
    return cast<AbstractClosureExpr>(this)->walk(Walker);
  case DeclContextKind::GenericTypeDecl:
    return cast<GenericTypeDecl>(this)->walk(Walker);
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(this)->walk(Walker);
  case DeclContextKind::TopLevelCodeDecl:
    return cast<TopLevelCodeDecl>(this)->walk(Walker);
  case DeclContextKind::AbstractFunctionDecl:
    return cast<AbstractFunctionDecl>(this)->walk(Walker);
  case DeclContextKind::SubscriptDecl:
    return cast<SubscriptDecl>(this)->walk(Walker);
  case DeclContextKind::SerializedLocal:
    llvm_unreachable("walk is unimplemented for deserialized contexts");
  case DeclContextKind::Initializer:
    // Is there any point in trying to walk the expression?
    return false;
  }
  llvm_unreachable("bad DeclContextKind");
}

void DeclContext::dumpContext() const {
  printContext(llvm::errs());
}

template <typename DCType>
static unsigned getLineNumber(DCType *DC) {
  SourceLoc loc = DC->getLoc();
  if (loc.isInvalid())
    return 0;

  const ASTContext &ctx = static_cast<const DeclContext *>(DC)->getASTContext();
  return ctx.SourceMgr.getLineAndColumn(loc).first;
}

bool DeclContext::classof(const Decl *D) {
  switch (D->getKind()) { //
#define DECL(ID, PARENT)               case DeclKind::ID: return false;
#define CONTEXT_DECL(ID, PARENT)       case DeclKind::ID: return true;
#define CONTEXT_VALUE_DECL(ID, PARENT) case DeclKind::ID: return true;
#include "swift/AST/DeclNodes.def"
  }

  llvm_unreachable("Unhandled DeclKind in switch.");
}

DeclContext *DeclContext::castDeclToDeclContext(const Decl *D) {
  switch (D->getKind()) {
#define DECL(ID, PARENT) \
  case DeclKind::ID: llvm_unreachable("not a decl context");
#define CONTEXT_DECL(ID, PARENT) \
  case DeclKind::ID: \
    return const_cast<DeclContext *>( \
        static_cast<const DeclContext*>(cast<ID##Decl>(D)));
#define CONTEXT_VALUE_DECL(ID, PARENT) CONTEXT_DECL(ID, PARENT)
#include "swift/AST/DeclNodes.def"
  }

  llvm_unreachable("Unhandled DeclKind in switch.");
}

unsigned DeclContext::printContext(raw_ostream &OS, unsigned indent) const {
  unsigned Depth = 0;
  if (auto *P = getParent())
    Depth = P->printContext(OS, indent);

  const char *Kind;
  switch (getContextKind()) {
  case DeclContextKind::Module:           Kind = "Module"; break;
  case DeclContextKind::FileUnit:         Kind = "FileUnit"; break;
  case DeclContextKind::SerializedLocal:  Kind = "Serialized Local"; break;
  case DeclContextKind::AbstractClosureExpr:
    Kind = "AbstractClosureExpr";
    break;
  case DeclContextKind::GenericTypeDecl:
    switch (cast<GenericTypeDecl>(this)->getKind()) {
#define DECL(ID, PARENT) \
    case DeclKind::ID: Kind = #ID "Decl"; break;
#include "swift/AST/DeclNodes.def"
    }
    break;
  case DeclContextKind::ExtensionDecl:    Kind = "ExtensionDecl"; break;
  case DeclContextKind::TopLevelCodeDecl: Kind = "TopLevelCodeDecl"; break;
  case DeclContextKind::Initializer:      Kind = "Initializer"; break;
  case DeclContextKind::AbstractFunctionDecl:
    Kind = "AbstractFunctionDecl";
    break;
  case DeclContextKind::SubscriptDecl:    Kind = "SubscriptDecl"; break;
  }
  OS.indent(Depth*2 + indent) << "0x" << (void*)this << " " << Kind;

  switch (getContextKind()) {
  case DeclContextKind::Module:
    OS << " name=" << cast<ModuleDecl>(this)->getName();
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
  case DeclContextKind::GenericTypeDecl:
    OS << " name=" << cast<GenericTypeDecl>(this)->getName();
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
    if (AFD->hasInterfaceType())
      OS << " : " << AFD->getInterfaceType();
    else
      OS << " : (no type set)";
    break;
  }
  case DeclContextKind::SubscriptDecl: {
    auto *SD = cast<SubscriptDecl>(this);
    OS << " name=" << SD->getName();
    if (SD->hasInterfaceType())
      OS << " : " << SD->getInterfaceType();
    else
      OS << " : (no type set)";
    break;
  }
  case DeclContextKind::Initializer:
    switch (cast<Initializer>(this)->getInitializerKind()) {
    case InitializerKind::PatternBinding: {
      auto init = cast<PatternBindingInitializer>(this);
      OS << " PatternBinding 0x" << (void*) init->getBinding()
         << " #" << init->getBindingIndex();
      break;
    }
    case InitializerKind::DefaultArgument: {
      auto init = cast<DefaultArgumentInitializer>(this);
      OS << " DefaultArgument index=" << init->getIndex();
      break;
    }
    }
    break;

  case DeclContextKind::SerializedLocal: {
    auto local = cast<SerializedLocalDeclContext>(this);
    switch (local->getLocalDeclContextKind()) {
    case LocalDeclContextKind::AbstractClosure: {
      auto serializedClosure = cast<SerializedAbstractClosureExpr>(local);
      OS << " closure : " << serializedClosure->getType();
      break;
    }
    case LocalDeclContextKind::DefaultArgumentInitializer: {
      auto init = cast<SerializedDefaultArgumentInitializer>(local);
      OS << "DefaultArgument index=" << init->getIndex();
      break;
    }
    case LocalDeclContextKind::PatternBindingInitializer: {
      auto init = cast<SerializedPatternBindingInitializer>(local);
      OS << " PatternBinding 0x" << (void*) init->getBinding()
         << " #" << init->getBindingIndex();
      break;
    }
    case LocalDeclContextKind::TopLevelCodeDecl:
      OS << " TopLevelCode";
      break;
    }
  }
  }

  OS << "\n";
  return Depth + 1;
}

ASTContext &IterableDeclContext::getASTContext() const {
  switch (getIterableContextKind()) {
  case IterableDeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(this)->getASTContext();

  case IterableDeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(this)->getASTContext();
  }

  llvm_unreachable("Unhandled IterableDeclContextKind in switch.");
}

DeclRange IterableDeclContext::getMembers() const {
  loadAllMembers();

  return DeclRange(FirstDeclAndLazyMembers.getPointer(), nullptr);
}

/// Add a member to this context.
void IterableDeclContext::addMember(Decl *member, Decl *Hint) {
  // Add the member to the list of declarations without notification.
  addMemberSilently(member, Hint);

  // Notify our parent declaration that we have added the member, which can
  // be used to update the lookup tables.
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

void IterableDeclContext::addMemberSilently(Decl *member, Decl *hint) const {
  assert(!member->NextDecl && "Already added to a container");

  // If there is a hint decl that specifies where to add this, just
  // link into the chain immediately following it.
  if (hint) {
    member->NextDecl = hint->NextDecl;
    hint->NextDecl = member;

    // If the hint was the last in the parent context's chain, update it.
    if (LastDeclAndKind.getPointer() == hint)
      LastDeclAndKind.setPointer(member);
    return;
  }

  if (auto last = LastDeclAndKind.getPointer()) {
    last->NextDecl = member;
    assert(last != member && "Simple cycle in decl list");
  } else {
    FirstDeclAndLazyMembers.setPointer(member);
  }
  LastDeclAndKind.setPointer(member);
}

void IterableDeclContext::setMemberLoader(LazyMemberLoader *loader,
                                          uint64_t contextData) {
  assert(!hasLazyMembers() && "already have lazy members");

  ASTContext &ctx = getASTContext();
  auto contextInfo = ctx.getOrCreateLazyIterableContextData(this, loader);
  FirstDeclAndLazyMembers.setInt(true);
  contextInfo->memberData = contextData;

  ++NumLazyIterableDeclContexts;
  ++NumUnloadedLazyIterableDeclContexts;
}

void IterableDeclContext::loadAllMembers() const {
  if (!hasLazyMembers())
    return;

  // Don't try to load all members re-entrant-ly.
  ASTContext &ctx = getASTContext();
  auto contextInfo = ctx.getOrCreateLazyIterableContextData(this,
    /*lazyLoader=*/nullptr);
  FirstDeclAndLazyMembers.setInt(false);

  const Decl *container = nullptr;
  switch (getIterableContextKind()) {
  case IterableDeclContextKind::NominalTypeDecl:
    container = cast<NominalTypeDecl>(this);
    break;

  case IterableDeclContextKind::ExtensionDecl:
    container = cast<ExtensionDecl>(this);
    break;
  }

  contextInfo->loader->loadAllMembers(const_cast<Decl *>(container),
                                      contextInfo->memberData);

  --NumUnloadedLazyIterableDeclContexts;
}

bool IterableDeclContext::classof(const Decl *D) {
  switch (D->getKind()) {
#define DECL(ID, PARENT)              case DeclKind::ID: return false;
#define NOMINAL_TYPE_DECL(ID, PARENT) case DeclKind::ID: return true;
#define EXTENSION_DECL(ID, PARENT)    case DeclKind::ID: return true;
#include "swift/AST/DeclNodes.def"
  }

  llvm_unreachable("Unhandled DeclKind in switch.");
}

IterableDeclContext *
IterableDeclContext::castDeclToIterableDeclContext(const Decl *D) {
  switch (D->getKind()) {
#define DECL(ID, PARENT) \
  case DeclKind::ID: llvm_unreachable("not a decl context");
#define NOMINAL_TYPE_DECL(ID, PARENT) \
  case DeclKind::ID: \
    return const_cast<IterableDeclContext *>( \
        static_cast<const IterableDeclContext*>(cast<ID##Decl>(D)));
#define EXTENSION_DECL(ID, PARENT) \
  case DeclKind::ID: \
    return const_cast<IterableDeclContext *>( \
        static_cast<const IterableDeclContext*>(cast<ID##Decl>(D)));
#include "swift/AST/DeclNodes.def"
  }

  llvm_unreachable("Unhandled DeclKind in switch.");
}

AccessScope::AccessScope(const DeclContext *DC, bool isPrivate)
    : Value(DC, isPrivate) {
  if (!DC || isa<ModuleDecl>(DC))
    assert(!isPrivate && "public or internal scope can't be private");
}

bool AccessScope::isFileScope() const {
  auto DC = getDeclContext();
  return DC && isa<FileUnit>(DC);
}

Accessibility AccessScope::accessibilityForDiagnostics() const {
  if (isPublic())
    return Accessibility::Public;
  if (isa<ModuleDecl>(getDeclContext()))
    return Accessibility::Internal;
  if (getDeclContext()->isModuleScopeContext()) {
    return isPrivate() ? Accessibility::Private : Accessibility::FilePrivate;
  }

  return Accessibility::Private;
}
