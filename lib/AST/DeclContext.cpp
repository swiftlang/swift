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

#include "swift/AST/DeclContext.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
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

GenericTypeDecl *DeclContext::getSelfTypeDecl() const {
  auto decl = const_cast<Decl*>(getAsDecl());
  if (!decl) return nullptr;

  auto ext = dyn_cast<ExtensionDecl>(decl);
  if (!ext) return dyn_cast<GenericTypeDecl>(decl);

  return ext->getExtendedNominal();
}

/// If this DeclContext is a NominalType declaration or an
/// extension thereof, return the NominalTypeDecl.
NominalTypeDecl *DeclContext::getSelfNominalTypeDecl() const {
  return dyn_cast_or_null<NominalTypeDecl>(getSelfTypeDecl());
}


ClassDecl *DeclContext::getSelfClassDecl() const {
  return dyn_cast_or_null<ClassDecl>(getSelfTypeDecl());
}

EnumDecl *DeclContext::getSelfEnumDecl() const {
  return dyn_cast_or_null<EnumDecl>(getSelfTypeDecl());
}

StructDecl *DeclContext::getSelfStructDecl() const {
  return dyn_cast_or_null<StructDecl>(getSelfTypeDecl());
}

ProtocolDecl *DeclContext::getSelfProtocolDecl() const {
  return dyn_cast_or_null<ProtocolDecl>(getSelfTypeDecl());
}

ProtocolDecl *DeclContext::getExtendedProtocolDecl() const {
  if (auto decl = const_cast<Decl*>(getAsDecl()))
    if (auto ED = dyn_cast<ExtensionDecl>(decl))
      return dyn_cast_or_null<ProtocolDecl>(ED->getExtendedNominal());
  return nullptr;
}

GenericTypeParamType *DeclContext::getProtocolSelfType() const {
  assert(getSelfProtocolDecl() && "not a protocol");

  if (auto proto = dyn_cast<ProtocolDecl>(this)) {
    const_cast<ProtocolDecl *>(proto)->createGenericParamsIfMissing();
  }

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
    auto *resolver = ED->getASTContext().getLazyResolver();
    assert(resolver && "Too late to resolve extensions");
    resolver->resolveExtension(const_cast<ExtensionDecl *>(ED));
    type = ED->getExtendedType();
  }

  if (type->hasError())
    return type;

  switch (kind) {
  case DeclTypeKind::DeclaredType:
    return type->getAnyNominal()->getDeclaredType();
  case DeclTypeKind::DeclaredTypeInContext:
    return type;
  case DeclTypeKind::DeclaredInterfaceType: {
    // FIXME: Need a sugar-preserving getExtendedInterfaceType for extensions
    if (auto nominal = type->getAnyNominal())
      return nominal->getDeclaredInterfaceType();

    auto typealias = cast<TypeAliasDecl>(type->getAnyGeneric());
    return typealias->getUnderlyingTypeLoc().getType();
  }
  }

  llvm_unreachable("Unhandled DeclTypeKind in switch.");
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
  auto dc = this;
  do {
    if (auto decl = dc->getAsDecl()) {
      if (auto GC = decl->getAsGenericContext()) {
        auto GP = GC->getGenericParams();
        // Extensions do not capture outer generic parameters.
        if (GP != nullptr || isa<ExtensionDecl>(decl))
          return GP;
      }
    }
  } while ((dc = dc->getParent()));

  return nullptr;
}

GenericSignature *DeclContext::getGenericSignatureOfContext() const {
  auto dc = this;
  do {
    if (auto decl = dc->getAsDecl())
      if (auto GC = decl->getAsGenericContext())
        return GC->getGenericSignature();
  } while ((dc = dc->getParent()));

  return nullptr;
}

GenericEnvironment *DeclContext::getGenericEnvironmentOfContext() const {
  auto dc = this;
  do {
    if (auto decl = dc->getAsDecl())
      if (auto GC = decl->getAsGenericContext())
        return GC->getGenericEnvironment();
  } while ((dc = dc->getParent()));

  return nullptr;
}

bool DeclContext::contextHasLazyGenericEnvironment() const {
  auto dc = this;
  do {
    if (auto decl = dc->getAsDecl())
      if (auto GC = decl->getAsGenericContext())
        return GC->hasLazyGenericEnvironment();
  } while ((dc = dc->getParent()));

  return false;
}

Type DeclContext::mapTypeIntoContext(Type type) const {
  return GenericEnvironment::mapTypeIntoContext(
      getGenericEnvironmentOfContext(), type);
}

DeclContext *DeclContext::getLocalContext() {
  if (isLocalContext())
    return this;
  if (isModuleContext())
    return nullptr;
  return getParent()->getLocalContext();
}

AbstractFunctionDecl *DeclContext::getInnermostMethodContext() {
  auto dc = this;
  do {
    if (auto decl = dc->getAsDecl()) {
      auto func = dyn_cast<AbstractFunctionDecl>(decl);
      // If we found a non-func decl, we're done.
      if (func == nullptr)
        return nullptr;
      if (func->getDeclContext()->isTypeContext())
        return func;
    }
  } while ((dc = dc->getParent()));

  return nullptr;
}

bool DeclContext::isTypeContext() const {
  if (auto decl = getAsDecl())
    return isa<NominalTypeDecl>(decl) || isa<ExtensionDecl>(decl);
  return false;
}

DeclContext *DeclContext::getInnermostTypeContext() {
  auto dc = this;
  do {
    if (auto decl = dc->getAsDecl())
      if (isa<NominalTypeDecl>(decl) || isa<ExtensionDecl>(decl))
        return dc;
  } while ((dc = dc->getParent()));

  return nullptr;
}

Decl *DeclContext::getInnermostDeclarationDeclContext() {
  auto DC = this;
  do {
    if (auto decl = DC->getAsDecl())
      return isa<ModuleDecl>(decl) ? nullptr : decl;
  } while ((DC = DC->getParent()));

  return nullptr;
}

DeclContext *DeclContext::getParentForLookup() const {
  if (isa<ProtocolDecl>(this) || isa<ExtensionDecl>(this)) {
    // If we are inside a protocol or an extension, skip directly
    // to the module scope context, without looking at any (invalid)
    // outer types.
    return getModuleScopeContext();
  }
  if (isa<NominalTypeDecl>(this)) {
    // If we are inside a nominal type that is inside a protocol,
    // skip the protocol.
    if (isa<ProtocolDecl>(getParent()))
      return getModuleScopeContext();
  }
  return getParent();
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
  auto DC = const_cast<DeclContext*>(this);

  while (true) {
    if (DC->ParentAndKind.getInt() == ASTHierarchy::FileUnit)
      return DC;
    if (auto NextDC = DC->getParent()) {
      DC = NextDC;
    } else {
      assert(isa<ModuleDecl>(DC->getAsDecl()));
      return DC;
    }
  }
}

/// Determine whether the given context is generic at any level.
bool DeclContext::isGenericContext() const {
  return getGenericParamsOfContext() != nullptr;
}

/// Get the most optimal resilience expansion for the body of this function.
/// If the body is able to be inlined into functions in other resilience
/// domains, this ensures that only sufficiently-conservative access patterns
/// are used.
ResilienceExpansion DeclContext::getResilienceExpansion() const {
  for (const auto *dc = this; dc->isLocalContext(); dc = dc->getParent()) {
    // Default argument initializer contexts have their resilience expansion
    // set when they're type checked.
    if (isa<DefaultArgumentInitializer>(dc)) {
      return cast<AbstractFunctionDecl>(dc->getParent())
          ->getDefaultArgumentResilienceExpansion();
    }

    // Stored property initializer contexts use minimal resilience expansion
    // if the type is formally fixed layout.
    if (isa<PatternBindingInitializer>(dc)) {
      if (auto *NTD = dyn_cast<NominalTypeDecl>(dc->getParent())) {
        auto nominalAccess =
          NTD->getFormalAccessScope(/*useDC=*/nullptr,
                                    /*treatUsableFromInlineAsPublic=*/true);
        if (!nominalAccess.isPublic())
          return ResilienceExpansion::Maximal;

        if (NTD->isFormallyResilient())
          return ResilienceExpansion::Maximal;

        return ResilienceExpansion::Minimal;
      }
    }

    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(dc)) {
      // If the function is a nested function, we will serialize its body if
      // we serialize the parent's body.
      if (AFD->getDeclContext()->isLocalContext())
        continue;

      // FIXME: Make sure this method is never called on decls that have not
      // been fully validated.
      if (!AFD->hasAccess())
        break;

      auto funcAccess =
        AFD->getFormalAccessScope(/*useDC=*/nullptr,
                                  /*treatUsableFromInlineAsPublic=*/true);

      // If the function is not externally visible, we will not be serializing
      // its body.
      if (!funcAccess.isPublic())
        break;

      // Bodies of public transparent and always-inline functions are
      // serialized, so use conservative access patterns.
      if (AFD->isTransparent())
        return ResilienceExpansion::Minimal;

      if (AFD->getAttrs().hasAttribute<InlinableAttr>())
        return ResilienceExpansion::Minimal;

      if (auto attr = AFD->getAttrs().getAttribute<InlineAttr>())
        if (attr->getKind() == InlineKind::Always)
          return ResilienceExpansion::Minimal;

      // If a property or subscript is @inlinable, the accessors are
      // @inlinable also.
      if (auto accessor = dyn_cast<AccessorDecl>(AFD))
        if (accessor->getStorage()->getAttrs().getAttribute<InlinableAttr>())
          return ResilienceExpansion::Minimal;
    }
  }

  return ResilienceExpansion::Maximal;
}

/// Determine whether the innermost context is generic.
bool DeclContext::isInnermostContextGeneric() const {
  if (auto Decl = getAsDecl())
    if (auto GC = Decl->getAsGenericContext())
      return GC->isGeneric();
  return false;
}

bool
DeclContext::isCascadingContextForLookup(bool functionsAreNonCascading) const {
  // FIXME: This is explicitly checking for attributes in some cases because
  // it can be called before access control is computed.
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

  case DeclContextKind::AbstractFunctionDecl:
    if (functionsAreNonCascading)
      return false;
    break;

  case DeclContextKind::SubscriptDecl:
    break;

  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
    return true;

  case DeclContextKind::GenericTypeDecl:
    break;

  case DeclContextKind::ExtensionDecl:
    return true;
  }

  return getParent()->isCascadingContextForLookup(true);
}

unsigned DeclContext::getSyntacticDepth() const {
  // Module scope == depth 0.
  if (isModuleScopeContext())
    return 0;

  return 1 + getParent()->getSyntacticDepth();
}

unsigned DeclContext::getSemanticDepth() const {
  // For extensions, count the depth of the nominal type being extended.
  if (isa<ExtensionDecl>(this)) {
    if (auto nominal = getSelfNominalTypeDecl())
      return nominal->getSemanticDepth();

    return 1;
  }

  // Module scope == depth 0.
  if (isModuleScopeContext())
    return 0;

  return 1 + getParent()->getSemanticDepth();
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

void AccessScope::dump() const {
  llvm::errs() << getAccessLevelSpelling(accessLevelForDiagnostics()) << ": ";

  if (isPublic()) {
    llvm::errs() << "(null)\n";
    return;
  }

  if (auto *file = dyn_cast<SourceFile>(getDeclContext())) {
    llvm::errs() << "file '" << file->getFilename() << "'\n";
    return;
  }

  if (auto *decl = getDeclContext()->getAsDecl()) {
    llvm::errs() << Decl::getKindName(decl->getKind()) << " ";
    if (auto *ext = dyn_cast<ExtensionDecl>(decl))
      llvm::errs() << ext->getExtendedNominal()->getName();
    else if (auto *named = dyn_cast<ValueDecl>(decl))
      llvm::errs() << named->getFullName();
    else
      llvm::errs() << (const void *)decl;

    SourceLoc loc = decl->getLoc();
    if (loc.isValid()) {
      llvm::errs() << " at ";
      loc.print(llvm::errs(), decl->getASTContext().SourceMgr);
    }
    llvm::errs() << "\n";

    return;
  }

  // If all else fails, dump the DeclContext tree.
  getDeclContext()->printContext(llvm::errs());
}

template <typename DCType>
static unsigned getLineNumber(DCType *DC) {
  SourceLoc loc = DC->getLoc();
  if (loc.isInvalid())
    return 0;

  const ASTContext &ctx = static_cast<const DeclContext *>(DC)->getASTContext();
  return ctx.SourceMgr.getLineAndColumn(loc).first;
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
  OS.indent(Depth*2 + indent) << (void*)this << " " << Kind;

  switch (getContextKind()) {
  case DeclContextKind::Module:
    OS << " name=" << cast<ModuleDecl>(this)->getName();
    break;
  case DeclContextKind::FileUnit:
    switch (cast<FileUnit>(this)->getKind()) {
    case FileUnitKind::Builtin:
      OS << " Builtin";
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
    OS << " name=" << AFD->getFullName();
    if (AFD->hasInterfaceType())
      OS << " : " << AFD->getInterfaceType();
    else
      OS << " : (no type set)";
    break;
  }
  case DeclContextKind::SubscriptDecl: {
    auto *SD = cast<SubscriptDecl>(this);
    OS << " name=" << SD->getBaseName();
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

const Decl *
IterableDeclContext::getDecl() const {
  switch (getIterableContextKind()) {
  case IterableDeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(this);
    break;

  case IterableDeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(this);
    break;
  }
  llvm_unreachable("Unhandled IterableDeclContextKind in switch.");
}

ASTContext &IterableDeclContext::getASTContext() const {
  return getDecl()->getASTContext();
}

DeclRange IterableDeclContext::getCurrentMembersWithoutLoading() const {
  return DeclRange(FirstDeclAndLazyMembers.getPointer(), nullptr);
}

DeclRange IterableDeclContext::getMembers() const {
  loadAllMembers();

  return getCurrentMembersWithoutLoading();
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
  auto lazyMembers = FirstDeclAndLazyMembers.getInt() | LazyMembers::Present;
  FirstDeclAndLazyMembers.setInt(LazyMembers(lazyMembers));
  contextInfo->memberData = contextData;

  ++NumLazyIterableDeclContexts;
  ++NumUnloadedLazyIterableDeclContexts;
  // FIXME: (transitional) increment the redundant "always-on" counter.
  if (auto s = ctx.Stats) {
    ++s->getFrontendCounters().NumLazyIterableDeclContexts;
    ++s->getFrontendCounters().NumUnloadedLazyIterableDeclContexts;
  }
}

void IterableDeclContext::loadAllMembers() const {
  if (!hasLazyMembers())
    return;

  // Don't try to load all members re-entrant-ly.
  ASTContext &ctx = getASTContext();
  auto contextInfo = ctx.getOrCreateLazyIterableContextData(this,
    /*lazyLoader=*/nullptr);
  auto lazyMembers = FirstDeclAndLazyMembers.getInt() & ~LazyMembers::Present;
  FirstDeclAndLazyMembers.setInt(LazyMembers(lazyMembers));

  const Decl *container = getDecl();
  contextInfo->loader->loadAllMembers(const_cast<Decl *>(container),
                                      contextInfo->memberData);

  --NumUnloadedLazyIterableDeclContexts;
  // FIXME: (transitional) decrement the redundant "always-on" counter.
  if (auto s = ctx.Stats)
    s->getFrontendCounters().NumUnloadedLazyIterableDeclContexts--;
}

bool IterableDeclContext::wasDeserialized() const {
  const DeclContext *DC = cast<DeclContext>(getDecl());
  if (auto F = dyn_cast<FileUnit>(DC->getModuleScopeContext())) {
    return F->getKind() == FileUnitKind::SerializedAST;
  }
  return false;
}

bool IterableDeclContext::classof(const Decl *D) {
  switch (D->getKind()) {
  default: return false;
#define DECL(ID, PARENT) // See previous line
#define ITERABLE_DECL(ID, PARENT) \
  case DeclKind::ID: return true;
#include "swift/AST/DeclNodes.def"
  }
}

IterableDeclContext *
IterableDeclContext::castDeclToIterableDeclContext(const Decl *D) {
  switch (D->getKind()) {
  default: llvm_unreachable("Decl is not a IterableDeclContext.");
#define DECL(ID, PARENT) // See previous line
#define ITERABLE_DECL(ID, PARENT) \
  case DeclKind::ID: \
    return const_cast<IterableDeclContext *>( \
        static_cast<const IterableDeclContext*>(cast<ID##Decl>(D)));
#include "swift/AST/DeclNodes.def"
  }
}

/// Return the DeclContext to compare when checking private access in
/// Swift 4 mode. The context returned is the type declaration if the context
/// and the type declaration are in the same file, otherwise it is the types
/// last extension in the source file. If the context does not refer to a
/// declaration or extension, the supplied context is returned.
static const DeclContext *
getPrivateDeclContext(const DeclContext *DC, const SourceFile *useSF) {
  if (DC->getASTContext().isSwiftVersion3())
    return DC;

  auto NTD = DC->getSelfNominalTypeDecl();
  if (!NTD)
    return DC;

  // use the type declaration as the private scope if it is in the same
  // file as useSF. This occurs for both extensions and declarations.
  if (NTD->getParentSourceFile() == useSF)
    return NTD;

  // Otherwise use the last extension declaration in the same file.
  const DeclContext *lastExtension = nullptr;
  for (ExtensionDecl *ED : NTD->getExtensions())
    if (ED->getParentSourceFile() == useSF)
      lastExtension = ED;

  // If there's no last extension, return the supplied context.
  return lastExtension ? lastExtension : DC;
}

AccessScope::AccessScope(const DeclContext *DC, bool isPrivate)
    : Value(DC, isPrivate) {
  if (isPrivate) {
    DC = getPrivateDeclContext(DC, DC->getParentSourceFile());
    Value.setPointer(DC);
  }
  if (!DC || isa<ModuleDecl>(DC))
    assert(!isPrivate && "public or internal scope can't be private");
}

bool AccessScope::isFileScope() const {
  auto DC = getDeclContext();
  return DC && isa<FileUnit>(DC);
}

AccessLevel AccessScope::accessLevelForDiagnostics() const {
  if (isPublic())
    return AccessLevel::Public;
  if (isa<ModuleDecl>(getDeclContext()))
    return AccessLevel::Internal;
  if (getDeclContext()->isModuleScopeContext()) {
    return isPrivate() ? AccessLevel::Private : AccessLevel::FilePrivate;
  }

  return AccessLevel::Private;
}

bool AccessScope::allowsPrivateAccess(const DeclContext *useDC, const DeclContext *sourceDC) {
  // Check the lexical scope.
  if (useDC->isChildContextOf(sourceDC))
    return true;

  // Only check lexical scope in Swift 3 mode
  if (useDC->getASTContext().isSwiftVersion3())
    return false;

  // Do not allow access if the sourceDC is in a different file
  auto useSF = useDC->getParentSourceFile();
  if (useSF != sourceDC->getParentSourceFile())
    return false;

  // Do not allow access if the sourceDC does not represent a type.
  auto sourceNTD = sourceDC->getSelfNominalTypeDecl();
  if (!sourceNTD)
    return false;

  // Compare the private scopes and iterate over the parent types.
  sourceDC = getPrivateDeclContext(sourceDC, useSF);
  while (!useDC->isModuleContext()) {
    useDC = getPrivateDeclContext(useDC, useSF);
    if (useDC == sourceDC)
      return true;

    // Get the parent type. If the context represents a type, look at the types
    // declaring context instead of the contexts parent. This will crawl up
    // the type hierarchy in nested extensions correctly.
    if (auto NTD = useDC->getSelfNominalTypeDecl())
      useDC = NTD->getDeclContext();
    else
      useDC = useDC->getParent();
  }

  return false;
}

DeclContext *Decl::getDeclContextForModule() const {
  if (auto module = dyn_cast<ModuleDecl>(this))
    return const_cast<ModuleDecl *>(module);

  return nullptr;
}

DeclContextKind DeclContext::getContextKind() const {
  switch (ParentAndKind.getInt()) {
  case ASTHierarchy::Expr:
    return DeclContextKind::AbstractClosureExpr;
  case ASTHierarchy::Initializer:
    return DeclContextKind::Initializer;
  case ASTHierarchy::SerializedLocal:
    return DeclContextKind::SerializedLocal;
  case ASTHierarchy::FileUnit:
    return DeclContextKind::FileUnit;
  case ASTHierarchy::Decl: {
    auto decl = reinterpret_cast<const Decl*>(this + 1);
    if (isa<AbstractFunctionDecl>(decl))
      return DeclContextKind::AbstractFunctionDecl;
    if (isa<GenericTypeDecl>(decl))
      return DeclContextKind::GenericTypeDecl;
    switch (decl->getKind()) {
    case DeclKind::Module:
      return DeclContextKind::Module;
    case DeclKind::TopLevelCode:
      return DeclContextKind::TopLevelCodeDecl;
    case DeclKind::Subscript:
      return DeclContextKind::SubscriptDecl;
    case DeclKind::Extension:
      return DeclContextKind::ExtensionDecl;
    default:
      llvm_unreachable("Unhandled Decl kind");
    }
  }
  }
  llvm_unreachable("Unhandled DeclContext ASTHierarchy");
}

#define DECL(Id, Parent) \
  static_assert(!std::is_base_of<DeclContext, Id##Decl>::value, \
                "Non-context Decl node has context?");
#define CONTEXT_DECL(Id, Parent) \
  static_assert(alignof(DeclContext) == alignof(Id##Decl), "Alignment error"); \
  static_assert(std::is_base_of<DeclContext, Id##Decl>::value, \
                "CONTEXT_DECL nodes must inherit from DeclContext");
#define CONTEXT_VALUE_DECL(Id, Parent) \
  static_assert(alignof(DeclContext) == alignof(Id##Decl), "Alignment error"); \
  static_assert(std::is_base_of<DeclContext, Id##Decl>::value, \
                "CONTEXT_VALUE_DECL nodes must inherit from DeclContext");
#include "swift/AST/DeclNodes.def"

#define EXPR(Id, Parent) \
  static_assert(!std::is_base_of<DeclContext, Id##Expr>::value, \
                "Non-context Expr node has context?");
#define CONTEXT_EXPR(Id, Parent) \
  static_assert(alignof(DeclContext) == alignof(Id##Expr), "Alignment error"); \
  static_assert(std::is_base_of<DeclContext, Id##Expr>::value, \
                "CONTEXT_EXPR nodes must inherit from DeclContext");
#include "swift/AST/ExprNodes.def"

#ifndef NDEBUG
// XXX -- static_cast is not static enough for use with static_assert().
// DO verify this by temporarily breaking a Decl or Expr.
// DO NOT assume that the compiler will emit this code blindly.
SWIFT_CONSTRUCTOR
static void verify_DeclContext_is_start_of_node() {
  auto decl = reinterpret_cast<Decl*>(0x1000 + sizeof(DeclContext));
#define DECL(Id, Parent)
#define CONTEXT_DECL(Id, Parent) \
  assert(reinterpret_cast<DeclContext*>(0x1000) == \
         static_cast<Id##Decl*>(decl));
#define CONTEXT_VALUE_DECL(Id, Parent) \
  assert(reinterpret_cast<DeclContext*>(0x1000) == \
         static_cast<Id##Decl*>(decl));
#include "swift/AST/DeclNodes.def"

  auto expr = reinterpret_cast<Expr*>(0x1000 + sizeof(DeclContext));
#define EXPR(Id, Parent)
#define CONTEXT_EXPR(Id, Parent) \
  assert(reinterpret_cast<DeclContext*>(0x1000) == \
         static_cast<Id##Expr*>(expr));
#include "swift/AST/ExprNodes.def"
}
#endif
