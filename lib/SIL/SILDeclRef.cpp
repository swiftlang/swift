//===--- SILDeclRef.cpp - Implements SILDeclRef ---------------------------===//
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

#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILLocation.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/SILLinkage.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
using namespace swift;

/// Get the method dispatch mechanism for a method.
MethodDispatch
swift::getMethodDispatch(AbstractFunctionDecl *method) {
  // Some methods are forced to be statically dispatched.
  if (method->hasForcedStaticDispatch())
    return MethodDispatch::Static;

  // Import-as-member declarations are always statically referenced.
  if (method->isImportAsMember())
    return MethodDispatch::Static;

  auto dc = method->getDeclContext();

  if (dc->getSelfClassDecl()) {
    if (method->isObjCDynamic()) {
      return MethodDispatch::Class;
    }

    // Final methods can be statically referenced.
    if (method->isFinal())
      return MethodDispatch::Static;

    // Imported class methods are dynamically dispatched.
    if (method->isObjC() && method->hasClangNode())
      return MethodDispatch::Class;

    // Members defined directly inside a class are dynamically dispatched.
    if (isa<ClassDecl>(dc)) {
      // Native convenience initializers are not dynamically dispatched unless
      // required.
      if (auto ctor = dyn_cast<ConstructorDecl>(method)) {
        if (!ctor->isRequired() && !ctor->isDesignatedInit()
            && !requiresForeignEntryPoint(ctor))
          return MethodDispatch::Static;
      }
      return MethodDispatch::Class;
    }
  }

  // Otherwise, it can be referenced statically.
  return MethodDispatch::Static;
}

bool swift::requiresForeignToNativeThunk(ValueDecl *vd) {
  // Functions imported from C, Objective-C methods imported from Objective-C,
  // as well as methods in @objc protocols (even protocols defined in Swift)
  // require a foreign to native thunk.
  auto dc = vd->getDeclContext();
  if (auto proto = dyn_cast<ProtocolDecl>(dc))
    if (proto->isObjC())
      return true;

  if (auto fd = dyn_cast<FuncDecl>(vd))
    return fd->hasClangNode();

  return false;
}

bool swift::requiresForeignEntryPoint(ValueDecl *vd) {
  assert(!isa<AbstractStorageDecl>(vd));

  if (vd->isObjCDynamic()) {
    return true;
  }

  if (vd->isObjC() && isa<ProtocolDecl>(vd->getDeclContext()))
    return true;

  if (vd->isImportAsMember())
    return true;

  if (vd->hasClangNode())
    return true;

  if (auto *accessor = dyn_cast<AccessorDecl>(vd)) {
    // Property accessors should be generated alongside the property.
    if (accessor->isGetterOrSetter()) {
      auto *asd = accessor->getStorage();
      if (asd->isObjC() && asd->hasClangNode())
        return true;
    }
  }

  return false;
}

SILDeclRef::SILDeclRef(ValueDecl *vd, SILDeclRef::Kind kind,
                       bool isCurried, bool isForeign)
  : loc(vd), kind(kind),
    isCurried(isCurried), isForeign(isForeign),
    isDirectReference(0), defaultArgIndex(0)
{}

SILDeclRef::SILDeclRef(SILDeclRef::Loc baseLoc,
                       bool isCurried, bool asForeign) 
  : isCurried(isCurried), isDirectReference(0), defaultArgIndex(0)
{
  if (auto *vd = baseLoc.dyn_cast<ValueDecl*>()) {
    if (auto *fd = dyn_cast<FuncDecl>(vd)) {
      // Map FuncDecls directly to Func SILDeclRefs.
      loc = fd;
      kind = Kind::Func;
    }
    // Map ConstructorDecls to the Allocator SILDeclRef of the constructor.
    else if (auto *cd = dyn_cast<ConstructorDecl>(vd)) {
      loc = cd;
      kind = Kind::Allocator;
    }
    // Map EnumElementDecls to the EnumElement SILDeclRef of the element.
    else if (auto *ed = dyn_cast<EnumElementDecl>(vd)) {
      loc = ed;
      kind = Kind::EnumElement;
    }
    // VarDecl constants require an explicit kind.
    else if (isa<VarDecl>(vd)) {
      llvm_unreachable("must create SILDeclRef for VarDecl with explicit kind");
    }
    // Map DestructorDecls to the Deallocator of the destructor.
    else if (auto dtor = dyn_cast<DestructorDecl>(vd)) {
      loc = dtor;
      kind = Kind::Deallocator;
    }
    else {
      llvm_unreachable("invalid loc decl for SILDeclRef!");
    }
  } else if (auto *ACE = baseLoc.dyn_cast<AbstractClosureExpr *>()) {
    loc = ACE;
    kind = Kind::Func;
  } else {
    llvm_unreachable("impossible SILDeclRef loc");
  }

  isForeign = asForeign;
}

Optional<AnyFunctionRef> SILDeclRef::getAnyFunctionRef() const {
  if (auto vd = loc.dyn_cast<ValueDecl*>()) {
    if (auto afd = dyn_cast<AbstractFunctionDecl>(vd)) {
      return AnyFunctionRef(afd);
    } else {
      return None;
    }
  }
  return AnyFunctionRef(loc.get<AbstractClosureExpr*>());
}

bool SILDeclRef::isThunk() const {
  return isCurried || isForeignToNativeThunk() || isNativeToForeignThunk();
}

bool SILDeclRef::isClangImported() const {
  if (!hasDecl())
    return false;

  ValueDecl *d = getDecl();
  DeclContext *moduleContext = d->getDeclContext()->getModuleScopeContext();

  if (isa<ClangModuleUnit>(moduleContext)) {
    if (isClangGenerated())
      return true;

    if (isa<ConstructorDecl>(d) || isa<EnumElementDecl>(d))
      return !isForeign;

    if (auto *FD = dyn_cast<FuncDecl>(d))
      if (isa<AccessorDecl>(FD) ||
          isa<NominalTypeDecl>(d->getDeclContext()))
        return !isForeign;
  }
  return false;
}

bool SILDeclRef::isClangGenerated() const {
  if (!hasDecl())
    return false;

  return isClangGenerated(getDecl()->getClangNode());
}

// FIXME: this is a weird predicate.
bool SILDeclRef::isClangGenerated(ClangNode node) {
  if (auto nd = dyn_cast_or_null<clang::NamedDecl>(node.getAsDecl())) {
    // ie, 'static inline' functions for which we must ask Clang to emit a body
    // for explicitly
    if (!nd->isExternallyVisible())
      return true;
  }

  return false;
}

bool SILDeclRef::isImplicit() const {
  if (hasDecl())
    return getDecl()->isImplicit();
  return getAbstractClosureExpr()->isImplicit();
}

SILLinkage SILDeclRef::getLinkage(ForDefinition_t forDefinition) const {
  if (getAbstractClosureExpr()) {
    return isSerialized() ? SILLinkage::Shared : SILLinkage::Private;
  }

  // Add External to the linkage (e.g. Public -> PublicExternal) if this is a
  // declaration not a definition.
  auto maybeAddExternal = [&](SILLinkage linkage) {
    return forDefinition ? linkage : addExternalToLinkage(linkage);
  };

  // Native function-local declarations have shared linkage.
  // FIXME: @objc declarations should be too, but we currently have no way
  // of marking them "used" other than making them external. 
  ValueDecl *d = getDecl();
  DeclContext *moduleContext = d->getDeclContext();
  while (!moduleContext->isModuleScopeContext()) {
    if (!isForeign && moduleContext->isLocalContext()) {
      return isSerialized() ? SILLinkage::Shared : SILLinkage::Private;
    }
    moduleContext = moduleContext->getParent();
  }

  // Enum constructors and curry thunks either have private or shared
  // linkage, dependings are essentially the same as thunks, they are
  // emitted by need and have shared linkage.
  if (isEnumElement() || isCurried) {
    switch (d->getEffectiveAccess()) {
    case AccessLevel::Private:
    case AccessLevel::FilePrivate:
      return maybeAddExternal(SILLinkage::Private);

    case AccessLevel::Internal:
    case AccessLevel::Public:
    case AccessLevel::Open:
      return SILLinkage::Shared;
    }
  }

  // Calling convention thunks have shared linkage.
  if (isForeignToNativeThunk())
    return SILLinkage::Shared;

  // If a function declares a @_cdecl name, its native-to-foreign thunk
  // is exported with the visibility of the function.
  if (isNativeToForeignThunk() && !d->getAttrs().hasAttribute<CDeclAttr>())
    return SILLinkage::Shared;

  // Declarations imported from Clang modules have shared linkage.
  if (isClangImported())
    return SILLinkage::Shared;

  // Default argument generators of Public functions have PublicNonABI linkage
  // if the function was type-checked in Swift 4 mode.
  if (kind == SILDeclRef::Kind::DefaultArgGenerator) {
    if (isSerialized())
      return maybeAddExternal(SILLinkage::PublicNonABI);
  }

  enum class Limit {
    /// No limit.
    None,
    /// The declaration is emitted on-demand; it should end up with internal
    /// or shared linkage.
    OnDemand,
    /// The declaration should never be made public.
    NeverPublic,
    /// The declaration should always be emitted into the client,
    AlwaysEmitIntoClient,
  };
  auto limit = Limit::None;

  // @_alwaysEmitIntoClient declarations are like the default arguments of
  // public functions; they are roots for dead code elimination and have
  // serialized bodies, but no public symbol in the generated binary.
  if (d->getAttrs().hasAttribute<AlwaysEmitIntoClientAttr>())
    limit = Limit::AlwaysEmitIntoClient;
  if (auto accessor = dyn_cast<AccessorDecl>(d)) {
    auto *storage = accessor->getStorage();
    if (storage->getAttrs().hasAttribute<AlwaysEmitIntoClientAttr>())
      limit = Limit::AlwaysEmitIntoClient;
  }

  // ivar initializers and destroyers are completely contained within the class
  // from which they come, and never get seen externally.
  if (isIVarInitializerOrDestroyer()) {
    limit = Limit::NeverPublic;
  }

  // The property wrapper backing initializer is never public for resilient
  // properties.
  if (kind == SILDeclRef::Kind::PropertyWrapperBackingInitializer) {
    if (cast<VarDecl>(d)->isResilient())
      limit = Limit::NeverPublic;
  }

  // Stored property initializers get the linkage of their containing type.
  if (isStoredPropertyInitializer()) {
    // Three cases:
    //
    // 1) Type is formally @_fixed_layout/@frozen. Root initializers can be
    //    declared @inlinable. The property initializer must only reference
    //    public symbols, and is serialized, so we give it PublicNonABI linkage.
    //
    // 2) Type is not formally @_fixed_layout/@frozen and the module is not
    //    resilient. Root initializers can be declared @inlinable. This is the 
    //    annoying case. We give the initializer public linkage if the type is
    //    public.
    //
    // 3) Type is resilient. The property initializer is never public because
    //    root initializers cannot be @inlinable.
    //
    // FIXME: Get rid of case 2 somehow.
    if (isSerialized())
      return maybeAddExternal(SILLinkage::PublicNonABI);

    d = cast<NominalTypeDecl>(d->getDeclContext());

    // FIXME: This should always be true.
    if (d->getModuleContext()->isResilient())
      limit = Limit::NeverPublic;
  }

  // The global addressor is never public for resilient globals.
  if (kind == Kind::GlobalAccessor) {
    if (cast<VarDecl>(d)->isResilient()) {
      limit = Limit::NeverPublic;
    }
  }

  // Forced-static-dispatch functions are created on-demand and have
  // at best shared linkage.
  if (auto fn = dyn_cast<FuncDecl>(d)) {
    if (fn->hasForcedStaticDispatch()) {
      limit = Limit::OnDemand;
    }
  }
  
  auto effectiveAccess = d->getEffectiveAccess();
  
  // Private setter implementations for an internal storage declaration should
  // be internal as well, so that a dynamically-writable
  // keypath can be formed from other files.
  if (auto accessor = dyn_cast<AccessorDecl>(d)) {
    if (accessor->isSetter()
       && accessor->getStorage()->getEffectiveAccess() == AccessLevel::Internal)
      effectiveAccess = AccessLevel::Internal;
  }

  switch (effectiveAccess) {
  case AccessLevel::Private:
  case AccessLevel::FilePrivate:
    return maybeAddExternal(SILLinkage::Private);

  case AccessLevel::Internal:
    if (limit == Limit::OnDemand)
      return SILLinkage::Shared;
    return maybeAddExternal(SILLinkage::Hidden);

  case AccessLevel::Public:
  case AccessLevel::Open:
    if (limit == Limit::OnDemand)
      return SILLinkage::Shared;
    if (limit == Limit::NeverPublic)
      return maybeAddExternal(SILLinkage::Hidden);
    if (limit == Limit::AlwaysEmitIntoClient)
      return maybeAddExternal(SILLinkage::PublicNonABI);
    return maybeAddExternal(SILLinkage::Public);
  }
  llvm_unreachable("unhandled access");
}

SILDeclRef SILDeclRef::getDefaultArgGenerator(Loc loc,
                                              unsigned defaultArgIndex) {
  SILDeclRef result;
  result.loc = loc;
  result.kind = Kind::DefaultArgGenerator;
  result.defaultArgIndex = defaultArgIndex;
  return result;
}

bool SILDeclRef::hasClosureExpr() const {
  return loc.is<AbstractClosureExpr *>()
    && isa<ClosureExpr>(getAbstractClosureExpr());
}

bool SILDeclRef::hasAutoClosureExpr() const {
  return loc.is<AbstractClosureExpr *>()
    && isa<AutoClosureExpr>(getAbstractClosureExpr());
}

bool SILDeclRef::hasFuncDecl() const {
  return loc.is<ValueDecl *>() && isa<FuncDecl>(getDecl());
}

ClosureExpr *SILDeclRef::getClosureExpr() const {
  return dyn_cast<ClosureExpr>(getAbstractClosureExpr());
}
AutoClosureExpr *SILDeclRef::getAutoClosureExpr() const {
  return dyn_cast<AutoClosureExpr>(getAbstractClosureExpr());
}

FuncDecl *SILDeclRef::getFuncDecl() const {
  return dyn_cast<FuncDecl>(getDecl());
}

bool SILDeclRef::isSetter() const {
  if (!hasDecl())
    return false;
  if (auto accessor = dyn_cast<AccessorDecl>(getDecl()))
    return accessor->isSetter();
  return false;
}

AbstractFunctionDecl *SILDeclRef::getAbstractFunctionDecl() const {
  return dyn_cast<AbstractFunctionDecl>(getDecl());
}

/// True if the function should be treated as transparent.
bool SILDeclRef::isTransparent() const {
  if (isEnumElement())
    return true;

  if (isStoredPropertyInitializer())
    return true;

  if (hasAutoClosureExpr())
    return true;

  if (hasDecl()) {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(getDecl()))
      return AFD->isTransparent();

    if (auto *ASD = dyn_cast<AbstractStorageDecl>(getDecl()))
      return ASD->isTransparent();
  }

  return false;
}

/// True if the function should have its body serialized.
IsSerialized_t SILDeclRef::isSerialized() const {
  DeclContext *dc;
  if (auto closure = getAbstractClosureExpr()) {
    dc = closure->getLocalContext();

    // Otherwise, ask the AST if we're inside an @inlinable context.
    if (dc->getResilienceExpansion() == ResilienceExpansion::Minimal) {
      if (isForeign)
        return IsSerializable;

      return IsSerialized;
    }

    return IsNotSerialized;
  }

  if (isIVarInitializerOrDestroyer())
    return IsNotSerialized;

  auto *d = getDecl();

  // Default argument generators are serialized if the containing
  // declaration is public.
  if (isDefaultArgGenerator()) {
    auto scope =
      d->getFormalAccessScope(/*useDC=*/nullptr,
                              /*treatUsableFromInlineAsPublic=*/true);

    if (scope.isPublic())
      return IsSerialized;
    return IsNotSerialized;
  }

  // Stored property initializers are inlinable if the type is explicitly
  // marked as @frozen.
  if (isStoredPropertyInitializer()) {
    auto *nominal = cast<NominalTypeDecl>(d->getDeclContext());
    auto scope =
      nominal->getFormalAccessScope(/*useDC=*/nullptr,
                                    /*treatUsableFromInlineAsPublic=*/true);
    if (!scope.isPublic())
      return IsNotSerialized;
    if (nominal->isFormallyResilient())
      return IsNotSerialized;
    return IsSerialized;
  }

  // Note: if 'd' is a function, then 'dc' is the function itself, not
  // its parent context.
  dc = d->getInnermostDeclContext();

  // Local functions are serializable if their parent function is
  // serializable.
  if (d->getDeclContext()->isLocalContext()) {
    if (dc->getResilienceExpansion() == ResilienceExpansion::Minimal)
      return IsSerializable;

    return IsNotSerialized;
  }

  // Anything else that is not public is not serializable.
  if (d->getEffectiveAccess() < AccessLevel::Public)
    return IsNotSerialized;

  // 'read' and 'modify' accessors synthesized on-demand are serialized if
  // visible outside the module.
  if (auto fn = dyn_cast<FuncDecl>(d))
    if (!isClangImported() &&
        fn->hasForcedStaticDispatch())
      return IsSerialized;

  // Enum element constructors are serializable if the enum is
  // @usableFromInline or public.
  if (isEnumElement())
    return IsSerializable;

  // Currying thunks are serialized if referenced from an inlinable
  // context -- Sema's semantic checks ensure the serialization of
  // such a thunk is valid, since it must in turn reference a public
  // symbol, or dispatch via class_method or witness_method.
  if (isCurried)
    return IsSerializable;

  if (isForeignToNativeThunk())
    return IsSerializable;

  // The allocating entry point for designated initializers are serialized
  // if the class is @usableFromInline or public.
  if (kind == SILDeclRef::Kind::Allocator) {
    auto *ctor = cast<ConstructorDecl>(d);
    if (ctor->isDesignatedInit() &&
        ctor->getDeclContext()->getSelfClassDecl()) {
      if (!ctor->hasClangNode())
        return IsSerialized;
    }
  }

  if (isForeign) {
    // @objc thunks for methods are not serializable since they're only
    // referenced from the method table.
    if (d->getDeclContext()->isTypeContext())
      return IsNotSerialized;

    // @objc thunks for top-level functions are serializable since they're
    // referenced from @convention(c) conversions inside inlinable
    // functions.
    return IsSerializable;
  }

  // Declarations imported from Clang modules are serialized if
  // referenced from an inlinable context.
  if (isClangImported())
    return IsSerializable;

  // Otherwise, ask the AST if we're inside an @inlinable context.
  if (dc->getResilienceExpansion() == ResilienceExpansion::Minimal)
    return IsSerialized;

  return IsNotSerialized;
}

/// True if the function has an @inline(never) attribute.
bool SILDeclRef::isNoinline() const {
  if (!hasDecl())
    return false;

  auto *decl = getDecl();
  if (auto *attr = decl->getAttrs().getAttribute<InlineAttr>())
    if (attr->getKind() == InlineKind::Never)
      return true;

  if (auto *accessorDecl = dyn_cast<AccessorDecl>(decl)) {
    auto *storage = accessorDecl->getStorage();
    if (auto *attr = storage->getAttrs().getAttribute<InlineAttr>())
      if (attr->getKind() == InlineKind::Never)
        return true;
  }

  return false;
}

/// True if the function has the @inline(__always) attribute.
bool SILDeclRef::isAlwaysInline() const {
  if (!hasDecl())
    return false;

  auto *decl = getDecl();
  if (auto attr = decl->getAttrs().getAttribute<InlineAttr>())
    if (attr->getKind() == InlineKind::Always)
      return true;

  if (auto *accessorDecl = dyn_cast<AccessorDecl>(decl)) {
    auto *storage = accessorDecl->getStorage();
    if (auto *attr = storage->getAttrs().getAttribute<InlineAttr>())
      if (attr->getKind() == InlineKind::Always)
        return true;
  }

  return false;
}

bool SILDeclRef::hasEffectsAttribute() const {
  if (!hasDecl())
    return false;
  return getDecl()->getAttrs().hasAttribute<EffectsAttr>();
}

EffectsKind SILDeclRef::getEffectsAttribute() const {
  assert(hasEffectsAttribute());
  EffectsAttr *MA = getDecl()->getAttrs().getAttribute<EffectsAttr>();
  return MA->getKind();
}

bool SILDeclRef::isForeignToNativeThunk() const {
  // Non-decl entry points are never natively foreign, so they would never
  // have a foreign-to-native thunk.
  if (!hasDecl())
    return false;
  if (requiresForeignToNativeThunk(getDecl()))
    return !isForeign;
  // ObjC initializing constructors and factories are foreign.
  // We emit a special native allocating constructor though.
  if (isa<ConstructorDecl>(getDecl())
      && (kind == Kind::Initializer
          || cast<ConstructorDecl>(getDecl())->isFactoryInit())
      && getDecl()->hasClangNode())
    return !isForeign;
  return false;
}

bool SILDeclRef::isNativeToForeignThunk() const {
  // We can have native-to-foreign thunks over closures.
  if (!hasDecl())
    return isForeign;
  // We can have native-to-foreign thunks over global or local native functions.
  // TODO: Static functions too.
  if (auto func = dyn_cast<FuncDecl>(getDecl())) {
    if (!func->getDeclContext()->isTypeContext()
        && !func->hasClangNode())
      return isForeign;
  }
  return false;
}

/// Use the Clang importer to mangle a Clang declaration.
static void mangleClangDecl(raw_ostream &buffer,
                            const clang::NamedDecl *clangDecl,
                            ASTContext &ctx) {
  auto *importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
  importer->getMangledName(buffer, clangDecl);
}

std::string SILDeclRef::mangle(ManglingKind MKind) const {
  using namespace Mangle;
  ASTMangler mangler;

  // As a special case, Clang functions and globals don't get mangled at all.
  if (hasDecl()) {
    if (auto clangDecl = getDecl()->getClangDecl()) {
      if (!isForeignToNativeThunk() && !isNativeToForeignThunk()
          && !isCurried) {
        if (auto namedClangDecl = dyn_cast<clang::DeclaratorDecl>(clangDecl)) {
          if (auto asmLabel = namedClangDecl->getAttr<clang::AsmLabelAttr>()) {
            std::string s(1, '\01');
            s += asmLabel->getLabel();
            return s;
          } else if (namedClangDecl->hasAttr<clang::OverloadableAttr>() ||
                     getDecl()->getASTContext().LangOpts.EnableCXXInterop) {
            std::string storage;
            llvm::raw_string_ostream SS(storage);
            mangleClangDecl(SS, namedClangDecl, getDecl()->getASTContext());
            return SS.str();
          }
          return namedClangDecl->getName();
        }
      }
    }
  }

  ASTMangler::SymbolKind SKind = ASTMangler::SymbolKind::Default;
  switch (MKind) {
    case SILDeclRef::ManglingKind::Default:
      if (isForeign) {
        SKind = ASTMangler::SymbolKind::SwiftAsObjCThunk;
      } else if (isDirectReference) {
        SKind = ASTMangler::SymbolKind::DirectMethodReferenceThunk;
      } else if (isForeignToNativeThunk()) {
        SKind = ASTMangler::SymbolKind::ObjCAsSwiftThunk;
      }
      break;
    case SILDeclRef::ManglingKind::DynamicThunk:
      SKind = ASTMangler::SymbolKind::DynamicThunk;
      break;
  }

  switch (kind) {
  case SILDeclRef::Kind::Func:
    if (!hasDecl())
      return mangler.mangleClosureEntity(getAbstractClosureExpr(), SKind);

    // As a special case, functions can have manually mangled names.
    // Use the SILGen name only for the original non-thunked, non-curried entry
    // point.
    if (auto NameA = getDecl()->getAttrs().getAttribute<SILGenNameAttr>())
      if (!NameA->Name.empty() &&
          !isForeignToNativeThunk() && !isNativeToForeignThunk()
          && !isCurried) {
        return NameA->Name;
      }
      
    // Use a given cdecl name for native-to-foreign thunks.
    if (auto CDeclA = getDecl()->getAttrs().getAttribute<CDeclAttr>())
      if (isNativeToForeignThunk()) {
        return CDeclA->Name;
      }

    // Otherwise, fall through into the 'other decl' case.
    LLVM_FALLTHROUGH;

  case SILDeclRef::Kind::EnumElement:
    return mangler.mangleEntity(getDecl(), isCurried, SKind);

  case SILDeclRef::Kind::Deallocator:
    assert(!isCurried);
    return mangler.mangleDestructorEntity(cast<DestructorDecl>(getDecl()),
                                          /*isDeallocating*/ true,
                                          SKind);

  case SILDeclRef::Kind::Destroyer:
    assert(!isCurried);
    return mangler.mangleDestructorEntity(cast<DestructorDecl>(getDecl()),
                                          /*isDeallocating*/ false,
                                          SKind);

  case SILDeclRef::Kind::Allocator:
    return mangler.mangleConstructorEntity(cast<ConstructorDecl>(getDecl()),
                                           /*allocating*/ true,
                                           isCurried,
                                           SKind);

  case SILDeclRef::Kind::Initializer:
    return mangler.mangleConstructorEntity(cast<ConstructorDecl>(getDecl()),
                                           /*allocating*/ false,
                                           isCurried,
                                           SKind);

  case SILDeclRef::Kind::IVarInitializer:
  case SILDeclRef::Kind::IVarDestroyer:
    assert(!isCurried);
    return mangler.mangleIVarInitDestroyEntity(cast<ClassDecl>(getDecl()),
                                  kind == SILDeclRef::Kind::IVarDestroyer,
                                  SKind);

  case SILDeclRef::Kind::GlobalAccessor:
    assert(!isCurried);
    return mangler.mangleAccessorEntity(AccessorKind::MutableAddress,
                                        cast<AbstractStorageDecl>(getDecl()),
                                        /*isStatic*/ false,
                                        SKind);

  case SILDeclRef::Kind::DefaultArgGenerator:
    assert(!isCurried);
    return mangler.mangleDefaultArgumentEntity(
                                        cast<DeclContext>(getDecl()),
                                        defaultArgIndex,
                                        SKind);

  case SILDeclRef::Kind::StoredPropertyInitializer:
    assert(!isCurried);
    return mangler.mangleInitializerEntity(cast<VarDecl>(getDecl()), SKind);

  case SILDeclRef::Kind::PropertyWrapperBackingInitializer:
    assert(!isCurried);
    return mangler.mangleBackingInitializerEntity(cast<VarDecl>(getDecl()),
                                                  SKind);
  }

  llvm_unreachable("bad entity kind!");
}

bool SILDeclRef::requiresNewVTableEntry() const {
  if (cast<AbstractFunctionDecl>(getDecl())->needsNewVTableEntry())
    return true;
  return false;
}

bool SILDeclRef::requiresNewWitnessTableEntry() const {
  return requiresNewWitnessTableEntry(cast<AbstractFunctionDecl>(getDecl()));
}

bool SILDeclRef::requiresNewWitnessTableEntry(AbstractFunctionDecl *func) {
  return func->getOverriddenDecls().empty();
}

SILDeclRef SILDeclRef::getOverridden() const {
  if (!hasDecl())
    return SILDeclRef();
  auto overridden = getDecl()->getOverriddenDecl();
  if (!overridden)
    return SILDeclRef();

  return SILDeclRef(overridden, kind, isCurried);
}

SILDeclRef SILDeclRef::getNextOverriddenVTableEntry() const {
  if (auto overridden = getOverridden()) {
    // If we overrode a foreign decl or dynamic method, if this is an
    // accessor for a property that overrides an ObjC decl, or if it is an
    // @NSManaged property, then it won't be in the vtable.
    if (overridden.getDecl()->hasClangNode())
      return SILDeclRef();
    
    // An @objc convenience initializer can be "overridden" in the sense that
    // its selector is reclaimed by a subclass's convenience init with the
    // same name. The AST models this as an override for the purposes of
    // ObjC selector validation, but it isn't for Swift method dispatch
    // purposes.
    if (overridden.kind == SILDeclRef::Kind::Allocator) {
      auto overriddenCtor = cast<ConstructorDecl>(overridden.getDecl());
      if (!overriddenCtor->isDesignatedInit()
          && !overriddenCtor->isRequired())
        return SILDeclRef();
    }

    // Initializing entry points for initializers won't be in the vtable.
    // For Swift designated initializers, they're only used in super.init
    // chains, which can always be statically resolved. Other native Swift
    // initializers only have allocating entry points. ObjC initializers always
    // have the initializing entry point (corresponding to the -init method)
    // but those are never in the vtable.
    if (overridden.kind == SILDeclRef::Kind::Initializer) {
      return SILDeclRef();
    }

    // Overrides of @objc dynamic declarations are not in the vtable.
    if (overridden.getDecl()->isObjCDynamic()) {
      return SILDeclRef();
    }
    
    if (auto *accessor = dyn_cast<AccessorDecl>(overridden.getDecl())) {
      auto *asd = accessor->getStorage();
      if (asd->hasClangNode())
        return SILDeclRef();
      if (asd->isObjCDynamic()) {
        return SILDeclRef();
      }
    }

    // If we overrode a decl from an extension, it won't be in a vtable
    // either. This can occur for extensions to ObjC classes.
    if (isa<ExtensionDecl>(overridden.getDecl()->getDeclContext()))
      return SILDeclRef();

    return overridden;
  }
  return SILDeclRef();
}

SILDeclRef SILDeclRef::getOverriddenWitnessTableEntry() const {
  auto bestOverridden =
    getOverriddenWitnessTableEntry(cast<AbstractFunctionDecl>(getDecl()));
  return SILDeclRef(bestOverridden, kind, isCurried);
}

AbstractFunctionDecl *SILDeclRef::getOverriddenWitnessTableEntry(
                                                 AbstractFunctionDecl *func) {
  if (!isa<ProtocolDecl>(func->getDeclContext()))
    return func;

  AbstractFunctionDecl *bestOverridden = nullptr;

  SmallVector<AbstractFunctionDecl *, 4> stack;
  SmallPtrSet<AbstractFunctionDecl *, 4> visited;
  stack.push_back(func);
  visited.insert(func);

  while (!stack.empty()) {
    auto current = stack.back();
    stack.pop_back();

    auto overriddenDecls = current->getOverriddenDecls();
    if (overriddenDecls.empty()) {
      // This entry introduced a witness table entry. Determine whether it is
      // better than the best entry we've seen thus far.
      if (!bestOverridden ||
          ProtocolDecl::compare(
                        cast<ProtocolDecl>(current->getDeclContext()),
                        cast<ProtocolDecl>(bestOverridden->getDeclContext()))
            < 0) {
        bestOverridden = cast<AbstractFunctionDecl>(current);
      }

      continue;
    }

    // Add overridden declarations to the stack.
    for (auto overridden : overriddenDecls) {
      auto overriddenFunc = cast<AbstractFunctionDecl>(overridden);
      if (visited.insert(overriddenFunc).second)
        stack.push_back(overriddenFunc);
    }
  }

  return bestOverridden;
}

SILDeclRef SILDeclRef::getOverriddenVTableEntry() const {
  SILDeclRef cur = *this, next = *this;
  do {
    cur = next;
    if (cur.requiresNewVTableEntry())
      return cur;
    next = cur.getNextOverriddenVTableEntry();
  } while (next);

  return cur;
}

SILLocation SILDeclRef::getAsRegularLocation() const {
  if (hasDecl())
    return RegularLocation(getDecl());
  return RegularLocation(getAbstractClosureExpr());
}

SubclassScope SILDeclRef::getSubclassScope() const {
  if (!hasDecl())
    return SubclassScope::NotApplicable;

  auto *decl = getDecl();

  if (!isa<AbstractFunctionDecl>(decl))
    return SubclassScope::NotApplicable;

  // If this declaration is a function which goes into a vtable, then it's
  // symbol must be as visible as its class, because derived classes have to put
  // all less visible methods of the base class into their vtables.

  if (auto *CD = dyn_cast<ConstructorDecl>(decl)) {
    // Initializing entry points do not appear in the vtable.
    if (kind == SILDeclRef::Kind::Initializer)
      return SubclassScope::NotApplicable;
    // Non-required convenience inits do not apper in the vtable.
    if (!CD->isRequired() && !CD->isDesignatedInit())
      return SubclassScope::NotApplicable;
  } else if (isa<DestructorDecl>(decl)) {
    // Detructors do not appear in the vtable.
    return SubclassScope::NotApplicable;
  } else {
    assert(isa<FuncDecl>(decl));
  }

  DeclContext *context = decl->getDeclContext();

  // Methods from extensions don't go in the vtable.
  if (isa<ExtensionDecl>(context))
    return SubclassScope::NotApplicable;

  // Various forms of thunks don't either.
  if (isThunk() || isForeign)
    return SubclassScope::NotApplicable;

  // Default arg generators don't go in the vtable.
  if (isDefaultArgGenerator())
    return SubclassScope::NotApplicable;

  // Only non-final methods in non-final classes go in the vtable.
  auto *classType = context->getSelfClassDecl();
  if (!classType || classType->isFinal())
    return SubclassScope::NotApplicable;

  if (decl->isFinal())
    return SubclassScope::NotApplicable;

  assert(decl->getEffectiveAccess() <= classType->getEffectiveAccess() &&
         "class must be as visible as its members");

  // FIXME: This is too narrow. Any class with resilient metadata should
  // probably have this, at least for method overrides that don't add new
  // vtable entries.
  if (classType->isResilient()) {
    if (isa<ConstructorDecl>(decl))
      return SubclassScope::NotApplicable;
    return SubclassScope::Resilient;
  }

  switch (classType->getEffectiveAccess()) {
  case AccessLevel::Private:
  case AccessLevel::FilePrivate:
    return SubclassScope::NotApplicable;
  case AccessLevel::Internal:
  case AccessLevel::Public:
    return SubclassScope::Internal;
  case AccessLevel::Open:
    return SubclassScope::External;
  }

  llvm_unreachable("Unhandled access level in switch.");
}

unsigned SILDeclRef::getParameterListCount() const {
  if (isCurried || !hasDecl() || kind == Kind::DefaultArgGenerator)
    return 1;

  auto *vd = getDecl();

  if (isa<AbstractFunctionDecl>(vd) || isa<EnumElementDecl>(vd)) {
    // For functions and enum elements, the number of parameter lists is the
    // same as in their interface type.
    return vd->getNumCurryLevels();
  } else if (isa<ClassDecl>(vd)) {
    return 2;
  } else if (isa<VarDecl>(vd)) {
    return 1;
  } else {
    llvm_unreachable("Unhandled ValueDecl for SILDeclRef");
  }
}

static bool isDesignatedConstructorForClass(ValueDecl *decl) {
  if (auto *ctor = dyn_cast_or_null<ConstructorDecl>(decl))
    if (ctor->getDeclContext()->getSelfClassDecl())
      return ctor->isDesignatedInit();
  return false;
}

bool SILDeclRef::canBeDynamicReplacement() const {
  if (kind == SILDeclRef::Kind::Destroyer ||
      kind == SILDeclRef::Kind::DefaultArgGenerator)
    return false;
  if (kind == SILDeclRef::Kind::Initializer)
    return isDesignatedConstructorForClass(getDecl());
  if (kind == SILDeclRef::Kind::Allocator)
    return !isDesignatedConstructorForClass(getDecl());
  return true;
}

bool SILDeclRef::isDynamicallyReplaceable() const {
  if (kind == SILDeclRef::Kind::DefaultArgGenerator)
    return false;
  if (isStoredPropertyInitializer())
    return false;

  // Class allocators are not dynamic replaceable.
  if (kind == SILDeclRef::Kind::Allocator &&
      isDesignatedConstructorForClass(getDecl()))
    return false;

  if (kind == SILDeclRef::Kind::Destroyer ||
      (kind == SILDeclRef::Kind::Initializer &&
       !isDesignatedConstructorForClass(getDecl())) ||
      kind == SILDeclRef::Kind::GlobalAccessor) {
    return false;
  }

  if (!hasDecl())
    return false;

  auto decl = getDecl();
  return decl->isNativeDynamic();
}
