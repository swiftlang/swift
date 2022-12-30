//===--- TypeCheckTypeWrapper.cpp - type wrappers -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for type wrappers.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeWrappers.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"

using namespace swift;

static PatternBindingDecl *injectVariable(DeclContext *DC, Identifier name,
                                          Type type,
                                          VarDecl::Introducer introducer,
                                          Expr *initializer = nullptr) {
  auto &ctx = DC->getASTContext();

  auto *var = new (ctx) VarDecl(/*isStatic=*/false, introducer,
                                /*nameLoc=*/SourceLoc(), name, DC);

  var->setImplicit();
  var->setSynthesized();
  var->setInterfaceType(type);

  Pattern *pattern = NamedPattern::createImplicit(ctx, var);
  pattern->setType(type);

  pattern = TypedPattern::createImplicit(ctx, pattern, type);

  return PatternBindingDecl::createImplicit(ctx, StaticSpellingKind::None,
                                            pattern, initializer, DC);
}

/// Create a property declaration and inject it into the given type.
static VarDecl *injectProperty(NominalTypeDecl *parent, Identifier name,
                               Type type, VarDecl::Introducer introducer,
                               AccessLevel accessLevel,
                               Expr *initializer = nullptr) {
  auto *PBD = injectVariable(parent, name, type, introducer, initializer);
  auto *var = PBD->getSingleVar();

  var->setAccess(accessLevel);

  parent->addMember(PBD);
  parent->addMember(var);

  return var;
}

bool VarDecl::isAccessedViaTypeWrapper() const {
  auto *mutableSelf = const_cast<VarDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           IsPropertyAccessedViaTypeWrapper{mutableSelf},
                           false);
}

VarDecl *VarDecl::getUnderlyingTypeWrapperStorage() const {
  auto *mutableSelf = const_cast<VarDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           GetTypeWrapperStorageForProperty{mutableSelf},
                           nullptr);
}

static void
getDeclaredProtocolConformances(NominalTypeDecl *decl,
                                SmallVectorImpl<ProtocolDecl *> &protocols) {
  for (unsigned i : indices(decl->getInherited())) {
    auto inheritedType = evaluateOrDefault(
        decl->getASTContext().evaluator,
        InheritedTypeRequest{decl, i, TypeResolutionStage::Interface}, Type());

    if (!(inheritedType && inheritedType->isConstraintType()))
      continue;

    if (auto *protocol =
            dyn_cast_or_null<ProtocolDecl>(inheritedType->getAnyNominal())) {
      protocols.push_back(protocol);
    }

    if (auto composition = inheritedType->getAs<ProtocolCompositionType>()) {
      for (auto member : composition->getMembers()) {
        if (auto *protocol =
                dyn_cast_or_null<ProtocolDecl>(member->getAnyNominal())) {
          protocols.push_back(protocol);
        }
      }
    }
  }
}

static void getTypeWrappers(NominalTypeDecl *decl,
                            SmallVectorImpl<TypeWrapperInfo> &typeWrappers) {
  auto &ctx = decl->getASTContext();

  // Attributes applied directly to the type.
  for (auto *attr : decl->getAttrs().getAttributes<CustomAttr>()) {
    auto *mutableAttr = const_cast<CustomAttr *>(attr);
    auto *nominal = evaluateOrDefault(
        ctx.evaluator, CustomAttrNominalRequest{mutableAttr, decl}, nullptr);

    if (!nominal)
      continue;

    auto *typeWrapper = nominal->getAttrs().getAttribute<TypeWrapperAttr>();
    if (typeWrapper && typeWrapper->isValid())
      typeWrappers.push_back(
          {mutableAttr, nominal, decl, /*isInferred=*/false});
  }

  // Do not allow transitive protocol inference between protocols.
  if (isa<ProtocolDecl>(decl))
    return;

  // Attributes inferred from (explicit) protocol conformances
  // associated with the declaration of the type.
  SmallVector<ProtocolDecl *, 4> protocols;
  getDeclaredProtocolConformances(decl, protocols);

  for (auto *protocol : protocols) {
    SmallVector<TypeWrapperInfo, 2> inferredAttrs;
    getTypeWrappers(protocol, inferredAttrs);

    // De-duplicate inferred type wrappers. This also makes sure
    // that if both protocol and conforming type explicitly declare
    // the same type wrapper there is no clash between them.
    for (const auto &inferredAttr : inferredAttrs) {
      if (llvm::find_if(typeWrappers, [&](const TypeWrapperInfo &attr) {
            return attr.Wrapper == inferredAttr.Wrapper;
          }) == typeWrappers.end())
        typeWrappers.push_back(inferredAttr.asInferred());
    }
  }
}

Optional<TypeWrapperInfo>
GetTypeWrapper::evaluate(Evaluator &evaluator, NominalTypeDecl *decl) const {
  auto &ctx = decl->getASTContext();

  // Note that we don't actually care whether there are duplicates,
  // using the same type wrapper multiple times is still an error.
  SmallVector<TypeWrapperInfo, 2> typeWrappers;

  getTypeWrappers(decl, typeWrappers);

  if (typeWrappers.empty())
    return None;

  if (typeWrappers.size() != 1) {
    ctx.Diags.diagnose(decl, diag::cannot_use_multiple_type_wrappers,
                       decl->getDescriptiveKind(), decl->getName());

    for (const auto &entry : typeWrappers) {
      if (entry.AttachedTo == decl) {
        ctx.Diags.diagnose(entry.Attr->getLocation(), diag::decl_declared_here,
                           entry.Wrapper->getName());
      } else {
        ctx.Diags.diagnose(decl, diag::type_wrapper_inferred_from,
                           entry.Wrapper->getName(),
                           entry.AttachedTo->getDescriptiveKind(),
                           entry.AttachedTo->getName());
      }
    }

    return None;
  }

  return typeWrappers.front();
}

Type GetTypeWrapperType::evaluate(Evaluator &evaluator,
                                  NominalTypeDecl *decl) const {
  auto typeWrapperInfo = decl->getTypeWrapper();
  if (!typeWrapperInfo)
    return Type();

  auto type = evaluateOrDefault(
      evaluator,
      CustomAttrTypeRequest{typeWrapperInfo->Attr, decl->getDeclContext(),
                            CustomAttrTypeKind::TypeWrapper},
      Type());

  if (!type || type->hasError()) {
    return ErrorType::get(decl->getASTContext());
  }
  return type;
}

VarDecl *NominalTypeDecl::getTypeWrapperProperty() const {
  auto *mutableSelf = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           GetTypeWrapperProperty{mutableSelf}, nullptr);
}

ConstructorDecl *NominalTypeDecl::getTypeWrappedTypeStorageInitializer() const {
  auto *mutableSelf = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      SynthesizeTypeWrappedTypeStorageWrapperInitializer{mutableSelf}, nullptr);
}

ConstructorDecl *
NominalTypeDecl::getTypeWrappedTypeMemberwiseInitializer() const {
  auto *mutableSelf = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      SynthesizeTypeWrappedTypeMemberwiseInitializer{mutableSelf}, nullptr);
}

TypeDecl *NominalTypeDecl::getTypeWrapperStorageDecl() const {
  auto *mutableSelf = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           GetTypeWrapperStorage{mutableSelf}, nullptr);
}

static AccessLevel
getAccessLevelForTypeWrapperStorage(NominalTypeDecl *attachedTo) {
  auto &ctx = attachedTo->getASTContext();

  if (isa<ProtocolDecl>(attachedTo))
    return attachedTo->getFormalAccess();

  llvm::SmallDenseMap<ProtocolDecl *, bool, 4> visitedProtocols;
  std::function<bool(ProtocolDecl *)> hasPublicStorageAssociatedType =
      [&](ProtocolDecl *protocol) {
        if (visitedProtocols.count(protocol))
          return visitedProtocols[protocol];

        auto recordResult = [&](ProtocolDecl *P, bool hasWrapper) {
          visitedProtocols[P] = hasWrapper;
          return hasWrapper;
        };

        if (auto *storage =
                protocol->getAssociatedType(ctx.Id_TypeWrapperStorage)) {
          if (storage->getFormalAccess() == AccessLevel::Public)
            return recordResult(protocol, true);
        }

        // Recursively check whether any of the parents have that
        // requirement.
        for (auto *parent : protocol->getProtocolDependencies()) {
          bool hasPublicStorage = hasPublicStorageAssociatedType(parent);
          (void)recordResult(parent, hasPublicStorage);

          if (hasPublicStorage)
            return recordResult(protocol, true);
        }

        return recordResult(protocol, false);
      };

  SmallVector<ProtocolDecl *, 4> protocols;
  getDeclaredProtocolConformances(attachedTo, protocols);

  for (auto *protocol : protocols) {
    if (hasPublicStorageAssociatedType(protocol))
      return AccessLevel::Public;
  }

  return AccessLevel::Internal;
}

TypeDecl *GetTypeWrapperStorage::evaluate(Evaluator &evaluator,
                                          NominalTypeDecl *parent) const {
  if (!parent->hasTypeWrapper())
    return nullptr;

  auto &ctx = parent->getASTContext();

  TypeDecl *storage = nullptr;
  if (isa<ProtocolDecl>(parent)) {
    // If type wrapper is associated with a protocol, we need to
    // inject a new associated type - $Storage.
    storage = new (ctx)
        AssociatedTypeDecl(parent, /*keywordLoc=*/SourceLoc(),
                           ctx.Id_TypeWrapperStorage, /*nameLoc=*/SourceLoc(),
                           /*defaultDefinition=*/nullptr,
                           /*trailingWhere=*/nullptr);
  } else {
    // For classes and structs we inject a new member struct - $Storage.
    storage = new (ctx)
        StructDecl(/*StructLoc=*/SourceLoc(), ctx.Id_TypeWrapperStorage,
                   /*NameLoc=*/SourceLoc(),
                   /*Inheritted=*/{},
                   /*GenericParams=*/nullptr, parent);
  }

  storage->setImplicit();
  storage->setSynthesized();
  storage->setAccess(getAccessLevelForTypeWrapperStorage(parent));

  parent->addMember(storage);

  return storage;
}

VarDecl *
GetTypeWrapperProperty::evaluate(Evaluator &evaluator,
                                 NominalTypeDecl *parent) const {
  auto &ctx = parent->getASTContext();

  auto typeWrapper = parent->getTypeWrapper();
  if (!typeWrapper)
    return nullptr;

  auto *storage = parent->getTypeWrapperStorageDecl();
  assert(storage);

  auto *typeWrapperType =
      evaluateOrDefault(ctx.evaluator, GetTypeWrapperType{parent}, Type())
          ->castTo<AnyGenericType>();
  assert(typeWrapperType);

  // $storage: Wrapper<<ParentType>, <ParentType>.$Storage>
  auto propertyTy = BoundGenericType::get(
      typeWrapper->Wrapper, /*Parent=*/typeWrapperType->getParent(),
      /*genericArgs=*/
      {parent->getSelfInterfaceType(), storage->getDeclaredInterfaceType()});

  return injectProperty(parent, ctx.Id_TypeWrapperProperty, propertyTy,
                        VarDecl::Introducer::Var,
                        getAccessLevelForTypeWrapperStorage(parent));
}

VarDecl *GetTypeWrapperStorageForProperty::evaluate(Evaluator &evaluator,
                                                    VarDecl *property) const {
  auto *wrappedType = property->getDeclContext()->getSelfNominalTypeDecl();
  if (!(wrappedType && wrappedType->hasTypeWrapper()))
    return nullptr;

  // Type wrappers support only stored `var`s.
  if (!property->isAccessedViaTypeWrapper())
    return nullptr;

  assert(!isa<ProtocolDecl>(wrappedType));

  auto *storage =
      cast<NominalTypeDecl>(wrappedType->getTypeWrapperStorageDecl());
  assert(storage);

  // Type wrapper variables are never initialized directly,
  // initialization expression (if any) becomes an default
  // argument of the initializer synthesized by the type wrapper.
  if (auto *PBD = property->getParentPatternBinding()) {
    PBD->setInitializerSubsumed(/*index=*/0);
  }

  return injectProperty(storage, property->getName(),
                        property->getValueInterfaceType(),
                        property->getIntroducer(), AccessLevel::Internal);
}

/// Given the property create a subscript to reach its type wrapper storage:
/// `$storage[storageKeyPath: \$Storage.<property>]`.
static SubscriptExpr *subscriptTypeWrappedProperty(VarDecl *var,
                                                   AccessorDecl *useDC) {
  auto &ctx = useDC->getASTContext();
  auto *parent = var->getDeclContext()->getSelfNominalTypeDecl();

  if (!(parent && parent->hasTypeWrapper()))
    return nullptr;

  auto *typeWrapperVar = parent->getTypeWrapperProperty();
  auto *storageVar = var->getUnderlyingTypeWrapperStorage();

  assert(typeWrapperVar);
  assert(storageVar);

  auto createRefToSelf = [&]() {
    return new (ctx) DeclRefExpr({useDC->getImplicitSelfDecl()},
                                 /*Loc=*/DeclNameLoc(), /*Implicit=*/true);
  };

  // \$Storage.<property-name>
  auto *storageKeyPath = KeyPathExpr::createImplicit(
      ctx, /*backslashLoc=*/SourceLoc(),
      {KeyPathExpr::Component::forProperty(
          {storageVar},
          useDC->mapTypeIntoContext(storageVar->getInterfaceType()),
          /*Loc=*/SourceLoc())},
      /*endLoc=*/SourceLoc());

  // WrappedType.<property-name>
  auto *propertyKeyPath = KeyPathExpr::createImplicit(
      ctx, /*backslashLoc=*/SourceLoc(),
      {KeyPathExpr::Component::forUnresolvedProperty(
          DeclNameRef(var->getName()), /*Loc=*/SourceLoc())},
      /*endLoc=*/SourceLoc());

  auto *subscriptBaseExpr = UnresolvedDotExpr::createImplicit(
      ctx, createRefToSelf(), typeWrapperVar->getName());

  SmallVector<Argument, 4> subscriptArgs;

  // If this is a reference type, let's see whether type wrapper supports
  // `wrappedSelf:propertyKeyPath:storageKeyPath:` overload.
  if (isa<ClassDecl>(parent)) {
    DeclName subscriptName(
        ctx, DeclBaseName::createSubscript(),
        {ctx.Id_wrappedSelf, ctx.Id_propertyKeyPath, ctx.Id_storageKeyPath});

    auto *typeWrapper = parent->getTypeWrapper()->Wrapper;
    auto candidates = typeWrapper->lookupDirect(subscriptName);

    if (!candidates.empty()) {
      subscriptArgs.push_back(
          Argument(/*loc=*/SourceLoc(), ctx.Id_wrappedSelf, createRefToSelf()));
    }
  }

  subscriptArgs.push_back(
      Argument(/*loc=*/SourceLoc(), ctx.Id_propertyKeyPath, propertyKeyPath));
  subscriptArgs.push_back(
      Argument(/*loc=*/SourceLoc(), ctx.Id_storageKeyPath, storageKeyPath));

  // $storage[storageKeyPath: \$Storage.<property-name>]
  return SubscriptExpr::create(ctx, subscriptBaseExpr,
                               ArgumentList::createImplicit(ctx, subscriptArgs),
                               ConcreteDeclRef(), /*implicit=*/true);
}

BraceStmt *
SynthesizeTypeWrappedPropertyGetterBody::evaluate(Evaluator &evaluator,
                                                  AccessorDecl *getter) const {
  assert(getter->isGetter());

  auto &ctx = getter->getASTContext();

  auto *var = dyn_cast<VarDecl>(getter->getStorage());
  if (!var)
    return nullptr;

  auto *subscript = subscriptTypeWrappedProperty(var, getter);
  if (!subscript)
    return nullptr;

  ASTNode body = new (ctx) ReturnStmt(SourceLoc(), subscript,
                                      /*isImplicit=*/true);
  return BraceStmt::create(ctx, /*lbloc=*/var->getLoc(), body,
                           /*rbloc=*/var->getLoc(), /*implicit=*/true);
}

BraceStmt *
SynthesizeTypeWrappedPropertySetterBody::evaluate(Evaluator &evaluator,
                                                  AccessorDecl *setter) const {
  assert(setter->isSetter());

  auto &ctx = setter->getASTContext();

  auto *var = dyn_cast<VarDecl>(setter->getStorage());
  if (!var)
    return nullptr;

  auto *subscript = subscriptTypeWrappedProperty(var, setter);
  if (!subscript)
    return nullptr;

  VarDecl *newValueParam = setter->getParameters()->get(0);

  auto *assignment = new (ctx) AssignExpr(
      subscript, /*EqualLoc=*/SourceLoc(),
      new (ctx) DeclRefExpr(newValueParam, DeclNameLoc(), /*IsImplicit=*/true),
      /*Implicit=*/true);

  ASTNode body = new (ctx) ReturnStmt(SourceLoc(), assignment,
                                      /*isImplicit=*/true);
  return BraceStmt::create(ctx, /*lbloc=*/var->getLoc(), body,
                           /*rbloc=*/var->getLoc(), /*implicit=*/true);
}

bool IsPropertyAccessedViaTypeWrapper::evaluate(Evaluator &evaluator,
                                                VarDecl *property) const {
  auto *parent = property->getDeclContext()->getSelfNominalTypeDecl();
  if (!(parent && parent->hasTypeWrapper()))
    return false;

  if (property->isStatic())
    return false;

  // If this property has `@typeWrapperIgnored` attribute
  // it should not be managed by a type wrapper.
  if (property->getAttrs().hasAttribute<TypeWrapperIgnoredAttr>())
    return false;

  // `lazy` properties are not wrapped.
  if (property->getAttrs().hasAttribute<LazyAttr>() ||
      property->isLazyStorageProperty())
    return false;

  // Properties with attached property wrappers are not considered
  // accessible via type wrapper directly, only their backing storage is.
  {
    // Wrapped property itself `<name>`
    if (property->hasAttachedPropertyWrapper())
      return false;

    // Projection - `$<name>`
    if (property->getOriginalWrappedProperty(
          PropertyWrapperSynthesizedPropertyKind::Projection))
      return false;

    // Backing storage (or wrapper property) - `_<name>`.
    //
    // This is the only thing that wrapper needs to handle because
    // all access to the wrapped variable and it's projection
    // is routed through it.
    if (auto *wrappedProperty = property->getOriginalWrappedProperty(
            PropertyWrapperSynthesizedPropertyKind::Backing)) {
      // If wrapped property is ignored - its backing storage is
      // ignored as well.
      return !wrappedProperty->getAttrs()
                  .hasAttribute<TypeWrapperIgnoredAttr>();
    }
  }

  // Don't wrap any compiler synthesized properties except to
  // property wrapper backing storage (checked above).
  if (property->isImplicit())
    return false;

  // Check whether this is a computed property.
  {
    auto declaresAccessor = [&](ArrayRef<AccessorKind> kinds) -> bool {
      return llvm::any_of(kinds, [&](const AccessorKind &kind) {
        return bool(property->getParsedAccessor(kind));
      });
    };

    // property has a getter.
    if (declaresAccessor(
            {AccessorKind::Get, AccessorKind::Read, AccessorKind::Address}))
      return false;

    // property has a setter.
    if (declaresAccessor({AccessorKind::Set, AccessorKind::Modify,
                          AccessorKind::MutableAddress}))
      return false;
  }

  return true;
}

VarDecl *SynthesizeLocalVariableForTypeWrapperStorage::evaluate(
    Evaluator &evaluator, ConstructorDecl *ctor) const {
  auto &ctx = ctor->getASTContext();

  if (ctor->isImplicit() || !ctor->isDesignatedInit())
    return nullptr;

  auto *DC = ctor->getDeclContext()->getSelfNominalTypeDecl();
  if (!(DC && DC->hasTypeWrapper()))
    return nullptr;

  // Default protocol initializers do not get transformed.
  if (isa<ProtocolDecl>(DC))
    return nullptr;

  auto *storageDecl = cast<NominalTypeDecl>(DC->getTypeWrapperStorageDecl());
  assert(storageDecl);

  SmallVector<TupleTypeElt, 4> members;
  for (auto *member : storageDecl->getMembers()) {
    if (auto *var = dyn_cast<VarDecl>(member)) {
      assert(var->hasStorage() &&
             "$Storage should have stored properties only");
      members.push_back({var->getValueInterfaceType(), var->getName()});
    }
  }

  auto *PBD =
      injectVariable(ctor, ctx.Id_localStorageVar, TupleType::get(members, ctx),
                     VarDecl::Introducer::Var);
  return PBD->getSingleVar();
}

ConstructorDecl *NominalTypeDecl::getTypeWrapperInitializer() const {
  auto *mutableSelf = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           GetTypeWrapperInitializer{mutableSelf}, nullptr);
}

ConstructorDecl *
GetTypeWrapperInitializer::evaluate(Evaluator &evaluator,
                                    NominalTypeDecl *typeWrapper) const {
  auto &ctx = typeWrapper->getASTContext();
  assert(typeWrapper->getAttrs().hasAttribute<TypeWrapperAttr>());

  auto ctors = typeWrapper->lookupDirect(DeclName(
      ctx, DeclBaseName::createConstructor(), {ctx.Id_for, ctx.Id_storage}));

  if (ctors.size() != 1)
    return nullptr;

  return cast<ConstructorDecl>(ctors.front());
}
