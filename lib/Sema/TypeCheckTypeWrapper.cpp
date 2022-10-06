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

NominalTypeDecl *NominalTypeDecl::getTypeWrapper() const {
  auto *mutableSelf = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           GetTypeWrapper{mutableSelf}, nullptr);
}

static void getTypeWrappers(
    NominalTypeDecl *decl,
    SmallVectorImpl<std::pair<CustomAttr *, NominalTypeDecl *>> &typeWrappers) {
  auto &ctx = decl->getASTContext();

  for (auto *attr : decl->getAttrs().getAttributes<CustomAttr>()) {
    auto *mutableAttr = const_cast<CustomAttr *>(attr);
    auto *nominal = evaluateOrDefault(
        ctx.evaluator, CustomAttrNominalRequest{mutableAttr, decl}, nullptr);

    if (!nominal)
      continue;

    auto *typeWrapper = nominal->getAttrs().getAttribute<TypeWrapperAttr>();
    if (typeWrapper && typeWrapper->isValid())
      typeWrappers.push_back({mutableAttr, nominal});
  }
}

NominalTypeDecl *GetTypeWrapper::evaluate(Evaluator &evaluator,
                                          NominalTypeDecl *decl) const {
  auto &ctx = decl->getASTContext();

  // Note that we don't actually care whether there are duplicates,
  // using the same type wrapper multiple times is still an error.
  SmallVector<std::pair<CustomAttr *, NominalTypeDecl *>, 2> typeWrappers;

  getTypeWrappers(decl, typeWrappers);

  if (typeWrappers.empty())
    return nullptr;

  if (typeWrappers.size() != 1) {
    ctx.Diags.diagnose(decl, diag::cannot_use_multiple_type_wrappers);
    return nullptr;
  }

  return typeWrappers.front().second;
}

Type GetTypeWrapperType::evaluate(Evaluator &evaluator,
                                  NominalTypeDecl *decl) const {
  SmallVector<std::pair<CustomAttr *, NominalTypeDecl *>, 2> typeWrappers;

  getTypeWrappers(decl, typeWrappers);

  if (typeWrappers.size() != 1)
    return Type();

  auto *typeWrapperAttr = typeWrappers.front().first;
  auto type = evaluateOrDefault(
      evaluator,
      CustomAttrTypeRequest{typeWrapperAttr, decl->getDeclContext(),
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

ConstructorDecl *
NominalTypeDecl::getTypeWrappedTypeMemberwiseInitializer() const {
  auto *mutableSelf = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      SynthesizeTypeWrappedTypeMemberwiseInitializer{mutableSelf}, nullptr);
}

NominalTypeDecl *NominalTypeDecl::getTypeWrapperStorageDecl() const {
  auto *mutableSelf = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           GetTypeWrapperStorage{mutableSelf}, nullptr);
}

NominalTypeDecl *
GetTypeWrapperStorage::evaluate(Evaluator &evaluator,
                                NominalTypeDecl *parent) const {
  if (!parent->hasTypeWrapper())
    return nullptr;

  auto &ctx = parent->getASTContext();

  auto *storage =
      new (ctx) StructDecl(/*StructLoc=*/SourceLoc(), ctx.Id_TypeWrapperStorage,
                           /*NameLoc=*/SourceLoc(),
                           /*Inheritted=*/{},
                           /*GenericParams=*/nullptr, parent);

  storage->setImplicit();
  storage->setSynthesized();
  storage->setAccess(AccessLevel::Internal);

  parent->addMember(storage);

  return storage;
}

VarDecl *
GetTypeWrapperProperty::evaluate(Evaluator &evaluator,
                                 NominalTypeDecl *parent) const {
  auto &ctx = parent->getASTContext();

  auto *typeWrapper = parent->getTypeWrapper();
  if (!typeWrapper)
    return nullptr;

  auto *storage = parent->getTypeWrapperStorageDecl();
  assert(storage);

  auto *typeWrapperType =
      evaluateOrDefault(ctx.evaluator, GetTypeWrapperType{parent}, Type())
          ->castTo<AnyGenericType>();
  assert(typeWrapperType);

  // $storage: Wrapper<$Storage>
  auto propertyTy = BoundGenericType::get(
      typeWrapper, /*Parent=*/typeWrapperType->getParent(),
      /*genericArgs=*/{storage->getInterfaceType()->getMetatypeInstanceType()});

  return injectProperty(parent, ctx.Id_TypeWrapperProperty, propertyTy,
                        VarDecl::Introducer::Var, AccessLevel::Internal);
}

VarDecl *GetTypeWrapperStorageForProperty::evaluate(Evaluator &evaluator,
                                                    VarDecl *property) const {
  auto *wrappedType = property->getDeclContext()->getSelfNominalTypeDecl();
  if (!(wrappedType && wrappedType->hasTypeWrapper()))
    return nullptr;

  // Type wrappers support only stored `var`s.
  if (!property->isAccessedViaTypeWrapper())
    return nullptr;

  auto *storage = wrappedType->getTypeWrapperStorageDecl();
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

  // \$Storage.<property-name>
  auto *argExpr = KeyPathExpr::createImplicit(
      ctx, /*backslashLoc=*/SourceLoc(),
      {KeyPathExpr::Component::forProperty(
          {storageVar},
          useDC->mapTypeIntoContext(storageVar->getInterfaceType()),
          /*Loc=*/SourceLoc())},
      /*endLoc=*/SourceLoc());

  auto *subscriptBaseExpr = UnresolvedDotExpr::createImplicit(
      ctx,
      new (ctx) DeclRefExpr({useDC->getImplicitSelfDecl()},
                            /*Loc=*/DeclNameLoc(), /*Implicit=*/true),
      typeWrapperVar->getName());

  // $storage[storageKeyPath: \$Storage.<property-name>]
  return SubscriptExpr::create(
      ctx, subscriptBaseExpr,
      ArgumentList::forImplicitSingle(ctx, ctx.Id_storageKeyPath, argExpr),
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

BraceStmt *SynthesizeTypeWrappedTypeMemberwiseInitializerBody::evaluate(
    Evaluator &evaluator, ConstructorDecl *ctor) const {
  auto &ctx = ctor->getASTContext();
  auto *parent = ctor->getDeclContext()->getSelfNominalTypeDecl();

  // self.$storage = .init(storage: $Storage(...))
  auto *storageType = parent->getTypeWrapperStorageDecl();
  assert(storageType);

  auto *typeWrapperVar = parent->getTypeWrapperProperty();
  assert(typeWrapperVar);

  auto *storageVarRef = UnresolvedDotExpr::createImplicit(
      ctx,
      new (ctx) DeclRefExpr({ctor->getImplicitSelfDecl()},
                            /*Loc=*/DeclNameLoc(), /*Implicit=*/true),
      typeWrapperVar->getName());

  // Check whether given parameter requires a direct assignment to
  // intialize the property.
  //
  // If `$Storage` doesn't have a member that corresponds
  // to the current parameter it means that this is a property
  // that not managed by the type wrapper which has to be
  // initialized by direct assignment: `self.<name> = <arg>`
  auto useDirectAssignment = [&](ParamDecl *param) {
    // Properties with property wrappers are always managed by the type wrapper
    if (param->hasAttachedPropertyWrapper())
      return false;
    return storageType->lookupDirect(param->getName()).empty();
  };

  SmallVector<ASTNode, 2> body;

  SmallVector<Argument, 4> initArgs;
  {
    for (auto *param : *ctor->getParameters()) {
      VarDecl *arg = param;

      if (useDirectAssignment(param)) {
        auto *propRef = UnresolvedDotExpr::createImplicit(
            ctx,
            new (ctx) DeclRefExpr({ctor->getImplicitSelfDecl()},
                                  /*Loc=*/DeclNameLoc(), /*Implicit=*/true),
            arg->getName());

        body.push_back(new (ctx) AssignExpr(
            propRef, /*EqualLoc=*/SourceLoc(),
            new (ctx) DeclRefExpr({arg}, /*DeclNameLoc=*/DeclNameLoc(),
                                  /*Implicit=*/true),
            /*Implicit=*/true));
        continue;
      }

      // type wrappers wrap only backing storage of a wrapped
      // property, so in this case we need to pass `_<name>` to
      // `$Storage` constructor.
      if (param->hasAttachedPropertyWrapper()) {
        arg = param->getPropertyWrapperBackingProperty();
        (void)param->getPropertyWrapperBackingPropertyType();
      }

      initArgs.push_back({/*labelLoc=*/SourceLoc(), arg->getName(),
                          new (ctx) DeclRefExpr(arg, /*Loc=*/DeclNameLoc(),
                                                /*Implicit=*/true)});
    }
  }

  auto *storageInit = CallExpr::createImplicit(
      ctx,
      TypeExpr::createImplicitForDecl(
          /*Loc=*/DeclNameLoc(), storageType, ctor,
          ctor->mapTypeIntoContext(storageType->getInterfaceType())),
      ArgumentList::createImplicit(ctx, initArgs));

  auto *initRef = new (ctx) UnresolvedMemberExpr(
      /*dotLoc=*/SourceLoc(), /*declNameLoc=*/DeclNameLoc(),
      DeclNameRef::createConstructor(), /*implicit=*/true);
  { initRef->setFunctionRefKind(FunctionRefKind::DoubleApply); }

  // .init($Storage(...))
  Expr *typeWrapperInit = CallExpr::createImplicit(
      ctx, initRef,
      ArgumentList::forImplicitSingle(ctx, ctx.Id_storage, storageInit));

  body.push_back(new (ctx) AssignExpr(storageVarRef, /*EqualLoc=*/SourceLoc(),
                                      typeWrapperInit,
                                      /*Implicit=*/true));

  return BraceStmt::create(ctx, /*lbloc=*/ctor->getLoc(), body,
                           /*rbloc=*/ctor->getLoc(), /*implicit=*/true);
}

VarDecl *SynthesizeLocalVariableForTypeWrapperStorage::evaluate(
    Evaluator &evaluator, ConstructorDecl *ctor) const {
  auto &ctx = ctor->getASTContext();

  if (ctor->isImplicit() || !ctor->isDesignatedInit())
    return nullptr;

  auto *DC = ctor->getDeclContext()->getSelfNominalTypeDecl();
  if (!(DC && DC->hasTypeWrapper()))
    return nullptr;

  auto *storageDecl = DC->getTypeWrapperStorageDecl();
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

  auto ctors = typeWrapper->lookupDirect(
      DeclName(ctx, DeclBaseName::createConstructor(), {ctx.Id_storage}));

  if (ctors.size() != 1)
    return nullptr;

  return cast<ConstructorDecl>(ctors.front());
}
