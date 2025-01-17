//===--- DeclSynthesizer.cpp - Synthesize helper Swift decls --------------===//
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

#include "SwiftDeclSynthesizer.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/Assertions.h"
#include "swift/ClangImporter/CXXMethodBridging.h"
#include "clang/AST/Mangle.h"
#include "clang/Sema/DelayedDiagnostic.h"

using namespace swift;
using namespace importer;

static Argument createSelfArg(AccessorDecl *accessorDecl) {
  ASTContext &ctx = accessorDecl->getASTContext();

  auto selfDecl = accessorDecl->getImplicitSelfDecl();
  auto selfRefExpr = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                           /*implicit*/ true);

  if (!accessorDecl->isMutating()) {
    selfRefExpr->setType(selfDecl->getInterfaceType());
    return Argument::unlabeled(selfRefExpr);
  }
  selfRefExpr->setType(LValueType::get(selfDecl->getInterfaceType()));
  return Argument::implicitInOut(ctx, selfRefExpr);
}

static CallExpr *createAccessorImplCallExpr(FuncDecl *accessorImpl,
                                            Argument selfArg,
                                            DeclRefExpr *keyRefExpr = nullptr) {
  ASTContext &ctx = accessorImpl->getASTContext();

  auto accessorImplExpr =
      new (ctx) DeclRefExpr(ConcreteDeclRef(accessorImpl), DeclNameLoc(),
                            /*Implicit*/ true);
  accessorImplExpr->setType(accessorImpl->getInterfaceType());

  auto accessorImplDotCallExpr =
      DotSyntaxCallExpr::create(ctx, accessorImplExpr, SourceLoc(), selfArg);
  accessorImplDotCallExpr->setType(accessorImpl->getMethodInterfaceType());
  accessorImplDotCallExpr->setThrows(nullptr);

  ArgumentList *argList;
  if (keyRefExpr) {
    argList = ArgumentList::forImplicitUnlabeled(ctx, {keyRefExpr});
  } else {
    argList = ArgumentList::forImplicitUnlabeled(ctx, {});
  }
  auto *accessorImplCallExpr =
      CallExpr::createImplicit(ctx, accessorImplDotCallExpr, argList);
  accessorImplCallExpr->setType(accessorImpl->getResultInterfaceType());
  accessorImplCallExpr->setThrows(nullptr);
  return accessorImplCallExpr;
}

static DeclRefExpr *createParamRefExpr(AccessorDecl *accessorDecl,
                                       unsigned index) {
  ASTContext &ctx = accessorDecl->getASTContext();

  auto paramDecl = accessorDecl->getParameters()->get(index);
  auto paramRefExpr = new (ctx) DeclRefExpr(paramDecl, DeclNameLoc(),
                                            /*Implicit*/ true);
  paramRefExpr->setType(paramDecl->getTypeInContext());
  return paramRefExpr;
}

static AccessorDecl *makeFieldGetterDecl(ClangImporter::Implementation &Impl,
                                         NominalTypeDecl *importedDecl,
                                         VarDecl *importedFieldDecl,
                                         ClangNode clangNode = ClangNode()) {
  auto &C = Impl.SwiftContext;

  auto *params = ParameterList::createEmpty(C);

  auto getterType = importedFieldDecl->getInterfaceType();
  auto getterDecl = AccessorDecl::create(
      C,
      /*declLoc=*/importedFieldDecl->getLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(), AccessorKind::Get, importedFieldDecl,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(),
      params, getterType, importedDecl, clangNode);
  getterDecl->setAccess(importedFieldDecl->getFormalAccess());
  getterDecl->setIsObjC(false);
  getterDecl->setIsDynamic(false);

  return getterDecl;
}

static AccessorDecl *makeFieldSetterDecl(ClangImporter::Implementation &Impl,
                                         NominalTypeDecl *importedDecl,
                                         VarDecl *importedFieldDecl,
                                         ClangNode clangNode = ClangNode()) {
  auto &C = Impl.SwiftContext;
  auto newValueDecl = new (C) ParamDecl(SourceLoc(), SourceLoc(), Identifier(),
                                        SourceLoc(), C.Id_value, importedDecl);
  newValueDecl->setSpecifier(ParamSpecifier::Default);
  newValueDecl->setInterfaceType(importedFieldDecl->getInterfaceType());

  auto *params = ParameterList::createWithoutLoc(newValueDecl);

  auto voidTy = TupleType::getEmpty(C);

  auto setterDecl = AccessorDecl::create(
      C,
      /*declLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(), AccessorKind::Set, importedFieldDecl,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(),
      params, voidTy, importedDecl, clangNode);
  setterDecl->setIsObjC(false);
  setterDecl->setIsDynamic(false);
  setterDecl->setSelfAccessKind(SelfAccessKind::Mutating);
  setterDecl->setAccess(importedFieldDecl->getFormalAccess());

  return setterDecl;
}

std::pair<VarDecl *, PatternBindingDecl *>
SwiftDeclSynthesizer::createVarWithPattern(DeclContext *dc, Identifier name,
                                           Type ty,
                                           VarDecl::Introducer introducer,
                                           bool isImplicit, AccessLevel access,
                                           AccessLevel setterAccess) {
  ASTContext &ctx = dc->getASTContext();

  // Create a variable to store the underlying value.
  auto var = new (ctx) VarDecl(
      /*IsStatic*/ false, introducer, SourceLoc(), name, dc);
  if (isImplicit)
    var->setImplicit();
  var->setInterfaceType(ty);
  var->setAccess(access);
  var->setSetterAccess(setterAccess);

  // Create a pattern binding to describe the variable.
  Pattern *varPattern = createTypedNamedPattern(var);
  auto *patternBinding = PatternBindingDecl::create(
      ctx, /*StaticLoc*/ SourceLoc(), StaticSpellingKind::None,
      /*VarLoc*/ SourceLoc(), varPattern, /*EqualLoc*/ SourceLoc(),
      /*InitExpr*/ nullptr, dc);
  if (isImplicit)
    patternBinding->setImplicit();

  return {var, patternBinding};
}

Pattern *SwiftDeclSynthesizer::createTypedNamedPattern(VarDecl *decl) {
  ASTContext &Ctx = decl->getASTContext();
  Type ty = decl->getTypeInContext();

  Pattern *P = new (Ctx) NamedPattern(decl);
  P->setType(ty);
  P->setImplicit();
  return TypedPattern::createImplicit(Ctx, P, ty);
}

namespace {
using ConstantGetterBodyContextData =
    llvm::PointerIntPair<Expr *, 2, ConstantConvertKind>;
}

Type SwiftDeclSynthesizer::getConstantLiteralType(
    Type type, ConstantConvertKind convertKind) {
  switch (convertKind) {
  case ConstantConvertKind::Construction:
  case ConstantConvertKind::ConstructionWithUnwrap: {
    auto found = ImporterImpl.RawTypes.find(type->getAnyNominal());
    assert(found != ImporterImpl.RawTypes.end());
    return found->second;
  }

  default:
    return type;
  }
}

ValueDecl *SwiftDeclSynthesizer::createConstant(
    Identifier name, DeclContext *dc, Type type, const clang::APValue &value,
    ConstantConvertKind convertKind, bool isStatic, ClangNode ClangN) {
  auto &context = ImporterImpl.SwiftContext;

  // Create the integer literal value.
  Expr *expr = nullptr;
  switch (value.getKind()) {
  case clang::APValue::AddrLabelDiff:
  case clang::APValue::Array:
  case clang::APValue::ComplexFloat:
  case clang::APValue::ComplexInt:
  case clang::APValue::FixedPoint:
  case clang::APValue::Indeterminate:
  case clang::APValue::LValue:
  case clang::APValue::MemberPointer:
  case clang::APValue::None:
  case clang::APValue::Struct:
  case clang::APValue::Union:
  case clang::APValue::Vector:
    llvm_unreachable("Unhandled APValue kind");

  case clang::APValue::Float:
  case clang::APValue::Int: {
    // Print the value.
    llvm::SmallString<16> printedValueBuf;
    if (value.getKind() == clang::APValue::Int) {
      value.getInt().toString(printedValueBuf);
    } else {
      assert(value.getFloat().isFinite() && "can't handle infinities or NaNs");
      value.getFloat().toString(printedValueBuf);
    }
    StringRef printedValue = printedValueBuf.str();

    // If this was a negative number, record that and strip off the '-'.
    bool isNegative = printedValue.front() == '-';
    if (isNegative)
      printedValue = printedValue.drop_front();

    auto literalType = getConstantLiteralType(type, convertKind);

    // Create the expression node.
    StringRef printedValueCopy(context.AllocateCopy(printedValue));
    if (value.getKind() == clang::APValue::Int) {
      bool isBool = type->getCanonicalType()->isBool();
      // Check if "type" is a C++ enum with an underlying type of "bool".
      if (!isBool && type->getStructOrBoundGenericStruct() &&
          type->getStructOrBoundGenericStruct()->getClangDecl()) {
        if (auto enumDecl = dyn_cast<clang::EnumDecl>(
                type->getStructOrBoundGenericStruct()->getClangDecl())) {
          isBool = enumDecl->getIntegerType()->isBooleanType();
        }
      }
      if (isBool) {
        auto *boolExpr = new (context)
            BooleanLiteralExpr(value.getInt().getBoolValue(), SourceLoc(),
                               /*Implicit=*/true);

        boolExpr->setBuiltinInitializer(context.getBoolBuiltinInitDecl());
        boolExpr->setType(literalType);

        expr = boolExpr;
      } else {
        auto *intExpr =
            new (context) IntegerLiteralExpr(printedValueCopy, SourceLoc(),
                                             /*Implicit=*/true);

        auto *intDecl = literalType->getAnyNominal();
        intExpr->setBuiltinInitializer(context.getIntBuiltinInitDecl(intDecl));
        intExpr->setType(literalType);

        expr = intExpr;
      }
    } else {
      auto *floatExpr =
          new (context) FloatLiteralExpr(printedValueCopy, SourceLoc(),
                                         /*Implicit=*/true);

      auto maxFloatTypeDecl = context.get_MaxBuiltinFloatTypeDecl();
      floatExpr->setBuiltinType(maxFloatTypeDecl->getUnderlyingType());

      auto *floatDecl = literalType->getAnyNominal();
      floatExpr->setBuiltinInitializer(
          context.getFloatBuiltinInitDecl(floatDecl));
      floatExpr->setType(literalType);

      expr = floatExpr;
    }

    if (isNegative)
      cast<NumberLiteralExpr>(expr)->setNegative(SourceLoc());

    break;
  }
  }

  assert(expr);
  return createConstant(name, dc, type, expr, convertKind, isStatic, ClangN);
}

ValueDecl *SwiftDeclSynthesizer::createConstant(
    Identifier name, DeclContext *dc, Type type, StringRef value,
    ConstantConvertKind convertKind, bool isStatic, ClangNode ClangN) {
  ASTContext &ctx = ImporterImpl.SwiftContext;

  auto expr = new (ctx) StringLiteralExpr(value, SourceRange());

  auto literalType = getConstantLiteralType(type, convertKind);
  auto *stringDecl = literalType->getAnyNominal();
  expr->setBuiltinInitializer(ctx.getStringBuiltinInitDecl(stringDecl));
  expr->setType(literalType);

  return createConstant(name, dc, type, expr, convertKind, isStatic, ClangN);
}

/// Synthesizer callback to synthesize the getter for a constant value.
static std::pair<BraceStmt *, bool>
synthesizeConstantGetterBody(AbstractFunctionDecl *afd, void *voidContext) {
  ASTContext &ctx = afd->getASTContext();
  auto func = cast<AccessorDecl>(afd);
  VarDecl *constantVar = cast<VarDecl>(func->getStorage());
  Type type = func->mapTypeIntoContext(constantVar->getValueInterfaceType());

  auto contextData =
      ConstantGetterBodyContextData::getFromOpaqueValue(voidContext);
  Expr *expr = contextData.getPointer();
  ConstantConvertKind convertKind = contextData.getInt();

  // If we need a conversion, add one now.
  switch (convertKind) {
  case ConstantConvertKind::None:
    break;

  case ConstantConvertKind::Construction:
  case ConstantConvertKind::ConstructionWithUnwrap: {
    auto typeRef = TypeExpr::createImplicit(type, ctx);

    // Reference init(rawValue: T)
    ConstructorDecl *init = nullptr;
    DeclName initName =
        DeclName(ctx, DeclBaseName::createConstructor(), {ctx.Id_rawValue});
    auto nominal = type->getAnyNominal();
    for (auto found : nominal->lookupDirect(initName)) {
      init = dyn_cast<ConstructorDecl>(found);
      if (init && init->getDeclContext() == nominal)
        break;
    }
    assert(init && "did not find init(rawValue:)");

    auto initTy = init->getInterfaceType()->removeArgumentLabels(1);
    auto declRef = new (ctx) DeclRefExpr(init, DeclNameLoc(), /*Implicit=*/true,
                                         AccessSemantics::Ordinary, initTy);

    // (Self) -> ...
    initTy = initTy->castTo<FunctionType>()->getResult();
    auto initRef = DotSyntaxCallExpr::create(
        ctx, declRef, SourceLoc(), Argument::unlabeled(typeRef), initTy);
    initRef->setThrows(nullptr);

    // (rawValue: T) -> ...
    initTy = initTy->castTo<FunctionType>()->getResult();

    auto *argList = ArgumentList::forImplicitSingle(ctx, ctx.Id_rawValue, expr);
    auto initCall = CallExpr::createImplicit(ctx, initRef, argList);
    initCall->setType(initTy);
    initCall->setThrows(nullptr);

    expr = initCall;

    // Force unwrap if our init(rawValue:) is failable, which is currently
    // the case with enums.
    if (convertKind == ConstantConvertKind::ConstructionWithUnwrap) {
      initTy = initTy->getOptionalObjectType();
      expr = new (ctx) ForceValueExpr(expr, SourceLoc());
      expr->setType(initTy);
    }

    assert(initTy->isEqual(type));
    break;
  }
  }

  // Create the return statement.
  auto ret = ReturnStmt::createImplicit(ctx, expr);

  return {BraceStmt::create(ctx, SourceLoc(), ASTNode(ret), SourceLoc()),
          /*isTypeChecked=*/true};
}

ValueDecl *SwiftDeclSynthesizer::createConstant(
    Identifier name, DeclContext *dc, Type type, Expr *valueExpr,
    ConstantConvertKind convertKind, bool isStatic, ClangNode ClangN) {
  auto &C = ImporterImpl.SwiftContext;

  VarDecl *var = nullptr;
  if (ClangN) {
    var = ImporterImpl.createDeclWithClangNode<VarDecl>(
        ClangN, AccessLevel::Public,
        /*IsStatic*/ isStatic, VarDecl::Introducer::Var, SourceLoc(), name, dc);
  } else {
    var = new (C) VarDecl(
        /*IsStatic*/ isStatic, VarDecl::Introducer::Var, SourceLoc(), name, dc);
  }

  var->setInterfaceType(type);
  var->setIsObjC(false);
  var->setIsDynamic(false);

  auto *params = ParameterList::createEmpty(C);

  // Create the getter function declaration.
  auto func = AccessorDecl::create(
      C,
      /*declLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(), AccessorKind::Get, var,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(),
      params, type, dc);
  func->setStatic(isStatic);
  func->setAccess(getOverridableAccessLevel(dc));
  func->setIsObjC(false);
  func->setIsDynamic(false);

  func->setBodySynthesizer(
      synthesizeConstantGetterBody,
      ConstantGetterBodyContextData(valueExpr, convertKind).getOpaqueValue());

  // Mark the function transparent so that we inline it away completely.
  func->getAttrs().add(new (C) TransparentAttr(/*implicit*/ true));
  auto nonisolatedAttr =
      new (C) NonisolatedAttr(/*unsafe=*/false, /*implicit=*/true);
  var->getAttrs().add(nonisolatedAttr);

  // Set the function up as the getter.
  ImporterImpl.makeComputed(var, func, nullptr);

  return var;
}

// MARK: Struct default initializers

/// Synthesize the body for an struct default initializer.
static std::pair<BraceStmt *, bool>
synthesizeStructDefaultConstructorBody(AbstractFunctionDecl *afd,
                                       void *context) {
  auto constructor = cast<ConstructorDecl>(afd);
  ASTContext &ctx = constructor->getASTContext();
  auto structDecl = static_cast<StructDecl *>(context);

  // Use a builtin to produce a zero initializer, and assign it to self.

  // Construct the left-hand reference to self.
  auto *selfDecl = constructor->getImplicitSelfDecl();
  Expr *lhs = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit=*/true);
  auto selfType = structDecl->getDeclaredInterfaceType();
  lhs->setType(LValueType::get(selfType));

  auto emptyTuple = TupleType::getEmpty(ctx);

  // Construct the right-hand call to Builtin.zeroInitializer.
  Identifier zeroInitID = ctx.getIdentifier("zeroInitializer");
  auto zeroInitializerFunc =
      cast<FuncDecl>(getBuiltinValueDecl(ctx, zeroInitID));
  SubstitutionMap subMap = SubstitutionMap::get(
      zeroInitializerFunc->getGenericSignature(), llvm::ArrayRef(selfType),
      LookUpConformanceInModule());
  ConcreteDeclRef concreteDeclRef(zeroInitializerFunc, subMap);
  auto zeroInitializerRef =
      new (ctx) DeclRefExpr(concreteDeclRef, DeclNameLoc(), /*implicit*/ true);
  // FIXME: Verify ExtInfo state is correct, not working by accident.
  FunctionType::ExtInfo info;
  zeroInitializerRef->setType(FunctionType::get({}, selfType, info));

  auto call = CallExpr::createImplicitEmpty(ctx, zeroInitializerRef);
  call->setType(selfType);
  call->setThrows(nullptr);

  auto assign = new (ctx) AssignExpr(lhs, SourceLoc(), call, /*implicit*/ true);
  assign->setType(emptyTuple);

  auto *ret = ReturnStmt::createImplicit(ctx, /*expr*/ nullptr);

  // Create the function body.
  auto body = BraceStmt::create(ctx, SourceLoc(), {assign, ret}, SourceLoc());
  return {body, /*isTypeChecked*/ true};
}

ConstructorDecl *
SwiftDeclSynthesizer::createDefaultConstructor(NominalTypeDecl *structDecl) {
  auto &context = ImporterImpl.SwiftContext;

  auto emptyPL = ParameterList::createEmpty(context);

  // Create the constructor.
  DeclName name(context, DeclBaseName::createConstructor(), emptyPL);
  auto constructor = new (context)
      ConstructorDecl(name, structDecl->getLoc(),
                      /*Failable=*/false, /*FailabilityLoc=*/SourceLoc(),
                      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
                      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                      /*ThrownType=*/TypeLoc(), emptyPL,
                      /*GenericParams=*/nullptr, structDecl,
                      /*LifetimeDependentTypeRepr*/ nullptr);

  constructor->setAccess(AccessLevel::Public);

  // Mark the constructor transparent so that we inline it away completely.
  constructor->getAttrs().add(new (context) TransparentAttr(/*implicit*/ true));

  constructor->setBodySynthesizer(synthesizeStructDefaultConstructorBody,
                                  structDecl);

  // We're done.
  return constructor;
}

// MARK: Struct value initializers

/// Synthesizer callback for the body of a struct value constructor.
static std::pair<BraceStmt *, bool>
synthesizeValueConstructorBody(AbstractFunctionDecl *afd, void *context) {
  auto constructor = cast<ConstructorDecl>(afd);
  ArrayRef<VarDecl *> members(static_cast<VarDecl **>(context) + 1,
                              static_cast<uintptr_t *>(context)[0]);

  ASTContext &ctx = constructor->getASTContext();

  // Assign all of the member variables appropriately.
  SmallVector<ASTNode, 4> stmts;

  auto *selfDecl = constructor->getImplicitSelfDecl();

  // To keep DI happy, initialize stored properties before computed.
  auto parameters = constructor->getParameters();
  for (unsigned pass = 0; pass < 2; ++pass) {
    unsigned paramPos = 0;

    for (unsigned i = 0, e = members.size(); i < e; ++i) {
      auto var = members[i];

      if (isa_and_nonnull<clang::IndirectFieldDecl>(var->getClangDecl()))
        continue;

      if (var->hasStorage() == (pass != 0)) {
        ++paramPos;
        continue;
      }

      // Construct left-hand side.
      Expr *lhs = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                        /*Implicit=*/true);
      lhs->setType(LValueType::get(selfDecl->getTypeInContext()));

      auto semantics = (var->hasStorage() ? AccessSemantics::DirectToStorage
                                          : AccessSemantics::Ordinary);

      lhs = new (ctx) MemberRefExpr(lhs, SourceLoc(), var, DeclNameLoc(),
                                    /*Implicit=*/true, semantics);
      lhs->setType(LValueType::get(var->getTypeInContext()));

      // Construct right-hand side.
      auto rhs = new (ctx) DeclRefExpr(parameters->get(paramPos), DeclNameLoc(),
                                       /*Implicit=*/true);
      rhs->setType(parameters->get(paramPos)->getTypeInContext());

      // Add assignment.
      auto assign = new (ctx) AssignExpr(lhs, SourceLoc(), rhs,
                                         /*Implicit=*/true);
      assign->setType(TupleType::getEmpty(ctx));

      stmts.push_back(assign);
      ++paramPos;
    }
  }

  stmts.push_back(ReturnStmt::createImplicit(ctx, /*expr*/ nullptr));

  // Create the function body.
  auto body = BraceStmt::create(ctx, SourceLoc(), stmts, SourceLoc());
  return {body, /*isTypeChecked=*/true};
}

ConstructorDecl *SwiftDeclSynthesizer::createValueConstructor(
    NominalTypeDecl *structDecl, ArrayRef<VarDecl *> members,
    bool wantCtorParamNames, bool wantBody) {
  auto &context = ImporterImpl.SwiftContext;

  // Construct the set of parameters from the list of members.
  SmallVector<ParamDecl *, 8> valueParameters;
  for (auto var : members) {
    if (var->isStatic())
      continue;

    bool generateParamName = wantCtorParamNames;

    if (var->hasClangNode()) {
      // TODO create value constructor with indirect fields instead of the
      // generated __Anonymous_field.
      if (isa<clang::IndirectFieldDecl>(var->getClangDecl()))
        continue;

      if (auto clangField = dyn_cast<clang::FieldDecl>(var->getClangDecl()))
        if (clangField->isAnonymousStructOrUnion() ||
            clangField->getDeclName().isEmpty())
          generateParamName = false;
    }

    Identifier argName = generateParamName ? var->getName() : Identifier();
    auto param =
        new (context) ParamDecl(SourceLoc(), SourceLoc(), argName, SourceLoc(),
                                var->getName(), structDecl);
    param->setSpecifier(ParamSpecifier::Default);
    param->setInterfaceType(var->getInterfaceType());
    ImporterImpl.recordImplicitUnwrapForDecl(
        param, var->isImplicitlyUnwrappedOptional());

    // Don't allow the parameter to accept temporary pointer conversions.
    param->setNonEphemeralIfPossible();

    valueParameters.push_back(param);
  }

  auto *paramList = ParameterList::create(context, valueParameters);

  // Create the constructor
  DeclName name(context, DeclBaseName::createConstructor(), paramList);
  auto constructor = new (context)
      ConstructorDecl(name, structDecl->getLoc(),
                      /*Failable=*/false, /*FailabilityLoc=*/SourceLoc(),
                      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
                      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                      /*ThrownType=*/TypeLoc(), paramList,
                      /*GenericParams=*/nullptr, structDecl,
                      /*LifetimeDependentTypeRepr*/ nullptr);

  constructor->setAccess(AccessLevel::Public);

  // Make the constructor transparent so we inline it away completely.
  constructor->getAttrs().add(new (context) TransparentAttr(/*implicit*/ true));

  if (wantBody) {
    auto memberMemory =
        context.AllocateUninitialized<uintptr_t>(members.size() + 1);
    memberMemory[0] = members.size();
    for (unsigned i : indices(members)) {
      memberMemory[i + 1] = reinterpret_cast<uintptr_t>(members[i]);
    }
    constructor->setBodySynthesizer(synthesizeValueConstructorBody,
                                    memberMemory.data());
  }

  // We're done.
  return constructor;
}

// MARK: Struct RawValue initializers

/// Synthesizer callback for a raw value bridging constructor body.
static std::pair<BraceStmt *, bool>
synthesizeRawValueBridgingConstructorBody(AbstractFunctionDecl *afd,
                                          void *context) {
  auto init = cast<ConstructorDecl>(afd);
  VarDecl *storedRawValue = static_cast<VarDecl *>(context);

  ASTContext &ctx = init->getASTContext();

  auto selfDecl = init->getImplicitSelfDecl();
  auto storedType = storedRawValue->getInterfaceType();

  // Construct left-hand side.
  Expr *lhs = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                    /*Implicit=*/true);
  lhs->setType(LValueType::get(selfDecl->getTypeInContext()));

  lhs = new (ctx)
      MemberRefExpr(lhs, SourceLoc(), storedRawValue, DeclNameLoc(),
                    /*Implicit=*/true, AccessSemantics::DirectToStorage);
  lhs->setType(LValueType::get(storedType));

  // Construct right-hand side.
  // FIXME: get the parameter from the init, and plug it in here.
  auto *paramDecl = init->getParameters()->get(0);
  auto *paramRef =
      new (ctx) DeclRefExpr(paramDecl, DeclNameLoc(), /*Implicit=*/true);
  paramRef->setType(paramDecl->getTypeInContext());

  Expr *rhs = paramRef;
  if (!storedRawValue->getInterfaceType()->isEqual(paramDecl->getInterfaceType())) {
    auto bridge = new (ctx) BridgeToObjCExpr(paramRef, storedType);
    bridge->setType(storedType);

    rhs = CoerceExpr::createImplicit(ctx, bridge, storedType);
  }

  // Add assignment.
  auto assign = new (ctx) AssignExpr(lhs, SourceLoc(), rhs,
                                     /*Implicit=*/true);
  assign->setType(TupleType::getEmpty(ctx));

  auto *ret = ReturnStmt::createImplicit(ctx, /*expr*/ nullptr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {assign, ret}, SourceLoc());
  return {body, /*isTypeChecked=*/true};
}

ConstructorDecl *SwiftDeclSynthesizer::createRawValueBridgingConstructor(
    StructDecl *structDecl, VarDecl *computedRawValue, VarDecl *storedRawValue,
    bool wantLabel, bool wantBody) {
  auto init = createValueConstructor(structDecl, computedRawValue,
                                     /*wantCtorParamNames=*/wantLabel,
                                     /*wantBody=*/false);
  // Insert our custom init body
  if (wantBody) {
    init->setBodySynthesizer(synthesizeRawValueBridgingConstructorBody,
                             storedRawValue);
  }

  return init;
}

void SwiftDeclSynthesizer::makeStructRawValuedWithBridge(
    StructDecl *structDecl, Type storedUnderlyingType, Type bridgedType,
    ArrayRef<KnownProtocolKind> synthesizedProtocolAttrs,
    bool makeUnlabeledValueInit) {
  auto &ctx = ImporterImpl.SwiftContext;

  ImporterImpl.addSynthesizedProtocolAttrs(structDecl,
                                           synthesizedProtocolAttrs);

  auto storedVarName = ctx.getIdentifier("_rawValue");
  auto computedVarName = ctx.Id_rawValue;

  // Create a variable to store the underlying value.
  VarDecl *storedVar;
  PatternBindingDecl *storedPatternBinding;
  std::tie(storedVar, storedPatternBinding) = createVarWithPattern(
      structDecl, storedVarName, storedUnderlyingType, VarDecl::Introducer::Var,
      /*isImplicit=*/true, AccessLevel::Private, AccessLevel::Private);

  // Create a computed value variable.
  auto computedVar = new (ctx) VarDecl(
      /*IsStatic*/ false, VarDecl::Introducer::Var, SourceLoc(),
      computedVarName, structDecl);
  computedVar->setInterfaceType(bridgedType);
  computedVar->setImplicit();
  computedVar->setAccess(AccessLevel::Public);
  computedVar->setSetterAccess(AccessLevel::Private);

  // Create the getter for the computed value variable.
  auto computedVarGetter =
      makeStructRawValueGetter(structDecl, computedVar, storedVar);
  ImporterImpl.makeComputed(computedVar, computedVarGetter, nullptr);

  // Create a pattern binding to describe the variable.
  Pattern *computedBindingPattern = createTypedNamedPattern(computedVar);
  auto *computedPatternBinding = PatternBindingDecl::createImplicit(
      ctx, StaticSpellingKind::None, computedBindingPattern,
      /*InitExpr*/ nullptr, structDecl);

  auto init =
      createRawValueBridgingConstructor(structDecl, computedVar, storedVar,
                                        /*wantLabel*/ true,
                                        /*wantBody*/ true);

  ConstructorDecl *unlabeledCtor = nullptr;
  if (makeUnlabeledValueInit)
    unlabeledCtor = createRawValueBridgingConstructor(
        structDecl, computedVar, storedVar,
        /*wantLabel*/ false, /*wantBody*/ true);

  if (unlabeledCtor)
    structDecl->addMember(unlabeledCtor);
  structDecl->addMember(init);
  structDecl->addMember(storedPatternBinding);
  structDecl->addMember(storedVar);
  structDecl->addMember(computedPatternBinding);
  structDecl->addMember(computedVar);

  ImporterImpl.addSynthesizedTypealias(structDecl, ctx.Id_RawValue,
                                       bridgedType);
  ImporterImpl.RawTypes[structDecl] = bridgedType;
}

void SwiftDeclSynthesizer::makeStructRawValued(
    StructDecl *structDecl, Type underlyingType,
    ArrayRef<KnownProtocolKind> synthesizedProtocolAttrs,
    MakeStructRawValuedOptions options, AccessLevel setterAccess) {
  auto &ctx = ImporterImpl.SwiftContext;

  ImporterImpl.addSynthesizedProtocolAttrs(structDecl,
                                           synthesizedProtocolAttrs);

  // Create a variable to store the underlying value.
  VarDecl *var;
  PatternBindingDecl *patternBinding;
  auto introducer = (options.contains(MakeStructRawValuedFlags::IsLet)
                         ? VarDecl::Introducer::Let
                         : VarDecl::Introducer::Var);
  std::tie(var, patternBinding) = createVarWithPattern(
      structDecl, ctx.Id_rawValue, underlyingType, introducer,
      options.contains(MakeStructRawValuedFlags::IsImplicit),
      AccessLevel::Public, setterAccess);

  assert(var->hasStorage());

  // Create constructors to initialize that value from a value of the
  // underlying type.
  if (options.contains(MakeStructRawValuedFlags::MakeUnlabeledValueInit))
    structDecl->addMember(createValueConstructor(structDecl, var,
                                                 /*wantCtorParamNames=*/false,
                                                 /*wantBody=*/true));

  auto *initRawValue = createValueConstructor(structDecl, var,
                                              /*wantCtorParamNames=*/true,
                                              /*wantBody=*/true);
  structDecl->addMember(initRawValue);
  structDecl->addMember(patternBinding);
  structDecl->addMember(var);

  ImporterImpl.addSynthesizedTypealias(structDecl, ctx.Id_RawValue,
                                       underlyingType);
  ImporterImpl.RawTypes[structDecl] = underlyingType;
}

// MARK: Unions

/// Synthesizer for the body of a union field getter.
static std::pair<BraceStmt *, bool>
synthesizeUnionFieldGetterBody(AbstractFunctionDecl *afd, void *context) {
  auto getterDecl = cast<AccessorDecl>(afd);
  ASTContext &ctx = getterDecl->getASTContext();
  auto importedFieldDecl = static_cast<VarDecl *>(context);

  auto selfDecl = getterDecl->getImplicitSelfDecl();

  auto selfRef = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                       /*implicit*/ true);
  selfRef->setType(selfDecl->getInterfaceType());

  auto reinterpretCast = cast<FuncDecl>(
      getBuiltinValueDecl(ctx, ctx.getIdentifier("reinterpretCast")));

  ConcreteDeclRef reinterpretCastRef(
      reinterpretCast,
      SubstitutionMap::get(
          reinterpretCast->getGenericSignature(),
          {selfDecl->getInterfaceType(), importedFieldDecl->getInterfaceType()},
          LookUpConformanceInModule()));
  auto reinterpretCastRefExpr =
      new (ctx) DeclRefExpr(reinterpretCastRef, DeclNameLoc(),
                            /*implicit*/ true);
  // FIXME: Verify ExtInfo state is correct, not working by accident.
  FunctionType::ExtInfo info;
  reinterpretCastRefExpr->setType(
      FunctionType::get(AnyFunctionType::Param(selfDecl->getInterfaceType()),
                        importedFieldDecl->getInterfaceType(), info));

  auto *argList = ArgumentList::forImplicitUnlabeled(ctx, {selfRef});
  auto reinterpreted =
      CallExpr::createImplicit(ctx, reinterpretCastRefExpr, argList);
  reinterpreted->setType(importedFieldDecl->getInterfaceType());
  reinterpreted->setThrows(nullptr);
  auto *ret = ReturnStmt::createImplicit(ctx, reinterpreted);
  auto body = BraceStmt::create(ctx, SourceLoc(), ASTNode(ret), SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked*/ true};
}

/// Synthesizer for the body of a union field setter.
static std::pair<BraceStmt *, bool>
synthesizeUnionFieldSetterBody(AbstractFunctionDecl *afd, void *context) {
  auto setterDecl = cast<AccessorDecl>(afd);
  ASTContext &ctx = setterDecl->getASTContext();

  auto inoutSelfDecl = setterDecl->getImplicitSelfDecl();

  auto inoutSelfRef = new (ctx) DeclRefExpr(inoutSelfDecl, DeclNameLoc(),
                                            /*implicit*/ true);
  inoutSelfRef->setType(LValueType::get(inoutSelfDecl->getInterfaceType()));

  auto newValueDecl = setterDecl->getParameters()->get(0);

  auto newValueRef = new (ctx) DeclRefExpr(newValueDecl, DeclNameLoc(),
                                           /*implicit*/ true);
  newValueRef->setType(newValueDecl->getInterfaceType());

  auto addressofFn =
      cast<FuncDecl>(getBuiltinValueDecl(ctx, ctx.getIdentifier("unprotectedAddressOf")));
  ConcreteDeclRef addressofFnRef(
      addressofFn, SubstitutionMap::get(addressofFn->getGenericSignature(),
                                        {inoutSelfDecl->getInterfaceType()},
                                        LookUpConformanceInModule()));
  auto addressofFnRefExpr =
      new (ctx) DeclRefExpr(addressofFnRef, DeclNameLoc(), /*implicit*/ true);
  // FIXME: Verify ExtInfo state is correct, not working by accident.
  FunctionType::ExtInfo addressOfInfo;
  addressofFnRefExpr->setType(FunctionType::get(
      AnyFunctionType::Param(inoutSelfDecl->getInterfaceType(), Identifier(),
                             ParameterTypeFlags().withInOut(true)),
      ctx.TheRawPointerType, addressOfInfo));

  auto *selfPtrArgs = ArgumentList::createImplicit(
      ctx, {Argument::implicitInOut(ctx, inoutSelfRef)});
  auto selfPointer =
      CallExpr::createImplicit(ctx, addressofFnRefExpr, selfPtrArgs);
  selfPointer->setType(ctx.TheRawPointerType);
  selfPointer->setThrows(nullptr);

  auto initializeFn =
      cast<FuncDecl>(getBuiltinValueDecl(ctx, ctx.getIdentifier("initialize")));
  ConcreteDeclRef initializeFnRef(
      initializeFn, SubstitutionMap::get(initializeFn->getGenericSignature(),
                                         {newValueDecl->getInterfaceType()},
                                         LookUpConformanceInModule()));
  auto initializeFnRefExpr =
      new (ctx) DeclRefExpr(initializeFnRef, DeclNameLoc(), /*implicit*/ true);
  // FIXME: Verify ExtInfo state is correct, not working by accident.
  FunctionType::ExtInfo initializeInfo;
  initializeFnRefExpr->setType(FunctionType::get(
      {AnyFunctionType::Param(newValueDecl->getInterfaceType()),
       AnyFunctionType::Param(ctx.TheRawPointerType)},
      TupleType::getEmpty(ctx), initializeInfo));

  auto *initArgs =
      ArgumentList::forImplicitUnlabeled(ctx, {newValueRef, selfPointer});
  auto initialize =
      CallExpr::createImplicit(ctx, initializeFnRefExpr, initArgs);
  initialize->setType(TupleType::getEmpty(ctx));
  initialize->setThrows(nullptr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {initialize}, SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked*/ true};
}

std::pair<AccessorDecl *, AccessorDecl *>
SwiftDeclSynthesizer::makeUnionFieldAccessors(
    NominalTypeDecl *importedUnionDecl, VarDecl *importedFieldDecl) {
  auto &C = ImporterImpl.SwiftContext;

  auto getterDecl =
      makeFieldGetterDecl(ImporterImpl, importedUnionDecl, importedFieldDecl);
  getterDecl->setBodySynthesizer(synthesizeUnionFieldGetterBody,
                                 importedFieldDecl);
  getterDecl->getAttrs().add(new (C) TransparentAttr(/*implicit*/ true));

  auto setterDecl =
      makeFieldSetterDecl(ImporterImpl, importedUnionDecl, importedFieldDecl);
  setterDecl->setBodySynthesizer(synthesizeUnionFieldSetterBody,
                                 importedFieldDecl);
  setterDecl->getAttrs().add(new (C) TransparentAttr(/*implicit*/ true));

  ImporterImpl.makeComputed(importedFieldDecl, getterDecl, setterDecl);
  return {getterDecl, setterDecl};
}

static clang::DeclarationName
getAccessorDeclarationName(clang::ASTContext &Ctx, NominalTypeDecl *structDecl,
                           VarDecl *fieldDecl, const char *suffix) {
  std::string id;
  llvm::raw_string_ostream IdStream(id);
  Mangle::ASTMangler mangler(structDecl->getASTContext());
  IdStream << "$" << mangler.mangleDeclAsUSR(structDecl, "") << "$"
           << fieldDecl->getName() << "$" << suffix;

  return clang::DeclarationName(&Ctx.Idents.get(IdStream.str()));
}

std::pair<FuncDecl *, FuncDecl *> SwiftDeclSynthesizer::makeBitFieldAccessors(
    clang::RecordDecl *structDecl, NominalTypeDecl *importedStructDecl,
    clang::FieldDecl *fieldDecl, VarDecl *importedFieldDecl) {
  clang::ASTContext &Ctx = ImporterImpl.getClangASTContext();

  // Getter: static inline FieldType get(RecordType self);
  auto recordType = Ctx.getRecordType(structDecl);
  auto recordPointerType = Ctx.getPointerType(recordType);
  auto fieldType = fieldDecl->getType();

  auto cGetterName = getAccessorDeclarationName(Ctx, importedStructDecl,
                                                importedFieldDecl, "getter");
  auto cGetterType =
      Ctx.getFunctionType(fieldDecl->getType(), recordType,
                          clang::FunctionProtoType::ExtProtoInfo());
  auto cGetterTypeInfo = Ctx.getTrivialTypeSourceInfo(cGetterType);
  auto cGetterDecl = clang::FunctionDecl::Create(
      Ctx, structDecl->getDeclContext(), clang::SourceLocation(),
      clang::SourceLocation(), cGetterName, cGetterType, cGetterTypeInfo,
      clang::SC_Static);
  cGetterDecl->setImplicit();
  cGetterDecl->setImplicitlyInline();
  assert(!cGetterDecl->isExternallyVisible());

  auto getterDecl = makeFieldGetterDecl(ImporterImpl, importedStructDecl,
                                        importedFieldDecl, cGetterDecl);

  // Setter: static inline void set(FieldType newValue, RecordType *self);
  SmallVector<clang::QualType, 8> cSetterParamTypes;
  cSetterParamTypes.push_back(fieldType);
  cSetterParamTypes.push_back(recordPointerType);

  auto cSetterName = getAccessorDeclarationName(Ctx, importedStructDecl,
                                                importedFieldDecl, "setter");
  auto cSetterType = Ctx.getFunctionType(
      Ctx.VoidTy, cSetterParamTypes, clang::FunctionProtoType::ExtProtoInfo());
  auto cSetterTypeInfo = Ctx.getTrivialTypeSourceInfo(cSetterType);

  auto cSetterDecl = clang::FunctionDecl::Create(
      Ctx, structDecl->getDeclContext(), clang::SourceLocation(),
      clang::SourceLocation(), cSetterName, cSetterType, cSetterTypeInfo,
      clang::SC_Static);
  cSetterDecl->setImplicit();
  cSetterDecl->setImplicitlyInline();
  assert(!cSetterDecl->isExternallyVisible());

  auto setterDecl = makeFieldSetterDecl(ImporterImpl, importedStructDecl,
                                        importedFieldDecl, cSetterDecl);

  ImporterImpl.makeComputed(importedFieldDecl, getterDecl, setterDecl);

  // Synthesize the getter body
  {
    auto cGetterSelfId = nullptr;
    auto recordTypeInfo = Ctx.getTrivialTypeSourceInfo(recordType);
    auto cGetterSelf = clang::ParmVarDecl::Create(
        Ctx, cGetterDecl, clang::SourceLocation(), clang::SourceLocation(),
        cGetterSelfId, recordType, recordTypeInfo, clang::SC_None, nullptr);
    cGetterSelf->setImplicit();
    cGetterDecl->setParams(cGetterSelf);

    auto cGetterSelfExpr = new (Ctx)
        clang::DeclRefExpr(Ctx, cGetterSelf, false, recordType,
                           clang::VK_PRValue, clang::SourceLocation());
    auto cGetterExpr = clang::MemberExpr::CreateImplicit(
        Ctx, cGetterSelfExpr,
        /*isarrow=*/false, fieldDecl, fieldType, clang::VK_PRValue,
        clang::OK_BitField);

    auto cGetterBody = clang::ReturnStmt::Create(Ctx, clang::SourceLocation(),
                                                 cGetterExpr, nullptr);
    cGetterDecl->setBody(cGetterBody);
  }

  // Synthesize the setter body
  {
    SmallVector<clang::ParmVarDecl *, 2> cSetterParams;
    auto fieldTypeInfo = Ctx.getTrivialTypeSourceInfo(fieldType);
    auto cSetterValue = clang::ParmVarDecl::Create(
        Ctx, cSetterDecl, clang::SourceLocation(), clang::SourceLocation(),
        /* nameID? */ nullptr, fieldType, fieldTypeInfo, clang::SC_None,
        nullptr);
    cSetterValue->setImplicit();
    cSetterParams.push_back(cSetterValue);
    auto recordPointerTypeInfo =
        Ctx.getTrivialTypeSourceInfo(recordPointerType);
    auto cSetterSelf = clang::ParmVarDecl::Create(
        Ctx, cSetterDecl, clang::SourceLocation(), clang::SourceLocation(),
        /* nameID? */ nullptr, recordPointerType, recordPointerTypeInfo,
        clang::SC_None, nullptr);
    cSetterSelf->setImplicit();
    cSetterParams.push_back(cSetterSelf);
    cSetterDecl->setParams(cSetterParams);

    auto cSetterSelfExpr = new (Ctx)
        clang::DeclRefExpr(Ctx, cSetterSelf, false, recordPointerType,
                           clang::VK_PRValue, clang::SourceLocation());

    auto cSetterMemberExpr = clang::MemberExpr::CreateImplicit(
        Ctx, cSetterSelfExpr,
        /*isarrow=*/true, fieldDecl, fieldType, clang::VK_LValue,
        clang::OK_BitField);

    auto cSetterValueExpr = new (Ctx)
        clang::DeclRefExpr(Ctx, cSetterValue, false, fieldType,
                           clang::VK_PRValue, clang::SourceLocation());

    auto cSetterExpr = clang::BinaryOperator::Create(
        Ctx, cSetterMemberExpr, cSetterValueExpr, clang::BO_Assign, fieldType,
        clang::VK_PRValue, clang::OK_Ordinary, clang::SourceLocation(),
        clang::FPOptionsOverride());

    cSetterDecl->setBody(cSetterExpr);
  }

  return {getterDecl, setterDecl};
}

/// Find the anonymous inner field declaration for the given anonymous field.
static VarDecl *findAnonymousInnerFieldDecl(VarDecl *importedFieldDecl,
                                            VarDecl *anonymousFieldDecl) {
  auto anonymousFieldType = anonymousFieldDecl->getInterfaceType();
  auto anonymousFieldTypeDecl =
      anonymousFieldType->getStructOrBoundGenericStruct();

  for (auto decl :
       anonymousFieldTypeDecl->lookupDirect(importedFieldDecl->getName())) {
    if (isa<VarDecl>(decl)) {
      return cast<VarDecl>(decl);
    }
  }

  llvm_unreachable("couldn't find anonymous inner field decl");
}

// MARK: Indirect fields

/// Synthesize the getter body for an indirect field.
static std::pair<BraceStmt *, bool>
synthesizeIndirectFieldGetterBody(AbstractFunctionDecl *afd, void *context) {
  auto getterDecl = cast<AccessorDecl>(afd);
  auto anonymousFieldDecl = static_cast<VarDecl *>(context);

  ASTContext &ctx = getterDecl->getASTContext();
  auto selfDecl = getterDecl->getImplicitSelfDecl();
  Expr *expr = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                     /*implicit*/ true);
  expr->setType(selfDecl->getInterfaceType());

  expr = new (ctx) MemberRefExpr(expr, SourceLoc(), anonymousFieldDecl,
                                 DeclNameLoc(), /*implicit*/ true);
  expr->setType(anonymousFieldDecl->getInterfaceType());

  auto importedFieldDecl = cast<VarDecl>(getterDecl->getStorage());
  auto anonymousInnerFieldDecl =
      findAnonymousInnerFieldDecl(importedFieldDecl, anonymousFieldDecl);
  expr = new (ctx) MemberRefExpr(expr, SourceLoc(), anonymousInnerFieldDecl,
                                 DeclNameLoc(), /*implicit*/ true);
  expr->setType(anonymousInnerFieldDecl->getInterfaceType());

  auto *ret = ReturnStmt::createImplicit(ctx, expr);
  auto body = BraceStmt::create(ctx, SourceLoc(), ASTNode(ret), SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked=*/true};
}

/// Synthesize the setter body for an indirect field.
static std::pair<BraceStmt *, bool>
synthesizeIndirectFieldSetterBody(AbstractFunctionDecl *afd, void *context) {
  auto setterDecl = cast<AccessorDecl>(afd);
  auto anonymousFieldDecl = static_cast<VarDecl *>(context);

  ASTContext &ctx = setterDecl->getASTContext();
  auto selfDecl = setterDecl->getImplicitSelfDecl();
  Expr *lhs = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                    /*implicit*/ true);
  lhs->setType(LValueType::get(selfDecl->getInterfaceType()));

  lhs = new (ctx) MemberRefExpr(lhs, SourceLoc(), anonymousFieldDecl,
                                DeclNameLoc(), /*implicit*/ true);
  lhs->setType(LValueType::get(anonymousFieldDecl->getInterfaceType()));

  auto importedFieldDecl = cast<VarDecl>(setterDecl->getStorage());
  auto anonymousInnerFieldDecl =
      findAnonymousInnerFieldDecl(importedFieldDecl, anonymousFieldDecl);

  lhs = new (ctx) MemberRefExpr(lhs, SourceLoc(), anonymousInnerFieldDecl,
                                DeclNameLoc(), /*implicit*/ true);
  lhs->setType(LValueType::get(anonymousInnerFieldDecl->getInterfaceType()));

  auto newValueDecl = setterDecl->getParameters()->get(0);

  auto rhs = new (ctx) DeclRefExpr(newValueDecl, DeclNameLoc(),
                                   /*implicit*/ true);
  rhs->setType(newValueDecl->getInterfaceType());

  auto assign = new (ctx) AssignExpr(lhs, SourceLoc(), rhs, /*implicit*/ true);
  assign->setType(TupleType::getEmpty(ctx));

  auto body = BraceStmt::create(ctx, SourceLoc(), {assign}, SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked=*/true};
}

std::pair<AccessorDecl *, AccessorDecl *>
SwiftDeclSynthesizer::makeIndirectFieldAccessors(
    const clang::IndirectFieldDecl *indirectField, ArrayRef<VarDecl *> members,
    NominalTypeDecl *importedStructDecl, VarDecl *importedFieldDecl) {
  auto &C = ImporterImpl.SwiftContext;

  auto getterDecl =
      makeFieldGetterDecl(ImporterImpl, importedStructDecl, importedFieldDecl);
  getterDecl->getAttrs().add(new (C) TransparentAttr(/*implicit*/ true));

  auto setterDecl =
      makeFieldSetterDecl(ImporterImpl, importedStructDecl, importedFieldDecl);
  setterDecl->getAttrs().add(new (C) TransparentAttr(/*implicit*/ true));

  ImporterImpl.makeComputed(importedFieldDecl, getterDecl, setterDecl);

  auto containingField = indirectField->chain().front();
  VarDecl *anonymousFieldDecl = nullptr;

  // Reverse scan of the members because indirect field are generated just
  // after the corresponding anonymous type, so a reverse scan allows
  // switching from O(n) to O(1) here.
  for (auto decl : reverse(members)) {
    if (decl->getClangDecl() == containingField) {
      anonymousFieldDecl = cast<VarDecl>(decl);
      break;
    }
  }
  assert(anonymousFieldDecl && "anonymous field not generated");
  getterDecl->setBodySynthesizer(synthesizeIndirectFieldGetterBody,
                                 anonymousFieldDecl);
  setterDecl->setBodySynthesizer(synthesizeIndirectFieldSetterBody,
                                 anonymousFieldDecl);

  return {getterDecl, setterDecl};
}

// MARK: Enum RawValue initializers

/// Synthesize the body of \c init?(rawValue:RawType) for an imported enum.
static std::pair<BraceStmt *, bool>
synthesizeEnumRawValueConstructorBody(AbstractFunctionDecl *afd,
                                      void *context) {
  ASTContext &ctx = afd->getASTContext();

  auto ctorDecl = cast<ConstructorDecl>(afd);
  auto enumDecl = static_cast<EnumDecl *>(context);
  auto selfDecl = ctorDecl->getImplicitSelfDecl();
  auto selfRef = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                       /*implicit*/ true);
  selfRef->setType(LValueType::get(selfDecl->getTypeInContext()));

  auto param = ctorDecl->getParameters()->get(0);
  auto paramRef = new (ctx) DeclRefExpr(param, DeclNameLoc(),
                                        /*implicit*/ true);
  paramRef->setType(param->getTypeInContext());

  auto reinterpretCast = cast<FuncDecl>(
      getBuiltinValueDecl(ctx, ctx.getIdentifier("reinterpretCast")));
  auto rawTy = enumDecl->getRawType();
  auto enumTy = enumDecl->getDeclaredInterfaceType();
  SubstitutionMap subMap = SubstitutionMap::get(
      reinterpretCast->getGenericSignature(), {rawTy, enumTy},
      LookUpConformanceInModule());
  ConcreteDeclRef concreteDeclRef(reinterpretCast, subMap);
  auto reinterpretCastRef =
      new (ctx) DeclRefExpr(concreteDeclRef, DeclNameLoc(), /*implicit*/ true);
  // FIXME: Verify ExtInfo state is correct, not working by accident.
  FunctionType::ExtInfo info;
  reinterpretCastRef->setType(
      FunctionType::get({FunctionType::Param(rawTy)}, enumTy, info));

  auto *argList = ArgumentList::forImplicitUnlabeled(ctx, {paramRef});
  auto reinterpreted =
      CallExpr::createImplicit(ctx, reinterpretCastRef, argList);
  reinterpreted->setType(enumTy);
  reinterpreted->setThrows(nullptr);

  auto assign = new (ctx) AssignExpr(selfRef, SourceLoc(), reinterpreted,
                                     /*implicit*/ true);
  assign->setType(TupleType::getEmpty(ctx));

  auto *ret = ReturnStmt::createImplicit(ctx, /*expr*/ nullptr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {assign, ret}, SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked=*/true};
}

ConstructorDecl *
SwiftDeclSynthesizer::makeEnumRawValueConstructor(EnumDecl *enumDecl) {
  ASTContext &C = ImporterImpl.SwiftContext;
  auto rawTy = enumDecl->getRawType();

  auto param = new (C) ParamDecl(SourceLoc(), SourceLoc(), C.Id_rawValue,
                                 SourceLoc(), C.Id_rawValue, enumDecl);
  param->setSpecifier(ParamSpecifier::Default);
  param->setInterfaceType(rawTy);

  auto paramPL = ParameterList::createWithoutLoc(param);

  DeclName name(C, DeclBaseName::createConstructor(), paramPL);
  auto *ctorDecl =
      new (C) ConstructorDecl(name, enumDecl->getLoc(),
                              /*Failable=*/true, /*FailabilityLoc=*/SourceLoc(),
                              /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
                              /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                              /*ThrownType=*/TypeLoc(), paramPL,
                              /*GenericParams=*/nullptr, enumDecl,
                              /*LifetimeDependentTypeRepr*/ nullptr);
  ctorDecl->setImplicit();
  ctorDecl->setAccess(AccessLevel::Public);
  ctorDecl->setBodySynthesizer(synthesizeEnumRawValueConstructorBody, enumDecl);
  return ctorDecl;
}

// MARK: Enum RawValue getters & setters

/// Synthesizer callback for an enum's rawValue getter.
static std::pair<BraceStmt *, bool>
synthesizeEnumRawValueGetterBody(AbstractFunctionDecl *afd, void *context) {
  auto getterDecl = cast<AccessorDecl>(afd);
  auto enumDecl = static_cast<EnumDecl *>(context);
  auto rawTy = enumDecl->getRawType();
  auto enumTy = enumDecl->getDeclaredInterfaceType();

  ASTContext &ctx = getterDecl->getASTContext();
  auto *selfDecl = getterDecl->getImplicitSelfDecl();
  auto selfRef = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                       /*implicit*/ true);
  selfRef->setType(selfDecl->getTypeInContext());

  auto reinterpretCast = cast<FuncDecl>(
      getBuiltinValueDecl(ctx, ctx.getIdentifier("reinterpretCast")));
  SubstitutionMap subMap = SubstitutionMap::get(
      reinterpretCast->getGenericSignature(), {enumTy, rawTy},
      LookUpConformanceInModule());
  ConcreteDeclRef concreteDeclRef(reinterpretCast, subMap);

  auto reinterpretCastRef =
      new (ctx) DeclRefExpr(concreteDeclRef, DeclNameLoc(), /*implicit*/ true);
  // FIXME: Verify ExtInfo state is correct, not working by accident.
  FunctionType::ExtInfo info;
  reinterpretCastRef->setType(
      FunctionType::get({FunctionType::Param(enumTy)}, rawTy, info));

  auto *argList = ArgumentList::forImplicitUnlabeled(ctx, {selfRef});
  auto reinterpreted =
      CallExpr::createImplicit(ctx, reinterpretCastRef, argList);
  reinterpreted->setType(rawTy);
  reinterpreted->setThrows(nullptr);

  auto *ret = ReturnStmt::createImplicit(ctx, reinterpreted);
  auto body = BraceStmt::create(ctx, SourceLoc(), ASTNode(ret), SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked=*/true};
}

// Build the rawValue getter for an imported NS_ENUM.
//   enum NSSomeEnum: RawType {
//     var rawValue: RawType {
//       return Builtin.reinterpretCast(self)
//     }
//   }
// Unlike a standard init(rawValue:) enum initializer, this does a reinterpret
// cast in order to preserve unknown or future cases from C.
void SwiftDeclSynthesizer::makeEnumRawValueGetter(EnumDecl *enumDecl,
                                                  VarDecl *rawValueDecl) {
  ASTContext &C = ImporterImpl.SwiftContext;

  auto rawTy = enumDecl->getRawType();

  auto *params = ParameterList::createEmpty(C);

  auto getterDecl = AccessorDecl::create(
      C,
      /*declLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(), AccessorKind::Get, rawValueDecl,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(),
      params, rawTy, enumDecl);
  getterDecl->setImplicit();
  getterDecl->setIsObjC(false);
  getterDecl->setIsDynamic(false);
  getterDecl->setIsTransparent(false);

  getterDecl->setAccess(AccessLevel::Public);
  getterDecl->setBodySynthesizer(synthesizeEnumRawValueGetterBody, enumDecl);
  ImporterImpl.makeComputed(rawValueDecl, getterDecl, nullptr);
}

// MARK: Struct RawValue getters

/// Synthesizer for the rawValue getter for an imported struct.
static std::pair<BraceStmt *, bool>
synthesizeStructRawValueGetterBody(AbstractFunctionDecl *afd, void *context) {
  auto getterDecl = cast<AccessorDecl>(afd);
  VarDecl *storedVar = static_cast<VarDecl *>(context);

  ASTContext &ctx = getterDecl->getASTContext();
  auto *selfDecl = getterDecl->getImplicitSelfDecl();
  auto selfRef = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                       /*implicit*/ true);
  selfRef->setType(selfDecl->getTypeInContext());

  auto storedType = storedVar->getInterfaceType();
  auto storedRef = new (ctx)
      MemberRefExpr(selfRef, SourceLoc(), storedVar, DeclNameLoc(),
                    /*Implicit=*/true, AccessSemantics::DirectToStorage);
  storedRef->setType(storedType);

  Expr *result = storedRef;

  Type computedType = getterDecl->getResultInterfaceType();
  if (!computedType->isEqual(storedType)) {
    auto bridge = new (ctx) BridgeFromObjCExpr(storedRef, computedType);
    bridge->setType(computedType);

    result = CoerceExpr::createImplicit(ctx, bridge, computedType);
  }

  auto ret = ReturnStmt::createImplicit(ctx, result);
  auto body = BraceStmt::create(ctx, SourceLoc(), ASTNode(ret), SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked=*/true};
}

AccessorDecl *SwiftDeclSynthesizer::makeStructRawValueGetter(
    StructDecl *structDecl, VarDecl *computedVar, VarDecl *storedVar) {
  assert(storedVar->hasStorage());

  ASTContext &C = ImporterImpl.SwiftContext;

  auto *params = ParameterList::createEmpty(C);

  auto computedType = computedVar->getInterfaceType();

  auto getterDecl = AccessorDecl::create(
      C,
      /*declLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(), AccessorKind::Get, computedVar,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(),
      params, computedType, structDecl);
  getterDecl->setImplicit();
  getterDecl->setIsObjC(false);
  getterDecl->setIsDynamic(false);
  getterDecl->setIsTransparent(false);

  getterDecl->setAccess(AccessLevel::Public);
  getterDecl->setBodySynthesizer(synthesizeStructRawValueGetterBody, storedVar);
  return getterDecl;
}

// MARK: ObjC subscripts

AccessorDecl *SwiftDeclSynthesizer::buildSubscriptGetterDecl(
    SubscriptDecl *subscript, const FuncDecl *getter, Type elementTy,
    DeclContext *dc, ParamDecl *index) {
  auto &C = ImporterImpl.SwiftContext;
  auto loc = getter->getLoc();

  auto *params = ParameterList::create(C, index);

  // Create the getter thunk.
  auto thunk = AccessorDecl::create(
      C,
      /*declLoc=*/loc,
      /*AccessorKeywordLoc=*/SourceLoc(), AccessorKind::Get, subscript,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(),
      params, elementTy, dc, getter->getClangNode());

  thunk->setAccess(getOverridableAccessLevel(dc));

  if (auto objcAttr = getter->getAttrs().getAttribute<ObjCAttr>())
    thunk->getAttrs().add(objcAttr->clone(C));
  thunk->setIsObjC(getter->isObjC());
  thunk->setIsDynamic(getter->isDynamic());
  // FIXME: Should we record thunks?

  return thunk;
}

AccessorDecl *SwiftDeclSynthesizer::buildSubscriptSetterDecl(
    SubscriptDecl *subscript, const FuncDecl *setter, Type elementInterfaceTy,
    DeclContext *dc, ParamDecl *index) {
  auto &C = ImporterImpl.SwiftContext;
  auto loc = setter->getLoc();

  // Objective-C subscript setters are imported with a function type
  // such as:
  //
  //   (self) -> (value, index) -> ()
  //
  // Build a setter thunk with the latter signature that maps to the
  // former.
  auto valueIndex = setter->getParameters();

  auto paramVarDecl = new (C) ParamDecl(SourceLoc(), SourceLoc(), Identifier(),
                                        loc, valueIndex->get(0)->getName(), dc);
  paramVarDecl->setSpecifier(ParamSpecifier::Default);
  paramVarDecl->setInterfaceType(elementInterfaceTy);

  auto valueIndicesPL = ParameterList::create(C, {paramVarDecl, index});

  // Create the setter thunk.
  auto thunk = AccessorDecl::create(
      C,
      /*declLoc=*/setter->getLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(), AccessorKind::Set, subscript,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(),
      valueIndicesPL, TupleType::getEmpty(C), dc,
      setter->getClangNode());

  thunk->setAccess(getOverridableAccessLevel(dc));

  if (auto objcAttr = setter->getAttrs().getAttribute<ObjCAttr>())
    thunk->getAttrs().add(objcAttr->clone(C));
  thunk->setIsObjC(setter->isObjC());
  thunk->setIsDynamic(setter->isDynamic());

  return thunk;
}

// MARK: C++ subscripts

Expr *SwiftDeclSynthesizer::synthesizeReturnReinterpretCast(ASTContext &ctx,
                                                            Type givenType,
                                                            Type exprType,
                                                            Expr *baseExpr) {
  auto reinterpretCast = cast<FuncDecl>(
      getBuiltinValueDecl(ctx, ctx.getIdentifier("reinterpretCast")));

  SubstitutionMap subMap = SubstitutionMap::get(
      reinterpretCast->getGenericSignature(), {givenType, exprType},
      LookUpConformanceInModule());
  ConcreteDeclRef concreteDeclRef(reinterpretCast, subMap);
  auto reinterpretCastRef =
      new (ctx) DeclRefExpr(concreteDeclRef, DeclNameLoc(), /*implicit*/ true);
  FunctionType::ExtInfo info;
  reinterpretCastRef->setType(
      FunctionType::get({FunctionType::Param(givenType)}, exprType, info));

  auto *argList = ArgumentList::forImplicitUnlabeled(ctx, {baseExpr});
  auto reinterpreted =
      CallExpr::createImplicit(ctx, reinterpretCastRef, argList);
  reinterpreted->setType(exprType);
  reinterpreted->setThrows(nullptr);
  return reinterpreted;
}

/// Synthesizer callback for a subscript getter or a getter for a
/// dereference property (`var pointee`). If the getter's implementation returns
/// an UnsafePointer or UnsafeMutablePointer, it unwraps the pointer and returns
/// the underlying value.
static std::pair<BraceStmt *, bool>
synthesizeUnwrappingGetterOrAddressGetterBody(AbstractFunctionDecl *afd,
                                              void *context, bool isAddress) {
  auto getterDecl = cast<AccessorDecl>(afd);
  auto getterImpl = static_cast<FuncDecl *>(context);

  ASTContext &ctx = getterDecl->getASTContext();

  auto selfArg = createSelfArg(getterDecl);
  DeclRefExpr *keyRefExpr = getterDecl->getParameters()->size() == 0
                                ? nullptr
                                : createParamRefExpr(getterDecl, 0);

  Type elementTy = getterDecl->getResultInterfaceType();

  auto *getterImplCallExpr =
      createAccessorImplCallExpr(getterImpl, selfArg, keyRefExpr);

  // This default handles C++'s operator[] that returns a value type.
  Expr *propertyExpr = getterImplCallExpr;
  PointerTypeKind ptrKind;

  // The following check returns true if the subscript operator returns a C++
  // reference type. This check actually checks to see if the type is a pointer
  // type, but this does not apply to C pointers because they are Optional types
  // when imported. TODO: Use a more obvious check here.
  if (!isAddress &&
      getterImpl->getResultInterfaceType()->getAnyPointerElementType(ptrKind)) {
    // `getterImpl` can return either UnsafePointer or UnsafeMutablePointer.
    // Retrieve the corresponding `.pointee` declaration.
    VarDecl *pointeePropertyDecl = ctx.getPointerPointeePropertyDecl(ptrKind);

    // Handle operator[] that returns a reference type.
    SubstitutionMap subMap = SubstitutionMap::get(
        ctx.getUnsafePointerDecl()->getGenericSignature(), {elementTy},
        LookUpConformanceInModule());
    auto pointeePropertyRefExpr = new (ctx) MemberRefExpr(
        getterImplCallExpr, SourceLoc(),
        ConcreteDeclRef(pointeePropertyDecl, subMap), DeclNameLoc(),
        /*implicit*/ true);
    pointeePropertyRefExpr->setType(elementTy);
    propertyExpr = pointeePropertyRefExpr;
  }
  // Cast an 'address' result from a mutable pointer if needed.
  if (isAddress &&
      getterImpl->getResultInterfaceType()->isUnsafeMutablePointer())
    propertyExpr = SwiftDeclSynthesizer::synthesizeReturnReinterpretCast(
        ctx, getterImpl->getResultInterfaceType(), elementTy, propertyExpr);

  auto *returnStmt = ReturnStmt::createImplicit(ctx, propertyExpr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked*/ true};
}

static std::pair<BraceStmt *, bool>
synthesizeUnwrappingGetterBody(AbstractFunctionDecl *afd, void *context) {
  return synthesizeUnwrappingGetterOrAddressGetterBody(afd, context,
                                                       /*isAddress=*/false);
}

static std::pair<BraceStmt *, bool>
synthesizeUnwrappingAddressGetterBody(AbstractFunctionDecl *afd,
                                      void *context) {
  return synthesizeUnwrappingGetterOrAddressGetterBody(afd, context,
                                                       /*isAddress=*/true);
}

/// Synthesizer callback for a subscript setter or a setter for a dereference
/// property (`var pointee`).
static std::pair<BraceStmt *, bool>
synthesizeUnwrappingSetterBody(AbstractFunctionDecl *afd, void *context) {
  auto setterDecl = cast<AccessorDecl>(afd);
  auto setterImpl = static_cast<FuncDecl *>(context);

  ASTContext &ctx = setterDecl->getASTContext();

  auto selfArg = createSelfArg(setterDecl);
  DeclRefExpr *valueParamRefExpr = createParamRefExpr(setterDecl, 0);
  // For a subscript this decl will have two parameters, for a pointee property
  // it will only have one.
  DeclRefExpr *keyParamRefExpr = setterDecl->getParameters()->size() == 1
                                     ? nullptr
                                     : createParamRefExpr(setterDecl, 1);

  Type elementTy = valueParamRefExpr->getDecl()->getInterfaceType();

  auto *setterImplCallExpr =
      createAccessorImplCallExpr(setterImpl, selfArg, keyParamRefExpr);

  VarDecl *pointeePropertyDecl =
      ctx.getPointerPointeePropertyDecl(PTK_UnsafeMutablePointer);

  SubstitutionMap subMap = SubstitutionMap::get(
      ctx.getUnsafeMutablePointerDecl()->getGenericSignature(), {elementTy},
      LookUpConformanceInModule());
  auto pointeePropertyRefExpr = new (ctx)
      MemberRefExpr(setterImplCallExpr, SourceLoc(),
                    ConcreteDeclRef(pointeePropertyDecl, subMap), DeclNameLoc(),
                    /*implicit*/ true);
  pointeePropertyRefExpr->setType(LValueType::get(elementTy));

  auto assignExpr = new (ctx)
      AssignExpr(pointeePropertyRefExpr, SourceLoc(), valueParamRefExpr,
                 /*implicit*/ true);
  assignExpr->setType(TupleType::getEmpty(ctx));

  auto body = BraceStmt::create(ctx, SourceLoc(),
                                {
                                    assignExpr,
                                },
                                SourceLoc());
  return {body, /*isTypeChecked*/ true};
}

static std::pair<BraceStmt *, bool>
synthesizeUnwrappingAddressSetterBody(AbstractFunctionDecl *afd,
                                      void *context) {
  auto setterDecl = cast<AccessorDecl>(afd);
  auto setterImpl = static_cast<FuncDecl *>(context);

  ASTContext &ctx = setterDecl->getASTContext();

  auto selfArg = createSelfArg(setterDecl);
  auto *setterImplCallExpr =
      createAccessorImplCallExpr(setterImpl, selfArg, nullptr);

  auto *returnStmt = ReturnStmt::createImplicit(ctx, setterImplCallExpr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked*/ true};
}

SubscriptDecl *SwiftDeclSynthesizer::makeSubscript(FuncDecl *getter,
                                                   FuncDecl *setter) {
  assert((getter || setter) &&
         "getter or setter required to generate subscript");

  // If only a setter (imported from non-const `operator[]`) is defined,
  // generate both get & set accessors from it.
  FuncDecl *getterImpl = getter ? getter : setter;
  FuncDecl *setterImpl = setter;

  // Get the return type wrapped in `Unsafe(Mutable)Pointer<T>`.
  const auto rawElementTy = getterImpl->getResultInterfaceType();
  // Unwrap `T`. Use rawElementTy for return by value.
  const auto elementTy = rawElementTy->getAnyPointerElementType()
                             ? rawElementTy->getAnyPointerElementType()
                             : rawElementTy;

  auto &ctx = ImporterImpl.SwiftContext;

  assert(getterImpl->getParameters()->size() == 1 &&
         "subscript can only have 1 parameter");
  auto bodyParam = ParamDecl::clone(ctx, getterImpl->getParameters()->get(0));
  // If the subscript parameter is unnamed, give it a name to make sure SILGen
  // creates a variable for it.
  if (bodyParam->getName().empty())
    bodyParam->setName(ctx.getIdentifier("__index"));

  auto bodyParams = ParameterList::create(ctx, bodyParam);
  DeclName name(ctx, DeclBaseName::createSubscript(), bodyParams);
  auto dc = getterImpl->getDeclContext();

  SubscriptDecl *subscript = SubscriptDecl::createImported(
      ctx, name, getterImpl->getLoc(), bodyParams, getterImpl->getLoc(),
      elementTy, dc, getterImpl->getGenericParams(),
      getterImpl->getClangNode());
  subscript->setAccess(AccessLevel::Public);

  AccessorDecl *getterDecl =
      AccessorDecl::create(ctx, getterImpl->getLoc(), getterImpl->getLoc(),
                           AccessorKind::Get, subscript,
                           /*async*/ false, SourceLoc(),
                           /*throws*/ false, SourceLoc(),
                           /*ThrownType=*/TypeLoc(), bodyParams, elementTy, dc);
  getterDecl->setAccess(AccessLevel::Public);
  getterDecl->setImplicit();
  getterDecl->setIsDynamic(false);
  getterDecl->setIsTransparent(true);
  getterDecl->setBodySynthesizer(synthesizeUnwrappingGetterBody, getterImpl);

  if (getterImpl->isMutating()) {
    getterDecl->setSelfAccessKind(SelfAccessKind::Mutating);
    subscript->setIsGetterMutating(true);
  }

  AccessorDecl *setterDecl = nullptr;
  if (setterImpl) {
    auto paramVarDecl =
        new (ctx) ParamDecl(SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
                            ctx.getIdentifier("newValue"), dc);
    paramVarDecl->setSpecifier(ParamSpecifier::Default);
    paramVarDecl->setInterfaceType(elementTy);

    auto setterParamList =
        ParameterList::create(ctx, {paramVarDecl, bodyParams->get(0)});

    setterDecl = AccessorDecl::create(
        ctx, setterImpl->getLoc(), setterImpl->getLoc(), AccessorKind::Set,
        subscript,
        /*async*/ false, SourceLoc(),
        /*throws*/ false, SourceLoc(), /*ThrownType=*/TypeLoc(),
        setterParamList, TupleType::getEmpty(ctx), dc);
    setterDecl->setAccess(AccessLevel::Public);
    setterDecl->setImplicit();
    setterDecl->setIsDynamic(false);
    setterDecl->setIsTransparent(true);
    setterDecl->setBodySynthesizer(synthesizeUnwrappingSetterBody, setterImpl);

    if (setterImpl->isMutating()) {
      setterDecl->setSelfAccessKind(SelfAccessKind::Mutating);
      subscript->setIsSetterMutating(true);
    }
  }

  ImporterImpl.makeComputed(subscript, getterDecl, setterDecl);

  // Implicitly unwrap Optional types for T *operator[].
  ImporterImpl.recordImplicitUnwrapForDecl(
      subscript, getterImpl->isImplicitlyUnwrappedOptional());

  return subscript;
}

// MARK: C++ dereference operator

VarDecl *
SwiftDeclSynthesizer::makeDereferencedPointeeProperty(FuncDecl *getter,
                                                      FuncDecl *setter) {
  assert((getter || setter) &&
         "getter or setter required to generate a pointee property");
  auto &ctx = ImporterImpl.SwiftContext;
  FuncDecl *getterImpl = getter ? getter : setter;
  FuncDecl *setterImpl = setter;
  auto dc = getterImpl->getDeclContext();

  // Get the return type wrapped in `Unsafe(Mutable)Pointer<T>`.
  const auto rawElementTy = getterImpl->getResultInterfaceType();
  // Unwrap `T`. Use rawElementTy for return by value.
  const auto elementTy = rawElementTy->getAnyPointerElementType()
                             ? rawElementTy->getAnyPointerElementType()
                             : rawElementTy;
  // Use 'address' or 'mutableAddress' accessors for non-copyable
  // types that are returned indirectly.
  bool isNoncopyable = dc->mapTypeIntoContext(elementTy)->isNoncopyable();
  bool isImplicit = !isNoncopyable;
  bool useAddress =
      rawElementTy->getAnyPointerElementType() && isNoncopyable;

  auto result = new (ctx)
      VarDecl(/*isStatic*/ false, VarDecl::Introducer::Var,
              getterImpl->getStartLoc(), ctx.getIdentifier("pointee"), dc);
  result->setInterfaceType(elementTy);
  result->setAccess(AccessLevel::Public);

  AccessorDecl *getterDecl = AccessorDecl::create(
      ctx, getterImpl->getLoc(), getterImpl->getLoc(),
      useAddress ? AccessorKind::Address : AccessorKind::Get, result,
      /*async*/ false, SourceLoc(),
      /*throws*/ false, SourceLoc(), /*ThrownType=*/TypeLoc(),
      ParameterList::createEmpty(ctx),
      useAddress ? elementTy->wrapInPointer(PTK_UnsafePointer) : elementTy, dc);
  getterDecl->setAccess(AccessLevel::Public);
  if (isImplicit)
    getterDecl->setImplicit();
  getterDecl->setIsDynamic(false);
  getterDecl->setIsTransparent(true);
  getterDecl->setBodySynthesizer(useAddress
                                     ? synthesizeUnwrappingAddressGetterBody
                                     : synthesizeUnwrappingGetterBody,
                                 getterImpl);

  if (getterImpl->isMutating()) {
    getterDecl->setSelfAccessKind(SelfAccessKind::Mutating);
    result->setIsGetterMutating(true);
  } else {
    getterDecl->setSelfAccessKind(SelfAccessKind::NonMutating);
    result->setIsGetterMutating(false);
  }

  AccessorDecl *setterDecl = nullptr;
  if (setterImpl) {
    auto paramVarDecl =
        new (ctx) ParamDecl(SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
                            ctx.getIdentifier("newValue"), dc);
    paramVarDecl->setSpecifier(ParamSpecifier::Default);
    paramVarDecl->setInterfaceType(elementTy);

    auto setterParamList = useAddress
                               ? ParameterList::create(ctx, {})
                               : ParameterList::create(ctx, {paramVarDecl});

    setterDecl = AccessorDecl::create(
        ctx, setterImpl->getLoc(), setterImpl->getLoc(),
        useAddress ? AccessorKind::MutableAddress : AccessorKind::Set, result,
        /*async*/ false, SourceLoc(),
        /*throws*/ false, SourceLoc(), /*ThrownType=*/TypeLoc(),
        setterParamList,
        useAddress ? elementTy->wrapInPointer(PTK_UnsafeMutablePointer)
                   : TupleType::getEmpty(ctx),
        dc);
    setterDecl->setAccess(AccessLevel::Public);
    if (isImplicit)
      setterDecl->setImplicit();
    setterDecl->setIsDynamic(false);
    setterDecl->setIsTransparent(true);
    setterDecl->setBodySynthesizer(useAddress
                                       ? synthesizeUnwrappingAddressSetterBody
                                       : synthesizeUnwrappingSetterBody,
                                   setterImpl);

    if (setterImpl->isMutating()) {
      setterDecl->setSelfAccessKind(SelfAccessKind::Mutating);
      result->setIsSetterMutating(true);
    } else {
      setterDecl->setSelfAccessKind(SelfAccessKind::NonMutating);
      result->setIsSetterMutating(false);
    }
  }

  ImporterImpl.makeComputed(result, getterDecl, setterDecl);
  return result;
}

// MARK: C++ increment operator

/// Synthesizer callback for a successor function.
///
/// \code
/// var __copy: Self
/// __copy = self
/// __copy.__operatorPlusPlus()
/// return __copy
/// \endcode
static std::pair<BraceStmt *, bool>
synthesizeSuccessorFuncBody(AbstractFunctionDecl *afd, void *context) {
  auto successorDecl = cast<FuncDecl>(afd);
  auto incrementImpl = static_cast<FuncDecl *>(context);

  ASTContext &ctx = successorDecl->getASTContext();
  auto emptyTupleTy = TupleType::getEmpty(ctx);
  auto returnTy = successorDecl->getResultInterfaceType();

  auto selfDecl = successorDecl->getImplicitSelfDecl();
  auto selfRefExpr = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                           /*implicit*/ true);
  selfRefExpr->setType(selfDecl->getInterfaceType());

  // Create a `__copy` variable.
  VarDecl *copyDecl = nullptr;
  PatternBindingDecl *patternDecl = nullptr;
  std::tie(copyDecl, patternDecl) = SwiftDeclSynthesizer::createVarWithPattern(
      successorDecl, ctx.getIdentifier("__copy"), returnTy,
      VarDecl::Introducer::Var,
      /*isImplicit*/ true, AccessLevel::Public, AccessLevel::Public);

  auto copyRefLValueExpr = new (ctx) DeclRefExpr(copyDecl, DeclNameLoc(),
                                                 /*implicit*/ true);
  copyRefLValueExpr->setType(LValueType::get(copyDecl->getInterfaceType()));

  // Copy `self` to `__copy`.
  auto copyAssignExpr = new (ctx) AssignExpr(copyRefLValueExpr, SourceLoc(),
                                             selfRefExpr, /*implicit*/ true);
  copyAssignExpr->setType(emptyTupleTy);

  // Call `operator++`.
  auto incrementExpr = createAccessorImplCallExpr(
      incrementImpl, Argument::implicitInOut(ctx, copyRefLValueExpr));

  auto copyRefRValueExpr = new (ctx) DeclRefExpr(copyDecl, DeclNameLoc(),
                                                 /*implicit*/ true);
  copyRefRValueExpr->setType(copyDecl->getInterfaceType());

  auto *returnStmt = ReturnStmt::createImplicit(ctx, copyRefRValueExpr);

  auto body = BraceStmt::create(ctx, SourceLoc(),
                                {
                                    copyDecl,
                                    patternDecl,
                                    copyAssignExpr,
                                    incrementExpr,
                                    returnStmt,
                                },
                                SourceLoc());
  return {body, /*isTypeChecked*/ true};
}

FuncDecl *SwiftDeclSynthesizer::makeSuccessorFunc(FuncDecl *incrementFunc) {
  auto &ctx = ImporterImpl.SwiftContext;
  auto dc = incrementFunc->getDeclContext();

  auto returnTy = incrementFunc->getImplicitSelfDecl()->getInterfaceType();

  auto nameId = ctx.getIdentifier("successor");
  auto *params = ParameterList::createEmpty(ctx);
  DeclName name(ctx, DeclBaseName(nameId), params);

  auto result = FuncDecl::createImplicit(
      ctx, StaticSpellingKind::None, name, SourceLoc(),
      /*Async*/ false, /*Throws*/ false, /*ThrownType=*/Type(),
      /*GenericParams*/ nullptr, params, returnTy, dc);

  result->setAccess(AccessLevel::Public);
  result->setIsDynamic(false);
  result->setBodySynthesizer(synthesizeSuccessorFuncBody, incrementFunc);

  return result;
}

// MARK: C++ arithmetic operators

static std::pair<BraceStmt *, bool>
synthesizeOperatorMethodBody(AbstractFunctionDecl *afd, void *context) {
  ASTContext &ctx = afd->getASTContext();

  auto funcDecl = cast<FuncDecl>(afd);
  auto methodDecl =
      static_cast<FuncDecl *>(context); /* Swift version of CXXMethod */

  SmallVector<Argument, 8> forwardingArgs;

  // We start from +1 since the first param is our lhs. All other params are
  // forwarded
  for (auto itr = funcDecl->getParameters()->begin() + 1;
       itr != funcDecl->getParameters()->end(); itr++) {
    auto param = *itr;
    auto isInOut = param->isInOut();
    auto paramTy = param->getTypeInContext();
    Expr *paramRefExpr =
        new (ctx) DeclRefExpr(param, DeclNameLoc(), /*Implicit*/ true);
    paramRefExpr->setType(isInOut ? LValueType::get(paramTy) : paramTy);

    auto arg = isInOut ? Argument::implicitInOut(ctx, paramRefExpr)
                       : Argument::unlabeled(paramRefExpr);
    forwardingArgs.push_back(arg);
  }

  auto methodExpr =
      new (ctx) DeclRefExpr(methodDecl, DeclNameLoc(), /*implicit*/ true);
  methodExpr->setType(methodDecl->getInterfaceType());

  // Lhs parameter
  auto baseParam = funcDecl->getParameters()->front();
  auto baseParamTy = baseParam->getTypeInContext();
  auto baseIsInOut = baseParam->isInOut();

  Expr *baseExpr =
      new (ctx) DeclRefExpr(baseParam, DeclNameLoc(), /*implicit*/ true);
  baseExpr->setType(baseIsInOut ? LValueType::get(baseParamTy) : baseParamTy);

  auto baseArg = baseIsInOut ? Argument::implicitInOut(ctx, baseExpr)
                             : Argument::unlabeled(baseExpr);
  auto dotCallExpr =
      DotSyntaxCallExpr::create(ctx, methodExpr, SourceLoc(), baseArg);
  dotCallExpr->setType(methodDecl->getMethodInterfaceType());
  dotCallExpr->setThrows(nullptr);

  auto *argList = ArgumentList::createImplicit(ctx, forwardingArgs);
  auto callExpr = CallExpr::createImplicit(ctx, dotCallExpr, argList);
  callExpr->setType(funcDecl->getResultInterfaceType());
  callExpr->setThrows(nullptr);

  auto *returnStmt = ReturnStmt::createImplicit(ctx, callExpr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked*/ true};
}

clang::CXXMethodDecl *SwiftDeclSynthesizer::synthesizeCXXForwardingMethod(
    const clang::CXXRecordDecl *derivedClass,
    const clang::CXXRecordDecl *baseClass, const clang::CXXMethodDecl *method,
    ForwardingMethodKind forwardingMethodKind,
    ReferenceReturnTypeBehaviorForBaseMethodSynthesis
        referenceReturnTypeBehavior,
    bool forceConstQualifier) {

  auto &clangCtx = ImporterImpl.getClangASTContext();
  auto &clangSema = ImporterImpl.getClangSema();
  assert(!method->isStatic() ||
         method->getNameInfo().getName().getCXXOverloadedOperator() ==
             clang::OO_Call);
  // When emitting symbolic decls, the method might not have a concrete
  // record type as this type.
  if (ImporterImpl.importSymbolicCXXDecls && !method->isStatic() &&
      !method->getThisType()->getPointeeCXXRecordDecl())
    return nullptr;

  // Create a new method in the derived class that calls the base method.
  clang::DeclarationName name = method->getNameInfo().getName();
  if (name.isIdentifier()) {
    std::string newName;
    llvm::raw_string_ostream os(newName);
    os << (forwardingMethodKind == ForwardingMethodKind::Virtual
               ? "__synthesizedVirtualCall_"
               : "__synthesizedBaseCall_")
       << name.getAsIdentifierInfo()->getName();
    name = clang::DeclarationName(
        &ImporterImpl.getClangPreprocessor().getIdentifierTable().get(
            os.str()));
  } else if (name.getCXXOverloadedOperator() == clang::OO_Subscript) {
    name = clang::DeclarationName(
        &ImporterImpl.getClangPreprocessor().getIdentifierTable().get(
            (forwardingMethodKind == ForwardingMethodKind::Virtual
                 ? "__synthesizedVirtualCall_operatorSubscript"
                 : "__synthesizedBaseCall_operatorSubscript")));
  } else if (name.getCXXOverloadedOperator() == clang::OO_Star) {
    name = clang::DeclarationName(
        &ImporterImpl.getClangPreprocessor().getIdentifierTable().get(
            (forwardingMethodKind == ForwardingMethodKind::Virtual
                 ? "__synthesizedVirtualCall_operatorStar"
                 : "__synthesizedBaseCall_operatorStar")));
  } else if (name.getCXXOverloadedOperator() == clang::OO_Call) {
    assert(forwardingMethodKind != ForwardingMethodKind::Virtual);
    name = clang::DeclarationName(
        &ImporterImpl.getClangPreprocessor().getIdentifierTable().get(
            "__synthesizedBaseCall_operatorCall"));
  }
  auto methodType = method->getType();
  // Check if we need to drop the reference from the return type
  // of the new method. This is needed when a synthesized `operator []`
  // derived-to-base call is invoked from Swift's subscript getter.
  if (referenceReturnTypeBehavior !=
      ReferenceReturnTypeBehaviorForBaseMethodSynthesis::KeepReference) {
    if (const auto *fpt = methodType->getAs<clang::FunctionProtoType>()) {
      auto retType = fpt->getReturnType();
      if (retType->isReferenceType() &&
          (referenceReturnTypeBehavior ==
               ReferenceReturnTypeBehaviorForBaseMethodSynthesis::
                   RemoveReference ||
           (referenceReturnTypeBehavior ==
                ReferenceReturnTypeBehaviorForBaseMethodSynthesis::
                    RemoveReferenceIfPointer &&
            retType->getPointeeType()->isPointerType()))) {
        methodType = clangCtx.getFunctionType(retType->getPointeeType(),
                                              fpt->getParamTypes(),
                                              fpt->getExtProtoInfo());
      }
    }
  }
  // Check if this method requires an additional `const` qualifier.
  // This might needed when a non-const synthesized `operator []`
  // derived-to-base call is invoked from Swift's subscript getter.
  bool castThisToNonConstThis = false;
  if (forceConstQualifier) {
    if (const auto *fpt = methodType->getAs<clang::FunctionProtoType>()) {
      auto info = fpt->getExtProtoInfo();
      if (!info.TypeQuals.hasConst()) {
        info.TypeQuals.addConst();
        castThisToNonConstThis = true;
        methodType = clangCtx.getFunctionType(fpt->getReturnType(),
                                              fpt->getParamTypes(), info);
      }
    }
  }
  auto newMethod = clang::CXXMethodDecl::Create(
      clangCtx, const_cast<clang::CXXRecordDecl *>(derivedClass),
      method->getSourceRange().getBegin(),
      clang::DeclarationNameInfo(name, clang::SourceLocation()), methodType,
      method->getTypeSourceInfo(),
      method->isStatic() ? clang::SC_None : method->getStorageClass(),
      method->UsesFPIntrin(), /*isInline=*/true, method->getConstexprKind(),
      method->getSourceRange().getEnd());
  newMethod->setImplicit();
  newMethod->setImplicitlyInline();
  newMethod->setAccess(clang::AccessSpecifier::AS_public);
  if (method->hasAttr<clang::CFReturnsRetainedAttr>()) {
    // Return an FRT field at +1 if the base method also follows this
    // convention.
    newMethod->addAttr(clang::CFReturnsRetainedAttr::CreateImplicit(clangCtx));
  }

  llvm::SmallVector<clang::ParmVarDecl *, 4> params;
  for (size_t i = 0; i < method->getNumParams(); ++i) {
    const auto &param = *method->getParamDecl(i);
    params.push_back(clang::ParmVarDecl::Create(
        clangCtx, newMethod, param.getSourceRange().getBegin(),
        param.getLocation(), param.getIdentifier(), param.getType(),
        param.getTypeSourceInfo(), param.getStorageClass(),
        /*DefExpr=*/nullptr));
  }
  newMethod->setParams(params);

  clang::Sema::SynthesizedFunctionScope scope(clangSema, newMethod);

  // Create a new Clang diagnostic pool to capture any diagnostics
  // emitted during the construction of the method.
  clang::sema::DelayedDiagnosticPool diagPool{
      clangSema.DelayedDiagnostics.getCurrentPool()};
  auto diagState = clangSema.DelayedDiagnostics.push(diagPool);

  // Construct the method's body.
  clang::Expr *thisExpr = clang::CXXThisExpr::Create(
      clangCtx, clang::SourceLocation(), newMethod->getThisType(),
      /*IsImplicit=*/false);
  if (castThisToNonConstThis) {
    auto baseClassPtr =
        clangCtx.getPointerType(clangCtx.getRecordType(derivedClass));
    clang::CastKind Kind;
    clang::CXXCastPath Path;
    clangSema.CheckPointerConversion(thisExpr, baseClassPtr, Kind, Path,
                                     /*IgnoreBaseAccess=*/false,
                                     /*Diagnose=*/true);
    auto conv = clangSema.ImpCastExprToType(thisExpr, baseClassPtr, Kind,
                                            clang::VK_PRValue, &Path);
    if (!conv.isUsable())
      return nullptr;
    thisExpr = conv.get();
  }

  auto memberExprTy =
      (method->isStatic() && method->getOverloadedOperator() ==
                                 clang::OverloadedOperatorKind::OO_Call)
          ? method->getType()
          : clangCtx.BoundMemberTy;
  auto memberExpr = clangSema.BuildMemberExpr(
      thisExpr, /*isArrow=*/true, clang::SourceLocation(),
      clang::NestedNameSpecifierLoc(), clang::SourceLocation(),
      const_cast<clang::CXXMethodDecl *>(method),
      clang::DeclAccessPair::make(const_cast<clang::CXXMethodDecl *>(method),
                                  clang::AS_public),
      /*HadMultipleCandidates=*/false, method->getNameInfo(),
      memberExprTy, clang::VK_PRValue, clang::OK_Ordinary);
  llvm::SmallVector<clang::Expr *, 4> args;
  for (size_t i = 0; i < newMethod->getNumParams(); ++i) {
    auto *param = newMethod->getParamDecl(i);
    auto type = param->getType();
    if (type->isReferenceType())
      type = type->getPointeeType();
    args.push_back(new (clangCtx) clang::DeclRefExpr(
        clangCtx, param, false, type, clang::ExprValueKind::VK_LValue,
        clang::SourceLocation()));
  }
  auto memberCall = clangSema.BuildCallExpr(
      nullptr, memberExpr, clang::SourceLocation(), args,
      clang::SourceLocation());
  if (!memberCall.isUsable())
    return nullptr;
  auto returnStmt =
      clangSema.BuildReturnStmt(clang::SourceLocation(), memberCall.get())
          .get();

  // Check if there were any Clang errors during the construction
  // of the method body.
  clangSema.DelayedDiagnostics.popWithoutEmitting(diagState);
  if (!diagPool.empty())
    return nullptr;

  newMethod->setBody(returnStmt);
  return newMethod;
}

FuncDecl *
SwiftDeclSynthesizer::makeOperator(FuncDecl *operatorMethod,
                                   clang::OverloadedOperatorKind opKind) {
  assert(opKind != clang::OverloadedOperatorKind::OO_None &&
         "expected a C++ operator");

  auto &ctx = ImporterImpl.SwiftContext;
  auto opName = clang::getOperatorSpelling(opKind);
  auto paramList = operatorMethod->getParameters();
  auto genericParamList = operatorMethod->getGenericParams();

  auto opId = ctx.getIdentifier(opName);

  auto parentCtx = operatorMethod->getDeclContext();

  auto lhsParam =
      new (ctx) ParamDecl(SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
                          ctx.getIdentifier("lhs"), parentCtx);

  lhsParam->setInterfaceType(
      operatorMethod->getDeclContext()->getSelfInterfaceType());

  if (operatorMethod->isMutating()) {
    // This implicitly makes the parameter indirect.
    lhsParam->setSpecifier(ParamSpecifier::InOut);
  } else {
    lhsParam->setSpecifier(ParamSpecifier::Default);
  }

  SmallVector<ParamDecl *, 4> newParams;
  newParams.push_back(lhsParam);

  for (auto param : *paramList) {
    newParams.push_back(param);
  }

  auto oldArgNames = operatorMethod->getName().getArgumentNames();
  SmallVector<Identifier, 4> newArgNames;
  newArgNames.push_back(Identifier());

  for (auto id : oldArgNames) {
    newArgNames.push_back(id);
  }

  auto opDeclName =
      DeclName(ctx, opId, {newArgNames.begin(), newArgNames.end()});

  auto topLevelStaticFuncDecl = FuncDecl::createImplicit(
      ctx, StaticSpellingKind::None, opDeclName, SourceLoc(),
      /*Async*/ false, /*Throws*/ false, /*ThrownType=*/Type(), 
      genericParamList, ParameterList::create(ctx, newParams),
      operatorMethod->getResultInterfaceType(), parentCtx);

  topLevelStaticFuncDecl->setAccess(AccessLevel::Public);
  topLevelStaticFuncDecl->setIsDynamic(false);
  topLevelStaticFuncDecl->setStatic();
  topLevelStaticFuncDecl->setBodySynthesizer(synthesizeOperatorMethodBody,
                                             operatorMethod);

  // If this is a unary prefix operator (e.g. `!`), add a `prefix` attribute.
  size_t numParams = operatorMethod->getParameters()->size();
  if (numParams == 0 || (operatorMethod->isStatic() && numParams == 1)) {
    topLevelStaticFuncDecl->getAttrs().add(new (ctx) PrefixAttr(SourceLoc()));
  }

  return topLevelStaticFuncDecl;
}

// MARK: C++ virtual methods

FuncDecl *SwiftDeclSynthesizer::makeVirtualMethod(
    const clang::CXXMethodDecl *clangMethodDecl) {
  auto clangDC = clangMethodDecl->getParent();
  auto &ctx = ImporterImpl.SwiftContext;

  assert(!clangMethodDecl->isStatic() &&
         "C++ virtual functions cannot be static");

  auto newMethod = synthesizeCXXForwardingMethod(
      clangDC, clangDC, clangMethodDecl, ForwardingMethodKind::Virtual,
      ReferenceReturnTypeBehaviorForBaseMethodSynthesis::KeepReference,
      /*forceConstQualifier*/ false);

  auto result = dyn_cast_or_null<FuncDecl>(
      ctx.getClangModuleLoader()->importDeclDirectly(newMethod));
  return result;
}

// MARK: C++ operators

FuncDecl *SwiftDeclSynthesizer::makeInstanceToStaticOperatorCallMethod(
    const clang::CXXMethodDecl *clangMethodDecl) {
  auto clangDC = clangMethodDecl->getParent();
  auto &ctx = ImporterImpl.SwiftContext;

  assert(clangMethodDecl->isStatic() && "Expected a static operator");

  auto newMethod = synthesizeCXXForwardingMethod(
      clangDC, clangDC, clangMethodDecl, ForwardingMethodKind::Base,
      ReferenceReturnTypeBehaviorForBaseMethodSynthesis::KeepReference,
      /*forceConstQualifier*/ true);
  newMethod->addAttr(clang::SwiftNameAttr::CreateImplicit(
      clangMethodDecl->getASTContext(), "callAsFunction"));

  auto result = dyn_cast_or_null<FuncDecl>(
      ctx.getClangModuleLoader()->importDeclDirectly(newMethod));
  return result;
}

// MARK: C++ properties

static std::pair<BraceStmt *, bool>
synthesizeComputedGetterFromCXXMethod(AbstractFunctionDecl *afd,
                                      void *context) {
  auto accessor = cast<AccessorDecl>(afd);
  auto method = static_cast<FuncDecl *>(context);

  auto selfArg = createSelfArg(accessor);

  auto *getterImplCallExpr = createAccessorImplCallExpr(method, selfArg);
  auto &ctx = method->getASTContext();
  auto *returnStmt = ReturnStmt::createImplicit(ctx, getterImplCallExpr);
  auto *body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc());

  return {body, /*isTypeChecked*/ true};
}

static std::pair<BraceStmt *, bool>
synthesizeComputedSetterFromCXXMethod(AbstractFunctionDecl *afd,
                                      void *context) {
  auto setterDecl = cast<AccessorDecl>(afd);
  auto setterImpl = static_cast<FuncDecl *>(context);

  auto selfArg = createSelfArg(setterDecl);
  DeclRefExpr *valueParamRefExpr = createParamRefExpr(setterDecl, 0);

  auto *getterImplCallExpr =
      createAccessorImplCallExpr(setterImpl, selfArg, valueParamRefExpr);

  auto body = BraceStmt::create(setterImpl->getASTContext(), SourceLoc(),
                                {getterImplCallExpr}, SourceLoc());
  return {body, /*isTypeChecked*/ true};
}

VarDecl *
SwiftDeclSynthesizer::makeComputedPropertyFromCXXMethods(FuncDecl *getter,
                                                         FuncDecl *setter) {
  auto &ctx = ImporterImpl.SwiftContext;
  auto dc = getter->getDeclContext();

  assert(isa<clang::CXXMethodDecl>(getter->getClangDecl()) &&
         (!setter || isa<clang::CXXMethodDecl>(setter->getClangDecl())) &&
         "Functions passed to makeProperty must be imported C++ method decls.");

  CXXMethodBridging bridgingInfo(
      cast<clang::CXXMethodDecl>(getter->getClangDecl()));
  assert(bridgingInfo.classify() == CXXMethodBridging::Kind::getter);

  auto importedName = bridgingInfo.importNameAsCamelCaseName();
  auto result =
      new (ctx) VarDecl(false, VarDecl::Introducer::Var, getter->getStartLoc(),
                        ctx.getIdentifier(importedName), dc);
  result->setInterfaceType(getter->getResultInterfaceType());
  result->setAccess(AccessLevel::Public);
  result->setImplInfo(StorageImplInfo::getMutableComputed());

  AccessorDecl *getterDecl = AccessorDecl::create(
      ctx, getter->getLoc(), getter->getLoc(), AccessorKind::Get, result,
      /*async*/ false, SourceLoc(),
      /*throws*/ false, SourceLoc(), /*ThrownType=*/TypeLoc(),
      ParameterList::createEmpty(ctx),
      getter->getResultInterfaceType(), dc);
  getterDecl->setAccess(AccessLevel::Public);
  getterDecl->setImplicit();
  getterDecl->setIsDynamic(false);
  getterDecl->setIsTransparent(true);
  getterDecl->setBodySynthesizer(synthesizeComputedGetterFromCXXMethod, getter);
  if (getter->isMutating()) {
    getterDecl->setSelfAccessKind(SelfAccessKind::Mutating);
    result->setIsGetterMutating(true);
  }

  AccessorDecl *setterDecl = nullptr;
  if (setter) {
    auto paramVarDecl =
        new (ctx) ParamDecl(SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
                            ctx.getIdentifier("newValue"), dc);
    paramVarDecl->setSpecifier(ParamSpecifier::Default);
    paramVarDecl->setInterfaceType(getter->getResultInterfaceType());

    auto setterParamList = ParameterList::create(ctx, {paramVarDecl});

    setterDecl = AccessorDecl::create(
        ctx, setter->getLoc(), setter->getLoc(), AccessorKind::Set, result,
        /*async*/ false, SourceLoc(),
        /*throws*/ false, SourceLoc(), /*thrownType*/ TypeLoc(),
        setterParamList, setter->getResultInterfaceType(), dc);
    setterDecl->setAccess(AccessLevel::Public);
    setterDecl->setImplicit();
    setterDecl->setIsDynamic(false);
    setterDecl->setIsTransparent(true);
    setterDecl->setBodySynthesizer(synthesizeComputedSetterFromCXXMethod,
                                   setter);

    if (setter->isMutating()) {
      setterDecl->setSelfAccessKind(SelfAccessKind::Mutating);
      result->setIsSetterMutating(true);
    } else {
      setterDecl->setSelfAccessKind(SelfAccessKind::NonMutating);
      result->setIsSetterMutating(false);
    }
  }

  ImporterImpl.makeComputed(result, getterDecl, setterDecl);

  return result;
}

static std::pair<BraceStmt *, bool>
synthesizeDefaultArgumentBody(AbstractFunctionDecl *afd, void *context) {
  auto funcDecl = cast<FuncDecl>(afd);
  auto clangParam = static_cast<const clang::ParmVarDecl *>(context);
  auto clangFuncDecl = cast<clang::FunctionDecl>(clangParam->getDeclContext());

  ASTContext &ctx = funcDecl->getASTContext();
  clang::ASTContext &clangCtx = clangParam->getASTContext();
  clang::Sema &clangSema = ctx.getClangModuleLoader()->getClangSema();

  auto clangDeclName = clang::DeclarationName(
      &clangCtx.Idents.get(("__cxx" + funcDecl->getNameStr()).str()));
  auto clangDeclContext = clangCtx.getTranslationUnitDecl();

  // The following also instantiates the default argument if needed.
  auto defaultArgCallExpr = clangSema.BuildCXXDefaultArgExpr(
      clang::SourceLocation(), const_cast<clang::FunctionDecl *>(clangFuncDecl),
      const_cast<clang::ParmVarDecl *>(clangParam));
  if (!defaultArgCallExpr.isUsable())
    return {nullptr, /*isTypeChecked=*/true};

  // The following requires the default argument to be instantiated.
  clang::QualType clangParamTy = clangParam->getDefaultArg()->getType();
  clang::QualType funcTy = clangCtx.getFunctionType(
      clangParamTy, {}, clang::FunctionProtoType::ExtProtoInfo());

  // Synthesize `return {default expr};`.
  auto defaultArgReturnStmt = clang::ReturnStmt::Create(
      clangCtx, clang::SourceLocation(), defaultArgCallExpr.get(), nullptr);

  // Synthesize `ParamTy __cxx__defaultArg_XYZ() { return {default expr}; }`.
  auto defaultArgFuncDecl = clang::FunctionDecl::Create(
      clangCtx, clangDeclContext, clang::SourceLocation(),
      clang::SourceLocation(), clangDeclName, funcTy,
      clangCtx.getTrivialTypeSourceInfo(clangParamTy),
      clang::StorageClass::SC_Static);
  defaultArgFuncDecl->setImplicit();
  defaultArgFuncDecl->setImplicitlyInline();
  defaultArgFuncDecl->setAccess(clang::AccessSpecifier::AS_public);
  defaultArgFuncDecl->setBody(defaultArgReturnStmt);

  // Import `func __cxx__defaultArg_XYZ() -> ParamTY` into Swift.
  auto defaultArgGenerator = dyn_cast_or_null<FuncDecl>(
      ctx.getClangModuleLoader()->importDeclDirectly(defaultArgFuncDecl));
  if (!defaultArgGenerator)
    return {nullptr, /*isTypeChecked=*/true};

  auto defaultArgGeneratorRef = new (ctx) DeclRefExpr(
      ConcreteDeclRef(defaultArgGenerator), DeclNameLoc(), /*Implicit=*/true);
  defaultArgGeneratorRef->setType(defaultArgGenerator->getInterfaceType());

  // Synthesize a call to `__cxx__defaultArg_XYZ()`.
  auto initCall = CallExpr::createImplicit(
      ctx, defaultArgGeneratorRef, ArgumentList::createImplicit(ctx, {}));
  initCall->setType(defaultArgGenerator->getResultInterfaceType());
  initCall->setThrows(nullptr);

  // Synthesize `return __cxx__defaultArg_XYZ()`.
  auto *returnStmt = ReturnStmt::createImplicit(ctx, initCall);

  auto body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit=*/true);
  return {body, /*isTypeChecked=*/true};
}

CallExpr *
SwiftDeclSynthesizer::makeDefaultArgument(const clang::ParmVarDecl *param,
                                          const swift::Type &swiftParamTy,
                                          SourceLoc paramLoc) {
  assert(param->hasDefaultArg() && "must have a C++ default argument");
  if (!param->getIdentifier())
    // Work around an assertion failure in CXXNameMangler::mangleUnqualifiedName
    // when mangling std::__fs::filesystem::path::format.
    return nullptr;

  ASTContext &ctx = ImporterImpl.SwiftContext;
  clang::ASTContext &clangCtx = param->getASTContext();
  auto clangFunc =
      cast<clang::FunctionDecl>(param->getParentFunctionOrMethod());
  if (isa<clang::CXXConstructorDecl>(clangFunc))
    // TODO: support default arguments of constructors
    // (https://github.com/apple/swift/issues/70124)
    return nullptr;

  std::string s;
  llvm::raw_string_ostream os(s);
  std::unique_ptr<clang::ItaniumMangleContext> mangler{
      clang::ItaniumMangleContext::create(clangCtx, clangCtx.getDiagnostics())};
  os << "__defaultArg_" << param->getFunctionScopeIndex() << "_";
  ImporterImpl.getMangledName(mangler.get(), clangFunc, os);

  // Synthesize `func __defaultArg_XYZ() -> ParamTy { ... }`.
  DeclName funcName(ctx, DeclBaseName(ctx.getIdentifier(s)),
                    ParameterList::createEmpty(ctx));
  auto funcDecl = FuncDecl::createImplicit(
      ctx, StaticSpellingKind::None, funcName, paramLoc, false, false, Type(),
      {}, ParameterList::createEmpty(ctx), swiftParamTy,
      ImporterImpl.ImportedHeaderUnit);
  funcDecl->setBodySynthesizer(synthesizeDefaultArgumentBody, (void *)param);
  funcDecl->setAccess(AccessLevel::Public);

  ImporterImpl.defaultArgGenerators[param] = funcDecl;

  auto declRefExpr = new (ctx)
      DeclRefExpr(ConcreteDeclRef(funcDecl), DeclNameLoc(), /*Implicit*/ true);
  declRefExpr->setType(funcDecl->getInterfaceType());
  declRefExpr->setFunctionRefInfo(FunctionRefInfo::singleBaseNameApply());

  auto callExpr = CallExpr::createImplicit(
      ctx, declRefExpr, ArgumentList::forImplicitUnlabeled(ctx, {}));
  callExpr->setType(funcDecl->getResultInterfaceType());
  callExpr->setThrows(nullptr);

  return callExpr;
}
