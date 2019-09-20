//===--- TypeCheckPropertyWrapper.cpp - property wrappers ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for property wrappers.
//
//===----------------------------------------------------------------------===//
#include "ConstraintSystem.h"
#include "TypeChecker.h"
#include "TypeCheckType.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/TypeCheckRequests.h"
using namespace swift;

/// Find the named property in a property wrapper to which access will
/// be delegated.
static VarDecl *findValueProperty(ASTContext &ctx, NominalTypeDecl *nominal,
                                  Identifier name, bool allowMissing) {
  SmallVector<VarDecl *, 2> vars;
  {
    SmallVector<ValueDecl *, 2> decls;
    nominal->lookupQualified(nominal, name, NL_QualifiedDefault, decls);
    for (const auto &foundDecl : decls) {
      auto foundVar = dyn_cast<VarDecl>(foundDecl);
      if (!foundVar || foundVar->isStatic() ||
          foundVar->getDeclContext() != nominal)
        continue;

      vars.push_back(foundVar);
    }
  }

  // Diagnose missing or ambiguous properties.
  switch (vars.size()) {
  case 0:
    if (!allowMissing) {
      nominal->diagnose(diag::property_wrapper_no_value_property,
                        nominal->getDeclaredType(), name);
    }
    return nullptr;

  case 1:
    break;

  default:
    nominal->diagnose(diag::property_wrapper_ambiguous_value_property,
                      nominal->getDeclaredType(), name);
    for (auto var : vars) {
      var->diagnose(diag::kind_declname_declared_here,
                    var->getDescriptiveKind(), var->getFullName());
    }
    return nullptr;
  }

  // The property must be as accessible as the nominal type.
  VarDecl *var = vars.front();
  if (var->getFormalAccess() < nominal->getFormalAccess()) {
    var->diagnose(diag::property_wrapper_type_requirement_not_accessible,
                  var->getFormalAccess(), var->getDescriptiveKind(),
                  var->getFullName(), nominal->getDeclaredType(),
                  nominal->getFormalAccess());
    return nullptr;
  }

  return var;
}

/// Determine whether we have a suitable wrapped-value initializer within
/// a property wrapper type.
static ConstructorDecl *findInitialValueInit(ASTContext &ctx,
                                             NominalTypeDecl *nominal,
                                             VarDecl *valueVar,
                                             Identifier argumentLabel) {
  SmallVector<ConstructorDecl *, 2> initialValueInitializers;
  DeclName initName(ctx, DeclBaseName::createConstructor(), {argumentLabel});
  SmallVector<ValueDecl *, 2> decls;
  nominal->lookupQualified(nominal, initName, NL_QualifiedDefault, decls);
  for (const auto &decl : decls) {
    auto init = dyn_cast<ConstructorDecl>(decl);
    if (!init || init->getDeclContext() != nominal)
      continue;

    initialValueInitializers.push_back(init);
  }

  switch (initialValueInitializers.size()) {
  case 0:
    return nullptr;

  case 1:
    break;

  default:
    // Diagnose ambiguous initializers.
    nominal->diagnose(diag::property_wrapper_ambiguous_initial_value_init,
                      nominal->getDeclaredType());
    for (auto init : initialValueInitializers) {
      init->diagnose(diag::kind_declname_declared_here,
                     init->getDescriptiveKind(), init->getFullName());
    }
    return nullptr;
  }

  // The initializer must be as accessible as the nominal type.
  auto init = initialValueInitializers.front();
  if (init->getFormalAccess() < nominal->getFormalAccess()) {
    init->diagnose(diag::property_wrapper_type_requirement_not_accessible,
                     init->getFormalAccess(), init->getDescriptiveKind(),
                     init->getFullName(), nominal->getDeclaredType(),
                     nominal->getFormalAccess());
    return nullptr;
  }

  // Retrieve the type of the 'value' property.
  if (!valueVar->hasInterfaceType())
    ctx.getLazyResolver()->resolveDeclSignature(valueVar);
  Type valueVarType = valueVar->getValueInterfaceType();

  // Retrieve the parameter type of the initializer.
  if (!init->hasInterfaceType())
    ctx.getLazyResolver()->resolveDeclSignature(init);
  Type paramType;
  if (auto *curriedInitType =
          init->getInterfaceType()->getAs<AnyFunctionType>()) {
    if (auto *initType =
          curriedInitType->getResult()->getAs<AnyFunctionType>()) {
      if (initType->getParams().size() == 1) {
        const auto &param = initType->getParams()[0];
        if (!param.isInOut() && !param.isVariadic()) {
          paramType = param.getPlainType();
          if (param.isAutoClosure()) {
            if (auto *fnType = paramType->getAs<FunctionType>())
              paramType = fnType->getResult();
          }
        }
      }
    }
  }

  // The parameter type must be the same as the type of `valueVar` or an
  // autoclosure thereof.
  if (!paramType->isEqual(valueVarType)) {
    init->diagnose(diag::property_wrapper_wrong_initial_value_init, initName,
                   paramType, valueVarType);
    valueVar->diagnose(diag::decl_declared_here, valueVar->getFullName());
    return nullptr;
  }

  // The initializer must not be failable.
  if (init->isFailable()) {
    init->diagnose(diag::property_wrapper_failable_init, initName);
    return nullptr;
  }

  return init;
}

/// Determine whether we have a suitable init() within a property
/// wrapper type.
static ConstructorDecl *findDefaultInit(ASTContext &ctx,
                                        NominalTypeDecl *nominal) {
  SmallVector<ConstructorDecl *, 2> defaultValueInitializers;
  DeclName initName(ctx, DeclBaseName::createConstructor(),
                    ArrayRef<Identifier>());
  SmallVector<ValueDecl *, 2> decls;
  nominal->lookupQualified(nominal, initName, NL_QualifiedDefault, decls);
  for (const auto &decl : decls) {
    auto init = dyn_cast<ConstructorDecl>(decl);
    if (!init || init->getDeclContext() != nominal)
      continue;

    defaultValueInitializers.push_back(init);
  }

  switch (defaultValueInitializers.size()) {
  case 0:
    return nullptr;

  case 1:
    break;

  default:
    // Diagnose ambiguous init() initializers.
    nominal->diagnose(diag::property_wrapper_ambiguous_default_value_init,
                      nominal->getDeclaredType());
    for (auto init : defaultValueInitializers) {
      init->diagnose(diag::kind_declname_declared_here,
                     init->getDescriptiveKind(), init->getFullName());
    }
    return nullptr;
  }

  // 'init()' must be as accessible as the nominal type.
  auto init = defaultValueInitializers.front();
  if (init->getFormalAccess() < nominal->getFormalAccess()) {
    init->diagnose(diag::property_wrapper_type_requirement_not_accessible,
                     init->getFormalAccess(), init->getDescriptiveKind(),
                     init->getFullName(), nominal->getDeclaredType(),
                     nominal->getFormalAccess());
    return nullptr;
  }

  // The initializer must not be failable.
  if (init->isFailable()) {
    init->diagnose(diag::property_wrapper_failable_init, initName);
    return nullptr;
  }

  return init;
}

/// Determine whether we have a suitable static subscript to which we
/// can pass along the enclosing self + key-paths.
static SubscriptDecl *findEnclosingSelfSubscript(ASTContext &ctx,
                                                 NominalTypeDecl *nominal,
                                                 Identifier propertyName) {
  Identifier argNames[] = {
    ctx.Id_enclosingInstance,
    propertyName,
    ctx.Id_storage
  };
  DeclName subscriptName(ctx, DeclBaseName::createSubscript(), argNames);

  SmallVector<SubscriptDecl *, 2> subscripts;
  for (auto member : nominal->lookupDirect(subscriptName)) {
    auto subscript = dyn_cast<SubscriptDecl>(member);
    if (!subscript)
      continue;

    if (subscript->isInstanceMember())
      continue;

    if (subscript->getDeclContext() != nominal)
      continue;

    subscripts.push_back(subscript);
  }

  switch (subscripts.size()) {
  case 0:
    return nullptr;

  case 1:
    break;

  default:
    // Diagnose ambiguous init() initializers.
    nominal->diagnose(diag::property_wrapper_ambiguous_enclosing_self_subscript,
                      nominal->getDeclaredType(), subscriptName);
    for (auto subscript : subscripts) {
      subscript->diagnose(diag::kind_declname_declared_here,
                          subscript->getDescriptiveKind(),
                          subscript->getFullName());
    }
    return nullptr;

  }

  auto subscript = subscripts.front();
  // the subscript must be as accessible as the nominal type.
  if (subscript->getFormalAccess() < nominal->getFormalAccess()) {
    subscript->diagnose(diag::property_wrapper_type_requirement_not_accessible,
                        subscript->getFormalAccess(),
                        subscript->getDescriptiveKind(),
                        subscript->getFullName(), nominal->getDeclaredType(),
                        nominal->getFormalAccess());
    return nullptr;
  }

  return subscript;
}

llvm::Expected<PropertyWrapperTypeInfo>
PropertyWrapperTypeInfoRequest::evaluate(
    Evaluator &eval, NominalTypeDecl *nominal) const {
  // We must have the @propertyWrapper attribute to continue.
  if (!nominal->getAttrs().hasAttribute<PropertyWrapperAttr>()) {
    return PropertyWrapperTypeInfo();
  }

  // Look for a non-static property named "wrappedValue" in the property
  // wrapper type.
  ASTContext &ctx = nominal->getASTContext();
  auto valueVar =
      findValueProperty(ctx, nominal, ctx.Id_wrappedValue,
                        /*allowMissing=*/false);
  if (!valueVar)
    return PropertyWrapperTypeInfo();

  if (!valueVar->hasInterfaceType())
    static_cast<TypeChecker &>(*ctx.getLazyResolver()).validateDecl(valueVar);

  PropertyWrapperTypeInfo result;
  result.valueVar = valueVar;
  result.wrappedValueInit =
      findInitialValueInit(ctx, nominal, valueVar, ctx.Id_wrappedValue);

  if (!result.wrappedValueInit) {
    // Look for the older name init(initialValue:).
    result.wrappedValueInit =
      findInitialValueInit(ctx, nominal, valueVar, ctx.Id_initialValue);
    if (result.wrappedValueInit &&
        result.wrappedValueInit->getLoc().isValid()) {
      auto diag = result.wrappedValueInit->diagnose(
          diag::property_wrapper_init_initialValue);
      auto param = result.wrappedValueInit->getParameters()->get(0);
      if (param->getArgumentNameLoc().isValid())
        diag.fixItReplace(param->getArgumentNameLoc(), "wrappedValue");
      else
        diag.fixItInsert(param->getLoc(), "wrappedValue ");
    }
  }

  result.defaultInit = findDefaultInit(ctx, nominal);
  result.projectedValueVar =
    findValueProperty(ctx, nominal, ctx.Id_projectedValue,
                      /*allowMissing=*/true);
  result.enclosingInstanceWrappedSubscript =
    findEnclosingSelfSubscript(ctx, nominal, ctx.Id_wrapped);
  result.enclosingInstanceProjectedSubscript =
    findEnclosingSelfSubscript(ctx, nominal, ctx.Id_projected);

  // If there was no projectedValue property, but there is a wrapperValue,
  // property, use that and warn.
  if (!result.projectedValueVar) {
    result.projectedValueVar =
      findValueProperty(ctx, nominal, ctx.Id_wrapperValue,
                        /*allowMissing=*/true);
    if (result.projectedValueVar &&
        result.projectedValueVar->getLoc().isValid()) {
      result.projectedValueVar->diagnose(diag::property_wrapper_wrapperValue)
        .fixItReplace(result.projectedValueVar->getNameLoc(),
                      "projectedValue");
    }
  }

  return result;
}

llvm::Expected<llvm::TinyPtrVector<CustomAttr *>>
AttachedPropertyWrappersRequest::evaluate(Evaluator &evaluator,
                                          VarDecl *var) const {
  ASTContext &ctx = var->getASTContext();
  auto dc = var->getDeclContext();
  llvm::TinyPtrVector<CustomAttr *> result;
  for (auto attr : var->getAttrs().getAttributes<CustomAttr>()) {
    auto mutableAttr = const_cast<CustomAttr *>(attr);
    // Figure out which nominal declaration this custom attribute refers to.
    auto nominal = evaluateOrDefault(
      ctx.evaluator, CustomAttrNominalRequest{mutableAttr, dc}, nullptr);

    // If we didn't find a nominal type with a @propertyWrapper attribute,
    // skip this custom attribute.
    if (!nominal || !nominal->getAttrs().hasAttribute<PropertyWrapperAttr>())
      continue;

    // If the declaration came from a module file, we've already done all of
    // the semantic checking required.
    auto sourceFile = dc->getParentSourceFile();
    if (!sourceFile) {
      result.push_back(mutableAttr);
      continue;
    }
      
    // Check various restrictions on which properties can have wrappers
    // attached to them.

    // Local properties do not yet support wrappers.
    if (var->getDeclContext()->isLocalContext()) {
      ctx.Diags.diagnose(attr->getLocation(), diag::property_wrapper_local);
      continue;
    }

    // Nor does top-level code.
    if (var->getDeclContext()->isModuleScopeContext()) {
      ctx.Diags.diagnose(attr->getLocation(), diag::property_wrapper_top_level);
      continue;
    }

    // Check that the variable is part of a single-variable pattern.
    auto binding = var->getParentPatternBinding();
    if (!binding || binding->getSingleVar() != var) {
      ctx.Diags.diagnose(attr->getLocation(),
                         diag::property_wrapper_not_single_var);
      continue;
    }

    // A property wrapper cannot be attached to a 'let'.
    if (var->isLet()) {
      ctx.Diags.diagnose(attr->getLocation(), diag::property_wrapper_let);
      continue;
    }

    // Check for conflicting attributes.
    if (var->getAttrs().hasAttribute<LazyAttr>() ||
        var->getAttrs().hasAttribute<NSCopyingAttr>() ||
        var->getAttrs().hasAttribute<NSManagedAttr>() ||
        (var->getAttrs().hasAttribute<ReferenceOwnershipAttr>() &&
         var->getAttrs().getAttribute<ReferenceOwnershipAttr>()->get() !=
             ReferenceOwnership::Strong)) {
      int whichKind;
      if (var->getAttrs().hasAttribute<LazyAttr>())
        whichKind = 0;
      else if (var->getAttrs().hasAttribute<NSCopyingAttr>())
        whichKind = 1;
      else if (var->getAttrs().hasAttribute<NSManagedAttr>())
        whichKind = 2;
      else {
        auto attr = var->getAttrs().getAttribute<ReferenceOwnershipAttr>();
        whichKind = 2 + static_cast<unsigned>(attr->get());
      }
      var->diagnose(diag::property_with_wrapper_conflict_attribute,
                    var->getFullName(), whichKind);
      continue;
    }

    // A property with a wrapper cannot be declared in a protocol, enum, or
    // an extension.
    if (isa<ProtocolDecl>(dc) ||
        (isa<ExtensionDecl>(dc) && var->isInstanceMember()) ||
        (isa<EnumDecl>(dc) && var->isInstanceMember())) {
      int whichKind;
      if (isa<ProtocolDecl>(dc))
        whichKind = 0;
      else if (isa<ExtensionDecl>(dc))
        whichKind = 1;
      else
        whichKind = 2;
      var->diagnose(diag::property_with_wrapper_in_bad_context,
                    var->getFullName(), whichKind)
        .highlight(attr->getRange());

      continue;
    }

    // Properties with wrappers must not override another property.
    if (auto classDecl = dyn_cast<ClassDecl>(dc)) {
      if (auto overrideAttr = var->getAttrs().getAttribute<OverrideAttr>()) {
        var->diagnose(diag::property_with_wrapper_overrides,
                      var->getFullName())
          .highlight(attr->getRange());
        continue;
      }
    }
    
    result.push_back(mutableAttr);
  }

  // Attributes are stored in reverse order in the AST, but we want them in
  // source order so that the outermost property wrapper comes first.
  std::reverse(result.begin(), result.end());
  return result;
}

llvm::Expected<Type>
AttachedPropertyWrapperTypeRequest::evaluate(Evaluator &evaluator,
                                             VarDecl *var,
                                             unsigned index) const {
  // Find the custom attributes for the attached property wrapper.
  llvm::Expected<llvm::TinyPtrVector<CustomAttr *>> customAttrVal =
      evaluator(AttachedPropertyWrappersRequest{var});
  if (!customAttrVal)
    return customAttrVal.takeError();

  // If there isn't an attached property wrapper at this index, we're done.
  if (index >= customAttrVal->size())
    return Type();
                                               
  auto customAttr = (*customAttrVal)[index];
  if (!customAttr)
    return Type();

  ASTContext &ctx = var->getASTContext();
  if (!ctx.getLazyResolver())
    return nullptr;

  auto resolution =
      TypeResolution::forContextual(var->getDeclContext());
  TypeResolutionOptions options(TypeResolverContext::PatternBindingDecl);
  options |= TypeResolutionFlags::AllowUnboundGenerics;

  auto &tc = *static_cast<TypeChecker *>(ctx.getLazyResolver());
  if (TypeChecker::validateType(tc.Context, customAttr->getTypeLoc(),
                                resolution, options))
    return ErrorType::get(ctx);

  return customAttr->getTypeLoc().getType();
}

llvm::Expected<Type>
PropertyWrapperBackingPropertyTypeRequest::evaluate(
                                    Evaluator &evaluator, VarDecl *var) const {
  llvm::Expected<Type> rawTypeResult =
    evaluator(AttachedPropertyWrapperTypeRequest{var, 0});
  if (!rawTypeResult)
    return rawTypeResult;

  Type rawType = *rawTypeResult;
  if (!rawType || rawType->hasError())
    return Type();

  if (!rawType->hasUnboundGenericType())
    return rawType->mapTypeOutOfContext();

  auto binding = var->getParentPatternBinding();
  if (!binding)
    return Type();

  ASTContext &ctx = var->getASTContext();
  if (!ctx.getLazyResolver())
    return Type();

  // If there's an initializer of some sort, checking it will determine the
  // property wrapper type.
  unsigned index = binding->getPatternEntryIndexForVarDecl(var);
  TypeChecker &tc = *static_cast<TypeChecker *>(ctx.getLazyResolver());
  if (binding->isInitialized(index)) {
    tc.validateDecl(var);
    if (!binding->isInitializerChecked(index))
      tc.typeCheckPatternBinding(binding, index);

    Type type = ctx.getSideCachedPropertyWrapperBackingPropertyType(var);
    assert(type || ctx.Diags.hadAnyError());
    return type;
  }

  // Compute the type of the property to plug in to the wrapper type.
  tc.validateDecl(var);
  Type propertyType = var->getType();
  if (!propertyType || propertyType->hasError())
    return Type();

  using namespace constraints;
  auto dc = var->getInnermostDeclContext();
  ConstraintSystem cs(tc, dc, None);
  auto emptyLocator = cs.getConstraintLocator(nullptr);
  
  auto wrapperAttrs = var->getAttachedPropertyWrappers();
  Type valueMemberType;
  Type outermostOpenedWrapperType;
  for (unsigned i : indices(wrapperAttrs)) {
    Type rawWrapperType = var->getAttachedPropertyWrapperType(i);
    if (!rawWrapperType)
      return Type();
    
    // Open the type.
    Type openedWrapperType =
      cs.openUnboundGenericType(rawWrapperType, emptyLocator);
    if (!outermostOpenedWrapperType)
      outermostOpenedWrapperType = openedWrapperType;
    
    // If we already have a value member type, it must be equivalent to
    // this opened wrapper type.
    if (valueMemberType) {
      cs.addConstraint(ConstraintKind::Equal, valueMemberType,
                       openedWrapperType, emptyLocator);
    }
    
    // Retrieve the type of the wrapped value.
    auto wrapperInfo = var->getAttachedPropertyWrapperTypeInfo(i);
    if (!wrapperInfo)
      return Type();

    valueMemberType = openedWrapperType->getTypeOfMember(
        dc->getParentModule(), wrapperInfo.valueVar);
  }
  
  // The resulting value member type must be equivalent to the property
  // type.
  cs.addConstraint(ConstraintKind::Equal, valueMemberType,
                   propertyType, emptyLocator);

  SmallVector<Solution, 4> solutions;
  if (cs.solve(nullptr, solutions) || solutions.size() != 1) {
    var->diagnose(diag::property_wrapper_incompatible_property,
                  propertyType, rawType);
    if (auto nominalWrapper = rawType->getAnyNominal()) {
      nominalWrapper->diagnose(diag::property_wrapper_declared_here,
                               nominalWrapper->getFullName());
    }
    return Type();
  }

  Type wrapperType = solutions.front().simplifyType(outermostOpenedWrapperType);
  return wrapperType->mapTypeOutOfContext();
}

Type swift::computeWrappedValueType(VarDecl *var, Type backingStorageType,
                                    Optional<unsigned> limit) {
  auto wrapperAttrs = var->getAttachedPropertyWrappers();
  unsigned realLimit = wrapperAttrs.size();
  if (limit)
    realLimit = std::min(*limit, realLimit);
                                    
  // Follow the chain of wrapped value properties.
  Type wrappedValueType = backingStorageType;
  DeclContext *dc = var->getDeclContext();
  ASTContext &ctx = dc->getASTContext();
  for (unsigned i : range(realLimit)) {
    auto wrappedInfo = var->getAttachedPropertyWrapperTypeInfo(i);
    if (!wrappedInfo)
      return wrappedValueType;

    if (!wrappedInfo.valueVar->hasInterfaceType()) {
      static_cast<TypeChecker&>(*ctx.getLazyResolver()).validateDecl(
          wrappedInfo.valueVar);
    }
    
    wrappedValueType = wrappedValueType->getTypeOfMember(
        dc->getParentModule(),
        wrappedInfo.valueVar,
        wrappedInfo.valueVar->getValueInterfaceType());
    if (wrappedValueType->hasError())
      break;
  }
                                    
  return wrappedValueType;
}

Expr *swift::buildPropertyWrapperInitialValueCall(
    VarDecl *var, Type backingStorageType, Expr *value,
    bool ignoreAttributeArgs) {
  // From the innermost wrapper type out, form init(wrapperValue:) calls.
  ASTContext &ctx = var->getASTContext();
  auto wrapperAttrs = var->getAttachedPropertyWrappers();
  Expr *initializer = value;
  for (unsigned i : reversed(indices(wrapperAttrs))) {
    Type wrapperType =
      backingStorageType ? computeWrappedValueType(var, backingStorageType, i)
                         : var->getAttachedPropertyWrapperType(i);
    if (!wrapperType)
      return nullptr;
    
    auto typeExpr = TypeExpr::createImplicitHack(
        wrapperAttrs[i]->getTypeLoc().getLoc(),
        wrapperType, ctx);

    SourceLoc startLoc = wrapperAttrs[i]->getTypeLoc().getSourceRange().Start;

    // If there were no arguments provided for the attribute at this level,
    // call `init(wrappedValue:)` directly.
    auto attr = wrapperAttrs[i];
    if (!attr->getArg() || ignoreAttributeArgs) {
      Identifier argName = ctx.Id_wrappedValue;
      if (auto init
              = var->getAttachedPropertyWrapperTypeInfo(i).wrappedValueInit) {
        argName = init->getFullName().getArgumentNames()[0];
      }
      
      auto endLoc = initializer->getEndLoc();
      if (endLoc.isInvalid() && startLoc.isValid())
        endLoc = wrapperAttrs[i]->getTypeLoc().getSourceRange().End;

      initializer = CallExpr::create(
         ctx, typeExpr, startLoc, {initializer}, {argName},
         {initializer->getStartLoc()}, endLoc,
         nullptr, /*implicit=*/true);
      continue;
    }

    // Splice `wrappedValue:` into the argument list.
    SmallVector<Expr *, 4> elements;
    SmallVector<Identifier, 4> elementNames;
    SmallVector<SourceLoc, 4> elementLocs;
    elements.push_back(initializer);
    elementNames.push_back(ctx.Id_wrappedValue);
    elementLocs.push_back(initializer->getStartLoc());

    if (auto tuple = dyn_cast<TupleExpr>(attr->getArg())) {
      for (unsigned i : range(tuple->getNumElements())) {
        elements.push_back(tuple->getElement(i));
        elementNames.push_back(tuple->getElementName(i));
        elementLocs.push_back(tuple->getElementNameLoc(i));
      }
    } else {
      auto paren = cast<ParenExpr>(attr->getArg());
      elements.push_back(paren->getSubExpr());
      elementNames.push_back(Identifier());
      elementLocs.push_back(SourceLoc());
    }
    
    auto endLoc = attr->getArg()->getEndLoc();
    if (endLoc.isInvalid() && startLoc.isValid())
      endLoc = wrapperAttrs[i]->getTypeLoc().getSourceRange().End;

    initializer = CallExpr::create(
        ctx, typeExpr, startLoc, elements, elementNames, elementLocs,
        endLoc, nullptr, /*implicit=*/true);
  }
  
  return initializer;
}
