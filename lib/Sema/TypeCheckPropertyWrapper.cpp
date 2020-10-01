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

/// The kind of property initializer to look for
enum class PropertyWrapperInitKind {
  /// An initial-value initializer (i.e. `init(initialValue:)`), which is
  /// deprecated.
  InitialValue,
  /// An wrapped-value initializer (i.e. `init(wrappedValue:)`)
  WrappedValue,
  /// An default-value initializer (i.e. `init()` or `init(defaultArgs...)`)
  Default
};

static bool isDeclNotAsAccessibleAsParent(ValueDecl *decl,
                                          NominalTypeDecl *parent) {
  return decl->getFormalAccess() <
         std::min(parent->getFormalAccess(), AccessLevel::Public);
}

/// Find the named property in a property wrapper to which access will
/// be delegated.
static VarDecl *findValueProperty(ASTContext &ctx, NominalTypeDecl *nominal,
                                  Identifier name, bool allowMissing) {
  SmallVector<VarDecl *, 2> vars;
  {
    SmallVector<ValueDecl *, 2> decls;
    nominal->lookupQualified(nominal, DeclNameRef(name), NL_QualifiedDefault,
                             decls);
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
                    var->getDescriptiveKind(), var->getName());
    }
    return nullptr;
  }

  // The property must be as accessible as the nominal type.
  VarDecl *var = vars.front();
  if (isDeclNotAsAccessibleAsParent(var, nominal)) {
    var->diagnose(diag::property_wrapper_type_requirement_not_accessible,
                  var->getFormalAccess(), var->getDescriptiveKind(),
                  var->getName(), nominal->getDeclaredType(),
                  nominal->getFormalAccess());
    return nullptr;
  }

  return var;
}

/// Determine whether we have a suitable initializer within a property wrapper
/// type.
static ConstructorDecl *
findSuitableWrapperInit(ASTContext &ctx, NominalTypeDecl *nominal,
                        VarDecl *valueVar, PropertyWrapperInitKind initKind,
                        const SmallVectorImpl<ValueDecl *> &decls) {
  enum class NonViableReason {
    Failable,
    ParameterTypeMismatch,
    Inaccessible,
  };

  SmallVector<std::tuple<ConstructorDecl *, NonViableReason, Type>, 2>
      nonviable;
  SmallVector<ConstructorDecl *, 2> viableInitializers;

  Identifier argumentLabel;
  switch (initKind) {
  case PropertyWrapperInitKind::InitialValue:
    argumentLabel = ctx.Id_initialValue;
    break;
  case PropertyWrapperInitKind::WrappedValue:
    argumentLabel = ctx.Id_wrappedValue;
    break;
  case PropertyWrapperInitKind::Default:
    break;
  }

  for (const auto &decl : decls) {
    auto init = dyn_cast<ConstructorDecl>(decl);
    if (!init || init->getDeclContext() != nominal || init->isGeneric())
      continue;

    ParamDecl *argumentParam = nullptr;
    bool hasExtraneousParam = false;
    // Check whether every parameter meets one of the following criteria:
    //   (1) The parameter has a default argument, or
    //   (2) The parameter has the given argument label.
    for (auto param : *init->getParameters()) {
      // Recognize the first parameter with the requested argument label.
      if (!argumentLabel.empty() && param->getArgumentName() == argumentLabel &&
          !argumentParam) {
        argumentParam = param;
        continue;
      }

      if (param->isDefaultArgument())
        continue;

      // Skip this init as the param doesn't meet the above criteria
      hasExtraneousParam = true;
      break;
    }

    if (hasExtraneousParam)
      continue;

    if (initKind != PropertyWrapperInitKind::Default) {
      if (!argumentParam)
        continue;

      if (argumentParam->isInOut() || argumentParam->isVariadic())
        continue;
    }

    // Failable initializers cannot be used.
    if (init->isFailable()) {
      nonviable.push_back(
          std::make_tuple(init, NonViableReason::Failable, Type()));
      continue;
    }

    // Check accessibility.
    if (isDeclNotAsAccessibleAsParent(init, nominal)) {
      nonviable.push_back(
          std::make_tuple(init, NonViableReason::Inaccessible, Type()));
      continue;
    }

    // Additional checks for initial-value and wrapped-value initializers
    if (initKind != PropertyWrapperInitKind::Default) {
      auto paramType = argumentParam->getInterfaceType();
      if (paramType->is<ErrorType>())
        continue;

      if (argumentParam->isAutoClosure()) {
        if (auto *fnType = paramType->getAs<FunctionType>())
          paramType = fnType->getResult();
      }

      // The parameter type must be the same as the type of `valueVar` or an
      // autoclosure thereof.
      if (!paramType->isEqual(valueVar->getValueInterfaceType())) {
        nonviable.push_back(std::make_tuple(
            init, NonViableReason::ParameterTypeMismatch, paramType));
        continue;
      }
    }

    viableInitializers.push_back(init);
  }

  // If we found some nonviable candidates but no viable ones, complain.
  if (viableInitializers.empty() && !nonviable.empty()) {
    for (const auto &candidate : nonviable) {
      auto init = std::get<0>(candidate);
      auto reason = std::get<1>(candidate);
      auto paramType = std::get<2>(candidate);
      switch (reason) {
      case NonViableReason::Failable:
        init->diagnose(diag::property_wrapper_failable_init,
                       init->getName());
        break;

      case NonViableReason::Inaccessible:
        init->diagnose(diag::property_wrapper_type_requirement_not_accessible,
                       init->getFormalAccess(), init->getDescriptiveKind(),
                       init->getName(), nominal->getDeclaredType(),
                       nominal->getFormalAccess());
        break;

      case NonViableReason::ParameterTypeMismatch:
        init->diagnose(diag::property_wrapper_wrong_initial_value_init,
                       init->getName(), paramType,
                       valueVar->getValueInterfaceType());
        valueVar->diagnose(diag::decl_declared_here, valueVar->getName());
        break;
      }
    }
  }

  return viableInitializers.empty() ? nullptr : viableInitializers.front();
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
                          subscript->getName());
    }
    return nullptr;

  }

  auto subscript = subscripts.front();
  // the subscript must be as accessible as the nominal type.
  if (isDeclNotAsAccessibleAsParent(subscript, nominal)) {
    subscript->diagnose(diag::property_wrapper_type_requirement_not_accessible,
                        subscript->getFormalAccess(),
                        subscript->getDescriptiveKind(),
                        subscript->getName(), nominal->getDeclaredType(),
                        nominal->getFormalAccess());
    return nullptr;
  }

  return subscript;
}

PropertyWrapperTypeInfo
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

  TypeChecker::addImplicitConstructors(nominal);

  SmallVector<ValueDecl *, 2> decls;
  nominal->lookupQualified(nominal, DeclNameRef::createConstructor(),
                           NL_QualifiedDefault, decls);

  PropertyWrapperTypeInfo result;
  result.valueVar = valueVar;
  if (auto init = findSuitableWrapperInit(ctx, nominal, valueVar,
                              PropertyWrapperInitKind::WrappedValue, decls)) {
    result.wrappedValueInit = PropertyWrapperTypeInfo::HasWrappedValueInit;
  } else if (auto init = findSuitableWrapperInit(
               ctx, nominal, valueVar, PropertyWrapperInitKind::InitialValue,
               decls)) {
    result.wrappedValueInit = PropertyWrapperTypeInfo::HasInitialValueInit;

    if (init->getLoc().isValid()) {
      auto diag = init->diagnose(diag::property_wrapper_init_initialValue);
      for (auto param : *init->getParameters()) {
        if (param->getArgumentName() == ctx.Id_initialValue) {
          if (param->getArgumentNameLoc().isValid())
            diag.fixItReplace(param->getArgumentNameLoc(), "wrappedValue");
          else
            diag.fixItInsert(param->getLoc(), "wrappedValue ");
          break;
        }
      }
    }
  }

  if (findSuitableWrapperInit(ctx, nominal, /*valueVar=*/nullptr,
                              PropertyWrapperInitKind::Default, decls)) {
    result.defaultInit = PropertyWrapperTypeInfo::HasDefaultValueInit;
  }

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

  bool hasInvalidDynamicSelf = false;
  if (result.projectedValueVar &&
      result.projectedValueVar->getValueInterfaceType()->hasDynamicSelfType()) {
    result.projectedValueVar->diagnose(
        diag::property_wrapper_dynamic_self_type, /*projectedValue=*/true);
    hasInvalidDynamicSelf = true;
  }

  if (result.valueVar->getValueInterfaceType()->hasDynamicSelfType()) {
    result.valueVar->diagnose(
        diag::property_wrapper_dynamic_self_type, /*projectedValue=*/false);
    hasInvalidDynamicSelf = true;
  }

  if (hasInvalidDynamicSelf)
    return PropertyWrapperTypeInfo();

  return result;
}

llvm::TinyPtrVector<CustomAttr *>
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
                    var->getName(), whichKind);
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
                    var->getName(), whichKind)
        .highlight(attr->getRange());

      continue;
    }

    // Properties with wrappers must not override another property.
    if (isa<ClassDecl>(dc)) {
      if (var->getAttrs().hasAttribute<OverrideAttr>()) {
        var->diagnose(diag::property_with_wrapper_overrides,
                      var->getName())
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

Type AttachedPropertyWrapperTypeRequest::evaluate(Evaluator &evaluator,
                                                  VarDecl *var,
                                                  unsigned index) const {
  // Find the custom attributes for the attached property wrapper.
  llvm::TinyPtrVector<CustomAttr *> customAttrVal =
      evaluateOrDefault(evaluator, AttachedPropertyWrappersRequest{var}, {});

  // If there isn't an attached property wrapper at this index, we're done.
  if (index >= customAttrVal.size())
    return Type();
                                               
  auto customAttr = customAttrVal[index];
  if (!customAttr)
    return Type();

  auto ty = evaluateOrDefault(
      evaluator,
      CustomAttrTypeRequest{customAttr, var->getDeclContext(),
                            CustomAttrTypeKind::PropertyDelegate},
      Type());
  if (!ty || ty->hasError()) {
    return ErrorType::get(var->getASTContext());
  }
  return ty;
}

Type
PropertyWrapperBackingPropertyTypeRequest::evaluate(
    Evaluator &evaluator, VarDecl *var) const {
  Type rawType =
      evaluateOrDefault(evaluator,
                        AttachedPropertyWrapperTypeRequest{var, 0}, Type());

  if (!rawType || rawType->hasError())
    return Type();

  auto binding = var->getParentPatternBinding();
  if (!binding)
    return Type();

  // If there's an initializer of some sort, checking it will determine the
  // property wrapper type.
  unsigned index = binding->getPatternEntryIndexForVarDecl(var);
  if (binding->isInitialized(index)) {
    // FIXME(InterfaceTypeRequest): Remove this.
    (void)var->getInterfaceType();
    if (!binding->isInitializerChecked(index))
      TypeChecker::typeCheckPatternBinding(binding, index);
  } else {
    using namespace constraints;
    auto dc = var->getInnermostDeclContext();
    ConstraintSystem cs(dc, ConstraintSystemFlags::AllowFixes);
    auto target = SolutionApplicationTarget::forUninitializedWrappedVar(var);
    auto solutions = cs.solve(target, FreeTypeVariableBinding::Disallow);

    if (!solutions || !cs.applySolution(solutions->front(), target)) {
      var->setInvalid();
      return Type();
    }
  }

  ASTContext &ctx = var->getASTContext();
  Type type = ctx.getSideCachedPropertyWrapperBackingPropertyType(var);
  assert(type || ctx.Diags.hadAnyError());
  return type;
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
  for (unsigned i : range(realLimit)) {
    auto wrappedInfo = var->getAttachedPropertyWrapperTypeInfo(i);
    if (!wrappedInfo)
      return wrappedValueType;

    wrappedValueType = wrappedValueType->getTypeOfMember(
        dc->getParentModule(),
        wrappedInfo.valueVar,
        wrappedInfo.valueVar->getValueInterfaceType());
    if (wrappedValueType->hasError())
      break;
  }
                                    
  return wrappedValueType;
}

Expr *swift::buildPropertyWrapperWrappedValueCall(
    VarDecl *var, Type backingStorageType, Expr *value, bool ignoreAttributeArgs,
    llvm::function_ref<void(ApplyExpr *)> innermostInitCallback) {
  // From the innermost wrapper type out, form init(wrapperValue:) calls.
  ASTContext &ctx = var->getASTContext();
  auto wrapperAttrs = var->getAttachedPropertyWrappers();
  Expr *initializer = value;
  ApplyExpr *innermostInit = nullptr;

  for (unsigned i : llvm::reverse(indices(wrapperAttrs))) {
    Type wrapperType =
      backingStorageType ? computeWrappedValueType(var, backingStorageType, i)
                         : var->getAttachedPropertyWrapperType(i);
    if (!wrapperType)
      return nullptr;

    auto reprRange = SourceRange();
    if (auto *repr = wrapperAttrs[i]->getTypeRepr()) {
      reprRange = repr->getSourceRange();
    }

    auto typeExpr =
        TypeExpr::createImplicitHack(reprRange.Start, wrapperType, ctx);

    SourceLoc startLoc = reprRange.Start;

    // If there were no arguments provided for the attribute at this level,
    // call `init(wrappedValue:)` directly.
    auto attr = wrapperAttrs[i];
    if (!attr->getArg() || ignoreAttributeArgs) {
      Identifier argName;
      switch (var->getAttachedPropertyWrapperTypeInfo(i).wrappedValueInit) {
      case PropertyWrapperTypeInfo::HasInitialValueInit:
        argName = ctx.Id_initialValue;
        break;

      case PropertyWrapperTypeInfo::HasWrappedValueInit:
      case PropertyWrapperTypeInfo::NoWrappedValueInit:
        argName = ctx.Id_wrappedValue;
        break;
      }

      auto endLoc = initializer->getEndLoc();
      if (endLoc.isInvalid() && startLoc.isValid())
        endLoc = reprRange.End;

      auto *init =
          CallExpr::create(ctx, typeExpr, startLoc, {initializer}, {argName},
                           {initializer->getStartLoc()}, endLoc,
                           /*trailingClosures=*/{}, /*implicit=*/true);
      initializer = init;

      if (!innermostInit)
        innermostInit = init;
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
      endLoc = reprRange.End;

    auto *init = CallExpr::create(ctx, typeExpr, startLoc, elements,
                                   elementNames, elementLocs, endLoc,
                                   /*trailingClosures=*/{}, /*implicit=*/true);
    initializer = init;

    if (!innermostInit)
      innermostInit = init;
  }

  // Invoke the callback, passing in the innermost init(wrappedValue:) call
  innermostInitCallback(innermostInit);

  return initializer;
}
