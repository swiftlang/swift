//===--- ConstraintSystem.cpp - Constraint-based Type Checking ------------===//
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
//
// This file implements the constraint-based type checker, anchored by the
// \c ConstraintSystem class, which provides type checking and type
// inference for expressions.
//
//===----------------------------------------------------------------------===//
#include "ConstraintSystem.h"
#include "ConstraintGraph.h"
#include "swift/AST/ArchetypeBuilder.h"

using namespace swift;
using namespace constraints;

ConstraintSystem::ConstraintSystem(TypeChecker &tc, DeclContext *dc)
  : TC(tc), DC(dc), Arena(tc.Context, Allocator),
    CG(*new ConstraintGraph(*this))
{
  assert(DC && "context required");
}

ConstraintSystem::~ConstraintSystem() {
  delete &CG;
}

bool ConstraintSystem::hasFreeTypeVariables() {
  // Look for any free type variables.
  for (auto tv : TypeVariables) {
    if (!tv->getImpl().hasRepresentativeOrFixed()) {
      return true;
    }
  }
  
  return false;
}

void ConstraintSystem::addTypeVariable(TypeVariableType *typeVar) {
  TypeVariables.push_back(typeVar);
  
  // Notify the constraint graph.
  (void)CG[typeVar];
}

void ConstraintSystem::mergeEquivalenceClasses(TypeVariableType *typeVar1,
                                               TypeVariableType *typeVar2) {
  assert(typeVar1 == getRepresentative(typeVar1) &&
         "typeVar1 is not the representative");
  assert(typeVar2 == getRepresentative(typeVar2) &&
         "typeVar2 is not the representative");
  assert(typeVar1 != typeVar2 && "cannot merge type with itself");
  typeVar1->getImpl().mergeEquivalenceClasses(typeVar2, getSavedBindings());

  // Merge nodes in the constraint graph.
  CG.mergeNodes(typeVar1, typeVar2);
  addTypeVariableConstraintsToWorkList(typeVar1);
}

void ConstraintSystem::assignFixedType(TypeVariableType *typeVar, Type type,
                                       bool updateState) {
  typeVar->getImpl().assignFixedType(type, getSavedBindings());

  if (!updateState)
    return;

  if (!type->is<TypeVariableType>()) {
    // If this type variable represents a literal, check whether we picked the
    // default literal type. First, find the corresponding protocol.
    ProtocolDecl *literalProtocol = nullptr;
    // If we have the constraint graph, we can check all type variables in
    // the equivalence class. This is the More Correct path.
    // FIXME: Eliminate the less-correct path.
    auto typeVarRep = getRepresentative(typeVar);
    for (auto tv : CG[typeVarRep].getEquivalenceClass()) {
      auto locator = tv->getImpl().getLocator();
      if (!locator || !locator->getPath().empty())
        continue;

      auto anchor = locator->getAnchor();
      if (!anchor)
        continue;

      literalProtocol = TC.getLiteralProtocol(anchor);
      if (literalProtocol)
        break;
    }

    // If the protocol has a default type, check it.
    if (literalProtocol) {
      if (auto defaultType = TC.getDefaultType(literalProtocol, DC)) {
        // Check whether the nominal types match. This makes sure that we
        // properly handle Slice vs. Slice<T>.
        if (defaultType->getAnyNominal() != type->getAnyNominal())
          increaseScore(SK_NonDefaultLiteral);
      }
    }
  }

  // Notify the constraint graph.
  CG.bindTypeVariable(typeVar, type);
  addTypeVariableConstraintsToWorkList(typeVar);
}

void ConstraintSystem::addTypeVariableConstraintsToWorkList(
       TypeVariableType *typeVar) {
  // Gather the constraints affected by a change to this type variable.
  SmallVector<Constraint *, 8> constraints;
  CG.gatherConstraints(typeVar, constraints);

  // Add any constraints that aren't already active to the worklist.
  for (auto constraint : constraints) {
    if (!constraint->isActive()) {
      ActiveConstraints.splice(ActiveConstraints.end(),
                               InactiveConstraints, constraint);
      constraint->setActive(true);
    }
  }
}

/// Retrieve a uniqued selector ID for the given declaration.
static std::pair<unsigned, CanType>
getDynamicResultSignature(ValueDecl *decl,
                          llvm::StringMap<unsigned> &selectors) {
  llvm::SmallString<32> buffer;

  StringRef selector;
  Type type;
  if (auto func = dyn_cast<FuncDecl>(decl)) {
    // Handle functions.
    selector = func->getObjCSelector(buffer);
    type = decl->getType()->castTo<AnyFunctionType>()->getResult();

    // Append a '+' for static methods, '-' for instance methods. This
    // distinguishes methods with a given name from properties that
    // might have the same name.
    if (func->isStatic()) {
      buffer += '+';
    } else {
      buffer += '-';
    }
    selector = buffer.str();
  } else if (auto var = dyn_cast<VarDecl>(decl)) {
    // Handle properties. Only the getter matters.
    selector = var->getObjCGetterSelector(buffer);
    type = decl->getType();
  } else if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    // Handle constructors.
    selector = ctor->getObjCSelector(buffer);
    type = decl->getType()->castTo<AnyFunctionType>()->getResult();
  } else if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
    selector = subscript->getObjCGetterSelector();
    type = subscript->getType();
  } else {
    llvm_unreachable("Dynamic lookup found a non-[objc] result");
  }

  // Look for this selector in the table. If we find it, we're done.
  auto known = selectors.find(selector);
  if (known != selectors.end())
    return { known->second, type->getCanonicalType() };

  // Add this selector to the table.
  unsigned result = selectors.size();
  selectors[selector] = result;
  return { result, type->getCanonicalType() };
}

LookupResult &ConstraintSystem::lookupMember(Type base, Identifier name) {
  base = base->getCanonicalType();

  // Check whether we've already performed this lookup.
  auto knownMember = MemberLookups.find({base, name});
  if (knownMember != MemberLookups.end())
    return *knownMember->second;

  // Lookup the member.
  MemberLookups[{base, name}] = Nothing;
  auto lookup = TC.lookupMember(base, name, DC);
  auto &result = MemberLookups[{base, name}];
  result = std::move(lookup);

  // If we aren't performing dynamic lookup, we're done.
  auto instanceTy = base->getRValueType();
  if (auto metaTy = instanceTy->getAs<MetatypeType>())
    instanceTy = metaTy->getInstanceType();
  auto protoTy = instanceTy->getAs<ProtocolType>();
  if (!*result ||
      !protoTy ||
      !protoTy->getDecl()->isSpecificProtocol(
                             KnownProtocolKind::DynamicLookup))
    return *result;

  // We are performing dynamic lookup. Filter out redundant results early.
  llvm::DenseSet<std::pair<unsigned, CanType>> known;
  llvm::StringMap<unsigned> selectors;
  result->filter([&](ValueDecl *decl) -> bool {
    return known.insert(getDynamicResultSignature(decl, selectors)).second;
  });

  return *result;
}

ArrayRef<Type> ConstraintSystem::getAlternativeLiteralTypes(
                  KnownProtocolKind kind) {
  unsigned index;

  switch (kind) {
#define PROTOCOL(Protocol) \
  case KnownProtocolKind::Protocol: llvm_unreachable("Not a literal protocol");
#define LITERAL_CONVERTIBLE_PROTOCOL(Protocol)
#include "swift/AST/KnownProtocols.def"

  case KnownProtocolKind::ArrayLiteralConvertible:
    index = 0;
    break;

  case KnownProtocolKind::CharacterLiteralConvertible:
    index = 1;
    break;

  case KnownProtocolKind::DictionaryLiteralConvertible:
    index = 2;
    break;

  case KnownProtocolKind::FloatLiteralConvertible:
    index = 3;
    break;

  case KnownProtocolKind::IntegerLiteralConvertible:
    index = 4;
    break;

  case KnownProtocolKind::StringInterpolationConvertible:
    index = 5;
    break;

  case KnownProtocolKind::StringLiteralConvertible:
    index = 6;
    break;
  }

  // If we already looked for alternative literal types, return those results.
  if (AlternativeLiteralTypes[index])
    return *AlternativeLiteralTypes[index];

  // Collect all of the types that conform to the given literal protocol.
  SmallVector<Type, 4> types;
  for (auto decl : TC.Context.getTypesThatConformTo(kind)) {
    Type type;
    if (auto nominal = dyn_cast<NominalTypeDecl>(decl))
      type = nominal->getDeclaredTypeOfContext();
    else
      type = cast<ExtensionDecl>(decl)->getDeclaredTypeOfContext();

    types.push_back(type);
  }

  AlternativeLiteralTypes[index] = allocateCopy(types);
  return *AlternativeLiteralTypes[index];
}

ConstraintLocator *ConstraintSystem::getConstraintLocator(
                     Expr *anchor,
                     ArrayRef<ConstraintLocator::PathElement> path) {
  // Check whether a locator with this anchor + path already exists.
  llvm::FoldingSetNodeID id;
  ConstraintLocator::Profile(id, anchor, path);
  void *insertPos = nullptr;
  auto locator = ConstraintLocators.FindNodeOrInsertPos(id, insertPos);
  if (locator)
    return locator;

  // Allocate a new locator and add it to the set.
  locator = ConstraintLocator::create(getAllocator(), anchor, path);
  ConstraintLocators.InsertNode(locator, insertPos);
  return locator;
}

ConstraintLocator *ConstraintSystem::getConstraintLocator(
                     const ConstraintLocatorBuilder &builder) {
  // If the builder has an empty path, just extract its base locator.
  if (builder.hasEmptyPath()) {
    return builder.getBaseLocator();
  }

  // We have to build a new locator. Extract the paths from the builder.
  SmallVector<LocatorPathElt, 4> path;
  Expr *anchor = builder.getLocatorParts(path);
  if (!anchor)
    return nullptr;

  return getConstraintLocator(anchor, path);
}

bool ConstraintSystem::addConstraint(Constraint *constraint,
                                     bool isExternallySolved,
                                     bool simplifyExisting) {
  switch (simplifyConstraint(*constraint)) {
  case SolutionKind::Error:
    if (!failedConstraint) {
      failedConstraint = constraint;
    }

    if (solverState) {
      solverState->retiredConstraints.push_front(constraint);
      if (!simplifyExisting) {
        solverState->generatedConstraints.push_back(constraint);
      }
    }

    return false;

  case SolutionKind::Solved:
    // This constraint has already been solved; there is nothing more
    // to do.
    // Record solved constraint.
    if (solverState) {
      solverState->retiredConstraints.push_front(constraint);
      if (!simplifyExisting)
        solverState->generatedConstraints.push_back(constraint);
    }

    // Remove the constraint from the constraint graph.
    if (simplifyExisting)
      CG.removeConstraint(constraint);
    
    return true;

  case SolutionKind::Unsolved:
    // We couldn't solve this constraint; add it to the pile.
    if (!isExternallySolved) {
      InactiveConstraints.push_back(constraint);        
    }

    // Add this constraint to the constraint graph.
    if (!simplifyExisting)
      CG.addConstraint(constraint);

    if (!simplifyExisting && solverState) {
      solverState->generatedConstraints.push_back(constraint);
    }

    return false;
  }
}

/// Check whether this is the depth 0, index 0 generic parameter, which is
/// used for the 'Self' type of a protocol.
static bool isProtocolSelfType(Type type) {
  auto gp = type->getAs<GenericTypeParamType>();
  if (!gp)
    return false;

  return gp->getDepth() == 0 && gp->getIndex() == 0;
}

namespace {
  /// Function object that retrieves a type variable corresponding to the
  /// given dependent type.
  class GetTypeVariable {
    ConstraintSystem &CS;
    DependentTypeOpener *Opener;

    /// The type variables introduced for (base type, associated type) pairs.
    llvm::DenseMap<std::pair<CanType, AssociatedTypeDecl *>, TypeVariableType *>
      MemberReplacements;

  public:
    GetTypeVariable(ConstraintSystem &cs, DependentTypeOpener *opener)
      : CS(cs), Opener(opener) { }

    TypeVariableType *operator()(Type base, AssociatedTypeDecl *member) {
      auto known = MemberReplacements.find({base->getCanonicalType(), member});
      if (known != MemberReplacements.end())
        return known->second;

      auto baseTypeVar = base->castTo<TypeVariableType>();
      auto archetype = baseTypeVar->getImpl().getArchetype()
                         ->getNestedType(member->getName());
      auto tv = CS.createTypeVariable(CS.getConstraintLocator(
                                        (Expr *)nullptr,
                                        LocatorPathElt(archetype)),
                                      TVO_PrefersSubtypeBinding);
      MemberReplacements[{base->getCanonicalType(), member}] = tv;

      // Determine whether we should bind the new type variable as a
      // member of the base type variable, or let it float.
      Type replacementType;
      bool shouldBindMember = true;
      if (Opener) {
        shouldBindMember = Opener->shouldBindAssociatedType(base, baseTypeVar,
                                                            member, tv,
                                                            replacementType);
      }

      // Bind the member's type variable as a type member of the base,
      // if needed.
      if (shouldBindMember)
        CS.addTypeMemberConstraint(base, member->getName(), tv);

      // If we have a replacement type, bind the member's type
      // variable to it.
      if (replacementType)
        CS.addConstraint(ConstraintKind::Bind, tv, replacementType);

      // Add associated type constraints.
      // FIXME: Would be better to walk the requirements of the protocol
      // of which the associated type is a member.
      if (auto superclass = member->getSuperclass()) {
        CS.addConstraint(ConstraintKind::Subtype, tv, superclass);
      }

      for (auto proto : member->getArchetype()->getConformsTo()) {
        CS.addConstraint(ConstraintKind::ConformsTo, tv,
                         proto->getDeclaredType());
      }
      return tv;
    }
  };

  /// Function object that replaces all occurrences of archetypes and
  /// dependent types with type variables.
  class ReplaceDependentTypes {
    ConstraintSystem &cs;
    DeclContext *dc;
    bool skipProtocolSelfConstraint;
    DependentTypeOpener *opener;
    llvm::DenseMap<CanType, TypeVariableType *> &replacements;
    GetTypeVariable &getTypeVariable;

  public:
    ReplaceDependentTypes(
        ConstraintSystem &cs,
        DeclContext *dc,
        bool skipProtocolSelfConstraint,
        DependentTypeOpener *opener,
        llvm::DenseMap<CanType, TypeVariableType *> &replacements,
        GetTypeVariable &getTypeVariable)
      : cs(cs), dc(dc), skipProtocolSelfConstraint(skipProtocolSelfConstraint),
        opener(opener), replacements(replacements), 
        getTypeVariable(getTypeVariable) { }

    Type operator()(Type type) {
      assert(!type->is<PolymorphicFunctionType>() && "Shouldn't get here");

      // Replace archetypes with fresh type variables.
      if (auto archetype = type->getAs<ArchetypeType>()) {
        auto known = replacements.find(archetype->getCanonicalType());
        if (known != replacements.end())
          return known->second;

        return archetype;
      }

      // Replace a generic type parameter with its corresponding type variable.
      if (auto genericParam = type->getAs<GenericTypeParamType>()) {
        auto known = replacements.find(genericParam->getCanonicalType());
        assert(known != replacements.end());
        return known->second;
      }

      // Replace a dependent member with a fresh type variable and make it a
      // member of its base type.
      if (auto dependentMember = type->getAs<DependentMemberType>()) {
        // Check whether we've already dealt with this dependent member.
        auto known = replacements.find(dependentMember->getCanonicalType());
        if (known != replacements.end())
          return known->second;

        // Replace archetypes in the base type.
        auto base = (*this)(dependentMember->getBase());
        auto result = getTypeVariable(base, dependentMember->getAssocType());
        replacements[dependentMember->getCanonicalType()] = result;
        return result;
      }

      // Create type variables for all of the parameters in a generic function
      // type.
      if (auto genericFn = type->getAs<GenericFunctionType>()) {
        // Open up the generic parameters and requirements.
        cs.openGeneric(dc,
                       genericFn->getGenericParams(),
                       genericFn->getRequirements(),
                       skipProtocolSelfConstraint,
                       opener,
                       replacements);

        // Transform the input and output types.
        Type inputTy = genericFn->getInput().transform(*this);
        if (!inputTy)
          return Type();

        Type resultTy = genericFn->getResult().transform(*this);
        if (!resultTy)
          return Type();

        // Build the resulting (non-generic) function type.
        return FunctionType::get(inputTy, resultTy);
      }

      // Open up unbound generic types, turning them into bound generic
      // types with type variables for each parameter.
      if (auto unbound = type->getAs<UnboundGenericType>()) {
        auto parentTy = unbound->getParent();
        if (parentTy)
          parentTy = parentTy.transform(*this);

        auto unboundDecl = unbound->getDecl();

        // Open up the generic type.
        cs.openGeneric(unboundDecl,
                       unboundDecl->getGenericParamTypes(),
                       unboundDecl->getGenericRequirements(),
                       /*skipProtocolSelfConstraint=*/false,
                       opener,
                       replacements);

        // Map the generic parameters to their corresponding type variables.
        llvm::SmallVector<Type, 4> arguments;
        for (auto gp : unboundDecl->getGenericParamTypes()) {
          assert(replacements.count(gp->getCanonicalType()) && 
                 "Missing generic parameter?");
          arguments.push_back(replacements[gp->getCanonicalType()]);
        }
        return BoundGenericType::get(unboundDecl, parentTy, arguments);
      }
      
      return type;
    }
  };
}

Type ConstraintSystem::openType(
       Type startingType,
       llvm::DenseMap<CanType, TypeVariableType *> &replacements,
       DeclContext *dc,
       bool skipProtocolSelfConstraint,
       DependentTypeOpener *opener) {
  GetTypeVariable getTypeVariable{*this, opener};

  ReplaceDependentTypes replaceDependentTypes(*this, dc,
                                              skipProtocolSelfConstraint,
                                              opener,
                                              replacements, getTypeVariable);
  return startingType.transform(replaceDependentTypes);
}

Type ConstraintSystem::openBindingType(Type type, DeclContext *dc) {
  Type result = openType(type, dc);
  // FIXME: Better way to identify Slice<T>.
  if (auto boundStruct
        = dyn_cast<BoundGenericStructType>(result.getPointer())) {
    if (!boundStruct->getParent() &&
        boundStruct->getDecl()->getName().str() == "Array" &&
        boundStruct->getGenericArgs().size() == 1) {
      if (auto replacement = getTypeChecker().getArraySliceType(
                               SourceLoc(), boundStruct->getGenericArgs()[0])) {
        return replacement;
      }
    }
  }

  return result;
}

static Type getFixedTypeRecursiveHelper(ConstraintSystem &cs,
                                        TypeVariableType *typeVar,
                                        bool wantRValue) {
  while (auto fixed = cs.getFixedType(typeVar)) {
    if (wantRValue)
      fixed = fixed->getRValueType();

    typeVar = fixed->getAs<TypeVariableType>();
    if (!typeVar)
      return fixed;
  }
  return nullptr;
}

Type ConstraintSystem::getFixedTypeRecursive(Type type, 
                                             TypeVariableType *&typeVar,
                                             bool wantRValue) {
  if (wantRValue)
    type = type->getRValueType();

  auto desugar = type->getDesugaredType();
  typeVar = desugar->getAs<TypeVariableType>();
  if (typeVar) {
    if (auto fixed = getFixedTypeRecursiveHelper(*this, typeVar, wantRValue)) {
      type = fixed;
      typeVar = nullptr;
    }
  }
  return type;
}

std::pair<Type, Type>
ConstraintSystem::getTypeOfReference(ValueDecl *value,
                                     bool isTypeReference,
                                     bool isSpecialized,
                                     DependentTypeOpener *opener) {
  if (value->getDeclContext()->isTypeContext() && isa<FuncDecl>(value)) {
    // Unqualified lookup can find operator names within nominal types.
    auto func = cast<FuncDecl>(value);
    assert(func->isOperator() && "Lookup should only find operators");

    auto openedType = openType(func->getInterfaceType(), func, false, opener);
    auto openedFnType = openedType->castTo<FunctionType>();

    // The 'Self' type must be bound to an archetype.
    // FIXME: We eventually want to loosen this constraint, to allow us
    // to find operator functions both in classes and in protocols to which
    // a class conforms (if there's a default implementation).
    addArchetypeConstraint(openedFnType->getInput()->getRValueInstanceType());

    // The reference implicitly binds 'self'.
    return { openedType, openedFnType->getResult() };
  }

  // If we have a type declaration, resolve it within the current context.
  if (auto typeDecl = dyn_cast<TypeDecl>(value)) {
    // Resolve the reference to this type declaration in our current context.
    auto type = getTypeChecker().resolveTypeInContext(typeDecl, DC,
                                                      isSpecialized);
    if (!type)
      return { nullptr, nullptr };

    // Open the type.
    type = openType(type, value->getInnermostDeclContext(), false, opener);

    // If it's a type reference, we're done.
    if (isTypeReference)
      return { type, type };

    // If it's a value reference, refer to the metatype.
    type = MetatypeType::get(type, getASTContext());
    return { type, type };
  }

  // Determine the type of the value, opening up that type if necessary.
  Type valueType = TC.getUnopenedTypeOfReference(value, Type(),
                                                 /*wantInterfaceType=*/true);

  // Adjust the type of the reference.
  valueType = adjustLValueForReference(
                openType(valueType,
                         value->getPotentialGenericDeclContext(),
                         /*skipProtocolSelfConstraint=*/false,
                         opener),
                value->getAttrs().isAssignment());
  return { valueType, valueType };
}

void ConstraintSystem::openGeneric(
       DeclContext *dc,
       ArrayRef<GenericTypeParamType *> params,
       ArrayRef<Requirement> requirements,
       bool skipProtocolSelfConstraint,
       DependentTypeOpener *opener,
       llvm::DenseMap<CanType, TypeVariableType *> &replacements) {

  // Create the type variables for the generic parameters.
  for (auto gp : params) {
    ArchetypeType *archetype = ArchetypeBuilder::mapTypeIntoContext(dc, gp)
                                 ->castTo<ArchetypeType>();
    auto typeVar = createTypeVariable(getConstraintLocator(
                                        (Expr *)nullptr,
                                        LocatorPathElt(archetype)),
                                      TVO_PrefersSubtypeBinding);
    replacements[gp->getCanonicalType()] = typeVar;

    // Note that we opened a generic parameter to a type variable.
    if (opener) {
      Type replacementType;
      opener->openedGenericParameter(gp, typeVar, replacementType);

      if (replacementType)
        addConstraint(ConstraintKind::Bind, typeVar, replacementType);
    }
  }

  GetTypeVariable getTypeVariable{*this, opener};
  ReplaceDependentTypes replaceDependentTypes(*this, dc,
                                              skipProtocolSelfConstraint,
                                              opener, replacements, 
                                              getTypeVariable);

  // Add the requirements as constraints.
  for (auto req : requirements) {
  switch (req.getKind()) {
    case RequirementKind::Conformance: {
      auto subjectTy = req.getFirstType().transform(replaceDependentTypes);
      if (auto proto = req.getSecondType()->getAs<ProtocolType>()) {
        if (!skipProtocolSelfConstraint ||
            !(isa<ProtocolDecl>(dc) || isa<ProtocolDecl>(dc->getParent())) ||
            !isProtocolSelfType(req.getFirstType())) {
          addConstraint(ConstraintKind::ConformsTo, subjectTy,
                        proto);
        }
      } else {
        addConstraint(ConstraintKind::Subtype, subjectTy,
                      req.getSecondType());
      }
      break;
    }

    case RequirementKind::SameType: {
      auto firstTy = req.getFirstType().transform(replaceDependentTypes);
      auto secondTy = req.getSecondType().transform(replaceDependentTypes);
      addConstraint(ConstraintKind::Bind, firstTy, secondTy);
      break;
    }

    case RequirementKind::ValueWitnessMarker:
      break;
  }
  }
}

/// Add the constraint on the type used for the 'Self' type for a member
/// reference.
///
/// \param cs The constraint system.
///
/// \param objectTy The type of the object that we're using to access the
/// member.
///
/// \param selfTy The instance type of the context in which the member is
/// declared.
static void addSelfConstraint(ConstraintSystem &cs, Type objectTy, Type selfTy){
  // When referencing a protocol member, we need the object type to be usable
  // as the Self type of the protocol, which covers anything that conforms to
  // the protocol as well as existentials that include that protocol.
  if (selfTy->is<ProtocolType>()) {
    cs.addConstraint(ConstraintKind::SelfObjectOfProtocol, objectTy, selfTy);
    return;
  }

  // Otherwise, use a subtype constraint for classes to cope with inheritance.
  if (selfTy->getClassOrBoundGenericClass()) {
    cs.addConstraint(ConstraintKind::Subtype, objectTy, selfTy);
    return;
  }

  // Otherwise, the types must be equivalent.
  cs.addConstraint(ConstraintKind::Equal, objectTy, selfTy);
}

/// Collect all of the generic parameters and requirements from the
/// given context and its outer contexts.
static void collectContextParamsAndRequirements(
              DeclContext *dc, 
              SmallVectorImpl<GenericTypeParamType *> &genericParams, 
              SmallVectorImpl<Requirement> &genericRequirements) {
  if (!dc->isTypeContext())
    return;

  // Recurse to outer context.
  collectContextParamsAndRequirements(dc->getParent(), genericParams,
                                      genericRequirements);

  // Add our generic parameters and requirements.
  auto nominal = dc->getDeclaredTypeOfContext()->getAnyNominal();
  genericParams.append(nominal->getGenericParamTypes().begin(),
                       nominal->getGenericParamTypes().end());
  genericRequirements.append(nominal->getGenericRequirements().begin(),
                             nominal->getGenericRequirements().end());
}

std::pair<Type, Type>
ConstraintSystem::getTypeOfMemberReference(Type baseTy, ValueDecl *value,
                                           bool isTypeReference,
                                           bool isDynamicResult,
                                           DependentTypeOpener *opener) {
  // Figure out the instance type used for the base.
  TypeVariableType *baseTypeVar = nullptr;
  Type baseObjTy = getFixedTypeRecursive(baseTy, baseTypeVar, 
                                         /*wantRValue=*/true);
  bool isInstance = true;
  if (auto baseMeta = baseObjTy->getAs<MetatypeType>()) {
    baseObjTy = baseMeta->getInstanceType();
    isInstance = false;
  }

  // If the base is a module type, just use the type of the decl.
  if (baseObjTy->is<ModuleType>()) {
    return getTypeOfReference(value, isTypeReference, /*isSpecialized=*/false,
                              opener);
  }

  // Handle associated type lookup as a special case, horribly.
  // FIXME: This is an awful hack.
  if (auto assocType = dyn_cast<AssociatedTypeDecl>(value)) {
    // Refer to a member of the archetype directly.
    if (auto archetype = baseObjTy->getAs<ArchetypeType>()) {
      Type memberTy = archetype->getNestedType(value->getName());
      if (!isTypeReference)
        memberTy = MetatypeType::get(memberTy, TC.Context);

      auto openedType = FunctionType::get(baseObjTy, memberTy);
      return { openedType, memberTy };
    }

    // If we have a nominal type that conforms to the protocol in which the
    // associated type resides, use the witness.
    if (!baseObjTy->isExistentialType() &&
        !baseObjTy->hasTypeVariable() &&
        baseObjTy->getAnyNominal()) {

      auto proto = cast<ProtocolDecl>(assocType->getDeclContext());
      ProtocolConformance *conformance = nullptr;
      if (TC.conformsToProtocol(baseObjTy, proto, DC, &conformance) &&
          conformance->isComplete()) {
        auto memberTy = conformance->getTypeWitness(assocType, &TC).Replacement;
        if (!isTypeReference)
          memberTy = MetatypeType::get(memberTy, TC.Context);

        auto openedType = FunctionType::get(baseObjTy, memberTy);
        return { openedType, memberTy };
      }
    }

    // FIXME: Totally bogus fallthrough.
    Type memberTy = isTypeReference? assocType->getDeclaredType()
                                   : assocType->getType();
    auto openedType = FunctionType::get(baseObjTy, memberTy);
    return { openedType, memberTy };
  }

  // Figure out the declaration context to use when opening this type.
  DeclContext *dc = value->getPotentialGenericDeclContext();

  // Open the type of the generic function or member of a generic type.
  Type openedType;
  if (auto genericFn = value->getInterfaceType()->getAs<GenericFunctionType>()){
    openedType = openType(genericFn, dc, /*skipProtocolSelfConstraint=*/true,
                          opener);
  } else {
    openedType = TC.getUnopenedTypeOfReference(value, baseTy,
                                               /*wantInterfaceType=*/true);

    Type selfTy;
    if (dc->isGenericContext()) {
      // Open up the generic parameter list for the container.
      auto nominal = dc->getDeclaredTypeOfContext()->getAnyNominal();
      llvm::DenseMap<CanType, TypeVariableType *> replacements;
      SmallVector<GenericTypeParamType *, 4> genericParams;
      SmallVector<Requirement, 4> genericRequirements;
      collectContextParamsAndRequirements(dc, genericParams,
                                          genericRequirements);
      openGeneric(dc, genericParams, genericRequirements,
                  /*skipProtocolSelfConstraint=*/true,
                  opener,
                  replacements);

      // Open up the type of the member.
      openedType = openType(openedType, replacements, nullptr, false, opener);

      // Determine the object type of 'self'.
      if (auto protocol = dyn_cast<ProtocolDecl>(nominal)) {
        // Retrieve the type variable for 'Self'.
        selfTy = replacements[protocol->getSelf()->getDeclaredType()
                                ->getCanonicalType()];
      } else {
        // Open the nominal type.
        selfTy = openType(nominal->getDeclaredInterfaceType(), replacements);
      }
    } else {
      selfTy = value->getDeclContext()->getDeclaredTypeOfContext();
    }
    
    // If we have a type reference, look through the metatype.
    if (isTypeReference)
      openedType = openedType->castTo<MetatypeType>()->getInstanceType();

    // If we're not coming from something function-like, prepend the type
    // for 'self' to the type.
    if (!isa<AbstractFunctionDecl>(value) && !isa<EnumElementDecl>(value)) {
      // If self is a struct, properly qualify it based on our base
      // qualification.  If we have an lvalue coming in, we expect an lvalue.
      // TODO: Not for @mutable methods, they don't require self to be an
      // lvalue.
      if (!selfTy->hasReferenceSemantics() &&
          baseTy->is<LValueType>())
        selfTy = LValueType::getImplicit(selfTy);

      openedType = FunctionType::get(selfTy, openedType);
    }
  }

  // Constrain the 'self' object type.
  auto openedFnType = openedType->castTo<FunctionType>();
  Type selfObjTy = openedFnType->getInput()->getRValueInstanceType();
  if (isa<ProtocolDecl>(value->getDeclContext())) {
    // For a protocol, substitute the base object directly. We don't need a
    // conformance constraint because we wouldn't have found the declaration
    // if it didn't conform.
    addConstraint(ConstraintKind::Equal, baseObjTy, selfObjTy);
  } else if (!isDynamicResult) {
    addSelfConstraint(*this, baseObjTy, selfObjTy);
  }

  // Compute the type of the reference.
  Type type;
  if (auto subscript = dyn_cast<SubscriptDecl>(value)) {
    // For a subscript, turn the element type into an optional or lvalue,
    // depending on whether the result type is optional/dynamic, is settable,
    // or is not.
    auto fnType = openedFnType->getResult()->castTo<FunctionType>();
    auto elementTy = fnType->getResult();
    if (isDynamicResult || subscript->getAttrs().isOptional())
      elementTy = OptionalType::get(elementTy);
    else if (subscript->isSettable())
      elementTy = LValueType::getImplicit(elementTy);
    type = FunctionType::get(fnType->getInput(), elementTy);
  } else if (isa<ProtocolDecl>(value->getDeclContext()) &&
             isa<AssociatedTypeDecl>(value)) {
    // When we have an associated type, the base type conforms to the
    // given protocol, so use the type witness directly.
    // FIXME: Diagnose existentials properly.
    // FIXME: Eliminate the "hasTypeVariables()" hack here.
    auto proto = cast<ProtocolDecl>(value->getDeclContext());
    auto assocType = cast<AssociatedTypeDecl>(value);

    type = openedFnType->getResult();
    if (baseObjTy->is<ArchetypeType>()) {
      // For an archetype, we substitute the base object for the base.
      // FIXME: Feels like a total hack.
    } else if (!baseObjTy->isExistentialType() &&
        !baseObjTy->is<ArchetypeType>() &&
        !baseObjTy->hasTypeVariable()) {
      ProtocolConformance *conformance = nullptr;
      if (TC.conformsToProtocol(baseObjTy, proto, DC, &conformance) &&
          conformance->isComplete()) {
        type = conformance->getTypeWitness(assocType, &TC).Replacement;
      }
    }
  } else if (isa<ConstructorDecl>(value) || isa<EnumElementDecl>(value) ||
             (isa<FuncDecl>(value) && cast<FuncDecl>(value)->isStatic()) ||
             (isa<VarDecl>(value) && cast<VarDecl>(value)->isStatic()) ||
             isa<TypeDecl>(value) ||
             isInstance) {
    // For a constructor, enum element, static method, static property,
    // or an instance method referenced through an instance, we've consumed the
    // curried 'self' already. For a type, strip off the 'self' we artificially
    // added.
    type = openedFnType->getResult();
  } else if (isDynamicResult && isa<AbstractFunctionDecl>(value)) {
    // For a dynamic result referring to an instance function through
    // an object of metatype type, replace the 'Self' parameter with
    // a DynamicLookup member.
    auto funcTy = type->castTo<AnyFunctionType>();
    Type resultTy = funcTy->getResult();
    Type inputTy = TC.getProtocol(SourceLoc(), KnownProtocolKind::DynamicLookup)
                     ->getDeclaredTypeOfContext();
    type = FunctionType::get(inputTy, resultTy, funcTy->getExtInfo());
  } else {
    type = openedType;
  }

  return { openedType, type };
}

void ConstraintSystem::addOverloadSet(Type boundType,
                                      ArrayRef<OverloadChoice> choices,
                                      ConstraintLocator *locator) {
  assert(!choices.empty() && "Empty overload set");

  SmallVector<Constraint *, 4> overloads;
  for (auto choice : choices) {
    overloads.push_back(Constraint::createBindOverload(*this, boundType, choice,
                                                       locator));
  }
  addConstraint(Constraint::createDisjunction(*this, overloads,
                                              locator));
}

/// \brief Retrieve the fully-materialized form of the given type.
static Type getMateralizedType(Type type, ASTContext &context) {
  if (auto lvalue = type->getAs<LValueType>()) {
    return lvalue->getObjectType();
  }

  if (auto tuple = type->getAs<TupleType>()) {
    bool anyChanged = false;
    SmallVector<TupleTypeElt, 4> elements;
    for (unsigned i = 0, n = tuple->getFields().size(); i != n; ++i) {
      auto elt = tuple->getFields()[i];
      auto eltType = getMateralizedType(elt.getType(), context);
      if (anyChanged) {
        elements.push_back(elt.getWithType(eltType));
        continue;
      }

      if (eltType.getPointer() != elt.getType().getPointer()) {
        elements.append(tuple->getFields().begin(),
                        tuple->getFields().begin() + i);
        elements.push_back(elt.getWithType(eltType));
        anyChanged = true;
      }
    }

    if (anyChanged) {
      return TupleType::get(elements, context);
    }
  }

  return type;
}

void ConstraintSystem::resolveOverload(ConstraintLocator *locator,
                                       Type boundType,
                                       OverloadChoice choice){
  // Determie the type to which we'll bind the overload set's type.
  Type refType;
  Type openedFullType;
  switch (choice.getKind()) {
  case OverloadChoiceKind::Decl:
  case OverloadChoiceKind::DeclViaDynamic:
  case OverloadChoiceKind::TypeDecl: {
    bool isTypeReference = choice.getKind() == OverloadChoiceKind::TypeDecl;
    bool isDynamicResult
      = choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
    // Retrieve the type of a reference to the specific declaration choice.
    if (choice.getBaseType())
      std::tie(openedFullType, refType)
        = getTypeOfMemberReference(choice.getBaseType(), choice.getDecl(),
                                   isTypeReference, isDynamicResult,
                                   nullptr);
    else
      std::tie(openedFullType, refType)
        = getTypeOfReference(choice.getDecl(),
                             isTypeReference,
                             choice.isSpecialized());

    if (isDynamicResult || choice.getDecl()->getAttrs().isOptional()) {
      // For a non-subscript declaration found via dynamic lookup or as an
      // optional requirement in a protocol, strip off the lvalue-ness (one
      // cannot assign to such declarations) and make a reference to that
      // declaration be optional.
      //
      // Subscript declarations are handled within
      // getTypeOfMemberReference(); their result types are optional.
      if (!isa<SubscriptDecl>(choice.getDecl()))
        refType = OptionalType::get(refType->getRValueType());
    } else {
      // Otherwise, adjust the lvalue type for this reference.
      bool isAssignment = choice.getDecl()->getAttrs().isAssignment();
      refType = adjustLValueForReference(refType, isAssignment);
    }

    break;
  }

  case OverloadChoiceKind::BaseType:
    refType = choice.getBaseType();
    break;

  case OverloadChoiceKind::TupleIndex: {
    if (auto lvalueTy = choice.getBaseType()->getAs<LValueType>()) {
      assert(lvalueTy->isImplicit() && "Cannot access tuple element of @inout");
      // When the base of a tuple lvalue, the member is always an lvalue.
      auto tuple = lvalueTy->getObjectType()->castTo<TupleType>();
      refType = tuple->getElementType(choice.getTupleIndex())->getRValueType();
      refType = LValueType::getImplicit(refType);
    } else {
      // When the base is a tuple rvalue, the member is always an rvalue.
      // FIXME: Do we have to strip several levels here? Possible.
      auto tuple = choice.getBaseType()->castTo<TupleType>();
      refType =getMateralizedType(tuple->getElementType(choice.getTupleIndex()),
                                  getASTContext());
    }
    break;
  }
  }

  // Add the type binding constraint.
  addConstraint(ConstraintKind::Bind, boundType, refType);

  // Note that we have resolved this overload.
  resolvedOverloadSets
    = new (*this) ResolvedOverloadSetListItem{resolvedOverloadSets,
                                              boundType,
                                              choice,
                                              locator,
                                              openedFullType,
                                              refType};
  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log.indent(solverState? solverState->depth * 2 : 2)
      << "(overload set choice binding "
      << boundType->getString() << " := "
      << refType->getString() << ")\n";
  }
}

Type ConstraintSystem::simplifyType(Type type,
       llvm::SmallPtrSet<TypeVariableType *, 16> &substituting) {
  return type.transform([&](Type type) -> Type {
            if (auto tvt = dyn_cast<TypeVariableType>(type.getPointer())) {
              tvt = getRepresentative(tvt);
              if (auto fixed = getFixedType(tvt)) {
                if (substituting.insert(tvt)) {
                  auto result = simplifyType(fixed, substituting);
                  substituting.erase(tvt);
                  return result;
                }
              }

              return tvt;
            }
                            
            return type;
         });
}

Type Solution::simplifyType(TypeChecker &tc, Type type) const {
  return type.transform([&](Type type) -> Type {
             if (auto tvt = dyn_cast<TypeVariableType>(type.getPointer())) {
               auto known = typeBindings.find(tvt);
               assert(known != typeBindings.end());
               type = known->second;
             }

             return type;
           });
}
