//===--- ConstraintSystem.cpp - Constraint-based Type Checking ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "llvm/ADT/SmallString.h"

using namespace swift;
using namespace constraints;

ConstraintSystem::ConstraintSystem(TypeChecker &tc, DeclContext *dc,
                                   ConstraintSystemOptions options)
  : TC(tc), DC(dc), Options(options),
    Arena(tc.Context, Allocator, 
          [&](TypeVariableType *baseTypeVar, AssociatedTypeDecl *assocType) {
            return getMemberType(baseTypeVar, assocType,
                                 ConstraintLocatorBuilder(nullptr),
                                 /*options=*/0);
          }),
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
                                               TypeVariableType *typeVar2,
                                               bool updateWorkList) {
  assert(typeVar1 == getRepresentative(typeVar1) &&
         "typeVar1 is not the representative");
  assert(typeVar2 == getRepresentative(typeVar2) &&
         "typeVar2 is not the representative");
  assert(typeVar1 != typeVar2 && "cannot merge type with itself");
  typeVar1->getImpl().mergeEquivalenceClasses(typeVar2, getSavedBindings());

  // Merge nodes in the constraint graph.
  CG.mergeNodes(typeVar1, typeVar2);

  if (updateWorkList) {
    addTypeVariableConstraintsToWorkList(typeVar1);
  }
}

void ConstraintSystem::assignFixedType(TypeVariableType *typeVar, Type type,
                                       bool updateState) {
  
  // If the type to be fixed is an optional type that wraps the type parameter
  // itself, we do not want to go through with the assignment. To do so would
  // force the type variable to be adjacent to itself.
  if (auto optValueType = type->getOptionalObjectType()) {
    if (optValueType->isEqual(typeVar))
      return;
  }
  
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
        // properly handle Array vs. Array<T>.
        if (defaultType->getAnyNominal() != type->getAnyNominal())
          increaseScore(SK_NonDefaultLiteral);
      }
    }
  }

  // Notify the constraint graph.
  CG.bindTypeVariable(typeVar, type);
  addTypeVariableConstraintsToWorkList(typeVar);
}

void ConstraintSystem::setMustBeMaterializableRecursive(Type type)
{
  assert(type->isMaterializable() &&
         "argument to setMustBeMaterializableRecursive may not be inherently "
         "non-materializable");
  TypeVariableType *typeVar = nullptr;
  type = getFixedTypeRecursive(type, typeVar, /*wantRValue=*/false);
  if (typeVar) {
    typeVar->getImpl().setMustBeMaterializable(getSavedBindings());
  } else if (auto *tupleTy = type->getAs<TupleType>()) {
    for (auto elt : tupleTy->getElementTypes()) {
      setMustBeMaterializableRecursive(elt);
    }
  }
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

/// Retrieve a dynamic result signature for the given declaration.
static std::tuple<char, ObjCSelector, CanType>
getDynamicResultSignature(ValueDecl *decl) {
  if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
    // Handle functions.
    auto type =
      decl->getInterfaceType()->castTo<AnyFunctionType>()->getResult();
    return std::make_tuple(func->isStatic(), func->getObjCSelector(),
                           type->getCanonicalType());
  }

  if (auto asd = dyn_cast<AbstractStorageDecl>(decl)) {
    // Handle properties and subscripts, anchored by the getter's selector.
    return std::make_tuple(asd->isStatic(), asd->getObjCGetterSelector(),
                           asd->getInterfaceType()->getCanonicalType());
  }

  llvm_unreachable("Not a valid @objc member");
}

LookupResult &ConstraintSystem::lookupMember(Type base, DeclName name) {
  base = base->getCanonicalType();

  // Check whether we've already performed this lookup.
  auto knownMember = MemberLookups.find({base, name});
  if (knownMember != MemberLookups.end())
    return *knownMember->second;

  // Lookup the member.
  NameLookupOptions lookupOptions = defaultMemberLookupOptions;
  if (isa<AbstractFunctionDecl>(DC))
    lookupOptions |= NameLookupFlags::KnownPrivate;

  MemberLookups[{base, name}] = None;
  auto lookup = TC.lookupMember(DC, base, name, lookupOptions);
  auto &result = MemberLookups[{base, name}];
  result = std::move(lookup);

  // If we aren't performing dynamic lookup, we're done.
  auto instanceTy = base->getRValueType();
  if (auto metaTy = instanceTy->getAs<AnyMetatypeType>())
    instanceTy = metaTy->getInstanceType();
  auto protoTy = instanceTy->getAs<ProtocolType>();
  if (!*result ||
      !protoTy ||
      !protoTy->getDecl()->isSpecificProtocol(
                             KnownProtocolKind::AnyObject))
    return *result;

  // We are performing dynamic lookup. Filter out redundant results early.
  llvm::DenseSet<std::tuple<char, ObjCSelector, CanType>> known;
  result->filter([&](ValueDecl *decl) -> bool {
    if (decl->isInvalid())
      return false;

    return known.insert(getDynamicResultSignature(decl)).second;
  });

  return *result;
}

ArrayRef<Type> ConstraintSystem::
getAlternativeLiteralTypes(KnownProtocolKind kind) {
  unsigned index;

  switch (kind) {
#define PROTOCOL_WITH_NAME(Id, Name) \
  case KnownProtocolKind::Id: llvm_unreachable("Not a literal protocol");
#define LITERAL_CONVERTIBLE_PROTOCOL_WITH_NAME(Id, Name)
#include "swift/AST/KnownProtocols.def"

  case KnownProtocolKind::ArrayLiteralConvertible:     index = 0; break;
  case KnownProtocolKind::DictionaryLiteralConvertible:index = 1; break;
  case KnownProtocolKind::ExtendedGraphemeClusterLiteralConvertible: index = 2;
    break;
  case KnownProtocolKind::FloatLiteralConvertible: index = 3; break;
  case KnownProtocolKind::IntegerLiteralConvertible: index = 4; break;
  case KnownProtocolKind::StringInterpolationConvertible: index = 5; break;
  case KnownProtocolKind::StringLiteralConvertible: index = 6; break;
  case KnownProtocolKind::NilLiteralConvertible: index = 7; break;
  case KnownProtocolKind::BooleanLiteralConvertible: index = 8; break;
  case KnownProtocolKind::UnicodeScalarLiteralConvertible: index = 9; break;
  case KnownProtocolKind::ColorLiteralConvertible: index = 10; break;
  case KnownProtocolKind::ImageLiteralConvertible: index = 11; break;
  case KnownProtocolKind::FileReferenceLiteralConvertible: index = 12; break;
  }

  // If we already looked for alternative literal types, return those results.
  if (AlternativeLiteralTypes[index])
    return *AlternativeLiteralTypes[index];

  SmallVector<Type, 4> types;

  // If the default literal type is bridged to a class type, add the class type.
  if (auto proto = TC.Context.getProtocol(kind)) {
    if (auto defaultType = TC.getDefaultType(proto, DC)) {
      if (auto bridgedClassType = TC.getBridgedToObjC(DC, defaultType)) {
        types.push_back(bridgedClassType);
      }
    }
  }

  // Some literal kinds are related.
  switch (kind) {
#define PROTOCOL_WITH_NAME(Id, Name) \
  case KnownProtocolKind::Id: llvm_unreachable("Not a literal protocol");
#define LITERAL_CONVERTIBLE_PROTOCOL_WITH_NAME(Id, Name)
#include "swift/AST/KnownProtocols.def"

  case KnownProtocolKind::ArrayLiteralConvertible:
  case KnownProtocolKind::DictionaryLiteralConvertible:
    break;

  case KnownProtocolKind::ExtendedGraphemeClusterLiteralConvertible:
  case KnownProtocolKind::StringInterpolationConvertible:
  case KnownProtocolKind::StringLiteralConvertible:
  case KnownProtocolKind::UnicodeScalarLiteralConvertible:
    break;

  case KnownProtocolKind::IntegerLiteralConvertible:
    // Integer literals can be treated as floating point literals.
    if (auto floatProto = TC.Context.getProtocol(
                            KnownProtocolKind::FloatLiteralConvertible)) {
      if (auto defaultType = TC.getDefaultType(floatProto, DC)) {
        types.push_back(defaultType);
      }
    }
    break;

  case KnownProtocolKind::FloatLiteralConvertible:
    break;

  case KnownProtocolKind::NilLiteralConvertible:
  case KnownProtocolKind::BooleanLiteralConvertible:
    break;
  case KnownProtocolKind::ColorLiteralConvertible:
  case KnownProtocolKind::ImageLiteralConvertible:
  case KnownProtocolKind::FileReferenceLiteralConvertible:
    break;
  }

  AlternativeLiteralTypes[index] = allocateCopy(types);
  return *AlternativeLiteralTypes[index];
}

ConstraintLocator *ConstraintSystem::getConstraintLocator(
                     Expr *anchor,
                     ArrayRef<ConstraintLocator::PathElement> path,
                     unsigned summaryFlags) {
  assert(summaryFlags == ConstraintLocator::getSummaryFlagsForPath(path));

  // Check whether a locator with this anchor + path already exists.
  llvm::FoldingSetNodeID id;
  ConstraintLocator::Profile(id, anchor, path);
  void *insertPos = nullptr;
  auto locator = ConstraintLocators.FindNodeOrInsertPos(id, insertPos);
  if (locator)
    return locator;

  // Allocate a new locator and add it to the set.
  locator = ConstraintLocator::create(getAllocator(), anchor, path,
                                      summaryFlags);
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
  return getConstraintLocator(anchor, path, builder.getSummaryFlags());
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

TypeVariableType *
ConstraintSystem::getMemberType(TypeVariableType *baseTypeVar, 
                                AssociatedTypeDecl *assocType,
                                ConstraintLocatorBuilder locator,
                                unsigned options) {
  return CG.getMemberType(baseTypeVar, assocType->getName(), [&]() {
    // FIXME: Premature associated type -> identifier mapping. We should
    // retain the associated type throughout.
    auto loc = getConstraintLocator(locator);
    auto memberTypeVar = createTypeVariable(loc, options);
    addConstraint(Constraint::create(*this, ConstraintKind::TypeMember,
                                     baseTypeVar, memberTypeVar, 
                                     assocType->getName(), loc));
    return memberTypeVar;
  });
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
    ConstraintGraph &CG;
    ConstraintLocatorBuilder &Locator;

  public:
    GetTypeVariable(ConstraintSystem &cs,
                    ConstraintLocatorBuilder &locator)
      : CS(cs), CG(CS.getConstraintGraph()), Locator(locator) {}

    TypeVariableType *operator()(Type base, AssociatedTypeDecl *member) {
      // FIXME: Premature associated type -> identifier mapping. We should
      // retain the associated type throughout.
      auto baseTypeVar = base->castTo<TypeVariableType>();
      return CG.getMemberType(baseTypeVar, member->getName(),
                              [&]() -> TypeVariableType* {
        auto implArchetype = baseTypeVar->getImpl().getArchetype();
        if (!implArchetype) {
          // If the base type variable doesn't have an associated archetype,
          // just form the member constraint.
          // FIXME: Additional requirements?
          auto locator = CS.getConstraintLocator(
                           Locator.withPathElement(member));
          auto memberTypeVar = CS.createTypeVariable(locator,
                                                     TVO_PrefersSubtypeBinding);
          CS.addConstraint(Constraint::create(CS, ConstraintKind::TypeMember,
                                              baseTypeVar, memberTypeVar,
                                              member->getName(), locator));
          return memberTypeVar;
        }
                                
                                
        ArchetypeType::NestedType nestedType;
        ArchetypeType* archetype = nullptr;
                                
        if (implArchetype->hasNestedType(member->getName())) {
          nestedType = implArchetype->getNestedType(member->getName());
          archetype = nestedType.getValue()->getAs<ArchetypeType>();
        } else if (implArchetype->isSelfDerived()) {
          archetype = implArchetype;
        }
                                
        ConstraintLocator *locator;
        if (archetype) {
          locator = CS.getConstraintLocator(
                      Locator.withPathElement(LocatorPathElt(archetype)));
        } else {
          // FIXME: Occurs when the nested type is a concrete type,
          // in which case it's quite silly to create a type variable at all.
          locator = CS.getConstraintLocator(Locator.withPathElement(member));
        }
                                
        auto memberTypeVar = CS.createTypeVariable(locator,
                                                   TVO_PrefersSubtypeBinding);

        // Bind the member's type variable as a type member of the base.
        CS.addConstraint(Constraint::create(CS, ConstraintKind::TypeMember,
                                            baseTypeVar, memberTypeVar, 
                                            member->getName(), locator));

        if (!archetype) {
          // If the nested type is not an archetype (because it was constrained
          // to a concrete type by a requirement), return the fresh type
          // variable now, and let binding occur during overload resolution.
          return memberTypeVar;
        }
                                
        // FIXME: Would be better to walk the requirements of the protocol
        // of which the associated type is a member.
        if (auto superclass = member->getSuperclass()) {
          CS.addConstraint(ConstraintKind::Subtype, memberTypeVar,
                           superclass, locator);
        }

        for (auto proto : member->getArchetype()->getConformsTo()) {
          CS.addConstraint(ConstraintKind::ConformsTo, memberTypeVar,
                           proto->getDeclaredType(), locator);
        }

        return memberTypeVar;
      });
    }
  };

  /// Function object that replaces all occurrences of archetypes and
  /// dependent types with type variables.
  class ReplaceDependentTypes {
    ConstraintSystem &cs;
    DeclContext *dc;
    bool skipProtocolSelfConstraint;
    unsigned minOpeningDepth;
    ConstraintLocatorBuilder &locator;
    llvm::DenseMap<CanType, TypeVariableType *> &replacements;
    GetTypeVariable &getTypeVariable;

  public:
    ReplaceDependentTypes(
        ConstraintSystem &cs,
        DeclContext *dc,
        bool skipProtocolSelfConstraint,
        unsigned minOpeningDepth,
        ConstraintLocatorBuilder &locator,
        llvm::DenseMap<CanType, TypeVariableType *> &replacements,
        GetTypeVariable &getTypeVariable)
      : cs(cs), dc(dc), skipProtocolSelfConstraint(skipProtocolSelfConstraint),
        minOpeningDepth(minOpeningDepth), locator(locator),
        replacements(replacements), getTypeVariable(getTypeVariable) { }

    Type operator()(Type type) {
      assert(!type->is<PolymorphicFunctionType>() && "Shouldn't get here");

      // Preserve parens when opening types.
      if (isa<ParenType>(type.getPointer())) {
        return type;
      }

      // Replace a generic type parameter with its corresponding type variable.
      if (auto genericParam = type->getAs<GenericTypeParamType>()) {
        auto known = replacements.find(genericParam->getCanonicalType());
        
        if (known == replacements.end())
          return cs.createTypeVariable(nullptr, TVO_PrefersSubtypeBinding);
        
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
        if (auto base =
            ((*this)(dependentMember->getBase()))->getAs<TypeVariableType>()) {
          auto result = getTypeVariable(base, dependentMember->getAssocType());
          replacements[dependentMember->getCanonicalType()] = result;
          return result;
        }
      }

      // Create type variables for all of the parameters in a generic function
      // type.
      if (auto genericFn = type->getAs<GenericFunctionType>()) {
        // Open up the generic parameters and requirements.
        cs.openGeneric(dc,
                       genericFn->getGenericSignature(),
                       skipProtocolSelfConstraint,
                       minOpeningDepth,
                       locator,
                       replacements);

        // Transform the input and output types.
        Type inputTy = genericFn->getInput().transform(*this);
        if (!inputTy)
          return Type();

        Type resultTy = genericFn->getResult().transform(*this);
        if (!resultTy)
          return Type();

        // Build the resulting (non-generic) function type.
        return FunctionType::get(inputTy, resultTy,
                                 FunctionType::ExtInfo().
                                  withThrows(genericFn->throws()));
      }

      // Open up unbound generic types, turning them into bound generic
      // types with type variables for each parameter.
      if (auto unbound = type->getAs<UnboundGenericType>()) {
        auto parentTy = unbound->getParent();
        if (parentTy)
          parentTy = parentTy.transform(*this);

        auto unboundDecl = unbound->getDecl();
        if (unboundDecl->isInvalid())
          return ErrorType::get(cs.getASTContext());
        
        // If the unbound decl hasn't been validated yet, we have a circular
        // dependency that isn't being diagnosed properly.
        if (!unboundDecl->getGenericSignature()) {
          cs.TC.diagnose(unboundDecl, diag::circular_reference);
          return ErrorType::get(cs.getASTContext());
        }
        
        
        // Open up the generic type.
        cs.openGeneric(unboundDecl,
                       unboundDecl->getInnermostGenericParamTypes(),
                       unboundDecl->getGenericRequirements(),
                       /*skipProtocolSelfConstraint=*/false,
                       minOpeningDepth,
                       locator,
                       replacements);
        
        // Map the generic parameters to their corresponding type variables.
        llvm::SmallVector<TypeLoc, 4> arguments;
        for (auto gp : unboundDecl->getInnermostGenericParamTypes()) {
          assert(replacements.count(gp->getCanonicalType()) &&
                 "Missing generic parameter?");
          arguments.push_back(TypeLoc::withoutLoc(
                              replacements[gp->getCanonicalType()]));
        }
        
        return cs.TC.applyUnboundGenericArguments(unbound, SourceLoc(), cs.DC,
                                                  arguments,
                                                  /*isGenericSignature*/false,
                                                  /*resolver*/nullptr);
      }
      
      return type;
    }
  };
}

Type ConstraintSystem::openType(
       Type startingType,
       ConstraintLocatorBuilder locator,
       llvm::DenseMap<CanType, TypeVariableType *> &replacements,
       DeclContext *dc,
       bool skipProtocolSelfConstraint,
       unsigned minOpeningDepth) {
  GetTypeVariable getTypeVariable{*this, locator};

  ReplaceDependentTypes replaceDependentTypes(*this, dc,
                                              skipProtocolSelfConstraint,
                                              minOpeningDepth,
                                              locator,
                                              replacements, getTypeVariable);
  return startingType.transform(replaceDependentTypes);
}

bool ConstraintSystem::isArrayType(Type t) {
  t = t->getDesugaredType();
  
  // ArraySliceType<T> desugars to Array<T>.
  if (isa<ArraySliceType>(t.getPointer()))
    return true;
  if (auto boundStruct = dyn_cast<BoundGenericStructType>(t.getPointer())) {
    return boundStruct->getDecl() == TC.Context.getArrayDecl();
  }
  
  return false;
}

Optional<std::pair<Type, Type>> ConstraintSystem::isDictionaryType(Type type) {
  if (auto boundStruct = type->getAs<BoundGenericStructType>()) {
    if (boundStruct->getDecl() != TC.Context.getDictionaryDecl())
      return None;

    auto genericArgs = boundStruct->getGenericArgs();
    return std::make_pair(genericArgs[0], genericArgs[1]);
  }

  return None;
}

bool ConstraintSystem::isSetType(Type type) {
  if (auto boundStruct = type->getAs<BoundGenericStructType>()) {
    return boundStruct->getDecl() == TC.Context.getSetDecl();
  }

  return false;
}

Type ConstraintSystem::openBindingType(Type type, 
                                       ConstraintLocatorBuilder locator) {
  Type result = openType(type, locator);
  
  if (isArrayType(type)) {
    auto boundStruct = cast<BoundGenericStructType>(type.getPointer());
    if (auto replacement = getTypeChecker().getArraySliceType(
                             SourceLoc(), boundStruct->getGenericArgs()[0])) {
      return replacement;
    }
  }

  if (auto dict = isDictionaryType(type)) {
    if (auto replacement = getTypeChecker().getDictionaryType(
                             SourceLoc(), dict->first, dict->second))
      return replacement;
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
                                             bool wantRValue,
                                             bool retainParens) {
  if (wantRValue)
    type = type->getRValueType();

  if (retainParens) {
    if (auto parenTy = dyn_cast<ParenType>(type.getPointer())) {
      type = getFixedTypeRecursive(parenTy->getUnderlyingType(), typeVar,
                                   wantRValue, retainParens);
      return ParenType::get(getASTContext(), type);
    }
  }

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

void ConstraintSystem::recordOpenedTypes(
       ConstraintLocatorBuilder locator,
       const llvm::DenseMap<CanType, TypeVariableType *> &replacements) {
  if (replacements.empty())
    return;

  // If the last path element is an archetype or associated type, ignore it.
  SmallVector<LocatorPathElt, 2> pathElts;
  Expr *anchor = locator.getLocatorParts(pathElts);
  if (!pathElts.empty() &&
      (pathElts.back().getKind() == ConstraintLocator::Archetype ||
       pathElts.back().getKind() == ConstraintLocator::AssociatedType))
    return;

  // If the locator is empty, ignore it.
  if (!anchor && pathElts.empty())
    return;

  ConstraintLocator *locatorPtr = getConstraintLocator(locator);
  assert(locatorPtr && "No locator for opened types?");
  assert(std::find_if(OpenedTypes.begin(), OpenedTypes.end(),
                      [&](const std::pair<ConstraintLocator *,
                          ArrayRef<OpenedType>> &entry) {
                        return entry.first == locatorPtr;
                      }) == OpenedTypes.end() &&
         "already registered opened types for this locator");

  OpenedType* openedTypes
    = Allocator.Allocate<OpenedType>(replacements.size());
  std::copy(replacements.begin(), replacements.end(), openedTypes);
  OpenedTypes.push_back({ locatorPtr,
    llvm::makeArrayRef(openedTypes,
                       replacements.size()) });
}

std::pair<Type, Type>
ConstraintSystem::getTypeOfReference(ValueDecl *value,
                                     bool isTypeReference,
                                     bool isSpecialized,
                                     ConstraintLocatorBuilder locator,
                                     const DeclRefExpr *base) {
  llvm::DenseMap<CanType, TypeVariableType *> replacements;

  if (value->getDeclContext()->isTypeContext() && isa<FuncDecl>(value)) {
    // Unqualified lookup can find operator names within nominal types.
    auto func = cast<FuncDecl>(value);
    assert(func->isOperator() && "Lookup should only find operators");

    auto openedType =
      openType(func->getInterfaceType(), locator,
               replacements, func,
               false,
               value->getDeclContext()->getGenericTypeContextDepth());
    auto openedFnType = openedType->castTo<FunctionType>();
    
    // If this is a method whose result type is dynamic Self, replace
    // DynamicSelf with the actual object type.
    if (func->hasDynamicSelf()) {
      Type selfTy = openedFnType->getInput()->getRValueInstanceType();
      openedType = openedType->replaceCovariantResultType(
                     selfTy,
                     func->getNumParameterLists());
      openedFnType = openedType->castTo<FunctionType>();
    }

    // The 'Self' type must be bound to an archetype.
    // FIXME: We eventually want to loosen this constraint, to allow us
    // to find operator functions both in classes and in protocols to which
    // a class conforms (if there's a default implementation).
    addArchetypeConstraint(openedFnType->getInput()->getRValueInstanceType(),
                           getConstraintLocator(locator));

    // If we opened up any type variables, record the replacements.
    recordOpenedTypes(locator, replacements);

    // The reference implicitly binds 'self'.
    return { openedType, openedFnType->getResult() };
  }

  // If we have a type declaration, resolve it within the current context.
  if (auto typeDecl = dyn_cast<TypeDecl>(value)) {
    // Resolve the reference to this type declaration in our current context.
    auto type = getTypeChecker().resolveTypeInContext(typeDecl, DC,
                                                      TR_InExpression,
                                                      isSpecialized);
    if (!type)
      return { nullptr, nullptr };

    // Open the type.
    type = openType(type, locator, replacements,
                    value->getInnermostDeclContext(),
                    false,
                    value->getDeclContext()->getGenericTypeContextDepth());

    // If we opened up any type variables, record the replacements.
    recordOpenedTypes(locator, replacements);

    // If it's a type reference or it's a module type, we're done.
    if (isTypeReference || type->is<ModuleType>())
      return { type, type };

    // If it's a value reference, refer to the metatype.
    type = MetatypeType::get(type);
    return { type, type };
  }

  // Determine the type of the value, opening up that type if necessary.
  Type valueType = TC.getUnopenedTypeOfReference(value, Type(), DC, base,
                                                 /*wantInterfaceType=*/true);

  // If this is a let-param whose type is a type variable, this is an untyped
  // closure param that may be bound to an inout type later. References to the
  // param should have lvalue type instead. Express the relationship with a new
  // constraint.
  if (auto *param = dyn_cast<ParamDecl>(value)) {
    if (param->isLet() && valueType->is<TypeVariableType>()) {
      Type paramType = valueType;
      valueType = createTypeVariable(getConstraintLocator(locator),
                                     TVO_CanBindToLValue);
      addConstraint(ConstraintKind::BindParam, paramType, valueType,
                    getConstraintLocator(locator));
    }
  }

  // Adjust the type of the reference.
  valueType = openType(valueType, locator,
                       replacements,
                       value->getInnermostDeclContext(),
                       /*skipProtocolSelfConstraint=*/false,
                       value->getDeclContext()->getGenericTypeContextDepth());

  // If we opened up any type variables, record the replacements.
  recordOpenedTypes(locator, replacements);

  return { valueType, valueType };
}

void ConstraintSystem::openGeneric(
       DeclContext *dc,
       GenericSignature *signature,
       bool skipProtocolSelfConstraint,
       unsigned minOpeningDepth,
       ConstraintLocatorBuilder locator,
       llvm::DenseMap<CanType, TypeVariableType *> &replacements) {
  // Use the minimized constraints; we can re-derive solutions for all the
  // implied constraints.
  auto minimized =
    signature->getCanonicalManglingSignature(*DC->getParentModule());

  openGeneric(dc,
              minimized->getGenericParams(),
              minimized->getRequirements(),
              skipProtocolSelfConstraint,
              minOpeningDepth,
              locator,
              replacements);
}

void ConstraintSystem::openGeneric(
       DeclContext *dc,
       ArrayRef<GenericTypeParamType *> params,
       ArrayRef<Requirement> requirements,
       bool skipProtocolSelfConstraint,
       unsigned minOpeningDepth,
       ConstraintLocatorBuilder locator,
       llvm::DenseMap<CanType, TypeVariableType *> &replacements) {
  auto locatorPtr = getConstraintLocator(locator);

  // Create the type variables for the generic parameters.
  for (auto gp : params) {
    ArchetypeType *archetype = ArchetypeBuilder::mapTypeIntoContext(dc, gp)
                                 ->castTo<ArchetypeType>();
    auto typeVar = createTypeVariable(getConstraintLocator(
                                        locator.withPathElement(
                                          LocatorPathElt(archetype))),
                                      TVO_PrefersSubtypeBinding |
                                      TVO_MustBeMaterializable);
    replacements[gp->getCanonicalType()] = typeVar;

    if (gp->getDepth() < minOpeningDepth)
      addConstraint(ConstraintKind::Bind, typeVar, archetype, locatorPtr);
  }

  
  GetTypeVariable getTypeVariable{*this, locator};
  ReplaceDependentTypes replaceDependentTypes(*this, dc,
                                              skipProtocolSelfConstraint,
                                              minOpeningDepth,
                                              locator, replacements, 
                                              getTypeVariable);

  // Remember that any new constraints generated by opening this generic are
  // due to the opening.
  locatorPtr = getConstraintLocator(
                     locator.withPathElement(ConstraintLocator::OpenedGeneric));


  // Add the requirements as constraints.
  for (auto req : requirements) {
  switch (req.getKind()) {
    case RequirementKind::Conformance: {
      auto subjectTy = req.getFirstType().transform(replaceDependentTypes);
      auto proto = req.getSecondType()->castTo<ProtocolType>();
      // Determine whether this is the protocol 'Self' constraint we should
      // skip.
      if (skipProtocolSelfConstraint &&
          (proto->getDecl() == dc->getAsProtocolOrProtocolExtensionContext() ||
           proto->getDecl()
             == dc->getParent()->getAsProtocolOrProtocolExtensionContext())&&
          isProtocolSelfType(req.getFirstType())) {
        break;
      }

      addConstraint(ConstraintKind::ConformsTo, subjectTy, proto,
                    locatorPtr);
      break;
    }

    case RequirementKind::Superclass: {
      auto subjectTy = req.getFirstType().transform(replaceDependentTypes);
      auto boundTy = req.getSecondType().transform(replaceDependentTypes);
      addConstraint(ConstraintKind::Subtype, subjectTy, boundTy, locatorPtr);
      break;
    }

    case RequirementKind::SameType: {
      auto firstTy = req.getFirstType().transform(replaceDependentTypes);
      auto secondTy = req.getSecondType().transform(replaceDependentTypes);
      addConstraint(ConstraintKind::Bind, firstTy, secondTy, locatorPtr);
      break;
    }

    case RequirementKind::WitnessMarker:
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
static void addSelfConstraint(ConstraintSystem &cs, Type objectTy, Type selfTy,
                              ConstraintLocatorBuilder locator){
  assert(!selfTy->is<ProtocolType>());

  // Otherwise, use a subtype constraint for classes to cope with inheritance.
  if (selfTy->getClassOrBoundGenericClass()) {
    cs.addConstraint(ConstraintKind::Subtype, objectTy, selfTy,
                     cs.getConstraintLocator(locator));
    return;
  }

  // Otherwise, the types must be equivalent.
  cs.addConstraint(ConstraintKind::Equal, objectTy, selfTy,
                   cs.getConstraintLocator(locator));
}

Type ConstraintSystem::replaceSelfTypeInArchetype(ArchetypeType *archetype) {
  assert(SelfTypeVar && "Meaningless unless there is a type variable for Self");
  if (auto parent = archetype->getParent()) {
    // Replace Self in the parent archetype. If nothing changes, we're done.
    Type newParent = replaceSelfTypeInArchetype(parent);
    if (newParent->getAs<ArchetypeType>() == parent)
      return archetype;

    // We expect to get a type variable back.
    return getMemberType(newParent->castTo<TypeVariableType>(),
                         archetype->getAssocType(),
                         ConstraintLocatorBuilder(nullptr),
                         /*options=*/0);
  }

  // If the archetype is the same as for the 'Self' type variable,
  // return the 'Self' type variable.
  if (SelfTypeVar->getImpl().getArchetype() == archetype)
    return SelfTypeVar;

  return archetype;
}

/// Determine whether the given locator is for a witness or requirement.
static bool isRequirementOrWitness(const ConstraintLocatorBuilder &locator) {
  if (auto last = locator.last()) {
    return last->getKind() == ConstraintLocator::Requirement ||
    last->getKind() == ConstraintLocator::Witness;
  }

  return false;
}

std::pair<Type, Type>
ConstraintSystem::getTypeOfMemberReference(
  Type baseTy, ValueDecl *value,
  bool isTypeReference,
  bool isDynamicResult,
  ConstraintLocatorBuilder locator,
  const DeclRefExpr *base,
  llvm::DenseMap<CanType, TypeVariableType *> *replacementsPtr) {
  // Figure out the instance type used for the base.
  TypeVariableType *baseTypeVar = nullptr;
  Type baseObjTy = getFixedTypeRecursive(baseTy, baseTypeVar, 
                                         /*wantRValue=*/true);
  bool isInstance = true;
  if (auto baseMeta = baseObjTy->getAs<AnyMetatypeType>()) {
    baseObjTy = baseMeta->getInstanceType();
    isInstance = false;
  }

  // If the base is a module type, just use the type of the decl.
  if (baseObjTy->is<ModuleType>()) {
    return getTypeOfReference(value, isTypeReference, /*isSpecialized=*/false,
                              locator, base);
  }

  // Handle associated type lookup as a special case, horribly.
  // FIXME: This is an awful hack.
  if (auto assocType = dyn_cast<AssociatedTypeDecl>(value)) {
    // Error recovery path.
    if (baseObjTy->isOpenedExistential()) {
      Type memberTy = ErrorType::get(TC.Context);
      auto openedType = FunctionType::get(baseObjTy, memberTy);
      return { openedType, memberTy };
    }

    // Refer to a member of the archetype directly.
    if (auto archetype = baseObjTy->getAs<ArchetypeType>()) {
      Type memberTy = archetype->getNestedTypeValue(value->getName());
      if (!isTypeReference)
        memberTy = MetatypeType::get(memberTy);

      auto openedType = FunctionType::get(baseObjTy, memberTy);
      return { openedType, memberTy };
    }

    // If we have a nominal type that conforms to the protocol in which the
    // associated type resides, use the witness.
    if (!baseObjTy->isExistentialType() &&
        baseObjTy->getAnyNominal()) {
      auto proto = cast<ProtocolDecl>(assocType->getDeclContext());
      ProtocolConformance *conformance = nullptr;
      if (TC.conformsToProtocol(baseObjTy, proto, DC,
                                ConformanceCheckFlags::InExpression,
                                &conformance)) {
        auto memberTy = conformance->getTypeWitness(assocType, &TC)
          .getReplacement();
        if (!isTypeReference)
          memberTy = MetatypeType::get(memberTy);

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
  DeclContext *dc = value->getInnermostDeclContext();
  unsigned minOpeningDepth =
      value->getDeclContext()->getGenericTypeContextDepth();

  // Open the type of the generic function or member of a generic type.
  Type openedType;
  auto isClassBoundExistential = false;
  llvm::DenseMap<CanType, TypeVariableType *> localReplacements;
  auto &replacements = replacementsPtr ? *replacementsPtr : localReplacements;
  if (auto genericFn = value->getInterfaceType()->getAs<GenericFunctionType>()){
    openedType = openType(genericFn, locator, replacements, dc,
                          /*skipProtocolSelfConstraint=*/true,
                          minOpeningDepth);
  } else {
    openedType = TC.getUnopenedTypeOfReference(value, baseTy, DC, base,
                                               /*wantInterfaceType=*/true);

    Type selfTy;
    if (auto sig = dc->getGenericSignatureOfContext()) {

      // Open up the generic parameter list for the container.
      openGeneric(dc, sig, /*skipProtocolSelfConstraint=*/true, minOpeningDepth,
                  locator, replacements);

      // Open up the type of the member.
      openedType = openType(openedType, locator, replacements, nullptr,
                            /*skipProtocolSelfConstraint=*/false,
                            minOpeningDepth);

      // Determine the object type of 'self'.
      auto nominal = value->getDeclContext()
          ->getAsNominalTypeOrNominalTypeExtensionContext();
      
      // We want to track if the generic context is represented by a
      // class-bound existential so we won't inappropriately wrap the
      // self type in an inout later on.
      if (auto metatype = nominal->getType()->getAs<AnyMetatypeType>()) {
        isClassBoundExistential = metatype->getInstanceType()->
                                            isClassExistentialType();
      }
      
      if (dc->getAsProtocolOrProtocolExtensionContext()) {
        // Retrieve the type variable for 'Self'.
        selfTy = replacements[dc->getProtocolSelf()->getDeclaredType()
                                ->getCanonicalType()];
      } else {
        // Open the nominal type.
        selfTy = openType(nominal->getDeclaredInterfaceType(), locator,
                          replacements);
      }
    } else {
      selfTy = value->getDeclContext()->getDeclaredTypeOfContext();
    }
    
    // If we have a type reference, look through the metatype.
    if (isTypeReference)
      openedType = openedType->castTo<AnyMetatypeType>()->getInstanceType();

    // If we're not coming from something function-like, prepend the type
    // for 'self' to the type.
    if (!isa<AbstractFunctionDecl>(value) && !isa<EnumElementDecl>(value)) {
      // If self is a struct, properly qualify it based on our base
      // qualification.  If we have an lvalue coming in, we expect an inout.
      if (!isClassBoundExistential &&
          !selfTy->hasReferenceSemantics() &&
          baseTy->is<LValueType>())
        selfTy = InOutType::get(selfTy);

      openedType = FunctionType::get(selfTy, openedType);
    }
  }

  // If this is a method whose result type has a dynamic Self return, replace
  // DynamicSelf with the actual object type.
  if (auto func = dyn_cast<FuncDecl>(value)) {
    if (func->hasDynamicSelf() ||
        (baseObjTy->isExistentialType() &&
         func->hasArchetypeSelf())) {
      openedType = openedType->replaceCovariantResultType(
                     baseObjTy,
                     func->getNumParameterLists());
    }
  }
  // If this is an initializer, replace the result type with the base
  // object type.
  else if (auto ctor = dyn_cast<ConstructorDecl>(value)) {
    auto resultTy = baseObjTy;
    if (ctor->getFailability() != OTK_None)
      resultTy = OptionalType::get(ctor->getFailability(), resultTy);

    openedType = openedType->replaceCovariantResultType(
                     resultTy,
                     /*uncurryLevel=*/ 2,
                     /*preserveOptionality=*/ false);
  }

  // If we are looking at a member of an existential, open the existential.
  Type baseOpenedTy = baseObjTy;

  if (baseObjTy->isExistentialType()) {
    ArchetypeType *openedArchetype = ArchetypeType::getOpened(baseObjTy);
    OpenedExistentialTypes.push_back({ getConstraintLocator(locator),
                                       openedArchetype });
    baseOpenedTy = openedArchetype;
  }

  // Constrain the 'self' object type.
  auto openedFnType = openedType->castTo<FunctionType>();
  Type selfObjTy = openedFnType->getInput()->getRValueInstanceType();
  if (value->getDeclContext()->getAsProtocolOrProtocolExtensionContext()) {
    // For a protocol, substitute the base object directly. We don't need a
    // conformance constraint because we wouldn't have found the declaration
    // if it didn't conform.
    addConstraint(ConstraintKind::Equal, baseOpenedTy, selfObjTy,
                  getConstraintLocator(locator));
  } else if (!isDynamicResult) {
    addSelfConstraint(*this, baseOpenedTy, selfObjTy, locator);
  }

  // Compute the type of the reference.
  Type type;
  if (auto subscript = dyn_cast<SubscriptDecl>(value)) {
    // For a subscript, turn the element type into an (@unchecked)
    // optional or lvalue, depending on whether the result type is
    // optional/dynamic, is settable, or is not.
    auto fnType = openedFnType->getResult()->castTo<FunctionType>();
    auto elementTy = fnType->getResult();
    if (!isRequirementOrWitness(locator)) {
      if (subscript->getAttrs().hasAttribute<OptionalAttr>())
        elementTy = OptionalType::get(elementTy->getRValueType());
      else if (isDynamicResult) {
        elementTy = ImplicitlyUnwrappedOptionalType::get(
                      elementTy->getRValueType());
      }
    }

    type = FunctionType::get(fnType->getInput(), elementTy);
  } else if (isa<ProtocolDecl>(value->getDeclContext()) &&
             isa<AssociatedTypeDecl>(value)) {
    // When we have an associated type, the base type conforms to the
    // given protocol, so use the type witness directly.
    // FIXME: Diagnose existentials properly.
    auto proto = cast<ProtocolDecl>(value->getDeclContext());
    auto assocType = cast<AssociatedTypeDecl>(value);

    type = openedFnType->getResult();
    if (baseOpenedTy->is<ArchetypeType>()) {
      // For an archetype, we substitute the base object for the base.
      // FIXME: Feels like a total hack.
    } else if (!baseOpenedTy->isExistentialType() &&
               !baseOpenedTy->is<ArchetypeType>()) {
      ProtocolConformance *conformance = nullptr;
      if (TC.conformsToProtocol(baseOpenedTy, proto, DC,
                                ConformanceCheckFlags::InExpression,
                                &conformance)) {
        type = conformance->getTypeWitness(assocType, &TC).getReplacement();
      }
    }
  } else if (!value->isInstanceMember() || isInstance) {
    // For a constructor, enum element, static method, static property,
    // or an instance method referenced through an instance, we've consumed the
    // curried 'self' already. For a type, strip off the 'self' we artificially
    // added.
    type = openedFnType->getResult();
  } else if (isDynamicResult && isa<AbstractFunctionDecl>(value)) {
    // For a dynamic result referring to an instance function through
    // an object of metatype type, replace the 'Self' parameter with
    // a AnyObject member.
    Type anyObjectTy = TC.getProtocol(SourceLoc(),
                                      KnownProtocolKind::AnyObject)
                                          ->getDeclaredTypeOfContext();

    type = openedFnType->replaceSelfParameterType(anyObjectTy);
  } else {
    // For an unbound instance method reference, replace the 'Self'
    // parameter with the base type.
    type = openedFnType->replaceSelfParameterType(baseObjTy);
  }

  // If we opened up any type variables, record the replacements.
  recordOpenedTypes(locator, replacements);

  return { openedType, type };
}

void ConstraintSystem::addOverloadSet(Type boundType,
                                      ArrayRef<OverloadChoice> choices,
                                      ConstraintLocator *locator,
                                      OverloadChoice *favoredChoice) {
  assert(!choices.empty() && "Empty overload set");

  SmallVector<Constraint *, 4> overloads;
  
  // As we do for other favored constraints, if a favored overload has been
  // specified, let it be the first term in the disjunction.
  if (favoredChoice) {
    auto bindOverloadConstraint =
        Constraint::createBindOverload(*this,
                                       boundType,
                                       *favoredChoice,
                                       locator);
    
    bindOverloadConstraint->setFavored();
    
    overloads.push_back(bindOverloadConstraint);
  }
  
  for (auto choice : choices) {
    if (favoredChoice && (favoredChoice == &choice))
      continue;
    
    overloads.push_back(Constraint::createBindOverload(*this, boundType, choice,
                                                       locator));
  }
  
  auto disjunction = Constraint::createDisjunction(*this, overloads, locator);
  
  if (favoredChoice)
    disjunction->setFavored();
  
  addConstraint(disjunction);
}

void ConstraintSystem::resolveOverload(ConstraintLocator *locator,
                                       Type boundType,
                                       OverloadChoice choice) {
  // Determine the type to which we'll bind the overload set's type.
  Type refType;
  Type openedFullType;
  switch (choice.getKind()) {
  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::Decl:
  case OverloadChoiceKind::DeclViaDynamic:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
  case OverloadChoiceKind::TypeDecl: {
    bool isTypeReference = choice.getKind() == OverloadChoiceKind::TypeDecl;
    bool isDynamicResult
      = choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
    // Retrieve the type of a reference to the specific declaration choice.
    if (choice.getBaseType()) {
      auto getDotBase = [](const Expr *E) -> const DeclRefExpr * {
        if (E == nullptr) return nullptr;
        switch (E->getKind()) {
        case ExprKind::MemberRef: {
          auto Base = cast<MemberRefExpr>(E)->getBase();
          return dyn_cast<const DeclRefExpr>(Base);
        }
        case ExprKind::UnresolvedDot: {
          auto Base = cast<UnresolvedDotExpr>(E)->getBase();
          return dyn_cast<const DeclRefExpr>(Base);
        }
        default:
          return nullptr;
        }
      };
      auto anchor = locator ? locator->getAnchor() : nullptr;
      auto base = getDotBase(anchor);
      std::tie(openedFullType, refType)
        = getTypeOfMemberReference(choice.getBaseType(), choice.getDecl(),
                                   isTypeReference, isDynamicResult,
                                   locator, base, nullptr);
    } else {
      std::tie(openedFullType, refType)
        = getTypeOfReference(choice.getDecl(), isTypeReference,
                             choice.isSpecialized(), locator);
    }

    if (!isRequirementOrWitness(locator) &&
        choice.getDecl()->getAttrs().hasAttribute<OptionalAttr>() &&
        !isa<SubscriptDecl>(choice.getDecl())) {
      // For a non-subscript declaration that is an optional
      // requirement in a protocol, strip off the lvalue-ness (FIXME:
      // one cannot assign to such declarations for now) and make a
      // reference to that declaration be optional.
      //
      // Subscript declarations are handled within
      // getTypeOfMemberReference(); their result types are optional.
      refType = OptionalType::get(refType->getRValueType());
    } 
    // For a non-subscript declaration found via dynamic lookup, strip
    // off the lvalue-ness (FIXME: as a temporary hack. We eventually
    // want this to work) and make a reference to that declaration be
    // an implicitly unwrapped optional.
    //
    // Subscript declarations are handled within
    // getTypeOfMemberReference(); their result types are unchecked
    // optional.
    else if (isDynamicResult && !isa<SubscriptDecl>(choice.getDecl())) {    
      refType = ImplicitlyUnwrappedOptionalType::get(refType->getRValueType());
    } 

    // If the declaration is unavailable, note that in the score.
    if (choice.getDecl()->getAttrs().isUnavailable(getASTContext())) {
      increaseScore(SK_Unavailable);
    }

    break;
  }

  case OverloadChoiceKind::BaseType:
    refType = choice.getBaseType();
    break;

  case OverloadChoiceKind::TupleIndex:
    if (auto lvalueTy = choice.getBaseType()->getAs<LValueType>()) {
      // When the base of a tuple lvalue, the member is always an lvalue.
      auto tuple = lvalueTy->getObjectType()->castTo<TupleType>();
      refType = tuple->getElementType(choice.getTupleIndex())->getRValueType();
      refType = LValueType::get(refType);
    } else {
      // When the base is a tuple rvalue, the member is always an rvalue.
      auto tuple = choice.getBaseType()->castTo<TupleType>();
      refType = tuple->getElementType(choice.getTupleIndex())->getRValueType();
    }
    break;
  }
  
  // If we have a type variable for the 'Self' of a protocol
  // requirement that's being opened, and the resulting type has an
  // archetype in it, replace the 'Self' archetype with the
  // corresponding type variable.
  // FIXME: See the comment for SelfTypeVar for information about this hack.
  if (SelfTypeVar && refType->hasArchetype()) {
    refType = refType.transform([&](Type type) -> Type {
        if (auto archetype = type->getAs<ArchetypeType>()) {
          return replaceSelfTypeInArchetype(archetype);
        }
        return type;
      });
  }
  assert(!refType->hasTypeParameter() && "Cannot have a dependent type here");
  
  // If we're binding to an init member, the 'throws' need to line up between
  // the bound and reference types.
  if (choice.isDecl()) {
    auto decl = choice.getDecl();
    if (auto CD = dyn_cast<ConstructorDecl>(decl)) {
      auto boundFunctionType = boundType->getAs<AnyFunctionType>();
        
      if (boundFunctionType &&
          CD->isBodyThrowing() != boundFunctionType->throws()) {
        boundType = FunctionType::get(boundFunctionType->getInput(),
                                      boundFunctionType->getResult(),
                                      boundFunctionType->getExtInfo().
                                                          withThrows());
      }
    }
  }

  // Add the type binding constraint.
  addConstraint(ConstraintKind::Bind, boundType, refType, locator);

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

/// Given that we're accessing a member of an ImplicitlyUnwrappedOptional<T>, is
/// the DC one of the special cases where we should not instead look at T?
static bool isPrivilegedAccessToImplicitlyUnwrappedOptional(DeclContext *DC,
                                                  NominalTypeDecl *D) {
  assert(D == DC->getASTContext().getImplicitlyUnwrappedOptionalDecl());

  // Walk up through the chain of current contexts.
  for (; ; DC = DC->getParent()) {
    assert(DC && "ran out of contexts before finding a module scope?");

    // Look through local contexts.
    if (DC->isLocalContext()) {
      continue;

    // If we're in a type context that's defining or extending
    // ImplicitlyUnwrappedOptional<T>, we're privileged.
    } else if (DC->isTypeContext()) {
      if (DC->getAsNominalTypeOrNominalTypeExtensionContext() == D)
        return true;

    // Otherwise, we're privileged if we're within the same file that
    // defines ImplicitlyUnwrappedOptional<T>.
    } else {
      assert(DC->isModuleScopeContext());
      return (DC == D->getModuleScopeContext());
    }
  }
}

Type ConstraintSystem::lookThroughImplicitlyUnwrappedOptionalType(Type type) {
  if (auto boundTy = type->getAs<BoundGenericEnumType>()) {
    auto boundDecl = boundTy->getDecl();
    if (boundDecl == TC.Context.getImplicitlyUnwrappedOptionalDecl() &&
        !isPrivilegedAccessToImplicitlyUnwrappedOptional(DC, boundDecl))
      return boundTy->getGenericArgs()[0];
  }
  return Type();
}

Type ConstraintSystem::simplifyType(Type type,
       llvm::SmallPtrSet<TypeVariableType *, 16> &substituting) {
  return type.transform([&](Type type) -> Type {
    if (auto tvt = dyn_cast<TypeVariableType>(type.getPointer())) {
      tvt = getRepresentative(tvt);
      if (auto fixed = getFixedType(tvt)) {
        if (substituting.insert(tvt).second) {
          auto result = simplifyType(fixed, substituting);
          substituting.erase(tvt);
          return result;
        }
      }
      
      return tvt;
    }

    // If this is a FunctionType and we inferred new function attributes, apply
    // them.
    if (auto ft = dyn_cast<FunctionType>(type.getPointer())) {
      auto it = extraFunctionAttrs.find(ft);
      if (it != extraFunctionAttrs.end()) {
        auto extInfo = ft->getExtInfo();
        if (it->second.isNoEscape())
          extInfo = extInfo.withNoEscape();
        if (it->second.isNoReturn())
          extInfo = extInfo.withIsNoReturn();
        if (it->second.throws())
          extInfo = extInfo.withThrows();
        return FunctionType::get(ft->getInput(), ft->getResult(), extInfo);
      }
    }
    

    return type;
  });
}

Type Solution::simplifyType(TypeChecker &tc, Type type) const {
  return type.transform([&](Type type) -> Type {
    if (auto tvt = dyn_cast<TypeVariableType>(type.getPointer())) {
      auto known = typeBindings.find(tvt);
      assert(known != typeBindings.end());
      return known->second;
    }

    // If this is a FunctionType and we inferred new function attributes, apply
    // them.
    if (auto ft = dyn_cast<FunctionType>(type.getPointer())) {
      auto &CS = getConstraintSystem();
      auto it = CS.extraFunctionAttrs.find(ft);
      if (it != CS.extraFunctionAttrs.end()) {
        auto extInfo = ft->getExtInfo();
        if (it->second.isNoEscape())
          extInfo = extInfo.withNoEscape();
        if (it->second.isNoReturn())
          extInfo = extInfo.withIsNoReturn();
        if (it->second.throws())
          extInfo = extInfo.withThrows();
        return FunctionType::get(simplifyType(tc, ft->getInput()),
                                 simplifyType(tc, ft->getResult()),
                                 extInfo);
      }
    }

    return type;
  });
}
