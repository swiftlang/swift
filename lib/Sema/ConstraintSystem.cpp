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
    // FIXME: Use ObjCSelector here!
    selector = func->getObjCSelector().getString(buffer);
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
  } else if (auto asd = dyn_cast<AbstractStorageDecl>(decl)) {
    // Handle properties and subscripts. Only the getter matters.
    selector = asd->getObjCGetterSelector().getString(buffer);
    type = asd->getType();
  } else if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    // Handle constructors.
    selector = ctor->getObjCSelector().getString(buffer);
    type = decl->getType()->castTo<AnyFunctionType>()->getResult();
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

LookupResult &ConstraintSystem::lookupMember(Type base, DeclName name) {
  base = base->getCanonicalType();

  // Check whether we've already performed this lookup.
  auto knownMember = MemberLookups.find({base, name});
  if (knownMember != MemberLookups.end())
    return *knownMember->second;

  // Lookup the member.
  MemberLookups[{base, name}] = None;
  auto lookup = TC.lookupMember(base, name, DC,
                                /*knownPrivate=*/isa<AbstractFunctionDecl>(DC));
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
  llvm::DenseSet<std::pair<unsigned, CanType>> known;
  llvm::StringMap<unsigned> selectors;
  result->filter([&](ValueDecl *decl) -> bool {
    if (decl->isInvalid())
      return false;

    return known.insert(getDynamicResultSignature(decl, selectors)).second;
  });

  return *result;
}

ArrayRef<Type> ConstraintSystem::
getAlternativeLiteralTypes(KnownProtocolKind kind) {
  unsigned index;

  switch (kind) {
#define PROTOCOL(Protocol) \
  case KnownProtocolKind::Protocol: llvm_unreachable("Not a literal protocol");
#define LITERAL_CONVERTIBLE_PROTOCOL(Protocol, Description)
#include "swift/AST/KnownProtocols.def"

  case KnownProtocolKind::ArrayLiteralConvertible:     index = 0; break;
  case KnownProtocolKind::CharacterLiteralConvertible: index = 1; break;
  case KnownProtocolKind::DictionaryLiteralConvertible:index = 2; break;
  case KnownProtocolKind::ExtendedGraphemeClusterLiteralConvertible: index = 3;
    break;
  case KnownProtocolKind::FloatLiteralConvertible: index = 4; break;
  case KnownProtocolKind::IntegerLiteralConvertible: index = 5; break;
  case KnownProtocolKind::StringInterpolationConvertible: index = 6; break;
  case KnownProtocolKind::StringLiteralConvertible: index = 7; break;
  case KnownProtocolKind::NilLiteralConvertible: index = 8; break;
  case KnownProtocolKind::BooleanLiteralConvertible: index = 9; break;
  case KnownProtocolKind::UnicodeScalarLiteralConvertible: index = 10; break;
  }

  // If we already looked for alternative literal types, return those results.
  if (AlternativeLiteralTypes[index])
    return *AlternativeLiteralTypes[index];

  SmallVector<Type, 4> types;

  // If the default literal type is bridged to a class type, add the class type.
  if (auto proto = TC.Context.getProtocol(kind)) {
    if (auto defaultType = TC.getDefaultType(proto, DC)) {
      if (auto bridgedClassType = TC.getBridgedToObjC(DC, true, defaultType)) {
        types.push_back(bridgedClassType);
      }
    }
  }

  // Some literal kinds are related.
  switch (kind) {
#define PROTOCOL(Protocol) \
  case KnownProtocolKind::Protocol: llvm_unreachable("Not a literal protocol");
#define LITERAL_CONVERTIBLE_PROTOCOL(Protocol, Description)
#include "swift/AST/KnownProtocols.def"

  case KnownProtocolKind::ArrayLiteralConvertible:
  case KnownProtocolKind::DictionaryLiteralConvertible:
    break;

  case KnownProtocolKind::CharacterLiteralConvertible:
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
    DependentTypeOpener *Opener;

  public:
    GetTypeVariable(ConstraintSystem &cs,
                    ConstraintLocatorBuilder &locator,
                    DependentTypeOpener *opener)
      : CS(cs), CG(CS.getConstraintGraph()), Locator(locator), Opener(opener) {}

    TypeVariableType *operator()(Type base, AssociatedTypeDecl *member) {
      // FIXME: Premature associated type -> identifier mapping. We should
      // retain the associated type throughout.
      auto baseTypeVar = base->castTo<TypeVariableType>();
      return CG.getMemberType(baseTypeVar, member->getName(),
                              [&]() -> TypeVariableType* {
        auto implArchetype = baseTypeVar->getImpl().getArchetype();
        if (!implArchetype) {
          return baseTypeVar;
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
        // Determine whether we should bind the new type variable as a
        // member of the base type variable, or let it float.
        Type replacementType;
        bool shouldBindMember = true;
        if (Opener) {
          shouldBindMember = Opener->shouldBindAssociatedType(base, baseTypeVar,
                                                              member, 
                                                              memberTypeVar,
                                                              replacementType);
        }

        // Bind the member's type variable as a type member of the base,
        // if needed.
        if (shouldBindMember) {
          CS.addConstraint(Constraint::create(CS, ConstraintKind::TypeMember,
                                              baseTypeVar, memberTypeVar, 
                                              member->getName(), locator));
        }

        // If we have a replacement type, bind the member's type
        // variable to it.
        if (replacementType)
          CS.addConstraint(ConstraintKind::Bind, memberTypeVar,
                           replacementType, locator);
                                
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
    DependentTypeOpener *opener;
    ConstraintLocatorBuilder &locator;
    llvm::DenseMap<CanType, TypeVariableType *> &replacements;
    GetTypeVariable &getTypeVariable;

  public:
    ReplaceDependentTypes(
        ConstraintSystem &cs,
        DeclContext *dc,
        bool skipProtocolSelfConstraint,
        DependentTypeOpener *opener,
        ConstraintLocatorBuilder &locator,
        llvm::DenseMap<CanType, TypeVariableType *> &replacements,
        GetTypeVariable &getTypeVariable)
      : cs(cs), dc(dc), skipProtocolSelfConstraint(skipProtocolSelfConstraint),
        opener(opener), locator(locator), replacements(replacements), 
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
        if (opener) {
          // If we have a mapping for this type parameter, there's nothing else to do.
          if (Type replacement = opener->mapGenericTypeParamType(genericParam)){
            return replacement;
          }
        }

        auto known = replacements.find(genericParam->getCanonicalType());
        
        if (known == replacements.end())
          return cs.createTypeVariable(nullptr, TVO_PrefersSubtypeBinding);
        
        return known->second;
      }

      // Replace a dependent member with a fresh type variable and make it a
      // member of its base type.
      if (auto dependentMember = type->getAs<DependentMemberType>()) {
        if (opener) {
          // If we have a mapping for this type parameter, there's nothing else to do.
          if (Type replacement
                = opener->mapDependentMemberType(dependentMember)) {
            return replacement;
          }
        }

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
                       genericFn->getGenericParams(),
                       genericFn->getRequirements(),
                       skipProtocolSelfConstraint,
                       opener,
                       locator,
                       replacements);

        // Transform the input and output types.
        Type inputTy = genericFn->getInput().transform(*this);
        if (!inputTy)
          return Type();

        // If we didn't get something that structurally has the parentheses,
        // add them back.
        if (!isa<TupleType>(inputTy.getPointer()) &&
            !isa<ParenType>(inputTy.getPointer())) {
          inputTy = ParenType::get(cs.getASTContext(), inputTy);
        }

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

        // Open up the generic type.
        cs.openGeneric(unboundDecl,
                       unboundDecl->getGenericParamTypes(),
                       unboundDecl->getGenericRequirements(),
                       /*skipProtocolSelfConstraint=*/false,
                       opener,
                       locator,
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
       ConstraintLocatorBuilder locator,
       llvm::DenseMap<CanType, TypeVariableType *> &replacements,
       DeclContext *dc,
       bool skipProtocolSelfConstraint,
       DependentTypeOpener *opener) {
  GetTypeVariable getTypeVariable{*this, locator, opener};

  ReplaceDependentTypes replaceDependentTypes(*this, dc,
                                              skipProtocolSelfConstraint,
                                              opener,
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
                                       ConstraintLocatorBuilder locator,
                                       DeclContext *dc) {
  Type result = openType(type, locator, dc);
  
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
                                     DependentTypeOpener *opener) {
  llvm::DenseMap<CanType, TypeVariableType *> replacements;

  if (value->getDeclContext()->isTypeContext() && isa<FuncDecl>(value)) {
    // Unqualified lookup can find operator names within nominal types.
    auto func = cast<FuncDecl>(value);
    assert(func->isOperator() && "Lookup should only find operators");

    auto openedType = openType(func->getInterfaceType(), locator,
                               replacements, func, false, opener);
    auto openedFnType = openedType->castTo<FunctionType>();
    
    // If this is a method whose result type is dynamic Self, replace
    // DynamicSelf with the actual object type.
    if (func->hasDynamicSelf()) {
      openedType = openedType.transform([&](Type type) {
        if (type->is<DynamicSelfType>())
          return openedFnType->getInput()->getRValueInstanceType();
        return type;
      });
      
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
                                                      isSpecialized);
    if (!type)
      return { nullptr, nullptr };

    // Open the type.
    type = openType(type, locator, replacements,
                    value->getInnermostDeclContext(), false, opener);

    // If we opened up any type variables, record the replacements.
    recordOpenedTypes(locator, replacements);

    // If it's a type reference, we're done.
    if (isTypeReference)
      return { type, type };

    // If it's a value reference, refer to the metatype.
    type = MetatypeType::get(type);
    return { type, type };
  }

  // Determine the type of the value, opening up that type if necessary.
  Type valueType = TC.getUnopenedTypeOfReference(value, Type(), DC,
                                                 /*wantInterfaceType=*/true);

  // Adjust the type of the reference.
  valueType = openType(valueType, locator,
                       replacements,
                       value->getPotentialGenericDeclContext(),
                       /*skipProtocolSelfConstraint=*/false,
                       opener);

  // If we opened up any type variables, record the replacements.
  recordOpenedTypes(locator, replacements);

  return { valueType, valueType };
}

void ConstraintSystem::openGeneric(
       DeclContext *dc,
       ArrayRef<GenericTypeParamType *> params,
       ArrayRef<Requirement> requirements,
       bool skipProtocolSelfConstraint,
       DependentTypeOpener *opener,
       ConstraintLocatorBuilder locator,
       llvm::DenseMap<CanType, TypeVariableType *> &replacements) {
  auto locatorPtr = getConstraintLocator(locator);

  // Create the type variables for the generic parameters.
  for (auto gp : params) {
    // If we have a mapping for this type parameter, there's nothing else to do.
    if (opener && opener->mapGenericTypeParamType(gp)) {
      continue;
    }

    ArchetypeType *archetype = ArchetypeBuilder::mapTypeIntoContext(dc, gp)
                                 ->castTo<ArchetypeType>();
    auto typeVar = createTypeVariable(getConstraintLocator(
                                        locator.withPathElement(
                                          LocatorPathElt(archetype))),
                                      TVO_PrefersSubtypeBinding);
    replacements[gp->getCanonicalType()] = typeVar;

    // Note that we opened a generic parameter to a type variable.
    if (opener) {
      Type replacementType;
      opener->openedGenericParameter(gp, typeVar, replacementType);

      if (replacementType)
        addConstraint(ConstraintKind::Bind, typeVar, replacementType,
                      locatorPtr);
    }
  }

  GetTypeVariable getTypeVariable{*this, locator, opener};
  ReplaceDependentTypes replaceDependentTypes(*this, dc,
                                              skipProtocolSelfConstraint,
                                              opener, locator, replacements, 
                                              getTypeVariable);

  // Add the requirements as constraints.
  for (auto req : requirements) {
  switch (req.getKind()) {
    case RequirementKind::Conformance: {
      auto subjectTy = req.getFirstType().transform(replaceDependentTypes);
      if (auto proto = req.getSecondType()->getAs<ProtocolType>()) {
        if (!skipProtocolSelfConstraint ||
            !(dc->isProtocolOrProtocolExtensionContext() ||
              dc->getParent()->isProtocolOrProtocolExtensionContext()) ||
            !isProtocolSelfType(req.getFirstType())) {
          addConstraint(ConstraintKind::ConformsTo, subjectTy, proto,
                        locatorPtr);
        }
      } else {
        auto boundTy = req.getSecondType().transform(replaceDependentTypes);
        addConstraint(ConstraintKind::Subtype, subjectTy, boundTy, locatorPtr);
      }
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
  // When referencing a protocol member, we need the object type to be usable
  // as the Self type of the protocol, which covers anything that conforms to
  // the protocol as well as existentials that include that protocol.
  if (selfTy->is<ProtocolType>()) {
    cs.addConstraint(ConstraintKind::SelfObjectOfProtocol, objectTy, selfTy,
                     cs.getConstraintLocator(locator));
    return;
  }

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

/// Rebuilds the given 'self' type using the given object type as the
/// replacement for the object type of self.
static Type rebuildSelfTypeWithObjectType(Type selfTy, Type objectTy) {
  auto existingObjectTy = selfTy->getRValueInstanceType();
  return selfTy.transform([=](Type type) -> Type {
    if (type->isEqual(existingObjectTy))
      return objectTy;
    return type;
  });
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

std::pair<Type, Type>
ConstraintSystem::getTypeOfMemberReference(Type baseTy, ValueDecl *value,
                                           bool isTypeReference,
                                           bool isDynamicResult,
                                           ConstraintLocatorBuilder locator,
                                           DependentTypeOpener *opener) {
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
                              locator, opener);
  }

  // Handle associated type lookup as a special case, horribly.
  // FIXME: This is an awful hack.
  if (auto assocType = dyn_cast<AssociatedTypeDecl>(value)) {
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
      if (TC.conformsToProtocol(baseObjTy, proto, DC, true, &conformance) &&
          conformance->hasTypeWitness(assocType, nullptr)) {
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
  DeclContext *dc = value->getPotentialGenericDeclContext();

  // Open the type of the generic function or member of a generic type.
  Type openedType;
  llvm::DenseMap<CanType, TypeVariableType *> replacements;
  if (auto genericFn = value->getInterfaceType()->getAs<GenericFunctionType>()){
    openedType = openType(genericFn, locator, replacements, dc,
                          /*skipProtocolSelfConstraint=*/true, opener);
  } else {
    openedType = TC.getUnopenedTypeOfReference(value, baseTy, DC,
                                               /*wantInterfaceType=*/true);

    Type selfTy;
    if (auto sig = dc->getGenericSignatureOfContext()) {
      // Open up the generic parameter list for the container.
      openGeneric(dc, sig->getGenericParams(), sig->getRequirements(),
                  /*skipProtocolSelfConstraint=*/true,
                  opener, locator, replacements);

      // Open up the type of the member.
      openedType = openType(openedType, locator, replacements, nullptr, false, 
                            opener);

      // Determine the object type of 'self'.
      auto nominal = value->getDeclContext()->getDeclaredTypeOfContext()
                       ->getAnyNominal();
      if (dc->isProtocolOrProtocolExtensionContext()) {
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
      if (!selfTy->hasReferenceSemantics() && baseTy->is<LValueType>())
        selfTy = InOutType::get(selfTy);

      openedType = FunctionType::get(selfTy, openedType);
    }
  }

  // If this is a method whose result type has a dynamic Self return, replace
  // DynamicSelf with the actual object type.
  bool hasDynamicSelf = false;
  if (auto func = dyn_cast<FuncDecl>(value)) {
    if (func->hasDynamicSelf()) {
      hasDynamicSelf = true;
      openedType = openedType.transform([&](Type type) {
          if (type->is<DynamicSelfType>())
            return baseObjTy;
          return type;
        });
    }
  }
  // If this is an initializer, replace the result type with the base
  // object type.
  else if (auto ctor = dyn_cast<ConstructorDecl>(value)) {
    auto outerFnType = openedType->castTo<FunctionType>();
    auto innerFnType = outerFnType->getResult()->castTo<FunctionType>();
    auto resultTy = baseObjTy;
    if (ctor->getFailability() != OTK_None)
      resultTy = OptionalType::get(ctor->getFailability(), resultTy);

    openedType = FunctionType::get(innerFnType->getInput(), resultTy,
                                   innerFnType->getExtInfo());
    openedType = FunctionType::get(outerFnType->getInput(), openedType,
                                   outerFnType->getExtInfo());
  }

  // If we are looking at a member of an existential, open the existential.
  if (baseObjTy->isExistentialType()) {
    ArchetypeType *openedArchetype = ArchetypeType::getOpened(baseObjTy);
    OpenedExistentialTypes.push_back({ getConstraintLocator(locator),
                                       openedArchetype });
    baseObjTy = openedArchetype;
  }

  // Constrain the 'self' object type.
  auto openedFnType = openedType->castTo<FunctionType>();
  Type selfObjTy = openedFnType->getInput()->getRValueInstanceType();
  if (value->getDeclContext()->isProtocolOrProtocolExtensionContext()) {
    // For a protocol, substitute the base object directly. We don't need a
    // conformance constraint because we wouldn't have found the declaration
    // if it didn't conform.
    addConstraint(ConstraintKind::Equal, baseObjTy, selfObjTy,
                  getConstraintLocator(locator));
  } else if (!isDynamicResult) {
    addSelfConstraint(*this, baseObjTy, selfObjTy, locator);
  }

  // Compute the type of the reference.
  Type type;
  if (auto subscript = dyn_cast<SubscriptDecl>(value)) {
    // For a subscript, turn the element type into an (@unchecked)
    // optional or lvalue, depending on whether the result type is
    // optional/dynamic, is settable, or is not.
    auto fnType = openedFnType->getResult()->castTo<FunctionType>();
    auto elementTy = fnType->getResult();
    if (subscript->getAttrs().hasAttribute<OptionalAttr>())
      elementTy = OptionalType::get(elementTy->getRValueType());
    else if (isDynamicResult)
      elementTy = ImplicitlyUnwrappedOptionalType::get(elementTy->getRValueType());

    type = FunctionType::get(fnType->getInput(), elementTy);
  } else if (isa<ProtocolDecl>(value->getDeclContext()) &&
             isa<AssociatedTypeDecl>(value)) {
    // When we have an associated type, the base type conforms to the
    // given protocol, so use the type witness directly.
    // FIXME: Diagnose existentials properly.
    auto proto = cast<ProtocolDecl>(value->getDeclContext());
    auto assocType = cast<AssociatedTypeDecl>(value);

    type = openedFnType->getResult();
    if (baseObjTy->is<ArchetypeType>()) {
      // For an archetype, we substitute the base object for the base.
      // FIXME: Feels like a total hack.
    } else if (!baseObjTy->isExistentialType() &&
               !baseObjTy->is<ArchetypeType>()) {
      ProtocolConformance *conformance = nullptr;
      if (TC.conformsToProtocol(baseObjTy, proto, DC, true, &conformance) &&
          conformance->hasTypeWitness(assocType, nullptr)) {
        type = conformance->getTypeWitness(assocType, &TC).getReplacement();
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
    // a AnyObject member.
    auto funcTy = type->castTo<AnyFunctionType>();
    Type resultTy = funcTy->getResult();
    Type inputTy = TC.getProtocol(SourceLoc(), KnownProtocolKind::AnyObject)
                     ->getDeclaredTypeOfContext();
    type = FunctionType::get(inputTy, resultTy, funcTy->getExtInfo());
  } else {
    type = openedType;

    // If we're referencing a method with dynamic Self that has 'self'
    // curried, replace the type of 'self' with the actual base object
    // type.
    if (hasDynamicSelf) {
      auto fnType = type->castTo<FunctionType>();
      auto selfTy = rebuildSelfTypeWithObjectType(fnType->getInput(), 
                                                  baseObjTy);      
      type = FunctionType::get(selfTy, fnType->getResult(),
                               fnType->getExtInfo());
    }
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
      std::tie(openedFullType, refType)
        = getTypeOfMemberReference(choice.getBaseType(), choice.getDecl(),
                                   isTypeReference, isDynamicResult,
                                   locator, nullptr);
    } else {
      std::tie(openedFullType, refType)
        = getTypeOfReference(choice.getDecl(), isTypeReference,
                             choice.isSpecialized(), locator, nullptr);
    }

    if (choice.isPotentiallyUnavailable()) {
      // For the moment, we do not treat constructors as optional
      // when potentially unavailable, even when
      // ExperimentalEnableUnavailableAsOptional is turned on.
      if (choice.getDecl()->getKind() != DeclKind::Constructor) {
        // Strip lvalue-ness and make type optional to reflect fact
        // that declaration may not be available.
        refType = getTypeWhenUnavailable(refType);
      }
    }
    
    if (choice.getDecl()->getAttrs().hasAttribute<OptionalAttr>() &&
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
      refType = tuple->getElementType(choice.getTupleIndex());
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
  assert(!refType->isDependentType() && "Cannot have a dependent type here");

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
      if (DC->getDeclaredTypeInContext()->getAnyNominal() == D)
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
