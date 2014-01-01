//===--- CSSimplify.cpp - Constraint Simplification -----------------------===//
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
// This file implements simplifications of constraints within the constraint
// system.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"

using namespace swift;
using namespace constraints;

static bool hasMandatoryTupleLabels(const ConstraintLocatorBuilder &locator) {
  if (Expr *e = locator.trySimplifyToExpr())
    return hasMandatoryTupleLabels(e);
  return false;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchTupleTypes(TupleType *tuple1, TupleType *tuple2,
                                  TypeMatchKind kind, unsigned flags,
                                  ConstraintLocatorBuilder locator) {
  unsigned subFlags = flags | TMF_GenerateConstraints;

  // Equality and subtyping have fairly strict requirements on tuple matching,
  // requiring element names to either match up or be disjoint.
  if (kind < TypeMatchKind::Conversion) {
    if (tuple1->getFields().size() != tuple2->getFields().size()) {
      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::TupleSizeMismatch, tuple1, tuple2);
      }

      return SolutionKind::Error;
    }

    for (unsigned i = 0, n = tuple1->getFields().size(); i != n; ++i) {
      const auto &elt1 = tuple1->getFields()[i];
      const auto &elt2 = tuple2->getFields()[i];

      // If the names don't match, we may have a conflict.
      if (elt1.getName() != elt2.getName()) {
        // Same-type requirements require exact name matches.
        if (kind == TypeMatchKind::SameType) {
          // Record this failure.
          if (shouldRecordFailures()) {
            recordFailure(getConstraintLocator(
                            locator.withPathElement(
                              LocatorPathElt::getNamedTupleElement(i))),
                          Failure::TupleNameMismatch, tuple1, tuple2);
          }

          return SolutionKind::Error;
        }

        // For subtyping constraints, just make sure that this name isn't
        // used at some other position.
        if (!elt2.getName().empty()) {
          int matched = tuple1->getNamedElementId(elt2.getName());
          if (matched != -1) {
            // Record this failure.
            if (shouldRecordFailures()) {
              recordFailure(getConstraintLocator(
                              locator.withPathElement(
                                LocatorPathElt::getNamedTupleElement(i))),
                            Failure::TupleNamePositionMismatch, tuple1, tuple2);
            }

            return SolutionKind::Error;
          }
        }
      }

      // Variadic bit must match.
      if (elt1.isVararg() != elt2.isVararg()) {
        // Record this failure.
        if (shouldRecordFailures()) {
          recordFailure(getConstraintLocator(
                          locator.withPathElement(
                            LocatorPathElt::getNamedTupleElement(i))),
                        Failure::TupleVariadicMismatch, tuple1, tuple2);
        }
        
        return SolutionKind::Error;
      }

      // Compare the element types.
      switch (matchTypes(elt1.getType(), elt2.getType(), kind, subFlags,
                         locator.withPathElement(
                           LocatorPathElt::getTupleElement(i)))) {
      case SolutionKind::Error:
        return SolutionKind::Error;

      case SolutionKind::Solved:
      case SolutionKind::Unsolved:
        break;
      }
    }
    return SolutionKind::Solved;
  }

  assert(kind >= TypeMatchKind::Conversion);

  // Compute the element shuffles for conversions.
  SmallVector<int, 16> sources;
  SmallVector<unsigned, 4> variadicArguments;
  if (computeTupleShuffle(tuple1, tuple2, sources, variadicArguments,
                          ::hasMandatoryTupleLabels(locator))) {
    // FIXME: Record why the tuple shuffle couldn't be computed.
    if (shouldRecordFailures()) {
      if (tuple1->getNumElements() != tuple2->getNumElements()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::TupleSizeMismatch, tuple1, tuple2);
      }
    }
    return SolutionKind::Error;
  }

  // Check each of the elements.
  bool hasVarArg = false;
  for (unsigned idx2 = 0, n = sources.size(); idx2 != n; ++idx2) {
    // Default-initialization always allowed for conversions.
    if (sources[idx2] == TupleShuffleExpr::DefaultInitialize) {
      continue;
    }

    // Variadic arguments handled below.
    if (sources[idx2] == TupleShuffleExpr::FirstVariadic) {
      hasVarArg = true;
      continue;
    }

    assert(sources[idx2] >= 0);
    unsigned idx1 = sources[idx2];

    // Match up the types.
    const auto &elt1 = tuple1->getFields()[idx1];
    const auto &elt2 = tuple2->getFields()[idx2];
    switch (matchTypes(elt1.getType(), elt2.getType(), kind, subFlags,
                       locator.withPathElement(
                         LocatorPathElt::getTupleElement(idx1)))) {
    case SolutionKind::Error:
      return SolutionKind::Error;

    case SolutionKind::Solved:
    case SolutionKind::Unsolved:
      break;
    }

  }

  // If we have variadic arguments to check, do so now.
  if (hasVarArg) {
    const auto &elt2 = tuple2->getFields().back();
    auto eltType2 = elt2.getVarargBaseTy();

    for (unsigned idx1 : variadicArguments) {
      switch (matchTypes(tuple1->getElementType(idx1), eltType2, kind, subFlags,
                         locator.withPathElement(
                           LocatorPathElt::getTupleElement(idx1)))) {
      case SolutionKind::Error:
        return SolutionKind::Error;

      case SolutionKind::Solved:
      case SolutionKind::Unsolved:
        break;
      }
    }
  }

  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchScalarToTupleTypes(Type type1, TupleType *tuple2,
                                          TypeMatchKind kind, unsigned flags,
                                          ConstraintLocatorBuilder locator) {
  int scalarFieldIdx = tuple2->getFieldForScalarInit();
  assert(scalarFieldIdx >= 0 && "Invalid tuple for scalar-to-tuple");
  const auto &elt = tuple2->getFields()[scalarFieldIdx];
  auto scalarFieldTy = elt.isVararg()? elt.getVarargBaseTy() : elt.getType();
  return matchTypes(type1, scalarFieldTy, kind, flags,
                    locator.withPathElement(ConstraintLocator::ScalarToTuple));
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchTupleToScalarTypes(TupleType *tuple1, Type type2,
                                          TypeMatchKind kind, unsigned flags,
                                          ConstraintLocatorBuilder locator) {
  assert(tuple1->getNumElements() == 1 && "Wrong number of elements");
  assert(!tuple1->getFields()[0].isVararg() && "Should not be variadic");
  return matchTypes(tuple1->getElementType(0),
                    type2, kind, flags,
                    locator.withPathElement(
                      LocatorPathElt::getTupleElement(0)));
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                     TypeMatchKind kind, unsigned flags,
                                     ConstraintLocatorBuilder locator) {
  // An @auto_closure function type can be a subtype of a
  // non-@auto_closure function type.
  if (func1->isAutoClosure() != func2->isAutoClosure()) {
    if (func2->isAutoClosure() || kind < TypeMatchKind::TrivialSubtype) {
      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::FunctionAutoclosureMismatch, func1, func2);
      }

      return SolutionKind::Error;
    }
  }

  // A @noreturn function type can be a subtype of a non-@noreturn function
  // type.
  if (func1->isNoReturn() != func2->isNoReturn()) {
    if (func2->isNoReturn() || kind < TypeMatchKind::SameType) {
      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      Failure::FunctionNoReturnMismatch, func1, func2);
      }

      return SolutionKind::Error;
    }
  }

  // Determine how we match up the input/result types.
  TypeMatchKind subKind;
  switch (kind) {
  case TypeMatchKind::BindType:
  case TypeMatchKind::SameType:
  case TypeMatchKind::TrivialSubtype:
    subKind = kind;
    break;

  case TypeMatchKind::Subtype:
    subKind = TypeMatchKind::TrivialSubtype;
    break;

  case TypeMatchKind::Conversion:
  case TypeMatchKind::OperatorConversion:
    subKind = TypeMatchKind::Subtype;
    break;
  }

  unsigned subFlags = flags | TMF_GenerateConstraints;

  // Input types can be contravariant (or equal).
  SolutionKind result = matchTypes(func2->getInput(), func1->getInput(),
                                   subKind, subFlags,
                                   locator.withPathElement(
                                     ConstraintLocator::FunctionArgument));
  if (result == SolutionKind::Error)
    return SolutionKind::Error;

  // Result type can be covariant (or equal).
  switch (matchTypes(func1->getResult(), func2->getResult(), subKind,
                     subFlags,
                     locator.withPathElement(
                       ConstraintLocator::FunctionResult))) {
  case SolutionKind::Error:
    return SolutionKind::Error;

  case SolutionKind::Solved:
    result = SolutionKind::Solved;
    break;

  case SolutionKind::Unsolved:
    result = SolutionKind::Unsolved;
    break;
  }

  return result;
}

/// \brief Map a failed type-matching kind to a failure kind, generically.
static Failure::FailureKind getRelationalFailureKind(TypeMatchKind kind) {
  switch (kind) {
  case TypeMatchKind::BindType:
  case TypeMatchKind::SameType:
    return Failure::TypesNotEqual;

  case TypeMatchKind::TrivialSubtype:
    return Failure::TypesNotTrivialSubtypes;

  case TypeMatchKind::Subtype:
    return Failure::TypesNotSubtypes;

  case TypeMatchKind::Conversion:
  case TypeMatchKind::OperatorConversion:
    return Failure::TypesNotConvertible;
  }

  llvm_unreachable("unhandled type matching kind");
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchSuperclassTypes(Type type1, Type type2,
                                       TypeMatchKind kind, unsigned flags,
                                       ConstraintLocatorBuilder locator) {
  auto classDecl2 = type2->getClassOrBoundGenericClass();
  bool done = false;
  for (auto super1 = TC.getSuperClassOf(type1);
       !done && super1;
       super1 = TC.getSuperClassOf(super1)) {
    if (super1->getClassOrBoundGenericClass() != classDecl2)
      continue;

    return matchTypes(super1, type2, TypeMatchKind::SameType,
                      TMF_GenerateConstraints, locator);
  }

  // Record this failure.
  // FIXME: Specialize diagnostic.
  if (shouldRecordFailures()) {
    recordFailure(getConstraintLocator(locator),
                  getRelationalFailureKind(kind), type1, type2);
  }

  return SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchDeepEqualityTypes(Type type1, Type type2,
                                         ConstraintLocatorBuilder locator) {
  // Handle nominal types that are not directly generic.
  if (auto nominal1 = type1->getAs<NominalType>()) {
    auto nominal2 = type2->castTo<NominalType>();

    assert((bool)nominal1->getParent() == (bool)nominal2->getParent() &&
           "Mismatched parents of nominal types");

    if (!nominal1->getParent())
      return SolutionKind::Solved;

    // Match up the parents, exactly.
    return matchTypes(nominal1->getParent(), nominal2->getParent(),
                      TypeMatchKind::SameType, TMF_GenerateConstraints,
                      locator.withPathElement(ConstraintLocator::ParentType));
  }

  auto bound1 = type1->castTo<BoundGenericType>();
  auto bound2 = type2->castTo<BoundGenericType>();

  // Match up the parents, exactly, if there are parents.
  assert((bool)bound1->getParent() == (bool)bound2->getParent() &&
         "Mismatched parents of bound generics");
  if (bound1->getParent()) {
    switch (matchTypes(bound1->getParent(), bound2->getParent(),
                       TypeMatchKind::SameType, TMF_GenerateConstraints,
                       locator.withPathElement(ConstraintLocator::ParentType))){
    case SolutionKind::Error:
      return SolutionKind::Error;

    case SolutionKind::Solved:
    case SolutionKind::Unsolved:
      break;
    }
  }

  // Match up the generic arguments, exactly.
  auto args1 = bound1->getGenericArgs();
  auto args2 = bound2->getGenericArgs();
  assert(args1.size() == args2.size() && "Mismatched generic args");
  for (unsigned i = 0, n = args1.size(); i != n; ++i) {
    switch (matchTypes(args1[i], args2[i], TypeMatchKind::SameType,
                       TMF_GenerateConstraints,
                       locator.withPathElement(
                         LocatorPathElt::getGenericArgument(i)))) {
    case SolutionKind::Error:
      return SolutionKind::Error;

    case SolutionKind::Solved:
    case SolutionKind::Unsolved:
      break;
    }
  }

  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchExistentialTypes(Type type1, Type type2,
                                        TypeMatchKind kind, unsigned flags,
                                        ConstraintLocatorBuilder locator) {
  SmallVector<ProtocolDecl *, 4> protocols;

  bool existential = type2->isExistentialType(protocols);
  assert(existential && "Bogus existential match");
  (void)existential;

  for (auto proto : protocols) {
    switch (simplifyConformsToConstraint(type1, proto, locator, false)) {
      case SolutionKind::Solved:
        break;

      case SolutionKind::Unsolved:
        // Add the constraint.
        addConstraint(ConstraintKind::ConformsTo, type1,
                      proto->getDeclaredType());
        break;

      case SolutionKind::Error:
        return SolutionKind::Error;
    }
  }

  return SolutionKind::Solved;
}

/// \brief Map a type-matching kind to a constraint kind.
static ConstraintKind getConstraintKind(TypeMatchKind kind) {
  switch (kind) {
  case TypeMatchKind::BindType:
    return ConstraintKind::Bind;

  case TypeMatchKind::SameType:
    return ConstraintKind::Equal;

  case TypeMatchKind::TrivialSubtype:
    return ConstraintKind::TrivialSubtype;

  case TypeMatchKind::Subtype:
    return ConstraintKind::Subtype;

  case TypeMatchKind::Conversion:
    return ConstraintKind::Conversion;
      
  case TypeMatchKind::OperatorConversion:
    return ConstraintKind::OperatorConversion;
  }

  llvm_unreachable("unhandled type matching kind");
}

/// Determine whether we should attempt a user-defined conversion.
static bool shouldTryUserConversion(ConstraintSystem &cs, Type type) {
  // Strip the l-value qualifier if present.
  if (!type->is<InOutType>())
    type = type->getRValueType();
  
  // If this isn't a type that can have user-defined conversions, there's
  // nothing to do.
  if (!type->getNominalOrBoundGenericNominal() && !type->is<ArchetypeType>())
    return false;

  // If there are no user-defined conversions, there's nothing to do.
  // FIXME: lame name!
  auto &ctx = cs.getASTContext();
  auto name = ctx.getIdentifier("__conversion");
  return static_cast<bool>(cs.lookupMember(type, name));
}

/// If the given type has user-defined conversions, introduce new
/// relational constraint between the result of performing the user-defined
/// conversion and an arbitrary other type.
static ConstraintSystem::SolutionKind
tryUserConversion(ConstraintSystem &cs, Type type, ConstraintKind kind,
                  Type otherType, ConstraintLocatorBuilder locator) {
  assert(kind != ConstraintKind::Construction &&
         kind != ConstraintKind::Conversion &&
         kind != ConstraintKind::OperatorConversion &&
         "Construction/conversion constraints create potential cycles");
  
  // If this isn't a type that can have user-defined conversions, there's
  // nothing to do.
  if (!shouldTryUserConversion(cs, type))
    return ConstraintSystem::SolutionKind::Unsolved;

  auto memberLocator = cs.getConstraintLocator(
                         locator.withPathElement(
                           ConstraintLocator::ConversionMember));
  auto inputTV = cs.createTypeVariable(
                   cs.getConstraintLocator(memberLocator,
                                           ConstraintLocator::FunctionArgument),
                   /*options=*/0);
  auto outputTV = cs.createTypeVariable(
                    cs.getConstraintLocator(memberLocator,
                                            ConstraintLocator::FunctionResult),
                    /*options=*/0);

  auto &ctx = cs.getASTContext();
  auto name = ctx.getIdentifier("__conversion");

  // The conversion function will have function type TI -> TO, for fresh
  // type variables TI and TO.
  cs.addValueMemberConstraint(type, name,
                              FunctionType::get(inputTV, outputTV),
                              memberLocator);

  // A conversion function must accept an empty parameter list ().
  // Note: This should never fail, because the declaration checker
  // should ensure that conversions have no non-defaulted parameters.
  cs.addConstraint(ConstraintKind::Conversion, TupleType::getEmpty(ctx),
                   inputTV, cs.getConstraintLocator(locator));

  // Relate the output of the conversion function to the other type, using
  // the provided constraint kind.
  // If the type we're converting to is existential, we can also have an
  // existential conversion here, so introduce a disjunction.
  auto resultLocator = cs.getConstraintLocator(
                         locator.withPathElement(
                           ConstraintLocator::ConversionResult));
  if (otherType->isExistentialType()) {
    Constraint *constraints[2] = {
      Constraint::create(cs, kind, outputTV, otherType, Identifier(),
                         resultLocator),
      Constraint::createRestricted(cs, ConstraintKind::Conversion,
                                   ConversionRestrictionKind::Existential,
                                   outputTV, otherType, resultLocator)
    };
    cs.addConstraint(Constraint::createDisjunction(cs, constraints,
                                                   resultLocator));
  } else {
    cs.addConstraint(kind, outputTV, otherType, resultLocator);
  }

  // We're adding a user-defined conversion.
  cs.increaseScore(SK_UserConversion);

  return ConstraintSystem::SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchTypes(Type type1, Type type2, TypeMatchKind kind,
                             unsigned flags,
                             ConstraintLocatorBuilder locator) {
  // If we have type variables that have been bound to fixed types, look through
  // to the fixed type.
  TypeVariableType *typeVar1;
  type1 = getFixedTypeRecursive(type1, typeVar1,
                                kind == TypeMatchKind::SameType);
  auto desugar1 = type1->getDesugaredType();

  TypeVariableType *typeVar2;
  type2 = getFixedTypeRecursive(type2, typeVar2,
                                kind == TypeMatchKind::SameType);
  auto desugar2 = type2->getDesugaredType();

  // If the types are obviously equivalent, we're done.
  if (desugar1 == desugar2)
    return SolutionKind::Solved;

  // If either (or both) types are type variables, unify the type variables.
  if (typeVar1 || typeVar2) {
    switch (kind) {
    case TypeMatchKind::BindType:
    case TypeMatchKind::SameType: {
      if (typeVar1 && typeVar2) {
        auto rep1 = getRepresentative(typeVar1);
        auto rep2 = getRepresentative(typeVar2);
        if (rep1 == rep2) {
          // We already merged these two types, so this constraint is
          // trivially solved.
          return SolutionKind::Solved;
        }

        // If exactly one of the type variables can bind to an lvalue, we
        // can't merge these two type variables.
        if (rep1->getImpl().canBindToLValue()
              != rep2->getImpl().canBindToLValue()) {
          if (flags & TMF_GenerateConstraints) {
            // Add a new constraint between these types. We consider the current
            // type-matching problem to the "solved" by this addition, because
            // this new constraint will be solved at a later point.
            // Obviously, this must not happen at the top level, or the algorithm
            // would not terminate.
            addConstraint(getConstraintKind(kind), rep1, rep2,
                          getConstraintLocator(locator));
            return SolutionKind::Solved;
          }

          return SolutionKind::Unsolved;
        }

        // Merge the equivalence classes corresponding to these two variables.
        mergeEquivalenceClasses(rep1, rep2);
        return SolutionKind::Solved;
      }

      // Provide a fixed type for the type variable.
      bool wantRvalue = kind == TypeMatchKind::SameType;
      if (typeVar1) {
        // If we want an rvalue, get the rvalue.
        if (wantRvalue)
          type2 = type2->getRValueType();

        // If the left-hand type variable cannot bind to an lvalue,
        // but we still have an lvalue, fail.
        if (!typeVar1->getImpl().canBindToLValue()) {
          if (type2->is<LValueType>()) {
            if (false && shouldRecordFailures()) {
              recordFailure(getConstraintLocator(locator),
                            Failure::IsForbiddenLValue, type1, type2);
            }
            return SolutionKind::Error;
          }

          // Okay. Bind below.
        }

        assignFixedType(typeVar1, type2);
        return SolutionKind::Solved;
      }

      // If we want an rvalue, get the rvalue.
      if (wantRvalue)
        type1 = type1->getRValueType();

      if (!typeVar2->getImpl().canBindToLValue()) {
        if (type1->is<LValueType>()) {
          if (false && shouldRecordFailures()) {
            recordFailure(getConstraintLocator(locator),
                          Failure::IsForbiddenLValue, type1, type2);
          }
          return SolutionKind::Error;
        }
        
        // Okay. Bind below.
      }

      assignFixedType(typeVar2, type1);
      return SolutionKind::Solved;
    }

    case TypeMatchKind::TrivialSubtype:
    case TypeMatchKind::Subtype:
    case TypeMatchKind::Conversion:
    case TypeMatchKind::OperatorConversion:
      if (flags & TMF_GenerateConstraints) {
        // Add a new constraint between these types. We consider the current
        // type-matching problem to the "solved" by this addition, because
        // this new constraint will be solved at a later point.
        // Obviously, this must not happen at the top level, or the algorithm
        // would not terminate.
        addConstraint(getConstraintKind(kind), type1, type2,
                      getConstraintLocator(locator));
        return SolutionKind::Solved;
      }

      // We couldn't solve this constraint. If only one of the types is a type
      // variable, perhaps we can do something with it below.
      if (typeVar1 && typeVar2)
        return typeVar1 == typeVar2 ? SolutionKind::Solved
                                    : SolutionKind::Unsolved;
        
      break;
    }
  }

  llvm::SmallVector<ConversionRestrictionKind, 4> potentialConversions;
  bool concrete = !typeVar1 && !typeVar2;

  // Decompose parallel structure.
  unsigned subFlags = flags | TMF_GenerateConstraints;
  if (desugar1->getKind() == desugar2->getKind()) {
    switch (desugar1->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("Type has not been desugared completely");

#define ARTIFICIAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("artificial type in constraint");

#define BUILTIN_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    case TypeKind::Module:
      if (desugar1 == desugar2) {
        return SolutionKind::Solved;
      }

      // Record this failure.
      if (shouldRecordFailures()) {
        recordFailure(getConstraintLocator(locator),
                      getRelationalFailureKind(kind), type1, type2);
      }

      return SolutionKind::Error;

    case TypeKind::Error:
      return SolutionKind::Error;

    case TypeKind::GenericTypeParam:
    case TypeKind::DependentMember:
      llvm_unreachable("unmapped dependent type in type checker");

    case TypeKind::TypeVariable:
    case TypeKind::Archetype:
      // Nothing to do here; handle type variables and archetypes below.
      break;

    case TypeKind::Tuple: {
      // Try the tuple-to-tuple conversion.
      potentialConversions.push_back(ConversionRestrictionKind::TupleToTuple);

      break;
    }

    case TypeKind::Enum:
    case TypeKind::Struct:
    case TypeKind::Class: {
      auto nominal1 = cast<NominalType>(desugar1);
      auto nominal2 = cast<NominalType>(desugar2);
      if (nominal1->getDecl() == nominal2->getDecl()) {
        potentialConversions.push_back(ConversionRestrictionKind::DeepEquality);
      }
      break;
    }

    case TypeKind::Protocol:
      // Nothing to do here; try existential and user-defined conversions below.
      break;

    case TypeKind::Metatype: {
      auto meta1 = cast<MetatypeType>(desugar1);
      auto meta2 = cast<MetatypeType>(desugar2);

      // metatype<B> < metatype<A> if A < B and both A and B are classes.
      TypeMatchKind subKind = TypeMatchKind::SameType;
      if (kind != TypeMatchKind::SameType &&
          (meta1->getInstanceType()->mayHaveSuperclass() ||
           meta2->getInstanceType()->getClassOrBoundGenericClass()))
        subKind = std::min(kind, TypeMatchKind::Subtype);
      
      return matchTypes(meta1->getInstanceType(), meta2->getInstanceType(),
                        subKind, subFlags,
                        locator.withPathElement(
                          ConstraintLocator::InstanceType));
    }

    case TypeKind::Function:
      return matchFunctionTypes(cast<FunctionType>(desugar1),
                                cast<FunctionType>(desugar2),
                                kind, flags, locator);

    case TypeKind::PolymorphicFunction:
    case TypeKind::GenericFunction:
      llvm_unreachable("Polymorphic function type should have been opened");

    case TypeKind::Array:
      return matchTypes(cast<ArrayType>(desugar1)->getBaseType(),
                        cast<ArrayType>(desugar2)->getBaseType(),
                        TypeMatchKind::SameType, subFlags,
                        locator.withPathElement(
                          ConstraintLocator::ArrayElementType));

    case TypeKind::ProtocolComposition:
      // Existential types handled below.
      break;

    case TypeKind::LValue:
      return matchTypes(cast<LValueType>(desugar1)->getObjectType(),
                        cast<LValueType>(desugar2)->getObjectType(),
                        TypeMatchKind::SameType, subFlags,
                        locator.withPathElement(
                          ConstraintLocator::ArrayElementType));
    
    case TypeKind::InOut:
      // If the RHS is an @inout type, the LHS must be an @lvalue type.
      if (kind >= TypeMatchKind::OperatorConversion) {
        if (shouldRecordFailures())
          recordFailure(getConstraintLocator(locator),
                        Failure::IsForbiddenLValue, type1, type2);
        return SolutionKind::Error;
      }
      return matchTypes(cast<InOutType>(desugar1)->getObjectType(),
                        cast<InOutType>(desugar2)->getObjectType(),
                        TypeMatchKind::SameType, subFlags,
                  locator.withPathElement(ConstraintLocator::ArrayElementType));

    case TypeKind::UnboundGeneric:
      llvm_unreachable("Unbound generic type should have been opened");

    case TypeKind::BoundGenericClass:
    case TypeKind::BoundGenericEnum:
    case TypeKind::BoundGenericStruct: {
      auto bound1 = cast<BoundGenericType>(desugar1);
      auto bound2 = cast<BoundGenericType>(desugar2);
      
      if (bound1->getDecl() == bound2->getDecl()) {
        potentialConversions.push_back(ConversionRestrictionKind::DeepEquality);
      }
      break;
    }
    }
  }

  // FIXME: Materialization

  if (concrete && kind >= TypeMatchKind::TrivialSubtype) {
    auto tuple1 = type1->getAs<TupleType>();
    auto tuple2 = type2->getAs<TupleType>();

    // Detect when the source and destination are both permit scalar
    // conversions, but the source has a name and the destination does not have
    // the same name.
    bool tuplesWithMismatchedNames = false;
    if (tuple1 && tuple2) {
      int scalar1 = tuple1->getFieldForScalarInit();
      int scalar2 = tuple2->getFieldForScalarInit();
      if (scalar1 >= 0 && scalar2 >= 0) {
        auto name1 = tuple1->getFields()[scalar1].getName();
        auto name2 = tuple2->getFields()[scalar2].getName();
        tuplesWithMismatchedNames = !name1.empty() && name1 != name2;
      }
    }

    if (tuple2 && !tuplesWithMismatchedNames) {
      // A scalar type is a trivial subtype of a one-element, non-variadic tuple
      // containing a single element if the scalar type is a subtype of
      // the type of that tuple's element.
      //
      // A scalar type can be converted to a tuple so long as there is at
      // most one non-defaulted element.
      if ((tuple2->getFields().size() == 1 &&
           !tuple2->getFields()[0].isVararg()) ||
          (kind >= TypeMatchKind::Conversion &&
           tuple2->getFieldForScalarInit() >= 0)) {
        potentialConversions.push_back(
          ConversionRestrictionKind::ScalarToTuple);

        // FIXME: Prohibits some user-defined conversions for tuples.
        goto commit_to_conversions;
      }
    }

    if (tuple1 && !tuplesWithMismatchedNames) {
      // A single-element tuple can be a trivial subtype of a scalar.
      if (tuple1->getFields().size() == 1 &&
          !tuple1->getFields()[0].isVararg()) {
        potentialConversions.push_back(
          ConversionRestrictionKind::TupleToScalar);
      }
    }

    // Subclass-to-superclass conversion.
    if (type1->mayHaveSuperclass() && type2->mayHaveSuperclass() &&
        type2->getClassOrBoundGenericClass() &&
        type1->getClassOrBoundGenericClass()
          != type2->getClassOrBoundGenericClass()) {
      potentialConversions.push_back(ConversionRestrictionKind::Superclass);
    }
  }

  if (concrete && kind >= TypeMatchKind::Conversion) {
    // An lvalue of type T1 can be converted to a value of type T2 so long as
    // T1 is convertible to T2 (by loading the value).
    if (type1->is<LValueType>())
      potentialConversions.push_back(
        ConversionRestrictionKind::LValueToRValue);

    // An expression can be converted to an auto-closure function type, creating
    // an implicit closure.
    if (auto function2 = type2->getAs<FunctionType>()) {
      if (function2->isAutoClosure())
        return matchTypes(type1, function2->getResult(), kind, subFlags,
                          locator.withPathElement(ConstraintLocator::Load));
    }
  }

  if (concrete && kind >= TypeMatchKind::OperatorConversion) {
    // If the RHS is an @inout type, the LHS must be an @lvalue type.
    if (auto *iot = type2->getAs<InOutType>()) {
      return matchTypes(type1, LValueType::get(iot->getObjectType()),
                        kind, subFlags,
                        locator.withPathElement(
                                ConstraintLocator::ArrayElementType));
    }
  }
  
  // For a subtyping relation involving two existential types or subtyping of
  // a class existential type, or a conversion from any type to an
  // existential type, check whether the first type conforms to each of the
  // protocols in the second type.
  if (type2->isExistentialType() &&
      (kind >= TypeMatchKind::Conversion ||
      (kind == TypeMatchKind::Subtype &&
       (type1->isExistentialType() || type2->isClassExistentialType())))) {
    potentialConversions.push_back(ConversionRestrictionKind::Existential);
  }

  // A value of type T can be converted to type U? if T is convertible to U.
  // A value of type T? can be converted to type U? if T is convertible to U.
  {
    BoundGenericType *boundGenericType2;
    if (concrete && kind >= TypeMatchKind::Conversion &&
        (boundGenericType2 = type2->getAs<BoundGenericType>())) {
      if (boundGenericType2->getDecl() == TC.Context.getOptionalDecl()) {
        assert(boundGenericType2->getGenericArgs().size() == 1);
        
        BoundGenericType *boundGenericType1
          = type1->getAs<BoundGenericType>();
        if (boundGenericType1
            && boundGenericType1->getDecl() == TC.Context.getOptionalDecl()) {
          assert(boundGenericType1->getGenericArgs().size() == 1);
          potentialConversions.push_back(
                                 ConversionRestrictionKind::OptionalToOptional);
        }
        
        potentialConversions.push_back(
          ConversionRestrictionKind::ValueToOptional);
      }
    }
  }

  // A nominal type can be converted to another type via a user-defined
  // conversion function.
  if (concrete && kind >= TypeMatchKind::Conversion &&
      shouldTryUserConversion(*this, type1)) {
    potentialConversions.push_back(ConversionRestrictionKind::User);
  }

commit_to_conversions:
  // When we hit this point, we're committed to the set of potential
  // conversions recorded thus far.
  //
  //
  // FIXME: One should only jump to this label in the case where we want to
  // cut off other potential conversions because we know none of them apply.
  // Gradually, those gotos should go away as we can handle more kinds of
  // conversions via disjunction constraints.
  if (potentialConversions.empty()) {
    // If one of the types is a type variable, we leave this unsolved.
    if (typeVar1 || typeVar2)
      return SolutionKind::Unsolved;

    // If we are supposed to record failures, do so.
    if (shouldRecordFailures()) {
      recordFailure(getConstraintLocator(locator),
                    getRelationalFailureKind(kind), type1, type2);
    }

    return SolutionKind::Error;
  }

  // Where there is more than one potential conversion, create a disjunction
  // so that we'll explore all of the options.
  if (potentialConversions.size() > 1) {
    auto fixedLocator = getConstraintLocator(locator);
    SmallVector<Constraint *, 2> constraints;
    for (auto potential : potentialConversions) {
      // Determine the constraint kind. For a deep equality constraint, only
      // perform equality.
      auto constraintKind = getConstraintKind(kind);
      if (potential == ConversionRestrictionKind::DeepEquality)
        constraintKind = ConstraintKind::Equal;

      constraints.push_back(
        Constraint::createRestricted(*this, constraintKind, potential, 
                                     type1, type2, fixedLocator));
    }
    addConstraint(Constraint::createDisjunction(*this, constraints,
                                                fixedLocator));
    return SolutionKind::Solved;
  }

  // For a single potential conversion, directly recurse, so that we
  // don't allocate a new constraint or constraint locator.
  switch (potentialConversions[0]) {
  case ConversionRestrictionKind::TupleToTuple:
    return matchTupleTypes(type1->castTo<TupleType>(),
                           type2->castTo<TupleType>(),
                           kind, flags, locator);

  case ConversionRestrictionKind::ScalarToTuple:
    return matchScalarToTupleTypes(type1, type2->castTo<TupleType>(), kind,
                                   subFlags, locator);

  case ConversionRestrictionKind::TupleToScalar:
    return matchTupleToScalarTypes(type1->castTo<TupleType>(), type2,
                                   kind, subFlags, locator);

  case ConversionRestrictionKind::DeepEquality:
    return matchDeepEqualityTypes(type1, type2, locator);

  case ConversionRestrictionKind::Superclass:
    return matchSuperclassTypes(type1, type2, kind, flags, locator);

  case ConversionRestrictionKind::LValueToRValue:
    return matchTypes(type1->getRValueType(), type2, kind, subFlags, locator);

  case ConversionRestrictionKind::Existential:
    return matchExistentialTypes(type1, type2, kind, flags, locator);

  case ConversionRestrictionKind::ValueToOptional: {
    auto boundGenericType2 = type2->castTo<BoundGenericType>();
    (void)boundGenericType2;
    assert(boundGenericType2->getDecl() == TC.Context.getOptionalDecl());
    assert(boundGenericType2->getGenericArgs().size() == 1);
    return matchTypes(type1,
                      type2->castTo<BoundGenericType>()->getGenericArgs()[0],
                      kind, subFlags, locator);
  }
      
  case ConversionRestrictionKind::OptionalToOptional: {
    auto boundGenericType1 = type1->castTo<BoundGenericType>();
    auto boundGenericType2 = type2->castTo<BoundGenericType>();
    (void)boundGenericType1; (void)boundGenericType2;
    assert(boundGenericType1->getDecl() == TC.Context.getOptionalDecl());
    assert(boundGenericType1->getGenericArgs().size() == 1);
    assert(boundGenericType2->getDecl() == TC.Context.getOptionalDecl());
    assert(boundGenericType2->getGenericArgs().size() == 1);
    return matchTypes(type1->castTo<BoundGenericType>()->getGenericArgs()[0],
                      type2->castTo<BoundGenericType>()->getGenericArgs()[0],
                      kind, subFlags, locator);
  }
      
  case ConversionRestrictionKind::User:
    return tryUserConversion(*this, type1, ConstraintKind::Subtype, type2,
                              locator);
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstructionConstraint(Type valueType, Type argType,
                                                 unsigned flags,
                                                 ConstraintLocator *locator) {
  // Desugar the value type.
  auto desugarValueType = valueType->getDesugaredType();

  // If we have a type variable that has been bound to a fixed type,
  // look through to that fixed type.
  auto desugarValueTypeVar = dyn_cast<TypeVariableType>(desugarValueType);
  if (desugarValueTypeVar) {
    if (auto fixed = getFixedType(desugarValueTypeVar)) {
      valueType = fixed;
      desugarValueType = fixed->getDesugaredType();
      desugarValueTypeVar = nullptr;
    }
  }

  switch (desugarValueType->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("Type has not been desugared completely");

#define ARTIFICIAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("artificial type in constraint");
    
  case TypeKind::Error:
    return SolutionKind::Error;

  case TypeKind::GenericFunction:
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    llvm_unreachable("unmapped dependent type");

  case TypeKind::TypeVariable:
    return SolutionKind::Unsolved;

  case TypeKind::Tuple: {
    // Tuple construction is simply tuple conversion.
    return matchTypes(argType, valueType, TypeMatchKind::Conversion,
                      flags|TMF_GenerateConstraints, locator);
  }

  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
  case TypeKind::Archetype:
    // Break out to handle the actual construction below.
    break;

  case TypeKind::PolymorphicFunction:
    llvm_unreachable("Polymorphic function type should have been opened");

  case TypeKind::UnboundGeneric:
    llvm_unreachable("Unbound generic type should have been opened");

#define BUILTIN_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Metatype:
  case TypeKind::Function:
  case TypeKind::Array:
  case TypeKind::ProtocolComposition:
  case TypeKind::LValue:
  case TypeKind::InOut:
  case TypeKind::Protocol:
  case TypeKind::Module:
    // If we are supposed to record failures, do so.
    if (shouldRecordFailures()) {
      recordFailure(locator, Failure::TypesNotConstructible,
                    valueType, argType);
    }
    
    return SolutionKind::Error;
  }

  auto ctors = TC.lookupConstructors(valueType, DC);
  if (!ctors) {
    // If we are supposed to record failures, do so.
    if (shouldRecordFailures()) {
      recordFailure(locator, Failure::TypesNotConstructible,
                    valueType, argType);
    }
    
    return SolutionKind::Error;
  }

  auto &context = getASTContext();
  // FIXME: lame name
  auto name = context.getIdentifier("init");
  auto applyLocator = getConstraintLocator(locator,
                                           ConstraintLocator::ApplyArgument);
  auto tv = createTypeVariable(applyLocator,
                               TVO_CanBindToLValue|TVO_PrefersSubtypeBinding);

  // The constructor will have function type T -> T2, for a fresh type
  // variable T. Note that these constraints specifically require a
  // match on the result type because the constructors for enums and struct
  // types always return a value of exactly that type.
  addValueMemberConstraint(valueType, name,
                           FunctionType::get(tv, valueType),
                           getConstraintLocator(
                             locator, 
                             ConstraintLocator::ConstructorMember));
  
  // The first type must be convertible to the constructor's argument type.
  addConstraint(ConstraintKind::Conversion, argType, tv, applyLocator);

  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind ConstraintSystem::simplifyConformsToConstraint(
                                 Type type,
                                 ProtocolDecl *protocol,
                                 ConstraintLocatorBuilder locator,
                                 bool allowNonConformingExistential) {
  // Dig out the fixed type to which this type refers.
  TypeVariableType *typeVar;
  type = getFixedTypeRecursive(type, typeVar, /*wantRValue=*/true);

  // If we hit a type variable without a fixed type, we can't
  // solve this yet.
  if (typeVar)
    return SolutionKind::Unsolved;

  // If existential types don't need to conform (i.e., they only need to
  // contain the protocol), check that separately.
  if (allowNonConformingExistential && type->isExistentialType()) {
    SmallVector<ProtocolDecl *, 4> protocols;
    bool isExistential = type->isExistentialType(protocols);
    assert(isExistential && "Not existential?");
    (void)isExistential;

    for (auto ap : protocols) {
      // If this isn't the protocol we're looking for, continue looking.
      if (ap == protocol || ap->inheritsFrom(protocol))
        return SolutionKind::Solved;
    }
  } else {
    // Check whether this type conforms to the protocol.
    if (TC.conformsToProtocol(type, protocol, DC))
      return SolutionKind::Solved;
  }

  // There's nothing more we can do; fail.
  recordFailure(getConstraintLocator(locator),
                Failure::DoesNotConformToProtocol, type,
                protocol->getDeclaredType());
  return SolutionKind::Error;
}

/// Determine the kind of checked cast to perform from the given type to
/// the given type.
///
/// This routine does not attempt to check whether the cast can actually
/// succeed; that's the caller's responsibility.
static CheckedCastKind getCheckedCastKind(Type fromType, Type toType) {
  // Classify the from/to types.
  bool toArchetype = toType->is<ArchetypeType>();
  bool fromArchetype = fromType->is<ArchetypeType>();
  bool toExistential = toType->isExistentialType();
  bool fromExistential = fromType->isExistentialType();

  // We can only downcast to an existential if the destination protocols are
  // objc and the source type is an objc class or an existential bounded by objc
  // protocols.
  if (toExistential) {
    return CheckedCastKind::ConcreteToUnrelatedExistential;
  }

  // A downcast can:
  //   - convert an archetype to a (different) archetype type.
  if (fromArchetype && toArchetype) {
    return CheckedCastKind::ArchetypeToArchetype;
  }

  //   - convert from an existential to an archetype or conforming concrete
  //     type.
  if (fromExistential) {
    if (toArchetype) {
      return CheckedCastKind::ExistentialToArchetype;
    }

    return CheckedCastKind::ExistentialToConcrete;
  }

  //   - convert an archetype to a concrete type fulfilling its constraints.
  if (fromArchetype) {
    return CheckedCastKind::ArchetypeToConcrete;
  }

  if (toArchetype) {
    //   - convert from a superclass to an archetype.
    if (toType->castTo<ArchetypeType>()->getSuperclass()) {
      return CheckedCastKind::SuperToArchetype;
    }

    //  - convert a concrete type to an archetype for which it fulfills
    //    constraints.
    return CheckedCastKind::ConcreteToArchetype;
  }

  // The remaining case is a class downcast.
  assert(!fromArchetype && "archetypes should have been handled above");
  assert(!toArchetype && "archetypes should have been handled above");
  assert(!fromExistential && "existentials should have been handled above");
  assert(!toExistential && "existentials should have been handled above");

  return CheckedCastKind::Downcast;

}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyCheckedCastConstraint(
                    Type fromType, Type toType,
                    ConstraintLocatorBuilder locator) {
  // Dig out the fixed type to which this type refers.
  TypeVariableType *typeVar1;
  fromType = getFixedTypeRecursive(fromType, typeVar1, /*wantRValue=*/true);

  // If we hit a type variable without a fixed type, we can't
  // solve this yet.
  if (typeVar1)
    return SolutionKind::Unsolved;

  // Dig out the fixed type to which this type refers.
  TypeVariableType *typeVar2;
  toType = getFixedTypeRecursive(toType, typeVar2, /*wantRValue=*/true);

  // If we hit a type variable without a fixed type, we can't
  // solve this yet.
  if (typeVar2)
    return SolutionKind::Unsolved;

  switch (getCheckedCastKind(fromType, toType)) {
  case CheckedCastKind::ArchetypeToArchetype:
  case CheckedCastKind::ConcreteToUnrelatedExistential:
  case CheckedCastKind::ExistentialToArchetype:
  case CheckedCastKind::SuperToArchetype:
    return SolutionKind::Solved;

  case CheckedCastKind::ArchetypeToConcrete:
  case CheckedCastKind::ConcreteToArchetype:
    // FIXME: Check substitutability.
    return SolutionKind::Solved;

  case CheckedCastKind::Downcast:
    addConstraint(ConstraintKind::Subtype, toType, fromType,
                  getConstraintLocator(locator));
    return SolutionKind::Solved;

  case CheckedCastKind::ExistentialToConcrete:
    addConstraint(ConstraintKind::Conversion, toType, fromType);
    return SolutionKind::Solved;

  case CheckedCastKind::Coercion:
  case CheckedCastKind::Unresolved:
    llvm_unreachable("Not a valid result");
  }
}

/// \brief Determine whether the given protocol member's signature involves
/// any associated types or Self.
static bool involvesAssociatedTypes(TypeChecker &tc, ValueDecl *decl) {
  Type type = decl->getType();

  // For a function or constructor,
  // Note that there are no destructor requirements, so we don't need to check
  // for destructors.
  if (isa<FuncDecl>(decl) || isa<ConstructorDecl>(decl))
    type = type->castTo<AnyFunctionType>()->getResult();

  // FIXME: Use interface type and look for dependent types.
  return type.findIf([](Type type) {
    if (auto archetype = type->getAs<ArchetypeType>()) {
      return archetype->getParent() || archetype->getSelfProtocol();
    }

    return false;
  });
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyMemberConstraint(const Constraint &constraint) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  Type baseTy = simplifyType(constraint.getFirstType());
  Type baseObjTy = baseTy->getRValueType();

  // Dig out the instance type.
  bool isMetatype = false;
  Type instanceTy = baseObjTy;
  if (auto baseObjMeta = baseObjTy->getAs<MetatypeType>()) {
    instanceTy = baseObjMeta->getInstanceType();
    isMetatype = true;
  }

  if (instanceTy->is<TypeVariableType>())
    return SolutionKind::Unsolved;
  
  // If the base type is a tuple type, look for the named or indexed member
  // of the tuple.
  Identifier name = constraint.getMember();
  Type memberTy = constraint.getSecondType();
  if (auto baseTuple = baseObjTy->getAs<TupleType>()) {
    StringRef nameStr = name.str();
    int fieldIdx = -1;
    // Resolve a number reference into the tuple type.
    unsigned Value = 0;
    if (!nameStr.getAsInteger(10, Value) &&
        Value < baseTuple->getFields().size()) {
      fieldIdx = Value;
    } else {
      fieldIdx = baseTuple->getNamedElementId(name);
    }

    if (fieldIdx == -1) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);
      return SolutionKind::Error;
    }

    // Add an overload set that selects this field.
    OverloadChoice choice(baseTy, fieldIdx);
    addBindOverloadConstraint(memberTy, choice, constraint.getLocator());
    return SolutionKind::Solved;
  }

  // FIXME: If the base type still involves type variables, we want this
  // constraint to be unsolved. This effectively requires us to solve the
  // left-hand side of a dot expression before we look for members.

  bool isExistential = instanceTy->isExistentialType();
  if (name.str() == "init") {
    // Constructors have their own approach to name lookup.
    auto ctors = TC.lookupConstructors(baseObjTy, DC);
    if (!ctors) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);

      return SolutionKind::Error;
    }

    // Introduce a new overload set.
    SmallVector<OverloadChoice, 4> choices;
    for (auto constructor : ctors) {
      // If the constructor is invalid, skip it.
      // FIXME: Note this as invalid, in case we don't find a solution,
      // so we don't let errors cascade further.
      TC.validateDecl(constructor, true);
      if (constructor->isInvalid())
        continue;

      // If our base is an existential type, we can't make use of any
      // constructor whose signature involves associated types.
      // FIXME: Mark this as 'unavailable'.
      if (isExistential &&
          involvesAssociatedTypes(getTypeChecker(), constructor))
        continue;

      choices.push_back(OverloadChoice(baseTy, constructor,
                                       /*isSpecialized=*/false));
    }

    if (choices.empty()) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);
      return SolutionKind::Error;
    }
    
    addOverloadSet(memberTy, choices, constraint.getLocator());
    return SolutionKind::Solved;
  }

  // If we want member types only, use member type lookup.
  if (constraint.getKind() == ConstraintKind::TypeMember) {
    auto lookup = TC.lookupMemberType(baseObjTy, name, DC);
    if (!lookup) {
      // FIXME: Customize diagnostic to mention types.
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);

      return SolutionKind::Error;
    }

    // Form the overload set.
    SmallVector<OverloadChoice, 4> choices;
    for (auto result : lookup) {
      // If the result is invalid, skip it.
      // FIXME: Note this as invalid, in case we don't find a solution,
      // so we don't let errors cascade further.
      TC.validateDecl(result.first, true);
      if (result.first->isInvalid())
        continue;

      choices.push_back(OverloadChoice(baseTy, result.first,
                                       /*isSpecialized=*/false));
    }

    if (choices.empty()) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);
      return SolutionKind::Error;
    }

    auto locator = getConstraintLocator(constraint.getLocator());
    addOverloadSet(memberTy, choices, locator);
    return SolutionKind::Solved;
  }

  // Look for members within the base.
  LookupResult &lookup = lookupMember(baseObjTy, name);
  if (!lookup) {
    // Check whether we actually performed a lookup with an integer value.
    unsigned index;
    if (!name.str().getAsInteger(10, index)) {
      // ".0" on a scalar just refers to the underlying scalar value.
      if (index == 0) {
        OverloadChoice identityChoice(baseTy, OverloadChoiceKind::BaseType);
        addBindOverloadConstraint(memberTy, identityChoice,
                                  constraint.getLocator());
        return SolutionKind::Solved;
      }

      // FIXME: Specialize diagnostic here?
    }

    recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                  baseObjTy, name);

    return SolutionKind::Error;
  }

  // The set of directly accessible types, which is only used when
  // we're performing dynamic lookup into an existential type.
  bool isDynamicLookup = false;
  if (auto protoTy = instanceTy->getAs<ProtocolType>()) {
    isDynamicLookup = protoTy->getDecl()->isSpecificProtocol(
                                            KnownProtocolKind::DynamicLookup);
  }

  // Introduce a new overload set to capture the choices.
  SmallVector<OverloadChoice, 4> choices;
  for (auto result : lookup) {
    // If the result is invalid, skip it.
    // FIXME: Note this as invalid, in case we don't find a solution,
    // so we don't let errors cascade further.
    TC.validateDecl(result, true);
    if (result->isInvalid())
      continue;

    // If our base is an existential type, we can't make use of any
    // member whose signature involves associated types.
    // FIXME: Mark this as 'unavailable'.
    if (isExistential && involvesAssociatedTypes(getTypeChecker(), result))
      continue;

    // If we are looking for a metatype member, don't include members that can
    // only be accessed on an instance of the object.
    // FIXME: Mark as 'unavailable' somehow.
    if (isMetatype && !(isa<FuncDecl>(result) || !result->isInstanceMember())) {
      continue;
    }

    // If we aren't looking in a metatype, ignore static functions, static
    // variables, and enum elements.
    if (!isMetatype && !baseObjTy->is<ModuleType>() &&
        !result->isInstanceMember())
      continue;

    // If we're doing dynamic lookup into a metatype of DynamicLookup and we've
    // found an instance member, ignore it.
    if (isDynamicLookup && isMetatype && result->isInstanceMember()) {
      // FIXME: Mark as 'unavailable' somehow.
      continue;
    }

    // Verify that @mutating methods on value types are only applied to settable
    // values.
    if (!isMetatype && !baseObjTy->hasReferenceSemantics() &&
        isa<FuncDecl>(result) && cast<FuncDecl>(result)->isMutating() &&
        result->isInstanceMember() && !baseTy->is<LValueType>())
      continue;

    // If we're looking into an existential type, check whether this
    // result was found via dynamic lookup.
    if (isDynamicLookup) {
      assert(result->getDeclContext()->isTypeContext() && "Dynamic lookup bug");

      // We found this declaration via dynamic lookup, record it as such.
      choices.push_back(OverloadChoice::getDeclViaDynamic(baseTy, result));
      continue;
    }

    choices.push_back(OverloadChoice(baseTy, result, /*isSpecialized=*/false));
  }

  if (choices.empty()) {
    recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                  baseObjTy, name);
    return SolutionKind::Error;
  }
  auto locator = getConstraintLocator(constraint.getLocator());
  addOverloadSet(memberTy, choices, locator);
  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyArchetypeConstraint(const Constraint &constraint) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  Type baseTy = constraint.getFirstType()->getRValueType();
  if (auto tv = dyn_cast<TypeVariableType>(baseTy.getPointer())) {
    auto fixed = getFixedType(tv);
    if (!fixed)
      return SolutionKind::Unsolved;

    // Continue with the fixed type.
    baseTy = fixed->getRValueType();
  }

  if (baseTy->is<ArchetypeType>()) {
    return SolutionKind::Solved;
  }

  // Record this failure.
  recordFailure(constraint.getLocator(), Failure::IsNotArchetype, baseTy);
  return SolutionKind::Error;
}

/// Simplify the given type for use in a type property constraint.
static Type simplifyForTypePropertyConstraint(ConstraintSystem &cs, Type type) {
  if (auto tv = dyn_cast<TypeVariableType>(type.getPointer())) {
    auto fixed = cs.getFixedType(tv);
    if (!fixed)
      return Type();

    // Continue with the fixed type.
    type = fixed;
  }

  return type;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyClassConstraint(const Constraint &constraint){
  auto baseTy = simplifyForTypePropertyConstraint(*this,
                                                  constraint.getFirstType());
  if (!baseTy)
    return SolutionKind::Unsolved;

  if (baseTy->getClassOrBoundGenericClass())
    return SolutionKind::Solved;

  if (auto archetype = baseTy->getAs<ArchetypeType>()) {
    if (archetype->requiresClass())
      return SolutionKind::Solved;
  }

  // Record this failure.
  recordFailure(constraint.getLocator(), Failure::IsNotClass, baseTy);
  return SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyDynamicLookupConstraint(const Constraint &constraint){
  auto baseTy = simplifyForTypePropertyConstraint(*this,
                                                  constraint.getFirstType());
  if (!baseTy)
    return SolutionKind::Unsolved;

  // Look through implicit lvalue types.
  if (auto lvalueTy = baseTy->getAs<LValueType>())
    baseTy = lvalueTy->getObjectType();

  if (auto protoTy = baseTy->getAs<ProtocolType>()) {
    if (protoTy->getDecl()->isSpecificProtocol(
                              KnownProtocolKind::DynamicLookup))
      return SolutionKind::Solved;
  }

  // Record this failure.
  recordFailure(constraint.getLocator(), Failure::IsNotArchetype, baseTy);
  return SolutionKind::Error;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyApplicableFnConstraint(const Constraint &constraint) {

  // By construction, the left hand side is a type that looks like the
  // following: $T1 -> $T2.
  Type type1 = constraint.getFirstType();
  assert(type1->is<FunctionType>());

  // Drill down to the concrete type on the right hand side.
  TypeVariableType *typeVar2;
  Type type2 = getFixedTypeRecursive(constraint.getSecondType(), typeVar2, 
                                     /*wantRValue=*/true);
  auto desugar2 = type2->getDesugaredType();

  // Force the right-hand side to be an rvalue.
  unsigned flags = TMF_GenerateConstraints;

  // If the types are obviously equivalent, we're done.
  if (type1.getPointer() == desugar2)
    return SolutionKind::Solved;

  // If right-hand side is a type variable, the constraint is unsolved.
  if (typeVar2)
    return SolutionKind::Unsolved;

  // Strip the 'ApplyFunction' off the locator.
  // FIXME: Perhaps ApplyFunction can go away entirely?
  ConstraintLocatorBuilder locator = constraint.getLocator();
  SmallVector<LocatorPathElt, 2> parts;
  Expr *anchor = locator.getLocatorParts(parts);
  assert(!parts.empty() && "Nonsensical applicable-function locator");
  assert(parts.back().getKind() == ConstraintLocator::ApplyFunction);
  parts.pop_back();
  ConstraintLocatorBuilder outerLocator = getConstraintLocator(anchor, parts);
  
  // For a function, bind the output and convert the argument to the input.
  auto func1 = type1->castTo<FunctionType>();
  if (desugar2->getKind() == TypeKind::Function) {
    auto func2 = cast<FunctionType>(desugar2);

    assert(func1->getResult()->is<TypeVariableType>() &&
           "the output of funct1 is a free variable by construction");

    // If this application is part of an operator, then we allow an implicit
    // lvalue to be compatible with @inout arguments.  This is used by
    // assignment operators.
    TypeMatchKind ArgConv = TypeMatchKind::Conversion;
    if (isa<PrefixUnaryExpr>(anchor) || isa<PostfixUnaryExpr>(anchor) ||
        isa<BinaryExpr>(anchor))
      ArgConv = TypeMatchKind::OperatorConversion;
    
    // The argument type must be convertible to the input type.
    if (matchTypes(func1->getInput(), func2->getInput(),
                   ArgConv, flags,
                   outerLocator.withPathElement(
                     ConstraintLocator::ApplyArgument))
          == SolutionKind::Error)
      return SolutionKind::Error;

    // The result types are equivalent.
    if (matchTypes(func1->getResult(), func2->getResult(),
                   TypeMatchKind::BindType,
                   flags,
                   locator.withPathElement(ConstraintLocator::FunctionResult))
          == SolutionKind::Error)
      return SolutionKind::Error;
    return SolutionKind::Solved;
  }

  // For a metatype, perform a construction.
  if (desugar2->getKind() == TypeKind::Metatype) {
    auto meta2 = cast<MetatypeType>(desugar2);
    auto instanceTy2 = meta2->getInstanceType();

    // Bind the result type to the instance type.
    if (matchTypes(func1->getResult(), instanceTy2,
                   TypeMatchKind::BindType,
                   flags,
                   locator.withPathElement(ConstraintLocator::FunctionResult))
        == SolutionKind::Error)
      return SolutionKind::Error;

    // Construct the instance from the input arguments.
    addConstraint(ConstraintKind::Construction, func1->getInput(), instanceTy2,
                  getConstraintLocator(outerLocator));
    return SolutionKind::Solved;
  }

  // If we are supposed to record failures, do so.
  if (shouldRecordFailures()) {
    recordFailure(getConstraintLocator(locator),
                  Failure::FunctionTypesMismatch,
                  type1, type2);
  }

  return SolutionKind::Error;
}

/// \brief Retrieve the type-matching kind corresponding to the given
/// constraint kind.
static TypeMatchKind getTypeMatchKind(ConstraintKind kind) {
  switch (kind) {
  case ConstraintKind::Bind: return TypeMatchKind::BindType;
  case ConstraintKind::Equal: return TypeMatchKind::SameType;
  case ConstraintKind::TrivialSubtype: return TypeMatchKind::TrivialSubtype;
  case ConstraintKind::Subtype: return TypeMatchKind::Subtype;
  case ConstraintKind::Conversion: return TypeMatchKind::Conversion;
  case ConstraintKind::OperatorConversion:
    return TypeMatchKind::OperatorConversion;

  case ConstraintKind::ApplicableFunction:
    llvm_unreachable("ApplicableFunction constraints don't involve "
                     "type matches");

  case ConstraintKind::BindOverload:
    llvm_unreachable("Overload binding constraints don't involve type matches");

  case ConstraintKind::Construction:
    llvm_unreachable("Construction constraints don't involve type matches");

  case ConstraintKind::ConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    llvm_unreachable("Conformance constraints don't involve type matches");

  case ConstraintKind::CheckedCast:
    llvm_unreachable("Checked cast constraints don't involve type matches");

  case ConstraintKind::ValueMember:
  case ConstraintKind::TypeMember:
    llvm_unreachable("Member constraints don't involve type matches");

  case ConstraintKind::Archetype:
  case ConstraintKind::Class:
  case ConstraintKind::DynamicLookupValue:
    llvm_unreachable("Type properties don't involve type matches");

  case ConstraintKind::Conjunction:
  case ConstraintKind::Disjunction:
    llvm_unreachable("Con/disjunction constraints don't involve type matches");
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstraint(const Constraint &constraint) {
  switch (constraint.getKind()) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::TrivialSubtype:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion:
  case ConstraintKind::OperatorConversion: {
    // For relational constraints, match up the types.
    auto matchKind = getTypeMatchKind(constraint.getKind());

    // If there is a restriction on this constraint, apply it directly rather
    // than going through the general \c matchTypes() machinery.
    if (auto restriction = constraint.getRestriction()) {
      SolutionKind result;
      switch (*restriction) {
      case ConversionRestrictionKind::TupleToTuple:
        result = matchTupleTypes(constraint.getFirstType()->castTo<TupleType>(),
                                 constraint.getSecondType()
                                   ->castTo<TupleType>(),
                                 matchKind, TMF_GenerateConstraints,
                                 constraint.getLocator());
        break;

      case ConversionRestrictionKind::ScalarToTuple:
        result = matchScalarToTupleTypes(constraint.getFirstType(),
                                         constraint.getSecondType()
                                           ->castTo<TupleType>(),
                                         matchKind, TMF_GenerateConstraints,
                                         constraint.getLocator());
        break;

      case ConversionRestrictionKind::TupleToScalar:
        result = matchTupleToScalarTypes(constraint.getFirstType()
                                           ->castTo<TupleType>(),
                                         constraint.getSecondType(),
                                         matchKind, TMF_GenerateConstraints,
                                         constraint.getLocator());
        break;

      case ConversionRestrictionKind::DeepEquality:
        return matchDeepEqualityTypes(constraint.getFirstType(),
                                      constraint.getSecondType(),
                                      constraint.getLocator());

      case ConversionRestrictionKind::Superclass:
        result = matchSuperclassTypes(constraint.getFirstType(),
                                       constraint.getSecondType(),
                                       matchKind, TMF_GenerateConstraints,
                                       constraint.getLocator());
        break;

      case ConversionRestrictionKind::LValueToRValue:
        result = matchTypes(constraint.getFirstType()->getRValueType(),
                            constraint.getSecondType(),
                            matchKind, TMF_GenerateConstraints,
                            constraint.getLocator());
        break;

      case ConversionRestrictionKind::Existential:
        result = matchExistentialTypes(constraint.getFirstType(),
                                       constraint.getSecondType(),
                                       matchKind, TMF_GenerateConstraints,
                                       constraint.getLocator());
        break;

      case ConversionRestrictionKind::ValueToOptional:
        assert(constraint.getSecondType()->castTo<BoundGenericType>()->getDecl()
                 == TC.Context.getOptionalDecl());
        result = matchTypes(constraint.getFirstType(),
                            constraint.getSecondType()
                              ->castTo<BoundGenericType>()
                              ->getGenericArgs()[0],
                            matchKind, TMF_GenerateConstraints,
                            constraint.getLocator());
        break;

      case ConversionRestrictionKind::OptionalToOptional:
        assert(constraint.getFirstType()->castTo<BoundGenericType>()->getDecl()
                 == TC.Context.getOptionalDecl());
        assert(constraint.getSecondType()->castTo<BoundGenericType>()->getDecl()
                 == TC.Context.getOptionalDecl());
        result = matchTypes(constraint.getFirstType()
                              ->castTo<BoundGenericType>()
                              ->getGenericArgs()[0],
                            constraint.getSecondType()
                              ->castTo<BoundGenericType>()
                              ->getGenericArgs()[0],
                            matchKind, TMF_GenerateConstraints,
                            constraint.getLocator());
        break;
          
      case ConversionRestrictionKind::User:
        assert(constraint.getKind() >= ConstraintKind::Conversion);
        result = tryUserConversion(*this, constraint.getFirstType(),
                                   ConstraintKind::Subtype,
                                   constraint.getSecondType(),
                                   constraint.getLocator());
        break;
      }

      // If we actually solved something, record what we did.
      switch(result) {
      case SolutionKind::Error:
      case SolutionKind::Unsolved:
        break;

      case SolutionKind::Solved:
        assert(solverState && "Can't record restriction without solver state");
        if (constraint.getKind() >= ConstraintKind::Conversion) {
          solverState->constraintRestrictions.push_back(
              std::make_tuple(constraint.getFirstType(),
                              constraint.getSecondType(), *restriction));
        }
        break;
      }

      return result;
    }

    return matchTypes(constraint.getFirstType(), constraint.getSecondType(),
                      matchKind,
                      TMF_None, constraint.getLocator());
  }

  case ConstraintKind::ApplicableFunction:
    return simplifyApplicableFnConstraint(constraint);

  case ConstraintKind::BindOverload:
    resolveOverload(constraint.getLocator(), constraint.getFirstType(),
                    constraint.getOverloadChoice());
    return SolutionKind::Solved;

  case ConstraintKind::Construction:
    return simplifyConstructionConstraint(constraint.getSecondType(),
                                          constraint.getFirstType(),
                                          TMF_None,
                                          constraint.getLocator());

  case ConstraintKind::ConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    return simplifyConformsToConstraint(
             constraint.getFirstType(),
             constraint.getProtocol(),
             constraint.getLocator(),
             constraint.getKind() == ConstraintKind::SelfObjectOfProtocol);

  case ConstraintKind::CheckedCast:
    return simplifyCheckedCastConstraint(constraint.getFirstType(),
                                         constraint.getSecondType(),
                                         constraint.getLocator());

  case ConstraintKind::ValueMember:
  case ConstraintKind::TypeMember:
    return simplifyMemberConstraint(constraint);

  case ConstraintKind::Archetype:
    return simplifyArchetypeConstraint(constraint);

  case ConstraintKind::Class:
    return simplifyClassConstraint(constraint);

  case ConstraintKind::DynamicLookupValue:
    return simplifyDynamicLookupConstraint(constraint);

  case ConstraintKind::Conjunction:
    // Process all of the constraints in the conjunction.
    for (auto con : constraint.getNestedConstraints()) {
      addConstraint(con);
      if (failedConstraint)
        return SolutionKind::Error;
    }
    return SolutionKind::Solved;

  case ConstraintKind::Disjunction:
    // Disjunction constraints are never solved here.
    return SolutionKind::Unsolved;
  }
}
