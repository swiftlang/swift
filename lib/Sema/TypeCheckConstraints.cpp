//===--- TypeCheckConstraints.cpp - Constraint-based Type Checking --------===//
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
// This file implements constraint-based type checking, including type
// inference.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "TypeChecker.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeCheckerDebugConsumer.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include <iterator>
#include <map>
#include <memory>
#include <utility>
#include <tuple>

using namespace swift;
using namespace constraints;
using llvm::SmallPtrSet;

//===--------------------------------------------------------------------===//
// Type variable implementation.
//===--------------------------------------------------------------------===//
#pragma mark Type variable implementation

void TypeVariableType::Implementation::print(llvm::raw_ostream &OS) {
  getTypeVariable()->print(OS, PrintOptions());
}

SavedTypeVariableBinding::SavedTypeVariableBinding(TypeVariableType *typeVar)
  : TypeVar(typeVar), ParentOrFixed(typeVar->getImpl().ParentOrFixed),
    Options(typeVar->getImpl().Options){ }

void SavedTypeVariableBinding::restore() {
  TypeVar->getImpl().ParentOrFixed = ParentOrFixed;
  TypeVar->getImpl().Options = Options;
}

ArchetypeType *TypeVariableType::Implementation::getArchetype() const {
  // Check whether we have a path that terminates at an archetype locator.
  if (!locator || locator->getPath().empty() ||
      locator->getPath().back().getKind() != ConstraintLocator::Archetype)
    return nullptr;

  // Retrieve the archetype.
  return locator->getPath().back().getArchetype();
}

// Only allow allocation of resolved overload set list items using the
// allocator in ASTContext.
void *ResolvedOverloadSetListItem::operator new(size_t bytes,
                                                ConstraintSystem &cs,
                                                unsigned alignment) {
  return cs.getAllocator().Allocate(bytes, alignment);
}

void *operator new(size_t bytes, ConstraintSystem& cs,
                   size_t alignment) {
  return cs.getAllocator().Allocate(bytes, alignment);
}

ConstraintSystem::ConstraintSystem(TypeChecker &tc, DeclContext *dc)
  : TC(tc), DC(dc), Arena(tc.Context, Allocator) {
  assert(DC && "context required");
}

ConstraintSystem::~ConstraintSystem() { }

bool ConstraintSystem::hasFreeTypeVariables() {
  // Look for any free type variables.
  for (auto tv : TypeVariables) {
    if (!tv->getImpl().hasRepresentativeOrFixed()) {
      return true;
    }
  }
  
  return false;
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
  if (auto metaTy = instanceTy->getAs<MetaTypeType>())
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

    if (!simplifyExisting && solverState) {
      solverState->generatedConstraints.push_back(constraint);
    }

    return false;

  case SolutionKind::Solved:
    // This constraint has already been solved; there is nothing more
    // to do.
    if (TC.getLangOpts().DebugConstraintSolver && !solverState)
      SolvedConstraints.push_back(constraint);

    // Record solved constraint.
    if (solverState) {
      solverState->retiredConstraints.push_back(constraint);
      if (!simplifyExisting)
        solverState->generatedConstraints.push_back(constraint);
    }
    return true;

  case SolutionKind::Unsolved:
    // We couldn't solve this constraint; add it to the pile.
    if (!isExternallySolved)
      Constraints.push_back(constraint);

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

Type ConstraintSystem::openType(
       Type startingType,
       ArrayRef<ArchetypeType *> archetypes,
       llvm::DenseMap<CanType, TypeVariableType *> &replacements,
       DeclContext *dc,
       bool skipProtocolSelfConstraint) {
  class GetTypeVariable {
    ConstraintSystem &CS;
    llvm::DenseMap<CanType, TypeVariableType *> &Replacements;

    /// The type variables introduced for (base type, associated type) pairs.
    llvm::DenseMap<std::pair<CanType, AssociatedTypeDecl *>,
                   TypeVariableType *>
      MemberReplacements;

  public:
    GetTypeVariable(ConstraintSystem &cs,
                    llvm::DenseMap<CanType, TypeVariableType *> &replacements)
      : CS(cs), Replacements(replacements) { }

    TypeVariableType *operator()(ArchetypeType *archetype) const {
      // Check whether we already have a replacement for this archetype.
      auto known = Replacements.find(archetype->getCanonicalType());
      if (known != Replacements.end())
        return known->second;

      // Create a new type variable to replace this archetype.
      // FIXME: Path to this declaration being opened, then to the archetype.
      auto tv = CS.createTypeVariable(
                  CS.getConstraintLocator((Expr *)nullptr,
                                          LocatorPathElt(archetype)),
                  TVO_PrefersSubtypeBinding);

      // If there is a superclass for the archetype, add the appropriate
      // trivial subtype requirement on the type variable.
      if (auto superclass = archetype->getSuperclass()) {
        CS.addConstraint(ConstraintKind::TrivialSubtype, tv, superclass);
      }

      // The type variable must be convertible of the composition of all of
      // its protocol conformance requirements, i.e., it must conform to
      // each of those protocols.
      auto conformsTo = archetype->getConformsTo();
      if (!conformsTo.empty()) {
        // FIXME: Can we do this more efficiently, since we know that the
        // protocol list has already been minimized?
        for (auto protocol : conformsTo) {
          CS.addConstraint(ConstraintKind::ConformsTo, tv,
                           protocol->getDeclaredType());
        }
      }

      // Record the type variable that corresponds to this archetype.
      Replacements[archetype->getCanonicalType()] = tv;

      // Build archetypes for each of the nested types.
      for (auto nested : archetype->getNestedTypes()) {
        auto nestedTv = (*this)(nested.second);
        CS.addTypeMemberConstraint(tv, nested.first, nestedTv);
      }

      return tv;
    }

    TypeVariableType *operator()(Type base, AssociatedTypeDecl *member) {
      auto known = MemberReplacements.find({base->getCanonicalType(), member});
      if (known != MemberReplacements.end())
        return known->second;

      auto archetype =base->castTo<TypeVariableType>()->getImpl().getArchetype()
                        ->getNestedType(member->getName());
      auto tv = CS.createTypeVariable(CS.getConstraintLocator(
                                        (Expr *)nullptr,
                                        LocatorPathElt(archetype)),
                                      TVO_PrefersSubtypeBinding);
      MemberReplacements[{base->getCanonicalType(), member}] = tv;
      CS.addTypeMemberConstraint(base, member->getName(), tv);

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
  } getTypeVariable{*this, replacements};

  // Create type variables for each archetype we're opening.
  for (auto archetype : archetypes)
    (void)getTypeVariable(archetype);

  std::function<Type(Type)> replaceArchetypes;
  replaceArchetypes = [&](Type type) -> Type {
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
      auto base = replaceArchetypes(dependentMember->getBase());
      auto result = getTypeVariable(base, dependentMember->getAssocType());
      replacements[dependentMember->getCanonicalType()] = result;
      return result;
    }

    // Create type variables for all of the archetypes in a polymorphic
    // function type.
    if (auto polyFn = type->getAs<PolymorphicFunctionType>()) {
      for (auto archetype : polyFn->getAllArchetypes())
        (void)getTypeVariable(archetype);

      // Transform the input and output types.
      Type inputTy = TC.transformType(polyFn->getInput(), replaceArchetypes);
      if (!inputTy)
        return Type();

      Type resultTy = TC.transformType(polyFn->getResult(), replaceArchetypes);
      if (!resultTy)
        return Type();

      // Build the resulting (non-polymorphic) function type.
      return FunctionType::get(inputTy, resultTy, TC.Context);
    }

    // Create type variables for all of the parameters in a generic function
    // type.
    if (auto genericFn = type->getAs<GenericFunctionType>()) {
      // Create the type variables for the generic parameters.
      for (auto gp : genericFn->getGenericParams()) {
        ArchetypeType *archetype = ArchetypeBuilder::mapTypeIntoContext(dc, gp)
                                     ->castTo<ArchetypeType>();
        auto typeVar = createTypeVariable(getConstraintLocator(
                                            (Expr *)nullptr,
                                            LocatorPathElt(archetype)),
                                          TVO_PrefersSubtypeBinding);
        replacements[gp->getCanonicalType()] = typeVar;
      }

      // Add the requirements as constraints.
      for (auto req : genericFn->getRequirements()) {
        switch (req.getKind()) {
        case RequirementKind::Conformance: {
          auto subjectTy = TC.transformType(req.getFirstType(),
                                            replaceArchetypes);
          if (auto proto = req.getSecondType()->getAs<ProtocolType>()) {
            if (!skipProtocolSelfConstraint ||
                !isa<ProtocolDecl>(dc->getParent()) ||
                !isProtocolSelfType(req.getFirstType())) {
              addConstraint(ConstraintKind::ConformsTo, subjectTy,
                            proto);
            }
          } else
            addConstraint(ConstraintKind::Subtype, subjectTy,
                          req.getSecondType());
          break;
        }

        case RequirementKind::SameType: {
          auto firstTy = TC.transformType(req.getFirstType(),
                                          replaceArchetypes);
          auto secondTy = TC.transformType(req.getSecondType(),
                                           replaceArchetypes);
          addConstraint(ConstraintKind::Bind, firstTy, secondTy);
          break;
        }

        case RequirementKind::ValueWitnessMarker:
          break;
        }
      }

      // Transform the input and output types.
      Type inputTy = TC.transformType(genericFn->getInput(), replaceArchetypes);
      if (!inputTy)
        return Type();

      Type resultTy = TC.transformType(genericFn->getResult(),
                                       replaceArchetypes);
      if (!resultTy)
        return Type();

      // Build the resulting (non-generic) function type.
      return FunctionType::get(inputTy, resultTy, TC.Context);
    }

    // Open up unbound generic types, turning them into bound generic
    // types with type variables for each parameter.
    if (auto unbound = type->getAs<UnboundGenericType>()) {
      auto parentTy = unbound->getParent();
      if (parentTy)
        parentTy = TC.transformType(parentTy, replaceArchetypes);

      auto unboundDecl = unbound->getDecl();
      SmallVector<Type, 4> arguments;
      // Open the primary archetypes and bind them to the type parameters.
      for (auto archetype :
             unboundDecl->getGenericParams()->getPrimaryArchetypes())
        arguments.push_back(getTypeVariable(archetype));
      // Open the secondary archetypes.
      for (auto archetype :
             unboundDecl->getGenericParams()->getAssociatedArchetypes())
        getTypeVariable(archetype);

      return BoundGenericType::get(unboundDecl, parentTy, arguments);
    }

    return type;
  };
  
  return TC.transformType(startingType, replaceArchetypes);
}

Type ConstraintSystem::openBindingType(Type type, DeclContext *dc) {
  Type result = openType(type, dc);
  // FIXME: Better way to identify Slice<T>.
  if (auto boundStruct
        = dyn_cast<BoundGenericStructType>(result.getPointer())) {
    if (!boundStruct->getParent() &&
        boundStruct->getDecl()->getName().str() == "Slice" &&
        boundStruct->getGenericArgs().size() == 1) {
      if (auto replacement = getTypeChecker().getArraySliceType(
                               SourceLoc(), boundStruct->getGenericArgs()[0])) {
        return replacement;
      }
    }
  }

  return result;
}

Type constraints::adjustLValueForReference(Type type, bool isAssignment,
                                           ASTContext &context) {
  LValueType::Qual quals = LValueType::Qual::Implicit;
  if (auto lv = type->getAs<LValueType>()) {
    // FIXME: The introduction of 'non-heap' here is an artifact of the type
    // checker's inability to model the address-of operator that carries the
    // heap bit from its input to its output while removing the 'implicit' bit.
    // When we actually apply the inferred types in a constraint system to a
    // concrete expression, the 'implicit' bits will be dropped and the
    // appropriate 'heap' bits will be re-introduced.
    return LValueType::get(lv->getObjectType(),
                           quals | lv->getQualifiers(),
                           context);
  }

  // For an assignment operator, the first parameter is an implicit inout.
  if (isAssignment) {
    if (auto funcTy = type->getAs<FunctionType>()) {
      Type inputTy;
      if (auto inputTupleTy = funcTy->getInput()->getAs<TupleType>()) {
        if (inputTupleTy->getFields().size() > 0) {
          auto &firstParam = inputTupleTy->getFields()[0];
          auto firstParamTy
            = adjustLValueForReference(firstParam.getType(), false, context);
          SmallVector<TupleTypeElt, 2> elements;
          elements.push_back(firstParam.getWithType(firstParamTy));
          elements.append(inputTupleTy->getFields().begin() + 1,
                          inputTupleTy->getFields().end());
          inputTy = TupleType::get(elements, context);
        } else {
          inputTy = funcTy->getInput();
        }
      } else {
        inputTy = adjustLValueForReference(funcTy->getInput(), false, context);
      }

      return FunctionType::get(inputTy, funcTy->getResult(),
                               funcTy->getExtInfo(),
                               context);
    }
  }

  return type;
}

bool constraints::computeTupleShuffle(TupleType *fromTuple, TupleType *toTuple,
                                      SmallVectorImpl<int> &sources,
                                      SmallVectorImpl<unsigned> &variadicArgs,
                                      bool sourceLabelsAreMandatory) {
  const int unassigned = -3;
  
  SmallVector<bool, 4> consumed(fromTuple->getFields().size(), false);
  sources.clear();
  variadicArgs.clear();
  sources.assign(toTuple->getFields().size(), unassigned);

  // Match up any named elements.
  for (unsigned i = 0, n = toTuple->getFields().size(); i != n; ++i) {
    const auto &toElt = toTuple->getFields()[i];

    // Skip unnamed elements.
    if (toElt.getName().empty())
      continue;

    // Find the corresponding named element.
    int matched = -1;
    {
      int index = 0;
      for (auto field : fromTuple->getFields()) {
        if (field.getName() == toElt.getName() && !consumed[index]) {
          matched = index;
          break;
        }
        ++index;
      }
    }
    if (matched == -1)
      continue;

    // Record this match.
    sources[i] = matched;
    consumed[matched] = true;
  }  

  // Resolve any unmatched elements.
  unsigned fromNext = 0, fromLast = fromTuple->getFields().size();
  auto skipToNextAvailableInput = [&] {
    while (fromNext != fromLast && consumed[fromNext])
      ++fromNext;
  };
  skipToNextAvailableInput();

  for (unsigned i = 0, n = toTuple->getFields().size(); i != n; ++i) {
    // Check whether we already found a value for this element.
    if (sources[i] != unassigned)
      continue;

    const auto &elt2 = toTuple->getFields()[i];

    // Variadic tuple elements match the rest of the input elements.
    if (elt2.isVararg()) {
      // Collect the remaining (unnamed) inputs.
      while (fromNext != fromLast) {
        // Labeled elements can't be adopted into varargs even if
        // they're non-mandatory.  There isn't a really strong reason
        // for this, though.
        if (fromTuple->getFields()[fromNext].hasName()) {
          return true;
        }

        variadicArgs.push_back(fromNext);
        consumed[fromNext] = true;
        skipToNextAvailableInput();
      }
      sources[i] = TupleShuffleExpr::FirstVariadic;
      break;
    }

    // If there aren't any more inputs, we can use a default argument.
    if (fromNext == fromLast) {
      if (elt2.hasInit()) {
        sources[i] = TupleShuffleExpr::DefaultInitialize;
        continue;
      }

      return true;
    }

    // Otherwise, assign this input to the next output element.

    // Complain if the input element is named and either the label is
    // mandatory or we're trying to match it with something with a
    // different label.
    if (fromTuple->getFields()[fromNext].hasName() &&
        (sourceLabelsAreMandatory || elt2.hasName())) {
      return true;
    }

    sources[i] = fromNext;
    consumed[fromNext] = true;
    skipToNextAvailableInput();
  }

  // Complain if we didn't reach the end of the inputs.
  if (fromNext != fromLast) {
    return true;
  }

  // If we got here, we should have claimed all the arguments.
  assert(std::find(consumed.begin(), consumed.end(), false) == consumed.end());
  return false;
}

static Type getFixedTypeRecursiveHelper(ConstraintSystem &cs,
                                        TypeVariableType *typeVar) {
  while (auto fixed = cs.getFixedType(typeVar)) {
    typeVar = fixed->getAs<TypeVariableType>();
    if (!typeVar)
      return fixed;
  }
  return nullptr;
}

/// \brief Retrieve the fixed type for this type variable, looking through a
/// chain of type variables to get at the underlying type.
static Type getFixedTypeRecursive(ConstraintSystem &cs,
                                  Type type, TypeVariableType *&typeVar) {
  auto desugar = type->getDesugaredType();
  typeVar = desugar->getAs<TypeVariableType>();
  if (typeVar) {
    if (auto fixed = getFixedTypeRecursiveHelper(cs, typeVar)) {
      type = fixed;
      typeVar = nullptr;
    }
  }
  return type;
}

// A variable or subscript is settable if:
// - its base type (the type of the 'a' in 'a[n]' or 'a.b') either has
//   reference semantics or has value semantics and is settable, AND
// - the 'var' or 'subscript' decl provides a setter
template<typename SomeValueDecl>
static LValueType::Qual settableQualForDecl(Type baseType,
                                            SomeValueDecl *decl) {
  if (decl->isSettableOnBase(baseType))
    return LValueType::Qual(0);
  return LValueType::Qual::NonSettable;
}

std::pair<Type, Type> ConstraintSystem::getTypeOfReference(ValueDecl *value,
                                                           bool isTypeReference,
                                                           bool isSpecialized) {
  if (value->getDeclContext()->isTypeContext() && isa<FuncDecl>(value)) {
    // Unqualified lookup can find operator names within nominal types.
    auto func = cast<FuncDecl>(value);
    assert(func->isOperator() && "Lookup should only find operators");

    auto openedType = openType(func->getInterfaceType(), func);
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
    type = openType(type, value->getInnermostDeclContext());

    // If it's a type reference, we're done.
    if (isTypeReference)
      return { type, type };

    // If it's a value reference, refer to the metatype.
    type = MetaTypeType::get(type, getASTContext());
    return { type, type };
  }

  // Determine the type of the value, opening up that type if necessary.
  Type valueType = TC.getUnopenedTypeOfReference(value);

  // If the declaration has an interface type, use it.
  if (auto interfaceTy = value->getInterfaceType()) {
    valueType = interfaceTy;
  }

  // Up and adjust the type of the reference.
  valueType = adjustLValueForReference(openType(
                                         valueType,
                                         value->getInnermostDeclContext()),
                                       value->getAttrs().isAssignment(),
                                       TC.Context);
  return { valueType, valueType };
}

/// \brief Retrieve the substituted type when replacing an archetype
/// in the type of a protocol member with an actual type.
static Type
getTypeForArchetype(ConstraintSystem &cs, ArchetypeType *archetype,
                    llvm::DenseMap<ArchetypeType *, Type> &mappedTypes) {
  // If we've already seen this archetype, return it.
  auto known = mappedTypes.find(archetype);
  if (known != mappedTypes.end())
    return known->second;

  // Get the type for the parent archetype.
  Type parentTy = getTypeForArchetype(cs, archetype->getParent(),
                                      mappedTypes);

  // Look for this member type.
  // FIXME: Ambiguity check.
  auto &tc = cs.getTypeChecker();
  LookupTypeResult lookup = tc.lookupMemberType(parentTy, archetype->getName(),
                                                cs.DC);
  assert(lookup.size() == 1 && "Couldn't find archetype for member lookup");
  auto type = lookup.front().second;
  mappedTypes[archetype] = type;
  return type;
}

Type ConstraintSystem::openTypeOfContext(
       DeclContext *dc,
       llvm::DenseMap<CanType, TypeVariableType *> &replacements,
       GenericParamList **genericParams) {
  GenericParamList *dcGenericParams = nullptr;
  Type result;
  if (auto nominalOwner = dyn_cast<NominalTypeDecl>(dc)) {
    result = nominalOwner->getDeclaredTypeInContext();
    dcGenericParams = nominalOwner->getGenericParamsOfContext();
  } else {
    auto extensionOwner = cast<ExtensionDecl>(dc);
    auto extendedTy = extensionOwner->getExtendedType();
    if (auto nominal = extendedTy->getAs<NominalType>()) {
      result = nominal->getDecl()->getDeclaredTypeInContext();
      dcGenericParams = nominal->getDecl()->getGenericParamsOfContext();
    } else if (auto bound = extendedTy->getAs<BoundGenericType>()) {
      result = bound->getDecl()->getDeclaredTypeInContext();
      dcGenericParams = bound->getDecl()->getGenericParamsOfContext();
    } else
      llvm_unreachable("unknown owner for type member");
  }

  // Save the generic parameters for the caller.
  if (genericParams)
    *genericParams = dcGenericParams;

  // If the owner is not specialized, we're done.
  if (!result->isSpecialized())
    return result;

  // Open up the types in the owner.
  SmallVector<ArchetypeType *, 4> allOpenArcheTypes;
  ArrayRef<ArchetypeType *> openArchetypes;
  if (dcGenericParams) {
    openArchetypes = dcGenericParams->getAllArchetypes();

    // If we have multiple levels, open them now.
    if (dcGenericParams->getOuterParameters()) {
      for (auto gp = dcGenericParams; gp; gp = gp->getOuterParameters()) {
        allOpenArcheTypes.append(gp->getAllArchetypes().begin(),
                                 gp->getAllArchetypes().end());
      }
      openArchetypes = allOpenArcheTypes;
    }
  }

  return openType(result, openArchetypes, replacements, dc);
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

std::pair<Type, Type>
ConstraintSystem::getTypeOfMemberReference(Type baseTy, ValueDecl *value,
                                           bool isTypeReference,
                                           bool isDynamicResult) {
  // Figure out the instance type used for the base.
  TypeVariableType *baseTypeVar = nullptr;
  Type baseObjTy = getFixedTypeRecursive(*this, baseTy, baseTypeVar)
                     ->getRValueType();
  bool isInstance = true;
  if (auto baseMeta = baseObjTy->getAs<MetaTypeType>()) {
    baseObjTy = baseMeta->getInstanceType();
    isInstance = false;
  }

  // If the base is a module type, just use the type of the decl.
  if (baseObjTy->is<ModuleType>()) {
    return getTypeOfReference(value, isTypeReference, /*isSpecialized=*/false);
  }

  // Open up a generic member via its generic function type.
  if (!isDynamicResult && !isTypeReference) {
    if (auto interfaceTy = value->getInterfaceType()) {
      if (interfaceTy->is<GenericFunctionType>()) {
        return getTypeOfMethodReference(baseTy, value);
      }
    }
  }

  // The types that have been opened up and replaced with type variables.
  llvm::DenseMap<CanType, TypeVariableType *> replacements;

  // Figure out the type of the owner.
  Type ownerTy = openTypeOfContext(value->getDeclContext(), replacements,
                                   nullptr);

  if (!isDynamicResult) {
    addSelfConstraint(*this, baseObjTy, ownerTy);
  }

  // Determine the type of the member.
  Type type;
  if (isTypeReference) {
    type = cast<TypeDecl>(value)->getDeclaredType();
  } else if (auto subscript = dyn_cast<SubscriptDecl>(value)) {
    // The type of a subscript operation is a function type mapping
    // from the indices to the element type.
    // If this is a dynamic lookup, the result type is optional;
    // otherwise, it's an lvalue.
    auto resultTy = subscript->getElementType();
    if (isDynamicResult)
      resultTy = OptionalType::get(resultTy, TC.Context);
    else 
      resultTy = LValueType::get(resultTy,
                                 LValueType::Qual::DefaultForMemberAccess|
                                 settableQualForDecl(baseTy, subscript),
                                 TC.Context);
    type = FunctionType::get(subscript->getIndices()->getType(), resultTy,
                             TC.Context);
  } else {
    type = TC.getUnopenedTypeOfReference(value, baseTy);
  }

  // If the declaration is a protocol member, we may have more substitutions to
  // perform.
  if (auto ownerProtoTy = ownerTy->getAs<ProtocolType>()) {
    // Turn the outer polymorphic function type into a monomorphic function
    // type, because the caller always handles the Self parameter explicitly.
    if (auto polyFn = type->getAs<PolymorphicFunctionType>()) {
      type = FunctionType::get(polyFn->getInput(), polyFn->getResult(),
                               polyFn->getExtInfo(), TC.Context);
    }

    // For a member of an archetype, substitute the base type for the 'Self'
    // type.
    if (baseObjTy->is<ArchetypeType>()) {
      auto selfArchetype = ownerProtoTy->getDecl()->getSelf()->getArchetype();

      llvm::DenseMap<ArchetypeType *, Type> mappedTypes;
      mappedTypes[selfArchetype] = baseObjTy;
      type = TC.transformType(type,
               [&](Type type) -> Type {
                 if (auto archetype = type->getAs<ArchetypeType>()) {
                   return getTypeForArchetype(*this, archetype, mappedTypes);
                 }
                 
                 if (auto polyTy = type->getAs<PolymorphicFunctionType>()) {
                   // Preserve generic method archetypes.
                   for (auto archetype : polyTy->getAllArchetypes())
                     mappedTypes[archetype] = archetype;
                 }

                 return type;
               });
    } else if (!baseObjTy->isExistentialType() &&
               !baseObjTy->hasTypeVariable()) {
      // When we have an associated type and the base type conforms to the
      // given protocol, use the type witness directly.
      ProtocolConformance *conformance = nullptr;
      if (TC.conformsToProtocol(baseObjTy, ownerProtoTy->getDecl(), DC,
                                &conformance)) {
        // FIXME: Eventually, deal with default function/property definitions.
        if (auto assocType = dyn_cast<AssociatedTypeDecl>(value)) {
          type = conformance->getTypeWitness(assocType).Replacement;
        }
      }
    }
  }

  type = openType(type, { }, replacements, value->getInnermostDeclContext());

  // Skip the 'self' argument if it's already been bound by the base.
  if (auto func = dyn_cast<FuncDecl>(value)) {
    if (func->isStatic() || isInstance) {
      type = type->castTo<AnyFunctionType>()->getResult();
    } else if (isDynamicResult) {
      // For a dynamic result referring to an instance function through
      // an object of metatype type, replace the 'Self' parameter with
      // a DynamicLookup member.
      auto funcTy = type->castTo<AnyFunctionType>();
      Type resultTy = funcTy->getResult();
      Type inputTy = TC.getProtocol(SourceLoc(),
                                    KnownProtocolKind::DynamicLookup)
                       ->getDeclaredTypeOfContext();
      type = FunctionType::get(inputTy, resultTy, funcTy->getExtInfo(),
                               TC.Context);
    }
  } else if (isa<ConstructorDecl>(value) || isa<EnumElementDecl>(value)) {
    type = type->castTo<AnyFunctionType>()->getResult();
  }


  return { Type(),
           adjustLValueForReference(type, value->getAttrs().isAssignment(),
                                    TC.Context) };
}

std::pair<Type, Type>
ConstraintSystem::getTypeOfMethodReference(Type baseTy, ValueDecl *value) {
  // Figure out the declaration context to use when opening this type.
  DeclContext *dc = value->getDeclContext();
  if (auto func = dyn_cast<AbstractFunctionDecl>(value))
    dc = func;

  // Open the type of the generic function.
  auto openedType = openType(value->getInterfaceType(), dc,
                             /*skipProtocolSelfConstraint=*/true);

  // Figure out the instance type used for the base.
  TypeVariableType *baseTypeVar = nullptr;
  Type baseObjTy = getFixedTypeRecursive(*this, baseTy, baseTypeVar)
                     ->getRValueType();
  bool isInstance = true;
  if (auto baseMeta = baseObjTy->getAs<MetaTypeType>()) {
    baseObjTy = baseMeta->getInstanceType();
    isInstance = false;
  }

  auto openedFnType = openedType->castTo<FunctionType>();

  // Constraint the 'self' object type.
  auto selfObjTy = openedFnType->getInput()->getRValueInstanceType();
  if (isa<ProtocolDecl>(value->getDeclContext())) {
    // For a protocol, substitute the base object directly. We don't need a
    // conformance constraint because we wouldn't have found the declaration
    // if it didn't conform.
    addConstraint(ConstraintKind::Equal, baseObjTy, selfObjTy);
  } else {
    addSelfConstraint(*this, baseObjTy, selfObjTy);
  }

  // Compute the type of the reference.
  Type type = openedType;

  // For a constructor, enum element, static method, or an instance method
  // referenced through an instance, we've consumed the curried 'self'
  // already.
  if (isa<ConstructorDecl>(value) || isa<EnumElementDecl>(value) ||
      (isa<FuncDecl>(value) && cast<FuncDecl>(value)->isStatic()) ||
      isInstance)
    type = openedFnType->getResult();

  return { openedType, type };
}

void ConstraintSystem::addOverloadSet(Type boundType,
                                      ArrayRef<OverloadChoice> choices,
                                      ConstraintLocator *locator) {
  assert(!choices.empty() && "Empty overload set");

  SmallVector<Constraint *, 4> overloads;
  for (auto choice : choices) {
    overloads.push_back(new (*this) Constraint(boundType, choice, locator));
  }
  addConstraint(Constraint::createDisjunction(*this, overloads,
                                              locator));
}

Expr *ConstraintLocatorBuilder::trySimplifyToExpr() const {
  SmallVector<LocatorPathElt, 4> pathBuffer;
  Expr *anchor = getLocatorParts(pathBuffer);
  ArrayRef<LocatorPathElt> path = pathBuffer;

  Expr *targetAnchor;
  SmallVector<LocatorPathElt, 4> targetPathBuffer;
  SourceRange range1, range2;

  simplifyLocator(anchor, path, targetAnchor, targetPathBuffer, range1, range2);
  return (path.empty() ? anchor : nullptr);
}

bool constraints::hasMandatoryTupleLabels(Expr *e) {
  return isa<TupleExpr>(e->getSemanticsProvidingExpr());
}

static bool hasMandatoryTupleLabels(const ConstraintLocatorBuilder &locator) {
  if (Expr *e = locator.trySimplifyToExpr())
    return hasMandatoryTupleLabels(e);
  return false;
}

//===--------------------------------------------------------------------===//
// Constraint simplification
//===--------------------------------------------------------------------===//
#pragma mark Constraint simplification

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

  assert(kind == TypeMatchKind::Conversion);

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
    switch (matchTypes(elt1.getType(), elt2.getType(),
                       TypeMatchKind::Conversion, subFlags,
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
      switch (matchTypes(tuple1->getElementType(idx1),
                         eltType2, TypeMatchKind::Conversion, subFlags,
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
  // An [auto_closure] function type can be a subtype of a
  // non-[auto_closure] function type.
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

  // A [noreturn] function type can be a subtype of a non-[noreturn] function
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
  // FIXME: Should allow other conversions as well.
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
  }

  llvm_unreachable("unhandled type matching kind");
}

/// Determine whether we should attempt a user-defined conversion.
static bool shouldTryUserConversion(ConstraintSystem &cs, Type type) {

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
         "Construction/conversion constraints create potential cycles");

  // If this isn't a type that can have user-defined conversions, there's
  // nothing to do.
  if (!type->getNominalOrBoundGenericNominal() && !type->is<ArchetypeType>())
    return ConstraintSystem::SolutionKind::Unsolved;

  // If there are no user-defined conversions, there's nothing to do.
  // FIXME: lame name!
  auto &ctx = cs.getASTContext();
  auto name = ctx.getIdentifier("__conversion");
  if (!cs.lookupMember(type, name))
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

  // The conversion function will have function type TI -> TO, for fresh
  // type variables TI and TO.
  cs.addValueMemberConstraint(type, name,
                              FunctionType::get(inputTV, outputTV, ctx),
                              memberLocator);

  // A conversion function must accept an empty parameter list ().
  // Note: This should never fail, because the declaration checker
  // should ensure that conversions have no non-defaulted parameters.
  cs.addConstraint(ConstraintKind::Conversion, TupleType::getEmpty(ctx),
                   inputTV, cs.getConstraintLocator(locator));

  // Relate the output of the conversion function to the other type, using
  // the provided constraint kind.
  cs.addConstraint(kind, outputTV, otherType,
                   cs.getConstraintLocator(
                     locator.withPathElement(
                       ConstraintLocator::ConversionResult)));

  return ConstraintSystem::SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchTypes(Type type1, Type type2, TypeMatchKind kind,
                             unsigned flags,
                             ConstraintLocatorBuilder locator) {
  // If we have type variables that have been bound to fixed types, look through
  // to the fixed type.
  TypeVariableType *typeVar1;
  type1 = getFixedTypeRecursive(*this, type1, typeVar1);
  auto desugar1 = type1->getDesugaredType();

  TypeVariableType *typeVar2;
  type2 = getFixedTypeRecursive(*this, type2, typeVar2);
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
        return typeVar1 == typeVar2? SolutionKind::Solved
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

    case TypeKind::MetaType: {
      auto meta1 = cast<MetaTypeType>(desugar1);
      auto meta2 = cast<MetaTypeType>(desugar2);

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

    case TypeKind::Function: {
      auto func1 = cast<FunctionType>(desugar1);
      auto func2 = cast<FunctionType>(desugar2);
      return matchFunctionTypes(func1, func2, kind, flags, locator);
    }

    case TypeKind::PolymorphicFunction:
    case TypeKind::GenericFunction:
      llvm_unreachable("Polymorphic function type should have been opened");

    case TypeKind::Array: {
      auto array1 = cast<ArrayType>(desugar1);
      auto array2 = cast<ArrayType>(desugar2);
      return matchTypes(array1->getBaseType(), array2->getBaseType(),
                        TypeMatchKind::SameType, subFlags,
                        locator.withPathElement(
                          ConstraintLocator::ArrayElementType));
    }

    case TypeKind::ProtocolComposition:
      // Existential types handled below.
      break;

    case TypeKind::LValue: {
      auto lvalue1 = cast<LValueType>(desugar1);
      auto lvalue2 = cast<LValueType>(desugar2);
      if (lvalue1->getQualifiers() != lvalue2->getQualifiers() &&
          !(kind >= TypeMatchKind::TrivialSubtype &&
            lvalue1->getQualifiers() < lvalue2->getQualifiers())) {
        // Record this failure.
        if (shouldRecordFailures()) {
          recordFailure(getConstraintLocator(locator),
                        Failure::LValueQualifiers, type1, type2);
        }

        return SolutionKind::Error;
      }

      return matchTypes(lvalue1->getObjectType(), lvalue2->getObjectType(),
                        TypeMatchKind::SameType, subFlags,
                        locator.withPathElement(
                          ConstraintLocator::ArrayElementType));
    }

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
    if (auto lvalue1 = type1->getAs<LValueType>()) {
      if (lvalue1->getQualifiers().isImplicit()) {
        potentialConversions.push_back(
          ConversionRestrictionKind::LValueToRValue);
      }
    }

    // An expression can be converted to an auto-closure function type, creating
    // an implicit closure.
    if (auto function2 = type2->getAs<FunctionType>()) {
      if (function2->isAutoClosure()) {
        return matchTypes(type1, function2->getResult(), kind, subFlags,
                          locator.withPathElement(ConstraintLocator::Load));
      }
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
  {
    BoundGenericType *boundGenericType2;
    if (concrete && kind >= TypeMatchKind::Conversion &&
        (boundGenericType2 = type2->getAs<BoundGenericType>())) {
      if (boundGenericType2->getDecl() == TC.Context.getOptionalDecl()) {
        assert(boundGenericType2->getGenericArgs().size() == 1);
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
        new (*this) Constraint(constraintKind, potential, type1, type2,
                               fixedLocator));
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

  case ConversionRestrictionKind::User:
    return tryUserConversion(*this, type1, ConstraintKind::Subtype, type2,
                              locator);
  }
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
                                   isTypeReference, isDynamicResult);
    else
      std::tie(openedFullType, refType)
        = getTypeOfReference(choice.getDecl(),
                             isTypeReference,
                             choice.isSpecialized());

    if (isDynamicResult) {
      // For a non-subscript declaration found via dynamic lookup,
      // strip off the lvalue-ness (one cannot assign to such
      // declarations) and make a reference to that declaration be
      // optional.
      //
      // Subscript declarations are handled within
      // getTypeOfMemberReference(); their result types are optional.
      if (!isa<SubscriptDecl>(choice.getDecl()))
        refType = OptionalType::get(refType->getRValueType(), TC.Context);
    } else {
      // Otherwise, adjust the lvalue type for this reference.
      bool isAssignment = choice.getDecl()->getAttrs().isAssignment();
      refType = adjustLValueForReference(refType, isAssignment,
                                         getASTContext());
    }

    break;
  }

  case OverloadChoiceKind::BaseType:
    refType = choice.getBaseType();
    break;

  case OverloadChoiceKind::FunctionReturningBaseType:
    refType = FunctionType::get(createTypeVariable(
                                  getConstraintLocator(
                                    locator,
                                    ConstraintLocator::FunctionResult),
                                  /*options=*/0),
                                choice.getBaseType(),
                                getASTContext());
    break;
  case OverloadChoiceKind::IdentityFunction:
    refType = FunctionType::get(choice.getBaseType(), choice.getBaseType(),
                                getASTContext());
    break;

  case OverloadChoiceKind::TupleIndex: {
    if (auto lvalueTy = choice.getBaseType()->getAs<LValueType>()) {
      // When the base of a tuple lvalue, the member is always an lvalue.
      auto tuple = lvalueTy->getObjectType()->castTo<TupleType>();
      refType = tuple->getElementType(choice.getTupleIndex())->getRValueType();
      refType = LValueType::get(refType, lvalueTy->getQualifiers(),
                                getASTContext());
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
  return TC.transformType(type,
                          [&](Type type) -> Type {
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
  case TypeKind::MetaType:
  case TypeKind::Function:
  case TypeKind::Array:
  case TypeKind::ProtocolComposition:
  case TypeKind::LValue:
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
                           FunctionType::get(tv, valueType, context),
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
  while (true) {
    TypeVariableType *typeVar;
    type = getFixedTypeRecursive(*this, type, typeVar);

    // If we hit a type variable without a fixed type, we can't
    // solve this yet.
    if (typeVar)
      return SolutionKind::Unsolved;

    auto rvalueType = type->getRValueType();
    if (rvalueType.getPointer() != type.getPointer()) {
      type = rvalueType;
      continue;
    }

    break;
  }

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
  if (auto baseObjMeta = baseObjTy->getAs<MetaTypeType>()) {
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

    // Check whether we have an 'identity' constructor.
    bool needIdentityConstructor = true;
    if (baseObjTy->getClassOrBoundGenericClass()) {
      // When we are constructing a class type, there is no coercion case
      // to consider.
      needIdentityConstructor = false;
    } else {
      // FIXME: Busted for generic types.
      for (auto constructor : ctors) {
        if (auto funcTy = constructor->getType()->getAs<FunctionType>()) {
          if ((funcTy = funcTy->getResult()->getAs<FunctionType>())) {
            // Dig out the input type.
            auto inputTy = funcTy->getInput();
            if (auto inputTupleTy = inputTy->getAs<TupleType>()) {
              int scalarIdx = inputTupleTy->getFieldForScalarInit();
              if (scalarIdx >= 0) {
                inputTy = inputTupleTy->getElementType(scalarIdx);
              }
            }

            if (inputTy->isEqual(baseObjTy)) {
              needIdentityConstructor = false;
              break;
            }
          }
        }
      }
    }
    
    // Introduce a new overload set.
    SmallVector<OverloadChoice, 4> choices;
    for (auto constructor : ctors) {
      // If the constructor is invalid, skip it.
      // FIXME: Note this as invalid, in case we don't find a solution,
      // so we don't let errors cascade further.
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

    // If we need an "identity" constructor, then add an entry in the
    // overload set for T -> T, where T is the base type. This entry acts as a
    // stand-in for conversion of the argument to T.
    if (needIdentityConstructor) {
      choices.push_back(OverloadChoice(baseTy,
                                       OverloadChoiceKind::IdentityFunction));
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
      if (result.first->isInvalid())
        continue;

      choices.push_back(OverloadChoice(baseTy, result.first,
                                       /*isSpecialized=*/false));
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
    if (isMetatype &&
        !(isa<FuncDecl>(result) ||
          isa<EnumElementDecl>(result) ||
          !result->isInstanceMember())) {
      continue;
    }

    // If we aren't looking in a metatype, ignore static functions.
    if (!isMetatype && !baseObjTy->is<ModuleType>() &&
        isa<FuncDecl>(result) && !result->isInstanceMember())
      continue;

    // If we're doing dynamic lookup into a metatype of DynamicLookup and we've
    // found an instance member, ignore it.
    if (isDynamicLookup && isMetatype && result->isInstanceMember()) {
      // FIXME: Mark as 'unavailable' somehow.
      continue;
    }

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
  if (auto lvalueTy = baseTy->getAs<LValueType>()) {
    if (lvalueTy->getQualifiers().isImplicit())
      baseTy = lvalueTy->getObjectType();
  }

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
  Type type2 = getFixedTypeRecursive(*this,constraint.getSecondType(),typeVar2);
  auto desugar2 = type2->getDesugaredType();

  // Force the right-hand side to be an rvalue.
  unsigned flags = TMF_GenerateConstraints;
  while (isa<LValueType>(desugar2)) {
    type2 = type2->castTo<LValueType>()->getObjectType();
    type2 = getFixedTypeRecursive(*this, type2, typeVar2);
    desugar2 = type2->getDesugaredType();
    flags |= TMF_GenerateConstraints;
  }

  // If the types are obviously equivalent, we're done.
  if (type1.getPointer() == desugar2)
    return SolutionKind::Solved;

  // If right-hand side is a type variable, the constraint is unsolved.
  if (typeVar2) {
    return SolutionKind::Unsolved;
  }

  // Bind the inputs and outputs.
  ConstraintLocatorBuilder locator = constraint.getLocator();
  if (desugar2->getKind() == TypeKind::Function) {
    auto func1 = type1->castTo<FunctionType>();
    auto func2 = cast<FunctionType>(desugar2);

    assert(func1->getInput()->is<TypeVariableType>() &&
           "the input of funct1 is a free variable by construction");
    assert(func1->getResult()->is<TypeVariableType>() &&
           "the output of funct1 is a free variable by construction");

    if (matchTypes(func1->getInput(), func2->getInput(),
                   TypeMatchKind::BindType, flags,
                   locator.withPathElement(
                     ConstraintLocator::FunctionArgument))
          == SolutionKind::Error)
      return SolutionKind::Error;

    if (matchTypes(func1->getResult(), func2->getResult(),
                   TypeMatchKind::BindType,
                   flags,
                   locator.withPathElement(ConstraintLocator::FunctionResult))
          == SolutionKind::Error)
      return SolutionKind::Error;
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
  case ConstraintKind::Conversion: {
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

      case ConversionRestrictionKind::User:
        assert(constraint.getKind() == ConstraintKind::Conversion);
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
        if (constraint.getKind() == ConstraintKind::Conversion) {
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

  case ConstraintKind::ApplicableFunction: {
    return simplifyApplicableFnConstraint(constraint);
  }

  case ConstraintKind::BindOverload: {
    resolveOverload(constraint.getLocator(), constraint.getFirstType(),
                    constraint.getOverloadChoice());
    return SolutionKind::Solved;
  }

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

Type Solution::simplifyType(TypeChecker &tc, Type type) const {
  return tc.transformType(type,
           [&](Type type) -> Type {
             if (auto tvt = dyn_cast<TypeVariableType>(type.getPointer())) {
               auto known = typeBindings.find(tvt);
               assert(known != typeBindings.end());
               type = known->second;
             }

             return type;
           });
}

//===--------------------------------------------------------------------===//
// Ranking solutions
//===--------------------------------------------------------------------===//
#pragma mark Ranking solutions

/// \brief Remove the initializers from any tuple types within the
/// given type.
static Type stripInitializers(TypeChecker &tc, Type origType) {
  return tc.transformType(origType, 
           [&](Type type) -> Type {
             if (auto tupleTy = type->getAs<TupleType>()) {
               SmallVector<TupleTypeElt, 4> fields;
               for (const auto &field : tupleTy->getFields()) {
                 fields.push_back(TupleTypeElt(field.getType(),
                                               field.getName(),
                                               DefaultArgumentKind::None,
                                               field.isVararg()));
                                               
               }
               return TupleType::get(fields, tc.Context);
             }
             return type;
           });
}

///\ brief Compare two declarations for equality when they are used.
///
static bool sameDecl(Decl *decl1, Decl *decl2) {
  if (decl1 == decl2)
    return true;

  // All types considered identical.
  // FIXME: This is a hack. What we really want is to have substituted the
  // base type into the declaration reference, so that we can compare the
  // actual types to which two type declarations resolve. If those types are
  // equivalent, then it doesn't matter which declaration is chosen.
  if (isa<TypeDecl>(decl1) && isa<TypeDecl>(decl2))
    return true;
  
  if (decl1->getKind() != decl2->getKind())
    return false;

  return false;
}

/// \brief Compare two overload choices for equality.
static bool sameOverloadChoice(const OverloadChoice &x,
                               const OverloadChoice &y) {
  if (x.getKind() != y.getKind())
    return false;

  switch (x.getKind()) {
  case OverloadChoiceKind::BaseType:
  case OverloadChoiceKind::FunctionReturningBaseType:
  case OverloadChoiceKind::IdentityFunction:
    // FIXME: Compare base types after substitution?
    return true;

  case OverloadChoiceKind::Decl:
  case OverloadChoiceKind::DeclViaDynamic:
    return sameDecl(x.getDecl(), y.getDecl());

  case OverloadChoiceKind::TypeDecl:
    // FIXME: Compare types after substitution?
    return sameDecl(x.getDecl(), y.getDecl());

  case OverloadChoiceKind::TupleIndex:
    return x.getTupleIndex() == y.getTupleIndex();
  }
}

/// Compare two declarations to determine whether one is a witness of the other.
static Comparison compareWitnessAndRequirement(TypeChecker &tc, DeclContext *dc,
                                               ValueDecl *decl1,
                                               ValueDecl *decl2) {
  // We only have a witness/requirement pair if exactly one of the declarations
  // comes from a protocol.
  auto proto1 = dyn_cast<ProtocolDecl>(decl1->getDeclContext());
  auto proto2 = dyn_cast<ProtocolDecl>(decl2->getDeclContext());
  if ((bool)proto1 == (bool)proto2)
    return Comparison::Unordered;

  // Figure out the protocol, requirement, and potential witness.
  ProtocolDecl *proto;
  ValueDecl *req;
  ValueDecl *potentialWitness;
  if (proto1) {
    proto = proto1;
    req = decl1;
    potentialWitness = decl2;
  } else {
    proto = proto2;
    req = decl2;
    potentialWitness = decl1;
  }

  // Cannot compare type declarations this way.
  // FIXME: Use the same type-substitution approach as lookupMemberType.
  if (isa<TypeDecl>(req))
    return Comparison::Unordered;

  if (!potentialWitness->getDeclContext()->isTypeContext())
    return Comparison::Unordered;

  // Determine whether the type of the witness's context conforms to the
  // protocol.
  auto owningType
    = potentialWitness->getDeclContext()->getDeclaredTypeInContext();
  ProtocolConformance *conformance = nullptr;
  if (!tc.conformsToProtocol(owningType, proto, dc, &conformance))
    return Comparison::Unordered;

  // If the witness and the potential witness are not the same, there's no
  // ordering here.
  if (conformance->getWitness(req).getDecl() != potentialWitness)
    return Comparison::Unordered;

  // We have a requirement/witness match.
  return proto1? Comparison::Worse : Comparison::Better;
}

/// \brief Determine whether the first declaration is as "specialized" as
/// the second declaration.
///
/// "Specialized" is essentially a form of subtyping, defined below.
static bool isDeclAsSpecializedAs(TypeChecker &tc, DeclContext *dc,
                                  ValueDecl *decl1, ValueDecl *decl2) {
  // If the kinds are different, there's nothing we can do.
  // FIXME: This is wrong for type declarations.
  if (decl1->getKind() != decl2->getKind())
    return false;

  // A witness is always more specialized than the requirement it satisfies.
  switch (compareWitnessAndRequirement(tc, dc, decl1, decl2)) {
  case Comparison::Unordered:
    break;

  case Comparison::Better:
    return true;

  case Comparison::Worse:
    return false;
  }

  Type type1;
  Type type2;

  if (auto func1 = dyn_cast<FuncDecl>(decl1)) {
    auto func2 = cast<FuncDecl>(decl2);
    type1 = func1->getType();
    type2 = func2->getType();

    // Skip the 'self' parameter.
    // FIXME: Might not actually be what we want to do. Think about this more.
    if (func1->getDeclContext()->isTypeContext())
      type1 = type1->castTo<AnyFunctionType>()->getResult();
    if (func2->getDeclContext()->isTypeContext())
      type2 = type2->castTo<AnyFunctionType>()->getResult();
  } else if (auto constructor1 = dyn_cast<ConstructorDecl>(decl1)) {
    auto constructor2 = cast<ConstructorDecl>(decl2);
    type1 = constructor1->getType();
    type2 = constructor2->getType();

    // Skip the 'self' parameter.
    // FIXME: Might not actually be what we want to do. Think about this more.
    type1 = type1->castTo<AnyFunctionType>()->getResult();
    type2 = type2->castTo<AnyFunctionType>()->getResult();
  } else if (auto subscript1 = dyn_cast<SubscriptDecl>(decl1)) {
    auto subscript2 = cast<SubscriptDecl>(decl2);
    type1 = subscript1->getType();
    type2 = subscript2->getType();
  } else {
    // FIXME: Deal with variables, types, etc.
    return false;
  }

  // If one is polymorphic and the other is not, prefer the monomorphic result.
  // FIXME: Isn't this a special case of the subtype check below?
  bool poly1 = type1->is<PolymorphicFunctionType>();
  bool poly2 = type2->is<PolymorphicFunctionType>();
  if (poly1 != poly2) {
    return poly2;
  }

  // FIXME: Should be able to compare polymorphic types here.
  if (poly1 || poly2) {
    return false;
  }

  // Check whether both the input and result types of the first are
  // subtypes of the second.
  auto funcTy1 = type1->castTo<FunctionType>();
  auto funcTy2 = type2->castTo<FunctionType>();
  auto &context = tc.Context;
  return tc.isSubtypeOf(funcTy1->getInput(), funcTy2->getInput(), dc) ||
        (funcTy1->getInput()->getUnlabeledType(context)->isEqual(
           funcTy2->getInput()->getUnlabeledType(context)) &&
          tc.isSubtypeOf(funcTy1->getResult(), funcTy2->getResult(), dc));
}

Comparison TypeChecker::compareDeclarations(DeclContext *dc,
                                            ValueDecl *decl1,
                                            ValueDecl *decl2){
  bool decl1Better = isDeclAsSpecializedAs(*this, dc, decl1, decl2);
  bool decl2Better = isDeclAsSpecializedAs(*this, dc, decl2, decl1);

  if (decl1Better == decl2Better)
    return Comparison::Unordered;

  return decl1Better? Comparison::Better : Comparison::Worse;
}

SolutionCompareResult ConstraintSystem::compareSolutions(
                        ConstraintSystem &cs,
                        ArrayRef<Solution> solutions,
                        const SolutionDiff &diff,
                        unsigned idx1,
                        unsigned idx2) {
  // Whether the solutions are identical.
  bool identical = true;

  // Solution comparison uses a scoring system to determine whether one
  // solution is better than the other. Retrieve the fixed scores for each of
  // the solutions, which we'll modify with relative scoring.
  int score1 = solutions[idx1].getFixedScore();
  int score2 = solutions[idx2].getFixedScore();

  // Compare overload sets.
  for (auto &overload : diff.overloads) {
    auto choice1 = overload.choices[idx1];
    auto choice2 = overload.choices[idx2];

    // If the systems made the same choice, there's nothing interesting here.
    if (sameOverloadChoice(choice1, choice2))
      continue;

    // The two systems are not identical.
    identical = false;
    
    // If the kinds of overload choice don't match...
    if (choice1.getKind() != choice2.getKind()) {
      // The identity function beats any declaration.
      if (choice1.getKind() == OverloadChoiceKind::IdentityFunction &&
          choice2.getKind() == OverloadChoiceKind::Decl) {
        ++score1;
        continue;
      }
      if (choice1.getKind() == OverloadChoiceKind::Decl &&
          choice2.getKind() == OverloadChoiceKind::IdentityFunction) {
        ++score2;
        continue;
      }

      // A declaration found directly beats any declaration found via dynamic
      // lookup.
      if (choice1.getKind() == OverloadChoiceKind::Decl &&
          choice2.getKind() == OverloadChoiceKind::DeclViaDynamic) {
        ++score1;
        continue;
      }
      if (choice1.getKind() == OverloadChoiceKind::DeclViaDynamic &&
          choice2.getKind() == OverloadChoiceKind::Decl) {
        ++score2;
        continue;
      }

      continue;
    }

    // The kinds of overload choice match, but the contents don't.
    auto &tc = cs.getTypeChecker();
    switch (choice1.getKind()) {
    case OverloadChoiceKind::TupleIndex:
      break;

    case OverloadChoiceKind::BaseType:
    case OverloadChoiceKind::FunctionReturningBaseType:
    case OverloadChoiceKind::IdentityFunction:
      llvm_unreachable("Never considered different");

    case OverloadChoiceKind::TypeDecl:
      break;

    case OverloadChoiceKind::DeclViaDynamic:
    case OverloadChoiceKind::Decl:
      // Determine whether one declaration is more specialized than the other.
      if (isDeclAsSpecializedAs(tc, cs.DC,
                                choice1.getDecl(), choice2.getDecl()))
        ++score1;
      if (isDeclAsSpecializedAs(tc, cs.DC,
                                choice2.getDecl(), choice1.getDecl()))
        ++score2;
      break;
    }
  }

  // Compare the type variable bindings.
  auto &tc = cs.getTypeChecker();
  for (auto &binding : diff.typeBindings) {
    // If the type variable isn't one for which we should be looking at the
    // bindings, don't.
    if (!binding.typeVar->getImpl().prefersSubtypeBinding())
      continue;

    auto type1 = binding.bindings[idx1];
    auto type2 = binding.bindings[idx2];

    // Strip any initializers from tuples in the type; they aren't
    // to be compared.
    type1 = stripInitializers(cs.getTypeChecker(), type1);
    type2 = stripInitializers(cs.getTypeChecker(), type2);

    // If the types are equivalent, there's nothing more to do.
    if (type1->isEqual(type2))
      continue;
    
    // If either of the types still contains type variables, we can't
    // compare them.
    // FIXME: This is really unfortunate. More type variable sharing
    // (when it's sane) would help us do much better here.
    if (type1->hasTypeVariable() || type2->hasTypeVariable()) {
      identical = false;
      continue;
    }

    // If one type is a subtype of the other, but not vice-verse,
    // we prefer the system with the more-constrained type.
    // FIXME: Collapse this check into the second check.
    bool type1Better = tc.isSubtypeOf(type1, type2, cs.DC);
    bool type2Better = tc.isSubtypeOf(type2, type1, cs.DC);
    if (type1Better || type2Better) {
      if (type1Better)
        ++score1;
      if (type2Better)
        ++score2;

      // Prefer the unlabeled form of a type.
      auto unlabeled1 = type1->getUnlabeledType(cs.getASTContext());
      auto unlabeled2 = type2->getUnlabeledType(cs.getASTContext());
      if (unlabeled1->isEqual(unlabeled2)) {
        if (type1->isEqual(unlabeled1)) {
          ++score1;
          continue;
        }
        if (type2->isEqual(unlabeled2)) {
          ++score2;
          continue;
        }
      }

      identical = false;
      continue;
    }

    // The systems are not considered equivalent.
    identical = false;

    // If one type is convertible to of the other, but not vice-versa.
    type1Better = tc.isConvertibleTo(type1, type2, cs.DC);
    type2Better = tc.isConvertibleTo(type2, type1, cs.DC);
    if (type1Better || type2Better) {
      if (type1Better)
        ++score1;
      if (type2Better)
        ++score2;
      continue;
    }

    // A concrete type is better than an archetype.
    // FIXME: Total hack.
    if (type1->is<ArchetypeType>() != type2->is<ArchetypeType>()) {
      if (type1->is<ArchetypeType>())
        ++score2;
      else
        ++score1;
      continue;
    }
  }

  // FIXME: There are type variables and overloads not common to both solutions
  // that haven't been considered. They make the systems different, but don't
  // affect ranking. We need to handle this.

  // If the scores are different, we have a winner.
  if (score1 != score2) {
    return score1 > score2? SolutionCompareResult::Better
                          : SolutionCompareResult::Worse;
  }

  // Neither system wins; report whether they were identical or not.
  return identical? SolutionCompareResult::Identical
                  : SolutionCompareResult::Incomparable;
}

Solution *
ConstraintSystem::findBestSolution(SmallVectorImpl<Solution> &viable){
  if (viable.empty())
    return nullptr;
  if (viable.size() == 1)
    return &viable[0];

  SolutionDiff diff(viable);

  // Find a potential best.
  unsigned bestIdx = 0;
  for (unsigned i = 1, n = viable.size(); i != n; ++i) {
    switch (compareSolutions(*this, viable, diff, i, bestIdx)) {
    case SolutionCompareResult::Identical:
      // FIXME: Might want to warn about this in debug builds, so we can
      // find a way to eliminate the redundancy in the search space.
    case SolutionCompareResult::Incomparable:
    case SolutionCompareResult::Worse:
      break;

    case SolutionCompareResult::Better:
      bestIdx = i;
      break;
    }
  }

  // Make sure that our current best is better than all of the solved systems.
  for (unsigned i = 0, n = viable.size(); i != n; ++i) {
    if (i == bestIdx)
      continue;

    switch (compareSolutions(*this, viable, diff, bestIdx, i)) {
    case SolutionCompareResult::Identical:
      // FIXME: Might want to warn about this in debug builds, so we can
      // find a way to eliminate the redundancy in the search space.
      SWIFT_FALLTHROUGH;
    case SolutionCompareResult::Better:
      break;

    case SolutionCompareResult::Incomparable:
    case SolutionCompareResult::Worse:
      return nullptr;
    }
  }

  // FIXME: If we lost our best, we should minimize the set of viable
  // solutions.

  return &viable[bestIdx];
}

SolutionDiff::SolutionDiff(ArrayRef<Solution> solutions)  {
  if (solutions.size() <= 1)
    return;

  // Populate the type bindings with the first solution.
  llvm::DenseMap<TypeVariableType *, SmallVector<Type, 2>> typeBindings;
  for (auto binding : solutions[0].typeBindings) {
    typeBindings[binding.first].push_back(binding.second);
  }

  // Populate the overload choices with the first solution.
  llvm::DenseMap<ConstraintLocator *, SmallVector<OverloadChoice, 2>>
    overloadChoices;
  for (auto choice : solutions[0].overloadChoices) {
    overloadChoices[choice.first].push_back(choice.second.choice);
  }

  // Find the type variables and overload locators common to all of the
  // solutions.
  for (auto &solution : solutions.slice(1)) {
    // For each type variable bound in all of the previous solutions, check
    // whether we have a binding for this type variable in this solution.
    SmallVector<TypeVariableType *, 4> removeTypeBindings;
    for (auto &binding : typeBindings) {
      auto known = solution.typeBindings.find(binding.first);
      if (known == solution.typeBindings.end()) {
        removeTypeBindings.push_back(binding.first);
        continue;
      }

      // Add this solution's binding to the results.
      binding.second.push_back(known->second);
    }

    // Remove those type variables for which this solution did not have a
    // binding.
    for (auto typeVar : removeTypeBindings) {
      typeBindings.erase(typeVar);
    }
    removeTypeBindings.clear();

    // For each overload locator for which we have an overload choice in
    // all of the previous solutions. Check whether we have an overload choice
    // in this solution.
    SmallVector<ConstraintLocator *, 4> removeOverloadChoices;
    for (auto &overloadChoice : overloadChoices) {
      auto known = solution.overloadChoices.find(overloadChoice.first);
      if (known == solution.overloadChoices.end()) {
        removeOverloadChoices.push_back(overloadChoice.first);
        continue;
      }

      // Add this solution's overload choice to the results.
      overloadChoice.second.push_back(known->second.choice);
    }

    // Remove those overload locators for which this solution did not have
    // an overload choice.
    for (auto overloadChoice : removeOverloadChoices) {
      overloadChoices.erase(overloadChoice);
    }
  }

  // Look through the type variables that have bindings in all of the
  // solutions, and add those that have differences to the diff.
  for (auto &binding : typeBindings) {
    Type singleType;
    for (auto type : binding.second) {
      if (!singleType)
        singleType = type;
      else if (!singleType->isEqual(type)) {
        // We have a difference. Add this binding to the diff.
        this->typeBindings.push_back(
          SolutionDiff::TypeBindingDiff{
            binding.first,
            std::move(binding.second)
          });

        break;
      }
    }
  }

  // Look through the overload locators that have overload choices in all of
  // the solutions, and add those that have differences to the diff.
  for (auto &overloadChoice : overloadChoices) {
    OverloadChoice singleChoice = overloadChoice.second[0];
    for (auto choice : overloadChoice.second) {
      if (!sameOverloadChoice(singleChoice, choice)) {
        // We have a difference. Add this set of overload choices to the diff.
        this->overloads.push_back(
          SolutionDiff::OverloadDiff{
            overloadChoice.first,
            overloadChoice.second
          });
        
      }
    }
  }
}

//===--------------------------------------------------------------------===//
// High-level entry points.
//===--------------------------------------------------------------------===//

static unsigned getNumArgs(ValueDecl *value) {
  if (!isa<FuncDecl>(value)) return ~0U;

  AnyFunctionType *fnTy = value->getType()->castTo<AnyFunctionType>();
  if (value->getDeclContext()->isTypeContext())
    fnTy = fnTy->getResult()->castTo<AnyFunctionType>();
  Type argTy = fnTy->getInput();
  if (auto tuple = argTy->getAs<TupleType>()) {
    return tuple->getFields().size();
  } else {
    return 1;
  }
}

static bool matchesDeclRefKind(ValueDecl *value, DeclRefKind refKind) {
  if (value->getType()->is<ErrorType>())
    return true;

  switch (refKind) {
      // An ordinary reference doesn't ignore anything.
    case DeclRefKind::Ordinary:
      return true;

      // A binary-operator reference only honors FuncDecls with a certain type.
    case DeclRefKind::BinaryOperator:
      return (getNumArgs(value) == 2);

    case DeclRefKind::PrefixOperator:
      return (!value->getAttrs().isPostfix() && getNumArgs(value) == 1);

    case DeclRefKind::PostfixOperator:
      return (value->getAttrs().isPostfix() && getNumArgs(value) == 1);
  }
  llvm_unreachable("bad declaration reference kind");
}

/// BindName - Bind an UnresolvedDeclRefExpr by performing name lookup and
/// returning the resultant expression.  Context is the DeclContext used
/// for the lookup.
static Expr *BindName(UnresolvedDeclRefExpr *UDRE, DeclContext *Context,
                      TypeChecker &TC) {
  // Process UnresolvedDeclRefExpr by doing an unqualified lookup.
  Identifier Name = UDRE->getName();
  SourceLoc Loc = UDRE->getLoc();

  // Perform standard value name lookup.
  UnqualifiedLookup Lookup(Name, Context, &TC);

  if (!Lookup.isSuccess()) {
    TC.diagnose(Loc, diag::use_unresolved_identifier, Name);
    return new (TC.Context) ErrorExpr(Loc);
  }

  // FIXME: Need to refactor the way we build an AST node from a lookup result!

  if (Lookup.Results.size() == 1 &&
      Lookup.Results[0].Kind == UnqualifiedLookupResult::ModuleName) {
    ModuleType *MT = ModuleType::get(Lookup.Results[0].getNamedModule());
    return new (TC.Context) ModuleExpr(Loc, MT);
  }

  bool AllDeclRefs = true;
  SmallVector<ValueDecl*, 4> ResultValues;
  for (auto Result : Lookup.Results) {
    switch (Result.Kind) {
      case UnqualifiedLookupResult::MemberProperty:
      case UnqualifiedLookupResult::MemberFunction:
      case UnqualifiedLookupResult::MetatypeMember:
      case UnqualifiedLookupResult::ExistentialMember:
      case UnqualifiedLookupResult::ArchetypeMember:
      case UnqualifiedLookupResult::MetaArchetypeMember:
      case UnqualifiedLookupResult::ModuleName:
        // Types are never referenced with an implicit 'self'.
        if (!isa<TypeDecl>(Result.getValueDecl())) {
          AllDeclRefs = false;
          break;
        }

        SWIFT_FALLTHROUGH;

      case UnqualifiedLookupResult::ModuleMember:
      case UnqualifiedLookupResult::LocalDecl: {
        ValueDecl *D = Result.getValueDecl();
        if (matchesDeclRefKind(D, UDRE->getRefKind()))
          ResultValues.push_back(D);
        break;
      }
    }
  }
  if (AllDeclRefs) {
    // Diagnose uses of operators that found no matching candidates.
    if (ResultValues.empty()) {
      assert(UDRE->getRefKind() != DeclRefKind::Ordinary);
      TC.diagnose(Loc, diag::use_nonmatching_operator, Name,
                  UDRE->getRefKind() == DeclRefKind::BinaryOperator ? 0 :
                  UDRE->getRefKind() == DeclRefKind::PrefixOperator ? 1 : 2);
      return new (TC.Context) ErrorExpr(Loc);
    }

    return TC.buildRefExpr(ResultValues, Loc, UDRE->isImplicit(),
                           UDRE->isSpecialized());
  }

  ResultValues.clear();
  bool AllMemberRefs = true;
  ValueDecl *Base = 0;
  for (auto Result : Lookup.Results) {
    switch (Result.Kind) {
      case UnqualifiedLookupResult::MemberProperty:
      case UnqualifiedLookupResult::MemberFunction:
      case UnqualifiedLookupResult::MetatypeMember:
      case UnqualifiedLookupResult::ExistentialMember:
        ResultValues.push_back(Result.getValueDecl());
        if (Base && Result.getBaseDecl() != Base) {
          AllMemberRefs = false;
          break;
        }
        Base = Result.getBaseDecl();
        break;
      case UnqualifiedLookupResult::ModuleMember:
      case UnqualifiedLookupResult::LocalDecl:
      case UnqualifiedLookupResult::ModuleName:
        AllMemberRefs = false;
        break;
      case UnqualifiedLookupResult::MetaArchetypeMember:
      case UnqualifiedLookupResult::ArchetypeMember:
        // FIXME: We need to extend OverloadedMemberRefExpr to deal with this.
        llvm_unreachable("Archetype members in overloaded member references");
        break;
    }
  }

  if (AllMemberRefs) {
    Expr *BaseExpr;
    if (auto NTD = dyn_cast<NominalTypeDecl>(Base)) {
      Type BaseTy = MetaTypeType::get(NTD->getDeclaredTypeInContext(),
                                      TC.Context);
      BaseExpr = new (TC.Context) MetatypeExpr(nullptr, Loc, BaseTy);
    } else {
      BaseExpr = new (TC.Context) DeclRefExpr(Base, Loc, /*implicit=*/true);
    }
    return new (TC.Context) UnresolvedDotExpr(BaseExpr, SourceLoc(), Name, Loc,
                                              UDRE->isImplicit());
  }
  
  llvm_unreachable("Can't represent lookup result");
}

namespace {
  class PreCheckExpression : public ASTWalker {
    TypeChecker &TC;
    DeclContext *DC;

  public:
    PreCheckExpression(TypeChecker &tc, DeclContext *dc) : TC(tc), DC(dc) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For closures, type-check the patterns and result type as written,
      // but do not walk into the body. That will be type-checked after
      // we've determine the complete function type.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        // Validate the parameters.
        if (TC.typeCheckPattern(closure->getParams(), DC, true)) {
          expr->setType(ErrorType::get(TC.Context));
          return { false, expr };
        }

        // Validate the result type, if present.
        if (closure->hasExplicitResultType() &&
            TC.validateType(closure->getExplicitResultTypeLoc(), DC)) {
          expr->setType(ErrorType::get(TC.Context));
          return { false, expr };
        }
        
        return { closure->hasSingleExpressionBody(), expr };
      }

      if (auto unresolved = dyn_cast<UnresolvedDeclRefExpr>(expr)) {
        return { true, BindName(unresolved, DC, TC) };
      }

      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      // Fold sequence expressions.
      if (auto seqExpr = dyn_cast<SequenceExpr>(expr)) {
        return TC.foldSequence(seqExpr, DC);
      }

      // Type check the type in an array new expression.
      if (auto newArray = dyn_cast<NewArrayExpr>(expr)) {
        // FIXME: Check that the element type has a default constructor.
        
        if (TC.validateType(newArray->getElementTypeLoc(), DC,
                            /*allowUnboundGenerics=*/true))
          return nullptr;

        // Check array bounds. They are subproblems that don't interact with
        // the surrounding expression context.
        for (unsigned i = newArray->getBounds().size(); i != 1; --i) {
          auto &bound = newArray->getBounds()[i-1];
          if (!bound.Value)
            continue;

          // All inner bounds must be constant.
          if (TC.typeCheckArrayBound(bound.Value, /*requireConstant=*/true, DC))
            return nullptr;
        }

        // The outermost bound does not need to be constant.
        if (TC.typeCheckArrayBound(newArray->getBounds()[0].Value,
                                   /*requireConstant=*/false, DC))
          return nullptr;

        return expr;
      }

      // Type check the type parameters in an UnresolvedSpecializeExpr.
      if (auto us = dyn_cast<UnresolvedSpecializeExpr>(expr)) {
        for (TypeLoc &type : us->getUnresolvedParams()) {
          if (TC.validateType(type, DC)) {
            TC.diagnose(us->getLAngleLoc(),
                        diag::while_parsing_as_left_angle_bracket);
            return nullptr;
          }
        }
        return expr;
      }

      // Type check the type parameters in cast expressions.
      if (auto cast = dyn_cast<ExplicitCastExpr>(expr)) {
        if (TC.validateType(cast->getCastTypeLoc(), DC))
          return nullptr;
        return expr;
      }

      return expr;
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      // Never walk into statements.
      return { false, stmt };
    }
  };
}

/// \brief Clean up the given ill-formed expression, removing any references
/// to type variables and setting error types on erroneous expression nodes.
static Expr *cleanupIllFormedExpression(ASTContext &context,
                                        ConstraintSystem *cs, Expr *expr) {
  class CleanupIllFormedExpression : public ASTWalker {
    ASTContext &context;
    ConstraintSystem *cs;

  public:
    CleanupIllFormedExpression(ASTContext &context, ConstraintSystem *cs)
      : context(context), cs(cs) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For closures, type-check the patterns and result type as written,
      // but do not walk into the body. That will be type-checked after
      // we've determine the complete function type.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        SmallVector<VarDecl *, 6> Params;
        closure->getParams()->collectVariables(Params);
        for (auto VD : Params) {
          Type T = VD->getType();
          if (cs)
            T = cs->simplifyType(T);
          if (T->hasTypeVariable())
            T = ErrorType::get(context);
          VD->overwriteType(T);
        }
        if (!closure->hasSingleExpressionBody()) {
          return { false, walkToExprPost(expr) };
        }

        return { true, expr };
      }

      return { true, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      Type type;
      if (expr->getType()) {
        type = expr->getType();
        if (cs)
          type = cs->simplifyType(type);
      }

      if (!type || type->hasTypeVariable())
        expr->setType(ErrorType::get(context));
      else
        expr->setType(type);
      return expr;
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      // Never walk into statements.
      return { false, stmt };
    }
  };

  if (!expr)
    return expr;
  
  return expr->walk(CleanupIllFormedExpression(context, cs));
}

namespace {
  /// \brief RAII object that cleans up the given expression if not explicitly
  /// disabled.
  class CleanupIllFormedExpressionRAII {
    ConstraintSystem &cs;
    Expr **expr;

  public:
    CleanupIllFormedExpressionRAII(ConstraintSystem &cs, Expr *&expr)
      : cs(cs), expr(&expr) { }

    ~CleanupIllFormedExpressionRAII() {
      if (expr) {
        *expr = cleanupIllFormedExpression(cs.getASTContext(), &cs, *expr);
      }
    }

    /// \brief Disable the cleanup of this expression; it doesn't need it.
    void disable() {
      expr = nullptr;
    }
  };
}

/// Pre-check the expression, validating any types that occur in the
/// expression and folding sequence expressions.
bool TypeChecker::preCheckExpression(Expr *&expr, DeclContext *dc) {
  if (auto result = expr->walk(PreCheckExpression(*this, dc))) {
    expr = result;
    return false;
  }

  expr = cleanupIllFormedExpression(dc->getASTContext(), nullptr, expr);
  return true;
}

#pragma mark High-level entry points
bool TypeChecker::typeCheckExpression(Expr *&expr, DeclContext *dc,
                                      Type convertType,
                                      bool discardedExpr,
                                      bool allowFreeTypeVariables) {
  PrettyStackTraceExpr stackTrace(Context, "type-checking", expr);

  // First, pre-check the expression, validating any types that occur in the
  // expression and folding sequence expressions.
  if (preCheckExpression(expr, dc))
    return true;

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc);
  CleanupIllFormedExpressionRAII cleanup(cs, expr);
  if (auto generatedExpr = cs.generateConstraints(expr))
    expr = generatedExpr;
  else {
    return true;
  }

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  if (convertType) {
    cs.addConstraint(ConstraintKind::Conversion, expr->getType(), convertType,
                     cs.getConstraintLocator(expr, { }));
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable, allowFreeTypeVariables)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr, log);
  }

  // Apply the solution to the expression.
  auto result = cs.applySolution(solution, expr);
  if (!result) {
    // Failure already diagnosed, above, as part of applying the solution.
   return true;
  }

  // If we're supposed to convert the expression to some particular type,
  // do so now.
  if (convertType) {
    result = solution.coerceToType(result, convertType,
                                   cs.getConstraintLocator(expr, { }));
    if (!result) {
      return true;
    }
  } else if (auto lvalueType = result->getType()->getAs<LValueType>()) {
    if (!lvalueType->getQualifiers().isImplicit()) {
      // We explicitly took a reference to the result, but didn't use it.
      // Complain and emit a Fix-It to zap the '&'.
      auto addressOf = cast<AddressOfExpr>(result->getSemanticsProvidingExpr());
      diagnose(addressOf->getLoc(), diag::reference_non_inout,
               lvalueType->getObjectType())
        .highlight(addressOf->getSubExpr()->getSourceRange())
        .fixItRemove(SourceRange(addressOf->getLoc()));

      // Strip the address-of expression.
      result = addressOf->getSubExpr();
      lvalueType = result->getType()->getAs<LValueType>();
    } 

    if (lvalueType && !discardedExpr) {
      // We referenced an lvalue. Load it.
      assert(lvalueType->getQualifiers().isImplicit() &&
             "Explicit lvalue diagnosed above");
      result = new (Context) LoadExpr(result, lvalueType->getObjectType());
    }
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    result->dump(log);
  }

  expr = result;
  cleanup.disable();
  return false;
}

bool TypeChecker::typeCheckExpressionShallow(Expr *&expr, DeclContext *dc,
                                             Type convertType) {
  PrettyStackTraceExpr stackTrace(Context, "shallow type-checking", expr);

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc);
  CleanupIllFormedExpressionRAII cleanup(cs, expr);
  if (auto generatedExpr = cs.generateConstraintsShallow(expr))
    expr = generatedExpr;
  else
    return true;

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  if (convertType) {
    cs.addConstraint(ConstraintKind::Conversion, expr->getType(), convertType,
                     cs.getConstraintLocator(expr, { }));
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr, log);
  }

  // Apply the solution to the expression.
  auto result = cs.applySolutionShallow(solution, expr);
  if (!result) {
    // Failure already diagnosed, above, as part of applying the solution.
    return true;
  }

  // If we're supposed to convert the expression to some particular type,
  // do so now.
  if (convertType) {
    result = solution.coerceToType(result, convertType,
                                   cs.getConstraintLocator(expr, { }));
    if (!result) {
      return true;
    }
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    result->dump(log);
  }

  expr = result;
  cleanup.disable();
  return false;
}

bool TypeChecker::typeCheckBinding(PatternBindingDecl *binding) {
  PrettyStackTraceDecl stackTrace("type-checking", binding);

  Expr *init = binding->getInit();
  assert(init && "type-checking an uninitialized binding?");

  auto pattern = binding->getPattern();

  DeclContext *dc = binding->getDeclContext();

  // First, pre-check the initializer, validating any types that occur in the
  // expression and folding sequence expressions.
  if (preCheckExpression(init, dc))
    return true;

  ConstraintSystem cs(*this, dc);

  // Collect constraints from the initializer.
  CleanupIllFormedExpressionRAII cleanup(cs, init);
  if (auto generatedExpr = cs.generateConstraints(init))
    init = generatedExpr;
  else {
    return true;
  }

  auto initLocator = cs.getConstraintLocator(init, { });

  // Collect constraints from the pattern.
  Type patternType = cs.generateConstraints(pattern, initLocator);
  if (!patternType) return true;

  // Add a conversion constraint between the types.
  cs.addConstraint(ConstraintKind::Conversion, init->getType(),
                   patternType, initLocator);

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given variable binding---\n";
    init->print(log);
    log << "\n";
    cs.dump(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Crappy diagnostic.
    diagnose(init->getLoc(), diag::constraint_type_check_fail)
      .highlight(init->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr, log);
  }

  // Apply the solution to the expression.
  init = cs.applySolution(solution, init);
  if (!init) {
    // Failure already diagnosed, above, as part of applying the solution.
   return true;
  }

  // Figure out what type the constraints decided on.
  patternType = solution.simplifyType(*this, patternType);

  // Convert the initializer to the type of the pattern.
  init = solution.coerceToType(init, patternType, initLocator);
  if (!init) {
    return true;
  }

  // Force the initializer to be materializable.
  // FIXME: work this into the constraint system
  init = coerceToMaterializable(init);
  if (!init) {
    return true;
  }

  // Apply the solution to the pattern as well.
  if (coerceToType(pattern, dc, init->getType(), /*allowOverride=*/true)) {
    return true;
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    init->dump(log);
  }

  binding->setPattern(pattern);
  binding->setInit(init, /*checked=*/true);
  cleanup.disable();
  return false;
}

/// \brief Compute the rvalue type of the given expression, which is the
/// destination of an assignment statement.
Type ConstraintSystem::computeAssignDestType(Expr *dest, SourceLoc equalLoc) {
  if (TupleExpr *TE = dyn_cast<TupleExpr>(dest)) {
    auto &ctx = getASTContext();
    SmallVector<TupleTypeElt, 4> destTupleTypes;
    for (unsigned i = 0; i != TE->getNumElements(); ++i) {
      Expr *subExpr = TE->getElement(i);
      Type elemTy = computeAssignDestType(subExpr, equalLoc);
      if (!elemTy)
        return Type();
      destTupleTypes.push_back(TupleTypeElt(elemTy, TE->getElementName(i)));
    }

    return TupleType::get(destTupleTypes, ctx);
  }

  Type destTy = simplifyType(dest->getType());
  if (LValueType *destLV = destTy->getAs<LValueType>()) {
    // If the destination is a settable lvalue, we're good; get its object type.
    if (!destLV->isSettable()) {
      // FIXME: error message refers to "variable or subscript" instead of
      // saying which one it is.
      getTypeChecker().diagnose(equalLoc, diag::assignment_lhs_not_settable)
        .highlight(dest->getSourceRange());
      return Type();
    }
    destTy = destLV->getObjectType();
  } else if (auto typeVar = dyn_cast<TypeVariableType>(destTy.getPointer())) {
    // The destination is a type variable. This type variable must be an
    // lvalue type, which we enforce via a subtyping relationship with
    // [inout(implicit, settable)] T, where T is a fresh type variable that
    // will be the object type of this particular expression type.
    auto objectTv = createTypeVariable(
                      getConstraintLocator(dest,
                                           ConstraintLocator::AssignDest),
                      TVO_CanBindToLValue);
    auto refTv = LValueType::get(objectTv,
                                 LValueType::Qual::Implicit,
                                 getASTContext());
    addConstraint(ConstraintKind::Subtype, typeVar, refTv);
    destTy = objectTv;
  } else {
    if (!destTy->is<ErrorType>())
      getTypeChecker().diagnose(equalLoc, diag::assignment_lhs_not_lvalue)
        .highlight(dest->getSourceRange());

    return Type();
  }
  
  return destTy;
}

bool TypeChecker::typeCheckCondition(Expr *&expr, DeclContext *dc) {
  PrettyStackTraceExpr stackTrace(Context, "type-checking condition", expr);

  if (preCheckExpression(expr, dc))
    return true;

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc);
  CleanupIllFormedExpressionRAII cleanup(cs, expr);
  if (auto generatedExpr = cs.generateConstraints(expr))
    expr = generatedExpr;
  else
    return true;

  // If the expression has type Builtin.Int1 (or an l-value with that
  // object type), go ahead and special-case that.  This doesn't need
  // to be deeply principled because builtin types are not user-facing.
  auto rvalueType = expr->getType()->getRValueType();
  if (rvalueType->isBuiltinIntegerType(1)) {
    cs.addConstraint(ConstraintKind::Conversion, expr->getType(),
                     rvalueType);

  // Otherwise, the result must be a LogicValue.
  } else {
    auto logicValueProto = getProtocol(expr->getLoc(),
                                       KnownProtocolKind::LogicValue);
    if (!logicValueProto) {
      return true;
    }

    cs.addConstraint(ConstraintKind::ConformsTo, expr->getType(),
                     logicValueProto->getDeclaredType(),
                     cs.getConstraintLocator(expr, { }));
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr, log);
  }

  // Apply the solution to the expression.
  auto result = cs.applySolution(solution, expr);
  if (!result) {
    // Failure already diagnosed, above, as part of applying the solution.
    return true;
  }

  // Convert the expression to a logic value.
  result = solution.convertToLogicValue(result,
                                        cs.getConstraintLocator(expr, { }));
  if (!result) {
    return true;
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    result->dump(log);
  }

  expr = result;
  cleanup.disable();
  return false;
}

bool TypeChecker::typeCheckArrayBound(Expr *&expr, bool constantRequired,
                                      DeclContext *dc) {
  PrettyStackTraceExpr stackTrace(Context, "type-checking array bound", expr);

  // If it's an integer literal expression, just convert the type directly.
  if (auto lit = dyn_cast<IntegerLiteralExpr>(
                   expr->getSemanticsProvidingExpr())) {
    // FIXME: the choice of 64-bit is rather arbitrary.
    expr->setType(BuiltinIntegerType::get(64, Context));

    // Constant array bounds must be non-zero.
    if (constantRequired) {
      uint64_t size = lit->getValue().getZExtValue();
      if (size == 0) {
        diagnose(lit->getLoc(), diag::new_array_bound_zero)
        .highlight(lit->getSourceRange());
        return nullptr;
      }
    }

    return false;
  }

  // Otherwise, if a constant expression is required, fail.
  if (constantRequired) {
    diagnose(expr->getLoc(), diag::non_constant_array)
      .highlight(expr->getSourceRange());
    return true;
  }

  // First, pre-check the expression, validating any types that occur in the
  // expression and folding sequence expressions.
  if (preCheckExpression(expr, dc))
    return true;

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc);
  CleanupIllFormedExpressionRAII cleanup(cs, expr);
  if (auto generatedExpr = cs.generateConstraints(expr))
    expr = generatedExpr;
  else
    return true;

  // The result must be an ArrayBound.
  auto arrayBoundProto = getProtocol(expr->getLoc(),
                                     KnownProtocolKind::ArrayBound);
  if (!arrayBoundProto) {
    return true;
  }

  cs.addConstraint(ConstraintKind::ConformsTo, expr->getType(),
                   arrayBoundProto->getDeclaredType(),
                   cs.getConstraintLocator(expr, { }));

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr, log);
  }

  // Apply the solution to the expression.
  auto result = cs.applySolution(solution, expr);
  if (!result) {
    // Failure already diagnosed, above, as part of applying the solution.
    return true;
  }

  // Convert the expression to an array bound.
  result = solution.convertToArrayBound(result,
                                        cs.getConstraintLocator(expr, { }));
  if (!result) {
    return true;
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    result->dump(log);
  }
  
  expr = result;
  cleanup.disable();
  return false;
}

/// Find the '~=` operator that can compare an expression inside a pattern to a
/// value of a given type.
bool TypeChecker::typeCheckExprPattern(ExprPattern *EP, DeclContext *DC,
                                       Type rhsType) {
  PrettyStackTracePattern stackTrace(Context, "type-checking", EP);

  // Create a variable to stand in for the RHS value.
  auto *matchVar = new (Context) VarDecl(EP->getLoc(),
                                         Context.getIdentifier("$match"),
                                         rhsType,
                                         DC);
  EP->setMatchVar(matchVar);
  
  // Find '~=' operators for the match.
  UnqualifiedLookup matchLookup(Context.getIdentifier("~="), DC, this);
  if (!matchLookup.isSuccess()) {
    diagnose(EP->getLoc(), diag::no_match_operator);
    return true;
  }
  
  SmallVector<ValueDecl*, 4> choices;
  for (auto &result : matchLookup.Results) {
    if (!result.hasValueDecl())
      continue;
    choices.push_back(result.getValueDecl());
  }
  
  if (choices.empty()) {
    diagnose(EP->getLoc(), diag::no_match_operator);
    return true;
  }
  
  // Build the 'expr ~= var' expression.
  auto *matchOp = buildRefExpr(choices, EP->getLoc(), /*Implicit=*/true);
  auto *matchVarRef = new (Context) DeclRefExpr(matchVar,
                                                EP->getLoc(),
                                                /*Implicit=*/true);
  
  Expr *matchArgElts[] = {EP->getSubExpr(), matchVarRef};
  auto *matchArgs
    = new (Context) TupleExpr(EP->getSubExpr()->getSourceRange().Start,
                    Context.AllocateCopy(MutableArrayRef<Expr*>(matchArgElts)),
                    nullptr,
                    EP->getSubExpr()->getSourceRange().End,
                    false, /*Implicit=*/true);
  
  Expr *matchCall = new (Context) BinaryExpr(matchOp, matchArgs,
                                             /*Implicit=*/true);
  
  // Check the expression as a condition.
  if (typeCheckCondition(matchCall, DC))
    return true;

  // Save the type-checked expression in the pattern.
  EP->setMatchExpr(matchCall);
  // Set the type on the pattern.
  EP->setType(rhsType);
  return false;
}

bool TypeChecker::isTrivialSubtypeOf(Type type1, Type type2, DeclContext *dc) {
  ConstraintSystem cs(*this, dc);
  cs.addConstraint(ConstraintKind::TrivialSubtype, type1, type2);
  SmallVector<Solution, 1> solutions;
  return !cs.solve(solutions);
}

bool TypeChecker::isSubtypeOf(Type type1, Type type2, DeclContext *dc) {
  ConstraintSystem cs(*this, dc);
  cs.addConstraint(ConstraintKind::Subtype, type1, type2);
  SmallVector<Solution, 1> solutions;
  return !cs.solve(solutions);
}

bool TypeChecker::isConvertibleTo(Type type1, Type type2, DeclContext *dc) {
  ConstraintSystem cs(*this, dc);
  cs.addConstraint(ConstraintKind::Conversion, type1, type2);
  SmallVector<Solution, 1> solutions;
  return !cs.solve(solutions);
}

bool TypeChecker::isSubstitutableFor(Type type1, ArchetypeType *type2,
                                     DeclContext *dc) {
  ConstraintSystem cs(*this, dc);
  
  llvm::DenseMap<CanType, TypeVariableType*> replacements;
  Type type2var = cs.openType(type2, type2, replacements, nullptr);

  cs.addConstraint(ConstraintKind::Equal, type1, type2var);
  
  SmallVector<Solution, 1> solution;
  return !cs.solve(solution);
}

Expr *TypeChecker::coerceToRValue(Expr *expr) {
  // If we already have an rvalue, we're done.
  auto lvalueTy = expr->getType()->getAs<LValueType>();
  if (!lvalueTy)
    return expr;

  // Can't load from an explicit lvalue.
  if (auto addrOf = dyn_cast<AddressOfExpr>(expr->getSemanticsProvidingExpr())){
    diagnose(expr->getLoc(), diag::load_of_explicit_lvalue,
             lvalueTy->getObjectType())
      .fixItRemove(SourceRange(expr->getLoc()));
    return coerceToRValue(addrOf->getSubExpr());
  }

  // Load the lvalue.
  return new (Context) LoadExpr(expr, lvalueTy->getObjectType());
}

Expr *TypeChecker::coerceToMaterializable(Expr *expr) {
  // Load lvalues.
  if (auto lvalue = expr->getType()->getAs<LValueType>()) {
    return new (Context) LoadExpr(expr, lvalue->getObjectType());
  }

  // Walk into parenthesized expressions to update the subexpression.
  if (auto paren = dyn_cast<ParenExpr>(expr)) {
    auto sub = coerceToMaterializable(paren->getSubExpr());
    paren->setSubExpr(sub);
    paren->setType(sub->getType());
    return paren;
  }

  // Walk into tuples to update the subexpressions.
  if (auto tuple = dyn_cast<TupleExpr>(expr)) {
    bool anyChanged = false;
    for (auto &elt : tuple->getElements()) {
      // Materialize the element.
      auto oldType = elt->getType();
      elt = coerceToMaterializable(elt);

      // If the type changed at all, make a note of it.
      if (elt->getType().getPointer() != oldType.getPointer()) {
        anyChanged = true;
      }
    }

    // If any of the types changed, rebuild the tuple type.
    if (anyChanged) {
      SmallVector<TupleTypeElt, 4> elements;
      elements.reserve(tuple->getElements().size());
      for (unsigned i = 0, n = tuple->getNumElements(); i != n; ++i) {
        Type type = tuple->getElement(i)->getType();
        Identifier name = tuple->getElementName(i);
        elements.push_back(TupleTypeElt(type, name));
      }
      tuple->setType(TupleType::get(elements, Context));
    }

    return tuple;
  }

  // Nothing to do.
  return expr;
}

bool TypeChecker::convertToType(Expr *&expr, Type type, DeclContext *dc) {
  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this, dc);
  CleanupIllFormedExpressionRAII cleanup(cs, expr);

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  cs.addConstraint(ConstraintKind::Conversion, expr->getType(), type,
                   cs.getConstraintLocator(expr, { }));

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr, log);
  }

  // Perform the conversion.
  Expr *result = solution.coerceToType(expr, type,
                                       cs.getConstraintLocator(expr, { }));
  if (!result) {
    return true;
  }

  if (getLangOpts().DebugConstraintSolver) {
    auto &log = Context.TypeCheckerDebug->getStream();
    log << "---Type-checked expression---\n";
    result->dump(log);
  }

  expr = result;
  cleanup.disable();
  return false;
}

//===--------------------------------------------------------------------===//
// Debugging
//===--------------------------------------------------------------------===//
#pragma mark Debugging

void Solution::dump(SourceManager *sm) const {
  dump(sm, llvm::errs());
}

void Solution::dump(SourceManager *sm, raw_ostream &out) const {
  out << "Fixed score: " << getFixedScore() << "\n\n";
  out << "Type variables:\n";
  for (auto binding : typeBindings) {
    out.indent(2);
    binding.first->getImpl().print(out);
    out << " as ";
    binding.second.print(out);
    out << "\n";
  }

  out << "\n";
  out << "Overload choices:\n";
  for (auto ovl : overloadChoices) {
    out.indent(2);
    if (ovl.first)
      ovl.first->dump(sm, out);
    out << " with ";

    auto choice = ovl.second.choice;
    switch (choice.getKind()) {
    case OverloadChoiceKind::Decl:
    case OverloadChoiceKind::DeclViaDynamic:
    case OverloadChoiceKind::TypeDecl:
      if (choice.getBaseType())
        out << choice.getBaseType()->getString() << ".";
        
      out << choice.getDecl()->getName().str() << ": "
        << ovl.second.openedType->getString() << "\n";
      break;

    case OverloadChoiceKind::BaseType:
      out << "base type " << choice.getBaseType()->getString() << "\n";
      break;

    case OverloadChoiceKind::FunctionReturningBaseType:
      out << "function returning base type "
        << choice.getBaseType()->getString() << "\n";
      break;
    case OverloadChoiceKind::IdentityFunction:
      out << "identity " << choice.getBaseType()->getString() << " -> "
        << choice.getBaseType()->getString() << "\n";
      break;
    case OverloadChoiceKind::TupleIndex:
      out << "tuple " << choice.getBaseType()->getString() << " index "
        << choice.getTupleIndex() << "\n";
      break;
    }
    out << "\n";
  }
}

void ConstraintSystem::dump() {
  dump(llvm::errs());
}

void ConstraintSystem::dump(raw_ostream &out) {
  out << "Type Variables:\n";
  for (auto tv : TypeVariables) {
    out.indent(2);
    tv->getImpl().print(out);
    if (tv->getImpl().canBindToLValue())
      out << " [lvalue allowed]";
    auto rep = getRepresentative(tv);
    if (rep == tv) {
      if (auto fixed = getFixedType(tv)) {
        out << " as ";
        fixed->print(out);
      }
    } else {
      out << " equivalent to ";
      rep->print(out);
    }
    out << "\n";
  }

  out << "\nUnsolved Constraints:\n";
  for (auto constraint : Constraints) {
    out.indent(2);
    constraint->print(out, &getTypeChecker().Context.SourceMgr);
    out << "\n";
  }

  out << "\nSolved Constraints:\n";
  for (auto constraint : SolvedConstraints) {
    out.indent(2);
    constraint->print(out, &getTypeChecker().Context.SourceMgr);
    out << "\n";
  }

  if (resolvedOverloadSets) {
    out << "Resolved overloads:\n";

    // Otherwise, report the resolved overloads.
    for (auto resolved = resolvedOverloadSets;
         resolved; resolved = resolved->Previous) {
      auto &choice = resolved->Choice;
      out << "  selected overload set choice ";
      switch (choice.getKind()) {
        case OverloadChoiceKind::Decl:
        case OverloadChoiceKind::DeclViaDynamic:
        case OverloadChoiceKind::TypeDecl:
          if (choice.getBaseType())
            out << choice.getBaseType()->getString() << ".";
          out << choice.getDecl()->getName().str() << ": "
            << resolved->BoundType->getString() << " == "
            << resolved->ImpliedType->getString() << "\n";
          break;

        case OverloadChoiceKind::BaseType:
          out << "base type " << choice.getBaseType()->getString() << "\n";
          break;

        case OverloadChoiceKind::FunctionReturningBaseType:
          out << "function returning base type "
              << choice.getBaseType()->getString() << "\n";
          break;
        case OverloadChoiceKind::IdentityFunction:
          out << "identity " << choice.getBaseType()->getString() << " -> "
              << choice.getBaseType()->getString() << "\n";
          break;
        case OverloadChoiceKind::TupleIndex:
          out << "tuple " << choice.getBaseType()->getString() << " index "
              << choice.getTupleIndex() << "\n";
          break;
      }
    }
    out << "\n";
  }

  if (failedConstraint) {
    out << "\nFailed constraint:\n";
    out.indent(2);
    failedConstraint->print(out, &getTypeChecker().Context.SourceMgr);
    out << "\n";
  }
}

/// Determine the semantics of a checked cast operation.
CheckedCastKind TypeChecker::typeCheckCheckedCast(Type fromType,
                                 Type toType,
                                 DeclContext *dc,
                                 SourceLoc diagLoc,
                                 SourceRange diagFromRange,
                                 SourceRange diagToRange,
                                 std::function<bool (Type)> convertToType) {
  Type origFromType = fromType;
  bool toArchetype = toType->is<ArchetypeType>();
  bool fromArchetype = fromType->is<ArchetypeType>();
  SmallVector<ProtocolDecl*, 2> toProtocols;
  bool toExistential = toType->isExistentialType(toProtocols);
  SmallVector<ProtocolDecl*, 2> fromProtocols;
  bool fromExistential = fromType->isExistentialType(fromProtocols);
  
  // If the from/to types are equivalent or implicitly convertible,
  // this should have been a coercion rather than a
  // checked cast (a as B). Complain.
  if (fromType->isEqual(toType) || isConvertibleTo(fromType, toType, dc)) {
    return CheckedCastKind::InvalidCoercible;
  }
  
  // We can only downcast to an existential if the destination protocols are
  // objc and the source type is an objc class or an existential bounded by objc
  // protocols.
  if (toExistential) {
    if (fromExistential) {
      for (auto fromProtocol : fromProtocols) {
        if (!fromProtocol->isObjC())
          goto unsupported_existential_cast;
      }
    } else {
      auto fromClass = fromType->getClassOrBoundGenericClass();
      if (!fromClass || !fromClass->isObjC())
        goto unsupported_existential_cast;
    }

    for (auto toProtocol : toProtocols) {
      if (!toProtocol->isObjC())
        goto unsupported_existential_cast;
    }
    
    return CheckedCastKind::ConcreteToUnrelatedExistential;
    
  unsupported_existential_cast:
    diagnose(diagLoc, diag::downcast_to_non_objc_existential,
             origFromType, toType)
      .highlight(diagFromRange)
      .highlight(diagToRange);
    return CheckedCastKind::Unresolved;
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
    } else if (isConvertibleTo(toType, fromType, dc)) {
      return CheckedCastKind::ExistentialToConcrete;
    } else {
      diagnose(diagLoc,
               diag::downcast_from_existential_to_unrelated,
               origFromType, toType)
        .highlight(diagFromRange)
        .highlight(diagToRange);
      return CheckedCastKind::Unresolved;
    }
  }
  
  //   - convert an archetype to a concrete type fulfilling its constraints.
  if (fromArchetype) {
    if (!isSubstitutableFor(toType, fromType->castTo<ArchetypeType>(), dc)) {
      diagnose(diagLoc,
               diag::downcast_from_archetype_to_unrelated,
               origFromType, toType)
        .highlight(diagFromRange)
        .highlight(diagToRange);
      return CheckedCastKind::Unresolved;
    }
    return CheckedCastKind::ArchetypeToConcrete;
  }
  
  if (toArchetype) {
    //   - convert from a superclass to an archetype.
    if (auto toSuperType = toType->castTo<ArchetypeType>()->getSuperclass()) {
      // Coerce to the supertype of the archetype.
      if (convertToType(toSuperType))
        return CheckedCastKind::Unresolved;
      
      return CheckedCastKind::SuperToArchetype;
    }
    
    //  - convert a concrete type to an archetype for which it fulfills
    //    constraints.
    if (isSubstitutableFor(fromType, toType->castTo<ArchetypeType>(), dc)) {
      return CheckedCastKind::ConcreteToArchetype;
    }
    
    diagnose(diagLoc,
             diag::downcast_from_concrete_to_unrelated_archetype,
             origFromType, toType)
      .highlight(diagFromRange)
      .highlight(diagToRange);
    return CheckedCastKind::Unresolved;
  }

  // The remaining case is a class downcast.

  assert(!fromArchetype && "archetypes should have been handled above");
  assert(!toArchetype && "archetypes should have been handled above");
  assert(!fromExistential && "existentials should have been handled above");
  assert(!toExistential && "existentials should have been handled above");

  // The destination type must be a subtype of the source type.
  if (!isSubtypeOf(toType, fromType, dc)) {
    diagnose(diagLoc, diag::downcast_to_unrelated, origFromType, toType)
      .highlight(diagFromRange)
      .highlight(diagToRange);
    return CheckedCastKind::Unresolved;
  }

  return CheckedCastKind::Downcast;
}
