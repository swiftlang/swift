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
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/NameLookup.h"
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
#include "llvm/Support/SourceMgr.h"
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

void TypeVariableType::Implementation::print(llvm::raw_ostream &Out) {
  Out << "$T" << ID;
}

void TypeVariableType::print(raw_ostream &OS) const {
  OS << "$T" << getImpl().getID();
}

SavedTypeVariableBinding::SavedTypeVariableBinding(TypeVariableType *typeVar)
  : TypeVar(typeVar), ParentOrFixed(typeVar->getImpl().ParentOrFixed) { }

void SavedTypeVariableBinding::restore() {
  TypeVar->getImpl().ParentOrFixed = ParentOrFixed;
}

//===--------------------------------------------------------------------===//
// Constraints
//===--------------------------------------------------------------------===//
#pragma mark Constraints

void ConstraintLocator::dump(llvm::SourceMgr *sm) {
  llvm::raw_ostream &out = llvm::errs();

  if (anchor) {
    out << Expr::getKindName(anchor->getKind());
    if (sm) {
      out << '@';
      anchor->getLoc().print(out, *sm);
    }
  }

  for (auto elt : getPath()) {
    out << " -> ";
    switch (elt.getKind()) {
    case AddressOf:
      out << "address of";
      break;

    case ArrayElementType:
      out << "array element";
      break;

    case ApplyArgument:
      out << "apply argument";
      break;

    case ApplyFunction:
      out << "apply function";
      break;

    case AssignSource:
      out << "assignment source";
      break;
        
    case ClosureResult:
      out << "closure result";
      break;

    case ConversionMember:
      out << "conversion member";
      break;

    case ConversionResult:
      out << "conversion result";
      break;

    case ConstructorMember:
      out << "constructor member";
      break;

    case FunctionArgument:
      out << "function argument";
      break;

    case FunctionResult:
      out << "function result";
      break;

    case GenericArgument:
      out << "generic argument #" << llvm::utostr(elt.getValue());
      break;

    case IfElse:
      out << "'else' branch of ternary" ;
      break;

    case IfThen:
      out << "'then' branch of ternary" ;
      break;

    case InstanceType:
      out << "instance type";
      break;

    case InterpolationArgument:
      out << "interpolation argument #" << llvm::utostr(elt.getValue());
      break;

    case Load:
      out << "load";
      break;

    case LvalueObjectType:
      out << "lvalue object type";
      break;

    case Member:
      out << "member";
      break;

    case MemberRefBase:
      out << "member reference base";
      break;

    case NamedTupleElement:
      out << "named tuple element #" << llvm::utostr(elt.getValue());
      break;

    case ParentType:
      out << "parent type";
      break;

    case RvalueAdjustment:
      out << "rvalue adjustment";
      break;

    case ScalarToTuple:
      out << "scalar to tuple";
      break;

    case SubscriptIndex:
      out << "subscript index";
      break;

    case SubscriptMember:
      out << "subscript member";
      break;

    case SubscriptResult:
      out << "subscript result";
      break;

    case TupleElement:
      out << "tuple element #" << llvm::utostr(elt.getValue());
      break;
    }
  }
}

void Constraint::print(llvm::raw_ostream &Out, llvm::SourceMgr *sm) {
  First->print(Out);

  bool skipSecond = false;

  switch (Kind) {
  case ConstraintKind::Bind: Out << " := "; break;
  case ConstraintKind::Equal: Out << " == "; break;
  case ConstraintKind::EqualRvalue: Out << " ==R "; break;
  case ConstraintKind::TrivialSubtype: Out << " <t "; break;
  case ConstraintKind::Subtype: Out << " < "; break;
  case ConstraintKind::Conversion: Out << " <c "; break;
  case ConstraintKind::Construction: Out << " <C "; break;
  case ConstraintKind::Literal:
    Out << " is ";
    switch (getLiteralKind()) {
    case LiteralKind::ASCIIString:
      Out << "an ASCII string";
      break;

    case LiteralKind::Char:
      Out << "a character";
      break;

    case LiteralKind::Float:
      Out << "a floating-point";
      break;

    case LiteralKind::Int:
      Out << "an integer";
      break;

    case LiteralKind::UTFString:
      Out << "a UTF-8 string";
      break;

    case LiteralKind::Array:
      Out << "an array";
      break;

    case LiteralKind::Dictionary:
      Out << "a dictionary";
      break;
    }
    Out << " literal";
    skipSecond = true;
    break;

  case ConstraintKind::ValueMember:
    Out << "[." << Member.str() << ": value] == ";
    break;
  case ConstraintKind::TypeMember:
    Out << "[." << Member.str() << ": type] == ";
    break;
  case ConstraintKind::Archetype:
    Out << " is an archetype";
    skipSecond = true;
    break;
  }

  if (!skipSecond)
    Second->print(Out);

  if (Locator) {
    Out << " [[";
    Locator->dump(sm);
    Out << "]];";
  }
}

void Constraint::dump(llvm::SourceMgr *sm) {
  print(llvm::errs(), sm);
}

void *operator new(size_t bytes, ConstraintSystem& cs,
                   size_t alignment) {
  return cs.getAllocator().Allocate(bytes, alignment);
}

OverloadSet *OverloadSet::getNew(ConstraintSystem &CS,
                                 Type boundType,
                                 ConstraintLocator *locator,
                                 ArrayRef<OverloadChoice> choices) {
  unsigned size = sizeof(OverloadSet)
                + sizeof(OverloadChoice) * choices.size();
  void *mem = CS.getAllocator().Allocate(size, alignof(OverloadSet));
  return ::new (mem) OverloadSet(CS.assignOverloadSetID(), locator, boundType,
                                 choices);
}

ConstraintSystem::ConstraintSystem(TypeChecker &tc)
  : TC(tc), Arena(tc.Context, Allocator)
{
}

ConstraintSystem::~ConstraintSystem() { }

bool ConstraintSystem::hasFreeTypeVariables() {
  // Look for any free type variables.
  for (auto tv : TypeVariables) {
    // We only care about the representatives.
    if (getRepresentative(tv) != tv)
      continue;

    if (auto fixed = getFixedType(tv)) {
      if (simplifyType(fixed)->hasTypeVariable())
        return false;

      continue;
    }
    
    return true;
  }
  
  return false;
}

MemberLookup &ConstraintSystem::lookupMember(Type base, Identifier name) {
  base = base->getCanonicalType();
  auto &ptr = MemberLookups[{base, name}];
  if (!ptr)
    ptr.reset(new MemberLookup(base, name, TC.TU));
  return *ptr;
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

  case SolutionKind::TriviallySolved:
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

Type ConstraintSystem::openType(
       Type startingType,
       ArrayRef<ArchetypeType *> archetypes,
       llvm::DenseMap<ArchetypeType *, TypeVariableType *> &replacements) {
  struct GetTypeVariable {
    ConstraintSystem &CS;
    llvm::DenseMap<ArchetypeType *, TypeVariableType *> &Replacements;

    TypeVariableType *operator()(ArchetypeType *archetype) const {
      // Check whether we already have a replacement for this archetype.
      auto known = Replacements.find(archetype);
      if (known != Replacements.end())
        return known->second;

      // Create a new type variable to replace this archetype.
      auto tv = CS.createTypeVariable(archetype);

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
        SmallVector<Type, 4> conformsToTypes;
        conformsToTypes.reserve(conformsTo.size());
        std::transform(conformsTo.begin(), conformsTo.end(),
                       std::back_inserter(conformsToTypes),
                       [](ProtocolDecl *proto) {
                         return proto->getDeclaredType();
                       });

        auto composition = ProtocolCompositionType::get(CS.TC.Context,
                                                        conformsToTypes);
        CS.addConstraint(ConstraintKind::Conversion, tv, composition);
      }

      // Record the type variable that corresponds to this archetype.
      Replacements[archetype] = tv;

      // Build archetypes for each of the nested types.
      for (auto nested : archetype->getNestedTypes()) {
        auto nestedTv = (*this)(nested.second);
        CS.addTypeMemberConstraint(tv, nested.first, nestedTv);
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
      auto known = replacements.find(archetype);
      if (known != replacements.end())
        return known->second;

      return archetype;
    }

    // Create type variables for all of the archetypes in a polymorphic
    // function type.
    if (auto polyFn = type->getAs<PolymorphicFunctionType>()) {
      for (auto archetype : polyFn->getGenericParams().getAllArchetypes())
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

Type ConstraintSystem::openBindingType(Type type) {
  Type result = openType(type);
  // FIXME: Better way to identify Slice<T>.
  if (auto boundStruct
        = dyn_cast<BoundGenericStructType>(result.getPointer())) {
    if (!boundStruct->getParent() &&
        boundStruct->getDecl()->getName().str() == "Slice" &&
        boundStruct->getGenericArgs().size() == 1) {
      return getTypeChecker().getArraySliceType(
               SourceLoc(), boundStruct->getGenericArgs()[0]);
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

  // For an assignment operator, the first parameter is an implicit byref.
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
                               funcTy->isAutoClosure(),
                               funcTy->isBlock(),
                               funcTy->isThin(),
                               context);
    }
  }

  return type;
}

bool constraints::computeTupleShuffle(TupleType *fromTuple, TupleType *toTuple,
                                      SmallVectorImpl<int> &sources,
                                      SmallVectorImpl<unsigned> &variadicArgs) {
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
  auto skipToNextUnnamedInput = [&] {
    while (fromNext != fromLast &&
           (consumed[fromNext] ||
            !fromTuple->getFields()[fromNext].getName().empty()))
      ++fromNext;
  };
  skipToNextUnnamedInput();

  for (unsigned i = 0, n = toTuple->getFields().size(); i != n; ++i) {
    // Check whether we already found a value for this element.
    if (sources[i] != unassigned)
      continue;

    const auto &elt2 = toTuple->getFields()[i];

    // Variadic tuple elements match the rest of the input elements.
    if (elt2.isVararg()) {
      // Collect the remaining (unnamed) inputs.
      while (fromNext != fromLast) {
        variadicArgs.push_back(fromNext);
        consumed[fromNext] = true;
        skipToNextUnnamedInput();
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

    sources[i] = fromNext;
    consumed[fromNext] = true;
    skipToNextUnnamedInput();
  }

  // Check whether there were any unused input values.
  // FIXME: Could short-circuit this check, above, by not skipping named
  // input values.
  return std::find(consumed.begin(), consumed.end(), false) != consumed.end();
}

// A property or subscript is settable if:
// - its base type (the type of the 'a' in 'a[n]' or 'a.b') either has
//   reference semantics or has value semantics and is settable, AND
// - the 'var' or 'subscript' decl for the property provides a setter
template<typename SomeValueDecl>
static LValueType::Qual settableQualForDecl(Type baseType,
                                            SomeValueDecl *decl) {
  bool settable = ((!baseType ||
                    baseType->isSettableLValue() ||
                    baseType->getRValueType()->hasReferenceSemantics()) &&
                   decl->isSettable());
  return settable ? LValueType::Qual(0) : LValueType::Qual::NonSettable;
}

// TODO This should replace ValueDecl::getTypeOfReference once the old
// type checker is retired.
static Type getTypeOfValueDeclReference(Type baseType,
                                        ValueDecl *decl,
                                        ASTContext &Context) {
  LValueType::Qual qual = LValueType::Qual::DefaultForVar |
                          settableQualForDecl(baseType, decl);
  
  if (decl->isReferencedAsLValue()) {
    if (LValueType *LVT = decl->getType()->getAs<LValueType>())
      return LValueType::get(LVT->getObjectType(), qual, Context);
    return LValueType::get(decl->getType(), qual, Context);
  }
  
  return decl->getType();
}

Type ConstraintSystem::getTypeOfReference(ValueDecl *value) {
  if (auto proto = dyn_cast<ProtocolDecl>(value->getDeclContext())) {
    // Unqualified lookup can find operator names within protocols.
    auto func = cast<FuncDecl>(value);
    assert(func->isOperator() && "Lookup should only find operators");

    // Skip the 'this' metatype parameter. It's not used for deduction.
    auto type = func->getTypeOfReference()->castTo<FunctionType>()->getResult();

    // Find the archetype for 'This'. We'll be opening it.
    auto thisArchetype
      = proto->getThis()->getDeclaredType()->castTo<ArchetypeType>();
    llvm::DenseMap<ArchetypeType *, TypeVariableType *> replacements;
    type = adjustLValueForReference(openType(type, { &thisArchetype, 1 },
                                             replacements),
                                    func->getAttrs().isAssignment(),
                                    TC.Context);

    // The type variable to which 'This' was opened must be bound to an
    // archetype.
    // FIXME: We may eventually want to loosen this constraint, to allow us
    // to find operator functions both in classes and in protocols to which
    // a class conforms (if there's a default implementation).
    addArchetypeConstraint(replacements[thisArchetype]);
    
    return type;
  }

  // Determine the type of the value, opening up that type if necessary.
  Type valueType = getTypeOfValueDeclReference(nullptr,
                                               value,
                                               TC.Context);
  valueType = adjustLValueForReference(openType(valueType),
                                  value->getAttrs().isAssignment(),
                                  TC.Context);
  return valueType;
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
  MemberLookup &lookup = cs.lookupMember(parentTy, archetype->getName());
  auto member = cast<TypeDecl>(lookup.Results[0].D);
  auto type = member->getDeclaredType();
  type = cs.getTypeChecker().substMemberTypeWithBase(type, member, parentTy);
  mappedTypes[archetype] = type;
  return type;
}

Type ConstraintSystem::openTypeOfContext(
       DeclContext *dc,
       bool openAllLevels,
       llvm::DenseMap<ArchetypeType *, TypeVariableType *> &replacements,
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
    } else if (auto unbound = extendedTy->getAs<UnboundGenericType>()) {
      result = unbound->getDecl()->getDeclaredTypeInContext();
      dcGenericParams = unbound->getDecl()->getGenericParamsOfContext();
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

    // If we have multiple levels and are supposed to open them all, do so now.
    if (openAllLevels && dcGenericParams->getOuterParameters()) {
      for (auto gp = dcGenericParams; gp; gp = gp->getOuterParameters()) {
        allOpenArcheTypes.append(gp->getAllArchetypes().begin(),
                                 gp->getAllArchetypes().end());
      }
      openArchetypes = allOpenArcheTypes;
    }
  }

  return openType(result, openArchetypes, replacements);
}

Type ConstraintSystem::getTypeOfMemberReference(Type baseTy, ValueDecl *value,
                                                bool isTypeReference) {
  // Figure out the instance type used for the base.
  Type baseObjTy = baseTy->getRValueType();
  bool isInstance = true;
  if (auto baseMeta = baseObjTy->getAs<MetaTypeType>()) {
    baseObjTy = baseMeta->getInstanceType();
    isInstance = false;
  }

  // If the base is a module type, just use the type of the decl.
  if (baseObjTy->is<ModuleType>())
    return getTypeOfReference(value);

  // The archetypes that have been opened up and replaced with type variables.
  llvm::DenseMap<ArchetypeType *, TypeVariableType *> replacements;

  // Figure out the type of the owner.
  Type ownerTy = openTypeOfContext(value->getDeclContext(), false, replacements,
                                   nullptr);

  // The base type must be convertible to the owner type. For most cases,
  // subtyping suffices. However, the owner might be a protocol and the base a
  // type that implements that protocol, if which case we need to model this
  // with a conversion constraint.
  addConstraint(ConstraintKind::Conversion, baseObjTy, ownerTy);

  // Determine the type of the member.
  Type type;
  if (isTypeReference)
    type = cast<TypeDecl>(value)->getDeclaredType();
  else if (auto subscript = dyn_cast<SubscriptDecl>(value)) {
    auto resultTy = LValueType::get(subscript->getElementType(),
                                    LValueType::Qual::DefaultForMemberAccess|
                                    LValueType::Qual::Implicit|
                                    settableQualForDecl(baseTy, subscript),
                                    TC.Context);
    type = FunctionType::get(subscript->getIndices()->getType(), resultTy,
                             TC.Context);
  } else
    type = getTypeOfValueDeclReference(baseTy, value, TC.Context);

  // For a member of an archetype, substitute the base type for the 'This'
  // type.
  if (baseObjTy->is<ArchetypeType>()) {
    if (auto ownerProtoTy = ownerTy->getAs<ProtocolType>()) {
      auto thisArchetype = ownerProtoTy->getDecl()->getThis()->getDeclaredType()
                             ->castTo<ArchetypeType>();

      llvm::DenseMap<ArchetypeType *, Type> mappedTypes;
      mappedTypes[thisArchetype] = baseObjTy;
      type = TC.transformType(type,
               [&](Type type) -> Type {
                 if (auto archetype = type->getAs<ArchetypeType>()) {
                   return getTypeForArchetype(*this, archetype, mappedTypes);
                 }

                 return type;
             });
    }
  }

  type = openType(type, { }, replacements);

  // Skip the 'this' argument if it's already been bound by the base.
  if (auto func = dyn_cast<FuncDecl>(value)) {
    if (func->isStatic() || isInstance)
      type = type->castTo<AnyFunctionType>()->getResult();
  } else if (isa<ConstructorDecl>(value) || isa<OneOfElementDecl>(value)) {
    type = type->castTo<AnyFunctionType>()->getResult();
  }
  return adjustLValueForReference(type, value->getAttrs().isAssignment(),
                                  TC.Context);
}

void ConstraintSystem::addOverloadSet(OverloadSet *ovl) {
  // If we have a locator, we can use it to find this overload set.
  // FIXME: We want to get to the point where we always have a locator.
  if (auto locator = ovl->getLocator()) {
    if (TC.getLangOpts().DebugConstraintSolver && solverState) {
      llvm::errs().indent(solverState->depth * 2)
        << "(bind locator ";
      locator->dump(&getASTContext().SourceMgr);
      llvm::errs() << " to overload set #" << ovl->getID() << ")\n";
    }

    // FIXME: Strengthen this condition; we shouldn't have re-insertion of
    // generated overload sets.
    assert(!GeneratedOverloadSets[locator] ||
           GeneratedOverloadSets[locator] == ovl);
    GeneratedOverloadSets[locator] = ovl;
    if (solverState) {
      solverState->generatedOverloadSets.push_back(locator);
    }
  }

  // If there are fewer than two choices, then we can simply resolve this
  // now.
  if (ovl->getChoices().size() < 2) {
    resolveOverload(ovl, 0);
    return;
  }

  UnresolvedOverloadSets.push_back(ovl);
}

OverloadSet *
ConstraintSystem::getGeneratedOverloadSet(ConstraintLocator *locator) {
  auto known = GeneratedOverloadSets.find(locator);
  if (known != GeneratedOverloadSets.end())
    return known->second;

  return nullptr;
}

/// \brief Find the overload choice that was assumed by this constraint
/// system (or one of its parents), along with the type it was given.
Optional<std::pair<OverloadChoice, Type>>
ConstraintSystem::getSelectedOverloadFromSet(OverloadSet *ovl) {
  auto known = ResolvedOverloads.find(ovl);
  if (known != ResolvedOverloads.end()) {
    return std::make_pair(ovl->getChoices()[known->second.first],
                          known->second.second);
  }

  return Nothing;
}

//===--------------------------------------------------------------------===//
// Constraint simplification
//===--------------------------------------------------------------------===//
#pragma mark Constraint simplification

Optional<ConstraintSystem::SolutionKind>
ConstraintSystem::matchTupleTypes(TupleType *tuple1, TupleType *tuple2,
                                  TypeMatchKind kind, unsigned flags,
                                  ConstraintLocatorBuilder locator,
                                  bool &trivial) {
  unsigned subFlags = flags | TMF_GenerateConstraints;

  // Equality and subtyping have fairly strict requirements on tuple matching,
  // requiring element names to either match up or be disjoint.
  if (kind < TypeMatchKind::Conversion) {
    if (tuple1->getFields().size() != tuple2->getFields().size()) {
      // If the second tuple can be initialized from a scalar, fall back to
      // that.
      if (tuple2->getFieldForScalarInit() >= 0)
        return Nothing;

      // Record this failure.
      if (flags & TMF_RecordFailures) {
        recordFailure(getConstraintLocator(locator),
                      Failure::TupleSizeMismatch, tuple1, tuple2);
      }

      return SolutionKind::Error;
    }

    SolutionKind result = SolutionKind::TriviallySolved;
    for (unsigned i = 0, n = tuple1->getFields().size(); i != n; ++i) {
      const auto &elt1 = tuple1->getFields()[i];
      const auto &elt2 = tuple2->getFields()[i];

      // If the names don't match, we may have a conflict.
      if (elt1.getName() != elt2.getName()) {
        // Same-type requirements require exact name matches.
        if (kind == TypeMatchKind::SameType) {
          // If the second tuple can be initialized from a scalar, fall back to
          // that.
          if (tuple2->getFieldForScalarInit() >= 0)
            return Nothing;

          // Record this failure.
          if (flags & TMF_RecordFailures) {
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
            // If the second tuple can be initialized from a scalar,
            // fall back to that.
            if (tuple2->getFieldForScalarInit() >= 0)
              return Nothing;

            // Record this failure.
            if (flags & TMF_RecordFailures) {
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
        // If the second tuple can be initialized from a scalar, fall back to
        // that.
        if (tuple2->getFieldForScalarInit() >= 0)
          return Nothing;

        // Record this failure.
        if (flags & TMF_RecordFailures) {
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
                           LocatorPathElt::getTupleElement(i)),
                         trivial)) {
      case SolutionKind::Error:
        return SolutionKind::Error;

      case SolutionKind::TriviallySolved:
        break;

      case SolutionKind::Solved:
        result = SolutionKind::Solved;
        break;

      case SolutionKind::Unsolved:
        result = SolutionKind::Unsolved;
        break;
      }
    }
    return result;
  }

  assert(kind == TypeMatchKind::Conversion);

  // Compute the element shuffles for conversions.
  SmallVector<int, 16> sources;
  SmallVector<unsigned, 4> variadicArguments;
  if (computeTupleShuffle(tuple1, tuple2, sources, variadicArguments)) {
    // If the second tuple can be initialized from a scalar, fall back to
    // that.
    if (tuple2->getFieldForScalarInit() >= 0)
      return Nothing;
    
    // FIXME: Record why the tuple shuffle couldn't be computed.
    return SolutionKind::Error;
  }

  // Check each of the elements.
  bool hasVarArg = false;
  SolutionKind result = SolutionKind::TriviallySolved;
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
                         LocatorPathElt::getTupleElement(idx1)),
                       trivial)) {
    case SolutionKind::Error:
      return SolutionKind::Error;

    case SolutionKind::TriviallySolved:
      break;

    case SolutionKind::Solved:
      result = SolutionKind::Solved;
      break;

    case SolutionKind::Unsolved:
      result = SolutionKind::Unsolved;
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
                           LocatorPathElt::getTupleElement(idx1)),
                         trivial)) {
      case SolutionKind::Error:
        return SolutionKind::Error;

      case SolutionKind::TriviallySolved:
        break;

      case SolutionKind::Solved:
        result = SolutionKind::Solved;
        break;

      case SolutionKind::Unsolved:
        result = SolutionKind::Unsolved;
        break;
      }
    }
  }

  return result;
}

ConstraintSystem::SolutionKind
ConstraintSystem::matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                     TypeMatchKind kind, unsigned flags,
                                     ConstraintLocatorBuilder locator,
                                     bool &trivial) {
  // An [auto_closure] function type can be a subtype of a
  // non-[auto_closure] function type.
  if (func1->isAutoClosure() != func2->isAutoClosure()) {
    if (func2->isAutoClosure() || kind < TypeMatchKind::TrivialSubtype) {
      // Record this failure.
      if (flags & TMF_RecordFailures) {
        recordFailure(getConstraintLocator(locator),
                      Failure::FunctionAutoclosureMismatch, func1, func2);
      }

      return SolutionKind::Error;
    }
  }

  // Determine how we match up the input/result types.
  TypeMatchKind subKind;
  switch (kind) {
  case TypeMatchKind::BindType:
  case TypeMatchKind::SameType:
  case TypeMatchKind::SameTypeRvalue:
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
                                     ConstraintLocator::FunctionArgument),
                                   trivial);
  if (result == SolutionKind::Error)
    return SolutionKind::Error;

  // Result type can be covariant (or equal).
  switch (matchTypes(func1->getResult(), func2->getResult(), subKind,
                     subFlags,
                     locator.withPathElement(ConstraintLocator::FunctionResult),
                     trivial)) {
  case SolutionKind::Error:
    return SolutionKind::Error;

  case SolutionKind::TriviallySolved:
    break;

  case SolutionKind::Solved:
    result = SolutionKind::Solved;
    break;

  case SolutionKind::Unsolved:
    result = SolutionKind::Unsolved;
    break;
  }

  return result;
}

/// \brief Map a type-matching kind to a constraint kind.
static ConstraintKind getConstraintKind(TypeMatchKind kind) {
  switch (kind) {
  case TypeMatchKind::BindType:
    return ConstraintKind::Bind;

  case TypeMatchKind::SameType:
    return ConstraintKind::Equal;

  case TypeMatchKind::SameTypeRvalue:
    return ConstraintKind::EqualRvalue;

  case TypeMatchKind::TrivialSubtype:
    return ConstraintKind::TrivialSubtype;

  case TypeMatchKind::Subtype:
    return ConstraintKind::Subtype;

  case TypeMatchKind::Conversion:
    return ConstraintKind::Conversion;
  }

  llvm_unreachable("unhandled type matching kind");
}

/// \brief Map a failed type-matching kind to a failure kind, generically.
static Failure::FailureKind getRelationalFailureKind(TypeMatchKind kind) {
  switch (kind) {
  case TypeMatchKind::BindType:
  case TypeMatchKind::SameType:
  case TypeMatchKind::SameTypeRvalue:
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
ConstraintSystem::matchTypes(Type type1, Type type2, TypeMatchKind kind,
                             unsigned flags,
                             ConstraintLocatorBuilder locator,
                             bool &trivial) {
  // Desugar both types.
  auto desugar1 = type1->getDesugaredType();
  auto desugar2 = type2->getDesugaredType();

  // If we have type variables that have been bound to fixed types, look through
  // to the fixed type.
  auto typeVar1 = dyn_cast<TypeVariableType>(desugar1);
  if (typeVar1) {
    if (auto fixed = getFixedType(typeVar1)) {
      type1 = fixed;
      desugar1 = fixed->getDesugaredType();
      typeVar1 = nullptr;
    }
  }

  auto typeVar2 = dyn_cast<TypeVariableType>(desugar2);
  if (typeVar2) {
    if (auto fixed = getFixedType(typeVar2)) {
      type2 = fixed;
      desugar2 = fixed->getDesugaredType();
      typeVar2 = nullptr;
    }
  }

  // If we have a same-type-as-rvalue constraint, and the right-hand side
  // has a form that is either definitely an lvalue or definitely an rvalue,
  // force the right-hand side to be an rvalue and tighten the constraint
  // to a same-type constraint.
  if (kind == TypeMatchKind::SameTypeRvalue) {
    if (isa<LValueType>(desugar2)) {
      // The right-hand side is an lvalue type. Strip off the lvalue and
      // call this a normal 'same-type' constraint.
      type2 = type2->castTo<LValueType>()->getObjectType();
      desugar2 = type2->getDesugaredType();
      kind = TypeMatchKind::SameType;
      flags |= TMF_GenerateConstraints;
    } else if (!type2->is<TypeVariableType>()) {
      // The right-hand side is guaranteed to be an rvalue type. Call this
      // a normal same-type constraint.
      kind = TypeMatchKind::SameType;
      flags |= TMF_GenerateConstraints;
    }

    if (auto desugarFunc2 = dyn_cast<FunctionType>(desugar2)) {
      // The right-hand side is a function type, which is guaranteed to be
      // an rvalue type. Call this a normal same-type constraint, and
      // strip off the [auto_closure], which is not part of the type.
      if (desugarFunc2->isAutoClosure()) {
        auto func2 = type2->castTo<FunctionType>();
        type2 = FunctionType::get(func2->getInput(), func2->getResult(),
                                  TC.Context);
        desugar2 = type2.getPointer();
      }
      kind = TypeMatchKind::SameType;
      flags |= TMF_GenerateConstraints;
    }
  }

  // If the types are obviously equivalent, we're done.
  if (desugar1 == desugar2)
    return SolutionKind::TriviallySolved;

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
          return SolutionKind::TriviallySolved;
        }

        // Merge the equivalence classes corresponding to these two variables.
        mergeEquivalenceClasses(rep1, rep2);
        return SolutionKind::Solved;
      }

      // Provide a fixed type for the type variable.
      bool wantRvalue = kind == TypeMatchKind::SameType;
      if (typeVar1)
        assignFixedType(typeVar1, wantRvalue ? type2->getRValueType() : type2);
      else
        assignFixedType(typeVar2, wantRvalue ? type1->getRValueType() : type1);
      return SolutionKind::Solved;
    }

    case TypeMatchKind::SameTypeRvalue:
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
        return typeVar1 == typeVar2? SolutionKind::TriviallySolved
                                   : SolutionKind::Unsolved;
        
      break;
    }
  }

  // Decompose parallel structure.
  unsigned subFlags = flags | TMF_GenerateConstraints;
  if (desugar1->getKind() == desugar2->getKind()) {
    switch (desugar1->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
      llvm_unreachable("Type has not been desugared completely");

#define ALWAYS_CANONICAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
        if (desugar1 == desugar2) {
          return SolutionKind::TriviallySolved;
        }

        // Record this failure.
        if (flags & TMF_RecordFailures) {
          recordFailure(getConstraintLocator(locator),
                        getRelationalFailureKind(kind), type1, type2);
        }

        return SolutionKind::Error;

    case TypeKind::Error:
      return SolutionKind::Error;

    case TypeKind::UnstructuredUnresolved:
      llvm_unreachable("Unstructured unresolved type");

    case TypeKind::TypeVariable:
      // Nothing to do here; handle type variables below.
      break;

    case TypeKind::Tuple: {
      auto tuple1 = cast<TupleType>(desugar1);
      auto tuple2 = cast<TupleType>(desugar2);
      if (auto result = matchTupleTypes(tuple1, tuple2, kind, flags, locator,
                                        trivial))
        return *result;

      // Break out to attempt scalar-to-tuple conversion, below.
      break;
    }

    case TypeKind::OneOf:
    case TypeKind::Struct:
    case TypeKind::Class:
    case TypeKind::Protocol: {
      auto nominal1 = cast<NominalType>(desugar1);
      auto nominal2 = cast<NominalType>(desugar2);
      if (nominal1->getDecl() == nominal2->getDecl()) {
        assert((bool)nominal1->getParent() == (bool)nominal2->getParent() &&
               "Mismatched parents of nominal types");

        if (!nominal1->getParent())
          return SolutionKind::TriviallySolved;

        // Match up the parents, exactly.
        return matchTypes(nominal1->getParent(), nominal2->getParent(),
                          TypeMatchKind::SameType, subFlags,
                          locator.withPathElement(
                            ConstraintLocator::ParentType),
                          trivial);
      }
      break;
    }

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
                          ConstraintLocator::InstanceType),
                        trivial);
    }

    case TypeKind::Function: {
      auto func1 = cast<FunctionType>(desugar1);
      auto func2 = cast<FunctionType>(desugar2);
      return matchFunctionTypes(func1, func2, kind, flags, locator, trivial);
    }

    case TypeKind::PolymorphicFunction:
      llvm_unreachable("Polymorphic function type should have been opened");

    case TypeKind::Array: {
      auto array1 = cast<ArrayType>(desugar1);
      auto array2 = cast<ArrayType>(desugar2);
      return matchTypes(array1->getBaseType(), array2->getBaseType(),
                        TypeMatchKind::SameType, subFlags,
                        locator.withPathElement(
                          ConstraintLocator::ArrayElementType),
                        trivial);
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
        if (flags & TMF_RecordFailures) {
          recordFailure(getConstraintLocator(locator),
                        Failure::LValueQualifiers, type1, type2);
        }

        return SolutionKind::Error;
      }

      return matchTypes(lvalue1->getObjectType(), lvalue2->getObjectType(),
                        TypeMatchKind::SameType, subFlags,
                        locator.withPathElement(
                          ConstraintLocator::ArrayElementType),
                        trivial);
    }

    case TypeKind::UnboundGeneric:
      llvm_unreachable("Unbound generic type should have been opened");

    case TypeKind::BoundGenericClass:
    case TypeKind::BoundGenericOneOf:
    case TypeKind::BoundGenericStruct: {
      auto bound1 = cast<BoundGenericType>(desugar1);
      auto bound2 = cast<BoundGenericType>(desugar2);
      
      if (bound1->getDecl() == bound2->getDecl()) {
        // Match up the parents, exactly, if there are parents.
        SolutionKind result = SolutionKind::TriviallySolved;
        assert((bool)bound1->getParent() == (bool)bound2->getParent() &&
               "Mismatched parents of bound generics");
        if (bound1->getParent()) {
          switch (matchTypes(bound1->getParent(), bound2->getParent(),
                             TypeMatchKind::SameType, TMF_GenerateConstraints,
                             locator.withPathElement(
                               ConstraintLocator::ParentType),
                             trivial)) {
          case SolutionKind::Error:
            // There may still be a conversion that can satisfy the constraint.
            // FIXME: The recursive match may have introduced new equality
            // constraints that are now invalid. rdar://problem/13140447
            if (kind >= TypeMatchKind::Conversion)
              break;

            // Record this failure.
            if (flags & TMF_RecordFailures) {
              recordFailure(getConstraintLocator(
                              locator.withPathElement(
                                ConstraintLocator::ParentType)),
                            getRelationalFailureKind(kind), type1, type2);
            }

            return SolutionKind::Error;

          case SolutionKind::TriviallySolved:
            break;

          case SolutionKind::Solved:
            result = SolutionKind::Solved;
            break;

          case SolutionKind::Unsolved:
            result = SolutionKind::Unsolved;
            break;
          }
        }

        // Match up the generic arguments, exactly.
        auto args1 = bound1->getGenericArgs();
        auto args2 = bound2->getGenericArgs();
        bool checkConversions = false;
        assert(args1.size() == args2.size() && "Mismatched generic args");
        for (unsigned i = 0, n = args1.size(); i != n; ++i) {
          switch (matchTypes(args1[i], args2[i], TypeMatchKind::SameType,
                             TMF_GenerateConstraints,
                             locator.withPathElement(
                               LocatorPathElt::getGenericArgument(i)),
                             trivial)) {
          case SolutionKind::Error:
            // There may still be a conversion that can satisfy this constraint.
            // FIXME: The recursive match may have introduced new equality
            // constraints that are now invalid. rdar://problem/13140447
            if (kind >= TypeMatchKind::Conversion) {
              checkConversions = true;
              break;
            }

            // Record this failure.
            if (flags & TMF_RecordFailures) {
              recordFailure(getConstraintLocator(
                              locator.withPathElement(
                                LocatorPathElt::getGenericArgument(i))),
                            getRelationalFailureKind(kind), type1, type2);
            }

            return SolutionKind::Error;

          case SolutionKind::TriviallySolved:
            break;

          case SolutionKind::Solved:
            result = SolutionKind::Solved;
            break;

          case SolutionKind::Unsolved:
            result = SolutionKind::Unsolved;
            break;
          }
        }

        if (!checkConversions)
          return result;
      }
      break;
    }
    }
  }

  // FIXME: Materialization

  bool concrete = !typeVar1 && !typeVar2;
  if (concrete && kind >= TypeMatchKind::TrivialSubtype) {
    if (auto tuple2 = type2->getAs<TupleType>()) {
      // A scalar type is a trivial subtype of a one-element, non-variadic tuple
      // containing a single element if the scalar type is a subtype of
      // the type of that tuple's element.
      if (tuple2->getFields().size() == 1 &&
          !tuple2->getFields()[0].isVararg()) {
        return matchTypes(type1, tuple2->getElementType(0), kind, subFlags,
                          locator.withPathElement(
                            ConstraintLocator::ScalarToTuple),
                          trivial);
      }

      // A scalar type can be converted to a tuple so long as there is at
      // most one non-defaulted element.
      if (kind >= TypeMatchKind::Conversion) {
        int scalarFieldIdx = tuple2->getFieldForScalarInit();
        if (scalarFieldIdx >= 0) {
          const auto &elt = tuple2->getFields()[scalarFieldIdx];
          auto scalarFieldTy = elt.isVararg()? elt.getVarargBaseTy()
                                             : elt.getType();
          return matchTypes(type1, scalarFieldTy, kind, subFlags,
                            locator.withPathElement(
                              ConstraintLocator::ScalarToTuple),
                            trivial);
        }
      }
    }

    if (type1->mayHaveSuperclass() && type2->mayHaveSuperclass()) {
      // Determines whether the first type is derived from the second.
      auto solveDerivedFrom =
        [&](Type type1, Type type2) -> Optional<SolutionKind> {
          if (auto archetype2 = type2->getAs<ArchetypeType>()) {
            type2 = archetype2->getSuperclass();
          }
          auto classDecl2 = type2->getClassOrBoundGenericClass();

          for (auto super1 = TC.getSuperClassOf(type1); super1;
               super1 = TC.getSuperClassOf(super1)) {
            if (super1->getClassOrBoundGenericClass() != classDecl2)
              continue;

            // FIXME: If we end up generating any constraints from this
            // match, we can't solve them immediately. We'll need to
            // split into another system.
            switch (auto result = matchTypes(super1, type2,
                                             TypeMatchKind::SameType,
                                             TMF_GenerateConstraints, locator,
                                             trivial)) {
              case SolutionKind::Error:
                continue;

              case SolutionKind::Solved:
              case SolutionKind::TriviallySolved:
              case SolutionKind::Unsolved:
                return result;
            }
          }

          return Nothing;
        };

      // A class (or bound generic class) is a subtype of another class
      // (or bound generic class) if it is derived from that class.
      if (auto upcastResult = solveDerivedFrom(type1, type2))
        return *upcastResult;
    }
  }

  if (concrete && kind >= TypeMatchKind::Conversion) {
    // An lvalue of type T1 can be converted to a value of type T2 so long as
    // T1 is convertible to T2 (by loading the value).
    if (auto lvalue1 = type1->getAs<LValueType>()) {
      return matchTypes(lvalue1->getObjectType(), type2, kind, subFlags,
                        locator, trivial);
    }

    // An expression can be converted to an auto-closure function type, creating
    // an implicit closure.
   if (auto function2 = type2->getAs<FunctionType>()) {
      if (function2->isAutoClosure()) {
        trivial = false;
        return matchTypes(type1, function2->getResult(), kind, subFlags,
                          locator.withPathElement(ConstraintLocator::Load),
                          trivial);
      }
    }
  }

  // For a subtyping relation involving two existential types, or a conversion
  // from any type, check whether the first type conforms to each of the
  // protocols in the second type.
  if (concrete &&
      (kind >= TypeMatchKind::Conversion ||
       (kind == TypeMatchKind::Subtype && type1->isExistentialType()))) {
    SmallVector<ProtocolDecl *, 4> protocols;

    if (type2->isExistentialType(protocols)) {
      // Substitute all type variables in the first type
      auto substType1 = simplifyType(type1);
      if (substType1->hasTypeVariable()) {
        // If type variables remain, we can't solve this now.
        // FIXME: In many cases, we *can* solve this now, because a generic
        // type will unconditionally conform to the named protocols.
        return SolutionKind::Unsolved;
      }

      for (auto proto : protocols) {
        if (!TC.conformsToProtocol(substType1, proto)) {
          // Record this failure.
          if (flags & TMF_RecordFailures) {
            recordFailure(getConstraintLocator(locator),
                          Failure::DoesNotConformToProtocol, type1,
                          proto->getDeclaredType());
          }

          return SolutionKind::Error;
        }
      }

      trivial = false;
      return SolutionKind::TriviallySolved;
    }
  }
  
  // A nominal type can be converted to another type via a user-defined
  // conversion function.
  if (concrete && kind >= TypeMatchKind::Conversion &&
      type1->getNominalOrBoundGenericNominal()) {
    auto &context = getASTContext();
    // FIXME: lame name!
    auto name = context.getIdentifier("__conversion");
    MemberLookup &lookup = lookupMember(type1, name);
    if (lookup.isSuccess()) {
      auto inputTV = createTypeVariable();
      auto outputTV = createTypeVariable();

      // The conversion function will have function type TI -> TO, for fresh
      // type variables TI and TO.
      addValueMemberConstraint(type1, name,
                               FunctionType::get(inputTV, outputTV, context),
                               getConstraintLocator(
                                  locator.withPathElement(
                                    ConstraintLocator::ConversionMember)));

      // A conversion function must accept an empty parameter list ().
      // Note: This should never fail, because the declaration checker
      // should ensure that conversions have no non-defaulted parameters.
      addConstraint(ConstraintKind::Conversion, TupleType::getEmpty(context),
                    inputTV, getConstraintLocator(locator));

      // The output of the conversion function must be a subtype of the
      // type we're trying to convert to. The use of subtyping here eliminates
      // multiple-step user-defined conversions, which also eliminates concerns
      // about cyclic conversions causing infinite loops in the constraint
      // solver.
      addConstraint(ConstraintKind::Subtype, outputTV, type2,
                    getConstraintLocator(
                      locator.withPathElement(
                        ConstraintLocator::ConversionResult)));
      
      return SolutionKind::Solved;
    }
  }

  // If one of the types is a type variable, we leave this unsolved.
  if (typeVar1 || typeVar2)
    return SolutionKind::Unsolved;

  // If we are supposed to record failures, do so.
  if (flags & TMF_RecordFailures) {
    recordFailure(getConstraintLocator(locator),
                  getRelationalFailureKind(kind), type1, type2);
  }

  return SolutionKind::Error;
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

void ConstraintSystem::resolveOverload(OverloadSet *ovl, unsigned idx) {
  // Determie the type to which we'll bind the overload set's type.
  auto &choice = ovl->getChoices()[idx];
  Type refType;
  switch (choice.getKind()) {
  case OverloadChoiceKind::Decl: {
    // Retrieve the type of a reference to the specific declaration choice.
    if (choice.getBaseType())
      refType = getTypeOfMemberReference(choice.getBaseType(),
                                         choice.getDecl(),
                                         /*FIXME:*/false);
    else
      refType = getTypeOfReference(choice.getDecl());

    bool isAssignment = choice.getDecl()->getAttrs().isAssignment();
    refType = adjustLValueForReference(refType, isAssignment,
                                       getASTContext());
    break;
  }

  case OverloadChoiceKind::BaseType:
    refType = choice.getBaseType();
    break;

  case OverloadChoiceKind::FunctionReturningBaseType:
    refType = FunctionType::get(createTypeVariable(),
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
  addConstraint(ConstraintKind::Bind, ovl->getBoundType(), refType);

  // Note that we have resolved this overload.
  ResolvedOverloads[ovl] = std::make_pair(idx, refType);
  if (solverState) {
    solverState->resolvedOverloadSets.push_back(ovl);

    if (TC.getLangOpts().DebugConstraintSolver) {
      llvm::errs().indent(solverState->depth * 2)
        << "(overload set #" << ovl->getID() << " choice #" << idx << ": "
        << ovl->getBoundType()->getString() << " := "
        << refType->getString() << ")\n";
    }
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
    
  case TypeKind::Error:
    return SolutionKind::Error;
    
  case TypeKind::UnstructuredUnresolved:
    llvm_unreachable("Unstructured unresolved type");
    
  case TypeKind::TypeVariable:
    return SolutionKind::Unsolved;

  case TypeKind::Tuple: {
    // Tuple construction is simply tuple conversion.
    bool trivial = false;
    return matchTypes(argType, valueType, TypeMatchKind::Conversion,
                      flags|TMF_GenerateConstraints, locator, trivial);
  }

  case TypeKind::OneOf:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericOneOf:
  case TypeKind::BoundGenericStruct:
    // Break out to handle the actual construction below.
    break;

  case TypeKind::PolymorphicFunction:
    llvm_unreachable("Polymorphic function type should have been opened");

  case TypeKind::UnboundGeneric:
    llvm_unreachable("Unbound generic type should have been opened");

#define ALWAYS_CANONICAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::MetaType:
  case TypeKind::Function:
  case TypeKind::Array:
  case TypeKind::ProtocolComposition:
  case TypeKind::LValue:
  case TypeKind::Protocol:
    // If we are supposed to record failures, do so.
    if (flags & TMF_RecordFailures) {
      recordFailure(locator, Failure::TypesNotConstructible,
                    valueType, argType);
    }
    
    return SolutionKind::Error;
  }

  SmallVector<ValueDecl *, 4> ctors;
  if (!TC.lookupConstructors(valueType, ctors)) {
    // If we are supposed to record failures, do so.
    if (flags & TMF_RecordFailures) {
      recordFailure(locator, Failure::TypesNotConstructible,
                    valueType, argType);
    }
    
    return SolutionKind::Error;
  }

  auto &context = getASTContext();
  // FIXME: lame name
  auto name = context.getIdentifier("constructor");
  auto tv = createTypeVariable();
  
  // The constructor will have function type T -> T2, for a fresh type
  // variable T. Note that these constraints specifically require a
  // match on the result type because the constructors for oneofs and struct
  // types always return a value of exactly that type.
  addValueMemberConstraint(valueType, name,
                           FunctionType::get(tv, valueType, context),
                           getConstraintLocator(
                             locator, 
                             ConstraintLocator::ConstructorMember));
  
  // The first type must be convertible to the constructor's argument type.
  addConstraint(ConstraintKind::Conversion, argType, tv,
                getConstraintLocator(locator,
                                     ConstraintLocator::ApplyArgument));

  return SolutionKind::Solved;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyLiteralConstraint(Type type, LiteralKind kind,
                                            ConstraintLocator *locator) {
  if (auto tv = dyn_cast<TypeVariableType>(type.getPointer())) {
    auto fixed = getFixedType(tv);
    if (!fixed)
      return SolutionKind::Unsolved;

    // Continue with the fixed type.
    type = fixed;
  }

  // Collection literals are special, because all of the real
  // requirements for them are kept separate from the
  // collection-literal requirement. Let this constraint be satisfied
  // when we've picked a concrete type.
  if (kind == LiteralKind::Array || kind == LiteralKind::Dictionary) {
    return (type->is<NominalType>() || type->is<BoundGenericType>())
             ? SolutionKind::TriviallySolved
             : SolutionKind::Unsolved;
  }

  // Look up the entry for this type/literalkind pair in LiteralChecks.
  unsigned &known = LiteralChecks[{type->getCanonicalType(), (unsigned)kind}];

  // If we haven't already checked whether this type is literal compatible, do
  // it now.
  if (known == 0) {
    // FIXME: We should do this caching in the translation unit.
    known = 1+(TC.isLiteralCompatibleType(type, SourceLoc(), kind,
                                          /*Complain=*/false).first != nullptr);
  }

  if (known == 2) {
    return SolutionKind::TriviallySolved;
  }

  // Record this failure.
  recordFailure(locator, Failure::IsNotLiteralType, type, kind);
  return  SolutionKind::Error;
}

/// \brief Determine whether the given protocol member's signature involves
/// any associated types.
///
/// Used to
static bool involvesAssociatedTypes(TypeChecker &tc, ValueDecl *decl) {
  Type type = decl->getType();

  // For a function or constructor,
  // Note that there are no destructor requirements, so we don't need to check
  // for destructors.
  if (isa<FuncDecl>(decl) || isa<ConstructorDecl>(decl))
    type = type->castTo<AnyFunctionType>()->getResult();

  // FIXME: Lame way to perform a search.
  return tc.transformType(type, [](Type type) -> Type {
    if (auto archetype = type->getAs<ArchetypeType>()) {
      if (archetype->getParent())
        return nullptr;
    }
    return type;
  }).isNull();
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyMemberConstraint(const Constraint &constraint) {
  // Resolve the base type, if we can. If we can't resolve the base type,
  // then we can't solve this constraint.
  Type baseTy = simplifyType(constraint.getFirstType());
  Type baseObjTy = baseTy->getRValueType();
  
  if (baseObjTy->is<TypeVariableType>() ||
      (baseObjTy->is<MetaTypeType>() && 
       baseObjTy->castTo<MetaTypeType>()->getInstanceType()
         ->is<TypeVariableType>()))
    return SolutionKind::Unsolved;
  
  // If the base type is a tuple type, look for the named or indexed member
  // of the tuple.
  Identifier name = constraint.getMember();
  Type memberTy = constraint.getSecondType();
  if (auto baseTuple = baseObjTy->getAs<TupleType>()) {
    StringRef nameStr = name.str();
    int fieldIdx = -1;
    bool isNamed;
    // Resolve a number reference into the tuple type.
    unsigned Value = 0;
    if (!nameStr.getAsInteger(10, Value) &&
        Value < baseTuple->getFields().size()) {
      fieldIdx = Value;
      isNamed = false;
    } else {
      fieldIdx = baseTuple->getNamedElementId(name);
      isNamed = true;
    }

    if (fieldIdx == -1) {
      recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                    baseObjTy, name);
      return SolutionKind::Error;
    }

    // Add an overload set that selects this field.
    OverloadChoice choice(baseTy, fieldIdx);
    addOverloadSet(OverloadSet::getNew(*this, memberTy, constraint.getLocator(),
                                       { &choice, 1 }));
    return SolutionKind::Solved;
  }

  // FIXME: If the base type still involves type variables, we want this
  // constraint to be unsolved. This effectively requires us to solve the
  // left-hand side of a dot expression before we look for members.

  bool isExistential = baseObjTy->isExistentialType();
  if (name.str() == "constructor") {
    // Constructors have their own approach to name lookup.
    SmallVector<ValueDecl *, 4> ctors;
    if (!TC.lookupConstructors(baseObjTy, ctors)) {
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
      // If our base is an existential type, we can't make use of any
      // constructor whose signature involves associated types.
      // FIXME: Mark this as 'unavailable'.
      if (isExistential &&
          involvesAssociatedTypes(getTypeChecker(), constructor))
        continue;

      choices.push_back(OverloadChoice(baseTy, constructor));
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
    
    addOverloadSet(OverloadSet::getNew(*this, memberTy, constraint.getLocator(),
                                       choices));
    return SolutionKind::Solved;
  }

  // Look for members within the base.
  MemberLookup &lookup = lookupMember(baseObjTy, name);
  if (!lookup.isSuccess()) {
    recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                  baseObjTy, name);

    return SolutionKind::Error;
  }

  // FIXME: If we expect a type member, make sure we actually got type members.

  // Introduce a new overload set to capture the choices.
  SmallVector<OverloadChoice, 4> choices;
  bool isMetatype = baseObjTy->is<MetaTypeType>();
  for (auto &result : lookup.Results) {
    // If our base is an existential type, we can't make use of any
    // member whose signature involves associated types.
    // FIXME: Mark this as 'unavailable'.
    if (isExistential && involvesAssociatedTypes(getTypeChecker(), result.D))
      continue;

    // If we are looking for a metatype member, don't include members that can
    // only be accessed on an instance of the object.
    // FIXME: Mark as 'unavailable' somehow.
    if (isMetatype &&
        !(isa<FuncDecl>(result.D) ||
          isa<OneOfElementDecl>(result.D) ||
          !result.D->isInstanceMember())) {
      continue;
    }

    choices.push_back(OverloadChoice(baseTy, result.D));
  }

  if (choices.empty()) {
    recordFailure(constraint.getLocator(), Failure::DoesNotHaveMember,
                  baseObjTy, name);
    return SolutionKind::Error;
  }
  auto locator = getConstraintLocator(constraint.getLocator());
  addOverloadSet(OverloadSet::getNew(*this, memberTy, locator, choices));
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
    return SolutionKind::TriviallySolved;
  }

  // Record this failure.
  recordFailure(constraint.getLocator(), Failure::IsNotArchetype, baseTy);
  return SolutionKind::Error;
}

/// \brief Retrieve the type-matching kind corresponding to the given
/// constraint kind.
static TypeMatchKind getTypeMatchKind(ConstraintKind kind) {
  switch (kind) {
  case ConstraintKind::Bind: return TypeMatchKind::BindType;
  case ConstraintKind::Equal: return TypeMatchKind::SameType;
  case ConstraintKind::EqualRvalue: return TypeMatchKind::SameTypeRvalue;
  case ConstraintKind::TrivialSubtype: return TypeMatchKind::TrivialSubtype;
  case ConstraintKind::Subtype: return TypeMatchKind::Subtype;
  case ConstraintKind::Conversion: return TypeMatchKind::Conversion;

  case ConstraintKind::Construction:
    llvm_unreachable("Construction constraints don't involve type matches");

  case ConstraintKind::Literal:
    llvm_unreachable("Literals don't involve type matches");

  case ConstraintKind::ValueMember:
  case ConstraintKind::TypeMember:
    llvm_unreachable("Member constraints don't involve type matches");

  case ConstraintKind::Archetype:
    llvm_unreachable("Archetype constraints don't involve type matches");
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyConstraint(const Constraint &constraint) {
  switch (constraint.getKind()) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::EqualRvalue:
  case ConstraintKind::TrivialSubtype:
  case ConstraintKind::Subtype:
  case ConstraintKind::Conversion: {
    // For relational constraints, match up the types.
    bool trivial = true;
    return matchTypes(constraint.getFirstType(), constraint.getSecondType(),
                      getTypeMatchKind(constraint.getKind()),
                      TMF_RecordFailures, constraint.getLocator(), trivial);
  }

  case ConstraintKind::Construction:
    return simplifyConstructionConstraint(constraint.getSecondType(),
                                          constraint.getFirstType(),
                                          TMF_RecordFailures,
                                          constraint.getLocator());

  case ConstraintKind::Literal:
    return simplifyLiteralConstraint(constraint.getFirstType(),
                                     constraint.getLiteralKind(),
                                     constraint.getLocator());

  case ConstraintKind::ValueMember:
  case ConstraintKind::TypeMember:
    return simplifyMemberConstraint(constraint);

  case ConstraintKind::Archetype:
    return simplifyArchetypeConstraint(constraint);
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

             // Strip the implicit bit off lvalue types.
             if (auto lvalue = type->getAs<LValueType>()) {
               auto wantQuals = lvalue->getQualifiers() -
                                  (LValueType::Qual::Implicit |
                                   LValueType::Qual::NonSettable);
               if (lvalue->getQualifiers() != wantQuals) {
                 auto objectType = simplifyType(tc, lvalue->getObjectType());
                 type = LValueType::get(objectType, wantQuals, tc.Context);
               }
             }

             return type;
           });
}

//===--------------------------------------------------------------------===//
// Ranking solutions
//===--------------------------------------------------------------------===//
#pragma mark Ranking solutions

bool ConstraintSystem::typeMatchesDefaultLiteralConstraint(TypeVariableType *tv,
                                                           Type type) {
  tv = getRepresentative(tv);

  // FIXME:
  for (auto constraint : Constraints) {
    // We only care about literal constraints...
    if (constraint->getClassification() != ConstraintClassification::Literal)
      continue;

    // on type variables...
    auto constraintTV
      = dyn_cast<TypeVariableType>(constraint->getFirstType().getPointer());
    if (!constraintTV)
      continue;

    // that have the same representative as the type we care about.
    if (getRepresentative(constraintTV) != tv)
      continue;

    // If the type we were given matches the default literal type for this
    // constraint, we found what we're looking for.
    // FIXME: isEqual() isn't right for Slice<T>.
    if (type->isEqual(TC.getDefaultLiteralType(constraint->getLiteralKind())))
      return true;
  }

  return false;
}

/// \brief Updates a solution comparison result based on whether a given metric
/// considers one solution set to be "at least as good as" the other.
///
/// \returns true if the caller should return the result immediately, because no
/// additional information would change it.
static bool updateSolutionCompareResult(SolutionCompareResult &result,
                                        bool firstAsGoodAsSecond,
                                        bool secondAsGoodAsFirst) {
  if (firstAsGoodAsSecond && secondAsGoodAsFirst) {
    result = SolutionCompareResult::Incomparable;
    return true;
  }

  if (firstAsGoodAsSecond != secondAsGoodAsFirst) {
    switch (result) {
    case SolutionCompareResult::Incomparable:
      return false;

    case SolutionCompareResult::Identical:
      result = firstAsGoodAsSecond? SolutionCompareResult::Better
                                  : SolutionCompareResult::Worse;
      break;

    case SolutionCompareResult::Better:
      if (secondAsGoodAsFirst)
        result = SolutionCompareResult::Incomparable;
      break;

    case SolutionCompareResult::Worse:
      if (firstAsGoodAsSecond)
        result =  SolutionCompareResult::Incomparable;
      break;
    }
  }

  return false;
}

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
                                               nullptr,
                                               field.getVarargBaseTy()));
                                               
               }
               return TupleType::get(fields, tc.Context);
             }
             return type;
           });
}

SolutionCompareResult ConstraintSystem::compareSolutions(ConstraintSystem &cs,
                                                         const Solution &sol1,
                                                         const Solution &sol2) {
  // Compare the sets of bound type variables in the two systems.
  auto compareTypeVariables =
    [&](const Solution &sol1, const Solution &sol2) -> SolutionCompareResult {
      SolutionCompareResult result = SolutionCompareResult::Identical;
      
      for (auto fixedTV1 : sol1.typeBindings) {
        auto boundTV1 = fixedTV1.first;

        // Find the fixed type in the second constraint system.
        auto known2 = sol2.typeBindings.find(boundTV1);
        if (known2 == sol2.typeBindings.end())
          continue;

        Type type2 = known2->second;

        auto type1 = fixedTV1.second;

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
        if (type1->hasTypeVariable() || type2->hasTypeVariable())
          return SolutionCompareResult::Incomparable;

        // If one type is a subtype of the other, but not vice-verse,
        // we prefer the system with the more-constrained type.
        // FIXME: Collapse this check into the second check.
        bool type1Trivial = false;
        bool type1Better = cs.matchTypes(type1, type2,
                                         TypeMatchKind::Subtype,
                                         TMF_None,
                                         ConstraintLocatorBuilder(nullptr),
                                         type1Trivial)
                             == SolutionKind::TriviallySolved;
        bool type2Trivial = false;
        bool type2Better = cs.matchTypes(type2, type1,
                                         TypeMatchKind::Subtype,
                                         TMF_None,
                                         ConstraintLocatorBuilder(nullptr),
                                         type2Trivial)
                             == SolutionKind::TriviallySolved;
        if (type1Better && type2Better) {
          if (updateSolutionCompareResult(result, type1Trivial, type2Trivial))
            return result;
        } else {
          if (updateSolutionCompareResult(result, type1Better, type2Better))
            return result;
        }
        if (type1Better || type2Better) {
          continue;
        }

        // If one type is convertible to of the other, but not vice-versa.
        type1Trivial = false;
        type1Better = cs.matchTypes(type1, type2,
                                    TypeMatchKind::Conversion,
                                    TMF_None,
                                    ConstraintLocatorBuilder(nullptr),
                                    type1Trivial)
                        == SolutionKind::TriviallySolved;
        type2Trivial = false;
        type2Better = cs.matchTypes(type2, type1,
                                    TypeMatchKind::Conversion,
                                    TMF_None,
                                    ConstraintLocatorBuilder(nullptr),
                                    type2Trivial)
                        == SolutionKind::TriviallySolved;
        if (updateSolutionCompareResult(result, type1Better, type2Better))
          return result;
        if (type1Better || type2Better) {
          continue;
        }
        
        // If the type variable was bound by a literal constraint, and the
        // type it is bound to happens to match the default literal
        // constraint in one system but not the other, we prefer the one
        // that matches the default.
        // Note that the constraint will be available in the parent of
        // the constraint system that assumed a value for the type variable
        // (or, of course, in the original system, since these constraints
        // are generated directly from the expression).
        // FIXME: Make it efficient to find these constraints. This is
        // silly.
        bool defaultLit1
          = cs.typeMatchesDefaultLiteralConstraint(boundTV1, type1);
        bool defaultLit2
          = cs.typeMatchesDefaultLiteralConstraint(boundTV1, type2);
        if (updateSolutionCompareResult(result, defaultLit1, defaultLit2))
          return result;

        // Prefer concrete types to existentials.
        bool isExistential1 = type1->isExistentialType();
        bool isExistential2 = type2->isExistentialType();
        if (updateSolutionCompareResult(result, isExistential1, isExistential2))
          return result;
      }

      return result;
    };

  // Compare the type variables bound in the first system to the type variables
  // bound in the second type system.
  SolutionCompareResult result = compareTypeVariables(sol1, sol2);
  
  // FIXME: There might be variables bound in the second type system but not
  // the first, but for now we don't really care.

  // FIXME: Compare overload sets.

  switch (result) {
  case SolutionCompareResult::Incomparable:
  case SolutionCompareResult::Better:
  case SolutionCompareResult::Worse:
    return result;

  case SolutionCompareResult::Identical:
    // FIXME: We haven't checked enough to conclude that two solutions are
    // identical. But it's convenient to call them identical.
    return SolutionCompareResult::Identical;
  }
}

Solution *
ConstraintSystem::findBestSolution(SmallVectorImpl<Solution> &viable){
  if (viable.empty())
    return nullptr;
  if (viable.size() == 1)
    return &viable[0];

  // Find a potential best. 
  Solution *best = nullptr;
  for (auto &solution : viable) {
    if (!best) {
      // Found the first solved system.
      best = &solution;
      continue;
    }

    switch (compareSolutions(*this, solution, *best)) {
    case SolutionCompareResult::Identical:
      // FIXME: Might want to warn about this in debug builds, so we can
      // find a way to eliminate the redundancy in the search space.
    case SolutionCompareResult::Incomparable:
    case SolutionCompareResult::Worse:
      break;

    case SolutionCompareResult::Better:
      best = &solution;
      break;
    }
  }

  if (!best)
    return nullptr;

  // Make sure that our current best is better than all of the solved systems.
  for (auto &solution : viable) {
    if (best == &solution)
      continue;

    switch (compareSolutions(*this, *best, solution)) {
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

  return best;
}

//===--------------------------------------------------------------------===//
// High-level entry points.
//===--------------------------------------------------------------------===//
namespace {
  class PreCheckExpression : public ASTWalker {
    TypeChecker &TC;

  public:
    PreCheckExpression(TypeChecker &tc) : TC(tc) { }

    bool walkToExprPre(Expr *expr) {
      // For FuncExprs, we just want to type-check the patterns as written,
      // but not walk into the body. The body will by type-checked separately.
      if (auto func = dyn_cast<FuncExpr>(expr)) {
        TC.semaFuncExpr(func, /*isFirstPass=*/false,
                        /*allowUnknownTypes=*/true);
        return false;
      }

      // For closures, type-check the patterns and result type as written,
      // but do not walk into the body. That will be type-checked after
      // we've determine the complete function type.
      if (auto closure = dyn_cast<PipeClosureExpr>(expr)) {
        // Validate the parameters.
        if (TC.typeCheckPattern(closure->getParams(), true, true)) {
          expr->setType(ErrorType::get(TC.Context));
          return false;
        }

        // Validate the result type, if present.
        if (closure->hasExplicitResultType() &&
            TC.validateType(closure->getExplicitResultTypeLoc())) {
          expr->setType(ErrorType::get(TC.Context));
          return false;
        }
        
        return closure->hasSingleExpressionBody();
      }

      return true;
    }

    Expr *walkToExprPost(Expr *expr) {
      // Fold sequence expressions.
      if (auto seqExpr = dyn_cast<SequenceExpr>(expr)) {
        return TC.foldSequence(seqExpr);
      }

      // Type check the type in an array new expression.
      if (auto newArray = dyn_cast<NewArrayExpr>(expr)) {
        // FIXME: Check that the element type has a default constructor.
        
        if (TC.validateType(newArray->getElementTypeLoc()))
          return nullptr;

        // Check array bounds. They are subproblems that don't interact with
        // the surrounding expression context.
        for (unsigned i = newArray->getBounds().size(); i != 1; --i) {
          auto &bound = newArray->getBounds()[i-1];
          if (!bound.Value)
            continue;

          // All inner bounds must be constant.
          if (TC.typeCheckArrayBound(bound.Value, /*requireConstant=*/true))
            return nullptr;
        }

        // The outermost bound does not need to be constant.
        if (TC.typeCheckArrayBound(newArray->getBounds()[0].Value,
                                   /*requireConstant=*/false))
          return nullptr;

        return expr;
      }

      // Type check the type parameters in an UnresolvedSpecializeExpr.
      if (auto us = dyn_cast<UnresolvedSpecializeExpr>(expr)) {
        for (TypeLoc &type : us->getUnresolvedParams()) {
          if (TC.validateType(type)) {
            TC.diagnose(us->getLAngleLoc(),
                        diag::while_parsing_as_left_angle_bracket);
            return nullptr;
          }
        }
        return expr;
      }

      // Type check the type parameters in cast expressions.
      if (auto cast = dyn_cast<ExplicitCastExpr>(expr)) {
        if (TC.validateType(cast->getTypeLoc()))
          return nullptr;
        return expr;
      }
      
      if (auto is = dyn_cast<IsSubtypeExpr>(expr)) {
        if (TC.validateType(is->getTypeLoc()))
          return nullptr;
        return expr;
      }

      return expr;
    }

    bool walkToStmtPre(Stmt *stmt) {
      // Never walk into statements.
      return false;
    }
  };
}

#pragma mark High-level entry points
bool TypeChecker::typeCheckExpression(Expr *&expr, Type convertType){
  // FIXME: Old type checker.
  if (!getLangOpts().UseConstraintSolver) {
    return typeCheckExpressionOld(expr, convertType);
  }

  // First, pre-check the expression, validating any types that occur in the
  // expression and folding sequence expressions.
  expr = expr->walk(PreCheckExpression(*this));
  if (!expr)
    return true;

  llvm::raw_ostream &log = llvm::errs();

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this);
  if (cs.generateConstraints(expr))
    return true;

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  if (convertType) {
    cs.addConstraint(ConstraintKind::Conversion, expr->getType(), convertType,
                     cs.getConstraintLocator(expr, { }));
  }

  if (getLangOpts().DebugConstraintSolver) {
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump();
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Dumping constraints by default due to crummy diagnostics.
    if (getLangOpts().DebugConstraintSolver || true) {
      log << "---Solved constraints---\n";
      cs.dump();

      if (!viable.empty()) {
        unsigned idx = 0;
        for (auto &solution : viable) {
          log << "---Solution #" << ++idx << "---\n";
          solution.dump(&Context.SourceMgr);
        }
      }

      if (viable.size() == 0)
        log << "No solution found.\n";
      else if (viable.size() == 1)
        log << "Unique solution found.\n";
      else {
        log << "Found " << viable.size() << " potential solutions.\n";
      }
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr);
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
    result = solution.coerceToType(result, convertType);
    if (!result) {
      return true;
    }
  }

  if (getLangOpts().DebugConstraintSolver) {
    log << "---Type-checked expression---\n";
    result->dump();
  }

  expr = result;
  return false;
}

/// \brief Compute the rvalue type of the given expression, which is the
/// destination of an assignment statement.
static Type computeAssignDestType(ConstraintSystem &cs, Expr *dest,
                                  SourceLoc equalLoc) {
  if (TupleExpr *TE = dyn_cast<TupleExpr>(dest)) {
    auto &ctx = cs.getASTContext();
    SmallVector<TupleTypeElt, 4> destTupleTypes;
    for (unsigned i = 0; i != TE->getNumElements(); ++i) {
      Expr *subExpr = TE->getElement(i);
      Type elemTy = computeAssignDestType(cs, subExpr, equalLoc);
      if (!elemTy)
        return Type();
      destTupleTypes.push_back(TupleTypeElt(elemTy, TE->getElementName(i)));
    }

    return TupleType::get(destTupleTypes, ctx);
  }

  Type destTy = cs.simplifyType(dest->getType());
  if (LValueType *destLV = destTy->getAs<LValueType>()) {
    // If the destination is a settable lvalue, we're good; get its object type.
    if (!destLV->isSettable()) {
      cs.getTypeChecker().diagnose(equalLoc, diag::assignment_lhs_not_settable)
        .highlight(dest->getSourceRange());
    }
    destTy = destLV->getObjectType();
  } else if (auto typeVar = dyn_cast<TypeVariableType>(destTy.getPointer())) {
    // The destination is a type variable. This type variable must be an
    // lvalue type, which we enforce via a subtyping relationship with
    // [byref(implicit, settable)] T, where T is a fresh type variable that
    // will be the object type of this particular expression type.
    auto objectTv = cs.createTypeVariable(dest);
    auto refTv = LValueType::get(objectTv,
                                 LValueType::Qual::Implicit,
                                 cs.getASTContext());
    cs.addConstraint(ConstraintKind::Subtype, typeVar, refTv);
    destTy = objectTv;
  } else {
    if (!destTy->is<ErrorType>())
      cs.getTypeChecker().diagnose(equalLoc, diag::assignment_lhs_not_lvalue)
        .highlight(dest->getSourceRange());

    return Type();
  }
  
  return destTy;
}

std::pair<Expr *, Expr *> TypeChecker::typeCheckAssignment(Expr *dest,
                                                           SourceLoc equalLoc,
                                                           Expr *src) {
  // FIXME: Old type checker.
  if (!getLangOpts().UseConstraintSolver) {
    if (typeCheckAssignmentOld(dest, equalLoc, src))
      return { nullptr, nullptr };

    return { dest, src };
  }

  // First, pre-check the destination and source, validating any types that
  // occur in the expression and folding sequence expressions.

  // Pre-check the destination.
  dest = dest->walk(PreCheckExpression(*this));
  if (!dest)
    return std::make_pair(nullptr, nullptr);

  // Pre-check the source.
  src = src->walk(PreCheckExpression(*this));
  if (!src)
    return { nullptr, nullptr };

  llvm::raw_ostream &log = llvm::errs();

  // Construct a constraint system from the destination and source.
  ConstraintSystem cs(*this);
  if (cs.generateConstraints(dest) || cs.generateConstraints(src))
    return { nullptr, nullptr };

  // Compute the type to which the source must be converted to allow assignment
  // to the destination.
  auto destTy = computeAssignDestType(cs, dest, equalLoc);
  if (!destTy)
    return { nullptr, nullptr };

  // The source must be convertible to the destination.
  auto assignLocator = cs.getConstraintLocator(src,
                                               ConstraintLocator::AssignSource);
  cs.addConstraint(ConstraintKind::Conversion, src->getType(), destTy,
                   assignLocator);

  if (getLangOpts().DebugConstraintSolver) {
    log << "---Initial constraints for the given assignment---\n";
    log << "Destination expression:\n";
    dest->print(log);
    log << "\n";
    log << "Source expression:\n";
    src->print(log);
    log << "\n";
    cs.dump();
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> solutions;
  if (cs.solve(solutions)) {
    // FIXME: Dumping constraints by default due to crummy diagnostics.
    if (getLangOpts().DebugConstraintSolver || true) {
      log << "---Solved constraints---\n";
      cs.dump();

      if (!solutions.empty()) {
        unsigned idx = 0;
        for (auto &solution : solutions) {
          log << "---Solution #" << ++idx << "---\n";
          solution.dump(&Context.SourceMgr);
        }
      }

      if (solutions.size() == 0)
        log << "No solution found.\n";
      else if (solutions.size() == 1)
        log << "Unique solution found.\n";
      else {
        log << "Found " << solutions.size() << " potential solutions.\n";
      }
    }

    // FIXME: Crappy diagnostic.
    diagnose(equalLoc, diag::constraint_assign_type_check_fail)
      .highlight(dest->getSourceRange())
      .highlight(src->getSourceRange());

    return { nullptr, nullptr };
  }

  auto &solution = solutions[0];
  if (getLangOpts().DebugConstraintSolver) {
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr);
  }

  // Apply the solution to the destination.
  dest = cs.applySolution(solution, dest);
  if (!dest) {
    // Failure already diagnosed, above, as part of applying the solution.
    return { nullptr, nullptr };
  }

  // Apply the solution to the source.
  src = cs.applySolution(solution, src);
  if (!src) {
    // Failure already diagnosed, above, as part of applying the solution.
    return { nullptr, nullptr };
  }

  // Convert the source to the simplified destination type.
  src = solution.coerceToType(src, solution.simplifyType(*this, destTy),
                              assignLocator);
  if (!src) {
    // Failure already diagnosed, above, as part of applying the solution.
    return { nullptr, nullptr };
  }

  if (getLangOpts().DebugConstraintSolver) {
    log << "---Type-checked expressions---\n";
    log << "Destination expression:\n";
    dest->print(log);
    log << "\n";
    log << "Source expression:\n";
    src->print(log);
    log << "\n";
  }

  return { dest, src };
}

bool TypeChecker::typeCheckCondition(Expr *&expr) {
  // FIXME: Old type checker.
  if (!getLangOpts().UseConstraintSolver) {
    return typeCheckConditionOld(expr);
  }

  // First, pre-check the expression, validating any types that occur in the
  // expression and folding sequence expressions.
  expr = expr->walk(PreCheckExpression(*this));
  if (!expr)
    return true;

  llvm::raw_ostream &log = llvm::errs();

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this);
  if (cs.generateConstraints(expr))
    return true;

  // The result must be a LogicValue.
  auto logicValueProto = getProtocol(KnownProtocolKind::LogicValue);
  if (!logicValueProto) {
    diagnose(expr->getLoc(), diag::condition_missing_proto);
    return true;
  }

  cs.addConstraint(ConstraintKind::Conversion, expr->getType(),
                   logicValueProto->getDeclaredType(),
                   cs.getConstraintLocator(expr, { }));

  if (getLangOpts().DebugConstraintSolver) {
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump();
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Dumping constraints by default due to crummy diagnostics.
    if (getLangOpts().DebugConstraintSolver || true) {
      log << "---Solved constraints---\n";
      cs.dump();

      if (!viable.empty()) {
        unsigned idx = 0;
        for (auto &solution : viable) {
          log << "---Solution #" << ++idx << "---\n";
          solution.dump(&Context.SourceMgr);
        }
      }

      if (viable.size() == 0)
        log << "No solution found.\n";
      else if (viable.size() == 1)
        log << "Unique solution found.\n";
      else {
        log << "Found " << viable.size() << " potential solutions.\n";
      }
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr);
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
    log << "---Type-checked expression---\n";
    result->dump();
  }

  expr = result;
  return false;
}

bool TypeChecker::typeCheckArrayBound(Expr *&expr, bool constantRequired) {
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

  // FIXME: Old type checker.
  if (!getLangOpts().UseConstraintSolver)
    return typeCheckArrayBoundOld(expr);
  
  // First, pre-check the expression, validating any types that occur in the
  // expression and folding sequence expressions.
  expr = expr->walk(PreCheckExpression(*this));
  if (!expr)
    return true;

  llvm::raw_ostream &log = llvm::errs();

  // Construct a constraint system from this expression.
  ConstraintSystem cs(*this);
  if (cs.generateConstraints(expr))
    return true;

  // The result must be an ArrayBound.
  auto arrayBoundProto = getProtocol(KnownProtocolKind::ArrayBound);
  if (!arrayBoundProto) {
    diagnose(expr->getLoc(), diag::array_bound_missing_proto);
    return true;
  }

  cs.addConstraint(ConstraintKind::Conversion, expr->getType(),
                   arrayBoundProto->getDeclaredType(),
                   cs.getConstraintLocator(expr, { }));

  if (getLangOpts().DebugConstraintSolver) {
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    cs.dump();
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (cs.solve(viable)) {
    // Try to provide a decent diagnostic.
    if (cs.diagnose()) {
      return true;
    }

    // FIXME: Dumping constraints by default due to crummy diagnostics.
    if (getLangOpts().DebugConstraintSolver || true) {
      log << "---Solved constraints---\n";
      cs.dump();

      if (!viable.empty()) {
        unsigned idx = 0;
        for (auto &solution : viable) {
          log << "---Solution #" << ++idx << "---\n";
          solution.dump(&Context.SourceMgr);
        }
      }

      if (viable.size() == 0)
        log << "No solution found.\n";
      else if (viable.size() == 1)
        log << "Unique solution found.\n";
      else {
        log << "Found " << viable.size() << " potential solutions.\n";
      }
    }

    // FIXME: Crappy diagnostic.
    diagnose(expr->getLoc(), diag::constraint_type_check_fail)
      .highlight(expr->getSourceRange());

    return true;
  }

  auto &solution = viable[0];
  if (getLangOpts().DebugConstraintSolver) {
    log << "---Solution---\n";
    solution.dump(&Context.SourceMgr);
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
    log << "---Type-checked expression---\n";
    result->dump();
  }
  
  expr = result;
  return false;
}

bool TypeChecker::isSubtypeOf(Type type1, Type type2, bool &isTrivial) {
  if (!getLangOpts().UseConstraintSolver) {
    return isSubtypeOfOld(type1, type2, isTrivial);
  }
  
  ConstraintSystem cs(*this);
  return cs.isSubtypeOf(type1, type2, isTrivial);
}

Expr *TypeChecker::coerceToRValue(Expr *expr) {
  if (!getLangOpts().UseConstraintSolver)
    return convertToRValueOld(expr);

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
  if (!getLangOpts().UseConstraintSolver) {
    return convertToMaterializableOld(expr);
  }

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

//===--------------------------------------------------------------------===//
// Debugging
//===--------------------------------------------------------------------===//
#pragma mark Debugging

void Solution::dump(llvm::SourceMgr *sm) const {
  llvm::raw_ostream &out = llvm::errs();

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
      ovl.first->dump(sm);
    out << " with ";

    auto choice = ovl.second.first;
    switch (choice.getKind()) {
    case OverloadChoiceKind::Decl:
      if (choice.getBaseType())
        out << choice.getBaseType()->getString() << ".";
        
      out << choice.getDecl()->getName().str() << ": "
        << ovl.second.second->getString() << "\n";
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
  llvm::raw_ostream &out = llvm::errs();

  if (!ResolvedOverloads.empty()) {
    out << "Resolved overloads:\n";

    // Otherwise, report the resolved overloads.
    assert(!ResolvedOverloads.empty());
    for (auto ovl : ResolvedOverloads) {
      auto &choice = ovl.first->getChoices()[ovl.second.first];
      out << "  selected overload set #" << ovl.first->getID()
          << " choice #" << ovl.second.first << " for ";
      switch (choice.getKind()) {
      case OverloadChoiceKind::Decl:
        if (choice.getBaseType())
          out << choice.getBaseType()->getString() << ".";
        out << choice.getDecl()->getName().str() << ": "
          << ovl.first->getBoundType()->getString() << " == "
          << ovl.second.second->getString() << "\n";
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

  out << "Type Variables:\n";
  for (auto tv : TypeVariables) {
    out.indent(2);
    tv->getImpl().print(out);
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

  if (!UnresolvedOverloadSets.empty()) {
    out << "\nUnresolved overload sets:\n";
    for (auto overload : UnresolvedOverloadSets) {
      out.indent(2) << "set #" << overload->getID() << " binds "
        << overload->getBoundType()->getString() << ":\n";
      for (auto choice : overload->getChoices()) {
        out.indent(4);
        switch (choice.getKind()) {
        case OverloadChoiceKind::Decl:
          if (choice.getBaseType())
            out << choice.getBaseType()->getString() << ".";
          out << choice.getDecl()->getName().str() << ": ";
          out << choice.getDecl()->getTypeOfReference()->getString() << '\n';
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
    }
  }

  if (failedConstraint) {
    out << "\nFailed constraint:\n";
    out.indent(2);
    failedConstraint->print(out, &getTypeChecker().Context.SourceMgr);
    out << "\n";
  }

  if (isSolved()) {
    out << "SOLVED (completely)\n";
  } else {
    out << "UNSOLVED\n";
  }
  
}
