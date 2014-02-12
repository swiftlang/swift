//===--- Type.cpp - Swift Language Type ASTs ------------------------------===//
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
//  This file implements the Type class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/AST.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <functional>
#include <iterator>
using namespace swift;

bool TypeLoc::isError() const {
  assert(wasValidated() && "Type not yet validated");
  return getType()->is<ErrorType>();
}

SourceRange TypeLoc::getSourceRange() const {
  if (TyR)
    return TyR->getSourceRange();
  return SourceRange();
}

// Only allow allocation of Types using the allocator in ASTContext.
void *TypeBase::operator new(size_t bytes, const ASTContext &ctx,
                             AllocationArena arena, unsigned alignment) {
  return ctx.Allocate(bytes, alignment, arena);
}

bool CanType::isActuallyCanonicalOrNull() const {
  return getPointer() == 0 || 
         getPointer() == llvm::DenseMapInfo<TypeBase*>::getTombstoneKey() ||
         getPointer()->isCanonical();
}

//===----------------------------------------------------------------------===//
// Various Type Methods.
//===----------------------------------------------------------------------===//

/// isEqual - Return true if these two types are equal, ignoring sugar.
bool TypeBase::isEqual(Type Other) {
  return getCanonicalType() == Other.getPointer()->getCanonicalType();
}

/// hasReferenceSemantics - Does this type have reference semantics?
bool TypeBase::hasReferenceSemantics() {
  return getCanonicalType().hasReferenceSemantics();
}

bool CanType::hasReferenceSemanticsImpl(CanType type) {
  // At the moment, Builtin.ObjectPointer, class types, and function types.
  switch (type->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    return false;

  case TypeKind::Error:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinVector:
  case TypeKind::Tuple:
  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Metatype:
  case TypeKind::Module:
  case TypeKind::Array:
  case TypeKind::LValue:
  case TypeKind::InOut:
  case TypeKind::TypeVariable:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
    return false;

  case TypeKind::UnownedStorage:
  case TypeKind::WeakStorage:
    return false; // This might seem non-obvious.

  case TypeKind::Archetype:
    return cast<SubstitutableType>(type)->requiresClass();
  case TypeKind::Protocol:
    return cast<ProtocolType>(type)->requiresClass();
  case TypeKind::ProtocolComposition:
    return cast<ProtocolCompositionType>(type)->requiresClass();

  case TypeKind::DynamicSelf:
    return cast<DynamicSelfType>(type)->getSelfType()->hasReferenceSemantics();
      
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::Class:
  case TypeKind::BoundGenericClass:
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
  case TypeKind::GenericFunction:
  case TypeKind::SILFunction:
    return true;

  case TypeKind::UnboundGeneric:
    return isa<ClassDecl>(cast<UnboundGenericType>(type)->getDecl());

  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    llvm_unreachable("Dependent types can't answer reference-semantics query");
  }

  llvm_unreachable("Unhandled type kind!");
}

/// hasOwnership - Are variables of this type permitted to have
/// ownership attributes?
///
/// This includes:
///   - class types, generic or not
///   - archetypes with class or class protocol bounds
///   - existentials with class or class protocol bounds
/// But not:
///   - function types
bool TypeBase::allowsOwnership() {
  CanType canonical = getCanonicalType();
  return (!isa<AnyFunctionType>(canonical)
          && canonical.hasReferenceSemantics());
}

bool CanType::isExistentialTypeImpl(CanType type) {
  return isa<ProtocolType>(type) || isa<ProtocolCompositionType>(type);
}

bool TypeBase::isExistentialType(SmallVectorImpl<ProtocolDecl *> &Protocols) {
  CanType T = getCanonicalType();
  if (auto Proto = dyn_cast<ProtocolType>(T)) {
    Protocols.push_back(Proto->getDecl());
    return true;
  }
  
  if (auto PC = dyn_cast<ProtocolCompositionType>(T)) {
    std::transform(PC->getProtocols().begin(), PC->getProtocols().end(),
                   std::back_inserter(Protocols),
                   [](Type T) { return T->castTo<ProtocolType>()->getDecl(); });
    return true;
  }

  assert(!T.isExistentialType());
  return false;
}

bool TypeBase::isSpecialized() {
  CanType CT = getCanonicalType();
  if (CT.getPointer() != this)
    return CT->isSpecialized();

  return CT.findIf([](Type type) -> bool {
    return isa<BoundGenericType>(type.getPointer());
  });
}

ArrayRef<Substitution> 
TypeBase::gatherAllSubstitutions(Module *module,
                                 SmallVectorImpl<Substitution> &scratchSpace,
                                 LazyResolver *resolver) {
  Type type(this);
  SmallVector<ArrayRef<Substitution>, 2> allSubstitutions;
  scratchSpace.clear();

  while (type) {
    // Record the substitutions in a bound generic type.
    if (auto boundGeneric = type->getAs<BoundGenericType>()) {
      allSubstitutions.push_back(boundGeneric->getSubstitutions(module,
                                                                resolver));
      type = boundGeneric->getParent();
      continue;
    }

    // Skip to the parent of a nominal type.
    if (auto nominal = type->getAs<NominalType>()) {
      type = nominal->getParent();
      continue;
    }

    llvm_unreachable("Not a nominal or bound generic type");
  }

  // If there are no substitutions, return an empty array.
  if (allSubstitutions.empty())
    return { };

  // If there is only one list of substitutions, return it. There's no
  // need to copy it.
  if (allSubstitutions.size() == 1)
    return allSubstitutions.front();

  for (auto substitutions : allSubstitutions)
    scratchSpace.append(substitutions.begin(), substitutions.end());
  return scratchSpace;
}

ArrayRef<Substitution> TypeBase::gatherAllSubstitutions(Module *module,
                                                        LazyResolver *resolver) {
  SmallVector<Substitution, 4> scratchSpace;
  auto subs = gatherAllSubstitutions(module, scratchSpace, resolver);
  if (scratchSpace.empty())
    return subs;

  return getASTContext().AllocateCopy(subs);
}


bool TypeBase::isUnspecializedGeneric() {
  CanType CT = getCanonicalType();
  if (CT.getPointer() != this)
    return CT->isUnspecializedGeneric();

  switch (getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    return false;

  case TypeKind::Error:
  case TypeKind::TypeVariable:
    llvm_unreachable("querying invalid type");

  case TypeKind::UnboundGeneric:
    return true;

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
    return true;

  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    auto funcTy = cast<AnyFunctionType>(this);
    return funcTy->getInput()->isUnspecializedGeneric() ||
           funcTy->getResult()->isUnspecializedGeneric();
  }

  case TypeKind::GenericFunction:
    return true;

  case TypeKind::Class:
  case TypeKind::Struct:
  case TypeKind::Enum:
    if (auto parentTy = cast<NominalType>(this)->getParent())
      return parentTy->isUnspecializedGeneric();
    return false;

  case TypeKind::Metatype:
    return cast<MetatypeType>(this)->getInstanceType()
             ->isUnspecializedGeneric();

  case TypeKind::UnownedStorage:
  case TypeKind::WeakStorage:
    return cast<ReferenceStorageType>(this)->getReferentType()
             ->isUnspecializedGeneric();

  case TypeKind::LValue:
    return cast<LValueType>(this)->getObjectType()->isUnspecializedGeneric();
  case TypeKind::InOut:
    return cast<InOutType>(this)->getObjectType()->isUnspecializedGeneric();

  case TypeKind::Tuple: {
    auto tupleTy = cast<TupleType>(this);
    for (auto &Elt : tupleTy->getFields())
      if (Elt.getType()->isUnspecializedGeneric())
        return true;

    return false;
  }

  case TypeKind::Archetype:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinVector:
  case TypeKind::Module:
  case TypeKind::DynamicSelf:
  case TypeKind::Protocol:
  case TypeKind::ProtocolComposition:
  case TypeKind::SILFunction:
    return false;
    
  case TypeKind::Array:
    return cast<ArrayType>(this)->getBaseType()->isUnspecializedGeneric();

  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    return false;
  }
}

void
TypeBase::getTypeVariables(SmallVectorImpl<TypeVariableType *> &typeVariables) {
  // If we know we don't have any type variables, we're done.
  if (hasTypeVariable()) {
    // Use Type::findIf() to walk the types, finding type variables along the
    // way.
    getCanonicalType().findIf([&](Type type) -> bool {
      if (auto tv = dyn_cast<TypeVariableType>(type.getPointer())) {
        typeVariables.push_back(tv);
      }

      return false;
    });
    assert(!typeVariables.empty() && "Did not find type variables!");
  }
}

static bool isLegalSILType(CanType type) {
  if (isa<LValueType>(type) || isa<InOutType>(type)) return false;
  if (isa<AnyFunctionType>(type)) return false;
  if (auto meta = dyn_cast<MetatypeType>(type))
    return meta->hasThin();
  if (auto tupleType = dyn_cast<TupleType>(type)) {
    for (auto eltType : tupleType.getElementTypes()) {
      if (!isLegalSILType(eltType)) return false;
    }
    return true;
  }
  return true;
}

bool TypeBase::isLegalSILType() {
  return ::isLegalSILType(getCanonicalType());
}

bool TypeBase::isVoid() {
  return isEqual(getASTContext().TheEmptyTupleType);
}

ClassDecl *TypeBase::getClassOrBoundGenericClass() {
  if (auto classTy = getAs<ClassType>())
    return classTy->getDecl();

  if (auto boundTy = getAs<BoundGenericType>())
    return dyn_cast<ClassDecl>(boundTy->getDecl());

  return nullptr;
}

StructDecl *TypeBase::getStructOrBoundGenericStruct() {
  if (auto structTy = getAs<StructType>())
    return structTy->getDecl();
  
  if (auto boundTy = getAs<BoundGenericType>())
    return dyn_cast<StructDecl>(boundTy->getDecl());
  
  return nullptr;
}

EnumDecl *TypeBase::getEnumOrBoundGenericEnum() {
  if (auto oofTy = getAs<EnumType>())
    return oofTy->getDecl();
  
  if (auto boundTy = getAs<BoundGenericType>())
    return dyn_cast<EnumDecl>(boundTy->getDecl());
  
  return nullptr;
}

NominalTypeDecl *TypeBase::getNominalOrBoundGenericNominal() {
  if (auto nominalTy = getAs<NominalType>())
    return nominalTy->getDecl();

  if (auto boundTy = getAs<BoundGenericType>())
    return boundTy->getDecl();

  return nullptr;
}

NominalTypeDecl *TypeBase::getAnyNominal() {
  if (auto nominalTy = getAs<NominalType>())
    return nominalTy->getDecl();

  if (auto boundTy = getAs<BoundGenericType>())
    return boundTy->getDecl();

  if (auto unboundTy = getAs<UnboundGenericType>())
    return unboundTy->getDecl();

  return nullptr;
}

Type TypeBase::getOptionalObjectType() {
  if (auto boundTy = getAs<BoundGenericType>())
    if (boundTy->getDecl()->classifyAsOptionalType() == OTK_Optional)
      return boundTy->getGenericArgs()[0];
  return Type();
}

Type TypeBase::getUncheckedOptionalObjectType() {
  if (auto boundTy = getAs<BoundGenericType>())
    if (boundTy->getDecl()->classifyAsOptionalType() == OTK_UncheckedOptional)
      return boundTy->getGenericArgs()[0];
  return Type();
}

Type TypeBase::getAnyOptionalObjectType() {
  if (auto boundTy = getAs<BoundGenericType>())
    if (boundTy->getDecl()->classifyAsOptionalType())
      return boundTy->getGenericArgs()[0];
  return Type();
}


static Type getStrippedType(const ASTContext &context, Type type,
                            bool stripLabels, bool stripDefaultArgs) {
  return type.transform([&](Type type) -> Type {
    auto *tuple = dyn_cast<TupleType>(type.getPointer());
    if (!tuple)
      return type;

    SmallVector<TupleTypeElt, 4> elements;
    bool anyChanged = false;
    unsigned idx = 0;
    for (const auto &elt : tuple->getFields()) {
      Type eltTy = getStrippedType(context, elt.getType(),
                                   stripLabels, stripDefaultArgs);
      if (anyChanged || eltTy.getPointer() != elt.getType().getPointer() ||
          (elt.hasInit() && stripDefaultArgs) ||
          (!elt.getName().empty() && stripLabels)) {
        if (!anyChanged) {
          elements.reserve(tuple->getFields().size());
          for (unsigned i = 0; i != idx; ++i) {
            const TupleTypeElt &elt = tuple->getFields()[i];
            Identifier newName = stripLabels? Identifier() : elt.getName();
            DefaultArgumentKind newDefArg
              = stripDefaultArgs? DefaultArgumentKind::None
                                : elt.getDefaultArgKind();
            elements.push_back(TupleTypeElt(elt.getType(), newName, newDefArg,
                                            elt.isVararg()));
          }
          anyChanged = true;
        }

        Identifier newName = stripLabels? Identifier() : elt.getName();
        DefaultArgumentKind newDefArg
          = stripDefaultArgs? DefaultArgumentKind::None
                            : elt.getDefaultArgKind();
        elements.push_back(TupleTypeElt(eltTy, newName, newDefArg,
                                        elt.isVararg()));
      }
      ++idx;
    }
    
    if (!anyChanged)
      return type;
    
    // An unlabeled 1-element tuple type is represented as a parenthesized
    // type.
    if (elements.size() == 1 && !elements[0].isVararg() && 
        elements[0].getName().empty())
      return ParenType::get(context, elements[0].getType());
    
    return TupleType::get(elements, context);
  });
}

Type TypeBase::getUnlabeledType(ASTContext &Context) {
  return getStrippedType(Context, Type(this), /*labels=*/true,
                         /*defaultArgs=*/true);
}

Type TypeBase::getWithoutDefaultArgs(const ASTContext &Context) {
  return getStrippedType(Context, Type(this), /*labels=*/false,
                         /*defaultArgs=*/true);
}

/// Retrieve the object type for a 'self' parameter, digging into one-element
/// tuples, lvalue types, and metatypes.
Type TypeBase::getRValueInstanceType() {
  Type type = this;
  
  // Look through argument list tuples.
  if (auto tupleTy = type->getAs<TupleType>()) {
    if (tupleTy->getNumElements() == 1 && !tupleTy->getFields()[0].isVararg())
      type = tupleTy->getElementType(0);
  }
  
  if (auto metaTy = type->getAs<MetatypeType>())
    return metaTy->getInstanceType();

  // For @mutable value type methods, we need to dig through inout types.
  return type->getInOutObjectType();
}

/// \brief Collect the protocols in the existential type T into the given
/// vector.
static void addProtocols(Type T, SmallVectorImpl<ProtocolDecl *> &Protocols) {
  if (auto Proto = T->getAs<ProtocolType>()) {
    Protocols.push_back(Proto->getDecl());
  } else if (auto PC = T->getAs<ProtocolCompositionType>()) {
    for (auto P : PC->getProtocols())
      addProtocols(P, Protocols);
  }
}

/// \brief Add the protocol (or protocols) in the type T to the stack of
/// protocols, checking whether any of the protocols had already been seen and
/// zapping those in the original list that we find again.
static void addMinimumProtocols(Type T,
                                SmallVectorImpl<ProtocolDecl *> &Protocols,
                           llvm::SmallDenseMap<ProtocolDecl *, unsigned> &Known,
                                llvm::SmallPtrSet<ProtocolDecl *, 16> &Visited,
                                SmallVector<ProtocolDecl *, 16> &Stack,
                                bool &ZappedAny) {
  if (auto Proto = T->getAs<ProtocolType>()) {
    auto KnownPos = Known.find(Proto->getDecl());
    if (KnownPos != Known.end()) {
      // We've come across a protocol that is in our original list. Zap it.
      Protocols[KnownPos->second] = nullptr;
      ZappedAny = true;
    }

    if (Visited.insert(Proto->getDecl())) {
      for (auto Inherited : Proto->getDecl()->getProtocols())
        addProtocols(Inherited->getDeclaredType(), Stack);
    }
    return;
  }
  
  if (auto PC = T->getAs<ProtocolCompositionType>()) {
    for (auto C : PC->getProtocols()) {
      addMinimumProtocols(C, Protocols, Known, Visited, Stack, ZappedAny);
    }
  }
}

/// \brief Compare two protocols to establish an ordering between them.
static int compareProtocols(ProtocolDecl * const* PP1,
                            ProtocolDecl * const* PP2) {
  auto *P1 = *PP1;
  auto *P2 = *PP2;
  Module *M1 = P1->getParentModule();
  Module *M2 = P2->getParentModule();

  // Try ordering based on module name, first.
  if (int result = M1->Name.str().compare(M2->Name.str()))
    return result;

  // Order based on protocol name.
  return P1->getName().str().compare(P2->getName().str());
}

void ProtocolType::canonicalizeProtocols(
       SmallVectorImpl<ProtocolDecl *> &protocols) {
  llvm::SmallDenseMap<ProtocolDecl *, unsigned> known;
  llvm::SmallPtrSet<ProtocolDecl *, 16> visited;
  SmallVector<ProtocolDecl *, 16> stack;
  bool zappedAny = false;

  // Seed the stack with the protocol declarations in the original list.
  // Zap any obvious duplicates along the way.
  for (unsigned I = 0, N = protocols.size(); I != N; ++I) {
    // Check whether we've seen this protocol before.
    auto knownPos = known.find(protocols[I]);
    
    // If we have not seen this protocol before, record it's index.
    if (knownPos == known.end()) {
      known[protocols[I]] = I;
      stack.push_back(protocols[I]);
      continue;
    }
    
    // We have seen this protocol before; zap this occurrence.
    protocols[I] = 0;
    zappedAny = true;
  }
  
  // Walk the inheritance hierarchies of all of the protocols. If we run into
  // one of the known protocols, zap it from the original list.
  while (!stack.empty()) {
    ProtocolDecl *Current = stack.back();
    stack.pop_back();
    
    // Add the protocols we inherited.
    for (auto Inherited : Current->getProtocols()) {
      addMinimumProtocols(Inherited->getDeclaredType(), protocols, known,
                          visited, stack, zappedAny);
    }
  }
  
  if (zappedAny)
    protocols.erase(std::remove(protocols.begin(), protocols.end(), nullptr),
                    protocols.end());

  // Sort the set of protocols by module + name, to give a stable
  // ordering.
  llvm::array_pod_sort(protocols.begin(), protocols.end(), compareProtocols);
}

CanGenericSignature GenericSignature::getCanonicalSignature() {
  if (CanonicalSignatureOrASTContext.is<ASTContext*>())
    return CanGenericSignature(this);
  
  if (auto p = CanonicalSignatureOrASTContext.dyn_cast<GenericSignature*>())
    return CanGenericSignature(p);
  
  ASTContext *C;
  if (!getGenericParams().empty())
    C = &getGenericParams().front()->getASTContext();
  else
    C = &getRequirements().front().getFirstType()->getASTContext();
  
  CanGenericSignature canSig = getCanonical(getGenericParams(),
                                            getRequirements(), *C);
  
  CanonicalSignatureOrASTContext = canSig;
  return canSig;
}

/// getCanonicalType - Return the canonical version of this type, which has
/// sugar from all levels stripped off.
CanType TypeBase::getCanonicalType() {
  assert(this != 0 &&
         "Cannot call getCanonicalType before name binding is complete "
         "or on a null pointer");

  // If the type is itself canonical, return it.
  if (isCanonical())
    return CanType(this);
  // If the canonical type was already computed, just return what we have.
  if (TypeBase *CT = CanonicalType.get<TypeBase*>())
    return CanType(CT);

  // Otherwise, compute and cache it.
  TypeBase *Result = 0;
  switch (getKind()) {
#define ALWAYS_CANONICAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Error:
  case TypeKind::TypeVariable:
    llvm_unreachable("these types are always canonical");

#define SUGARED_TYPE(id, parent) \
  case TypeKind::id: \
    Result = cast<id##Type>(this)-> \
             getSinglyDesugaredType()->getCanonicalType().getPointer(); \
    break;
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"

  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Class: {
    auto nominalTy = cast<NominalType>(this);
    auto parentTy = nominalTy->getParent()->getCanonicalType();
    Result = NominalType::get(nominalTy->getDecl(), parentTy,
                              parentTy->getASTContext());
    break;
  }

  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(this);
    assert(!TT->getFields().empty() && "Empty tuples are always canonical");

    SmallVector<TupleTypeElt, 8> CanElts;
    CanElts.reserve(TT->getFields().size());
    for (const TupleTypeElt &field : TT->getFields()) {
      assert(!field.getType().isNull() &&
             "Cannot get canonical type of un-typechecked TupleType!");
      CanElts.push_back(TupleTypeElt(field.getType()->getCanonicalType(),
                                     field.getName(),
                                     field.getDefaultArgKind(),
                                     field.isVararg()));
    }

    const ASTContext &C = CanElts[0].getType()->getASTContext();
    Result = TupleType::get(CanElts, C)->castTo<TupleType>();
    break;
  }

  case TypeKind::GenericTypeParam: {
    GenericTypeParamType *gp = cast<GenericTypeParamType>(this);
    auto gpDecl = gp->getDecl();

    Result = GenericTypeParamType::get(gpDecl->getDepth(), gpDecl->getIndex(),
                                       gpDecl->getASTContext());
    break;
  }

  case TypeKind::DependentMember: {
    auto dependent = cast<DependentMemberType>(this);
    auto base = dependent->getBase()->getCanonicalType();
    const ASTContext &ctx = base->getASTContext();
    if (auto assocType = dependent->getAssocType())
      Result = DependentMemberType::get(base, assocType, ctx);
    else
      Result = DependentMemberType::get(base, dependent->getName(), ctx);
    break;
  }

  case TypeKind::UnownedStorage: {
    auto ref = cast<UnownedStorageType>(this);
    Type referentType = ref->getReferentType()->getCanonicalType();
    Result = UnownedStorageType::get(referentType,
                                     referentType->getASTContext());
    break;
  }
  case TypeKind::WeakStorage: {
    auto ref = cast<WeakStorageType>(this);
    Type referentType = ref->getReferentType()->getCanonicalType();
    Result = WeakStorageType::get(referentType, referentType->getASTContext());
    break;
  }
  case TypeKind::LValue:
    Result = LValueType::get(getRValueType()->getCanonicalType());
    break;
  case TypeKind::InOut:
    Result = InOutType::get(getInOutObjectType()->getCanonicalType());
    break;
  case TypeKind::PolymorphicFunction: {
    PolymorphicFunctionType *FT = cast<PolymorphicFunctionType>(this);
    Type In = FT->getInput()->getCanonicalType();
    Type Out = FT->getResult()->getCanonicalType();
    Result = PolymorphicFunctionType::get(In, Out, &FT->getGenericParams(),
                                          FT->getExtInfo());
    break;
  }
  case TypeKind::GenericFunction: {
    GenericFunctionType *function = cast<GenericFunctionType>(this);

    // Canonicalize the signature.
    GenericSignature *sig = function->getGenericSignature()
      ->getCanonicalSignature();
    
    // Canonicalize generic parameters.
    SmallVector<GenericTypeParamType *, 4> genericParams;
    for (auto param : function->getGenericParams()) {
      auto newParam = param->getCanonicalType()->castTo<GenericTypeParamType>();
      genericParams.push_back(newParam);
    }

    // Transform requirements.
    SmallVector<Requirement, 4> requirements;
    for (const auto &req : function->getRequirements()) {
      auto firstType = req.getFirstType()->getCanonicalType();
      auto secondType = req.getSecondType();
      if (secondType)
        secondType = secondType->getCanonicalType();
      requirements.push_back(Requirement(req.getKind(), firstType, secondType));
    }

    // Transform input type.
    auto inputTy = function->getInput()->getCanonicalType();
    auto resultTy = function->getResult()->getCanonicalType();

    Result = GenericFunctionType::get(sig, inputTy, resultTy,
                                      function->getExtInfo());
    break;
  }

  case TypeKind::SILFunction:
    llvm_unreachable("SILFunctionTypes are always canonical!");

  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(this);
    Type In = FT->getInput()->getCanonicalType();
    Type Out = FT->getResult()->getCanonicalType();
    Result = FunctionType::get(In, Out, FT->getExtInfo());
    break;
  }
  case TypeKind::Array: {
    ArrayType *AT = cast<ArrayType>(this);
    Type EltTy = AT->getBaseType()->getCanonicalType();
    Result = ArrayType::get(EltTy, AT->getSize());
    break;
  }
  case TypeKind::ProtocolComposition: {
    SmallVector<Type, 4> CanProtos;
    for (Type t : cast<ProtocolCompositionType>(this)->getProtocols())
      CanProtos.push_back(t->getCanonicalType());
    assert(!CanProtos.empty() && "Non-canonical empty composition?");
    const ASTContext &C = CanProtos[0]->getASTContext();
    Type Composition = ProtocolCompositionType::get(C, CanProtos);
    Result = Composition.getPointer();
    break;
  }
  case TypeKind::Metatype: {
    MetatypeType *MT = cast<MetatypeType>(this);
    Type InstanceTy = MT->getInstanceType()->getCanonicalType();
    if (MT->hasThin())
      Result = MetatypeType::get(InstanceTy, MT->isThin(),
                                 InstanceTy->getASTContext());
    else
      Result = MetatypeType::get(InstanceTy, InstanceTy->getASTContext());
    break;
  }
  case TypeKind::DynamicSelf: {
    DynamicSelfType *DST = cast<DynamicSelfType>(this);
    Type SelfTy = DST->getSelfType()->getCanonicalType();
    Result = DynamicSelfType::get(SelfTy, SelfTy->getASTContext());
    break;
  }
  case TypeKind::UnboundGeneric: {
    auto unbound = cast<UnboundGenericType>(this);
    Type parentTy = unbound->getParent()->getCanonicalType();
    Result = UnboundGenericType::get(unbound->getDecl(), parentTy,
                                     parentTy->getASTContext());
    break;
  }
  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct: {
    BoundGenericType *BGT = cast<BoundGenericType>(this);
    Type parentTy;
    if (BGT->getParent())
      parentTy = BGT->getParent()->getCanonicalType();
    SmallVector<Type, 4> CanGenericArgs;
    for (Type Arg : BGT->getGenericArgs())
      CanGenericArgs.push_back(Arg->getCanonicalType());
    Result = BoundGenericType::get(BGT->getDecl(), parentTy, CanGenericArgs);
    break;
  }
  }
    
  
  // Cache the canonical type for future queries.
  assert(Result && "Case not implemented!");
  CanonicalType = Result;
  return CanType(Result);
}


TypeBase *TypeBase::getDesugaredType() {
  switch (getKind()) {
#define ALWAYS_CANONICAL_TYPE(id, parent) case TypeKind::id:
#define UNCHECKED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Tuple:
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
  case TypeKind::GenericFunction:
  case TypeKind::SILFunction:
  case TypeKind::Array:
  case TypeKind::LValue:
  case TypeKind::InOut:
  case TypeKind::ProtocolComposition:
  case TypeKind::Metatype:
  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
  case TypeKind::UnownedStorage:
  case TypeKind::WeakStorage:
  case TypeKind::DynamicSelf:
    // None of these types have sugar at the outer level.
    return this;
#define SUGARED_TYPE(ID, PARENT) \
  case TypeKind::ID: \
    return cast<ID##Type>(this)->getSinglyDesugaredType()->getDesugaredType();
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
  }

  llvm_unreachable("Unknown type kind");
}

TypeBase *ParenType::getSinglyDesugaredType() {
  return getUnderlyingType().getPointer();
}

TypeBase *NameAliasType::getSinglyDesugaredType() {
  return getDecl()->getUnderlyingType().getPointer();
}

TypeBase *SyntaxSugarType::getSinglyDesugaredType() {
  return getImplementationType().getPointer();
}

Type SyntaxSugarType::getImplementationType() {
  if (ImplOrContext.is<Type>())
    return ImplOrContext.get<Type>();

  // Find the generic type that implements this syntactic sugar type.
  auto &ctx = *ImplOrContext.get<const ASTContext *>();
  NominalTypeDecl *implDecl;

  if (isa<ArraySliceType>(this)) {
    implDecl = ctx.getSliceDecl();
    assert(implDecl && "Slice type has not been set yet");
  } else if (isa<OptionalType>(this)) {
    implDecl = ctx.getOptionalDecl();
    assert(implDecl && "Optional type has not been set yet");
  } else if (isa<UncheckedOptionalType>(this)) {
    implDecl = ctx.getUncheckedOptionalDecl();
    assert(implDecl && "Optional type has not been set yet");
  } else {
    llvm_unreachable("Unhandled syntax sugar type");
  }

  // Record the implementation type.
  ImplOrContext = BoundGenericType::get(implDecl, Type(), Base);
  return ImplOrContext.get<Type>();
}

TypeBase *SubstitutedType::getSinglyDesugaredType() {
  return getReplacementType().getPointer();
}

unsigned GenericTypeParamType::getDepth() const {
  if (auto param = getDecl()) {
    return param->getDepth();
  }

  auto fixedNum = ParamOrDepthIndex.get<Fixnum<31>>();
  return fixedNum >> 16;
}

unsigned GenericTypeParamType::getIndex() const {
  if (auto param = getDecl()) {
    return param->getIndex();
  }

  auto fixedNum = ParamOrDepthIndex.get<Fixnum<31>>();
  return fixedNum & 0xFFFF;
}

Identifier GenericTypeParamType::getName() const {
  // Use the declaration name if we still have that sugar.
  if (auto decl = getDecl())
    return decl->getName();
  
  // Otherwise, we're canonical. Produce an anonymous '<tau>_n_n' name.
  assert(isCanonical());
  // getASTContext() doesn't actually mutate an already-canonical type.
  auto &C = const_cast<GenericTypeParamType*>(this)->getASTContext();
  auto &names = C.CanonicalGenericTypeParamTypeNames;
  unsigned depthIndex = ParamOrDepthIndex.get<Fixnum<31>>();
  auto cached = names.find(depthIndex);
  if (cached != names.end())
    return cached->second;
  
  llvm::SmallString<10> nameBuf;
  llvm::raw_svector_ostream os(nameBuf);

  static const char *tau = u8"\u03C4_";
  
  os << tau << getDepth() << '_' << getIndex();
  Identifier name = C.getIdentifier(os.str());
  names.insert({depthIndex, name});
  return name;
}

TypeBase *AssociatedTypeType::getSinglyDesugaredType() {
  return getDecl()->getArchetype();
}

const llvm::fltSemantics &BuiltinFloatType::getAPFloatSemantics() const {
  switch (getFPKind()) {
  case BuiltinFloatType::IEEE16:  return APFloat::IEEEhalf;
  case BuiltinFloatType::IEEE32:  return APFloat::IEEEsingle;
  case BuiltinFloatType::IEEE64:  return APFloat::IEEEdouble;
  case BuiltinFloatType::IEEE80:  return APFloat::x87DoubleExtended;
  case BuiltinFloatType::IEEE128: return APFloat::IEEEquad;
  case BuiltinFloatType::PPC128:  return APFloat::PPCDoubleDouble;
  }
  assert(0 && "Unknown FP semantics");
  return APFloat::IEEEhalf;
}

bool TypeBase::isSpelledLike(Type other) {
  TypeBase *me = this;
  TypeBase *them = other.getPointer();
  
  if (me == them)
    return true;
  
  if (me->getKind() != them->getKind())
    return false;

  switch (me->getKind()) {
#define ALWAYS_CANONICAL_TYPE(id, parent) case TypeKind::id:
#define UNCHECKED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::NameAlias:
  case TypeKind::Substituted:
  case TypeKind::AssociatedType:
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
  case TypeKind::DynamicSelf:
    return false;

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct: {
    auto bgMe = cast<BoundGenericType>(me);
    auto bgThem = cast<BoundGenericType>(them);
    if (bgMe->getDecl() != bgThem->getDecl())
      return false;
    if (bgMe->getGenericArgs().size() != bgThem->getGenericArgs().size())
      return false;
    for (size_t i = 0, sz = bgMe->getGenericArgs().size(); i < sz; ++i)
      if (!bgMe->getGenericArgs()[i]->isSpelledLike(bgThem->getGenericArgs()[i]))
        return false;
    return true;
  }

  case TypeKind::Tuple: {
    auto tMe = cast<TupleType>(me);
    auto tThem = cast<TupleType>(them);
    if (tMe->getFields().size() != tThem->getFields().size())
      return false;
    for (size_t i = 0, sz = tMe->getFields().size(); i < sz; ++i) {
      auto &myField = tMe->getFields()[i], &theirField = tThem->getFields()[i];
      if (myField.hasInit() != theirField.hasInit())
        return false;
      
      if (myField.getName() != theirField.getName())
        return false;
      
      if (myField.isVararg() != theirField.isVararg())
        return false;
      if (!myField.getType()->isSpelledLike(theirField.getType()))
        return false;
    }
    return true;
  }

  case TypeKind::SILFunction:
  case TypeKind::PolymorphicFunction:
  case TypeKind::GenericFunction: {
    // Polymorphic function types should never be explicitly spelled.
    return false;
  }

  // TODO: change this to is same ExtInfo.
  case TypeKind::Function: {
    auto fMe = cast<FunctionType>(me);
    auto fThem = cast<FunctionType>(them);
    if (fMe->isAutoClosure() != fThem->isAutoClosure())
      return false;
    if (fMe->isBlock() != fThem->isBlock())
      return false;
    if (fMe->isThin() != fThem->isThin())
      return false;
    if (fMe->isNoReturn() != fThem->isNoReturn())
      return false;
    if (!fMe->getInput()->isSpelledLike(fThem->getInput()))
      return false;
    if (!fMe->getResult()->isSpelledLike(fThem->getResult()))
      return false;
    return true;
  }

  case TypeKind::Array: {
    auto aMe = cast<ArrayType>(me);
    auto aThem = cast<ArrayType>(them);
    if (aMe->getSize() != aThem->getSize())
      return false;
    return aMe->getBaseType()->isSpelledLike(aThem->getBaseType());
  }
      
  case TypeKind::LValue: {
    auto lMe = cast<LValueType>(me);
    auto lThem = cast<LValueType>(them);
    return lMe->getObjectType()->isSpelledLike(lThem->getObjectType());
  }
  case TypeKind::InOut: {
    auto lMe = cast<InOutType>(me);
    auto lThem = cast<InOutType>(them);
    return lMe->getObjectType()->isSpelledLike(lThem->getObjectType());
  }
  case TypeKind::ProtocolComposition: {
    auto pMe = cast<ProtocolCompositionType>(me);
    auto pThem = cast<ProtocolCompositionType>(them);
    if (pMe->getProtocols().size() != pThem->getProtocols().size())
      return false;
    for (size_t i = 0, sz = pMe->getProtocols().size(); i < sz; ++i)
      if (!pMe->getProtocols()[i]->isSpelledLike(pThem->getProtocols()[i]))
        return false;
    return true;
  }
  case TypeKind::Metatype: {
    auto mMe = cast<MetatypeType>(me);
    auto mThem = cast<MetatypeType>(them);
    return mMe->getInstanceType()->isSpelledLike(mThem->getInstanceType());
  }
  case TypeKind::Paren: {
    auto pMe = cast<ParenType>(me);
    auto pThem = cast<ParenType>(them);
    return pMe->getUnderlyingType()->isSpelledLike(pThem->getUnderlyingType());
  }
  case TypeKind::ArraySlice:
  case TypeKind::Optional:
  case TypeKind::UncheckedOptional: {
    auto aMe = cast<SyntaxSugarType>(me);
    auto aThem = cast<SyntaxSugarType>(them);
    return aMe->getBaseType()->isSpelledLike(aThem->getBaseType());
  }
  case TypeKind::UnownedStorage:
  case TypeKind::WeakStorage: {
    auto rMe = cast<ReferenceStorageType>(me);
    auto rThem = cast<ReferenceStorageType>(them);
    return rMe->getReferentType()->isSpelledLike(rThem->getReferentType());
  }
  }

  llvm_unreachable("Unknown type kind");
}

Type TypeBase::getSuperclass(LazyResolver *resolver) {
  Type superclassTy;
  Type specializedTy;
  Module *module = nullptr;
  if (auto classTy = getAs<ClassType>()) {
    superclassTy = classTy->getDecl()->getSuperclass();
    module = classTy->getDecl()->getModuleContext();
    if (auto parentTy = classTy->getParent()) {
      if (parentTy->isSpecialized())
        specializedTy = parentTy;
    }
  } else if (auto boundTy = getAs<BoundGenericType>()) {
    if (auto classDecl = dyn_cast<ClassDecl>(boundTy->getDecl())) {
      superclassTy = classDecl->getSuperclass();
      module = classDecl->getModuleContext();
      specializedTy = this;
    }
  } else if (auto substitutableTy = getAs<SubstitutableType>()) {
    return substitutableTy->getSuperclass();
  } else if (auto dynamicSelfTy = getAs<DynamicSelfType>()) {
    return dynamicSelfTy->getSelfType();
  } else {
    // No other types have superclasses.
    return nullptr;
  }

  if (!specializedTy || !superclassTy)
    return superclassTy;

  // If the type is specialized, we need to gather all of the substitutions.
  // We've already dealt with the top level, but continue gathering
  // specializations from the parent types.
  TypeSubstitutionMap substitutions;
  while (specializedTy) {
    if (auto nominalTy = specializedTy->getAs<NominalType>()) {
      specializedTy = nominalTy->getParent();
      continue;
    }

    // Introduce substitutions for each of the generic parameters/arguments.
    auto boundTy = specializedTy->castTo<BoundGenericType>();
    auto gp = boundTy->getDecl()->getGenericParams()->getParams();
    for (unsigned i = 0, n = boundTy->getGenericArgs().size(); i != n; ++i) {
      auto archetype = gp[i].getAsTypeParam()->getArchetype();
      substitutions[archetype] = boundTy->getGenericArgs()[i];
    }

    specializedTy = boundTy->getParent();
  }

  // Perform substitutions into the base type.
  return superclassTy.subst(module, substitutions, /*ignoreMissing=*/false,
                            resolver);
}

bool TypeBase::isSuperclassOf(Type ty, LazyResolver *resolver) {
  // For there to be a superclass relationship, we must be a superclass, and
  // the potential subtype must be a class or superclass-bounded archetype.
  if (!getClassOrBoundGenericClass() || !ty->mayHaveSuperclass())
    return false;

  do {
    if (ty->isEqual(this))
      return true;
  } while ((ty = ty->getSuperclass(nullptr)));
  return false;
}

/// Is t1 not just a subtype of t2, but one such that its values are
/// trivially convertible to values of the other?
static bool isTrivialSubtypeOf(CanType t1, CanType t2, LazyResolver *resolver) {
  if (t1 == t2) return true;

  // Scalar-to-tuple and tuple-to-tuple.
  if (auto tuple2 = dyn_cast<TupleType>(t2)) {
    // We only ever look into singleton tuples on the RHS if we're
    // certain that the LHS isn't also a singleton tuple.
    auto tuple1 = dyn_cast<TupleType>(t1);
    if (!tuple1 || tuple1->getNumElements() != tuple2->getNumElements()) {
      if (tuple2->getNumElements() == 1)
        return isTrivialSubtypeOf(t1, tuple2.getElementType(0), resolver);
      return false;
    }

    for (auto i : indices(tuple1.getElementTypes())) {
      if (!isTrivialSubtypeOf(tuple1.getElementType(i),
                              tuple2.getElementType(i),
                              resolver))
        return false;
    }
    return true;
  }

  // Function-to-function.  (Polymorphic functions?)
  if (auto fn2 = dyn_cast<FunctionType>(t2)) {
    auto fn1 = dyn_cast<FunctionType>(t1);
    if (!fn1 || fn1->getExtInfo() != fn2->getExtInfo())
      return false;
    // Inputs are contravariant, results are covariant.
    return (isTrivialSubtypeOf(fn2.getInput(), fn1.getInput(), resolver) &&
            isTrivialSubtypeOf(fn1.getResult(), fn2.getResult(), resolver));
  }

  // Class-to-optional and class optional-to-optional.  Note that this
  // is not recursive: T? is not necessarily a trivial subtype of T??,
  // although it is a subtype.
  if (auto bound2 = dyn_cast<BoundGenericType>(t2)) {
    if (auto optKind2 = bound2->getDecl()->classifyAsOptionalType()) {
      auto obj2 = bound2.getGenericArgs()[0];

      // Optional-to-optional.
      if (auto bound1 = dyn_cast<BoundGenericType>(t1)) {
        if (auto optKind1 = bound1->getDecl()->classifyAsOptionalType()) {
          auto obj1 = bound1.getGenericArgs()[0];
          // T? is not a subtype of @unchecked T?, but all other
          // combinations are fine.
          return (optKind1 >= optKind2 && obj2->isSuperclassOf(obj1, resolver));
        } else if (!isa<BoundGenericClassType>(bound1)) {
          return false;
        }
      }

      // Class-to-optional.
      return obj2->isSuperclassOf(t1, resolver);
    } else if (!isa<BoundGenericClassType>(bound2)) {
      return false;
    }
  }

  // Class-to-class.
  return t2->isSuperclassOf(t1, resolver);
}

bool TypeBase::isTrivialSubtypeOf(Type other, LazyResolver *resolver) {
  return ::isTrivialSubtypeOf(getCanonicalType(), other->getCanonicalType(),
                              resolver);
}

/// hasAnyDefaultValues - Return true if any of our elements has a default
/// value.
bool TupleType::hasAnyDefaultValues() const {
  for (const TupleTypeElt &Elt : Fields)
    if (Elt.hasInit())
      return true;
  return false;
}

/// getNamedElementId - If this tuple has a field with the specified name,
/// return the field index, otherwise return -1.
int TupleType::getNamedElementId(Identifier I) const {
  for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
    if (Fields[i].getName() == I)
      return i;
  }

  // Otherwise, name not found.
  return -1;
}

/// getFieldForScalarInit - If a tuple of this type can be initialized with a
/// scalar, return the field number that the scalar is assigned to.  If not,
/// return -1.
int TupleType::getFieldForScalarInit() const {
  if (Fields.empty()) return -1;
  
  int FieldWithoutDefault = -1;
  for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
    // Ignore fields with a default value.
    if (Fields[i].hasInit()) continue;
    
    // If we already saw a non-vararg field missing a default value, then we
    // cannot assign a scalar to this tuple.
    if (FieldWithoutDefault != -1) {
      // Vararg fields are okay; they'll just end up being empty.
      if (Fields[i].isVararg())
        continue;
    
      return -1;
    }
    
    // Otherwise, remember this field number.
    FieldWithoutDefault = i;    
  }
  
  // If all the elements have default values, the scalar initializes the first
  // value in the tuple.
  return FieldWithoutDefault == -1 ? 0 : FieldWithoutDefault;
}


bool SubstitutableType::requiresClass() const {
  if (Superclass)
    return true;
  for (ProtocolDecl *conformed : getConformsTo())
    if (conformed->requiresClass())
      return true;
  return false;
}

ArchetypeType *ArchetypeType::getNew(const ASTContext &Ctx,
                                     ArchetypeType *Parent,
                                     AssocTypeOrProtocolType AssocTypeOrProto,
                                     Identifier Name, ArrayRef<Type> ConformsTo,
                                     Type Superclass,
                                     Optional<unsigned> Index) {
  // Gather the set of protocol declarations to which this archetype conforms.
  SmallVector<ProtocolDecl *, 4> ConformsToProtos;
  for (auto P : ConformsTo) {
    addProtocols(P, ConformsToProtos);
  }
  ProtocolType::canonicalizeProtocols(ConformsToProtos);

  auto arena = AllocationArena::Permanent;
  return new (Ctx, arena) ArchetypeType(Ctx, Parent, AssocTypeOrProto, Name,
                                        Ctx.AllocateCopy(ConformsToProtos),
                                        Superclass, Index);
}

ArchetypeType *
ArchetypeType::getNew(const ASTContext &Ctx, ArchetypeType *Parent,
                      AssocTypeOrProtocolType AssocTypeOrProto,
                      Identifier Name,
                      SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                      Type Superclass, Optional<unsigned> Index) {
  // Gather the set of protocol declarations to which this archetype conforms.
  ProtocolType::canonicalizeProtocols(ConformsTo);

  auto arena = AllocationArena::Permanent;
  return new (Ctx, arena) ArchetypeType(Ctx, Parent, AssocTypeOrProto, Name,
                                        Ctx.AllocateCopy(ConformsTo),
                                        Superclass, Index);
}

ArchetypeType *
ArchetypeType::getNew(Type existential) {
  auto arena = AllocationArena::Permanent;
  llvm::SmallVector<ProtocolDecl *, 4> conformsTo;
  assert(existential->isExistentialType() && "Not an existential type?");
  existential->isExistentialType(conformsTo);
  ProtocolType::canonicalizeProtocols(conformsTo);
  auto &ctx = existential->getASTContext();
  return new (ctx, arena) ArchetypeType(ctx, existential, 
                                        ctx.AllocateCopy(conformsTo),
                                        existential->getSuperclass(nullptr));
}

namespace {
  /// \brief Function object that orders archetypes by name.
  struct OrderArchetypeByName {
    bool operator()(std::pair<Identifier, ArchetypeType *> X,
                    std::pair<Identifier, ArchetypeType *> Y) const {
      return X.first.str() < Y.second->getName().str();
    }

    bool operator()(std::pair<Identifier, ArchetypeType *> X,
                    Identifier Y) const {
      return X.first.str() < Y.str();
    }

    bool operator()(Identifier X,
                    std::pair<Identifier, ArchetypeType *> Y) const {
      return X.str() < Y.first.str();
    }

    bool operator()(Identifier X, Identifier Y) const {
      return X.str() < Y.str();
    }
  };
}

ArchetypeType *ArchetypeType::getNestedType(Identifier Name) const {
  ArrayRef<std::pair<Identifier, ArchetypeType *>>::const_iterator Pos
    = std::lower_bound(NestedTypes.begin(), NestedTypes.end(), Name,
                       OrderArchetypeByName());
  assert(Pos != NestedTypes.end() && Pos->first == Name);
  return Pos->second;
}

void
ArchetypeType::
setNestedTypes(ASTContext &Ctx,
               MutableArrayRef<std::pair<Identifier, ArchetypeType *>> Nested) {
  std::sort(Nested.begin(), Nested.end(), OrderArchetypeByName());
  NestedTypes = Ctx.AllocateCopy(Nested);
}

static void collectFullName(const ArchetypeType *Archetype,
                            SmallVectorImpl<char> &Result) {
  if (auto Parent = Archetype->getParent()) {
    collectFullName(Parent, Result);
    Result.push_back('.');
  }
  Result.append(Archetype->getName().str().begin(),
                Archetype->getName().str().end());
}

std::string ArchetypeType::getFullName() const {
  llvm::SmallString<64> Result;
  collectFullName(this, Result);
  return Result.str().str();
}

void ProtocolCompositionType::Profile(llvm::FoldingSetNodeID &ID,
                                      ArrayRef<Type> Protocols) {
  for (auto P : Protocols)
    ID.AddPointer(P.getPointer());
}

bool ProtocolType::requiresClass() const {
  return getDecl()->requiresClass();
}

bool ProtocolCompositionType::requiresClass() const {
  for (Type t : getProtocols()) {
    SmallVector<ProtocolDecl*, 2> protocols;
    if (t->isExistentialType(protocols))
      for (auto *proto : protocols)
        if (proto->requiresClass())
          return true;
  }
  return false;
}

Type ProtocolCompositionType::get(const ASTContext &C,
                                  ArrayRef<Type> ProtocolTypes) {
  for (Type t : ProtocolTypes) {
    if (!t->isCanonical())
      return build(C, ProtocolTypes);
  }
    
  SmallVector<ProtocolDecl *, 4> Protocols;
  for (Type t : ProtocolTypes)
    addProtocols(t, Protocols);
  
  // Minimize the set of protocols composed together.
  ProtocolType::canonicalizeProtocols(Protocols);

  // If one protocol remains, its nominal type is the canonical type.
  if (Protocols.size() == 1)
    return Protocols.front()->getDeclaredType();

  // Form the set of canonical protocol types from the protocol
  // declarations, and use that to buid the canonical composition type.
  SmallVector<Type, 4> CanProtocolTypes;
  std::transform(Protocols.begin(), Protocols.end(),
                 std::back_inserter(CanProtocolTypes),
                 [](ProtocolDecl *Proto) {
                   return Proto->getDeclaredType();
                 });

  return build(C, CanProtocolTypes);
}

ArrayRef<GenericParam> PolymorphicFunctionType::getGenericParameters() const {
  return Params->getParams();
}

ArrayRef<ArchetypeType *> PolymorphicFunctionType::getAllArchetypes() const {
  return Params->getAllArchetypes();
}

static void getReplacementTypes(const GenericParamList &genericParams,
                                ArrayRef<Substitution> subs,
                                SmallVectorImpl<Type> &replacements) {
  (void) genericParams;
  
#ifndef NDEBUG
  // Ensure the substitution vector matches the generic parameter order.
  auto params = genericParams.getNestedGenericParams();
  auto pi = params.begin();
#endif
  
  for (auto &sub : subs) {
    // FIXME: Only substitute primary archetypes.
    if (!sub.Archetype->isPrimary())
      continue;
    
    assert((pi++)->getAsTypeParam()->getArchetype() == sub.Archetype
           && "substitution doesn't match archetype");
    replacements.push_back(sub.Replacement);
  }
  
  assert(pi == params.end()
         && "did not substitute all archetypes?!");
}

static TypeSubstitutionMap getSubstitutionMapFromReplacementTypes(
                                        const GenericParamList &genericParams,
                                        ArrayRef<Type> args) {
  TypeSubstitutionMap map;
  unsigned i = 0;
  for (auto &param : genericParams.getNestedGenericParams()) {
    map.insert(std::make_pair(param.getAsTypeParam()->getArchetype(),
                              args[i++]));
  }
  assert(i == args.size()
         && "got more arguments than generic parameters");
  
  return map;
}

TypeSubstitutionMap
GenericParamList::getSubstitutionMap(ArrayRef<swift::Substitution> Subs) const {
  SmallVector<Type, 4> replacementTypes;
  getReplacementTypes(*this, Subs, replacementTypes);
  return getSubstitutionMapFromReplacementTypes(*this, replacementTypes);
}

FunctionType *PolymorphicFunctionType::substGenericArgs(Module *module,
                                                        ArrayRef<Type> args) {
  TypeSubstitutionMap subs
    = getSubstitutionMapFromReplacementTypes(getGenericParams(), args);
  
  Type input = getInput().subst(module, subs, true, nullptr);
  Type result = getResult().subst(module, subs, true, nullptr);
  return FunctionType::get(input, result, getExtInfo());
}

FunctionType *PolymorphicFunctionType::substGenericArgs(Module *module,
                                                  ArrayRef<Substitution> subs) {
  SmallVector<Type, 4> replacements;
  getReplacementTypes(getGenericParams(), subs, replacements);
  
  return substGenericArgs(module, replacements);
}

FunctionType *
GenericFunctionType::substGenericArgs(Module *M, ArrayRef<Type> args) const {
  auto params = getGenericParams();
  assert(args.size() == params.size());
  
  TypeSubstitutionMap subs;
  for (size_t i = 0, e = args.size(); i != e; ++i) {
    subs.insert(std::make_pair(params[i], args[i]));
  }
  
  Type input = getInput().subst(M, subs, true, nullptr);
  Type result = getResult().subst(M, subs, true, nullptr);
  return FunctionType::get(input, result, getExtInfo());
}

AnyFunctionType *
GenericFunctionType::partialSubstGenericArgs(Module *M, ArrayRef<Type> args)
const {
  auto params = getGenericParams();
  
  // If we're fully applying the generic params, fall through to
  // substGenericArgs.
  if (args.size() == params.size())
    return substGenericArgs(M, args);
  
  assert(args.size() < params.size());
  
  TypeSubstitutionMap subs;
  for (size_t i = 0, e = args.size(); i != e; ++i) {
    subs.insert(std::make_pair(params[i], args[i]));
  }

  // Get the slice of the generic parameters and requirements we haven't
  // applied.
  auto unappliedParams = params.slice(args.size());
  
  // Requirements are in parameter order.
  // FIXME: Except for same-type requirements. Those need to be substituted
  // and propagated somehow.
  auto unappliedReqts = getRequirements();
  auto rootType = [](Type t) -> Type {
    while (auto dmt = t->getAs<DependentMemberType>()) {
      t = dmt->getBase();
    }
    return t;
  };
  
  for (auto appliedParam : params.slice(0, args.size())) {
    while (!unappliedReqts.empty()
           && rootType(unappliedReqts[0].getFirstType())
               ->isEqual(appliedParam)) {
      unappliedReqts = unappliedReqts.slice(1);
    }
  }
  
  GenericSignature *sig = GenericSignature::get(unappliedParams, unappliedReqts,
                                                M->getASTContext());
  
  Type input = getInput().subst(M, subs, true, nullptr);
  Type result = getResult().subst(M, subs, true, nullptr);
  return GenericFunctionType::get(sig, input, result, getExtInfo());
}

TypeSubstitutionMap
GenericSignature::getSubstitutionMap(
                                ArrayRef<GenericTypeParamType *> genericParams,
                                ArrayRef<Substitution> args) {
  TypeSubstitutionMap subs;
  
  // An empty parameter list gives an empty map.
  if (genericParams.empty()) {
    assert(args.empty() && "substitutions but no generic params?!");
    return subs;
  }
  
  assert(!args.empty() && "no substitutions?!");
  ASTContext &C = args[0].Archetype->getASTContext();
  llvm::DenseMap<ArchetypeType*, CanType> archetypeMap;
  
  // The substitution vector should be ordered so that substitutions for
  // primary archetypes at each depth are followed by the associated archetypes
  // for that depth. We should thus be able to convert primary archetypes to
  // generic parameters in order, then convert associated types to dependent
  // member types of archetypes we've already seen.
  unsigned genericParam = 0;
  for (auto &arg : args) {
    CanType fromType;
    if (auto parent = arg.Archetype->getParent()) {
      // If it's an associated type, substitute a dependent member type.
      assert(archetypeMap.count(parent));
      fromType = DependentMemberType::get(archetypeMap.find(parent)->second,
                                          arg.Archetype->getAssocType(), C)
        ->getCanonicalType();
    } else {
      // If no parent, it's a primary archetype. Substitute the next generic
      // parameter.
      auto param = genericParams[genericParam++];
      fromType = CanType(param);
      // FIXME: DependentMemberTypes are not SubstitutableTypes.
      subs[param] = arg.Replacement;
    }
    
    archetypeMap[arg.Archetype] = fromType;
  }
  
  assert(genericParam == genericParams.size()
         && "did not substitute all generic params!");
  
  return subs;
}

FunctionType *
GenericFunctionType::substGenericArgs(Module *M, ArrayRef<Substitution> args) {
  auto params = getGenericParams();
  (void)params;
  assert(args.size() == params.size());
  
  TypeSubstitutionMap subs
    = GenericSignature::getSubstitutionMap(getGenericParams(), args);

  Type input = getInput().subst(M, subs, true, nullptr);
  Type result = getResult().subst(M, subs, true, nullptr);
  return FunctionType::get(input, result, getExtInfo());
}

static Type getMemberForBaseType(Module *module,
                                 Type substBase,
                                 AssociatedTypeDecl *assocType,
                                 Identifier name,
                                 LazyResolver *resolver) {
  // If the parent is an archetype, extract the child archetype with the
  // given name.
  if (auto archetypeParent = substBase->getAs<ArchetypeType>()) {
    return archetypeParent->getNestedType(name);
  }

  // If the parent is a type variable, retrieve its member type
  // variable.
  if (auto typeVarParent = substBase->getAs<TypeVariableType>()) {
    assert(assocType && "Missing associated type");
    return substBase->getASTContext().getTypeVariableMemberType(typeVarParent,
                                                                assocType);
  }

  // Retrieve the member type with the given name.

  // Tuples don't have member types.
  if (substBase->is<TupleType>()) {
    return Type();
  }

  // If we know the associated type, look in the witness table.
  if (assocType) {
    // If the parent is dependent, create a dependent member type.
    if (substBase->isDependentType()) {
      return DependentMemberType::get(substBase, assocType,
                                      substBase->getASTContext());
    }

    auto proto = assocType->getProtocol();
    // FIXME: Introduce substituted type node here?
    auto conformance = module->lookupConformance(substBase, proto,
                                                 resolver);
    switch (conformance.getInt()) {
    case ConformanceKind::DoesNotConform:
    case ConformanceKind::UncheckedConforms:
      return Type();

    case ConformanceKind::Conforms:
      return conformance.getPointer()->getTypeWitness(assocType,
                                                      resolver).Replacement;
    }
  }

  // FIXME: This is a fallback. We want the above, conformance-based
  // result to be the only viable path.
  if (resolver) {
    if (Type memberType = resolver->resolveMemberType(module, substBase, name)){
      return memberType;
    }
  }

  return Type();
}

Type DependentMemberType::substBaseType(Module *module,
                                        Type substBase,
                                        LazyResolver *resolver) {
  if (substBase.getPointer() == getBase().getPointer())
    return this;

  // If the base remains dependent after substitution, so do we.
  if (substBase->isDependentType()) {
    if (getAssocType())
      return DependentMemberType::get(substBase, getAssocType(),
                                      getASTContext());
    else
      return DependentMemberType::get(substBase, getName(),
                                      getASTContext());
  }

  return getMemberForBaseType(module, substBase, getAssocType(), getName(),
                              resolver);
}

Type Type::subst(Module *module, TypeSubstitutionMap &substitutions,
                 bool ignoreMissing, LazyResolver *resolver) const {
  /// Return the original type or a null type, depending on the 'ignoreMissing'
  /// flag.
  auto failed = [&](Type t){ return ignoreMissing? t : Type(); };
  
  return transform([&](Type type) -> Type {
    assert(!isa<SILFunctionType>(type.getPointer()) &&
           "should not be doing AST type-substitution on a lowered SIL type;"
           "use SILType::subst");

    // We only substitute for substitutable types and dependent member types.
    
    // For dependent member types, we may need to look up the member if the
    // base is resolved to a non-dependent type.
    if (auto depMemTy = type->getAs<DependentMemberType>()) {
      auto newBase = depMemTy->getBase()
        .subst(module, substitutions, ignoreMissing, resolver);
      
      // Resolve the member relative to the substituted base.
      if (Type r = depMemTy->substBaseType(module, newBase, resolver))
        return r;
      return failed(type);
    }
    
    auto substOrig = type->getAs<SubstitutableType>();
    if (!substOrig)
      return type;

    // If we have a substitution for this type, use it.
    auto key = substOrig->getCanonicalType()->castTo<SubstitutableType>();
    auto known = substitutions.find(key);
    if (known != substitutions.end() && known->second)
      return SubstitutedType::get(substOrig, known->second,
                                  module->getASTContext());

    // If we don't have a substitution for this type and it doesn't have a
    // parent, then we're not substituting it.
    auto parent = substOrig->getParent();
    if (!parent)
      return type;

    // Substitute into the parent type.
    Type substParent = Type(parent).subst(module, substitutions, ignoreMissing,
                                          resolver);
    if (!substParent)
      return Type();

    // If the parent didn't change, we won't change.
    if (substParent.getPointer() == parent)
      return type;

    // Get the associated type reference from a child archetype.
    AssociatedTypeDecl *assocType = nullptr;
    if (auto archetype = substOrig->getAs<ArchetypeType>())
      assocType = archetype->getAssocType();
    
    if (Type r = getMemberForBaseType(module, substParent, assocType,
                                      substOrig->getName(), resolver))
      return r;
    return failed(type);
  });
}

Type TypeBase::getTypeOfMember(Module *module, const ValueDecl *member,
                               LazyResolver *resolver, Type memberType) {
  // If no member type was provided, use the member's type.
  if (!memberType)
    memberType = member->getType();

  // If the member is not part of a type, there's nothing to substitute.
  auto memberDC = member->getDeclContext();
  if (!memberDC->isTypeContext())
    return memberType;

  // Ignore lvalues in the base type.
  Type baseTy(getRValueType());

  // Look through the metatype; it has no bearing on the result.
  if (auto metaBase = baseTy->getAs<MetatypeType>()) {
    baseTy = metaBase->getInstanceType()->getRValueType();
  }

  // If the member is part of a protocol, we need to substitute in the
  // type of Self.
  if (auto memberProtocol = dyn_cast<ProtocolDecl>(memberDC)) {
    // We only substitute into archetypes for now.
    // FIXME: This seems like an odd restriction.
    if (!baseTy->is<ArchetypeType>())
      return memberType;

    // FIXME: This feels painfully inefficient. We're creating a dense map
    // for a single substitution.
    TypeSubstitutionMap substitutions;
    substitutions[memberProtocol->getSelf()->getArchetype()] = baseTy;
    return memberType.subst(module, substitutions, /*ignoreMissing=*/false,
                            resolver);
  }

  // Find the superclass type with the context matching that of the member.
  auto ownerNominal = memberDC->getDeclaredTypeOfContext()->getAnyNominal();
  while (baseTy->getAnyNominal() != ownerNominal) {
    baseTy = baseTy->getSuperclass(resolver);
    assert(baseTy && "Couldn't find appropriate context");
  }

  // If the base type isn't specialized, there's nothing to substitute.
  if (!baseTy->isSpecialized())
    return memberType;

  // Gather all of the substitutions for all levels of generic arguments.
  TypeSubstitutionMap substitutions;
  while (baseTy) {
    // For a bound generic type, gather the generic parameter -> generic
    // argument substitutions.
    if (auto boundGeneric = baseTy->getAs<BoundGenericType>()) {
      auto params = boundGeneric->getDecl()->getGenericParams()->getParams();
      auto args = boundGeneric->getGenericArgs();
      for (unsigned i = 0, n = args.size(); i != n; ++i) {
        // FIXME: Shouldn't need both archetype and generic parameter mappings.
        substitutions[params[i].getAsTypeParam()->getArchetype()] = args[i];
        substitutions[params[i].getAsTypeParam()->getDeclaredType()
                        ->getCanonicalType()->getAs<GenericTypeParamType>()]
          = args[i];
      }

      // Continue looking into the parent.
      baseTy = boundGeneric->getParent();
      continue;
    }

    // Continue looking into the parent.
    baseTy = baseTy->castTo<NominalType>()->getParent();
  }

  // Perform the substitution.
  return memberType.subst(module, substitutions, /*ignoreMissing=*/false,
                          resolver);
}

Identifier DependentMemberType::getName() const {
  if (NameOrAssocType.is<Identifier>())
    return NameOrAssocType.get<Identifier>();

  return NameOrAssocType.get<AssociatedTypeDecl *>()->getName();
}


Type Type::transform(const std::function<Type(Type)> &fn) const {
  // Transform this type node.
  Type transformed = fn(*this);

  // If the client changed the type, we're done.
  if (!transformed || transformed.getPointer() != getPointer())
    return transformed;

  // Recursive into children of this type.
  TypeBase *base = getPointer();
  switch (base->getKind()) {
#define ALWAYS_CANONICAL_TYPE(Id, Parent) \
case TypeKind::Id:
#define TYPE(Id, Parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Error:
  case TypeKind::TypeVariable:
  case TypeKind::AssociatedType:
  case TypeKind::GenericTypeParam:
    return *this;

  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Class: {
    auto nominalTy = cast<NominalType>(base);
    if (auto parentTy = nominalTy->getParent()) {
      parentTy = parentTy.transform(fn);
      if (!parentTy)
        return Type();

      if (parentTy.getPointer() == nominalTy->getParent().getPointer())
        return *this;

      return NominalType::get(nominalTy->getDecl(), parentTy,
                              Ptr->getASTContext());
    }

    return *this;
  }

  case TypeKind::SILFunction: {
    auto fnTy = cast<SILFunctionType>(base);
    bool changed = false;

    SILResultInfo origInterfaceResult = fnTy->getInterfaceResult();
    Type transInterfaceResult = origInterfaceResult.getType().transform(fn);
    if (!transInterfaceResult)
      return Type();
    CanType canTransInterfaceResult = transInterfaceResult->getCanonicalType();
    changed = changed || (canTransInterfaceResult != origInterfaceResult.getType());

    SmallVector<SILParameterInfo, 8> transInterfaceParams;
    for (auto origParam : fnTy->getInterfaceParameters()) {
      Type transParam = origParam.getType().transform(fn);
      if (!transParam) return Type();

      CanType canTransParam = transParam->getCanonicalType();
      transInterfaceParams.push_back(SILParameterInfo(canTransParam,
                                             origParam.getConvention()));
      changed = changed || (canTransParam != origParam.getType());
    }

    if (!changed) return *this;

    return SILFunctionType::get(fnTy->getGenericSignature(),
                                fnTy->getExtInfo(),
                                fnTy->getCalleeConvention(),
                                transInterfaceParams,
                                SILResultInfo(canTransInterfaceResult,
                                          origInterfaceResult.getConvention()),
                                Ptr->getASTContext());
  }

  case TypeKind::UnownedStorage:
  case TypeKind::WeakStorage: {
    auto storageTy = cast<ReferenceStorageType>(base);
    Type refTy = storageTy->getReferentType();
    Type substRefTy = refTy.transform(fn);
    if (!substRefTy)
      return Type();

    if (substRefTy.getPointer() == refTy.getPointer())
      return *this;

    return ReferenceStorageType::get(substRefTy, storageTy->getOwnership(),
                                     Ptr->getASTContext());
  }

  case TypeKind::UnboundGeneric: {
    auto unbound = cast<UnboundGenericType>(base);
    Type substParentTy;
    if (auto parentTy = unbound->getParent()) {
      substParentTy = parentTy.transform(fn);
      if (!substParentTy)
        return Type();

      if (substParentTy.getPointer() == parentTy.getPointer())
        return *this;

      return UnboundGenericType::get(unbound->getDecl(), substParentTy,
                                     Ptr->getASTContext());
    }

    return *this;
  }

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct: {
    auto bound = cast<BoundGenericType>(base);
    SmallVector<Type, 4> substArgs;
    bool anyChanged = false;
    Type substParentTy;
    if (auto parentTy = bound->getParent()) {
      substParentTy = parentTy.transform(fn);
      if (!substParentTy)
        return Type();

      if (substParentTy.getPointer() != parentTy.getPointer())
        anyChanged = true;
    }

    for (auto arg : bound->getGenericArgs()) {
      Type substArg = arg.transform(fn);
      if (!substArg)
        return Type();
      substArgs.push_back(substArg);
      if (substArg.getPointer() != arg.getPointer())
        anyChanged = true;
    }

    if (!anyChanged)
      return *this;

    return BoundGenericType::get(bound->getDecl(), substParentTy, substArgs);
  }

  case TypeKind::Metatype: {
    auto meta = cast<MetatypeType>(base);
    auto instanceTy = meta->getInstanceType().transform(fn);
    if (!instanceTy)
      return Type();

    if (instanceTy.getPointer() == meta->getInstanceType().getPointer())
      return *this;

    if (meta->hasThin())
      return MetatypeType::get(instanceTy, meta->isThin(),
                               Ptr->getASTContext());
    return MetatypeType::get(instanceTy, Ptr->getASTContext());
  }

  case TypeKind::DynamicSelf: {
    auto dynamicSelf = cast<DynamicSelfType>(base);
    auto selfTy = dynamicSelf->getSelfType().transform(fn);
    if (!selfTy)
      return Type();

    if (selfTy.getPointer() == dynamicSelf->getSelfType().getPointer())
      return *this;

    return DynamicSelfType::get(selfTy, selfTy->getASTContext());
  }

  case TypeKind::NameAlias: {
    auto alias = cast<NameAliasType>(base);
    auto underlyingTy = alias->getDecl()->getUnderlyingType().transform(fn);
    if (!underlyingTy)
      return Type();

    if (underlyingTy.getPointer() ==
          alias->getDecl()->getUnderlyingType().getPointer())
      return *this;

    return SubstitutedType::get(*this, underlyingTy, Ptr->getASTContext());
  }

  case TypeKind::Paren: {
    auto paren = cast<ParenType>(base);
    Type underlying = paren->getUnderlyingType().transform(fn);
    if (!underlying)
      return Type();

    if (underlying.getPointer() == paren->getUnderlyingType().getPointer())
      return *this;

    return ParenType::get(Ptr->getASTContext(), underlying);
  }

  case TypeKind::Tuple: {
    auto tuple = cast<TupleType>(base);
    bool anyChanged = false;
    SmallVector<TupleTypeElt, 4> elements;
    unsigned Index = 0;
    for (const auto &elt : tuple->getFields()) {
      Type eltTy = elt.getType().transform(fn);
      if (!eltTy)
        return Type();

      // If nothing has changd, just keep going.
      if (!anyChanged && eltTy.getPointer() == elt.getType().getPointer()) {
        ++Index;
        continue;
      }

      // If this is the first change we've seen, copy all of the previous
      // elements.
      if (!anyChanged) {
        // Copy all of the previous elements.
        for (unsigned I = 0; I != Index; ++I) {
          const TupleTypeElt &FromElt =tuple->getFields()[I];
          elements.push_back(TupleTypeElt(FromElt.getType(), FromElt.getName(),
                                          FromElt.getDefaultArgKind(),
                                          FromElt.isVararg()));
        }

        anyChanged = true;
      }

      // Add the new tuple element, with the new type, no initializer,
      elements.push_back(TupleTypeElt(eltTy, elt.getName(),
                                      elt.getDefaultArgKind(), elt.isVararg()));
      ++Index;
    }

    if (!anyChanged)
      return *this;

    return TupleType::get(elements, Ptr->getASTContext());
  }


  case TypeKind::DependentMember: {
    auto dependent = cast<DependentMemberType>(base);
    auto dependentBase = dependent->getBase().transform(fn);
    if (!dependentBase)
      return Type();

    if (dependentBase.getPointer() == dependent->getBase().getPointer())
      return *this;

    return DependentMemberType::get(dependentBase, dependent->getName(),
                                    Ptr->getASTContext());
  }

  case TypeKind::Substituted: {
    auto substAT = cast<SubstitutedType>(base);
    auto substTy = substAT->getReplacementType().transform(fn);
    if (!substTy)
      return Type();

    if (substTy.getPointer() == substAT->getReplacementType().getPointer())
      return *this;

    return SubstitutedType::get(substAT->getOriginal(), substTy,
                                Ptr->getASTContext());
  }

  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    auto function = cast<AnyFunctionType>(base);
    auto inputTy = function->getInput().transform(fn);
    if (!inputTy)
      return Type();
    auto resultTy = function->getResult().transform(fn);
    if (!resultTy)
      return Type();

    if (inputTy.getPointer() == function->getInput().getPointer() &&
        resultTy.getPointer() == function->getResult().getPointer())
      return *this;

    if (auto polyFn = dyn_cast<PolymorphicFunctionType>(function)) {
      return PolymorphicFunctionType::get(inputTy, resultTy,
                                          &polyFn->getGenericParams(),
                                          function->getExtInfo());
    }

    return FunctionType::get(inputTy, resultTy,
                             function->getExtInfo());
  }

  case TypeKind::GenericFunction: {
    GenericFunctionType *function = cast<GenericFunctionType>(base);
    bool anyChanges = false;

    // Transform generic parameters.
    SmallVector<GenericTypeParamType *, 4> genericParams;
    for (auto param : function->getGenericParams()) {
      Type paramTy = Type(param).transform(fn);
      if (!paramTy)
        return Type();

      if (auto newParam = paramTy->getAs<GenericTypeParamType>()) {
        if (newParam != param)
          anyChanges = true;

        genericParams.push_back(newParam);
      } else {
        anyChanges = true;
      }
    }

    // Transform requirements.
    SmallVector<Requirement, 4> requirements;
    for (const auto &req : function->getRequirements()) {
      auto firstType = req.getFirstType().transform(fn);
      if (!firstType)
        return Type();

      Type secondType = req.getSecondType();
      if (secondType) {
        secondType = secondType.transform(fn);
        if (!secondType)
          return Type();
      }

      if (firstType->isDependentType() || 
          (secondType && secondType->isDependentType())) {
        if (firstType.getPointer() != req.getFirstType().getPointer() ||
            secondType.getPointer() != req.getSecondType().getPointer())
          anyChanges = true;

        requirements.push_back(Requirement(req.getKind(), firstType,
                                           secondType));
      } else
        anyChanges = true;
    }
    
    auto sig = GenericSignature::get(genericParams, requirements,
                                     Ptr->getASTContext());

    // Transform input type.
    auto inputTy = function->getInput().transform(fn);
    if (!inputTy)
      return Type();

    // Transform result type.
    auto resultTy = function->getResult().transform(fn);
    if (!resultTy)
      return Type();

    // Check whether anything changed.
    if (!anyChanges &&
        inputTy.getPointer() == function->getInput().getPointer() &&
        resultTy.getPointer() == function->getResult().getPointer())
      return *this;

    // If no generic parameters remain, this is a non-generic function type.
    if (genericParams.empty())
      return FunctionType::get(inputTy, resultTy, function->getExtInfo());

    // Produce the new generic function type.
    
    return GenericFunctionType::get(sig, inputTy, resultTy,
                                    function->getExtInfo());
  }

  case TypeKind::Array: {
    auto array = cast<ArrayType>(base);
    auto baseTy = array->getBaseType().transform(fn);
    if (!baseTy)
      return Type();

    if (baseTy.getPointer() == array->getBaseType().getPointer())
      return *this;

    return ArrayType::get(baseTy, array->getSize());
  }

  case TypeKind::ArraySlice: {
    auto slice = cast<ArraySliceType>(base);
    auto baseTy = slice->getBaseType().transform(fn);
    if (!baseTy)
      return Type();

    if (baseTy.getPointer() == slice->getBaseType().getPointer())
      return *this;

    return ArraySliceType::get(baseTy);
  }

  case TypeKind::Optional: {
    auto optional = cast<OptionalType>(base);
    auto baseTy = optional->getBaseType().transform(fn);
    if (!baseTy)
      return Type();

    if (baseTy.getPointer() == optional->getBaseType().getPointer())
      return *this;

    return OptionalType::get(baseTy);
  }

  case TypeKind::UncheckedOptional: {
    auto optional = cast<UncheckedOptionalType>(base);
    auto baseTy = optional->getBaseType().transform(fn);
    if (!baseTy)
      return Type();

    if (baseTy.getPointer() == optional->getBaseType().getPointer())
      return *this;

    return UncheckedOptionalType::get(baseTy);
  }

  case TypeKind::LValue: {
    auto lvalue = cast<LValueType>(base);
    auto objectTy = lvalue->getObjectType().transform(fn);
    if (!objectTy) return Type();

    return objectTy.getPointer() == lvalue->getObjectType().getPointer() ?
      *this : LValueType::get(objectTy);
  }

  case TypeKind::InOut: {
    auto inout = cast<InOutType>(base);
    auto objectTy = inout->getObjectType().transform(fn);
    if (!objectTy) return Type();
    
    return objectTy.getPointer() == inout->getObjectType().getPointer() ?
      *this : InOutType::get(objectTy);
  }

  case TypeKind::ProtocolComposition: {
    auto pc = cast<ProtocolCompositionType>(base);
    SmallVector<Type, 4> protocols;
    bool anyChanged = false;
    unsigned index = 0;
    for (auto proto : pc->getProtocols()) {
      auto substProto = proto.transform(fn);
      if (!substProto)
        return Type();
      
      if (anyChanged) {
        protocols.push_back(substProto);
        ++index;
        continue;
      }
      
      if (substProto.getPointer() != proto.getPointer()) {
        anyChanged = true;
        protocols.append(protocols.begin(), protocols.begin() + index);
        protocols.push_back(substProto);
      }
      
      ++index;
    }
    
    if (!anyChanged)
      return *this;
    
    return ProtocolCompositionType::get(Ptr->getASTContext(), protocols);
  }
  }
  
  llvm_unreachable("Unhandled type in transformation");
}


bool Type::findIf(const std::function<bool(Type)> &pred) const {
  class Walker : public TypeWalker {
    const std::function<bool(Type)> &Pred;
  public:
    explicit Walker(const std::function<bool(Type)> &pred) : Pred(pred) {}

    virtual Action walkToTypePre(Type ty) override {
      if (Pred(ty))
        return Action::Stop;
      return Action::Continue;
    }
  };

  return walk(Walker(pred));
}

static TypeSubstitutionMap getSubstitutionMap(ArrayRef<Substitution> subs) {
  TypeSubstitutionMap r;
  
  for (auto sub : subs) {
    if (sub.Archetype->isPrimary())
      r[sub.Archetype] = sub.Replacement;
  }
  
  return r;
}

Substitution Substitution::subst(Module *module,
                                 ArrayRef<Substitution> subs) const {
  TypeSubstitutionMap subMap = getSubstitutionMap(subs);
  return subst(module, subs, subMap);
}

Substitution Substitution::subst(Module *module,
                                 ArrayRef<Substitution> subs,
                                 TypeSubstitutionMap &subMap) const {
  // Substitute the replacement.
  Type substReplacement
    = Replacement.subst(module, subMap, /*ignoreMissing*/false, nullptr);
  assert(substReplacement && "substitution replacement failed");
  
  if (substReplacement->isEqual(Replacement))
    return *this;
  
  bool conformancesChanged = false;
  SmallVector<ProtocolConformance *, 4> substConformance;
  substConformance.reserve(Conformance.size());
  
  // When substituting a concrete type for an archetype, we need to fill in the
  // conformances.
  if (auto replacementArch = Replacement->getAs<ArchetypeType>()) {
    if (!substReplacement->is<ArchetypeType>()) {
      conformancesChanged = true;
      // Find the substitution that satisfied the archetype.
      auto archetypeSub = std::find_if(subs.begin(), subs.end(),
         [&](const Substitution &s) { return s.Archetype == replacementArch; });
      assert(archetypeSub != subs.end()
             && "no substitution for substituted archetype?!");
      // Get the conformances for the type that apply to the original
      // substituted archetype.
      for (auto proto : Archetype->getConformsTo()) {
        for (auto c : archetypeSub->Conformance) {
          if (c->getProtocol() == proto) {
            substConformance.push_back(c);
            goto found_conformance;
          }
          if (c->getProtocol()->inheritsFrom(proto)) {
            substConformance.push_back(c->getInheritedConformance(proto));
            goto found_conformance;
          }
        }
        assert(false && "did not find conformance for archetype requirement?!");
found_conformance:;
      }
    }
  } else {
    // If we substituted a concrete type for another, we need to substitute the
    // conformance to apply to the new type.
    for (auto c : Conformance) {
      auto substC = c->subst(module, substReplacement, subs, subMap);
      if (c != substC)
        conformancesChanged = true;
      substConformance.push_back(substC);
    }
  }
  
  ArrayRef<ProtocolConformance *> substConformanceRef;
  if (conformancesChanged)
    substConformanceRef = module->getASTContext().AllocateCopy(substConformance);
  else
    substConformanceRef = Conformance;

  assert(substReplacement->is<ArchetypeType>()
         || substConformanceRef.size() == Archetype->getConformsTo().size());

  return Substitution{Archetype, substReplacement, substConformanceRef};
}
