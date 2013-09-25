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

/// isMaterializable - Is this type 'materializable' according to the
/// rules of the language?  Basically, does it not contain any l-value
/// types?
bool TypeBase::isMaterializable() {
  // Tuples are materializable if all their elements are.
  if (TupleType *tuple = getAs<TupleType>()) {
    for (auto &field : tuple->getFields())
      if (!field.getType()->isMaterializable())
        return false;
    return true;
  }

  // Some l-values may be materializable someday.
  if (LValueType *lvalue = getAs<LValueType>())
    return lvalue->isMaterializable();

  // Everything else is materializable.
  return true;
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
  case TypeKind::MetaType:
  case TypeKind::Module:
  case TypeKind::Array:
  case TypeKind::LValue:
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
      
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::Class:
  case TypeKind::BoundGenericClass:
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
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

  case TypeKind::Class:
  case TypeKind::Struct:
  case TypeKind::Enum:
    if (auto parentTy = cast<NominalType>(this)->getParent())
      return parentTy->isUnspecializedGeneric();
    return false;

  case TypeKind::MetaType:
    return cast<MetaTypeType>(this)->getInstanceType()
             ->isUnspecializedGeneric();

  case TypeKind::UnownedStorage:
  case TypeKind::WeakStorage:
    return cast<ReferenceStorageType>(this)->getReferentType()
             ->isUnspecializedGeneric();

  case TypeKind::LValue:
    return cast<LValueType>(this)->getObjectType()->isUnspecializedGeneric();

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
  case TypeKind::Protocol:
  case TypeKind::ProtocolComposition:
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

Type TypeBase::getOptionalObjectType(const ASTContext &context) {
  if (auto boundTy = getAs<BoundGenericType>())
    if (boundTy->getDecl() == context.getOptionalDecl())
      return boundTy->getGenericArgs()[0];
  return Type();
}

static Type getStrippedType(const ASTContext &context, Type type,
                            bool stripLabels, bool stripDefaultArgs) {
  return type.transform(context,
                        [&](Type type) -> Type {
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

/// \brief 'Minimize' the given set of protocols by eliminating any mentions of
/// protocols that are already covered by inheritance due to other entries in
/// the protocol list.
static void minimizeProtocols(SmallVectorImpl<ProtocolDecl *> &Protocols) {
  llvm::SmallDenseMap<ProtocolDecl *, unsigned> Known;
  llvm::SmallPtrSet<ProtocolDecl *, 16> Visited;
  SmallVector<ProtocolDecl *, 16> Stack;
  bool ZappedAny = false;

  // Seed the stack with the protocol declarations in the original list.
  // Zap any obvious duplicates along the way.
  for (unsigned I = 0, N = Protocols.size(); I != N; ++I) {
    // Check whether we've seen this protocol before.
    auto KnownPos = Known.find(Protocols[I]);
    
    // If we have not seen this protocol before, record it's index.
    if (KnownPos == Known.end()) {
      Known[Protocols[I]] = I;
      Stack.push_back(Protocols[I]);
      continue;
    }
    
    // We have seen this protocol before; zap this occurrance.
    Protocols[I] = 0;
    ZappedAny = true;
  }
  
  // Walk the inheritance hierarchies of all of the protocols. If we run into
  // one of the known protocols, zap it from the original list.
  while (!Stack.empty()) {
    ProtocolDecl *Current = Stack.back();
    Stack.pop_back();
    
    // Add the protocols we inherited.
    for (auto Inherited : Current->getProtocols()) {
      addMinimumProtocols(Inherited->getDeclaredType(), Protocols, Known,
                          Visited, Stack, ZappedAny);
    }
  }
  
  if (ZappedAny)
    Protocols.erase(std::remove(Protocols.begin(), Protocols.end(), nullptr),
                    Protocols.end());
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

/// getCanonicalType - Return the canonical version of this type, which has
/// sugar from all levels stripped off.
CanType TypeBase::getCanonicalType() {
  assert(this != 0 &&
         "Cannot call getCanonicalType before name binding is complete");

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
             getDesugaredType()->getCanonicalType().getPointer(); \
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
    // FIXME: Actually canonicalize to a sensible representation that doesn't
    // contain the declaration.
    Result = this;
    break;
  }

  case TypeKind::DependentMember: {
    auto dependent = cast<DependentMemberType>(this);
    auto base = dependent->getBase()->getCanonicalType();
    const ASTContext &ctx = base->getASTContext();
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
  case TypeKind::LValue: {
    LValueType *lvalue = cast<LValueType>(this);
    Type objectType = lvalue->getObjectType();
    objectType = objectType->getCanonicalType();
    Result = LValueType::get(objectType, lvalue->getQualifiers(),
                             objectType->getASTContext());
    break;
  }
  case TypeKind::PolymorphicFunction: {
    PolymorphicFunctionType *FT = cast<PolymorphicFunctionType>(this);
    Type In = FT->getInput()->getCanonicalType();
    Type Out = FT->getResult()->getCanonicalType();
    Result = PolymorphicFunctionType::get(In, Out, &FT->getGenericParams(),
                                          FT->getExtInfo(),
                                          In->getASTContext());
    break;
  }
  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(this);
    Type In = FT->getInput()->getCanonicalType();
    Type Out = FT->getResult()->getCanonicalType();
    Result = FunctionType::get(In, Out,
                               FT->getExtInfo(),
                               In->getASTContext());
    break;
  }
  case TypeKind::Array: {
    ArrayType *AT = cast<ArrayType>(this);
    Type EltTy = AT->getBaseType()->getCanonicalType();
    Result = ArrayType::get(EltTy, AT->getSize(), EltTy->getASTContext());
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
  case TypeKind::MetaType: {
    MetaTypeType *MT = cast<MetaTypeType>(this);
    Type InstanceTy = MT->getInstanceType()->getCanonicalType();
    Result = MetaTypeType::get(InstanceTy, InstanceTy->getASTContext());
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
  case TypeKind::Array:
  case TypeKind::LValue:
  case TypeKind::ProtocolComposition:
  case TypeKind::MetaType:
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
    // None of these types have sugar at the outer level.
    return this;
  case TypeKind::Paren:
    return cast<ParenType>(this)->getDesugaredType();
  case TypeKind::NameAlias:
    return cast<NameAliasType>(this)->getDesugaredType();
  case TypeKind::AssociatedType:
    return cast<AssociatedTypeType>(this)->getDesugaredType();
  case TypeKind::ArraySlice:
  case TypeKind::Optional:
    return cast<SyntaxSugarType>(this)->getDesugaredType();
  case TypeKind::Substituted:
    return cast<SubstitutedType>(this)->getDesugaredType();
  }

  llvm_unreachable("Unknown type kind");
}

TypeBase *ParenType::getDesugaredType() {
  return getUnderlyingType()->getDesugaredType();
}

TypeBase *NameAliasType::getDesugaredType() {
  return getDecl()->getUnderlyingType()->getDesugaredType();
}

TypeBase *SyntaxSugarType::getDesugaredType() {
  return getImplementationType()->getDesugaredType();
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
  } else {
    llvm_unreachable("Unhandled syntax sugar type");
  }

  // Record the implementation type.
  ImplOrContext = BoundGenericType::get(implDecl, Type(), Base);
  return ImplOrContext.get<Type>();
}

TypeBase *SubstitutedType::getDesugaredType() {
  return getReplacementType()->getDesugaredType();
}

unsigned GenericTypeParamType::getDepth() const {
  return Param->getDepth();
}

unsigned GenericTypeParamType::getIndex() const {
  return Param->getIndex();
}

TypeBase *AssociatedTypeType::getDesugaredType() {
  return getDecl()->getArchetype()->getDesugaredType();
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

  case TypeKind::PolymorphicFunction: {
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
  case TypeKind::MetaType: {
    auto mMe = cast<MetaTypeType>(me);
    auto mThem = cast<MetaTypeType>(them);
    return mMe->getInstanceType()->isSpelledLike(mThem->getInstanceType());
  }
  case TypeKind::Paren: {
    auto pMe = cast<ParenType>(me);
    auto pThem = cast<ParenType>(them);
    return pMe->getUnderlyingType()->isSpelledLike(pThem->getUnderlyingType());
  }
  case TypeKind::ArraySlice:
  case TypeKind::Optional: {
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

bool TypeBase::isDependentType() {
  auto canon = getCanonicalType();
  return canon.findIf([](Type type) -> bool {
    // The presence of a generic type parameter indicates a dependent type.
    return isa<GenericTypeParamType>(type.getPointer());
  });
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

TupleType::TupleType(ArrayRef<TupleTypeElt> fields, const ASTContext *CanCtx,
                     bool hasTypeVariable)
  : TypeBase(TypeKind::Tuple, CanCtx, hasTypeVariable), Fields(fields) { }

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
                                     AssociatedTypeDecl *AssocType,
                                     Identifier Name, ArrayRef<Type> ConformsTo,
                                     Type Superclass,
                                     Optional<unsigned> Index) {
  // Gather the set of protocol declarations to which this archetype conforms.
  SmallVector<ProtocolDecl *, 4> ConformsToProtos;
  for (auto P : ConformsTo) {
    addProtocols(P, ConformsToProtos);
  }
  minimizeProtocols(ConformsToProtos);
  llvm::array_pod_sort(ConformsToProtos.begin(), ConformsToProtos.end(),
                       compareProtocols);

  auto arena = AllocationArena::Permanent;
  return new (Ctx, arena) ArchetypeType(Ctx, Parent, AssocType, Name,
                                        Ctx.AllocateCopy(ConformsToProtos),
                                        Superclass, Index);
}

ArchetypeType *
ArchetypeType::getNew(const ASTContext &Ctx, ArchetypeType *Parent,
                      AssociatedTypeDecl *AssocType,
                      Identifier Name,
                      SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                      Type Superclass, Optional<unsigned> Index) {
  // Gather the set of protocol declarations to which this archetype conforms.
  minimizeProtocols(ConformsTo);
  llvm::array_pod_sort(ConformsTo.begin(), ConformsTo.end(), compareProtocols);

  auto arena = AllocationArena::Permanent;
  return new (Ctx, arena) ArchetypeType(Ctx, Parent, AssocType, Name,
                                        Ctx.AllocateCopy(ConformsTo),
                                        Superclass, Index);
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
  minimizeProtocols(Protocols);

  // If one protocol remains, its nominal type is the canonical type.
  if (Protocols.size() == 1)
    return Protocols.front()->getDeclaredType();

  // Sort the set of protocols by module + name, to give a stable
  // ordering.
  // FIXME: Consider namespaces here as well.
  llvm::array_pod_sort(Protocols.begin(), Protocols.end(), compareProtocols);

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

FunctionType *PolymorphicFunctionType::substGenericArgs(Module *module,
                                                        ArrayRef<Type> args) {
  auto params = getGenericParameters();
  assert(args.size() <= params.size());

  TypeSubstitutionMap subs;
  for (size_t i = 0, e = args.size(); i != e; ++i) {
    subs.insert(std::make_pair(params[i].getAsTypeParam()->getArchetype(),
                               args[i]));
  }
  Type input = getInput().subst(module, subs, true, nullptr);
  Type result = getResult().subst(module, subs, true, nullptr);
  return FunctionType::get(input, result, getExtInfo(),
                           module->getASTContext());
}

Type Type::subst(Module *module, TypeSubstitutionMap &substitutions,
                 bool ignoreMissing, LazyResolver *resolver) const {
  return transform(module->getASTContext(), [&](Type type) -> Type {
    // We only substitute for substitutable types.
    auto substOrig = type->getAs<SubstitutableType>();
    if (!substOrig)
      return type;

    // If we have a substitution for this type, use it.
    auto known = substitutions.find(substOrig);
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

    // If the parent is an archetype, extract the child archetype with the
    // given name.
    if (auto archetypeParent = substParent->getAs<ArchetypeType>()) {
      return archetypeParent->getNestedType(substOrig->getName());
    }

    // Retrieve the type with the given name.

    // Tuples don't have member types.
    if (substParent->is<TupleType>()) {
      return ignoreMissing? type : Type();
    }

    // If we have an archetype for which we know the associated type,
    // look in the witness table.
    if (auto archetype = substOrig->getAs<ArchetypeType>()) {
      if (auto assocType = archetype->getAssocType()) {
        auto proto = cast<ProtocolDecl>(assocType->getDeclContext());
        // FIXME: Introduce substituted type node here?
        auto conformance = module->lookupConformance(substParent, proto,
                                                     resolver);
        switch (conformance.getInt()) {
        case ConformanceKind::DoesNotConform:
        case ConformanceKind::UncheckedConforms:
          return ignoreMissing? type : Type();

        case ConformanceKind::Conforms:
          return conformance.getPointer()->getTypeWitness(assocType).Replacement;
        }
      }
    }

    // FIXME: This is a fallback. We want the above, conformance-based
    // result to be the only viable path.
    if (resolver) {
      if (Type memberType = resolver->resolveMemberType(substParent,
                                                        substOrig->getName())) {
        return memberType;
      }
    }

    return ignoreMissing? type : Type();
  });
}

Type TypeBase::getTypeOfMember(Module *module, ValueDecl *member,
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
  if (auto metaBase = baseTy->getAs<MetaTypeType>()) {
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
        substitutions[params[i].getAsTypeParam()->getArchetype()] = args[i];
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


