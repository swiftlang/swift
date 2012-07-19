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
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <iterator>
using namespace swift;

// Only allow allocation of Stmts using the allocator in ASTContext.
void *TypeBase::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
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
  // At the moment, Builtin.ObjectPointer, class types, and function types.
  return is<BuiltinObjectPointerType>() || is<ClassType>() || is<FunctionType>();
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
  
  return false;
}

Type TypeBase::getUnlabeledType(ASTContext &Context) {
  switch (getKind()) {
  case TypeKind::Error: 
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinFloat:
  case TypeKind::UnstructuredUnresolved:
  case TypeKind::OneOf:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::MetaType:
  case TypeKind::Module:
  case TypeKind::Protocol:
  case TypeKind::Archetype:
  case TypeKind::ProtocolComposition:
  case TypeKind::UnboundGeneric:
  case TypeKind::BoundGeneric:
  case TypeKind::DeducibleGenericParam:
    return this;

  case TypeKind::NameAlias:
    if (TypeAliasDecl *D = cast<NameAliasType>(this)->getDecl()) {
      Type UnderlingTy = D->getUnderlyingType()->getUnlabeledType(Context);
      if (UnderlingTy.getPointer() != D->getUnderlyingType().getPointer())
        return UnderlingTy;
    }
    
    return this;
      
  case TypeKind::Identifier: {
    IdentifierType *IdentTy = cast<IdentifierType>(this);
    if (IdentTy->isMapped()) {
      Type MappedTy = IdentTy->getMappedType()->getUnlabeledType(Context);
      if (MappedTy.getPointer() != IdentTy->getMappedType().getPointer())
        return MappedTy;
    }
    return this;
  }
  
  case TypeKind::Paren: {
    ParenType *ParenTy = cast<ParenType>(this);
    Type UnderlyingTy = ParenTy->getUnderlyingType()->getUnlabeledType(Context);
    if (UnderlyingTy.getPointer() != ParenTy->getUnderlyingType().getPointer())
      return ParenType::get(Context, UnderlyingTy);
    return this;
  }
      
  case TypeKind::Tuple: {
    TupleType *TupleTy = cast<TupleType>(this);
    llvm::SmallVector<TupleTypeElt, 4> Elements;
    bool Rebuild = false;
    unsigned Idx = 0;
    for (const TupleTypeElt &Elt : TupleTy->getFields()) {
      Type EltTy = Elt.getType()->getUnlabeledType(Context);
      if (EltTy.getPointer() != Elt.getType().getPointer() ||
          Elt.getInit() || !Elt.getName().empty() || Rebuild) {
        if (!Rebuild) {
          Elements.reserve(TupleTy->getFields().size());
          for (unsigned I = 0; I != Idx; ++I) {
            const TupleTypeElt &Elt = TupleTy->getFields()[I];
            Elements.push_back(TupleTypeElt(Elt.getType(), Identifier(),
                                            nullptr, Elt.getVarargBaseTy()));
          }
          Rebuild = true;
        }

        Type VarargBaseType;
        if (Elt.isVararg())
          VarargBaseType = Elt.getVarargBaseTy()->getUnlabeledType(Context);
        Elements.push_back(TupleTypeElt(EltTy, Identifier(),
                                        nullptr, VarargBaseType));
      }
      ++Idx;
    }
    
    if (!Rebuild)
      return this;
    
    // An unlabeled 1-element tuple type is represented as a parenthesized
    // type.
    if (Elements.size() == 1 && !Elements[0].isVararg())
      return ParenType::get(Context, Elements[0].getType());
    
    return TupleType::get(Elements, Context);
  }
      
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    AnyFunctionType *FunctionTy = cast<AnyFunctionType>(this);
    Type InputTy = FunctionTy->getInput()->getUnlabeledType(Context);
    Type ResultTy = FunctionTy->getResult()->getUnlabeledType(Context);
    if (InputTy.getPointer() != FunctionTy->getInput().getPointer() ||
        ResultTy.getPointer() != FunctionTy->getResult().getPointer()) {
      if (auto monoFn = dyn_cast<FunctionType>(FunctionTy)) {
        return FunctionType::get(InputTy, ResultTy,
                                 monoFn->isAutoClosure(), Context);
      } else {
        auto polyFn = cast<PolymorphicFunctionType>(FunctionTy);
        return PolymorphicFunctionType::get(InputTy, ResultTy,
                                            &polyFn->getGenericParams(),
                                            Context);
      }
    }
    
    return this;
  }
      
  case TypeKind::Array: {
    ArrayType *ArrayTy = cast<ArrayType>(this);
    Type BaseTy = ArrayTy->getBaseType()->getUnlabeledType(Context);
    if (BaseTy.getPointer() != ArrayTy->getBaseType().getPointer())
      return ArrayType::get(BaseTy, ArrayTy->getSize(), Context);
    
    return this;
  }

  case TypeKind::ArraySlice: {
    ArraySliceType *sliceTy = cast<ArraySliceType>(this);
    Type baseTy = sliceTy->getBaseType()->getUnlabeledType(Context);
    if (baseTy.getPointer() != sliceTy->getBaseType().getPointer()) {
      ArraySliceType *newSliceTy 
        = ArraySliceType::get(baseTy, sliceTy->getFirstRefLoc(), Context);
      if (!newSliceTy->hasImplementationType())
        newSliceTy->setImplementationType(sliceTy->getImplementationType());
      return newSliceTy;
    }
    
    return this;
  }
      
  case TypeKind::LValue: {
    LValueType *LValueTy = cast<LValueType>(this);
    Type ObjectTy = LValueTy->getObjectType()->getUnlabeledType(Context);
    if (ObjectTy.getPointer() != LValueTy->getObjectType().getPointer())
      return LValueType::get(ObjectTy, LValueTy->getQualifiers(), Context);
    return this;
  }
      
  case TypeKind::Substituted: {
    SubstitutedType *SubstTy = cast<SubstitutedType>(this);
    Type NewSubstTy = SubstTy->getReplacementType()->getUnlabeledType(Context);
    if (NewSubstTy.getPointer() != SubstTy->getReplacementType().getPointer())
      return SubstitutedType::get(SubstTy->getOriginal(), NewSubstTy,
                                     Context);
    return this;
  }
  }
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
      for (auto Inherited : Proto->getDecl()->getInherited())
        addProtocols(Inherited, Stack);
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
    for (auto Inherited : Current->getInherited()) {
      addMinimumProtocols(Inherited, Protocols, Known, Visited, Stack,
                          ZappedAny);
    }
  }
  
  if (ZappedAny)
    Protocols.erase(std::remove(Protocols.begin(), Protocols.end(), nullptr),
                    Protocols.end());
}

/// \brief Compare two protocols to establish an ordering between them.
static int compareProtocols(const void *V1, const void *V2) {
  const ProtocolDecl *P1 = *reinterpret_cast<const ProtocolDecl *const *>(V1);
  const ProtocolDecl *P2 = *reinterpret_cast<const ProtocolDecl *const *>(V2);
 
  DeclContext *DC1 = P1->getDeclContext();
  while (!DC1->isModuleContext())
    DC1 = DC1->getParent();
  DeclContext *DC2 = P2->getDeclContext();
  while (!DC2->isModuleContext())
    DC2 = DC2->getParent();
  
  // Try ordering based on module name, first.
  if (int result
        = cast<Module>(DC1)->Name.str().compare(cast<Module>(DC2)->Name.str()))
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
  if (CanonicalType.is<ASTContext*>())
    return CanType(this);
  // If the canonical type was already computed, just return what we have.
  if (TypeBase *CT = CanonicalType.get<TypeBase*>())
    return CanType(CT);
  
  // Otherwise, compute and cache it.
  TypeBase *Result = 0;
  switch (getKind()) {
#define ALWAYS_CANONICAL_TYPE(id, parent) case TypeKind::id:
#define UNCHECKED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("these types are always canonical");

#define SUGARED_TYPE(id, parent) \
  case TypeKind::id: \
    Result = cast<id##Type>(this)-> \
             getDesugaredType()->getCanonicalType().getPointer(); \
    break;
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"

  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(this);
    assert(!TT->getFields().empty() && "Empty tuples are always canonical");

    SmallVector<TupleTypeElt, 8> CanElts;
    CanElts.reserve(TT->getFields().size());
    for (const TupleTypeElt &field : TT->getFields()) {
      assert(!field.getType().isNull() &&
             "Cannot get canonical type of un-typechecked TupleType!");
      Type canVarargBaseTy;
      if (field.isVararg())
        canVarargBaseTy = field.getVarargBaseTy()->getCanonicalType();
      CanElts.push_back(TupleTypeElt(field.getType()->getCanonicalType(),
                                     field.getName(),
                                     field.getInit(),
                                     canVarargBaseTy));
    }
    
    Result = TupleType::get(CanElts, CanElts[0].getType()->getASTContext());
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
                                          In->getASTContext());
    break;
  }
  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(this);
    Type In = FT->getInput()->getCanonicalType();
    Type Out = FT->getResult()->getCanonicalType();
    Result = FunctionType::get(In, Out, FT->isAutoClosure(),
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
    ASTContext &C = CanProtos[0]->getASTContext();
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
  case TypeKind::BoundGeneric: {
    BoundGenericType *BGT = cast<BoundGenericType>(this);
    SmallVector<Type, 4> CanGenericArgs;
    for (Type Arg : BGT->getGenericArgs())
      CanGenericArgs.push_back(Arg->getCanonicalType());
    Result = BoundGenericType::get(BGT->getDecl(), CanGenericArgs);
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
  case TypeKind::BoundGeneric:
    // None of these types have sugar at the outer level.
    return this;
  case TypeKind::Paren:
    return cast<ParenType>(this)->getDesugaredType();
  case TypeKind::Identifier:
    return cast<IdentifierType>(this)->getDesugaredType();
  case TypeKind::NameAlias:
    return cast<NameAliasType>(this)->getDesugaredType();
  case TypeKind::ArraySlice:
    return cast<ArraySliceType>(this)->getDesugaredType();
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

TypeBase *IdentifierType::getDesugaredType() {
  return getMappedType()->getDesugaredType();
}

TypeBase *ArraySliceType::getDesugaredType() {
  return getImplementationType()->getDesugaredType();
}

TypeBase *SubstitutedType::getDesugaredType() {
  return getReplacementType()->getDesugaredType();
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

bool IdentifierType::isMapped() const {
  return !Components.back().Value.isNull();
}

Type IdentifierType::getMappedType() {
  assert(!Components.back().Value.isNull() &&
         "Name binding haven't resolved this to a type yet");
  return Components.back().Value.get<Type>();
}

TupleType::TupleType(ArrayRef<TupleTypeElt> fields, ASTContext *CanCtx)
  : TypeBase(TypeKind::Tuple, CanCtx, /*Unresolved=*/false), Fields(fields) 
{
  // Determine whether this tuple type is unresolved.
  for (const auto &F : Fields) {
    if (!F.getType().isNull() && F.getType()->isUnresolvedType()) {
      setUnresolved();
      break;
    }
  }
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
    
    // If we already saw a field missing a default value, then we cannot assign
    // a scalar to this tuple.
    if (FieldWithoutDefault != -1 && !Fields[i].isVararg())
      return -1;
    
    // Otherwise, remember this field number.
    FieldWithoutDefault = i;    
  }
  
  // If all the elements have default values, the scalar initializes the first
  // value in the tuple.
  return FieldWithoutDefault == -1 ? 0 : FieldWithoutDefault;
}


/// updateInitializedElementType - This methods updates the element type and
/// initializer for a non-canonical TupleType that has an initializer for the
/// specified element.  This should only be used by TypeChecker.
void TupleType::updateInitializedElementType(unsigned EltNo, Type NewTy) {
  TupleTypeElt &Elt = const_cast<TupleTypeElt&>(Fields[EltNo]);
  assert(Elt.hasInit() && "Can only update elements with default values");
  Elt = TupleTypeElt(NewTy, Elt.getName(), Elt.getInit());
}

ArchetypeType *ArchetypeType::getNew(ASTContext &Ctx, StringRef DisplayName,
                                     ArrayRef<Type> ConformsTo,
                                     Optional<unsigned> Index) {
  void *Mem = Ctx.Allocate(sizeof(ArchetypeType) + DisplayName.size(),
                           llvm::alignOf<ArchetypeType>());
  char *StoredStringData = (char*)Mem + sizeof(ArchetypeType);
  memcpy(StoredStringData, DisplayName.data(), DisplayName.size());

  // Gather the set of protocol declarations to which this archetype conforms.
  SmallVector<ProtocolDecl *, 4> ConformsToProtos;
  for (auto P : ConformsTo) {
    addProtocols(P, ConformsToProtos);
  }
  minimizeProtocols(ConformsToProtos);
  llvm::array_pod_sort(ConformsToProtos.begin(), ConformsToProtos.end(),
                       compareProtocols);

  return ::new (Mem) ArchetypeType(Ctx,
                                   StringRef(StoredStringData,
                                             DisplayName.size()),
                                   Ctx.AllocateCopy(ConformsToProtos),
                                   Index);
}

DeducibleGenericParamType *
DeducibleGenericParamType::getNew(ASTContext &Ctx, Identifier Name,
                                  unsigned Index,
                                  ArrayRef<ProtocolDecl *> ConformsTo) {
  return new (Ctx) DeducibleGenericParamType(Ctx, Name, Index, ConformsTo);
}

void ProtocolCompositionType::Profile(llvm::FoldingSetNodeID &ID,
                                      ArrayRef<Type> Protocols) {
  for (auto P : Protocols)
    ID.AddPointer(P.getPointer());
}

bool BoundGenericType::hasConformanceInformation() {
  return getCanonicalType()->castTo<BoundGenericType>()->AllConformances
           != nullptr;
}

ArrayRef<ProtocolConformance *>
BoundGenericType::getConformances(unsigned Index) {
  auto *Canon = getCanonicalType()->castTo<BoundGenericType>();
  if (Canon != this)
    return Canon->getConformances(Index);

  assert(AllConformances && "Protocol-conformance information not recorded!");
  unsigned Start = AllConformances->Offsets[Index];
  auto GP = TheDecl->getGenericParams()->getParams()[Index];
  auto Archetype
    = GP.getAsTypeParam()->getDeclaredType()->getAs<ArchetypeType>();
  unsigned Length = Archetype->getConformsTo().size();
  return ArrayRef<ProtocolConformance *>(
           AllConformances->Conformances.begin() + Start,
           AllConformances->Conformances.begin() + Start + Length);
}

void
BoundGenericType::setConformances(ArrayRef<unsigned> Offsets,
                                  ArrayRef<ProtocolConformance *> Conformances){
  auto *Canon = getCanonicalType()->castTo<BoundGenericType>();
  if (Canon != this)
    return Canon->setConformances(Offsets, Conformances);

  assert(!AllConformances && "Already have protocol-conformance information!");
  ASTContext &Ctx = getASTContext();
  void *Mem = Ctx.Allocate<AllConformancesType>(1);
  AllConformances = new (Mem) AllConformancesType;
  AllConformances->Offsets = Ctx.AllocateCopy(Offsets);
  AllConformances->Conformances = Ctx.AllocateCopy(Conformances);
}

Type
ProtocolCompositionType::get(ASTContext &C, ArrayRef<Type> ProtocolTypes) {
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

//===----------------------------------------------------------------------===//
//  Type Printing
//===----------------------------------------------------------------------===//

void Type::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void Type::print(raw_ostream &OS) const {
  if (isNull())
    OS << "<null>";
  else
    Ptr->print(OS);
}

/// getString - Return the name of the type as a string, for use in
/// diagnostics only.
std::string Type::getString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS);
  return OS.str();
}


/// getString - Return the name of the type as a string, for use in
/// diagnostics only.
std::string TypeBase::getString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS);
  return OS.str();
}


void TypeBase::dump() const {
  print(llvm::errs());
  if (isUnresolvedType())
    llvm::errs() << " [unresolved]";
  llvm::errs() << '\n';
}

void TypeBase::print(raw_ostream &OS) const {
  switch (getKind()) {
#define TYPE(id, parent) \
  case TypeKind::id:            return cast<id##Type>(this)->print(OS);
#include "swift/AST/TypeNodes.def"
  }
  llvm_unreachable("bad type kind!");
}

void BuiltinRawPointerType::print(raw_ostream &OS) const {
  OS << "Builtin.RawPointer";
}

void BuiltinObjectPointerType::print(raw_ostream &OS) const {
  OS << "Builtin.ObjectPointer";
}

void BuiltinObjCPointerType::print(raw_ostream &OS) const {
  OS << "Builtin.ObjCPointer";
}

void BuiltinIntegerType::print(raw_ostream &OS) const {
  OS << "Builtin.Int" << cast<BuiltinIntegerType>(this)->getBitWidth();
}

void BuiltinFloatType::print(raw_ostream &OS) const {
  switch (getFPKind()) {
  case IEEE16:  OS << "Builtin.FP_IEEE16"; return;
  case IEEE32:  OS << "Builtin.FP_IEEE32"; return;
  case IEEE64:  OS << "Builtin.FP_IEEE64"; return;
  case IEEE80:  OS << "Builtin.FP_IEEE80"; return;
  case IEEE128: OS << "Builtin.FP_IEEE128"; return;
  case PPC128:  OS << "Builtin.FP_PPC128"; return;
  }
}

void ErrorType::print(raw_ostream &OS) const {
  OS << "<<error type>>";
}

void UnstructuredUnresolvedType::print(raw_ostream &OS) const {
  OS << "<<unresolved type>>";
}

void ParenType::print(raw_ostream &OS) const {
  OS << '(';
  UnderlyingType->print(OS);
  OS << ')';
}

void NameAliasType::print(raw_ostream &OS) const {
  OS << TheDecl->getName().get();
}

static void printGenericArgs(raw_ostream &OS, ArrayRef<Type> Args) {
  if (Args.empty())
    return;

  OS << '<';
  bool First = true;
  for (Type Arg : Args) {
    if (First)
      First = false;
    else
      OS << ", ";
    Arg->print(OS);
  }
  OS << '>';
}

void IdentifierType::print(raw_ostream &OS) const {
  OS << Components[0].Id.get();
  printGenericArgs(OS, Components[0].GenericArgs);

  for (const Component &C : Components.slice(1, Components.size()-1)) {
    OS << '.' << C.Id.get();
    printGenericArgs(OS, C.GenericArgs);
  }
}

void MetaTypeType::print(raw_ostream &OS) const {
  OS << "metatype<";
  InstanceType->print(OS);
  OS << '>';
}

void ModuleType::print(raw_ostream &OS) const {
  OS << "module<" << TheModule->Name << '>';
}


void TupleType::print(raw_ostream &OS) const {
  OS << "(";
  
  for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
    if (i) OS << ", ";
    const TupleTypeElt &TD = Fields[i];
    
    if (TD.hasName())
      OS << TD.getName() << " : ";

    if (TD.isVararg())
      OS <<  TD.getVarargBaseTy() << "...";
    else
      OS << TD.getType();
  }
  OS << ')';
}

void FunctionType::print(raw_ostream &OS) const {
  if (isAutoClosure())
    OS << "[auto_closure]";
  OS << getInput() << " -> " << getResult();
}

void PolymorphicFunctionType::print(raw_ostream &OS) const {
  OS << '<';
  auto params = getGenericParams().getParams();
  for (unsigned i = 0, e = params.size(); i != e; ++i) {
    if (i) OS << ", ";

    TypeAliasDecl *paramTy = params[i].getAsTypeParam();
    OS << paramTy->getName().str();
    auto inherited = paramTy->getInherited();
    for (unsigned ii = 0, ie = inherited.size(); ii != ie; ++ii) {
      OS << (ii ? StringRef(", ") : " : ") << inherited[ii];
    }
  }
  OS << "> " << getInput() << " -> " << getResult();
}

void ArraySliceType::print(raw_ostream &OS) const {
  OS << Base << "[]";
}

void ArrayType::print(raw_ostream &OS) const {
  OS << Base << '[' << Size << ']';
}

void ProtocolType::print(raw_ostream &OS) const {
  OS << getDecl()->getName().str();
}

void ProtocolCompositionType::print(raw_ostream &OS) const {
  OS << "protocol<";
  bool First = true;
  for (auto Proto : Protocols) {
    if (First)
      First = false;
    else
      OS << ", ";
    Proto->print(OS);
  }
  OS << ">";
}

void LValueType::print(raw_ostream &OS) const {
  OS << "[byref";

  Qual qs = getQualifiers();
  if (qs != Qual::DefaultForType) {
    bool hasQual = false;
#define APPEND_QUAL(cond, text) do { \
      if (cond) {                    \
        if (hasQual) OS << ", ";     \
        hasQual = true;              \
        OS << text;                  \
      }                              \
    } while(false)

    OS << '(';
    APPEND_QUAL(!(qs & Qual::NonHeap), "heap");
    OS << ')';

#undef APPEND_QUAL
  }
  OS << "] ";
  getObjectType()->print(OS);
}

void UnboundGenericType::print(raw_ostream &OS) const {
  OS << getDecl()->getName().get();
}

void BoundGenericType::print(raw_ostream &OS) const {
  OS << getDecl()->getName().get();
  printGenericArgs(OS, getGenericArgs());
}

void StructType::print(raw_ostream &OS) const {
  OS << getDecl()->getName().get();
}

void ClassType::print(raw_ostream &OS) const {
  OS << getDecl()->getName().get();
}

void OneOfType::print(raw_ostream &OS) const {
  OS << getDecl()->getName().get();
}

void ArchetypeType::print(raw_ostream &OS) const {
  OS << DisplayName;
}

void DeducibleGenericParamType::print(raw_ostream &OS) const {
  OS << getName().str();
}

void SubstitutedType::print(raw_ostream &OS) const {
  getReplacementType()->print(OS);
}

//===----------------------------------------------------------------------===//
//  TypeLoc implementation
//===----------------------------------------------------------------------===//

TypeLoc::TypeLoc(SourceRange Range) : Range(Range) {}

void *TypeLoc::operator new(size_t Bytes, ASTContext &C,
                            unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

TypeLoc *TypeLoc::get(ASTContext &Context, SourceRange Range) {
  return new (Context) TypeLoc(Range);
}
