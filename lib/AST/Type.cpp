//===--- Type.cpp - Swift Language Type ASTs ------------------------------===//
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
//  This file implements the Type class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "ForeignRepresentationInfo.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/AST.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/Fallthrough.h"
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

NominalTypeDecl *CanType::getAnyNominal() const {
  return dyn_cast_or_null<NominalTypeDecl>(getAnyGeneric());
}

GenericTypeDecl *CanType::getAnyGeneric() const {
  if (auto nominalTy = dyn_cast<NominalType>(*this))
    return (GenericTypeDecl*)nominalTy->getDecl();
  
  if (auto boundTy = dyn_cast<BoundGenericType>(*this))
    return (GenericTypeDecl*)boundTy->getDecl();
  
  if (auto unboundTy = dyn_cast<UnboundGenericType>(*this))
    return unboundTy->getDecl();
  return nullptr;
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

bool TypeBase::isNever() {
  if (auto nominalDecl = getAnyNominal())
    if (auto enumDecl = dyn_cast<EnumDecl>(nominalDecl))
      if (enumDecl->getAllElements().empty())
        return true;

  return false;
}

bool TypeBase::isAny() {
  return isEqual(getASTContext().TheAnyType);
}

bool TypeBase::isAnyClassReferenceType() {
  return getCanonicalType().isAnyClassReferenceType();
}

bool CanType::isReferenceTypeImpl(CanType type, bool functionsCount) {
  switch (type->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("sugared canonical type?");

  // These types are always class references.
  case TypeKind::BuiltinUnknownObject:
  case TypeKind::BuiltinNativeObject:
  case TypeKind::BuiltinBridgeObject:
  case TypeKind::Class:
  case TypeKind::BoundGenericClass:
  case TypeKind::SILBox:
    return true;

  // For Self types, recur on the underlying type.
  case TypeKind::DynamicSelf:
    return isReferenceTypeImpl(cast<DynamicSelfType>(type).getSelfType(),
                               functionsCount);

  // Archetypes and existentials are only class references if class-bounded.
  case TypeKind::Archetype:
    return cast<ArchetypeType>(type)->requiresClass();
  case TypeKind::Protocol:
    return cast<ProtocolType>(type)->requiresClass();
  case TypeKind::ProtocolComposition:
    return cast<ProtocolCompositionType>(type)->requiresClass();

  case TypeKind::UnboundGeneric:
    return isa<ClassDecl>(cast<UnboundGenericType>(type)->getDecl());

  // Functions have reference semantics, but are not class references.
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
  case TypeKind::GenericFunction:
  case TypeKind::SILFunction:
    return functionsCount;

  // Nothing else is statically just a class reference.
  case TypeKind::SILBlockStorage:
  case TypeKind::Error:
  case TypeKind::Unresolved:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinUnsafeValueBuffer:
  case TypeKind::BuiltinVector:
  case TypeKind::Tuple:
  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Metatype:
  case TypeKind::ExistentialMetatype:
  case TypeKind::Module:
  case TypeKind::LValue:
  case TypeKind::InOut:
  case TypeKind::TypeVariable:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage:
    return false;

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
  return getCanonicalType().isAnyClassReferenceType();
}

bool TypeBase::isAnyExistentialType(SmallVectorImpl<ProtocolDecl*> &protocols) {
  return getCanonicalType().isAnyExistentialType(protocols);
}

bool CanType::isAnyExistentialTypeImpl(CanType type,
                                       SmallVectorImpl<ProtocolDecl*> &protocols) {
  if (auto metatype = dyn_cast<ExistentialMetatypeType>(type)) {
    metatype.getInstanceType().getAnyExistentialTypeProtocols(protocols);
    return true;
  }
  return isExistentialTypeImpl(type, protocols);
}

bool TypeBase::isExistentialType(SmallVectorImpl<ProtocolDecl *> &protocols) {
  return getCanonicalType().isExistentialType(protocols);
}

bool CanType::isExistentialTypeImpl(CanType type,
                                    SmallVectorImpl<ProtocolDecl*> &protocols) {
  if (auto proto = dyn_cast<ProtocolType>(type)) {
    proto.getAnyExistentialTypeProtocols(protocols);
    return true;
  }
  
  if (auto comp = dyn_cast<ProtocolCompositionType>(type)) {
    comp.getAnyExistentialTypeProtocols(protocols);
    return true;
  }

  assert(!type.isExistentialType());
  return false;
}

void TypeBase::getAnyExistentialTypeProtocols(
                                   SmallVectorImpl<ProtocolDecl*> &protocols) {
  getCanonicalType().getAnyExistentialTypeProtocols(protocols);
}

void CanType::getAnyExistentialTypeProtocolsImpl(CanType type,
                                   SmallVectorImpl<ProtocolDecl*> &protocols) {
  if (auto proto = dyn_cast<ProtocolType>(type)) {
    proto.getAnyExistentialTypeProtocols(protocols);
  } else if (auto comp = dyn_cast<ProtocolCompositionType>(type)) {
    comp.getAnyExistentialTypeProtocols(protocols);
  } else if (auto metatype = dyn_cast<ExistentialMetatypeType>(type)) {
    metatype.getAnyExistentialTypeProtocols(protocols);
  } else {
    llvm_unreachable("type was not any kind of existential type!");
  }
}

bool TypeBase::isObjCExistentialType() {
  return getCanonicalType().isObjCExistentialType();
}

bool CanType::isObjCExistentialTypeImpl(CanType type) {
  if (!type.isExistentialType()) return false;

  SmallVector<ProtocolDecl *, 4> protocols;
  type.getAnyExistentialTypeProtocols(protocols);

  // Must have at least one protocol to be class-bounded.
  if (protocols.empty())
    return false;

  // Any non-AnyObject, non-@objc protocol makes this no longer ObjC-compatible.
  for (auto proto : protocols) {
    if (proto->isSpecificProtocol(KnownProtocolKind::AnyObject))
      continue;
    if (proto->isObjC())
      continue;
    
    return false;
  }
  return true;
}

bool TypeBase::isSpecialized() {
  CanType CT = getCanonicalType();
  if (CT.getPointer() != this)
    return CT->isSpecialized();

  return CT.findIf([](Type type) -> bool {
    return isa<BoundGenericType>(type.getPointer());
  });
}

ArrayRef<Type> TypeBase::getAllGenericArgs(SmallVectorImpl<Type> &scratch) {
  Type type(this);
  SmallVector<ArrayRef<Type>, 2> allGenericArgs;

  while (type) {
    // Gather generic arguments from a bound generic type.
    if (auto bound = type->getAs<BoundGenericType>()) {
      allGenericArgs.push_back(bound->getGenericArgs());

      // Continue up to the parent.
      type = bound->getParent();
      continue;
    }

    // Use the generic type parameter types for an unbound generic type.
    if (auto unbound = type->getAs<UnboundGenericType>()) {
      auto genericSig = unbound->getDecl()->getGenericSignature();
      auto genericParams = genericSig->getInnermostGenericParams();
      allGenericArgs.push_back(
        llvm::makeArrayRef((const Type *)genericParams.data(),
                           genericParams.size()));

      // Continue up to the parent.
      type = unbound->getParent();
      continue;
    }

    // For a protocol type, use its Self parameter.
    if (auto protoType = type->getAs<ProtocolType>()) {
      auto proto = protoType->getDecl();
      allGenericArgs.push_back(
        llvm::makeArrayRef(proto->getSelfInterfaceType()));

      // Continue up to the parent.
      type = protoType->getParent();
      continue;
    }

    // Look through non-generic nominal types.
    if (auto nominal = type->getAs<NominalType>()) {
      type = nominal->getParent();
      continue;
    }

    break;
  }

  // Trivial case: no generic arguments.
  if (allGenericArgs.empty())
    return { };

  // Common case: a single set of generic arguments, for which we need no
  // allocation.
  if (allGenericArgs.size() == 1)
    return allGenericArgs.front();

  // General case: concatenate all of the generic argument lists together.
  scratch.clear();
  for (auto args : reversed(allGenericArgs))
    scratch.append(args.begin(), args.end());
  return scratch;
}

bool TypeBase::isUnspecializedGeneric() {
  CanType CT = getCanonicalType();
  if (CT.getPointer() != this)
    return CT->isUnspecializedGeneric();

  switch (getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("we're only working with CanType's here");

  case TypeKind::Error:
  case TypeKind::Unresolved:
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

  case TypeKind::ExistentialMetatype:
  case TypeKind::Metatype:
    return cast<AnyMetatypeType>(this)->getInstanceType()
             ->isUnspecializedGeneric();

  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage:
    return cast<ReferenceStorageType>(this)->getReferentType()
             ->isUnspecializedGeneric();

  case TypeKind::LValue:
    return cast<LValueType>(this)->getObjectType()->isUnspecializedGeneric();
  case TypeKind::InOut:
    return cast<InOutType>(this)->getObjectType()->isUnspecializedGeneric();

  case TypeKind::Tuple: {
    auto tupleTy = cast<TupleType>(this);
    for (auto &Elt : tupleTy->getElements())
      if (Elt.getType()->isUnspecializedGeneric())
        return true;

    return false;
  }

  case TypeKind::Archetype:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinUnknownObject:
  case TypeKind::BuiltinNativeObject:
  case TypeKind::BuiltinBridgeObject:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinUnsafeValueBuffer:
  case TypeKind::BuiltinVector:
  case TypeKind::Module:
  case TypeKind::DynamicSelf:
  case TypeKind::Protocol:
  case TypeKind::ProtocolComposition:
  case TypeKind::SILFunction:
    return false;

  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    return false;
      
  case TypeKind::SILBlockStorage:
    return cast<SILBlockStorageType>(this)->getCaptureType()
        ->isUnspecializedGeneric();
  case TypeKind::SILBox:
    return cast<SILBoxType>(this)->getBoxedType()
        ->isUnspecializedGeneric();
  }
  llvm_unreachable("bad TypeKind");
}

bool TypeBase::hasOpenedExistential(ArchetypeType *opened) {
  assert(opened->getOpenedExistentialType() &&
         "not an opened existential type");

  if (!hasOpenedExistential())
    return false;

  return getCanonicalType().findIf([&](Type type) -> bool {
    return opened == dyn_cast<ArchetypeType>(type.getPointer());
  });
}

void TypeBase::getOpenedExistentials(
       SmallVectorImpl<ArchetypeType *> &opened) {
  if (!hasOpenedExistential())
    return;

  SmallPtrSet<ArchetypeType *, 4> known;
  getCanonicalType().findIf([&](Type type) -> bool {
    auto archetype = dyn_cast<ArchetypeType>(type.getPointer());
    if (!archetype)
      return false;

    if (!archetype->getOpenedExistentialType())
      return false;

    if (known.insert(archetype).second)
      opened.push_back(archetype);

    return false;
  });
}

Type TypeBase::eraseOpenedExistential(Module *module,
                                      ArchetypeType *opened) {
  assert(opened->getOpenedExistentialType() &&
         "Not an opened existential type?");

  if (!hasOpenedExistential())
    return Type(this);

  TypeSubstitutionMap substitutions;
  substitutions[opened] = opened->getOpenedExistentialType();
  return Type(this).subst(module, substitutions, None);
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
  if (!type->isMaterializable()) return false;
  if (isa<AnyFunctionType>(type)) return false;
  if (auto meta = dyn_cast<AnyMetatypeType>(type))
    return meta->hasRepresentation();
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
  if (auto TT = getAs<TupleType>())
    return TT->getNumElements() == 0;
  return false;
}

/// \brief Check if this type is equal to Swift.Bool.
bool TypeBase::isBool() {
  if (auto NTD = getAnyNominal())
    if (isa<StructDecl>(NTD))
      return getASTContext().getBoolDecl() == NTD;
  return false;
}


bool TypeBase::isAssignableType() {
  if (isLValueType()) return true;
  if (auto tuple = getAs<TupleType>()) {
    for (auto eltType : tuple->getElementTypes()) {
      if (!eltType->isAssignableType())
        return false;
    }
    return true;
  }
  return false;
}

namespace {
class GetRValueTypeVisitor : public TypeVisitor<GetRValueTypeVisitor, Type> {
public:
  Type visitLValueType(LValueType *lvt) {
    // Look through lvalue types.
    assert(!lvt->getObjectType()->isLValueType()
           && "unexpected nested lvalue");
    return lvt->getObjectType();
  }
  
  Type visitTupleType(TupleType *tt) {
    // Look through lvalues in tuples.
    SmallVector<TupleTypeElt, 4> elts;
    for (auto &elt : tt->getElements()) {
      elts.push_back(elt.getWithType(visit(elt.getType())));
    }
    return TupleType::get(elts, tt->getASTContext());
  }
  
  Type visitParenType(ParenType *pt) {
    return ParenType::get(pt->getASTContext(), visit(pt->getUnderlyingType()));
  }
  
  Type visitType(TypeBase *t) {
    // Other types should not structurally contain lvalues.
    assert(!t->isLValueType()
           && "unexpected structural lvalue");
    return t;
  }
};
} // end anonymous namespace

Type TypeBase::getRValueType() {
  // If the type is not an lvalue, this is a no-op.
  if (!isLValueType())
    return this;
  
  return GetRValueTypeVisitor().visit(this);
}

Type TypeBase::getOptionalObjectType() {
  if (auto boundTy = getAs<BoundGenericEnumType>())
    if (boundTy->getDecl()->classifyAsOptionalType() == OTK_Optional)
      return boundTy->getGenericArgs()[0];
  return Type();
}

Type TypeBase::getImplicitlyUnwrappedOptionalObjectType() {
  if (auto boundTy = getAs<BoundGenericEnumType>())
    if (boundTy->getDecl()->classifyAsOptionalType() == OTK_ImplicitlyUnwrappedOptional)
      return boundTy->getGenericArgs()[0];
  return Type();
}

Type TypeBase::getAnyOptionalObjectType(OptionalTypeKind &kind) {
  if (auto boundTy = getAs<BoundGenericEnumType>())
    if ((kind = boundTy->getDecl()->classifyAsOptionalType()))
      return boundTy->getGenericArgs()[0];
  kind = OTK_None;
  return Type();
}

CanType CanType::getAnyOptionalObjectTypeImpl(CanType type,
                                              OptionalTypeKind &kind) {
  if (auto boundTy = dyn_cast<BoundGenericEnumType>(type))
    if ((kind = boundTy->getDecl()->classifyAsOptionalType()))
      return boundTy.getGenericArgs()[0];
  kind = OTK_None;
  return CanType();
}

Type TypeBase::getAnyPointerElementType(PointerTypeKind &PTK) {
  auto &C = getASTContext();
  if (auto nominalTy = getAs<NominalType>()) {
    if (nominalTy->getDecl() == C.getUnsafeMutableRawPointerDecl()) {
      PTK = PTK_UnsafeMutableRawPointer;
      return C.TheEmptyTupleType;
    }
    if (nominalTy->getDecl() == C.getUnsafeRawPointerDecl()) {
      PTK = PTK_UnsafeRawPointer;
      return C.TheEmptyTupleType;
    }
  }
  if (auto boundTy = getAs<BoundGenericType>()) {
    if (boundTy->getDecl() == C.getUnsafeMutablePointerDecl()) {
      PTK = PTK_UnsafeMutablePointer;
    } else if (boundTy->getDecl() == C.getUnsafePointerDecl()) {
      PTK = PTK_UnsafePointer;
    } else if (
      boundTy->getDecl() == C.getAutoreleasingUnsafeMutablePointerDecl()
    ) {
      PTK = PTK_AutoreleasingUnsafeMutablePointer;
    } else {
      return Type();
    }
    return boundTy->getGenericArgs()[0];
  }
  return Type();
}

Type TypeBase::lookThroughAllAnyOptionalTypes() {
  Type type(this);
  while (auto objType = type->getAnyOptionalObjectType())
    type = objType;

  return type;
}

Type TypeBase::lookThroughAllAnyOptionalTypes(SmallVectorImpl<Type> &optionals){
  Type type(this);
  while (auto objType = type->getAnyOptionalObjectType()) {
    optionals.push_back(type);
    type = objType;
  }

  return type;
}

ClassDecl *CanType::getClassBoundImpl(CanType type) {
  if (auto classTy = dyn_cast<ClassType>(type))
    return classTy->getDecl();

  if (auto boundTy = dyn_cast<BoundGenericClassType>(type))
    return boundTy->getDecl();

  if (auto archetypeTy = dyn_cast<ArchetypeType>(type)) {
    assert(archetypeTy->requiresClass());
    if (Type supertype = archetypeTy->getSuperclass()) {
      return supertype->getClassOrBoundGenericClass();
    }
    return nullptr;
  }

  llvm_unreachable("class has no class bound!");
}

bool TypeBase::isAnyObject() {
  if (auto proto = getAs<ProtocolType>())
    return proto->getDecl()->isSpecificProtocol(KnownProtocolKind::AnyObject);

  return false;
}

bool TypeBase::isEmptyExistentialComposition() {
  if (auto emtType = ExistentialMetatypeType::get(this)) {
    if (auto pcType = emtType->getInstanceType()->
        getAs<ProtocolCompositionType>()) {
      return pcType->getProtocols().empty();
    }
  }
  
  return false;
}

bool TypeBase::isExistentialWithError() {
  // FIXME: Compute this as a bit in TypeBase so this operation isn't
  // overly expensive.
  SmallVector<ProtocolDecl *, 4> protocols;
  if (!getCanonicalType()->isExistentialType(protocols)) return false;

  auto errorProto =
    getASTContext().getProtocol(KnownProtocolKind::Error);
  if (!errorProto) return false;

  for (auto proto : protocols) {
    if (proto == errorProto || proto->inheritsFrom(errorProto))
      return true;
  }

  return false;
}


static Type getStrippedType(const ASTContext &context, Type type,
                            bool stripLabels) {
  return type.transform([&](Type type) -> Type {
    auto *tuple = dyn_cast<TupleType>(type.getPointer());
    if (!tuple)
      return type;

    SmallVector<TupleTypeElt, 4> elements;
    bool anyChanged = false;
    unsigned idx = 0;
    for (const auto &elt : tuple->getElements()) {
      Type eltTy = getStrippedType(context, elt.getType(),
                                   stripLabels);
      if (anyChanged || eltTy.getPointer() != elt.getType().getPointer() ||
          (elt.hasName() && stripLabels)) {
        if (!anyChanged) {
          elements.reserve(tuple->getNumElements());
          for (unsigned i = 0; i != idx; ++i) {
            const TupleTypeElt &elt = tuple->getElement(i);
            Identifier newName = stripLabels? Identifier() : elt.getName();
            elements.push_back(TupleTypeElt(elt.getType(), newName,
                                            elt.isVararg()));
          }
          anyChanged = true;
        }

        Identifier newName = stripLabels? Identifier() : elt.getName();
        elements.push_back(TupleTypeElt(eltTy, newName, elt.isVararg()));
      }
      ++idx;
    }
    
    if (!anyChanged)
      return type;
    
    // An unlabeled 1-element tuple type is represented as a parenthesized
    // type.
    if (elements.size() == 1 && !elements[0].isVararg() && 
        !elements[0].hasName())
      return ParenType::get(context, elements[0].getType());
    
    return TupleType::get(elements, context);
  });
}

Type TypeBase::getUnlabeledType(ASTContext &Context) {
  return getStrippedType(Context, Type(this), /*labels=*/true);
}

Type TypeBase::getWithoutParens() {
  Type Ty = this;
  while (auto ParenTy = dyn_cast<ParenType>(Ty.getPointer()))
    Ty = ParenTy->getUnderlyingType();
  return Ty;
}

Type TypeBase::replaceCovariantResultType(Type newResultType,
                                          unsigned uncurryLevel,
                                          bool preserveOptionality) {
  if (uncurryLevel == 0) {
    if (preserveOptionality) {
      OptionalTypeKind resultOTK;
      if (auto objectType = getAnyOptionalObjectType(resultOTK)) {
        assert(!newResultType->getAnyOptionalObjectType());
        return OptionalType::get(
            resultOTK,
            objectType->replaceCovariantResultType(
                newResultType, uncurryLevel, preserveOptionality));
      }
    }

    return newResultType;
  }

  // Determine the input and result types of this function.
  auto fnType = this->castTo<AnyFunctionType>();
  Type inputType = fnType->getInput();
  Type resultType =
    fnType->getResult()->replaceCovariantResultType(newResultType,
                                                    uncurryLevel - 1,
                                                    preserveOptionality);
  
  // Produce the resulting function type.
  if (auto genericFn = dyn_cast<GenericFunctionType>(fnType)) {
    return GenericFunctionType::get(genericFn->getGenericSignature(),
                                    inputType, resultType,
                                    fnType->getExtInfo());
  }
  
  if (auto polyFn = dyn_cast<PolymorphicFunctionType>(fnType)) {
    return PolymorphicFunctionType::get(inputType, resultType,
                                        &polyFn->getGenericParams(),
                                        fnType->getExtInfo());
  }
  
  return FunctionType::get(inputType, resultType, fnType->getExtInfo());
}

SmallVector<CallArgParam, 4>
swift::decomposeArgType(Type type, ArrayRef<Identifier> argumentLabels) {
  SmallVector<CallArgParam, 4> result;
  switch (type->getKind()) {
  case TypeKind::Tuple: {
    auto tupleTy = cast<TupleType>(type.getPointer());

    // If we have one argument label but a tuple argument with != 1 element,
    // put the whole tuple into the argument.
    // FIXME: This horribleness is due to the mis-modeling of arguments as
    // ParenType or TupleType.
    if (argumentLabels.size() == 1 && tupleTy->getNumElements() != 1) {
      // Break out to do the default thing below.
      break;
    }

    for (auto i : range(0, tupleTy->getNumElements())) {
      const auto &elt = tupleTy->getElement(i);
      assert(!elt.isVararg() && "Vararg argument tuple doesn't make sense");
      CallArgParam argParam;
      argParam.Ty = elt.getType();
      argParam.Label = argumentLabels[i];
      result.push_back(argParam);
    }
    return result;
  }

  case TypeKind::Paren: {
    CallArgParam argParam;
    argParam.Ty = cast<ParenType>(type.getPointer())->getUnderlyingType();
    result.push_back(argParam);
    return result;
  }

  default:
    // Default behavior below; inject the argument as the sole parameter.
    break;
  }

  // Just inject this parameter.
  assert(result.empty());
  CallArgParam argParam;
  argParam.Ty = type;
  assert(argumentLabels.size() == 1);
  argParam.Label = argumentLabels[0];
  result.push_back(argParam);
  return result;
}

SmallVector<CallArgParam, 4>
swift::decomposeParamType(Type type, const ValueDecl *paramOwner,
                          unsigned level) {
  // Find the corresponding parameter list.
  const ParameterList *paramList = nullptr;
  if (paramOwner) {
    if (auto func = dyn_cast<AbstractFunctionDecl>(paramOwner)) {
      if (level < func->getNumParameterLists())
        paramList = func->getParameterList(level);
    } else if (auto subscript = dyn_cast<SubscriptDecl>(paramOwner)) {
      if (level == 1)
        paramList = subscript->getIndices();
    }
  }

  SmallVector<CallArgParam, 4> result;
  switch (type->getKind()) {
  case TypeKind::Tuple: {
    auto tupleTy = cast<TupleType>(type.getPointer());

    // FIXME: In the weird case where we have a tuple type that should
    // be wrapped in a ParenType but isn't, just... forget it happened.
    if (paramList && tupleTy->getNumElements() != paramList->size() &&
        paramList->size() == 1)
      paramList = nullptr;

    for (auto i : range(0, tupleTy->getNumElements())) {
      const auto &elt = tupleTy->getElement(i);

      CallArgParam argParam;
      argParam.Ty = elt.isVararg() ? elt.getVarargBaseTy() : elt.getType();
      argParam.Label = elt.getName();
      argParam.HasDefaultArgument =
          paramList && paramList->get(i)->isDefaultArgument();
      argParam.Variadic = elt.isVararg();
      result.push_back(argParam);
    }
    break;
  }

  case TypeKind::Paren: {
    CallArgParam argParam;
    argParam.Ty = cast<ParenType>(type.getPointer())->getUnderlyingType();
    argParam.HasDefaultArgument =
        paramList && paramList->get(0)->isDefaultArgument();
    result.push_back(argParam);
    break;
  }

  default: {
    CallArgParam argParam;
    argParam.Ty = type;
    result.push_back(argParam);
    break;
  }
  }

  return result;
}

/// Turn a param list into a symbolic and printable representation that does not
/// include the types, something like (_:, b:, c:)
std::string swift::getParamListAsString(ArrayRef<CallArgParam> params) {
  std::string result = "(";

  bool isFirst = true;
  for (auto &param : params) {
    if (isFirst)
      isFirst = false;
    else
      result += ", ";

    if (param.hasLabel())
      result += param.Label.str();
    else
      result += "_";
    result += ":";
  }

  result += ')';
  return result;
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

/// Returns a new function type exactly like this one but with the self
/// parameter replaced. Only makes sense for members of types.
Type TypeBase::replaceSelfParameterType(Type newSelf) {
  auto fnTy = castTo<AnyFunctionType>();
  Type input = rebuildSelfTypeWithObjectType(fnTy->getInput(), newSelf);

  if (auto genericFnTy = getAs<GenericFunctionType>()) {
    return GenericFunctionType::get(genericFnTy->getGenericSignature(),
                                    input,
                                    fnTy->getResult(),
                                    fnTy->getExtInfo());
  }

  if (auto polyFnTy = getAs<PolymorphicFunctionType>()) {
    return PolymorphicFunctionType::get(input,
                                        fnTy->getResult(),
                                        &polyFnTy->getGenericParams());
  }

  return FunctionType::get(input,
                           fnTy->getResult(),
                           fnTy->getExtInfo());
}

/// Retrieve the object type for a 'self' parameter, digging into one-element
/// tuples, inout types, and metatypes.
Type TypeBase::getRValueInstanceType() {
  Type type = this;
  
  // Look through argument list tuples.
  if (auto tupleTy = type->getAs<TupleType>()) {
    if (tupleTy->getNumElements() == 1 && !tupleTy->getElement(0).isVararg())
      type = tupleTy->getElementType(0);
  }
  
  if (auto metaTy = type->getAs<AnyMetatypeType>())
    return metaTy->getInstanceType();

  // For mutable value type methods, we need to dig through inout types.
  return type->getInOutObjectType();
}

TypeDecl *TypeBase::getDirectlyReferencedTypeDecl() const {
  if (auto module = dyn_cast<ModuleType>(this))
    return module->getModule();

  if (auto nominal = dyn_cast<NominalType>(this))
    return nominal->getDecl();

  if (auto bound = dyn_cast<BoundGenericType>(this))
    return bound->getDecl();

  if (auto unbound = dyn_cast<UnboundGenericType>(this))
    return unbound->getDecl();

  if (auto alias = dyn_cast<NameAliasType>(this))
    return alias->getDecl();

  if (auto gp = dyn_cast<GenericTypeParamType>(this))
    return gp->getDecl();

  if (auto depMem = dyn_cast<DependentMemberType>(this))
    return depMem->getAssocType();

  if (auto archetype = dyn_cast<ArchetypeType>(this)) {
    if (auto proto = archetype->getSelfProtocol())
      return proto->getProtocolSelf();

    if (auto assoc = archetype->getAssocType())
      return assoc;

    return nullptr;
  }

  return nullptr;
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

    if (Visited.insert(Proto->getDecl()).second) {
      Stack.push_back(Proto->getDecl());
      for (auto Inherited : Proto->getDecl()->getInheritedProtocols(nullptr))
        addMinimumProtocols(Inherited->getDeclaredType(), Protocols, Known,
                            Visited, Stack, ZappedAny);
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
int ProtocolType::compareProtocols(ProtocolDecl * const* PP1,
                                   ProtocolDecl * const* PP2) {
  auto *P1 = *PP1;
  auto *P2 = *PP2;
  Module *M1 = P1->getParentModule();
  Module *M2 = P2->getParentModule();

  // Try ordering based on module name, first.
  if (int result = M1->getName().str().compare(M2->getName().str()))
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
    
    // If we have not seen this protocol before, record its index.
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
    for (auto Inherited : Current->getInheritedProtocols(nullptr)) {
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

/// getCanonicalType - Return the canonical version of this type, which has
/// sugar from all levels stripped off.
CanType TypeBase::getCanonicalType() {
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
  case TypeKind::Unresolved:
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
    assert(TT->getNumElements() != 0 && "Empty tuples are always canonical");

    SmallVector<TupleTypeElt, 8> CanElts;
    CanElts.reserve(TT->getNumElements());
    for (const TupleTypeElt &field : TT->getElements()) {
      assert(!field.getType().isNull() &&
             "Cannot get canonical type of un-typechecked TupleType!");
      CanElts.push_back(TupleTypeElt(field.getType()->getCanonicalType(),
                                     field.getName(),
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

  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage: {
    auto ref = cast<ReferenceStorageType>(this);
    Type referentType = ref->getReferentType()->getCanonicalType();
    Result = ReferenceStorageType::get(referentType, ref->getOwnership(),
                                       referentType->getASTContext());
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
    
    // Transform the input and result types.
    auto &ctx = function->getInput()->getASTContext();
    auto &mod = *ctx.TheBuiltinModule;
    auto inputTy = sig->getCanonicalTypeInContext(function->getInput(), mod);
    auto resultTy = sig->getCanonicalTypeInContext(function->getResult(), mod);

    Result = GenericFunctionType::get(sig, inputTy, resultTy,
                                      function->getExtInfo());
    assert(Result->isCanonical());
    break;
  }
      
  case TypeKind::SILBlockStorage:
  case TypeKind::SILBox:
  case TypeKind::SILFunction:
    llvm_unreachable("SIL-only types are always canonical!");

  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(this);
    Type In = FT->getInput()->getCanonicalType();
    Type Out = FT->getResult()->getCanonicalType();
    Result = FunctionType::get(In, Out, FT->getExtInfo());
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
  case TypeKind::ExistentialMetatype: {
    auto metatype = cast<ExistentialMetatypeType>(this);
    auto instanceType = metatype->getInstanceType()->getCanonicalType();
    if (metatype->hasRepresentation())
      Result = ExistentialMetatypeType::get(instanceType,
                                            metatype->getRepresentation());
    else
      Result = ExistentialMetatypeType::get(instanceType);
    break;
  }
  case TypeKind::Metatype: {
    MetatypeType *MT = cast<MetatypeType>(this);
    Type InstanceTy = MT->getInstanceType()->getCanonicalType();
    if (MT->hasRepresentation())
      Result = MetatypeType::get(InstanceTy, MT->getRepresentation());
    else
      Result = MetatypeType::get(InstanceTy);
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
  case TypeKind::SILBlockStorage:
  case TypeKind::SILBox:
  case TypeKind::SILFunction:
  case TypeKind::LValue:
  case TypeKind::InOut:
  case TypeKind::ProtocolComposition:
  case TypeKind::ExistentialMetatype:
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
  case TypeKind::UnmanagedStorage:
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
  auto *TAD = getDecl();

  // The type for a generic TypeAliasDecl is an UnboundGenericType.
  if (TAD->getGenericParams())
    return UnboundGenericType::get(TAD,
                           TAD->getDeclContext()->getDeclaredTypeInContext(),
                                   TAD->getASTContext());

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
    implDecl = ctx.getArrayDecl();
    assert(implDecl && "Array type has not been set yet");
  } else if (isa<OptionalType>(this)) {
    implDecl = ctx.getOptionalDecl();
    assert(implDecl && "Optional type has not been set yet");
  } else if (isa<ImplicitlyUnwrappedOptionalType>(this)) {
    implDecl = ctx.getImplicitlyUnwrappedOptionalDecl();
    assert(implDecl && "Optional type has not been set yet");
  } else {
    llvm_unreachable("Unhandled syntax sugar type");
  }

  // Record the implementation type.
  ImplOrContext = BoundGenericType::get(implDecl, Type(), Base);
  return ImplOrContext.get<Type>();
}

TypeBase *DictionaryType::getSinglyDesugaredType() {
  return getImplementationType().getPointer();
}

Type DictionaryType::getImplementationType() {
  if (ImplOrContext.is<Type>())
    return ImplOrContext.get<Type>();

  // Find the generic type that implements this syntactic sugar type.
  auto &ctx = *ImplOrContext.get<const ASTContext *>();
  NominalTypeDecl *implDecl = ctx.getDictionaryDecl();
  assert(implDecl && "Dictionary type has not been set yet");

  // Record the implementation type.
  ImplOrContext = BoundGenericType::get(implDecl, Type(), { Key, Value });
  return ImplOrContext.get<Type>();
}

TypeBase *SubstitutedType::getSinglyDesugaredType() {
  return getReplacementType().getPointer();
}

unsigned GenericTypeParamType::getDepth() const {
  if (auto param = getDecl()) {
    return param->getDepth();
  }

  auto fixedNum = ParamOrDepthIndex.get<DepthIndexTy>();
  return fixedNum >> 16;
}

unsigned GenericTypeParamType::getIndex() const {
  if (auto param = getDecl()) {
    return param->getIndex();
  }

  auto fixedNum = ParamOrDepthIndex.get<DepthIndexTy>();
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
  unsigned depthIndex = ParamOrDepthIndex.get<DepthIndexTy>();
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
  llvm::report_fatal_error("Unknown FP semantics");
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
    if (tMe->getNumElements() != tThem->getNumElements())
      return false;
    for (size_t i = 0, sz = tMe->getNumElements(); i < sz; ++i) {
      auto &myField = tMe->getElement(i), &theirField = tThem->getElement(i);
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
  case TypeKind::SILBlockStorage:
  case TypeKind::SILBox:
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
    if (fMe->getRepresentation() != fThem->getRepresentation())
      return false;
    if (!fMe->getInput()->isSpelledLike(fThem->getInput()))
      return false;
    if (!fMe->getResult()->isSpelledLike(fThem->getResult()))
      return false;
    return true;
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
  case TypeKind::ExistentialMetatype: {
    auto mMe = cast<ExistentialMetatypeType>(me);
    auto mThem = cast<ExistentialMetatypeType>(them);
    return mMe->getInstanceType()->isSpelledLike(mThem->getInstanceType());
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
  case TypeKind::ImplicitlyUnwrappedOptional: {
    auto aMe = cast<SyntaxSugarType>(me);
    auto aThem = cast<SyntaxSugarType>(them);
    return aMe->getBaseType()->isSpelledLike(aThem->getBaseType());
  }
  case TypeKind::Dictionary: {
    auto aMe = cast<DictionaryType>(me);
    auto aThem = cast<DictionaryType>(them);
    return aMe->getKeyType()->isSpelledLike(aThem->getKeyType()) &&
           aMe->getValueType()->isSpelledLike(aThem->getValueType());
  }
  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage: {
    auto rMe = cast<ReferenceStorageType>(me);
    auto rThem = cast<ReferenceStorageType>(them);
    return rMe->getReferentType()->isSpelledLike(rThem->getReferentType());
  }
  }

  llvm_unreachable("Unknown type kind");
}

Type TypeBase::getSuperclass(LazyResolver *resolver) {
  ClassDecl *classDecl = getClassOrBoundGenericClass();

  // Handle some special non-class types here.
  if (!classDecl) {
    if (auto archetype = getAs<ArchetypeType>())
      return archetype->getSuperclass();

    if (auto dynamicSelfTy = getAs<DynamicSelfType>())
      return dynamicSelfTy->getSelfType();

    // No other types have superclasses.
    return nullptr;
  }

  // We have a class, so get the superclass type.
  //
  // If the derived class is generic, the superclass type may contain
  // generic type parameters from the signature of the derived class.
  Type superclassTy = classDecl->getSuperclass();

  // If there's no superclass, or it is fully concrete, we're done.
  if (!superclassTy || !superclassTy->hasTypeParameter())
    return superclassTy;

  // Gather substitutions from the self type, and apply them to the original
  // superclass type to form the substituted superclass type.
  Module *module = classDecl->getModuleContext();
  auto *sig = classDecl->getGenericSignatureOfContext();
  auto subs = sig->getSubstitutionMap(gatherAllSubstitutions(module, resolver));

  return superclassTy.subst(module, subs, None);
}

bool TypeBase::isExactSuperclassOf(Type ty, LazyResolver *resolver) {
  // For there to be a superclass relationship, we must be a superclass, and
  // the potential subtype must be a class or superclass-bounded archetype.
  if (!getClassOrBoundGenericClass() || !ty->mayHaveSuperclass())
    return false;

  do {
    if (ty->isEqual(this))
      return true;
    if (ty->getAnyNominal() && ty->getAnyNominal()->isInvalid())
      return false;
  } while ((ty = ty->getSuperclass(resolver)));
  return false;
}

/// Returns true if type `a` has archetypes that can be bound to form `b`.
bool TypeBase::isBindableTo(Type b, LazyResolver *resolver) {
  class IsBindableVisitor : public TypeVisitor<IsBindableVisitor, bool, CanType>
  {
    llvm::DenseMap<ArchetypeType *, CanType> Bindings;
    LazyResolver *Resolver;
  public:
    IsBindableVisitor(LazyResolver *Resolver)
      : Resolver(Resolver) {}
  
    bool visitArchetypeType(ArchetypeType *orig, CanType subst) {
      // If we already bound this archetype, make sure the new binding candidate
      // is the same type.
      auto bound = Bindings.find(orig);
      if (bound != Bindings.end()) {
        return bound->second->isEqual(subst);
      }
      
      auto canBindClassConstrainedArchetype = [](CanType t) -> bool {
        // Classes and class-constrained archetypes.
        if (t->mayHaveSuperclass())
          return true;
        
        // Pure @objc existentials.
        if (t->isObjCExistentialType())
          return true;
        
        return false;
      };
      
      // Check that the archetype isn't constrained in a way that makes the
      // binding impossible.
      // For instance, if the archetype is class-constrained, and the binding
      // is not a class, it can never be bound.
      if (orig->requiresClass() && !canBindClassConstrainedArchetype(subst))
        return false;
      
      // TODO: If the archetype has a superclass constraint, check that the
      // substitution is a subclass.
      
      // TODO: For private types or protocols, we might be able to definitively
      // deny bindings.
      
      // Otherwise, there may be an external retroactive conformance that
      // allows the binding.
      
      // Remember the binding, and succeed.
      Bindings.insert({orig, subst});
      return true;
    }
    
    bool visitType(TypeBase *orig, CanType subst) {
      if (CanType(orig) == subst)
        return true;
      
      llvm_unreachable("not a valid canonical type substitution");
    }
    
    bool visitNominalType(NominalType *nom, CanType subst) {
      if (auto substNom = dyn_cast<NominalType>(subst)) {
        if (nom->getDecl() != substNom->getDecl())
          return false;
        
        if (nom->getDecl()->isInvalid())
          return false;
        
        // Same decl should always either have or not have a parent.
        assert((bool)nom->getParent() == (bool)substNom->getParent());
        
        if (nom->getParent())
          return visit(nom->getParent()->getCanonicalType(),
                       substNom->getParent()->getCanonicalType());
        return true;
      }
      return false;
    }
    
    bool visitAnyMetatypeType(AnyMetatypeType *meta, CanType subst) {
      if (auto substMeta = dyn_cast<AnyMetatypeType>(subst)) {
        if (substMeta->getKind() != meta->getKind())
          return false;
        return visit(meta->getInstanceType()->getCanonicalType(),
                     substMeta->getInstanceType()->getCanonicalType());
      }
      return false;
    }
    
    bool visitTupleType(TupleType *tuple, CanType subst) {
      if (auto substTuple = dyn_cast<TupleType>(subst)) {
        // Tuple elements must match.
        if (tuple->getNumElements() != substTuple->getNumElements())
          return false;
        // TODO: Label reordering?
        for (unsigned i : indices(tuple->getElements())) {
          auto elt = tuple->getElements()[i],
               substElt = substTuple->getElements()[i];
          if (elt.getName() != substElt.getName())
            return false;
          if (!visit(elt.getType(), substElt.getType()->getCanonicalType()))
            return false;
        }
        return true;
      }
      return false;
    }
    
    bool visitDependentMemberType(DependentMemberType *dt, CanType subst) {
      llvm_unreachable("can't visit dependent types");
    }
    bool visitGenericTypeParamType(GenericTypeParamType *dt, CanType subst) {
      llvm_unreachable("can't visit dependent types");
    }
    
    bool visitFunctionType(FunctionType *func, CanType subst) {
      if (auto substFunc = dyn_cast<FunctionType>(subst)) {
        if (func->getExtInfo() != substFunc->getExtInfo())
          return false;
        
        if (!visit(func->getInput()->getCanonicalType(),
                   substFunc->getInput()->getCanonicalType()))
          return false;
        
        return visit(func->getResult()->getCanonicalType(),
                     substFunc->getResult()->getCanonicalType());
      }
      return false;
    }
    
    bool visitSILFunctionType(SILFunctionType *func,
                              CanType subst) {
      if (auto substFunc = dyn_cast<SILFunctionType>(subst)) {
        if (func->getExtInfo() != substFunc->getExtInfo())
          return false;
        
        // TODO: Generic signatures
        if (func->getGenericSignature() || substFunc->getGenericSignature())
          return false;
        
        if (func->getParameters().size() != substFunc->getParameters().size())
          return false;
        if (func->getAllResults().size() != substFunc->getAllResults().size())
          return false;
        
        for (unsigned i : indices(func->getParameters())) {
          if (func->getParameters()[i].getConvention()
                != substFunc->getParameters()[i].getConvention())
            return false;
          if (!visit(func->getParameters()[i].getType(),
                     substFunc->getParameters()[i].getType()))
            return false;
        }
        
        for (unsigned i : indices(func->getAllResults())) {
          if (func->getAllResults()[i].getConvention()
                != substFunc->getAllResults()[i].getConvention())
            return false;
          
          if (!visit(func->getAllResults()[i].getType(),
                     substFunc->getAllResults()[i].getType()))
            return false;
        }
        
        return true;
      }
      
      return false;
    }
    
    bool visitBoundGenericType(BoundGenericType *bgt, CanType subst) {
      if (auto substBGT = dyn_cast<BoundGenericType>(subst)) {
        if (bgt->getDecl() != substBGT->getDecl())
          return false;
        
        if (bgt->getDecl()->isInvalid())
          return false;
        
        auto origSubs = bgt->gatherAllSubstitutions(
            bgt->getDecl()->getParentModule(), Resolver);
        auto substSubs = substBGT->gatherAllSubstitutions(
            bgt->getDecl()->getParentModule(), Resolver);
        assert(origSubs.size() == substSubs.size());
        for (unsigned subi : indices(origSubs)) {
          if (!visit(origSubs[subi].getReplacement()->getCanonicalType(),
                     substSubs[subi].getReplacement()->getCanonicalType()))
            return false;
          assert(origSubs[subi].getConformances().size()
                 == substSubs[subi].getConformances().size());
          for (unsigned conformancei :
                 indices(origSubs[subi].getConformances())) {
            // An abstract conformance can be bound to a concrete one.
            // A concrete conformance may be bindable to a different
            // specialization of the same root conformance.
            auto origConf = origSubs[subi].getConformances()[conformancei],
                 substConf = substSubs[subi].getConformances()[conformancei];
            if (origConf.isConcrete()) {
              if (!substConf.isConcrete())
                return false;
              if (origConf.getConcrete()->getRootNormalConformance()
                   != substConf.getConcrete()->getRootNormalConformance())
                return false;
            }
          }
        }
        
        // Same decl should always either have or not have a parent.
        assert((bool)bgt->getParent() == (bool)substBGT->getParent());
        if (bgt->getParent())
          return visit(bgt->getParent()->getCanonicalType(),
                       substBGT->getParent()->getCanonicalType());
        return true;
      }
      return false;
    }
  };
  
  return IsBindableVisitor(resolver).visit(getCanonicalType(),
                                           b->getCanonicalType());
}

bool TypeBase::isBindableToSuperclassOf(Type ty, LazyResolver *resolver) {
  // Do an exact match if no archetypes are involved.
  if (!hasArchetype())
    return isExactSuperclassOf(ty, resolver);
  
  // For there to be a superclass relationship,
  // the potential subtype must be a class or superclass-bounded archetype.
  if (!ty->mayHaveSuperclass())
    return false;

  // If the type is itself an archetype, we could always potentially bind it
  // to the superclass (via external retroactive conformance, even if the
  // type isn't statically known to conform).
  //
  // We could theoretically reject cases where the set of conformances is known
  // (say the protocol or classes are private or internal).
  if (is<ArchetypeType>())
    return true;
  
  do {
    if (isBindableTo(ty, resolver))
      return true;
    if (ty->getAnyNominal() && ty->getAnyNominal()->isInvalid())
      return false;
  } while ((ty = ty->getSuperclass(resolver)));
  return false;
}

static bool isBridgeableObjectType(CanType type) {
  // Metatypes aren't always trivially bridgeable unless they've been
  // SIL-lowered to have an @objc representation.
  if (auto metaTy = dyn_cast<AnyMetatypeType>(type)) {
    if (!metaTy->hasRepresentation())
      return false;

    if (metaTy->getRepresentation() != MetatypeRepresentation::ObjC)
      return false;

    if (auto metatype = dyn_cast<MetatypeType>(type)) {
      CanType instanceType = metatype.getInstanceType();
      return instanceType->mayHaveSuperclass();
    }

    // @objc protocol metatypes.
    if (auto metatype = dyn_cast<ExistentialMetatypeType>(type)) {
      return metatype.getInstanceType()->isObjCExistentialType();
    }
  }

  // Classes and class-constrained archetypes.
  if (type->mayHaveSuperclass())
    return true;

  // Pure-ObjC existential types.
  if (type.isObjCExistentialType()) {
    return true;
  }

  // Blocks.
  if (auto fnType = dyn_cast<AnyFunctionType>(type)) {
    return fnType->getRepresentation()
      == AnyFunctionType::Representation::Block;
  } else if (auto fnType = dyn_cast<SILFunctionType>(type)) {
    return fnType->getRepresentation()
      == SILFunctionType::Representation::Block;
  }

  return false;
}

static bool hasRetainablePointerRepresentation(CanType type) {
  // Look through one level of Optional<> or ImplicitlyUnwrappedOptional<>.
  if (auto objType = type.getAnyOptionalObjectType()) {
    type = objType;
  }

  return isBridgeableObjectType(type);
}

bool TypeBase::hasRetainablePointerRepresentation() {
  return ::hasRetainablePointerRepresentation(getCanonicalType());
}

bool TypeBase::isBridgeableObjectType() {
  return ::isBridgeableObjectType(getCanonicalType());
}

bool TypeBase::isPotentiallyBridgedValueType() {
  // struct and enum types
  if (auto nominal = getAnyNominal()) {
    if (isa<StructDecl>(nominal) || isa<EnumDecl>(nominal))
      return true;
  }

  // Error existentials.
  if (isExistentialWithError()) return true;

  // Archetypes.
  return is<ArchetypeType>();
}

/// Determine whether this is a representable Objective-C object type.
static ForeignRepresentableKind
getObjCObjectRepresentable(Type type, const DeclContext *dc) {
  // @objc metatypes are representable when their instance type is.
  if (auto metatype = type->getAs<AnyMetatypeType>()) {
    auto instanceType = metatype->getInstanceType();

    // Consider metatype of any existential type as not Objective-C
    // representable.
    if (metatype->is<MetatypeType>() && instanceType->isAnyExistentialType())
      return ForeignRepresentableKind::None;

    // If the instance type is not representable verbatim, the metatype is not
    // representable.
    if (getObjCObjectRepresentable(instanceType, dc)
          != ForeignRepresentableKind::Object)
      return ForeignRepresentableKind::None;

    // Objective-C metatypes are trivially representable.
    if (metatype->hasRepresentation() &&
        metatype->getRepresentation() == MetatypeRepresentation::ObjC)
      return ForeignRepresentableKind::Object;

    // All other metatypes are bridged.
    return ForeignRepresentableKind::Bridged;
  }

  // Look through DynamicSelfType.
  if (auto dynSelf = type->getAs<DynamicSelfType>())
    type = dynSelf->getSelfType();

  // @objc classes.
  if (auto classDecl = type->getClassOrBoundGenericClass()) {
    auto &ctx = classDecl->getASTContext();
    if (auto resolver = ctx.getLazyResolver())
      resolver->resolveDeclSignature(classDecl);

    if (classDecl->isObjC())
      return ForeignRepresentableKind::Object;
  }

  // Objective-C existential types.
  if (type->isObjCExistentialType())
    return ForeignRepresentableKind::Object;
  
  // Any can be bridged to id.
  if (type->getASTContext().LangOpts.EnableIdAsAny) {
    if (type->isAny()) {
      return ForeignRepresentableKind::Bridged;
    }
  }
  
  // Class-constrained generic parameters, from ObjC generic classes.
  if (auto tyContext = dc->getInnermostTypeContext())
    if (auto clas = tyContext->getAsClassOrClassExtensionContext())
      if (clas->hasClangNode())
        if (auto archetype = type->getAs<ArchetypeType>())
          if (archetype->requiresClass())
            return ForeignRepresentableKind::Object;

  return ForeignRepresentableKind::None;
}

/// Determine the foreign representation of this type.
///
/// This function determines when and how a particular type is mapped
/// into a foreign language. Any changes to the logic here also need
/// to be reflected in PrintAsObjC, so that the Swift type will be
/// properly printed for (Objective-)C and in SIL's bridging logic.
static std::pair<ForeignRepresentableKind, ProtocolConformance *>
getForeignRepresentable(Type type, ForeignLanguage language,
                        const DeclContext *dc) {
  // Look through one level of optional type, but remember that we did.
  bool wasOptional = false;
  if (auto valueType = type->getAnyOptionalObjectType()) {
    type = valueType;
    wasOptional = true;
  }

  // Objective-C object types, including metatypes.
  if (language == ForeignLanguage::ObjectiveC) {
    auto representable = getObjCObjectRepresentable(type, dc);
    if (representable != ForeignRepresentableKind::None)
      return { representable, nullptr };
  }

  // Local function that simply produces a failing result.
  auto failure = []() -> std::pair<ForeignRepresentableKind,
                                   ProtocolConformance *> {
    return { ForeignRepresentableKind::None, nullptr };
  };

  // Function types.
  if (auto functionType = type->getAs<FunctionType>()) {
    // Cannot handle throwing functions.
    if (functionType->getExtInfo().throws())
      return failure();

    // Whether we have found any types that are bridged.
    bool anyBridged = false;
    bool anyStaticBridged = false;

    // Local function to combine the result of a recursive invocation.
    //
    // Returns true on failure.
    auto recurse = [&](Type componentType) -> bool {
      switch (componentType->getForeignRepresentableIn(language, dc).first) {
      case ForeignRepresentableKind::None:
        return true;

      case ForeignRepresentableKind::Trivial:
      case ForeignRepresentableKind::Object:
        return false;

      case ForeignRepresentableKind::Bridged:
      case ForeignRepresentableKind::BridgedError:
        anyBridged = true;
        return false;

      case ForeignRepresentableKind::StaticBridged:
        anyStaticBridged = true;
        return false;
      }
    };

    // Check the representation of the function type.
    bool isBlock = false;
    switch (functionType->getRepresentation()) {
    case AnyFunctionType::Representation::Thin:
      return failure();

    case AnyFunctionType::Representation::Swift:
      anyStaticBridged = true;
      break;

    case AnyFunctionType::Representation::Block:
      isBlock = true;
      break;

    case AnyFunctionType::Representation::CFunctionPointer:
      break;
    }

    // Look at the result type.
    Type resultType = functionType->getResult();
    if (!resultType->isVoid() && recurse(resultType))
      return failure();

    // Look at the input types.
    Type inputType = functionType->getInput();
    if (auto inputTuple = inputType->getAs<TupleType>()) {
      for (const auto &elt : inputTuple->getElements()) {
        if (elt.isVararg())
          return failure();
        if (recurse(elt.getType()))
          return failure();
      }
    } else if (recurse(inputType)) {
      return failure();
    }

    // We have something representable; check how it is representable.
    return { anyStaticBridged ? ForeignRepresentableKind::StaticBridged
                 : anyBridged ? ForeignRepresentableKind::Bridged
                 : isBlock    ? ForeignRepresentableKind::Object
                 : ForeignRepresentableKind::Trivial,
             nullptr };
  }

  // In Objective-C, type parameters are always objects.
  if (type->isTypeParameter() && language == ForeignLanguage::ObjectiveC)
    return { ForeignRepresentableKind::Object, nullptr };

  auto nominal = type->getAnyNominal();
  if (!nominal) return failure();

  ASTContext &ctx = nominal->getASTContext();

  // Unmanaged<T> can be trivially represented in Objective-C if T
  // is trivially represented in Objective-C.
  if (language == ForeignLanguage::ObjectiveC &&
      nominal == ctx.getUnmanagedDecl()) {
    auto boundGenericType = type->getAs<BoundGenericType>();

    // Note: works around a broken Unmanaged<> definition.
    if (!boundGenericType || boundGenericType->getGenericArgs().size() != 1)
      return failure();
    
    auto typeArgument = boundGenericType->getGenericArgs()[0];
    if (typeArgument->isTriviallyRepresentableIn(language, dc))
      return { ForeignRepresentableKind::Trivial, nullptr };

    return failure();
  }

  // If the type was imported from Clang, check whether it is
  // representable in the requested language.
  if (nominal->hasClangNode() || nominal->isObjC()) {
    switch (language) {
    case ForeignLanguage::C:
      // Imported classes and protocols are not representable in C.
      if (isa<ClassDecl>(nominal) || isa<ProtocolDecl>(nominal))
        return failure();
      SWIFT_FALLTHROUGH;

    case ForeignLanguage::ObjectiveC:
      if (isa<StructDecl>(nominal) || isa<EnumDecl>(nominal)) {
        // Optional structs are not representable in (Objective-)C if they
        // originally came from C, whether or not they are bridged, unless they
        // came from swift_newtype. If they are defined in Swift, they are only
        // representable if they are bridged (checked below).
        if (wasOptional) {
          if (nominal->hasClangNode()) {
            Type underlyingType =
                nominal->getDeclaredType()->getSwiftNewtypeUnderlyingType();
            if (underlyingType) {
              return getForeignRepresentable(OptionalType::get(underlyingType),
                                             language, dc);
            }
            return failure();
          }
          break;
        }
      }

      return { ForeignRepresentableKind::Trivial, nullptr };
    }
  }

  // Pointers may be representable in ObjC.
  PointerTypeKind pointerKind;
  if (auto pointerElt = type->getAnyPointerElementType(pointerKind)) {
    switch (pointerKind) {
    case PTK_UnsafeMutableRawPointer:
    case PTK_UnsafeRawPointer:
    case PTK_UnsafeMutablePointer:
    case PTK_UnsafePointer:
      // An UnsafeMutablePointer<T> or UnsafePointer<T> is
      // representable if T is trivially representable or Void.
      if (pointerElt->isVoid() ||
          pointerElt->isTriviallyRepresentableIn(language, dc))
        return { ForeignRepresentableKind::Trivial, nullptr };

      return failure();

    case PTK_AutoreleasingUnsafeMutablePointer:
      // An AutoreleasingUnsafeMutablePointer<T> is representable in
      // Objective-C if T is a representable object type in
      // Objective-C.

      // Allow one level of optionality.
      if (auto objectType = pointerElt->getAnyOptionalObjectType())
        pointerElt = objectType;

      if (language == ForeignLanguage::ObjectiveC &&
          getObjCObjectRepresentable(pointerElt, dc)
            != ForeignRepresentableKind::None)
        return { ForeignRepresentableKind::Trivial, nullptr };

      return failure();
    }
  }

  // Determine whether this nominal type is known to be representable
  // in this foreign language.
  auto result = ctx.getForeignRepresentationInfo(nominal, language, dc);
  if (result.getKind() == ForeignRepresentableKind::None) return failure();

  if (wasOptional && !result.isRepresentableAsOptional())
    return failure();

  // If our nominal type has type arguments, make sure they are
  // representable as well. Because type arguments are not actually
  // translated separately, whether they are trivially representable
  // or bridged representable doesn't impact our final result.
  if (auto boundGenericType = type->getAs<BoundGenericType>()) {
    for (auto typeArg : boundGenericType->getGenericArgs()) {
      // Type arguments cannot be optional.
      if (typeArg->getAnyOptionalObjectType())
        return failure();

      // And must be representable either an object or bridged.
      switch (typeArg->getForeignRepresentableIn(language, dc).first) {
      case ForeignRepresentableKind::None:
      case ForeignRepresentableKind::StaticBridged:
        return failure();

      case ForeignRepresentableKind::Trivial:
        // FIXME: We allow trivially-representable cases that also
        // conform to _ObjectiveCBridgeable. This may not be desirable
        // and should be re-evaluated.
        if (auto nominal = typeArg->getAnyNominal()) {
          if (auto objcBridgeable
                = ctx.getProtocol(KnownProtocolKind::ObjectiveCBridgeable)) {
            SmallVector<ProtocolConformance *, 1> conformances;
            if (nominal->lookupConformance(dc->getParentModule(),
                                           objcBridgeable,
                                           conformances))
              break;
          }
        }
        
        return failure();
        
      case ForeignRepresentableKind::Object:
      case ForeignRepresentableKind::Bridged:
      case ForeignRepresentableKind::BridgedError:
        break;
      }
    }
  }

  return { result.getKind(), result.getConformance() };
}

std::pair<ForeignRepresentableKind, ProtocolConformance *>
TypeBase::getForeignRepresentableIn(ForeignLanguage language,
                                    const DeclContext *dc) {
  return getForeignRepresentable(Type(this), language, dc);
}

bool TypeBase::isRepresentableIn(ForeignLanguage language,
                                 const DeclContext *dc) {
  switch (getForeignRepresentableIn(language, dc).first) {
  case ForeignRepresentableKind::None:
    return false;

  case ForeignRepresentableKind::Trivial:
  case ForeignRepresentableKind::Object:
  case ForeignRepresentableKind::Bridged:
  case ForeignRepresentableKind::BridgedError:
  case ForeignRepresentableKind::StaticBridged:
    return true;
  }
}

bool TypeBase::isTriviallyRepresentableIn(ForeignLanguage language,
                                          const DeclContext *dc) {
  switch (getForeignRepresentableIn(language, dc).first) {
  case ForeignRepresentableKind::None:
  case ForeignRepresentableKind::Bridged:
  case ForeignRepresentableKind::BridgedError:
  case ForeignRepresentableKind::StaticBridged:
    return false;

  case ForeignRepresentableKind::Trivial:
  case ForeignRepresentableKind::Object:
    return true;
  }
}

/// Is t1 not just a subtype of t2, but one such that its values are
/// trivially convertible to values of the other?
static bool canOverride(CanType t1, CanType t2,
                        OverrideMatchMode matchMode,
                        bool isParameter,
                        bool insideOptional,
                        LazyResolver *resolver) {
  if (t1 == t2) return true;

  // First try unwrapping optionals.
  // Make sure we only unwrap at most one layer of optional.
  if (!insideOptional) {
    // Value-to-optional and optional-to-optional.
    if (auto obj2 = t2.getAnyOptionalObjectType()) {
      // Optional-to-optional.
      if (auto obj1 = t1.getAnyOptionalObjectType()) {
        // Allow T? and T! to freely override one another.
        return canOverride(obj1, obj2, matchMode,
                           /*isParameter=*/false,
                           /*insideOptional=*/true,
                           resolver);
      }

      // Value-to-optional.
      return canOverride(t1, obj2, matchMode,
                         /*isParameter=*/false,
                         /*insideOptional=*/true,
                         resolver);

    } else if (matchMode == OverrideMatchMode::AllowTopLevelOptionalMismatch) {
      // Optional-to-value, normally disallowed.
      if (auto obj1 = t1.getAnyOptionalObjectType()) {
        return canOverride(obj1, t2, matchMode,
                           /*isParameter=*/false,
                           /*insideOptional=*/true,
                           resolver);
      }
    }
  }

  // Scalar-to-tuple and tuple-to-tuple.
  if (auto tuple2 = dyn_cast<TupleType>(t2)) {
    // We only ever look into singleton tuples on the RHS if we're
    // certain that the LHS isn't also a singleton tuple.
    auto tuple1 = dyn_cast<TupleType>(t1);
    if (!tuple1 || tuple1->getNumElements() != tuple2->getNumElements()) {
      if (tuple2->getNumElements() == 1)
        return canOverride(t1, tuple2.getElementType(0),
                           matchMode,
                           isParameter,
                           /*insideOptional=*/false,
                           resolver);
      return false;
    }

    for (auto i : indices(tuple1.getElementTypes())) {
      if (!canOverride(tuple1.getElementType(i),
                       tuple2.getElementType(i),
                       matchMode,
                       isParameter,
                       /*insideOptional=*/false,
                       resolver))
        return false;
    }
    return true;
  }

  // Function-to-function.  (Polymorphic functions?)
  if (auto fn2 = dyn_cast<FunctionType>(t2)) {
    auto fn1 = dyn_cast<FunctionType>(t1);
    if (!fn1)
      return false;

    // Allow the base type to be throwing even if the overriding type isn't
    auto ext1 = fn1->getExtInfo();
    auto ext2 = fn2->getExtInfo();
    if (ext2.throws()) ext1 = ext1.withThrows(true);
    if (ext1 != ext2)
      return false;

    // Inputs are contravariant, results are covariant.
    return (canOverride(fn2.getInput(), fn1.getInput(),
                        matchMode,
                        /*isParameter=*/true,
                        /*insideOptional=*/false,
                        resolver) &&
            canOverride(fn1.getResult(), fn2.getResult(),
                        matchMode,
                        /*isParameter=*/false,
                        /*insideOptional=*/false,
                        resolver));
  }

  if (matchMode == OverrideMatchMode::AllowNonOptionalForIUOParam &&
      isParameter && !insideOptional) {
    // Allow T to override T! in certain cases.
    if (auto obj1 = t1->getImplicitlyUnwrappedOptionalObjectType()) {
      t1 = obj1->getCanonicalType();
      if (t1 == t2) return true;
    }
  }

  // Class-to-class.
  return t2->isExactSuperclassOf(t1, resolver);
}

bool TypeBase::canOverride(Type other, OverrideMatchMode matchMode,
                           LazyResolver *resolver) {
  return ::canOverride(getCanonicalType(), other->getCanonicalType(),
                       matchMode,
                       /*isParameter=*/false,
                       /*insideOptional=*/false,
                       resolver);
}

/// getNamedElementId - If this tuple has a field with the specified name,
/// return the field index, otherwise return -1.
int TupleType::getNamedElementId(Identifier I) const {
  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    if (Elements[i].getName() == I)
      return i;
  }

  // Otherwise, name not found.
  return -1;
}

/// getElementForScalarInit - If a tuple of this type can be initialized with a
/// scalar, return the field number that the scalar is assigned to.  If not,
/// return -1.
int TupleType::getElementForScalarInit() const {
  if (Elements.empty()) return -1;
  
  int FieldWithoutDefault = -1;
  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    // If we already saw a non-vararg field missing a default value, then we
    // cannot assign a scalar to this tuple.
    if (FieldWithoutDefault != -1) {
      // Vararg fields are okay; they'll just end up being empty.
      if (Elements[i].isVararg())
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

/// If this tuple has a varargs element to it, return the base type of the
/// varargs element (i.e., if it is "Int...", this returns Int, not [Int]).
/// Otherwise, this returns Type().
Type TupleType::getVarArgsBaseType() const {
  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    if (Elements[i].isVararg())
      return Elements[i].getVarargBaseTy();
  }
  
  return Type();
}


CanArchetypeType ArchetypeType::getNew(const ASTContext &Ctx,
                                       ArchetypeType *Parent,
                                       AssocTypeOrProtocolType AssocTypeOrProto,
                                       Identifier Name,
                                       ArrayRef<Type> ConformsTo,
                                       Type Superclass,
                                       bool isRecursive) {
  // Gather the set of protocol declarations to which this archetype conforms.
  SmallVector<ProtocolDecl *, 4> ConformsToProtos;
  for (auto P : ConformsTo) {
    addProtocols(P, ConformsToProtos);
  }
  ProtocolType::canonicalizeProtocols(ConformsToProtos);

  auto arena = AllocationArena::Permanent;
  return CanArchetypeType(
           new (Ctx, arena) ArchetypeType(Ctx, Parent, AssocTypeOrProto, Name,
                                          Ctx.AllocateCopy(ConformsToProtos),
                                          Superclass, isRecursive));
}

CanArchetypeType
ArchetypeType::getNew(const ASTContext &Ctx, ArchetypeType *Parent,
                      AssocTypeOrProtocolType AssocTypeOrProto,
                      Identifier Name,
                      SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                      Type Superclass, bool isRecursive) {
  // Gather the set of protocol declarations to which this archetype conforms.
  ProtocolType::canonicalizeProtocols(ConformsTo);

  auto arena = AllocationArena::Permanent;
  return CanArchetypeType(
           new (Ctx, arena) ArchetypeType(Ctx, Parent, AssocTypeOrProto, Name,
                                          Ctx.AllocateCopy(ConformsTo),
                                          Superclass, isRecursive));
}

bool ArchetypeType::requiresClass() const {
  if (Superclass)
    return true;
  for (ProtocolDecl *conformed : getConformsTo())
    if (conformed->requiresClass())
      return true;
  return false;
}

void ArchetypeType::resolveNestedType(
       std::pair<Identifier, NestedType> &nested) const {
  auto &ctx = const_cast<ArchetypeType *>(this)->getASTContext();
  auto lazyArchetype = ctx.getLazyArchetype(this);
  nested.second = lazyArchetype.second->getNestedType(nested.first,
                                                      *lazyArchetype.first)
                    ->getType(*lazyArchetype.first);
}

namespace {
  /// \brief Function object that orders archetypes by name.
  struct OrderArchetypeByName {
    using NestedType = ArchetypeType::NestedType;
    bool operator()(std::pair<Identifier, NestedType> X,
                    std::pair<Identifier, NestedType> Y) const {
      return X.first.str() < Y.first.str();
    }

    bool operator()(std::pair<Identifier, NestedType> X,
                    Identifier Y) const {
      return X.first.str() < Y.str();
    }

    bool operator()(Identifier X,
                    std::pair<Identifier, NestedType> Y) const {
      return X.str() < Y.first.str();
    }

    bool operator()(Identifier X, Identifier Y) const {
      return X.str() < Y.str();
    }
  };
}

ArchetypeType::NestedType ArchetypeType::getNestedType(Identifier Name) const {
  auto Pos = std::lower_bound(NestedTypes.begin(), NestedTypes.end(), Name,
                              OrderArchetypeByName());
  if ((Pos == NestedTypes.end() || Pos->first != Name) && this->isRecursive) {
    if (Name == this->getName()) {
      NestedType rec = NestedType::forArchetype((ArchetypeType*)this);
    
      return rec;
    } else {
      auto conformances = this->getConformsTo();
      
      for (auto conformance : conformances) {
        auto conformanceType = conformance->getType().getPointer();
        
        if (auto metatypeType = dyn_cast<MetatypeType>(conformanceType)) {
          conformanceType = metatypeType->getInstanceType().getPointer();
          
          if (auto protocolType = dyn_cast<ProtocolType>(conformanceType)) {
            conformanceType = protocolType->getDecl()
                                          ->getSelfTypeInContext().getPointer();
          }
        }
        
        if (auto conformedArchetype = dyn_cast<ArchetypeType>(conformanceType)){
          return conformedArchetype->getNestedType(Name);
        }
      }
    }
  }

  if (Pos == NestedTypes.end() || Pos->first != Name)
    return NestedType::forConcreteType(
             ErrorType::get(
               const_cast<ArchetypeType *>(this)->getASTContext()));

  // If the type is null, lazily resolve it. 
  if (!Pos->second) {
    resolveNestedType(*Pos);
  }

  return Pos->second;
}

bool ArchetypeType::hasNestedType(Identifier Name) const {
  auto Pos = std::lower_bound(NestedTypes.begin(), NestedTypes.end(), Name,
                              OrderArchetypeByName());
  return Pos != NestedTypes.end() && Pos->first == Name;
}

ArrayRef<std::pair<Identifier, ArchetypeType::NestedType>>
ArchetypeType::getNestedTypes(bool resolveTypes) const {
  if (resolveTypes) {
    for (auto &nested : NestedTypes) {
      if (!nested.second)
        resolveNestedType(nested);
    }
  }

  return NestedTypes;
}

void ArchetypeType::setNestedTypes(
       ASTContext &Ctx,
       MutableArrayRef<std::pair<Identifier, NestedType>> Nested) {
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

void ProtocolCompositionType::getAnyExistentialTypeProtocols(
                                   SmallVectorImpl<ProtocolDecl *> &protos) {
  // The canonical type for a protocol composition canonicalizes the
  // order of the protocols.
  auto canonical = cast<ProtocolCompositionType>(getCanonicalType());
  canonical.getAnyExistentialTypeProtocols(protos);
}

bool ProtocolCompositionType::requiresClass() const {
  for (Type t : getProtocols()) {
    if (const ProtocolType *proto = t->getAs<ProtocolType>()) {
      if (proto->requiresClass())
        return true;
    } else {
      if (t->castTo<ProtocolCompositionType>()->requiresClass())
        return true;
    }
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
  // declarations, and use that to build the canonical composition type.
  SmallVector<Type, 4> CanProtocolTypes;
  std::transform(Protocols.begin(), Protocols.end(),
                 std::back_inserter(CanProtocolTypes),
                 [](ProtocolDecl *Proto) {
                   return Proto->getDeclaredType();
                 });

  return build(C, CanProtocolTypes);
}

ArrayRef<GenericTypeParamDecl *>
PolymorphicFunctionType::getGenericParameters() const {
  return Params->getParams();
}

TypeSubstitutionMap
GenericParamList::getSubstitutionMap(ArrayRef<swift::Substitution> Subs) const {
  TypeSubstitutionMap map;
  
  for (auto arch : getAllNestedArchetypes()) {
    auto sub = Subs.front();
    Subs = Subs.slice(1);
    
    map.insert({arch, sub.getReplacement()});
  }
  
  assert(Subs.empty() && "did not use all substitutions?!");
  return map;
}

FunctionType *
GenericFunctionType::substGenericArgs(Module *M, ArrayRef<Substitution> args) {
  auto params = getGenericParams();
  (void)params;
  
  TypeSubstitutionMap subs
    = getGenericSignature()->getSubstitutionMap(args);

  Type input = getInput().subst(M, subs, SubstFlags::IgnoreMissing);
  Type result = getResult().subst(M, subs, SubstFlags::IgnoreMissing);
  return FunctionType::get(input, result, getExtInfo());
}

static Type getMemberForBaseType(Module *module,
                                 Type substBase,
                                 AssociatedTypeDecl *assocType,
                                 Identifier name,
                                 SubstOptions options) {
  // Error recovery path.
  if (substBase->isOpenedExistential())
    return ErrorType::get(module->getASTContext());

  // If the parent is an archetype, extract the child archetype with the
  // given name.
  if (auto archetypeParent = substBase->getAs<ArchetypeType>()) {
    if (archetypeParent->hasNestedType(name))
      return archetypeParent->getNestedTypeValue(name);

    if (auto parent = archetypeParent->getParent()) {
      // If the archetype doesn't have the requested type and the parent is not
      // self derived, error out
      return parent->isSelfDerived() ? parent->getNestedTypeValue(name)
                                     : ErrorType::get(module->getASTContext());
    }

    // If looking for an associated type and the archetype is constrained to a
    // class, continue to the default associated type lookup
    if (!assocType || !archetypeParent->getSuperclass()) {
      // else just error out
      return ErrorType::get(module->getASTContext());
    }
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

  // If the parent is dependent, create a dependent member type.
  if (substBase->isTypeParameter()) {
    if (assocType)
      return DependentMemberType::get(substBase, assocType,
                                      substBase->getASTContext());
    else
      return DependentMemberType::get(substBase, name,
                                      substBase->getASTContext());
  }

  // If we know the associated type, look in the witness table.
  LazyResolver *resolver = substBase->getASTContext().getLazyResolver();
  if (assocType) {
    auto proto = assocType->getProtocol();
    // FIXME: Introduce substituted type node here?
    auto conformance = module->lookupConformance(substBase, proto, resolver);
    if (!conformance)
      return Type();

    // If we have an unsatisfied type witness while we're checking the
    // conformances we're supposed to skip this conformance's unsatisfied type
    // witnesses, and we have an unsatisfied type witness, return
    // "missing".
    assert(conformance->isConcrete());
    if (conformance->getConcrete()->getRootNormalConformance()->getState()
          == ProtocolConformanceState::CheckingTypeWitnesses &&
        !conformance->getConcrete()->hasTypeWitness(assocType, nullptr))
      return Type();

    return conformance->getConcrete()->getTypeWitness(assocType, resolver)
             .getReplacement();
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
  if (substBase.getPointer() == getBase().getPointer() &&
      substBase->hasTypeParameter())
    return this;

  return getMemberForBaseType(module, substBase, getAssocType(), getName(),
                              None);
}

Type Type::subst(Module *module, TypeSubstitutionMap &substitutions,
                 SubstOptions options) const {
  /// Return the original type or a null type, depending on the 'ignoreMissing'
  /// flag.
  auto failed = [&](Type t){
    return options.contains(SubstFlags::IgnoreMissing) ? t : Type();
  };
  
  return transform([&](Type type) -> Type {
    assert((options.contains(SubstFlags::AllowLoweredTypes) ||
            !isa<SILFunctionType>(type.getPointer())) &&
           "should not be doing AST type-substitution on a lowered SIL type;"
           "use SILType::subst");

    // We only substitute for substitutable types and dependent member types.
    
    // For dependent member types, we may need to look up the member if the
    // base is resolved to a non-dependent type.
    if (auto depMemTy = type->getAs<DependentMemberType>()) {
      // Check whether we have a direct substitution for the dependent type.
      // FIXME: This arguably should be getMemberForBaseType's responsibility.
      auto known =
        substitutions.find(depMemTy->getCanonicalType().getPointer());
      if (known != substitutions.end() && known->second) {
        return SubstitutedType::get(type, known->second,
                                    module->getASTContext());
      }
    
      auto newBase = depMemTy->getBase().subst(module, substitutions, options);
      if (!newBase)
        return failed(type);
      
      if (Type r = getMemberForBaseType(module, newBase,
                                        depMemTy->getAssocType(),
                                        depMemTy->getName(), options))
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
      return SubstitutedType::get(type, known->second,
                                  module->getASTContext());

    // If we don't have a substitution for this type and it doesn't have a
    // parent, then we're not substituting it.
    auto parent = substOrig->getParent();
    if (!parent)
      return type;

    // Substitute into the parent type.
    Type substParent = Type(parent).subst(module, substitutions, options);
    if (!substParent)
      return Type();

    // If the parent didn't change, we won't change.
    if (substParent.getPointer() == parent)
      return type;

    // Get the associated type reference from a child archetype.
    AssociatedTypeDecl *assocType = nullptr;
    if (auto archetype = substOrig->getAs<ArchetypeType>()) {
      assocType = archetype->getAssocType();
    }
    
    
    if (Type r = getMemberForBaseType(module, substParent, assocType,
                                      substOrig->getName(), options))
      return r;
    return failed(type);
  });
}

TypeSubstitutionMap TypeBase::getMemberSubstitutions(const DeclContext *dc) {

  // Ignore lvalues in the base type.
  Type baseTy(getRValueType());

  // Look through the metatype; it has no bearing on the result.
  if (auto metaBase = baseTy->getAs<AnyMetatypeType>()) {
    baseTy = metaBase->getInstanceType()->getRValueType();
  }

  // The resulting set of substitutions. Always use this to ensure we
  // don't miss out on NRVO anywhere.
  TypeSubstitutionMap substitutions;

  // If the member is part of a protocol or extension thereof, we need
  // to substitute in the type of Self.
  if (dc->getAsProtocolOrProtocolExtensionContext()) {
    // FIXME: This feels painfully inefficient. We're creating a dense map
    // for a single substitution.
    substitutions[dc->getSelfInterfaceType()
                    ->getCanonicalType()
                    ->castTo<GenericTypeParamType>()]
      = baseTy;
    return substitutions;
  }

  // Extract the lazy resolver.
  LazyResolver *resolver = dc->getASTContext().getLazyResolver();

  // Find the superclass type with the context matching that of the member.
  auto ownerNominal = dc->getAsNominalTypeOrNominalTypeExtensionContext();
  while (!baseTy->is<ErrorType>() &&
         baseTy->getAnyNominal() &&
         baseTy->getAnyNominal() != ownerNominal) {
    baseTy = baseTy->getSuperclass(resolver);
    assert(baseTy && "Couldn't find appropriate context");
  }

  // If the base type isn't specialized, there's nothing to substitute.
  if (!baseTy->isSpecialized())
    return substitutions;

  // Gather all of the substitutions for all levels of generic arguments.
  GenericParamList *curGenericParams = dc->getGenericParamsOfContext();
  while (baseTy) {
    // For a bound generic type, gather the generic parameter -> generic
    // argument substitutions.
    if (auto boundGeneric = baseTy->getAs<BoundGenericType>()) {
      auto params = curGenericParams->getParams();
      auto args = boundGeneric->getGenericArgs();
      for (unsigned i = 0, n = args.size(); i != n; ++i) {
        substitutions[params[i]->getDeclaredType()->getCanonicalType()
                        ->castTo<GenericTypeParamType>()]
          = args[i];
      }

      // Continue looking into the parent.
      baseTy = boundGeneric->getParent();
      curGenericParams = curGenericParams->getOuterParameters();
      continue;
    }

    // Continue looking into the parent.
    if (auto nominalTy = baseTy->getAs<NominalType>()) {
      baseTy = nominalTy->getParent();
      continue;
    }

    // We're done.
    break;
  }

  return substitutions;
}

Type TypeBase::getTypeOfMember(Module *module, const ValueDecl *member,
                               LazyResolver *resolver, Type memberType) {
  // If no member type was provided, use the member's type.
  if (!memberType)
    memberType = member->getInterfaceType();

  return getTypeOfMember(module, memberType, member->getDeclContext());
}

Type TypeBase::getTypeOfMember(Module *module, Type memberType,
                               const DeclContext *memberDC) {
  // If the member is not part of a type, there's nothing to substitute.
  if (!memberDC->isTypeContext())
    return memberType;

  // Compute the set of member substitutions to apply.
  TypeSubstitutionMap substitutions = getMemberSubstitutions(memberDC);
  if (substitutions.empty())
    return memberType;

  // Perform the substitutions.
  return memberType.subst(module, substitutions, None);
}

Identifier DependentMemberType::getName() const {
  if (NameOrAssocType.is<Identifier>())
    return NameOrAssocType.get<Identifier>();

  return NameOrAssocType.get<AssociatedTypeDecl *>()->getName();
}

static bool transformSILResult(SILResultInfo &result, bool &changed,
                               llvm::function_ref<Type(Type)> fn) {
  Type transType = result.getType().transform(fn);
  if (!transType) return true;

  CanType canTransType = transType->getCanonicalType();
  if (canTransType != result.getType()) {
    changed = true;
    result = result.getWithType(canTransType);
  }
  return false;
}

static bool transformSILParameter(SILParameterInfo &param, bool &changed,
                                  llvm::function_ref<Type(Type)> fn) {
  Type transType = param.getType().transform(fn);
  if (!transType) return true;

  CanType canTransType = transType->getCanonicalType();
  if (canTransType != param.getType()) {
    changed = true;
    param = param.getWithType(canTransType);
  }
  return false;
}

Type Type::transform(llvm::function_ref<Type(Type)> fn) const {
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
  case TypeKind::Unresolved:
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
      
  case TypeKind::SILBlockStorage: {
    auto storageTy = cast<SILBlockStorageType>(base);
    Type transCap = storageTy->getCaptureType().transform(fn);
    if (!transCap)
      return Type();
    CanType canTransCap = transCap->getCanonicalType();
    if (canTransCap != storageTy->getCaptureType())
      return SILBlockStorageType::get(canTransCap);
    return storageTy;
  }

  case TypeKind::SILBox: {
    auto storageTy = cast<SILBoxType>(base);
    Type transBoxed = storageTy->getBoxedType().transform(fn);
    if (!transBoxed)
      return Type();
    CanType canTransBoxed = transBoxed->getCanonicalType();
    if (canTransBoxed != storageTy->getBoxedType())
      return SILBoxType::get(canTransBoxed);
    return storageTy;
  }

  case TypeKind::SILFunction: {
    auto fnTy = cast<SILFunctionType>(base);
    bool changed = false;

    SmallVector<SILParameterInfo, 8> transInterfaceParams;
    for (SILParameterInfo param : fnTy->getParameters()) {
      if (transformSILParameter(param, changed, fn)) return Type();
      transInterfaceParams.push_back(param);
    }

    SmallVector<SILResultInfo, 8> transInterfaceResults;
    for (SILResultInfo result : fnTy->getAllResults()) {
      if (transformSILResult(result, changed, fn)) return Type();
      transInterfaceResults.push_back(result);
    }

    Optional<SILResultInfo> transErrorResult;
    if (fnTy->hasErrorResult()) {
      SILResultInfo result = fnTy->getErrorResult();
      if (transformSILResult(result, changed, fn)) return Type();
      transErrorResult = result;
    }

    if (!changed) return *this;

    return SILFunctionType::get(fnTy->getGenericSignature(),
                                fnTy->getExtInfo(),
                                fnTy->getCalleeConvention(),
                                transInterfaceParams,
                                transInterfaceResults,
                                transErrorResult,
                                Ptr->getASTContext());
  }

  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
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

  case TypeKind::ExistentialMetatype: {
    auto meta = cast<ExistentialMetatypeType>(base);
    auto instanceTy = meta->getInstanceType().transform(fn);
    if (!instanceTy)
      return Type();

    if (instanceTy.getPointer() == meta->getInstanceType().getPointer())
      return *this;

    if (meta->hasRepresentation())
      return ExistentialMetatypeType::get(instanceTy,
                                          meta->getRepresentation());
    return ExistentialMetatypeType::get(instanceTy);
  }

  case TypeKind::Metatype: {
    auto meta = cast<MetatypeType>(base);
    auto instanceTy = meta->getInstanceType().transform(fn);
    if (!instanceTy)
      return Type();

    if (instanceTy.getPointer() == meta->getInstanceType().getPointer())
      return *this;

    if (meta->hasRepresentation())
      return MetatypeType::get(instanceTy, meta->getRepresentation());
    return MetatypeType::get(instanceTy);
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
    for (const auto &elt : tuple->getElements()) {
      Type eltTy = elt.getType().transform(fn);
      if (!eltTy)
        return Type();

      // If nothing has changed, just keep going.
      if (!anyChanged && eltTy.getPointer() == elt.getType().getPointer()) {
        ++Index;
        continue;
      }

      // If this is the first change we've seen, copy all of the previous
      // elements.
      if (!anyChanged) {
        // Copy all of the previous elements.
        for (unsigned I = 0; I != Index; ++I) {
          const TupleTypeElt &FromElt =tuple->getElement(I);
          elements.push_back(TupleTypeElt(FromElt.getType(), FromElt.getName(),
                                          FromElt.isVararg()));
        }

        anyChanged = true;
      }

      // Add the new tuple element, with the new type, no initializer,
      elements.push_back(TupleTypeElt(eltTy, elt.getName(), elt.isVararg()));
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

    if (auto assocType = dependent->getAssocType())
      return DependentMemberType::get(dependentBase, assocType,
                                      Ptr->getASTContext());

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

      if (firstType->hasTypeParameter() ||
          (secondType && secondType->hasTypeParameter())) {
        if (firstType.getPointer() != req.getFirstType().getPointer() ||
            secondType.getPointer() != req.getSecondType().getPointer())
          anyChanges = true;

        requirements.push_back(Requirement(req.getKind(), firstType,
                                           secondType));
      } else
        anyChanges = true;
    }
    
    auto sig = GenericSignature::get(genericParams, requirements);

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

  case TypeKind::ImplicitlyUnwrappedOptional: {
    auto optional = cast<ImplicitlyUnwrappedOptionalType>(base);
    auto baseTy = optional->getBaseType().transform(fn);
    if (!baseTy)
      return Type();

    if (baseTy.getPointer() == optional->getBaseType().getPointer())
      return *this;

    return ImplicitlyUnwrappedOptionalType::get(baseTy);
  }

  case TypeKind::Dictionary: {
    auto dict = cast<DictionaryType>(base);
    auto keyTy = dict->getKeyType().transform(fn);
    if (!keyTy)
      return Type();

    auto valueTy = dict->getValueType().transform(fn);
    if (!valueTy)
      return Type();

    if (keyTy.getPointer() == dict->getKeyType().getPointer() &&
        valueTy.getPointer() == dict->getValueType().getPointer())
      return *this;

    return DictionaryType::get(keyTy, valueTy);
  }

  case TypeKind::LValue: {
    auto lvalue = cast<LValueType>(base);
    auto objectTy = lvalue->getObjectType().transform(fn);
    if (!objectTy || objectTy->is<ErrorType>())
      return objectTy;

    return objectTy.getPointer() == lvalue->getObjectType().getPointer() ?
      *this : LValueType::get(objectTy);
  }

  case TypeKind::InOut: {
    auto inout = cast<InOutType>(base);
    auto objectTy = inout->getObjectType().transform(fn);
    if (!objectTy || objectTy->is<ErrorType>())
      return objectTy;
    
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


bool Type::findIf(llvm::function_ref<bool(Type)> pred) const {
  class Walker : public TypeWalker {
    llvm::function_ref<bool(Type)> Pred;
  public:
    explicit Walker(llvm::function_ref<bool(Type)> pred) : Pred(pred) {}

    virtual Action walkToTypePre(Type ty) override {
      if (Pred(ty))
        return Action::Stop;
      return Action::Continue;
    }
  };

  return walk(Walker(pred));
}

TypeTraitResult TypeBase::canBeClass() {
  // Any bridgeable object type can be a class.
  if (isBridgeableObjectType())
    return TypeTraitResult::Is;

  CanType self = getCanonicalType();

  // Dependent types might be bound to classes.
  if (isa<SubstitutableType>(self))
    return TypeTraitResult::CanBe;
  if (isa<DependentMemberType>(self))
    return TypeTraitResult::CanBe;
  
  return TypeTraitResult::IsNot;
}

bool Type::isPrivateStdlibType(bool whitelistProtocols) const {
  Type Ty = *this;
  if (!Ty)
    return false;

  // A 'public' typealias can have an 'internal' type.
  if (NameAliasType *NAT = dyn_cast<NameAliasType>(Ty.getPointer()))
    return NAT->getDecl()->isPrivateStdlibDecl(whitelistProtocols);

  if (auto Paren = dyn_cast<ParenType>(Ty.getPointer()))
    return Paren->getUnderlyingType().isPrivateStdlibType(whitelistProtocols);

  if (Type Unwrapped = Ty->getAnyOptionalObjectType())
    return Unwrapped.isPrivateStdlibType(whitelistProtocols);

  if (auto TyD = Ty->getAnyNominal())
    if (TyD->isPrivateStdlibDecl(whitelistProtocols))
      return true;

  return false;
}

bool UnownedStorageType::isLoadable(ResilienceExpansion resilience) const {
  return getReferentType()->usesNativeReferenceCounting(resilience);
}

static bool doesOpaqueClassUseNativeReferenceCounting(const ASTContext &ctx) {
  return !ctx.LangOpts.EnableObjCInterop;
}

static bool usesNativeReferenceCounting(ClassDecl *theClass,
                                        ResilienceExpansion resilience) {
  // NOTE: if you change this, change irgen::getReferenceCountingForClass.
  // TODO: Resilience? there might be some legal avenue of changing this.
  while (Type supertype = theClass->getSuperclass()) {
    theClass = supertype->getClassOrBoundGenericClass();
    assert(theClass);
  }
  return !theClass->hasClangNode();
}

bool TypeBase::usesNativeReferenceCounting(ResilienceExpansion resilience) {
  assert(allowsOwnership());

  CanType type = getCanonicalType();
  switch (type->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("sugared canonical type?");

  case TypeKind::BuiltinNativeObject:
  case TypeKind::SILBox:
    return true;

  case TypeKind::BuiltinUnknownObject:
  case TypeKind::BuiltinBridgeObject:
    return ::doesOpaqueClassUseNativeReferenceCounting(type->getASTContext());

  case TypeKind::Class:
    return ::usesNativeReferenceCounting(cast<ClassType>(type)->getDecl(),
                                         resilience);
  case TypeKind::BoundGenericClass:
    return ::usesNativeReferenceCounting(
                                  cast<BoundGenericClassType>(type)->getDecl(),
                                         resilience);

  case TypeKind::DynamicSelf:
    return cast<DynamicSelfType>(type).getSelfType()
             ->usesNativeReferenceCounting(resilience);

  case TypeKind::Archetype: {
    auto archetype = cast<ArchetypeType>(type);
    assert(archetype->requiresClass());
    if (auto supertype = archetype->getSuperclass())
      return supertype->usesNativeReferenceCounting(resilience);
    return ::doesOpaqueClassUseNativeReferenceCounting(type->getASTContext());
  }

  case TypeKind::Protocol:
  case TypeKind::ProtocolComposition:
    return ::doesOpaqueClassUseNativeReferenceCounting(type->getASTContext());

  case TypeKind::UnboundGeneric:
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
  case TypeKind::GenericFunction:
  case TypeKind::SILFunction:
  case TypeKind::SILBlockStorage:
  case TypeKind::Error:
  case TypeKind::Unresolved:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinUnsafeValueBuffer:
  case TypeKind::BuiltinVector:
  case TypeKind::Tuple:
  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Metatype:
  case TypeKind::ExistentialMetatype:
  case TypeKind::Module:
  case TypeKind::LValue:
  case TypeKind::InOut:
  case TypeKind::TypeVariable:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage:
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    llvm_unreachable("type is not a class reference");
  }

  llvm_unreachable("Unhandled type kind!");
}
