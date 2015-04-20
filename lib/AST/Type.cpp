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

#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeVisitor.h"
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
using llvm::Fixnum;

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
    return true;

  // For Self types, recurse on the underlying type.
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

ArrayRef<Substitution> 
TypeBase::gatherAllSubstitutions(Module *module,
                                 SmallVectorImpl<Substitution> &scratchSpace,
                                 LazyResolver *resolver,
                                 DeclContext *gpContext) {
  Type type(this);
  SmallVector<ArrayRef<Substitution>, 2> allSubstitutions;
  scratchSpace.clear();

  while (type) {
    // Record the substitutions in a bound generic type.
    if (auto boundGeneric = type->getAs<BoundGenericType>()) {
      allSubstitutions.push_back(boundGeneric->getSubstitutions(module,
                                                                resolver,
                                                                gpContext));
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
                                                        LazyResolver *resolver,
                                                        DeclContext *gpContext){
  SmallVector<Substitution, 4> scratchSpace;
  auto subs = gatherAllSubstitutions(module, scratchSpace, resolver,
                                     gpContext);
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
  return Type(this).subst(module, substitutions, false,
                          getASTContext().getLazyResolver());
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
  return isEqual(getASTContext().TheEmptyTupleType);
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

Type TypeBase::getAnyPointerElementType(PointerTypeKind &PTK)  {
  if (auto boundTy = getAs<BoundGenericType>()) {
    auto &C = getASTContext();
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

static Type getStrippedType(const ASTContext &context, Type type,
                            bool stripLabels, bool stripDefaultArgs) {
  return type.transform([&](Type type) -> Type {
    auto *tuple = dyn_cast<TupleType>(type.getPointer());
    if (!tuple)
      return type;

    SmallVector<TupleTypeElt, 4> elements;
    bool anyChanged = false;
    unsigned idx = 0;
    for (const auto &elt : tuple->getElements()) {
      Type eltTy = getStrippedType(context, elt.getType(),
                                   stripLabels, stripDefaultArgs);
      if (anyChanged || eltTy.getPointer() != elt.getType().getPointer() ||
          (elt.hasInit() && stripDefaultArgs) ||
          (elt.hasName() && stripLabels)) {
        if (!anyChanged) {
          elements.reserve(tuple->getNumElements());
          for (unsigned i = 0; i != idx; ++i) {
            const TupleTypeElt &elt = tuple->getElement(i);
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
        !elements[0].hasName())
      return ParenType::get(context, elements[0].getType());
    
    return TupleType::get(elements, context);
  });
}

Type TypeBase::getUnlabeledType(ASTContext &Context) {
  return getStrippedType(Context, Type(this), /*labels=*/true,
                         /*defaultArgs=*/true);
}

Type TypeBase::getRelabeledType(ASTContext &ctx, 
                                ArrayRef<Identifier> labels) {
  if (auto tupleTy = dyn_cast<TupleType>(this)) {
    assert(labels.size() == tupleTy->getNumElements() && 
           "Wrong number of labels");
    SmallVector<TupleTypeElt, 4> elements;
    unsigned i = 0;
    bool anyChanged = false;
    for (const auto &elt : tupleTy->getElements()) {
      if (elt.getName() != labels[i])
        anyChanged = true;

      elements.push_back(TupleTypeElt(elt.getType(), labels[i], 
                                      elt.getDefaultArgKind(), elt.isVararg()));
      ++i;
    }

    if (!anyChanged)
      return this;

    return TupleType::get(elements, ctx);
  }

  // If there is no label, the type is unchanged.
  if (labels[0].empty())
    return this;

  // Create a one-element tuple to capture the label.
  TupleTypeElt elt(this, labels[0]);
  return TupleType::get(elt, ctx);  
}

Type TypeBase::getWithoutDefaultArgs(const ASTContext &Context) {
  return getStrippedType(Context, Type(this), /*labels=*/false,
                         /*defaultArgs=*/true);
}

Type TypeBase::getWithoutParens() {
  Type Ty = this;
  while (auto ParenTy = dyn_cast<ParenType>(Ty.getPointer()))
    Ty = ParenTy->getUnderlyingType();
  return Ty;
}

Type TypeBase::replaceCovariantResultType(Type newResultType,
                                          unsigned uncurryLevel) {
  if (uncurryLevel == 0) {
    // Preserve optionality of the result.
    assert(!newResultType->getAnyOptionalObjectType());
    if (auto boundGeneric = getAs<BoundGenericType>()) {
      switch (boundGeneric->getDecl()->classifyAsOptionalType()) {
      case OTK_None:
        return newResultType;
      case OTK_Optional:
        return OptionalType::get(newResultType);
      case OTK_ImplicitlyUnwrappedOptional:
        return ImplicitlyUnwrappedOptionalType::get(newResultType);
      }
      llvm_unreachable("bad optional type kind");
    }

    return newResultType;
  }

  // Determine the input and result types of this function.
  auto fnType = this->castTo<AnyFunctionType>();
  Type inputType = fnType->getInput();
  Type resultType =
    fnType->getResult()->replaceCovariantResultType(newResultType,
                                                    uncurryLevel - 1);
  
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

Type TypeBase::getWithoutNoReturn(unsigned UncurryLevel) {
  if (UncurryLevel == 0)
    return this;

  auto *FnType = this->castTo<AnyFunctionType>();
  Type InputType = FnType->getInput();
  Type ResultType = FnType->getResult()->getWithoutNoReturn(UncurryLevel - 1);
  auto TheExtInfo = FnType->getExtInfo().withIsNoReturn(false);
  if (auto *GFT = dyn_cast<GenericFunctionType>(FnType)) {
    return GenericFunctionType::get(GFT->getGenericSignature(),
                                    InputType, ResultType,
                                    TheExtInfo);
  }
  if (auto *PFT = dyn_cast<PolymorphicFunctionType>(FnType)) {
    return PolymorphicFunctionType::get(InputType, ResultType,
                                        &PFT->getGenericParams(),
                                        TheExtInfo);
  }
  return FunctionType::get(InputType, ResultType, TheExtInfo);
}

/// Retrieve the object type for a 'self' parameter, digging into one-element
/// tuples, lvalue types, and metatypes.
Type TypeBase::getRValueInstanceType() {
  Type type = this;
  
  // Look through argument list tuples.
  if (auto tupleTy = type->getAs<TupleType>()) {
    if (tupleTy->getNumElements() == 1 && !tupleTy->getElement(0).isVararg())
      type = tupleTy->getElementType(0);
  }
  
  if (auto metaTy = type->getAs<AnyMetatypeType>())
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

    if (Visited.insert(Proto->getDecl()).second) {
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
int ProtocolType::compareProtocols(ProtocolDecl * const* PP1,
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

ASTContext &GenericSignature::getASTContext(
                                ArrayRef<swift::GenericTypeParamType *> params,
                                ArrayRef<swift::Requirement> requirements) {
  // The params and requirements cannot both be empty.
  if (!params.empty())
    return params.front()->getASTContext();
  else
    return requirements.front().getFirstType()->getASTContext();
}

CanGenericSignature GenericSignature::getCanonicalSignature() {
  if (CanonicalSignatureOrASTContext.is<ASTContext*>())
    return CanGenericSignature(this);
  
  if (auto p = CanonicalSignatureOrASTContext.dyn_cast<GenericSignature*>())
    return CanGenericSignature(p);
  
  CanGenericSignature canSig = getCanonical(getGenericParams(),
                                            getRequirements());
  
  CanonicalSignatureOrASTContext = canSig;
  return canSig;
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
      
  case TypeKind::SILBlockStorage:
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
  case TypeKind::SILBlockStorage:
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
    if (fMe->isNoReturn() != fThem->isNoReturn())
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
      if (classDecl->hasSuperclass()) {
        // FIXME: Lame to rely on archetypes in the substitution below.
        superclassTy = ArchetypeBuilder::mapTypeIntoContext(
                         classDecl,
                         classDecl->getSuperclass());
        module = classDecl->getModuleContext();
        specializedTy = this;
      }
    }
  } else if (auto archetype = getAs<ArchetypeType>()) {
    return archetype->getSuperclass();
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
      auto archetype = gp[i]->getArchetype();
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
  } while ((ty = ty->getSuperclass(resolver)));
  return false;
}

static bool isBridgeableObjectType(CanType type) {
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

  // Class metatypes.
  if (auto metatype = dyn_cast<MetatypeType>(type)) {
    CanType instanceType = metatype.getInstanceType();
    return instanceType->mayHaveSuperclass();
  }

  // @objc protocol metatypes.
  if (auto metatype = dyn_cast<ExistentialMetatypeType>(type)) {
    return metatype.getInstanceType()->isObjCExistentialType();
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
  if (auto nominal = getAnyNominal()) {
    return isa<StructDecl>(nominal) || isa<EnumDecl>(nominal);
  }
  
  return false;
}

/// Is t1 not just a subtype of t2, but one such that its values are
/// trivially convertible to values of the other?
static bool canOverride(CanType t1, CanType t2,
                        bool allowUnsafeParameterOverride,
                        bool isParameter,
                        LazyResolver *resolver) {
  if (t1 == t2) return true;

  // Scalar-to-tuple and tuple-to-tuple.
  if (auto tuple2 = dyn_cast<TupleType>(t2)) {
    // We only ever look into singleton tuples on the RHS if we're
    // certain that the LHS isn't also a singleton tuple.
    auto tuple1 = dyn_cast<TupleType>(t1);
    if (!tuple1 || tuple1->getNumElements() != tuple2->getNumElements()) {
      if (tuple2->getNumElements() == 1)
        return canOverride(t1, tuple2.getElementType(0),
                           allowUnsafeParameterOverride, isParameter, resolver);
      return false;
    }

    for (auto i : indices(tuple1.getElementTypes())) {
      if (!canOverride(tuple1.getElementType(i),
                       tuple2.getElementType(i),
                       allowUnsafeParameterOverride, isParameter, resolver))
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
    return (canOverride(fn2.getInput(), fn1.getInput(),
                        allowUnsafeParameterOverride, true, resolver) &&
            canOverride(fn1.getResult(), fn2.getResult(),
                        allowUnsafeParameterOverride, false, resolver));
  }

  // Class-to-optional and class optional-to-optional.  Note that this
  // is not recursive: T? is not necessarily a trivial subtype of T??,
  // although it is a subtype.
  if (auto obj2 = t2.getAnyOptionalObjectType()) {
    // Value-to-optional.
    // FIXME: this isn't trivial on non-class types!  But we have to
    // allow it because we don't want the model to be specific to
    // bridged types.  We should generalize to arbitrary subtypes
    // and do the thunking necessary to make the model work.
    if (obj2 == t1)
      return true;

    // Optional-to-optional.
    if (auto obj1 = t1.getAnyOptionalObjectType()) {
      // Allow T? and T! to freely override one another.
      // We can assume that these types have the exact same
      // representation as long as the underlying types have exact
      // same representation constraints.  Unfortunately, subtyping
      // might give us more information and invalidate this!
      // Objective-C doesn't portably give more than a single level of
      // optional type without side-data, so the only *guarantee* can
      // take advantage of is a single level of class pointer.  So
      // really we need thunking for the same reason as above.
      return canOverride(obj1, obj2, allowUnsafeParameterOverride,
                         isParameter, resolver);
    }

    // Class-to-optional.
    return obj2->isSuperclassOf(t1, resolver);
  }

  // Allow T to override T! in certain cases.
  // FIXME: this should force a thunk that does the checking!
  if (allowUnsafeParameterOverride && isParameter) {
    if (auto obj1 = t1->getImplicitlyUnwrappedOptionalObjectType()) {
      t1 = obj1->getCanonicalType();
      if (t1 == t2) return true;
    }
  }

  // Class-to-class.
  return t2->isSuperclassOf(t1, resolver);
}

bool TypeBase::canOverride(Type other, bool allowUnsafeParameterOverride,
                           LazyResolver *resolver) {
  return ::canOverride(getCanonicalType(), other->getCanonicalType(),
                       allowUnsafeParameterOverride, false, resolver);
}

/// hasAnyDefaultValues - Return true if any of our elements has a default
/// value.
bool TupleType::hasAnyDefaultValues() const {
  for (const TupleTypeElt &Elt : Elements)
    if (Elt.hasInit())
      return true;
  return false;
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
    // Ignore fields with a default value.
    if (Elements[i].hasInit()) continue;
    
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
            conformanceType = protocolType->getDecl()->getProtocolSelf()
                                ->getArchetype();
          }
        }
        
        if (auto conformedArchetype = dyn_cast<ArchetypeType>(conformanceType)){
          return conformedArchetype->getNestedType(Name);
        }
      }
    }
  }
  
  assert(Pos != NestedTypes.end() && Pos->first == Name);
  return Pos->second;
}

bool ArchetypeType::hasNestedType(Identifier Name) const {
  auto Pos = std::lower_bound(NestedTypes.begin(), NestedTypes.end(), Name,
                              OrderArchetypeByName());
  return Pos != NestedTypes.end() && Pos->first == Name;
}

void
ArchetypeType::
setNestedTypes(ASTContext &Ctx,
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
  // declarations, and use that to buid the canonical composition type.
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

ArrayRef<ArchetypeType *> PolymorphicFunctionType::getAllArchetypes() const {
  return Params->getAllArchetypes();
}

FunctionType *PolymorphicFunctionType::substGenericArgs(Module *module,
                                                        ArrayRef<Type> args) {
  TypeSubstitutionMap map;
  for (auto &param : getGenericParams().getNestedGenericParams()) {
    map.insert(std::make_pair(param->getArchetype(), args.front()));
    args = args.slice(1);
  }
  
  assert(args.empty()
         && "number of args did not match number of generic params");
  
  Type input = getInput().subst(module, map, true, nullptr);
  Type result = getResult().subst(module, map, true, nullptr);
  return FunctionType::get(input, result, getExtInfo());
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

FunctionType *PolymorphicFunctionType::substGenericArgs(Module *module,
                                                  ArrayRef<Substitution> subs) {
  TypeSubstitutionMap map
    = getGenericParams().getSubstitutionMap(subs);
  
  Type input = getInput().subst(module, map, true, nullptr);
  Type result = getResult().subst(module, map, true, nullptr);
  return FunctionType::get(input, result, getExtInfo());
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
  auto appliedParams = params.slice(0, args.size());
  auto unappliedParams = params.slice(args.size());
  
  // Drop requirements rooted in the applied generic parameters.
  SmallVector<Requirement, 4> unappliedReqts;
  
  auto rootType = [](Type t) -> TypeBase* {
    while (auto dmt = t->getAs<DependentMemberType>()) {
      t = dmt->getBase();
    }
    return t.getPointer();
  };
  
  auto rootedInAppliedParam = [&](Type t) -> bool {
    return std::find(appliedParams.begin(), appliedParams.end(), rootType(t))
             != appliedParams.end();
  };
  
  for (auto &reqt : getRequirements()) {
    switch (reqt.getKind()) {
    case RequirementKind::Conformance:
    case RequirementKind::WitnessMarker:
      // Substituting the parameter eliminates conformance constraints rooted
      // in the parameter.
      if (rootedInAppliedParam(reqt.getFirstType()))
        continue;
      break;
        
    case RequirementKind::SameType: {
      // Same-type constraints are eliminated if both sides of the constraint
      // are rooted in substituted parameters.
      if (rootedInAppliedParam(reqt.getFirstType())
          && rootedInAppliedParam(reqt.getSecondType()))
        continue;
        
      // Otherwise, substitute the constrained types.
      unappliedReqts.push_back(Requirement(RequirementKind::SameType,
                           reqt.getFirstType().subst(M, subs, true, nullptr),
                           reqt.getSecondType().subst(M, subs, true, nullptr)));
      continue;
    }
    }
    unappliedReqts.push_back(reqt);
  }
  
  GenericSignature *sig = GenericSignature::get(unappliedParams, unappliedReqts);
  
  Type input = getInput().subst(M, subs, true, nullptr);
  Type result = getResult().subst(M, subs, true, nullptr);
  return GenericFunctionType::get(sig, input, result, getExtInfo());
}

TypeSubstitutionMap
GenericSignature::getSubstitutionMap(ArrayRef<Substitution> args) const {
  TypeSubstitutionMap subs;
  
  // An empty parameter list gives an empty map.
  if (getGenericParams().empty()) {
    assert(args.empty() && "substitutions but no generic params?!");
    return subs;
  }
  
  // Seed the type map with pre-existing substitutions.
  for (auto sub : args) {
    subs[sub.getArchetype()] = sub.getReplacement();
  }
  
  for (auto depTy : getAllDependentTypes()) {
    auto replacement = args.front().getReplacement();
    args = args.slice(1);
    
    if (auto subTy = depTy->getAs<SubstitutableType>()) {
      subs[subTy] = replacement;
    }
    else if (auto dTy = depTy->getAs<DependentMemberType>()) {
      subs[dTy] = replacement;
    }
  }
  
  assert(args.empty() && "did not use all substitutions?!");
  return subs;
}

FunctionType *
GenericFunctionType::substGenericArgs(Module *M, ArrayRef<Substitution> args) {
  auto params = getGenericParams();
  (void)params;
  assert(args.size() == params.size());
  
  TypeSubstitutionMap subs
    = getGenericSignature()->getSubstitutionMap(args);

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
    
    if (!archetypeParent->hasNestedType(name) &&
        archetypeParent->getParent()->isSelfDerived()) {
      return archetypeParent->getParent()->getNestedTypeValue(name);
    }
    
    return archetypeParent->getNestedTypeValue(name);
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
    if (substBase->is<GenericTypeParamType>() || substBase->is<DependentMemberType>()) {
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
      switch (conformance.getPointer()->getState()) {
      case ProtocolConformanceState::Invalid:
        return Type();

      case ProtocolConformanceState::Incomplete:
      case ProtocolConformanceState::Checking:
      case ProtocolConformanceState::Complete:
        return conformance.getPointer()->getTypeWitness(assocType,
                                                     resolver).getReplacement();
      }
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
  if (substBase->is<GenericTypeParamType>() ||
      substBase->is<DependentMemberType>()) {
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
      if (Type r = depMemTy->substBaseType(module, newBase, resolver)) {
        return r;
      }
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
    Type substParent = Type(parent).subst(module, substitutions, ignoreMissing,
                                          resolver);
    if (!substParent)
      return Type();

    // If the parent didn't change, we won't change.
    if (substParent.getPointer() == parent)
      return type;

    // Get the associated type reference from a child archetype.
    AssociatedTypeDecl *assocType = nullptr;
    if (auto archetype = substOrig->getAs<ArchetypeType>()) {
      assocType = archetype->getAssocType();
      
//      if (archetype == substOrig)
//        return type;
    }
    
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
    memberType = member->getInterfaceType();

  return getTypeOfMember(module, memberType, member->getDeclContext());
}

Type TypeBase::getTypeOfMember(Module *module, Type memberType,
                               DeclContext *memberDC) {
  // If the member is not part of a type, there's nothing to substitute.
  if (!memberDC->isTypeContext())
    return memberType;

  LazyResolver *resolver = memberDC->getASTContext().getLazyResolver();

  // Ignore lvalues in the base type.
  Type baseTy(getRValueType());

  // Look through the metatype; it has no bearing on the result.
  if (auto metaBase = baseTy->getAs<AnyMetatypeType>()) {
    baseTy = metaBase->getInstanceType()->getRValueType();
  }

  // If the member is part of a protocol or extension thereof, we need
  // to substitute in the type of Self.
  if (memberDC->isProtocolOrProtocolExtensionContext()) {
    // We only substitute into archetypes for now for protocols.
    // FIXME: This seems like an odd restriction. Whatever is depending on
    // this, shouldn't.
    if (!baseTy->is<ArchetypeType>() && isa<ProtocolDecl>(memberDC))
      return memberType;

    // FIXME: This feels painfully inefficient. We're creating a dense map
    // for a single substitution.
    TypeSubstitutionMap substitutions;
    substitutions[memberDC->getProtocolSelf()->getArchetype()] = baseTy;
    substitutions[memberDC->getProtocolSelf()->getDeclaredType()
                    ->getCanonicalType()->castTo<GenericTypeParamType>()]
      = baseTy;
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
  GenericParamList *curGenericParams = memberDC->getGenericParamsOfContext();
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

static bool transformSILResult(SILResultInfo &result, bool &changed,
                               const std::function<Type(Type)> &fn) {
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
                                  const std::function<Type(Type)> &fn) {
  Type transType = param.getType().transform(fn);
  if (!transType) return true;

  CanType canTransType = transType->getCanonicalType();
  if (canTransType != param.getType()) {
    changed = true;
    param = param.getWithType(canTransType);
  }
  return false;
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

  case TypeKind::SILFunction: {
    auto fnTy = cast<SILFunctionType>(base);
    bool changed = false;

    SILResultInfo transResult = fnTy->getResult();
    if (transformSILResult(transResult, changed, fn)) return Type();

    SmallVector<SILParameterInfo, 8> transInterfaceParams;
    for (SILParameterInfo param : fnTy->getParameters()) {
      if (transformSILParameter(param, changed, fn)) return Type();
      transInterfaceParams.push_back(param);
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
                                transResult,
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
          const TupleTypeElt &FromElt =tuple->getElement(I);
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

TypeTraitResult TypeBase::canBeClass() {
  // Any bridgeable object type can be a class.
  if (isBridgeableObjectType())
    return TypeTraitResult::Is;

  CanType self = getCanonicalType();
  if (auto m = dyn_cast<AnyMetatypeType>(self))
    self = m.getInstanceType();

  // Dependent types might be bound to classes.
  if (isa<SubstitutableType>(self))
    return TypeTraitResult::CanBe;
  if (isa<DependentMemberType>(self))
    return TypeTraitResult::CanBe;
  
  return TypeTraitResult::IsNot;
}

bool Type::isPrivateStdlibType() const {
  Type Ty = *this;
  if (!Ty)
    return false;

  // A 'public' typealias can have an 'internal' type.
  if (NameAliasType *NAT = dyn_cast<NameAliasType>(Ty.getPointer()))
    return NAT->getDecl()->isPrivateStdlibDecl();

  if (auto Paren = dyn_cast<ParenType>(Ty.getPointer()))
    return Paren->getUnderlyingType().isPrivateStdlibType();

  if (Type Unwrapped = Ty->getAnyOptionalObjectType())
    return Unwrapped.isPrivateStdlibType();

  if (auto TyD = Ty->getAnyNominal())
    if (TyD->isPrivateStdlibDecl())
      return true;

  return false;
}
