//===--- TypeLowering.cpp - Type information for SILGen ---------*- C++ -*-===//
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

#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace Lowering;

static CanType getKnownType(Optional<CanType> &cacheSlot,
                            ASTContext &C,
                            StringRef moduleName,
                            StringRef typeName) {
  CanType t = cacheSlot.cache([&] {
    Optional<UnqualifiedLookup> lookup
      = UnqualifiedLookup::forModuleAndName(C, moduleName, typeName);
    if (!lookup)
      return CanType();
    if (TypeDecl *typeDecl = lookup->getSingleTypeResult())
      return typeDecl->getDeclaredType()->getCanonicalType();
    return CanType();
  });

  assert(t && "bridging type not found but we got past the clang importer?!");
  
  DEBUG(llvm::dbgs() << "Bridging type " << moduleName << '.' << typeName
          << " mapped to ";
        if (t)
          t->print(llvm::dbgs());
        else
          llvm::dbgs() << "<null>";
        llvm::dbgs() << '\n');
  return t;
}

#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType) \
  CanType TypeConverter::get##BridgedType##Type() {         \
    return getKnownType(BridgedType##Ty, M.getASTContext(), \
                        #BridgedModule, #BridgedType);      \
  }                                                         \
  CanType TypeConverter::get##NativeType##Type() {          \
    return getKnownType(NativeType##Ty, M.getASTContext(),  \
                        #NativeModule, #NativeType);        \
  }
#include "swift/SIL/BridgedTypes.def"

UncurryDirection TypeConverter::getUncurryDirection(AbstractCC cc) {
  switch (cc) {
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    return UncurryDirection::LeftToRight;

  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    return UncurryDirection::RightToLeft;
  }
}

/// Bridge the elements of an input tuple type.
static CanType getBridgedInputType(TypeConverter &tc,
                                   AbstractCC cc,
                                   CanType input) {
  if (auto tuple = dyn_cast<TupleType>(input)) {
    SmallVector<TupleTypeElt, 4> bridgedFields;
    bool changed = false;
    for (auto &elt : tuple->getFields()) {
      CanType bridged = CanType(tc.getLoweredBridgedType(elt.getType(), cc));
      if (bridged != CanType(elt.getType())) {
        changed = true;
        bridgedFields.push_back(elt.getWithType(bridged));
      } else {
        bridgedFields.push_back(elt);
      }
    }
    
    if (!changed)
      return input;
    return CanType(TupleType::get(bridgedFields, input->getASTContext()));
  }
  
  return tc.getLoweredBridgedType(input, cc)->getCanonicalType();
}

/// Bridge a result type.
static CanType getBridgedResultType(TypeConverter &tc,
                                    AbstractCC cc,
                                    CanType result) {
  return tc.getLoweredBridgedType(result, cc)->getCanonicalType();
}

/// Fast path for bridging types in a function type without uncurrying.
static CanAnyFunctionType getBridgedFunctionType(TypeConverter &tc,
                                                 CanAnyFunctionType t) {
  switch (t->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    // No bridging needed for native functions.
    return t;

  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    if (auto pft = dyn_cast<PolymorphicFunctionType>(t)) {
      return CanAnyFunctionType(PolymorphicFunctionType::get(
                  getBridgedInputType(tc, t->getAbstractCC(), t.getInput()),
                  getBridgedResultType(tc, t->getAbstractCC(), t.getResult()),
                  &pft->getGenericParams(),
                  t->getExtInfo(),
                  t->getASTContext()));
    }
    auto ft = cast<FunctionType>(t);
    return CanAnyFunctionType(FunctionType::get(
                getBridgedInputType(tc, t->getAbstractCC(), t.getInput()),
                getBridgedResultType(tc, t->getAbstractCC(), t.getResult()),
                ft->getExtInfo(),
                t->getASTContext()));
  }
}

CanAnyFunctionType TypeConverter::getUncurriedFunctionType(CanAnyFunctionType t,
                                                        unsigned uncurryLevel) {
  if (uncurryLevel == 0)
    return getBridgedFunctionType(*this, t);

  AnyFunctionType::ExtInfo outerInfo = t->getExtInfo();
  AbstractCC outerCC = outerInfo.getCC();
  assert(!outerInfo.isAutoClosure() && "auto_closures cannot be curried");
  assert(!outerInfo.isBlock() && "objc blocks cannot be curried");
  
  // The uncurried input types.
  SmallVector<TupleTypeElt, 4> inputs;
  
  // The uncurried generic parameter list components.
  bool isPolymorphic = false;
  SmallVector<GenericParam, 4> genericParams;
  SmallVector<Requirement, 4> requirements;
  SmallVector<ArchetypeType *, 4> allArchetypes;
  GenericParamList *outerParameters = nullptr;

  // Merge inputs and generic parameters from the uncurry levels.
  for (;;) {
    inputs.push_back(TupleTypeElt(t->getInput()));
    
    if (auto pft = dyn_cast<PolymorphicFunctionType>(t)) {
      isPolymorphic = true;
      GenericParamList &params = pft->getGenericParams();
      if (GenericParamList *outer = params.getOuterParameters()) {
        if (!outerParameters)
          outerParameters = outer;
      }
      
      genericParams.append(params.getParams().begin(),
                           params.getParams().end());
      requirements.append(params.getRequirements().begin(),
                          params.getRequirements().end());
      allArchetypes.append(params.getAllArchetypes().begin(),
                           params.getAllArchetypes().end());
    }
    
    if (uncurryLevel-- == 0)
      break;
    t = cast<AnyFunctionType>(t.getResult());
  }
  
  CanType resultType = t.getResult();
  
  // Bridge input and result types.
  switch (outerCC) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    // Native functions don't need bridging.
    break;
  
  case AbstractCC::C:
    for (auto &input : inputs)
      input = input.getWithType(
               getBridgedInputType(*this, outerCC, CanType(input.getType())));
    resultType = getBridgedResultType(*this, outerCC, resultType);
    break;
  case AbstractCC::ObjCMethod:
    // The "this" parameter should not get bridged.
    for (auto &input : make_range(inputs.begin()+1, inputs.end()))
      input = input.getWithType(
                getBridgedInputType(*this, outerCC, CanType(input.getType())));
    resultType = getBridgedResultType(*this, outerCC, resultType);
    break;
  }
  
  // Put the inputs in the order expected by the calling convention.
  switch (getUncurryDirection(outerCC)) {
  case UncurryDirection::LeftToRight:
    break;
  case UncurryDirection::RightToLeft:
    std::reverse(inputs.begin(), inputs.end());
    break;
  }
  
  // Create the new function type.
  const ASTContext &C = t->getASTContext();
  Type inputType = TupleType::get(inputs, C);
  if (isPolymorphic) {
    auto *curriedGenericParams = GenericParamList::create(C,
                                                          SourceLoc(),
                                                          genericParams,
                                                          SourceLoc(),
                                                          requirements,
                                                          SourceLoc());
    curriedGenericParams->setAllArchetypes(C.AllocateCopy(allArchetypes));
    curriedGenericParams->setOuterParameters(outerParameters);
    
    return CanPolymorphicFunctionType(
           PolymorphicFunctionType::get(inputType, resultType,
                                        curriedGenericParams,
                                        outerInfo, C));
  } else {
    return CanFunctionType(FunctionType::get(inputType, resultType,
                                             outerInfo, C));
  }    
}

Type Lowering::getThinFunctionType(Type t, AbstractCC cc) {
  if (auto *ft = t->getAs<FunctionType>())
    return FunctionType::get(ft->getInput(), ft->getResult(),
                             ft->getExtInfo()
                               .withIsThin(true)
                               .withCallingConv(cc),
                             ft->getASTContext());
  
  if (auto *pft = t->getAs<PolymorphicFunctionType>())
    return PolymorphicFunctionType::get(pft->getInput(), pft->getResult(),
                                        &pft->getGenericParams(),
                                        pft->getExtInfo()
                                               .withIsThin(true)
                                               .withCallingConv(cc),
                                        pft->getASTContext());

  return t;
}

Type Lowering::getThinFunctionType(Type t) {
  return getThinFunctionType(t, t->castTo<AnyFunctionType>()->getAbstractCC());
}

Type Lowering::getThickFunctionType(Type t, AbstractCC cc) {
  if (auto *fTy = t->getAs<FunctionType>())
    return FunctionType::get(fTy->getInput(), fTy->getResult(),
                             fTy->getExtInfo()
                               .withIsThin(false)
                               .withCallingConv(cc),
                             fTy->getASTContext());
  
  if (auto *pfTy = t->getAs<PolymorphicFunctionType>())
    return PolymorphicFunctionType::get(pfTy->getInput(), pfTy->getResult(),
                                        &pfTy->getGenericParams(),
                                        pfTy->getExtInfo()
                                                .withIsThin(false)
                                                .withCallingConv(cc),
                                        pfTy->getASTContext());

  return t;
}

Type Lowering::getThickFunctionType(Type t) {
  return getThickFunctionType(t, t->castTo<AnyFunctionType>()->getAbstractCC());
}

CaptureKind Lowering::getDeclCaptureKind(ValueDecl *capture) {
  if (VarDecl *var = dyn_cast<VarDecl>(capture))
    if (var->isProperty())
      return var->isSettable()? CaptureKind::GetterSetter : CaptureKind::Getter;

  if (capture->getType()->is<LValueType>())
    return CaptureKind::Byref;
  if (capture->getTypeOfReference()->is<LValueType>()) {
    // FIXME: Not-used-as-lvalue captures can be captured by value.

    // If the capture has a fixed lifetime, we can pass it simply by reference.
    if (capture->hasFixedLifetime())
      return CaptureKind::Byref;
    // Otherwise, we need to pass a box.
    return CaptureKind::Box;
  }
  return CaptureKind::Constant;
}

enum class LoweredTypeKind {
  /// Trivial and loadable.
  Trivial,

  /// Non-trivial but still loadable.
  Scalar,

  /// Non-trivial and not loadable.
  AddressOnly
};

static LoweredTypeKind classifyType(CanType type, SILModule &M);

namespace {
  template <class Impl, class RetTy>
  class TypeClassifierBase : public CanTypeVisitor<Impl, RetTy> {
    SILModule &M;
    Impl &asDerived() { return *static_cast<Impl*>(this); }
  protected:
    TypeClassifierBase(SILModule &M) : M(M) {}
  public:
    RetTy handle(CanType type, LoweredTypeKind kind) {
      switch (kind) {
      case LoweredTypeKind::AddressOnly:
        return asDerived().handleAddressOnly(type);
      case LoweredTypeKind::Scalar:
        return asDerived().handleScalar(type);
      case LoweredTypeKind::Trivial:
        return asDerived().handleTrivial(type);
      }
      llvm_unreachable("bad type lowering kind");
    }

#define IMPL(TYPE, LOWERING)                            \
    RetTy visit##TYPE##Type(Can##TYPE##Type type) {     \
      return asDerived().handle##LOWERING(type);        \
    }

    IMPL(BuiltinInteger, Trivial)
    IMPL(BuiltinFloat, Trivial)
    IMPL(BuiltinRawPointer, Trivial)
    IMPL(BuiltinOpaquePointer, Trivial)
    IMPL(BuiltinObjectPointer, Scalar)
    IMPL(BuiltinObjCPointer, Scalar)
    IMPL(BuiltinVector, Trivial)
    IMPL(Class, Scalar)
    IMPL(BoundGenericClass, Scalar)
    IMPL(MetaType, Trivial)
    IMPL(AnyFunction, Scalar)
    IMPL(Array, AddressOnly) // who knows?
    IMPL(Module, Trivial)

#undef IMPL

    RetTy visitLValueType(CanLValueType type) {
      llvm_unreachable("shouldn't get an l-value type here");
    }

    RetTy visitReferenceStorageType(CanReferenceStorageType type) {
      switch (type->getOwnership()) {
      case Ownership::Strong:  llvm_unreachable("explicit strong ownership");
      case Ownership::Unowned: return asDerived().handleScalar(type);
      case Ownership::Weak:    return asDerived().handleAddressOnly(type);
      }
      llvm_unreachable("bad ownership kind");
    }

    // These types are address-only unless they're class-constrained.
    template <class T> RetTy visitAbstracted(T type) {
      if (type->requiresClass()) {
        return asDerived().handleScalar(type);
      } else {
        return asDerived().handleAddressOnly(type);        
      }
    }
    RetTy visitArchetypeType(CanArchetypeType type) {
      return visitAbstracted(type);
    }
    RetTy visitProtocolType(CanProtocolType type) {
      return visitAbstracted(type);
    }
    RetTy visitProtocolCompositionType(CanProtocolCompositionType type) {
      return visitAbstracted(type);
    }

    // Unions depend on their enumerators.
    RetTy visitUnionType(CanUnionType type) {
      return asDerived().visitAnyUnionType(type, type->getDecl());
    }
    RetTy visitBoundGenericUnionType(CanBoundGenericUnionType type) {
      return asDerived().visitAnyUnionType(type, type->getDecl());
    }
    RetTy visitAnyUnionType(CanType type, UnionDecl *D) {
      // FIXME
      return asDerived().handleTrivial(type);
    }

    // Structs depend on their physical fields.
    RetTy visitStructType(CanStructType type) {
      return asDerived().visitAnyStructType(type, type->getDecl());
    }
    RetTy visitBoundGenericStructType(CanBoundGenericStructType type) {
      return asDerived().visitAnyStructType(type, type->getDecl());
    }
    RetTy visitAnyStructType(CanType type, StructDecl *D) {
      // FIXME: if this struct is resilient to anybody, it needs to be
      // AddressOnly.
      auto structKind = LoweredTypeKind::Trivial;
      for (Decl *member : D->getMembers()) {
        VarDecl *field = dyn_cast<VarDecl>(member);
        if (!field || field->isProperty()) continue;

        CanType fieldType = field->getType()->getCanonicalType();
        auto fieldKind = classifyType(fieldType, M);
        structKind = std::max(structKind, fieldKind);
      }
      return asDerived().handle(type, structKind);
    }

    // Tuples depend on their elements.
    RetTy visitTupleType(CanTupleType type) {
      auto tupleKind = LoweredTypeKind::Trivial;
      for (auto eltType : type.getElementTypes()) {
        auto eltKind = classifyType(eltType, M);
        tupleKind = std::max(tupleKind, eltKind);
      }
      return asDerived().handle(type, tupleKind);
    }
  };

  class TypeClassifier :
      public TypeClassifierBase<TypeClassifier, LoweredTypeKind> {
  public:
    TypeClassifier(SILModule &M) : TypeClassifierBase(M) {}

    LoweredTypeKind handleScalar(CanType type) {
      return LoweredTypeKind::Scalar;
    }
    LoweredTypeKind handleTrivial(CanType type) {
      return LoweredTypeKind::Trivial;
    }
    LoweredTypeKind handleAddressOnly(CanType type) {
      return LoweredTypeKind::AddressOnly;
    }
  };
}

static LoweredTypeKind classifyType(CanType type, SILModule &M) {
  // FIXME: caching?
  return TypeClassifier(M).visit(type);
}

/// True if the type, or the referenced type of an address
/// type, is address-only.  For example, it could be a resilient struct or
/// something of unknown size.
bool SILType::isAddressOnly(CanType type, SILModule &M) {
  return classifyType(type, M) == LoweredTypeKind::AddressOnly;
}

namespace {
  /// A class for trivial, loadable types.
  class TrivialTypeLowering : public TypeLowering {
  public:
    TrivialTypeLowering(SILType type)
      : TypeLowering(type, IsTrivial, IsNotAddressOnly) {}
  };

  /// A class for non-trivial but loadable types.
  class ScalarTypeLowering : public TypeLowering {
  public:
    ScalarTypeLowering(SILType type)
      : TypeLowering(type, IsNotTrivial, IsNotAddressOnly) {}
  };

  /// A class for non-trivial, address-only types.
  class AddressOnlyTypeLowering : public TypeLowering {
  public:
    AddressOnlyTypeLowering(SILType type)
      : TypeLowering(type, IsNotTrivial, IsAddressOnly) {}
  };

  /// Build the appropriate TypeLowering subclass for the given type.
  class LowerType :
      public TypeClassifierBase<LowerType, const TypeLowering *> {
    TypeConverter &TC;
  public:
    LowerType(TypeConverter &TC) : TypeClassifierBase(TC.M), TC(TC) {}

    const TypeLowering *handleTrivial(CanType type) {
      auto silType = SILType::getPrimitiveType(type, false);
      return new (TC) TrivialTypeLowering(silType);
    }
  
    const TypeLowering *handleScalar(CanType type) {
      auto silType = SILType::getPrimitiveType(type, false);
      return new (TC) ScalarTypeLowering(silType);
    }

    const TypeLowering *handleAddressOnly(CanType type) {
      auto silType = SILType::getPrimitiveType(type, true);
      return new (TC) AddressOnlyTypeLowering(silType);
    }
  };
}

TypeConverter::TypeConverter(SILModule &m)
  : M(m), Context(m.getASTContext()) {
}

TypeConverter::~TypeConverter() {
  // The bump pointer allocator destructor will deallocate but not destroy all
  // our TypeLowerings.
  for (auto &ti : Types) {
    // Destroy only the unique entries.
    CanType srcType = CanType(ti.first.first);
    CanType mappedType = ti.second->getLoweredType().getSwiftRValueType();
    if (srcType == mappedType || isa<LValueType>(srcType))
      ti.second->~TypeLowering();
  }
}

void *TypeLowering::operator new(size_t size, TypeConverter &tc) {
  return tc.TypeLoweringBPA.Allocate<TypeLowering>();
}
  
const TypeLowering &
TypeConverter::getTypeLowering(Type origType, unsigned uncurryLevel) {
  CanType type = origType->getCanonicalType();
  auto key = getTypeKey(type, uncurryLevel);
  auto existing = Types.find(key);
  if (existing != Types.end()) {
    assert(existing->second && "reentered getTypeLowering");
    return *existing->second;
  }

  // LValue types are a special case for lowering, because they get completely
  // removed and represented as 'address' SILTypes.
  if (auto lvalueType = dyn_cast<LValueType>(type)) {
    // Derive SILType for LValueType from the object type.
    CanType objectType = lvalueType.getObjectType();
    SILType loweredType =
      getLoweredType(objectType, uncurryLevel).getAddressType();

    auto *theInfo = new (*this) TrivialTypeLowering(loweredType);
    Types[key] = theInfo;
    return *theInfo;
  }

  // Uncurry and lower function types.  This transformation is
  // essentially a kind of canonicalization, which makes it idempotent
  // at uncurry level 0.  We exploit that in our caching logic.
  if (auto fnType = dyn_cast<AnyFunctionType>(type)) {
    CanType loweredType = getUncurriedFunctionType(fnType, uncurryLevel);

    // If the lowering process changed the type, re-check the cache
    // and add a cache entry for the unlowered type.
    if (loweredType != type) {
      auto &typeInfo = getTypeLoweringForLoweredType(loweredType);
      Types[key] = &typeInfo;
      return typeInfo;
    }

    // If it didn't, use the standard logic.
  }

  // The Swift type directly corresponds to the lowered type; don't
  // re-check the cache.
  assert(uncurryLevel == 0);
  return getTypeLoweringForUncachedLoweredType(type);
}

const TypeLowering &
TypeConverter::getTypeLowering(SILType type) {
  return getTypeLoweringForLoweredType(type.getSwiftRValueType());
}

const TypeLowering &
TypeConverter::getTypeLoweringForLoweredType(CanType type) {
  assert(!isa<LValueType>(type) && "didn't lower out l-value type?");

  // Re-using uncurry level 0 is reasonable because our uncurrying
  // transforms are idempotent at this level.  This means we don't
  // need a ton of redundant entries in the map.
  auto key = getTypeKey(type, 0);
  auto existing = Types.find(key);
  if (existing != Types.end()) {
    assert(existing->second && "reentered getTypeLoweringForLoweredType");
    return *existing->second;
  }

  return getTypeLoweringForUncachedLoweredType(type);
}

/// Do type-lowering for a lowered type which is not already in the cache.
const TypeLowering &
TypeConverter::getTypeLoweringForUncachedLoweredType(CanType type) {
  auto key = getTypeKey(type, 0);
  assert(!Types.count(key) && "re-entrant or already cached");
  assert(!isa<LValueType>(type) && "didn't lower out l-value type?");

#ifndef NDEBUG
  // Catch reentrancy bugs.
  Types[key] = nullptr;
#endif

  auto *theInfo = LowerType(*this).visit(type);
  Types[key] = theInfo;
  return *theInfo;
}

static bool isClassOrProtocolMethod(ValueDecl *vd) {
  if (!vd->getDeclContext())
    return false;
  Type contextType = vd->getDeclContext()->getDeclaredTypeInContext();
  if (!contextType)
    return false;
  return contextType->getClassOrBoundGenericClass()
    || contextType->isClassExistentialType();
}

static AbstractCC getAbstractCC(SILDeclRef c) {
  // If this is an ObjC thunk, it always has ObjC calling convention.
  if (c.isObjC)
    return c.hasDecl() && isClassOrProtocolMethod(c.getDecl())
      ? AbstractCC::ObjCMethod
      : AbstractCC::C;
  
  // Anonymous functions currently always have Freestanding CC.
  if (!c.hasDecl())
    return AbstractCC::Freestanding;
  
  // Assert that there is a native entry point available.
  // FIXME: We don't emit calls to ObjC properties correctly as class_method
  // dispatches yet.
  assert((!c.getDecl()->hasClangNode() ||
          c.kind == SILDeclRef::Kind::Getter ||
          c.kind == SILDeclRef::Kind::Setter)
         && "should not be referencing native entry point of foreign decl");

  if (c.getDecl()->isInstanceMember() ||
      c.kind == SILDeclRef::Kind::Initializer)
    return AbstractCC::Method;
  return AbstractCC::Freestanding;
}

SILType TypeConverter::getConstantType(SILDeclRef constant) {
  auto found = constantTypes.find(constant);
  if (found != constantTypes.end())
    return found->second;

  AbstractCC cc = getAbstractCC(constant);
  Type swiftTy = getThinFunctionType(makeConstantType(constant), cc);
  SILType loweredTy
    = getTypeLowering(swiftTy, constant.uncurryLevel).getLoweredType();
  DEBUG(llvm::dbgs() << "constant ";
        constant.print(llvm::dbgs());
        llvm::dbgs() << " has type ";
        loweredTy.print(llvm::dbgs());
        llvm::dbgs() << " cc " << unsigned(cc) << "\n");
  constantTypes[constant] = loweredTy;
  return loweredTy;
}

/// Get the type of a property accessor, () -> T for a getter or (value:T) -> ()
/// for a setter.
Type TypeConverter::getPropertyType(SILDeclRef::Kind kind,
                                    Type valueType) const {
  if (kind == SILDeclRef::Kind::Getter) {
    return FunctionType::get(TupleType::getEmpty(Context), valueType, Context);
  }
  if (kind == SILDeclRef::Kind::Setter) {
    TupleTypeElt valueParam(valueType, Context.getIdentifier("value"));
    return FunctionType::get(TupleType::get(valueParam, Context),
                             TupleType::getEmpty(Context),
                             Context);
  }
  llvm_unreachable("not a property constant");
}

/// Get the type of a subscript accessor, Index -> PropertyAccessor.
Type TypeConverter::getSubscriptPropertyType(SILDeclRef::Kind kind,
                                             Type indexType,
                                             Type elementType) const {
  Type propertyType = getPropertyType(kind, elementType);
  return FunctionType::get(indexType, propertyType, Context);
}

/// Get the type of the 'this' parameter for methods of a type.
Type TypeConverter::getMethodThisType(Type thisType) const {
  if (thisType->hasReferenceSemantics()) {
    return thisType;
  } else {
    return LValueType::get(thisType, LValueType::Qual::DefaultForType, Context);
  }
}

Type TypeConverter::getMethodTypeInContext(Type /*nullable*/ contextType,
                                       Type methodType,
                                       GenericParamList *genericParams) const {
  if (!contextType)
    return methodType;
  Type thisType = getMethodThisType(contextType);
  
  if (genericParams)
    return PolymorphicFunctionType::get(thisType, methodType, genericParams,
                                        Context);

  return FunctionType::get(thisType, methodType, Context);
}

/// Get the type of a global variable accessor function, () -> [byref] T.
static Type getGlobalAccessorType(Type varType, ASTContext &C) {
  return FunctionType::get(TupleType::getEmpty(C),
                           LValueType::get(varType,
                                           LValueType::Qual::DefaultForType, C),
                           C);
}

/// Get the type of a default argument generator, () -> T.
static Type getDefaultArgGeneratorType(ValueDecl *vd, unsigned defaultArgIndex,
                                       ASTContext &context) {
  auto resultTy = vd->getDefaultArg(defaultArgIndex).second;
  assert(resultTy && "Didn't find default argument?");
  return FunctionType::get(TupleType::getEmpty(context), resultTy, context);
}

/// Get the type of a destructor function, This -> ().
static Type getDestroyingDestructorType(ClassDecl *cd, ASTContext &C) {
  Type classType = cd->getDeclaredTypeInContext();

  if (cd->getGenericParams())
    return PolymorphicFunctionType::get(classType,
                                        C.TheObjectPointerType,
                                        cd->getGenericParams(),
                                        C);

  return FunctionType::get(classType, C.TheObjectPointerType, C);
}

Type TypeConverter::getFunctionTypeWithCaptures(AnyFunctionType *funcType,
                                                ArrayRef<ValueDecl*> captures,
                                                DeclContext *parentContext) {
  assert(!funcType->isThin());
  if (captures.empty())
    return funcType;
  
  SmallVector<TupleTypeElt, 8> inputFields;

  for (ValueDecl *capture : captures) {
    switch (getDeclCaptureKind(capture)) {
    case CaptureKind::Constant:
      // Constants are captured by value.
      assert(!capture->getTypeOfReference()->is<LValueType>() &&
             "constant capture is an lvalue?!");
      inputFields.push_back(TupleTypeElt(capture->getType()));
      break;
    case CaptureKind::Byref: {
      // Capture the address.
      assert((capture->getType()->is<LValueType>() ||
              capture->hasFixedLifetime()) &&
             "byref capture not an lvalue or fixed-lifetime var?!");
      Type objectType
        = capture->getType()->getRValueType();
      LValueType *lvType = LValueType::get(objectType,
                                           LValueType::Qual::DefaultForType,
                                           Context);
      inputFields.push_back(TupleTypeElt(lvType));
      break;
    }
    case CaptureKind::GetterSetter: {
      // Capture the setter and getter closures.
      Type setterTy = getPropertyType(SILDeclRef::Kind::Setter,
                                      capture->getType());
      inputFields.push_back(TupleTypeElt(setterTy));
      SWIFT_FALLTHROUGH;
    }
    case CaptureKind::Getter: {
      // Capture the getter closure.
      Type getterTy = getPropertyType(SILDeclRef::Kind::Getter,
                                      capture->getType());
      inputFields.push_back(TupleTypeElt(getterTy));
      break;
    }
    case CaptureKind::Box:
      // Capture the owning ObjectPointer and the address of the value.
      assert(capture->getTypeOfReference()->is<LValueType>() &&
             "lvalue capture not an lvalue?!");
      inputFields.push_back(Context.TheObjectPointerType);
      LValueType *lvType = LValueType::get(capture->getType(),
                                           LValueType::Qual::DefaultForType,
                                           Context);
      inputFields.push_back(TupleTypeElt(lvType));
      break;
    }
  }
  
  Type capturedInputs = TupleType::get(inputFields, Context);
  
  // Capture generic parameters from the enclosing context.
  GenericParamList *genericParams = parentContext->getGenericParamsOfContext();
  if (genericParams)
    return PolymorphicFunctionType::get(capturedInputs, funcType,
                                        genericParams,
                                        Context);
  
  return FunctionType::get(capturedInputs, funcType, Context);
}

Type TypeConverter::makeConstantType(SILDeclRef c) {
  ValueDecl *vd = c.loc.dyn_cast<ValueDecl*>();

  switch (c.kind) {
  case SILDeclRef::Kind::Func:
    if (CapturingExpr *e = c.loc.dyn_cast<CapturingExpr*>()) {
      auto *funcTy = e->getType()->castTo<AnyFunctionType>();
      return getFunctionTypeWithCaptures(funcTy, e->getCaptures(),
                                         e->getParent());
    } else {
      FuncDecl *func = cast<FuncDecl>(vd);
      auto *funcTy = func->getTypeOfReference()->castTo<AnyFunctionType>();
      return getFunctionTypeWithCaptures(funcTy,
                                         func->getCaptures(),
                                         func->getDeclContext());
    }

  case SILDeclRef::Kind::Getter:
  case SILDeclRef::Kind::Setter: {
    Type contextType = vd->getDeclContext()->getDeclaredTypeOfContext();
    GenericParamList *genericParams = nullptr;
    if (contextType) {
      if (UnboundGenericType *ugt = contextType->getAs<UnboundGenericType>()) {
        // Bind the generic parameters.
        // FIXME: see computeThisType()
        genericParams = ugt->getDecl()->getGenericParams();
        contextType = vd->getDeclContext()->getDeclaredTypeInContext();
      }
    }
    
    if (SubscriptDecl *sd = dyn_cast<SubscriptDecl>(vd)) {
      Type subscriptType = getSubscriptPropertyType(c.kind,
                                                    sd->getIndices()->getType(),
                                                    sd->getElementType());
      return getMethodTypeInContext(contextType, subscriptType, genericParams);
    }

    Type propertyType = getPropertyType(c.kind, vd->getType());
    Type propertyMethodType = getMethodTypeInContext(contextType,
                                                     propertyType,
                                                     genericParams);
    
    // If this is a local variable, its property methods may be closures.
    if (VarDecl *var = dyn_cast<VarDecl>(c.getDecl())) {
      if (var->isProperty()) {
        FuncDecl *property = c.kind == SILDeclRef::Kind::Getter
          ? var->getGetter()
          : var->getSetter();
        auto *propTy = propertyMethodType->castTo<AnyFunctionType>();
        return getFunctionTypeWithCaptures(propTy,
                                           property->getCaptures(),
                                           var->getDeclContext());
      }
    }
    return propertyMethodType;
  }
      
  case SILDeclRef::Kind::Allocator:
  case SILDeclRef::Kind::UnionElement:
    return vd->getTypeOfReference();
  
  case SILDeclRef::Kind::Initializer:
    return cast<ConstructorDecl>(vd)->getInitializerType();
  
  case SILDeclRef::Kind::Destroyer:
    return getDestroyingDestructorType(cast<ClassDecl>(vd), Context);
  
  case SILDeclRef::Kind::GlobalAccessor: {
    VarDecl *var = cast<VarDecl>(vd);
    assert(!var->isProperty() && "constant ref to non-physical global var");
    return getGlobalAccessorType(var->getType(), Context);
  }
  case SILDeclRef::Kind::DefaultArgGenerator: {
    return getDefaultArgGeneratorType(vd, c.defaultArgIndex, Context);
  }
  }
}

Type TypeConverter::getLoweredBridgedType(Type t, AbstractCC cc) {  
  switch (cc) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    // No bridging needed for native CCs.
    return t;
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    // Map native types back to bridged types.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType) \
    if (t->isEqual(get##NativeType##Type()))                         \
      return get##BridgedType##Type();
#include "swift/SIL/BridgedTypes.def"
    return t;
  }
}
