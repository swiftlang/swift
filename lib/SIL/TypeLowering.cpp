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
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SIL/TypeVisitor.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace Lowering;

Type Lowering::getThinFunctionType(Type t) {
  if (auto *ft = t->getAs<FunctionType>()) {
    return FunctionType::get(ft->getInput(), ft->getResult(),
                             ft->isAutoClosure(),
                             ft->isBlock(),
                             /*isThin*/ true,
                             ft->getASTContext());
  } else if (auto *pft = t->getAs<PolymorphicFunctionType>()) {
    return PolymorphicFunctionType::get(pft->getInput(), pft->getResult(),
                                        &pft->getGenericParams(),
                                        /*isThin*/ true,
                                        pft->getASTContext());
  } else {
    return t;
  }
}

Type Lowering::getThickFunctionType(Type t) {
  if (auto *fTy = t->getAs<FunctionType>()) {
    return FunctionType::get(fTy->getInput(),
                             fTy->getResult(),
                             fTy->isAutoClosure(),
                             fTy->isBlock(),
                             /*isThin*/ false,
                             fTy->getASTContext());
  } else if (auto *pfTy = t->getAs<PolymorphicFunctionType>()) {
    return PolymorphicFunctionType::get(pfTy->getInput(),
                                        pfTy->getResult(),
                                        &pfTy->getGenericParams(),
                                        /*isThin*/ false,
                                        pfTy->getASTContext());
  } else {
    return t;
  }
}

CaptureKind Lowering::getDeclCaptureKind(ValueDecl *capture) {
  if (VarDecl *var = dyn_cast<VarDecl>(capture)) {
    if (var->isProperty()) {
      return var->isSettable()
      ? CaptureKind::GetterSetter
      : CaptureKind::Getter;
    }
  }
  
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
  
static bool isAddressOnly(Type t) {
  // Archetypes and existentials are always address-only.
  // FIXME: Class archetypes and existentials will one day be representable as
  // reference types.
  // FIXME: resilient structs
  return t->is<ArchetypeType>() || t->isExistentialType();
}

namespace {
  enum Loadable_t { IsAddressOnly, IsLoadable };
}
  
/// LoadableTypeLoweringInfoVisitor - Recursively descend into fragile struct and
/// tuple types and visit their element types, storing information about the
/// reference type members in the TypeLoweringInfo for the type.
class Lowering::LoadableTypeLoweringInfoVisitor
  : public Lowering::TypeVisitor<LoadableTypeLoweringInfoVisitor, Loadable_t>
{
  TypeLoweringInfo &theInfo;
  ReferenceTypePath currentElement;
public:
  LoadableTypeLoweringInfoVisitor(TypeLoweringInfo &theInfo) : theInfo(theInfo) {}
  
  void pushPath() { currentElement.path.push_back({Type(), 0}); }
  void popPath() { currentElement.path.pop_back(); }
  void setPathType(Type t) { currentElement.path.back().type = t; }
  void advancePath() { ++currentElement.path.back().index; }
  
  Loadable_t visitType(TypeBase *t) {
    if (t->hasReferenceSemantics()) {
      theInfo.referenceTypeElements.push_back(currentElement);
      return IsLoadable;
    }
    if (isAddressOnly(t))
      return IsAddressOnly;
    return IsLoadable;
  }
  
  Loadable_t walkStructDecl(StructDecl *sd) {
    // FIXME: if this struct has a resilient attribute, mark the TypeLoweringInfo
    // as addressOnly and bail without checking fields.
    
    pushPath();
    for (Decl *d : sd->getMembers())
      if (VarDecl *vd = dyn_cast<VarDecl>(d))
        if (!vd->isProperty()) {
          CanType ct = vd->getType()->getCanonicalType();
          setPathType(ct);
          if (visit(ct) == IsAddressOnly) {
            return IsAddressOnly;
          }
          advancePath();
        }
    popPath();
    return IsLoadable;
  }
  
  Loadable_t visitBoundGenericType(BoundGenericType *gt) {
    if (StructDecl *sd = dyn_cast<StructDecl>(gt->getDecl()))
      return walkStructDecl(sd);
    else
      return this->visitType(gt);
  }
  
  Loadable_t visitNominalType(NominalType *t) {
    if (StructDecl *sd = dyn_cast<StructDecl>(t->getDecl()))
      return walkStructDecl(sd);
    else
      return this->visitType(t);
  }
  
  Loadable_t visitTupleType(TupleType *t) {
    pushPath();
    for (TupleTypeElt const &elt : t->getFields()) {
      CanType ct = elt.getType()->getCanonicalType();
      setPathType(ct);
      if (visit(ct) == IsAddressOnly) {
        return IsAddressOnly;
      }
      advancePath();
    }
    popPath();
    return IsLoadable;
  }
};
  
TypeConverter::TypeConverter(SILModule &m)
  : M(m), Context(m.getContext()) {
}

TypeConverter::~TypeConverter() {
  // The bump pointer allocator destructor will deallocate but not destroy all
  // our TypeLoweringInfos.
  for (auto &ti : types) {
    ti.second->~TypeLoweringInfo();
  }
}
  
void TypeConverter::makeLayoutForDecl(
                        SmallVectorImpl<SILCompoundTypeInfo::Element> &elements,
                        NominalTypeDecl *decl) {
  if (ClassDecl *cd = dyn_cast<ClassDecl>(decl)) {
    if (cd->hasBaseClass()) {
      makeLayoutForDecl(elements,
                        cd->getBaseClass()->getClassOrBoundGenericClass());
    }
  }
  
  for (Decl *d : decl->getMembers()) {
    if (VarDecl *vd = dyn_cast<VarDecl>(d)) {
      if (!vd->isProperty()) {
        // FIXME: specialize type of generic fields
        elements.push_back({getLoweredType(vd->getType()), vd});
      }
    }
  }
}
  
namespace {
  /// Recursively destructure tuple-type arguments into SIL argument types.
  class LoweredFunctionInputTypeVisitor
    : public Lowering::TypeVisitor<LoweredFunctionInputTypeVisitor>
  {
    TypeConverter &tc;
    SmallVectorImpl<SILType> &inputTypes;
  public:
    LoweredFunctionInputTypeVisitor(TypeConverter &tc,
                                    SmallVectorImpl<SILType> &inputTypes)
      : tc(tc), inputTypes(inputTypes) {}
    
    void visitType(TypeBase *t) {
      inputTypes.push_back(tc.getLoweredType(t));
    }
    
    void visitTupleType(TupleType *tt) {
      for (auto &field : tt->getFields()) {
        visit(field.getType()->getCanonicalType());
      }
    }
  };
} // end anonymous namespace
  
SILFunctionTypeInfo *TypeConverter::makeInfoForFunctionType(AnyFunctionType *ft,
                                                            AbstractCC cc,
                                                            unsigned uncurries)
{
  CanType topType(ft);
  SmallVector<SILType, 8> inputTypes;
  SmallVector<unsigned, 3> uncurriedInputCounts;
  // Destructure the uncurried input tuple types.
  for (;;) {
    LoweredFunctionInputTypeVisitor(*this, inputTypes)
      .visit(ft->getInput()->getCanonicalType());
    uncurriedInputCounts.push_back(inputTypes.size());
    if (uncurries-- == 0)
      break;
    ft = ft->getResult()->castTo<AnyFunctionType>();
  }
  
  // If the result type lowers to an address-only type, add it as an indirect
  // return argument.
  SILType resultType = getLoweredType(ft->getResult());
  bool hasIndirectReturn = resultType.isAddressOnly();
  if (hasIndirectReturn) {
    inputTypes.push_back(resultType);
    resultType = getEmptyTupleType();
  }
  
  return SILFunctionTypeInfo::create(topType,
                                     inputTypes,
                                     resultType,
                                     uncurriedInputCounts,
                                     hasIndirectReturn,
                                     cc,
                                     M);
}

SILTypeInfo *TypeConverter::makeSILTypeInfo(CanType t,
                                            AbstractCC cc,
                                            unsigned uncurryLevel) {
  //
  // Make a SILCompoundTypeInfo for struct or class types.
  NominalTypeDecl *ntd = nullptr;
  if (NominalType *nt = t->getAs<NominalType>()) {
    ntd = nt->getDecl();
  } else if (BoundGenericType *bgt = t->getAs<BoundGenericType>()) {
    ntd = bgt->getDecl();
  }

  if (ntd) {
    SmallVector<SILCompoundTypeInfo::Element, 4> compoundElements;
    // FIXME: record resilient attribute
    // FIXME: We need to apply subsitutions from bound generic types to the
    // generic field types.
    makeLayoutForDecl(compoundElements, ntd);
    return SILCompoundTypeInfo::create(t, compoundElements, M);
  }
  
  //
  // Make a SILCompoundTypeInfo for tuple types.
  if (TupleType *tt = t->getAs<TupleType>()) {
    SmallVector<SILCompoundTypeInfo::Element, 4> compoundElements;
    for (auto &elt : tt->getFields()) {
      compoundElements.push_back({getLoweredType(elt.getType()), nullptr});
    }
    return SILCompoundTypeInfo::create(t, compoundElements, M);
  }
  
  //
  // Make a SILFunctionTypeInfo for function types.
  if (AnyFunctionType *ft = t->getAs<AnyFunctionType>()) {
    return makeInfoForFunctionType(ft, cc, uncurryLevel);
  }
  
  //
  // Other types don't need any additional SILTypeInfo.
  return nullptr;
}
  
TypeLoweringInfo const &
TypeConverter::makeTypeLoweringInfo(CanType t,
                                    AbstractCC cc,
                                    unsigned uncurryLevel) {
  void *infoBuffer = TypeLoweringInfoBPA.Allocate<TypeLoweringInfo>();
  TypeLoweringInfo *theInfo = ::new (infoBuffer) TypeLoweringInfo();
  types[getTypeKey(t, cc, uncurryLevel)] = theInfo;
  bool address = false;
  bool addressOnly = false;
  
  if (LValueType *lvt = t->getAs<LValueType>()) {
    t = lvt->getObjectType()->getCanonicalType();
    address = true;
    addressOnly = getTypeLoweringInfo(t).isAddressOnly();
  } else if (t->hasReferenceSemantics()) {
    // Reference types are always loadable, and need only to retain/release
    // themselves.
    addressOnly = false;
    theInfo->referenceTypeElements.push_back(ReferenceTypePath());
  } else if (isAddressOnly(t)) {
    addressOnly = true;
  } else {
    // walk aggregate types to determine address-only-ness and find reference
    // type elements.
    addressOnly =
      LoadableTypeLoweringInfoVisitor(*theInfo).visit(t) == IsAddressOnly;
    if (addressOnly)
      theInfo->referenceTypeElements.clear();
  }

  // Derive SILType for LValueType from the object type.
  if (address) {
    theInfo->loweredType = getLoweredType(t, cc, uncurryLevel).getAddressType();
  }
  // Generate SILTypeInfo for lowered types that need it.
  else if (SILTypeInfo *info = makeSILTypeInfo(t, cc, uncurryLevel)) {
    theInfo->loweredType = SILType(info,
                                   /*address=*/ address || addressOnly,
                                   /*loadable=*/ !addressOnly);
  // If there's no SILTypeInfo, create a SILType from just the Swift type.
  } else {
    theInfo->loweredType = SILType(t,
                                   /*address=*/ address || addressOnly,
                                   /*loadable=*/ !addressOnly);
  }
  
  return *theInfo;
}
  
TypeLoweringInfo const &
TypeConverter::getTypeLoweringInfo(Type t,
                                   AbstractCC cc,
                                   unsigned uncurryLevel) {
  CanType ct = t->getCanonicalType();
  auto existing = types.find(getTypeKey(ct, cc, uncurryLevel));
  if (existing == types.end()) {
    return makeTypeLoweringInfo(ct, cc, uncurryLevel);
  } else
    return *existing->second;
}

static AbstractCC getAbstractCC(SILConstant c) {
  // Anonymous functions currently always have Freestanding CC.
  if (!c.hasDecl())
    return AbstractCC::Freestanding;
  
  // FIXME: We don't emit calls to ObjC properties correctly as class_method
  // dispatches yet.
  if (c.getDecl()->hasClangNode() &&
      c.kind != SILConstant::Kind::Getter &&
      c.kind != SILConstant::Kind::Setter)
    return AbstractCC::C;
  if (c.getDecl()->isInstanceMember() ||
      c.kind == SILConstant::Kind::Initializer)
    return AbstractCC::Method;
  return AbstractCC::Freestanding;
}

SILType TypeConverter::getConstantType(SILConstant constant) {
  auto found = constantTypes.find(constant);
  if (found == constantTypes.end()) {
    Type swiftTy = getThinFunctionType(makeConstantType(constant));
    AbstractCC cc = getAbstractCC(constant);
    SILType loweredTy
      = getTypeLoweringInfo(swiftTy, cc, constant.uncurryLevel).getLoweredType();
    DEBUG(llvm::dbgs() << "constant ";
          constant.print(llvm::dbgs());
          llvm::dbgs() << " has type ";
          loweredTy.print(llvm::dbgs());
          llvm::dbgs() << "\n");
    constantTypes[constant] = loweredTy;
    return loweredTy;
  } else
    return found->second;
}

/// Get the type of a property accessor, () -> T for a getter or (value:T) -> ()
/// for a setter.
Type TypeConverter::getPropertyType(SILConstant::Kind kind,
                                    Type valueType) const {
  if (kind == SILConstant::Kind::Getter) {
    return FunctionType::get(TupleType::getEmpty(Context), valueType, Context);
  }
  if (kind == SILConstant::Kind::Setter) {
    TupleTypeElt valueParam(valueType, Context.getIdentifier("value"));
    return FunctionType::get(TupleType::get(valueParam, Context),
                             TupleType::getEmpty(Context),
                             Context);
  }
  llvm_unreachable("not a property constant");
}

/// Get the type of a subscript accessor, Index -> PropertyAccessor.
Type TypeConverter::getSubscriptPropertyType(SILConstant::Kind kind,
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
  
  if (genericParams) {
    return PolymorphicFunctionType::get(thisType, methodType,
                                        genericParams,
                                        Context);
  }

  return FunctionType::get(thisType, methodType,
                           Context);
}

/// Get the type of a global variable accessor function, () -> [byref] T.
static Type getGlobalAccessorType(Type varType, ASTContext &C) {
  return FunctionType::get(TupleType::getEmpty(C),
                           LValueType::get(varType,
                                           LValueType::Qual::DefaultForType,
                                           C),
                           C);
}

/// Get the type of a destructor function, This -> ().
static Type getDestroyingDestructorType(ClassDecl *cd, ASTContext &C) {
  Type classType = cd->getDeclaredTypeInContext();

  if (cd->getGenericParams()) {
    return PolymorphicFunctionType::get(classType,
                                        C.TheObjectPointerType,
                                        cd->getGenericParams(),
                                        C);
  } else {
    return FunctionType::get(classType,
                             C.TheObjectPointerType,
                             C);
  }
}

static Type getFunctionTypeWithCaptures(TypeConverter &types,
                                        AnyFunctionType *funcType,
                                        ArrayRef<ValueDecl*> captures) {
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
                                           types.Context);
      inputFields.push_back(TupleTypeElt(lvType));
      break;
    }
    case CaptureKind::GetterSetter: {
      // Capture the setter and getter closures.
      Type setterTy = types.getPropertyType(SILConstant::Kind::Setter,
                                            capture->getType());
      inputFields.push_back(TupleTypeElt(setterTy));
      [[clang::fallthrough]];
    }
    case CaptureKind::Getter: {
      // Capture the getter closure.
      Type getterTy = types.getPropertyType(SILConstant::Kind::Getter,
                                            capture->getType());
      inputFields.push_back(TupleTypeElt(getterTy));
      break;
    }
    case CaptureKind::Box:
      // Capture the owning ObjectPointer and the address of the value.
      assert(capture->getTypeOfReference()->is<LValueType>() &&
             "lvalue capture not an lvalue?!");
      inputFields.push_back(types.Context.TheObjectPointerType);
      LValueType *lvType = LValueType::get(capture->getType(),
                                           LValueType::Qual::DefaultForType,
                                           types.Context);
      inputFields.push_back(TupleTypeElt(lvType));
      break;
    }
  }
  
  Type capturedInputs = TupleType::get(inputFields, types.Context);

  // FIXME: If there are generic captures, the archetypes from the context
  // need to become generic parameters of the closure.
  return FunctionType::get(capturedInputs,
                           funcType,
                           types.Context);
}

Type TypeConverter::makeConstantType(SILConstant c) {
  // FIXME: This needs to be cleaned up to switch on c.kind
  
  if (ValueDecl *vd = c.loc.dyn_cast<ValueDecl*>()) {
    Type /*nullable*/ contextType =
      vd->getDeclContext()->getDeclaredTypeOfContext();
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
      // If this is a subscript accessor, derive the accessor type.
      Type subscriptType = getSubscriptPropertyType(c.kind,
                                                    sd->getIndices()->getType(),
                                                    sd->getElementType());
      return getMethodTypeInContext(contextType, subscriptType, genericParams);
    } else {
      // If this is a destructor, derive the destructor type.
      if (c.kind == SILConstant::Kind::Destroyer) {
        return getDestroyingDestructorType(cast<ClassDecl>(vd), Context);
      }
      
      // If this is a constructor initializer, derive the initializer type.
      if (c.kind == SILConstant::Kind::Initializer) {
        return cast<ConstructorDecl>(vd)->getInitializerType();
      }
      
      // If this is a property accessor, derive the property type.
      if (c.isProperty()) {
        Type propertyType = getPropertyType(c.kind, vd->getType());
        Type propertyMethodType = getMethodTypeInContext(contextType,
                                                         propertyType,
                                                         genericParams);
        
        // If this is a local variable, its property methods may be closures.
        if (VarDecl *var = dyn_cast<VarDecl>(vd)) {
          if (var->isProperty()) {
            FuncDecl *property = c.kind == SILConstant::Kind::Getter
              ? var->getGetter()
              : var->getSetter();
            auto *propTy = propertyMethodType->castTo<AnyFunctionType>();
            return getFunctionTypeWithCaptures(*this,
                                           propTy,
                                           property->getCaptures());
          }
        }
        return propertyMethodType;
      }

      // If it's a global var, derive the initializer/accessor function type
      // () -> [byref] T
      if (VarDecl *var = dyn_cast<VarDecl>(vd)) {
        assert(!var->isProperty() && "constant ref to non-physical global var");
        return getGlobalAccessorType(var->getType(), Context);
      }
      
      // If it's a function, mangle the function type with its capture
      // arguments.
      if (FuncDecl *func = dyn_cast<FuncDecl>(vd)) {
        auto *funcTy = func->getTypeOfReference()->castTo<AnyFunctionType>();
        assert(c.kind == SILConstant::Kind::Func &&
               "non-Func SILConstant for function");
        return getFunctionTypeWithCaptures(*this, funcTy,
                                           func->getCaptures());
      }
      
      // Otherwise, return the Swift-level type.
      return vd->getTypeOfReference();
    }
  } else if (CapturingExpr *e = c.loc.dyn_cast<CapturingExpr*>()) {
    assert(c.kind == SILConstant::Kind::Func &&
           "non-Func SILConstant for local function");
    auto *funcTy = e->getType()->castTo<AnyFunctionType>();
    return getFunctionTypeWithCaptures(*this, funcTy, e->getCaptures());
  }
  llvm_unreachable("unexpected constant loc");
}
