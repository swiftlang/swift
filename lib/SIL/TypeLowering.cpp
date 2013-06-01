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
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SIL/TypeVisitor.h"
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
  
  DEBUG(llvm::dbgs() << "Bridging type " << moduleName << '.' << typeName
          << " mapped to ";
        if (t)
          t->print(llvm::dbgs());
        else
          llvm::dbgs() << "<null>";
        llvm::dbgs() << '\n');
  return t;
}

CanType TypeConverter::getStringType() {
  return getKnownType(StringTy, M.getASTContext(), "swift", "String");
}

CanType TypeConverter::getNSStringType() {
  return getKnownType(NSStringTy, M.getASTContext(), "Foundation", "NSString");
}

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
static Type getBridgedInputType(TypeConverter &tc,
                                AbstractCC cc,
                                Type input) {
  if (auto *tuple = input->getAs<TupleType>()) {
    SmallVector<TupleTypeElt, 4> bridgedFields;
    bool changed = false;
    for (TupleTypeElt const &field : tuple->getFields()) {
      Type bridged = tc.getLoweredBridgedType(field.getType(), cc);
      if (!bridged->isEqual(field.getType())) {
        changed = true;
        bridgedFields.push_back(TupleTypeElt(bridged,
                                             field.getName(),
                                             field.getInit(),
                                             field.getVarargBaseTy()));
      } else {
        bridgedFields.push_back(field);
      }
    }
    
    if (!changed)
      return input;
    return TupleType::get(bridgedFields, input->getASTContext());
  }
  
  return tc.getLoweredBridgedType(input, cc);
}

/// Bridge a result type.
static Type getBridgedResultType(TypeConverter &tc,
                                 AbstractCC cc,
                                 Type result) {
  return tc.getLoweredBridgedType(result, cc);
}

/// Fast path for bridging types in a function type without uncurrying.
static AnyFunctionType *getBridgedFunctionType(TypeConverter &tc,
                                               AnyFunctionType *t) {
  switch (t->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    // No bridging needed for native functions.
    return t;

  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    if (auto *pft = dyn_cast<PolymorphicFunctionType>(t)) {
      return PolymorphicFunctionType::get(
                  getBridgedInputType(tc, t->getAbstractCC(), t->getInput()),
                  getBridgedResultType(tc, t->getAbstractCC(), t->getResult()),
                  &pft->getGenericParams(),
                  t->isThin(),
                  t->getAbstractCC(),
                  t->getASTContext());
    }
    auto *ft = cast<FunctionType>(t);
    return FunctionType::get(
                getBridgedInputType(tc, t->getAbstractCC(), t->getInput()),
                getBridgedResultType(tc, t->getAbstractCC(), t->getResult()),
                ft->isAutoClosure(),
                ft->isBlock(),
                t->isThin(),
                t->getAbstractCC(),
                t->getASTContext());
  }
}

AnyFunctionType *TypeConverter::getUncurriedFunctionType(AnyFunctionType *t,
                                                       unsigned uncurryLevel) {
  if (uncurryLevel == 0)
    return getBridgedFunctionType(*this, t);
  
  AbstractCC outerCC = t->getAbstractCC();
  bool outerThinness = t->isThin();
  assert(!t->isAutoClosure() && "auto_closures cannot be curried");
  assert(!t->isBlock() && "objc blocks cannot be curried");
  
  // The uncurried input types.
  SmallVector<TupleTypeElt, 4> inputs;
  
  // The uncurried generic parameter list components.
  bool isPolymorphic = false;
  SmallVector<GenericParam, 4> genericParams;
  SmallVector<Requirement, 4> requirements;
  SmallVector<ArchetypeType *, 4> allArchetypes;
  GenericParamList *outerParameters = nullptr, *lastOuterParameters = nullptr;

  // Merge inputs and generic parameters from the uncurry levels.
  for (;;) {
    inputs.push_back(TupleTypeElt(t->getInput()));
    
    if (auto *pft = dyn_cast<PolymorphicFunctionType>(t)) {
      isPolymorphic = true;
      GenericParamList &params = pft->getGenericParams();
      if (GenericParamList *outer = params.getOuterParameters()) {
        if (!outerParameters)
          outerParameters = outer;
        assert((!lastOuterParameters || lastOuterParameters == outer)
               && "outer parameters do not nest");
        lastOuterParameters = outer;
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
    t = t->getResult()->castTo<AnyFunctionType>();
  }
  
  Type resultType = t->getResult();
  
  // Bridge input and result types.
  switch (outerCC) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    // Native functions don't need bridging.
    break;
  
  case AbstractCC::C:
    for (auto &input : inputs)
      input = getBridgedInputType(*this, outerCC, input.getType());
    resultType = getBridgedResultType(*this, outerCC, resultType);
    break;
  case AbstractCC::ObjCMethod:
    // The "this" parameter should not get bridged.
    for (auto &input : make_range(inputs.begin()+1, inputs.end()))
      input = getBridgedInputType(*this, outerCC, input.getType());
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
  ASTContext &C = t->getASTContext();
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
    
    return PolymorphicFunctionType::get(inputType, resultType,
                                        curriedGenericParams,
                                        outerThinness, outerCC, C);
  } else {
    return FunctionType::get(inputType, resultType,
                             /*autoClosure*/ false, /*block*/ false,
                             outerThinness, outerCC, C);
  }    
}

Type Lowering::getThinFunctionType(Type t, AbstractCC cc) {
  if (auto *ft = t->getAs<FunctionType>())
    return FunctionType::get(ft->getInput(), ft->getResult(),
                             ft->isAutoClosure(),
                             ft->isBlock(),
                             /*isThin*/ true,
                             cc,
                             ft->getASTContext());
  
  if (auto *pft = t->getAs<PolymorphicFunctionType>())
    return PolymorphicFunctionType::get(pft->getInput(), pft->getResult(),
                                        &pft->getGenericParams(),
                                        /*isThin*/ true,
                                        cc,
                                        pft->getASTContext());

  return t;
}

Type Lowering::getThinFunctionType(Type t) {
  return getThinFunctionType(t, t->castTo<AnyFunctionType>()->getAbstractCC());
}

Type Lowering::getThickFunctionType(Type t, AbstractCC cc) {
  if (auto *fTy = t->getAs<FunctionType>())
    return FunctionType::get(fTy->getInput(), fTy->getResult(),
                             fTy->isAutoClosure(),
                             fTy->isBlock(),
                             /*isThin*/ false,
                             cc,
                             fTy->getASTContext());
  
  if (auto *pfTy = t->getAs<PolymorphicFunctionType>())
    return PolymorphicFunctionType::get(pfTy->getInput(), pfTy->getResult(),
                                        &pfTy->getGenericParams(),
                                        /*isThin*/ false,
                                        cc,
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
  
/// LoadableTypeLoweringInfoVisitor - Recursively descend into fragile struct
/// and tuple types and visit their element types, storing information about the
/// reference type members in the TypeLoweringInfo for the type.
///
/// This is only invoked on loadable types.
///
class Lowering::LoadableTypeLoweringInfoVisitor
  : public Lowering::TypeVisitor<LoadableTypeLoweringInfoVisitor> {
  TypeLoweringInfo &theInfo;
  ReferenceTypePath currentElement;
public:
  LoadableTypeLoweringInfoVisitor(TypeLoweringInfo &theInfo)
    : theInfo(theInfo) {}
  
  void pushPath() { currentElement.path.push_back({}); }
  void popPath() { currentElement.path.pop_back(); }
  void setPath(ReferenceTypePath::Component c) {
    currentElement.path.back() = c;
  }
  
  void visitType(TypeBase *t) {
    if (t->hasReferenceSemantics())
      theInfo.referenceTypeElements.push_back(currentElement);
  }
  
  void walkStructDecl(StructDecl *sd) {
    pushPath();
    for (Decl *d : sd->getMembers())
      if (VarDecl *vd = dyn_cast<VarDecl>(d))
        if (!vd->isProperty()) {
          CanType ct = vd->getType()->getCanonicalType();
          setPath(ReferenceTypePath::Component::forStructField(ct, vd));
          visit(ct);
        }
    popPath();
  }
  
  void visitBoundGenericType(BoundGenericType *gt) {
    if (StructDecl *sd = dyn_cast<StructDecl>(gt->getDecl()))
      walkStructDecl(sd);
    else
      visitType(gt);
  }
  
  void visitNominalType(NominalType *t) {
    if (StructDecl *sd = dyn_cast<StructDecl>(t->getDecl()))
      walkStructDecl(sd);
    else
      this->visitType(t);
  }
  
  void visitTupleType(TupleType *t) {
    pushPath();
    unsigned i = 0;
    for (TupleTypeElt const &elt : t->getFields()) {
      CanType ct = elt.getType()->getCanonicalType();
      setPath(ReferenceTypePath::Component::forTupleElement(ct, i++));
      visit(ct);
    }
    popPath();
  }
};
  
TypeConverter::TypeConverter(SILModule &m)
  : M(m), Context(m.getASTContext()) {
}

TypeConverter::~TypeConverter() {
  // The bump pointer allocator destructor will deallocate but not destroy all
  // our TypeLoweringInfos.
  for (auto &ti : types) {
    ti.second->~TypeLoweringInfo();
  }
}
  
const TypeLoweringInfo &
TypeConverter::makeTypeLoweringInfo(CanType t, unsigned uncurryLevel) {
  void *infoBuffer = TypeLoweringInfoBPA.Allocate<TypeLoweringInfo>();
  TypeLoweringInfo *theInfo = ::new (infoBuffer) TypeLoweringInfo();
  types[getTypeKey(t, uncurryLevel)] = theInfo;
  
  // LValue types are a special case for lowering, because they get completely
  // removed, represented as 'address' SILTypes.
  if (LValueType *lvt = t->getAs<LValueType>()) {
    // Derive SILType for LValueType from the object type.
    t = lvt->getObjectType()->getCanonicalType();
    theInfo->loweredType = getLoweredType(t, uncurryLevel).getAddressType();
    return *theInfo;
  }
  
  bool addressOnly = SILType::isAddressOnly(t, M);
  if (t->hasReferenceSemantics()) {
    // Reference types need only to retain/release themselves.
    theInfo->referenceTypeElements.push_back(ReferenceTypePath());
  } else if (!addressOnly) {
    // Walk aggregate types to find reference type elements.
    LoadableTypeLoweringInfoVisitor(*theInfo).visit(t);
  }

  // Uncurry function types.
  if (AnyFunctionType *ft = t->getAs<AnyFunctionType>()) {
    assert(!addressOnly && "function types should never be address-only");
    auto *uncurried = getUncurriedFunctionType(ft, uncurryLevel);
    theInfo->loweredType = SILType(uncurried->getCanonicalType(),
                                   /*address=*/ false);
  } else {
    // Otherwise, the Swift type maps directly to a SILType.
    assert(uncurryLevel == 0 &&
           "non-function type cannot have an uncurry level");
    theInfo->loweredType = SILType(t, /*address=*/ addressOnly);
  }
  
  return *theInfo;
}
  
const TypeLoweringInfo &
TypeConverter::getTypeLoweringInfo(Type t, unsigned uncurryLevel) {
  CanType ct = t->getCanonicalType();
  auto existing = types.find(getTypeKey(ct, uncurryLevel));
  if (existing == types.end())
    return makeTypeLoweringInfo(ct, uncurryLevel);

  return *existing->second;
}

static bool isClassMethod(ValueDecl *vd) {
  if (!vd->getDeclContext())
    return false;
  if (!vd->getDeclContext()->getDeclaredTypeInContext())
    return false;
  return vd->getDeclContext()->getDeclaredTypeInContext()
    ->getClassOrBoundGenericClass();
}

static AbstractCC getAbstractCC(SILConstant c) {
  // If this is an ObjC thunk, it always has ObjC calling convention.
  if (c.isObjC)
    return c.hasDecl() && isClassMethod(c.getDecl())
      ? AbstractCC::ObjCMethod
      : AbstractCC::C;
  
  // Anonymous functions currently always have Freestanding CC.
  if (!c.hasDecl())
    return AbstractCC::Freestanding;
  
  // Assert that there is a native entry point available.
  // FIXME: We don't emit calls to ObjC properties correctly as class_method
  // dispatches yet.
  assert((!c.getDecl()->hasClangNode() ||
          c.kind == SILConstant::Kind::Getter ||
          c.kind == SILConstant::Kind::Setter)
         && "should not be referencing native entry point of foreign decl");

  if (c.getDecl()->isInstanceMember() ||
      c.kind == SILConstant::Kind::Initializer)
    return AbstractCC::Method;
  return AbstractCC::Freestanding;
}

SILType TypeConverter::getConstantType(SILConstant constant) {
  auto found = constantTypes.find(constant);
  if (found != constantTypes.end())
    return found->second;

  AbstractCC cc = getAbstractCC(constant);
  Type swiftTy = getThinFunctionType(makeConstantType(constant), cc);
  SILType loweredTy
    = getTypeLoweringInfo(swiftTy, constant.uncurryLevel).getLoweredType();
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
      Type setterTy = getPropertyType(SILConstant::Kind::Setter,
                                      capture->getType());
      inputFields.push_back(TupleTypeElt(setterTy));
      SWIFT_FALLTHROUGH;
    }
    case CaptureKind::Getter: {
      // Capture the getter closure.
      Type getterTy = getPropertyType(SILConstant::Kind::Getter,
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

Type TypeConverter::makeConstantType(SILConstant c) {
  ValueDecl *vd = c.loc.dyn_cast<ValueDecl*>();

  switch (c.kind) {
  case SILConstant::Kind::Func:
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

  case SILConstant::Kind::Getter:
  case SILConstant::Kind::Setter: {
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
        FuncDecl *property = c.kind == SILConstant::Kind::Getter
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
      
  case SILConstant::Kind::Allocator:
  case SILConstant::Kind::OneOfElement:
    return vd->getTypeOfReference();
  
  case SILConstant::Kind::Initializer:
    return cast<ConstructorDecl>(vd)->getInitializerType();
  
  case SILConstant::Kind::Destroyer:
    return getDestroyingDestructorType(cast<ClassDecl>(vd), Context);
  
  case SILConstant::Kind::GlobalAccessor: {
    VarDecl *var = cast<VarDecl>(vd);
    assert(!var->isProperty() && "constant ref to non-physical global var");
    return getGlobalAccessorType(var->getType(), Context);
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
    // Swift String maps to ObjC NSString.
    if (getStringType() && getNSStringType() && t->isEqual(getStringType()))
      return getNSStringType();
    return t;
  }
}
