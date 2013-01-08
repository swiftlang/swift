//===--- SILGenExpr.cpp - Implements Lowering of ASTs -> SIL for Exprs ----===//
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

#include "SILGen.h"
#include "swift/AST/AST.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/SIL/BBArgument.h"
#include "Initialization.h"
#include "LValue.h"
#include "ManagedValue.h"
#include "TypeInfo.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace Lowering;

namespace {
  class CleanupRValue : public Cleanup {
    Value rv;
  public:
    CleanupRValue(Value rv) : rv(rv) {}
    
    void emit(SILGenFunction &gen) override {
      gen.emitReleaseRValue(SILLocation(), rv);
    }
  };
  
  class CleanupMaterializeAllocation : public Cleanup {
    Value alloc;
  public:
    CleanupMaterializeAllocation(Value alloc) : alloc(alloc) {}
    
    void emit(SILGenFunction &gen) override {
      gen.B.createDeallocVar(SILLocation(), AllocKind::Stack, alloc);
    }
  };
  
  class CleanupMaterializeValue : public Cleanup {
    Value address;
  public:
    CleanupMaterializeValue(Value address) : address(address) {}
    
    void emit(SILGenFunction &gen) override {
      Value tmpValue = gen.B.createLoad(SILLocation(), address);
      gen.emitReleaseRValue(SILLocation(), tmpValue);
    }
  };
  
  class CleanupMaterializeAddressOnlyValue : public Cleanup {
    Value address;
  public:
    CleanupMaterializeAddressOnlyValue(Value address) : address(address) {}
    
    void emit(SILGenFunction &gen) override {
      gen.B.createDestroyAddr(SILLocation(), address);
    }
  };

  } // end anonymous namespace

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(Value v) {
  if (v.getType().isAddress() ||
      getTypeInfo(v.getType().getSwiftType()).isTrivial()) {
    return ManagedValue(v);
  } else {
    Cleanups.pushCleanup<CleanupRValue>(v);
    return ManagedValue(v, getCleanupsDepth());
  }
}

static void initializeWithResult(SILGenFunction &gen,
                                 SILLocation loc,
                                 Initialization *I,
                                 ManagedValue result) {
  if (I->hasAddress()) {
    if (result.getType().isAddressOnly())
      result.forwardInto(gen, loc, I->getAddress(), /*isInitialize=*/true);
    else {
      gen.B.createStore(loc, result.forward(gen), I->getAddress());
    }
    I->finishInitialization(gen);
    return;
  }
  
  // Destructure the tuple result into the subinitializations.
  // FIXME: address-only tuples
  // FIXME: visitTupleExpr and friends could destructure their subexpressions
  // directly into the subinitializations.
  ArrayRef<Initialization*> subInits = I->getSubInitializations();
  if (subInits.empty()) {
    // The initialization is a black hole or an empty tuple, in either case
    // we can do nothing and drop the result on the floor.
    return;
  }
  
  Value resultV = result.forward(gen);
  TupleType *tty = resultV.getType().getAs<TupleType>();
  
  assert(tty && "tuple initialization for non-tuple result?!");
  assert(tty->getFields().size() == subInits.size() &&
         "tuple initialization size does not match tuple size?!");
  for (size_t i = 0; i < subInits.size(); ++i) {
    SILType eltTy = gen.getLoweredLoadableType(tty->getFields()[i].getType());
    ManagedValue elt = ManagedValue(gen.B.createExtract(loc, resultV, i,
                                                        eltTy));
    initializeWithResult(gen, loc, subInits[i], elt);
  }
  I->finishInitialization(gen);
}

void SILGenFunction::emitExprInto(Expr *E, Initialization *I) {
  // FIXME: actually emit into the initialization. The initialization should
  // be passed down in the context argument to visit, and it should be the
  // visit*Expr method's responsibility to store to it if possible.
  ManagedValue result = visit(E);
  initializeWithResult(*this, E, I, result);
}

ManagedValue SILGenFunction::visit(swift::Expr *E) {
  return visit(E, SGFContext());
}

ManagedValue SILGenFunction::visitExpr(Expr *E, SGFContext C) {
  E->dump();
  llvm_unreachable("Not yet implemented");
}

namespace {

static Value emitApplyArgument(SILGenFunction &gen, Expr *arg,
                                llvm::SmallVectorImpl<Writeback> &writebacks) {
  if (arg->getType()->is<LValueType>()) {
    // Construct a logical lvalue for the byref argument, materialize the
    // lvalue, and arrange for it to be written back after the call.
    LValue lv = SILGenLValue(gen).visit(arg);
    Value addr = gen.emitMaterializedLoadFromLValue(arg, lv)
      .getUnmanagedValue();
    if (!lv.isPhysical()) {
      writebacks.push_back({::std::move(lv), addr});
    }
    return addr;
  } else {
    ManagedValue argV = gen.visit(arg);
    if (argV.isAddressOnlyValue()) {
      // Address-only arguments are passed by address. The callee does not take
      // ownership.
      return argV.getValue();
    } else {
      // Loadable arguments are passed by value, and the callee takes ownership.
      return argV.forward(gen);
    }
  }
}
  
static bool isSingleElementTupleType(Type t) {
  TupleType *tt = t->getAs<TupleType>();
  return tt && tt->getFields().size() == 1;
}
  
} // end anonymous namespace

void SILGenFunction::emitApplyArguments(Expr *argsExpr,
                               llvm::SmallVectorImpl<Value> &ArgsV,
                               llvm::SmallVectorImpl<Writeback> &writebacks) {
  if (ParenExpr *pe = dyn_cast<ParenExpr>(argsExpr))
    argsExpr = pe->getSubExpr();
  
  // Special case Arg being a TupleExpr or single-element ScalarToTupleExpr to
  // inline the arguments and not create a tuple instruction.
  if (TupleExpr *te = dyn_cast<TupleExpr>(argsExpr)) {
    for (auto arg : te->getElements())
      ArgsV.push_back(emitApplyArgument(*this, arg, writebacks));
  } else if (isa<ScalarToTupleExpr>(argsExpr) &&
             isSingleElementTupleType(argsExpr->getType())) {
    ArgsV.push_back(emitApplyArgument(*this,
                            dyn_cast<ScalarToTupleExpr>(argsExpr)->getSubExpr(),
                            writebacks));
  } else {
    ArgsV.push_back(emitApplyArgument(*this, argsExpr, writebacks));
  }
}

ManagedValue SILGenFunction::emitApply(SILLocation Loc,
                                       Value Fn, ArrayRef<Value> Args) {
  FunctionType *fty = Fn.getType().getAs<FunctionType>();
  TypeInfo const &resultTI = getTypeInfo(fty->getResult());
  if (resultTI.isAddressOnly()) {
    // Allocate a temporary to house the indirect return, and pass it to the
    // function as an implicit argument.
    // FIXME: If Args is a prepackaged tuple argument, we need to explode it.
    SmallVector<Value, 4> argsWithReturn(Args.begin(), Args.end());
    Value buffer = B.createAllocVar(Loc, AllocKind::Stack,
                                    resultTI.getLoweredType());
    Cleanups.pushCleanup<CleanupMaterializeAllocation>(buffer);
    argsWithReturn.push_back(buffer);
    B.createApply(Loc, Fn, SILType::getEmptyTupleType(F.getContext()),
                  argsWithReturn);
    Cleanups.pushCleanup<CleanupMaterializeAddressOnlyValue>(buffer);
    return ManagedValue(buffer, getCleanupsDepth(),
                        /*isAddressOnlyValue=*/true);
  } else {
    // Receive the result by value.
    Value result = B.createApply(Loc, Fn, resultTI.getLoweredType(), Args);
    return emitManagedRValueWithCleanup(result);
  }
}

ManagedValue SILGenFunction::visitApplyExpr(ApplyExpr *E, SGFContext C) {
  Value FnV = visit(E->getFn()).forward(*this);
  llvm::SmallVector<Value, 10> ArgsV;
  llvm::SmallVector<Writeback, 2> writebacks;
  
  emitApplyArguments(E->getArg(), ArgsV, writebacks);
  
  ManagedValue r = emitApply(E, FnV, ArgsV);

  // FIXME: writebacks need to be applied later, perhaps at full expr scope
  // exit, in order for method writeback to do the right thing, because
  // foo.bar.bas() could really be bas(get_bar(&foo)), and modifications by
  // bas() ought to show up in foo's bar.
  for (auto &wb : writebacks) {
    Value newValue = B.createLoad(E, wb.tempAddress);
    emitAssignToLValue(E, emitManagedRValueWithCleanup(newValue), wb.lvalue);
  }
  
  return r;
}

Value SILGenFunction::emitGlobalConstantRef(SILLocation loc,
                                            SILConstant constant) {
  return B.createConstantRef(loc, constant, SGM.getConstantType(constant));
}

Value SILGenFunction::emitUnmanagedConstantRef(SILLocation loc,
                                               SILConstant constant) {
  // If this is a reference to a local constant, grab it.
  if (LocalConstants.count(constant)) {
    return LocalConstants[constant];
  }
  
  // Otherwise, use a global ConstantRefInst.
  return emitGlobalConstantRef(loc, constant);
}

ManagedValue SILGenFunction::emitConstantRef(SILLocation loc,
                                             SILConstant constant) {
  // If this is a reference to a local constant, grab it.
  if (LocalConstants.count(constant)) {
    Value v = LocalConstants[constant];
    emitRetainRValue(loc, v);
    return emitManagedRValueWithCleanup(v);
  }
  
  // Otherwise, use a global ConstantRefInst.
  Value c = emitGlobalConstantRef(loc, constant);
  return ManagedValue(c);
}

ManagedValue SILGenFunction::emitReferenceToDecl(SILLocation loc,
                                                 ValueDecl *decl) {
  // If this is a reference to a type, produce a metatype.
  if (isa<TypeDecl>(decl)) {
    assert(decl->getType()->is<MetaTypeType>() &&
           "type declref does not have metatype type?!");
    return ManagedValue(B.createMetatype(loc, getLoweredType(decl->getType())));
  }
  
  // If this is a reference to a var, produce an address.
  if (VarDecl *var = dyn_cast<VarDecl>(decl)) {
    // If it's a property, invoke its getter.
    if (var->isProperty()) {
      ManagedValue get = emitConstantRef(loc,
                                        SILConstant(decl, SILConstant::Getter));
      return ManagedValue(emitGetProperty(loc, get).address);
    }
    
    // For local decls, we should have the address we allocated on hand.
    if (VarLocs.count(decl)) {
      return ManagedValue(VarLocs[decl].address);
    }
    assert(!decl->getDeclContext()->isLocalContext() &&
           "no location for local var!");
    // If this is a global variable, invoke its accessor function to get its
    // address.
    Value accessor = emitGlobalConstantRef(loc, SILConstant(decl));
    return emitApply(loc, accessor, {});
  }
  
  // If the referenced decl isn't a VarDecl, it should be a constant of some
  // sort.
  assert(!decl->getTypeOfReference()->is<LValueType>() &&
         "unexpected lvalue decl ref?!");

  return emitConstantRef(loc, SILConstant(decl));
}

ManagedValue SILGenFunction::visitDeclRefExpr(DeclRefExpr *E, SGFContext C) {
  return emitReferenceToDecl(E, E->getDecl());
}

ManagedValue SILGenFunction::visitIntegerLiteralExpr(IntegerLiteralExpr *E,
                                                     SGFContext C) {
  return ManagedValue(B.createIntegerLiteral(E));
}
ManagedValue SILGenFunction::visitFloatLiteralExpr(FloatLiteralExpr *E,
                                                   SGFContext C) {
  return ManagedValue(B.createFloatLiteral(E));
}
ManagedValue SILGenFunction::visitCharacterLiteralExpr(CharacterLiteralExpr *E,
                                                       SGFContext C)
{
  return ManagedValue(B.createIntegerLiteral(E));
}
ManagedValue SILGenFunction::visitStringLiteralExpr(StringLiteralExpr *E,
                                                    SGFContext C) {
  return ManagedValue(B.createStringLiteral(E));
}

ManagedValue SILGenFunction::visitLoadExpr(LoadExpr *E, SGFContext C) {
  ManagedValue SubV = visit(E->getSubExpr());
  TypeInfo const &ti = getTypeInfo(E->getType());
  if (ti.isAddressOnly()) {
    // We can't load address-only types. Just pass on the address.
    return ManagedValue(SubV.getUnmanagedValue(),
                        /*isAddressOnlyValue=*/ true);
  }
  Value loadedV = B.createLoad(E, SubV.getUnmanagedValue());
  emitRetainRValue(E, loadedV);
  return emitManagedRValueWithCleanup(loadedV);
}

namespace {
  static Materialize emitMaterialize(SILGenFunction &gen,
                                     SILLocation loc, ManagedValue v) {
    // Address-only values are already materialized.
    if (v.isAddressOnlyValue()) {
      return Materialize{v.getValue(), v.getCleanup(),
                         /*isAddressOnlyValue=*/ true};
    }
    
    assert(!v.getType().isAddress() &&
           "can't materialize a reference");
    
    Value tmpMem = gen.B.createAllocVar(loc, AllocKind::Stack, v.getType());
    gen.Cleanups.pushCleanup<CleanupMaterializeAllocation>(tmpMem);
    gen.B.createStore(loc, v.forward(gen), tmpMem);
    
    CleanupsDepth valueCleanup = CleanupsDepth::invalid();
    if (!gen.getTypeInfo(v.getType().getSwiftType()).isTrivial()) {
      gen.Cleanups.pushCleanup<CleanupMaterializeValue>(tmpMem);
      valueCleanup = gen.getCleanupsDepth();
    }
    
    return Materialize{tmpMem, valueCleanup,
                       /*isAddressOnlyValue=*/ false};
  }
}

ManagedValue Materialize::consume(SILGenFunction &gen, SILLocation loc) {
  assert(!isAddressOnlyValue &&
         "address-only value must be consumed with consumeInto");
  if (valueCleanup.isValid())
    gen.Cleanups.setCleanupState(valueCleanup, CleanupState::Dead);
  return gen.emitManagedRValueWithCleanup(gen.B.createLoad(loc, address));
}

ManagedValue SILGenFunction::visitMaterializeExpr(MaterializeExpr *E,
                                                  SGFContext C) {
  // Evaluate the value, then use it to initialize a new temporary and return
  // the temp's address.
  ManagedValue V = visit(E->getSubExpr());
  return ManagedValue(emitMaterialize(*this, E, V).address);
}

static ManagedValue emitImplicitConvert(SILGenFunction &gen,
                                        ImplicitConversionExpr *e) {
  ManagedValue original = gen.visit(e->getSubExpr());
  Value converted = gen.B.createImplicitConvert(e,
                                        original.getValue(),
                                        gen.getLoweredType(e->getType()));
  return ManagedValue(converted, original.getCleanup());
}

ManagedValue SILGenFunction::visitDerivedToBaseExpr(DerivedToBaseExpr *E,
                                                    SGFContext C) {
  return emitImplicitConvert(*this, E);
}

ManagedValue SILGenFunction::visitMetatypeConversionExpr(
                                                   MetatypeConversionExpr *E,
                                                   SGFContext C) {
  visit(E->getSubExpr());
  return ManagedValue(B.createMetatype(E,
                                       getLoweredLoadableType(E->getType())));
}

ManagedValue SILGenFunction::visitArchetypeToSuperExpr(
                                               swift::ArchetypeToSuperExpr *E,
                                               SGFContext C) {
  ManagedValue archetype = visit(E->getSubExpr());
  Value base = B.createArchetypeToSuper(E,
                                        archetype.getValue(),
                                        getLoweredLoadableType(E->getType()));
  if (archetype.hasCleanup()) {
    // If an archetype has a base class, then it must have been of some
    // reference type, so releasing the resulting class-type value is equivalent
    // to destroying the archetype. We can thus "take" the archetype and disable
    // its destructor cleanup if it was a temporary.
    Cleanups.setCleanupState(archetype.getCleanup(), CleanupState::Dead);
  } else {
    // The archetype isn't a temporary, so we need our own retain.
    emitRetainRValue(E, base);
  }
  return emitManagedRValueWithCleanup(base);
}

ManagedValue SILGenFunction::visitRequalifyExpr(RequalifyExpr *E,
                                                SGFContext C) {
  if (E->getType()->is<LValueType>()) {
    // Ignore lvalue qualifiers.
    return visit(E->getSubExpr());
  }
  return emitImplicitConvert(*this, E);
}

ManagedValue SILGenFunction::visitFunctionConversionExpr(
                                                      FunctionConversionExpr *E,
                                                      SGFContext C)
{
  return emitImplicitConvert(*this, E);
}

ManagedValue SILGenFunction::visitErasureExpr(ErasureExpr *E, SGFContext C) {
  ManagedValue concrete = visit(E->getSubExpr());
  // Allocate the existential.
  Value existential = B.createAllocVar(E, AllocKind::Stack,
                                       getLoweredType(E->getType()));
  Cleanups.pushCleanup<CleanupMaterializeAllocation>(existential);
  // Allocate its internal value.
  Value valueAddr = B.createInitExistential(E, existential,
                                             concrete.getType());
  // Initialize the internal value.
  if (concrete.getType().isAddressOnly()) {
    concrete.forwardInto(*this, E, valueAddr,
                         /*isInitialize=*/true);
  } else {
    B.createStore(E, concrete.forward(*this), valueAddr);
  }
  
  Cleanups.pushCleanup<CleanupMaterializeAddressOnlyValue>(existential);
  
  return ManagedValue(existential, getCleanupsDepth(),
                      /*isAddressOnlyValue=*/true);
}

ManagedValue SILGenFunction::visitCoerceExpr(CoerceExpr *E, SGFContext C) {
  // FIXME: do something with lhs value?
  visit(E->getLHS());
  return ManagedValue(B.createCoerce(E, visit(E->getRHS()).getValue(),
                                     getLoweredType(E->getType())));
}

ManagedValue SILGenFunction::visitDowncastExpr(DowncastExpr *E, SGFContext C) {
  // FIXME: do something with lhs value?
  visit(E->getLHS());
  return ManagedValue(B.createDowncast(E, visit(E->getRHS()).getValue(),
                                       getLoweredLoadableType(E->getType())));
}

ManagedValue SILGenFunction::visitSuperToArchetypeExpr(SuperToArchetypeExpr *E,
                                                       SGFContext C) {
  visit(E->getLHS());
  ManagedValue base = visit(E->getRHS());
  // Allocate an archetype to hold the downcast value.
  Value archetype = B.createAllocVar(E, AllocKind::Stack,
                                     getLoweredType(E->getType()));
  Cleanups.pushCleanup<CleanupMaterializeAllocation>(archetype);
  // Initialize it with a SuperToArchetypeInst.
  B.createSuperToArchetype(E, base.forward(*this), archetype);
  Cleanups.pushCleanup<CleanupMaterializeAddressOnlyValue>(archetype);
  
  return ManagedValue(archetype, getCleanupsDepth(),
                      /*isAddressOnlyValue=*/true);
}

ManagedValue SILGenFunction::visitParenExpr(ParenExpr *E, SGFContext C) {
  return visit(E->getSubExpr());
}

ManagedValue SILGenFunction::visitTupleExpr(TupleExpr *E, SGFContext C) {
  llvm::SmallVector<Value, 10> ArgsV;
  for (auto &I : E->getElements())
    ArgsV.push_back(visit(I).forward(*this));
  // FIXME: address-only tuple
  return emitManagedRValueWithCleanup(B.createTuple(E,
                                          getLoweredLoadableType(E->getType()),
                                          ArgsV));
}

ManagedValue SILGenFunction::visitGetMetatypeExpr(GetMetatypeExpr *E,
                                                  SGFContext C) {
  visit(E->getSubExpr());
  return ManagedValue(B.createMetatype(E,
                                       getLoweredLoadableType(E->getType())));
}

ManagedValue SILGenFunction::visitSpecializeExpr(SpecializeExpr *E,
                                                 SGFContext C) {
  return emitManagedRValueWithCleanup(B.createSpecialize(
                                    E,
                                    visit(E->getSubExpr()).getUnmanagedValue(),
                                    E->getSubstitutions(),
                                    getLoweredLoadableType(E->getType())));
}

ManagedValue SILGenFunction::visitAddressOfExpr(AddressOfExpr *E,
                                                SGFContext C) {
  return visit(E->getSubExpr());
}

namespace {

static ManagedValue emitExtractLoadableElement(SILGenFunction &gen,
                                               Expr *e, ManagedValue base,
                                               unsigned elt) {
  if (e->getType()->is<LValueType>()) {
    // Get the element address relative to the aggregate's address.
    Value address;
    if (base.getType().hasReferenceSemantics()) {
      address = gen.B.createRefElementAddr(e,
                                     base.getValue(),
                                     elt,
                                     gen.getLoweredType(e->getType()));
    } else {
      assert(base.getType().isAddress() &&
             "base of lvalue member ref must be ref type or address");
      address = gen.B.createElementAddr(e,
                                      base.getUnmanagedValue(),
                                      elt,
                                      gen.getLoweredType(e->getType()));
    }
    return ManagedValue(address);
  } else {
    // Extract the element from the original aggregate value.
    Value extract = gen.B.createExtract(e,
                                        base.getValue(),
                                        elt,
                                        gen.getLoweredType(e->getType()));
    
    gen.emitRetainRValue(e, extract);
    return gen.emitManagedRValueWithCleanup(extract);
  }
}
  
template<typename ANY_MEMBER_REF_EXPR>
ManagedValue emitAnyMemberRefExpr(SILGenFunction &gen,
                                  ANY_MEMBER_REF_EXPR *e,
                                  ArrayRef<Substitution> substitutions) {
  TypeInfo const &ti = gen.getTypeInfo(e->getBase()->getType()
                                       ->getRValueType());
  
  if (ti.hasFragileElement(e->getDecl())) {
    // We can get to the element directly with element_addr.
    FragileElement element = ti.getFragileElement(e->getDecl());
    return emitExtractLoadableElement(gen, e, gen.visit(e->getBase()),
                                      element.index);
  } else {
    Type baseTy = e->getBase()->getType();
    (void)baseTy;
    // We have to load the element indirectly through a property.
    assert((baseTy->is<LValueType>() || baseTy->hasReferenceSemantics()) &&
           "member ref of a non-lvalue?!");
    ManagedValue base = gen.visit(e->getBase());
    // Get the getter function, which will have type This -> () -> T.
    // The type will be a polymorphic function if the This type is generic.
    ManagedValue getter = gen.emitSpecializedPropertyConstantRef(e,
                                              e->getBase(),
                                              /*subscriptExpr=*/nullptr,
                                              SILConstant(e->getDecl(),
                                                          SILConstant::Getter),
                                              substitutions);
    // Apply the "this" parameter.
    ManagedValue getterDel = gen.emitApply(e,
                                           getter.forward(gen),
                                           base.forward(gen));
    // Apply the getter.
    Materialize propTemp = gen.emitGetProperty(e, getterDel);
    return ManagedValue(propTemp.address);
  }  
}

} // end anonymous namespace

ManagedValue SILGenFunction::emitSpecializedPropertyConstantRef(
                                          Expr *expr, Expr *baseExpr,
                                          Expr /*nullable*/ *subscriptExpr,
                                          SILConstant constant,
                                          ArrayRef<Substitution> substitutions)
{
  // Get the accessor function. The type will be a polymorphic function if
  // the This type is generic.
  ManagedValue method(emitGlobalConstantRef(expr, constant));
  // If there are substitutions, specialize the generic getter.
  if (!substitutions.empty()) {
    assert(method.getType().is<PolymorphicFunctionType>() &&
           "generic getter is not of a poly function type");
    TypeConverter &tc = SGM.Types;
    Type propType;
    if (subscriptExpr) {
      propType = tc.getSubscriptPropertyType(constant.getKind(),
                                            subscriptExpr->getType(),
                                            expr->getType()->getRValueType());
    } else {
      propType = tc.getPropertyType(constant.getKind(),
                                   expr->getType()->getRValueType());
    }
    propType = tc.getMethodTypeInContext(baseExpr->getType()->getRValueType(),
                                        propType);
    SILType lPropType = getLoweredLoadableType(propType);
    method = emitManagedRValueWithCleanup(
                            B.createSpecialize(expr, method.getUnmanagedValue(),
                                               substitutions, lPropType));
  }
  assert(method.getType().is<FunctionType>() &&
         "getter is not of a concrete function type");
  return method;
}

ManagedValue SILGenFunction::emitMethodRef(SILLocation loc,
                                           Value thisValue,
                                           SILConstant methodConstant) {
  SILType methodType = SGM.getConstantType(methodConstant);
  Value methodValue = B.createConstantRef(loc,
                                          methodConstant,
                                          methodType);
  /// If the 'this' type is a bound generic, specialize the method ref with
  /// its substitutions
  if (BoundGenericType *bgt = thisValue.getType().getAs<BoundGenericType>()) {
    PolymorphicFunctionType *methodFT =
      methodType.castTo<PolymorphicFunctionType>();
    Type specializedType = FunctionType::get(thisValue.getType().getSwiftType(),
                                             methodFT->getResult(),
                                             F.getContext());
    methodValue = B.createSpecialize(loc, methodValue,
                                     bgt->getSubstitutions(),
                                     getLoweredLoadableType(specializedType));
    return emitManagedRValueWithCleanup(methodValue);
  }
  
  return ManagedValue(methodValue);
}

ManagedValue SILGenFunction::visitMemberRefExpr(MemberRefExpr *E,
                                                SGFContext C) {
  return emitAnyMemberRefExpr(*this, E, {});
}

ManagedValue SILGenFunction::visitGenericMemberRefExpr(GenericMemberRefExpr *E,
                                                       SGFContext C)
{
  if (E->getBase()->getType()->is<MetaTypeType>()) {
    assert(E->getType()->is<MetaTypeType>() &&
           "generic_member_ref of metatype should give metatype");
    // If the base and member are metatypes, emit an associated_metatype inst
    // to extract the associated type from the type metadata.
    Value baseMetatype = visit(E->getBase()).getUnmanagedValue();
    return ManagedValue(B.createAssociatedMetatype(E,
                             baseMetatype,
                             getLoweredLoadableType(E->getType())));

  }
  return emitAnyMemberRefExpr(*this, E, E->getSubstitutions());
}

ManagedValue SILGenFunction::visitArchetypeMemberRefExpr(
                                                    ArchetypeMemberRefExpr *E,
                                                    SGFContext C) {
  Value archetype = visit(E->getBase()).getUnmanagedValue();
  assert((archetype.getType().isAddress() ||
          archetype.getType().is<MetaTypeType>()) &&
         "archetype must be an address or metatype");
  if (isa<FuncDecl>(E->getDecl())) {
    // This is a method reference. Extract the method implementation from the
    // archetype and apply the "this" argument.
    Value method = B.createArchetypeMethod(E, archetype,
                                         SILConstant(E->getDecl()),
                                         getLoweredLoadableType(E->getType()));
    return emitApply(E, method, archetype);
  } else {
    llvm_unreachable("archetype properties not yet implemented");
  }
}

ManagedValue SILGenFunction::visitExistentialMemberRefExpr(
                                                 ExistentialMemberRefExpr *E,
                                                 SGFContext C) {
  Value existential = visit(E->getBase()).getUnmanagedValue();
  assert(existential.getType().isAddress() &&
         "existential must be an address");
  if (isa<FuncDecl>(E->getDecl())) {
    // This is a method reference. Extract the method implementation from the
    // archetype and apply the "this" argument.
    Value method = B.createProtocolMethod(E, existential,
                                         SILConstant(E->getDecl()),
                                         getLoweredLoadableType(E->getType()));
    Value projection = B.createProjectExistential(E, existential);
    return emitApply(E, method, projection);
  } else {
    llvm_unreachable("existential properties not yet implemented");
  }
}

ManagedValue SILGenFunction::visitDotSyntaxBaseIgnoredExpr(
                                                  DotSyntaxBaseIgnoredExpr *E,
                                                  SGFContext C) {
  visit(E->getLHS());
  return visit(E->getRHS());
}

ManagedValue SILGenFunction::visitModuleExpr(ModuleExpr *E, SGFContext C) {
  // FIXME: modules are currently empty types. if we end up having module
  // metatypes this will need to change.
  return ManagedValue(B.createZeroValue(E,
                                        getLoweredLoadableType(E->getType())));
}

namespace {

template<typename ANY_SUBSCRIPT_EXPR>
ManagedValue emitAnySubscriptExpr(SILGenFunction &gen,
                                  ANY_SUBSCRIPT_EXPR *e,
                                  ArrayRef<Substitution> substitutions)
{
  SubscriptDecl *sd = e->getDecl();
  ManagedValue base = gen.visit(e->getBase());
  llvm::SmallVector<Value, 2> indexArgs;
  llvm::SmallVector<Writeback, 2> writebacks;
  
  gen.emitApplyArguments(e->getIndex(), indexArgs, writebacks);
  assert(writebacks.empty() && "subscript should not have byref args");
  
  // Get the getter function, which will have type This -> Index -> () -> T.
  ManagedValue getterMethod = gen.emitSpecializedPropertyConstantRef(
                                          e, e->getBase(), e->getIndex(),
                                          SILConstant(sd, SILConstant::Getter),
                                          substitutions);
  
  // Apply the "this" parameter.
  ManagedValue getterDelegate = gen.emitApply(e,
                                           getterMethod.forward(gen),
                                           base.forward(gen));
  // Apply the index parameter.
  ManagedValue getter = gen.emitApply(e, getterDelegate.forward(gen),
                                      indexArgs);
  // Apply the getter.
  Materialize propTemp = gen.emitGetProperty(e, getter);
  return ManagedValue(propTemp.address);
}

}

ManagedValue SILGenFunction::visitSubscriptExpr(SubscriptExpr *E,
                                                SGFContext C) {
  return emitAnySubscriptExpr(*this, E, {});
}

ManagedValue SILGenFunction::visitGenericSubscriptExpr(GenericSubscriptExpr *E,
                                                       SGFContext C)
{
  return emitAnySubscriptExpr(*this, E, E->getSubstitutions());
}

ManagedValue SILGenFunction::visitTupleElementExpr(TupleElementExpr *E,
                                                   SGFContext C) {
  // FIXME: address-only tuples
  return emitExtractLoadableElement(*this, E, visit(E->getBase()),
                                    E->getFieldNumber());
}

/// emitArrayInjectionCall - Form an array "Slice" out of an ObjectPointer
/// (which represents the retain count), a base pointer to some elements, and a
/// length
ManagedValue SILGenFunction::emitArrayInjectionCall(Value ObjectPtr,
                                            Value BasePtr,
                                            Value Length,
                                            Expr *ArrayInjectionFunction) {
  // Bitcast the BasePtr (an lvalue) to Builtin.RawPointer if it isn't already.
  if (BasePtr.getType() != SILType::getRawPointerType(F.getContext()))
    BasePtr = B.createImplicitConvert(SILLocation(),
                              BasePtr,
                              SILType::getRawPointerType(F.getContext()));

  Value InjectionFn = visit(ArrayInjectionFunction).forward(*this);
  Value InjectionArgs[] = { BasePtr, ObjectPtr, Length };
  return emitApply(SILLocation(), InjectionFn, InjectionArgs);
}


ManagedValue SILGenFunction::emitTupleShuffle(Expr *E,
                                      ArrayRef<Value> InOps,
                                      ArrayRef<int> ElementMapping,
                                      Expr *VarargsInjectionFunction) {
  // Collect the new elements.
  SmallVector<Value, 8> ResultElements;

  // Loop over each result element to compute it.
  ArrayRef<TupleTypeElt> outerFields =
    E->getType()->castTo<TupleType>()->getFields();

  auto shuffleIndexIterator = ElementMapping.begin();
  for (const TupleTypeElt &outerField : outerFields) {
    int shuffleIndex = *shuffleIndexIterator++;

    // If the shuffle index is -1, we're supposed to use the default value.
    if (shuffleIndex == -1) {
      assert(outerField.hasInit() && "no default initializer for field!");
      ResultElements.push_back(visit(outerField.getInit()->getExpr())
                                 .forward(*this));
      continue;
    }

    // If the shuffle index is -2, it is the beginning of the list of
    // varargs inputs.  Save this case for last.
    if (shuffleIndex != -2) {
      // Map from a different tuple element.
      ResultElements.push_back(InOps[shuffleIndex]);
      continue;
    }

    assert(outerField.isVararg() && "Cannot initialize nonvariadic element");

    // Okay, we have a varargs tuple element.  All the remaining elements feed
    // into the varargs portion of this, which is then constructed into a Slice
    // through an informal protocol captured by the InjectionFn in the
    // TupleShuffleExpr.
    auto shuffleIndexIteratorEnd = ElementMapping.end();
    unsigned NumArrayElts = shuffleIndexIteratorEnd - shuffleIndexIterator;
    Value NumEltsVal = B.createIntegerValueInst(NumArrayElts,
                            SILType::getBuiltinIntegerType(64, F.getContext()));
    AllocArrayInst *AllocArray =
      B.createAllocArray(E,
                         getLoweredType(outerField.getVarargBaseTy()),
                         NumEltsVal);

    Value ObjectPtr(AllocArray, 0);
    Value BasePtr(AllocArray, 1);

    unsigned CurElem = 0;
    while (shuffleIndexIterator != shuffleIndexIteratorEnd) {
      unsigned SourceField = *shuffleIndexIterator++;
      
      Value EltLoc = BasePtr;
      if (CurElem) EltLoc = B.createIndexAddr(E, EltLoc, CurElem);

      B.createStore(E, InOps[SourceField], EltLoc);
      ++CurElem;
    }

    ResultElements.push_back(emitArrayInjectionCall(ObjectPtr, BasePtr,
                                        NumEltsVal, VarargsInjectionFunction)
                               .forward(*this));
    break;
  }

  // FIXME address-only tuples
  return emitManagedRValueWithCleanup(B.createTuple(E,
                                          getLoweredLoadableType(E->getType()),
                                          ResultElements));
}

ManagedValue SILGenFunction::visitTupleShuffleExpr(TupleShuffleExpr *E,
                                                   SGFContext C) {
  // TupleShuffle expands out to extracts+inserts.  Start by emitting the base
  // expression that we'll shuffle.
  Value Op = visit(E->getSubExpr()).getValue();
  SmallVector<Value, 8> InElts;
  unsigned EltNo = 0;
  for (auto &InField : Op.getType().castTo<TupleType>()->getFields()) {
    // FIXME address-only tuples
    Value elt = B.createExtract(SILLocation(), Op, EltNo++,
                                getLoweredLoadableType(InField.getType()));
    emitRetainRValue(E, elt);
    InElts.push_back(elt);
  }

  return emitTupleShuffle(E, InElts, E->getElementMapping(),
                          E->getVarargsInjectionFunctionOrNull());
}

ManagedValue SILGenFunction::visitScalarToTupleExpr(ScalarToTupleExpr *E,
                                                    SGFContext C) {
  // Emit the argument and turn it into a trivial tuple.
  Value Arg = visit(E->getSubExpr()).getValue();

  // If we don't have exactly the same tuple, perform a shuffle to create
  // default arguments etc.
  SmallVector<int, 8> ShuffleMask;

  auto outerFields = E->getType()->castTo<TupleType>()->getFields();
  for (unsigned i = 0, e = outerFields.size(); i != e; ++i) {
    // If we get to the last argument and it is a varargs list, make sure to
    // mark it with a "-2" entry.
    if (outerFields[i].isVararg())
      ShuffleMask.push_back(-2);

    // If we have a field with a default value, emit that value.  Otherwise, use
    // the tuple we have as input.
    if (i == E->getScalarField())
      ShuffleMask.push_back(0);
    else if (!outerFields[i].isVararg())
      ShuffleMask.push_back(-1);
  }

  return emitTupleShuffle(E, Arg, ShuffleMask,E->getVarargsInjectionFunction());
}

ManagedValue SILGenFunction::visitNewArrayExpr(NewArrayExpr *E, SGFContext C) {
  Value NumElements = visit(E->getBounds()[0].Value).getValue();

  // Allocate the array.
  AllocArrayInst *AllocArray = B.createAllocArray(E,
                                            getLoweredType(E->getElementType()),
                                            NumElements);

  Value ObjectPtr(AllocArray, 0), BasePtr(AllocArray, 1);

  // FIXME: We need to initialize the elements of the array that are now
  // allocated.

  // Finally, build and return a Slice instance using the object
  // header/base/count.
  return emitArrayInjectionCall(ObjectPtr, BasePtr, NumElements,
                                E->getInjectionFunction());
}



ManagedValue SILGenFunction::visitMetatypeExpr(MetatypeExpr *E, SGFContext C) {
  return ManagedValue(B.createMetatype(E,
                                       getLoweredLoadableType(E->getType())));
}

ManagedValue SILGenFunction::emitClosureForCapturingExpr(SILLocation loc,
                                                         SILConstant constant,
                                                         CapturingExpr *body) {
  auto captures = body->getCaptures();
  if (!captures.empty()) {
    
    llvm::SmallVector<Value, 4> capturedArgs;
    for (ValueDecl *capture : captures) {
      switch (getDeclCaptureKind(capture)) {
        case CaptureKind::LValue: {
          // LValues are captured as both the box owning the value and the
          // address of the value.
          assert(VarLocs.count(capture) &&
                 "no location for captured var!");
          
          VarLoc const &vl = VarLocs[capture];
          assert(vl.box && "no box for captured var!");
          assert(vl.address && "no address for captured var!");
          B.createRetain(loc, vl.box);
          capturedArgs.push_back(vl.box);
          capturedArgs.push_back(vl.address);
          break;
        }
        case CaptureKind::Byref: {
          // Byrefs are captured by address only.
          assert(VarLocs.count(capture) &&
                 "no location for captured byref!");
          capturedArgs.push_back(VarLocs[capture].address);
          break;
        }
        case CaptureKind::Constant: {
          // Value is a constant such as a local func. Pass on the reference.
          ManagedValue v = emitReferenceToDecl(loc, capture);
          capturedArgs.push_back(v.forward(*this));
          break;
        }
        case CaptureKind::GetterSetter: {
          // Pass the setter and getter closure references on.
          ManagedValue v = emitConstantRef(loc, SILConstant(capture,
                                                       SILConstant::Setter));
          capturedArgs.push_back(v.forward(*this));
          /* FALLTHROUGH */
        }
        case CaptureKind::Getter: {
          // Pass the getter closure reference on.
          ManagedValue v = emitConstantRef(loc, SILConstant(capture,
                                                       SILConstant::Getter));
          capturedArgs.push_back(v.forward(*this));
          break;
        }
      }
    }
    
    Value functionRef = emitGlobalConstantRef(loc, constant);
    return emitManagedRValueWithCleanup(B.createClosure(loc,
                                                    functionRef, capturedArgs));
  } else {
    return ManagedValue(emitGlobalConstantRef(loc, constant));
  }
}

Materialize SILGenFunction::emitGetProperty(SILLocation loc,
                                            ManagedValue getter) {
  // Call the getter and then materialize the return value as an lvalue.
  ManagedValue result = emitApply(loc, getter.forward(*this), {});
  return emitMaterialize(*this, loc, result);
}

ManagedValue SILGenFunction::visitFuncExpr(FuncExpr *e, SGFContext C) {
  // Generate the local function body.
  SGM.emitFunction(e, e);

  // Generate the closure (if any) for the function reference.
  return emitClosureForCapturingExpr(e, SILConstant(e), e);
}

ManagedValue SILGenFunction::visitClosureExpr(ClosureExpr *e, SGFContext C) {
  // Generate the closure body.
  SGM.emitClosure(e);
  
  // Generate the closure value (if any) for the closure expr's function
  // reference.
  return emitClosureForCapturingExpr(e, SILConstant(e), e);
}

void SILGenFunction::emitFunction(FuncExpr *fe) {
  emitProlog(fe, fe->getBodyParamPatterns(), fe->getResultType(F.getContext()));
  visit(fe->getBody());
}

void SILGenFunction::emitClosure(ClosureExpr *ce) {
  emitProlog(ce, ce->getParamPatterns(),
             ce->getType()->castTo<FunctionType>()->getResult());

  // Closure expressions implicitly return the result of their body expression.
  // FIXME: address-only return from closure. refactor common code from
  // visitReturnStmt
  Value result;
  {
    FullExpr scope(Cleanups);
    result = visit(ce->getBody()).forward(*this);
  }
  if (B.hasValidInsertionPoint())
    Cleanups.emitReturnAndCleanups(ce->getBody(), result);
}

bool SILGenFunction::emitEpilogBB(SILLocation loc) {
  assert(epilogBB && "no epilog bb to emit?!");
  
  // If the epilog was not branched to at all, just unwind like a "return"
  // and emit the epilog into the current BB.
  if (epilogBB->pred_empty()) {
    epilogBB->eraseFromParent();

    // If the current bb is terminated then the epilog is just unreachable.
    if (!B.hasValidInsertionPoint())
      return false;
    
    Cleanups.emitCleanupsForReturn(loc);
  } else {
    // If the body didn't explicitly return, we need to branch out of it as if
    // returning. emitReturnAndCleanups will do that.
    if (B.hasValidInsertionPoint())
      Cleanups.emitReturnAndCleanups(loc, Value());
    // Emit the epilog into the epilog bb.
    B.emitBlock(epilogBB);
  }
  return true;
}

void SILGenFunction::emitDestructor(ClassDecl *cd, DestructorDecl *dd) {
  Value thisValue = emitDestructorProlog(cd, dd);

  // Create a basic block to jump to for the implicit destruction behavior
  // of releasing the elements and calling the base class destructor.
  // We won't actually emit the block until we finish with the destructor body.
  epilogBB = new (SGM.M) BasicBlock(&F, "destructor");
  
  // Emit the destructor body, if any.
  if (dd)
    visit(dd->getBody());
  
  if (!emitEpilogBB(dd))
    return;
  
  // Release our members.
  // FIXME: generic params
  // FIXME: Can a destructor always consider its fields fragile like this?
  TypeInfo const &thisTI = getTypeInfo(thisValue.getType().getSwiftType());
  for (Decl *member : cd->getMembers()) {
    if (VarDecl *vd = dyn_cast<VarDecl>(member)) {
      if (vd->isProperty())
        continue;
      assert(thisTI.hasFragileElement(vd));
      unsigned fieldNo = thisTI.getFragileElement(vd).index;
      TypeInfo const &ti = getTypeInfo(vd->getType());
      if (!ti.isTrivial()) {
        Value addr = B.createRefElementAddr(dd, thisValue, fieldNo,
                                          ti.getLoweredType().getAddressType());
        if (ti.isAddressOnly()) {
          B.createDestroyAddr(dd, addr);
        } else {
          Value field = B.createLoad(dd, addr);
          emitReleaseRValue(dd, field);
        }
      }
      ++fieldNo;
    }
  }
  
  // If we have a base class, invoke its destructor.
  if (Type baseTy = cd->getBaseClass()) {
    SILConstant dtorConstant =
      SILConstant(baseTy->getClassOrBoundGenericClass(),
                  SILConstant::Destructor);
    Value baseThis = B.createImplicitConvert(dd,
                                             thisValue,
                                             getLoweredLoadableType(baseTy));
    ManagedValue dtorValue = emitMethodRef(dd, baseThis, dtorConstant);
    B.createApply(dd, dtorValue.forward(*this),
                  SILType::getEmptyTupleType(SGM.M.getContext()),
                  baseThis);
  } else {
    // Otherwise, deallocate the instance ourselves.
    B.createDeallocRef(dd, thisValue);
  }
  
  B.createReturn(dd, B.createEmptyTuple(dd));
}

namespace {
  class CleanupMaterializeThisValue : public Cleanup {
    ConstructorDecl *ctor;
    Value thisLV;
    Value &thisValue;
  public:
    CleanupMaterializeThisValue(ConstructorDecl *ctor,
                                Value thisLV, Value &thisValue)
      : ctor(ctor), thisLV(thisLV), thisValue(thisValue) {}
    
    void emit(SILGenFunction &gen) override {
      thisValue = gen.B.createLoad(ctor, thisLV);
      gen.B.createDeallocVar(ctor, AllocKind::Stack, thisLV);
    }
  };
}


static void emitConstructorMetatypeArg(SILGenFunction &gen,
                                       ConstructorDecl *ctor) {
  // In addition to the declared arguments, the constructor implicitly takes
  // the metatype as its first argument, like a static function.
  Type metatype = ctor->getType()->castTo<AnyFunctionType>()->getInput();
  new (gen.F.getModule()) BBArgument(gen.getLoweredType(metatype),
                                     gen.F.begin());
}

void SILGenFunction::emitValueConstructor(ConstructorDecl *ctor) {
  // Emit the prolog.
  emitConstructorMetatypeArg(*this, ctor);
  emitProlog(ctor->getArguments(), ctor->getImplicitThisDecl()->getType());
  
  //
  // Create the 'this' value.
  //
  VarDecl *thisDecl = ctor->getImplicitThisDecl();
  SILType thisTy = getLoweredType(thisDecl->getType());
  Value thisLV, thisValue;
  assert(!ctor->getAllocThisExpr() && "alloc_this expr for value type?!");
  assert(!thisTy.hasReferenceSemantics() && "can't emit a ref type ctor here");
  if (thisTy.isAddressOnly()) {
    // If 'this' is of an address-only value type, then we can construct the
    // indirect return argument directly.
    assert(IndirectReturnAddress &&
           "address-only constructor without indirect return?!");
    thisLV = IndirectReturnAddress;
  } else {
    // Materialize a temporary for the loadable value type.
    thisLV = B.createAllocVar(ctor, AllocKind::Stack,
                                 thisTy);
    // Set up a cleanup to load the final value at the end of the constructor
    // body.
    Cleanups.pushCleanup<CleanupMaterializeThisValue>(ctor, thisLV, thisValue);
  }
  VarLocs[thisDecl] = {Value(), thisLV};
  
  // Create a basic block to jump to for the implicit 'this' return.
  // We won't emit this until after we've emitted the body.
  epilogBB = new (SGM.M) BasicBlock(&F, "constructor");

  //
  // Emit the constructor body.
  // FIXME: call base class constructor  
  //
  visit(ctor->getBody());
  
  //
  // Return 'this' in the epilog.
  //
  if (!emitEpilogBB(ctor))
    return;

  if (thisTy.isAddressOnly()) {
    // We already initialized the return value in-place.
    B.createReturn(ctor, B.createEmptyTuple(ctor));
  } else {
    assert(thisValue && "thisValue not initialized?!");
    B.createReturn(ctor, thisValue);
  }
}

namespace {
  // Unlike the ArgumentInitVisitor, this visitor generates arguments but leaves
  // them collected into a tuple instead of storing them to lvalues, so that the
  // argument set can just be forwarded to another function.
  class ArgumentForwardVisitor
    : public PatternVisitor<ArgumentForwardVisitor, Value>
  {
    SILGenFunction &gen;
  public:
    ArgumentForwardVisitor(SILGenFunction &gen) : gen(gen) {}
    
    Value makeArgument(Type ty) {
      assert(ty && "no type?!");
      return new (gen.F.getModule()) BBArgument(gen.getLoweredType(ty),
                                                gen.F.begin());
    }

    Value visitParenPattern(ParenPattern *P) {
      return visit(P->getSubPattern());
    }
    
    Value visitTypedPattern(TypedPattern *P) {
      return visit(P->getSubPattern());
    }
    
    Value visitTuplePattern(TuplePattern *P) {
      SmallVector<Value, 4> elements;
      for (auto &elt : P->getFields())
        elements.push_back(visit(elt.getPattern()));
      return gen.B.createTuple(SILLocation(),
                               gen.getLoweredType(P->getType()),
                               elements);
    }
    
    Value visitAnyPattern(AnyPattern *P) {
      return makeArgument(P->getType());
    }
    
    Value visitNamedPattern(NamedPattern *P) {
      return makeArgument(P->getType());
    }
  };
} // end anonymous namespace

void SILGenFunction::emitClassConstructorAllocator(ConstructorDecl *ctor) {
  // Emit the prolog. Since we're just going to forward our args directly
  // to the initializer, keep them tupled up instead of allocating local
  // variables for all of them.
  emitConstructorMetatypeArg(*this, ctor);
  Value args = ArgumentForwardVisitor(*this).visit(ctor->getArguments());
  
  // Allocate the "this" value.
  VarDecl *thisDecl = ctor->getImplicitThisDecl();
  SILType thisTy = getLoweredType(thisDecl->getType());
  assert(thisTy.hasReferenceSemantics() &&
         "can't emit a value type ctor here");
  Value thisValue;
  if (ctor->getAllocThisExpr()) {
    FullExpr allocThisScope(Cleanups);
    // If the constructor has an alloc-this expr, emit it to get "this".
    thisValue = visit(ctor->getAllocThisExpr()).forward(*this);
    assert(thisValue.getType() == thisTy &&
           "alloc-this expr type did not match this type?!");
  } else {
    // Otherwise, just emit an alloc_ref instruction for the default allocation
    // path.
    // FIXME: should have a cleanup in case of exception
    thisValue = B.createAllocRef(ctor, AllocKind::Heap, thisTy);
  }

  // Call the initializer.
  SILConstant initConstant = SILConstant(ctor, SILConstant::Initializer);
  ManagedValue initVal = emitMethodRef(ctor, thisValue, initConstant);
  SILType initDelType = getLoweredLoadableType(
                     initVal.getType().castTo<AnyFunctionType>()->getResult());
  B.createRetain(ctor, thisValue);
  Value initDel = B.createApply(ctor, initVal.forward(*this),
                                initDelType, thisValue);
  B.createApply(ctor, initDel, SILType::getEmptyTupleType(F.getContext()),
                args);
  
  // Return 'this'.
  B.createReturn(ctor, thisValue);
}

void SILGenFunction::emitClassConstructorInitializer(ConstructorDecl *ctor) {
  // Emit the 'this' argument and make an lvalue for it.
  VarDecl *thisDecl = ctor->getImplicitThisDecl();
  SILType thisTy = getLoweredType(thisDecl->getType());
  assert(thisTy.hasReferenceSemantics() &&
         "can't emit a value type ctor here");
  Value thisValue = new (SGM.M) BBArgument(thisTy, F.begin());
  Value thisLV = B.createAllocVar(ctor, AllocKind::Stack, thisTy);
  Cleanups.pushCleanup<CleanupMaterializeAllocation>(thisLV);
  B.createStore(ctor, thisValue, thisLV);
  Cleanups.pushCleanup<CleanupMaterializeValue>(thisLV);
  VarLocs[thisDecl] = {Value(), thisLV};
  
  // Emit the prolog for the other arguments.
  emitProlog(ctor->getArguments(), TupleType::getEmpty(F.getContext()));
  
  // Emit the constructor body.
  visit(ctor->getBody());
}


ManagedValue SILGenFunction::visitInterpolatedStringLiteralExpr(
                                              InterpolatedStringLiteralExpr *E,
                                              SGFContext C)
{
  return visit(E->getSemanticExpr());
}
