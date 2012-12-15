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
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(Value v) {
  if (v.getType()->is<LValueType>() ||
      getTypeInfo(v.getType()).isTrivial()) {
    return ManagedValue(v);
  } else {
    Cleanups.pushCleanup<CleanupRValue>(v);
    return ManagedValue(v, getCleanupsDepth());
  }
}

ManagedValue SILGenFunction::visitExpr(Expr *E) {
  E->dump();
  llvm_unreachable("Not yet implemented");
}

namespace {

static Value emitApplyArgument(SILGenFunction &gen, Expr *arg,
                                llvm::SmallVectorImpl<Writeback> &writebacks) {
  if (arg->getType()->is<LValueType>()) {
    LValue lv = SILGenLValue(gen).visit(arg);
    Value addr = gen.emitMaterializedLoadFromLValue(arg, lv)
      .getUnmanagedValue();
    if (!lv.isPhysical()) {
      writebacks.push_back({::std::move(lv), addr});
    }
    return addr;
  } else {
    return gen.visit(arg).forward(gen);
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

ManagedValue SILGenFunction::visitApplyExpr(ApplyExpr *E) {
  // FIXME: This assumes that all Swift arguments and returns lower one-to-one
  // to SIL arguments and returns, which won't hold up in the face of
  // address-only types.
  Value FnV = visit(E->getFn()).forward(*this);
  llvm::SmallVector<Value, 10> ArgsV;
  llvm::SmallVector<Writeback, 2> writebacks;
  
  emitApplyArguments(E->getArg(), ArgsV, writebacks);
  
  ManagedValue r = emitManagedRValueWithCleanup(B.createApply(E, FnV, ArgsV));
  
  for (auto &wb : writebacks) {
    Value newValue = B.createLoad(E, wb.tempAddress);
    emitAssignToLValue(E, newValue, wb.lvalue);
  }
  
  return r;
}

Value SILGenFunction::emitUnmanagedConstantRef(SILLocation loc,
                                               SILConstant constant) {
  // If this is a reference to a local constant, grab it.
  if (LocalConstants.count(constant)) {
    return LocalConstants[constant];
  }
  
  // Otherwise, use a global ConstantRefInst.
  return B.createConstantRef(loc, constant);
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
  Value c = B.createConstantRef(loc, constant);
  return ManagedValue(c);
}

ManagedValue SILGenFunction::emitReferenceToDecl(SILLocation loc,
                                                 ValueDecl *decl) {
  
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
    Value accessor = B.createConstantRef(loc, SILConstant(decl));
    Value address = B.createApply(loc, accessor, {});
    return ManagedValue(address);
  }
  
  // If the referenced decl isn't a VarDecl, it should be a constant of some
  // sort.
  assert(!decl->getTypeOfReference()->is<LValueType>() &&
         "unexpected lvalue decl ref?!");

  return emitConstantRef(loc, SILConstant(decl));
}

ManagedValue SILGenFunction::visitDeclRefExpr(DeclRefExpr *E) {
  return emitReferenceToDecl(E, E->getDecl());
}

ManagedValue SILGenFunction::visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
  return ManagedValue(B.createIntegerLiteral(E));
}
ManagedValue SILGenFunction::visitFloatLiteralExpr(FloatLiteralExpr *E) {
  return ManagedValue(B.createFloatLiteral(E));
}
ManagedValue SILGenFunction::visitCharacterLiteralExpr(CharacterLiteralExpr *E)
{
  return ManagedValue(B.createIntegerLiteral(E));
}
ManagedValue SILGenFunction::visitStringLiteralExpr(StringLiteralExpr *E) {
  return ManagedValue(B.createStringLiteral(E));
}

ManagedValue SILGenFunction::visitLoadExpr(LoadExpr *E) {
  ManagedValue SubV = visit(E->getSubExpr());
  Value loadedV = B.createLoad(E, SubV.getUnmanagedValue());
  emitRetainRValue(E, loadedV);
  return emitManagedRValueWithCleanup(loadedV);
}

namespace {
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

  static Materialize emitMaterialize(SILGenFunction &gen,
                                      SILLocation loc, Value v) {
    Value tmpMem = gen.B.createAllocVar(loc, AllocKind::Stack, v.getType());
    gen.Cleanups.pushCleanup<CleanupMaterializeAllocation>(tmpMem);
    gen.B.createStore(loc, v, tmpMem);
    
    CleanupsDepth valueCleanup = CleanupsDepth::invalid();
    if (!gen.getTypeInfo(v.getType()).isTrivial()) {
      gen.Cleanups.pushCleanup<CleanupMaterializeValue>(tmpMem);
      valueCleanup = gen.getCleanupsDepth();
    }
    
    return Materialize{tmpMem, valueCleanup};
  }
}

ManagedValue Materialize::consume(SILGenFunction &gen, SILLocation loc) {
  if (valueCleanup.isValid())
    gen.Cleanups.setCleanupState(valueCleanup, CleanupState::Dead);
  return gen.emitManagedRValueWithCleanup(gen.B.createLoad(loc, address));
}

ManagedValue SILGenFunction::visitMaterializeExpr(MaterializeExpr *E) {
  // Evaluate the value, then use it to initialize a new temporary and return
  // the temp's address.
  Value V = visit(E->getSubExpr()).forward(*this);
  return ManagedValue(emitMaterialize(*this, E, V).address);
}

ManagedValue SILGenFunction::visitRequalifyExpr(RequalifyExpr *E) {
  return ManagedValue(B.createImplicitConvert(E,
                                      visit(E->getSubExpr()).getValue(),
                                      E->getType()));
}

ManagedValue SILGenFunction::visitFunctionConversionExpr(
                                                      FunctionConversionExpr *E)
{
  return ManagedValue(B.createImplicitConvert(E,
                                      visit(E->getSubExpr()).getValue(),
                                      E->getType()));
}

ManagedValue SILGenFunction::visitCoerceExpr(CoerceExpr *E) {
  // FIXME: do something with lhs value?
  visit(E->getLHS());
  return ManagedValue(B.createCoerce(E, visit(E->getRHS()).getValue(),
                                     E->getType()));
}

ManagedValue SILGenFunction::visitDowncastExpr(DowncastExpr *E) {
  // FIXME: do something with lhs value?
  visit(E->getLHS());
  return ManagedValue(B.createDowncast(E, visit(E->getRHS()).getValue(),
                                       E->getType()));
}

ManagedValue SILGenFunction::visitParenExpr(ParenExpr *E) {
  return visit(E->getSubExpr());
}

ManagedValue SILGenFunction::visitTupleExpr(TupleExpr *E) {
  llvm::SmallVector<Value, 10> ArgsV;
  for (auto &I : E->getElements())
    ArgsV.push_back(visit(I).forward(*this));
  return emitManagedRValueWithCleanup(B.createTuple(E, E->getType(), ArgsV));
}

ManagedValue SILGenFunction::visitGetMetatypeExpr(GetMetatypeExpr *E) {
  visit(E->getSubExpr());
  return ManagedValue(B.createMetatype(E));
}

ManagedValue SILGenFunction::visitSpecializeExpr(SpecializeExpr *E) {
  return emitManagedRValueWithCleanup(B.createSpecialize(
                                     E,
                                    visit(E->getSubExpr()).getUnmanagedValue(),
                                    E->getSubstitutions(),
                                    E->getType()));
}

ManagedValue SILGenFunction::visitAddressOfExpr(AddressOfExpr *E) {
  return visit(E->getSubExpr());
}

namespace {

static ManagedValue emitExtractLoadableElement(SILGenFunction &gen,
                                               Expr *e, ManagedValue base,
                                               unsigned elt) {
  if (e->getType()->is<LValueType>()) {
    // Get the element address relative to the aggregate's address.
    Value address;
    if (base.getValue().getType()->hasReferenceSemantics()) {
      address = gen.B.createRefElementAddr(e,
                                           base.getValue(),
                                           elt,
                                           e->getType());
    } else {
      assert(base.getValue().getType()->is<LValueType>() &&
             "base of lvalue member ref must be ref type or address");
      address = gen.B.createElementAddr(e,
                                        base.getUnmanagedValue(),
                                        elt,
                                        e->getType());
    }
    return ManagedValue(address);
  } else {
    // Extract the element from the original aggregate value.
    Value extract = gen.B.createExtract(e,
                                        base.getValue(),
                                        elt,
                                        e->getType());
    
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
  
  if (ti.hasFragileElement(e->getDecl()->getName())) {
    // We can get to the element directly with element_addr.
    FragileElement element = ti.getFragileElement(e->getDecl()->getName());
    return emitExtractLoadableElement(gen, e, gen.visit(e->getBase()),
                                      element.index);
  } else {
    Type baseTy = e->getBase()->getType();
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
    Value getterDel = gen.B.createApply(e,
                                        getter.forward(gen),
                                        base.forward(gen));
    // Apply the getter.
    Materialize propTemp = gen.emitGetProperty(e,
                                  gen.emitManagedRValueWithCleanup(getterDel));
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
  ManagedValue method(B.createConstantRef(expr, constant));
  // If there are substitutions, specialize the generic getter.
  if (!substitutions.empty()) {
    assert(method.getValue().getType()->is<PolymorphicFunctionType>() &&
           "generic getter is not of a poly function type");
    SILModule &m = F.getModule();
    Type propType;
    if (subscriptExpr) {
      propType = m.getSubscriptPropertyType(constant.id,
                                            subscriptExpr->getType(),
                                            expr->getType()->getRValueType());
    } else {
      propType = m.getPropertyType(constant.id,
                                   expr->getType()->getRValueType());
    }
    propType = m.getMethodTypeInContext(baseExpr->getType()->getRValueType(),
                                        propType);
    method = emitManagedRValueWithCleanup(
                            B.createSpecialize(expr, method.getUnmanagedValue(),
                                               substitutions, propType));
  }
  assert(method.getValue().getType()->is<FunctionType>() &&
         "getter is not of a concrete function type");
  return method;
}

ManagedValue SILGenFunction::visitMemberRefExpr(MemberRefExpr *E) {
  return emitAnyMemberRefExpr(*this, E, {});
}

ManagedValue SILGenFunction::visitGenericMemberRefExpr(GenericMemberRefExpr *E)
{
  // FIXME: Kludge to handle type variable references, which currently manifest
  // as GenericMemberRefExprs off the generic type. According to John this might
  // be an AST problem.
  if (E->getType()->is<MetaTypeType>() &&
      E->getBase()->getType()->is<MetaTypeType>()) {
    visit(E->getBase());
    return ManagedValue(B.createMetatype(E));
  }
  
  return emitAnyMemberRefExpr(*this, E, E->getSubstitutions());
}

ManagedValue SILGenFunction::visitArchetypeMemberRefExpr(
                                                    ArchetypeMemberRefExpr *E) {
  Value archetype = visit(E->getBase()).getUnmanagedValue();
  assert((archetype.getType()->is<LValueType>() ||
          archetype.getType()->is<MetaTypeType>()) &&
         "archetype must be an address or metatype");
  if (isa<FuncDecl>(E->getDecl())) {
    // This is a method reference. Extract the method implementation from the
    // archetype and apply the "this" argument.
    Value method = B.createArchetypeMethod(E, archetype,
                                           SILConstant(E->getDecl()),
                                           E->getType());
    Value delegate = B.createApply(E, method, archetype);
    return emitManagedRValueWithCleanup(delegate);
  } else {
    llvm_unreachable("archetype properties not yet implemented");
  }
}

ManagedValue SILGenFunction::visitExistentialMemberRefExpr(
                                                 ExistentialMemberRefExpr *E) {
  Value existential = visit(E->getBase()).getUnmanagedValue();
  assert(existential.getType()->is<LValueType>() &&
         "archetype must be an address or metatype");
  if (isa<FuncDecl>(E->getDecl())) {
    // This is a method reference. Extract the method implementation from the
    // archetype and apply the "this" argument.
    Value method = B.createExistentialMethod(E, existential,
                                             SILConstant(E->getDecl()),
                                             E->getType());
    Value delegate = B.createApply(E, method, existential);
    return emitManagedRValueWithCleanup(delegate);
  } else {
    llvm_unreachable("existential properties not yet implemented");
  }
}

ManagedValue SILGenFunction::visitDotSyntaxBaseIgnoredExpr(
                                                  DotSyntaxBaseIgnoredExpr *E) {
  visit(E->getLHS());
  return visit(E->getRHS());
}

ManagedValue SILGenFunction::visitModuleExpr(ModuleExpr *E) {
  // FIXME: modules are currently empty types. if we end up having module
  // metatypes this will need to change.
  return ManagedValue(B.createZeroValue(E, E->getType()));
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
  Value getterDelegate = gen.B.createApply(e,
                                           getterMethod.forward(gen),
                                           base.forward(gen));
  // Apply the index parameter.
  Value getter = gen.B.createApply(e, getterDelegate, indexArgs);
  // Apply the getter.
  Materialize propTemp = gen.emitGetProperty(e,
                                      gen.emitManagedRValueWithCleanup(getter));
  return ManagedValue(propTemp.address);
}

}

ManagedValue SILGenFunction::visitSubscriptExpr(SubscriptExpr *E) {
  return emitAnySubscriptExpr(*this, E, {});
}

ManagedValue SILGenFunction::visitGenericSubscriptExpr(GenericSubscriptExpr *E)
{
  return emitAnySubscriptExpr(*this, E, E->getSubstitutions());
}

ManagedValue SILGenFunction::visitTupleElementExpr(TupleElementExpr *E) {
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
  if (!BasePtr.getType()->isEqual(F.getContext().TheRawPointerType))
    BasePtr = B.createImplicitConvert(SILLocation(),
                              BasePtr,
                              F.getContext().TheRawPointerType);

  Value InjectionFn = visit(ArrayInjectionFunction).getUnmanagedValue();
  Value InjectionArgs[] = { BasePtr, ObjectPtr, Length };
  return emitManagedRValueWithCleanup(B.createApply(SILLocation(),
                                                   InjectionFn, InjectionArgs));
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
                                   BuiltinIntegerType::get(64, F.getContext()));
    AllocArrayInst *AllocArray =
      B.createAllocArray(E, outerField.getVarargBaseTy(), NumEltsVal);

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

  return emitManagedRValueWithCleanup(B.createTuple(E, E->getType(),
                                                       ResultElements));
}

ManagedValue SILGenFunction::visitTupleShuffleExpr(TupleShuffleExpr *E) {
  // TupleShuffle expands out to extracts+inserts.  Start by emitting the base
  // expression that we'll shuffle.
  Value Op = visit(E->getSubExpr()).getValue();
  SmallVector<Value, 8> InElts;
  unsigned EltNo = 0;
  for (auto &InField : Op.getType()->castTo<TupleType>()->getFields()) {
    Value elt = B.createExtract(SILLocation(), Op, EltNo++,
                                     InField.getType());
    emitRetainRValue(E, elt);
    InElts.push_back(elt);
  }

  return emitTupleShuffle(E, InElts, E->getElementMapping(),
                          E->getVarargsInjectionFunctionOrNull());
}

ManagedValue SILGenFunction::visitScalarToTupleExpr(ScalarToTupleExpr *E) {
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

ManagedValue SILGenFunction::visitNewArrayExpr(NewArrayExpr *E) {
  Value NumElements = visit(E->getBounds()[0].Value).getValue();

  // Allocate the array.
  AllocArrayInst *AllocArray = B.createAllocArray(E, E->getElementType(),
                                                  NumElements);

  Value ObjectPtr(AllocArray, 0), BasePtr(AllocArray, 1);

  // FIXME: We need to initialize the elements of the array that are now
  // allocated.

  // Finally, build and return a Slice instance using the object
  // header/base/count.
  return emitArrayInjectionCall(ObjectPtr, BasePtr, NumElements,
                                E->getInjectionFunction());
}



ManagedValue SILGenFunction::visitMetatypeExpr(MetatypeExpr *E) {
  return ManagedValue(B.createMetatype(E));
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
    
    Value functionRef = B.createConstantRef(loc, constant);
    return emitManagedRValueWithCleanup(B.createClosure(loc,
                                                    functionRef, capturedArgs));
  } else {
    return ManagedValue(B.createConstantRef(loc, constant));
  }
}

Materialize SILGenFunction::emitGetProperty(SILLocation loc,
                                             ManagedValue getter) {
  // Call the getter and then materialize the return value as an lvalue.
  Value result = B.createApply(loc, getter.forward(*this), {});
  return emitMaterialize(*this, loc, result);
}

ManagedValue SILGenFunction::visitFuncExpr(FuncExpr *e) {
  // Generate the local function body.
  SGM.emitFunction(e, e);

  // Generate the closure (if any) for the function reference.
  return emitClosureForCapturingExpr(e, SILConstant(e), e);
}

ManagedValue SILGenFunction::visitClosureExpr(ClosureExpr *e) {
  // Generate the closure body.
  SGM.emitClosure(e);
  
  // Generate the closure value (if any) for the closure expr's function
  // reference.
  return emitClosureForCapturingExpr(e, SILConstant(e), e);
}

void SILGenFunction::emitClosureBody(Expr *body) {
  // Closure expressions implicitly return the result of their body expression.
  Value result;
  {
    FullExpr scope(Cleanups);
    result = visit(body).forward(*this);
  }
  if (B.hasValidInsertionPoint())
    Cleanups.emitReturnAndCleanups(body, result);
}

ManagedValue SILGenFunction::visitInterpolatedStringLiteralExpr(
                                              InterpolatedStringLiteralExpr *E)
{
  return visit(E->getSemanticExpr());
}