//===--- SILGenApply.cpp - Constructs call sites for SILGen ---------------===//
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
#include "ManagedValue.h"

using namespace swift;
using namespace Lowering;

/// SILGenApply - An ASTVisitor for building SIL function calls.
/// Nested ApplyExprs applied to an underlying curried function or method
/// reference are flattened into a single SIL apply to the most uncurried entry
/// point fitting the call site, avoiding pointless intermediate closure
/// construction.
class LLVM_LIBRARY_VISIBILITY SILGenApply
  : public ExprVisitor<SILGenApply>
{
public:
  SILGenFunction &gen;
  ManagedValue callee;
  ManagedValue thisParam;
  std::vector<ApplyExpr*> callSites;
  std::vector<ArrayRef<Substitution>> substitutions;
  Expr *sideEffect;
  
  SILGenApply(SILGenFunction &gen) : gen(gen), sideEffect(nullptr) {}
  
  void setCallee(ManagedValue theCallee) {
    assert(!callee.getValue() && "already set callee!");
    callee = theCallee;
  }
  
  void setSideEffect(Expr *sideEffectExpr) {
    assert(!sideEffect && "already set side effect!");
    sideEffect = sideEffectExpr;
  }
  
  void setThisParam(ManagedValue theThisParam) {
    assert(!thisParam.getValue() && "already set this!");
    thisParam = theThisParam;
  }
  
  /// Fall back to an unknown, indirect callee.
  void visitExpr(Expr *e) {
    setCallee(gen.visit(e));
  }
  
  /// Add a call site to the curry.
  void visitApplyExpr(ApplyExpr *e) {
    if (e->isSuper()) {
      applySuper(e);
    } else {
      callSites.push_back(e);
      visit(e->getFn());
    }
  }
  
  /// Add specializations to the curry.
  void visitSpecializeExpr(SpecializeExpr *e) {
    substitutions.push_back(e->getSubstitutions());
    visit(e->getSubExpr());
  }
  
  //
  // Known callees.
  //
  void visitDeclRefExpr(DeclRefExpr *e) {
    // If this is a class method, emit class_method to dynamically dispatch the
    // call.
    if (auto *fe = dyn_cast<FuncDecl>(e->getDecl())) {
      if (isa<ClassDecl>(fe->getDeclContext())) {
        ApplyExpr *thisCallSite = callSites.back();
        callSites.pop_back();
        setThisParam(gen.visit(thisCallSite->getArg()));
        SILConstant constant(fe);
        setCallee(ManagedValue(
                   gen.B.createClassMethod(thisCallSite,
                                           thisParam.getValue(),
                                           constant,
                                           gen.SGM.getConstantType(constant))));
        return;
      }
    }
    
    // FIXME: Store context values for local funcs in a way that we can
    // apply them directly as an added "call site" here.
    setCallee(gen.emitReferenceToDecl(e, e->getDecl()));
  }
  void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *e) {
    setCallee(ManagedValue(gen.emitGlobalConstantRef(e,
                    SILConstant(e->getDecl(), SILConstant::Kind::Initializer))));
  }
  void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e) {
    setSideEffect(e->getLHS());
    visit(e->getRHS());
  }
  void visitExistentialMemberRefExpr(ExistentialMemberRefExpr *e) {
    Value existential = gen.visit(e->getBase()).getUnmanagedValue();
    assert(existential.getType().isAddress() && "loadable existential?!");
    // FIXME: Use existential_metatype if method is static.
    setThisParam(ManagedValue(gen.B.createProjectExistential(e, existential)));
    setCallee(ManagedValue(gen.emitProtocolMethod(e, existential)));
  }
  void visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *e) {
    setThisParam(gen.visit(e->getBase()));
    assert(thisParam.getValue().getType().isAddress()
           && "loadable archetype?!");
    setCallee(ManagedValue(gen.emitArchetypeMethod(e, thisParam.getValue())));
  }
  void visitFunctionConversionExpr(FunctionConversionExpr *e) {
    visit(e->getSubExpr());
  }
  
  void applySuper(ApplyExpr *apply) {
    // Load the 'super' argument.
    // FIXME: Eliminate the implicit coercions of the SuperExpr.
    Expr *arg = apply->getArg();
    while (auto *conversion = dyn_cast<ImplicitConversionExpr>(arg))
      arg = conversion->getSubExpr();
    ManagedValue super = gen.visit(arg);
    if (super.isLValue()) {
      super = gen.emitManagedRValueWithCleanup(
                                       gen.B.createLoad(arg, super.getValue()));
    }
    setThisParam(super);
    
    // The callee for a super call has to be either a method or constructor.
    Expr *fn = apply->getFn();
    SILConstant constant;
    if (auto *ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(fn)) {
      constant = SILConstant(ctorRef->getDecl(), SILConstant::Kind::Initializer);
    } else if (auto *declRef = dyn_cast<DeclRefExpr>(fn)) {
      assert(isa<FuncDecl>(declRef->getDecl()) && "non-function super call?!");
      constant = SILConstant(declRef->getDecl());
    } else
      llvm_unreachable("invalid super callee");
  
    setCallee(ManagedValue(gen.B.createSuperMethod(apply, super.getValue(),
                                 constant, gen.SGM.getConstantType(constant))));
  }
};


namespace {  
  static void emitDestructureArgumentTuple(SILGenFunction &gen,
                                           SILLocation loc,
                                           Value argValue,
                                           SmallVectorImpl<Value> &argsV) {
    TupleType *ty = argValue.getType().castTo<TupleType>();
    
    // FIXME: address-only tuple
    // FIXME: varargs
    for (size_t i = 0, size = ty->getFields().size(); i < size; ++i) {
      Value elt = gen.B.createExtract(loc, argValue, i,
                      gen.getLoweredLoadableType(ty->getFields()[i].getType()));
      if (ty->getFields()[i].getType()->is<TupleType>())
        emitDestructureArgumentTuple(gen, loc, elt, argsV);
      else
        argsV.push_back(elt);
    }
  }
} // end anonymous namespace

void SILGenFunction::emitApplyArgumentValue(SILLocation loc,
                                            ManagedValue argValue,
                                            llvm::SmallVectorImpl<Value> &argsV)
{
  // If the result of the subexpression is a tuple value, destructure it.
  if (!argValue.isLValue() && argValue.getType().is<TupleType>()) {
    emitDestructureArgumentTuple(*this, loc,
                                 argValue.forward(*this), argsV);
    return;
  }
  
  // Otherwise, the value is a single argument.
  argsV.push_back(argValue.forwardArgument(*this, loc));
}

void SILGenFunction::emitApplyArguments(Expr *argsExpr,
                                        llvm::SmallVectorImpl<Value> &ArgsV) {
  // Skim off any ParenExprs.
  while (ParenExpr *pe = dyn_cast<ParenExpr>(argsExpr))
    argsExpr = pe->getSubExpr();
  
  // FIXME: byref arguments need to generate LValues with writebacks.
  
  // Visit the subexpression with an argument vector context so that tupling
  // expressions will destructure into the argument list.
  ManagedValue argValue = visit(argsExpr, SGFContext(ArgsV));
  
  // If visiting the subexpression returned a null value, then it emitted its
  // elements into the argument vector.
  if (!argValue.getValue())
    return;
  
  emitApplyArgumentValue(argsExpr, argValue, ArgsV);
}

ManagedValue SILGenFunction::emitApply(SILLocation Loc,
                                       Value Fn, ArrayRef<Value> Args) {
  // Get the result type.
  FunctionType *fty = Fn.getType().castTo<FunctionType>();
  for (unsigned uncurry = 0;
       uncurry < Fn.getType().getUncurryLevel();
       ++uncurry) {
    fty = fty->getResult()->castTo<FunctionType>();
  }
  TypeLoweringInfo const &resultTI = getTypeLoweringInfo(fty->getResult());
  
  if (resultTI.isAddressOnly()) {
    // Allocate a temporary to house the indirect return, and pass it to the
    // function as an implicit argument.
    SmallVector<Value, 4> argsWithReturn(Args.begin(), Args.end());
    Value buffer = emitTemporaryAllocation(Loc, resultTI.getLoweredType());
    argsWithReturn.push_back(buffer);
    B.createApply(Loc, Fn, SILType::getEmptyTupleType(F.getContext()),
                  argsWithReturn);
    return emitManagedAddressOnlyValue(buffer);
  } else {
    // Receive the result by value.
    Value result = B.createApply(Loc, Fn, resultTI.getLoweredType(), Args);
    return emitManagedRValueWithCleanup(result);
  }
}

ManagedValue SILGenFunction::emitApplyExpr(ApplyExpr *e) {
  SILGenApply apply(*this);
  
  // Decompose the call site.
  apply.visit(e);
  
  // Evaluate and discard the side effect if present.
  if (apply.sideEffect)
    visit(apply.sideEffect);
  
  // Build the call to the best uncurried entry point.
  unsigned curries = apply.callee.getType().getUncurryLevel() + 1;
  // FIXME: Need to fix emitApplyArgument to preserve cleanups and emit into
  // a ManagedValue vector instead of Value. We shouldn't forward until the
  // final apply is emitted.
  SmallVector<Value, 8> args;
  // Apply 'this' if provided.
  if (apply.thisParam.getValue()) {
    args.push_back(apply.thisParam.forward(*this));
    --curries;
  }
  // Apply arguments from call sites, innermost to outermost.
  auto site = apply.callSites.rbegin(), end = apply.callSites.rend();
  for (; curries > 0 && site != end; ++site, --curries) {
    emitApplyArguments((*site)->getArg(), args);
  }
  // Find the right callee.
  ManagedValue callee = apply.callee;
  // FIXME: If we had curry levels remaining, ask for a curried thunk.
  if (curries > 0) {
    llvm_unreachable("curried thunk not yet implemented");
  }
  
  // Emit the call.
  ManagedValue result = emitApply(e, callee.forward(*this), args);
  
  // If there are remaining call sites, apply them to the result function.
  for (; site != end; ++site) {
    args.clear();
    emitApplyArguments((*site)->getArg(), args);
    result = emitApply(e, result.forward(*this), args);
  }
  
  return result;
}