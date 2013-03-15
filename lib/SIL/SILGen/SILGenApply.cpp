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

namespace {
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
  std::vector<Substitution> substitutions;
  FunctionType *specializedType;
  SpecializeExpr *specializeLoc;
  Expr *sideEffect;
  
  SILGenApply(SILGenFunction &gen) : gen(gen),
    specializedType(nullptr), specializeLoc(nullptr), sideEffect(nullptr) {}
  
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
    visit(e->getSubExpr());
    substitutions.insert(substitutions.end(),
                         e->getSubstitutions().begin(),
                         e->getSubstitutions().end());
    // Currently generic methods of generic types are the deepest we should
    // be able to stack specializations.
    // Save the type of the SpecializeExpr.
    if (!specializedType) {
      assert(substitutions.size() == 1 &&
             "saw specialization before setting specialized type?");
      specializedType = e->getType()->castTo<FunctionType>();
      specializeLoc = e;
    } else {
      assert(substitutions.size() == 2 &&
             "saw more than two specializations?!");
      assert(callee.getType().getUncurryLevel() >= 1 &&
             "multiple specializations of non-uncurried method?!");
      specializedType = FunctionType::get(specializedType->getInput(),
                                          e->getType(),
                                          specializedType->getASTContext());
    }
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
    assert((thisParam.getValue().getType().isAddress()
            || thisParam.getValue().getType().is<MetaTypeType>())
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
  
  ManagedValue getSpecializedCallee() {
    // If the callee needs to be specialized, do so.
    if (specializedType) {
      CleanupsDepth cleanup = callee.getCleanup();
      SILType ty = gen.getLoweredLoadableType(specializedType,
                                              callee.getType().getUncurryLevel());
      Value spec = gen.B.createSpecialize(specializeLoc, callee.getValue(),
                                          substitutions, ty);
      return ManagedValue(spec, cleanup);
    }
    
    return callee;
  }
};

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

namespace {
  class CallEmission {
    SILGenFunction &gen;
    
    // FIXME: Need to fix emitApplyArgument to preserve cleanups and emit into
    // a ManagedValue vector instead of Value. We shouldn't forward until the
    // final apply is emitted.
    SmallVector<Value, 8> uncurriedArgs;
    std::vector<SmallVector<Value, 2>> extraArgs;
    ManagedValue callee;
    unsigned uncurries;
    bool applied;
    
  public:
    CallEmission(SILGenFunction &gen, ManagedValue callee)
      : gen(gen),
        callee(callee),
        uncurries(callee.getType().getUncurryLevel() + 1),
        applied(false)
    {}
    
    SmallVectorImpl<Value> &addArgs() {
      assert(!applied && "already applied!");

      // Append to the main argument list if we have uncurry levels remaining.
      if (uncurries > 0) {
        --uncurries;
        return uncurriedArgs;
      }
      // Otherwise, apply these arguments to the result of the uncurried call.
      extraArgs.emplace_back();
      return extraArgs.back();
    }
    
    // FIXME: Tie argument clauses to more specific expr nodes.
    ManagedValue apply(SILLocation loc) {
      assert(!applied && "already applied!");
      
      applied = true;

      // See if we need a curried thunk.
      // FIXME: Implement this.
      if (uncurries > 0) {
        llvm_unreachable("curried thunk not yet implemented");
      }

      // Emit the uncurried call.
      ManagedValue result = gen.emitApply(loc, callee.forward(gen), uncurriedArgs);
      
      // If there are remaining call sites, apply them to the result function.
      for (ArrayRef<Value> args : extraArgs) {
        result = gen.emitApply(loc, result.forward(gen), args);
      }
      
      return result;
    }
    
    ~CallEmission() { assert(applied && "never applied!"); }

    // Movable, but not copyable.
    CallEmission(CallEmission &&e)
      : gen(e.gen),
        uncurriedArgs(e.uncurriedArgs),
        extraArgs(std::move(e.extraArgs)),
        callee(std::move(e.callee)),
        uncurries(e.uncurries),
        applied(e.applied) {
      e.applied = true;
    }
  private:
    CallEmission(const CallEmission &) = delete;
    CallEmission &operator=(const CallEmission &) = delete;
  };
}

static CallEmission prepareApplyExpr(SILGenFunction &gen, Expr *e) {
  SILGenApply apply(gen);
  
  // Decompose the call site.
  apply.visit(e);
  
  // Evaluate and discard the side effect if present.
  if (apply.sideEffect)
    gen.visit(apply.sideEffect);
  
  // Build the call.
  CallEmission emission(gen, apply.getSpecializedCallee());
  
  // Apply 'this' if provided.
  if (apply.thisParam.getValue()) {
    emission.addArgs().push_back(apply.thisParam.forward(gen));
  }
  // Apply arguments from call sites, innermost to outermost.
  for (auto site = apply.callSites.rbegin(), end = apply.callSites.rend();
       site != end;
       ++site) {
    gen.emitApplyArguments((*site)->getArg(), emission.addArgs());
  }
  
  return emission;
}

ManagedValue SILGenFunction::emitApplyExpr(ApplyExpr *e) {
  return prepareApplyExpr(*this, e).apply(e);
}

/// emitArrayInjectionCall - Form an array "Slice" out of an ObjectPointer
/// (which represents the retain count), a base pointer to some elements, and a
/// length.
ManagedValue SILGenFunction::emitArrayInjectionCall(Value ObjectPtr,
                                            Value BasePtr,
                                            Value Length,
                                            Expr *ArrayInjectionFunction) {
  // Bitcast the BasePtr (an lvalue) to Builtin.RawPointer if it isn't already.
  if (BasePtr.getType() != SILType::getRawPointerType(F.getContext()))
    BasePtr = B.createImplicitConvert(SILLocation(),
                              BasePtr,
                              SILType::getRawPointerType(F.getContext()));

  // Construct a call to the injection function.
  CallEmission emission = prepareApplyExpr(*this, ArrayInjectionFunction);
  Value InjectionArgs[] = {BasePtr, ObjectPtr, Length};
  
  emission.addArgs().append(std::begin(InjectionArgs), std::end(InjectionArgs));
  
  return emission.apply(SILLocation());
}

/// Emit a call to a getter and materialize its result.
Materialize SILGenFunction::emitGetProperty(SILLocation loc,
                                  ManagedValue getter,
                                  Optional<ManagedValue> thisValue,
                                  Optional<ArrayRef<Value>> subscripts) {
  CallEmission emission(*this, getter);
  // This ->
  if (thisValue)
    emission.addArgs().push_back(thisValue->forward(*this));
  // Index ->
  if (subscripts)
    emission.addArgs().append(subscripts->begin(), subscripts->end());
  // () ->
  emission.addArgs();
  // T
  ManagedValue result = emission.apply(loc);
  return emitMaterialize(loc, result);
}

void SILGenFunction::emitSetProperty(SILLocation loc,
                                     ManagedValue setter,
                                     Optional<ManagedValue> thisValue,
                                     Optional<ArrayRef<Value>> subscripts,
                                     ManagedValue result) {
  CallEmission emission(*this, setter);
  // This ->
  if (thisValue)
    emission.addArgs().push_back(thisValue->forward(*this));
  // Index ->
  if (subscripts)
    emission.addArgs().append(subscripts->begin(), subscripts->end());
  // T ->
  emission.addArgs().push_back(result.forward(*this));
  // ()
  emission.apply(loc);
}