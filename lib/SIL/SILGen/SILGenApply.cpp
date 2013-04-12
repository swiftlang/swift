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
#include "RValue.h"

using namespace swift;
using namespace Lowering;

namespace {
  
/// Abstractly represents a callee, and knows how to emit the entry point
/// reference for a callee at any valid uncurry level.
class Callee {
public:
  enum class Kind {
    /// A generic SIL value.
    /// FIXME: We should use more specific kinds so we can emit curried calls
    /// to methods.
    GenericValue,
    /// A standalone function, referenceable by a ConstantRefInst.
    StandaloneFunction,
  };
  
  const Kind kind;
  
  // Move, don't copy.
  Callee(const Callee &) = delete;
  Callee &operator=(const Callee &) = delete;
private:
  union {
    ManagedValue genericValue;
    SILConstant standaloneFunction;
  };
  std::vector<Substitution> substitutions;
  SILType specializedType;
  SpecializeExpr *specializeLoc = nullptr;
  
public:
  Callee(ManagedValue genericValue)
    : kind(Kind::GenericValue), genericValue(genericValue),
      specializedType(genericValue.getType())
  {}
  
  Callee(SILGenFunction &gen, SILConstant standaloneFunction)
    : kind(Kind::StandaloneFunction), standaloneFunction(standaloneFunction),
      specializedType(gen.SGM.getConstantType(standaloneFunction))
  {}
  
  Callee(Callee &&) = default;
  Callee &operator=(Callee &&) = default;
  
  void addSubstitutions(SILGenFunction &gen,
                        SpecializeExpr *e,
                        unsigned callDepth) {
    // Currently generic methods of generic types are the deepest we should
    // be able to stack specializations.
    // FIXME: Generic local functions can add type parameters to arbitrary
    // depth.
    assert(callDepth < 2 && "specialization below 'this' or argument depth?!");
    substitutions.insert(substitutions.end(),
                         e->getSubstitutions().begin(),
                         e->getSubstitutions().end());
    // Save the type of the SpecializeExpr at the right depth in the type.
    assert(specializedType.getUncurryLevel() >= callDepth
           && "specializations below uncurry level?!");
    if (callDepth == 0) {
      specializedType = gen.getLoweredLoadableType(
                                             getThinFunctionType(e->getType()),
                                             specializedType.getUncurryLevel());
    } else {
      FunctionType *ft = specializedType.castTo<FunctionType>();
      Type outerInput = ft->getInput();
      Type newSpecialized = FunctionType::get(outerInput,
                                              e->getType(),
                                              /*isAutoClosure*/ false,
                                              /*isBlock*/ false,
                                              /*isThin*/ true,
                                              outerInput->getASTContext());
      specializedType = gen.getLoweredLoadableType(newSpecialized,
                                             specializedType.getUncurryLevel());
    }
    specializeLoc = e;
  }
  
  unsigned getNaturalUncurryLevel() const {
    return specializedType.getUncurryLevel();
  }
  
  ManagedValue getAtUncurryLevel(SILGenFunction &gen, unsigned level) const {
    ManagedValue mv;

    switch (kind) {
    case Kind::GenericValue:
      assert(level == genericValue.getType().getUncurryLevel()
             && "currying non-standalone function not yet supported");
      mv = genericValue;
      break;
    case Kind::StandaloneFunction: {
      assert(level <= standaloneFunction.uncurryLevel
             && "currying past natural uncurry level of standalone function");
      SILConstant constant = standaloneFunction.atUncurryLevel(level);
      SILValue ref = gen.emitGlobalConstantRef(SILLocation(), constant);
      mv = ManagedValue(ref, ManagedValue::Unmanaged);
      break;
    }
    };
    
    // If the callee needs to be specialized, do so.
    if (specializeLoc) {
      CleanupsDepth cleanup = mv.getCleanup();
      SILValue spec = gen.B.createSpecialize(specializeLoc, mv.getValue(),
                                             substitutions, specializedType);
      mv = ManagedValue(spec, cleanup);
    }
    
    return mv;
  }
};
  
/// An ASTVisitor for building SIL function calls.
/// Nested ApplyExprs applied to an underlying curried function or method
/// reference are flattened into a single SIL apply to the most uncurried entry
/// point fitting the call site, avoiding pointless intermediate closure
/// construction.
class SILGenApply : public ExprVisitor<SILGenApply>
{
public:
  SILGenFunction &gen;
  Optional<Callee> callee;
  RValue thisParam;
  std::vector<ApplyExpr*> callSites;
  Expr *sideEffect;
  unsigned callDepth;
  
  SILGenApply(SILGenFunction &gen)
    : gen(gen), sideEffect(nullptr), callDepth(0)
  {}
  
  void setCallee(ManagedValue theCallee) {
    assert((thisParam ? callDepth == 1 : callDepth == 0)
           && "setting callee at non-zero call depth?!");
    assert(!callee && "already set callee!");
    callee.emplace(theCallee);
  }
  
  void setCallee(SILConstant standaloneCallee) {
    assert((thisParam ? callDepth == 1 : callDepth == 0)
           && "setting callee at non-zero call depth?!");
    assert(!callee && "already set callee!");
    callee.emplace(gen, standaloneCallee);
  }
  
  void setSideEffect(Expr *sideEffectExpr) {
    assert(!sideEffect && "already set side effect!");
    sideEffect = sideEffectExpr;
  }
  
  void setThisParam(RValue &&theThisParam) {
    assert(!thisParam && "already set this!");
    thisParam = std::move(theThisParam);
    ++callDepth;
  }

  /// Fall back to an unknown, indirect callee.
  void visitExpr(Expr *e) {
    setCallee(gen.visit(e).getAsSingleValue(gen));
  }
  
  /// Add a call site to the curry.
  void visitApplyExpr(ApplyExpr *e) {
    if (e->isSuper()) {
      applySuper(e);
    } else {
      callSites.push_back(e);
      visit(e->getFn());
    }
    ++callDepth;
  }
  
  /// Add specializations to the curry.
  void visitSpecializeExpr(SpecializeExpr *e) {
    visit(e->getSubExpr());
    assert(callee && "did not find callee below SpecializeExpr?!");
    callee->addSubstitutions(gen, e, callDepth);
  }
  
  //
  // Known callees.
  //
  void visitDeclRefExpr(DeclRefExpr *e) {
    // If this is a non-extension class method, emit class_method to
    // dynamically dispatch the call.
    // FIXME: Or if it's an ObjC method. Extension methods on classes will
    // hopefully become dynamically dispatched too--SIL should be ignorant of
    // ObjC-ness.
    if (auto *fe = dyn_cast<FuncDecl>(e->getDecl())) {
      if (isa<ClassDecl>(fe->getDeclContext()) || fe->isObjC()) {
        ApplyExpr *thisCallSite = callSites.back();
        callSites.pop_back();
        setThisParam(gen.visit(thisCallSite->getArg()));
        SILConstant constant(fe);
        
        setCallee(ManagedValue(
                   gen.B.createClassMethod(thisCallSite,
                                           thisParam.peekScalarValue(),
                                           constant,
                                           gen.SGM.getConstantType(constant)),
                   ManagedValue::Unmanaged));
        // setThisParam bumps the callDepth, but we aren't really past the
        // 'this' call depth in this case.
        --callDepth;
        return;
      }
    }
    
    // FIXME: Store context values for local funcs in a way that we can
    // apply them directly as an added "call site" here.
    SILConstant constant(e->getDecl());

    // Obtain a reference for a local closure.
    if (gen.LocalConstants.count(constant))
      setCallee(gen.emitReferenceToDecl(e, e->getDecl()));
    // Otherwise, stash the SILConstant.
    else
      setCallee(constant);
  }
  void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *e) {
    setCallee(ManagedValue(gen.emitGlobalConstantRef(e,
                    SILConstant(e->getDecl(), SILConstant::Kind::Initializer)),
                  ManagedValue::Unmanaged));
  }
  void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e) {
    setSideEffect(e->getLHS());
    visit(e->getRHS());
  }
  void visitExistentialMemberRefExpr(ExistentialMemberRefExpr *e) {
    ManagedValue existential = gen.visit(e->getBase()).getAsSingleValue(gen);
    assert(existential.getType().isAddress() && "loadable existential?!");
    // FIXME: Use existential_metatype if method is static.
    
    // Attach the existential cleanup to the projection so that it gets consumed
    // (or not) when the call is applied to it (or isn't).
    ManagedValue projection
      = ManagedValue(gen.B.createProjectExistential(e, existential.getValue()),
                     existential.getCleanup());
    
    setThisParam(RValue(gen, projection));
    setCallee(ManagedValue(gen.emitProtocolMethod(e, existential.getValue()),
                           ManagedValue::Unmanaged));
  }
  void visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *e) {
    setThisParam(gen.visit(e->getBase()));
    setCallee(ManagedValue(gen.emitArchetypeMethod(e,
                                                   thisParam.peekScalarValue()),
                           ManagedValue::Unmanaged));
  }
  void visitFunctionConversionExpr(FunctionConversionExpr *e) {
    visit(e->getSubExpr());
  }
  
  void visitParenExpr(ParenExpr *e) {
    visit(e->getSubExpr());
  }
  
  void visitCoerceExpr(CoerceExpr *e) {
    visit(e->getSubExpr());
  }
  
  void applySuper(ApplyExpr *apply) {
    // Load the 'super' argument.
    // FIXME: Eliminate the implicit coercions of the SuperExpr.
    Expr *arg = apply->getArg();
    while (auto *conversion = dyn_cast<ImplicitConversionExpr>(arg))
      arg = conversion->getSubExpr();
    ManagedValue super = gen.visit(arg).getAsSingleValue(gen);
    if (super.isLValue()) {
      super = gen.emitManagedRValueWithCleanup(
                                       gen.B.createLoad(arg, super.getValue()));
      gen.emitRetainRValue(arg, super.getValue());
    }
    
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

    // Upcast 'this' parameter to the super type.
    SILType constantTy = gen.SGM.getConstantType(constant);
    SILType constantThisTy
      = gen.getLoweredLoadableType(constantTy.castTo<FunctionType>()->getInput());
    SILValue superUpcast = gen.B.createUpcast(apply->getArg(), super.getValue(),
                                           constantThisTy);
    
    setThisParam(RValue(gen, ManagedValue(superUpcast, super.getCleanup())));
    setCallee(ManagedValue(gen.B.createSuperMethod(apply, super.getValue(),
                                 constant, constantTy),
                           ManagedValue::Unmanaged));
  }
  
  Callee getCallee() {
    assert(callee && "did not find callee?!");
    return *std::move(callee);
  }
};

} // end anonymous namespace

ManagedValue SILGenFunction::emitApply(SILLocation Loc,
                                       SILValue Fn,
                                       ArrayRef<ManagedValue> Args) {
  // Get the result type.
  Type resultTy = Fn.getType().getFunctionResultType();
  TypeLoweringInfo const &resultTI = getTypeLoweringInfo(resultTy);
  
  if (resultTI.isAddressOnly()) {
    // Allocate a temporary to house the indirect return, and pass it to the
    // function as an implicit argument.
    // FIXME: Should pass down SGFContext so we can emit into an initialization.
    SILValue buffer = emitTemporaryAllocation(Loc, resultTI.getLoweredType());
    auto argsWithReturn
      = map<SmallVector<SILValue, 4>>(Args,
                                      [&](ManagedValue v) {
                                        return v.forwardArgument(*this, Loc);
                                      });
    argsWithReturn.push_back(buffer);
    B.createApply(Loc, Fn, SGM.Types.getEmptyTupleType(),
                  argsWithReturn);

    return emitManagedRValueWithCleanup(buffer);
  } else {
    // Receive the result by value.
    auto fwdArgs
      = map<SmallVector<SILValue, 4>>(Args,
                                      [&](ManagedValue v) {
                                        return v.forwardArgument(*this, Loc);
                                      });

    SILValue result = B.createApply(Loc, Fn, resultTI.getLoweredType(),
                                    fwdArgs);
    return resultTy->is<LValueType>()
      ? ManagedValue(result, ManagedValue::LValue)
      : emitManagedRValueWithCleanup(result);
  }
}

namespace {
  class CallEmission {
    SILGenFunction &gen;
    
    // FIXME: Need to fix emitApplyArgument to preserve cleanups and emit into
    // a ManagedValue vector instead of SILValue. We shouldn't forward until the
    // final apply is emitted.
    SmallVector<ManagedValue, 8> uncurriedArgs;
    std::vector<SmallVector<ManagedValue, 2>> extraArgs;
    Callee callee;
    unsigned uncurries;
    bool applied;
    
  public:
    CallEmission(SILGenFunction &gen, Callee &&callee)
      : gen(gen),
        callee(std::move(callee)),
        uncurries(callee.getNaturalUncurryLevel() + 1),
        applied(false)
    {}
    
    void addArgs(RValue &&args) {
      assert(!applied && "already applied!");

      // Append to the main argument list if we have uncurry levels remaining.
      if (uncurries > 0) {
        --uncurries;
        std::move(args).getAll(uncurriedArgs);
        return;
      }
      
      // Otherwise, apply these arguments to the result of the previous call.
      extraArgs.emplace_back();
      std::move(args).getAll(extraArgs.back());
    }
    
    // FIXME: Tie argument clauses to more specific expr nodes.
    ManagedValue apply(SILLocation loc) {
      assert(!applied && "already applied!");
      
      applied = true;

      // Get the callee value at the needed uncurry level.
      unsigned uncurryLevel = callee.getNaturalUncurryLevel() - uncurries;
      ManagedValue calleeValue = callee.getAtUncurryLevel(gen, uncurryLevel);

      // Emit the uncurried call.
      ManagedValue result = gen.emitApply(loc,
                                          calleeValue.forward(gen),
                                          uncurriedArgs);
      
      // If there are remaining call sites, apply them to the result function.
      for (ArrayRef<ManagedValue> args : extraArgs) {
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
} // end anonymous namespace

static CallEmission prepareApplyExpr(SILGenFunction &gen, Expr *e) {
  SILGenApply apply(gen);
  
  // Decompose the call site.
  apply.visit(e);
  
  // Evaluate and discard the side effect if present.
  if (apply.sideEffect)
    gen.visit(apply.sideEffect);
  
  // Build the call.
  CallEmission emission(gen, apply.getCallee());
  
  // Apply 'this' if provided.
  if (apply.thisParam) {
    emission.addArgs(std::move(apply.thisParam));
  }
  // Apply arguments from call sites, innermost to outermost.
  for (auto site = apply.callSites.rbegin(), end = apply.callSites.rend();
       site != end;
       ++site) {
    emission.addArgs(gen.visit((*site)->getArg()));
  }
  
  return emission;
}

RValue SILGenFunction::emitApplyExpr(ApplyExpr *e) {
  return RValue(*this, prepareApplyExpr(*this, e).apply(e));
}

/// emitArrayInjectionCall - Form an array "Slice" out of an ObjectPointer
/// (which represents the retain count), a base pointer to some elements, and a
/// length.
ManagedValue SILGenFunction::emitArrayInjectionCall(SILValue ObjectPtr,
                                            SILValue BasePtr,
                                            SILValue Length,
                                            Expr *ArrayInjectionFunction) {
  // Bitcast the BasePtr (an lvalue) to Builtin.RawPointer if it isn't already.
  if (BasePtr.getType() != SILType::getRawPointerType(F.getContext()))
    BasePtr = B.createAddressToPointer(SILLocation(),
                              BasePtr,
                              SILType::getRawPointerType(F.getContext()));

  // Construct a call to the injection function.
  CallEmission emission = prepareApplyExpr(*this, ArrayInjectionFunction);
  
  CanType injectionArgsTy
   = ArrayInjectionFunction->getType()->getAs<FunctionType>()->getInput()
     ->getCanonicalType();
  RValue InjectionArgs(injectionArgsTy);
  InjectionArgs.addElement(RValue(*this,
                                ManagedValue(BasePtr, ManagedValue::Unmanaged)));
  // FIXME: ObjectPtr ought to have a cleanup on it.
  InjectionArgs.addElement(RValue(*this,
                                ManagedValue(ObjectPtr, ManagedValue::Unmanaged)));
  InjectionArgs.addElement(RValue(*this,
                                ManagedValue(Length, ManagedValue::Unmanaged)));
  
  emission.addArgs(std::move(InjectionArgs));
  return emission.apply(SILLocation());
}

/// Emit a call to a getter and materialize its result.
Materialize SILGenFunction::emitGetProperty(SILLocation loc,
                                            ManagedValue getter,
                                            RValue &&thisValue,
                                            RValue &&subscripts) {
  CallEmission emission(*this, getter);
  // This ->
  if (thisValue)
    emission.addArgs(std::move(thisValue));
  // Index ->
  if (subscripts)
    emission.addArgs(std::move(subscripts));
  // () ->
  emission.addArgs(emitEmptyTupleRValue(loc));
  // T
  ManagedValue result = emission.apply(loc);
  return emitMaterialize(loc, result);
}

void SILGenFunction::emitSetProperty(SILLocation loc,
                                     ManagedValue setter,
                                     RValue &&thisValue,
                                     RValue &&subscripts,
                                     RValue &&setValue) {
  CallEmission emission(*this, setter);
  // This ->
  if (thisValue)
    emission.addArgs(std::move(thisValue));
  // Index ->
  if (subscripts)
    emission.addArgs(std::move(subscripts));
  // T ->
  emission.addArgs(std::move(setValue));
  // ()
  emission.apply(loc);
}