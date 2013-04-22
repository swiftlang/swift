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
#include "OwnershipConventions.h"
#include "RValue.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Module.h"
#include "swift/Basic/Range.h"

using namespace swift;
using namespace Lowering;

namespace {

class CallEmission;
  
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
  
  using SpecializedEmitter = ManagedValue (*)(SILGenFunction &,
                                              SILLocation,
                                              ArrayRef<Substitution>,
                                              ArrayRef<ManagedValue>,
                                              SGFContext);
  
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
  OwnershipConventions ownership;
  
  static SpecializedEmitter getSpecializedEmitterForSILBuiltin(SILConstant c);
  
public:
  Callee(ManagedValue genericValue, OwnershipConventions &&ownership)
    : kind(Kind::GenericValue), genericValue(genericValue),
      specializedType(genericValue.getType()),
      ownership(std::move(ownership))
  {}
  
  Callee(SILGenFunction &gen, SILConstant standaloneFunction)
    : kind(Kind::StandaloneFunction), standaloneFunction(standaloneFunction),
      specializedType(gen.SGM.getConstantType(standaloneFunction)),
      ownership(OwnershipConventions::get(gen, standaloneFunction))
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
  
  OwnershipConventions const &getOwnershipConventions() const {
    // FIXME: May need to adjust ownership conventions with uncurry level?
    return ownership;
  }
  
  ArrayRef<Substitution> getSubstitutions() const {
    return substitutions;
  }

  /// Return a specialized emission function if this is a function with a known
  /// lowering, such as a builtin, or return null if there is no specialized
  /// emitter.
  SpecializedEmitter getSpecializedEmitter(unsigned uncurryLevel) const {
    // Currently we have no curried known functions.
    if (uncurryLevel != 0)
      return nullptr;
    
    switch (kind) {
    case Kind::StandaloneFunction: {
      if (SpecializedEmitter e
            = getSpecializedEmitterForSILBuiltin(standaloneFunction))
        return e;
    }
    case Kind::GenericValue:
      break;
    }
    return nullptr;
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
  
  void setCallee(ManagedValue theCallee, OwnershipConventions &&ownership) {
    assert((thisParam ? callDepth == 1 : callDepth == 0)
           && "setting callee at non-zero call depth?!");
    assert(!callee && "already set callee!");
    callee.emplace(theCallee, std::move(ownership));
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
    ManagedValue fn = gen.visit(e).getAsSingleValue(gen);
    setCallee(fn, OwnershipConventions::getDefault(fn.getType()));
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
    // hopefully become dynamically dispatched too.
    if (auto *fe = dyn_cast<FuncDecl>(e->getDecl())) {
      if (isa<ClassDecl>(fe->getDeclContext()) || fe->isObjC()) {
        ApplyExpr *thisCallSite = callSites.back();
        callSites.pop_back();
        setThisParam(gen.visit(thisCallSite->getArg()));
        SILConstant constant(fe);
        
        SILValue classMethod = gen.B.createClassMethod(thisCallSite,
                                             thisParam.peekScalarValue(),
                                             constant,
                                             gen.SGM.getConstantType(constant));
        setCallee(ManagedValue(classMethod, ManagedValue::Unmanaged),
                  OwnershipConventions::get(gen, constant));
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
    if (gen.LocalConstants.count(constant)) {
      ManagedValue localFn = gen.emitReferenceToDecl(e, e->getDecl());
      setCallee(localFn,
                OwnershipConventions::getDefault(localFn.getType()));
    }
    // Otherwise, stash the SILConstant.
    else
      setCallee(constant);
  }
  void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *e) {
    setCallee(SILConstant(e->getDecl(), SILConstant::Kind::Initializer));
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
    ManagedValue protoMethod(gen.emitProtocolMethod(e, existential.getValue()),
                             ManagedValue::Unmanaged);
    setCallee(protoMethod,
              OwnershipConventions::getDefault(protoMethod.getType()));
  }
  void visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *e) {
    setThisParam(gen.visit(e->getBase()));
    ManagedValue archeMethod(
                      gen.emitArchetypeMethod(e, thisParam.peekScalarValue()),
                      ManagedValue::Unmanaged);
    setCallee(archeMethod,
              OwnershipConventions::getDefault(archeMethod.getType()));
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
    
    SILValue superMethod = gen.B.createSuperMethod(apply, super.getValue(),
                                                   constant, constantTy);
    setCallee(ManagedValue(superMethod, ManagedValue::Unmanaged),
              OwnershipConventions::get(gen, constant));
  }
  
  Callee getCallee() {
    assert(callee && "did not find callee?!");
    return *std::move(callee);
  }
};

} // end anonymous namespace

ManagedValue SILGenFunction::emitApply(SILLocation Loc,
                                       ManagedValue Fn,
                                       ArrayRef<ManagedValue> Args,
                                       OwnershipConventions const &Ownership,
                                       SGFContext C) {
  // Conditionally consume the cleanup on an input value.
  auto forwardIfConsumed = [&](ManagedValue v, bool consumed) -> SILValue {
    return consumed
      ? v.forwardArgument(*this, Loc)
      : v.getArgumentValue(*this, Loc);
  };
  
  // Get the result type.
  Type resultTy = Fn.getType().getFunctionResultType();
  TypeLoweringInfo const &resultTI = getTypeLoweringInfo(resultTy);
  
  // Get the callee value.
  SILValue fnValue = Ownership.isCalleeConsumed()
    ? Fn.forward(*this)
    : Fn.getValue();
  
  // Gather the arguments.
  SmallVector<SILValue, 4> argValues;
  for (size_t i = 0; i < Args.size(); ++i)
    argValues.push_back(
                  forwardIfConsumed(Args[i], Ownership.isArgumentConsumed(i)));
  
  if (resultTI.isAddressOnly()) {
    // Allocate a temporary to house the indirect return, and pass it to the
    // function as an implicit argument.
    SILValue buffer = getBufferForExprResult(Loc, resultTI.getLoweredType(), C);
    argValues.push_back(buffer);
    B.createApply(Loc, fnValue, SGM.Types.getEmptyTupleType(),
                  argValues);

    /// FIXME: Can ObjC/C functions return types SIL considers address-only?
    /// Do we need to copy here if the return value is Unretained?
    return emitManagedRValueWithCleanup(buffer);
  } else {
    // Receive the result by value.
    SILValue result = B.createApply(Loc, fnValue,
                                    resultTI.getLoweredType(),
                                    argValues);
    
    // Take ownership of the return value, if necessary.
    switch (Ownership.getReturn()) {
    case OwnershipConventions::Return::Retained:
      // Already retained.
      break;

    case OwnershipConventions::Return::Autoreleased:
      // Autoreleased. Retain using retain_autoreleased.
      B.createRetainAutoreleased(Loc, result);
      break;
        
    case OwnershipConventions::Return::Unretained:
      // Unretained. Retain the value.
      emitRetainRValue(Loc, result);
      break;
    }
    
    return resultTy->is<LValueType>()
      ? ManagedValue(result, ManagedValue::LValue)
      : emitManagedRValueWithCleanup(result);
  }
}

namespace {
  class CallSite {
  public:
    enum class Kind {
      Expr,
      Value
    };

    const Kind kind;
    SILLocation loc;

  private:
    union {
      Expr *expr;
      RValue value;
    };
    
  public:
    CallSite(ApplyExpr *apply)
      : kind(Kind::Expr), loc(apply), expr(apply->getArg()) {}
    
    CallSite(SILLocation loc, Expr *expr)
      : kind(Kind::Expr), loc(loc), expr(expr) {}
    
    CallSite(SILLocation loc, RValue &&value)
      : kind(Kind::Value), loc(loc), value(std::move(value)) {}
    
    ~CallSite() {
      switch (kind) {
      case Kind::Expr:
        return;
      case Kind::Value:
        value.~RValue();
        return;
      }
    }
    
    CallSite(CallSite &&o) : kind(o.kind), loc(o.loc) {
      switch (kind) {
      case Kind::Expr:
        expr = o.expr;
        return;
      case Kind::Value:
        ::new (&value) RValue(std::move(o.value));
        return;
      }
    }
    
    void emit(SILGenFunction &gen, SmallVectorImpl<ManagedValue> &args) && {
      switch (kind) {
      case Kind::Expr:
        gen.visit(expr).getAll(args);
        return;

      case Kind::Value:
        std::move(value).getAll(args);
        return;
      }
    }
  };
  
  class CallEmission {
    SILGenFunction &gen;
    
    std::vector<CallSite> uncurriedSites;
    std::vector<CallSite> extraSites;
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
    
    void addCallSite(CallSite &&site) {
      assert(!applied && "already applied!");

      // Append to the main argument list if we have uncurry levels remaining.
      if (uncurries > 0) {
        --uncurries;
        uncurriedSites.push_back(std::move(site));
        return;
      }
      
      // Otherwise, apply these arguments to the result of the previous call.
      extraSites.push_back(std::move(site));
    }
    
    template<typename...T>
    void addCallSite(T &&...args) {
      addCallSite(CallSite{std::forward<T>(args)...});
    }
    
    ManagedValue apply(SGFContext C = SGFContext()) {
      assert(!applied && "already applied!");
      
      applied = true;

      // Get the callee value at the needed uncurry level.
      unsigned uncurryLevel = callee.getNaturalUncurryLevel() - uncurries;
      
      // Get either the specialized emitter for a known function, or the
      // function value for a normal callee.
      Callee::SpecializedEmitter specializedEmitter
        = callee.getSpecializedEmitter(uncurryLevel);

      ManagedValue calleeValue;
      if (!specializedEmitter)
        calleeValue = callee.getAtUncurryLevel(gen, uncurryLevel);
      
      // Collect the arguments to the uncurried call.
      SmallVector<ManagedValue, 4> args;
      SILLocation uncurriedLoc;
      for (auto &site : uncurriedSites) {
        uncurriedLoc = site.loc;
        std::move(site).emit(gen, args);
      }
      
      // We use the context emit-into initialization only for the outermost
      // call.
      SGFContext uncurriedContext = extraSites.empty() ? C : SGFContext();

      // Emit the uncurried call.
      ManagedValue result;
      
      if (specializedEmitter)
        result = specializedEmitter(gen,
                                    uncurriedLoc,
                                    callee.getSubstitutions(),
                                    args,
                                    uncurriedContext);
      else
        result = gen.emitApply(uncurriedLoc, calleeValue, args,
                               callee.getOwnershipConventions(),
                               uncurriedContext);
      
      // If there are remaining call sites, apply them to the result function.
      for (unsigned i = 0, size = extraSites.size(); i < size; ++i) {
        args.clear();
        SILLocation loc = extraSites[i].loc;
        std::move(extraSites[i]).emit(gen, args);
        SGFContext context = i == size - 1 ? C : SGFContext();
        result = gen.emitApply(loc, result, args,
                               callee.getOwnershipConventions(),
                               context);
      }
      
      return result;
    }
    
    ~CallEmission() { assert(applied && "never applied!"); }

    // Movable, but not copyable.
    CallEmission(CallEmission &&e)
      : gen(e.gen),
        uncurriedSites(std::move(e.uncurriedSites)),
        extraSites(std::move(e.extraSites)),
        callee(std::move(e.callee)),
        uncurries(e.uncurries),
        applied(e.applied) {
      e.applied = true;
    }
    
  private:
    CallEmission(const CallEmission &) = delete;
    CallEmission &operator=(const CallEmission &) = delete;
  };

  /// Specialized emitter for Builtin.load and Builtin.move.
  static ManagedValue emitBuiltinLoadOrMove(SILGenFunction &gen,
                                            SILLocation loc,
                                            ArrayRef<Substitution> substitutions,
                                            ArrayRef<ManagedValue> args,
                                            SGFContext C,
                                            bool isTake) {
    assert(substitutions.size() == 1 && "load should have single substitution");
    assert(args.size() == 1 && "load should have a single argument");
    
    // The substitution gives the type of the load.
    SILType loadedType = gen.getLoweredType(substitutions[0].Replacement);
    // Convert the pointer argument to a SIL address.
    SILValue addr = gen.B.createPointerToAddress(loc, args[0].getUnmanagedValue(),
                                                 loadedType.getAddressType());
    // Perform the load.
    return gen.emitLoad(loc, addr, C, isTake);
  }

  static ManagedValue emitBuiltinLoad(SILGenFunction &gen,
                                      SILLocation loc,
                                      ArrayRef<Substitution> substitutions,
                                      ArrayRef<ManagedValue> args,
                                      SGFContext C) {
    return emitBuiltinLoadOrMove(gen, loc, substitutions, args, C,
                                 /*isTake*/ false);
  }

  static ManagedValue emitBuiltinMove(SILGenFunction &gen,
                                      SILLocation loc,
                                      ArrayRef<Substitution> substitutions,
                                      ArrayRef<ManagedValue> args,
                                      SGFContext C) {
    return emitBuiltinLoadOrMove(gen, loc, substitutions, args, C,
                                 /*isTake*/ true);
  }

  /// Specialized emitter for Builtin.destroy.
  static ManagedValue emitBuiltinDestroy(SILGenFunction &gen,
                                         SILLocation loc,
                                         ArrayRef<Substitution> substitutions,
                                         ArrayRef<ManagedValue> args,
                                         SGFContext C) {
    assert(args.size() == 2 && "destroy should have two arguments");
    assert(substitutions.size() == 1 &&
           "destroy should have a single substitution");
    // The substitution determines the type of the thing we're destroying.
    auto &ti = gen.getTypeLoweringInfo(substitutions[0].Replacement);
    
    // Destroy is a no-op for trivial types.
    if (ti.isTrivial())
      return ManagedValue(gen.emitEmptyTuple(loc), ManagedValue::Unmanaged);
    
    SILType destroyType = ti.getLoweredType();

    // Convert the pointer argument to a SIL address.
    SILValue addr = gen.B.createPointerToAddress(loc, args[1].getUnmanagedValue(),
                                                 destroyType.getAddressType());
    
    if (destroyType.isAddressOnly()) {
      // If the type is address-only, destroy it indirectly.
      gen.B.createDestroyAddr(loc, addr);
    } else {
      // Otherwise, load and release the value.
      SILValue value = gen.B.createLoad(loc, addr);
      gen.emitReleaseRValue(loc, value);
    }
    
    return ManagedValue(gen.emitEmptyTuple(loc), ManagedValue::Unmanaged);
  }

  /// Specialized emitter for Builtin.assign and Builtin.init.
  static ManagedValue emitBuiltinAssignOrInit(SILGenFunction &gen,
                                        SILLocation loc,
                                        ArrayRef<Substitution> substitutions,
                                        ArrayRef<ManagedValue> args,
                                        SGFContext C,
                                        bool isInitialization) {
    assert(args.size() >= 2 && "assign should have two arguments");
    assert(substitutions.size() == 1 &&
           "assign should have a single substitution");

    // The substitution determines the type of the thing we're destroying.
    SILType assignType = gen.getLoweredType(substitutions[0].Replacement);
    
    // Convert the destination pointer argument to a SIL address.
    SILValue addr = gen.B.createPointerToAddress(loc,
                                                 args.back().getUnmanagedValue(),
                                                 assignType.getAddressType());
    
    // Build the value to be assigned, reconstructing tuples if needed.
    ManagedValue src = RValue(args.slice(0, args.size() - 1),
                              assignType.getSwiftRValueType())
      .getAsSingleValue(gen);
    
    if (isInitialization)
      src.forwardInto(gen, loc, addr);
    else
      src.assignInto(gen, loc, addr);
    return ManagedValue(gen.emitEmptyTuple(loc), ManagedValue::Unmanaged);
  }

  static ManagedValue emitBuiltinAssign(SILGenFunction &gen,
                                        SILLocation loc,
                                        ArrayRef<Substitution> substitutions,
                                        ArrayRef<ManagedValue> args,
                                        SGFContext C) {
    return emitBuiltinAssignOrInit(gen, loc, substitutions, args, C,
                                   /*isInitialization*/ false);
  }

  static ManagedValue emitBuiltinInit(SILGenFunction &gen,
                                      SILLocation loc,
                                      ArrayRef<Substitution> substitutions,
                                      ArrayRef<ManagedValue> args,
                                      SGFContext C) {
    return emitBuiltinAssignOrInit(gen, loc, substitutions, args, C,
                                   /*isInitialization*/ true);
  }

  /// Specialized emitter for Builtin.castToObjectPointer.
  static ManagedValue emitBuiltinCastToObjectPointer(SILGenFunction &gen,
                                           SILLocation loc,
                                           ArrayRef<Substitution> substitutions,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext C) {
    assert(args.size() == 1 && "cast should have a single argument");
    
    // Save the cleanup on the argument so we can forward it onto the cast
    // result.
    auto cleanup = args[0].getCleanup();
    
    // Take the reference type argument and cast it to ObjectPointer.
    SILType objPointerType = SILType::getObjectPointerType(gen.F.getContext());
    SILValue result = gen.B.createRefToObjectPointer(loc, args[0].getValue(),
                                                     objPointerType);
    // Return the cast result with the original cleanup.
    return ManagedValue(result, cleanup);
  }

  /// Specialized emitter for Builtin.castFromObjectPointer.
  static ManagedValue emitBuiltinCastFromObjectPointer(SILGenFunction &gen,
                                           SILLocation loc,
                                           ArrayRef<Substitution> substitutions,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext C) {
    assert(args.size() == 1 && "cast should have a single argument");
    assert(substitutions.size() == 1 &&
           "cast should have a single substitution");

    // Save the cleanup on the argument so we can forward it onto the cast
    // result.
    auto cleanup = args[0].getCleanup();

    // The substitution determines the destination type.
    // FIXME: Archetype destination type?
    SILType destType = gen.getLoweredLoadableType(substitutions[0].Replacement);
    
    // Take the reference type argument and cast it.
    SILValue result = gen.B.createObjectPointerToRef(loc, args[0].getValue(),
                                                     destType);
    // Return the cast result with the original cleanup.
    return ManagedValue(result, cleanup);
  }

  /// Specialized emitter for Builtin.bridgeToRawPointer.
  static ManagedValue emitBuiltinBridgeToRawPointer(SILGenFunction &gen,
                                          SILLocation loc,
                                          ArrayRef<Substitution> substitutions,
                                          ArrayRef<ManagedValue> args,
                                          SGFContext C) {
    assert(args.size() == 1 && "bridge should have a single argument");
    
    // Take the reference type argument and cast it to RawPointer.
    // RawPointers do not have ownership semantics, so the cleanup on the
    // argument remains.
    SILType rawPointerType = SILType::getRawPointerType(gen.F.getContext());
    SILValue result = gen.B.createRefToRawPointer(loc, args[0].getValue(),
                                                  rawPointerType);
    return ManagedValue(result, ManagedValue::Unmanaged);
  }

  /// Specialized emitter for Builtin.bridgeFromRawPointer.
  static ManagedValue emitBuiltinBridgeFromRawPointer(SILGenFunction &gen,
                                          SILLocation loc,
                                          ArrayRef<Substitution> substitutions,
                                          ArrayRef<ManagedValue> args,
                                          SGFContext C) {
    assert(substitutions.size() == 1 &&
           "bridge should have a single substitution");
    assert(args.size() == 1 && "bridge should have a single argument");
    
    // The substitution determines the destination type.
    // FIXME: Archetype destination type?
    SILType destType = gen.getLoweredLoadableType(substitutions[0].Replacement);

    // Take the raw pointer argument and cast it to the destination type.
    SILValue result = gen.B.createRawPointerToRef(loc, args[0].getUnmanagedValue(),
                                                  destType);
    // The result has ownership semantics, so retain it with a cleanup.
    gen.emitRetainRValue(loc, result);
    return gen.emitManagedRValueWithCleanup(result);
  }

  /// Specialized emitter for Builtin.addressof.
  static ManagedValue emitBuiltinAddressOf(SILGenFunction &gen,
                                           SILLocation loc,
                                           ArrayRef<Substitution> substitutions,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext C) {
    assert(args.size() == 1 && "addressof should have a single argument");
    
    // Take the address argument and cast it to RawPointer.
    SILType rawPointerType = SILType::getRawPointerType(gen.F.getContext());
    SILValue result = gen.B.createAddressToPointer(loc, args[0].getUnmanagedValue(),
                                                   rawPointerType);
    return ManagedValue(result, ManagedValue::Unmanaged);
  }

  Callee::SpecializedEmitter
  Callee::getSpecializedEmitterForSILBuiltin(SILConstant function) {
    // Filter out non-function members and non-builtin modules.

    if (function.kind != SILConstant::Kind::Func)
      return nullptr;
    if (!function.hasDecl())
      return nullptr;
    
    ValueDecl *decl = function.getDecl();
    
    if (!isa<BuiltinModule>(decl->getDeclContext()))
      return nullptr;
    
    SmallVector<Type, 2> types;
    StringRef name =
      getBuiltinBaseName(decl->getASTContext(), decl->getName().str(), types);

    // Match SIL builtins to their emitters.
    #define BUILTIN(Id, Name)
    #define BUILTIN_SIL_OPERATION(Id, Name, Overload) \
      if (name.equals(Name)) \
        return &emitBuiltin##Id;

    #include "swift/AST/Builtins.def"
    
    return nullptr;
  }
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
  if (apply.thisParam)
    emission.addCallSite(SILLocation(), std::move(apply.thisParam));

  // Apply arguments from call sites, innermost to outermost.
  for (auto site = apply.callSites.rbegin(), end = apply.callSites.rend();
       site != end;
       ++site) {
    emission.addCallSite(*site);
  }
  
  return emission;
}

RValue SILGenFunction::emitApplyExpr(ApplyExpr *e, SGFContext c) {
  return RValue(*this, prepareApplyExpr(*this, e).apply(c));
}

/// emitArrayInjectionCall - Form an array "Slice" out of an ObjectPointer
/// (which represents the retain count), a base pointer to some elements, and a
/// length.
ManagedValue SILGenFunction::emitArrayInjectionCall(ManagedValue ObjectPtr,
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
  InjectionArgs.addElement(RValue(*this, ObjectPtr));
  InjectionArgs.addElement(RValue(*this,
                                ManagedValue(Length, ManagedValue::Unmanaged)));
  
  emission.addCallSite(SILLocation(), std::move(InjectionArgs));
  return emission.apply();
}

ManagedValue SILGenFunction::emitSpecializedPropertyConstantRef(
                                          SILLocation loc,
                                          SILConstant constant,
                                          ArrayRef<Substitution> substitutions,
                                          Type substPropertyType)
{
  // If the accessor is a local constant, use it.
  // FIXME: Can local properties ever be generic?
  if (LocalConstants.count(constant)) {
    SILValue v = LocalConstants[constant];
    emitRetainRValue(loc, v);
    return emitManagedRValueWithCleanup(v);
  }
  
  // Get the accessor function. The type will be a polymorphic function if
  // the This type is generic.
  SILValue method = emitGlobalConstantRef(loc, constant);
  
  // If there are substitutions, specialize the generic getter.
  // FIXME: Generic subscript operator could add another layer of
  // substitutions.
  if (!substitutions.empty()) {
    assert(method.getType().is<PolymorphicFunctionType>() &&
           "generic getter is not of a poly function type");
    substPropertyType = getThinFunctionType(substPropertyType);
    SILType loweredPropertyType = getLoweredLoadableType(substPropertyType,
                                                         constant.uncurryLevel);
    
    method = B.createSpecialize(loc, method, substitutions,
                                loweredPropertyType);
  }
  assert(method.getType().is<FunctionType>() &&
         "getter is not of a concrete function type");
  return ManagedValue(method, ManagedValue::Unmanaged);
}

/// Emit a call to a getter and materialize its result.
Materialize SILGenFunction::emitGetProperty(SILLocation loc,
                                            SILConstant get,
                                            ArrayRef<Substitution> substitutions,
                                            RValue &&thisValue,
                                            RValue &&subscripts,
                                            Type resultType) {
  // Derive the specialized type of the accessor.
  auto &tc = SGM.Types;
  Type propType;
  if (subscripts)
    propType = tc.getSubscriptPropertyType(SILConstant::Kind::Getter,
                                           subscripts.getType(),
                                           resultType);
  else
    propType = tc.getPropertyType(SILConstant::Kind::Getter, resultType);
  if (thisValue)
    propType = tc.getMethodTypeInContext(thisValue.getType()->getRValueType(),
                                         propType);
  
  ManagedValue getter = emitSpecializedPropertyConstantRef(loc, get,
                                                           substitutions,
                                                           propType);
  
  CallEmission emission(*this, Callee(getter,
                                      OwnershipConventions::get(*this, get)));
  // This ->
  if (thisValue)
    emission.addCallSite(loc, std::move(thisValue));
  // Index ->
  if (subscripts)
    emission.addCallSite(loc, std::move(subscripts));
  // () ->
  emission.addCallSite(loc, emitEmptyTupleRValue(loc));
  // T
  ManagedValue result = emission.apply();
  return emitMaterialize(loc, result);
}

void SILGenFunction::emitSetProperty(SILLocation loc,
                                     SILConstant set,
                                     ArrayRef<Substitution> substitutions,
                                     RValue &&thisValue,
                                     RValue &&subscripts,
                                     RValue &&setValue) {
  // Derive the specialized type of the accessor.
  auto &tc = SGM.Types;
  Type propType;
  if (subscripts)
    propType = tc.getSubscriptPropertyType(SILConstant::Kind::Setter,
                                           subscripts.getType(),
                                           setValue.getType());
  else
    propType = tc.getPropertyType(SILConstant::Kind::Setter,
                                  setValue.getType());
  if (thisValue)
    propType = tc.getMethodTypeInContext(thisValue.getType()->getRValueType(),
                                         propType);

  ManagedValue setter = emitSpecializedPropertyConstantRef(loc, set,
                                                           substitutions,
                                                           propType);

  CallEmission emission(*this, Callee(setter,
                                      OwnershipConventions::get(*this, set)));
  // This ->
  if (thisValue)
    emission.addCallSite(loc, std::move(thisValue));
  // Index ->
  if (subscripts)
    emission.addCallSite(loc, std::move(subscripts));
  // T ->
  emission.addCallSite(loc, std::move(setValue));
  // ()
  emission.apply();
}