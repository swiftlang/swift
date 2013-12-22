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

#include "LValue.h"
#include "RValue.h"
#include "Scope.h"
#include "Initialization.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Module.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/PrettyStackTrace.h"

using namespace swift;
using namespace Lowering;

/// Retrieve the type to use for a method found via dynamic lookup.
static CanAnyFunctionType getDynamicMethodType(SILGenModule &SGM,
                                               SILValue proto,
                                               SILDeclRef methodName,
                                               Type memberType) {
  auto &ctx = SGM.getASTContext();
  CanType selfTy;
  if (methodName.getDecl()->isInstanceMember()) {
    selfTy = ctx.TheObjCPointerType;
  } else {
    selfTy = proto.getType().getSwiftType();
  }
  auto extInfo = FunctionType::ExtInfo()
                   .withCallingConv(SGM.getConstantCC(methodName))
                   .withIsThin(true);

  return CanFunctionType::get(selfTy, memberType->getCanonicalType(),
                              extInfo);
}

namespace {

/// Abstractly represents a callee, and knows how to emit the entry point
/// reference for a callee at any valid uncurry level.
class Callee {
public:
  enum class Kind {
    /// An indirect function value.
    IndirectValue,
    /// A direct standalone function call, referenceable by a FunctionRefInst.
    StandaloneFunction,

    VirtualMethod_First,
      /// A method call using class method dispatch.
      ClassMethod = VirtualMethod_First,
      /// A method call using super method dispatch.
      SuperMethod,
    VirtualMethod_Last = SuperMethod,
  
    GenericMethod_First,
      /// A method call using archetype dispatch.
      ArchetypeMethod = GenericMethod_First,
      /// A method call using protocol dispatch.
      ProtocolMethod,
      /// A method call using dynamic lookup.
      DynamicMethod,
    GenericMethod_Last = DynamicMethod
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
    ManagedValue indirectValue;
    SILDeclRef standaloneFunction;
    struct {
      SILValue selfValue;
      SILDeclRef methodName;
    } method;
  };
  ArrayRef<Substitution> substitutions;
  CanType OrigFormalType;
  CanAnyFunctionType SubstFormalType;
  Optional<SILLocation> specializeLoc;
  bool isTransparent;
  bool HasSubstitutions = false;

  // The pointer back to the AST node that produced the callee.
  SILLocation Loc;

  static SpecializedEmitter getSpecializedEmitterForSILBuiltin(SILDeclRef c,
                                                               SILModule &M);

  Callee(ManagedValue indirectValue, CanType origFormalType,
         CanAnyFunctionType substFormalType, bool isTransparent, SILLocation L)
    : kind(Kind::IndirectValue),
      indirectValue(indirectValue),
      OrigFormalType(origFormalType),
      SubstFormalType(substFormalType),
      isTransparent(isTransparent),
      Loc(L)
  {}

  static CanAnyFunctionType getConstantFormalType(SILGenFunction &gen,
                                                  SILDeclRef fn) {
    return gen.SGM.Types.getConstantFormalType(fn.atUncurryLevel(0));
  }

  Callee(SILGenFunction &gen, SILDeclRef standaloneFunction,
         CanAnyFunctionType substFormalType, SILLocation l)
    : kind(Kind::StandaloneFunction), standaloneFunction(standaloneFunction),
      OrigFormalType(getConstantFormalType(gen, standaloneFunction)),
      SubstFormalType(substFormalType),
      isTransparent(standaloneFunction.isTransparent()),
      Loc(l)
  {
  }

  Callee(Kind methodKind,
         SILGenFunction &gen,
         SILValue selfValue,
         SILDeclRef methodName,
         CanAnyFunctionType substFormalType,
         SILLocation l)
    : kind(methodKind), method{selfValue, methodName},
      OrigFormalType(getConstantFormalType(gen, methodName)),
      SubstFormalType(substFormalType),
      isTransparent(false),
      Loc(l)
  {
  }

  static CanArchetypeType getArchetypeForSelf(CanType selfType) {
    if (auto mt = dyn_cast<MetatypeType>(selfType)) {
      return cast<ArchetypeType>(mt.getInstanceType());
    } else {
      return cast<ArchetypeType>(selfType);
    }
  }

  /// Build a clause that looks like 'origParamType' but uses 'selfType'
  /// in place of the underlying archetype.
  static CanType buildSubstSelfType(CanType origParamType, CanType selfType,
                                    ASTContext &ctx) {
    if (auto lv = dyn_cast<LValueType>(origParamType)) {
      selfType = buildSubstSelfType(lv.getObjectType(), selfType, ctx);
      return CanLValueType::get(selfType, lv->getQualifiers(), ctx);
    }

    if (auto tuple = dyn_cast<TupleType>(origParamType)) {
      assert(tuple->getNumElements() == 1);
      selfType = buildSubstSelfType(tuple.getElementType(0), selfType, ctx);

      auto field = tuple->getFields()[0].getWithType(selfType);
      return CanType(TupleType::get(field, ctx));
    }

    // These first two asserts will crash before they return if
    // they're actually wrong.
    assert(getArchetypeForSelf(origParamType));
    assert(getArchetypeForSelf(selfType));
    assert(isa<MetatypeType>(origParamType) == isa<MetatypeType>(selfType));
    return selfType;
  }

  CanSILFunctionType getSubstFunctionType(SILGenModule &SGM,
                                          CanSILFunctionType origFnType,
                                          CanAnyFunctionType origLoweredType,
                                          unsigned uncurryLevel) const {
    if (!HasSubstitutions) return origFnType;

    assert(origLoweredType);
    auto substLoweredType =
      SGM.Types.getLoweredASTFunctionType(SubstFormalType, uncurryLevel,
                                          origLoweredType->getExtInfo());
    return SGM.Types.substFunctionType(origFnType, origLoweredType,
                                       substLoweredType);
  }

  /// Return the value being passed as 'self' for this protocol callee.
  CanType getProtocolSelfType(SILGenModule &SGM) const {
    if (kind == Kind::ProtocolMethod) {
      auto member = method.methodName.getDecl();
      auto proto = cast<ProtocolDecl>(member->getDeclContext());
      auto type = proto->getSelf()->getArchetype()->getCanonicalType();
      if (!member->isInstanceMember())
        type = CanType(MetatypeType::get(type, SGM.getASTContext()));
      return type;
    } else if (kind == Kind::ArchetypeMethod) {
      return method.selfValue.getType().getSwiftRValueType();
    } else {
      llvm_unreachable("bad callee kind for protocol method");
    }
  }

  /// Add the 'self' clause back to the substituted formal type of
  /// this protocol method.
  void addProtocolSelfToFormalType(SILGenModule &SGM) {
    // The result types of the expressions yielding protocol values
    // (reflected in SubstFormalType) reflect an implicit level of
    // function application, including some extra polymorphic
    // substitution.
    HasSubstitutions = true;

    auto &ctx = SGM.getASTContext();

    // Add the 'self' parameter back.  We want it to look like a
    // substitution of the appropriate clause from the original type.
    auto polyFormalType = cast<PolymorphicFunctionType>(OrigFormalType);
    auto selfType = getProtocolSelfType(SGM);
    auto substSelfType =
      buildSubstSelfType(polyFormalType.getInput(), selfType, ctx);

    bool isThin;
    if (SGM.getASTContext().LangOpts.EmitSILProtocolWitnessTables) {
      // Existential witnesses are always "thick" with the polymorphic info
      isThin = false;
    } else {
      // This function is thin only if it's a class archetype, because
      // otherwise it implicitly binds up the metatype value.
      isThin = getArchetypeForSelf(selfType)->requiresClass();
    }
    auto extInfo = FunctionType::ExtInfo(AbstractCC::Method, isThin,
                                         /*noreturn*/ false);

    SubstFormalType = CanFunctionType::get(substSelfType, SubstFormalType,
                                           extInfo);
  }

  SILType getProtocolClosureType(SILGenModule &SGM,
                                 SILDeclRef constant,
                                 SILConstantInfo &constantInfo,
                                 CanArchetypeType &archetype) const {
    CanType selfType = getProtocolSelfType(SGM);

    // Find the archetype.
    archetype = getArchetypeForSelf(selfType);

    // addProtocolSelfToFormalType initializes SubstFormalType->isThin()
    // appropriately for the archetype kind.
    bool isThin = /*thin*/ SubstFormalType->isThin();

    // We may need to make the OrigLoweredType thick so that
    // computing the substFnType below preserves this information.
    if (!isThin) {
      constantInfo.LoweredType =
        getThickFunctionType(constantInfo.LoweredType);
    }

    // The expected result of archetype_method is a partial application
    // of the archetype method to the expected archetype.
    CanAnyFunctionType partialSubstFormalType =
      cast<FunctionType>(
        cast<PolymorphicFunctionType>(OrigFormalType)
          ->substGenericArgs(SGM.SwiftModule, archetype)
            ->getCanonicalType());

    auto partialSubstUncurriedType =
      SGM.Types.getConstantFunctionType(constant, partialSubstFormalType,
                                        isThin);
    return SILType::getPrimitiveObjectType(partialSubstUncurriedType);
  }

  /// Add the 'self' type to the substituted function type of this
  /// dynamic callee.
  void addDynamicSelfToFormalType(SILGenModule &SGM) {
    assert(kind == Kind::DynamicMethod);

    // Drop the original self clause.
    CanType methodType = OrigFormalType;
    methodType = cast<AnyFunctionType>(methodType).getResult();

    // Replace it with the dynamic self type.
    OrigFormalType = getDynamicMethodType(SGM, method.selfValue,
                                          method.methodName, methodType);

    // Add a self clause to the substituted type.
    auto origFormalType = cast<AnyFunctionType>(OrigFormalType);
    auto selfType = origFormalType.getInput();
    SubstFormalType = CanFunctionType::get(selfType, SubstFormalType,
                                           origFormalType->getExtInfo());
  }

public:
  static Callee forIndirect(ManagedValue indirectValue,
                            CanType origFormalType,
                            CanAnyFunctionType substFormalType,
                            bool isTransparent,
                            SILLocation l) {
    return Callee(indirectValue, origFormalType, substFormalType, isTransparent, l);
  }
  static Callee forDirect(SILGenFunction &gen, SILDeclRef c,
                          CanAnyFunctionType substFormalType, SILLocation l) {
    return Callee(gen, c, substFormalType, l);
  }
  static Callee forClassMethod(SILGenFunction &gen, SILValue selfValue,
                               SILDeclRef name, CanAnyFunctionType substFormalType,
                               SILLocation l) {
    return Callee(Kind::ClassMethod, gen, selfValue, name, substFormalType, l);
  }
  static Callee forSuperMethod(SILGenFunction &gen, SILValue selfValue,
                               SILDeclRef name, CanAnyFunctionType substFormalType,
                               SILLocation l) {
    return Callee(Kind::SuperMethod, gen, selfValue, name, substFormalType, l);
  }
  static Callee forArchetype(SILGenFunction &gen, SILValue value,
                             SILDeclRef name, CanAnyFunctionType substFormalType,
                             SILLocation l) {
    Callee callee(Kind::ArchetypeMethod, gen, value, name, substFormalType, l);
    callee.addProtocolSelfToFormalType(gen.SGM);
    return callee;
  }
  static Callee forProtocol(SILGenFunction &gen, SILValue proto,
                            SILDeclRef name, CanAnyFunctionType substFormalType,
                            SILLocation l) {
    Callee callee(Kind::ProtocolMethod, gen, proto, name, substFormalType, l);
    callee.addProtocolSelfToFormalType(gen.SGM);
    return callee;
  }
  static Callee forDynamic(SILGenFunction &gen, SILValue proto,
                           SILDeclRef name, CanAnyFunctionType substFormalType,
                           SILLocation l) {
    Callee callee(Kind::DynamicMethod, gen, proto, name, substFormalType, l);
    callee.addDynamicSelfToFormalType(gen.SGM);
    return callee;
  }
  Callee(Callee &&) = default;
  Callee &operator=(Callee &&) = default;

  void setSubstitutions(SILGenFunction &gen,
                        SILLocation loc,
                        ArrayRef<Substitution> newSubs,
                        unsigned callDepth) {
    // Currently generic methods of generic types are the deepest we should
    // be able to stack specializations.
    // FIXME: Generic local functions can add type parameters to arbitrary
    // depth.
    assert(callDepth < 2 && "specialization below 'self' or argument depth?!");
    assert(substitutions.empty() && "Already have substitutions?");
    substitutions = newSubs;

    assert(getNaturalUncurryLevel() >= callDepth
           && "specializations below uncurry level?!");
    specializeLoc = loc;
    HasSubstitutions = true;
  }

  CanType getOrigFormalType() const {
    return OrigFormalType;
  };

  CanAnyFunctionType getSubstFormalType() const {
    return SubstFormalType;
  };

  unsigned getNaturalUncurryLevel() const {
    switch (kind) {
    case Kind::IndirectValue:
      return 0;
        
    case Kind::StandaloneFunction:
      return standaloneFunction.uncurryLevel;

    case Kind::ClassMethod:
    case Kind::SuperMethod:
    case Kind::ArchetypeMethod:
    case Kind::ProtocolMethod:
    case Kind::DynamicMethod:
      return method.methodName.uncurryLevel;
    }
  }
  
  std::tuple<ManagedValue, CanSILFunctionType, bool>
  getAtUncurryLevel(SILGenFunction &gen, unsigned level) const {
    ManagedValue mv;
    bool transparent = isTransparent;
    SILConstantInfo constantInfo = {};

    switch (kind) {
    case Kind::IndirectValue:
      assert(level == 0 && "can't curry indirect function");
      mv = indirectValue;
      assert(!HasSubstitutions);
      break;

    case Kind::StandaloneFunction: {
      assert(level <= standaloneFunction.uncurryLevel
             && "uncurrying past natural uncurry level of standalone function");
      if (level < standaloneFunction.uncurryLevel)
        transparent = false;
      auto constant = standaloneFunction.atUncurryLevel(level);
      constantInfo = gen.getConstantInfo(constant);
      SILValue ref = gen.emitGlobalFunctionRef(Loc, constant, constantInfo);
      mv = ManagedValue(ref, ManagedValue::Unmanaged);
      break;
    }
    case Kind::ClassMethod: {
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");
      auto constant = method.methodName.atUncurryLevel(level);
      constantInfo = gen.getConstantInfo(constant);
      
      // If the call is curried, emit a direct call to the curry thunk.
      if (level < method.methodName.uncurryLevel) {
        SILValue ref = gen.emitGlobalFunctionRef(Loc, constant, constantInfo);
        mv = ManagedValue(ref, ManagedValue::Unmanaged);
        transparent = false;
        break;
      }
      
      // Otherwise, do the dynamic dispatch inline.
      SILValue methodVal = gen.B.createClassMethod(Loc,
                                                   method.selfValue,
                                                   constant,
                                                   constantInfo.getSILType(),
                                                   /*volatile*/
                                                     constant.isForeign);
      
      mv = ManagedValue(methodVal, ManagedValue::Unmanaged);
      break;
    }
    case Kind::SuperMethod: {
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");
      assert(level >= 1
             && "currying 'self' of super method dispatch not yet supported");

      auto constant = method.methodName.atUncurryLevel(level);
      constantInfo = gen.getConstantInfo(constant);
      SILValue methodVal = gen.B.createSuperMethod(Loc,
                                                   method.selfValue,
                                                   constant,
                                                   constantInfo.getSILType(),
                                                   /*volatile*/
                                                     constant.isForeign);
      
      mv = ManagedValue(methodVal, ManagedValue::Unmanaged);
      break;
    }
    case Kind::ArchetypeMethod: {
      assert(level >= 1
             && "currying 'self' of generic method dispatch not yet supported");
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");

      auto constant = method.methodName.atUncurryLevel(level);
      constantInfo = gen.getConstantInfo(constant);

      if (gen.getASTContext().LangOpts.EmitSILProtocolWitnessTables) {
        // Look up the witness for the archetype.
        auto selfType = getProtocolSelfType(gen.SGM);
        auto archetype = getArchetypeForSelf(selfType);
        SILValue fn = gen.B.createArchetypeMethod(Loc,
                                    SILType::getPrimitiveObjectType(archetype),
                                    /*conformance*/ nullptr,
                                    constant,
                                    constantInfo.getSILType(),
                                    constant.isForeign);
        mv = ManagedValue(fn, ManagedValue::Unmanaged);
      } else {
        CanArchetypeType archetype;
        SILType closureType =
          getProtocolClosureType(gen.SGM, constant, constantInfo, archetype);

        SILValue fn = gen.B.createArchetypeMethod(Loc,
                                      SILType::getPrimitiveObjectType(archetype),
                                              /*conformance*/ nullptr,
                                              constant,
                                              closureType,
                                              /*volatile*/ constant.isForeign);
        mv = ManagedValue(fn, ManagedValue::Unmanaged);
      }
      break;
    }
    case Kind::ProtocolMethod: {
      assert(level >= 1
             && "currying 'self' of generic method dispatch not yet supported");
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");
      
      auto constant = method.methodName.atUncurryLevel(level);
      constantInfo = gen.getConstantInfo(constant);

      CanArchetypeType archetype; // not used
      SILType closureType =
        getProtocolClosureType(gen.SGM, constant, constantInfo, archetype);

      SILValue fn = gen.B.createProtocolMethod(Loc,
                                               method.selfValue,
                                               constant,
                                               closureType,
                                               /*volatile*/ constant.isForeign);
      mv = ManagedValue(fn, ManagedValue::Unmanaged);
      break;
    }
    case Kind::DynamicMethod: {
      assert(level >= 1
             && "currying 'self' of dynamic method dispatch not yet supported");
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");

      auto constant = method.methodName.atUncurryLevel(level);
      constantInfo = gen.getConstantInfo(constant);

      auto closureType =
        gen.SGM.Types.getConstantFunctionType(method.methodName,
                                         cast<AnyFunctionType>(OrigFormalType),
                                              /*thin*/ true);

      SILValue fn = gen.B.createDynamicMethod(Loc,
                          method.selfValue,
                          constant,
                          SILType::getPrimitiveObjectType(closureType),
                          /*volatile*/ constant.isForeign);
      mv = ManagedValue(fn, ManagedValue::Unmanaged);
      break;
    }
    }

    CanSILFunctionType substFnType =
      getSubstFunctionType(gen.SGM, mv.getType().castTo<SILFunctionType>(),
                           constantInfo.LoweredType, level);
    
    return std::make_tuple(mv, substFnType, transparent);
  }
  
  ArrayRef<Substitution> getSubstitutions() const {
    return substitutions;
  }

  /// Return a specialized emission function if this is a function with a known
  /// lowering, such as a builtin, or return null if there is no specialized
  /// emitter.
  Optional<SpecializedEmitter>
  getSpecializedEmitter(SILGenModule &SGM, unsigned uncurryLevel) const {
    // Currently we have no curried known functions.
    if (uncurryLevel != 0)
      return Nothing;
    
    switch (kind) {
    case Kind::StandaloneFunction: {
      if (SpecializedEmitter e
            = getSpecializedEmitterForSILBuiltin(standaloneFunction, SGM.M)) {
        return e;
      }
      SWIFT_FALLTHROUGH;
    }
    case Kind::IndirectValue:
    case Kind::ClassMethod:
    case Kind::SuperMethod:
    case Kind::ArchetypeMethod:
    case Kind::ProtocolMethod:
    case Kind::DynamicMethod:
      return Nothing;
    }
    llvm_unreachable("bad callee kind");
  }
};

/// Get the 'Self' type of a DynamicLookup operand to use as the result type of
/// projecting the object instance handle.
SILType getSelfTypeForDynamicLookup(SILGenFunction &gen,
                                    SILValue existential) {
  CanType ty = existential.getType().getSwiftRValueType();
  ProtocolDecl *proto = cast<ProtocolType>(ty)->getDecl();
  // DynamicLookup is a class protocol so its projection should be loadable.
  return gen.getLoweredLoadableType(proto->getSelf()->getArchetype());
}
  
/// An ASTVisitor for building SIL function calls.
/// Nested ApplyExprs applied to an underlying curried function or method
/// reference are flattened into a single SIL apply to the most uncurried entry
/// point fitting the call site, avoiding pointless intermediate closure
/// construction.
class SILGenApply : public Lowering::ExprVisitor<SILGenApply>
{
public:
  SILGenFunction &gen;
  Optional<Callee> callee;
  RValue selfParam;
  Expr *SelfApplyExpr = nullptr;
  std::vector<ApplyExpr*> callSites;
  Expr *sideEffect = nullptr;
  unsigned callDepth = 0;
  
  SILGenApply(SILGenFunction &gen)
    : gen(gen)
  {}
  
  void setCallee(Callee &&c) {
    assert((selfParam ? callDepth == 1 : callDepth == 0)
           && "setting callee at non-zero call depth?!");
    assert(!callee && "already set callee!");
    callee.emplace(std::move(c));
  }
  
  void setSideEffect(Expr *sideEffectExpr) {
    assert(!sideEffect && "already set side effect!");
    sideEffect = sideEffectExpr;
  }
  
  void setSelfParam(RValue &&theSelfParam, Expr *theSelfApplyExpr) {
    assert(!selfParam && "already set this!");
    selfParam = std::move(theSelfParam);
    SelfApplyExpr = theSelfApplyExpr;
    ++callDepth;
  }

  void decompose(Expr *e) {
    visit(e);
  }

  CanFunctionType getSubstFnType() {
    // TODO: optimize this if there are no specializes in play
    auto getSiteType = [](ApplyExpr *site) {
      return cast<FunctionType>(site->getFn()->getType()->getCanonicalType());
    };

    CanFunctionType fnType;

    auto addSite = [&](ApplyExpr *site) {
      auto siteType = getSiteType(site);

      // If this is the first call site, use its formal type directly.
      if (!fnType) {
        fnType = siteType;
        return;
      }

      fnType = CanFunctionType::get(siteType.getInput(), fnType,
                                    siteType->getExtInfo());
    };

    for (auto callSite : callSites) {
      addSite(callSite);
    }
    if (auto selfApply = dyn_cast_or_null<ApplyExpr>(SelfApplyExpr)) {
      addSite(selfApply);
    }

    assert(fnType && "found no call sites?");
    return fnType;
  }

  /// Fall back to an unknown, indirect callee.
  void visitExpr(Expr *e) {
    ManagedValue fn = gen.emitRValue(e).getAsSingleValue(gen, e);
    auto origType = cast<AnyFunctionType>(e->getType()->getCanonicalType());
    setCallee(Callee::forIndirect(fn, origType, getSubstFnType(), false, e));
  }

  void visitLoadExpr(LoadExpr *e) {
    bool isTransparent = false;
    if (DeclRefExpr *d = dyn_cast<DeclRefExpr>(e->getSubExpr())) {
      // This marks all calls to auto closure typed variables as transparent.
      // As of writing, local variable auto closures are currently allowed by
      // the parser, but the plan is that they will be disallowed, so this will
      // actually only apply to auto closure parameters, which is what we want
      auto *t = d->getDecl()->getType()->castTo<AnyFunctionType>();
      isTransparent = t->getExtInfo().isAutoClosure();
    }
    // TODO: preserve the function pointer at its original abstraction level
    ManagedValue fn = gen.emitRValue(e).getAsSingleValue(gen, e);
    auto origType = cast<AnyFunctionType>(e->getType()->getCanonicalType());
    setCallee(Callee::forIndirect(fn, origType, getSubstFnType(),
                                  isTransparent, e));
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
  
  //
  // Known callees.
  //
  void visitDeclRefExpr(DeclRefExpr *e) {
    // If this is a non-extension class method, emit class_method to
    // dynamically dispatch the call.
    // FIXME: Or if it's an ObjC method. Extension methods on classes will
    // hopefully become dynamically dispatched too.
    if (auto *fd = dyn_cast<FuncDecl>(e->getDecl())) {
      if (isa<ClassDecl>(fd->getDeclContext()) || fd->isObjC()) {
        ApplyExpr *thisCallSite = callSites.back();
        callSites.pop_back();
        setSelfParam(gen.emitRValue(thisCallSite->getArg()), thisCallSite);
        SILDeclRef constant(fd,
                            SILDeclRef::ConstructAtNaturalUncurryLevel,
                            gen.SGM.requiresObjCDispatch(fd));
        
        setCallee(Callee::forClassMethod(gen, selfParam.peekScalarValue(),
                                         constant, getSubstFnType(), e));
        
        // setSelfParam bumps the callDepth, but we aren't really past the
        // 'self' call depth in this case.
        --callDepth;

        // If there are substitutions, add them.
        if (e->getDeclRef().isSpecialized()) {
          callee->setSubstitutions(gen, e, e->getDeclRef().getSubstitutions(),
                                   callDepth);
        }

        return;
      }
    }
    
    // FIXME: Store context values for local funcs in a way that we can
    // apply them directly as an added "call site" here.
    SILDeclRef constant(e->getDecl(),
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         gen.SGM.requiresObjCDispatch(e->getDecl()));

    // Obtain a reference for a local closure.
    if (gen.LocalFunctions.count(constant)) {
      ManagedValue localFn = gen.emitReferenceToDecl(e, e->getDeclRef());
      auto type = cast<AnyFunctionType>(e->getType()->getCanonicalType());
      setCallee(Callee::forIndirect(localFn, type, type, false, e));

    // Otherwise, stash the SILDeclRef.
    } else {
      setCallee(Callee::forDirect(gen, constant, getSubstFnType(), e));
    }

    // If there are substitutions, add them.
    if (e->getDeclRef().isSpecialized()) {
      callee->setSubstitutions(gen, e, e->getDeclRef().getSubstitutions(),
                               callDepth);
    }
  }
  void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *e) {
    // FIXME: We might need to go through ObjC dispatch for references to
    // constructors imported from Clang (which won't have a direct entry point)
    // or to delegate to a designated initializer.
    setCallee(Callee::forDirect(gen,
                SILDeclRef(e->getDecl(), SILDeclRef::Kind::Initializer),
                                getSubstFnType(), e));

    // If there are substitutions, add them.
    if (e->getDeclRef().isSpecialized())
      callee->setSubstitutions(gen, e, e->getDeclRef().getSubstitutions(),
                               callDepth);
  }
  void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e) {
    setSideEffect(e->getLHS());
    visit(e->getRHS());
  }

  /// Skip over all of the 'Self'-related substitutions within the given set
  /// of substitutions.
  ArrayRef<Substitution> getNonSelfSubstitutions(ArrayRef<Substitution> subs) {
    unsigned innerIdx = 0, n = subs.size();
    for (; innerIdx != n; ++innerIdx) {
      auto archetype = subs[innerIdx].Archetype;
      while (archetype->getParent())
        archetype = archetype->getParent();
      if (!archetype->getSelfProtocol()) {
        break;
      }
    }

    return subs.slice(innerIdx);
  }

  void visitExistentialMemberRefExpr(ExistentialMemberRefExpr *e) {
    ManagedValue existential =
      gen.emitLValueOrRValueAsRValue(e->getBase())
         .getAsSingleValue(gen, e->getBase());
    
    auto *fd = dyn_cast<FuncDecl>(e->getDecl());
    assert(fd && "existential properties not yet supported");

    auto *proto = cast<ProtocolDecl>(fd->getDeclContext());

    if (e->getDecl()->isInstanceMember()) {
      // Attach the existential cleanup to the projection so that it gets consumed
      // (or not) when the call is applied to it (or isn't).
      ManagedValue proj;
      SILType protoSelfTy
        = gen.getLoweredType(proto->getSelf()->getArchetype());
      if (existential.getType().isClassExistentialType()) {
        SILValue val = gen.B.createProjectExistentialRef(e,
                                           existential.getValue(), protoSelfTy);
        proj = ManagedValue(val, existential.getCleanup());
      } else {
        assert(protoSelfTy.isAddress() && "Self should be address-only");
        SILValue val = gen.B.createProjectExistential(e, existential.getValue(),
                                                      protoSelfTy);
        proj = ManagedValue(val, ManagedValue::Unmanaged);
      }

      setSelfParam(RValue(gen, e, protoSelfTy.getSwiftType(), proj), e);
    } else {
      assert(existential.getType().is<MetatypeType>() &&
             "non-existential-metatype for existential static method?!");
      
      setSelfParam(RValue(gen, e, existential.getType().getSwiftRValueType(),
                          existential), e);
    }

    // The declaration is always specialized (due to Self); ignore the
    // substitutions related to Self. Skip the substitutions involving Self.
    ArrayRef<Substitution> subs = getNonSelfSubstitutions(
                                    e->getDeclRef().getSubstitutions());

    // Method calls through ObjC protocols require ObjC dispatch.
    bool isObjC = proto->isObjC();

    setCallee(Callee::forProtocol(gen, existential.getValue(),
                                  SILDeclRef(fd).asForeign(isObjC),
                                  getSubstFnType(), e));

    // If there are substitutions, add them now.
    if (!subs.empty()) {
      callee->setSubstitutions(gen, e, subs, callDepth);
    }
  }
  void visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *e) {
    setSelfParam(gen.emitLValueOrRValueAsRValue(e->getBase()), e);
    
    auto *fd = dyn_cast<FuncDecl>(e->getDecl());
    assert(fd && "archetype properties not yet supported");

    // Method calls through ObjC protocols require ObjC dispatch.
    auto proto = cast<ProtocolDecl>(fd->getDeclContext());
    bool isObjC = proto->isObjC();

    ArrayRef<Substitution> subs;
    if (gen.getASTContext().LangOpts.EmitSILProtocolWitnessTables) {
      subs = e->getDeclRef().getSubstitutions();
    } else {
      // The declaration is always specialized (due to Self); ignore the
      // substitutions related to Self. Skip the substitutions involving Self.
      subs = getNonSelfSubstitutions(e->getDeclRef().getSubstitutions());
    }

    // Figure out the result type of this expression. If we had any
    // substitutions not related to 'Self', we'll need to produce a
    // PolymorphicFunctionType to mollify callee handling.
    // FIXME: This is a temporary hack that will go away when callee handling
    // no longer depends on PolymorphicFunctionType at all.
    Type resultTy = e->getType();
    if (!subs.empty()) {
      TypeSubstitutionMap substitutions;
      substitutions[proto->getSelf()->getArchetype()]
        = e->getDeclRef().getSubstitutions()[0].Replacement;

      resultTy = e->getDecl()->getType()
                   ->castTo<PolymorphicFunctionType>()->getResult()
                   .subst(gen.SGM.SwiftModule, substitutions, false, nullptr);
    }
    setCallee(Callee::forArchetype(gen, selfParam.peekScalarValue(),
                                   SILDeclRef(fd).asForeign(isObjC),
                                   getSubstFnType(), e));

    // If there are substitutions, add them now.
    if (!subs.empty()) {
      callee->setSubstitutions(gen, e, subs, callDepth);
    }
  }

  void visitFunctionConversionExpr(FunctionConversionExpr *e) {
    visit(e->getSubExpr());
  }
  
  void visitParenExpr(ParenExpr *e) {
    visit(e->getSubExpr());
  }
  
  void applySuper(ApplyExpr *apply) {
    // Load the 'super' argument.
    Expr *arg = apply->getArg();
    ManagedValue super = gen.emitRValue(arg).getAsSingleValue(gen, arg);
    if (super.isLValue()) {
      auto superValue = gen.B.createLoad(arg, super.getValue());
      super = gen.emitManagedRetain(arg, superValue);
    }

    // The callee for a super call has to be either a method or constructor.
    Expr *fn = apply->getFn();
    ArrayRef<Substitution> substitutions;
    SILDeclRef constant;
    if (auto *ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(fn)) {
      constant = SILDeclRef(ctorRef->getDecl(), SILDeclRef::Kind::Initializer,
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         gen.SGM.requiresObjCSuperDispatch(ctorRef->getDecl()));

      if (ctorRef->getDeclRef().isSpecialized())
        substitutions = ctorRef->getDeclRef().getSubstitutions();
    } else if (auto *declRef = dyn_cast<DeclRefExpr>(fn)) {
      assert(isa<FuncDecl>(declRef->getDecl()) && "non-function super call?!");
      constant = SILDeclRef(declRef->getDecl(),
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         gen.SGM.requiresObjCSuperDispatch(declRef->getDecl()));

      if (declRef->getDeclRef().isSpecialized())
        substitutions = declRef->getDeclRef().getSubstitutions();
    } else
      llvm_unreachable("invalid super callee");

    // Upcast 'self' parameter to the super type.
    CanType superFormalType = arg->getType()->getRValueType()->getCanonicalType();
    SILType superTy = gen.getLoweredLoadableType(superFormalType);
    SILValue superUpcast = gen.B.createUpcast(arg, super.getValue(), superTy);
    
    setSelfParam(RValue(gen, apply, superFormalType,
                        ManagedValue(superUpcast, super.getCleanup())),
                 apply);
    
    SILValue superMethod;
    if (constant.isForeign) {
      // ObjC super calls require dynamic dispatch.
      setCallee(Callee::forSuperMethod(gen, super.getValue(), constant,
                                       getSubstFnType(), fn));
    } else {
      // Native Swift super calls are direct.
      setCallee(Callee::forDirect(gen, constant, getSubstFnType(), fn));
    }

    // If there are any substitutions for the callee, apply them now.
    if (!substitutions.empty())
      callee->setSubstitutions(gen, fn, substitutions, callDepth-1);
  }
  
  Callee getCallee() {
    assert(callee && "did not find callee?!");
    return *std::move(callee);
  }

  /// Ignore parentheses and implicit conversions.
  static Expr *ignoreParensAndImpConversions(Expr *expr) {
    while (true) {
      if (auto ice = dyn_cast<ImplicitConversionExpr>(expr)) {
        expr = ice->getSubExpr();
        continue;
      }

      auto valueProviding = expr->getValueProvidingExpr();
      if (valueProviding != expr) {
        expr = valueProviding;
        continue;
      }

      return expr;
    }
  }
  
  void visitForceValueExpr(ForceValueExpr *e) {
    // If this application is a dynamic member reference that is forced to
    // succeed with the '!' operator, emit it as a direct invocation of the
    // method we found.
    if (emitForcedDynamicMemberRef(e))
      return;

    visitExpr(e);
  }

  /// If this application forces a dynamic member reference with !, emit
  /// a direct reference to the member.
  bool emitForcedDynamicMemberRef(ForceValueExpr *e) {
    // Check whether the argument is a dynamic member reference.
    auto arg = ignoreParensAndImpConversions(e->getSubExpr());
    auto dynamicMemberRef = dyn_cast<DynamicMemberRefExpr>(arg);
    if (!dynamicMemberRef)
      return false;

    // objc_msgSend() already contains the dynamic method lookup check, which
    // allows us to avoid emitting a specific test for the dynamic method.
    // Bail out early if we aren't using Objective-C dispatch.
    if (!gen.SGM.requiresObjCDispatch(dynamicMemberRef->getMember().getDecl()))
      return false;

    // Since we'll be collapsing this call site, make sure there's another
    // call site that will actually perform the invocation.
    if (callSites.empty())
      return false;

    // Only methods can be forced.
    auto *fd = dyn_cast<FuncDecl>(dynamicMemberRef->getMember().getDecl());
    if (!fd)
      return false;

    // We found it. Emit the base.
    ManagedValue existential =
      gen.emitRValue(dynamicMemberRef->getBase())
        .getAsSingleValue(gen, dynamicMemberRef->getBase());

    assert(fd->isObjC() && "Dynamic member references require [objc]");
    SILValue val;
    if (fd->isInstanceMember()) {
      assert(fd->isInstanceMember() && "Non-instance dynamic member reference");

      // Attach the existential cleanup to the projection so that it gets consumed
      // (or not) when the call is applied to it (or isn't).
      val = gen.B.createProjectExistentialRef(dynamicMemberRef,
                      existential.getValue(),
                      getSelfTypeForDynamicLookup(gen, existential.getValue()));
      val = gen.B.createRefToObjectPointer(dynamicMemberRef, val,
                             SILType::getObjCPointerType(gen.getASTContext()));
      ManagedValue proj(val, existential.getCleanup());
      setSelfParam(RValue(gen, dynamicMemberRef,
                          proj.getType().getSwiftRValueType(), proj),
                   dynamicMemberRef);
    } else {
      assert(existential.getType().is<MetatypeType>() &&
             "non-dynamic-lookup-metatype for static method?!");
      val = existential.getValue();
      ManagedValue proj(val, existential.getCleanup());
      setSelfParam(RValue(gen, dynamicMemberRef,
                          existential.getType().getSwiftRValueType(),
                          existential),
                   dynamicMemberRef);
    }

    // Determine the type of the method we referenced, by replacing the
    // class type of the 'Self' parameter with Builtin.ObjCPointer.
    SILDeclRef member(fd, SILDeclRef::ConstructAtNaturalUncurryLevel,
                      /*isObjC=*/true);

    setCallee(Callee::forDynamic(gen, val, member, getSubstFnType(), e));
    return true;
  }
};

} // end anonymous namespace

#ifndef NDEBUG
static bool areOnlyAbstractionDifferent(CanType type1, CanType type2) {
  assert(type1->isLegalSILType());
  assert(type2->isLegalSILType());

  // Exact equality is fine.
  if (type1 == type2) return true;

  // Either both types should be tuples or neither should be.
  if (auto tuple1 = dyn_cast<TupleType>(type1)) {
    auto tuple2 = dyn_cast<TupleType>(type2);
    if (!tuple2) return false;
    if (tuple1->getNumElements() != tuple2->getNumElements()) return false;
    for (auto i : indices(tuple2->getElementTypes()))
      if (!areOnlyAbstractionDifferent(tuple1.getElementType(i),
                                       tuple2.getElementType(i)))
        return false;
    return true;
  }
  if (isa<TupleType>(type2)) return false;

  // Either both types should be metatypes or neither should be.
  if (auto meta1 = dyn_cast<MetatypeType>(type1)) {
    auto meta2 = dyn_cast<MetatypeType>(type2);
    if (!meta2) return false;
    if (meta1.getInstanceType() != meta2.getInstanceType()) return false;
    return true;
  }
  
  // Either both types should be functions or neither should be.
  if (auto fn1 = dyn_cast<SILFunctionType>(type1)) {
    auto fn2 = dyn_cast<SILFunctionType>(type2);
    if (!fn2) return false;
    // TODO: maybe there are checks we can do here?
    (void) fn1; (void) fn2;
    return true;
  }
  if (isa<SILFunctionType>(type2)) return false;

  llvm_unreachable("no other types should differ by abstraction");
}
#endif

static bool isNative(AbstractCC cc) {
  switch (cc) {
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    return false;
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    return true;
  }
  llvm_unreachable("bad CC");
}

/// Given two SIL types which are representations of the same type,
/// check whether they have an abstraction difference.
static bool hasAbstractionDifference(AbstractCC cc,
                                     CanType type1, CanType type2) {
  assert(!isNative(cc) || areOnlyAbstractionDifferent(type1, type2));

  // Assuming that we've applied the same substitutions to both types,
  // abstraction equality should equal type equality.
  return (type1 != type2);
}

/// Emit a raw apply operation, performing no additional lowering of
/// either the arguments or the result.
static SILValue emitRawApply(SILGenFunction &gen,
                             SILLocation loc,
                             ManagedValue fn,
                             ArrayRef<Substitution> subs,
                             ArrayRef<ManagedValue> args,
                             CanSILFunctionType substFnType,
                             bool transparent,
                             SILValue resultAddr) {
  // Get the callee value.
  SILValue fnValue = substFnType->isCalleeConsumed()
    ? fn.forward(gen)
    : fn.getValue();

  SmallVector<SILValue, 4> argValues;

  // Add the buffer for the indirect return if needed.
  assert(bool(resultAddr) == substFnType->hasIndirectResult());
  if (substFnType->hasIndirectResult()) {
    assert(resultAddr.getType() ==
             substFnType->getIndirectResult().getSILType().getAddressType());
    argValues.push_back(resultAddr);
  }

  auto inputTypes = substFnType->getParametersWithoutIndirectResult();
  assert(inputTypes.size() == args.size());
  
  // Gather the arguments.
  for (auto i : indices(args)) {
    auto argValue = (inputTypes[i].isConsumed() ? args[i].forward(gen)
                                                : args[i].getValue());
#ifndef NDEBUG
    if (argValue.getType() != inputTypes[i].getSILType()) {
      auto &out = llvm::errs();
      out << "TYPE MISMATCH IN ARGUMENT " << i << " OF APPLY AT ";
      printSILLocationDescription(out, loc, gen.getASTContext());
      out << "  argument value: ";
      argValue.print(out);
      out << "  parameter type: ";
      inputTypes[i].print(out);
      out << "\n";
      abort();
    }
#endif
    argValues.push_back(argValue);
  }

  auto resultType = substFnType->getResult().getSILType();
  auto calleeType = SILType::getPrimitiveObjectType(substFnType);
  SILValue result = gen.B.createApply(loc, fnValue, calleeType,
                                      resultType, subs, argValues,
                                      transparent);
  return result;
}

/// Emit a function application, assuming that the arguments have been
/// lowered appropriately for the abstraction level but that the
/// result does need to be turned back into something matching a
/// formal type.
static ManagedValue emitApply(SILGenFunction &gen,
                              SILLocation loc,
                              ManagedValue fn,
                              ArrayRef<Substitution> subs,
                              ArrayRef<ManagedValue> args,
                              CanSILFunctionType substFnType,
                              AbstractionPattern origResultType,
                              CanType substResultType,
                              bool transparent,
                              SGFContext evalContext) {
  auto &formalResultTL = gen.getTypeLowering(substResultType);
  auto loweredFormalResultType = formalResultTL.getLoweredType();

  SILType actualResultType = substFnType->getSemanticResultSILType();

  // Check whether there are abstraction differences (beyond just
  // direct vs. indirect) between the lowered formal result type and
  // the actual result type we got back.  Note that this will also
  // include bridging differences.
  bool hasAbsDiffs =
    hasAbstractionDifference(substFnType->getAbstractCC(),
                             loweredFormalResultType.getSwiftRValueType(),
                             actualResultType.getSwiftRValueType());

  // Prepare a result address if necessary.
  SILValue resultAddr;
  bool emittedIntoContext = false;
  if (substFnType->hasIndirectResult()) {
    // Get the result type with the abstraction prescribed by the
    // function we're calling.
    assert(actualResultType.isAddress());

    // The context will expect the natural representation of the
    // result type.  If there's no abstraction difference between that
    // and the actual form of the result, then we can emit directly
    // into the context.
    emittedIntoContext = !hasAbsDiffs;
    if (emittedIntoContext) {
      resultAddr = gen.getBufferForExprResult(loc, actualResultType,
                                              evalContext);

    // Otherwise, we need a temporary of the right abstraction.
    } else {
      resultAddr =
        gen.emitTemporaryAllocation(loc, actualResultType.getObjectType());
    }
  }

  // Emit the raw application.
  SILValue scalarResult = emitRawApply(gen, loc, fn, subs, args,
                                       substFnType, transparent, resultAddr);

  // If we emitted into the eval context, then it's because there was
  // no abstraction difference *or* bridging to do.  But our caller
  // might not expect to get a result indirectly.
  if (emittedIntoContext) {
    assert(substFnType->hasIndirectResult());
    assert(!hasAbsDiffs);
    auto managedBuffer =
      gen.manageBufferForExprResult(resultAddr, formalResultTL, evalContext);

    // managedBuffer will be null here to indicate that we satisfied
    // the evalContext.  If so, we're done.
    if (!managedBuffer) return ManagedValue();

    // Otherwise, if the expected type is address-only, we're done.
    if (formalResultTL.isAddressOnly())
      return managedBuffer;

    // Otherwise, deactivate the cleanup we just entered; we're about
    // to take from the address.
    resultAddr = managedBuffer.forward(gen);
  }

  // Get the type lowering for the actual result representation.
  // This is very likely to be the same as that for the formal result.
  auto &actualResultTL
    = (actualResultType == loweredFormalResultType
         ? formalResultTL : gen.getTypeLowering(actualResultType));

  // If the expected result is an address, manage the result address.
  if (formalResultTL.isAddressOnly()) {
    assert(resultAddr);
    auto managedActualResult =
      gen.emitManagedBufferWithCleanup(resultAddr, actualResultTL);

    if (!hasAbsDiffs) return managedActualResult;
    return gen.emitOrigToSubstValue(loc, managedActualResult,
                                    origResultType, substResultType,
                                    evalContext);
  }

  // Okay, we want a scalar result.
  assert(!actualResultTL.isAddressOnly() &&
         "actual result is address-only when formal result is not?");

  ManagedValue managedScalar;

  // If we got an indirect result, emit a take out of the result address.
  if (substFnType->hasIndirectResult()) {
    managedScalar = gen.emitLoad(loc, resultAddr, actualResultTL,
                                 SGFContext(), IsTake);

  // Otherwise, manage the direct result.
  } else {
    switch (substFnType->getResult().getConvention()) {
    case ResultConvention::Owned:
      // Already retained.
      break;

    case ResultConvention::Autoreleased:
      // Autoreleased. Retain using retain_autoreleased.
      gen.B.createStrongRetainAutoreleased(loc, scalarResult);
      break;
    
    case ResultConvention::Unowned:
      // Unretained. Retain the value.
      scalarResult = actualResultTL.emitCopyValue(gen.B, loc, scalarResult);
      break;
    }

    managedScalar = gen.emitManagedRValueWithCleanup(scalarResult,
                                                     actualResultTL);
  }

  // Fast path: no abstraction differences or bridging.
  if (!hasAbsDiffs) return managedScalar;

  // Remove abstraction differences.
  if (origResultType.getAsType() != substResultType) {
    managedScalar = gen.emitOrigToSubstValue(loc, managedScalar,
                                             origResultType,
                                             substResultType,
                                             SGFContext());
  }

  // Convert the result to a native value.
  return gen.emitBridgedToNativeValue(loc, managedScalar,
                                      substFnType->getAbstractCC(),
                                      substResultType);
}

ManagedValue SILGenFunction::emitMonomorphicApply(SILLocation loc,
                                                  ManagedValue fn,
                                                  ArrayRef<ManagedValue> args,
                                                  CanType resultType,
                                                  bool transparent) {
  auto fnType = fn.getType().castTo<SILFunctionType>();
  assert(!fnType->isPolymorphic());
  return emitApply(*this, loc, fn, {}, args, fnType,
                   AbstractionPattern(resultType), resultType,
                   transparent, SGFContext());
}

/// Count the number of SILParameterInfos that are needed in order to
/// pass the given argument.  This wouldn't be necessary if we uncurried
/// left-to-right.
static unsigned getFlattenedValueCount(AbstractionPattern origType,
                                       CanType substType) {
  if (auto origTuple = dyn_cast<TupleType>(origType.getAsType())) {
    auto substTuple = cast<TupleType>(substType);
    unsigned count = 0;
    assert(origTuple->getNumElements() == substTuple->getNumElements());
    for (auto i : indices(origTuple.getElementTypes())) {
      count += getFlattenedValueCount(origType.getTupleElementType(i),
                                      substTuple.getElementType(i));
    }
    return count;
  }

  // Most general form of an unmaterializable type.
  if (auto substTuple = dyn_cast<TupleType>(substType)) {
    assert(origType.isOpaque());
    if (!substTuple->isMaterializable()) {
      unsigned count = 0;
      for (auto substElt : substTuple.getElementTypes()) {
        count += getFlattenedValueCount(origType, substElt);
      }
      return count;
    }
  }
  return 1;
}

static AbstractionPattern claimNextParamClause(AbstractionPattern &type) {
  auto result = type.getFunctionInputType();
  type = type.getFunctionResultType();
  return result;
}

namespace {
  class ArgEmitter {
    SILGenFunction &SGF;
    AbstractCC CC;
    ArrayRef<SILParameterInfo> ParamInfos;
    SmallVectorImpl<ManagedValue> &Args;
  public:
    ArgEmitter(SILGenFunction &SGF, AbstractCC cc,
               ArrayRef<SILParameterInfo> paramInfos,
               SmallVectorImpl<ManagedValue> &args)
      : SGF(SGF), CC(cc), ParamInfos(paramInfos), Args(args) {}

    void emit(RValueSource &&arg, AbstractionPattern origParamType) {
      // If it was a tuple in the original type, the parameters will
      // have been exploded.
      if (isa<TupleType>(origParamType.getAsType())) {
        emitExpanded(std::move(arg), origParamType);
        return;
      }

      auto substArgType = arg.getSubstType();

      // Otherwise, if the substituted type is a tuple, then we should
      // emit the tuple in its most general form, because there's a
      // substitution of an opaque archetype to a tuple or function
      // type in play.  The most general convention is generally to
      // pass the entire tuple indirectly, but if it's not
      // materializable, the convention is actually to break it up
      // into materializable chunks.  See the comment in SILType.cpp.
      if (isUnmaterializableTupleType(substArgType)) {
        assert(origParamType.isOpaque());
        emitExpanded(std::move(arg), origParamType);
        return;
      }

      // Okay, everything else will be passed as a single value, one
      // way or another.

      // The substituted parameter type.  Might be different from the
      // substituted argument type by abstraction and/or bridging.
      auto param = claimNextParameter();

      auto loweredSubstArgType =
        SGF.getLoweredType(substArgType).getSwiftRValueType();

      // L-values are mostly straightforward.
      if (isa<LValueType>(substArgType)) {
        assert(param.isIndirectInOut());
        emitInOut(std::move(arg), loweredSubstArgType, param.getType(),
                  origParamType, substArgType);
        return;
      }

      // If the original type is passed indirectly, copy to memory if
      // it's not already there.  (Note that this potentially includes
      // conventions which pass indirectly without transferring
      // ownership, like Itanium C++.)
      if (param.isIndirect()) {
        assert(!param.isIndirectResult());
        auto value = std::move(arg).materialize(SGF, origParamType,
                                                param.getSILType());
        Args.push_back(value);
        return;
      }

      // Okay, if the original parameter is passed directly, then we
      // just need to handle abstraction differences and bridging.
      emitDirect(std::move(arg), origParamType, param);
    }

  private:
    SILParameterInfo claimNextParameter() {
      assert(!ParamInfos.empty());
      auto param = ParamInfos.front();
      ParamInfos = ParamInfos.slice(1);
      return param;
    }

    bool isUnmaterializableTupleType(CanType type) {
      if (auto tuple = dyn_cast<TupleType>(type))
        if (!tuple->isMaterializable())
          return true;
      return false;
    }

    /// Emit an argument as an expanded tuple.
    void emitExpanded(RValueSource &&arg, AbstractionPattern origParamType) {
      CanTupleType substArgType = cast<TupleType>(arg.getSubstType());

      // The original type isn't necessarily a tuple.
      assert(origParamType.matchesTuple(substArgType));

      // If we're working with an r-value, just expand it out and emit
      // all the elements individually.
      if (arg.isRValue()) {
        auto loc = arg.getKnownRValueLocation();
        SmallVector<RValue, 4> elts;
        std::move(arg).asKnownRValue().extractElements(elts);
        for (auto i : indices(substArgType.getElementTypes())) {
          emit({ loc, std::move(elts[i]) },
               origParamType.getTupleElementType(i));
        }
        return;
      }

      // Otherwise, we're working with an expression.
      Expr *e = std::move(arg).asKnownExpr();
      e = e->getSemanticsProvidingExpr();

      // If the source expression is a tuple literal, we can break it
      // up directly.
      if (auto tuple = dyn_cast<TupleExpr>(e)) {
        for (auto i : indices(tuple->getElements())) {
          emit(tuple->getElements()[i],
               origParamType.getTupleElementType(i));
        }
        return;
      }

      // TODO: tuple shuffles, etc.

      // Fall back to the r-value case.
      emitExpanded({ e, SGF.emitRValue(e) }, origParamType);
    }

    void emitInOut(RValueSource &&arg,
                   CanType loweredSubstArgType, CanType loweredSubstParamType,
                   AbstractionPattern origType, CanType substType) {
      if (arg.isRValue()) {
        emitInOut(arg.getKnownRValueLocation(), std::move(arg).asKnownRValue(),
                  loweredSubstArgType, loweredSubstParamType,
                  origType, substType);
      } else {
        Expr *e = std::move(arg).asKnownExpr();
        emitInOut(e, SGF.emitLValueAsRValue(e),
                  loweredSubstArgType, loweredSubstParamType,
                  origType, substType);
      }
    }

    void emitInOut(SILLocation loc, RValue &&rvalue,
                   CanType loweredSubstArgType, CanType loweredSubstParamType,
                   AbstractionPattern origType, CanType substType) {
      if (hasAbstractionDifference(CC, loweredSubstParamType,
                                   loweredSubstArgType)) {
        // This could actually just be type-bridging.  In any case,
        // it can be dealt with with writeback.
        llvm::errs() << "Unimplemented: abstraction difference in l-value\n";
        llvm::errs() << "\n  "; loweredSubstParamType.dump();
        llvm::errs() << "\n  "; loweredSubstArgType.dump();
        abort();
      }

      std::move(rvalue).getAll(Args);
      return;
    }

    void emitDirect(RValueSource &&arg, AbstractionPattern origParamType,
                    SILParameterInfo param) {
      if (arg.isRValue()) {
        emitDirect(arg.getKnownRValueLocation(), std::move(arg).asKnownRValue(),
                   origParamType, param);
      } else {
        Expr *e = std::move(arg).asKnownExpr();
        emitDirect(e, SGF.emitRValue(e), origParamType, param);
      }
    }

    void emitDirect(SILLocation loc, RValue &&arg,
                    AbstractionPattern origParamType,
                    SILParameterInfo param) {
      auto value = std::move(arg).getScalarValue();
      if (isNative(CC)) {
        value = SGF.emitSubstToOrigValue(loc, value, origParamType,
                                         arg.getType());
      } else {
        value = SGF.emitNativeToBridgedValue(loc, value, CC,
                                             origParamType.getAsType(),
                                             arg.getType(), param.getType());
      }
      Args.push_back(value);
    }
  };

  /// A structure for conveniently claiming sets of uncurried parameters.
  struct ParamLowering {
    ArrayRef<SILParameterInfo> Params;
    AbstractCC CC;

    ParamLowering(CanSILFunctionType fnType) :
      Params(fnType->getParametersWithoutIndirectResult()),
      CC(fnType->getAbstractCC()) {}

    ArrayRef<SILParameterInfo>
    claimParams(AbstractionPattern origParamType, CanType substParamType) {
      unsigned count = getFlattenedValueCount(origParamType, substParamType);
      assert(count <= Params.size());
      auto result = Params.slice(Params.size() - count, count);
      Params = Params.slice(0, Params.size() - count);
      return result;
    }

    ~ParamLowering() {
      assert(Params.empty() && "didn't consume all the parameters");
    }
  };
  
  class CallSite {
  public:
    SILLocation Loc;
    CanType SubstResultType;

  private:
    RValueSource ArgValue;
    
  public:
    CallSite(ApplyExpr *apply)
      : Loc(apply), SubstResultType(apply->getType()->getCanonicalType()),
        ArgValue(apply->getArg()) {
    }
  
    CallSite(SILLocation loc, Expr *expr, Type resultType)
      : Loc(loc), SubstResultType(resultType->getCanonicalType()),
        ArgValue(expr) {
    }
  
    CallSite(SILLocation loc, RValueSource &&value, Type resultType)
      : Loc(loc), SubstResultType(resultType->getCanonicalType()),
        ArgValue(std::move(value)) {
    }
  
    /// Return the substituted, unlowered AST type of the argument.
    CanType getSubstArgType() const {
      return ArgValue.getSubstType();
    }

    /// Return the substituted, unlowered AST type of the result of
    /// this application.
    CanType getSubstResultType() const {
      return SubstResultType;
    }
    
    void emit(SILGenFunction &gen,
              AbstractionPattern origParamType,
              ParamLowering &lowering,
              SmallVectorImpl<ManagedValue> &args) && {
      auto params = lowering.claimParams(origParamType, getSubstArgType());

      ArgEmitter emitter(gen, lowering.CC, params, args);
      emitter.emit(std::move(ArgValue), origParamType);
    }
  };

  class CallEmission {
    SILGenFunction &gen;
    
    std::vector<CallSite> uncurriedSites;
    std::vector<CallSite> extraSites;
    Callee callee;
    Optional<WritebackScope> initialWritebackScope;
    unsigned uncurries;
    bool applied;
    
  public:
    CallEmission(SILGenFunction &gen, Callee &&callee)
      : gen(gen),
        callee(std::move(callee)),
        uncurries(callee.getNaturalUncurryLevel() + 1),
        applied(false)
    {}
    
    CallEmission(SILGenFunction &gen, Callee &&callee,
                 WritebackScope &&writebackScope)
      : CallEmission(gen, std::move(callee))
    {
      initialWritebackScope.emplace(std::move(writebackScope));
    }
    
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
      Callee::SpecializedEmitter specializedEmitter = nullptr;
      CanSILFunctionType substFnType;
      ManagedValue mv;
      bool transparent;

      AbstractionPattern origFormalType(callee.getOrigFormalType());

      // Check for a specialized emitter.
      auto maybeEmitter = callee.getSpecializedEmitter(gen.SGM, uncurryLevel);
      if (maybeEmitter) {
        specializedEmitter = maybeEmitter.getValue();

        // We want to emit the arguments as fully-substituted values
        // because that's what the specialized emitters expect.
        auto formalType = callee.getSubstFormalType();
        origFormalType = AbstractionPattern(formalType);
        substFnType = gen.getLoweredType(formalType, uncurryLevel)
          .castTo<SILFunctionType>();
        transparent = false; // irrelevant
      } else {
        std::tie(mv, substFnType, transparent) =
          callee.getAtUncurryLevel(gen, uncurryLevel);
      }

      // Emit the arguments.
      Optional<SILLocation> uncurriedLoc;
      SmallVector<SmallVector<ManagedValue, 4>, 2> args;
      args.reserve(uncurriedSites.size());
      {
        ParamLowering paramLowering(substFnType);
       
        // Collect the arguments to the uncurried call.
        for (auto &site : uncurriedSites) {
          AbstractionPattern origParamType = claimNextParamClause(origFormalType);
          uncurriedLoc = site.Loc;
          args.push_back({});
          std::move(site).emit(gen, origParamType, paramLowering, args.back());
        }
      }
      assert(uncurriedLoc);

      // Uncurry the arguments in calling convention order.
      SmallVector<ManagedValue, 4> uncurriedArgs;
      for (auto &argSet : reversed(args))
        uncurriedArgs.append(argSet.begin(), argSet.end());
      args = {};
      
      // We use the context emit-into initialization only for the outermost
      // call.
      SGFContext uncurriedContext =
        (extraSites.empty() ? C : SGFContext::Ungeneralized);

      // Emit the uncurried call.
      ManagedValue result;

      if (specializedEmitter)
        result = specializedEmitter(gen,
                                    uncurriedLoc.getValue(),
                                    callee.getSubstitutions(),
                                    uncurriedArgs,
                                    uncurriedContext);
      else
        result = emitApply(gen, uncurriedLoc.getValue(), mv,
                           callee.getSubstitutions(),
                           uncurriedArgs,
                           substFnType,
                           origFormalType,
                           uncurriedSites.back().getSubstResultType(),
                           transparent,
                           uncurriedContext);
      
      // End the initial writeback scope, if any.
      initialWritebackScope.reset();
      
      // If there are remaining call sites, apply them to the result function.
      // Each chained call gets its own writeback scope.
      for (unsigned i = 0, size = extraSites.size(); i < size; ++i) {
        WritebackScope scope(gen);

        auto substFnType = result.getType().castTo<SILFunctionType>();
        ParamLowering paramLowering(substFnType);

        uncurriedArgs.clear();

        AbstractionPattern origParamType = claimNextParamClause(origFormalType);
        std::move(extraSites[i]).emit(gen, origParamType, paramLowering,
                                      uncurriedArgs);
        SGFContext context = i == size - 1 ? C : SGFContext::Ungeneralized;
        SILLocation loc = extraSites[i].Loc;
        result = emitApply(gen, loc, result, {}, uncurriedArgs,
                           substFnType,
                           origFormalType,
                           extraSites[i].getSubstResultType(),
                           false, context);
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
                                            IsTake_t isTake) {
    assert(substitutions.size() == 1 && "load should have single substitution");
    assert(args.size() == 1 && "load should have a single argument");
    
    // The substitution gives the type of the load.  This is always a
    // first-class type; there is no way to e.g. produce a [weak] load
    // with this builtin.
    auto &rvalueTL = gen.getTypeLowering(substitutions[0].Replacement);
    SILType loadedType = rvalueTL.getLoweredType();

    // Convert the pointer argument to a SIL address.
    SILValue addr = gen.B.createPointerToAddress(loc, args[0].getUnmanagedValue(),
                                                 loadedType.getAddressType());
    // Perform the load.
    return gen.emitLoad(loc, addr, rvalueTL, C, isTake);
  }

  static ManagedValue emitBuiltinLoad(SILGenFunction &gen,
                                      SILLocation loc,
                                      ArrayRef<Substitution> substitutions,
                                      ArrayRef<ManagedValue> args,
                                      SGFContext C) {
    return emitBuiltinLoadOrMove(gen, loc, substitutions, args, C, IsNotTake);
  }

  static ManagedValue emitBuiltinMove(SILGenFunction &gen,
                                      SILLocation loc,
                                      ArrayRef<Substitution> substitutions,
                                      ArrayRef<ManagedValue> args,
                                      SGFContext C) {
    return emitBuiltinLoadOrMove(gen, loc, substitutions, args, C, IsTake);
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
    auto &ti = gen.getTypeLowering(substitutions[0].Replacement);
    
    // Destroy is a no-op for trivial types.
    if (ti.isTrivial())
      return ManagedValue(gen.emitEmptyTuple(loc), ManagedValue::Unmanaged);
    
    SILType destroyType = ti.getLoweredType();

    // Convert the pointer argument to a SIL address.
    SILValue addr =
      gen.B.createPointerToAddress(loc, args[1].getUnmanagedValue(),
                                   destroyType.getAddressType());
    
    // Destroy the value indirectly. Canonicalization will promote to loads
    // and releases if appropriate.
    gen.B.emitDestroyAddr(loc, addr);
    
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
    CanType assignFormalType = substitutions[0].Replacement->getCanonicalType();
    SILType assignType = gen.getLoweredType(assignFormalType);
    
    // Convert the destination pointer argument to a SIL address.
    SILValue addr = gen.B.createPointerToAddress(loc,
                                                 args.back().getUnmanagedValue(),
                                                 assignType.getAddressType());
    
    // Build the value to be assigned, reconstructing tuples if needed.
    ManagedValue src = RValue(args.slice(0, args.size() - 1), assignFormalType)
      .getAsSingleValue(gen, loc);
    
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
    assert(substitutions.size() == 1 && "cast should have a type substitution");
    
    // Bail if the source type is not a class reference of some kind.
    if (!substitutions[0].Replacement->mayHaveSuperclass() &&
        !substitutions[0].Replacement->isClassExistentialType()) {
      gen.SGM.diagnose(loc, diag::invalid_sil_builtin,
                       "castToObjectPointer source must be a class");
      // FIXME: Recovery?
      exit(1);
    }
    
    // Save the cleanup on the argument so we can forward it onto the cast
    // result.
    auto cleanup = args[0].getCleanup();
    
    // Take the reference type argument and cast it to ObjectPointer.
    SILType objPointerType = SILType::getObjectPointerType(gen.F.getASTContext());
    SILValue arg = args[0].getValue();

    // If the argument is existential, project it.
    if (substitutions[0].Replacement->isClassExistentialType()) {
      SmallVector<ProtocolDecl *, 4> protocols;
      substitutions[0].Replacement->isExistentialType(protocols);
      ProtocolDecl *proto = *std::find_if(protocols.begin(), protocols.end(),
                                          [](ProtocolDecl *proto) {
                                            return proto->requiresClass();
                                          });
      SILType protoSelfTy = gen.getLoweredType(
                              proto->getSelf()->getArchetype());
      arg = gen.B.createProjectExistentialRef(loc, arg, protoSelfTy);
    }

    SILValue result = gen.B.createRefToObjectPointer(loc, arg, objPointerType);
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

    // Bail if the source type is not a class reference of some kind.
    if (!substitutions[0].Replacement->mayHaveSuperclass()) {
      gen.SGM.diagnose(loc, diag::invalid_sil_builtin,
                       "castFromObjectPointer dest must be a class");
      // FIXME: Recovery?
      exit(1);
    }
    
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
    SILType rawPointerType = SILType::getRawPointerType(gen.F.getASTContext());
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
    auto &destLowering = gen.getTypeLowering(substitutions[0].Replacement);
    assert(destLowering.isLoadable());
    SILType destType = destLowering.getLoweredType();

    // Take the raw pointer argument and cast it to the destination type.
    SILValue result = gen.B.createRawPointerToRef(loc, args[0].getUnmanagedValue(),
                                                  destType);
    // The result has ownership semantics, so retain it with a cleanup.
    return gen.emitManagedRetain(loc, result, destLowering);
  }

  /// Specialized emitter for Builtin.addressof.
  static ManagedValue emitBuiltinAddressOf(SILGenFunction &gen,
                                           SILLocation loc,
                                           ArrayRef<Substitution> substitutions,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext C) {
    assert(args.size() == 1 && "addressof should have a single argument");
    
    // Take the address argument and cast it to RawPointer.
    SILType rawPointerType = SILType::getRawPointerType(gen.F.getASTContext());
    SILValue result = gen.B.createAddressToPointer(loc, args[0].getUnmanagedValue(),
                                                   rawPointerType);
    return ManagedValue(result, ManagedValue::Unmanaged);
  }

  /// Specialized emitter for Builtin.typeof.
  static ManagedValue emitBuiltinTypeOf(SILGenFunction &gen,
                                        SILLocation loc,
                                        ArrayRef<Substitution> substitutions,
                                        ArrayRef<ManagedValue> args,
                                        SGFContext C) {
    assert(args.size() == 1 && "typeof should have a single argument");
    
    // Get the metatype of the argument.
    SILValue metaTy = gen.emitMetatypeOfValue(loc, args[0].getValue());
    return ManagedValue(metaTy, ManagedValue::Unmanaged);
  }

  /// Specialized emitter for Builtin.gep.
  static ManagedValue emitBuiltinGep(SILGenFunction &gen,
                                     SILLocation loc,
                                     ArrayRef<Substitution> substitutions,
                                     ArrayRef<ManagedValue> args,
                                     SGFContext C) {
    assert(args.size() == 2 && "gep should be given two arguments");
    
    SILValue offsetPtr = gen.B.createIndexRawPointer(loc,
                                                   args[0].getUnmanagedValue(),
                                                   args[1].getUnmanagedValue());
    return ManagedValue(offsetPtr, ManagedValue::Unmanaged);
  }
  
  /// Specialized emitter for Builtin.condfail.
  static ManagedValue emitBuiltinCondFail(SILGenFunction &gen,
                                          SILLocation loc,
                                          ArrayRef<Substitution> substitutions,
                                          ArrayRef<ManagedValue> args,
                                          SGFContext C) {
    assert(args.size() == 1 && "condfail should be given one argument");
    
    gen.B.createCondFail(loc, args[0].getUnmanagedValue());
    return ManagedValue(gen.emitEmptyTuple(loc), ManagedValue::Unmanaged);
  }

  Callee::SpecializedEmitter
  Callee::getSpecializedEmitterForSILBuiltin(SILDeclRef function,
                                             SILModule &SILM) {
    // Filter out non-function members and non-builtin modules.

    if (function.kind != SILDeclRef::Kind::Func)
      return nullptr;
    if (!function.hasDecl())
      return nullptr;
    
    ValueDecl *decl = function.getDecl();
    
    if (!isa<BuiltinUnit>(decl->getDeclContext()))
      return nullptr;

    const BuiltinInfo &Builtin = SILM.getBuiltinInfo(decl->getName());

    // Match SIL builtins to their emitters.
    #define BUILTIN(Id, Name, Attrs)
    #define BUILTIN_SIL_OPERATION(Id, Name, Overload) \
      if (Builtin.ID == BuiltinValueKind::Id) \
        return &emitBuiltin##Id;

    #include "swift/AST/Builtins.def"
    
    return nullptr;
  }
} // end anonymous namespace

static CallEmission prepareApplyExpr(SILGenFunction &gen, Expr *e) {
  // Set up writebacks for the call(s).
  WritebackScope writebacks(gen);
  
  SILGenApply apply(gen);
  
  // Decompose the call site.
  apply.decompose(e);
  
  // Evaluate and discard the side effect if present.
  if (apply.sideEffect)
    gen.emitRValue(apply.sideEffect);
  
  // Build the call.
  // Pass the writeback scope on to CallEmission so it can thread scopes through
  // nested calls.
  CallEmission emission(gen, apply.getCallee(), std::move(writebacks));
  
  // Apply 'self' if provided.
  if (apply.selfParam)
    emission.addCallSite(RegularLocation(e),
                         RValueSource(apply.SelfApplyExpr,
                                      std::move(apply.selfParam)),
                         apply.SelfApplyExpr->getType());

  // Apply arguments from call sites, innermost to outermost.
  for (auto site = apply.callSites.rbegin(), end = apply.callSites.rend();
       site != end;
       ++site) {
    emission.addCallSite(*site);
  }
  
  return emission;
}

RValue SILGenFunction::emitApplyExpr(ApplyExpr *e, SGFContext c) {
  ManagedValue result = prepareApplyExpr(*this, e).apply(c);
  return (result ? RValue(*this, e, result) : RValue());
}

ManagedValue
SILGenFunction::emitApplyOfLibraryIntrinsic(SILLocation loc,
                                            FuncDecl *fn,
                                            ArrayRef<Substitution> subs,
                                            ArrayRef<ManagedValue> args,
                                            SGFContext ctx) {
  auto origFormalType = 
    cast<AnyFunctionType>(fn->getType()->getCanonicalType());
  auto substFormalType = origFormalType;
  if (!subs.empty()) {
    auto polyFnType = cast<PolymorphicFunctionType>(substFormalType);
    auto applied = polyFnType->substGenericArgs(SGM.SwiftModule, subs);
    substFormalType = cast<FunctionType>(applied->getCanonicalType());
  }

  auto callee = Callee::forDirect(*this, SILDeclRef(fn), substFormalType, loc);
  callee.setSubstitutions(*this, loc, subs, 0);

  ManagedValue mv;
  CanSILFunctionType substFnType;
  bool transparent;
  std::tie(mv, substFnType, transparent) = callee.getAtUncurryLevel(*this, 0);

  assert(substFnType->getAbstractCC() == AbstractCC::Freestanding);

  return emitApply(*this, loc, mv, subs, args, substFnType,
                   AbstractionPattern(origFormalType.getResult()),
                   substFormalType.getResult(),
                   transparent, ctx);
}

/// emitArrayInjectionCall - Form an array "Array" out of an ObjectPointer
/// (which represents the retain count), a base pointer to some elements, and a
/// length.
ManagedValue SILGenFunction::emitArrayInjectionCall(ManagedValue ObjectPtr,
                                            SILValue BasePtr,
                                            SILValue Length,
                                            Expr *ArrayInjectionFunction,
                                            SILLocation Loc) {
  // Bitcast the BasePtr (an lvalue) to Builtin.RawPointer if it isn't already.
  if (BasePtr.getType() != SILType::getRawPointerType(F.getASTContext()))
    BasePtr = B.createAddressToPointer(Loc,
                              BasePtr,
                              SILType::getRawPointerType(F.getASTContext()));

  // Construct a call to the injection function.
  CallEmission emission = prepareApplyExpr(*this, ArrayInjectionFunction);
  auto *injectionFnTy
    = ArrayInjectionFunction->getType()->getAs<FunctionType>();
  
  auto injectionArgsTy = cast<TupleType>(injectionFnTy->getInput()->getCanonicalType());
  RValue InjectionArgs(injectionArgsTy);
  InjectionArgs.addElement(RValue(*this, Loc, injectionArgsTy.getElementType(0),
                               ManagedValue(BasePtr, ManagedValue::Unmanaged)));
  InjectionArgs.addElement(RValue(*this, Loc, injectionArgsTy.getElementType(1),
                                  ObjectPtr));
  InjectionArgs.addElement(RValue(*this, Loc, injectionArgsTy.getElementType(2),
                                ManagedValue(Length, ManagedValue::Unmanaged)));
  
  emission.addCallSite(Loc, RValueSource(Loc, std::move(InjectionArgs)),
                       injectionFnTy->getResult());
  return emission.apply();
}

static Callee getBaseAccessorFunctionRef(SILGenFunction &gen,
                                         SILLocation loc,
                                         SILDeclRef constant,
                                         RValueSource &selfValue,
                                         CanAnyFunctionType substAccessorType) {
  ValueDecl *decl = constant.getDecl();

  // FIXME: Have a nicely-abstracted way to figure out which kind of
  // dispatch we're doing.
  // FIXME: We should do this for any declaration within a class. However,
  // IRGen doesn't yet have the machinery for handling class_method on
  // getters and setters.
  if (gen.SGM.requiresObjCDispatch(decl)) {
    auto self = selfValue.forceAndPeekRValue(gen).peekScalarValue();
    return Callee::forClassMethod(gen, self, constant, substAccessorType, loc);
  }

  return Callee::forDirect(gen, constant, substAccessorType, loc);
}

static Callee 
emitSpecializedAccessorFunctionRef(SILGenFunction &gen,
                                   SILLocation loc,
                                   SILDeclRef constant,
                                   ArrayRef<Substitution> substitutions,
                                   RValueSource &selfValue)
{
  // If the accessor is a local constant, use it.
  // FIXME: Can local properties ever be generic?
  if (gen.LocalFunctions.count(constant)) {
    SILValue v = gen.LocalFunctions[constant];
    auto formalType =
      gen.SGM.Types.getConstantFormalTypeWithoutCaptures(constant);
    return Callee::forIndirect(gen.emitManagedRetain(loc, v),
                               formalType, formalType, false, loc);
  }

  SILConstantInfo constantInfo = gen.getConstantInfo(constant);

  // Apply substitutions to the callee type.
  CanAnyFunctionType substAccessorType = constantInfo.FormalType;
  if (!substitutions.empty()) {
    auto polyFn = cast<PolymorphicFunctionType>(substAccessorType);
    auto substFn = polyFn->substGenericArgs(gen.SGM.SwiftModule, substitutions);
    substAccessorType = cast<FunctionType>(substFn->getCanonicalType());
  }
  
  // Get the accessor function. The type will be a polymorphic function if
  // the Self type is generic.
  // FIXME: Dynamic dispatch for archetype/existential methods.
  Callee callee = getBaseAccessorFunctionRef(gen, loc, constant, selfValue,
                                             substAccessorType);
  
  // If there are substitutions, specialize the generic accessor.
  // FIXME: Generic subscript operator could add another layer of
  // substitutions.
  if (!substitutions.empty()) {
    callee.setSubstitutions(gen, loc, substitutions, 0);
  }
  return callee;
}

/// Emit a call to a getter.
ManagedValue SILGenFunction::emitGetAccessor(SILLocation loc,
                                             SILDeclRef get,
                                             ArrayRef<Substitution> substitutions,
                                             RValueSource &&selfValue,
                                             RValueSource &&subscripts,
                                             CanType resultType,
                                             SGFContext c) {
  Callee getter = emitSpecializedAccessorFunctionRef(*this, loc, get,
                                                     substitutions, selfValue);
  CanAnyFunctionType accessType = getter.getSubstFormalType();

  CallEmission emission(*this, std::move(getter));
  // Self ->
  if (selfValue) {
    emission.addCallSite(loc, std::move(selfValue), accessType.getResult());
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }
  // Index ->
  if (subscripts) {
    emission.addCallSite(loc, std::move(subscripts), accessType.getResult());
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }
  // () ->
  emission.addCallSite(loc, RValueSource(loc, emitEmptyTupleRValue(loc)),
                       accessType.getResult());
  // T
  return emission.apply(c);
}

void SILGenFunction::emitSetAccessor(SILLocation loc,
                                     SILDeclRef set,
                                     ArrayRef<Substitution> substitutions,
                                     RValueSource &&selfValue,
                                     RValueSource &&subscripts,
                                     RValueSource &&setValue) {
  Callee setter = emitSpecializedAccessorFunctionRef(*this, loc, set,
                                                     substitutions, selfValue);
  CanAnyFunctionType accessType = setter.getSubstFormalType();

  CallEmission emission(*this, std::move(setter));
  // Self ->
  if (selfValue) {
    emission.addCallSite(loc, std::move(selfValue), accessType.getResult());
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }
  // Index ->
  if (subscripts) {
    emission.addCallSite(loc, std::move(subscripts), accessType.getResult());
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }
  // T ->
  setValue.rewriteType(accessType.getInput());
  emission.addCallSite(loc, std::move(setValue), accessType.getResult());
  // ()
  emission.apply();
}

RValue SILGenFunction::emitDynamicMemberRefExpr(DynamicMemberRefExpr *e,
                                                SGFContext c) {
  // Emit the operand.
  ManagedValue existential = emitRValue(e->getBase(), c)
                               .getAsSingleValue(*this, e->getBase());

  SILValue operand = existential.getValue();
  if (e->getMember().getDecl()->isInstanceMember()) {
    operand = B.createProjectExistentialRef(e, operand,
                                  getSelfTypeForDynamicLookup(*this, operand));
    operand = B.createRefToObjectPointer(e, operand,
                                 SILType::getObjCPointerType(getASTContext()));
  }

  // Create the has-member block.
  SILBasicBlock *hasMemberBB = new (F.getModule()) SILBasicBlock(&F);

  // Create the no-member block.
  SILBasicBlock *noMemberBB = new (F.getModule()) SILBasicBlock(&F);

  // Create the continuation block.
  SILBasicBlock *contBB = new (F.getModule()) SILBasicBlock(&F);

  // The continuation block
  const TypeLowering &optTL = getTypeLowering(e->getType());
  auto loweredOptTy = optTL.getLoweredType();

  SILValue optTemp = emitTemporaryAllocation(e, loweredOptTy);

  // Create the branch.
  SILDeclRef member(e->getMember().getDecl(),
                    isa<VarDecl>(e->getMember().getDecl())
                      ? SILDeclRef::Kind::Getter
                      : SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC=*/true);
  B.createDynamicMethodBranch(e, operand, member, hasMemberBB, noMemberBB);

  // Create the has-member branch.
  {
    B.emitBlock(hasMemberBB);

    FullExpr hasMemberScope(Cleanups, CleanupLocation(e));

    // The argument to the has-member block is the uncurried method.
    auto valueTy =
      e->getType()->getOptionalObjectType(getASTContext())->getCanonicalType();
    auto methodTy = valueTy;

    // For a computed variable, we want the getter.
    if (member.isAccessor())
      methodTy = CanFunctionType::get(TupleType::getEmpty(getASTContext()),
                                      methodTy);

    auto dynamicMethodTy = getDynamicMethodType(SGM, operand, member,
                                                methodTy);
    auto loweredMethodTy = getLoweredType(dynamicMethodTy, 1);
    SILValue memberArg = new (F.getModule()) SILArgument(loweredMethodTy,
                                                         hasMemberBB);

    // Create the result value.
    SILValue result = B.createPartialApply(e, memberArg, memberArg.getType(),
                                           {}, operand,
                                           getLoweredType(methodTy));
    if (member.isAccessor()) {
      result = B.createApply(e, result, result.getType(),
                             getLoweredType(valueTy), {}, {});
    }

    // Package up the result in an optional.
    RValue resultRV = RValue(*this, e, valueTy,
                             emitManagedRValueWithCleanup(result));
    emitInjectOptionalValueInto(e, {e, std::move(resultRV)}, optTemp, optTL);

    // Branch to the continuation block.
    B.createBranch(e, contBB);
  }

  // Create the no-member branch.
  {
    B.emitBlock(noMemberBB);

    emitInjectOptionalNothingInto(e, optTemp, optTL);

    // Branch to the continuation block.
    B.createBranch(e, contBB);
  }

  // Emit the continuation block.
  B.emitBlock(contBB);

  // Package up the result.
  auto optResult = B.createLoad(e, optTemp);
  return RValue(*this, e, emitManagedRValueWithCleanup(optResult, optTL));
}

RValue SILGenFunction::emitDynamicSubscriptExpr(DynamicSubscriptExpr *e, 
                                                SGFContext c) {
  // Emit the base operand.
  ManagedValue existential = emitRValue(e->getBase(), c)
                               .getAsSingleValue(*this, e->getBase());

  SILValue base = existential.getValue();
  base = B.createProjectExistentialRef(e, base, 
           getSelfTypeForDynamicLookup(*this, base));
  base = B.createRefToObjectPointer(e, base,
           SILType::getObjCPointerType(getASTContext()));

  // Emit the index.
  RValue index = emitRValue(e->getIndex());

  // Create the has-member block.
  SILBasicBlock *hasMemberBB = new (F.getModule()) SILBasicBlock(&F);

  // Create the no-member block.
  SILBasicBlock *noMemberBB = new (F.getModule()) SILBasicBlock(&F);

  // Create the continuation block.
  SILBasicBlock *contBB = new (F.getModule()) SILBasicBlock(&F);

  // The continuation block
  const TypeLowering &optTL = getTypeLowering(e->getType());
  auto loweredOptTy = optTL.getLoweredType();
  SILValue optTemp = emitTemporaryAllocation(e, loweredOptTy);

  // Create the branch.
  SILDeclRef member(e->getMember().getDecl(),
                    SILDeclRef::Kind::Getter,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC=*/true);
  B.createDynamicMethodBranch(e, base, member, hasMemberBB, noMemberBB);

  // Create the has-member branch.
  {
    B.emitBlock(hasMemberBB);

    FullExpr hasMemberScope(Cleanups, CleanupLocation(e));

    // The argument to the has-member block is the uncurried method.
    auto valueTy =
      e->getType()->getOptionalObjectType(getASTContext())->getCanonicalType();
    auto subscript = cast<SubscriptDecl>(e->getMember().getDecl());
    auto methodTy = subscript->getGetterType()->castTo<AnyFunctionType>()
                      ->getResult();
    auto dynamicMethodTy = getDynamicMethodType(SGM, base, member,
                                                methodTy);
    auto loweredMethodTy = getLoweredType(dynamicMethodTy, 2);
    SILValue memberArg = new (F.getModule()) SILArgument(loweredMethodTy,
                                                         hasMemberBB);
    // Emit the application of 'self'.
    SILValue result = B.createPartialApply(e, memberArg, memberArg.getType(),
                                           {}, base,
                                           getLoweredType(methodTy, 1));

    // Emit the index.
    llvm::SmallVector<SILValue, 1> indexArgs;
    std::move(index).forwardAll(*this, indexArgs);
    auto &valueTL = getTypeLowering(valueTy);
    result = B.createApply(e, result, result.getType(),
                           valueTL.getLoweredType(), {}, indexArgs);

    // Package up the result in an optional.
    RValue resultRV =
      RValue(*this, e, valueTy, emitManagedRValueWithCleanup(result, valueTL));
    emitInjectOptionalValueInto(e, {e, std::move(resultRV)}, optTemp, optTL);

    // Branch to the continuation block.
    B.createBranch(e, contBB);
  }

  // Create the no-member branch.
  {
    B.emitBlock(noMemberBB);

    emitInjectOptionalNothingInto(e, optTemp, optTL);

    // Branch to the continuation block.
    B.createBranch(e, contBB);
  }

  // Emit the continuation block.
  B.emitBlock(contBB);

  // Package up the result.
  auto optValue = B.createLoad(e, optTemp);
  return RValue(*this, e, emitManagedRValueWithCleanup(optValue, optTL));
}
