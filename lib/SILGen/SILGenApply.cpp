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
#include "OwnershipConventions.h"
#include "RValue.h"
#include "Scope.h"
#include "SILGen.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Module.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILArgument.h"

using namespace swift;
using namespace Lowering;

/// Retrieve the type to use for a method found via dynamic lookup.
static CanType getDynamicMethodType(SILGenFunction &gen,
                                    SILValue proto,
                                    SILDeclRef methodName,
                                    Type memberType) {
  auto &ctx = memberType->getASTContext();
  Type selfTy;
  if (methodName.getDecl()->isInstanceMember()) {
    selfTy = ctx.TheObjCPointerType;
  } else {
    selfTy = proto.getType().getSwiftType();
  }
  auto info = FunctionType::ExtInfo()
                .withCallingConv(gen.SGM.getConstantCC(methodName))
                .withIsThin(true);

  return FunctionType::get(selfTy, memberType, info, ctx)
           ->getCanonicalType();
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
    struct {
      SILValue selfValue;
      SILDeclRef methodName;
      CanType origType;
    } genericMethod;
  };
  std::vector<Substitution> substitutions;
  // There is an initialization order dependency between genericMethod and
  // specializedType.
  CanType specializedType;
  Optional<SILLocation> specializeLoc;
  bool isTransparent;

  // The pointer back to the AST node that produced the callee.
  SILLocation Loc;

  static SpecializedEmitter getSpecializedEmitterForSILBuiltin(SILDeclRef c,
                                                               SILModule &M);

  Callee(ManagedValue indirectValue, bool isTransparent, SILLocation L)
    : kind(Kind::IndirectValue),
      indirectValue(indirectValue),
      specializedType(indirectValue.getType().getSwiftRValueType()),
      specializeLoc(),
      isTransparent(isTransparent),
      Loc(L)
  {}

  Callee(SILGenFunction &gen, SILDeclRef standaloneFunction, SILLocation l)
    : kind(Kind::StandaloneFunction), standaloneFunction(standaloneFunction),
      specializedType(
                gen.SGM.getConstantType(standaloneFunction.atUncurryLevel(0))
                  .getSwiftRValueType()),
      specializeLoc(),
      isTransparent(standaloneFunction.isTransparent()),
      Loc(l)
  {
  }

  Callee(Kind methodKind,
         SILGenFunction &gen,
         SILValue selfValue,
         SILDeclRef methodName,
         SILLocation l)
    : kind(methodKind), method{selfValue, methodName},
      specializedType(
                gen.SGM.getConstantType(methodName.atUncurryLevel(0))
                  .getSwiftRValueType()),
      specializeLoc(),
      isTransparent(false),
      Loc(l)
  {
    assert(kind >= Kind::VirtualMethod_First &&
           kind <= Kind::VirtualMethod_Last &&
           "this constructor is only used for class/super method callees");
  }
  
  static const enum class ForArchetype_t {} ForArchetype{};
  static const enum class ForProtocol_t {} ForProtocol{};
  static const enum class ForDynamic_t {} ForDynamic{};

  static ArchetypeType *getArchetypeType(SILType t) {
    if (auto metatype = t.getAs<MetaTypeType>())
      return cast<ArchetypeType>(metatype.getInstanceType());
    else
      return t.castTo<ArchetypeType>();
  }
  
  Callee(ForArchetype_t,
         SILGenFunction &gen,
         SILValue archetype, SILDeclRef methodName, Type memberType,
         SILLocation l)
    : kind(Kind::ArchetypeMethod),
      genericMethod{archetype,
                    methodName,
                    FunctionType::get(archetype.getType().getSwiftType(),
                          memberType,
                          FunctionType::ExtInfo()
                            .withIsThin(getArchetypeType(archetype.getType())
                                          ->requiresClass())
                            .withCallingConv(gen.SGM.getConstantCC(methodName)),
                          memberType->getASTContext())
                      ->getCanonicalType()},
    specializedType(genericMethod.origType),
    isTransparent(false),
    Loc(l)
  {
  }

  static CanType getProtocolMethodType(SILGenFunction &gen,
                                       SILValue proto,
                                       SILDeclRef methodName,
                                       Type memberType) {
    // 'self' for instance methods is projected out of the existential container
    // as its Self archetype.
    // 'self' for existential metatypes is the metatype itself.
    Type selfTy;
    bool isThin;
    if (methodName.getDecl()->isInstanceMember()) {
      selfTy = cast<ProtocolDecl>(methodName.getDecl()->getDeclContext())
            ->getSelf()->getArchetype();
      if (proto.getType().isClassExistentialType()) {
        isThin = true;
      } else {
        selfTy = LValueType::get(selfTy,
                                 LValueType::Qual::DefaultForInOutSelf,
                                 selfTy->getASTContext());
        isThin = false;
      }
    } else {
      selfTy = proto.getType().getSwiftType();
      isThin = false;
    }
    
    // This is a method reference. Extract the method implementation from the
    // archetype and apply the "self" argument.
    auto Info = FunctionType::ExtInfo()
                  .withCallingConv(gen.SGM.getConstantCC(methodName))
                  .withIsThin(isThin);
    return FunctionType::get(selfTy,
                             memberType,
                             Info,
                             memberType->getASTContext())
      ->getCanonicalType();
  }

  Callee(ForProtocol_t,
         SILGenFunction &gen,
         SILValue proto, SILDeclRef methodName, Type memberType, SILLocation l)
    : kind(Kind::ProtocolMethod),
      genericMethod{proto, methodName,
                    getProtocolMethodType(gen, proto, methodName, memberType)},
      specializedType(genericMethod.origType),
      isTransparent(false),
      Loc(l)
  {}

  Callee(ForDynamic_t,
         SILGenFunction &gen,
         SILValue proto, SILDeclRef methodName, Type memberType, SILLocation l)
    : kind(Kind::DynamicMethod),
      genericMethod{proto, methodName,
                    getDynamicMethodType(gen, proto, methodName, memberType)},
      specializedType(genericMethod.origType),
      isTransparent(false),
      Loc(l)
  {}

public:
  static Callee forIndirect(ManagedValue indirectValue, bool isTransparent,
                            SILLocation l) {
    return Callee(indirectValue, isTransparent, l);
  }
  static Callee forDirect(SILGenFunction &gen, SILDeclRef c, SILLocation l) {
    return Callee(gen, c, l);
  }
  static Callee forClassMethod(SILGenFunction &gen, SILValue selfValue,
                               SILDeclRef name, SILLocation l) {
    return Callee(Kind::ClassMethod, gen, selfValue, name, l);
  }
  static Callee forSuperMethod(SILGenFunction &gen, SILValue selfValue,
                               SILDeclRef name, SILLocation l) {
    return Callee(Kind::SuperMethod, gen, selfValue, name, l);
  }
  static Callee forArchetype(SILGenFunction &gen, SILValue archetypeValue,
                             SILDeclRef name, Type memberType, SILLocation l) {
    return Callee(ForArchetype, gen, archetypeValue, name, memberType, l);
  }
  static Callee forProtocol(SILGenFunction &gen, SILValue proto,
                            SILDeclRef name, Type memberType, SILLocation l) {
    return Callee(ForProtocol, gen, proto, name, memberType, l);
  }
  static Callee forDynamic(SILGenFunction &gen, SILValue proto,
                           SILDeclRef name, Type memberType, SILLocation l) {
    return Callee(ForDynamic, gen, proto, name, memberType, l);
  }
  Callee(Callee &&) = default;
  Callee &operator=(Callee &&) = default;

  void addSubstitutions(SILGenFunction &gen,
                        SILLocation loc,
                        ArrayRef<Substitution> newSubs,
                        CanType subType,
                        unsigned callDepth) {
    // Currently generic methods of generic types are the deepest we should
    // be able to stack specializations.
    // FIXME: Generic local functions can add type parameters to arbitrary
    // depth.
    assert(callDepth < 2 && "specialization below 'self' or argument depth?!");
    substitutions.insert(substitutions.end(),
                         newSubs.begin(),
                         newSubs.end());
    // Save the type of the SpecializeExpr at the right depth in the type.
    assert(getNaturalUncurryLevel() >= callDepth
           && "specializations below uncurry level?!");
    AbstractCC cc = cast<AnyFunctionType>(specializedType)->getAbstractCC();
    bool wasThin = cast<AnyFunctionType>(specializedType)->isThin();
    if (callDepth == 0) {
      Type subFnType = wasThin
        ? getThinFunctionType(subType, cc)
        : getThickFunctionType(subType, cc);
      specializedType = subFnType->getCanonicalType();
    } else {
      FunctionType *ft = cast<FunctionType>(specializedType);
      Type outerInput = ft->getInput();
      auto Info = FunctionType::ExtInfo()
                    .withCallingConv(cc)
                    .withIsThin(ft->isThin());
      specializedType = CanType(FunctionType::get(outerInput,
                                                  subType,
                                                  Info,
                                                  outerInput->getASTContext()));
    }
    specializeLoc = loc;
  }
  
  void addSubstitutions(SILGenFunction &gen,
                        SpecializeExpr *e,
                        unsigned callDepth) {
    addSubstitutions(gen, e, e->getSubstitutions(),
                     e->getType()->getCanonicalType(), callDepth);
  }

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
  
  std::tuple<ManagedValue, OwnershipConventions, bool>
  getAtUncurryLevel(SILGenFunction &gen, unsigned level) const {
    ManagedValue mv;
    OwnershipConventions ownership;
    bool transparent = isTransparent;

    switch (kind) {
    case Kind::IndirectValue:
      assert(level == 0 && "can't curry indirect function");
      mv = indirectValue;
      ownership = OwnershipConventions::getDefault(gen, mv.getType());
      break;

    case Kind::StandaloneFunction: {
      assert(level <= standaloneFunction.uncurryLevel
             && "uncurrying past natural uncurry level of standalone function");
      if (level < standaloneFunction.uncurryLevel)
        transparent = false;
      SILDeclRef constant = standaloneFunction.atUncurryLevel(level);
      SILValue ref = gen.emitGlobalFunctionRef(Loc, constant);
      mv = ManagedValue(ref, ManagedValue::Unmanaged);
      ownership = OwnershipConventions::get(gen, constant, ref.getType());
      break;
    }
    case Kind::ClassMethod: {
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");
      SILDeclRef c = method.methodName.atUncurryLevel(level);
      ownership = OwnershipConventions::get(gen, c, gen.SGM.getConstantType(c));
      
      // If the call is curried, emit a direct call to the curry thunk.
      if (level < method.methodName.uncurryLevel) {
        SILValue ref = gen.emitGlobalFunctionRef(Loc, c);
        mv = ManagedValue(ref, ManagedValue::Unmanaged);
        break;
      }
      
      // Otherwise, do the dynamic dispatch inline.
      SILValue methodVal = gen.B.createClassMethod(Loc,
                                                   method.selfValue,
                                                   c, gen.SGM.getConstantType(c),
                                                   /*volatile*/ c.isForeign);
      
      mv = ManagedValue(methodVal, ManagedValue::Unmanaged);
      break;
    }
    case Kind::SuperMethod: {
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");
      assert(level >= 1
             && "currying 'self' of super method dispatch not yet supported");

      SILDeclRef c = method.methodName.atUncurryLevel(level);
      ownership = OwnershipConventions::get(gen, c, gen.SGM.getConstantType(c));
      
      SILValue methodVal = gen.B.createSuperMethod(Loc,
                                                   method.selfValue,
                                                   c, gen.SGM.getConstantType(c),
                                                   /*volatile*/ c.isForeign);
      
      mv = ManagedValue(methodVal, ManagedValue::Unmanaged);
      break;
    }
    case Kind::ArchetypeMethod: {
      assert(level >= 1
             && "currying 'self' of generic method dispatch not yet supported");
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");

      SILDeclRef constant = genericMethod.methodName.atUncurryLevel(level);
      CanType archetypeType
        = genericMethod.selfValue.getType().getSwiftRValueType();
      if (auto metatype = dyn_cast<MetaTypeType>(archetypeType))
        archetypeType = CanType(metatype->getInstanceType());
      SILValue method = gen.B.createArchetypeMethod(Loc,
                           gen.getLoweredType(archetypeType),
                           constant,
                           gen.getLoweredType(genericMethod.origType, level),
                           /*volatile*/ constant.isForeign);
      mv = ManagedValue(method, ManagedValue::Unmanaged);
      ownership = OwnershipConventions::get(gen, constant, method.getType());
      break;
    }
    case Kind::ProtocolMethod: {
      assert(level >= 1
             && "currying 'self' of generic method dispatch not yet supported");
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");
      
      SILDeclRef constant = genericMethod.methodName.atUncurryLevel(level);
      SILValue method = gen.B.createProtocolMethod(Loc,
                            genericMethod.selfValue,
                            constant,
                            gen.getLoweredType(genericMethod.origType, level),
                            /*volatile*/ constant.isForeign);
      mv = ManagedValue(method, ManagedValue::Unmanaged);
      ownership = OwnershipConventions::get(gen, constant, method.getType());
      break;
    }
    case Kind::DynamicMethod: {
      assert(level >= 1
             && "currying 'self' of dynamic method dispatch not yet supported");
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");

      SILDeclRef constant = genericMethod.methodName.atUncurryLevel(level);
      SILValue method = gen.B.createDynamicMethod(Loc,
                          genericMethod.selfValue,
                          constant,
                          gen.getLoweredType(genericMethod.origType, level),
                          /*volatile*/ constant.isForeign);
      mv = ManagedValue(method, ManagedValue::Unmanaged);
      ownership = OwnershipConventions::get(gen, constant, method.getType());
      break;
    }
    }
    
    // If the callee needs to be specialized, do so.
    if (specializeLoc) {
      SILType specializedUncurriedType
        = gen.getLoweredLoadableType(specializedType, level);
      
      // Recalculate the ownership conventions because the substitutions may
      // have changed the function signature.
      // FIXME: Currently only native methods can be specialized, so always use
      // default ownership semantics.
      ownership = OwnershipConventions::getDefault(gen, specializedUncurriedType);
    }
    
    return {mv, ownership, transparent};
  }
  
  ArrayRef<Substitution> getSubstitutions() const {
    return substitutions;
  }

  /// Return a specialized emission function if this is a function with a known
  /// lowering, such as a builtin, or return null if there is no specialized
  /// emitter.
  SpecializedEmitter getSpecializedEmitter(unsigned uncurryLevel,
                                           SILModule &SILM) const {
    // Currently we have no curried known functions.
    if (uncurryLevel != 0)
      return nullptr;
    
    switch (kind) {
    case Kind::StandaloneFunction: {
      if (SpecializedEmitter e
            = getSpecializedEmitterForSILBuiltin(standaloneFunction, SILM))
        return e;
      SWIFT_FALLTHROUGH;
    }
    case Kind::IndirectValue:
    case Kind::ClassMethod:
    case Kind::SuperMethod:
    case Kind::ArchetypeMethod:
    case Kind::ProtocolMethod:
    case Kind::DynamicMethod:
      return nullptr;
    }
  }
};

/// Get the 'Self' type of a DynamicLookup operand to use as the result type of
/// projecting the object instance handle.
SILType getSelfTypeForDynamicLookup(SILGenFunction &gen,
                                    SILValue existential) {
  CanType ty = existential.getType().getSwiftRValueType();
  ProtocolDecl *proto = cast<ProtocolType>(ty)->getDecl();
  // DynamicLookup is a class protocol so its projection should be loadable.
  return gen.getLoweredLoadableType(proto->getSelf()->getDeclaredType());
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

  /// Fall back to an unknown, indirect callee.
  void visitExpr(Expr *e) {
    ManagedValue fn = gen.emitRValue(e).getAsSingleValue(gen, e);
    setCallee(Callee::forIndirect(fn, false, e));
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
    ManagedValue fn = gen.emitRValue(e).getAsSingleValue(gen, e);
    setCallee(Callee::forIndirect(fn, isTransparent, e));
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
    if (auto *fd = dyn_cast<FuncDecl>(e->getDecl())) {
      if (isa<ClassDecl>(fd->getDeclContext()) || fd->isObjC()) {
        ApplyExpr *thisCallSite = callSites.back();
        callSites.pop_back();
        setSelfParam(gen.emitRValue(thisCallSite->getArg()), thisCallSite);
        SILDeclRef constant(fd,
                             SILDeclRef::ConstructAtNaturalUncurryLevel,
                             gen.SGM.requiresObjCDispatch(fd));
        
        setCallee(Callee::forClassMethod(gen, selfParam.peekScalarValue(),
                                         constant, e));
        
        // setSelfParam bumps the callDepth, but we aren't really past the
        // 'self' call depth in this case.
        --callDepth;
        return;
      }
    }
    
    // FIXME: Store context values for local funcs in a way that we can
    // apply them directly as an added "call site" here.
    SILDeclRef constant(e->getDecl(),
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         gen.SGM.requiresObjCDispatch(e->getDecl()));

    // Obtain a reference for a local closure.
    if (gen.LocalConstants.count(constant)) {
      ManagedValue localFn = gen.emitReferenceToDecl(e, e->getDecl());
      setCallee(Callee::forIndirect(localFn, false, e));

    // Otherwise, stash the SILDeclRef.
    } else {
      setCallee(Callee::forDirect(gen, constant, e));
    }
  }
  void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *e) {
    // FIXME: We might need to go through ObjC dispatch for references to
    // constructors imported from Clang (which won't have a direct entry point)
    // or to delegate to a designated initializer.
    setCallee(Callee::forDirect(gen,
                SILDeclRef(e->getDecl(), SILDeclRef::Kind::Initializer), e));
  }
  void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e) {
    setSideEffect(e->getLHS());
    visit(e->getRHS());
  }
  void visitExistentialMemberRefExpr(ExistentialMemberRefExpr *e) {
    ManagedValue existential =
      gen.emitRValue(e->getBase()).getAsSingleValue(gen, e->getBase());
    
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

      setSelfParam(RValue(gen, proj, e), e);
    } else {
      assert(existential.getType().is<MetaTypeType>() &&
             "non-existential-metatype for existential static method?!");
      setSelfParam(RValue(gen, existential, e), e);
    }

    // Method calls through ObjC protocols require ObjC dispatch.
    bool isObjC = proto->isObjC();
    
    setCallee(Callee::forProtocol(gen, existential.getValue(),
                                  SILDeclRef(fd).asForeign(isObjC),
                                  e->getType(), e));
  }
  void visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *e) {
    setSelfParam(gen.emitRValue(e->getBase()), e);
    
    auto *fd = dyn_cast<FuncDecl>(e->getDecl());
    assert(fd && "archetype properties not yet supported");

    // Method calls through ObjC protocols require ObjC dispatch.
    bool isObjC = cast<ProtocolDecl>(fd->getDeclContext())->isObjC();
    
    setCallee(Callee::forArchetype(gen, selfParam.peekScalarValue(),
                                   SILDeclRef(fd).asForeign(isObjC),
                                   e->getType(), e));
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
    SILDeclRef constant;
    SpecializeExpr *specialize = dyn_cast<SpecializeExpr>(fn);
    if (specialize)
      fn = specialize->getSubExpr();

    if (auto *ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(fn)) {
      constant = SILDeclRef(ctorRef->getDecl(), SILDeclRef::Kind::Initializer,
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         gen.SGM.requiresObjCSuperDispatch(ctorRef->getDecl()));
    } else if (auto *declRef = dyn_cast<DeclRefExpr>(fn)) {
      assert(isa<FuncDecl>(declRef->getDecl()) && "non-function super call?!");
      constant = SILDeclRef(declRef->getDecl(),
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         gen.SGM.requiresObjCSuperDispatch(declRef->getDecl()));
    } else
      llvm_unreachable("invalid super callee");

    // Upcast 'self' parameter to the super type.
    SILType superTy
      = gen.getLoweredLoadableType(arg->getType()->getRValueType());
    SILValue superUpcast = gen.B.createUpcast(arg, super.getValue(),
                                              superTy);
    
    setSelfParam(RValue(gen,
                        ManagedValue(superUpcast, super.getCleanup()), apply),
                 apply);
    
    SILValue superMethod;
    if (constant.isForeign) {
      // ObjC super calls require dynamic dispatch.
      setCallee(Callee::forSuperMethod(gen, super.getValue(), constant, fn));
    } else {
      // Native Swift super calls are direct.
      setCallee(Callee::forDirect(gen, constant, fn));
    }

    if (specialize)
      callee->addSubstitutions(gen, specialize, callDepth-1);
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
      setSelfParam(RValue(gen, proj, dynamicMemberRef), dynamicMemberRef);
    } else {
      assert(existential.getType().is<MetaTypeType>() &&
             "non-dynamic-lookup-metatype for static method?!");
      val = existential.getValue();
      ManagedValue proj(val, existential.getCleanup());
      setSelfParam(RValue(gen, existential, dynamicMemberRef),
                   dynamicMemberRef);
    }

    // Determine the type of the method we referenced, by replacing the
    // class type of the 'Self' parameter with Builtin.ObjCPointer.
    SILDeclRef member(fd, SILDeclRef::ConstructAtNaturalUncurryLevel,
                      /*isObjC=*/true);

    auto methodTy = e->getType();

    setCallee(Callee::forDynamic(gen, val, member, methodTy, e));
    return true;
  }
};

} // end anonymous namespace

ManagedValue SILGenFunction::emitApply(SILLocation Loc,
                                       ManagedValue Fn,
                                       ArrayRef<Substitution> Subs,
                                       ArrayRef<ManagedValue> Args,
                                       CanType NativeResultTy,
                                       OwnershipConventions const &Ownership,
                                       bool Transparent,
                                       SGFContext C) {
  AbstractCC cc = Fn.getType().getAbstractCC();
  
  // Bridge and conditionally consume the cleanup on an input value.
  auto forwardIfConsumed
    = [&](ManagedValue v, SILType destTy, bool consumed) -> SILValue {
      return consumed
        ? v.forwardArgument(*this, Loc, cc, destTy.getSwiftType())
        : v.getArgumentValue(*this, Loc, cc, destTy.getSwiftType());
    };

  // Get the substituted function type.
  SILType calleeTy = Fn.getType();
  if (!Subs.empty()) {
    calleeTy = getLoweredLoadableType(calleeTy.castTo<PolymorphicFunctionType>()
                                    ->substGenericArgs(SGM.SwiftModule, Subs));
  }
  
  // Get the result type.
  Type resultTy = calleeTy.getFunctionResultType();
  const TypeLowering &resultTI = getTypeLowering(resultTy);
  SILType instructionTy = resultTI.getLoweredType();
  
  // Get the callee value.
  SILValue fnValue = Ownership.isCalleeConsumed()
    ? Fn.forward(*this)
    : Fn.getValue();

  SmallVector<SILValue, 4> argValues;

  // Prepare a buffer for an indirect return if needed.
  SILValue indirectReturn;
  if (resultTI.isAddressOnly()) {
    indirectReturn = getBufferForExprResult(Loc, resultTI.getLoweredType(), C);
    instructionTy = SGM.Types.getEmptyTupleType();
    argValues.push_back(indirectReturn);
  }
  
  ArrayRef<SILType> inputTypes
    = calleeTy.getFunctionTypeInfo(SGM.M)
        ->getInputTypesWithoutIndirectReturnType();
  
  // Gather the arguments.
  for (size_t i = 0, size = Args.size(); i < size; ++i) {
    SILValue argValue
      = forwardIfConsumed(Args[i],
                          inputTypes[i],
                          Ownership.isArgumentConsumed(i));
    argValues.push_back(argValue);
  }
  
  // Get the substituted type of the callee.
  SILType substCalleeTy = fnValue.getType();
  if (!Subs.empty()) {
    substCalleeTy = getLoweredLoadableType(
      substCalleeTy.castTo<PolymorphicFunctionType>()
        ->substGenericArgs(SGM.SwiftModule, Subs));
  }
  
  SILValue result = B.createApply(Loc, fnValue, substCalleeTy,
                                  instructionTy,
                                  Subs,
                                  argValues,
                                  Transparent);
  
  // Take ownership of the return value, if necessary.
  ManagedValue bridgedResult;
  if (indirectReturn) {
    /// FIXME: Can ObjC/C functions return types SIL considers address-only?
    /// Do we need to copy here if the return value is Unretained?
    assert(Ownership.getReturn() == OwnershipConventions::Return::Retained
           && "address-only result with non-Retained ownership not implemented");
    bridgedResult = manageBufferForExprResult(indirectReturn, resultTI, C);
    if (!bridgedResult) return ManagedValue();
  } else {
    switch (Ownership.getReturn()) {
    case OwnershipConventions::Return::Retained:
      // Already retained.
      break;

    case OwnershipConventions::Return::Autoreleased:
      // Autoreleased. Retain using retain_autoreleased.
      B.createStrongRetainAutoreleased(Loc, result);
      break;
    
    case OwnershipConventions::Return::Unretained:
      // Unretained. Retain the value.
      result = resultTI.emitCopyValue(B, Loc, result);
      break;
    }
  
    bridgedResult = resultTy->is<LValueType>()
      ? ManagedValue(result, ManagedValue::LValue)
      : emitManagedRValueWithCleanup(result, resultTI);
  }
  
  // Convert the result to a native value.
  return emitBridgedToNativeValue(Loc, bridgedResult, cc, NativeResultTy);
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
    CanType resultType;

  private:
    union {
      Expr *expr;
      RValue value;
    };
    
  public:
    CallSite(ApplyExpr *apply)
      : kind(Kind::Expr), loc(apply),
        resultType(apply->getType()->getCanonicalType()),
        expr(apply->getArg()) {
      assert(resultType);
    }
  
    CallSite(SILLocation loc, Expr *expr, Type resultType)
      : kind(Kind::Expr), loc(loc),
        resultType(resultType->getCanonicalType()),
        expr(expr) {
      assert(resultType);
    }
  
    CallSite(SILLocation loc, RValue &&value, Type resultType)
      : kind(Kind::Value), loc(loc),
        resultType(resultType->getCanonicalType()),
        value(std::move(value)) {
      assert(resultType);
    }
  
    ~CallSite() {
      switch (kind) {
      case Kind::Expr:
        return;
      case Kind::Value:
        value.~RValue();
        return;
      }
    }
    
    CallSite(CallSite &&o)
      : kind(o.kind), loc(o.loc), resultType(o.resultType)
    {
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
        gen.emitRValue(expr).getAll(args);
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
      Callee::SpecializedEmitter specializedEmitter
        = callee.getSpecializedEmitter(uncurryLevel, gen.SGM.M);

      ManagedValue mv;
      OwnershipConventions ownership;
      bool transparent;
      auto cc = AbstractCC::Freestanding;
      if (!specializedEmitter) {
        std::tie(mv, ownership, transparent)
          = callee.getAtUncurryLevel(gen, uncurryLevel);
        cc = mv.getType().getAbstractCC();
      }
      
      // Collect the arguments to the uncurried call.
      SmallVector<SmallVector<ManagedValue, 4>, 2> args;
      Optional<SILLocation> uncurriedLoc;
      CanType uncurriedResultTy;
      for (auto &site : uncurriedSites) {
        uncurriedLoc = site.loc;
        uncurriedResultTy = site.resultType;
        args.push_back({});
        std::move(site).emit(gen, args.back());
      }

      assert(uncurriedLoc);

      // Uncurry the arguments in calling convention order.
      SmallVector<ManagedValue, 4> uncurriedArgs;
      for (auto &argSet : reversed(args))
        uncurriedArgs.append(argSet.begin(), argSet.end());
      args = {};
      
      // We use the context emit-into initialization only for the outermost
      // call.
      SGFContext uncurriedContext = extraSites.empty() ? C : SGFContext();

      // Emit the uncurried call.
      ManagedValue result;

      if (specializedEmitter)
        result = specializedEmitter(gen,
                                    uncurriedLoc.getValue(),
                                    callee.getSubstitutions(),
                                    uncurriedArgs,
                                    uncurriedContext);
      else
        result = gen.emitApply(uncurriedLoc.getValue(), mv,
                               callee.getSubstitutions(),
                               uncurriedArgs,
                               uncurriedResultTy, ownership, transparent,
                               uncurriedContext);
      
      // End the initial writeback scope, if any.
      initialWritebackScope.reset();
      
      // If there are remaining call sites, apply them to the result function.
      // Each chained call gets its own writeback scope.
      for (unsigned i = 0, size = extraSites.size(); i < size; ++i) {
        WritebackScope scope(gen);
        uncurriedArgs.clear();
        SILLocation loc = extraSites[i].loc;
        std::move(extraSites[i]).emit(gen, uncurriedArgs);
        SGFContext context = i == size - 1 ? C : SGFContext();
        result = gen.emitApply(loc, result, {},
                               uncurriedArgs,
                               extraSites[i].resultType,
                               ownership, false, context);
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
    SILType assignType = gen.getLoweredType(substitutions[0].Replacement);
    
    // Convert the destination pointer argument to a SIL address.
    SILValue addr = gen.B.createPointerToAddress(loc,
                                                 args.back().getUnmanagedValue(),
                                                 assignType.getAddressType());
    
    // Build the value to be assigned, reconstructing tuples if needed.
    ManagedValue src = RValue(args.slice(0, args.size() - 1),
                              assignType.getSwiftRValueType())
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
    if (!substitutions[0].Replacement->mayHaveSuperclass()) {
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

  Callee::SpecializedEmitter
  Callee::getSpecializedEmitterForSILBuiltin(SILDeclRef function,
                                             SILModule &SILM) {
    // Filter out non-function members and non-builtin modules.

    if (function.kind != SILDeclRef::Kind::Func)
      return nullptr;
    if (!function.hasDecl())
      return nullptr;
    
    ValueDecl *decl = function.getDecl();
    
    if (!isa<BuiltinModule>(decl->getDeclContext()))
      return nullptr;

    const BuiltinInfo &Builtin = SILM.getBuiltinInfo(cast<FuncDecl>(decl));

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
  apply.visit(e);
  
  // Evaluate and discard the side effect if present.
  if (apply.sideEffect)
    gen.emitRValue(apply.sideEffect);
  
  // Build the call.
  // Pass the writeback scope on to CallEmission so it can thread scopes through
  // nested calls.
  CallEmission emission(gen, apply.getCallee(), std::move(writebacks));
  
  // Apply 'self' if provided.
  if (apply.selfParam)
    emission.addCallSite(RegularLocation(e), std::move(apply.selfParam),
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
  return (result ? RValue(*this, result, e) : RValue());
}

ManagedValue
SILGenFunction::emitApplyOfLibraryIntrinsic(SILLocation loc,
                                            FuncDecl *fn,
                                            ArrayRef<Substitution> subs,
                                            ArrayRef<ManagedValue> args,
                                            CanType resultType,
                                            SGFContext ctx) {
  Callee callee = Callee::forDirect(*this, SILDeclRef(fn), loc);

  ManagedValue mv;
  OwnershipConventions ownership;
  bool transparent;
  std::tie(mv, ownership, transparent) = callee.getAtUncurryLevel(*this, 0);

  return emitApply(loc, mv, subs, args, resultType, ownership, transparent, ctx);
}

/// emitArrayInjectionCall - Form an array "Slice" out of an ObjectPointer
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
  
  CanType injectionArgsTy = injectionFnTy->getInput()->getCanonicalType();
  RValue InjectionArgs(injectionArgsTy);
  InjectionArgs.addElement(RValue(*this,
                                ManagedValue(BasePtr, ManagedValue::Unmanaged),
                                Loc));
  InjectionArgs.addElement(RValue(*this, ObjectPtr, Loc));
  InjectionArgs.addElement(RValue(*this,
                                ManagedValue(Length, ManagedValue::Unmanaged),
                                Loc));
  
  emission.addCallSite(Loc, std::move(InjectionArgs),
                       injectionFnTy->getResult());
  return emission.apply();
}

static Callee getBaseAccessorFunctionRef(SILGenFunction &gen,
                                         SILLocation loc,
                                         SILDeclRef constant,
                                         RValue &selfValue) {
  ValueDecl *decl = constant.getDecl();

  // FIXME: Have a nicely-abstracted way to figure out which kind of
  // dispatch we're doing.
  // FIXME: We should do this for any declaration within a class. However,
  // IRGen doesn't yet have the machinery for handling class_method on
  // getters and setters.
  if (decl->isObjC()) {
    return Callee::forClassMethod(gen, selfValue.peekScalarValue(), constant,
                                  loc);
  }

  return Callee::forDirect(gen, constant, loc);
}

static Callee 
emitSpecializedAccessorFunctionRef(SILGenFunction &gen,
                                   SILLocation loc,
                                   SILDeclRef constant,
                                   ArrayRef<Substitution> substitutions,
                                   RValue &selfValue,
                                   Type substAccessorType)
{
  // If the accessor is a local constant, use it.
  // FIXME: Can local properties ever be generic?
  if (gen.LocalConstants.count(constant)) {
    SILValue v = gen.LocalConstants[constant];
    return Callee::forIndirect(gen.emitManagedRetain(loc, v), false, loc);
  }
  
  // Get the accessor function. The type will be a polymorphic function if
  // the Self type is generic.
  // FIXME: Dynamic dispatch for archetype/existential methods.
  Callee callee = getBaseAccessorFunctionRef(gen, loc, constant, selfValue);
  
  // If there are substitutions, specialize the generic accessor.
  // FIXME: Generic subscript operator could add another layer of
  // substitutions.
  if (!substitutions.empty()) {
    substAccessorType = getThinFunctionType(substAccessorType,
                                            gen.SGM.getConstantCC(constant));

    callee.addSubstitutions(gen, loc, substitutions,
                            substAccessorType->getCanonicalType(), 0);
  }
  return callee;
}

/// Emit a call to a getter.
ManagedValue SILGenFunction::emitGetAccessor(SILLocation loc,
                                             SILDeclRef get,
                                             ArrayRef<Substitution> substitutions,
                                             RValue &&selfValue,
                                             RValue &&subscripts,
                                             Type resultType,
                                             SGFContext c) {
  // Derive the specialized type of the accessor.
  auto &tc = SGM.Types;
  Type accessType;
  if (subscripts)
    accessType = tc.getSubscriptAccessorType(SILDeclRef::Kind::Getter,
                                             subscripts.getType(),
                                             resultType);
  else
    accessType = tc.getAccessorType(SILDeclRef::Kind::Getter, resultType);
  if (selfValue)
    accessType = tc.getMethodTypeInContext(selfValue.getType()->getRValueType(),
                                           accessType);

  Callee getter = emitSpecializedAccessorFunctionRef(*this, loc, get,
                                                     substitutions, selfValue,
                                                     accessType);
  
  CallEmission emission(*this, std::move(getter));
  auto *accessFnTy = accessType->castTo<AnyFunctionType>();
  // Self ->
  if (selfValue) {
    emission.addCallSite(loc, std::move(selfValue), accessFnTy->getResult());
    accessFnTy = accessFnTy->getResult()->castTo<AnyFunctionType>();
  }
  // Index ->
  if (subscripts) {
    emission.addCallSite(loc, std::move(subscripts), accessFnTy->getResult());
    accessFnTy = accessFnTy->getResult()->castTo<AnyFunctionType>();
  }
  // () ->
  emission.addCallSite(loc, emitEmptyTupleRValue(loc), accessFnTy->getResult());
  // T
  return emission.apply(c);
}

void SILGenFunction::emitSetAccessor(SILLocation loc,
                                     SILDeclRef set,
                                     ArrayRef<Substitution> substitutions,
                                     RValue &&selfValue,
                                     RValue &&subscripts,
                                     RValue &&setValue) {
  // Derive the specialized type of the accessor.
  auto &tc = SGM.Types;
  Type accessType;
  if (subscripts)
    accessType = tc.getSubscriptAccessorType(SILDeclRef::Kind::Setter,
                                             subscripts.getType(),
                                             setValue.getType());
  else
    accessType = tc.getAccessorType(SILDeclRef::Kind::Setter,
                                    setValue.getType());
  if (selfValue)
    accessType = tc.getMethodTypeInContext(selfValue.getType()->getRValueType(),
                                           accessType);

  Callee setter = emitSpecializedAccessorFunctionRef(*this, loc, set,
                                                     substitutions, selfValue,
                                                     accessType);

  CallEmission emission(*this, std::move(setter));
  auto *accessFnTy = accessType->castTo<AnyFunctionType>();
  // Self ->
  if (selfValue) {
    emission.addCallSite(loc, std::move(selfValue), accessFnTy->getResult());
    accessFnTy = accessFnTy->getResult()->castTo<AnyFunctionType>();
  }
  // Index ->
  if (subscripts) {
    emission.addCallSite(loc, std::move(subscripts), accessFnTy->getResult());
    accessFnTy = accessFnTy->getResult()->castTo<AnyFunctionType>();
  }
  // T ->
  emission.addCallSite(loc, std::move(setValue), accessFnTy->getResult());
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
  SILValue optMethodArg = new (F.getModule()) SILArgument(loweredOptTy, contBB);

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
    auto valueTy =e->getType()->castTo<BoundGenericType>()->getGenericArgs()[0];
    auto methodTy = valueTy;

    // For a computed variable, we want the getter.
    if (member.isAccessor())
      methodTy = FunctionType::get(TupleType::getEmpty(getASTContext()),
                                   methodTy, getASTContext());

    auto dynamicMethodTy = getDynamicMethodType(*this, operand, member,
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
    RValue resultRV = RValue(*this, emitManagedRValueWithCleanup(result), e);
    SILValue optResult =
      emitInjectOptionalValue(e, std::move(resultRV), optTL).forward(*this);

    // Branch to the continuation block.
    B.createBranch(e, contBB, optResult);
  }

  // Create the no-member branch.
  {
    B.emitBlock(noMemberBB);

    // Branch to the continuation block.
    B.createBranch(e, contBB,
                   emitInjectOptionalNothing(e, optTL).forward(*this));
  }

  // Emit the continuation block.
  B.emitBlock(contBB);

  // Package up the result.
  return RValue(*this, emitManagedRValueWithCleanup(optMethodArg), e);
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
  SILValue optMethodArg = new (F.getModule()) SILArgument(loweredOptTy, contBB);

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
    auto valueTy =e->getType()->castTo<BoundGenericType>()->getGenericArgs()[0];
    auto subscript = cast<SubscriptDecl>(e->getMember().getDecl());
    auto methodTy = subscript->getGetter()->getType()
                      ->castTo<AnyFunctionType>()->getResult();
    auto dynamicMethodTy = getDynamicMethodType(*this, base, member,
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
      RValue(*this, emitManagedRValueWithCleanup(result, valueTL), e);
    SILValue optResult =
      emitInjectOptionalValue(e, std::move(resultRV), optTL).forward(*this);

    // Branch to the continuation block.
    B.createBranch(e, contBB, optResult);
  }

  // Create the no-member branch.
  {
    B.emitBlock(noMemberBB);

    // Branch to the continuation block.
    B.createBranch(e, contBB, emitInjectOptionalNothing(e, optTL).forward(*this));
  }

  // Emit the continuation block.
  B.emitBlock(contBB);

  // Package up the result.
  return RValue(*this, emitManagedRValueWithCleanup(optMethodArg), e);
}
