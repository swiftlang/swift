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
#include "SpecializedEmitter.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Module.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/PrettyStackTrace.h"

using namespace swift;
using namespace Lowering;

/// Get the method dispatch mechanism for a method.
MethodDispatch
SILGenFunction::getMethodDispatch(AbstractFunctionDecl *method) {
  // Final methods can be statically referenced.
  if (method->isFinal())
    return MethodDispatch::Static;
  
  // If this declaration is in a class but not marked final, then it is
  // always dynamically dispatched.
  auto dc = method->getDeclContext();
  if (isa<ClassDecl>(dc))
    return MethodDispatch::Class;

  // Class extension methods are only dynamically dispatched if they're
  // dispatched by objc_msgSend, which happens if they're foreign or dynamic.
  if (auto declaredType = dc->getDeclaredTypeInContext())
    if (declaredType->getClassOrBoundGenericClass()) {
      if (method->hasClangNode())
        return MethodDispatch::Class;
      if (auto fd = dyn_cast<FuncDecl>(method)) {
        if (fd->isAccessor() && fd->getAccessorStorageDecl()->hasClangNode())
          return MethodDispatch::Class;
      }
      if (method->getAttrs().hasAttribute<DynamicAttr>())
        return MethodDispatch::Class;
    }
  
  // Otherwise, it can be referenced statically.
  return MethodDispatch::Static;
}

/// Retrieve the type to use for a method found via dynamic lookup.
static CanAnyFunctionType getDynamicMethodFormalType(SILGenModule &SGM,
                                                     SILValue proto,
                                                     ValueDecl *member,
                                                     SILDeclRef methodName,
                                                     Type memberType) {
  auto &ctx = SGM.getASTContext();
  CanType selfTy;
  if (member->isInstanceMember()) {
    selfTy = ctx.TheUnknownObjectType;
  } else {
    selfTy = proto.getType().getSwiftType();
  }
  auto extInfo = FunctionType::ExtInfo()
                   .withCallingConv(SGM.getConstantCC(methodName))
                   .withRepresentation(FunctionType::Representation::Thin);

  return CanFunctionType::get(selfTy, memberType->getCanonicalType(),
                              extInfo);
}

/// Replace the 'self' parameter in the given type.
static CanSILFunctionType
replaceSelfTypeForDynamicLookup(ASTContext &ctx,
                                CanSILFunctionType fnType,
                                CanType newSelfType,
                                SILDeclRef methodName) {
  auto oldParams = fnType->getParameters();
  SmallVector<SILParameterInfo, 4> newParams;
  newParams.append(oldParams.begin(), oldParams.end() - 1);
  newParams.push_back({newSelfType, oldParams.back().getConvention()});
  
  auto newResult = fnType->getResult();
  // If the method returns Self, substitute AnyObject for the result type.
  if (auto fnDecl = dyn_cast<FuncDecl>(methodName.getDecl())) {
    if (fnDecl->hasDynamicSelf()) {
      auto anyObjectTy = ctx.getProtocol(KnownProtocolKind::AnyObject)
                                  ->getDeclaredType();
      auto newResultTy
        = newResult.getType()->replaceCovariantResultType(anyObjectTy, 0);
      newResult = SILResultInfo(newResultTy->getCanonicalType(),
                                newResult.getConvention());
    }
  }
  
  return SILFunctionType::get(nullptr,
                              fnType->getExtInfo(),
                              fnType->getCalleeConvention(),
                              newParams,
                              newResult,
                              ctx);
}

static Type getExistentialArchetype(SILValue existential) {
  CanType ty = existential.getType().getSwiftRValueType();
  if (ty->is<ArchetypeType>())
    return ty;
  return cast<ProtocolType>(ty)->getDecl()->getSelf()->getArchetype();
}

/// Retrieve the type to use for a method found via dynamic lookup.
static CanSILFunctionType getDynamicMethodLoweredType(SILGenFunction &gen,
                                                      SILValue proto,
                                                      SILDeclRef methodName) {
  auto &ctx = gen.getASTContext();
  
  // Determine the opaque 'self' parameter type.
  CanType selfTy;
  if (methodName.getDecl()->isInstanceMember()) {
    selfTy = getExistentialArchetype(proto)->getCanonicalType();
  } else {
    selfTy = proto.getType().getSwiftType();
  }
  
  // Replace the 'self' parameter type in the method type with it.
  auto methodTy = gen.SGM.getConstantType(methodName).castTo<SILFunctionType>();
  return replaceSelfTypeForDynamicLookup(ctx, methodTy, selfTy, methodName);
}

/// Remap all of the 'Self'-related substitutions within the given set of
/// substitutions to an opened archetype.
///
/// TODO: This should be done by the type checker.
static void remapSelfSubstitutions(SILGenFunction &gen,
                                   ArrayRef<Substitution> &subs,
                                   Type newSelfArchetype) {
  MutableArrayRef<Substitution> replacementBuf;
  
  auto copySubstitutions = [&]{
    if (!replacementBuf.empty())
      return;
    replacementBuf = gen.getASTContext().Allocate<Substitution>(subs.size());
    memcpy(replacementBuf.data(), subs.data(),
           sizeof(Substitution) * subs.size());
    subs = replacementBuf;
  };
  
  unsigned i = 0, n = subs.size();
  for (; i != n; ++i) {
    auto archetype = subs[i].getArchetype();
    auto rootArchetype = archetype;
    while (rootArchetype->getParent())
      rootArchetype = rootArchetype->getParent();
    if (!rootArchetype->getSelfProtocol())
      continue;
    
    copySubstitutions();
    
    TypeSubstitutionMap map;
    map[rootArchetype] = newSelfArchetype;
    
    auto substType = Type(archetype)
      .subst(gen.SGM.SwiftModule, map, /*ignoreMissing*/false,
             nullptr);
    assert(substType && "could not substitute?!");
    
    replacementBuf[i] = Substitution(archetype,
                                          substType,
                                          subs[i].getConformances());
  }

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
      WitnessMethod = GenericMethod_First,
      /// A method call using dynamic lookup.
      DynamicMethod,
    GenericMethod_Last = DynamicMethod
  };

  const Kind kind;

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
  CanType OrigFormalOldType, OrigFormalInterfaceType;
  CanAnyFunctionType SubstFormalType;
  Optional<SILLocation> specializeLoc;
  bool isTransparent;
  bool HasSubstitutions = false;

  // The pointer back to the AST node that produced the callee.
  SILLocation Loc;


public:
  void setTransparent(bool T) { isTransparent = T; }
private:

  Callee(ManagedValue indirectValue,
         CanType origFormalType,
         CanAnyFunctionType substFormalType,
         bool isTransparent, SILLocation L)
    : kind(Kind::IndirectValue),
      indirectValue(indirectValue),
      OrigFormalOldType(origFormalType),
      OrigFormalInterfaceType(origFormalType),
      SubstFormalType(substFormalType),
      isTransparent(isTransparent),
      Loc(L)
  {}

  static CanAnyFunctionType getConstantFormalType(SILGenFunction &gen,
                                                  SILValue selfValue,
                                                  SILDeclRef fn)
  SIL_FUNCTION_TYPE_DEPRECATED {
    return gen.SGM.Types.getConstantInfo(fn.atUncurryLevel(0)).FormalType;
  }

  static CanAnyFunctionType getConstantFormalInterfaceType(SILGenFunction &gen,
                                                  SILValue selfValue,
                                                  SILDeclRef fn) {
    return gen.SGM.Types.getConstantInfo(fn.atUncurryLevel(0))
             .FormalInterfaceType;
  }

  Callee(SILGenFunction &gen, SILDeclRef standaloneFunction,
         CanAnyFunctionType substFormalType,
         SILLocation l)
    : kind(Kind::StandaloneFunction), standaloneFunction(standaloneFunction),
      OrigFormalOldType(getConstantFormalType(gen, SILValue(), 
                                              standaloneFunction)),
      OrigFormalInterfaceType(getConstantFormalInterfaceType(gen, SILValue(),
                                                           standaloneFunction)),
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
      OrigFormalOldType(getConstantFormalType(gen, selfValue, methodName)),
      OrigFormalInterfaceType(getConstantFormalInterfaceType(gen, selfValue, 
                                                             methodName)),
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
    assert(!isa<LValueType>(origParamType) && "Self can't be @lvalue");
    if (auto lv = dyn_cast<InOutType>(origParamType)) {
      selfType = buildSubstSelfType(lv.getObjectType(), selfType, ctx);
      return CanInOutType::get(selfType);
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
                                          unsigned uncurryLevel,
                                          Optional<SILDeclRef> constant) const {
    if (!HasSubstitutions) return origFnType;

    assert(origLoweredType);
    auto substLoweredType =
      SGM.Types.getLoweredASTFunctionType(SubstFormalType, uncurryLevel,
                                          origLoweredType->getExtInfo(),
                                          constant);
    auto substLoweredInterfaceType =
      SGM.Types.getLoweredASTFunctionType(SubstFormalType, uncurryLevel,
                                          origLoweredType->getExtInfo(),
                                          constant);
    return SGM.Types.substFunctionType(origFnType, origLoweredType,
                                       substLoweredType,
                                       substLoweredInterfaceType);
  }

  /// Return the value being passed as 'self' for this protocol callee.
  CanType getProtocolSelfType(SILGenModule &SGM) const {
    if (kind == Kind::WitnessMethod) {
      return method.selfValue.getType().getSwiftRValueType();
    } else {
      llvm_unreachable("bad callee kind for protocol method");
    }
  }

  /// Add the 'self' clause back to the substituted formal type of
  /// this protocol method.
  void addProtocolSelfToFormalType(SILGenModule &SGM, SILDeclRef name) {
    // The result types of the expressions yielding protocol values
    // (reflected in SubstFormalType) reflect an implicit level of
    // function application, including some extra polymorphic
    // substitution.
    HasSubstitutions = true;

    auto &ctx = SGM.getASTContext();

    // Add the 'self' parameter back.  We want it to look like a
    // substitution of the appropriate clause from the original type.
    auto polyFormalType = cast<PolymorphicFunctionType>(OrigFormalOldType);
    auto selfType = getProtocolSelfType(SGM);
    auto substSelfType =
      buildSubstSelfType(polyFormalType.getInput(), selfType, ctx);

    // Existential witnesses are always "thick" with the polymorphic info,
    // unless @objc.
    auto rep = name.isForeign
      ? FunctionType::Representation::Thin
      : FunctionType::Representation::Thick;
    auto extInfo = FunctionType::ExtInfo(AbstractCC::Method, rep,
                                         /*noreturn*/ false);

    SubstFormalType = CanFunctionType::get(substSelfType, SubstFormalType,
                                           extInfo);
  }

  SILType getProtocolClosureType(SILGenModule &SGM,
                                 SILDeclRef constant,
                                 SILConstantInfo &constantInfo,
                                 CanArchetypeType &archetype) const {
    auto &C = SGM.getASTContext();
    
    CanType selfType = getProtocolSelfType(SGM);

    // Find the archetype.
    archetype = getArchetypeForSelf(selfType);

    // addProtocolSelfToFormalType initializes SubstFormalType->isThin()
    // appropriately for the archetype kind.
    FunctionType::Representation rep = SubstFormalType->getRepresentation();

    // We may need to make the OrigLoweredType thick so that
    // computing the substFnType below preserves this information.
    if (rep != FunctionType::Representation::Thin) {
      constantInfo.LoweredType =
        adjustFunctionType(constantInfo.LoweredType, rep);
      constantInfo.LoweredInterfaceType =
        adjustFunctionType(constantInfo.LoweredInterfaceType, rep);
    }

    // The expected result of witness_method is a partial application
    // of the archetype method to the expected archetype.
    CanAnyFunctionType partialSubstFormalType =
      cast<FunctionType>(
        cast<PolymorphicFunctionType>(OrigFormalOldType)
          ->substGenericArgs(SGM.SwiftModule, archetype)
            ->getCanonicalType());
    
    // If the requirement is generic, sever its connection to the substituted
    // outer parameter list.
    if (auto polyMethod = dyn_cast<PolymorphicFunctionType>(
                                          partialSubstFormalType.getResult())) {
      auto origParams = &polyMethod->getGenericParams();
      
      auto emptyOuterParams = GenericParamList::getEmpty(C);
      auto params = origParams->cloneWithOuterParameters(C, emptyOuterParams);
      
      polyMethod = CanPolymorphicFunctionType::get(polyMethod.getInput(),
                                                   polyMethod.getResult(),
                                                   params,
                                                   polyMethod->getExtInfo());
      partialSubstFormalType
        = CanFunctionType::get(partialSubstFormalType.getInput(), polyMethod);
    }

    // Same thing for the interface type.

    auto partialSubstInterfaceType =
      cast<AnyFunctionType>(
        cast<GenericFunctionType>(OrigFormalInterfaceType)
          ->partialSubstGenericArgs(SGM.SwiftModule, archetype)
            ->getCanonicalType());
    
    auto partialSubstUncurriedType =
      SGM.Types.getConstantFunctionType(constant, partialSubstFormalType,
                                        partialSubstInterfaceType,
                                        rep);
    return SILType::getPrimitiveObjectType(partialSubstUncurriedType);
  }

  /// Add the 'self' type to the substituted function type of this
  /// dynamic callee.
  void addDynamicCalleeSelfToFormalType(SILGenModule &SGM) {
    assert(kind == Kind::DynamicMethod);

    // Drop the original self clause.
    CanType methodType = OrigFormalOldType;
    methodType = cast<AnyFunctionType>(methodType).getResult();

    // Replace it with the dynamic self type.
    OrigFormalOldType = OrigFormalInterfaceType
      = getDynamicMethodFormalType(SGM, method.selfValue,
                                   method.methodName.getDecl(),
                                   method.methodName, methodType);

    // Add a self clause to the substituted type.
    auto origFormalType = cast<AnyFunctionType>(OrigFormalOldType);
    auto selfType = origFormalType.getInput();
    SubstFormalType
      = CanFunctionType::get(selfType, SubstFormalType,
                             origFormalType->getExtInfo());
  }

public:

  static Callee forIndirect(ManagedValue indirectValue,
                            CanType origFormalType,
                            CanAnyFunctionType substFormalType,
                            bool isTransparent,
                            SILLocation l) {
    return Callee(indirectValue,
                  origFormalType,
                  substFormalType,
                  isTransparent, l);
  }
  static Callee forDirect(SILGenFunction &gen, SILDeclRef c,
                          CanAnyFunctionType substFormalType,
                          SILLocation l) {
    return Callee(gen, c, substFormalType, l);
  }
  static Callee forClassMethod(SILGenFunction &gen, SILValue selfValue,
                               SILDeclRef name,
                               CanAnyFunctionType substFormalType,
                               SILLocation l) {
    return Callee(Kind::ClassMethod, gen, selfValue, name,
                  substFormalType, l);
  }
  static Callee forSuperMethod(SILGenFunction &gen, SILValue selfValue,
                               SILDeclRef name,
                               CanAnyFunctionType substFormalType,
                               SILLocation l) {
    return Callee(Kind::SuperMethod, gen, selfValue, name,
                  substFormalType, l);
  }
  static Callee forArchetype(SILGenFunction &gen, SILValue value,
                             SILDeclRef name,
                             CanAnyFunctionType substFormalType,
                             SILLocation l) {
    Callee callee(Kind::WitnessMethod, gen, value, name,
                  substFormalType, l);
    callee.addProtocolSelfToFormalType(gen.SGM, name);
    return callee;
  }
  static Callee forDynamic(SILGenFunction &gen, SILValue proto,
                           SILDeclRef name, CanAnyFunctionType substFormalType,
                           SILLocation l) {
    Callee callee(Kind::DynamicMethod, gen, proto, name,
                  substFormalType, l);
    callee.addDynamicCalleeSelfToFormalType(gen.SGM);
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
    return OrigFormalOldType;
  }

  CanAnyFunctionType getSubstFormalType() const {
    return SubstFormalType;
  }

  unsigned getNaturalUncurryLevel() const {
    switch (kind) {
    case Kind::IndirectValue:
      return 0;
        
    case Kind::StandaloneFunction:
      return standaloneFunction.uncurryLevel;

    case Kind::ClassMethod:
    case Kind::SuperMethod:
    case Kind::WitnessMethod:
    case Kind::DynamicMethod:
      return method.methodName.uncurryLevel;
    }
  }
  
  std::tuple<ManagedValue, CanSILFunctionType, bool>
  getAtUncurryLevel(SILGenFunction &gen, unsigned level) const {
    ManagedValue mv;
    bool transparent = isTransparent;
    SILConstantInfo constantInfo;
    Optional<SILDeclRef> constant = None;

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
      constant = standaloneFunction.atUncurryLevel(level);
      constantInfo = gen.getConstantInfo(*constant);
      SILValue ref = gen.emitGlobalFunctionRef(Loc, *constant, constantInfo);
      mv = ManagedValue::forUnmanaged(ref);
      break;
    }
    case Kind::ClassMethod: {
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");
      constant = method.methodName.atUncurryLevel(level);
      constantInfo = gen.getConstantInfo(*constant);
      
      // If the call is curried, emit a direct call to the curry thunk.
      if (level < method.methodName.uncurryLevel) {
        SILValue ref = gen.emitGlobalFunctionRef(Loc, *constant, constantInfo);
        mv = ManagedValue::forUnmanaged(ref);
        transparent = false;
        break;
      }
      
      // Otherwise, do the dynamic dispatch inline.
      SILValue methodVal = gen.B.createClassMethod(Loc,
                                                   method.selfValue,
                                                   *constant,
                                                   constantInfo.getSILType(),
                                                   /*volatile*/
                                                     constant->isForeign);
      
      mv = ManagedValue::forUnmanaged(methodVal);
      break;
    }
    case Kind::SuperMethod: {
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");
      assert(level >= 1
             && "currying 'self' of super method dispatch not yet supported");

      constant = method.methodName.atUncurryLevel(level);
      constantInfo = gen.getConstantInfo(*constant);
      SILValue methodVal = gen.B.createSuperMethod(Loc,
                                                   method.selfValue,
                                                   *constant,
                                                   constantInfo.getSILType(),
                                                   /*volatile*/
                                                     constant->isForeign);
      
      mv = ManagedValue::forUnmanaged(methodVal);
      break;
    }
    case Kind::WitnessMethod: {
      assert(level <= method.methodName.uncurryLevel
             && "uncurrying past natural uncurry level of method");
      constant = method.methodName.atUncurryLevel(level);
      constantInfo = gen.getConstantInfo(*constant);

      // If the call is curried, emit a direct call to the curry thunk.
      if (level < method.methodName.uncurryLevel) {
        SILValue ref = gen.emitGlobalFunctionRef(Loc, *constant, constantInfo);
        mv = ManagedValue::forUnmanaged(ref);
        transparent = false;
        break;
      }

      // Look up the witness for the archetype.
      auto selfType = getProtocolSelfType(gen.SGM);
      auto archetype = getArchetypeForSelf(selfType);
      // Get the openend existential value if the archetype is an opened
      // existential type.
      SILValue OpenedExistential;
      if (!archetype->getOpenedExistentialType().isNull())
        OpenedExistential = method.selfValue;

      SILValue fn = gen.B.createWitnessMethod(Loc,
                                  archetype,
                                  /*conformance*/ nullptr,
                                  *constant,
                                  constantInfo.getSILType(),
                                  OpenedExistential,
                                  constant->isForeign);
      mv = ManagedValue::forUnmanaged(fn);
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
        replaceSelfTypeForDynamicLookup(gen.getASTContext(),
                                constantInfo.SILFnType,
                                method.selfValue.getType().getSwiftRValueType(),
                                method.methodName);
      
      SILValue fn = gen.B.createDynamicMethod(Loc,
                          method.selfValue,
                          constant,
                          SILType::getPrimitiveObjectType(closureType),
                          /*volatile*/ constant.isForeign);
      mv = ManagedValue::forUnmanaged(fn);
      break;
    }
    }

    CanSILFunctionType substFnType =
      getSubstFunctionType(gen.SGM, mv.getType().castTo<SILFunctionType>(),
                           constantInfo.LoweredType, level, constant);
    
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
      return None;
    
    switch (kind) {
    case Kind::StandaloneFunction: {
      return SpecializedEmitter::forDecl(SGM, standaloneFunction);
    }
    case Kind::IndirectValue:
    case Kind::ClassMethod:
    case Kind::SuperMethod:
    case Kind::WitnessMethod:
    case Kind::DynamicMethod:
      return None;
    }
    llvm_unreachable("bad callee kind");
  }
};
  
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

  /// Get the type of the function for substitution purposes.
  ///
  /// \param otherCtorRefUsesAllocating If true, the OtherConstructorDeclRef
  /// refers to the initializing 
  CanFunctionType getSubstFnType(bool otherCtorRefUsesAllocating = false) {
    // TODO: optimize this if there are no specializes in play
    auto getSiteType = [&](ApplyExpr *site, bool otherCtorRefUsesAllocating) {
      if (otherCtorRefUsesAllocating) {
        // We have a reference to an initializing constructor, but we will
        // actually be using the allocating constructor. Update the type
        // appropriately.
        // FIXME: Re-derive the type from the declaration + substitutions?
        auto ctorRef = cast<OtherConstructorDeclRefExpr>(
                         site->getFn()->getSemanticsProvidingExpr());
        auto fnType = ctorRef->getType()->castTo<FunctionType>();
        auto selfTy = MetatypeType::get(
                        fnType->getInput()->getInOutObjectType());
        return CanFunctionType::get(selfTy->getCanonicalType(),
                                    fnType->getResult()->getCanonicalType(),
                                    fnType->getExtInfo());
      }

      return cast<FunctionType>(site->getFn()->getType()->getCanonicalType());
    };

    CanFunctionType fnType;

    auto addSite = [&](ApplyExpr *site, bool otherCtorRefUsesAllocating) {
      auto siteType = getSiteType(site, otherCtorRefUsesAllocating);

      // If this is the first call site, use its formal type directly.
      if (!fnType) {
        fnType = siteType;
        return;
      }

      fnType = CanFunctionType::get(siteType.getInput(), fnType,
                                    siteType->getExtInfo());
    };

    for (auto callSite : callSites) {
      addSite(callSite, false);
    }
    // The self application might be a MemberRefExpr if "self" is an archetype.
    if (auto selfApply = dyn_cast_or_null<ApplyExpr>(SelfApplyExpr)) {
      addSite(selfApply, otherCtorRefUsesAllocating);
    }

    assert(fnType && "found no call sites?");
    return fnType;
  }

  /// Fall back to an unknown, indirect callee.
  void visitExpr(Expr *e) {
    // This marks all calls to auto closure typed variables as transparent.
    // As of writing, local variable auto closures are currently allowed by
    // the parser, but the plan is that they will be disallowed, so this will
    // actually only apply to auto closure parameters, which is what we want
    auto *t = e->getType()->castTo<AnyFunctionType>();
    bool isTransparent = t->getExtInfo().isAutoClosure();

    ManagedValue fn = gen.emitRValueAsSingleValue(e);
    auto origType = cast<AnyFunctionType>(e->getType()->getCanonicalType());
    setCallee(Callee::forIndirect(fn, origType, getSubstFnType(), isTransparent,
                                  e));
  }

  void visitLoadExpr(LoadExpr *e) {
    bool isTransparent = false;
    if (DeclRefExpr *d = dyn_cast<DeclRefExpr>(e->getSubExpr())) {
      // This marks all calls to auto closure typed variables as transparent.
      // As of writing, local variable auto closures are currently allowed by
      // the parser, but the plan is that they will be disallowed, so this will
      // actually only apply to auto closure parameters, which is what we want
      Type Ty = d->getDecl()->getType()->getInOutObjectType();

      // If the decl type is a function type, figure out if it is an auto
      // closure.
      if (auto *AnyF = Ty->getAs<AnyFunctionType>()) {
        isTransparent = AnyF->getExtInfo().isAutoClosure();
      }
    }
    // TODO: preserve the function pointer at its original abstraction level
    ManagedValue fn = gen.emitRValueAsSingleValue(e);
    auto origType = cast<AnyFunctionType>(e->getType()->getCanonicalType());
    setCallee(Callee::forIndirect(fn, origType, getSubstFnType(),
                                  isTransparent, e));
  }
  
  /// Add a call site to the curry.
  void visitApplyExpr(ApplyExpr *e) {
    if (e->isSuper()) {
      applySuper(e);
    } else if (applyInitDelegation(e)) {
      // Already done
    } else {
      callSites.push_back(e);
      visit(e->getFn());
    }
    ++callDepth;
  }

  /// Given a metatype value for the type, allocate an Objective-C
  /// object (with alloc_ref_dynamic) of that type.
  ///
  /// \returns the self object.
  ManagedValue allocateObjCObject(ManagedValue selfMeta, SILLocation loc) {
    auto metaType = selfMeta.getType().castTo<AnyMetatypeType>();
    CanType type = metaType.getInstanceType();

    // Convert to an Objective-C metatype representation, if needed.
    ManagedValue selfMetaObjC;
    if (metaType->getRepresentation() == MetatypeRepresentation::ObjC) {
      selfMetaObjC = selfMeta;
    } else {
      CanAnyMetatypeType objcMetaType;
      if (isa<MetatypeType>(metaType)) {
        objcMetaType = CanMetatypeType::get(type, MetatypeRepresentation::ObjC);
      } else {
        objcMetaType = CanExistentialMetatypeType::get(type,
                                                  MetatypeRepresentation::ObjC);
      }
      selfMetaObjC = ManagedValue(
                       gen.B.emitThickToObjCMetatype(
                         loc, selfMeta.getValue(),
                         gen.SGM.getLoweredType(objcMetaType)),
                       selfMeta.getCleanup());
    }

    // Allocate the object.
    return ManagedValue(gen.B.createAllocRefDynamic(
                          loc,
                          selfMetaObjC.getValue(),
                          gen.SGM.getLoweredType(type),
                          /*objc=*/true),
                          selfMetaObjC.getCleanup());
  }

  //
  // Known callees.
  //
  void visitDeclRefExpr(DeclRefExpr *e) {
    // If we need to perform dynamic dispatch for the given function,
    // emit class_method to do so.
    if (auto afd = dyn_cast<AbstractFunctionDecl>(e->getDecl())) {
      Optional<SILDeclRef::Kind> kind;
      bool isDynamicallyDispatched;
      bool requiresAllocRefDynamic = false;
      
      // Determine whether the method is dynamically dispatched.
      if (e->getAccessSemantics() != AccessSemantics::Ordinary) {
        isDynamicallyDispatched = false;
      } else {
        switch (gen.getMethodDispatch(afd)) {
        case MethodDispatch::Class:
          isDynamicallyDispatched = true;
          break;
        case MethodDispatch::Static:
          isDynamicallyDispatched = false;
          break;
        }
      }

      if (isa<FuncDecl>(afd) && isDynamicallyDispatched) {
        kind = SILDeclRef::Kind::Func;
      } else if (auto ctor = dyn_cast<ConstructorDecl>(afd)) {
        ApplyExpr *thisCallSite = callSites.back();
        // Required constructors are dynamically dispatched when the 'self'
        // value is not statically derived.
        if (ctor->isRequired() &&
            thisCallSite->getArg()->getType()->is<AnyMetatypeType>() &&
            !thisCallSite->getArg()->isStaticallyDerivedMetatype()) {
          if (gen.SGM.requiresObjCDispatch(afd)) {
            // When we're performing Objective-C dispatch, we don't have an
            // allocating constructor to call. So, perform an alloc_ref_dynamic
            // and pass that along to the initializer.
            requiresAllocRefDynamic = true;
            kind = SILDeclRef::Kind::Initializer;
          } else {
            kind = SILDeclRef::Kind::Allocator;
          }
        } else {
          isDynamicallyDispatched = false;
        }
      }

      if (isDynamicallyDispatched) {
        ApplyExpr *thisCallSite = callSites.back();
        callSites.pop_back();
        RValue self = gen.emitRValue(thisCallSite->getArg());

        // If we require a dynamic allocation of the object here, do so now.
        if (requiresAllocRefDynamic) {
          SILLocation loc = thisCallSite->getArg();
          auto selfValue = allocateObjCObject(
                             std::move(self).getAsSingleValue(gen, loc),
                             loc);
          self = RValue(gen, loc, selfValue.getType().getSwiftRValueType(),
                        selfValue);
        }

        setSelfParam(std::move(self), thisCallSite);
        SILDeclRef constant(afd, kind.getValue(),
                            SILDeclRef::ConstructAtBestResilienceExpansion,
                            SILDeclRef::ConstructAtNaturalUncurryLevel,
                            gen.SGM.requiresObjCDispatch(afd));
        
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

    // If this is a direct reference to a vardecl, it must be a let constant
    // (which doesn't need to be loaded).  Just emit its value directly.
    if (auto *vd = dyn_cast<VarDecl>(e->getDecl())) {
      (void)vd;
      assert(vd->isLet() && "Direct reference to vardecl that isn't a let?");
      visitExpr(e);
      return;
    }
    
    // FIXME: Store context values for local funcs in a way that we can
    // apply them directly as an added "call site" here.
    SILDeclRef constant(e->getDecl(),
                        SILDeclRef::ConstructAtBestResilienceExpansion,
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         gen.SGM.requiresObjCDispatch(e->getDecl()));

    // Obtain a reference for a local closure.
    if (gen.LocalFunctions.count(constant)) {
      ManagedValue localFn =
        gen.emitRValueForDecl(e, e->getDeclRef(), e->getType(),
                              AccessSemantics::Ordinary);
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
      auto archetype = subs[innerIdx].getArchetype();
      while (archetype->getParent())
        archetype = archetype->getParent();
      if (!archetype->getSelfProtocol()) {
        break;
      }
    }

    return subs.slice(innerIdx);
  }
  
  void visitMemberRefExpr(MemberRefExpr *e) {
    auto *fd = dyn_cast<AbstractFunctionDecl>(e->getMember().getDecl());

    // If the base is a non-protocol, non-archetype type, then this is a load of
    // a function pointer out of a vardecl.  Just emit it as an rvalue.
    if (!fd)
      return visitExpr(e);

    // We have four cases to deal with here:
    //
    //  1) for a "static" / "type" method, the base is a metatype.
    //  2) for a classbound protocol, the base is a class-bound protocol rvalue,
    //     which is loadable.
    //  3) for a mutating method, the base has inout type.
    //  4) for a nonmutating method, the base is a general protocol/archetype
    //     rvalue, which is address-only.  The base is passed at +0, so it isn't
    //     consumed.
    //
    // In the last case, the AST has this call typed as being applied to an
    // rvalue, but the witness is actually expecting a pointer to the +0 value
    // in memory.  We access this with the open_existential instruction, or
    // just pass in the address since archetypes are address-only.
    //
    // FIXME: This is hideous. We should be letting emitRawApply figure this
    // out.
    SGFContext allowPlusZero = SGFContext::AllowPlusZero;
    auto *proto = cast<ProtocolDecl>(fd->getDeclContext());
    if (proto->requiresClass())
      allowPlusZero = SGFContext();
    
    auto baseVal = gen.emitRValueAsSingleValue(e->getBase(), allowPlusZero);

    auto baseTy = e->getBase()->getType()->getLValueOrInOutObjectType();

    ArrayRef<Substitution> subs = e->getMember().getSubstitutions();
    
    // Figure out the kind of declaration reference we're working with.
    SILDeclRef::Kind kind = SILDeclRef::Kind::Func;
    if (isa<ConstructorDecl>(fd)) {
      if (proto->isObjC()) {
        // For Objective-C initializers, we only have an initializing
        // initializer. We need to allocate the object ourselves.
        kind = SILDeclRef::Kind::Initializer;

        baseVal = allocateObjCObject(baseVal, e->getBase());
      } else {
        // For non-Objective-C initializers, we have an allocating
        // initializer to call.
        kind = SILDeclRef::Kind::Allocator;
      }
    }

    // Open existential types to get at the value and witnesses inside.
    //
    // FIXME: The AST could do this for us better.
    if (baseTy->isExistentialType()) {
      // TODO: Existential type methods ideally wouldn't be a special case.
      if (!fd->isInstanceMember()) {
        assert(baseVal.getType().is<ExistentialMetatypeType>() &&
               "non-existential-metatype for existential static method?!");
        // FIXME: It is impossible to invoke a class method on a protocol
        // directly, it must be done through an archetype.
        setSelfParam(RValue(gen, e, baseVal.getType().getSwiftRValueType(),
                            baseVal), e);
        goto did_set_self_param;
      }
      
      CanType baseOpenedTy = ArchetypeType::getOpened(baseTy)
        ->getCanonicalType();
      SILType loweredOpenedTy = gen.getLoweredType(baseOpenedTy);
      
      // Attach the existential cleanup to the projection so that it gets
      // consumed (or not) when the call is applied to it (or isn't).
      if (baseVal.getType().isClassExistentialType()) {
        SILValue val = gen.B.createOpenExistentialRef(e,
                                          baseVal.getValue(), loweredOpenedTy);
        baseVal = ManagedValue(val, baseVal.getCleanup());
      } else {
        assert(loweredOpenedTy.isAddress() && "Self should be address-only");
        SILValue val = gen.B.createOpenExistential(e, baseVal.getValue(),
                                                   loweredOpenedTy);
        baseVal = ManagedValue::forUnmanaged(val);
      }
      
      // Remap substitutions from the protocol type to the opened archetype.
      remapSelfSubstitutions(gen, subs, baseOpenedTy);
      baseTy = baseOpenedTy;
    }
    
    // If we're calling a non-class-constrained protocol member through a
    // refinement of the protocol that is class-constrained, then we have to
    // materialize the value in order to pass it indirectly.
    if (fd->isInstanceMember()
        && !proto->requiresClass()
        && !baseVal.getType().isAddress()) {
      auto temp = gen.emitTemporaryAllocation(e, baseVal.getType());
      gen.B.createStore(e, baseVal.getValue(), temp);
      baseVal = ManagedValue(temp, baseVal.getCleanup());
    }

    setSelfParam(RValue(gen, e->getBase(), baseVal.getType().getSwiftType(),
                        baseVal), e);
    
  did_set_self_param:
    // Method calls through ObjC protocols require ObjC dispatch.
    bool isObjC = proto->isObjC();
    
    assert(!baseTy->getRValueInstanceType()->isExistentialType()
           && "did not open existential type?!");
    
    // Gross. We won't have any call sites if this is a curried generic method
    // application, because it doesn't look like a call site in the AST, but a
    // member ref.
    CanFunctionType substFnType;
    if (!callSites.empty() || dyn_cast_or_null<ApplyExpr>(SelfApplyExpr))
      substFnType = getSubstFnType();
    else
      substFnType = cast<FunctionType>(e->getType()->getCanonicalType());
    
    setCallee(Callee::forArchetype(gen, selfParam.peekScalarValue(),
                                   SILDeclRef(fd, kind).asForeign(isObjC),
                                   substFnType, e));

    // If there are substitutions, add them now.
    if (!subs.empty()) {
      callee->setSubstitutions(gen, e, subs, callDepth);
    }
    
  }

  void visitFunctionConversionExpr(FunctionConversionExpr *e) {
    // FIXME: Check whether this function conversion requires us to build a
    // thunk.
    visit(e->getSubExpr());
  }

  void visitCovariantFunctionConversionExpr(CovariantFunctionConversionExpr *e){
    // FIXME: These expressions merely adjust the result type for DynamicSelf
    // in an unchecked, ABI-compatible manner. They shouldn't prevent us form
    // forming a complete call.
    visitExpr(e);
  }

  void visitIdentityExpr(IdentityExpr *e) {
    visit(e->getSubExpr());
  }
  
  void applySuper(ApplyExpr *apply) {
    // Load the 'super' argument.
    Expr *arg = apply->getArg();
    ManagedValue super = gen.emitRValueAsSingleValue(arg);

    // The callee for a super call has to be either a method or constructor.
    Expr *fn = apply->getFn();
    ArrayRef<Substitution> substitutions;
    SILDeclRef constant;
    if (auto *ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(fn)) {
      constant = SILDeclRef(ctorRef->getDecl(), SILDeclRef::Kind::Initializer,
                         SILDeclRef::ConstructAtBestResilienceExpansion,
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         gen.SGM.requiresObjCSuperDispatch(ctorRef->getDecl()));

      if (ctorRef->getDeclRef().isSpecialized())
        substitutions = ctorRef->getDeclRef().getSubstitutions();
    } else if (auto *declRef = dyn_cast<DeclRefExpr>(fn)) {
      assert(isa<FuncDecl>(declRef->getDecl()) && "non-function super call?!");
      constant = SILDeclRef(declRef->getDecl(),
                         SILDeclRef::ConstructAtBestResilienceExpansion,
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         gen.SGM.requiresObjCSuperDispatch(declRef->getDecl()));

      if (declRef->getDeclRef().isSpecialized())
        substitutions = declRef->getDeclRef().getSubstitutions();
    } else
      llvm_unreachable("invalid super callee");

    CanType superFormalType = arg->getType()->getCanonicalType();
    setSelfParam(RValue(gen, apply, superFormalType, super), apply);
    
    SILValue superMethod;
    if (constant.isForeign) {
      SILValue Input = super.getValue();
      while (auto *UI = dyn_cast<UpcastInst>(Input))
        Input = UI->getOperand();
      // ObjC super calls require dynamic dispatch.
      setCallee(Callee::forSuperMethod(gen, Input, constant,
                                       getSubstFnType(), fn));
    } else {
      // Native Swift super calls are direct.
      setCallee(Callee::forDirect(gen, constant, getSubstFnType(), fn));
    }

    // If there are any substitutions for the callee, apply them now.
    if (!substitutions.empty())
      callee->setSubstitutions(gen, fn, substitutions, callDepth-1);
  }

  /// Try to emit the given application as initializer delegation.
  bool applyInitDelegation(ApplyExpr *expr) {
    // Dig out the constructor we're delegating to.
    Expr *fn = expr->getFn();
    auto ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(
                     fn->getSemanticsProvidingExpr());
    if (!ctorRef)
      return false;

    // Determine whether we'll need to use an allocating constructor (vs. the
    // initializing constructor).
    auto nominal = ctorRef->getDecl()->getDeclContext()
                     ->getDeclaredTypeOfContext()->getAnyNominal();
    bool useAllocatingCtor = isa<StructDecl>(nominal) || 
                             isa<EnumDecl>(nominal) ||
                             ctorRef->getDecl()->isFactoryInit();

    // Load the 'self' argument.
    Expr *arg = expr->getArg();
    ManagedValue self;

    // If we're using the allocating constructor, we need to pass along the
    // metatype.
    if (useAllocatingCtor) {
      self = ManagedValue::forUnmanaged(gen.emitMetatypeOfValue(expr, arg));
    } else {
      self = gen.emitRValueAsSingleValue(arg);
    }

    CanType selfFormalType = arg->getType()->getCanonicalType();
    setSelfParam(RValue(gen, expr, selfFormalType, self), expr);

    // Determine the callee. For structs and enums, this is the allocating
    // constructor (because there is no initializing constructor). For classes,
    // this is the initializing constructor, to which we will dynamically
    // dispatch.
    if (gen.getMethodDispatch(ctorRef->getDecl()) == MethodDispatch::Class) {
      // Dynamic dispatch to the initializer.
      setCallee(Callee::forClassMethod(
                  gen,
                  self.getValue(),
                  SILDeclRef(ctorRef->getDecl(),
                             useAllocatingCtor
                               ? SILDeclRef::Kind::Allocator
                               : SILDeclRef::Kind::Initializer,
                             SILDeclRef::ConstructAtBestResilienceExpansion,
                             SILDeclRef::ConstructAtNaturalUncurryLevel,
                             gen.SGM.requiresObjCDispatch(ctorRef->getDecl())),
                  getSubstFnType(), fn));
    } else {
      // Directly call the peer constructor.
      setCallee(Callee::forDirect(gen,
                                  SILDeclRef(ctorRef->getDecl(),
                                             useAllocatingCtor
                                               ? SILDeclRef::Kind::Allocator
                                               : SILDeclRef::Kind::Initializer,
                               SILDeclRef::ConstructAtBestResilienceExpansion),
                                  getSubstFnType(useAllocatingCtor), fn));
      // In direct peer delegation cases, do not mark the apply as @transparent
      // even if the underlying implementation is @transparent.  This is a hack
      // but is important because transparent inlining happends before DI, and
      // DI needs to see the call to the delegated constructor.
      callee->setTransparent(false);
    }

    // Set up the substitutions, if we have any.
    if (ctorRef->getDeclRef().isSpecialized())
      callee->setSubstitutions(gen, fn,
                               ctorRef->getDeclRef().getSubstitutions(),
                               callDepth-1);

    return true;
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

      // Simple optional-to-optional conversions.  This doesn't work
      // for the full generality of OptionalEvaluationExpr, but it
      // works given that we check the result for certain forms.
      if (auto eval = dyn_cast<OptionalEvaluationExpr>(expr)) {
        if (auto inject = dyn_cast<InjectIntoOptionalExpr>(eval->getSubExpr())) {
          if (auto bind = dyn_cast<BindOptionalExpr>(inject->getSubExpr())) {
            if (bind->getDepth() == 0)
              return bind->getSubExpr();
          }
        }
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

    // Since we'll be collapsing this call site, make sure there's another
    // call site that will actually perform the invocation.
    if (callSites.empty())
      return false;

    // Only @objc methods can be forced.
    auto *fd = dyn_cast<FuncDecl>(dynamicMemberRef->getMember().getDecl());
    if (!fd || !fd->isObjC())
      return false;

    // We found it. Emit the base.
    ManagedValue existential =
      gen.emitRValueAsSingleValue(dynamicMemberRef->getBase());

    SILValue val;
    if (fd->isInstanceMember()) {
      assert(fd->isInstanceMember() && "Non-instance dynamic member reference");

      Type openedType = ArchetypeType::getOpened(
                                    existential.getType().getSwiftRValueType());
      SILType loweredOpenedType = gen.getLoweredLoadableType(openedType);
      
      // Attach the existential cleanup to the projection so that it gets
      // consumed (or not) when the call is applied to it (or isn't).
      val = gen.B.createOpenExistentialRef(dynamicMemberRef,
                      existential.getValue(),
                      loweredOpenedType);
      ManagedValue proj(val, existential.getCleanup());
      setSelfParam(RValue(gen, dynamicMemberRef,
                          proj.getType().getSwiftRValueType(), proj),
                   dynamicMemberRef);
    } else {
      assert(existential.getType().is<ExistentialMetatypeType>() &&
             "non-dynamic-lookup-metatype for static method?!");
      val = existential.getValue();
      ManagedValue proj(val, existential.getCleanup());
      setSelfParam(RValue(gen, dynamicMemberRef,
                          existential.getType().getSwiftRValueType(),
                          existential),
                   dynamicMemberRef);
    }

    // Determine the type of the method we referenced, by replacing the
    // class type of the 'Self' parameter with Builtin.UnknownObject.
    SILDeclRef member(fd, SILDeclRef::ConstructAtBestResilienceExpansion,
                      SILDeclRef::ConstructAtNaturalUncurryLevel,
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
  if (auto meta1 = dyn_cast<AnyMetatypeType>(type1)) {
    auto meta2 = dyn_cast<AnyMetatypeType>(type2);
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
ManagedValue SILGenFunction::emitApply(
                              SILLocation loc,
                              ManagedValue fn,
                              ArrayRef<Substitution> subs,
                              ArrayRef<ManagedValue> args,
                              CanSILFunctionType substFnType,
                              AbstractionPattern origResultType,
                              CanType substResultType,
                              bool transparent,
                              Optional<AbstractCC> overrideCC,
                              SGFContext evalContext) {
  auto &formalResultTL = getTypeLowering(substResultType);
  auto loweredFormalResultType = formalResultTL.getLoweredType();
  AbstractCC cc = overrideCC ? *overrideCC : substFnType->getAbstractCC();

  SILType actualResultType = substFnType->getSemanticResultSILType();

  // Check whether there are abstraction differences (beyond just
  // direct vs. indirect) between the lowered formal result type and
  // the actual result type we got back.  Note that this will also
  // include bridging differences.
  bool hasAbsDiffs =
    hasAbstractionDifference(cc,
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
      resultAddr = getBufferForExprResult(loc, actualResultType,
                                              evalContext);

    // Otherwise, we need a temporary of the right abstraction.
    } else {
      resultAddr =
        emitTemporaryAllocation(loc, actualResultType.getObjectType());
    }
  }
  
  // If the function returns an inner pointer, we'll need to lifetime-extend
  // the 'self' parameter.
  SILValue lifetimeExtendedSelf;
  if (substFnType->getResult().getConvention()
        == ResultConvention::UnownedInnerPointer) {
    auto selfMV = args.back();
    lifetimeExtendedSelf = selfMV.getValue();
    
    switch (substFnType->getParameters().back().getConvention()) {
    case ParameterConvention::Direct_Owned:
      // If the callee will consume the 'self' parameter, let's retain it so we
      // can keep it alive.
      B.emitRetainValueOperation(loc, lifetimeExtendedSelf);
      break;
    case ParameterConvention::Direct_Guaranteed:
    case ParameterConvention::Direct_Unowned:
      // We'll manually manage the argument's lifetime after the
      // call. Disable its cleanup, forcing a copy if it was emitted +0.
      if (selfMV.hasCleanup()) {
        selfMV.forwardCleanup(*this);
      } else {
        lifetimeExtendedSelf = selfMV.copyUnmanaged(*this, loc).forward(*this);
      }
      break;
        
    case ParameterConvention::Indirect_In_Guaranteed:
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_Out:
      // We may need to support this at some point, but currently only imported
      // objc methods are returns_inner_pointer.
      llvm_unreachable("indirect self argument to method that"
                       " returns_inner_pointer?!");
    }
  }
  
  // Emit the raw application.
  SILValue scalarResult = emitRawApply(*this, loc, fn, subs, args,
                                       substFnType, transparent, resultAddr);

  // If we emitted into the eval context, then it's because there was
  // no abstraction difference *or* bridging to do.  But our caller
  // might not expect to get a result indirectly.
  if (emittedIntoContext) {
    assert(substFnType->hasIndirectResult());
    assert(!hasAbsDiffs);
    auto managedBuffer =
      manageBufferForExprResult(resultAddr, formalResultTL, evalContext);

    // managedBuffer will be null here to indicate that we satisfied
    // the evalContext.  If so, we're done.
    // We are also done if the expected type is address-only.
    if (managedBuffer.isInContext() || formalResultTL.isAddressOnly())
      return managedBuffer;

    // Otherwise, deactivate the cleanup we just entered; we're about
    // to take from the address.
    resultAddr = managedBuffer.forward(*this);
  }

  // Get the type lowering for the actual result representation.
  // This is very likely to be the same as that for the formal result.
  auto &actualResultTL
    = (actualResultType == loweredFormalResultType
         ? formalResultTL : getTypeLowering(actualResultType));

  // If the expected result is an address, manage the result address.
  if (formalResultTL.isAddressOnly()) {
    assert(resultAddr);
    auto managedActualResult =
      emitManagedBufferWithCleanup(resultAddr, actualResultTL);

    if (!hasAbsDiffs) return managedActualResult;
    return emitOrigToSubstValue(loc, managedActualResult,
                                    origResultType, substResultType,
                                    evalContext);
  }

  // Okay, we want a scalar result.
  assert(!actualResultTL.isAddressOnly() &&
         "actual result is address-only when formal result is not?");

  ManagedValue managedScalar;

  // If we got an indirect result, emit a take out of the result address.
  if (substFnType->hasIndirectResult()) {
    managedScalar = emitLoad(loc, resultAddr, actualResultTL,
                             SGFContext(), IsTake);

  // Otherwise, manage the direct result.
  } else {
    switch (substFnType->getResult().getConvention()) {
    case ResultConvention::Owned:
      // Already retained.
      break;

    case ResultConvention::Autoreleased:
      // Autoreleased. Retain using retain_autoreleased.
      B.createStrongRetainAutoreleased(loc, scalarResult);
      break;
        
    case ResultConvention::UnownedInnerPointer:
      // Autorelease the 'self' value to lifetime-extend it.
      assert(lifetimeExtendedSelf.isValid()
             && "did not save lifetime-extended self param");
      B.createAutoreleaseValue(loc, lifetimeExtendedSelf);
      SWIFT_FALLTHROUGH;
        
    case ResultConvention::Unowned:
      // Unretained. Retain the value.
      actualResultTL.emitRetainValue(B, loc, scalarResult);
      break;
    }

    managedScalar = emitManagedRValueWithCleanup(scalarResult,
                                                 actualResultTL);
  }

  // Fast path: no abstraction differences or bridging.
  if (!hasAbsDiffs) return managedScalar;

  // Remove abstraction differences.
  if (origResultType.getAsType() != substResultType) {
    managedScalar = emitOrigToSubstValue(loc, managedScalar,
                                         origResultType,
                                         substResultType,
                                         SGFContext());
  }

  // Convert the result to a native value.
  return emitBridgedToNativeValue(loc, managedScalar, cc,
                                  substResultType);
}

ManagedValue SILGenFunction::emitMonomorphicApply(SILLocation loc,
                                                  ManagedValue fn,
                                                  ArrayRef<ManagedValue> args,
                                                  CanType resultType,
                                                  bool transparent,
                                              Optional<AbstractCC> overrideCC) {
  auto fnType = fn.getType().castTo<SILFunctionType>();
  assert(!fnType->isPolymorphic());
  return emitApply(loc, fn, {}, args, fnType,
                   AbstractionPattern(resultType), resultType,
                   transparent, overrideCC, SGFContext());
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

static CanType claimNextParamClause(CanAnyFunctionType &type) {
  auto result = type.getInput();
  type = dyn_cast<AnyFunctionType>(type.getResult());
  return result;
}

namespace {
  class ArgEmitter {
    SILGenFunction &SGF;
    AbstractCC CC;
    ArrayRef<SILParameterInfo> ParamInfos;
    SmallVectorImpl<ManagedValue> &Args;

    /// This keeps track of any inout arguments that are emitted.
    SmallVector<std::pair<SILValue, SILLocation>, 2> InOutArguments;
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

      // If the caller takes the argument indirectly, the argument has an
      // inout type.
      if (param.isIndirectInOut()) {
        assert(isa<InOutType>(substArgType));
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
      SILLocation loc = arg.getLocation();
      ManagedValue address;
      if (arg.isRValue()) {
        // If the argument is already lowered to an RValue, it must be the
        // receiver of a self argument, which will be the first inout.
        address = std::move(arg).asKnownRValue().getScalarValue();
      } else {
        auto *e = cast<InOutExpr>(std::move(arg).asKnownExpr()->
                                     getSemanticsProvidingExpr());

        LValue lv = SGF.emitLValue(e->getSubExpr(), AccessKind::ReadWrite);
        address = SGF.emitAddressOfLValue(e->getSubExpr(), std::move(lv),
                                          AccessKind::ReadWrite);
      }

      if (hasAbstractionDifference(CC, loweredSubstParamType,
                                   loweredSubstArgType)) {
        // This could actually just be type-bridging.  In any case,
        // it can be dealt with with writeback.
        llvm::errs() << "Unimplemented: abstraction difference in l-value\n";
        llvm::errs() << "\n  "; loweredSubstParamType.dump();
        llvm::errs() << "\n  "; loweredSubstArgType.dump();
        abort();
      }

      // We know that the rvalue has a single ManagedValue.  Check to see if we
      // have an obvious alias with a previously emitted argument.  Note that we
      // could do this in a later SILDiagnostics pass as well: this would be
      // stronger (more equivalences exposed) but would have worse source
      // location information.
      
      // TODO: This uses exact SILValue equivalence to detect aliases, we could
      // do something stronger here to catch other obvious cases.
      for (auto prev : InOutArguments) {
        if (prev.first == address.getValue()) {
          SGF.SGM.diagnose(loc, diag::inout_argument_alias)
            .highlight(loc.getSourceRange());
          SGF.SGM.diagnose(prev.second, diag::previous_inout_alias)
            .highlight(prev.second.getSourceRange());
        }
      }
      
      InOutArguments.push_back({address.getValue(), loc});
      Args.push_back(address);
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
    
    void emit(SILGenFunction &gen, AbstractionPattern origParamType,
              ParamLowering &lowering, SmallVectorImpl<ManagedValue> &args) && {
      auto params = lowering.claimParams(origParamType, getSubstArgType());

      ArgEmitter emitter(gen, lowering.CC, params, args);
      emitter.emit(std::move(ArgValue), origParamType);
    }

    RValueSource &&forward() && {
      return std::move(ArgValue);
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

      // Check for a specialized emitter.
      Optional<SpecializedEmitter> specializedEmitter =
        callee.getSpecializedEmitter(gen.SGM, uncurryLevel);

      CanSILFunctionType substFnType;
      ManagedValue mv;
      bool transparent;

      AbstractionPattern origFormalType(callee.getOrigFormalType());
      CanAnyFunctionType formalType = callee.getSubstFormalType();

      if (specializedEmitter) {
        // We want to emit the arguments as fully-substituted values
        // because that's what the specialized emitters expect.
        origFormalType = AbstractionPattern(formalType);
        substFnType = gen.getLoweredType(formalType, uncurryLevel)
          .castTo<SILFunctionType>();
        transparent = false; // irrelevant
      } else {
        std::tie(mv, substFnType, transparent) =
          callee.getAtUncurryLevel(gen, uncurryLevel);
      }

      // Emit the first level of call.
      ManagedValue result;

      // We use the context emit-into initialization only for the
      // outermost call.
      SGFContext uncurriedContext =
        (extraSites.empty() ? C : SGFContext());

      // If we have an early emitter, just let it take over for the
      // uncurried call site.
      if (specializedEmitter &&
          specializedEmitter->isEarlyEmitter()) {
        auto emitter = specializedEmitter->getEarlyEmitter();

        assert(uncurriedSites.size() == 1);
        CanFunctionType formalApplyType = cast<FunctionType>(formalType);
        SILLocation uncurriedLoc = uncurriedSites[0].Loc;
        claimNextParamClause(origFormalType);
        claimNextParamClause(formalType);

        // We should be able to enforce that these arguments are
        // always still expressions.
        Expr *argument = std::move(uncurriedSites[0]).forward().asKnownExpr();
        result = emitter(gen, uncurriedLoc,
                         callee.getSubstitutions(),
                         argument,
                         formalApplyType,
                         uncurriedContext);

      // Otherwise, emit the uncurried arguments now and perform
      // the call.
      } else {
        // Emit the arguments.
        Optional<SILLocation> uncurriedLoc;
        SmallVector<SmallVector<ManagedValue, 4>, 2> args;
        CanFunctionType formalApplyType;
        args.reserve(uncurriedSites.size());
        {
          ParamLowering paramLowering(substFnType);
       
          // Collect the arguments to the uncurried call.
          for (auto &site : uncurriedSites) {
            AbstractionPattern origParamType =
              claimNextParamClause(origFormalType);
            formalApplyType = cast<FunctionType>(formalType);
            claimNextParamClause(formalType);
            uncurriedLoc = site.Loc;
            args.push_back({});
            std::move(site).emit(gen, origParamType, paramLowering,
                                 args.back());
          }
        }
        assert(uncurriedLoc);
        assert(formalApplyType);

        // Uncurry the arguments in calling convention order.
        SmallVector<ManagedValue, 4> uncurriedArgs;
        for (auto &argSet : reversed(args))
          uncurriedArgs.append(argSet.begin(), argSet.end());
        args = {};
      
        // Emit the uncurried call.
        if (!specializedEmitter) {
          result = gen.emitApply(uncurriedLoc.getValue(), mv,
                                 callee.getSubstitutions(),
                                 uncurriedArgs,
                                 substFnType,
                                 origFormalType,
                                 uncurriedSites.back().getSubstResultType(),
                                 transparent, None,
                                 uncurriedContext);
        } else if (specializedEmitter->isLateEmitter()) {
          auto emitter = specializedEmitter->getLateEmitter();
          result = emitter(gen,
                           uncurriedLoc.getValue(),
                           callee.getSubstitutions(),
                           uncurriedArgs,
                           formalApplyType,
                           uncurriedContext);
        } else {
          assert(specializedEmitter->isNamedBuiltin());
          auto builtinName = specializedEmitter->getBuiltinName();
          SmallVector<SILValue, 4> consumedArgs;
          for (auto arg : uncurriedArgs) {
            consumedArgs.push_back(arg.forward(gen));
          }
          auto resultVal =
            gen.B.createBuiltin(uncurriedLoc.getValue(), builtinName,
                                substFnType->getResult().getSILType(),
                                callee.getSubstitutions(),
                                consumedArgs);
          result = gen.emitManagedRValueWithCleanup(resultVal);
        }
      }
      
      // End the initial writeback scope, if any.
      initialWritebackScope.reset();
      
      // If there are remaining call sites, apply them to the result function.
      // Each chained call gets its own writeback scope.
      for (unsigned i = 0, size = extraSites.size(); i < size; ++i) {
        WritebackScope scope(gen);

        auto substFnType = result.getType().castTo<SILFunctionType>();
        ParamLowering paramLowering(substFnType);

        SmallVector<ManagedValue, 4> siteArgs;

        // The result function has already been reabstracted to the substituted
        // type, so use the substituted formal type as the abstraction pattern
        // for argument passing now.
        AbstractionPattern origParamType(claimNextParamClause(formalType));
        std::move(extraSites[i]).emit(gen, origParamType, paramLowering,
                                      siteArgs);
        SGFContext context = i == size - 1 ? C : SGFContext();
        SILLocation loc = extraSites[i].Loc;
        result = gen.emitApply(loc, result, {}, siteArgs,
                           substFnType,
                           origFormalType,
                           extraSites[i].getSubstResultType(),
                           false, None, context);
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

RValue SILGenFunction::emitApplyExpr(Expr *e, SGFContext c) {
  return RValue(*this, e, prepareApplyExpr(*this, e).apply(c));
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

  return emitApply(loc, mv, subs, args, substFnType,
                   AbstractionPattern(origFormalType.getResult()),
                   substFormalType.getResult(),
                   transparent, None, ctx);
}

/// Allocate an uninitialized array of a given size, returning the array
/// and a pointer to its uninitialized contents, which must be initialized
/// before the array is valid.
std::pair<ManagedValue, SILValue>
SILGenFunction::emitUninitializedArrayAllocation(Type ArrayTy,
                                                 SILValue Length,
                                                 SILLocation Loc) {
  auto &Ctx = getASTContext();
  auto allocate = Ctx.getAllocateUninitializedArray(nullptr);
  auto allocateArchetypes = allocate->getGenericParams()->getAllArchetypes();
  
  auto arrayElementTy = ArrayTy->castTo<BoundGenericType>()
    ->getGenericArgs()[0];
  
  // Invoke the intrinsic, which returns a tuple.
  Substitution sub{allocateArchetypes[0], arrayElementTy, {}};
  auto result = emitApplyOfLibraryIntrinsic(Loc, allocate,
                                            sub,
                                            ManagedValue::forUnmanaged(Length),
                                            SGFContext());
  
  // Explode the tuple.
  TupleTypeElt elts[] = {ArrayTy, Ctx.TheRawPointerType};
  auto tupleTy = TupleType::get(elts, Ctx)->getCanonicalType();
  RValue resultTuple(*this, Loc, tupleTy, result);
  SmallVector<ManagedValue, 2> resultElts;
  std::move(resultTuple).getAll(resultElts);
  
  return {resultElts[0], resultElts[1].getUnmanagedValue()};
}

static Callee getBaseAccessorFunctionRef(SILGenFunction &gen,
                                         SILLocation loc,
                                         SILDeclRef constant,
                                         RValueSource &selfValue,
                                         bool isSuper,
                                         bool isDirectUse,
                                         CanAnyFunctionType substAccessorType,
                                         ArrayRef<Substitution> &substitutions){
  auto *decl = cast<AbstractFunctionDecl>(constant.getDecl());

  // If this is a method in a protocol, generate it as a protocol call.
  if (auto *protoDecl = dyn_cast<ProtocolDecl>(decl->getDeclContext())) {
    assert(!isDirectUse && "direct use of protocol accessor?");
    
    // Method calls through ObjC protocols require ObjC dispatch.
    constant = constant.asForeign(protoDecl->isObjC());

    // The protocol self is implicitly decurried.
    substAccessorType = CanAnyFunctionType(substAccessorType->getResult()
                                           ->castTo<AnyFunctionType>());

    // If this is an archetype case, construct an archetype call.
    auto selfTy = selfValue.getSubstType()->getInOutObjectType();
    if (!selfTy->isAnyExistentialType()) {
      SILValue baseVal =
        selfValue.forceAndPeekRValue(gen).peekScalarValue();
    
      return Callee::forArchetype(gen, baseVal, constant,
                                  substAccessorType, loc);
    }
    
    // If this is a protocol (not archetype) use, open the contained type.
    ManagedValue baseVal =
      std::move(selfValue).getAsSingleValue(gen, SGFContext::AllowPlusZero);
    
    Type openedTy = ArchetypeType::getOpened(selfTy);
    SILType loweredOpenedTy = gen.getLoweredType(openedTy);
    auto selfLoc = selfValue.getLocation();

    ManagedValue projVal;
    if (baseVal.getType().isClassExistentialType()) {
      // Attach the existential cleanup to the projection so that it gets
      // consumed (or not) when the call is applied to it (or isn't).
      SILValue val = gen.B.createOpenExistentialRef(loc,
                                                    baseVal.getValue(),
                                                    loweredOpenedTy);
      projVal = ManagedValue(val, baseVal.getCleanup());
    } else {
      assert(loweredOpenedTy.isAddress() && "Self should be address-only");
      SILValue val = gen.B.createOpenExistential(selfLoc,
                                                 baseVal.getValue(),
                                                 loweredOpenedTy);
      projVal = ManagedValue::forUnmanaged(val);
    }

    // Update the self value to use the projection.
    selfValue = RValueSource(selfLoc, RValue(gen, selfLoc,
                                             loweredOpenedTy.getSwiftType(),
                                             projVal));
    remapSelfSubstitutions(gen, substitutions, openedTy);
    
    return Callee::forArchetype(gen, projVal.getValue(), constant,
                                substAccessorType, loc);
  }

  bool isClassDispatch = false;
  if (!isDirectUse) {
    switch (gen.getMethodDispatch(decl)) {
    case MethodDispatch::Class:
      isClassDispatch = true;
      break;
    case MethodDispatch::Static:
      isClassDispatch = false;
      break;
    }
  }

  // Dispatch in a struct/enum or to an final method is always direct.
  if (!isClassDispatch || decl->isFinal())
    return Callee::forDirect(gen, constant, substAccessorType, loc);

  // Otherwise, if we have a non-final class dispatch to a normal method,
  // perform a dynamic dispatch.
  auto self = selfValue.forceAndPeekRValue(gen).peekScalarValue();
  if (!isSuper)
    return Callee::forClassMethod(gen, self, constant, substAccessorType,
                                  loc);

  // If this is a "super." dispatch, we either do a direct dispatch in the case
  // of swift classes or an objc super call.
  while (auto *upcast = dyn_cast<UpcastInst>(self))
    self = upcast->getOperand();
  
  if (constant.isForeign)
    return Callee::forSuperMethod(gen, self, constant, substAccessorType,loc);

  return Callee::forDirect(gen, constant, substAccessorType, loc);
}

static Callee 
emitSpecializedAccessorFunctionRef(SILGenFunction &gen,
                                   SILLocation loc,
                                   SILDeclRef constant,
                                   ArrayRef<Substitution> substitutions,
                                   RValueSource &selfValue,
                                   bool isSuper,
                                   bool isDirectUse)
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
  Callee callee = getBaseAccessorFunctionRef(gen, loc, constant, selfValue,
                                             isSuper, isDirectUse,
                                             substAccessorType, substitutions);
  
  // If there are substitutions, specialize the generic accessor.
  // FIXME: Generic subscript operator could add another layer of
  // substitutions.
  if (!substitutions.empty()) {
    callee.setSubstitutions(gen, loc, substitutions, 0);
  }
  return callee;
}

RValueSource SILGenFunction::prepareAccessorBaseArg(SILLocation loc,
                                                    ManagedValue base,
                                                    SILDeclRef accessor) {
  auto accessorType = SGM.Types.getConstantFunctionType(accessor);
  SILParameterInfo selfParam = accessorType->getParameters().back();

  assert(!base.isInContext());
  assert(!base.isLValue() || !base.hasCleanup());
  SILType baseType = base.getValue().getType();

  // If the base is currently an address, we may have to copy it.
  if (baseType.isAddress()) {
    auto needsLoad = [&] {
      switch (selfParam.getConvention()) {
      // If the accessor wants the value 'inout', always pass the
      // address we were given.  This is semantically required.
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_In_Guaranteed:
        return false;

      // If the accessor wants the value 'in', we have to copy if the
      // base isn't a temporary.  We aren't allowed to pass aliased
      // memory to 'in', and we have pass at +1.
      case ParameterConvention::Indirect_In:
        return base.isPlusZeroRValueOrTrivial();

      case ParameterConvention::Indirect_Out:
        llvm_unreachable("out parameter not expected here");

      // If the accessor wants the value directly, we definitely have to
      // load.  TODO: don't load-and-retain if the value is passed at +0.
      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Direct_Guaranteed:
        return true;
      }
      llvm_unreachable("bad convention");
    };
    if (needsLoad()) {
      // The load can only be a take if the base is a +1 rvalue.
      auto shouldTake = IsTake_t(base.hasCleanup());

      base = emitLoad(loc, base.forward(*this), getTypeLowering(baseType),
                      SGFContext(), shouldTake);
    }

  // If the base is currently a value, we may have to drop it in
  // memory or copy it.
  } else {
    assert(!base.isLValue());

    // We need to produce the value at +1 if it's going to be consumed.
    if (selfParam.isConsumed() && !base.hasCleanup()) {
      base = base.copyUnmanaged(*this, loc);
    }

    // If the parameter is indirect, we need to drop the value into
    // temporary memory.
    if (selfParam.isIndirect()) {
      auto temporary = emitTemporaryAllocation(loc, baseType);
      bool hadCleanup = base.hasCleanup();
      B.createStore(loc, base.forward(*this), temporary);

      // The temporary memory is +0 if the value was.  (But note that
      // we'll make it +1 if the parameter is consumed.)
      if (hadCleanup) {
        base = ManagedValue(temporary, enterDestroyCleanup(temporary));
      } else {
        base = ManagedValue::forUnmanaged(temporary);
      }
    }
  }

  return RValueSource(loc, RValue(*this, loc,
                                  base.getType().getSwiftType(), base));
}

SILDeclRef SILGenFunction::getGetterDeclRef(AbstractStorageDecl *storage,
                                            bool isDirectUse) {
  return SILDeclRef(storage->getGetter(), SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtBestResilienceExpansion,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    !isDirectUse && storage->requiresObjCGetterAndSetter());
}

/// Emit a call to a getter.
ManagedValue SILGenFunction::
emitGetAccessor(SILLocation loc, SILDeclRef get,
                ArrayRef<Substitution> substitutions, RValueSource &&selfValue,
                bool isSuper, bool isDirectUse,
                RValue &&subscripts, SGFContext c) {
  Callee getter = emitSpecializedAccessorFunctionRef(*this, loc, get,
                                                     substitutions, selfValue,
                                                     isSuper, isDirectUse);
  CanAnyFunctionType accessType = getter.getSubstFormalType();

  CallEmission emission(*this, std::move(getter));
  // Self ->
  if (selfValue) {
    emission.addCallSite(loc, std::move(selfValue), accessType.getResult());
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }
  // Index or () if none.
  if (!subscripts)
    subscripts = emitEmptyTupleRValue(loc);

  emission.addCallSite(loc, RValueSource(loc, std::move(subscripts)),
                       accessType.getResult());

  // T
  return emission.apply(c);
}

SILDeclRef SILGenFunction::getSetterDeclRef(AbstractStorageDecl *storage,
                                            bool isDirectUse) {
  return SILDeclRef(storage->getSetter(), SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtBestResilienceExpansion,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    !isDirectUse && storage->requiresObjCGetterAndSetter());
}

void SILGenFunction::emitSetAccessor(SILLocation loc, SILDeclRef set,
                                     ArrayRef<Substitution> substitutions,
                                     RValueSource &&selfValue,
                                     bool isSuper, bool isDirectUse,
                                     RValue &&subscripts, RValue &&setValue) {
  Callee setter = emitSpecializedAccessorFunctionRef(*this, loc, set,
                                                     substitutions, selfValue,
                                                     isSuper, isDirectUse);
  CanAnyFunctionType accessType = setter.getSubstFormalType();

  CallEmission emission(*this, std::move(setter));
  // Self ->
  if (selfValue) {
    emission.addCallSite(loc, std::move(selfValue), accessType.getResult());
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }

  // (value)  or (value, indices)
  if (subscripts) {
    // If we have a value and index list, create a new rvalue to represent the
    // both of them together.  The value goes first.
    SmallVector<ManagedValue, 4> Elts;
    std::move(setValue).getAll(Elts);
    std::move(subscripts).getAll(Elts);
    setValue = RValue(Elts, accessType.getInput());
  } else {
    setValue.rewriteType(accessType.getInput());
  }
  emission.addCallSite(loc, RValueSource(loc, std::move(setValue)),
                       accessType.getResult());
  // ()
  emission.apply();
}

SILDeclRef
SILGenFunction::getMaterializeForSetDeclRef(AbstractStorageDecl *storage,
                                            bool isDirectUse) {
  return SILDeclRef(storage->getMaterializeForSetFunc(),
                    SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtBestResilienceExpansion,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*foreign*/ false);
}

std::pair<SILValue, SILValue> SILGenFunction::
emitMaterializeForSetAccessor(SILLocation loc, SILDeclRef materializeForSet,
                              ArrayRef<Substitution> substitutions,
                              RValueSource &&selfValue,
                              bool isSuper, bool isDirectUse,
                              RValue &&subscripts, SILValue buffer,
                              SILValue callbackStorage) {

  Callee callee = emitSpecializedAccessorFunctionRef(*this, loc,
                                                     materializeForSet,
                                                     substitutions, selfValue,
                                                     isSuper, isDirectUse);
  CanAnyFunctionType accessType = callee.getSubstFormalType();

  CallEmission emission(*this, std::move(callee));
  // Self ->
  if (selfValue) {
    emission.addCallSite(loc, std::move(selfValue), accessType.getResult());
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }

  // (buffer, callbackStorage)  or (buffer, callbackStorage, indices) ->
  RValue args = [&] {
    SmallVector<ManagedValue, 4> elts;

    auto bufferPtr =
      B.createAddressToPointer(loc, buffer,
                               SILType::getRawPointerType(getASTContext()));
    elts.push_back(ManagedValue::forUnmanaged(bufferPtr));

    elts.push_back(ManagedValue::forLValue(callbackStorage));

    if (subscripts) {
      std::move(subscripts).getAll(elts);
    }
    return RValue(elts, accessType.getInput());
  }();
  emission.addCallSite(loc, RValueSource(loc, std::move(args)),
                       accessType.getResult());
  // (buffer, optionalCallback)
  SILValue pointerAndOptionalCallback = emission.apply().getUnmanagedValue();

  // Project out the materialized address.
  SILValue address = B.createTupleExtract(loc, pointerAndOptionalCallback, 0);
  address = B.createPointerToAddress(loc, address, buffer.getType());

  // Project out the optional callback.
  SILValue optionalCallback =
    B.createTupleExtract(loc, pointerAndOptionalCallback, 1);

  return { address, optionalCallback };
}

SILDeclRef SILGenFunction::getAddressorDeclRef(AbstractStorageDecl *storage,
                                               AccessKind accessKind,
                                               bool isDirectUse) {
  FuncDecl *addressorFunc = storage->getAddressorForAccess(accessKind);
  return SILDeclRef(addressorFunc, SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtBestResilienceExpansion,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*foreign*/ false);
}

/// Emit a call to an addressor.
///
/// The first return value is the address, which will always be an
/// l-value managed value.  The second return value is the owner
/// pointer, if applicable.
std::pair<ManagedValue, ManagedValue> SILGenFunction::
emitAddressorAccessor(SILLocation loc, SILDeclRef addressor,
                      ArrayRef<Substitution> substitutions,
                      RValueSource &&selfValue, bool isSuper, bool isDirectUse,
                      RValue &&subscripts, SILType addressType) {

  Callee callee =
    emitSpecializedAccessorFunctionRef(*this, loc, addressor,
                                       substitutions, selfValue,
                                       isSuper, isDirectUse);
  CanAnyFunctionType accessType = callee.getSubstFormalType();

  CallEmission emission(*this, std::move(callee));
  // Self ->
  if (selfValue) {
    emission.addCallSite(loc, std::move(selfValue), accessType.getResult());
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }
  // Index or () if none.
  if (!subscripts)
    subscripts = emitEmptyTupleRValue(loc);

  emission.addCallSite(loc, RValueSource(loc, std::move(subscripts)),
                       accessType.getResult());

  // Unsafe{Mutable}Pointer<T> or
  // (Unsafe{Mutable}Pointer<T>, Builtin.NativePointer) or
  // (Unsafe{Mutable}Pointer<T>, Builtin.NativePointer?) or  
  SILValue result = emission.apply().forward(*this);

  SILValue pointer;
  ManagedValue owner;
  switch (cast<FuncDecl>(addressor.getDecl())->getAddressorKind()) {
  case AddressorKind::NotAddressor:
    llvm_unreachable("not an addressor!");
  case AddressorKind::Unsafe:
    pointer = result;
    owner = ManagedValue();
    break;
  case AddressorKind::Owning:
  case AddressorKind::Pinning:
    pointer = B.createTupleExtract(loc, result, 0);
    owner = emitManagedRValueWithCleanup(B.createTupleExtract(loc, result, 1));
    break;
  }

  // Drill down to the raw pointer using intrinsic knowledge of those types.
  auto pointerType =
    pointer.getType().castTo<BoundGenericStructType>()->getDecl();
  auto props = pointerType->getStoredProperties();
  assert(props.begin() != props.end());
  assert(std::next(props.begin()) == props.end());
  VarDecl *rawPointerField = *props.begin();
  pointer = B.createStructExtract(loc, pointer, rawPointerField,
                                  SILType::getRawPointerType(getASTContext()));

  // Convert to the appropriate address type and return.
  SILValue address = B.createPointerToAddress(loc, pointer, addressType);

  // Mark dependence as necessary.
  switch (cast<FuncDecl>(addressor.getDecl())->getAddressorKind()) {
  case AddressorKind::NotAddressor:
    llvm_unreachable("not an addressor!");
  case AddressorKind::Unsafe:
    // TODO: we should probably mark dependence on the base.
    break;
  case AddressorKind::Owning:
  case AddressorKind::Pinning:
    address = B.createMarkDependence(loc, address, owner.getValue());
    break;
  }

  return { ManagedValue::forLValue(address), owner };
}


ManagedValue SILGenFunction::emitApplyConversionFunction(SILLocation loc,
                                                         Expr *funcExpr,
                                                         Type resultType,
                                                         RValue &&operand) {
  // Walk the function expression, which should produce a reference to the
  // callee, leaving the final curry level unapplied.
  CallEmission emission = prepareApplyExpr(*this, funcExpr);
  // Rewrite the operand type to the expected argument type, to handle tuple
  // conversions etc.
  operand.rewriteType(funcExpr->getType()->castTo<FunctionType>()->getInput()
                        ->getCanonicalType());
  // Add the operand as the final callsite.
  emission.addCallSite(loc, RValueSource(loc, std::move(operand)), resultType);
  return emission.apply();
}

// Create a partial application of a dynamic method, applying bridging thunks
// if necessary.
static SILValue emitDynamicPartialApply(SILGenFunction &gen,
                                        SILLocation loc,
                                        SILValue method,
                                        SILValue self,
                                        CanFunctionType methodTy) {
  // Pop the self type off of the function type.
  // Just to be weird, partially applying an objc method produces a native
  // function (?!)
  auto fnTy = method.getType().castTo<SILFunctionType>();
  // If the original method has an @unowned_inner_pointer return, the partial
  // application thunk will lifetime-extend 'self' for us.
  auto resultInfo = fnTy->getResult();
  if (resultInfo.getConvention() == ResultConvention::UnownedInnerPointer)
    resultInfo = SILResultInfo(resultInfo.getType(), ResultConvention::Unowned);
  
  auto partialApplyTy = SILFunctionType::get(fnTy->getGenericSignature(),
                     fnTy->getExtInfo()
                       .withCallingConv(AbstractCC::Freestanding)
                       .withRepresentation(FunctionType::Representation::Thick),
                     ParameterConvention::Direct_Owned,
                     fnTy->getParameters()
                       .slice(0, fnTy->getParameters().size() - 1),
                     resultInfo, gen.getASTContext());
  
  // Retain 'self' because the partial apply will take ownership.
  // We can't simply forward 'self' because the partial apply is conditional.
#if 0
  auto CMV = ConsumableManagedValue(ManagedValue::forUnmanaged(self),
                                    CastConsumptionKind::CopyOnSuccess);
  self = gen.getManagedValue(loc, CMV).forward(gen);
#else
  if (!self.getType().isAddress())
    gen.B.emitRetainValueOperation(loc, self);
#endif
  
  SILValue result = gen.B.createPartialApply(loc, method, method.getType(), {},
                        self, SILType::getPrimitiveObjectType(partialApplyTy));
  // If necessary, thunk to the native ownership conventions and bridged types.
  auto nativeTy = gen.getLoweredLoadableType(methodTy).castTo<SILFunctionType>();
  
  if (nativeTy != partialApplyTy) {
    result = gen.emitBlockToFunc(loc, ManagedValue::forUnmanaged(result),
                                 nativeTy).forward(gen);
  }
  
  return result;
}

RValue SILGenFunction::emitDynamicMemberRefExpr(DynamicMemberRefExpr *e,
                                                SGFContext c) {
  // Emit the operand.
  ManagedValue existential = emitRValueAsSingleValue(e->getBase());

  SILValue operand = existential.getValue();
  if (e->getMember().getDecl()->isInstanceMember()) {
    
    // Attach the existential cleanup to the projection so that it gets
    // consumed (or not) when the call is applied to it (or isn't).
    ManagedValue proj;
    Type openedType = ArchetypeType::getOpened(operand.getType()
                                                         .getSwiftRValueType());
    SILType loweredOpenedType = getLoweredType(openedType);
    
    if (operand.getType().isClassExistentialType()) {
      SILValue val = B.createOpenExistentialRef(e, operand, loweredOpenedType);
      proj = ManagedValue(val, existential.getCleanup());
    } else {
      assert(loweredOpenedType.isAddress() && "Self should be address-only");
      SILValue val = B.createOpenExistential(e, operand, loweredOpenedType);
      proj = ManagedValue::forUnmanaged(val);
    }
    
    operand = proj.getValue();
  } else {
    auto metatype = operand.getType().castTo<ExistentialMetatypeType>();
    assert(metatype->getRepresentation() == MetatypeRepresentation::Thick);
    metatype = CanExistentialMetatypeType::get(metatype.getInstanceType(),
                                               MetatypeRepresentation::ObjC);
    operand = B.createThickToObjCMetatype(e, operand,
                                    SILType::getPrimitiveObjectType(metatype));
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
  FuncDecl *memberFunc;
  if (auto *VD = dyn_cast<VarDecl>(e->getMember().getDecl()))
    memberFunc = VD->getGetter();
  else
    memberFunc = cast<FuncDecl>(e->getMember().getDecl());
  SILDeclRef member(memberFunc, SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtBestResilienceExpansion,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC=*/true);
  B.createDynamicMethodBranch(e, operand, member, hasMemberBB, noMemberBB);

  // Create the has-member branch.
  {
    B.emitBlock(hasMemberBB);

    FullExpr hasMemberScope(Cleanups, CleanupLocation(e));

    // The argument to the has-member block is the uncurried method.
    auto valueTy = e->getType()->getCanonicalType().getAnyOptionalObjectType();
    auto methodTy = valueTy;

    // For a computed variable, we want the getter.
    if (isa<VarDecl>(e->getMember().getDecl()))
      methodTy = CanFunctionType::get(TupleType::getEmpty(getASTContext()),
                                      methodTy);

    auto dynamicMethodTy = getDynamicMethodLoweredType(*this, operand, member);
    auto loweredMethodTy = SILType::getPrimitiveObjectType(dynamicMethodTy);
    SILValue memberArg = new (F.getModule()) SILArgument(hasMemberBB,
                                                         loweredMethodTy);

    // Create the result value.
    SILValue result = emitDynamicPartialApply(*this, e, memberArg, operand,
                                              cast<FunctionType>(methodTy));
    if (isa<VarDecl>(e->getMember().getDecl())) {
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
  ManagedValue existential = emitRValueAsSingleValue(e->getBase());

  Type openedType = ArchetypeType::getOpened(
                                   existential.getType().getSwiftRValueType());
  SILType loweredOpenedType = getLoweredLoadableType(openedType);
  
  SILValue base = existential.getValue();
  base = B.createOpenExistentialRef(e, base, loweredOpenedType);

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
  auto subscriptDecl = cast<SubscriptDecl>(e->getMember().getDecl());
  SILDeclRef member(subscriptDecl->getGetter(),
                    SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtBestResilienceExpansion,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC=*/true);
  B.createDynamicMethodBranch(e, base, member, hasMemberBB, noMemberBB);

  // Create the has-member branch.
  {
    B.emitBlock(hasMemberBB);

    FullExpr hasMemberScope(Cleanups, CleanupLocation(e));

    // The argument to the has-member block is the uncurried method.
    auto valueTy = e->getType()->getCanonicalType().getAnyOptionalObjectType();
    auto methodTy =
      subscriptDecl->getGetter()->getType()->castTo<AnyFunctionType>()
                      ->getResult()->getCanonicalType();
    auto dynamicMethodTy = getDynamicMethodLoweredType(*this, base, member);
    auto loweredMethodTy = SILType::getPrimitiveObjectType(dynamicMethodTy);
    SILValue memberArg = new (F.getModule()) SILArgument(hasMemberBB,
                                                         loweredMethodTy);
    // Emit the application of 'self'.
    SILValue result = emitDynamicPartialApply(*this, e, memberArg, base,
                                              cast<FunctionType>(methodTy));
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
