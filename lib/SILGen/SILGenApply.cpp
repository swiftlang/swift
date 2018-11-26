//===--- SILGenApply.cpp - Constructs call sites for SILGen ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ArgumentScope.h"
#include "ArgumentSource.h"
#include "Callee.h"
#include "Conversion.h"
#include "FormalEvaluation.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "ResultPlan.h"
#include "Scope.h"
#include "SpecializedEmitter.h"
#include "Varargs.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/ExternalUnion.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/Unicode.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "llvm/Support/Compiler.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
//                             Utility Functions
//===----------------------------------------------------------------------===//

/// Return the abstraction pattern to use when calling a function value.
static AbstractionPattern
getIndirectApplyAbstractionPattern(SILGenFunction &SGF,
                                   CanFunctionType fnType) {
  assert(fnType);
  AbstractionPattern pattern(fnType);
  switch (fnType->getRepresentation()) {
  case FunctionTypeRepresentation::Swift:
  case FunctionTypeRepresentation::Thin:
    return pattern;

  case FunctionTypeRepresentation::CFunctionPointer:
  case FunctionTypeRepresentation::Block: {
    // C and block function parameters and results are implicitly
    // bridged to a foreign type.
    auto bridgedType =
      SGF.SGM.Types.getBridgedFunctionType(pattern, fnType,
                                           fnType->getExtInfo());
    pattern.rewriteType(CanGenericSignature(), bridgedType);
    return pattern;
  }
  }
  llvm_unreachable("bad representation");
}

/// Return the formal type for the partial-apply result type of a
/// dynamic method invocation.
static CanFunctionType
getPartialApplyOfDynamicMethodFormalType(SILGenModule &SGM, SILDeclRef member,
                                         ConcreteDeclRef memberRef) {
  auto memberCI = SGM.Types.getConstantInfo(member);

  // Construct a non-generic version of the formal type.
  // This works because we're only using foreign members, where presumably
  // substitution doesn't matter.
  CanAnyFunctionType completeMethodTy = memberCI.LoweredType;
  if (auto genericFnType = dyn_cast<GenericFunctionType>(completeMethodTy)) {
    completeMethodTy = cast<FunctionType>(
      genericFnType->substGenericArgs(memberRef.getSubstitutions())
                   ->getCanonicalType());
  }

  // Adjust the parameters by removing the self parameter, which we
  // will be partially applying.
  auto params = completeMethodTy.getParams().drop_back();

  // Adjust the result type to replace dynamic-self with AnyObject.
  CanType resultType = completeMethodTy.getResult();
  if (auto fnDecl = dyn_cast<FuncDecl>(member.getDecl())) {
    if (fnDecl->hasDynamicSelf()) {
      auto anyObjectTy = SGM.getASTContext().getAnyObjectType();
      resultType = resultType->replaceCovariantResultType(anyObjectTy, 0)
                             ->getCanonicalType();
    }
  }

  // Adjust the ExtInfo by using a Swift representation.
  auto extInfo = completeMethodTy->getExtInfo()
                   .withRepresentation(FunctionTypeRepresentation::Swift);

  auto fnType = CanFunctionType::get(params, resultType, extInfo);
  return fnType;
}

/// Retrieve the type to use for a method found via dynamic lookup.
static SILType
getDynamicMethodLoweredType(SILModule &M,
                            SILDeclRef constant,
                            CanAnyFunctionType substMemberTy) {
  assert(constant.isForeign);
  auto objcFormalTy = substMemberTy.withExtInfo(substMemberTy->getExtInfo()
             .withSILRepresentation(SILFunctionTypeRepresentation::ObjCMethod));
  return SILType::getPrimitiveObjectType(
      M.Types.getUncachedSILFunctionTypeForConstant(constant, objcFormalTy));
}

/// Check if we can perform a dynamic dispatch on a super method call.
static bool canUseStaticDispatch(SILGenFunction &SGF,
                                 SILDeclRef constant) {
  auto *funcDecl = cast<AbstractFunctionDecl>(constant.getDecl());

  if (funcDecl->isFinal())
    return true;
  // Extension methods currently must be statically dispatched, unless they're
  // @objc or dynamic.
  if (funcDecl->getDeclContext()->isExtensionContext()
      && !constant.isForeign)
    return true;

  // We cannot form a direct reference to a method body defined in
  // Objective-C.
  if (constant.isForeign)
    return false;

  // If we cannot form a direct reference due to resilience constraints,
  // we have to dynamic dispatch.
  if (SGF.F.isSerialized())
    return false;

  // If the method is defined in the same module, we can reference it
  // directly.
  auto thisModule = SGF.SGM.M.getSwiftModule();
  if (thisModule == funcDecl->getModuleContext())
    return true;

  // Otherwise, we must dynamic dispatch.
  return false;
}

static SILValue getOriginalSelfValue(SILValue selfValue) {
  if (auto *TTOI = dyn_cast<ThickToObjCMetatypeInst>(selfValue))
    selfValue = TTOI->getOperand();

  if (auto *BBI = dyn_cast<BeginBorrowInst>(selfValue))
    selfValue = BBI->getOperand();

  while (auto *UI = dyn_cast<UpcastInst>(selfValue))
    selfValue = UI->getOperand();

  if (auto *UTBCI = dyn_cast<UncheckedTrivialBitCastInst>(selfValue))
    selfValue = UTBCI->getOperand();

  return selfValue;
}

/// Borrow self and then upcast self to its original type. If self is a
/// metatype, we just return the original metatype since metatypes are trivial.
static ManagedValue borrowedCastToOriginalSelfType(SILGenFunction &SGF,
                                                   SILLocation loc,
                                                   ManagedValue self) {
  SILValue originalSelf = getOriginalSelfValue(self.getValue());
  SILType originalSelfType = originalSelf->getType();

  // If we have a metatype, then we just return the original self value since
  // metatypes are trivial, so we can avoid ownership concerns.
  if (originalSelfType.is<AnyMetatypeType>()) {
    assert(originalSelfType.isTrivial(SGF.getModule()) &&
           "Metatypes should always be trivial");
    return ManagedValue::forUnmanaged(originalSelf);
  }

  // Otherwise, we have a non-metatype. Use a borrow+unchecked_ref_cast.
  return SGF.B.createUncheckedRefCast(loc, self.borrow(SGF, loc),
                                      originalSelfType);
}

static ManagedValue convertOwnershipConventionGivenParamInfo(SILGenFunction &SGF,
                                                             SILParameterInfo param,
                                                             ManagedValue value,
                                                             SILLocation loc) {
  if (param.isConsumed() &&
      value.getOwnershipKind() == ValueOwnershipKind::Guaranteed) {
    return value.copyUnmanaged(SGF, loc);
  }

  if (SGF.F.getModule().getOptions().EnableSILOwnership &&
      value.getOwnershipKind() == ValueOwnershipKind::Owned) {
    if (param.isDirectGuaranteed() || (!SGF.silConv.useLoweredAddresses() &&
                                       param.isIndirectInGuaranteed())) {
      return value.borrow(SGF, loc);
    }
  }

  return value;
}

static void convertOwnershipConventionsGivenParamInfos(
    SILGenFunction &SGF,
    ArrayRef<SILParameterInfo> params,
    ArrayRef<ManagedValue> values,
    SILLocation loc,
    llvm::SmallVectorImpl<ManagedValue> &outVar) {
  assert(params.size() == values.size() &&
         "Different number of params from arguments");
  transform(indices(params), std::back_inserter(outVar),
            [&](unsigned i) -> ManagedValue {
              return convertOwnershipConventionGivenParamInfo(SGF, params[i], values[i], loc);
            });
}

//===----------------------------------------------------------------------===//
//                                   Callee
//===----------------------------------------------------------------------===//

namespace {

/// Abstractly represents a callee, which may be a constant or function value,
/// and knows how to perform dynamic dispatch and reference the appropriate
/// entry point at any valid uncurry level.
class Callee {
public:
  enum class Kind {
    /// An indirect function value.
    IndirectValue,

    /// A direct standalone function call, referenceable by a FunctionRefInst.
    StandaloneFunction,

    /// Enum case constructor call.
    EnumElement,

    /// A method call using class method dispatch.
    ClassMethod,

    /// A method call using super method dispatch.
    SuperMethod,

    /// A method call using protocol witness table dispatch.
    WitnessMethod,

    /// A method call using dynamic lookup.
    DynamicMethod,
  };

  const Kind kind;

  bool isMaterializeForSet = false;

  // Move, don't copy.
  Callee(const Callee &) = delete;
  Callee &operator=(const Callee &) = delete;

private:
  /// An IndirectValue callee represents something like a swift closure or a c
  /// function pointer where we have /no/ information at all on what the callee
  /// is. This contrasts with a class method, where we may not know the exact
  /// method that is being called, but we have some information from the type
  /// system that we have an actual method.
  ///
  /// *NOTE* This will never be non-null if Constant is non-null.
  ManagedValue IndirectValue;

  /// If we are trying to call a specific method or function, this field is set
  /// to the decl ref information for that callee.
  ///
  /// *NOTE* This should never be non-null if IndirectValue is non-null.
  SILDeclRef Constant;

  /// The abstraction pattern of the callee.
  AbstractionPattern OrigFormalInterfaceType;

  /// The callee's formal type with substitutions applied.
  CanFunctionType SubstFormalInterfaceType;

  /// The substitutions applied to OrigFormalInterfaceType to produce
  /// SubstFormalInterfaceType.
  SubstitutionMap Substitutions;

  /// The list of values captured by our callee.
  Optional<SmallVector<ManagedValue, 2>> Captures;

  // The pointer back to the AST node that produced the callee.
  SILLocation Loc;

  static CanFunctionType
  getSubstFormalInterfaceType(CanAnyFunctionType substFormalType,
                              SubstitutionMap subs) {
    if (auto *gft = substFormalType->getAs<GenericFunctionType>()) {
      return cast<FunctionType>(
        gft->substGenericArgs(subs)
          ->getCanonicalType());
    }

    return cast<FunctionType>(substFormalType);
  }

  /// Constructor for Callee::forIndirect.
  Callee(ManagedValue indirectValue,
         AbstractionPattern origFormalType,
         CanFunctionType substFormalType,
         SILLocation l)
    : kind(Kind::IndirectValue),
      IndirectValue(indirectValue),
      OrigFormalInterfaceType(origFormalType),
      SubstFormalInterfaceType(substFormalType),
      Loc(l)
  {}

  /// Constructor for Callee::forDirect.
  Callee(SILGenFunction &SGF, SILDeclRef standaloneFunction,
         AbstractionPattern origFormalType,
         CanAnyFunctionType substFormalType,
         SubstitutionMap subs, SILLocation l)
    : kind(Kind::StandaloneFunction), Constant(standaloneFunction),
      OrigFormalInterfaceType(origFormalType),
      SubstFormalInterfaceType(getSubstFormalInterfaceType(substFormalType,
                                                           subs)),
      Substitutions(subs),
      Loc(l)
  {
  }

  /// Constructor called by all for* factory methods except forDirect and
  /// forIndirect.
  Callee(Kind methodKind, SILGenFunction &SGF, SILDeclRef methodName,
         AbstractionPattern origFormalType, CanAnyFunctionType substFormalType,
         SubstitutionMap subs, SILLocation l)
      : kind(methodKind), Constant(methodName),
        OrigFormalInterfaceType(origFormalType),
        SubstFormalInterfaceType(
            getSubstFormalInterfaceType(substFormalType, subs)),
        Substitutions(subs), Loc(l) {}

public:

  static Callee forIndirect(ManagedValue indirectValue,
                            AbstractionPattern origFormalType,
                            CanFunctionType substFormalType,
                            SILLocation l) {
    return Callee(indirectValue, origFormalType, substFormalType, l);
  }
  static Callee forDirect(SILGenFunction &SGF, SILDeclRef c,
                          SubstitutionMap subs,
                          SILLocation l) {
    auto &ci = SGF.getConstantInfo(c);
    return Callee(SGF, c, ci.FormalPattern, ci.FormalType, subs, l);
  }
  static Callee forEnumElement(SILGenFunction &SGF, SILDeclRef c,
                               SubstitutionMap subs,
                               SILLocation l) {
    assert(isa<EnumElementDecl>(c.getDecl()));
    auto &ci = SGF.getConstantInfo(c);
    return Callee(Kind::EnumElement, SGF, c, ci.FormalPattern,
                  ci.FormalType, subs, l);
  }
  static Callee forClassMethod(SILGenFunction &SGF,
                               SILDeclRef c, SubstitutionMap subs,
                               SILLocation l) {
    auto base = c.getOverriddenVTableEntry();
    auto &baseCI = SGF.getConstantInfo(base);
    auto &derivedCI = SGF.getConstantInfo(c);
    return Callee(Kind::ClassMethod, SGF, c,
                  baseCI.FormalPattern, derivedCI.FormalType, subs, l);
  }
  static Callee forSuperMethod(SILGenFunction &SGF,
                               SILDeclRef c, SubstitutionMap subs,
                               SILLocation l) {
    auto &ci = SGF.getConstantInfo(c);
    return Callee(Kind::SuperMethod, SGF, c,
                  ci.FormalPattern, ci.FormalType, subs, l);
  }
  static Callee forWitnessMethod(SILGenFunction &SGF,
                                 CanType protocolSelfType,
                                 SILDeclRef c,
                                 SubstitutionMap subs,
                                 SILLocation l) {
    auto &ci = SGF.getConstantInfo(c);
    return Callee(Kind::WitnessMethod, SGF, c, ci.FormalPattern,
                  ci.FormalType, subs, l);
  }
  static Callee forDynamic(SILGenFunction &SGF,
                           SILDeclRef c, SubstitutionMap constantSubs,
                           CanAnyFunctionType substFormalType,
                           SubstitutionMap subs, SILLocation l) {
    auto &ci = SGF.getConstantInfo(c);
    AbstractionPattern origFormalType = ci.FormalPattern;

    // Replace the original self type with the partially-applied subst type.
    auto origFormalFnType = cast<AnyFunctionType>(origFormalType.getType());
    if (auto genericFnType = dyn_cast<GenericFunctionType>(origFormalFnType)) {
      // If we have a generic function type, substitute it.  This is normally
      // a huge no-no, but the partial-application hacks we're doing here
      // really kindof mandate it, and it works out because we're always using
      // a foreign function.  If/when we support native dynamic functions,
      // this will stop working and we will need a completely different
      // approach.
      origFormalFnType =
        cast<FunctionType>(genericFnType->substGenericArgs(constantSubs)
                                        ->getCanonicalType());
    }
    origFormalType.rewriteType(CanGenericSignature(), origFormalFnType);

    return Callee(Kind::DynamicMethod, SGF, c, origFormalType,
                  substFormalType, subs, l);
  }

  Callee(Callee &&) = default;
  Callee &operator=(Callee &&) = default;

  void setCaptures(SmallVectorImpl<ManagedValue> &&captures) {
    Captures = std::move(captures);
  }
  
  ArrayRef<ManagedValue> getCaptures() const {
    if (Captures)
      return *Captures;
    return {};
  }
  
  bool hasCaptures() const {
    return Captures.hasValue();
  }

  AbstractionPattern getOrigFormalType() const {
    return AbstractionPattern(OrigFormalInterfaceType);
  }

  CanFunctionType getSubstFormalType() const {
    return SubstFormalInterfaceType;
  }

  unsigned getParameterListCount() const {
    switch (kind) {
    case Kind::IndirectValue:
      return 1;

    case Kind::StandaloneFunction:
    case Kind::EnumElement:
    case Kind::ClassMethod:
    case Kind::SuperMethod:
    case Kind::WitnessMethod:
    case Kind::DynamicMethod:
      return Constant.getParameterListCount();
    }

    llvm_unreachable("Unhandled Kind in switch.");
  }

  bool requiresSelfValueForDispatch() const {
    switch (kind) {
    case Kind::IndirectValue:
    case Kind::StandaloneFunction:
    case Kind::EnumElement:
      return false;
    case Kind::WitnessMethod:
      if (Constant.isForeign)
        return true;
      return false;
    case Kind::ClassMethod:
    case Kind::SuperMethod:
    case Kind::DynamicMethod:
      return true;
    }

    llvm_unreachable("Unhandled Kind in switch.");
  }

  EnumElementDecl *getEnumElementDecl() {
    assert(kind == Kind::EnumElement);
    return cast<EnumElementDecl>(Constant.getDecl());
  }

  CalleeTypeInfo createCalleeTypeInfo(SILGenFunction &SGF,
                                      Optional<SILDeclRef> constant,
                                      SILType formalFnType) const & {
    CalleeTypeInfo result;

    result.substFnType =
        formalFnType.castTo<SILFunctionType>()->substGenericArgs(SGF.SGM.M,
                                                                 Substitutions);

    if (!constant || !constant->isForeign)
      return result;

    auto func = cast<AbstractFunctionDecl>(constant->getDecl());
    result.foreignError = func->getForeignErrorConvention();
    result.foreignSelf = func->getImportAsMemberStatus();

    return result;
  }

  SILDeclRef getCurriedConstant(bool isCurried) const {
    if (isCurried) {
      auto constant = Constant.asCurried();

      // If we're currying a direct reference to a class-dispatched method,
      // make sure we emit the right set of thunks.
      if (kind == Kind::StandaloneFunction) {
        if (auto func = Constant.getAbstractFunctionDecl()) {
          if (getMethodDispatch(func) == MethodDispatch::Class) {
            return constant.asDirectReference(true);
          }
        }
      }

      return constant;
    }

    return Constant;
  }

  ManagedValue getFnValue(SILGenFunction &SGF, bool isCurried,
                          Optional<ManagedValue> borrowedSelf) const & {
    Optional<SILDeclRef> constant = None;

    if (!Constant) {
      assert(!isCurried && "can't curry indirect function");
    } else {
      constant = getCurriedConstant(isCurried);

      // If the call is curried, emit a direct call to the curry thunk.
      if (constant->isCurried) {
        auto constantInfo = SGF.getConstantInfo(*constant);
        SILValue ref = SGF.emitGlobalFunctionRef(Loc, *constant, constantInfo);
        return ManagedValue::forUnmanaged(ref);
      }
    }

    switch (kind) {
    case Kind::IndirectValue:
      assert(Substitutions.empty());
      return IndirectValue;

    case Kind::StandaloneFunction: {
      auto constantInfo = SGF.getConstantInfo(*constant);
      SILValue ref = SGF.emitGlobalFunctionRef(Loc, *constant, constantInfo);
      return ManagedValue::forUnmanaged(ref);
    }
    case Kind::EnumElement:
      llvm_unreachable("Should have been curried");
    case Kind::ClassMethod: {
      auto methodTy = SGF.SGM.Types.getConstantOverrideType(*constant);

      // Otherwise, do the dynamic dispatch inline.
      Scope S(SGF, Loc);

      SILValue methodVal;
      if (!constant->isForeign) {
        methodVal = SGF.emitClassMethodRef(
            Loc, borrowedSelf->getValue(), *constant, methodTy);
      } else {
        methodVal = SGF.B.createObjCMethod(
            Loc, borrowedSelf->getValue(), *constant,
            SILType::getPrimitiveObjectType(methodTy));
      }
      return ManagedValue::forUnmanaged(methodVal);
    }
    case Kind::SuperMethod: {
      assert(!constant->isCurried);

      Scope S(SGF, Loc);
      ManagedValue castValue = borrowedCastToOriginalSelfType(
        SGF, Loc, *borrowedSelf);

      auto base = constant->getOverriddenVTableEntry();
      auto constantInfo =
          SGF.SGM.Types.getConstantOverrideInfo(*constant, base);

      if (!constant->isForeign) {
        return SGF.B.createSuperMethod(Loc, castValue, *constant,
                                       constantInfo.getSILType());
      } else {
        return SGF.B.createObjCSuperMethod(Loc, castValue, *constant,
                                           constantInfo.getSILType());
      }
    }
    case Kind::WitnessMethod: {
      auto constantInfo = SGF.getConstantInfo(*constant);

      auto proto = cast<ProtocolDecl>(Constant.getDecl()->getDeclContext());
      auto selfType = proto->getSelfInterfaceType()->getCanonicalType();
      auto lookupType = selfType.subst(Substitutions)->getCanonicalType();
      auto conformance = *Substitutions.lookupConformance(selfType, proto);

      SILValue fn;

      if (!constant->isForeign) {

        fn = SGF.B.createWitnessMethod(
          Loc, lookupType, conformance, *constant,
          constantInfo.getSILType());
      } else {
        fn = SGF.B.createObjCMethod(Loc, borrowedSelf->getValue(),
                                    *constant, constantInfo.getSILType());
      }

      return ManagedValue::forUnmanaged(fn);
    }
    case Kind::DynamicMethod: {
      auto closureType = getDynamicMethodLoweredType(
          SGF.SGM.M, *constant, getSubstFormalType());

      Scope S(SGF, Loc);
      SILValue fn = SGF.B.createObjCMethod(
          Loc, borrowedSelf->getValue(), *constant,
          closureType);
      return ManagedValue::forUnmanaged(fn);
    }
    }
  }

  CalleeTypeInfo getTypeInfo(SILGenFunction &SGF, bool isCurried) const & {
    Optional<SILDeclRef> constant = None;

    if (!Constant) {
      assert(!isCurried && "can't curry indirect function");
    } else {
      constant = getCurriedConstant(isCurried);

      // If the call is curried, emit a direct call to the curry thunk.
      if (constant->isCurried) {
        auto constantInfo = SGF.getConstantInfo(*constant);
        return createCalleeTypeInfo(SGF, constant, constantInfo.getSILType());
      }
    }

    switch (kind) {
    case Kind::IndirectValue:
      assert(Substitutions.empty());
      return createCalleeTypeInfo(SGF, constant, IndirectValue.getType());

    case Kind::StandaloneFunction: {
      auto constantInfo = SGF.getConstantInfo(*constant);
      return createCalleeTypeInfo(SGF, constant, constantInfo.getSILType());
    }
    case Kind::EnumElement:
      llvm_unreachable("Should have been curried");
    case Kind::ClassMethod: {
      auto constantInfo = SGF.SGM.Types.getConstantOverrideInfo(*constant);
      return createCalleeTypeInfo(SGF, constant, constantInfo.getSILType());
    }
    case Kind::SuperMethod: {
      auto base = constant->getOverriddenVTableEntry();
      auto constantInfo =
          SGF.SGM.Types.getConstantOverrideInfo(*constant, base);
      return createCalleeTypeInfo(SGF, constant, constantInfo.getSILType());
    }
    case Kind::WitnessMethod: {
      auto constantInfo = SGF.getConstantInfo(*constant);
      return createCalleeTypeInfo(SGF, constant, constantInfo.getSILType());
    }
    case Kind::DynamicMethod: {
      auto formalType = getDynamicMethodLoweredType(
          SGF.SGM.M, *constant, getSubstFormalType());
      return createCalleeTypeInfo(SGF, constant, formalType);
    }
    }
  }

  SubstitutionMap getSubstitutions() const {
    return Substitutions;
  }

  SILDeclRef getMethodName() const {
    return Constant;
  }

  /// Return a specialized emission function if this is a function with a known
  /// lowering, such as a builtin, or return null if there is no specialized
  /// emitter.
  Optional<SpecializedEmitter>
  getSpecializedEmitter(SILGenModule &SGM) const {
    switch (kind) {
    case Kind::StandaloneFunction: {
      return SpecializedEmitter::forDecl(SGM, Constant);
    }
    case Kind::EnumElement:
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

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                           SILGenApply ASTVisitor
//===----------------------------------------------------------------------===//

/// For ObjC init methods, we generate a shared-linkage Swift allocating entry
/// point that does the [[T alloc] init] dance. We want to use this native
/// thunk where we expect to be calling an allocating entry point for an ObjC
/// constructor.
static bool isConstructorWithGeneratedAllocatorThunk(ValueDecl *vd) {
  return vd->isObjC() && isa<ConstructorDecl>(vd);
}

namespace {

/// An ASTVisitor for decomposing a nesting of ApplyExprs into an initial
/// Callee and a list of CallSites. The CallEmission class below uses these
/// to generate the actual SIL call.
///
/// Formally, an ApplyExpr in the AST always has a single argument, which may
/// be of tuple type, possibly empty. Also, some callees have a formal type
/// which is curried -- for example, methods have type Self -> Arg -> Result.
///
/// However, SIL functions take zero or more parameters and the natural entry
/// point of a method takes Self as an additional argument, rather than
/// returning a partial application.
///
/// Therefore, nested ApplyExprs applied to a constant are flattened into a
/// single call of the most uncurried entry point fitting the call site.
/// This avoids intermediate closure construction.
///
/// For example, a method reference 'self.method' decomposes into curry thunk
/// as the callee, with a single call site '(self)'.
///
/// On the other hand, a call of a method 'self.method(x)(y)' with a function
/// return type decomposes into the method's natural entry point as the callee,
/// and two call sites, first '(x, self)' then '(y)'.
class SILGenApply : public Lowering::ExprVisitor<SILGenApply> {
public:
  /// The SILGenFunction that we are emitting SIL into.
  SILGenFunction &SGF;

  /// The apply callee that abstractly represents the entry point that is being
  /// called.
  Optional<Callee> applyCallee;

  /// The lvalue or rvalue representing the argument source of self.
  ArgumentSource selfParam;
  Type selfType;
  std::vector<ApplyExpr*> callSites;
  Expr *sideEffect = nullptr;

  SILGenApply(SILGenFunction &SGF)
    : SGF(SGF)
  {}

  void setCallee(Callee &&c) {
    assert(!applyCallee && "already set callee!");
    applyCallee.emplace(std::move(c));
  }

  void setSideEffect(Expr *sideEffectExpr) {
    assert(!sideEffect && "already set side effect!");
    sideEffect = sideEffectExpr;
  }

  void setSelfParam(ArgumentSource &&theSelfParam, Expr *theSelfApplyExpr) {
    assert(!selfParam && "already set this!");
    selfParam = std::move(theSelfParam);
    selfType = theSelfApplyExpr->getType();
  }

  void decompose(Expr *e) {
    visit(e);
  }

  /// Fall back to an unknown, indirect callee.
  void visitExpr(Expr *e) {
    // TODO: preserve the function pointer at its original abstraction level
    // when loading from memory.

    ManagedValue fn = SGF.emitRValueAsSingleValue(e);
    auto substType = cast<FunctionType>(e->getType()->getCanonicalType());

    // When calling an C or block function, there's implicit bridging.
    auto origType = getIndirectApplyAbstractionPattern(SGF, substType);

    setCallee(Callee::forIndirect(fn, origType, substType, e));
  }

  /// Add a call site to the curry.
  void visitApplyExpr(ApplyExpr *e) {
    if (e->isSuper()) {
      applySuper(e);
      return;
    }

    if (applyInitDelegation(e))
      return;

    callSites.push_back(e);
    visit(e->getFn());
  }

  /// Idempotently convert a metatype to an objc metatype.
  std::pair<ManagedValue, SILType> convertToObjCMetatype(ManagedValue selfMeta,
                                                         SILLocation loc) {
    auto metaType = selfMeta.getType().castTo<AnyMetatypeType>();
    CanType instanceType = metaType.getInstanceType();

    // If we are already objc, just return.
    if (metaType->getRepresentation() == MetatypeRepresentation::ObjC) {
      return {selfMeta, SGF.SGM.getLoweredType(instanceType)};
    }

    CanAnyMetatypeType objcMetaType;
    if (isa<MetatypeType>(metaType)) {
      objcMetaType =
          CanMetatypeType::get(instanceType, MetatypeRepresentation::ObjC);
    } else {
      objcMetaType = CanExistentialMetatypeType::get(
          instanceType, MetatypeRepresentation::ObjC);
    }
    // ObjC metatypes are trivial and thus do not have a cleanup. Only if we
    // convert them to an object do they become non-trivial.
    assert(!selfMeta.hasCleanup());
    auto result = ManagedValue::forUnmanaged(SGF.B.emitThickToObjCMetatype(
        loc, selfMeta.getValue(), SGF.SGM.getLoweredType(objcMetaType)));
    return {result, SGF.SGM.getLoweredType(instanceType)};
  }

  /// Given a metatype value for the type, allocate an Objective-C
  /// object (with alloc_ref_dynamic) of that type.
  ///
  /// \returns the self object.
  ManagedValue allocateObjCObject(ManagedValue selfMeta, SILLocation loc) {
    // Convert to an Objective-C metatype representation, if needed.
    ManagedValue selfMetaObjC;
    SILType instanceType;
    std::tie(selfMetaObjC, instanceType) = convertToObjCMetatype(selfMeta, loc);

    // Allocate the object.
    return SGF.B.createAllocRefDynamic(loc, selfMetaObjC, instanceType,
                                       /*objc=*/true, {}, {});
  }

  void processProtocolMethod(DeclRefExpr *e, AbstractFunctionDecl *afd,
                             ProtocolDecl *proto) {
    assert(!callSites.empty());
    ApplyExpr *thisCallSite = callSites.back();
    callSites.pop_back();

    ArgumentSource selfValue = thisCallSite->getArg();

    auto subs = e->getDeclRef().getSubstitutions();

    SILDeclRef::Kind kind = SILDeclRef::Kind::Func;
    if (isa<ConstructorDecl>(afd)) {
      if (proto->isObjC()) {
        SILLocation loc = thisCallSite->getArg();

        // For Objective-C initializers, we only have an initializing
        // initializer. We need to allocate the object ourselves.
        kind = SILDeclRef::Kind::Initializer;

        auto metatype = std::move(selfValue).getAsSingleValue(SGF);
        auto allocated = allocateObjCObject(metatype, loc);
        auto allocatedType = allocated.getType().getASTType();
        selfValue =
            ArgumentSource(loc, RValue(SGF, loc, allocatedType, allocated));
      } else {
        // For non-Objective-C initializers, we have an allocating
        // initializer to call.
        kind = SILDeclRef::Kind::Allocator;
      }
    }

    SILDeclRef constant(afd, kind);
    constant = constant.asForeign(afd->isObjC());

    // Prepare the callee.
    Callee theCallee = Callee::forWitnessMethod(
        SGF, selfValue.getSubstRValueType(),
        constant, subs, e);

    setSelfParam(std::move(selfValue), thisCallSite);
    setCallee(std::move(theCallee));
  }

  bool isClassMethod(DeclRefExpr *e, AbstractFunctionDecl *afd) {
    if (e->getAccessSemantics() != AccessSemantics::Ordinary)
      return false;

    if (getMethodDispatch(afd) == MethodDispatch::Static)
      return false;

    if (auto ctor = dyn_cast<ConstructorDecl>(afd)) {
      // Non-required initializers are statically dispatched.
      if (!ctor->isRequired())
        return false;

      // Required constructors are statically dispatched when the 'self'
      // value is statically derived.
      ApplyExpr *thisCallSite = callSites.back();
      assert(thisCallSite->getArg()->getType()->is<AnyMetatypeType>());
      if (thisCallSite->getArg()->isStaticallyDerivedMetatype())
        return false;
    }

    // Ok, we're dynamically dispatched.
    return true;
  }

  void processClassMethod(DeclRefExpr *e, AbstractFunctionDecl *afd) {
    SILDeclRef::Kind kind;
    bool requiresAllocRefDynamic = false;

    if (isa<FuncDecl>(afd)) {
      kind = SILDeclRef::Kind::Func;
    } else {
      if (requiresForeignEntryPoint(afd)) {
        // When we're performing Objective-C dispatch, we don't have an
        // allocating constructor to call. So, perform an alloc_ref_dynamic
        // and pass that along to the initializer.
        requiresAllocRefDynamic = true;
        kind = SILDeclRef::Kind::Initializer;
      } else {
        kind = SILDeclRef::Kind::Allocator;
      }
    }

    ApplyExpr *thisCallSite = callSites.back();
    callSites.pop_back();

    // If we require a dynamic allocation of the object here, do so now.
    if (requiresAllocRefDynamic) {
      SILLocation loc = thisCallSite->getArg();
      RValue selfMetatype = SGF.emitRValue(thisCallSite->getArg());
      auto selfValue =
          allocateObjCObject(std::move(selfMetatype).getAsSingleValue(SGF, loc), loc);
      RValue self = RValue(SGF, loc, selfValue.getType().getASTType(),
                           selfValue);
      ArgumentSource selfArgSource(thisCallSite->getArg(), std::move(self));
      setSelfParam(std::move(selfArgSource), thisCallSite);
    } else {
      ArgumentSource selfArgSource(thisCallSite->getArg());
      setSelfParam(std::move(selfArgSource), thisCallSite);
    }

    auto constant = SILDeclRef(afd, kind)
                        .asForeign(requiresForeignEntryPoint(afd));

    auto subs = e->getDeclRef().getSubstitutions();
    setCallee(Callee::forClassMethod(SGF, constant, subs, e));
  }

  //
  // Known callees.
  //
  void visitDeclRefExpr(DeclRefExpr *e) {
    auto subs = e->getDeclRef().getSubstitutions();

    // If this is a direct reference to a vardecl, just emit its value directly.
    // Recursive references to callable declarations are allowed.
    if (isa<VarDecl>(e->getDecl())) {
      visitExpr(e);
      return;
    }

    // Enum case constructor references are open-coded.
    if (auto *eed = dyn_cast<EnumElementDecl>(e->getDecl())) {
      setCallee(Callee::forEnumElement(SGF, SILDeclRef(eed), subs, e));
      return;
    }

    // Ok, we have a constructor or a function.
    auto *afd = cast<AbstractFunctionDecl>(e->getDecl());

    // Witness method or @objc protocol dispatch.
    if (auto *proto = dyn_cast<ProtocolDecl>(afd->getDeclContext())) {
      processProtocolMethod(e, afd, proto);
      return;
    }

    // VTable class method or @objc class method dispatch.
    if (isClassMethod(e, afd)) {
      processClassMethod(e, afd);
      return;
    }

    // Otherwise, we have a statically-dispatched call.
    auto constant = SILDeclRef(e->getDecl())
      .asForeign(!isConstructorWithGeneratedAllocatorThunk(e->getDecl())
                 && requiresForeignEntryPoint(e->getDecl()));

    auto captureInfo = SGF.SGM.Types.getLoweredLocalCaptures(afd);
    if (afd->getDeclContext()->isLocalContext() &&
        !captureInfo.hasGenericParamCaptures())
      subs = SubstitutionMap();

    setCallee(Callee::forDirect(SGF, constant, subs, e));
    
    // If the decl ref requires captures, emit the capture params.
    if (!captureInfo.getCaptures().empty()) {
      SmallVector<ManagedValue, 4> captures;
      SGF.emitCaptures(e, afd, CaptureEmission::ImmediateApplication,
                       captures);
      applyCallee->setCaptures(std::move(captures));
    }
  }
  
  void visitAbstractClosureExpr(AbstractClosureExpr *e) {
    // Emit the closure body.
    SGF.SGM.emitClosure(e);

    // If we're in top-level code, we don't need to physically capture script
    // globals, but we still need to mark them as escaping so that DI can flag
    // uninitialized uses.
    if (&SGF == SGF.SGM.TopLevelSGF) {
      SGF.SGM.emitMarkFunctionEscapeForTopLevelCodeGlobals(e,e->getCaptureInfo());
    }
    
    // A directly-called closure can be emitted as a direct call instead of
    // really producing a closure object.
    SILDeclRef constant(e);

    SubstitutionMap subs;
    if (e->getCaptureInfo().hasGenericParamCaptures())
      subs = SGF.getForwardingSubstitutionMap();

    setCallee(Callee::forDirect(SGF, constant, subs, e));
    
    // If the closure requires captures, emit them.
    bool hasCaptures = SGF.SGM.M.Types.hasLoweredLocalCaptures(e);
    if (hasCaptures) {
      SmallVector<ManagedValue, 4> captures;
      SGF.emitCaptures(e, e, CaptureEmission::ImmediateApplication,
                       captures);
      applyCallee->setCaptures(std::move(captures));
    }
  }
  
  void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *e) {
    auto subs = e->getDeclRef().getSubstitutions();

    // FIXME: We might need to go through ObjC dispatch for references to
    // constructors imported from Clang (which won't have a direct entry point)
    // or to delegate to a designated initializer.
    setCallee(Callee::forDirect(SGF,
                SILDeclRef(e->getDecl(), SILDeclRef::Kind::Initializer),
                           subs, e));
  }

  void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e) {
    setSideEffect(e->getLHS());
    visit(e->getRHS());
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

  void visitImplicitlyUnwrappedFunctionConversionExpr(
      ImplicitlyUnwrappedFunctionConversionExpr *e) {
    // These are generated for short term use in the type checker.
    llvm_unreachable(
        "We should not see ImplicitlyUnwrappedFunctionConversionExpr here");
  }

  void visitIdentityExpr(IdentityExpr *e) {
    visit(e->getSubExpr());
  }

  void applySuper(ApplyExpr *apply) {
    // Load the 'super' argument.
    Expr *arg = apply->getArg();
    RValue super;
    CanType superFormalType = arg->getType()->getCanonicalType();

    // The callee for a super call has to be either a method or constructor.
    Expr *fn = apply->getFn();
    SubstitutionMap substitutions;
    SILDeclRef constant;
    if (auto *ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(fn)) {
      constant = SILDeclRef(ctorRef->getDecl(), SILDeclRef::Kind::Initializer)
        .asForeign(requiresForeignEntryPoint(ctorRef->getDecl()));

      if (ctorRef->getDeclRef().isSpecialized())
        substitutions = ctorRef->getDeclRef().getSubstitutions();

      assert(SGF.SelfInitDelegationState ==
             SILGenFunction::WillSharedBorrowSelf);
      SGF.SelfInitDelegationState = SILGenFunction::WillExclusiveBorrowSelf;
      super = SGF.emitRValue(arg);
      assert(SGF.SelfInitDelegationState ==
             SILGenFunction::DidExclusiveBorrowSelf);

      // We know that we have a single ManagedValue rvalue for self.
      ManagedValue superMV = std::move(super).getScalarValue();

      // Check if super is not the same as our base type. This means that we
      // performed an upcast, and we must have consumed the special cleanup
      // we installed.  Install a new special cleanup.
      if (superMV.getValue() != SGF.InitDelegationSelf.getValue()) {
        SILValue underlyingSelf = SGF.InitDelegationSelf.getValue();
        SGF.InitDelegationSelf = ManagedValue::forUnmanaged(underlyingSelf);
        CleanupHandle newWriteback = SGF.enterDelegateInitSelfWritebackCleanup(
            SGF.InitDelegationLoc.getValue(), SGF.InitDelegationSelfBox,
            superMV.forward(SGF));
        SGF.SuperInitDelegationSelf =
            ManagedValue(superMV.getValue(), newWriteback);
        super = RValue(SGF, SGF.InitDelegationLoc.getValue(), superFormalType,
                       SGF.SuperInitDelegationSelf);
      }

    } else if (auto *declRef = dyn_cast<DeclRefExpr>(fn)) {
      assert(isa<FuncDecl>(declRef->getDecl()) && "non-function super call?!");
      constant = SILDeclRef(declRef->getDecl())
        .asForeign(requiresForeignEntryPoint(declRef->getDecl()));

      if (declRef->getDeclRef().isSpecialized())
        substitutions = declRef->getDeclRef().getSubstitutions();
      super = SGF.emitRValue(arg);
    } else {
      llvm_unreachable("invalid super callee");
    }

    assert(super.isComplete() && "At this point super should be a complete "
                                 "rvalue that is not in any special states");
    ArgumentSource superArgSource(arg, std::move(super));
    if (!canUseStaticDispatch(SGF, constant)) {
      // ObjC super calls require dynamic dispatch.
      setCallee(Callee::forSuperMethod(SGF, constant, substitutions, fn));
    } else {
      // Native Swift super calls to final methods are direct.
      setCallee(Callee::forDirect(SGF, constant, substitutions, fn));
    }

    setSelfParam(std::move(superArgSource), apply);
  }

  /// Walk the given \c selfArg expression that produces the appropriate
  /// `self` for a call, applying the same transformations to the provided
  /// \c selfValue (which might be a metatype).
  ///
  /// This is used for initializer delegation, so it covers only the narrow
  /// subset of expressions used there.
  ManagedValue emitCorrespondingSelfValue(ManagedValue selfValue,
                                          Expr *selfArg) {
    while (true) {
      // Handle archetype-to-super and derived-to-base upcasts.
      if (isa<ArchetypeToSuperExpr>(selfArg) ||
          isa<DerivedToBaseExpr>(selfArg)) {
        auto ice = cast<ImplicitConversionExpr>(selfArg);
        auto resultTy = ice->getType()->getCanonicalType();

        // If the 'self' value is a metatype, update the target type
        // accordingly.
        if (auto selfMetaTy = selfValue.getType().getAs<AnyMetatypeType>()) {
          resultTy = CanMetatypeType::get(resultTy,
                                          selfMetaTy->getRepresentation());
        }
        auto loweredResultTy = SGF.getLoweredLoadableType(resultTy);
        if (loweredResultTy != selfValue.getType()) {
          selfValue = SGF.emitManagedRValueWithCleanup(
              SGF.B.createUpcast(ice, selfValue.forward(SGF), loweredResultTy));
        }

        selfArg = ice->getSubExpr();
        continue;
      }

      // Skip over loads.
      if (auto load = dyn_cast<LoadExpr>(selfArg)) {
        selfArg = load->getSubExpr();
        continue;
      }

      // Skip over inout expressions.
      if (auto inout = dyn_cast<InOutExpr>(selfArg)) {
        selfArg = inout->getSubExpr();
        continue;
      }

      // Declaration references terminate the search.
      if (isa<DeclRefExpr>(selfArg))
        break;

      llvm_unreachable("unhandled conversion for metatype value");
    }

    return selfValue;
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
                     ->getAsNominalTypeOrNominalTypeExtensionContext();
    bool useAllocatingCtor;

    // Value types only have allocating initializers.
    if (isa<StructDecl>(nominal) || isa<EnumDecl>(nominal))
      useAllocatingCtor = true;
    // Protocols only witness allocating initializers, except for @objc
    // protocols, which only witness initializing initializers.
    else if (auto proto = dyn_cast<ProtocolDecl>(nominal)) {
      useAllocatingCtor = !proto->isObjC();
    // Factory initializers are effectively "allocating" initializers with no
    // corresponding initializing entry point.
    } else if (ctorRef->getDecl()->isFactoryInit()) {
      useAllocatingCtor = true;
    } else {
      // We've established we're in a class initializer or a protocol extension
      // initializer for a class-bound protocol, In either case, we're
      // delegating initialization, but we only have an instance in the former
      // case.
      assert(isa<ClassDecl>(nominal)
             && "some new kind of init context we haven't implemented");
      useAllocatingCtor = static_cast<bool>(SGF.AllocatorMetatype) &&
                          !ctorRef->getDecl()->isObjC();
    }

    // Load the 'self' argument.
    Expr *arg = expr->getArg();
    ManagedValue self;
    CanType selfFormalType = arg->getType()->getCanonicalType();

    // If we're using the allocating constructor, we need to pass along the
    // metatype.
    if (useAllocatingCtor) {
      selfFormalType = CanMetatypeType::get(
          selfFormalType->getInOutObjectType()->getCanonicalType());

      // If the initializer is a C function imported as a member,
      // there is no 'self' parameter. Mark it undef.
      if (ctorRef->getDecl()->isImportAsMember()) {
        self = SGF.emitUndef(expr, selfFormalType);
      } else if (SGF.AllocatorMetatype) {
        self = emitCorrespondingSelfValue(
            ManagedValue::forUnmanaged(SGF.AllocatorMetatype), arg);
      } else {
        self = ManagedValue::forUnmanaged(SGF.emitMetatypeOfValue(expr, arg));
      }
    } else {
      // If we're in a protocol extension initializer, we haven't allocated
      // "self" yet at this point. Do so. Use alloc_ref_dynamic since we should
      // only ever get here in ObjC protocol extensions currently.
      if (SGF.AllocatorMetatype) {
        assert(ctorRef->getDecl()->isObjC()
               && "only expect to delegate an initializer from an allocator "
                  "in objc protocol extensions");
        
        self = allocateObjCObject(
                        ManagedValue::forUnmanaged(SGF.AllocatorMetatype), arg);

        // Perform any adjustments needed to 'self'.
        self = emitCorrespondingSelfValue(self, arg);
      } else {
        assert(SGF.SelfInitDelegationState ==
               SILGenFunction::WillSharedBorrowSelf);
        SGF.SelfInitDelegationState = SILGenFunction::WillExclusiveBorrowSelf;
        self = SGF.emitRValueAsSingleValue(arg);
        assert(SGF.SelfInitDelegationState ==
               SILGenFunction::DidExclusiveBorrowSelf);
      }
    }

    auto subs = ctorRef->getDeclRef().getSubstitutions();
    ArgumentSource selfArgSource(arg, RValue(SGF, expr, selfFormalType, self));

    SILDeclRef constant(ctorRef->getDecl(),
                        useAllocatingCtor
                         ? SILDeclRef::Kind::Allocator
                         : SILDeclRef::Kind::Initializer);

    constant = constant.asForeign(requiresForeignEntryPoint(ctorRef->getDecl()));

    // Determine the callee. For structs and enums, this is the allocating
    // constructor (because there is no initializing constructor). For protocol
    // default implementations, we also use the allocating constructor, because
    // that's the only thing that's witnessed. For classes,
    // this is the initializing constructor, to which we will dynamically
    // dispatch.
    if (isa<ProtocolDecl>(ctorRef->getDecl()->getDeclContext())) {
      // Look up the witness for the constructor.
      setCallee(Callee::forWitnessMethod(
          SGF, self.getType().getASTType(),
          constant, subs, expr));
    } else if (getMethodDispatch(ctorRef->getDecl())
                 == MethodDispatch::Class) {
      // Dynamic dispatch to the initializer.
      Scope S(SGF, expr);
      setCallee(Callee::forClassMethod(
          SGF, constant, subs, fn));
    } else {
      // Directly call the peer constructor.
      setCallee(Callee::forDirect(SGF, constant, subs, fn));
    }

    setSelfParam(std::move(selfArgSource), expr);

    return true;
  }

  Callee getCallee() {
    assert(applyCallee && "did not find callee?!");
    return std::move(*applyCallee);
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

    auto openExistential = dyn_cast<OpenExistentialExpr>(arg);
    if (openExistential)
      arg = openExistential->getSubExpr();

    auto dynamicMemberRef = dyn_cast<DynamicMemberRefExpr>(arg);
    if (!dynamicMemberRef)
      return false;

    // Since we'll be collapsing this call site, make sure there's another
    // call site that will actually perform the invocation.
    if (callSites.empty())
      return false;

    // Only @objc methods can be forced.
    auto memberRef = dynamicMemberRef->getMember();
    auto *fd = dyn_cast<FuncDecl>(memberRef.getDecl());
    if (!fd || !fd->isObjC())
      return false;

    // Local function that actually emits the dynamic member reference.
    auto emitDynamicMemberRef = [&] {
      // We found it. Emit the base.
      ArgumentSource baseArgSource(dynamicMemberRef->getBase(),
                                   SGF.emitRValue(dynamicMemberRef->getBase()));

      // Determine the type of the method we referenced, by replacing the
      // class type of the 'Self' parameter with AnyObject.
      auto member = SILDeclRef(fd).asForeign();

      auto substFormalType = cast<FunctionType>(dynamicMemberRef->getType()
                                                    ->getCanonicalType()
                                                    .getOptionalObjectType());
      substFormalType = CanFunctionType::get(
        dynamicMemberRef->getBase()->getType()->getCanonicalType(),
        substFormalType, AnyFunctionType::ExtInfo());

      setCallee(Callee::forDynamic(SGF, member,
                                   memberRef.getSubstitutions(),
                                   substFormalType, {}, e));
      setSelfParam(std::move(baseArgSource), dynamicMemberRef);
    };

    // When we have an open existential, open it and then emit the
    // member reference.
    if (openExistential) {
      SGF.emitOpenExistentialExpr(openExistential,
                                  [&](Expr*) { emitDynamicMemberRef(); });
    } else {
      emitDynamicMemberRef();
    }
    return true;
  }
};

} // end anonymous namespace

static RValue emitStringLiteral(SILGenFunction &SGF, Expr *E, StringRef Str,
                                SGFContext C,
                                StringLiteralExpr::Encoding encoding) {
  uint64_t Length;
  bool isASCII = true;
  for (unsigned char c : Str) {
    if (c > 127) {
      isASCII = false;
      break;
    }
  }
  bool useConstantStringBuiltin = false;
  StringLiteralInst::Encoding instEncoding;
  ConstStringLiteralInst::Encoding constInstEncoding;
  switch (encoding) {
  case StringLiteralExpr::UTF8:
    instEncoding = StringLiteralInst::Encoding::UTF8;
    Length = Str.size();
    break;

  case StringLiteralExpr::UTF16: {
    instEncoding = StringLiteralInst::Encoding::UTF16;
    Length = unicode::getUTF16Length(Str);
    break;
  }
  case StringLiteralExpr::UTF8ConstString:
    constInstEncoding = ConstStringLiteralInst::Encoding::UTF8;
    useConstantStringBuiltin = true;
    break;

  case StringLiteralExpr::UTF16ConstString: {
    constInstEncoding = ConstStringLiteralInst::Encoding::UTF16;
    useConstantStringBuiltin = true;
    break;
  }
  case StringLiteralExpr::OneUnicodeScalar: {
    SILType Int32Ty = SILType::getBuiltinIntegerType(32, SGF.getASTContext());
    SILValue UnicodeScalarValue =
        SGF.B.createIntegerLiteral(E, Int32Ty,
                                   unicode::extractFirstUnicodeScalar(Str));
    return RValue(SGF, E, Int32Ty.getASTType(),
                  ManagedValue::forUnmanaged(UnicodeScalarValue));
  }
  }

  // Should we build a constant string literal?
  if (useConstantStringBuiltin) {
    auto *string = SGF.B.createConstStringLiteral(E, Str, constInstEncoding);
    ManagedValue Elts[] = {ManagedValue::forUnmanaged(string)};
    TupleTypeElt TypeElts[] = {Elts[0].getType().getASTType()};
    CanType ty =
        TupleType::get(TypeElts, SGF.getASTContext())->getCanonicalType();
    return RValue(SGF, Elts, ty);
  }

  // The string literal provides the data.
  auto *string = SGF.B.createStringLiteral(E, Str, instEncoding);

  // The length is lowered as an integer_literal.
  auto WordTy = SILType::getBuiltinWordType(SGF.getASTContext());
  auto *lengthInst = SGF.B.createIntegerLiteral(E, WordTy, Length);

  // The 'isascii' bit is lowered as an integer_literal.
  auto Int1Ty = SILType::getBuiltinIntegerType(1, SGF.getASTContext());
  auto *isASCIIInst = SGF.B.createIntegerLiteral(E, Int1Ty, isASCII);

  ManagedValue EltsArray[] = {
    ManagedValue::forUnmanaged(string),
    ManagedValue::forUnmanaged(lengthInst),
    ManagedValue::forUnmanaged(isASCIIInst)
  };

  TupleTypeElt TypeEltsArray[] = {
    EltsArray[0].getType().getASTType(),
    EltsArray[1].getType().getASTType(),
    EltsArray[2].getType().getASTType()
  };

  ArrayRef<ManagedValue> Elts;
  ArrayRef<TupleTypeElt> TypeElts;
  switch (instEncoding) {
  case StringLiteralInst::Encoding::UTF16:
    Elts = llvm::makeArrayRef(EltsArray).slice(0, 2);
    TypeElts = llvm::makeArrayRef(TypeEltsArray).slice(0, 2);
    break;

  case StringLiteralInst::Encoding::UTF8:
    Elts = EltsArray;
    TypeElts = TypeEltsArray;
    break;

  case StringLiteralInst::Encoding::ObjCSelector:
    llvm_unreachable("Objective-C selectors cannot be formed here");
  }

  CanType ty =
    TupleType::get(TypeElts, SGF.getASTContext())->getCanonicalType();
  return RValue(SGF, Elts, ty);
}

/// Emit a raw apply operation, performing no additional lowering of
/// either the arguments or the result.
static void emitRawApply(SILGenFunction &SGF,
                         SILLocation loc,
                         ManagedValue fn,
                         SubstitutionMap subs,
                         ArrayRef<ManagedValue> args,
                         CanSILFunctionType substFnType,
                         ApplyOptions options,
                         ArrayRef<SILValue> indirectResultAddrs,
                         SmallVectorImpl<SILValue> &rawResults) {
  SILFunctionConventions substFnConv(substFnType, SGF.SGM.M);
  // Get the callee value.
  bool isConsumed = substFnType->isCalleeConsumed();
  bool isUnowned = substFnType->isCalleeUnowned();
  SILValue fnValue =
      isUnowned ? fn.getValue()
                : isConsumed ? fn.forward(SGF) : fn.borrow(SGF, loc).getValue();

  SmallVector<SILValue, 4> argValues;

  // Add the buffers for the indirect results if needed.
#ifndef NDEBUG
  assert(indirectResultAddrs.size() == substFnConv.getNumIndirectSILResults());
  unsigned resultIdx = 0;
  for (auto indResultTy : substFnConv.getIndirectSILResultTypes()) {
    assert(indResultTy == indirectResultAddrs[resultIdx++]->getType());
  }
#endif
  argValues.append(indirectResultAddrs.begin(), indirectResultAddrs.end());

  auto inputParams = substFnType->getParameters();
  assert(inputParams.size() == args.size());

  // Gather the arguments.
  for (auto i : indices(args)) {
    auto argValue = (inputParams[i].isConsumed() ? args[i].forward(SGF)
                                                 : args[i].getValue());
#ifndef NDEBUG
    auto inputTy = substFnConv.getSILType(inputParams[i]);
    if (argValue->getType() != inputTy) {
      auto &out = llvm::errs();
      out << "TYPE MISMATCH IN ARGUMENT " << i << " OF APPLY AT ";
      printSILLocationDescription(out, loc, SGF.getASTContext());
      out << "  argument value: ";
      argValue->print(out);
      out << "  parameter type: ";
      inputTy.print(out);
      out << "\n";
      abort();
    }
#endif
    argValues.push_back(argValue);
  }

  auto resultType = substFnConv.getSILResultType();
  auto calleeType = SILType::getPrimitiveObjectType(substFnType);

  // If the function is a coroutine, we need to use 'begin_apply'.
  if (substFnType->isCoroutine()) {
    assert(!substFnType->hasErrorResult());
    auto apply = SGF.B.createBeginApply(loc, fnValue, subs, argValues);
    for (auto result : apply->getAllResults())
      rawResults.push_back(result);
    return;
  }

  // If we don't have an error result, we can make a simple 'apply'.
  if (!substFnType->hasErrorResult()) {
    auto result = SGF.B.createApply(loc, fnValue, calleeType,
                                    resultType, subs, argValues);
    rawResults.push_back(result);

  // Otherwise, we need to create a try_apply.
  } else {
    SILBasicBlock *normalBB = SGF.createBasicBlock();
    auto result =
      normalBB->createPHIArgument(resultType, ValueOwnershipKind::Owned);
    rawResults.push_back(result);

    SILBasicBlock *errorBB =
      SGF.getTryApplyErrorDest(loc, substFnType->getErrorResult(),
                               options & ApplyOptions::DoesNotThrow);

    SGF.B.createTryApply(loc, fnValue, calleeType, subs, argValues,
                         normalBB, errorBB);
    SGF.B.emitBlock(normalBB);
  }
}

static bool hasUnownedInnerPointerResult(CanSILFunctionType fnType) {
  for (auto result : fnType->getResults()) {
    if (result.getConvention() == ResultConvention::UnownedInnerPointer)
      return true;
  }
  return false;
}

//===----------------------------------------------------------------------===//
//                             Argument Emission
//===----------------------------------------------------------------------===//

/// Count the number of SILParameterInfos that are needed in order to
/// pass the given argument.
static unsigned getFlattenedValueCount(AbstractionPattern origType,
                                       CanType substType,
                                       ImportAsMemberStatus foreignSelf) {
  // C functions imported as static methods don't consume any real arguments.
  if (foreignSelf.isStatic())
    return 0;

  // The count is always 1 unless the substituted type is a tuple.
  auto substTuple = dyn_cast<TupleType>(substType);
  if (!substTuple) return 1;

  // If the original type is opaque and the substituted type is
  // materializable, the count is 1 anyway.
  if (origType.isTypeParameter() && substTuple->isMaterializable())
    return 1;

  // Otherwise, add up the elements.
  unsigned count = 0;
  for (auto i : indices(substTuple.getElementTypes())) {
    count += getFlattenedValueCount(origType.getTupleElementType(i),
                                    substTuple.getElementType(i),
                                    ImportAsMemberStatus());
  }
  return count;
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

/// The original argument expression for some sort of complex
/// argument emission.
class OriginalArgument {
  llvm::PointerIntPair<Expr*, 1, bool> ExprAndIsIndirect;

public:
  OriginalArgument() = default;
  OriginalArgument(Expr *expr, bool indirect)
    : ExprAndIsIndirect(expr, indirect) {}

  Expr *getExpr() const { return ExprAndIsIndirect.getPointer(); }
  bool isIndirect() const { return ExprAndIsIndirect.getInt(); }
};
  
/// A possibly-discontiguous slice of function parameters claimed by a
/// function application.
class ClaimedParamsRef {
public:
  static constexpr const unsigned NoSkip = (unsigned)-1;
private:
  ArrayRef<SILParameterInfo> Params;
  
  // The index of the param excluded from this range, if any, or ~0.
  unsigned SkipParamIndex;
  
  friend struct ParamLowering;
  explicit ClaimedParamsRef(ArrayRef<SILParameterInfo> params,
                            unsigned skip)
    : Params(params), SkipParamIndex(skip)
  {
    // Eagerly chop a skipped parameter off either end.
    if (SkipParamIndex == 0) {
      Params = Params.slice(1);
      SkipParamIndex = NoSkip;
    }
    assert(!hasSkip() || SkipParamIndex < Params.size());
  }
  
  bool hasSkip() const {
    return SkipParamIndex != (unsigned)NoSkip;
  }
public:
  ClaimedParamsRef() : Params({}), SkipParamIndex(-1) {}
  explicit ClaimedParamsRef(ArrayRef<SILParameterInfo> params)
    : Params(params), SkipParamIndex(NoSkip)
  {}

  struct iterator : public std::iterator<std::random_access_iterator_tag,
                                         SILParameterInfo>
  {
    const SILParameterInfo *Base;
    unsigned I, SkipParamIndex;
    
    iterator(const SILParameterInfo *Base,
             unsigned I, unsigned SkipParamIndex)
      : Base(Base), I(I), SkipParamIndex(SkipParamIndex)
    {}
    
    iterator &operator++() {
      ++I;
      if (I == SkipParamIndex)
        ++I;
      return *this;
    }
    iterator operator++(int) {
      iterator old(*this);
      ++*this;
      return old;
    }
    iterator &operator--() {
      --I;
      if (I == SkipParamIndex)
        --I;
      return *this;
    }
    iterator operator--(int) {
      iterator old(*this);
      --*this;
      return old;
    }
    
    const SILParameterInfo &operator*() const {
      return Base[I];
    }
    const SILParameterInfo *operator->() const {
      return Base + I;
    }
    
    bool operator==(iterator other) const {
      return Base == other.Base && I == other.I
          && SkipParamIndex == other.SkipParamIndex;
    }
    
    bool operator!=(iterator other) const {
      return !(*this == other);
    }
    
    iterator operator+(std::ptrdiff_t distance) const {
      if (distance > 0)
        return goForward(distance);
      if (distance < 0)
        return goBackward(distance);
      return *this;
    }
    iterator operator-(std::ptrdiff_t distance) const {
      if (distance > 0)
        return goBackward(distance);
      if (distance < 0)
        return goForward(distance);
      return *this;
    }
    std::ptrdiff_t operator-(iterator other) const {
      assert(Base == other.Base && SkipParamIndex == other.SkipParamIndex);
      auto baseDistance = (std::ptrdiff_t)I - (std::ptrdiff_t)other.I;
      if (std::min(I, other.I) < SkipParamIndex &&
          std::max(I, other.I) > SkipParamIndex)
        return baseDistance - 1;
      return baseDistance;
    }
    
    iterator goBackward(unsigned distance) const {
      auto result = *this;
      if (I > SkipParamIndex && I <= SkipParamIndex + distance)
        result.I -= (distance + 1);
      result.I -= distance;
      return result;
    }
    
    iterator goForward(unsigned distance) const {
      auto result = *this;
      if (I < SkipParamIndex && I + distance >= SkipParamIndex)
        result.I += distance + 1;
      result.I += distance;
      return result;
    }
  };
  
  iterator begin() const {
    return iterator{Params.data(), 0, SkipParamIndex};
  }
  
  iterator end() const {
    return iterator{Params.data(), (unsigned)Params.size(), SkipParamIndex};
  }
  
  unsigned size() const {
    return Params.size() - (hasSkip() ? 1 : 0);
  }
  
  bool empty() const { return size() == 0; }
  
  SILParameterInfo front() const { return *begin(); }
  
  ClaimedParamsRef slice(unsigned start) const {
    if (start >= SkipParamIndex)
      return ClaimedParamsRef(Params.slice(start + 1), NoSkip);
    return ClaimedParamsRef(Params.slice(start),
                            hasSkip() ? SkipParamIndex - start : NoSkip);
  }
  ClaimedParamsRef slice(unsigned start, unsigned count) const {
    if (start >= SkipParamIndex)
      return ClaimedParamsRef(Params.slice(start + 1, count), NoSkip);
    unsigned newSkip = SkipParamIndex;
    if (hasSkip())
      newSkip -= start;
    
    if (newSkip < count)
      return ClaimedParamsRef(Params.slice(start, count+1), newSkip);
    return ClaimedParamsRef(Params.slice(start, count), NoSkip);
  }
};

/// A delayed argument.  Call arguments are evaluated in two phases:
/// a formal evaluation phase and a formal access phase.  The primary
/// example of this is an l-value that is passed by reference, where
/// the access to the l-value does not begin until the formal access
/// phase, but there are other examples, generally relating to pointer
/// conversions.
///
/// A DelayedArgument represents the part of evaluating an argument
/// that's been delayed until the formal access phase.
class DelayedArgument {
public:
  enum KindTy {
    /// This is a true inout argument.
    InOut,

    LastLVKindWithoutExtra = InOut,

    /// The l-value needs to be converted to a pointer type.
    LValueToPointer,

    /// An array l-value needs to be converted to a pointer type.
    LValueArrayToPointer,

    LastLVKind = LValueArrayToPointer,

    /// An array r-value needs to be converted to a pointer type.
    RValueArrayToPointer,

    /// A string r-value needs to be converted to a pointer type.
    RValueStringToPointer,
    
    /// A function conversion needs to occur.
    FunctionConversion,
    
    LastRVKind = FunctionConversion,
    
    /// A default argument that needs to be evaluated.
    DefaultArgument,
  };

private:
  KindTy Kind;

  struct LValueStorage {
    LValue LV;
    SILLocation Loc;

    LValueStorage(LValue &&lv, SILLocation loc) : LV(std::move(lv)), Loc(loc) {}
  };
  struct RValueStorage {
    ManagedValue RV;

    RValueStorage(ManagedValue rv) : RV(rv) {}
  };
  struct DefaultArgumentStorage {
    SILLocation loc;
    ConcreteDeclRef defaultArgsOwner;
    unsigned destIndex;
    CanType resultType;
    AbstractionPattern origResultType;
    ClaimedParamsRef paramsToEmit;
    SILFunctionTypeRepresentation functionRepresentation;
    
    DefaultArgumentStorage(SILLocation loc,
                           ConcreteDeclRef defaultArgsOwner,
                           unsigned destIndex,
                           CanType resultType,
                           AbstractionPattern origResultType,
                           ClaimedParamsRef paramsToEmit,
                           SILFunctionTypeRepresentation functionRepresentation)
      : loc(loc), defaultArgsOwner(defaultArgsOwner), destIndex(destIndex),
        resultType(resultType), origResultType(origResultType),
        paramsToEmit(paramsToEmit),
        functionRepresentation(functionRepresentation)
    {}
  };

  using ValueMembers =
    ExternalUnionMembers<RValueStorage, LValueStorage,
                         DefaultArgumentStorage>;
  static ValueMembers::Index getValueMemberIndexForKind(KindTy kind) {
    return (kind <= LastLVKind ? ValueMembers::indexOf<LValueStorage>() :
            kind <= LastRVKind ? ValueMembers::indexOf<RValueStorage>()
                               : ValueMembers::indexOf<DefaultArgumentStorage>());
  }

  /// Storage for either the l-value or the r-value.
  ExternalUnion<KindTy, ValueMembers, getValueMemberIndexForKind> Value;

  LValueStorage &LV() { return Value.get<LValueStorage>(Kind); }
  const LValueStorage &LV() const { return Value.get<LValueStorage>(Kind); }
  RValueStorage &RV() { return Value.get<RValueStorage>(Kind); }
  const RValueStorage &RV() const { return Value.get<RValueStorage>(Kind); }

  /// The original argument expression, which will be emitted down
  /// to the point from which the l-value or r-value was generated.
  OriginalArgument Original;

  using PointerAccessInfo = SILGenFunction::PointerAccessInfo;
  using ArrayAccessInfo = SILGenFunction::ArrayAccessInfo;
  
  using ExtraMembers =
    ExternalUnionMembers<void,
                         ArrayAccessInfo,
                         PointerAccessInfo>;
  static ExtraMembers::Index getExtraMemberIndexForKind(KindTy kind) {
    switch (kind) {
    case LValueToPointer:
      return ExtraMembers::indexOf<PointerAccessInfo>();
    case LValueArrayToPointer:
    case RValueArrayToPointer:
      return ExtraMembers::indexOf<ArrayAccessInfo>();
    default:
      return ExtraMembers::indexOf<void>();
    }
  }

  ExternalUnion<KindTy, ExtraMembers, getExtraMemberIndexForKind> Extra;

public:
  DelayedArgument(KindTy kind, LValue &&lv, SILLocation loc)
      : Kind(kind) {
    assert(kind <= LastLVKindWithoutExtra &&
           "this constructor should only be used for simple l-value kinds");
    Value.emplace<LValueStorage>(Kind, std::move(lv), loc);
  }

  DelayedArgument(KindTy kind, ManagedValue rv, OriginalArgument original)
      : Kind(kind), Original(original) {
    Value.emplace<RValueStorage>(Kind, rv);
  }

  DelayedArgument(SILGenFunction::PointerAccessInfo pointerInfo,
                  LValue &&lv, SILLocation loc, OriginalArgument original)
      : Kind(LValueToPointer), Original(original) {
    Value.emplace<LValueStorage>(Kind, std::move(lv), loc);
    Extra.emplace<PointerAccessInfo>(Kind, pointerInfo);
  }

  DelayedArgument(SILGenFunction::ArrayAccessInfo arrayInfo,
                  LValue &&lv, SILLocation loc, OriginalArgument original)
      : Kind(LValueArrayToPointer), Original(original) {
    Value.emplace<LValueStorage>(Kind, std::move(lv), loc);
    Extra.emplace<ArrayAccessInfo>(Kind, arrayInfo);
  }

  DelayedArgument(KindTy kind,
                  SILGenFunction::ArrayAccessInfo arrayInfo,
                  ManagedValue rv, OriginalArgument original)
      : Kind(kind), Original(original) {
    Value.emplace<RValueStorage>(Kind, rv);
    Extra.emplace<ArrayAccessInfo>(Kind, arrayInfo);
  }
  
  DelayedArgument(SILLocation loc,
                  ConcreteDeclRef defaultArgsOwner,
                  unsigned destIndex,
                  CanType resultType,
                  AbstractionPattern origResultType,
                  ClaimedParamsRef params,
                  SILFunctionTypeRepresentation functionTypeRepresentation)
    : Kind(DefaultArgument) {
    Value.emplace<DefaultArgumentStorage>(Kind, loc, defaultArgsOwner,
                                          destIndex,
                                          resultType,
                                          origResultType, params,
                                          functionTypeRepresentation);
  }

  DelayedArgument(DelayedArgument &&other)
      : Kind(other.Kind), Original(other.Original) {
    Value.moveConstruct(Kind, std::move(other.Value));
    Extra.moveConstruct(Kind, std::move(other.Extra));
  }

  DelayedArgument &operator=(DelayedArgument &&other) {
    Value.moveAssign(Kind, other.Kind, std::move(other.Value));
    Extra.moveAssign(Kind, other.Kind, std::move(other.Extra));
    Kind = other.Kind;
    Original = other.Original;
    return *this;
  }

  ~DelayedArgument() {
    Extra.destruct(Kind);
    Value.destruct(Kind);
  }

  bool isSimpleInOut() const { return Kind == InOut; }
  SILLocation getInOutLocation() const {
    assert(isSimpleInOut());
    return LV().Loc;
  }

  void emit(SILGenFunction &SGF,
            SmallVectorImpl<ManagedValue> &args,
            int &argIndex) {
    switch (Kind) {
    case InOut:
      args[argIndex] = emitInOut(SGF);
      return;
    case LValueToPointer:
    case LValueArrayToPointer:
    case RValueArrayToPointer:
    case RValueStringToPointer:
    case FunctionConversion:
      args[argIndex] = finishOriginalArgument(SGF);
      return;
    case DefaultArgument:
      emitDefaultArgument(SGF, Value.get<DefaultArgumentStorage>(Kind),
                          args, argIndex);
      return;
    }
    llvm_unreachable("bad kind");
  }

private:
  ManagedValue emitInOut(SILGenFunction &SGF) {
    return emitAddress(SGF, AccessKind::ReadWrite);
  }

  ManagedValue emitBorrowIndirect(SILGenFunction &SGF) {
    return emitAddress(SGF, AccessKind::Read);
  }

  ManagedValue emitBorrowDirect(SILGenFunction &SGF) {
    ManagedValue address = emitAddress(SGF, AccessKind::Read);
    return SGF.B.createLoadBorrow(LV().Loc, address);
  }

  ManagedValue emitAddress(SILGenFunction &SGF, AccessKind accessKind) {
    auto tsanKind =
      (accessKind == AccessKind::Read ? TSanKind::None : TSanKind::InoutAccess);
    return SGF.emitAddressOfLValue(LV().Loc, std::move(LV().LV),
                                   accessKind, tsanKind);
  }

  /// Replay the original argument expression.
  ManagedValue finishOriginalArgument(SILGenFunction &SGF) {
    auto results = finishOriginalExpr(SGF, Original.getExpr());
    auto value = results.first; // just let the owner go

    if (Original.isIndirect() && !value.getType().isAddress()) {
      value = value.materialize(SGF, Original.getExpr());
    }

    return value;
  }
  
  void emitDefaultArgument(SILGenFunction &SGF,
                           const DefaultArgumentStorage &info,
                           SmallVectorImpl<ManagedValue> &args,
                           int &argIndex);

  // (value, owner)
  std::pair<ManagedValue, ManagedValue>
  finishOriginalExpr(SILGenFunction &SGF, Expr *expr) {

    // This needs to handle all of the recursive cases from
    // ArgEmission::maybeEmitDelayed.

    expr = expr->getSemanticsProvidingExpr();

    // Handle injections into optionals.
    if (auto inject = dyn_cast<InjectIntoOptionalExpr>(expr)) {
      auto ownedValue =
        finishOriginalExpr(SGF, inject->getSubExpr());
      auto &optionalTL = SGF.getTypeLowering(expr->getType());

      auto optValue = SGF.emitInjectOptional(inject, optionalTL, SGFContext(),
                              [&](SGFContext ctx) { return ownedValue.first; });
      return {optValue, ownedValue.second};
    }

    // Handle try!.
    if (auto forceTry = dyn_cast<ForceTryExpr>(expr)) {
      // Handle throws from the accessor?  But what if the writeback throws?
      SILGenFunction::ForceTryEmission emission(SGF, forceTry);
      return finishOriginalExpr(SGF, forceTry->getSubExpr());
    }

    // Handle optional evaluations.
    if (auto optEval = dyn_cast<OptionalEvaluationExpr>(expr)) {
      return finishOptionalEvaluation(SGF, optEval);
    }

    // Done with the recursive cases.  Make sure we handled everything.
    assert(isa<InOutToPointerExpr>(expr) ||
           isa<ArrayToPointerExpr>(expr) ||
           isa<StringToPointerExpr>(expr) ||
           isa<FunctionConversionExpr>(expr));

    switch (Kind) {
    case InOut:
    case DefaultArgument:
      llvm_unreachable("no original expr to finish in these cases");

    case LValueToPointer:
      return {SGF.emitLValueToPointer(LV().Loc, std::move(LV().LV),
                                      Extra.get<PointerAccessInfo>(Kind)),
              /*owner*/ ManagedValue()};

    case LValueArrayToPointer:
      return SGF.emitArrayToPointer(LV().Loc, std::move(LV().LV),
                                    Extra.get<ArrayAccessInfo>(Kind));

    case RValueArrayToPointer: {
      auto pointerExpr = cast<ArrayToPointerExpr>(expr);
      auto optArrayValue = RV().RV;
      auto arrayValue = emitBindOptionals(SGF, optArrayValue,
                                          pointerExpr->getSubExpr());
      return SGF.emitArrayToPointer(pointerExpr, arrayValue,
                                    Extra.get<ArrayAccessInfo>(Kind));
    }

    case RValueStringToPointer: {
      auto pointerExpr = cast<StringToPointerExpr>(expr);
      auto optStringValue = RV().RV;
      auto stringValue =
        emitBindOptionals(SGF, optStringValue, pointerExpr->getSubExpr());
      return SGF.emitStringToPointer(pointerExpr, stringValue,
                                     pointerExpr->getType());
    }
    case FunctionConversion: {
      auto funcConv = cast<FunctionConversionExpr>(expr);
      auto optFuncValue = RV().RV;
      auto funcValue =
        emitBindOptionals(SGF, optFuncValue, funcConv->getSubExpr());
      return {SGF.emitTransformedValue(funcConv, funcValue,
                          funcConv->getSubExpr()->getType()->getCanonicalType(),
                          funcConv->getType()->getCanonicalType(),
                          SGFContext()),
              ManagedValue()};
    }
    }
    llvm_unreachable("bad kind");
  }

  ManagedValue emitBindOptionals(SILGenFunction &SGF, ManagedValue optValue,
                                 Expr *expr) {
    expr = expr->getSemanticsProvidingExpr();
    auto bind = dyn_cast<BindOptionalExpr>(expr);

    // If we don't find a bind, the value isn't optional.
    if (!bind) return optValue;

    // Recurse.
    optValue = emitBindOptionals(SGF, optValue, bind->getSubExpr());

    // Check whether the value is non-nil and if the value is not-nil, return
    // the unwrapped value.
    return SGF.emitBindOptional(bind, optValue, bind->getDepth());
  }

  std::pair<ManagedValue, ManagedValue>
  finishOptionalEvaluation(SILGenFunction &SGF, OptionalEvaluationExpr *eval) {
    SmallVector<ManagedValue, 2> results;

    SGF.emitOptionalEvaluation(eval, eval->getType(), results, SGFContext(),
      [&](SmallVectorImpl<ManagedValue> &results, SGFContext C) {
        // Recurse.
        auto values = finishOriginalExpr(SGF, eval->getSubExpr());

        // Our primary result is the value.
        results.push_back(values.first);

        // Our secondary result is the owner, if we have one.
        if (auto owner = values.second) results.push_back(owner);
      });

    assert(results.size() == 1 || results.size() == 2);

    ManagedValue value = results[0];

    ManagedValue owner;
    if (results.size() == 2) {
      owner = results[1];

      // Create a new value-dependence here if the primary result is
      // trivial.
      auto &valueTL = SGF.getTypeLowering(value.getType());
      if (valueTL.isTrivial()) {
        SILValue dependentValue =
          SGF.B.createMarkDependence(eval, value.forward(SGF),
                                     owner.getValue());
        value = SGF.emitManagedRValueWithCleanup(dependentValue, valueTL);
      }
    }

    return {value, owner};
  }
};

} // end anonymous namespace

/// Perform the formal-access phase of call argument emission by emitting
/// all of the delayed arguments.
static void emitDelayedArguments(SILGenFunction &SGF,
                                 MutableArrayRef<DelayedArgument> delayedArgs,
                         MutableArrayRef<SmallVector<ManagedValue, 4>> args) {
  assert(!delayedArgs.empty());

  SmallVector<std::pair<SILValue, SILLocation>, 4> emittedInoutArgs;
  auto delayedNext = delayedArgs.begin();

  // The assumption we make is that 'args' and 'delayedArgs' were built
  // up in parallel, with empty spots being dropped into 'args'
  // wherever there's a delayed argument to insert.
  //
  // Note that this also begins the formal accesses in evaluation order.
  for (auto &siteArgs : args) {
    // NB: siteArgs.size() may change during iteration
    for (int i = 0; i < (int)siteArgs.size(); ++i) {
      auto &siteArg = siteArgs[i];
      
      if (siteArg) continue;

      assert(delayedNext != delayedArgs.end());
      auto &delayedArg = *delayedNext;

      // Emit the delayed argument and replace it in the arguments array.
      delayedArg.emit(SGF, siteArgs, i);

      // Remember all the simple inouts we emitted so we can perform
      // a basic inout-aliasing analysis.
      // This should be completely obviated by static enforcement.
      if (delayedArg.isSimpleInOut()) {
        emittedInoutArgs.push_back({siteArg.getValue(),
                                    delayedArg.getInOutLocation()});
      }

      if (++delayedNext == delayedArgs.end())
        goto done;
    }
  }

  llvm_unreachable("ran out of null arguments before we ran out of inouts");

done:

  // Check to see if we have multiple inout arguments which obviously
  // alias.  Note that we could do this in a later SILDiagnostics pass
  // as well: this would be stronger (more equivalences exposed) but
  // would have worse source location information.
  for (auto i = emittedInoutArgs.begin(), e = emittedInoutArgs.end();
         i != e; ++i) {
    for (auto j = emittedInoutArgs.begin(); j != i; ++j) {
      if (!RValue::areObviouslySameValue(i->first, j->first)) continue;

      SGF.SGM.diagnose(i->second, diag::inout_argument_alias)
        .highlight(i->second.getSourceRange());
      SGF.SGM.diagnose(j->second, diag::previous_inout_alias)
        .highlight(j->second.getSourceRange());
    }
  }
}

namespace {

/// A destination for an argument other than just "onto to the end
/// of the arguments lists".
///
/// This allows us to re-use the argument expression emitter for
/// some weird cases, like a shuffled tuple where some of the
/// arguments are going into a varargs array.
struct ArgSpecialDest {
  VarargsInfo *SharedInfo;
  unsigned Index;
  CleanupHandle Cleanup;

  ArgSpecialDest() : SharedInfo(nullptr) {}
  explicit ArgSpecialDest(VarargsInfo &info, unsigned index)
    : SharedInfo(&info), Index(index) {}

  // Reference semantics: need to preserve the cleanup handle.
  ArgSpecialDest(const ArgSpecialDest &) = delete;
  ArgSpecialDest &operator=(const ArgSpecialDest &) = delete;
  ArgSpecialDest(ArgSpecialDest &&other)
    : SharedInfo(other.SharedInfo), Index(other.Index),
      Cleanup(other.Cleanup) {
    other.SharedInfo = nullptr;
  }
  ArgSpecialDest &operator=(ArgSpecialDest &&other) {
    assert(!isValid() && "overwriting valid special destination!");
    SharedInfo = other.SharedInfo;
    Index = other.Index;
    Cleanup = other.Cleanup;
    other.SharedInfo = nullptr;
    return *this;
  }

  ~ArgSpecialDest() {
    assert(!isValid() && "failed to deactivate special dest");
  }

  /// Is this a valid special destination?
  ///
  /// Most of the time, most arguments don't have special
  /// destinations, and making an array of Optional<Special special
  /// destinations has t
  bool isValid() const { return SharedInfo != nullptr; }

  /// Fill this special destination with a value.
  void fill(SILGenFunction &SGF, ArgumentSource &&arg,
            AbstractionPattern _unused_origType,
            SILType loweredSubstParamType) {
    assert(isValid() && "filling an invalid destination");

    SILLocation loc = arg.getLocation();
    auto destAddr = SharedInfo->getBaseAddress();
    if (Index != 0) {
      SILValue index = SGF.B.createIntegerLiteral(loc,
                  SILType::getBuiltinWordType(SGF.getASTContext()), Index);
      destAddr = SGF.B.createIndexAddr(loc, destAddr, index);
    }

    assert(destAddr->getType() == loweredSubstParamType.getAddressType());

    auto &destTL = SharedInfo->getBaseTypeLowering();
    Cleanup =
        SGF.enterDormantFormalAccessTemporaryCleanup(destAddr, loc, destTL);

    TemporaryInitialization init(destAddr, Cleanup);
    std::move(arg).forwardInto(SGF, SharedInfo->getBaseAbstractionPattern(),
                               &init, destTL);
  }

  /// Deactivate this special destination.  Must always be called
  /// before destruction.
  void deactivate(SILGenFunction &SGF) {
    assert(isValid() && "deactivating an invalid destination");
    if (Cleanup.isValid())
      SGF.Cleanups.forwardCleanup(Cleanup);
    SharedInfo = nullptr;
  }
};

using ArgSpecialDestArray = MutableArrayRef<ArgSpecialDest>;

class TupleShuffleArgEmitter;

class ArgEmitter {
  // TODO: Refactor out the parts of ArgEmitter needed by TupleShuffleArgEmitter
  // into its own "context struct".
  friend class TupleShuffleArgEmitter;

  SILGenFunction &SGF;
  SILFunctionTypeRepresentation Rep;
  Optional<ForeignErrorConvention> ForeignError;
  ImportAsMemberStatus ForeignSelf;
  ClaimedParamsRef ParamInfos;
  SmallVectorImpl<ManagedValue> &Args;

  /// Track any delayed arguments that are emitted.  Each corresponds
  /// in order to a "hole" (a null value) in Args.
  SmallVectorImpl<DelayedArgument> &DelayedArguments;

  Optional<ArgSpecialDestArray> SpecialDests;
public:
  ArgEmitter(SILGenFunction &SGF, SILFunctionTypeRepresentation Rep,
             ClaimedParamsRef paramInfos,
             SmallVectorImpl<ManagedValue> &args,
             SmallVectorImpl<DelayedArgument> &delayedArgs,
             const Optional<ForeignErrorConvention> &foreignError,
             ImportAsMemberStatus foreignSelf,
             Optional<ArgSpecialDestArray> specialDests = None)
    : SGF(SGF), Rep(Rep), ForeignError(foreignError),
      ForeignSelf(foreignSelf),
      ParamInfos(paramInfos),
      Args(args), DelayedArguments(delayedArgs), SpecialDests(specialDests) {
    assert(!specialDests || specialDests->size() == paramInfos.size());
  }

  void emitTopLevel(ArgumentSource &&arg, AbstractionPattern origParamType) {
    emit(std::move(arg), origParamType);
    maybeEmitForeignErrorArgument();
  }

private:
  void emit(ArgumentSource &&arg, AbstractionPattern origParamType) {
    // If it was a tuple in the original type, or the argument
    // requires the callee to evaluate, the parameters will have
    // been exploded.
    if (origParamType.isTuple() || arg.requiresCalleeToEvaluate()) {
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
    //
    // FIXME: Once -swift-version 3 code generation goes away, we
    // can simplify this.
    if (isUnmaterializableTupleType(substArgType)) {
      assert(origParamType.isTypeParameter());
      emitExpanded(std::move(arg), origParamType);
      return;
    }

    // Okay, everything else will be passed as a single value, one
    // way or another.

    // If this is a discarded foreign static 'self' parameter, force the
    // argument and discard it.
    if (ForeignSelf.isStatic()) {
      std::move(arg).getAsRValue(SGF);
      return;
    }

    // Adjust for the foreign-error argument if necessary.
    maybeEmitForeignErrorArgument();

    // The substituted parameter type.  Might be different from the
    // substituted argument type by abstraction and/or bridging.
    SILParameterInfo param = claimNextParameter();
    ArgSpecialDest *specialDest = claimNextSpecialDest();

    // Unwrap InOutType here.
    assert(isa<InOutType>(substArgType) == param.isIndirectInOut());
    if (auto inoutType = dyn_cast<InOutType>(substArgType))
      substArgType = inoutType.getObjectType();

    // Make sure we use the same value category for these so that we
    // can hereafter just use simple equality checks to test for
    // abstraction.
    SILType loweredSubstArgType = SGF.getLoweredType(substArgType);
    if (param.isIndirectInOut()) {
      loweredSubstArgType =
        SILType::getPrimitiveAddressType(loweredSubstArgType.getASTType());
    }
    SILType loweredSubstParamType =
      SILType::getPrimitiveType(param.getType(),
                                loweredSubstArgType.getCategory());

    // If the caller takes the argument indirectly, the argument has an
    // inout type.
    if (param.isIndirectInOut()) {
      assert(!specialDest);
      emitInOut(std::move(arg), loweredSubstArgType, loweredSubstParamType,
                origParamType, substArgType);
      return;
    }

    // If the original type is passed indirectly, copy to memory if
    // it's not already there.  (Note that this potentially includes
    // conventions which pass indirectly without transferring
    // ownership, like Itanium C++.)
    if (specialDest) {
      assert(param.isFormalIndirect() &&
             "SpecialDest should imply indirect parameter");
      // TODO: Change the way we initialize array storage in opaque mode
      emitIndirectInto(std::move(arg), origParamType, loweredSubstParamType,
                       *specialDest);
      Args.push_back(ManagedValue::forInContext());
      return;
    } else if (SGF.silConv.isSILIndirect(param)) {
      emitIndirect(std::move(arg), loweredSubstArgType, origParamType, param);
      return;
    }

    // Okay, if the original parameter is passed directly, then we
    // just need to handle abstraction differences and bridging.
    assert(!specialDest);
    emitDirect(std::move(arg), loweredSubstArgType, origParamType, param);
  }

  SILParameterInfo claimNextParameter() {
    assert(!ParamInfos.empty());
    auto param = ParamInfos.front();
    ParamInfos = ParamInfos.slice(1);
    return param;
  }

  /// Claim the next destination, returning a null pointer if there
  /// is no special destination.
  ArgSpecialDest *claimNextSpecialDest() {
    if (!SpecialDests) return nullptr;
    assert(!SpecialDests->empty());
    auto dest = &SpecialDests->front();
    SpecialDests = SpecialDests->slice(1);
    return (dest->isValid() ? dest : nullptr);
  }

  bool isUnmaterializableTupleType(CanType type) {
    if (auto tuple = dyn_cast<TupleType>(type))
      if (tuple->hasInOutElement())
        return true;
    return false;
  }

  /// Emit an argument as an expanded tuple.
  void emitExpanded(ArgumentSource &&arg, AbstractionPattern origParamType) {
    assert(!arg.isLValue() && "argument is l-value but parameter is tuple?");

    // If we're working with an r-value, just expand it out and emit
    // all the elements individually.
    if (arg.isRValue()) {
      if (CanTupleType substArgType =
              dyn_cast<TupleType>(arg.getSubstType())) {
        // The original type isn't necessarily a tuple.
        assert(origParamType.matchesTuple(substArgType));

        auto loc = arg.getKnownRValueLocation();
        SmallVector<RValue, 4> elts;
        std::move(arg).asKnownRValue(SGF).extractElements(elts);
        for (auto i : indices(substArgType.getElementTypes())) {
          emit({ loc, std::move(elts[i]) },
               origParamType.getTupleElementType(i));
        }
        return;
      }

      auto loc = arg.getKnownRValueLocation();
      SmallVector<RValue, 1> elts;
      std::move(arg).asKnownRValue(SGF).extractElements(elts);
      emit({ loc, std::move(elts[0]) },
           origParamType.getTupleElementType(0));
      return;
    }

    // If we're working with a tuple source, expand it.
    if (arg.isTuple()) {
      (void) std::move(arg).withKnownTupleElementSources<int>(
                          [&](SILLocation loc, CanTupleType type,
                              MutableArrayRef<ArgumentSource> elts) {
        for (auto i : indices(elts)) {
          emit(std::move(elts[i]), origParamType.getTupleElementType(i));
        }
        return 0; // We need a fake return value because <void> won't compile.
      });
      return;
    }

    // Otherwise, we're working with an expression.
    Expr *e = std::move(arg).asKnownExpr();
    e = e->getSemanticsProvidingExpr();

    // If the source expression is a tuple literal, we can break it
    // up directly.
    if (auto tuple = dyn_cast<TupleExpr>(e)) {
      for (auto i : indices(tuple->getElements())) {
        emit(tuple->getElement(i),
             origParamType.getTupleElementType(i));
      }
      return;
    }

    if (auto shuffle = dyn_cast<TupleShuffleExpr>(e)) {
      emitShuffle(shuffle, origParamType);
      return;
    }

    // Fall back to the r-value case.
    emitExpanded({ e, SGF.emitRValue(e) }, origParamType);
  }

  void emitShuffle(TupleShuffleExpr *shuffle, AbstractionPattern origType);

  void emitIndirect(ArgumentSource &&arg,
                    SILType loweredSubstArgType,
                    AbstractionPattern origParamType,
                    SILParameterInfo param) {
    auto contexts = getRValueEmissionContexts(loweredSubstArgType, param);
    ManagedValue result;

    // If no abstraction is required, try to honor the emission contexts.
    if (!contexts.RequiresReabstraction) {
      auto loc = arg.getLocation();

      // Peephole certain argument emissions.
      if (arg.isExpr()) {
        auto expr = std::move(arg).asKnownExpr();

        // Try the peepholes.
        if (maybeEmitDelayed(expr, OriginalArgument(expr, /*indirect*/ true)))
          return;

        // Otherwise, just use the default logic.
        result = SGF.emitRValueAsSingleValue(expr, contexts.FinalContext);
      } else {
        result = std::move(arg).getAsSingleValue(SGF, contexts.FinalContext);
      }

      // If it's not already in memory, put it there.
      if (!result.getType().isAddress()) {
        result = result.materialize(SGF, loc);
      }

    // Otherwise, simultaneously emit and reabstract.
    } else {
      result = std::move(arg).materialize(SGF, origParamType,
                                          SGF.getSILType(param));
    }

    Args.push_back(result);
  }

  void emitIndirectInto(ArgumentSource &&arg,
                        AbstractionPattern origType,
                        SILType loweredSubstParamType,
                        ArgSpecialDest &dest) {
    dest.fill(SGF, std::move(arg), origType, loweredSubstParamType);
  }

  void emitInOut(ArgumentSource &&arg,
                 SILType loweredSubstArgType, SILType loweredSubstParamType,
                 AbstractionPattern origType, CanType substType) {
    SILLocation loc = arg.getLocation();

    LValue lv = [&]{
      // If the argument is already lowered to an LValue, it must be the
      // receiver of a self argument, which will be the first inout.
      if (arg.isLValue()) {
        return std::move(arg).asKnownLValue();

      // This is logically wrong, but propagating l-values within
      // RValues is hard to avoid in custom argument-emission code
      // without making ArgumentSource capable of holding mixed
      // RValue/LValue tuples.  (materializeForSet has to do this,
      // for one.)  The onus is on the caller to ensure that formal
      // access semantics are honored.
      } else if (arg.isRValue()) {
        auto address = std::move(arg).asKnownRValue(SGF).getAsSingleValue(
            SGF, arg.getKnownRValueLocation());
        assert(address.isLValue());
        return LValue::forAddress(address, None,
                                  AbstractionPattern(substType),
                                  substType);
      } else {
        auto *e = cast<InOutExpr>(std::move(arg).asKnownExpr()->
                                  getSemanticsProvidingExpr());
        return SGF.emitLValue(e->getSubExpr(), AccessKind::ReadWrite);
      }
    }();

    if (loweredSubstParamType.hasAbstractionDifference(Rep,
                                                       loweredSubstArgType)) {
      AbstractionPattern origObjectType = origType.transformType(
        [](CanType type)->CanType {
          return CanType(type->getInOutObjectType());
        });
      lv.addSubstToOrigComponent(origObjectType, loweredSubstParamType);
    }

    // Leave an empty space in the ManagedValue sequence and
    // remember that we had an inout argument.
    DelayedArguments.emplace_back(DelayedArgument::InOut, std::move(lv), loc);
    Args.push_back(ManagedValue());
    return;
  }

  void emitDirect(ArgumentSource &&arg, SILType loweredSubstArgType,
                  AbstractionPattern origParamType,
                  SILParameterInfo param) {
    ManagedValue value;
    auto loc = arg.getLocation();

    auto convertOwnershipConvention = [&](ManagedValue value) {
      return convertOwnershipConventionGivenParamInfo(SGF, param, value, loc);
    };

    auto contexts = getRValueEmissionContexts(loweredSubstArgType, param);
    if (contexts.RequiresReabstraction) {
      auto conversion = [&] {
        switch (getSILFunctionLanguage(Rep)) {
        case SILFunctionLanguage::Swift:
          return Conversion::getSubstToOrig(origParamType, arg.getSubstType());
        case SILFunctionLanguage::C:
          return Conversion::getBridging(Conversion::BridgeToObjC,
                                         arg.getSubstType(),
                                         origParamType.getType(),
                                         param.getSILStorageType());
        }
        llvm_unreachable("bad language");
      }();
      value = emitConvertedArgument(std::move(arg), conversion,
                                    contexts.FinalContext);
      Args.push_back(convertOwnershipConvention(value));
      return;
    }

    // Peephole certain argument emissions.
    if (arg.isExpr()) {
      auto expr = std::move(arg).asKnownExpr();

      // Try the peepholes.
      if (maybeEmitDelayed(expr, OriginalArgument(expr, /*indirect*/ false)))
        return;

      // Any borrows from any rvalue accesses, we want to be cleaned up at this
      // point.
      FormalEvaluationScope S(SGF);

      // Otherwise, just use the default logic.
      value = SGF.emitRValueAsSingleValue(expr, contexts.FinalContext);
      Args.push_back(convertOwnershipConvention(value));
      return;
    }

    value = std::move(arg).getAsSingleValue(SGF, contexts.FinalContext);
    Args.push_back(convertOwnershipConvention(value));
  }

  bool maybeEmitDelayed(Expr *expr, OriginalArgument original) {
    expr = expr->getSemanticsProvidingExpr();

    // Delay accessing inout-to-pointer arguments until the call.
    if (auto inoutToPointer = dyn_cast<InOutToPointerExpr>(expr)) {
      return emitDelayedConversion(inoutToPointer, original);
    }

    // Delay accessing array-to-pointer arguments until the call.
    if (auto arrayToPointer = dyn_cast<ArrayToPointerExpr>(expr)) {
      return emitDelayedConversion(arrayToPointer, original);
    }

    // Delay accessing string-to-pointer arguments until the call.
    if (auto stringToPointer = dyn_cast<StringToPointerExpr>(expr)) {
      return emitDelayedConversion(stringToPointer, original);
    }
    
    // Delay function conversions involving the opened Self type of an
    // existential whose opening is itself delayed.
    //
    // This comes up when invoking protocol methods on an existential that
    // have covariant arguments of function type with Self arguments, e.g.:
    //
    //    protocol P {
    //      mutating func foo(_: (Self) -> Void)
    //    }
    //
    //    func bar(x: inout P) {
    //      x.foo { y in return }
    //    }
    //
    // Although the type-erased method is presented as formally taking an
    // argument of the existential type P, it still has a conversion thunk to
    // perform type erasure on the argument coming from the underlying
    // implementation. Since the `self` argument is inout, it isn't formally
    // opened until late when formal accesses begin, so this closure conversion
    // must also be deferred until after that occurs.
    if (auto funcConv = dyn_cast<FunctionConversionExpr>(expr)) {
      auto destTy = funcConv->getType()->castTo<AnyFunctionType>();
      auto srcTy = funcConv->getSubExpr()->getType()->castTo<AnyFunctionType>();
      
      if (destTy->hasOpenedExistential()
          && !srcTy->hasOpenedExistential()
          && destTy->getRepresentation() == srcTy->getRepresentation()) {
        return emitDelayedConversion(funcConv, original);
      }
    }

    // Any recursive cases we handle here need to be handled in
    // DelayedArgument::finishOriginalExpr.

    // Handle optional evaluations.
    if (auto optional = dyn_cast<OptionalEvaluationExpr>(expr)) {
      // The validity of just recursing here depends on the fact
      // that we only return true for the specific conversions above,
      // which are constrained by the ASTVerifier to only appear in
      // specific forms.
      return maybeEmitDelayed(optional->getSubExpr(), original);
    }

    // Handle injections into optionals.
    if (auto inject = dyn_cast<InjectIntoOptionalExpr>(expr)) {
      return maybeEmitDelayed(inject->getSubExpr(), original);
    }

    // Handle try! expressions.
    if (auto forceTry = dyn_cast<ForceTryExpr>(expr)) {
      // Any expressions in the l-value must be routed appropriately.
      SILGenFunction::ForceTryEmission emission(SGF, forceTry);

      return maybeEmitDelayed(forceTry->getSubExpr(), original);
    }

    return false;
  }

  bool emitDelayedConversion(InOutToPointerExpr *pointerExpr,
                             OriginalArgument original) {
    auto info = SGF.getPointerAccessInfo(pointerExpr->getType());
    LValueOptions options;
    options.IsNonAccessing = pointerExpr->isNonAccessing();
    LValue lv = SGF.emitLValue(pointerExpr->getSubExpr(), info.AccessKind,
                               options);
    DelayedArguments.emplace_back(info, std::move(lv), pointerExpr, original);
    Args.push_back(ManagedValue());
    return true;
  }

  bool emitDelayedConversion(ArrayToPointerExpr *pointerExpr,
                             OriginalArgument original) {
    auto arrayExpr = pointerExpr->getSubExpr();

    // If the source of the conversion is an inout, emit the l-value
    // but delay the formal access.
    if (arrayExpr->isSemanticallyInOutExpr()) {
      auto info = SGF.getArrayAccessInfo(pointerExpr->getType(),
                                         arrayExpr->getType()->getInOutObjectType());
      LValueOptions options;
      options.IsNonAccessing = pointerExpr->isNonAccessing();
      LValue lv = SGF.emitLValue(arrayExpr, info.AccessKind, options);
      DelayedArguments.emplace_back(info, std::move(lv), pointerExpr,
                                    original);
      Args.push_back(ManagedValue());
      return true;
    }

    // Otherwise, it's an r-value conversion.
    auto info = SGF.getArrayAccessInfo(pointerExpr->getType(),
                                       arrayExpr->getType());

    auto rvalueExpr = lookThroughBindOptionals(arrayExpr);
    ManagedValue value = SGF.emitRValueAsSingleValue(rvalueExpr);
    DelayedArguments.emplace_back(DelayedArgument::RValueArrayToPointer,
                                  info, value, original);
    Args.push_back(ManagedValue());
    return true;
  }

  /// Emit an rvalue-array-to-pointer conversion as a delayed argument.
  bool emitDelayedConversion(StringToPointerExpr *pointerExpr,
                             OriginalArgument original) {
    auto rvalueExpr = lookThroughBindOptionals(pointerExpr->getSubExpr());
    ManagedValue value = SGF.emitRValueAsSingleValue(rvalueExpr);
    DelayedArguments.emplace_back(DelayedArgument::RValueStringToPointer,
                                  value, original);
    Args.push_back(ManagedValue());
    return true;
  }
  
  bool emitDelayedConversion(FunctionConversionExpr *funcConv,
                             OriginalArgument original) {
    auto rvalueExpr = lookThroughBindOptionals(funcConv->getSubExpr());
    ManagedValue value = SGF.emitRValueAsSingleValue(rvalueExpr);
    DelayedArguments.emplace_back(DelayedArgument::FunctionConversion,
                                  value, original);
    Args.push_back(ManagedValue());
    return true;
  }

  static Expr *lookThroughBindOptionals(Expr *expr) {
    while (true) {
      expr = expr->getSemanticsProvidingExpr();
      if (auto bind = dyn_cast<BindOptionalExpr>(expr)) {
        expr = bind->getSubExpr();
      } else {
        return expr;
      }
    }
  }

  ManagedValue emitConvertedArgument(ArgumentSource &&arg,
                                     Conversion conversion,
                                     SGFContext C) {
    auto loc = arg.getLocation();
    Scope scope(SGF, loc);

    // TODO: honor C here.
    auto result = std::move(arg).getConverted(SGF, conversion);

    return scope.popPreservingValue(result);
  }
  
  void maybeEmitForeignErrorArgument() {
    if (!ForeignError ||
        ForeignError->getErrorParameterIndex() != Args.size())
      return;

    SILParameterInfo param = claimNextParameter();
    ArgSpecialDest *specialDest = claimNextSpecialDest();

    assert(param.getConvention() == ParameterConvention::Direct_Unowned);
    assert(!specialDest && "special dest for error argument?");
    (void) param; (void) specialDest;

    // Leave a placeholder in the position.
    Args.push_back(ManagedValue::forInContext());
  }

  struct EmissionContexts {
    /// The context for emitting the r-value.
    SGFContext FinalContext;
    /// If the context requires reabstraction
    bool RequiresReabstraction;
  };
  static EmissionContexts getRValueEmissionContexts(SILType loweredArgType,
                                                    SILParameterInfo param) {
    bool requiresReabstraction =
        loweredArgType.getASTType() != param.getType();
    // If the parameter is consumed, we have to emit at +1.
    if (param.isConsumed()) {
      return {SGFContext(), requiresReabstraction};
    }

    // Otherwise, we can emit the final value at +0 (but only with a
    // guarantee that the value will survive).
    //
    // TODO: we can pass at +0 (immediate) to an unowned parameter
    // if we know that there will be no arbitrary side-effects
    // between now and the call.
    return {SGFContext::AllowGuaranteedPlusZero, requiresReabstraction};
  }
};

void DelayedArgument::emitDefaultArgument(SILGenFunction &SGF,
                                          const DefaultArgumentStorage &info,
                                          SmallVectorImpl<ManagedValue> &args,
                                          int &argIndex) {
  auto value = SGF.emitApplyOfDefaultArgGenerator(info.loc,
                                                  info.defaultArgsOwner,
                                                  info.destIndex,
                                                  info.resultType,
                                                  info.origResultType);
  
  SmallVector<ManagedValue, 4> loweredArgs;
  SmallVector<DelayedArgument, 4> delayedArgs;
  Optional<ForeignErrorConvention> errorConvention = None;
  auto emitter = ArgEmitter(SGF, info.functionRepresentation,
                            info.paramsToEmit,
                            loweredArgs, delayedArgs,
                            errorConvention, ImportAsMemberStatus());
  
  emitter.emitTopLevel(ArgumentSource(info.loc, std::move(value)),
                       info.origResultType);
  assert(delayedArgs.empty());
  assert(!errorConvention);
  
  // Splice the emitted default argument into the argument list.
  if (loweredArgs.size() == 1) {
    args[argIndex] = loweredArgs.front();
  } else {
    args.erase(args.begin() + argIndex);
    args.insert(args.begin() + argIndex,
                loweredArgs.begin(), loweredArgs.end());
    argIndex += loweredArgs.size() - 1;
  }
}

struct ElementExtent {
  /// The parameters which go into this tuple element.
  /// This is set in the first pass.
  ClaimedParamsRef Params;
  /// The destination index, if any.
  /// This is set in the first pass.
  unsigned DestIndex : 30;
  unsigned HasDestIndex : 1;
#ifndef NDEBUG
  unsigned Used : 1;
#endif
  /// The arguments which feed this tuple element.
  /// This is set in the second pass.
  ArrayRef<ManagedValue> Args;
  /// The inout arguments which feed this tuple element.
  /// This is set in the second pass.
  MutableArrayRef<DelayedArgument> DelayedArgs;

  ElementExtent()
      : HasDestIndex(false)
#ifndef NDEBUG
        ,
        Used(false)
#endif
  {
  }
};

class TupleShuffleArgEmitter {
  Expr *inner;
  Expr *outer;
  ArrayRef<TupleTypeElt> innerElts;
  ConcreteDeclRef defaultArgsOwner;
  ArrayRef<Expr *> callerDefaultArgs;
  ArrayRef<int> elementMapping;
  ArrayRef<unsigned> variadicArgs;
  Type varargsArrayType;
  AbstractionPattern origParamType;
  bool isResultScalar;

  TupleTypeElt singleOuterElement;
  ArrayRef<TupleTypeElt> outerElements;
  CanType canVarargsArrayType;

  /// The original parameter type.
  SmallVector<AbstractionPattern, 8> origInnerElts;
  AbstractionPattern innerOrigParamType;
  /// Flattened inner parameter sequence.
  SmallVector<SILParameterInfo, 8> innerParams;
  /// Extents of the inner elements.
  SmallVector<ElementExtent, 8> innerExtents;
  Optional<VarargsInfo> varargsInfo;
  SILParameterInfo variadicParamInfo; // innerExtents will point at this
  Optional<SmallVector<ArgSpecialDest, 8>> innerSpecialDests;

  // Used by flattenPatternFromInnerExtendIntoInnerParams and
  // splitInnerArgumentsCorrectly.
  SmallVector<ManagedValue, 8> innerArgs;
  SmallVector<DelayedArgument, 2> innerDelayedArgs;

public:
  TupleShuffleArgEmitter(TupleShuffleExpr *e, ArrayRef<TupleTypeElt> innerElts,
                         AbstractionPattern origParamType)
      : inner(e->getSubExpr()), outer(e), innerElts(innerElts),
        defaultArgsOwner(e->getDefaultArgsOwner()),
        callerDefaultArgs(e->getCallerDefaultArgs()),
        elementMapping(e->getElementMapping()),
        variadicArgs(e->getVariadicArgs()),
        varargsArrayType(e->getVarargsArrayTypeOrNull()),
        origParamType(origParamType), isResultScalar(e->isResultScalar()),
        canVarargsArrayType(),
        origInnerElts(innerElts.size(), AbstractionPattern::getInvalid()),
        innerOrigParamType(AbstractionPattern::getInvalid()), innerParams(),
        innerExtents(innerElts.size()), varargsInfo(), variadicParamInfo(),
        innerSpecialDests() {

    // Decompose the shuffle result.
    CanType resultType = e->getType()->getCanonicalType();
    if (isResultScalar) {
      singleOuterElement = TupleTypeElt(resultType);
      outerElements = singleOuterElement;
    } else {
      outerElements = cast<TupleType>(resultType)->getElements();
    }

    if (varargsArrayType)
      canVarargsArrayType = varargsArrayType->getCanonicalType();
  }

  TupleShuffleArgEmitter(const TupleShuffleArgEmitter &) = delete;
  TupleShuffleArgEmitter &operator=(const TupleShuffleArgEmitter &) = delete;
  TupleShuffleArgEmitter(TupleShuffleArgEmitter &&) = delete;
  TupleShuffleArgEmitter &operator=(TupleShuffleArgEmitter &&) = delete;

  void emit(ArgEmitter &parent);

private:
  void constructInnerTupleTypeInfo(ArgEmitter &parent);
  void flattenPatternFromInnerExtendIntoInnerParams(ArgEmitter &parent);
  void splitInnerArgumentsCorrectly(ArgEmitter &parent);
  void emitDefaultArgsAndFinalize(ArgEmitter &parent);

  AbstractionPattern getOutputOrigElementType(unsigned index) {
    if (isResultScalar) {
      assert(index == 0);
      return origParamType;
    } else {
      return origParamType.getTupleElementType(index);
    }
  }
};

} // end anonymous namespace

void TupleShuffleArgEmitter::constructInnerTupleTypeInfo(ArgEmitter &parent) {
  unsigned nextParamIndex = 0;
  for (unsigned outerIndex : indices(outerElements)) {
    CanType substEltType =
        outerElements[outerIndex].getType()->getCanonicalType();
    AbstractionPattern origEltType =
        getOutputOrigElementType(outerIndex);
    unsigned numParams =
        getFlattenedValueCount(origEltType, substEltType, parent.ForeignSelf);

    // Skip the foreign-error parameter.
    assert((!parent.ForeignError ||
            parent.ForeignError->getErrorParameterIndex() <= nextParamIndex ||
            parent.ForeignError->getErrorParameterIndex() >=
                nextParamIndex + numParams) &&
           "error parameter falls within shuffled range?");
    if (numParams && // Don't skip it twice if there's an empty tuple.
        parent.ForeignError &&
        parent.ForeignError->getErrorParameterIndex() == nextParamIndex) {
      nextParamIndex++;
    }

    // Grab the parameter infos corresponding to this tuple element
    // (but don't drop them from ParamInfos yet).
    auto eltParams = parent.ParamInfos.slice(nextParamIndex, numParams);
    nextParamIndex += numParams;

    int innerIndex = elementMapping[outerIndex];
    if (innerIndex >= 0) {
#ifndef NDEBUG
      assert(!innerExtents[innerIndex].Used && "using element twice");
      innerExtents[innerIndex].Used = true;
#endif
      innerExtents[innerIndex].Params = eltParams;
      origInnerElts[innerIndex] = origEltType;
    } else if (innerIndex == TupleShuffleExpr::Variadic) {
      auto &varargsField = outerElements[outerIndex];
      assert(varargsField.isVararg());
      assert(!varargsInfo.hasValue() && "already had varargs entry?");

      CanType varargsEltType = CanType(varargsField.getVarargBaseTy());
      unsigned numVarargs = variadicArgs.size();
      assert(canVarargsArrayType == substEltType);

      // Create the array value.
      varargsInfo.emplace(emitBeginVarargs(parent.SGF, outer, varargsEltType,
                                           canVarargsArrayType, numVarargs));

      // If we have any varargs, we'll need to actually initialize
      // the array buffer.
      if (numVarargs) {
        // For this, we'll need special destinations.
        assert(!innerSpecialDests);
        innerSpecialDests.emplace();

        // Prepare the variadic "arguments" as single +1 indirect parameters
        // with the array's desired abstraction pattern.  The vararg element
        // type should be materializable, and the abstraction pattern should be
        // opaque, so ArgEmitter's lowering should always generate exactly one
        // "argument" per element even if the substituted element type is a
        // tuple.
        variadicParamInfo =
          SILParameterInfo(varargsInfo->getBaseTypeLowering()
                           .getLoweredType().getASTType(),
                           ParameterConvention::Indirect_In);

        unsigned i = 0;
        for (unsigned innerIndex : variadicArgs) {
          // Find out where the next varargs element is coming from.
          assert(innerIndex >= 0 && "special source for varargs element??");
#ifndef NDEBUG
          assert(!innerExtents[innerIndex].Used && "using element twice");
          innerExtents[innerIndex].Used = true;
#endif

          // Set the destination index.
          innerExtents[innerIndex].HasDestIndex = true;
          innerExtents[innerIndex].DestIndex = i++;

          // Use the singleton param info we prepared before.
          innerExtents[innerIndex].Params =
            ClaimedParamsRef(variadicParamInfo);

          // Propagate the element abstraction pattern.
          origInnerElts[innerIndex] =
            varargsInfo->getBaseAbstractionPattern();
        }
      }
    }
  }
}

void TupleShuffleArgEmitter::flattenPatternFromInnerExtendIntoInnerParams(
    ArgEmitter &parent) {
  for (auto &extent : innerExtents) {
    assert(extent.Used && "didn't use all the inner tuple elements!");

    for (auto param : extent.Params) {
      innerParams.push_back(param);
    }

    // Fill in the special destinations array.
    if (innerSpecialDests) {
      // Use the saved index if applicable.
      if (extent.HasDestIndex) {
        assert(extent.Params.size() == 1);
        innerSpecialDests->push_back(
            ArgSpecialDest(*varargsInfo, extent.DestIndex));

        // Otherwise, fill in with the appropriate number of invalid
        // special dests.
      } else {
        // ArgSpecialDest isn't copyable, so we can't just use append.
        for (auto &p : extent.Params) {
          (void)p;
          innerSpecialDests->push_back(ArgSpecialDest());
        }
      }
    }
  }
}

void TupleShuffleArgEmitter::splitInnerArgumentsCorrectly(ArgEmitter &parent) {
  ArrayRef<ManagedValue> nextArgs = innerArgs;
  MutableArrayRef<DelayedArgument> nextDelayedArgs = innerDelayedArgs;
  for (auto &extent : innerExtents) {
    auto length = extent.Params.size();

    // Claim the next N inner args for this inner argument.
    extent.Args = nextArgs.slice(0, length);
    nextArgs = nextArgs.slice(length);

    // Claim the correct number of inout arguments as well.
    size_t numDelayed = 0;
    for (auto arg : extent.Args) {
      assert(!arg.isInContext() || extent.HasDestIndex);
      if (!arg)
        numDelayed++;
    }
    extent.DelayedArgs = nextDelayedArgs.slice(0, numDelayed);
    nextDelayedArgs = nextDelayedArgs.slice(numDelayed);
  }

  assert(nextArgs.empty() && "didn't claim all args");
  assert(nextDelayedArgs.empty() && "didn't claim all inout args");
}

void TupleShuffleArgEmitter::emitDefaultArgsAndFinalize(ArgEmitter &parent) {
  unsigned nextCallerDefaultArg = 0;
  for (unsigned outerIndex = 0, e = outerElements.size();
         outerIndex != e; ++outerIndex) {
    // If this comes from an inner element, move the appropriate
    // inner element values over.
    int innerIndex = elementMapping[outerIndex];
    if (innerIndex >= 0) {
      auto &extent = innerExtents[innerIndex];
      auto numArgs = extent.Args.size();

      parent.maybeEmitForeignErrorArgument();

      // Drop N parameters off of ParamInfos.
      parent.ParamInfos = parent.ParamInfos.slice(numArgs);

      // Move the appropriate inner arguments over as outer arguments.
      parent.Args.append(extent.Args.begin(), extent.Args.end());
      for (auto &delayedArg : extent.DelayedArgs)
        parent.DelayedArguments.push_back(std::move(delayedArg));
      continue;
    }

    // If this is default initialization, prepare to emit the default argument
    // generator later.
    if (innerIndex == TupleShuffleExpr::DefaultInitialize) {
      // Otherwise, emit the default initializer, then map that as a
      // default argument.
      CanType eltType = outerElements[outerIndex].getType()->getCanonicalType();
      auto origType = getOutputOrigElementType(outerIndex);
      
      auto numParams = getFlattenedValueCount(origType, eltType,
                                              ImportAsMemberStatus());
      parent.DelayedArguments.emplace_back(outer, defaultArgsOwner,
                                         outerIndex, eltType, origType,
                                         parent.ParamInfos.slice(0, numParams),
                                         parent.Rep);
      parent.ParamInfos = parent.ParamInfos.slice(numParams);
      parent.Args.push_back(ManagedValue());
      continue;
    }

    // If this is caller default initialization, generate the
    // appropriate value.
    if (innerIndex == TupleShuffleExpr::CallerDefaultInitialize) {
      auto arg = callerDefaultArgs[nextCallerDefaultArg++];
      parent.emit(ArgumentSource(arg),
                  getOutputOrigElementType(outerIndex));
      continue;
    }

    // If we're supposed to create a varargs array with the rest, do so.
    if (innerIndex == TupleShuffleExpr::Variadic) {
      auto &varargsField = outerElements[outerIndex];
      assert(varargsField.isVararg() &&
             "Cannot initialize nonvariadic element");
      assert(varargsInfo.hasValue());
      (void) varargsField;

      // We've successfully built the varargs array; deactivate all
      // the special destinations.
      if (innerSpecialDests) {
        for (auto &dest : *innerSpecialDests) {
          if (dest.isValid())
            dest.deactivate(parent.SGF);
        }
      }

      CanType eltType = outerElements[outerIndex].getType()->getCanonicalType();
      ManagedValue varargs =
          emitEndVarargs(parent.SGF, outer, std::move(*varargsInfo));
      parent.emit(
          ArgumentSource(outer, RValue(parent.SGF, outer, eltType, varargs)),
          getOutputOrigElementType(outerIndex));
      continue;
    }

    // That's the last special case defined so far.
    llvm_unreachable("unexpected special case in tuple shuffle!");
  }
}

void TupleShuffleArgEmitter::emit(ArgEmitter &parent) {
  // We could support dest addrs here, but it can't actually happen
  // with the current limitations on default arguments in tuples.
  assert(!parent.SpecialDests && "shuffle nested within varargs expansion?");

  // First, construct an abstraction pattern and parameter sequence
  // which we can use to emit the inner tuple.
  constructInnerTupleTypeInfo(parent);

  // The inner abstraction pattern is opaque if we started with an
  // opaque pattern; otherwise, it's a tuple of the de-shuffled
  // tuple elements.
  innerOrigParamType = origParamType;
  if (!origParamType.isTypeParameter()) {
    // That "tuple" might not actually be a tuple.
    if (innerElts.size() == 1 && !innerElts[0].hasName()) {
      innerOrigParamType = origInnerElts[0];
    } else {
      innerOrigParamType = AbstractionPattern::getTuple(origInnerElts);
    }
  }

  flattenPatternFromInnerExtendIntoInnerParams(parent);

  // Emit the inner expression.
  if (!innerParams.empty()) {
    ArgEmitter(parent.SGF, parent.Rep, ClaimedParamsRef(innerParams), innerArgs,
               innerDelayedArgs,
               /*foreign error*/ None, /*foreign self*/ ImportAsMemberStatus(),
               (innerSpecialDests ? ArgSpecialDestArray(*innerSpecialDests)
                                  : Optional<ArgSpecialDestArray>()))
        .emitTopLevel(ArgumentSource(inner), innerOrigParamType);
  }

  // Make a second pass to split the inner arguments correctly.
  splitInnerArgumentsCorrectly(parent);

  // Make a final pass to emit default arguments and move things into
  // the outer arguments lists.
  emitDefaultArgsAndFinalize(parent);
}

void ArgEmitter::emitShuffle(TupleShuffleExpr *E,
                             AbstractionPattern origParamType) {
  ArrayRef<TupleTypeElt> srcElts;
  TupleTypeElt singletonSrcElt;
  auto srcEltTy = E->getSubExpr()->getType()->getCanonicalType();
  if (E->isSourceScalar()) {
    ParameterTypeFlags flags;
    if (E->getSubExpr()->isSemanticallyInOutExpr()) {
      flags = flags.withInOut(true);
    }
    singletonSrcElt = {srcEltTy->getInOutObjectType(), Identifier(), flags};
    srcElts = singletonSrcElt;
  } else {
    srcElts = cast<TupleType>(srcEltTy)->getElements();
  }

  TupleShuffleArgEmitter(E, srcElts, origParamType).emit(*this);
}

namespace {
/// Cleanup to destroy an uninitialized box.
class DeallocateUninitializedBox : public Cleanup {
  SILValue box;
public:
  DeallocateUninitializedBox(SILValue box) : box(box) {}

  void emit(SILGenFunction &SGF, CleanupLocation l, ForUnwind_t forUnwind) override {
    SGF.B.createDeallocBox(l, box);
  }

  void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
    llvm::errs() << "DeallocateUninitializedBox "
                 << "State:" << getState() << " "
                 << "Box: " << box << "\n";
#endif
  }
};
} // end anonymous namespace

CleanupHandle SILGenFunction::enterDeallocBoxCleanup(SILValue box) {
  Cleanups.pushCleanup<DeallocateUninitializedBox>(box);
  return Cleanups.getTopCleanup();
}

/// This is an initialization for a box.
class BoxInitialization : public SingleBufferInitialization {
  SILValue box;
  SILValue addr;
  CleanupHandle uninitCleanup;
  CleanupHandle initCleanup;

public:
  BoxInitialization(SILValue box, SILValue addr,
                    CleanupHandle uninitCleanup,
                    CleanupHandle initCleanup)
    : box(box), addr(addr),
      uninitCleanup(uninitCleanup),
      initCleanup(initCleanup) {}

  void finishInitialization(SILGenFunction &SGF) override {
    SingleBufferInitialization::finishInitialization(SGF);
    SGF.Cleanups.setCleanupState(uninitCleanup, CleanupState::Dead);
    if (initCleanup.isValid())
        SGF.Cleanups.setCleanupState(initCleanup, CleanupState::Active);
  }

  SILValue getAddressForInPlaceInitialization(SILGenFunction &SGF,
                                              SILLocation loc) override {
    return addr;
  }

  bool isInPlaceInitializationOfGlobal() const override {
    return false;
  }

  ManagedValue getManagedBox() const {
    return ManagedValue(box, initCleanup);
  }
};

namespace {

/// A structure for conveniently claiming sets of uncurried parameters.
struct ParamLowering {
  ArrayRef<SILParameterInfo> Params;
  unsigned ClaimedForeignSelf = -1;
  SILFunctionTypeRepresentation Rep;
  SILFunctionConventions fnConv;

  ParamLowering(CanSILFunctionType fnType, SILGenFunction &SGF)
      : Params(fnType->getParameters()), Rep(fnType->getRepresentation()),
        fnConv(fnType, SGF.SGM.M) {}

  ClaimedParamsRef
  claimParams(AbstractionPattern origParamType, CanType substParamType,
              const Optional<ForeignErrorConvention> &foreignError,
              ImportAsMemberStatus foreignSelf) {
    unsigned count =
        getFlattenedValueCount(origParamType, substParamType, foreignSelf);
    if (foreignError)
      count++;

    if (foreignSelf.isImportAsMember()) {
      // Claim only the self parameter.
      assert(ClaimedForeignSelf == (unsigned)-1 &&
             "already claimed foreign self?!");
      if (foreignSelf.isStatic()) {
        // Imported as a static method, no real self param to claim.
        return {};
      }
      ClaimedForeignSelf = foreignSelf.getSelfIndex();
      return ClaimedParamsRef(Params[ClaimedForeignSelf],
                              ClaimedParamsRef::NoSkip);
    }

    if (ClaimedForeignSelf != (unsigned)-1) {
      assert(count + 1 == Params.size() &&
             "not claiming all params after foreign self?!");
      auto result = Params;
      Params = {};
      return ClaimedParamsRef(result, ClaimedForeignSelf);
    }

    assert(count <= Params.size());
    auto result = Params.slice(Params.size() - count, count);
    Params = Params.slice(0, Params.size() - count);
    return ClaimedParamsRef(result, (unsigned)-1);
  }

  ArrayRef<SILParameterInfo>
  claimCaptureParams(ArrayRef<ManagedValue> captures) {
    auto firstCapture = Params.size() - captures.size();
#ifndef NDEBUG
    assert(Params.size() >= captures.size() && "more captures than params?!");
    for (unsigned i = 0; i < captures.size(); ++i) {
      assert(fnConv.getSILType(Params[i + firstCapture]) ==
                 captures[i].getType() &&
             "capture doesn't match param type");
    }
#endif

    auto result = Params.slice(firstCapture, captures.size());
    Params = Params.slice(0, firstCapture);
    return result;
  }

  ~ParamLowering() {
    assert(Params.empty() && "didn't consume all the parameters");
  }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                                  CallSite
//===----------------------------------------------------------------------===//

namespace {

/// An application of possibly unevaluated arguments in the form of an
/// ArgumentSource to a Callee.
class CallSite {
public:
  SILLocation Loc;
  CanType SubstResultType;

private:
  ArgumentSource ArgValue;
  bool Throws;

public:
  CallSite(ApplyExpr *apply)
      : Loc(apply), SubstResultType(apply->getType()->getCanonicalType()),
        ArgValue(apply->getArg()), Throws(apply->throws()) {}

  CallSite(SILLocation loc, ArgumentSource &&value, CanType resultType,
           bool throws)
      : Loc(loc), SubstResultType(resultType), ArgValue(std::move(value)),
        Throws(throws) {}

  CallSite(SILLocation loc, ArgumentSource &&value, CanAnyFunctionType fnType)
      : CallSite(loc, std::move(value), fnType.getResult(), fnType->throws()) {}

  /// Return the substituted, unlowered AST type of the argument.
  CanType getSubstArgType() const { return ArgValue.getSubstType(); }

  /// Return the substituted, unlowered AST type of the result of
  /// this application.
  CanType getSubstResultType() const { return SubstResultType; }

  bool throws() const { return Throws; }

  /// Evaluate arguments and begin any inout formal accesses.
  void emit(SILGenFunction &SGF, AbstractionPattern origParamType,
            ParamLowering &lowering, SmallVectorImpl<ManagedValue> &args,
            SmallVectorImpl<DelayedArgument> &delayedArgs,
            const Optional<ForeignErrorConvention> &foreignError,
            ImportAsMemberStatus foreignSelf) && {
    auto params = lowering.claimParams(origParamType, getSubstArgType(),
                                       foreignError, foreignSelf);

    ArgEmitter emitter(SGF, lowering.Rep, params, args, delayedArgs,
                       foreignError, foreignSelf);
    emitter.emitTopLevel(std::move(ArgValue), origParamType);
  }

  /// Take the arguments for special processing, in place of the above.
  ArgumentSource &&forward() && { return std::move(ArgValue); }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                                CallEmission
//===----------------------------------------------------------------------===//

namespace {

/// Once the Callee and CallSites have been prepared by SILGenApply,
/// generate SIL for a fully-formed call.
///
/// The lowered function type of the callee defines an abstraction pattern
/// for evaluating argument values of tuple type directly into explosions of
/// scalars where possible.
///
/// If there are more call sites than the natural uncurry level, they are
/// have to be applied recursively to each intermediate callee.
///
/// Also inout formal access and parameter and result conventions are
/// handled here, with some special logic required for calls with +0 self.
class CallEmission {
  SILGenFunction &SGF;

  std::vector<CallSite> uncurriedSites;
  std::vector<CallSite> extraSites;
  Callee callee;
  FormalEvaluationScope initialWritebackScope;
  unsigned expectedSiteCount;

public:
  /// Create an emission for a call of the given callee.
  CallEmission(SILGenFunction &SGF, Callee &&callee,
               FormalEvaluationScope &&writebackScope)
      : SGF(SGF), callee(std::move(callee)),
        initialWritebackScope(std::move(writebackScope)),
        expectedSiteCount(callee.getParameterListCount()) {}

  /// A factory method for decomposing the apply expr \p e into a call
  /// emission.
  static CallEmission forApplyExpr(SILGenFunction &SGF, Expr *e);

  /// Add a level of function application by passing in its possibly
  /// unevaluated arguments and their formal type.
  void addCallSite(CallSite &&site) {
    // Append to the main argument list if we have uncurry levels remaining.
    if (uncurriedSites.size() < expectedSiteCount) {
      uncurriedSites.push_back(std::move(site));
      return;
    }

    // Otherwise, apply these arguments to the result of the previous call.
    extraSites.push_back(std::move(site));
  }

  /// Add a level of function application by passing in its possibly
  /// unevaluated arguments and their formal type
  template<typename...T>
  void addCallSite(T &&...args) {
    addCallSite(CallSite{std::forward<T>(args)...});
  }

  /// Is this a fully-applied enum element constructor call?
  bool isEnumElementConstructor() {
    return (callee.kind == Callee::Kind::EnumElement &&
            uncurriedSites.size() == expectedSiteCount);
  }

  /// True if this is a completely unapplied super method call
  bool isPartiallyAppliedSuperMethod() {
    return (callee.kind == Callee::Kind::SuperMethod &&
            uncurriedSites.size() == 1);
  }

  CleanupHandle applyCoroutine(SmallVectorImpl<ManagedValue> &yields);

  RValue apply(SGFContext C = SGFContext()) {
    initialWritebackScope.verify();

    // Emit the first level of call.
    auto firstLevelResult = applyFirstLevelCallee(C);

    // End of the initial writeback scope.
    initialWritebackScope.verify();
    initialWritebackScope.pop();

    // If we do not have any more call sites, bail early and just return the
    // value.
    if (extraSites.empty()) {
      return std::move(firstLevelResult.value);
    }

    // At this point, firstLevelResult should have a formal type for the
    // remaining call sites. Do a quick assert to make sure that we have our
    // rvalue and the relevant foreign type.
    assert(firstLevelResult.isComplete());

    AbstractionPattern origFormalType =
        getIndirectApplyAbstractionPattern(SGF, firstLevelResult.formalType);
    bool formalTypeThrows =
        !cast<FunctionType>(firstLevelResult.formalType)->getExtInfo().throws();

    // Then handle the remaining call sites.
    return applyRemainingCallSites(std::move(firstLevelResult.value),
                                   origFormalType, firstLevelResult.foreignSelf,
                                   C, formalTypeThrows);
  }

  // Movable, but not copyable.
  CallEmission(CallEmission &&e) = default;

private:
  CallEmission(const CallEmission &) = delete;
  CallEmission &operator=(const CallEmission &) = delete;

  /// Emit all of the arguments for a normal apply. This means an apply that
  /// is not:
  ///
  /// 1. A specialized emitter (e.g. an emitter for a builtin).
  /// 2. A partially applied super method.
  /// 3. An enum element constructor.
  ///
  /// It is though all other initial calls and subsequent callees that we feed
  /// the first callee into.
  ///
  /// This returns whether or not any arguments were able to throw in
  /// ApplyOptions.
  ApplyOptions emitArgumentsForNormalApply(
      CanFunctionType &formalType, AbstractionPattern &origFormalType,
      CanSILFunctionType substFnType,
      const Optional<ForeignErrorConvention> &foreignError,
      ImportAsMemberStatus foreignSelf,
      SmallVectorImpl<ManagedValue> &uncurriedArgs,
      Optional<SILLocation> &uncurriedLoc, CanFunctionType &formalApplyType);

  struct FirstLevelApplicationResult {
    RValue value;
    CanFunctionType formalType;
    ImportAsMemberStatus foreignSelf;

    FirstLevelApplicationResult() = default;

    // Delete copy constructor/operator,
    FirstLevelApplicationResult(const FirstLevelApplicationResult &) = delete;
    FirstLevelApplicationResult &
    operator=(const FirstLevelApplicationResult &) = delete;

    // This is a move only type.
    FirstLevelApplicationResult(FirstLevelApplicationResult &&other)
        : value(std::move(other.value)), formalType(other.formalType),
          foreignSelf(other.foreignSelf) {}
    FirstLevelApplicationResult &
    operator=(FirstLevelApplicationResult &&other) {
      value = std::move(other.value);
      formalType = other.formalType;
      foreignSelf = other.foreignSelf;
      return *this;
    }

    /// Verify some variants around a complete FirstLevelApplicationResult.
    ///
    /// The specific invariants is that value is complete and that we have a
    /// formal type.
    bool isComplete() const { return value.isComplete() && bool(formalType); }
  };

  FirstLevelApplicationResult
  applySpecializedEmitter(SpecializedEmitter &specializedEmitter, SGFContext C);

  FirstLevelApplicationResult applyPartiallyAppliedSuperMethod(SGFContext C);

  FirstLevelApplicationResult applyEnumElementConstructor(SGFContext C);

  FirstLevelApplicationResult applyNormalCall(SGFContext C);

  FirstLevelApplicationResult applyFirstLevelCallee(SGFContext C);

  RValue applyRemainingCallSites(RValue &&result,
                                 AbstractionPattern origFormalType,
                                 ImportAsMemberStatus foreignSelf, SGFContext C,
                                 bool formalTypeThrows);
};

} // end anonymous namespace

/// This function claims param clauses from the passed in formal type until the
/// type is completely uncurried. This will be the final result type for a
/// normal call.
static AbstractionPattern
getUncurriedOrigFormalResultType(AbstractionPattern origFormalType,
                                 unsigned numUncurriedSites) {
  for (unsigned i = 0, e = numUncurriedSites; i < e; ++i) {
    claimNextParamClause(origFormalType);
  }

  return origFormalType;
}

namespace {
/// Cleanup to end a coroutine application.
class EndCoroutineApply : public Cleanup {
  SILValue ApplyToken;
public:
  EndCoroutineApply(SILValue applyToken) : ApplyToken(applyToken) {}

  void emit(SILGenFunction &SGF, CleanupLocation l, ForUnwind_t forUnwind) override {
    if (forUnwind) {
      SGF.B.createAbortApply(l, ApplyToken);
    } else {
      SGF.B.createEndApply(l, ApplyToken);
    }
  }

  void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
    llvm::errs() << "EndCoroutineApply "
                 << "State:" << getState() << " "
                 << "Token: " << ApplyToken << "\n";
#endif
  }
};
}

CleanupHandle
CallEmission::applyCoroutine(SmallVectorImpl<ManagedValue> &yields) {
  auto origFormalType = callee.getOrigFormalType();
  CanFunctionType formalType = callee.getSubstFormalType();

  const bool isCurried = false;

  // Get the callee type information.
  auto calleeTypeInfo = callee.getTypeInfo(SGF, isCurried);

  SmallVector<ManagedValue, 4> uncurriedArgs;
  Optional<SILLocation> uncurriedLoc;
  CanFunctionType formalApplyType;

  // Evaluate the arguments.
  ApplyOptions options = emitArgumentsForNormalApply(
      formalType, origFormalType, calleeTypeInfo.substFnType,
      calleeTypeInfo.foreignError, calleeTypeInfo.foreignSelf, uncurriedArgs,
      uncurriedLoc, formalApplyType);

  // Now evaluate the callee.
  Optional<ManagedValue> borrowedSelf;
  if (callee.requiresSelfValueForDispatch()) {
    borrowedSelf = uncurriedArgs.back();
  }

  auto fnValue = callee.getFnValue(SGF, isCurried, borrowedSelf);

  // Emit the uncurried call.
  SmallVector<SILValue, 4> rawResults;
  emitRawApply(SGF, uncurriedLoc.getValue(), fnValue, callee.getSubstitutions(),
               uncurriedArgs, calleeTypeInfo.substFnType, options,
               /*indirect results*/ {}, rawResults);

  auto token = rawResults.pop_back_val();
  auto yieldValues = llvm::makeArrayRef(rawResults);

  // Push a cleanup to end the application.
  // TODO: destroy all the arguments at exactly this point?
  SGF.Cleanups.pushCleanup<EndCoroutineApply>(token);
  auto endApplyHandle = SGF.getTopCleanup();

  // Manage all the yielded values.
  auto yieldInfos = calleeTypeInfo.substFnType->getYields();
  assert(yieldValues.size() == yieldInfos.size());
  for (auto i : indices(yieldValues)) {
    auto value = yieldValues[i];
    auto info = yieldInfos[i];
    if (info.isIndirectInOut()) {
      yields.push_back(ManagedValue::forLValue(value));
    } else if (info.isConsumed()) {
      yields.push_back(SGF.emitManagedRValueWithCleanup(value));
    } else if (info.isDirectGuaranteed()) {
      yields.push_back(ManagedValue::forBorrowedRValue(value));
    } else {
      yields.push_back(ManagedValue::forTrivialRValue(value));
    }
  }

  return endApplyHandle;
}

CallEmission::FirstLevelApplicationResult
CallEmission::applyFirstLevelCallee(SGFContext C) {
  // Check for a specialized emitter.
  if (uncurriedSites.size() == expectedSiteCount) {
    if (auto emitter = callee.getSpecializedEmitter(SGF.SGM)) {
      return applySpecializedEmitter(emitter.getValue(), C);
    }
  }

  if (isPartiallyAppliedSuperMethod()) {
    return applyPartiallyAppliedSuperMethod(C);
  }

  if (isEnumElementConstructor()) {
    return applyEnumElementConstructor(C);
  }

  return applyNormalCall(C);
}

CallEmission::FirstLevelApplicationResult
CallEmission::applyNormalCall(SGFContext C) {
  FirstLevelApplicationResult firstLevelResult;

  // We use the context emit-into initialization only for the
  // outermost call.
  SGFContext uncurriedContext = (extraSites.empty() ? C : SGFContext());

  firstLevelResult.formalType = callee.getSubstFormalType();
  auto origFormalType = callee.getOrigFormalType();

  bool isCurried = (uncurriedSites.size() < callee.getParameterListCount());

  // Get the callee type information.
  auto calleeTypeInfo = callee.getTypeInfo(SGF, isCurried);

  // In C language modes, substitute the type of the AbstractionPattern
  // so that we won't see type parameters down when we try to form bridging
  // conversions.
  if (calleeTypeInfo.substFnType->getLanguage() == SILFunctionLanguage::C) {
    if (auto genericFnType =
          dyn_cast<GenericFunctionType>(origFormalType.getType())) {
      auto fnType = genericFnType->substGenericArgs(callee.getSubstitutions());
      origFormalType.rewriteType(CanGenericSignature(),
                                 fnType->getCanonicalType());
    }
  }

  // Initialize the rest of the call info.
  calleeTypeInfo.origResultType =
      getUncurriedOrigFormalResultType(origFormalType, uncurriedSites.size());
  calleeTypeInfo.substResultType = uncurriedSites.back().getSubstResultType();

  ResultPlanPtr resultPlan = ResultPlanBuilder::computeResultPlan(
      SGF, calleeTypeInfo, uncurriedSites.back().Loc, uncurriedContext);

  ArgumentScope argScope(SGF, uncurriedSites.back().Loc);

  // Emit the arguments.
  SmallVector<ManagedValue, 4> uncurriedArgs;
  Optional<SILLocation> uncurriedLoc;
  CanFunctionType formalApplyType;

  // *NOTE* We pass in initial options as a reference so that we can pass to
  // emitApply if any of the arguments could have thrown.
  ApplyOptions options = emitArgumentsForNormalApply(
      firstLevelResult.formalType, origFormalType, calleeTypeInfo.substFnType,
      calleeTypeInfo.foreignError, calleeTypeInfo.foreignSelf, uncurriedArgs,
      uncurriedLoc, formalApplyType);

  // Now evaluate the callee.
  Optional<ManagedValue> borrowedSelf;
  if (callee.requiresSelfValueForDispatch()) {
    borrowedSelf = uncurriedArgs.back();
  }

  auto mv = callee.getFnValue(SGF, isCurried, borrowedSelf);

  // Materialize for set could temporarily escape its arguments.
  if (callee.isMaterializeForSet) {
    auto indices = ArrayRef<ManagedValue>(uncurriedArgs).slice(2);
    for (auto index : indices) {
      auto *toNoEscape = dyn_cast<ConvertEscapeToNoEscapeInst>(index.getValue());
      if (!toNoEscape) continue;
      toNoEscape->setEscapedByUser();
    }
  }
  // Emit the uncurried call.
  firstLevelResult.value = SGF.emitApply(
      std::move(resultPlan), std::move(argScope), uncurriedLoc.getValue(), mv,
      callee.getSubstitutions(), uncurriedArgs, calleeTypeInfo, options,
      uncurriedContext);
  firstLevelResult.foreignSelf = calleeTypeInfo.foreignSelf;
  return firstLevelResult;
}

CallEmission::FirstLevelApplicationResult
CallEmission::applyEnumElementConstructor(SGFContext C) {
  FirstLevelApplicationResult firstLevelResult;
  SGFContext uncurriedContext = (extraSites.empty() ? C : SGFContext());

  // Get the callee type information.
  //
  // Enum payloads are always stored at the abstraction level of the
  // unsubstituted payload type. This means that unlike with specialized
  // emitters above, enum constructors use the AST-level abstraction
  // pattern, to ensure that function types in payloads are re-abstracted
  // correctly.
  firstLevelResult.formalType = callee.getSubstFormalType();
  auto origFormalType = callee.getOrigFormalType();
  auto substFnType =
      SGF.getSILFunctionType(origFormalType, firstLevelResult.formalType);

  // We have a fully-applied enum element constructor: open-code the
  // construction.
  EnumElementDecl *element = callee.getEnumElementDecl();

  SILLocation uncurriedLoc = uncurriedSites[0].Loc;

  CanType formalResultType = firstLevelResult.formalType.getResult();

  // Ignore metatype argument
  claimNextParamClause(origFormalType);
  claimNextParamClause(firstLevelResult.formalType);
  std::move(uncurriedSites[0]).forward().getAsSingleValue(SGF);

  // Get the payload argument.
  ArgumentSource payload;
  if (element->hasAssociatedValues()) {
    assert(uncurriedSites.size() == 2);
    formalResultType = firstLevelResult.formalType.getResult();
    claimNextParamClause(origFormalType);
    claimNextParamClause(firstLevelResult.formalType);
    payload = std::move(uncurriedSites[1]).forward();
  } else {
    assert(uncurriedSites.size() == 1);
  }

  assert(substFnType->getNumResults() == 1);
  (void)substFnType;
  ManagedValue resultMV = SGF.emitInjectEnum(
      uncurriedLoc, std::move(payload), SGF.getLoweredType(formalResultType),
      element, uncurriedContext);
  firstLevelResult.value =
      RValue(SGF, uncurriedLoc, formalResultType, resultMV);
  return firstLevelResult;
}

CallEmission::FirstLevelApplicationResult
CallEmission::applyPartiallyAppliedSuperMethod(SGFContext C) {
  FirstLevelApplicationResult firstLevelResult;

  // We want to emit the arguments as fully-substituted values
  // because that's what the partially applied super method expects;
  firstLevelResult.formalType = callee.getSubstFormalType();
  auto origFormalType = AbstractionPattern(firstLevelResult.formalType);
  auto substFnType =
      SGF.getSILFunctionType(origFormalType, firstLevelResult.formalType);

  // Emit the arguments.
  SmallVector<ManagedValue, 4> uncurriedArgs;
  Optional<SILLocation> uncurriedLoc;
  CanFunctionType formalApplyType;
  ApplyOptions options = emitArgumentsForNormalApply(
      firstLevelResult.formalType, origFormalType, substFnType,
      Optional<ForeignErrorConvention>(), firstLevelResult.foreignSelf,
      uncurriedArgs, uncurriedLoc, formalApplyType);
  (void)options;

  // Emit the uncurried call.
  assert(uncurriedArgs.size() == 1 && "Can only partially apply the "
                                      "self parameter of a super "
                                      "method call");

  auto constant = callee.getMethodName();
  auto loc = uncurriedLoc.getValue();
  auto subs = callee.getSubstitutions();
  auto upcastedSelf = uncurriedArgs.back();

  // Make sure that upcasted self is at +1 since we are going to place it into a
  // partial_apply.
  upcastedSelf = upcastedSelf.ensurePlusOne(SGF, loc);

  auto constantInfo = SGF.getConstantInfo(callee.getMethodName());
  auto functionTy = constantInfo.getSILType();
  ManagedValue superMethod;
  {
    Scope S(SGF, loc);
    ManagedValue castValue =
        borrowedCastToOriginalSelfType(SGF, loc, upcastedSelf);
    if (!constant.isForeign) {
      superMethod = SGF.B.createSuperMethod(loc, castValue, constant,
                                            functionTy);
    } else {
      superMethod = SGF.B.createObjCSuperMethod(loc, castValue, constant,
                                                functionTy);
    }
  }
  auto calleeConvention = ParameterConvention::Direct_Guaranteed;
  auto closureTy = SILGenBuilder::getPartialApplyResultType(
      constantInfo.getSILType(), 1, SGF.B.getModule(), subs, calleeConvention);

  auto &module = SGF.getFunction().getModule();

  auto partialApplyTy = functionTy;
  if (constantInfo.SILFnType->isPolymorphic() && !subs.empty())
    partialApplyTy = partialApplyTy.substGenericArgs(module, subs);

  ManagedValue pa = SGF.B.createPartialApply(loc, superMethod, partialApplyTy,
                                             subs, {upcastedSelf},
                                             closureTy);
  assert(!closureTy.castTo<SILFunctionType>()->isNoEscape());
  firstLevelResult.value = RValue(SGF, loc, formalApplyType.getResult(), pa);
  return firstLevelResult;
}

CallEmission::FirstLevelApplicationResult
CallEmission::applySpecializedEmitter(SpecializedEmitter &specializedEmitter,
                                      SGFContext C) {
  FirstLevelApplicationResult firstLevelResult;

  // We use the context emit-into initialization only for the
  // outermost call.
  SGFContext uncurriedContext = (extraSites.empty() ? C : SGFContext());

  ManagedValue mv;

  // Get the callee type information. We want to emit the arguments as
  // fully-substituted values because that's what the specialized emitters
  // expect.
  firstLevelResult.formalType = callee.getSubstFormalType();
  auto origFormalType = AbstractionPattern(firstLevelResult.formalType);
  auto substFnType =
      SGF.getSILFunctionType(origFormalType, firstLevelResult.formalType);

  // If we have an early emitter, just let it take over for the
  // uncurried call site.
  if (specializedEmitter.isEarlyEmitter()) {
    auto emitter = specializedEmitter.getEarlyEmitter();

    assert(uncurriedSites.size() == 1);
    CanFunctionType formalApplyType =
        cast<FunctionType>(firstLevelResult.formalType);
    assert(!formalApplyType->getExtInfo().throws());
    CanType formalResultType = formalApplyType.getResult();
    SILLocation uncurriedLoc = uncurriedSites[0].Loc;
    claimNextParamClause(origFormalType);
    claimNextParamClause(firstLevelResult.formalType);

    // We should be able to enforce that these arguments are
    // always still expressions.
    Expr *argument = std::move(uncurriedSites[0]).forward().asKnownExpr();
    ManagedValue resultMV =
        emitter(SGF, uncurriedLoc, callee.getSubstitutions(),
                argument, uncurriedContext);
    firstLevelResult.value =
        RValue(SGF, uncurriedLoc, formalResultType, resultMV);
    return firstLevelResult;
  }

  // Emit the arguments.
  SmallVector<ManagedValue, 4> uncurriedArgs;
  Optional<SILLocation> uncurriedLoc;
  CanFunctionType formalApplyType;
  emitArgumentsForNormalApply(firstLevelResult.formalType, origFormalType,
                              substFnType, Optional<ForeignErrorConvention>(),
                              firstLevelResult.foreignSelf, uncurriedArgs,
                              uncurriedLoc, formalApplyType);

  // If we have a late emitter, just delegate to that emitter and return.
  if (specializedEmitter.isLateEmitter()) {
    auto emitter = specializedEmitter.getLateEmitter();
    ManagedValue mv = emitter(SGF, *uncurriedLoc,
                              callee.getSubstitutions(),
                              uncurriedArgs, uncurriedContext);
    firstLevelResult.value =
        RValue(SGF, *uncurriedLoc, formalApplyType.getResult(), mv);
    return firstLevelResult;
  }

  // Builtins.
  assert(specializedEmitter.isNamedBuiltin());
  auto builtinName = specializedEmitter.getBuiltinName();
  SmallVector<SILValue, 4> consumedArgs;
  for (auto arg : uncurriedArgs) {
    // Builtins have a special convention that takes everything at +1.
    auto maybePlusOne = arg.ensurePlusOne(SGF, uncurriedLoc.getValue());
    consumedArgs.push_back(maybePlusOne.forward(SGF));
  }
  SILFunctionConventions substConv(substFnType, SGF.SGM.M);
  auto resultVal = SGF.B.createBuiltin(uncurriedLoc.getValue(), builtinName,
                                       substConv.getSILResultType(),
                                       callee.getSubstitutions(),
                                       consumedArgs);
  firstLevelResult.value =
      RValue(SGF, *uncurriedLoc, formalApplyType.getResult(),
             SGF.emitManagedRValueWithCleanup(resultVal));
  return firstLevelResult;
}

ApplyOptions CallEmission::emitArgumentsForNormalApply(
    CanFunctionType &formalType, AbstractionPattern &origFormalType,
    CanSILFunctionType substFnType,
    const Optional<ForeignErrorConvention> &foreignError,
    ImportAsMemberStatus foreignSelf,
    SmallVectorImpl<ManagedValue> &uncurriedArgs,
    Optional<SILLocation> &uncurriedLoc, CanFunctionType &formalApplyType) {
  ApplyOptions options = ApplyOptions::None;

  SmallVector<SmallVector<ManagedValue, 4>, 2> args;
  SmallVector<DelayedArgument, 2> delayedArgs;
  auto expectedUncurriedOrigResultFormalType =
      getUncurriedOrigFormalResultType(origFormalType, uncurriedSites.size());
  (void)expectedUncurriedOrigResultFormalType;

  args.reserve(uncurriedSites.size());
  {
    ParamLowering paramLowering(substFnType, SGF);

    assert(!foreignError || uncurriedSites.size() == 1 ||
           (uncurriedSites.size() == 2 && substFnType->hasSelfParam()));

    if (!uncurriedSites.back().throws()) {
      options |= ApplyOptions::DoesNotThrow;
    }

    // Collect the captures, if any.
    if (callee.hasCaptures()) {
      (void)paramLowering.claimCaptureParams(callee.getCaptures());
      args.push_back({});
      args.back().append(callee.getCaptures().begin(),
                         callee.getCaptures().end());
    }

    // Collect the arguments to the uncurried call.
    for (auto &site : uncurriedSites) {
      AbstractionPattern origParamType = claimNextParamClause(origFormalType);
      formalApplyType = cast<FunctionType>(formalType);
      claimNextParamClause(formalType);
      uncurriedLoc = site.Loc;
      args.push_back({});

      bool isParamSite = &site == &uncurriedSites.back();

      std::move(site).emit(SGF, origParamType, paramLowering, args.back(),
                           delayedArgs,
                           // Claim the foreign error with the method
                           // formal params.
                           isParamSite ? foreignError : None,
                           // Claim the foreign "self" with the self
                           // param.
                           isParamSite ? ImportAsMemberStatus() : foreignSelf);
    }
  }
  assert(uncurriedLoc);
  assert(formalApplyType);
  assert(origFormalType.getType() ==
             expectedUncurriedOrigResultFormalType.getType() &&
         "expectedUncurriedOrigResultFormalType and emitArgumentsForNormalCall "
         "are out of sync");

  // Emit any delayed arguments: formal accesses to inout arguments, etc.
  if (!delayedArgs.empty()) {
    emitDelayedArguments(SGF, delayedArgs, args);
  }

  // Uncurry the arguments in calling convention order.
  for (auto &argSet : reversed(args))
    uncurriedArgs.append(argSet.begin(), argSet.end());
  args = {};

  // Move the foreign "self" argument into position.
  if (foreignSelf.isInstance()) {
    auto selfArg = uncurriedArgs.back();
    std::move_backward(uncurriedArgs.begin() + foreignSelf.getSelfIndex(),
                       uncurriedArgs.end() - 1, uncurriedArgs.end());
    uncurriedArgs[foreignSelf.getSelfIndex()] = selfArg;
  }

  return options;
}

RValue CallEmission::applyRemainingCallSites(RValue &&result,
                                             AbstractionPattern origFormalType,
                                             ImportAsMemberStatus foreignSelf,
                                             SGFContext C,
                                             bool formalTypeThrows) {
  assert(!extraSites.empty() &&
         "We should only get here if we actually have extra callsites");

  // Apply the remaining call sites to the result function.
  // Each chained call gets its own writeback scope.
  for (unsigned i = 0, size = extraSites.size(); i < size; ++i) {
    FormalEvaluationScope writebackScope(SGF);

    SILLocation loc = extraSites[i].Loc;

    auto functionMV = std::move(result).getAsSingleValue(SGF, loc);

    auto substFnType = functionMV.getType().castTo<SILFunctionType>();
    ParamLowering paramLowering(substFnType, SGF);

    SmallVector<ManagedValue, 4> siteArgs;
    SmallVector<DelayedArgument, 2> delayedArgs;

    // TODO: foreign errors for block or function pointer values?
    assert(substFnType->hasErrorResult() || formalTypeThrows);

    AbstractionPattern origParamType = claimNextParamClause(origFormalType);
    AbstractionPattern origResultType = origFormalType;

    SGFContext context = i == size - 1 ? C : SGFContext();

    // Create the callee type info and initialize our indirect results.
    CalleeTypeInfo calleeTypeInfo(
        substFnType, origResultType, extraSites[i].getSubstResultType(),
        Optional<ForeignErrorConvention>(), foreignSelf);
    ResultPlanPtr resultPtr =
        ResultPlanBuilder::computeResultPlan(SGF, calleeTypeInfo, loc, context);
    ArgumentScope argScope(SGF, loc);

    std::move(extraSites[i])
        .emit(SGF, origParamType, paramLowering, siteArgs, delayedArgs,
              calleeTypeInfo.foreignError, calleeTypeInfo.foreignSelf);
    if (!delayedArgs.empty()) {
      emitDelayedArguments(SGF, delayedArgs, siteArgs);
    }

    result = SGF.emitApply(std::move(resultPtr), std::move(argScope), loc,
                           functionMV, {}, siteArgs, calleeTypeInfo,
                           ApplyOptions::None, context);
  }

  return std::move(result);
}

CallEmission CallEmission::forApplyExpr(SILGenFunction &SGF, Expr *e) {
  // Set up writebacks for the call(s).
  FormalEvaluationScope writebacks(SGF);

  SILGenApply apply(SGF);

  // Decompose the call site.
  apply.decompose(e);

  // Evaluate and discard the side effect if present.
  if (apply.sideEffect)
    SGF.emitRValue(apply.sideEffect);

  // Build the call.
  // Pass the writeback scope on to CallEmission so it can thread scopes through
  // nested calls.
  CallEmission emission(SGF, apply.getCallee(), std::move(writebacks));

  // Apply 'self' if provided.
  if (apply.selfParam) {
    emission.addCallSite(RegularLocation(e), std::move(apply.selfParam),
                         apply.selfType->getCanonicalType(), /*throws*/ false);
  }

  // Apply arguments from call sites, innermost to outermost.
  for (auto site = apply.callSites.rbegin(), end = apply.callSites.rend();
       site != end;
       ++site) {
    emission.addCallSite(*site);
  }

  return emission;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entrypoints
//===----------------------------------------------------------------------===//

/// Emit a function application, assuming that the arguments have been
/// lowered appropriately for the abstraction level but that the
/// result does need to be turned back into something matching a
/// formal type.
RValue SILGenFunction::emitApply(ResultPlanPtr &&resultPlan,
                                 ArgumentScope &&argScope, SILLocation loc,
                                 ManagedValue fn, SubstitutionMap subs,
                                 ArrayRef<ManagedValue> args,
                                 const CalleeTypeInfo &calleeTypeInfo,
                                 ApplyOptions options, SGFContext evalContext) {
  auto substFnType = calleeTypeInfo.substFnType;
  auto substResultType = calleeTypeInfo.substResultType;

  // Create the result plan.
  SmallVector<SILValue, 4> indirectResultAddrs;
  resultPlan->gatherIndirectResultAddrs(*this, loc, indirectResultAddrs);

  // If the function returns an inner pointer, we'll need to lifetime-extend
  // the 'self' parameter.
  SILValue lifetimeExtendedSelf;
  bool hasAlreadyLifetimeExtendedSelf = false;
  if (hasUnownedInnerPointerResult(substFnType)) {
    auto selfMV = args.back();
    lifetimeExtendedSelf = selfMV.getValue();

    switch (substFnType->getParameters().back().getConvention()) {
    case ParameterConvention::Direct_Owned:
      // If the callee will consume the 'self' parameter, let's retain it so we
      // can keep it alive.
      lifetimeExtendedSelf =
          B.emitCopyValueOperation(loc, lifetimeExtendedSelf);
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
    case ParameterConvention::Indirect_In_Constant:
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_InoutAliasable:
      // We may need to support this at some point, but currently only imported
      // objc methods are returns_inner_pointer.
      llvm_unreachable("indirect self argument to method that"
                       " returns_inner_pointer?!");
    }
  }

  // If there's a foreign error parameter, fill it in.
  ManagedValue errorTemp;
  if (auto foreignError = calleeTypeInfo.foreignError) {
    unsigned errorParamIndex =
        calleeTypeInfo.foreignError->getErrorParameterIndex();

    // This is pretty evil.
    auto &errorArgSlot = const_cast<ManagedValue &>(args[errorParamIndex]);

    std::tie(errorTemp, errorArgSlot) =
        resultPlan->emitForeignErrorArgument(*this, loc).getValue();
  }

  // Emit the raw application.
  auto genericSig =
    fn.getType().castTo<SILFunctionType>()->getGenericSignature();

  // When calling a closure that's defined in a generic context but does not
  // capture any generic parameters, we will have substitutions, but the
  // function type won't have a generic signature. Drop the substitutions in
  // this case.
  if (genericSig == nullptr) {
    subs = SubstitutionMap();

  // Otherwise, the substitutions should match the generic signature.
  } else {
    assert(genericSig->getCanonicalSignature() ==
           subs.getGenericSignature()->getCanonicalSignature());
  }

  auto rawDirectResult = [&] {
    SmallVector<SILValue, 1> rawDirectResults;
    emitRawApply(*this, loc, fn, subs, args, substFnType, options,
                 indirectResultAddrs, rawDirectResults);
    assert(rawDirectResults.size() == 1);
    return rawDirectResults[0];
  }();

  // Pop the argument scope.
  argScope.pop();

  if (substFnType->isNoReturnFunction())
    loc.markAutoGenerated();

  // Explode the direct results.
  SILFunctionConventions substFnConv(substFnType, SGM.M);
  SmallVector<ManagedValue, 4> directResults;
  auto addManagedDirectResult = [&](SILValue result,
                                    const SILResultInfo &resultInfo) {
    auto &resultTL = getTypeLowering(resultInfo.getType());

    switch (resultInfo.getConvention()) {
    case ResultConvention::Indirect:
      assert(!substFnConv.isSILIndirect(resultInfo) &&
             "indirect direct result?");
      break;

    case ResultConvention::Owned:
      break;

    // For autoreleased results, the reclaim is implicit, so the value is
    // effectively +1.
    case ResultConvention::Autoreleased:
      break;

    // Autorelease the 'self' value to lifetime-extend it.
    case ResultConvention::UnownedInnerPointer:
      assert(lifetimeExtendedSelf &&
             "did not save lifetime-extended self param");
      if (!hasAlreadyLifetimeExtendedSelf) {
        B.createAutoreleaseValue(loc, lifetimeExtendedSelf,
                                 B.getDefaultAtomicity());
        hasAlreadyLifetimeExtendedSelf = true;
      }
      LLVM_FALLTHROUGH;

    case ResultConvention::Unowned:
      // Unretained. Retain the value.
      result = resultTL.emitCopyValue(B, loc, result);
      break;
    }

    directResults.push_back(emitManagedRValueWithCleanup(result, resultTL));
  };

  auto directSILResults = substFnConv.getDirectSILResults();
  if (directSILResults.empty()) {
    // Nothing to do.
  } else if (substFnConv.getNumDirectSILResults() == 1) {
    addManagedDirectResult(rawDirectResult, *directSILResults.begin());
  } else {
    llvm::SmallVector<std::pair<SILValue, const SILResultInfo &>, 8>
        copiedResults;
    {
      Scope S(Cleanups, CleanupLocation::get(loc));

      // First create an rvalue cleanup for our direct result.
      ManagedValue managedDirectResult =
          emitManagedRValueWithCleanup(rawDirectResult);
      // Then borrow the managed direct result.
      ManagedValue borrowedDirectResult =
          managedDirectResult.borrow(*this, loc);
      // Then create unmanaged copies of the direct result and forward the
      // result as expected by addManageDirectResult.
      unsigned Index = 0;
      for (const SILResultInfo &directResult : directSILResults) {
        ManagedValue elt =
            B.createTupleExtract(loc, borrowedDirectResult, Index,
                                 substFnConv.getSILType(directResult));
        SILValue v = elt.copyUnmanaged(*this, loc).forward(*this);
        // We assume that unowned inner pointers, autoreleased values, and
        // indirect values are never returned in tuples.
        // FIXME: can this assertion be removed without lowered addresses?
        assert(directResult.getConvention() == ResultConvention::Owned ||
               directResult.getConvention() == ResultConvention::Unowned ||
               !substFnConv.useLoweredAddresses());
        copiedResults.push_back({v, directResult});
        ++Index;
      }
      // Then allow the cleanups to be emitted in the proper reverse order.
    }
    // Finally add our managed direct results.
    for (auto p : copiedResults) {
      addManagedDirectResult(p.first, p.second);
    }
  }

  // If there was a foreign error convention, consider it.
  // TODO: maybe this should happen after managing the result if it's
  // not a result-checking convention?
  if (auto foreignError = calleeTypeInfo.foreignError) {
    bool doesNotThrow = (options & ApplyOptions::DoesNotThrow);
    emitForeignErrorCheck(loc, directResults, errorTemp, doesNotThrow,
                          *foreignError);
  }

  auto directResultsArray = makeArrayRef(directResults);
  RValue result =
      resultPlan->finish(*this, loc, substResultType, directResultsArray);
  assert(directResultsArray.empty() && "didn't claim all direct results");

  return result;
}

RValue SILGenFunction::emitMonomorphicApply(
    SILLocation loc, ManagedValue fn, ArrayRef<ManagedValue> args,
    CanType foreignResultType, CanType nativeResultType, ApplyOptions options,
    Optional<SILFunctionTypeRepresentation> overrideRep,
    const Optional<ForeignErrorConvention> &foreignError,
    SGFContext evalContext) {
  auto fnType = fn.getType().castTo<SILFunctionType>();
  assert(!fnType->isPolymorphic());
  CalleeTypeInfo calleeTypeInfo(fnType, AbstractionPattern(foreignResultType),
                                nativeResultType, foreignError,
                                ImportAsMemberStatus(), overrideRep);
  ResultPlanPtr resultPlan = ResultPlanBuilder::computeResultPlan(
      *this, calleeTypeInfo, loc, evalContext);
  ArgumentScope argScope(*this, loc);
  return emitApply(std::move(resultPlan), std::move(argScope), loc, fn, {},
                   args, calleeTypeInfo, options, evalContext);
}

/// Emit either an 'apply' or a 'try_apply', with the error branch of
/// the 'try_apply' simply branching out of all cleanups and throwing.
SILValue SILGenFunction::emitApplyWithRethrow(SILLocation loc, SILValue fn,
                                              SILType substFnType,
                                              SubstitutionMap subs,
                                              ArrayRef<SILValue> args) {
  CanSILFunctionType silFnType = substFnType.castTo<SILFunctionType>();
  SILFunctionConventions fnConv(silFnType, SGM.M);
  SILType resultType = fnConv.getSILResultType();

  if (!silFnType->hasErrorResult()) {
    return B.createApply(loc, fn, substFnType, resultType, subs, args);
  }

  SILBasicBlock *errorBB = createBasicBlock();
  SILBasicBlock *normalBB = createBasicBlock();
  B.createTryApply(loc, fn, substFnType, subs, args, normalBB, errorBB);

  // Emit the rethrow logic.
  {
    B.emitBlock(errorBB);
    SILValue error = errorBB->createPHIArgument(fnConv.getSILErrorType(),
                                                ValueOwnershipKind::Owned);

    B.createBuiltin(loc, SGM.getASTContext().getIdentifier("willThrow"),
                    SGM.Types.getEmptyTupleType(), {}, {error});

    Cleanups.emitCleanupsForReturn(CleanupLocation::get(loc));
    B.createThrow(loc, error);
  }

  // Enter the normal path.
  B.emitBlock(normalBB);
  return normalBB->createPHIArgument(resultType, ValueOwnershipKind::Owned);
}

void SILGenFunction::emitYield(SILLocation loc,
                               MutableArrayRef<ArgumentSource> valueSources,
                               ArrayRef<AbstractionPattern> origTypes,
                               JumpDest unwindDest) {
  assert(valueSources.size() == origTypes.size());

  ArgumentScope evalScope(*this, loc);

  SmallVector<ManagedValue, 4> yieldArgs;
  SmallVector<DelayedArgument, 2> delayedArgs;

  auto fnType = F.getLoweredFunctionType();
  SmallVector<SILParameterInfo, 4> substYieldTys;
  for (auto origYield : fnType->getYields()) {
    substYieldTys.push_back({
      F.mapTypeIntoContext(origYield.getType())->getCanonicalType(),
      origYield.getConvention()
    });
  }

  ArgEmitter emitter(*this, fnType->getRepresentation(),
                     ClaimedParamsRef(substYieldTys),
                     yieldArgs, delayedArgs,
                     /*foreign error*/None,
                     ImportAsMemberStatus());

  for (auto i : indices(valueSources)) {
    emitter.emitTopLevel(std::move(valueSources[i]), origTypes[i]);
  }

  if (!delayedArgs.empty())
    emitDelayedArguments(*this, delayedArgs, yieldArgs);

  SmallVector<SILValue, 4> yieldValues;
  for (auto arg : yieldArgs)
    yieldValues.push_back(arg.getValue());

  // The normal continuation block.
  auto resumeBB = createBasicBlock();

  // The unwind block.  We can use the dest block we were passed
  // directly if there are no active cleanups between here and it.
  bool requiresSeparateUnwindBB =
    Cleanups.hasAnyActiveCleanups(unwindDest.getDepth());
  auto unwindBB = requiresSeparateUnwindBB
                    ? createBasicBlock(FunctionSection::Postmatter)
                    : unwindDest.getBlock();

  // Perform the yield.
  B.createYield(loc, yieldValues, resumeBB, unwindBB);

  // Emit the unwind branch if necessary.
  if (requiresSeparateUnwindBB) {
    SILGenSavedInsertionPoint savedIP(*this, unwindBB,
                                      FunctionSection::Postmatter);
    Cleanups.emitBranchAndCleanups(unwindDest, loc);
  }

  // Emit the resumption path.
  B.emitBlock(resumeBB);
  evalScope.pop();
}

/// Emits SIL instructions to create an enum value. Attempts to avoid
/// unnecessary copies by emitting the payload directly into the enum
/// payload, or into the box in the case of an indirect payload.
ManagedValue SILGenFunction::emitInjectEnum(SILLocation loc,
                                            ArgumentSource payload,
                                            SILType enumTy,
                                            EnumElementDecl *element,
                                            SGFContext C) {
  // Easy case -- no payload
  if (!payload) {
    if (enumTy.isLoadable(SGM.M) || !silConv.useLoweredAddresses()) {
      return emitManagedRValueWithCleanup(
          B.createEnum(loc, SILValue(), element, enumTy.getObjectType()));
    }

    // Emit the enum directly into the context if possible
    return B.bufferForExpr(loc, enumTy, getTypeLowering(enumTy), C,
                           [&](SILValue newAddr) {
                             B.createInjectEnumAddr(loc, newAddr, element);
                           });
  }

  ManagedValue payloadMV;
  AbstractionPattern origFormalType =
      (element == getASTContext().getOptionalSomeDecl()
           ? AbstractionPattern(payload.getSubstType())
           : SGM.M.Types.getAbstractionPattern(element));
  auto &payloadTL = getTypeLowering(origFormalType, payload.getSubstType());

  SILType loweredPayloadType = payloadTL.getLoweredType();

  // If the payload is indirect, emit it into a heap allocated box.
  //
  // To avoid copies, evaluate it directly into the box, being
  // careful to stage the cleanups so that if the expression
  // throws, we know to deallocate the uninitialized box.
  if (element->isIndirect() || element->getParentEnum()->isIndirect()) {
    auto boxTy = SGM.M.Types.getBoxTypeForEnumElement(enumTy, element);
    auto *box = B.createAllocBox(loc, boxTy);
    auto *addr = B.createProjectBox(loc, box, 0);

    CleanupHandle initCleanup = enterDestroyCleanup(box);
    Cleanups.setCleanupState(initCleanup, CleanupState::Dormant);
    CleanupHandle uninitCleanup = enterDeallocBoxCleanup(box);

    BoxInitialization dest(box, addr, uninitCleanup, initCleanup);

    std::move(payload).forwardInto(*this, origFormalType, &dest, payloadTL);

    payloadMV = dest.getManagedBox();
    loweredPayloadType = payloadMV.getType();
  }

  // Loadable with payload
  if (enumTy.isLoadable(SGM.M) || !silConv.useLoweredAddresses()) {
    if (!payloadMV) {
      // If the payload was indirect, we already evaluated it and
      // have a single value. Otherwise, evaluate the payload.
      payloadMV = std::move(payload).getAsSingleValue(*this, origFormalType);
    }

    SILValue argValue = payloadMV.forward(*this);

    return emitManagedRValueWithCleanup(
        B.createEnum(loc, argValue, element, enumTy.getObjectType()));
  }

  // Address-only with payload
  return B.bufferForExpr(
      loc, enumTy, getTypeLowering(enumTy), C, [&](SILValue bufferAddr) {
        SILValue resultData = B.createInitEnumDataAddr(
            loc, bufferAddr, element, loweredPayloadType.getAddressType());

        if (payloadMV) {
          // If the payload was indirect, we already evaluated it and
          // have a single value. Store it into the result.
          B.emitStoreValueOperation(loc, payloadMV.forward(*this), resultData,
                                    StoreOwnershipQualifier::Init);
        } else if (payloadTL.isLoadable()) {
          // The payload of this specific enum case might be loadable
          // even if the overall enum is address-only.
          payloadMV =
              std::move(payload).getAsSingleValue(*this, origFormalType);
          B.emitStoreValueOperation(loc, payloadMV.forward(*this), resultData,
                                    StoreOwnershipQualifier::Init);
        } else {
          // The payload is address-only. Evaluate it directly into
          // the enum.

          TemporaryInitialization dest(resultData, CleanupHandle::invalid());
          std::move(payload).forwardInto(*this, origFormalType, &dest,
                                         payloadTL);
        }

        // The payload is initialized, now apply the tag.
        B.createInjectEnumAddr(loc, bufferAddr, element);
      });
}

RValue SILGenFunction::emitApplyExpr(Expr *e, SGFContext c) {
  CallEmission emission = CallEmission::forApplyExpr(*this, e);
  return emission.apply(c);
}

RValue
SILGenFunction::emitApplyOfLibraryIntrinsic(SILLocation loc,
                                            FuncDecl *fn,
                                            SubstitutionMap subMap,
                                            ArrayRef<ManagedValue> args,
                                            SGFContext ctx) {
  auto callee = Callee::forDirect(*this, SILDeclRef(fn), subMap, loc);

  auto origFormalType = callee.getOrigFormalType();
  auto substFormalType = callee.getSubstFormalType();

  auto calleeTypeInfo = callee.getTypeInfo(*this, /*isCurried=*/false);

  Optional<ManagedValue> borrowedSelf;
  if (callee.requiresSelfValueForDispatch())
    borrowedSelf = args.back();
  auto mv = callee.getFnValue(*this, /*isCurried=*/false,
                              borrowedSelf);

  assert(!calleeTypeInfo.foreignError);
  assert(!calleeTypeInfo.foreignSelf.isImportAsMember());
  assert(calleeTypeInfo.substFnType->getExtInfo().getLanguage() ==
         SILFunctionLanguage::Swift);

  calleeTypeInfo.origResultType = origFormalType.getFunctionResultType();
  calleeTypeInfo.substResultType = substFormalType.getResult();

  SILFunctionConventions silConv(calleeTypeInfo.substFnType, getModule());
  llvm::SmallVector<ManagedValue, 8> finalArgs;
  convertOwnershipConventionsGivenParamInfos(*this, silConv.getParameters(),
                                             args, loc, finalArgs);

  ResultPlanPtr resultPlan =
  ResultPlanBuilder::computeResultPlan(*this, calleeTypeInfo, loc, ctx);
  ArgumentScope argScope(*this, loc);
  return emitApply(std::move(resultPlan), std::move(argScope), loc, mv, subMap,
                   finalArgs, calleeTypeInfo, ApplyOptions::None, ctx);
}

static StringRef
getMagicFunctionString(SILGenFunction &SGF) {
  assert(SGF.MagicFunctionName
         && "asking for #function but we don't have a function name?!");
  if (SGF.MagicFunctionString.empty()) {
    llvm::raw_string_ostream os(SGF.MagicFunctionString);
    SGF.MagicFunctionName.printPretty(os);
  }
  return SGF.MagicFunctionString;
}

/// Emit an application of the given allocating initializer.
static RValue emitApplyAllocatingInitializer(SILGenFunction &SGF,
                                             SILLocation loc,
                                             ConcreteDeclRef init,
                                             RValue &&args,
                                             Type overriddenSelfType,
                                             SGFContext C) {
  ConstructorDecl *ctor = cast<ConstructorDecl>(init.getDecl());

  // Form the reference to the allocating initializer.
  auto initRef = SILDeclRef(ctor, SILDeclRef::Kind::Allocator)
    .asForeign(requiresForeignEntryPoint(ctor));
  auto initConstant = SGF.getConstantInfo(initRef);
  auto subs = init.getSubstitutions();

  // Scope any further writeback just within this operation.
  FormalEvaluationScope writebackScope(SGF);

  // Form the metatype argument.
  ManagedValue selfMetaVal;
  SILType selfMetaTy;
  {
    // Determine the self metatype type.
    CanSILFunctionType substFnType =
      initConstant.SILFnType->substGenericArgs(SGF.SGM.M, subs);
    SILType selfParamMetaTy = SGF.getSILType(substFnType->getSelfParameter());

    if (overriddenSelfType) {
      // If the 'self' type has been overridden, form a metatype to the
      // overriding 'Self' type.
      Type overriddenSelfMetaType =
        MetatypeType::get(overriddenSelfType,
                          selfParamMetaTy.castTo<MetatypeType>()
                              ->getRepresentation());
      selfMetaTy =
        SGF.getLoweredType(overriddenSelfMetaType->getCanonicalType());
    } else {
      selfMetaTy = selfParamMetaTy;
    }

    // Form the metatype value.
    SILValue selfMeta = SGF.B.createMetatype(loc, selfMetaTy);

    // If the types differ, we need an upcast.
    if (selfMetaTy != selfParamMetaTy)
      selfMeta = SGF.B.createUpcast(loc, selfMeta, selfParamMetaTy);

    selfMetaVal = ManagedValue::forUnmanaged(selfMeta);
  }

  // Form the callee.
  Optional<Callee> callee;
  if (isa<ProtocolDecl>(ctor->getDeclContext())) {
    callee.emplace(Callee::forWitnessMethod(
        SGF, selfMetaVal.getType().getASTType(),
        initRef, subs, loc));
  } else if (getMethodDispatch(ctor) == MethodDispatch::Class) {
    callee.emplace(Callee::forClassMethod(SGF, initRef, subs, loc));
  } else {
    callee.emplace(Callee::forDirect(SGF, initRef, subs, loc));
  }

  auto substFormalType = callee->getSubstFormalType();

  // For an inheritable initializer, determine whether we'll need to adjust the
  // result type.
  bool requiresDowncast = false;
  if (ctor->isRequired() && overriddenSelfType) {
    CanType substResultType = substFormalType;
    substResultType = cast<FunctionType>(substResultType).getResult();
    substResultType = cast<FunctionType>(substResultType).getResult();

    if (!substResultType->isEqual(overriddenSelfType))
      requiresDowncast = true;
  }

  // Form the call emission.
  CallEmission emission(SGF, std::move(*callee), std::move(writebackScope));

  // Self metatype.
  emission.addCallSite(loc,
                       ArgumentSource(loc,
                                      RValue(SGF, loc,
                                             selfMetaVal.getType()
                                               .getASTType(),
                                             std::move(selfMetaVal))),
                       substFormalType);

  // Arguments
  emission.addCallSite(loc, ArgumentSource(loc, std::move(args)),
                       cast<FunctionType>(substFormalType.getResult()));

  // Perform the call.
  RValue result = emission.apply(requiresDowncast ? SGFContext() : C);

  // If we need a downcast, do it down.
  if (requiresDowncast) {
    ManagedValue v = std::move(result).getAsSingleValue(SGF, loc);
    CanType canOverriddenSelfType = overriddenSelfType->getCanonicalType();
    SILType loweredResultTy = SGF.getLoweredType(canOverriddenSelfType);
    v = SGF.B.createUncheckedRefCast(loc, v, loweredResultTy);
    result = RValue(SGF, loc, canOverriddenSelfType, v);
  }

  return result;
}

/// Emit a literal that applies the various initializers.
RValue SILGenFunction::emitLiteral(LiteralExpr *literal, SGFContext C) {
  ConcreteDeclRef builtinInit;
  ConcreteDeclRef init;
  // Emit the raw, builtin literal arguments.
  RValue builtinLiteralArgs;
  if (auto stringLiteral = dyn_cast<StringLiteralExpr>(literal)) {
    builtinLiteralArgs = emitStringLiteral(*this, literal,
                                           stringLiteral->getValue(), C,
                                           stringLiteral->getEncoding());
    builtinInit = stringLiteral->getBuiltinInitializer();
    init = stringLiteral->getInitializer();
  } else {
    ASTContext &ctx = getASTContext();
    SourceLoc loc = literal->getStartLoc();

    auto magicLiteral = cast<MagicIdentifierLiteralExpr>(literal);
    switch (magicLiteral->getKind()) {
    case MagicIdentifierLiteralExpr::File: {
      std::string value;
      if (loc.isValid())
        value = ctx.SourceMgr.getDisplayNameForLoc(loc);
      builtinLiteralArgs = emitStringLiteral(*this, literal, value, C,
                                             magicLiteral->getStringEncoding());
      builtinInit = magicLiteral->getBuiltinInitializer();
      init = magicLiteral->getInitializer();
      break;
    }

    case MagicIdentifierLiteralExpr::Function: {
      StringRef value = "";
      if (loc.isValid())
        value = getMagicFunctionString(*this);
      builtinLiteralArgs = emitStringLiteral(*this, literal, value, C,
                                             magicLiteral->getStringEncoding());
      builtinInit = magicLiteral->getBuiltinInitializer();
      init = magicLiteral->getInitializer();
      break;
    }

    case MagicIdentifierLiteralExpr::Line:
    case MagicIdentifierLiteralExpr::Column:
    case MagicIdentifierLiteralExpr::DSOHandle:
      llvm_unreachable("handled elsewhere");
    }
  }

  // Helper routine to add an argument label if we need one.
  auto relabelArgument = [&](ConcreteDeclRef callee, RValue &arg) {
    auto name = callee.getDecl()->getFullName();
    auto argLabels = name.getArgumentNames();
    if (argLabels.size() == 1 && !argLabels[0].empty() &&
        !isa<TupleType>(arg.getType())) {
      Type newType = TupleType::get({TupleTypeElt(arg.getType(), argLabels[0])},
                                    getASTContext());
      arg.rewriteType(newType->getCanonicalType());
    }
  };

  // Call the builtin initializer.
  relabelArgument(builtinInit, builtinLiteralArgs);
  RValue builtinLiteral =
    emitApplyAllocatingInitializer(*this, literal, builtinInit,
                                   std::move(builtinLiteralArgs),
                                   Type(),
                                   init ? SGFContext() : C);

  // If we were able to directly initialize the literal we wanted, we're done.
  if (!init) return builtinLiteral;

  // Otherwise, perform the second initialization step.
  relabelArgument(init, builtinLiteral);
  RValue result = emitApplyAllocatingInitializer(*this, literal, init,
                                                 std::move(builtinLiteral),
                                                 literal->getType(), C);
  return result;
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

  // Invoke the intrinsic, which returns a tuple.
  auto subMap = ArrayTy->getContextSubstitutionMap(SGM.M.getSwiftModule(),
                                                   Ctx.getArrayDecl());
  auto result = emitApplyOfLibraryIntrinsic(Loc, allocate,
                                            subMap,
                                            ManagedValue::forUnmanaged(Length),
                                            SGFContext());

  // Explode the tuple.
  SmallVector<ManagedValue, 2> resultElts;
  std::move(result).getAll(resultElts);

  return {resultElts[0], resultElts[1].getUnmanagedValue()};
}

/// Deallocate an uninitialized array.
void SILGenFunction::emitUninitializedArrayDeallocation(SILLocation loc,
                                                        SILValue array) {
  auto &Ctx = getASTContext();
  auto deallocate = Ctx.getDeallocateUninitializedArray(nullptr);

  CanType arrayTy = array->getType().getASTType();

  // Invoke the intrinsic.
  auto subMap = arrayTy->getContextSubstitutionMap(SGM.M.getSwiftModule(),
                                                   Ctx.getArrayDecl());
  emitApplyOfLibraryIntrinsic(loc, deallocate, subMap,
                              ManagedValue::forUnmanaged(array),
                              SGFContext());
}

namespace {
  /// A cleanup that deallocates an uninitialized array.
  class DeallocateUninitializedArray: public Cleanup {
    SILValue Array;
  public:
    DeallocateUninitializedArray(SILValue array)
      : Array(array) {}

    void emit(SILGenFunction &SGF, CleanupLocation l, ForUnwind_t forUnwind) override {
      SGF.emitUninitializedArrayDeallocation(l, Array);
    }

    void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
      llvm::errs() << "DeallocateUninitializedArray "
                   << "State:" << getState() << " "
                   << "Array:" << Array << "\n";
#endif
    }
  };
} // end anonymous namespace

CleanupHandle
SILGenFunction::enterDeallocateUninitializedArrayCleanup(SILValue array) {
  Cleanups.pushCleanup<DeallocateUninitializedArray>(array);
  return Cleanups.getTopCleanup();
}

static Callee getBaseAccessorFunctionRef(SILGenFunction &SGF,
                                         SILLocation loc,
                                         SILDeclRef constant,
                                         ArgumentSource &selfValue,
                                         bool isSuper,
                                         bool isDirectUse,
                                         SubstitutionMap subs) {
  auto *decl = cast<AbstractFunctionDecl>(constant.getDecl());

  // The accessor might be a local function that does not capture any
  // generic parameters, in which case we don't want to pass in any
  // substitutions.
  auto captureInfo = SGF.SGM.Types.getLoweredLocalCaptures(decl);
  if (decl->getDeclContext()->isLocalContext() &&
      !captureInfo.hasGenericParamCaptures()) {
    subs = SubstitutionMap();
  }

  // If this is a method in a protocol, generate it as a protocol call.
  if (isa<ProtocolDecl>(decl->getDeclContext())) {
    assert(!isDirectUse && "direct use of protocol accessor?");
    assert(!isSuper && "super call to protocol method?");

    return Callee::forWitnessMethod(
        SGF, selfValue.getSubstRValueType(),
        constant, subs, loc);
  }

  bool isClassDispatch = false;
  if (!isDirectUse) {
    switch (getMethodDispatch(decl)) {
    case MethodDispatch::Class:
      isClassDispatch = true;
      break;
    case MethodDispatch::Static:
      isClassDispatch = false;
      break;
    }
  }

  // Dispatch in a struct/enum or to a final method is always direct.
  if (!isClassDispatch)
    return Callee::forDirect(SGF, constant, subs, loc);

  // Otherwise, if we have a non-final class dispatch to a normal method,
  // perform a dynamic dispatch.
  if (!isSuper)
    return Callee::forClassMethod(SGF, constant, subs, loc);

  // If this is a "super." dispatch, we do a dynamic dispatch for objc methods
  // or non-final native Swift methods.
  if (!canUseStaticDispatch(SGF, constant))
    return Callee::forSuperMethod(SGF, constant, subs, loc);

  return Callee::forDirect(SGF, constant, subs, loc);
}

static Callee
emitSpecializedAccessorFunctionRef(SILGenFunction &SGF,
                                   SILLocation loc,
                                   SILDeclRef constant,
                                   SubstitutionMap substitutions,
                                   ArgumentSource &selfValue,
                                   bool isSuper,
                                   bool isDirectUse)
{
  // Get the accessor function. The type will be a polymorphic function if
  // the Self type is generic.
  Callee callee = getBaseAccessorFunctionRef(SGF, loc, constant, selfValue,
                                             isSuper, isDirectUse,
                                             substitutions);
  
  // Collect captures if the accessor has them.
  auto accessorFn = cast<AbstractFunctionDecl>(constant.getDecl());
  if (SGF.SGM.M.Types.hasLoweredLocalCaptures(accessorFn)) {
    assert(!selfValue && "local property has self param?!");
    SmallVector<ManagedValue, 4> captures;
    SGF.emitCaptures(loc, accessorFn, CaptureEmission::ImmediateApplication,
                     captures);
    callee.setCaptures(std::move(captures));
  }

  return callee;
}

namespace {

/// A builder class that creates the base argument for accessors.
///
/// *NOTE* All cleanups created inside of this builder on base arguments must be
/// formal access to ensure that we do not extend the lifetime of a guaranteed
/// base after the accessor is evaluated.
struct AccessorBaseArgPreparer final {
  SILGenFunction &SGF;
  SILLocation loc;
  ManagedValue base;
  CanType baseFormalType;
  SILDeclRef accessor;
  SILParameterInfo selfParam;
  SILType baseLoweredType;

  AccessorBaseArgPreparer(SILGenFunction &SGF, SILLocation loc,
                          ManagedValue base, CanType baseFormalType,
                          SILDeclRef accessor);
  ArgumentSource prepare();

private:
  /// Prepare our base if we have an address base.
  ArgumentSource prepareAccessorAddressBaseArg();
  /// Prepare our base if we have an object base.
  ArgumentSource prepareAccessorObjectBaseArg();

  /// Returns true if given an address base, we need to load the underlying
  /// address. Asserts if baseLoweredType is not an address.
  bool shouldLoadBaseAddress() const;
};

} // end anonymous namespace

bool AccessorBaseArgPreparer::shouldLoadBaseAddress() const {
  assert(baseLoweredType.isAddress() &&
         "Should only call this helper method if the base is an address");
  switch (selfParam.getConvention()) {
  // If the accessor wants the value 'inout', always pass the
  // address we were given.  This is semantically required.
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    return false;

  // If the accessor wants the value 'in', we have to copy if the
  // base isn't a temporary.  We aren't allowed to pass aliased
  // memory to 'in', and we have pass at +1.
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Guaranteed:
    // TODO: We shouldn't be able to get an lvalue here, but the AST
    // sometimes produces an inout base for non-mutating accessors.
    // rdar://problem/19782170
    // assert(!base.isLValue());
    return base.isLValue() || base.isPlusZeroRValueOrTrivial();

  // If the accessor wants the value directly, we definitely have to
  // load.
  case ParameterConvention::Direct_Owned:
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
    return true;

  // Should not show up here.
  case ParameterConvention::Indirect_In_Constant:
    break;
  }
  llvm_unreachable("bad convention");
}

ArgumentSource AccessorBaseArgPreparer::prepareAccessorAddressBaseArg() {
  // If the base is currently an address, we may have to copy it.
  if (shouldLoadBaseAddress()) {
    if (selfParam.isConsumed() ||
        base.getType().isAddressOnly(SGF.getModule())) {
      // The load can only be a take if the base is a +1 rvalue.
      auto shouldTake = IsTake_t(base.hasCleanup());

      base = SGF.emitFormalAccessLoad(loc, base.forward(SGF),
                                      SGF.getTypeLowering(baseLoweredType),
                                      SGFContext(), shouldTake);
      return ArgumentSource(loc, RValue(SGF, loc, baseFormalType, base));
    }

    // If we do not have a consumed base and need to perform a load, perform a
    // formal access load borrow.
    base = SGF.B.createFormalAccessLoadBorrow(loc, base);
    return ArgumentSource(loc, RValue(SGF, loc, baseFormalType, base));
  }

  // Handle inout bases specially here.
  if (selfParam.isIndirectInOut()) {
    // It sometimes happens that we get r-value bases here, e.g. when calling a
    // mutating setter on a materialized temporary.  Just don't claim the value.
    if (!base.isLValue()) {
      base = ManagedValue::forLValue(base.getValue());
    }

    // FIXME: this assumes that there's never meaningful reabstraction of self
    // arguments.
    return ArgumentSource(
        loc, LValue::forAddress(base, None, AbstractionPattern(baseFormalType),
                                baseFormalType));
  }

  // Otherwise, we have a value that we can forward without any additional
  // handling.
  return ArgumentSource(loc, RValue(SGF, loc, baseFormalType, base));
}

ArgumentSource AccessorBaseArgPreparer::prepareAccessorObjectBaseArg() {
  // If the base is currently scalar, we may have to drop it in
  // memory or copy it.
  assert(!base.isLValue());

  // We need to produce the value at +1 if it's going to be consumed.
  if (selfParam.isConsumed() && !base.hasCleanup()) {
    base = base.formalAccessCopyUnmanaged(SGF, loc);
  }

  // If the parameter is indirect, we need to drop the value into
  // temporary memory.
  if (SGF.silConv.isSILIndirect(selfParam)) {
    // It's a really bad idea to materialize when we're about to
    // pass a value to an inout argument, because it's a really easy
    // way to silently drop modifications (e.g. from a mutating
    // getter in a writeback pair).  Our caller should always take
    // responsibility for that decision (by doing the materialization
    // itself).
    assert(!selfParam.isIndirectMutating() &&
           "passing unmaterialized r-value as inout argument");

    base = base.materialize(SGF, loc);
  }

  return ArgumentSource(loc, RValue(SGF, loc, baseFormalType, base));
}

AccessorBaseArgPreparer::AccessorBaseArgPreparer(SILGenFunction &SGF,
                                                 SILLocation loc,
                                                 ManagedValue base,
                                                 CanType baseFormalType,
                                                 SILDeclRef accessor)
    : SGF(SGF), loc(loc), base(base), baseFormalType(baseFormalType),
      accessor(accessor),
      selfParam(SGF.SGM.Types.getConstantSelfParameter(accessor)),
      baseLoweredType(base.getType()) {
  assert(!base.isInContext());
  assert(!base.isLValue() || !base.hasCleanup());
}

ArgumentSource AccessorBaseArgPreparer::prepare() {
  // If the base is a boxed existential, we will open it later.
  if (baseLoweredType.getPreferredExistentialRepresentation(SGF.SGM.M) ==
      ExistentialRepresentation::Boxed) {
    assert(!baseLoweredType.isAddress() &&
           "boxed existential should not be an address");
    return ArgumentSource(loc, RValue(SGF, loc, baseFormalType, base));
  }

  if (baseLoweredType.isAddress())
    return prepareAccessorAddressBaseArg();

  // At this point, we know we have an object.
  assert(baseLoweredType.isObject());
  return prepareAccessorObjectBaseArg();
}

ArgumentSource SILGenFunction::prepareAccessorBaseArg(SILLocation loc,
                                                      ManagedValue base,
                                                      CanType baseFormalType,
                                                      SILDeclRef accessor) {
  if (!base)
    return ArgumentSource();

  AccessorBaseArgPreparer Preparer(*this, loc, base, baseFormalType, accessor);
  return Preparer.prepare();
}

SILDeclRef SILGenModule::getGetterDeclRef(AbstractStorageDecl *storage) {
  auto *getter = storage->getGetter();
  return SILDeclRef(getter, SILDeclRef::Kind::Func)
    .asForeign(requiresForeignEntryPoint(getter));
}

/// Emit a call to a getter.
RValue SILGenFunction::
emitGetAccessor(SILLocation loc, SILDeclRef get,
                SubstitutionMap substitutions,
                ArgumentSource &&selfValue,
                bool isSuper, bool isDirectUse,
                RValue &&subscripts, SGFContext c) {
  // Scope any further writeback just within this operation.
  FormalEvaluationScope writebackScope(*this);

  Callee getter = emitSpecializedAccessorFunctionRef(*this, loc, get,
                                                     substitutions, selfValue,
                                                     isSuper, isDirectUse);
  bool hasSelf = (bool)selfValue;
  CanAnyFunctionType accessType = getter.getSubstFormalType();

  CallEmission emission(*this, std::move(getter), std::move(writebackScope));
  // Self ->
  if (hasSelf) {
    emission.addCallSite(loc, std::move(selfValue), accessType);
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }
  // Index or () if none.
  if (subscripts.isNull())
    subscripts = emitEmptyTupleRValue(loc, SGFContext());

  emission.addCallSite(loc, ArgumentSource(loc, std::move(subscripts)),
                       accessType);

  // T
  return emission.apply(c);
}

SILDeclRef SILGenModule::getSetterDeclRef(AbstractStorageDecl *storage) {
  auto *setter = storage->getSetter();
  return SILDeclRef(setter, SILDeclRef::Kind::Func)
    .asForeign(requiresForeignEntryPoint(setter));
}

void SILGenFunction::emitSetAccessor(SILLocation loc, SILDeclRef set,
                                     SubstitutionMap substitutions,
                                     ArgumentSource &&selfValue,
                                     bool isSuper, bool isDirectUse,
                                     RValue &&subscripts,
                                     ArgumentSource &&setValue) {
  // Scope any further writeback just within this operation.
  FormalEvaluationScope writebackScope(*this);

  Callee setter = emitSpecializedAccessorFunctionRef(*this, loc, set,
                                                     substitutions, selfValue,
                                                     isSuper, isDirectUse);
  bool hasSelf = (bool)selfValue;
  CanAnyFunctionType accessType = setter.getSubstFormalType();

  CallEmission emission(*this, std::move(setter), std::move(writebackScope));
  // Self ->
  if (hasSelf) {
    emission.addCallSite(loc, std::move(selfValue), accessType);
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }

  // (value)  or (value, indices)
  if (!subscripts.isNull()) {
    // If we have a value and index list, create a new rvalue to represent the
    // both of them together.
    auto inputTupleType = dyn_cast<TupleType>(accessType.getInput());

    SmallVector<ArgumentSource, 4> eltSources;

    // The value comes first.
    eltSources.push_back(std::move(setValue));

    // The indices come after.  Whether they are expanded or not depends on
    // whether they were written as separate parameters, which should be
    // reflected in the params list.
    // TODO: we should really take an array of RValues.
    if (accessType->getNumParams() != 2) {
      auto subscriptsTupleType = cast<TupleType>(subscripts.getType());
      assert(accessType.getParams().size()
              == 1 + subscriptsTupleType->getNumElements());
      (void)subscriptsTupleType;
      SmallVector<RValue, 8> eltRVs;
      std::move(subscripts).extractElements(eltRVs);
      for (auto &elt : eltRVs)
        eltSources.emplace_back(loc, std::move(elt));
    } else {
      assert(inputTupleType && "Must have an input tuple here");
      subscripts.rewriteType(inputTupleType.getElementType(1));
      eltSources.emplace_back(loc, std::move(subscripts));
    }

    if (eltSources.size() == 1) {
      setValue = std::move(eltSources.front());
    } else {
      assert(inputTupleType);
      setValue = ArgumentSource(loc, inputTupleType, eltSources);
    }

  } else {
    setValue.rewriteType(accessType.getInput());
  }
  emission.addCallSite(loc, std::move(setValue), accessType);
  // ()
  emission.apply();
}

SILDeclRef
SILGenModule::getMaterializeForSetDeclRef(AbstractStorageDecl *storage) {
  return SILDeclRef(storage->getMaterializeForSetFunc(),
                    SILDeclRef::Kind::Func);
}

MaterializedLValue SILGenFunction::
emitMaterializeForSetAccessor(SILLocation loc, SILDeclRef materializeForSet,
                              SubstitutionMap substitutions,
                              ArgumentSource &&selfValue,
                              bool isSuper, bool isDirectUse,
                              RValue &&subscripts, SILValue buffer,
                              SILValue callbackStorage) {
  // Scope any further writeback just within this operation.
  FormalEvaluationScope writebackScope(*this);

  Callee callee = emitSpecializedAccessorFunctionRef(*this, loc,
                                                     materializeForSet,
                                                     substitutions, selfValue,
                                                     isSuper, isDirectUse);
  callee.isMaterializeForSet = true;

  bool hasSelf = (bool)selfValue;
  auto accessType = callee.getSubstFormalType();

  CallEmission emission(*this, std::move(callee), std::move(writebackScope));
  // Self ->
  if (hasSelf) {
    emission.addCallSite(loc, std::move(selfValue), accessType);
    accessType = cast<FunctionType>(accessType.getResult());
  }

  // (buffer, callbackStorage)  or (buffer, callbackStorage, indices) ->
  // Note that this "RValue" stores a mixed LValue/RValue tuple.
  RValue args = [&] () -> RValue {
    SmallVector<ManagedValue, 4> elts;

    auto bufferPtr =
      B.createAddressToPointer(loc, buffer,
                               SILType::getRawPointerType(getASTContext()));
    elts.push_back(ManagedValue::forUnmanaged(bufferPtr));

    elts.push_back(ManagedValue::forLValue(callbackStorage));

    if (!subscripts.isNull()) {
      std::move(subscripts).getAll(elts);
    }
    return RValue(*this, elts, accessType.getInput());
  }();
  emission.addCallSite(loc, ArgumentSource(loc, std::move(args)), accessType);
  // (buffer, optionalCallback)
  SmallVector<ManagedValue, 2> results;
  emission.apply().getAll(results);

  // Project out the materialized address. The address directly returned by
  // materialize for set is strictly typed, whether it is the local buffer or
  // stored property.
  SILValue address = results[0].getUnmanagedValue();
  address = B.createPointerToAddress(loc, address, buffer->getType(),
                                     /*isStrict*/ true,
                                     /*isInvariant*/ false);

  // Project out the optional callback.
  SILValue optionalCallback = results[1].getUnmanagedValue();

  auto origAccessType = SGM.Types.getConstantInfo(materializeForSet).FormalType;

  assert(origAccessType->getParams().size() == 1 &&
         "more than one self parameter?");
  auto origSelfType = origAccessType
      ->getParams()[0].getPlainType()
      ->getCanonicalType();

  CanGenericSignature genericSig;
  if (auto genericFnType = dyn_cast<GenericFunctionType>(origAccessType))
    genericSig = genericFnType.getGenericSignature();

  return MaterializedLValue(ManagedValue::forLValue(address),
                            origSelfType, genericSig,
                            optionalCallback, callbackStorage);
}

SILDeclRef SILGenModule::getAddressorDeclRef(AbstractStorageDecl *storage) {
  FuncDecl *addressorFunc = storage->getAddressor();
  return SILDeclRef(addressorFunc, SILDeclRef::Kind::Func);
}

SILDeclRef SILGenModule::getMutableAddressorDeclRef(
                                                 AbstractStorageDecl *storage) {
  FuncDecl *addressorFunc = storage->getMutableAddressor();
  return SILDeclRef(addressorFunc, SILDeclRef::Kind::Func);
}

/// Emit a call to an addressor.
///
/// The first return value is the address, which will always be an
/// l-value managed value.  The second return value is the owner
/// pointer, if applicable.
std::pair<ManagedValue, ManagedValue> SILGenFunction::
emitAddressorAccessor(SILLocation loc, SILDeclRef addressor,
                      SubstitutionMap substitutions,
                      ArgumentSource &&selfValue,
                      bool isSuper, bool isDirectUse,
                      RValue &&subscripts, SILType addressType) {
  // Scope any further writeback just within this operation.
  FormalEvaluationScope writebackScope(*this);

  Callee callee =
    emitSpecializedAccessorFunctionRef(*this, loc, addressor,
                                       substitutions, selfValue,
                                       isSuper, isDirectUse);
  bool hasSelf = (bool)selfValue;
  CanAnyFunctionType accessType = callee.getSubstFormalType();

  CallEmission emission(*this, std::move(callee), std::move(writebackScope));
  // Self ->
  if (hasSelf) {
    emission.addCallSite(loc, std::move(selfValue), accessType);
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }
  // Index or () if none.
  if (subscripts.isNull())
    subscripts = emitEmptyTupleRValue(loc, SGFContext());

  emission.addCallSite(loc, ArgumentSource(loc, std::move(subscripts)),
                       accessType);

  // Unsafe{Mutable}Pointer<T> or
  // (Unsafe{Mutable}Pointer<T>, Builtin.UnknownPointer) or
  // (Unsafe{Mutable}Pointer<T>, Builtin.NativePointer) or
  // (Unsafe{Mutable}Pointer<T>, Builtin.NativePointer?) or
  SmallVector<ManagedValue, 2> results;
  emission.apply().getAll(results);

  SILValue pointer;
  ManagedValue owner;
  switch (cast<AccessorDecl>(addressor.getDecl())->getAddressorKind()) {
  case AddressorKind::NotAddressor:
    llvm_unreachable("not an addressor!");
  case AddressorKind::Unsafe:
    assert(results.size() == 1);
    pointer = results[0].getUnmanagedValue();
    owner = ManagedValue();
    break;
  case AddressorKind::Owning:
  case AddressorKind::NativeOwning:
  case AddressorKind::NativePinning:
    assert(results.size() == 2);
    pointer = results[0].getUnmanagedValue();
    owner = results[1];
    break;
  }

  // Drill down to the raw pointer using intrinsic knowledge of those types.
  auto pointerType =
    pointer->getType().castTo<BoundGenericStructType>()->getDecl();
  auto props = pointerType->getStoredProperties();
  assert(props.begin() != props.end());
  assert(std::next(props.begin()) == props.end());
  VarDecl *rawPointerField = *props.begin();
  pointer = B.createStructExtract(loc, pointer, rawPointerField,
                                  SILType::getRawPointerType(getASTContext()));

  // Convert to the appropriate address type and return.
  SILValue address = B.createPointerToAddress(loc, pointer, addressType,
                                              /*isStrict*/ true,
                                              /*isInvariant*/ false);

  // Mark dependence as necessary.
  switch (cast<AccessorDecl>(addressor.getDecl())->getAddressorKind()) {
  case AddressorKind::NotAddressor:
    llvm_unreachable("not an addressor!");
  case AddressorKind::Unsafe:
    // TODO: we should probably mark dependence on the base.
    break;
  case AddressorKind::Owning:
  case AddressorKind::NativeOwning:
  case AddressorKind::NativePinning:
    address = B.createMarkDependence(loc, address, owner.getValue());
    break;
  }

  return { ManagedValue::forLValue(address), owner };
}

SILDeclRef
SILGenModule::getReadCoroutineDeclRef(AbstractStorageDecl *storage) {
  FuncDecl *accessorFunc = storage->getReadCoroutine();
  assert(accessorFunc);
  return SILDeclRef(accessorFunc, SILDeclRef::Kind::Func);
}

SILDeclRef
SILGenModule::getModifyCoroutineDeclRef(AbstractStorageDecl *storage) {
  FuncDecl *accessorFunc = storage->getModifyCoroutine();
  assert(accessorFunc);
  return SILDeclRef(accessorFunc, SILDeclRef::Kind::Func);
}

CleanupHandle
SILGenFunction::emitCoroutineAccessor(SILLocation loc, SILDeclRef accessor,
                                      SubstitutionMap substitutions,
                                      ArgumentSource &&selfValue,
                                      bool isSuper, bool isDirectUse,
                                      RValue &&subscripts,
                                      SmallVectorImpl<ManagedValue> &yields) {
  Callee callee =
    emitSpecializedAccessorFunctionRef(*this, loc, accessor,
                                       substitutions, selfValue,
                                       isSuper, isDirectUse);

  // We're already in a full formal-evaluation scope.
  // Make a dead writeback scope; applyCoroutine won't try to pop this.
  FormalEvaluationScope writebackScope(*this);
  writebackScope.pop();

  bool hasSelf = (bool)selfValue;
  CanAnyFunctionType accessType = callee.getSubstFormalType();

  CallEmission emission(*this, std::move(callee), std::move(writebackScope));
  // Self ->
  if (hasSelf) {
    emission.addCallSite(loc, std::move(selfValue), accessType);
    accessType = cast<AnyFunctionType>(accessType.getResult());
  }
  // Index or () if none.
  if (subscripts.isNull())
    subscripts = emitEmptyTupleRValue(loc, SGFContext());

  emission.addCallSite(loc, ArgumentSource(loc, std::move(subscripts)),
                       accessType);

  auto endApplyHandle = emission.applyCoroutine(yields);

  return endApplyHandle;
}

// Create a partial application of a dynamic method, applying bridging thunks
// if necessary.
static ManagedValue emitDynamicPartialApply(SILGenFunction &SGF,
                                            SILLocation loc,
                                            SILValue method,
                                            SILValue self,
                                         CanAnyFunctionType foreignFormalType,
                                         CanAnyFunctionType nativeFormalType) {
  auto calleeConvention = ParameterConvention::Direct_Guaranteed;

  auto partialApplyTy =
      SILBuilder::getPartialApplyResultType(method->getType(),
                                            /*argCount*/ 1, SGF.SGM.M,
                                            /*subs*/ {}, calleeConvention);

  // Retain 'self' because the partial apply will take ownership.
  // We can't simply forward 'self' because the partial apply is conditional.
  if (!self->getType().isAddress())
    self = SGF.B.emitCopyValueOperation(loc, self);

  SILValue resultValue =
    SGF.B.createPartialApply(loc, method, method->getType(), {},
                             self, partialApplyTy);
  ManagedValue result = SGF.emitManagedRValueWithCleanup(resultValue);

  // If necessary, thunk to the native ownership conventions and bridged types.
  auto nativeTy =
    SGF.getLoweredLoadableType(nativeFormalType).castTo<SILFunctionType>();

  if (nativeTy != partialApplyTy.getASTType()) {
    result = SGF.emitBlockToFunc(loc, result, foreignFormalType,
                                 nativeFormalType, nativeTy);
  }

  return result;
}

RValue SILGenFunction::emitDynamicMemberRefExpr(DynamicMemberRefExpr *e,
                                                SGFContext c) {
  // Emit the operand.
  ManagedValue base = emitRValueAsSingleValue(e->getBase());

  SILValue operand = base.getValue();
  if (!e->getMember().getDecl()->isInstanceMember()) {
    auto metatype = operand->getType().castTo<MetatypeType>();
    assert(metatype->getRepresentation() == MetatypeRepresentation::Thick);
    metatype = CanMetatypeType::get(metatype.getInstanceType(),
                                    MetatypeRepresentation::ObjC);
    operand = B.createThickToObjCMetatype(e, operand,
                                    SILType::getPrimitiveObjectType(metatype));
  }

  // Create the continuation block.
  SILBasicBlock *contBB = createBasicBlock();

  // Create the no-member block.
  SILBasicBlock *noMemberBB = createBasicBlock();

  // Create the has-member block.
  SILBasicBlock *hasMemberBB = createBasicBlock();

  // The continuation block
  auto memberMethodTy = e->getType()->getOptionalObjectType();

  const TypeLowering &optTL = getTypeLowering(e->getType());
  auto loweredOptTy = optTL.getLoweredType();

  SILValue optTemp = emitTemporaryAllocation(e, loweredOptTy);

  // Create the branch.
  FuncDecl *memberFunc;
  if (auto *VD = dyn_cast<VarDecl>(e->getMember().getDecl())) {
    memberFunc = VD->getGetter();
    memberMethodTy = FunctionType::get(getASTContext().TheEmptyTupleType,
                                       memberMethodTy);
  } else
    memberFunc = cast<FuncDecl>(e->getMember().getDecl());
  auto member = SILDeclRef(memberFunc, SILDeclRef::Kind::Func)
    .asForeign();
  B.createDynamicMethodBranch(e, operand, member, hasMemberBB, noMemberBB);

  // Create the has-member branch.
  {
    B.emitBlock(hasMemberBB);

    FullExpr hasMemberScope(Cleanups, CleanupLocation(e));

    // The argument to the has-member block is the uncurried method.
    auto valueTy = e->getType()->getCanonicalType().getOptionalObjectType();
    CanFunctionType methodTy;

    // For a computed variable, we want the getter.
    if (isa<VarDecl>(e->getMember().getDecl())) {
      methodTy = CanFunctionType::get(TupleType::getEmpty(getASTContext()),
                                      valueTy);
    } else {
      methodTy = cast<FunctionType>(valueTy);
    }

    // Build a partially-applied foreign formal type.
    // TODO: instead of building this and then potentially converting, we
    // should just build a single thunk.
    auto foreignMethodTy =
      getPartialApplyOfDynamicMethodFormalType(SGM, member, e->getMember());

    auto memberFnTy = CanFunctionType::get(
                                       operand->getType().getASTType(),
                                       memberMethodTy->getCanonicalType());

    auto loweredMethodTy = getDynamicMethodLoweredType(SGM.M, member,
                                                       memberFnTy);
    SILValue memberArg = hasMemberBB->createPHIArgument(
        loweredMethodTy, ValueOwnershipKind::Owned);

    // Create the result value.
    Scope applyScope(Cleanups, CleanupLocation(e));
    ManagedValue result =
      emitDynamicPartialApply(*this, e, memberArg, operand,
                              foreignMethodTy, methodTy);

    RValue resultRV;
    if (isa<VarDecl>(e->getMember().getDecl())) {
      resultRV = emitMonomorphicApply(e, result, {},
                                      foreignMethodTy.getResult(), valueTy,
                                      ApplyOptions::DoesNotThrow,
                                      None, None);
    } else {
      resultRV = RValue(*this, e, valueTy, result);
    }

    // Package up the result in an optional.
    emitInjectOptionalValueInto(e, {e, std::move(resultRV)}, optTemp, optTL);

    applyScope.pop();
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
  auto optResult = optTemp;
  if (optTL.isLoadable())
    optResult = optTL.emitLoad(B, e, optResult, LoadOwnershipQualifier::Take);
  return RValue(*this, e, emitManagedRValueWithCleanup(optResult, optTL));
}

RValue SILGenFunction::emitDynamicSubscriptExpr(DynamicSubscriptExpr *e,
                                                SGFContext c) {
  // Emit the base operand.
  ManagedValue managedBase = emitRValueAsSingleValue(e->getBase());

  SILValue base = managedBase.getValue();

  // Emit the index.
  RValue index = emitRValue(e->getIndex());

  // Create the continuation block.
  SILBasicBlock *contBB = createBasicBlock();

  // Create the no-member block.
  SILBasicBlock *noMemberBB = createBasicBlock();

  // Create the has-member block.
  SILBasicBlock *hasMemberBB = createBasicBlock();

  const TypeLowering &optTL = getTypeLowering(e->getType());
  auto loweredOptTy = optTL.getLoweredType();
  SILValue optTemp = emitTemporaryAllocation(e, loweredOptTy);

  // Create the branch.
  auto subscriptDecl = cast<SubscriptDecl>(e->getMember().getDecl());
  auto member = SILDeclRef(subscriptDecl->getGetter(),
                           SILDeclRef::Kind::Func)
    .asForeign();
  B.createDynamicMethodBranch(e, base, member, hasMemberBB, noMemberBB);

  // Create the has-member branch.
  {
    B.emitBlock(hasMemberBB);

    FullExpr hasMemberScope(Cleanups, CleanupLocation(e));

    // The argument to the has-member block is the uncurried method.
    // Build the substituted getter type from the AST nodes.
    auto valueTy = e->getType()->getCanonicalType().getOptionalObjectType();
    auto indexTy = e->getIndex()->getType()->getCanonicalType();
    auto methodTy = CanFunctionType::get(indexTy,
                                         valueTy);
    auto foreignMethodTy =
      getPartialApplyOfDynamicMethodFormalType(SGM, member, e->getMember());
    
    auto functionTy = CanFunctionType::get(base->getType().getASTType(),
                                           methodTy);
    auto loweredMethodTy = getDynamicMethodLoweredType(SGM.M, member,
                                                       functionTy);
    SILValue memberArg = hasMemberBB->createPHIArgument(
        loweredMethodTy, ValueOwnershipKind::Owned);
    // Emit the application of 'self'.
    Scope applyScope(Cleanups, CleanupLocation(e));
    ManagedValue result = emitDynamicPartialApply(*this, e, memberArg, base,
                                                  foreignMethodTy, methodTy);
    // Emit the index.
    llvm::SmallVector<ManagedValue, 2> indexArgs;
    std::move(index).getAll(indexArgs);
    
    auto resultRV = emitMonomorphicApply(e, result, indexArgs,
                                         foreignMethodTy.getResult(), valueTy,
                                         ApplyOptions::DoesNotThrow,
                                         None, None);

    // Package up the result in an optional.
    emitInjectOptionalValueInto(e, {e, std::move(resultRV)}, optTemp, optTL);

    applyScope.pop();
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
  auto optResult = optTemp;
  if (optTL.isLoadable())
    optResult = optTL.emitLoad(B, e, optResult, LoadOwnershipQualifier::Take);
  return RValue(*this, e, emitManagedRValueWithCleanup(optResult, optTL));
}

ManagedValue ArgumentScope::popPreservingValue(ManagedValue mv) {
  formalEvalScope.pop();
  return normalScope.popPreservingValue(mv);
}

RValue ArgumentScope::popPreservingValue(RValue &&rv) {
  formalEvalScope.pop();
  return normalScope.popPreservingValue(std::move(rv));
}
