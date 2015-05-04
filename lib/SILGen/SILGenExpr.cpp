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
#include "Condition.h"
#include "Scope.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Unicode.h"
#include "swift/Basic/type_traits.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SIL/DynamicCasts.h"
#include "ExitableFullExpr.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "ArgumentSource.h"
#include "SILGenDynamicCast.h"
#include "Varargs.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/MemoryBuffer.h"

#include "swift/AST/DiagnosticsSIL.h"

using namespace swift;
using namespace Lowering;

SILGenFunction::OpaqueValueRAII::~OpaqueValueRAII() {
  // Destroy the value, unless it was both uniquely referenced and consumed.
  auto entry = Self.OpaqueValues.find(OpaqueValue);
  if (Destroy && 
      (!entry->second.isConsumable || !entry->second.hasBeenConsumed)) {
    const SILValue &value = entry->second.value;
    auto &lowering = Self.getTypeLowering(value.getType().getSwiftRValueType());
    lowering.emitDestroyRValue(Self.B, OpaqueValue, value);
  }

  // Remove the opaque value.
  Self.OpaqueValues.erase(entry);
}

ManagedValue SILGenFunction::emitManagedRetain(SILLocation loc,
                                               SILValue v) {
  auto &lowering = getTypeLowering(v.getType().getSwiftRValueType());
  return emitManagedRetain(loc, v, lowering);
}

ManagedValue SILGenFunction::emitManagedRetain(SILLocation loc,
                                               SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType() == v.getType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(v);
  assert(!lowering.isAddressOnly() && "cannot retain an unloadable type");

  lowering.emitRetainValue(B, loc, v);
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v) {
  auto &lowering = getTypeLowering(v.getType());
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType() == v.getType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(v);
  
  return ManagedValue(v, enterDestroyCleanup(v));
}

ManagedValue SILGenFunction::emitManagedBufferWithCleanup(SILValue v) {
  auto &lowering = getTypeLowering(v.getType());
  return emitManagedBufferWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedBufferWithCleanup(SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v.getType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(v);

  return ManagedValue(v, enterDestroyCleanup(v));
}

void SILGenFunction::emitExprInto(Expr *E, Initialization *I) {
  // Handle the special case of copying an lvalue.
  if (auto load = dyn_cast<LoadExpr>(E)) {
    WritebackScope writeback(*this);
    auto lv = emitLValue(load->getSubExpr(), AccessKind::Read);
    emitCopyLValueInto(E, std::move(lv), I);
    return;
  }

  RValue result = emitRValue(E, SGFContext(I));
  if (result)
    std::move(result).forwardInto(*this, I, E);
}

namespace {
  class RValueEmitter
      : public Lowering::ExprVisitor<RValueEmitter, RValue, SGFContext>
  {
    SILGenFunction &SGF;
    typedef Lowering::ExprVisitor<RValueEmitter,RValue,SGFContext> super;
  public:
    RValueEmitter(SILGenFunction &SGF) : SGF(SGF) {}

    using super::visit;
    RValue visit(Expr *E) {
      assert(!E->getType()->is<LValueType>() &&
             !E->getType()->is<InOutType>() &&
             "RValueEmitter shouldn't be called on lvalues");
      return visit(E, SGFContext());
    }

    // These always produce lvalues.
    RValue visitInOutExpr(InOutExpr *E, SGFContext C) {
      LValue lv = SGF.emitLValue(E->getSubExpr(), AccessKind::ReadWrite);
      return RValue(SGF, E, SGF.emitAddressOfLValue(E->getSubExpr(),
                                                    std::move(lv),
                                                    AccessKind::ReadWrite));
    }
    
    RValue visitApplyExpr(ApplyExpr *E, SGFContext C);
    
    RValue visitDiscardAssignmentExpr(DiscardAssignmentExpr *E, SGFContext C) {
      llvm_unreachable("cannot appear in rvalue");
    }
    RValue visitDeclRefExpr(DeclRefExpr *E, SGFContext C);
    RValue visitTypeExpr(TypeExpr *E, SGFContext C);
    RValue visitSuperRefExpr(SuperRefExpr *E, SGFContext C);
    RValue visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E,
                                            SGFContext C);

    RValue visitThrowExpr(ThrowExpr *E, SGFContext C);
    RValue visitForceTryExpr(ForceTryExpr *E, SGFContext C);

    RValue visitNilLiteralExpr(NilLiteralExpr *E, SGFContext C);
    RValue visitIntegerLiteralExpr(IntegerLiteralExpr *E, SGFContext C);
    RValue visitFloatLiteralExpr(FloatLiteralExpr *E, SGFContext C);
    RValue visitBooleanLiteralExpr(BooleanLiteralExpr *E, SGFContext C);
    RValue visitCharacterLiteralExpr(CharacterLiteralExpr *E, SGFContext C);
        
    RValue emitStringLiteral(Expr *E, StringRef Str, SGFContext C,
                             StringLiteralExpr::Encoding encoding);
        
    RValue visitStringLiteralExpr(StringLiteralExpr *E, SGFContext C);
    RValue visitLoadExpr(LoadExpr *E, SGFContext C);
    RValue visitDerivedToBaseExpr(DerivedToBaseExpr *E, SGFContext C);
    RValue visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                       SGFContext C);
    RValue visitCollectionUpcastConversionExpr(
             CollectionUpcastConversionExpr *E,
             SGFContext C);
    RValue visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E, SGFContext C);
    RValue visitFunctionConversionExpr(FunctionConversionExpr *E,
                                       SGFContext C);
    RValue visitCovariantFunctionConversionExpr(
             CovariantFunctionConversionExpr *E,
             SGFContext C);
    RValue visitCovariantReturnConversionExpr(
             CovariantReturnConversionExpr *E,
             SGFContext C);
    RValue visitErasureExpr(ErasureExpr *E, SGFContext C);
    RValue visitMetatypeErasureExpr(MetatypeErasureExpr *E, SGFContext C);
    RValue visitForcedCheckedCastExpr(ForcedCheckedCastExpr *E,
                                      SGFContext C);
    RValue visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *E,
                                           SGFContext C);
    RValue visitIsExpr(IsExpr *E, SGFContext C);
    RValue visitCoerceExpr(CoerceExpr *E, SGFContext C);
    RValue visitTupleExpr(TupleExpr *E, SGFContext C);
    RValue visitScalarToTupleExpr(ScalarToTupleExpr *E, SGFContext C);
    RValue visitMemberRefExpr(MemberRefExpr *E, SGFContext C);
    RValue visitDynamicMemberRefExpr(DynamicMemberRefExpr *E, SGFContext C);
    RValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E,
                                         SGFContext C);
    RValue visitModuleExpr(ModuleExpr *E, SGFContext C);
    RValue visitTupleElementExpr(TupleElementExpr *E, SGFContext C);
    RValue visitSubscriptExpr(SubscriptExpr *E, SGFContext C);
    RValue visitDynamicSubscriptExpr(DynamicSubscriptExpr *E,
                                     SGFContext C);
    RValue visitTupleShuffleExpr(TupleShuffleExpr *E, SGFContext C);
    RValue visitDynamicTypeExpr(DynamicTypeExpr *E, SGFContext C);
    RValue visitCaptureListExpr(CaptureListExpr *E, SGFContext C);
    RValue visitAbstractClosureExpr(AbstractClosureExpr *E, SGFContext C);
    RValue visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                              SGFContext C);
    RValue visitObjectLiteralExpr(ObjectLiteralExpr *E, SGFContext C);
    RValue visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E,
                                           SGFContext C);
    RValue visitCollectionExpr(CollectionExpr *E, SGFContext C);
    RValue visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E,
                                            SGFContext C);
    RValue visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *E, SGFContext C);
    RValue visitLValueToPointerExpr(LValueToPointerExpr *E, SGFContext C);
    RValue visitClassMetatypeToObjectExpr(ClassMetatypeToObjectExpr *E,
                                          SGFContext C);
    RValue visitExistentialMetatypeToObjectExpr(ExistentialMetatypeToObjectExpr *E,
                                                SGFContext C);
    RValue visitProtocolMetatypeToObjectExpr(ProtocolMetatypeToObjectExpr *E,
                                             SGFContext C);
    RValue visitIfExpr(IfExpr *E, SGFContext C);
    
    RValue visitDefaultValueExpr(DefaultValueExpr *E, SGFContext C);
    RValue visitAssignExpr(AssignExpr *E, SGFContext C);

    RValue visitBindOptionalExpr(BindOptionalExpr *E, SGFContext C);
    RValue visitOptionalEvaluationExpr(OptionalEvaluationExpr *E,
                                       SGFContext C);
    RValue visitForceValueExpr(ForceValueExpr *E, SGFContext C);
    RValue emitForceValue(SILLocation loc, Expr *E,
                          unsigned numOptionalEvaluations,
                          SGFContext C);
    RValue visitOpenExistentialExpr(OpenExistentialExpr *E, SGFContext C);

    RValue visitOpaqueValueExpr(OpaqueValueExpr *E, SGFContext C);

    RValue visitInOutToPointerExpr(InOutToPointerExpr *E, SGFContext C);
    RValue visitArrayToPointerExpr(ArrayToPointerExpr *E, SGFContext C);
    RValue visitStringToPointerExpr(StringToPointerExpr *E, SGFContext C);
    RValue visitPointerToPointerExpr(PointerToPointerExpr *E, SGFContext C);
    RValue visitForeignObjectConversionExpr(ForeignObjectConversionExpr *E,
                                            SGFContext C);
    
    RValue visitAvailabilityQueryExpr(AvailabilityQueryExpr *E, SGFContext C);
    RValue visitUnavailableToOptionalExpr(UnavailableToOptionalExpr *E,
                                          SGFContext C);
  };
}

RValue RValueEmitter::visitApplyExpr(ApplyExpr *E, SGFContext C) {
  return SGF.emitApplyExpr(E, C);
}

SILValue SILGenFunction::emitEmptyTuple(SILLocation loc) {
  return B.createTuple(loc,
               getLoweredType(TupleType::getEmpty(SGM.M.getASTContext())), {});
}

/// Emit the specified declaration as an address if possible,
/// otherwise return null.
ManagedValue SILGenFunction::emitLValueForDecl(SILLocation loc, VarDecl *var,
                                               CanType formalRValueType,
                                               AccessKind accessKind,
                                               AccessSemantics semantics) {
  // For local decls, use the address we allocated or the value if we have it.
  auto It = VarLocs.find(var);
  if (It != VarLocs.end()) {
    // If this has an address, return it.  By-value let's have no address.
    SILValue ptr = It->second.value;
    if (ptr.getType().isAddress())
      return ManagedValue::forLValue(ptr);
    
    // Otherwise, it is an RValue let.
    return ManagedValue();
  }

  switch (var->getAccessStrategy(semantics, accessKind)) {
  case AccessStrategy::Storage:
    // The only kind of stored variable that should make it to here is
    // a global variable.  Just invoke its accessor function to get its
    // address.
    return emitGlobalVariableRef(loc, var);

  case AccessStrategy::Addressor: {
    LValue lvalue =
      emitLValueForAddressedNonMemberVarDecl(loc, var, formalRValueType,
                                             accessKind, semantics);
    return emitAddressOfLValue(loc, std::move(lvalue), accessKind);
  }

  case AccessStrategy::DirectToAccessor:
  case AccessStrategy::DispatchToAccessor:
    return ManagedValue();
  }
  llvm_unreachable("bad access strategy");
}


ManagedValue SILGenFunction::
emitRValueForDecl(SILLocation loc, ConcreteDeclRef declRef, Type ncRefType,
                  AccessSemantics semantics, SGFContext C) {
  assert(!ncRefType->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");
  
  // Any writebacks for this access are tightly scoped.
  WritebackScope scope(*this);
  
  // If this is an decl that we have an lvalue for, produce and return it.
  ValueDecl *decl = declRef.getDecl();
  
  if (!ncRefType) ncRefType = decl->getType();
  CanType refType = ncRefType->getCanonicalType();
  
  // If this is a reference to a type, produce a metatype.
  if (isa<TypeDecl>(decl)) {
    assert(decl->getType()->is<MetatypeType>() &&
           "type declref does not have metatype type?!");
    return ManagedValue::forUnmanaged(B.createMetatype(loc,
                                                      getLoweredType(refType)));
  }
  
  // If this is a reference to a var, produce an address or value.
  if (auto *var = dyn_cast<VarDecl>(decl)) {
    assert(!declRef.isSpecialized() &&
           "Cannot handle specialized variable references");

    // If this VarDecl is represented as an address, emit it as an lvalue, then
    // perform a load to get the rvalue.
    if (auto Result = emitLValueForDecl(loc, var, refType,
                                        AccessKind::Read, semantics)) {
      IsTake_t takes;
      // 'self' may need to be taken during an 'init' delegation.
      if (var->getName() == getASTContext().Id_self) {
        switch (SelfInitDelegationState) {
        case NormalSelf:
          // Don't consume self.
          takes = IsNotTake;
          break;
        
        case WillConsumeSelf:
          // Consume self, and remember we did so.
          takes = IsTake;
          SelfInitDelegationState = DidConsumeSelf;
          break;
            
        case DidConsumeSelf:
          // We already consumed self. This shouldn't happen in valid code, because
          // 'super.init(self)' is a DI violation, but we haven't run DI yet,
          // so we can't actually crash here. At least emit
          // somewhat balanced code.
          takes = IsNotTake;
          break;
        }
      } else {
        takes = IsNotTake;
      }

      // We should only end up in this path for local and global variables,
      // i.e. ones whose lifetime is assured for the duration of the evaluation.
      // Therefore, if the variable is a constant, the value is guaranteed
      // valid as well.
      return emitLoad(loc, Result.getLValueAddress(), getTypeLowering(refType),
                      C, takes, /*guaranteed*/ var->isLet());
    }

    // For local decls, use the address we allocated or the value if we have it.
    auto It = VarLocs.find(decl);
    if (It != VarLocs.end()) {
      // Mutable lvalue and address-only 'let's are LValues.
      assert(!It->second.value.getType().isAddress() &&
             "LValue cases should be handled above");

      SILValue Scalar = It->second.value;

      // For weak and unowned types, convert the reference to the right
      // pointer.
      if (Scalar.getType().is<ReferenceStorageType>()) {
        Scalar = emitConversionToSemanticRValue(loc, Scalar,
                                                getTypeLowering(refType));
        // emitConversionToSemanticRValue always produces a +1 strong result.
        return emitManagedRValueWithCleanup(Scalar);
      }

      auto Result = ManagedValue::forUnmanaged(Scalar);

      // If the client can't handle a +0 result, retain it to get a +1.
      // This is a 'let', so we can make guarantees.
      return C.isGuaranteedPlusZeroOk()
        ? Result : Result.copyUnmanaged(*this, loc);
    }

    assert(var->hasAccessorFunctions() && "Unknown rvalue case");

    bool isDirectAccessorUse = (semantics == AccessSemantics::DirectToAccessor);
    SILDeclRef getter = getGetterDeclRef(var, isDirectAccessorUse);

    ArgumentSource selfSource;
    
    // Global properties have no base or subscript. Static properties
    // use the metatype as their base.
    // FIXME: This has to be dynamically looked up for classes, and
    // dynamically instantiated for generics.
    if (var->isStatic()) {
      auto baseTy = cast<NominalTypeDecl>(var->getDeclContext())
        ->getDeclaredInterfaceType();
      assert(!baseTy->is<BoundGenericType>() &&
             "generic static stored properties not implemented");
      assert((baseTy->getStructOrBoundGenericStruct() ||
              baseTy->getEnumOrBoundGenericEnum()) &&
             "static stored properties for classes/protocols not implemented");
      auto baseMeta = MetatypeType::get(baseTy)->getCanonicalType();

      auto metatype = B.createMetatype(loc,
                                       getLoweredLoadableType(baseMeta));
      auto metatypeMV = ManagedValue::forUnmanaged(metatype);
      auto metatypeRV = RValue(*this, loc, baseMeta, metatypeMV);
      selfSource = ArgumentSource(loc, std::move(metatypeRV));
    }
    return emitGetAccessor(loc, getter,
                           ArrayRef<Substitution>(), std::move(selfSource),
                           /*isSuper=*/false, isDirectAccessorUse,
                           RValue(), C);
  }
  
  // If the referenced decl isn't a VarDecl, it should be a constant of some
  // sort.

  // If the referenced decl is a local func with context, then the SILDeclRef
  // uncurry level is one deeper (for the context vars).
  bool hasLocalCaptures = false;
  unsigned uncurryLevel = 0;
  if (auto *fd = dyn_cast<FuncDecl>(decl)) {
    hasLocalCaptures = fd->getCaptureInfo().hasLocalCaptures();
    if (hasLocalCaptures)
      ++uncurryLevel;
  }

  auto silDeclRef = SILDeclRef(decl, ResilienceExpansion::Minimal, uncurryLevel);
  auto constantInfo = getConstantInfo(silDeclRef);

  ManagedValue result = emitFunctionRef(loc, silDeclRef, constantInfo);

  // Get the lowered AST types:
  //  - the original type
  auto origLoweredFormalType = AbstractionPattern(constantInfo.LoweredType);
  if (hasLocalCaptures) {
    auto formalTypeWithoutCaptures =
      cast<AnyFunctionType>(constantInfo.FormalType.getResult());
    origLoweredFormalType =
      AbstractionPattern(
              SGM.Types.getLoweredASTFunctionType(formalTypeWithoutCaptures,0,
                                                  silDeclRef));
  }

  // - the substituted type
  auto substFormalType = cast<AnyFunctionType>(refType);
  auto substLoweredFormalType =
    SGM.Types.getLoweredASTFunctionType(substFormalType, 0, silDeclRef);

  // If the declaration reference is specialized, create the partial
  // application.
  if (declRef.isSpecialized()) {
    // Substitute the function type.
    auto origFnType = result.getType().castTo<SILFunctionType>();
    auto substFnType = origFnType->substGenericArgs(
                                                    SGM.M, SGM.SwiftModule,
                                                    declRef.getSubstitutions());
    auto closureType = adjustFunctionType(substFnType,
                                        SILFunctionType::Representation::Thick);

    SILValue spec = B.createPartialApply(loc, result.forward(*this),
                                SILType::getPrimitiveObjectType(substFnType),
                                         declRef.getSubstitutions(),
                                         { },
                                SILType::getPrimitiveObjectType(closureType));
    result = emitManagedRValueWithCleanup(spec);
  }

  // Generalize if necessary.
  return emitGeneralizedFunctionValue(loc, result, origLoweredFormalType,
                                      substLoweredFormalType);
}

static AbstractionPattern
getOrigFormalRValueType(SILGenFunction &gen, VarDecl *field) {
  auto origType = gen.SGM.Types.getAbstractionPattern(field);
  return origType.getReferenceStorageReferentType();
}

static SILDeclRef getRValueAccessorDeclRef(SILGenFunction &SGF,
                                           AbstractStorageDecl *storage,
                                           AccessStrategy strategy) {
  switch (strategy) {
  case AccessStrategy::Storage:
    llvm_unreachable("should already have been filtered out!");

  case AccessStrategy::DirectToAccessor:
    return SGF.getGetterDeclRef(storage, true);

  case AccessStrategy::DispatchToAccessor:
    return SGF.getGetterDeclRef(storage, false);

  case AccessStrategy::Addressor:
    return SGF.getAddressorDeclRef(storage, AccessKind::Read,
                                   /*always direct for now*/ true);
  }
  llvm_unreachable("should already have been filtered out!");
}

static ManagedValue
emitRValueWithAccessor(SILGenFunction &SGF, SILLocation loc,
                       AbstractStorageDecl *storage,
                       ArrayRef<Substitution> substitutions,
                       ArgumentSource &&baseRV, RValue &&subscriptRV,
                       bool isSuper, AccessStrategy strategy,
                       SILDeclRef accessor,
                       AbstractionPattern origFormalType,
                       CanType substFormalType,
                       SGFContext C) {
  bool isDirectUse = (strategy == AccessStrategy::DirectToAccessor);

  switch (strategy) {
  case AccessStrategy::Storage:
    llvm_unreachable("should already have been filtered out!");

  // The easy path here is if we don't need to use an addressor.
  case AccessStrategy::DirectToAccessor:
  case AccessStrategy::DispatchToAccessor: {
    return SGF.emitGetAccessor(loc, accessor, substitutions,
                               std::move(baseRV), isSuper, isDirectUse,
                               std::move(subscriptRV), C);
  }

  case AccessStrategy::Addressor:
    break;
  }

  auto &storageTL = SGF.getTypeLowering(origFormalType, substFormalType);
  SILType storageType = storageTL.getLoweredType().getAddressType();

  auto addressorResult =
    SGF.emitAddressorAccessor(loc, accessor, substitutions,
                              std::move(baseRV), isSuper, isDirectUse,
                              std::move(subscriptRV), storageType);

  SILValue address = addressorResult.first.getLValueAddress();

  SILType loweredSubstType =
    SGF.getLoweredType(substFormalType).getAddressType();
  bool hasAbstraction = (loweredSubstType != storageType);

  ManagedValue result =
    SGF.emitLoad(loc, address, storageTL,
                 (hasAbstraction ? SGFContext() : C), IsNotTake);
  if (hasAbstraction) {
    result = SGF.emitOrigToSubstValue(loc, result, origFormalType,
                                      substFormalType, C);
  }

  switch (cast<FuncDecl>(accessor.getDecl())->getAddressorKind()) {
  case AddressorKind::NotAddressor: llvm_unreachable("inconsistent");
  case AddressorKind::Unsafe:
    // Nothing to do.
    break;
  case AddressorKind::Owning:
  case AddressorKind::NativeOwning:
    // Emit the release immediately.
    SGF.B.emitStrongReleaseAndFold(loc, addressorResult.second.forward(SGF));
    break;
  case AddressorKind::NativePinning:
    // Emit the unpin immediately.
    SGF.B.createStrongUnpin(loc, addressorResult.second.forward(SGF));
    break;
  }
  
  return result;
}

/// Produce a singular RValue for a load from the specified property.  This
/// is designed to work with RValue ManagedValue bases that are either +0 or +1.
ManagedValue SILGenFunction::
emitRValueForPropertyLoad(SILLocation loc, ManagedValue base,
                          bool isSuper, VarDecl *field,
                          ArrayRef<Substitution> substitutions,
                          AccessSemantics semantics,
                          Type propTy, SGFContext C) {
  AccessStrategy strategy =
    field->getAccessStrategy(semantics, AccessKind::Read);

  // If we should call an accessor of some kind, do so.
  if (strategy != AccessStrategy::Storage) {
    auto accessor = getRValueAccessorDeclRef(*this, field, strategy);
    ArgumentSource baseRV = prepareAccessorBaseArg(loc, base, accessor);

    AbstractionPattern origFormalType =
      getOrigFormalRValueType(*this, field);
    auto substFormalType = propTy->getCanonicalType();

    return emitRValueWithAccessor(*this, loc, field, substitutions,
                                  std::move(baseRV), RValue(),
                                  isSuper, strategy, accessor,
                                  origFormalType, substFormalType, C);
  }

  assert(field->hasStorage() &&
         "Cannot directly access value without storage");

  // For static variables, emit a reference to the global variable backing
  // them.
  // FIXME: This has to be dynamically looked up for classes, and
  // dynamically instantiated for generics.
  if (field->isStatic()) {
    auto baseMeta = base.getType().castTo<MetatypeType>().getInstanceType();
    (void)baseMeta;
    assert(!baseMeta->is<BoundGenericType>() &&
           "generic static stored properties not implemented");
    if (field->getDeclContext()->isClassOrClassExtensionContext() &&
        field->hasStorage())
      // FIXME: don't need to check hasStorage, already done above
      assert(field->isFinal() && "non-final class stored properties not implemented");

    return emitRValueForDecl(loc, field, propTy, semantics, C);
  }

  // If the base is a reference type, just handle this as loading the lvalue.
  if (base.getType().getSwiftRValueType()->hasReferenceSemantics()) {
    LValue LV = emitPropertyLValue(loc, base, field, AccessKind::Read,
                                   AccessSemantics::DirectToStorage);
    return emitLoadOfLValue(loc, std::move(LV), C);
  }

  // rvalue MemberRefExprs are produced in two cases: when accessing a 'let'
  // decl member, and when the base is a (non-lvalue) struct.
  assert(base.getType().getSwiftRValueType()->getAnyNominal() &&
         "The base of an rvalue MemberRefExpr should be an rvalue value");

  // If the accessed field is stored, emit a StructExtract on the base.

  auto substFormalType = propTy->getCanonicalType();
  auto &lowering = getTypeLowering(substFormalType);

  // Check for an abstraction difference.
  AbstractionPattern origFormalType =
    getOrigFormalRValueType(*this, field);
  bool hasAbstractionChange = false;
  if (!origFormalType.isExactType(substFormalType)) {
    auto &abstractedTL = getTypeLowering(origFormalType, substFormalType);
    hasAbstractionChange =
      (abstractedTL.getLoweredType() != lowering.getLoweredType());
  }

  ManagedValue Result;
  if (!base.getType().isAddress()) {
    // For non-address-only structs, we emit a struct_extract sequence.
    SILValue Scalar = B.createStructExtract(loc, base.getValue(), field);
    Result = ManagedValue::forUnmanaged(Scalar);

    if (Result.getType().is<ReferenceStorageType>()) {
      // For weak and unowned types, convert the reference to the right
      // pointer, producing a +1.
      Scalar = emitConversionToSemanticRValue(loc, Scalar, lowering);
      Result = emitManagedRValueWithCleanup(Scalar, lowering);

    } else if (hasAbstractionChange || !C.isImmediatePlusZeroOk()) {
      // If we have an abstraction change or if we have to produce a result at
      // +1, then emit a RetainValue.  Without further analysis, we
      // can't prove that the base will stay alive, so we can only
      // return +0 to an immediate consumer.
      Result = Result.copyUnmanaged(*this, loc);
    }
  } else {
    // For address-only sequences, the base is in memory.  Emit a
    // struct_element_addr to get to the field, and then load the element as an
    // rvalue.
    SILValue ElementPtr =
      B.createStructElementAddr(loc, base.getValue(), field);

    Result = emitLoad(loc, ElementPtr, lowering,
                      hasAbstractionChange ? SGFContext() : C, IsNotTake);
  }

  // If we're accessing this member with an abstraction change, perform that
  // now.
  if (hasAbstractionChange)
    Result = emitOrigToSubstValue(loc, Result, origFormalType,
                                  substFormalType, C);
  return Result;
}


RValue RValueEmitter::visitDeclRefExpr(DeclRefExpr *E, SGFContext C) {
  auto Val = SGF.emitRValueForDecl(E, E->getDeclRef(), E->getType(),
                                   E->getAccessSemantics(), C);
  return RValue(SGF, E, Val);
}

RValue RValueEmitter::visitTypeExpr(TypeExpr *E, SGFContext C) {
  assert(E->getType()->is<AnyMetatypeType>() &&
         "TypeExpr must have metatype type");
  auto Val = SGF.B.createMetatype(E, SGF.getLoweredType(E->getType()));
  return RValue(SGF, E, ManagedValue::forUnmanaged(Val));
}


RValue RValueEmitter::visitSuperRefExpr(SuperRefExpr *E, SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");
  auto Self = SGF.emitRValueForDecl(E, E->getSelf(),
                                    E->getSelf()->getType(),
                                    AccessSemantics::Ordinary);

  // Perform an upcast to convert self to the indicated super type.
  auto Result = SGF.B.createUpcast(E, Self.getValue(),
                                   SGF.getLoweredType(E->getType()));

  return RValue(SGF, E, ManagedValue(Result, Self.getCleanup()));

}

RValue RValueEmitter::visitOtherConstructorDeclRefExpr(
                                OtherConstructorDeclRefExpr *E, SGFContext C) {
  // This should always be a child of an ApplyExpr and so will be emitted by
  // SILGenApply.
  llvm_unreachable("unapplied reference to constructor?!");
}

RValue RValueEmitter::visitNilLiteralExpr(NilLiteralExpr *E, SGFContext C) {
  llvm_unreachable("NilLiteralExpr not lowered?");
}

RValue RValueEmitter::visitIntegerLiteralExpr(IntegerLiteralExpr *E,
                                              SGFContext C) {
  return RValue(SGF, E,
                ManagedValue::forUnmanaged(SGF.B.createIntegerLiteral(E)));
}
RValue RValueEmitter::visitFloatLiteralExpr(FloatLiteralExpr *E,
                                            SGFContext C) {
  return RValue(SGF, E,
                ManagedValue::forUnmanaged(SGF.B.createFloatLiteral(E)));
}

RValue RValueEmitter::visitBooleanLiteralExpr(BooleanLiteralExpr *E, 
                                              SGFContext C) {
  auto i1Ty = SILType::getBuiltinIntegerType(1, SGF.getASTContext());
  SILValue boolValue = SGF.B.createIntegerLiteral(E, i1Ty, E->getValue());
  return RValue(SGF, E, ManagedValue::forUnmanaged(boolValue));
}

RValue RValueEmitter::visitCharacterLiteralExpr(CharacterLiteralExpr *E,
                                                SGFContext C) {
  return RValue(SGF, E,
                ManagedValue::forUnmanaged(SGF.B.createIntegerLiteral(E)));
}

RValue RValueEmitter::emitStringLiteral(Expr *E, StringRef Str,
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

  StringLiteralInst::Encoding instEncoding;
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
  case StringLiteralExpr::OneUnicodeScalar: {
    SILType Ty = SGF.getLoweredLoadableType(E->getType());
    SILValue UnicodeScalarValue =
        SGF.B.createIntegerLiteral(E, Ty,
                                   unicode::extractFirstUnicodeScalar(Str));
    return RValue(SGF, E, ManagedValue::forUnmanaged(UnicodeScalarValue));
  }
  }

  // The string literal provides the data.
  StringLiteralInst *string = SGF.B.createStringLiteral(E, Str, instEncoding);
  CanType ty = E->getType()->getCanonicalType();

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

  ArrayRef<ManagedValue> Elts;
  switch (instEncoding) {
  case StringLiteralInst::Encoding::UTF16:
    Elts = llvm::makeArrayRef(EltsArray).slice(0, 2);
    break;

  case StringLiteralInst::Encoding::UTF8:
    Elts = EltsArray;
    break;
  }

  return RValue(Elts, ty);
}


RValue RValueEmitter::visitStringLiteralExpr(StringLiteralExpr *E,
                                             SGFContext C) {
  return emitStringLiteral(E, E->getValue(), C, E->getEncoding());
}

RValue RValueEmitter::visitLoadExpr(LoadExpr *E, SGFContext C) {
  LValue lv = SGF.emitLValue(E->getSubExpr(), AccessKind::Read);
  return RValue(SGF, E, SGF.emitLoadOfLValue(E, std::move(lv), C));
}

SILValue SILGenFunction::emitTemporaryAllocation(SILLocation loc,
                                                 SILType ty) {
  ty = ty.getObjectType();
  auto alloc = B.createAllocStack(loc, ty);
  enterDeallocStackCleanup(alloc->getContainerResult());
  return alloc->getAddressResult();
}

// Return an initialization address we can emit directly into.
static SILValue getAddressForInPlaceInitialization(const Initialization *I) {
  return I ? I->getAddressForInPlaceInitialization() : SILValue();
}

SILValue SILGenFunction::
getBufferForExprResult(SILLocation loc, SILType ty, SGFContext C) {
  // If you change this, change manageBufferForExprResult below as well.

  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (SILValue address = getAddressForInPlaceInitialization(C.getEmitInto()))
    return address;
  
  // If we couldn't emit into the Initialization, emit into a temporary
  // allocation.
  return emitTemporaryAllocation(loc, ty.getObjectType());
}

ManagedValue SILGenFunction::
manageBufferForExprResult(SILValue buffer, const TypeLowering &bufferTL,
                          SGFContext C) {
  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (getAddressForInPlaceInitialization(C.getEmitInto())) {
    C.getEmitInto()->finishInitialization(*this);
    return ManagedValue::forInContext();
  }
  
  // Add a cleanup for the temporary we allocated.
  if (bufferTL.isTrivial())
    return ManagedValue::forUnmanaged(buffer);

  return ManagedValue(buffer, enterDestroyCleanup(buffer));
}

RValue RValueEmitter::visitForceTryExpr(ForceTryExpr *E, SGFContext C) {
  SILGenFunction::ForceTryScope scope(SGF, E);
  return visit(E->getSubExpr(), C);
}

SILGenFunction::ForceTryScope::ForceTryScope(SILGenFunction &gen,
                                             SILLocation loc)
  : SGF(gen), TryBB(gen.createBasicBlock(FunctionSection::Postmatter)),
    Loc(loc), OldThrowDest(gen.ThrowDest) {
  gen.ThrowDest = JumpDest(TryBB, gen.Cleanups.getCleanupsDepth(),
                           CleanupLocation::get(loc));
}

SILGenFunction::ForceTryScope::~ForceTryScope() {
  // Restore the old throw dest.
  SGF.ThrowDest = OldThrowDest;

  // If there are no uses of the try block, just drop it.
  if (TryBB->pred_empty()) {
    SGF.eraseBasicBlock(TryBB);
    return;
  }

  // Otherwise, we need to emit it.
  SavedInsertionPoint scope(SGF, TryBB, FunctionSection::Postmatter);

  ASTContext &ctx = SGF.getASTContext();
  auto error = TryBB->createBBArg(SILType::getExceptionType(ctx));
  SGF.B.createBuiltin(Loc, ctx.getIdentifier("unexpectedError"),
                      SGF.SGM.Types.getEmptyTupleType(), {}, {error});
  SGF.B.createUnreachable(Loc);
}

RValue RValueEmitter::visitThrowExpr(ThrowExpr *E, SGFContext C) {
  // Create a continuation block to return the result in.
  // Expression emission isn't allowed to not have an insertion point.
  auto contBB = SGF.createBasicBlock();

  // If we have a valid throw destination, emit the exception and jump there.
  if (SGF.ThrowDest.isValid()) {
    ManagedValue exn = SGF.emitRValueAsSingleValue(E->getSubExpr());

    SGF.emitThrow(E, exn, /* emit a call to willThrow */ true);
    
  // Otherwise, diagnose.
  } else {
    SGF.SGM.diagnose(E, diag::unhandled_throw);

    // The diagnostic above is an error, so we don't care about leaks,
    // but we do need to not produce invalid SIL.
    SGF.B.createUnreachable(E);
  }

  // Emit an empty tuple in the result.
  SGF.B.emitBlock(contBB);
  return SGF.emitEmptyTupleRValue(E, C);
}

RValue RValueEmitter::visitDerivedToBaseExpr(DerivedToBaseExpr *E,
                                             SGFContext C) {
  ManagedValue original = SGF.emitRValueAsSingleValue(E->getSubExpr());

  // Derived-to-base casts in the AST might not be reflected as such
  // in the SIL type system, for example, a cast from DynamicSelf
  // directly to its own Self type.
  auto loweredResultTy = SGF.getLoweredType(E->getType());
  if (original.getType() == loweredResultTy)
    return RValue(SGF, E, original);

  SILValue converted = SGF.B.createUpcast(E, original.getValue(), 
                                          loweredResultTy);
  return RValue(SGF, E, ManagedValue(converted, original.getCleanup()));
}

RValue RValueEmitter::visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                                  SGFContext C) {
  SILValue metaBase =
    SGF.emitRValueAsSingleValue(E->getSubExpr()).getUnmanagedValue();

  // Metatype conversion casts in the AST might not be reflected as
  // such in the SIL type system, for example, a cast from DynamicSelf.Type
  // directly to its own Self.Type.
  auto loweredResultTy = SGF.getLoweredLoadableType(E->getType());
  if (metaBase.getType() == loweredResultTy)
    return RValue(SGF, E, ManagedValue::forUnmanaged(metaBase));

  auto upcast = SGF.B.createUpcast(E, metaBase, loweredResultTy);
  return RValue(SGF, E, ManagedValue::forUnmanaged(upcast));
}

RValue RValueEmitter::
visitCollectionUpcastConversionExpr(CollectionUpcastConversionExpr *E,
                                    SGFContext C) {
  
  SILLocation loc = RegularLocation(E);
  
  // Get the sub expression argument as a managed value
  auto mv = SGF.emitRValueAsSingleValue(E->getSubExpr());
  
  // Compute substitutions for the intrinsic call.
  auto fromCollection = cast<BoundGenericStructType>(
                          E->getSubExpr()->getType()->getCanonicalType());
  auto toCollection = cast<BoundGenericStructType>(
                        E->getType()->getCanonicalType());

  // Get the intrinsic function.
  auto &ctx = SGF.getASTContext();
  FuncDecl *fn = nullptr;
  if (fromCollection->getDecl() == ctx.getArrayDecl()) {
    fn = ctx.getArrayForceCast(nullptr);
  } else if (fromCollection->getDecl() == ctx.getDictionaryDecl()) {
    fn = E->bridgesToObjC() ? ctx.getDictionaryBridgeToObjectiveC(nullptr)
                            : ctx.getDictionaryUpCast(nullptr);
  } else if (fromCollection->getDecl() == ctx.getSetDecl()) {
    fn = E->bridgesToObjC() ? ctx.getSetBridgeToObjectiveC(nullptr)
                            : ctx.getSetUpCast(nullptr);
  } else {
    llvm_unreachable("unsupported collection upcast kind");
  }
  
  auto fnArcheTypes = fn->getGenericParams()->getPrimaryArchetypes();
  auto fromSubsts = fromCollection->getSubstitutions(SGF.SGM.SwiftModule,nullptr);
  auto toSubsts = toCollection->getSubstitutions(SGF.SGM.SwiftModule,nullptr);
  assert(fnArcheTypes.size() == fromSubsts.size() + toSubsts.size() &&
         "wrong number of generic collection parameters");
  
  // Form type parameter substitutions.
  int aIdx = 0;
  SmallVector<Substitution, 4> subs;
  for (auto sub: fromSubsts){
    subs.push_back(Substitution{fnArcheTypes[aIdx++], sub.getReplacement(),
                                sub.getConformances()});
  }
  for (auto sub: toSubsts){
    subs.push_back(Substitution{fnArcheTypes[aIdx++], sub.getReplacement(),
                                sub.getConformances()});
  }

  auto emitApply = SGF.emitApplyOfLibraryIntrinsic(loc, fn, subs, {mv}, C);
  
  return RValue(SGF, E, emitApply);
}

RValue RValueEmitter::visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E,
                                                SGFContext C) {
  ManagedValue archetype = SGF.emitRValueAsSingleValue(E->getSubExpr());
  // Replace the cleanup with a new one on the superclass value so we always use
  // concrete retain/release operations.
  SILValue base = SGF.B.createUpcast(E,
                                    archetype.forward(SGF),
                                    SGF.getLoweredLoadableType(E->getType()));
  return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(base));
}

static RValue emitCFunctionPointer(SILGenFunction &gen,
                                   FunctionConversionExpr *conversionExpr) {
  auto expr = conversionExpr->getSubExpr();
  
  // Look through base-ignored exprs to get to the function ref.
  auto semanticExpr = expr->getSemanticsProvidingExpr();
  while (auto ignoredBase = dyn_cast<DotSyntaxBaseIgnoredExpr>(semanticExpr)){
    gen.emitIgnoredExpr(ignoredBase->getLHS());
    semanticExpr = ignoredBase->getRHS()->getSemanticsProvidingExpr();
  }

  // Recover the decl reference.
  SILDeclRef::Loc loc;
  
  auto setLocFromConcreteDeclRef = [&](ConcreteDeclRef declRef) {
    // TODO: Handle generic instantiations, where we need to eagerly specialize
    // on the given generic parameters, and static methods, where we need to drop
    // in the metatype.
    assert(!declRef.getDecl()->getDeclContext()->isTypeContext()
           && "c pointers to static methods not implemented");
    assert(declRef.getSubstitutions().empty()
           && "c pointers to generics not implemented");
    loc = declRef.getDecl();
  };
  
  if (auto declRef = dyn_cast<DeclRefExpr>(semanticExpr)) {
    setLocFromConcreteDeclRef(declRef->getDeclRef());
  } else if (auto memberRef = dyn_cast<MemberRefExpr>(semanticExpr)) {
    setLocFromConcreteDeclRef(memberRef->getMember());
  } else if (auto closure = dyn_cast<AbstractClosureExpr>(semanticExpr)) {
    loc = closure;
    // Emit the closure body.
    gen.SGM.emitClosure(closure);
  } else {
    llvm_unreachable("c function pointer converted from a non-concrete decl ref");
  }

  // Produce a reference to the C-compatible entry point for the function.
  SILDeclRef cEntryPoint(loc, ResilienceExpansion::Minimal,
                         /*uncurryLevel*/ 0,
                         /*foreign*/ true);
  SILValue cRef = gen.emitGlobalFunctionRef(expr, cEntryPoint);
  return RValue(gen, conversionExpr, ManagedValue::forUnmanaged(cRef));
}

RValue RValueEmitter::visitFunctionConversionExpr(FunctionConversionExpr *e,
                                                  SGFContext C)
{
  // A "conversion" to a C function pointer is done by referencing the thunk
  // (or original C function) with the C calling convention.
  if (e->getType()->castTo<AnyFunctionType>()->getRepresentation()
        == AnyFunctionType::Representation::CFunctionPointer)
    return emitCFunctionPointer(SGF, e);

  ManagedValue original = SGF.emitRValueAsSingleValue(e->getSubExpr());
  
  // Break the conversion into two stages:
  // - changing the signature within the representation
  // - changing the representation
  
  // First, the signature:
  
  auto srcTy = e->getSubExpr()->getType()->castTo<FunctionType>();
  CanAnyFunctionType destRepTy
    = cast<FunctionType>(e->getType()->getCanonicalType());
  CanAnyFunctionType destTy = CanFunctionType::get(
       destRepTy.getInput(), destRepTy.getResult(),
       destRepTy->getExtInfo().withRepresentation(srcTy->getRepresentation()));
  
  // Retain the thinness of the original function type.
  auto origRep = original.getType().castTo<SILFunctionType>()->getRepresentation();
  AnyFunctionType::Representation astRep;
  switch (origRep) {
  case SILFunctionType::Representation::Thin:
  case SILFunctionType::Representation::Method:
  case SILFunctionType::Representation::WitnessMethod:
  case SILFunctionType::Representation::ObjCMethod:
    astRep = AnyFunctionType::Representation::Thin;
    break;
  case SILFunctionType::Representation::Thick:
    astRep = AnyFunctionType::Representation::Swift;
    break;
  case SILFunctionType::Representation::Block:
    astRep = AnyFunctionType::Representation::Block;
    break;
  case SILFunctionType::Representation::CFunctionPointer:
    astRep = AnyFunctionType::Representation::CFunctionPointer;
    break;
  }
  if (astRep != destTy->getRepresentation()) {
    destTy = adjustFunctionType(destTy, astRep);
  }

  SILType resultType = SGF.getLoweredType(destTy);
  auto resultFTy = resultType.castTo<SILFunctionType>();

  ManagedValue result;
  if (resultType == original.getType()) {
    // Don't make a conversion instruction if it's unnecessary.
    result = original;
  } else {
    SILValue converted =
      SGF.B.createConvertFunction(e, original.getValue(), resultType);
    result = ManagedValue(converted, original.getCleanup());
  }
  
  // Now, the representation:
  
  if (destRepTy != destTy) {
    // The only currently possible representation changes are block -> thick and
    // thick -> block.
    switch (destRepTy->getRepresentation()) {
    case AnyFunctionType::Representation::Block:
      switch (resultFTy->getRepresentation()) {
      case SILFunctionType::Representation::Thin: {
        // Make thick first.
        auto v = SGF.B.createThinToThickFunction(e, result.getValue(),
          SILType::getPrimitiveObjectType(
           adjustFunctionType(resultFTy, SILFunctionType::Representation::Thick)));
        result = ManagedValue(v, result.getCleanup());
        SWIFT_FALLTHROUGH;
      }
      case SILFunctionType::Representation::Thick:
        // Convert to a block.
        result = SGF.emitFuncToBlock(e, result,
                       SGF.getLoweredType(destRepTy).castTo<SILFunctionType>());
        break;
      case SILFunctionType::Representation::Block:
        llvm_unreachable("should not try block-to-block repr change");
      case SILFunctionType::Representation::CFunctionPointer:
        llvm_unreachable("c function pointer conversion not handled here");
      case SILFunctionType::Representation::Method:
      case SILFunctionType::Representation::ObjCMethod:
      case SILFunctionType::Representation::WitnessMethod:
        llvm_unreachable("should not do function conversion to method rep");
      }
      break;
    case AnyFunctionType::Representation::Swift: {
      // FIXME: We'll need to fix up no-throw-to-throw function conversions.
      assert(destRepTy->throws() ||
             (resultFTy->getRepresentation()
               == SILFunctionType::Representation::Block)
             && "only block-to-thick repr changes supported");
      result = SGF.emitBlockToFunc(e, result,
                       SGF.getLoweredType(destRepTy).castTo<SILFunctionType>());
      break;
    }
    case AnyFunctionType::Representation::Thin:
    case AnyFunctionType::Representation::CFunctionPointer:
      llvm_unreachable("not supported by sema");
    }
  }
  
  return RValue(SGF, e, result);
}

RValue RValueEmitter::visitCovariantFunctionConversionExpr(
                        CovariantFunctionConversionExpr *e,
                        SGFContext C) {
  ManagedValue original = SGF.emitRValueAsSingleValue(e->getSubExpr());
  CanAnyFunctionType destTy
    = cast<AnyFunctionType>(e->getType()->getCanonicalType());
  SILType resultType = SGF.getLoweredType(destTy);
  SILValue result = SGF.B.createConvertFunction(e, 
                                                original.forward(SGF),
                                                resultType);
  return RValue(SGF, e, SGF.emitManagedRValueWithCleanup(result));
}

static ManagedValue createUnsafeDowncast(SILGenFunction &gen,
                                         SILLocation loc,
                                         ManagedValue input,
                                         SILType resultTy) {
  SILValue result = gen.B.createUncheckedRefCast(loc,
                                                 input.forward(gen),
                                                 resultTy);
  return gen.emitManagedRValueWithCleanup(result);
}

RValue RValueEmitter::visitCovariantReturnConversionExpr(
                        CovariantReturnConversionExpr *e,
                        SGFContext C) {
  SILType resultType = SGF.getLoweredType(e->getType());

  ManagedValue original = SGF.emitRValueAsSingleValue(e->getSubExpr());
  ManagedValue result;
  if (resultType.getSwiftRValueType().getAnyOptionalObjectType()) {
    result = SGF.emitOptionalToOptional(e, original, resultType,
                                        createUnsafeDowncast);
  } else {
    result = createUnsafeDowncast(SGF, e, original, resultType);
  }

  return RValue(SGF, e, result);
}

static RValue emitClassBoundedErasure(SILGenFunction &gen, ErasureExpr *E) {
  ManagedValue sub = gen.emitRValueAsSingleValue(E->getSubExpr());
  SILType resultTy = gen.getLoweredLoadableType(E->getType());
  
  SILValue v;
  
  Type subType = E->getSubExpr()->getType();
  
  if (subType->isExistentialType()) {
    // If the source value is already of protocol type, open the value so we can
    // take it.
    auto archetype = ArchetypeType::getOpened(E->getSubExpr()->getType());
    subType = archetype;
    auto openedTy = gen.getLoweredLoadableType(subType);
    SILValue openedVal
      = gen.B.createOpenExistentialRef(E, sub.forward(gen), openedTy);
    gen.setArchetypeOpeningSite(archetype, openedVal);
    sub = gen.emitManagedRValueWithCleanup(openedVal);
  }
  
  // Create a new existential container value around the class
  // instance.
  v = gen.B.createInitExistentialRef(E, resultTy,
                               subType->getCanonicalType(),
                               sub.getValue(), E->getConformances());

  return RValue(gen, E, ManagedValue(v, sub.getCleanup()));
}

namespace {
  /// A cleanup that deinitializes an opaque existential container
  /// after its value is taken.
  class TakeFromExistentialCleanup: public Cleanup {
    SILValue existentialAddr;
  public:
    TakeFromExistentialCleanup(SILValue existentialAddr)
      : existentialAddr(existentialAddr) {}
    
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      gen.B.createDeinitExistentialAddr(l, existentialAddr);
    }
  };
}

static std::pair<ManagedValue, CanArchetypeType>
emitOpenExistentialForErasure(SILGenFunction &gen,
                              SILLocation loc,
                              Expr *subExpr) {
  Type subType = subExpr->getType();
  assert(subType->isExistentialType());
  
  // If the source value is already of a protocol type, open the existential
  // container so we can steal its value.
  // TODO: Have a way to represent this operation in-place. The supertype
  // should be able to fit in the memory of the subtype existential.
  ManagedValue subExistential = gen.emitRValueAsSingleValue(subExpr);
  CanArchetypeType subFormalTy = ArchetypeType::getOpened(subType);
  SILType subLoweredTy = gen.getLoweredType(subFormalTy);
  bool isTake = subExistential.hasCleanup();
  SILValue subPayload;
  switch (subExistential.getType()
                            .getPreferredExistentialRepresentation(gen.SGM.M)) {
  case ExistentialRepresentation::None:
    llvm_unreachable("not existential");
  case ExistentialRepresentation::Metatype:
    llvm_unreachable("metatype-to-address-only erasure shouldn't happen");
  case ExistentialRepresentation::Opaque:
    subPayload = gen.B.createOpenExistentialAddr(loc,
                                                 subExistential.forward(gen),
                                                 subLoweredTy);
    // If we're going to take the payload, we need to deinit the leftover
    // existential shell.
    if (isTake)
      gen.Cleanups.pushCleanup<TakeFromExistentialCleanup>(
                                                     subExistential.getValue());

    break;
  case ExistentialRepresentation::Class:
    subPayload = gen.B.createOpenExistentialRef(loc,
                                                subExistential.forward(gen),
                                                subLoweredTy);
    break;
  // We currently don't have any boxed protocol compositions or boxed
  // protocols that inherit, so this should never occur yet. If that changes,
  // we would need to open_existential_box here.
  case ExistentialRepresentation::Boxed:
    // Can never take from a box; the value might be shared.
    isTake = false;
    llvm_unreachable("boxed-to-unboxed existential erasure not implemented");
  }
  gen.setArchetypeOpeningSite(subFormalTy, subPayload);
  ManagedValue subMV = isTake
    ? gen.emitManagedRValueWithCleanup(subPayload)
    : ManagedValue::forUnmanaged(subPayload);
  
  return {subMV, subFormalTy};
}

static RValue emitAddressOnlyErasure(SILGenFunction &gen, ErasureExpr *E,
                                     SGFContext C) {
  // FIXME: Need to stage cleanups here. If code fails between
  // InitExistential and initializing the value, clean up using
  // DeinitExistential.
  
  // Allocate the existential.
  auto &existentialTL = gen.getTypeLowering(E->getType());
  SILValue existential =
    gen.getBufferForExprResult(E, existentialTL.getLoweredType(), C);
  
  Type subType = E->getSubExpr()->getType();
  
  if (subType->isExistentialType()) {
    FullExpr scope(gen.Cleanups, CleanupLocation(E));

    ManagedValue subMV;
    CanArchetypeType subFormalTy;
    std::tie(subMV, subFormalTy)
       = emitOpenExistentialForErasure(gen, E, E->getSubExpr());

    // Set up the destination existential, and forward the payload into it.
    SILValue destAddr = gen.B.createInitExistentialAddr(E, existential,
                                                        subFormalTy,
                                                        subMV.getType(),
                                                        E->getConformances());
    
    subMV.forwardInto(gen, E, destAddr);
  } else {
    // Otherwise, we need to initialize a new existential container from
    // scratch.
    
    // Allocate the concrete value inside the container.
    auto concreteFormalType = subType->getCanonicalType();
    
    auto archetype = ArchetypeType::getOpened(E->getType());
    AbstractionPattern abstractionPattern(archetype);
    auto &concreteTL = gen.getTypeLowering(abstractionPattern,
                                           concreteFormalType);
    
    SILValue valueAddr = gen.B.createInitExistentialAddr(E, existential,
                                concreteFormalType,
                                concreteTL.getLoweredType(),
                                E->getConformances());
    // Initialize the concrete value in-place.
    InitializationPtr init(new KnownAddressInitialization(valueAddr));
    ManagedValue mv = gen.emitRValueAsOrig(E->getSubExpr(), abstractionPattern,
                                           concreteTL, SGFContext(init.get()));
    if (!mv.isInContext()) {
      mv.forwardInto(gen, E, init->getAddress());
      init->finishInitialization(gen);
    }
  }

  return RValue(gen, E,
                gen.manageBufferForExprResult(existential, existentialTL, C));
}

static RValue emitBoxedErasure(SILGenFunction &gen, ErasureExpr *E) {
  // FIXME: Need to stage cleanups here. If code fails between
  // AllocExistentialBox and initializing the value, clean up using
  // DeallocExistentialBox.
  auto &existentialTL = gen.getTypeLowering(E->getType());

  Type subType = E->getSubExpr()->getType();
  SILValue existential;
  if (subType->isExistentialType()) {
    FullExpr scope(gen.Cleanups, CleanupLocation(E));
    
    // Open the inner existential and steal its payload into a new box.
    ManagedValue subMV;
    CanArchetypeType subFormalTy;
    std::tie(subMV, subFormalTy)
      = emitOpenExistentialForErasure(gen, E, E->getSubExpr());

    auto box = gen.B.createAllocExistentialBox(E,
                                               existentialTL.getLoweredType(),
                                               subFormalTy, subMV.getType(),
                                               E->getConformances());
    existential = box->getExistentialResult();
    subMV.forwardInto(gen, E, box->getValueAddressResult());
  } else {
    // Allocate a box and evaluate the subexpression into it.
    auto concreteFormalType = subType->getCanonicalType();
    
    auto archetype = ArchetypeType::getOpened(E->getType());
    AbstractionPattern abstractionPattern(archetype);
    auto &concreteTL = gen.getTypeLowering(abstractionPattern,
                                           concreteFormalType);

    auto box = gen.B.createAllocExistentialBox(E,
                                               existentialTL.getLoweredType(),
                                               concreteFormalType,
                                               concreteTL.getLoweredType(),
                                               E->getConformances());
    existential = box->getExistentialResult();
    auto valueAddr = box->getValueAddressResult();
    
    // Initialize the concrete value in-place.
    InitializationPtr init(new KnownAddressInitialization(valueAddr));
    ManagedValue mv = gen.emitRValueAsOrig(E->getSubExpr(), abstractionPattern,
                                           concreteTL, SGFContext(init.get()));
    if (!mv.isInContext()) {
      mv.forwardInto(gen, E, init->getAddress());
      init->finishInitialization(gen);
    }
  }
  
  return RValue(gen, E, gen.emitManagedRValueWithCleanup(existential));
}

RValue RValueEmitter::visitErasureExpr(ErasureExpr *E, SGFContext C) {
  switch (SILType::getPrimitiveObjectType(E->getType()->getCanonicalType())
            .getPreferredExistentialRepresentation(SGF.SGM.M,
                                                   E->getSubExpr()->getType())){
  case ExistentialRepresentation::None:
    llvm_unreachable("not an existential type");
  case ExistentialRepresentation::Metatype:
    llvm_unreachable("metatype erasure should be represented by "
                     "MetatypeErasureExpr");
  case ExistentialRepresentation::Class:
    return emitClassBoundedErasure(SGF, E);
  case ExistentialRepresentation::Boxed:
    return emitBoxedErasure(SGF, E);
  case ExistentialRepresentation::Opaque:
    return emitAddressOnlyErasure(SGF, E, C);
  }
}

/// Given an existential type or metatype, produce the type that
/// results from opening the underlying existential type.
static CanType getOpenedTypeForExistential(CanType type,
                                           CanArchetypeType &openedArchetype) {
  assert(type.isAnyExistentialType());
  if (auto metatype = dyn_cast<ExistentialMetatypeType>(type)) {
    auto instance = getOpenedTypeForExistential(metatype.getInstanceType(),
                                                openedArchetype);
    return CanMetatypeType::get(instance);
  }
  openedArchetype = ArchetypeType::getOpened(type);
  return openedArchetype;
}

static SILType
getOpenedTypeForLoweredExistentialMetatype(SILType type,
                                           CanArchetypeType &openedArchetype) {
  auto metatype = type.castTo<ExistentialMetatypeType>();
  auto instanceType = getOpenedTypeForExistential(metatype.getInstanceType(),
                                                  openedArchetype);
  auto resultType =
    CanMetatypeType::get(instanceType, metatype->getRepresentation());
  return SILType::getPrimitiveObjectType(resultType);
}

static SILValue emitOpenExistentialMetatype(SILGenFunction &SGF,
                                            SILLocation loc,
                                            SILValue metatype) {
  CanArchetypeType openedArchetype;
  SILType resultType =
    getOpenedTypeForLoweredExistentialMetatype(metatype.getType(),
                                               openedArchetype);
  auto openingSite =
    SGF.B.createOpenExistentialMetatype(loc, metatype, resultType);
  SGF.setArchetypeOpeningSite(openedArchetype, openingSite);
  return openingSite;
}

RValue RValueEmitter::visitMetatypeErasureExpr(MetatypeErasureExpr *E,
                                               SGFContext C) {
  SILValue metatype =
    SGF.emitRValueAsSingleValue(E->getSubExpr()).getUnmanagedValue();

  // Thicken the metatype if necessary.
  auto metatypeTy = metatype.getType().castTo<AnyMetatypeType>();
  if (isa<MetatypeType>(metatypeTy)) {
    if (metatypeTy->getRepresentation() == MetatypeRepresentation::Thin) {
      auto thickMetatypeTy = CanMetatypeType::get(metatypeTy.getInstanceType(),
                                                  MetatypeRepresentation::Thick);
      metatype = SGF.B.createMetatype(E,
                             SILType::getPrimitiveObjectType(thickMetatypeTy));
    }

  // If we're starting from an existential metatype, open it first.
  } else {
    assert(isa<ExistentialMetatypeType>(metatypeTy));
    metatype = emitOpenExistentialMetatype(SGF, E, metatype);
  }

  assert(metatype.getType().castTo<AnyMetatypeType>()->getRepresentation()
           == MetatypeRepresentation::Thick);

  auto loweredResultTy = SGF.getLoweredLoadableType(E->getType());
  auto upcast =
    SGF.B.createInitExistentialMetatype(E, metatype, loweredResultTy,
                                        E->getConformances());
  return RValue(SGF, E, ManagedValue::forUnmanaged(upcast));
}

/// Treating this as a successful operation, turn a CMV into a +1 MV.
ManagedValue SILGenFunction::getManagedValue(SILLocation loc,
                                             ConsumableManagedValue value) {
  // If the consumption rules say that this is already +1 given a
  // successful operation, just use the value.
  if (value.isOwned())
    return value.getFinalManagedValue();

  SILType valueTy = value.getType();
  auto &valueTL = getTypeLowering(valueTy);

  // If the type is trivial, it's always +1.
  if (valueTL.isTrivial())
    return ManagedValue::forUnmanaged(value.getValue());

  // If it's an object, retain and enter a release cleanup.
  if (valueTy.isObject()) {
    valueTL.emitRetainValue(B, loc, value.getValue());
    return emitManagedRValueWithCleanup(value.getValue(), valueTL);
  }

  // Otherwise, produce a temporary and copy into that.
  auto temporary = emitTemporary(loc, valueTL);
  valueTL.emitCopyInto(B, loc, value.getValue(), temporary->getAddress(),
                       IsNotTake, IsInitialization);
  temporary->finishInitialization(*this);
  return temporary->getManagedAddress();
}

RValue RValueEmitter::visitForcedCheckedCastExpr(ForcedCheckedCastExpr *E,
                                                 SGFContext C) {
  return emitUnconditionalCheckedCast(SGF, E, E->getSubExpr(), E->getType(),
                                      E->getCastKind(), C);
}


RValue RValueEmitter::
visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *E,
                                SGFContext C) {
  ManagedValue operand = SGF.emitRValueAsSingleValue(E->getSubExpr());
  return emitConditionalCheckedCast(SGF, E, operand, E->getSubExpr()->getType(),
                                    E->getType(), E->getCastKind(), C);
}

RValue RValueEmitter::visitIsExpr(IsExpr *E, SGFContext C) {
  SILValue isa = emitIsa(SGF, E, E->getSubExpr(),
                         E->getCastTypeLoc().getType(), E->getCastKind());

  // Call the _getBool library intrinsic.
  ASTContext &ctx = SGF.SGM.M.getASTContext();
  auto result =
    SGF.emitApplyOfLibraryIntrinsic(E, ctx.getGetBoolDecl(nullptr), {},
                                    ManagedValue::forUnmanaged(isa),
                                    C);
  return RValue(SGF, E, result);
}

RValue RValueEmitter::visitCoerceExpr(CoerceExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

VarargsInfo Lowering::emitBeginVarargs(SILGenFunction &gen, SILLocation loc,
                                       CanType baseTy, CanType arrayTy,
                                       unsigned numElements) {
  // Reabstract the base type against the array element type.
  AbstractionPattern baseAbstraction(
    arrayTy->getNominalOrBoundGenericNominal()
           ->getGenericParams()->getPrimaryArchetypes()[0]);
  
  // Allocate the array.
  SILValue numEltsVal = gen.B.createIntegerLiteral(loc,
                             SILType::getBuiltinWordType(gen.getASTContext()),
                             numElements);
  // The first result is the array value.
  ManagedValue array;
  // The second result is a RawPointer to the base address of the array.
  SILValue basePtr;
  std::tie(array, basePtr)
    = gen.emitUninitializedArrayAllocation(arrayTy, numEltsVal, loc);

  auto &baseTL = gen.getTypeLowering(baseAbstraction, baseTy);

  // Turn the pointer into an address.
  basePtr = gen.B.createPointerToAddress(loc, basePtr,
                                     baseTL.getLoweredType().getAddressType());

  return VarargsInfo(array, basePtr, baseTL, baseAbstraction);
}

ManagedValue Lowering::emitEndVarargs(SILGenFunction &gen, SILLocation loc,
                                      VarargsInfo &&varargs) {
  // TODO: maybe do something to finalize the array value here?
  return varargs.getArray();
}

static ManagedValue emitVarargs(SILGenFunction &gen,
                                SILLocation loc,
                                Type _baseTy,
                                ArrayRef<ManagedValue> elements,
                                Type _arrayTy) {
  auto baseTy = _baseTy->getCanonicalType();
  auto arrayTy = _arrayTy->getCanonicalType();

  auto varargs = emitBeginVarargs(gen, loc, baseTy, arrayTy, elements.size());
  AbstractionPattern baseAbstraction = varargs.getBaseAbstractionPattern();
  SILValue basePtr = varargs.getBaseAddress();
  
  // Initialize the members.
  // TODO: If we need to cleanly unwind at this point, we would need to arrange
  // for the partially-initialized array to be cleaned up somehow, maybe by
  // poking its count to the actually-initialized size at the point of failure.
  
  for (size_t i = 0, size = elements.size(); i < size; ++i) {
    SILValue eltPtr = basePtr;
    if (i != 0) {
      SILValue index = gen.B.createIntegerLiteral(loc,
                  SILType::getBuiltinWordType(gen.F.getASTContext()), i);
      eltPtr = gen.B.createIndexAddr(loc, basePtr, index);
    }
    ManagedValue v = elements[i];
    v = gen.emitSubstToOrigValue(loc, v, baseAbstraction, baseTy);
    v.forwardInto(gen, loc, eltPtr);
  }

  return emitEndVarargs(gen, loc, std::move(varargs));
}

RValue RValueEmitter::visitTupleExpr(TupleExpr *E, SGFContext C) {
  auto type = cast<TupleType>(E->getType()->getCanonicalType());

  // If we have an Initialization, emit the tuple elements into its elements.
  if (Initialization *I = C.getEmitInto()) {
    if (I->canSplitIntoSubelementAddresses()) {
      SmallVector<InitializationPtr, 4> subInitializationBuf;
      auto subInitializations =
        I->getSubInitializationsForTuple(SGF, type, subInitializationBuf,
                                         RegularLocation(E));
      assert(subInitializations.size() == E->getElements().size() &&
             "initialization for tuple has wrong number of elements");
      for (unsigned i = 0, size = subInitializations.size(); i < size; ++i)
        SGF.emitExprInto(E->getElement(i), subInitializations[i].get());
      return RValue();
    }
  }
    
  RValue result(type);
  for (Expr *elt : E->getElements())
    result.addElement(SGF.emitRValue(elt));
  return result;
}

RValue RValueEmitter::visitMemberRefExpr(MemberRefExpr *E, SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");

  if (isa<TypeDecl>(E->getMember().getDecl())) {
    // Emit the metatype for the associated type.
    visit(E->getBase());
    SILValue MT =
      SGF.B.createMetatype(E, SGF.getLoweredLoadableType(E->getType()));
    return RValue(SGF, E, ManagedValue::forUnmanaged(MT));
  }
  
  if (isa<AbstractFunctionDecl>(E->getMember().getDecl())) {
    // Method references into generics are represented as member refs instead
    // of apply exprs for some reason. Send this down the correct path to be
    // treated as a curried method application.
    return SGF.emitApplyExpr(E, C);
  }

  // Check to see if we should do this with a simple struct_extract.
  auto field = cast<VarDecl>(E->getMember().getDecl());
  if (E->getBase()->getType()->getStructOrBoundGenericStruct()) {
    AccessStrategy strategy =
      field->getAccessStrategy(E->getAccessSemantics(), AccessKind::Read);
    if (strategy == AccessStrategy::Storage) {
      ManagedValue base = SGF.emitRValueAsSingleValue(E->getBase(),
                                          SGFContext::AllowImmediatePlusZero);
      ManagedValue result =
        SGF.emitRValueForPropertyLoad(E, base, E->isSuper(), field,
                                      E->getMember().getSubstitutions(),
                                      E->getAccessSemantics(),
                                      E->getType(), C);
      return RValue(SGF, E, result);
    }
  }

  // Everything else should use the l-value logic.

  // Any writebacks for this access are tightly scoped.
  WritebackScope scope(SGF);

  LValue lv = SGF.emitLValue(E, AccessKind::Read);
  return RValue(SGF, E, SGF.emitLoadOfLValue(E, std::move(lv), C));
}

RValue RValueEmitter::visitDynamicMemberRefExpr(DynamicMemberRefExpr *E,
                                                SGFContext C) {
  return SGF.emitDynamicMemberRefExpr(E, C);
}

RValue RValueEmitter::
visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E, SGFContext C) {
  visit(E->getLHS());
  return visit(E->getRHS());
}

RValue RValueEmitter::visitSubscriptExpr(SubscriptExpr *E, SGFContext C) {
  // Any writebacks for this access are tightly scoped.
  WritebackScope scope(SGF);

  LValue lv = SGF.emitLValue(E, AccessKind::Read);
  return RValue(SGF, E, SGF.emitLoadOfLValue(E, std::move(lv), C));
}

RValue RValueEmitter::visitDynamicSubscriptExpr(
                                      DynamicSubscriptExpr *E, SGFContext C) {
  return SGF.emitDynamicSubscriptExpr(E, C);
}


RValue RValueEmitter::visitModuleExpr(ModuleExpr *E, SGFContext C) {
  // Produce an undef value. The module value should never actually be used.
  SILValue module = SILUndef::get(SGF.getLoweredLoadableType(E->getType()),
                                  SGF.SGM.M);
  return RValue(SGF, E, ManagedValue::forUnmanaged(module));
}

RValue RValueEmitter::visitTupleElementExpr(TupleElementExpr *E,
                                            SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");
  
  // If our client is ok with a +0 result, then we can compute our base as +0
  // and return its element that way.  It would not be ok to reuse the Context's
  // address buffer though, since our base value will a different type than the
  // element.
  SGFContext SubContext = C.withFollowingProjection();
  
  return visit(E->getBase(), SubContext).extractElement(E->getFieldNumber());
}

ManagedValue
SILGenFunction::emitApplyOfDefaultArgGenerator(SILLocation loc,
                                               ConcreteDeclRef defaultArgsOwner,
                                               unsigned destIndex,
                                               CanType resultType,
                                             AbstractionPattern origResultType,
                                               SGFContext C) {
  SILDeclRef generator 
    = SILDeclRef::getDefaultArgGenerator(defaultArgsOwner.getDecl(),
                                         destIndex);

  auto fnRef = emitFunctionRef(loc, generator);
  auto fnType = fnRef.getType().castTo<SILFunctionType>();
  auto substFnType = fnType->substGenericArgs(SGM.M, SGM.M.getSwiftModule(),
                                         defaultArgsOwner.getSubstitutions());
  return emitApply(loc, fnRef, defaultArgsOwner.getSubstitutions(),
                   {}, substFnType,
                   origResultType, resultType,
                   generator.isTransparent(), None, None, C);
}

RValue RValueEmitter::visitTupleShuffleExpr(TupleShuffleExpr *E,
                                            SGFContext C) {
  /* TODO:
  // If we're emitting into an initialization, we can try shuffling the
  // elements of the initialization.
  if (Initialization *I = C.getEmitInto()) {
    emitTupleShuffleExprInto(*this, E, I);
    return RValue();
  }
   */

  // Emit the sub-expression tuple and destructure it into elements.
  SmallVector<RValue, 4> elements;
  visit(E->getSubExpr()).extractElements(elements);
  
  // Prepare a new tuple to hold the shuffled result.
  RValue result(E->getType()->getCanonicalType());
  
  auto outerFields = E->getType()->castTo<TupleType>()->getElements();
  auto shuffleIndexIterator = E->getElementMapping().begin();
  auto shuffleIndexEnd = E->getElementMapping().end();
  for (auto &field : outerFields) {
    assert(shuffleIndexIterator != shuffleIndexEnd &&
           "ran out of shuffle indexes before running out of fields?!");
    int shuffleIndex = *shuffleIndexIterator++;
    
    assert(shuffleIndex != TupleShuffleExpr::DefaultInitialize &&
           shuffleIndex != TupleShuffleExpr::CallerDefaultInitialize &&
           "Only argument tuples can have default initializers & varargs");

    // If the shuffle index is FirstVariadic, it is the beginning of the list of
    // varargs inputs.  Save this case for last.
    if (shuffleIndex != TupleShuffleExpr::FirstVariadic) {
      // Map from a different tuple element.
      result.addElement(std::move(elements[shuffleIndex]));
      continue;
    }

    assert(field.isVararg() && "Cannot initialize nonvariadic element");
    
    // Okay, we have a varargs tuple element.  All the remaining elements feed
    // into the varargs portion of this, which is then constructed into an Array
    // through an informal protocol captured by the InjectionFn in the
    // TupleShuffleExpr.
    assert(E->getVarargsArrayTypeOrNull() &&
           "no injection type for varargs tuple?!");
    SmallVector<ManagedValue, 4> variadicValues;
    
    while (shuffleIndexIterator != shuffleIndexEnd) {
      unsigned sourceField = *shuffleIndexIterator++;
      variadicValues.push_back(
                     std::move(elements[sourceField]).getAsSingleValue(SGF, E));
    }
    
    ManagedValue varargs = emitVarargs(SGF, E, field.getVarargBaseTy(),
                                       variadicValues,
                                       E->getVarargsArrayType());
    result.addElement(RValue(SGF, E, field.getType()->getCanonicalType(),
                             varargs));
    break;
  }
  
  return result;
}

static void emitScalarToTupleExprInto(SILGenFunction &gen,
                                      ScalarToTupleExpr *E,
                                      Initialization *I) {
  auto tupleType = cast<TupleType>(E->getType()->getCanonicalType());
  auto outerFields = tupleType->getElements();
  unsigned scalarField = E->getScalarField();
  bool isScalarFieldVariadic = outerFields[scalarField].isVararg();

  // Decompose the initialization.
  SmallVector<InitializationPtr, 4> subInitializationBuf;
  auto subInitializations = I->getSubInitializationsForTuple(gen, tupleType,
                                                          subInitializationBuf,
                                                          RegularLocation(E));
  assert(subInitializations.size() == outerFields.size() &&
         "initialization size does not match tuple size?!");
  
  // If the scalar field isn't variadic, emit it into the destination field of
  // the tuple.
  Initialization *scalarInit = subInitializations[E->getScalarField()].get();
  if (!isScalarFieldVariadic) {
    gen.emitExprInto(E->getSubExpr(), scalarInit);
  } else {
    // Otherwise, create the vararg and store it to the vararg field.
    ManagedValue scalar = gen.emitRValueAsSingleValue(E->getSubExpr());
    ManagedValue varargs = emitVarargs(gen, E, E->getSubExpr()->getType(),
                                       scalar, E->getVarargsArrayType());
    varargs.forwardInto(gen, E, scalarInit->getAddress());
    scalarInit->finishInitialization(gen);
  }
  
  // Emit the non-scalar fields.
  for (unsigned i = 0, e = outerFields.size(); i != e; ++i) {
    if (i == E->getScalarField())
      continue;

    // Fill the vararg field with an empty array.
    if (outerFields[i].isVararg()) {
      assert(i == e - 1 && "vararg isn't last?!");
      ManagedValue varargs = emitVarargs(gen, E,
                                         outerFields[i].getVarargBaseTy(),
                                         {}, E->getVarargsArrayType());
      varargs.forwardInto(gen, E, subInitializations[i]->getAddress());
      subInitializations[i]->finishInitialization(gen);
      continue;
    }

    const auto &element = E->getElement(i);
    // If this element comes from a default argument generator, emit a call to
    // that generator in-place.
    assert(outerFields[i].hasInit() &&
           "no default initializer in non-scalar field of scalar-to-tuple?!");
    if (auto defaultArgOwner = element.dyn_cast<ConcreteDeclRef>()) {
      SILDeclRef generator
        = SILDeclRef::getDefaultArgGenerator(defaultArgOwner.getDecl(), i);
      auto fnRef = gen.emitFunctionRef(E, generator);
      auto resultType = tupleType.getElementType(i);
      auto apply = gen.emitMonomorphicApply(E, fnRef, {}, resultType,
                                            generator.isTransparent(),
                                            None, None);
      apply.forwardInto(gen, E,
                        subInitializations[i].get()->getAddressOrNull());
      subInitializations[i]->finishInitialization(gen);
      continue;
    }

    // We have an caller-side default argument. Emit it in-place.
    Expr *defArg = element.get<Expr *>();
    gen.emitExprInto(defArg, subInitializations[i].get());
  }
}

RValue RValueEmitter::visitScalarToTupleExpr(ScalarToTupleExpr *E,
                                             SGFContext C) {
  // If we're emitting into an Initialization, we can decompose the
  // initialization.
  if (Initialization *I = C.getEmitInto()) {
    if (I->canSplitIntoSubelementAddresses()) {
      emitScalarToTupleExprInto(SGF, E, I);
      return RValue();
    }
  }

  // Emit the scalar member.
  RValue scalar = SGF.emitRValue(E->getSubExpr());

  // Prepare a tuple rvalue to house the result.
  RValue result(E->getType()->getCanonicalType());
  
  // Create a tuple from the scalar along with any default values or varargs.
  auto outerFields = E->getType()->castTo<TupleType>()->getElements();
  for (unsigned i = 0, e = outerFields.size(); i != e; ++i) {
    // Handle the variadic argument. If we didn't emit the scalar field yet,
    // it goes into the variadic array; otherwise, the variadic array is empty.
    assert(!outerFields[i].isVararg() &&
           "Only argument tuples can have default initializers & varargs");
    if (outerFields[i].isVararg()) {
      assert(i == e - 1 && "vararg isn't last?!");
      ManagedValue varargs;
      if (!scalar.isUsed())
        varargs = emitVarargs(SGF, E, outerFields[i].getVarargBaseTy(),
                              std::move(scalar).getAsSingleValue(SGF, E),
                              E->getVarargsArrayType());
      else
        varargs = emitVarargs(SGF, E, outerFields[i].getVarargBaseTy(),
                              {}, E->getVarargsArrayType());
      result.addElement(RValue(SGF, E,
                               outerFields[i].getType()->getCanonicalType(),
                               varargs));
      break;
    }

    // A null element indicates that this is the position of the scalar. Add
    // the scalar here.
    assert(E->getElement(i).isNull() && "Unknown scalar to tuple conversion");
    result.addElement(std::move(scalar));
  }

  return result;
}

SILValue SILGenFunction::emitMetatypeOfValue(SILLocation loc, Expr *baseExpr) {
  Type formalBaseType = baseExpr->getType()->getLValueOrInOutObjectType();
  CanType baseTy = formalBaseType->getCanonicalType();

  // For class, archetype, and protocol types, look up the dynamic metatype.
  if (baseTy.isAnyExistentialType()) {
    SILType metaTy = getLoweredLoadableType(
                                      CanExistentialMetatypeType::get(baseTy));
    auto base = emitRValueAsSingleValue(baseExpr,
                                  SGFContext::AllowImmediatePlusZero).getValue();
    return B.createExistentialMetatype(loc, metaTy, base);
  }

  SILType metaTy = getLoweredLoadableType(CanMetatypeType::get(baseTy));
  // If the lowered metatype has a thick representation, we need to derive it
  // dynamically from the instance.
  if (metaTy.castTo<MetatypeType>()->getRepresentation()
          != MetatypeRepresentation::Thin) {
    auto base = emitRValueAsSingleValue(baseExpr,
                               SGFContext::AllowImmediatePlusZero).getValue();
    return B.createValueMetatype(loc, metaTy, base);
  }
  
  // Otherwise, ignore the base and return the static thin metatype.
  emitIgnoredExpr(baseExpr);
  return B.createMetatype(loc, metaTy);
}

RValue RValueEmitter::visitDynamicTypeExpr(DynamicTypeExpr *E, SGFContext C) {
  auto metatype = SGF.emitMetatypeOfValue(E, E->getBase());
  return RValue(SGF, E, ManagedValue::forUnmanaged(metatype));
}

RValue RValueEmitter::visitCaptureListExpr(CaptureListExpr *E, SGFContext C) {
  // ClosureExpr's evaluate their bound variables.
  for (auto capture : E->getCaptureList()) {
    SGF.visit(capture.Var);
    SGF.visit(capture.Init);
  }

  // Then they evaluate to their body.
  return visit(E->getClosureBody(), C);
}


RValue RValueEmitter::visitAbstractClosureExpr(AbstractClosureExpr *e,
                                               SGFContext C) {
  // Generate the closure function, if we haven't already.
  // We may visit the same closure expr multiple times in some cases, for
  // instance, when closures appear as in-line initializers of stored properties,
  // in which case the closure will be emitted into every initializer of the
  // containing type.
  if (!SGF.SGM.hasFunction(SILDeclRef(e)))
    SGF.SGM.emitClosure(e);

  // Generate the closure value (if any) for the closure expr's function
  // reference.
  return RValue(SGF, e,
                SGF.emitClosureValue(e, SILDeclRef(e),
                                     SGF.getForwardingSubstitutions(), e));
}

RValue RValueEmitter::
visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                   SGFContext C) {
  return visit(E->getSemanticExpr(), C);
}

RValue RValueEmitter::
visitObjectLiteralExpr(ObjectLiteralExpr *E, SGFContext C) {
  return visit(E->getSemanticExpr(), C);
}

static StringRef
getMagicFunctionString(SILGenFunction &gen) {
  assert(gen.MagicFunctionName
         && "asking for __FUNCTION__ but we don't have a function name?!");
  if (gen.MagicFunctionString.empty()) {
    llvm::raw_string_ostream os(gen.MagicFunctionString);
    gen.MagicFunctionName.printPretty(os);
  }
  return gen.MagicFunctionString;
}

RValue RValueEmitter::
visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E, SGFContext C) {
  ASTContext &Ctx = SGF.SGM.M.getASTContext();
  SILType Ty = SGF.getLoweredLoadableType(E->getType());
  SourceLoc Loc;
  
  // If "overrideLocationForMagicIdentifiers" is set, then we use it as the
  // location point for these magic identifiers.
  if (SGF.overrideLocationForMagicIdentifiers)
    Loc = SGF.overrideLocationForMagicIdentifiers.getValue();
  else
    Loc = E->getStartLoc();
  
  switch (E->getKind()) {
  case MagicIdentifierLiteralExpr::File: {
    StringRef Value = "";
    if (Loc.isValid()) {
      unsigned BufferID = Ctx.SourceMgr.findBufferContainingLoc(Loc);
      Value = Ctx.SourceMgr.getIdentifierForBuffer(BufferID);
    }

    return emitStringLiteral(E, Value, C, E->getStringEncoding());
  }
  case MagicIdentifierLiteralExpr::Function: {
    StringRef Value = "";
    if (Loc.isValid())
      Value = getMagicFunctionString(SGF);
    return emitStringLiteral(E, Value, C, E->getStringEncoding());
  }
  case MagicIdentifierLiteralExpr::Line: {
    unsigned Value = 0;
    if (Loc.isValid())
      Value = Ctx.SourceMgr.getLineAndColumn(Loc).first;

    SILValue V = SGF.B.createIntegerLiteral(E, Ty, Value);
    return RValue(SGF, E, ManagedValue::forUnmanaged(V));
  }
  case MagicIdentifierLiteralExpr::Column: {
    unsigned Value = 0;
    if (Loc.isValid())
      Value = Ctx.SourceMgr.getLineAndColumn(Loc).second;

    SILValue V = SGF.B.createIntegerLiteral(E, Ty, Value);
    return RValue(SGF, E, ManagedValue::forUnmanaged(V));
  }

  case MagicIdentifierLiteralExpr::DSOHandle: {
    auto Val = SGF.emitRValueForDecl(E, SGF.SGM.SwiftModule->getDSOHandle(), 
                                     E->getType(),
                                     AccessSemantics::Ordinary,
                                     C);
    return RValue(SGF, E, Val);
  }
  }
}

RValue RValueEmitter::visitCollectionExpr(CollectionExpr *E, SGFContext C) {
  return visit(E->getSemanticExpr(), C);
}

RValue RValueEmitter::visitRebindSelfInConstructorExpr(
                                RebindSelfInConstructorExpr *E, SGFContext C) {
  auto selfDecl = E->getSelf();
  auto ctorDecl = cast<ConstructorDecl>(selfDecl->getDeclContext());
  auto selfTy = selfDecl->getType()->getInOutObjectType();
  
  auto newSelfTy = E->getSubExpr()->getType();
  OptionalTypeKind failability;
  if (auto objTy = newSelfTy->getAnyOptionalObjectType(failability))
    newSelfTy = objTy;
  
  bool isSuper = !newSelfTy->isEqual(selfTy);

  // The subexpression consumes the current 'self' binding.
  assert(SGF.SelfInitDelegationState == SILGenFunction::NormalSelf
         && "already doing something funky with self?!");
  SGF.SelfInitDelegationState = SILGenFunction::WillConsumeSelf;
  
  // Emit the subexpression.
  ManagedValue newSelf = SGF.emitRValueAsSingleValue(E->getSubExpr());

  // We know that self is a box, so get its address.
  SILValue selfAddr =
    SGF.emitLValueForDecl(E, selfDecl, selfTy->getCanonicalType(),
                          AccessKind::Write).getLValueAddress();

  // In a class self.init or super.init situation, the self value will 'take'
  // the value out of the box, leaving it as an unowned reference in an
  // otherwise valid box.  We need to null it out so that a release of the box
  // (e.g. on an error path of a failable init) will not do an extra release of
  // the bit pattern in the box.
  if (SGF.SelfInitDelegationState == SILGenFunction::DidConsumeSelf) {
    auto Zero = SGF.B.createNullClass(E, selfAddr.getType().getObjectType());
    SGF.B.createStore(E, Zero, selfAddr);
  }
  
  // If the delegated-to initializer can fail, check for the potential failure.
  switch (failability) {
  case OTK_None:
    // Not failable.
    break;

  case OTK_Optional:
  case OTK_ImplicitlyUnwrappedOptional: {
    // If the current constructor is not failable, abort.
    switch (ctorDecl->getFailability()) {
    case OTK_Optional:
    case OTK_ImplicitlyUnwrappedOptional: {
      SILBasicBlock *someBB = SGF.createBasicBlock();

      auto hasValue = SGF.emitDoesOptionalHaveValue(E, newSelf.getValue());
      
      assert(SGF.FailDest.isValid() && "too big to fail");
      
      // On the failure case, we don't need to clean up the 'self' returned
      // by the call to the other constructor, since we know it is nil and
      // therefore dynamically trivial.
      SGF.Cleanups.setCleanupState(newSelf.getCleanup(), CleanupState::Dormant);
      auto noneBB = SGF.Cleanups.emitBlockForCleanups(SGF.FailDest, E);
      SGF.Cleanups.setCleanupState(newSelf.getCleanup(), CleanupState::Active);
      
      SGF.B.createCondBranch(E, hasValue, someBB, noneBB);
      
      // Otherwise, project out the value and carry on.
      SGF.B.emitBlock(someBB);
      
      // If the current constructor is not failable, force out the value.
      newSelf = SGF.emitUncheckedGetOptionalValueFrom(E, newSelf,
                                      SGF.getTypeLowering(newSelf.getType()),
                                                      SGFContext());
      break;
    }
    case OTK_None: {
      // Materialize the value so we can pass it to
      // emitCheckedGetOptionalValueFrom, which requires it to be indirect.
      auto mat = SGF.emitMaterialize(E, newSelf);

      auto matMV = ManagedValue(mat.address, mat.valueCleanup);
      // If the current constructor is not failable, force out the value.
      newSelf = SGF.emitCheckedGetOptionalValueFrom(E, matMV,
                                         SGF.getTypeLowering(newSelf.getType()),
                                         SGFContext());
      break;
    }
    }
    break;
  }
  }
  
  // If we called a superclass constructor, cast down to the subclass.
  if (isSuper) {
    assert(newSelf.getType().isObject() &&
           newSelf.getType().hasReferenceSemantics() &&
           "delegating ctor type mismatch for non-reference type?!");
    CleanupHandle newSelfCleanup = newSelf.getCleanup();

    SILValue newSelfValue;
    auto destTy = SGF.getLoweredLoadableType(E->getSelf()->getType());

    // Assume that the returned 'self' is the appropriate subclass
    // type (or a derived class thereof). Only Objective-C classes can
    // violate this assumption.
    newSelfValue = SGF.B.createUncheckedRefCast(E, newSelf.getValue(),
                                                destTy);
    newSelf = ManagedValue(newSelfValue, newSelfCleanup);
  }

  // Forward or assign into the box depending on whether we actually consumed
  // 'self'.
  switch (SGF.SelfInitDelegationState) {
  case SILGenFunction::NormalSelf:
    llvm_unreachable("self isn't normal in a constructor delegation");
    
  case SILGenFunction::WillConsumeSelf:
    // We didn't consume, so reassign.
    newSelf.assignInto(SGF, E, selfAddr);
    break;
  
  case SILGenFunction::DidConsumeSelf:
    // We did consume, so reinitialize.
    newSelf.forwardInto(SGF, E, selfAddr);
    break;
  }
  SGF.SelfInitDelegationState = SILGenFunction::NormalSelf;

  // If we are using Objective-C allocation, the caller can return
  // nil. When this happens with an explicitly-written super.init or
  // self.init invocation, return early if we did get nil.
  //
  // TODO: Remove this when failable initializers are fully implemented.
  auto classDecl = selfTy->getClassOrBoundGenericClass();
  if (classDecl && !E->getSubExpr()->isImplicit() &&
      usesObjCAllocator(classDecl)) {
    // Check whether the new self is null.
    SILValue isNonnullSelf = SGF.B.createIsNonnull(E, newSelf.getValue());
    Condition cond = SGF.emitCondition(isNonnullSelf, E, 
                                       /*hasFalseCode=*/false,
                                       /*invertValue=*/true,
                                       { });

    // If self is null, branch to the epilog.
    cond.enterTrue(SGF);
    SGF.Cleanups.emitBranchAndCleanups(SGF.ReturnDest, E, { });
    cond.exitTrue(SGF);

    cond.complete(SGF);
  }

  return SGF.emitEmptyTupleRValue(E, C);
}

/// Determine whether the given declaration returns a non-optional object that
/// might actually be nil.
///
/// This is an awful hack that makes it possible to work around several kinds
/// of problems:
///   - initializers currently cannot fail, so they always return non-optional.
///   - an Objective-C method might have been annotated to state (incorrectly)
///     that it returns a non-optional object
///   - an Objective-C property might be annotated to state (incorrectly) that
///     it is non-optional
static bool mayLieAboutNonOptionalReturn(ValueDecl *decl) {
  // Any Objective-C initializer, because failure propagates from any
  // initializer written in Objective-C (and there's no way to tell).
  if (auto constructor = dyn_cast<ConstructorDecl>(decl)) {
    return constructor->isObjC();
  }

  // Functions that return non-optional reference type and were imported from
  // Objective-C.
  if (auto func = dyn_cast<FuncDecl>(decl)) {
    return func->hasClangNode() &&
           func->getResultType()->hasReferenceSemantics();
  }

  // Properties of non-optional reference type that were imported from
  // Objective-C.
  if (auto var = dyn_cast<VarDecl>(decl)) {
    return var->hasClangNode() && 
      var->getType()->getReferenceStorageReferent()->hasReferenceSemantics();
  }

  // Subscripts of non-optional reference type that were imported from
  // Objective-C.
  if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
    return subscript->hasClangNode() &&
           subscript->getElementType()->hasReferenceSemantics();
  }
  return false;
}

/// Determine whether the given expression returns a non-optional object that
/// might actually be nil.
///
/// This is an awful hack that makes it possible to work around several kinds
/// of problems:
///   - initializers currently cannot fail, so they always return non-optional.
///   - an Objective-C method might have been annotated to state (incorrectly)
///     that it returns a non-optional object
///   - an Objective-C property might be annotated to state (incorrectly) that
///     it is non-optional
static bool mayLieAboutNonOptionalReturn(Expr *expr) {
  expr = expr->getSemanticsProvidingExpr();

  /// A reference to a declaration.
  if (auto declRef = dyn_cast<DeclRefExpr>(expr)) {
    return mayLieAboutNonOptionalReturn(declRef->getDecl());
  }

  // An application, which we look through to get the function we're calling.
  if (auto apply = dyn_cast<ApplyExpr>(expr)) {
    return mayLieAboutNonOptionalReturn(apply->getFn());
  }

  // A load.
  if (auto load = dyn_cast<LoadExpr>(expr)) {
    return mayLieAboutNonOptionalReturn(load->getSubExpr());
  }

  // A reference to a member.
  if (auto member = dyn_cast<MemberRefExpr>(expr)) {
    return mayLieAboutNonOptionalReturn(member->getMember().getDecl());
  }

  // A reference to a subscript.
  if (auto subscript = dyn_cast<SubscriptExpr>(expr)) {
    return mayLieAboutNonOptionalReturn(subscript->getDecl().getDecl());
  }

  // A reference to a member found via dynamic lookup.
  if (auto member = dyn_cast<DynamicMemberRefExpr>(expr)) {
    return mayLieAboutNonOptionalReturn(member->getMember().getDecl());
  }

  // A reference to a subscript found via dynamic lookup.
  if (auto subscript = dyn_cast<DynamicSubscriptExpr>(expr)) {
    return mayLieAboutNonOptionalReturn(subscript->getMember().getDecl());
  }

  return false;
}

RValue RValueEmitter::visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *E,
                                                  SGFContext C) {
  // This is an awful hack. When the source expression might produce a
  // non-optional reference that could legitimated be nil, such as with an
  // initializer, allow this workaround to capture that nil:
  //
  //   let x: NSFoo? = NSFoo(potentiallyFailingInit: x)
  //
  // However, our optimizer is smart enough now to recognize that an initializer
  // can "never" produce nil, and will optimize away any attempts to check the
  // resulting optional for nil. As a special case, when we're injecting the
  // result of an ObjC constructor into an optional, do it using an unchecked
  // bitcast, which is opaque to the optimizer.
  if (mayLieAboutNonOptionalReturn(E->getSubExpr())) {
    auto result = SGF.emitRValueAsSingleValue(E->getSubExpr());
    auto optType = SGF.getLoweredLoadableType(E->getType());
    SILValue bitcast = SGF.B.createUncheckedRefBitCast(E, result.getValue(),
                                                       optType);
    ManagedValue bitcastMV = ManagedValue(bitcast, result.getCleanup());
    return RValue(SGF, E, bitcastMV);
  }

  // Create a buffer for the result if this is an address-only optional.
  auto &optTL = SGF.getTypeLowering(E->getType());
  if (!optTL.isAddressOnly()) {
    auto result = SGF.emitRValueAsSingleValue(E->getSubExpr());
    result = SGF.getOptionalSomeValue(E, result, optTL);
    return RValue(SGF, E, result);
  }
  
  SILValue optAddr = SGF.getBufferForExprResult(E, optTL.getLoweredType(), C);
  
  SGF.emitInjectOptionalValueInto(E, E->getSubExpr(), optAddr, optTL);
  
  ManagedValue result = SGF.manageBufferForExprResult(optAddr, optTL, C);
  if (result.isInContext())
    return RValue();
  return RValue(SGF, E, result);
}

RValue RValueEmitter::visitLValueToPointerExpr(LValueToPointerExpr *E,
                                               SGFContext C) {
  LValue lv = SGF.emitLValue(E->getSubExpr(), AccessKind::ReadWrite);
  SILValue address = SGF.emitAddressOfLValue(E->getSubExpr(),
                                             std::move(lv),
                                             AccessKind::ReadWrite)
    .getUnmanagedValue();
  // TODO: Reabstract the lvalue to match the abstraction level expected by
  // the inout address conversion's InOutType. For now, just report cases where
  // we would need a reabstraction as unsupported.
  SILType abstractedTy
    = SGF.getLoweredType(AbstractionPattern(E->getAbstractionPatternType()),
                         E->getSubExpr()->getType()->getLValueOrInOutObjectType());
  if (address.getType().getObjectType() != abstractedTy)
    SGF.SGM.diagnose(E, diag::not_implemented,
                     "abstraction difference in inout conversion");
  
  SILValue ptr = SGF.B.createAddressToPointer(E, address,
                              SILType::getRawPointerType(SGF.getASTContext()));
  return RValue(SGF, E, ManagedValue::forUnmanaged(ptr));
}

RValue RValueEmitter::visitClassMetatypeToObjectExpr(
                                                   ClassMetatypeToObjectExpr *E,
                                                   SGFContext C) {
  SILValue value = SGF.emitRValueAsSingleValue(E->getSubExpr())
    .getUnmanagedValue();
  
  // Convert the metatype to objc representation.
  auto metatypeTy = value.getType().castTo<MetatypeType>();
  auto objcMetatypeTy = CanMetatypeType::get(metatypeTy.getInstanceType(),
                                             MetatypeRepresentation::ObjC);
  value = SGF.B.createThickToObjCMetatype(E, value,
                               SILType::getPrimitiveObjectType(objcMetatypeTy));
  
  // Convert to an object reference.
  value = SGF.B.createObjCMetatypeToObject(E, value,
                                      SGF.getLoweredLoadableType(E->getType()));
  
  return RValue(SGF, E, ManagedValue::forUnmanaged(value));
}

RValue RValueEmitter::visitExistentialMetatypeToObjectExpr(
                                             ExistentialMetatypeToObjectExpr *E,
                                             SGFContext C) {
  SILValue value = SGF.emitRValueAsSingleValue(E->getSubExpr())
    .getUnmanagedValue();
  
  // Convert the metatype to objc representation.
  auto metatypeTy = value.getType().castTo<ExistentialMetatypeType>();
  auto objcMetatypeTy = CanExistentialMetatypeType::get(
                                              metatypeTy.getInstanceType(),
                                              MetatypeRepresentation::ObjC);
  value = SGF.B.createThickToObjCMetatype(E, value,
                               SILType::getPrimitiveObjectType(objcMetatypeTy));
  
  // Convert to an object reference.
  value = SGF.B.createObjCExistentialMetatypeToObject(E, value,
                                      SGF.getLoweredLoadableType(E->getType()));
  
  return RValue(SGF, E, ManagedValue::forUnmanaged(value));
}

RValue RValueEmitter::visitProtocolMetatypeToObjectExpr(
                                                ProtocolMetatypeToObjectExpr *E,
                                                SGFContext C) {
  SGF.emitIgnoredExpr(E->getSubExpr());
  ProtocolDecl *protocol = E->getSubExpr()->getType()->castTo<MetatypeType>()
    ->getInstanceType()->castTo<ProtocolType>()->getDecl();
  SILValue value = SGF.B.createObjCProtocol(E, protocol,
                                      SGF.getLoweredLoadableType(E->getType()));
  
  // Protocol objects, despite being global objects, inherit default reference
  // counting semantics from NSObject, so we need to retain the protocol
  // reference when we use it to prevent it being released and attempting to
  // deallocate itself. It doesn't matter if we ever actually clean up that
  // retain though.
  SGF.B.createStrongRetain(E, value);
  
  return RValue(SGF, E, ManagedValue::forUnmanaged(value));
}

RValue RValueEmitter::visitIfExpr(IfExpr *E, SGFContext C) {
  auto &lowering = SGF.getTypeLowering(E->getType());
  
  if (lowering.isLoadable()) {
    // If the result is loadable, emit each branch and forward its result
    // into the destination block argument.
    
    // FIXME: We could avoid imploding and reexploding tuples here.
    Condition cond = SGF.emitCondition(E->getCondExpr(),
                                       /*hasFalse*/ true,
                                       /*invertCondition*/ false,
                                       SGF.getLoweredType(E->getType()));
    
    cond.enterTrue(SGF);
    SGF.emitProfilerIncrement(E);
    SILValue trueValue;
    {
      auto TE = E->getThenExpr();
      FullExpr trueScope(SGF.Cleanups, CleanupLocation(TE));
      trueValue = visit(TE).forwardAsSingleValue(SGF, TE);
    }
    cond.exitTrue(SGF, trueValue);
    
    cond.enterFalse(SGF);
    SILValue falseValue;
    {
      auto EE = E->getElseExpr();
      FullExpr falseScope(SGF.Cleanups, CleanupLocation(EE));
      falseValue = visit(EE).forwardAsSingleValue(SGF, EE);
    }
    cond.exitFalse(SGF, falseValue);
    
    SILBasicBlock *cont = cond.complete(SGF);
    assert(cont && "no continuation block for if expr?!");
    
    SILValue result = cont->bbarg_begin()[0];
    
    return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(result));
  } else {
    // If the result is address-only, emit the result into a common stack buffer
    // that dominates both branches.
    SILValue resultAddr = SGF.getBufferForExprResult(
                                               E, lowering.getLoweredType(), C);
    
    Condition cond = SGF.emitCondition(E->getCondExpr(),
                                       /*hasFalse*/ true,
                                       /*invertCondition*/ false);
    cond.enterTrue(SGF);
    SGF.emitProfilerIncrement(E);
    {
      auto TE = E->getThenExpr();
      FullExpr trueScope(SGF.Cleanups, CleanupLocation(TE));
      KnownAddressInitialization init(resultAddr);
      SGF.emitExprInto(TE, &init);
    }
    cond.exitTrue(SGF);
    
    cond.enterFalse(SGF);
    {
      auto EE = E->getElseExpr();
      FullExpr trueScope(SGF.Cleanups, CleanupLocation(EE));
      KnownAddressInitialization init(resultAddr);
      SGF.emitExprInto(EE, &init);
    }
    cond.exitFalse(SGF);
    
    cond.complete(SGF);

    return RValue(SGF, E,
                  SGF.manageBufferForExprResult(resultAddr, lowering, C));
  }
}

RValue RValueEmitter::visitDefaultValueExpr(DefaultValueExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

/// Emit an optional-to-optional transformation.
ManagedValue
SILGenFunction::emitOptionalToOptional(SILLocation loc,
                                       ManagedValue input,
                                       SILType resultTy,
                                       const ValueTransform &transformValue) {
  auto contBB = createBasicBlock();
  auto isNotPresentBB = createBasicBlock();
  auto isPresentBB = createBasicBlock();

  // Create a temporary for the output optional.
  auto &resultTL = getTypeLowering(resultTy);

  // If the result is address-only, we need to return something in memory,
  // otherwise the result is the BBArgument in the merge point.
  SILValue result;
  if (resultTL.isAddressOnly())
    result = emitTemporaryAllocation(loc, resultTy);
  else
    result = new (F.getModule()) SILArgument(contBB, resultTL.getLoweredType());

  
  // Branch on whether the input is optional, this doesn't consume the value.
  auto isPresent = emitDoesOptionalHaveValue(loc, input.getValue());
  B.createCondBranch(loc, isPresent, isPresentBB, isNotPresentBB);

  // If it's present, apply the recursive transformation to the value.
  B.emitBlock(isPresentBB);
  SILValue branchArg;
  {
    // Don't allow cleanups to escape the conditional block.
    FullExpr presentScope(Cleanups, CleanupLocation::get(loc));

    CanType resultValueTy =
      resultTy.getSwiftRValueType().getAnyOptionalObjectType();
    assert(resultValueTy);
    SILType loweredResultValueTy = getLoweredType(resultValueTy);

    // Pull the value out.  This will load if the value is not address-only.
    auto &inputTL = getTypeLowering(input.getType());
    auto inputValue = emitUncheckedGetOptionalValueFrom(loc, input,
                                                        inputTL, SGFContext());

    // Transform it.
    auto resultValue = transformValue(*this, loc, inputValue,
                                      loweredResultValueTy);

    // Inject that into the result type if the result is address-only.
    if (resultTL.isAddressOnly()) {
      ArgumentSource resultValueRV(loc, RValue(resultValue, resultValueTy));
      emitInjectOptionalValueInto(loc, std::move(resultValueRV),
                                  result, resultTL);
    } else {
      resultValue = getOptionalSomeValue(loc, resultValue, resultTL);
      branchArg = resultValue.forward(*this);
    }
  }
  if (branchArg)
    B.createBranch(loc, contBB, branchArg);
  else
    B.createBranch(loc, contBB);

  // If it's not present, inject 'nothing' into the result.
  B.emitBlock(isNotPresentBB);
  if (resultTL.isAddressOnly()) {
    emitInjectOptionalNothingInto(loc, result, resultTL);
    B.createBranch(loc, contBB);
  } else {
    branchArg = getOptionalNoneValue(loc, resultTL);
    B.createBranch(loc, contBB, branchArg);
  }

  // Continue.
  B.emitBlock(contBB);
  if (resultTL.isAddressOnly())
    return emitManagedBufferWithCleanup(result, resultTL);

  return emitManagedRValueWithCleanup(result, resultTL);
}

RValue SILGenFunction::emitEmptyTupleRValue(SILLocation loc,
                                            SGFContext C) {
  return RValue(CanType(TupleType::getEmpty(F.getASTContext())));
}

namespace {
  /// A visitor for creating a flattened list of LValues from a
  /// tuple-of-lvalues expression.
  ///
  /// Note that we can have tuples down to arbitrary depths in the
  /// type, but every branch should lead to an l-value otherwise.
  class TupleLValueEmitter
      : public Lowering::ExprVisitor<TupleLValueEmitter> {
    SILGenFunction &SGF;

    AccessKind TheAccessKind;

    /// A flattened list of l-values.
    SmallVectorImpl<Optional<LValue>> &Results;
  public:
    TupleLValueEmitter(SILGenFunction &SGF, AccessKind accessKind,
                       SmallVectorImpl<Optional<LValue>> &results)
      : SGF(SGF), TheAccessKind(accessKind), Results(results) {}

    // If the destination is a tuple, recursively destructure.
    void visitTupleExpr(TupleExpr *E) {
      assert(E->getType()->is<TupleType>());
      assert(!E->getType()->isMaterializable());
      for (auto &elt : E->getElements()) {
        visit(elt);
      }
    }

    // If the destination is '_', queue up a discard.
    void visitDiscardAssignmentExpr(DiscardAssignmentExpr *E) {
      Results.push_back(None);
    }

    // Otherwise, queue up a scalar assignment to an lvalue.
    void visitExpr(Expr *E) {
      assert(E->getType()->is<LValueType>());
      Results.push_back(SGF.emitLValue(E, TheAccessKind));
    }
  };

  /// A visitor for consuming tuples of l-values.
  class TupleLValueAssigner
      : public CanTypeVisitor<TupleLValueAssigner, void, RValue &&> {
    SILGenFunction &SGF;
    SILLocation AssignLoc;
    MutableArrayRef<Optional<LValue>> DestLVQueue;

    Optional<LValue> &&getNextDest() {
      assert(!DestLVQueue.empty());
      Optional<LValue> &next = DestLVQueue.front();
      DestLVQueue = DestLVQueue.slice(1);
      return std::move(next);
    }

  public:
    TupleLValueAssigner(SILGenFunction &SGF, SILLocation assignLoc,
                        SmallVectorImpl<Optional<LValue>> &destLVs)
      : SGF(SGF), AssignLoc(assignLoc), DestLVQueue(destLVs) {}

    /// Top-level entrypoint.
    void emit(CanType destType, RValue &&src) {
      visitTupleType(cast<TupleType>(destType), std::move(src));
      assert(DestLVQueue.empty() && "didn't consume all l-values!");
    }

    // If the destination is a tuple, recursively destructure.
    void visitTupleType(CanTupleType destTupleType, RValue &&srcTuple) {
      // Break up the source r-value.
      SmallVector<RValue, 4> srcElts;
      std::move(srcTuple).extractElements(srcElts);

      // Consume source elements off the queue.
      unsigned eltIndex = 0;
      for (CanType destEltType : destTupleType.getElementTypes()) {
        visit(destEltType, std::move(srcElts[eltIndex++]));
      }
    }

    // Okay, otherwise we pull one destination off the queue.
    void visitType(CanType destType, RValue &&src) {
      assert(isa<LValueType>(destType));

      Optional<LValue> &&next = getNextDest();

      // If the destination is a discard, do nothing.
      if (!next.hasValue())
        return;

      // Otherwise, emit the scalar assignment.
      SGF.emitAssignToLValue(AssignLoc, std::move(src),
                             std::move(next.getValue()));
    }
  };
}

/// Emit a simple assignment, i.e.
///
///   dest = src
///
/// The destination operand can be an arbitrarily-structured tuple of
/// l-values.
static void emitSimpleAssignment(SILGenFunction &SGF, SILLocation loc,
                                 Expr *dest, Expr *src) {
  // Handle lvalue-to-lvalue assignments with a high-level copy_addr
  // instruction if possible.
  if (auto *srcLoad = dyn_cast<LoadExpr>(src)) {
    // Check that the two l-value expressions have the same type.
    // Compound l-values like (a,b) have tuple type, so this check
    // also prevents us from getting into that case.
    if (dest->getType()->isEqual(srcLoad->getSubExpr()->getType())) {
      assert(!dest->getType()->is<TupleType>());
      WritebackScope writeback(SGF);
      auto destLV = SGF.emitLValue(dest, AccessKind::Write);
      auto srcLV = SGF.emitLValue(srcLoad->getSubExpr(), AccessKind::Read);
      SGF.emitAssignLValueToLValue(loc, std::move(srcLV), std::move(destLV));
      return;
    }
  }

  // Handle tuple destinations by destructuring them if present.
  CanType destType = dest->getType()->getCanonicalType();
  assert(!destType->isMaterializable());

  // But avoid this in the common case.
  if (!isa<TupleType>(destType)) {
    // If we're assigning to a discard, just emit the operand as ignored.
    dest = dest->getSemanticsProvidingExpr();
    if (isa<DiscardAssignmentExpr>(dest)) {
      SGF.emitIgnoredExpr(src);
      return;
    }

    WritebackScope writeback(SGF);
    LValue destLV = SGF.emitLValue(dest, AccessKind::Write);
    RValue srcRV = SGF.emitRValue(src);
    SGF.emitAssignToLValue(loc, std::move(srcRV), std::move(destLV));
    return;
  }

  WritebackScope writeback(SGF);

  // Produce a flattened queue of LValues.
  SmallVector<Optional<LValue>, 4> destLVs;
  TupleLValueEmitter(SGF, AccessKind::Write, destLVs).visit(dest);

  // Emit the r-value.
  RValue srcRV = SGF.emitRValue(src);

  // Recurse on the type of the destination, pulling LValues as
  // needed from the queue we built up before.
  TupleLValueAssigner(SGF, loc, destLVs).emit(destType, std::move(srcRV));
}

RValue RValueEmitter::visitAssignExpr(AssignExpr *E, SGFContext C) {
  FullExpr scope(SGF.Cleanups, CleanupLocation(E));
  emitSimpleAssignment(SGF, E, E->getDest(), E->getSrc());
  return SGF.emitEmptyTupleRValue(E, C);
}

void SILGenFunction::emitBindOptional(SILLocation loc,
                                      ManagedValue optionalAddrOrValue,
                                      unsigned depth) {
  assert(depth < BindOptionalFailureDests.size());
  auto failureDest = BindOptionalFailureDests[BindOptionalFailureDests.size()
                                                - depth - 1];

  // Check whether the optional has a value.
  SILBasicBlock *hasValueBB = createBasicBlock();
  auto hasValue = emitDoesOptionalHaveValue(loc,optionalAddrOrValue.getValue());

  // If there is a cleanup for the optional value being tested, we can disable
  // it on the failure path.  We don't need to destroy it because we know that
  // on that path it is nil.
  if (optionalAddrOrValue.hasCleanup())
    Cleanups.setCleanupState(optionalAddrOrValue.getCleanup(),
                             CleanupState::Dormant);
    
  // If not, thread out through a bunch of cleanups.
  SILBasicBlock *hasNoValueBB = Cleanups.emitBlockForCleanups(failureDest, loc);
  B.createCondBranch(loc, hasValue, hasValueBB, hasNoValueBB);
  
  // If so, continue.
  B.emitBlock(hasValueBB);

  // Reenable the cleanup for the optional on the normal path.
  if (optionalAddrOrValue.hasCleanup())
    Cleanups.setCleanupState(optionalAddrOrValue.getCleanup(),
                             CleanupState::Active);
}

RValue RValueEmitter::visitBindOptionalExpr(BindOptionalExpr *E, SGFContext C) {
  // Create a temporary of type Optional<T> if it is address-only.
  auto &optTL = SGF.getTypeLowering(E->getSubExpr()->getType());
  
  ManagedValue optValue;
  if (optTL.isLoadable()) {
    optValue = SGF.emitRValueAsSingleValue(E->getSubExpr());
  } else {
    auto temp = SGF.emitTemporary(E, optTL);
    optValue = temp->getManagedAddress();

    // Emit the operand into the temporary.
    SGF.emitExprInto(E->getSubExpr(), temp.get());

  }

  // Check to see whether the optional is present, if not, jump to the current
  // nil handler block.
  SGF.emitBindOptional(E, optValue, E->getDepth());

  // If we continued, get the value out as the result of the expression.
  auto resultValue = SGF.emitUncheckedGetOptionalValueFrom(E, optValue,
                                                           optTL, C);
  return RValue(SGF, E, resultValue);
}

namespace {
  /// A RAII object to save and restore BindOptionalFailureDest.
  class RestoreOptionalFailureDest {
    SILGenFunction &SGF;
#ifndef NDEBUG
    unsigned Depth;
#endif
  public:
    RestoreOptionalFailureDest(SILGenFunction &SGF, JumpDest &&dest)
      : SGF(SGF)
#ifndef NDEBUG
      , Depth(SGF.BindOptionalFailureDests.size())
#endif
    {
      SGF.BindOptionalFailureDests.push_back(std::move(dest));
    }
    ~RestoreOptionalFailureDest() {
      assert(SGF.BindOptionalFailureDests.size() == Depth + 1);
      SGF.BindOptionalFailureDests.pop_back();
    }
  };
}

RValue RValueEmitter::visitOptionalEvaluationExpr(OptionalEvaluationExpr *E,
                                                  SGFContext C) {
  auto &optTL = SGF.getTypeLowering(E->getType());

  Initialization *optInit = C.getEmitInto();
  bool usingProvidedContext = optInit && optInit->isSingleBuffer();

  // Form the optional using address operations if the type is address-only or
  // if we already have an address to use.
  bool isByAddress = usingProvidedContext || optTL.isAddressOnly();
  
  std::unique_ptr<TemporaryInitialization> optTemp;
  if (!usingProvidedContext && isByAddress) {
    // Allocate the temporary for the Optional<T> if we didn't get one from the
    // context.  This needs to happen outside of the cleanups scope we're about
    // to push.
    optTemp = SGF.emitTemporary(E, optTL);
    optInit = optTemp.get();
  }

  // Enter a cleanups scope.
  FullExpr scope(SGF.Cleanups, E);

  SILBasicBlock *contBB = SGF.createBasicBlock();

  // Install a new optional-failure destination just outside of the
  // cleanups scope.
  SILBasicBlock *failureBB = SGF.createBasicBlock();
  RestoreOptionalFailureDest restoreFailureDest(SGF,
                    JumpDest(failureBB, SGF.Cleanups.getCleanupsDepth(), E));

  SILValue NormalArgument;
  if (isByAddress) {
    // Emit the operand into the temporary.
    SGF.emitExprInto(E->getSubExpr(), optInit);
  } else {
    NormalArgument = SGF.emitRValueAsSingleValue(E->getSubExpr()).forward(SGF);
  }

  // We fell out of the normal result, which generated a T? as either
  // a scalar in NormalArgument or directly into optInit.

  // This concludes the conditional scope.
  scope.pop();

  // Branch to the continuation block.
  if (NormalArgument)
    SGF.B.createBranch(E, contBB, NormalArgument);
  else
    SGF.B.createBranch(E, contBB);

  // If control branched to the failure block, inject .None into the
  // result type.
  SGF.B.emitBlock(failureBB);

  if (isByAddress) {
    SGF.emitInjectOptionalNothingInto(E, optInit->getAddress(), optTL);
    SGF.B.createBranch(E, contBB);
  } else {
    auto branchArg = SGF.getOptionalNoneValue(E, optTL);
    SGF.B.createBranch(E, contBB, branchArg);
  }

  // Emit the continuation block.
  SGF.B.emitBlock(contBB);

  // If we emitted into the provided context, we're done.
  if (usingProvidedContext)
    return RValue();

  // If this was done in SSA registers, then the value is provided as an
  // argument to the block.
  if (!isByAddress) {
    auto arg = new (SGF.SGM.M) SILArgument(contBB, optTL.getLoweredType());
    return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(arg, optTL));
  }
  
  assert(optTemp);
  auto result = optTemp->getManagedAddress();
  if (!optTL.isAddressOnly()) {
    auto optValue = SGF.B.createLoad(E, result.forward(SGF));
    result = SGF.emitManagedRValueWithCleanup(optValue, optTL);
  }

  return RValue(SGF, E, result);
}

RValue RValueEmitter::visitForceValueExpr(ForceValueExpr *E, SGFContext C) {
  return emitForceValue(E, E->getSubExpr(), 0, C);
}

/// Emit an expression in a forced context.
///
/// \param loc - the location that is causing the force
/// \param E - the forced expression
/// \param numOptionalEvaluations - the number of enclosing
///   OptionalEvaluationExprs that we've opened.
RValue RValueEmitter::emitForceValue(SILLocation loc, Expr *E,
                                     unsigned numOptionalEvaluations,
                                     SGFContext C) {
  auto valueType = E->getType()->getAnyOptionalObjectType();
  assert(valueType);
  E = E->getSemanticsProvidingExpr();

  // If the subexpression is a conditional checked cast, emit an unconditional
  // cast, which drastically simplifies the generated SIL for something like:
  //
  //   (x as? Foo)!
  if (auto checkedCast = dyn_cast<ConditionalCheckedCastExpr>(E)) {
    return emitUnconditionalCheckedCast(SGF, loc, checkedCast->getSubExpr(),
                                        valueType, checkedCast->getCastKind(),
                                        C);
  }

  // If the subexpression is a monadic optional operation, peephole
  // the emission of the operation.
  if (auto eval = dyn_cast<OptionalEvaluationExpr>(E)) {
    CleanupLocation cleanupLoc = CleanupLocation::get(loc);
    SILBasicBlock *failureBB;
    JumpDest failureDest(cleanupLoc);

    // Set up an optional-failure scope (which cannot actually return).
    // We can just borrow the enclosing one if we're in a nested context.
    if (numOptionalEvaluations) {
      failureBB = nullptr; // remember that we did this
      failureDest = SGF.BindOptionalFailureDests.back();
    } else {
      failureBB = SGF.createBasicBlock(FunctionSection::Postmatter);
      failureDest = JumpDest(failureBB, SGF.Cleanups.getCleanupsDepth(),
                             cleanupLoc);
    }
    RestoreOptionalFailureDest restoreFailureDest(SGF, std::move(failureDest));
    RValue result = emitForceValue(loc, eval->getSubExpr(),
                                   numOptionalEvaluations + 1, C);

    // Emit the failure destination, but only if actually used.
    if (failureBB) {
      if (failureBB->pred_empty()) {
        SGF.eraseBasicBlock(failureBB);
      } else {
        SILBuilder failureBuilder(failureBB);
        failureBuilder.setTrackingList(SGF.getBuilder().getTrackingList());
        auto boolTy = SILType::getBuiltinIntegerType(1, SGF.getASTContext());
        auto trueV = failureBuilder.createIntegerLiteral(loc, boolTy, 1);
        failureBuilder.createCondFail(loc, trueV);
        failureBuilder.createUnreachable(loc);
      }
    }

    return result;
  }

  // Handle injections.
  if (auto injection = dyn_cast<InjectIntoOptionalExpr>(E)) {
    auto subexpr = injection->getSubExpr()->getSemanticsProvidingExpr();

    // An injection of a bind is the idiom for a conversion between
    // optional types (e.g. ImplicitlyUnwrappedOptional<T> -> Optional<T>).
    // Handle it specially to avoid unnecessary control flow.
    if (auto bindOptional = dyn_cast<BindOptionalExpr>(subexpr)) {
      if (bindOptional->getDepth() < numOptionalEvaluations) {
        return emitForceValue(loc, bindOptional->getSubExpr(),
                              numOptionalEvaluations, C);
      }
    }

    // Otherwise, just emit the injected value directly into the result.
    return SGF.emitRValue(injection->getSubExpr(), C);
  }

  // Otherwise, emit the value into memory and use the optional intrinsic.
  const TypeLowering &optTL = SGF.getTypeLowering(E->getType());
  auto optTemp = SGF.emitTemporary(E, optTL);
  SGF.emitExprInto(E, optTemp.get());

  ManagedValue V =
    SGF.emitCheckedGetOptionalValueFrom(loc,
                                        optTemp->getManagedAddress(), optTL, C);
  return RValue(SGF, loc, valueType->getCanonicalType(), V);
}

void SILGenFunction::emitOpenExistentialImpl(
       OpenExistentialExpr *E,
       llvm::function_ref<void(Expr *)> emitSubExpr) {
  Optional<WritebackScope> writebackScope;

  // Emit the existential value.
  ManagedValue existentialValue;
  if (E->getExistentialValue()->getType()->is<LValueType>()) {
    // Create a writeback scope for the access to the existential lvalue.
    writebackScope.emplace(*this);

    existentialValue = emitAddressOfLValue(
                         E->getExistentialValue(),
                         emitLValue(E->getExistentialValue(),
                                    AccessKind::ReadWrite),
                         AccessKind::ReadWrite);
  } else {
    existentialValue = emitRValueAsSingleValue(
                         E->getExistentialValue(),
                         SGFContext::AllowGuaranteedPlusZero);
  }

  // Open the existential value into the opened archetype value.
  bool isUnique = E->getOpaqueValue()->isUniquelyReferenced();
  bool canConsume;
  SILValue archetypeValue;
  
  Type opaqueValueType = E->getOpaqueValue()->getType()->getRValueType();
  switch (existentialValue.getType()
            .getPreferredExistentialRepresentation(SGM.M)) {
  case ExistentialRepresentation::Opaque:
    assert(existentialValue.getValue().getType().isAddress());
    archetypeValue = B.createOpenExistentialAddr(
                       E, existentialValue.forward(*this),
                       getLoweredType(opaqueValueType));
    if (existentialValue.hasCleanup()) {
      canConsume = true;
      // Leave a cleanup to deinit the existential container.
      Cleanups.pushCleanup<TakeFromExistentialCleanup>(
                                                   existentialValue.getValue());
    } else {
      canConsume = false;
    }
    break;
  case ExistentialRepresentation::Metatype:
    assert(existentialValue.getValue().getType().isObject());
    archetypeValue = B.createOpenExistentialMetatype(
                       E, existentialValue.forward(*this),
                       getLoweredType(opaqueValueType));
    // Metatypes are always trivial. Consuming would be a no-op.
    canConsume = false;
    break;
  case ExistentialRepresentation::Class:
    assert(existentialValue.getValue().getType().isObject());
    archetypeValue = B.createOpenExistentialRef(
                       E, existentialValue.forward(*this),
                       getLoweredType(opaqueValueType));
    canConsume = existentialValue.hasCleanup();
    break;
  case ExistentialRepresentation::Boxed:
    assert(existentialValue.getValue().getType().isObject());
    // NB: Don't forward the cleanup, because consuming a boxed value won't
    // consume the box reference.
    archetypeValue = B.createOpenExistentialBox(
                       E, existentialValue.getValue(),
                       getLoweredType(opaqueValueType));
    // The boxed value can't be assumed to be uniquely referenced. We can never
    // consume it.
    // TODO: We could use isUniquelyReferenced to shorten the duration of
    // the box to the point that the opaque value is copied out.
    isUnique = false;
    canConsume = false;
    break;
  case ExistentialRepresentation::None:
    llvm_unreachable("not existential");
  }
  setArchetypeOpeningSite(CanArchetypeType(E->getOpenedArchetype()),
                          archetypeValue);
  
  // Register the opaque value for the projected existential.
  SILGenFunction::OpaqueValueRAII opaqueValueRAII(
                                    *this, E->getOpaqueValue(),
                                    archetypeValue,
                                    /*destroy=*/canConsume,
                                    /*uniquely referenced=*/isUnique);

  emitSubExpr(E->getSubExpr());
}

RValue RValueEmitter::visitOpenExistentialExpr(OpenExistentialExpr *E,
                                               SGFContext C) {
  return SGF.emitOpenExistential<RValue>(E,
                                         [&](Expr *subExpr) -> RValue {
                                           return visit(subExpr, C);
                                         });
}

RValue RValueEmitter::visitOpaqueValueExpr(OpaqueValueExpr *E, SGFContext C) {
  assert(SGF.OpaqueValues.count(E) && "Didn't bind OpaqueValueExpr");

  auto &entry = SGF.OpaqueValues[E];

  // If the context wants a +0 value, guaranteed or immediate, we can give it to
  // them, because OpenExistential emission guarantees the value.
  if (C.isGuaranteedPlusZeroOk()) {
    return RValue(SGF, E, ManagedValue::forUnmanaged(entry.value));
  }

  // If the opaque value is consumable, we can just return the
  // value with a cleanup. There is no need to retain it separately.
  if (entry.isConsumable) {
    assert(!entry.hasBeenConsumed
           && "Uniquely-referenced opaque value already consumed");
    entry.hasBeenConsumed = true;
    return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(entry.value));
  }

  // Otherwise, copy the value.
  return RValue(SGF, E,
                ManagedValue::forUnmanaged(entry.value).copyUnmanaged(SGF, E));
}

ProtocolDecl *SILGenFunction::getPointerProtocol() {
  if (SGM.PointerProtocol)
    return *SGM.PointerProtocol;
  
  SmallVector<ValueDecl*, 1> lookup;
  getASTContext().lookupInSwiftModule("_PointerType", lookup);
  // FIXME: Should check for protocol in Sema
  assert(lookup.size() == 1 && "no _PointerType protocol");
  assert(isa<ProtocolDecl>(lookup[0]) && "_PointerType is not a protocol");
  SGM.PointerProtocol = cast<ProtocolDecl>(lookup[0]);
  return cast<ProtocolDecl>(lookup[0]);
}

/// Produce a Substitution for a type that conforms to the standard library
/// _Pointer protocol.
Substitution SILGenFunction::getPointerSubstitution(Type pointerType,
                                                    ArchetypeType *archetype) {
  auto &Ctx = getASTContext();
  ProtocolDecl *pointerProto = getPointerProtocol();
  auto conformance
    = Ctx.getStdlibModule()->lookupConformance(pointerType, pointerProto,
                                               nullptr);
  assert(conformance.getInt() == ConformanceKind::Conforms
         && "not a _Pointer type");

  // FIXME: Cache this
  ProtocolConformance *conformances[] = {conformance.getPointer()};
  auto conformancesCopy = Ctx.AllocateCopy(conformances);
  
  return Substitution{archetype, pointerType, conformancesCopy};
}

namespace {
class AutoreleasingWritebackComponent : public LogicalPathComponent {
public:
  AutoreleasingWritebackComponent(LValueTypeData typeData)
    : LogicalPathComponent(typeData, AutoreleasingWritebackKind)
  {}
  
  std::unique_ptr<LogicalPathComponent>
  clone(SILGenFunction &gen, SILLocation l) const override {
    return std::unique_ptr<LogicalPathComponent>(
      new AutoreleasingWritebackComponent(getTypeData()));
  }

  AccessKind getBaseAccessKind(SILGenFunction &gen,
                               AccessKind kind) const override {
    return kind;
  }
  
  void set(SILGenFunction &gen, SILLocation loc,
           RValue &&value, ManagedValue base) && override {
    // Convert the value back to a +1 strong reference.
    auto unowned = std::move(value).getAsSingleValue(gen, loc).getUnmanagedValue();
    auto strongType = SILType::getPrimitiveObjectType(
              unowned.getType().castTo<UnmanagedStorageType>().getReferentType());
    auto owned = gen.B.createUnmanagedToRef(loc, unowned, strongType);
    gen.B.createRetainValue(loc, owned);
    auto ownedMV = gen.emitManagedRValueWithCleanup(owned);
    
    // Reassign the +1 storage with it.
    ownedMV.assignInto(gen, loc, base.getUnmanagedValue());
  }
  
  ManagedValue get(SILGenFunction &gen, SILLocation loc,
                   ManagedValue base, SGFContext c) && override {
    // Load the value at +0.
    SILValue owned = gen.B.createLoad(loc, base.getUnmanagedValue());
    // Convert it to unowned.
    auto unownedType = SILType::getPrimitiveObjectType(
            CanUnmanagedStorageType::get(owned.getType().getSwiftRValueType()));
    SILValue unowned = gen.B.createRefToUnmanaged(loc, owned, unownedType);
    
    return ManagedValue::forUnmanaged(unowned);
  }

  /// Compare 'this' lvalue and the 'rhs' lvalue (which is guaranteed to have
  /// the same dynamic PathComponent type as the receiver) to see if they are
  /// identical.  If so, there is a conflicting writeback happening, so emit a
  /// diagnostic.
  void diagnoseWritebackConflict(LogicalPathComponent *RHS,
                                 SILLocation loc1, SILLocation loc2,
                                 SILGenFunction &gen) override {
    //      auto &rhs = (GetterSetterComponent&)*RHS;
  }

  void print(raw_ostream &OS) const override {
    OS << "AutoreleasingWritebackComponent()\n";
  }
};
} // end anonymous namespace

RValue RValueEmitter::visitInOutToPointerExpr(InOutToPointerExpr *E,
                                              SGFContext C) {
  // If we're converting on the behalf of an
  // AutoreleasingUnsafeMutablePointer, convert the lvalue to
  // unowned(unsafe), so we can point at +0 storage.
  PointerTypeKind pointerKind;
  Type elt = E->getType()->getAnyPointerElementType(pointerKind);
  assert(elt && "not a pointer");
  (void)elt;

  AccessKind accessKind =
    (pointerKind == PTK_UnsafePointer
       ? AccessKind::Read : AccessKind::ReadWrite);

  // Get the original lvalue.
  LValue lv = SGF.emitLValue(cast<InOutExpr>(E->getSubExpr())->getSubExpr(),
                             accessKind);

  auto ptr = SGF.emitLValueToPointer(E, std::move(lv),
                                     E->getType()->getCanonicalType(),
                                     pointerKind, accessKind);
  return RValue(SGF, E, ptr);
}

/// Convert an l-value to a pointer type: unsafe, unsafe-mutable, or
/// autoreleasing-unsafe-mutable.
ManagedValue SILGenFunction::emitLValueToPointer(SILLocation loc,
                                                 LValue &&lv,
                                                 CanType pointerType,
                                                 PointerTypeKind pointerKind,
                                                 AccessKind accessKind) {
  switch (pointerKind) {
  case PTK_UnsafeMutablePointer:
  case PTK_UnsafePointer:
    // +1 is fine.
    break;

  case PTK_AutoreleasingUnsafeMutablePointer: {
    // Set up a writeback through a +0 buffer.
    LValueTypeData typeData = lv.getTypeData();
    SILType rvalueType = SILType::getPrimitiveObjectType(
      CanUnmanagedStorageType::get(typeData.TypeOfRValue.getSwiftRValueType()));

    LValueTypeData unownedTypeData(
      AbstractionPattern(
        CanUnmanagedStorageType::get(typeData.OrigFormalType.getType())),
      CanUnmanagedStorageType::get(typeData.SubstFormalType),
      rvalueType);
    lv.add<AutoreleasingWritebackComponent>(unownedTypeData);
    break;
  }
  }

  // Get the lvalue address as a raw pointer.
  SILValue address =
    emitAddressOfLValue(loc, std::move(lv), accessKind).getUnmanagedValue();
  address = B.createAddressToPointer(loc, address,
                               SILType::getRawPointerType(getASTContext()));
  
  // Disable nested writeback scopes for any calls evaluated during the
  // conversion intrinsic.
  InOutConversionScope scope(*this);
  
  // Invoke the conversion intrinsic.
  FuncDecl *converter =
    getASTContext().getConvertInOutToPointerArgument(nullptr);
  Substitution sub = getPointerSubstitution(pointerType,
                          converter->getGenericParams()->getAllArchetypes()[0]);
  return emitApplyOfLibraryIntrinsic(loc, converter, sub,
                                     ManagedValue::forUnmanaged(address),
                                     SGFContext());
}
RValue RValueEmitter::visitArrayToPointerExpr(ArrayToPointerExpr *E,
                                              SGFContext C) {
  WritebackScope writeback(SGF);

  auto &Ctx = SGF.getASTContext();
  FuncDecl *converter;
  ManagedValue orig;

  // Convert the array mutably if it's being passed inout.
  auto subExpr = E->getSubExpr();
  if (subExpr->getType()->is<InOutType>()) {
    converter = Ctx.getConvertMutableArrayToPointerArgument(nullptr);
    orig = SGF.emitAddressOfLValue(subExpr,
                               SGF.emitLValue(subExpr, AccessKind::ReadWrite),
                                   AccessKind::ReadWrite);
  } else {
    converter = Ctx.getConvertConstArrayToPointerArgument(nullptr);
    orig = SGF.emitRValueAsSingleValue(subExpr);
  }
  auto converterArchetypes = converter->getGenericParams()->getAllArchetypes();

  // Invoke the conversion intrinsic, which will produce an owner-pointer pair.
  Substitution subs[2] = {
    Substitution{
      converterArchetypes[0],
      subExpr->getType()->getInOutObjectType()
        ->castTo<BoundGenericType>()
        ->getGenericArgs()[0],
      {}
    },
    SGF.getPointerSubstitution(E->getType(),
                               converterArchetypes[1]),
  };
  auto result = SGF.emitApplyOfLibraryIntrinsic(E, converter, subs, orig, C);
  
  // Lifetime-extend the owner, and pass on the pointer.
  auto owner = SGF.B.createTupleExtract(E, result.forward(SGF), 0);
  SGF.emitManagedRValueWithCleanup(owner);
  auto pointer = SGF.B.createTupleExtract(E, result.getValue(), 1);
  return RValue(SGF, E, ManagedValue::forUnmanaged(pointer));
}
RValue RValueEmitter::visitStringToPointerExpr(StringToPointerExpr *E,
                                               SGFContext C) {
  auto &Ctx = SGF.getASTContext();
  FuncDecl *converter = Ctx.getConvertConstStringToUTF8PointerArgument(nullptr);
  auto converterArchetypes = converter->getGenericParams()->getAllArchetypes();

  // Get the original value.
  ManagedValue orig = SGF.emitRValueAsSingleValue(E->getSubExpr());
  
  // Invoke the conversion intrinsic, which will produce an owner-pointer pair.
  Substitution sub =
    SGF.getPointerSubstitution(E->getType(),
                               converterArchetypes[0]);
  auto result = SGF.emitApplyOfLibraryIntrinsic(E, converter, sub, orig, C);
  
  // Lifetime-extend the owner, and pass on the pointer.
  auto owner = SGF.B.createTupleExtract(E, result.forward(SGF), 0);
  SGF.emitManagedRValueWithCleanup(owner);
  auto pointer = SGF.B.createTupleExtract(E, result.getValue(), 1);
  return RValue(SGF, E, ManagedValue::forUnmanaged(pointer));
}
RValue RValueEmitter::visitPointerToPointerExpr(PointerToPointerExpr *E,
                                                SGFContext C) {
  auto &Ctx = SGF.getASTContext();
  auto converter = Ctx.getConvertPointerToPointerArgument(nullptr);
  auto converterArchetypes = converter->getGenericParams()->getAllArchetypes();

  // Get the original pointer value, abstracted to the converter function's
  // expected level.
  AbstractionPattern origTy(converter->getType()->castTo<AnyFunctionType>()
                                                ->getInput());
  auto &origTL = SGF.getTypeLowering(origTy, E->getSubExpr()->getType());
  ManagedValue orig = SGF.emitRValueAsOrig(E->getSubExpr(), origTy, origTL);
  // The generic function currently always requires indirection, but pointers
  // are always loadable.
  auto origBuf = SGF.emitTemporaryAllocation(E, orig.getType());
  SGF.B.createStore(E, orig.forward(SGF), origBuf);
  orig = SGF.emitManagedBufferWithCleanup(origBuf);
  
  // Invoke the conversion intrinsic to convert to the destination type.
  Substitution subs[2] = {
    SGF.getPointerSubstitution(E->getSubExpr()->getType(), converterArchetypes[0]),
    SGF.getPointerSubstitution(E->getType(), converterArchetypes[1]),
  };
  
  auto result = SGF.emitApplyOfLibraryIntrinsic(E, converter, subs, orig, C);
  return RValue(SGF, E, result);
}

RValue RValueEmitter::visitForeignObjectConversionExpr(
         ForeignObjectConversionExpr *E,
         SGFContext C) {
  // Get the original value.
  ManagedValue orig = SGF.emitRValueAsSingleValue(E->getSubExpr());
  ManagedValue result(SGF.B.createUncheckedRefCast(
                        E, orig.getValue(),
                        SGF.getLoweredType(E->getType())),
                      orig.getCleanup());
  return RValue(SGF, E, E->getType()->getCanonicalType(), result);
}

/// Emit a check that returns 1 if the running OS version is in
/// the specified version range and 0 otherwise. The returned SILValue
/// (which has type Builtin.Int1) represents the result of this check.
static SILValue emitOSVersionRangeCheck(SILLocation loc,
                                        const VersionRange &range,
                                        SILGenFunction &SGF, SGFContext C) {
  // Emit constants for the checked version range.
  clang::VersionTuple Vers = range.getLowerEndpoint();
  unsigned major = Vers.getMajor();
  unsigned minor =
      (Vers.getMinor().hasValue() ? Vers.getMinor().getValue() : 0);
  unsigned subminor =
      (Vers.getSubminor().hasValue() ? Vers.getSubminor().getValue() : 0);

  SILType wordType = SILType::getBuiltinWordType(SGF.getASTContext());

  SILValue majorValue = SGF.B.createIntegerLiteral(loc, wordType, major);
  SILValue minorValue = SGF.B.createIntegerLiteral(loc, wordType, minor);
  SILValue subminorValue = SGF.B.createIntegerLiteral(loc, wordType, subminor);

  // Emit call to _stdlib_isOSVersionAtLeast(major, minor, patch)
  FuncDecl *versionQueryDecl =
      SGF.getASTContext().getIsOSVersionAtLeastDecl(nullptr);
  assert(versionQueryDecl);

  auto silDeclRef = SILDeclRef(versionQueryDecl);
  SILValue availabilityGTEFn = SGF.emitGlobalFunctionRef(
      loc, silDeclRef, SGF.getConstantInfo(silDeclRef));

  SILValue args[] = {majorValue, minorValue, subminorValue};

  SILValue queryResult = SGF.B.createApply(loc, availabilityGTEFn, args);

  return queryResult;
}

RValue RValueEmitter::visitAvailabilityQueryExpr(AvailabilityQueryExpr *E,
                                                 SGFContext C) {
  // Check the running OS version to determine whether it is in the range
  // specified by E.
  SILValue inRange = emitOSVersionRangeCheck(E, E->getAvailableRange(), SGF, C);

  // Convert Builtin.Int1 result into Bool with the _getBool library intrinsic.
  ASTContext &ctx = SGF.SGM.M.getASTContext();
  auto result =
      SGF.emitApplyOfLibraryIntrinsic(E, ctx.getGetBoolDecl(nullptr), {},
                                      ManagedValue::forUnmanaged(inRange), C);
  return RValue(SGF, E, result);
}

RValue
RValueEmitter::visitUnavailableToOptionalExpr(UnavailableToOptionalExpr *E,
                                              SGFContext C) {
  // Emit construction of an optional value for E's declaration reference.
  // The value will be .None if the underlying declaration reference is
  // unavailable and .Some(rvalue) if the declaration is available.

  // E must have an optional type.
  assert(E->getType().getPointer()->getOptionalObjectType().getPointer());

  Expr *unavailExpr = E->getSubExpr();

  SILType silOptType = SGF.getLoweredType(E->getType());
  SILLocation loc(E);

  SILValue allocatedOptional = SGF.emitTemporaryAllocation(loc, silOptType);

  // Emit the check for availability
  SILValue isAvailable;
  const UnavailabilityReason &Reason = E->getReason();
  switch (Reason.getReasonKind()) {
  case UnavailabilityReason::Kind::RequiresOSVersionRange:
    isAvailable = emitOSVersionRangeCheck(
        loc, Reason.getRequiredOSVersionRange(), SGF, C);
    break;

  case UnavailabilityReason::Kind::ExplicitlyWeakLinked:
    // We don't handle explicit weak linking yet.
    // In the future, we will do so by converting the global variable
    // lvalue to an address and comparing to 0.
    llvm_unreachable("Unimplemented optional for weakly-linked global");
  }

  Condition cond = SGF.emitCondition(isAvailable, loc);
  cond.enterTrue(SGF);
  {
    ArgumentSource source;
    if (E->getSubExpr()->getType()->getAs<LValueType>()) {
      // If the unavailable expression is an lvalue, we will load it.
      auto lval = SGF.emitLValue(unavailExpr, AccessKind::Read);
      ManagedValue loadedValue = SGF.emitLoadOfLValue(loc, std::move(lval), C);
      source = ArgumentSource(
          loc, RValue(SGF, loc, lval.getSubstFormalType(), loadedValue));
    } else {
      source = ArgumentSource(unavailExpr);
    }

    SGF.emitInjectOptionalValueInto(loc, std::move(source), allocatedOptional,
                                    SGF.getTypeLowering(silOptType));
  }
  cond.exitTrue(SGF);

  cond.enterFalse(SGF);
  {
    // If the declaration is not available, inject .None.
    SGF.emitInjectOptionalNothingInto(loc, allocatedOptional,
                                      SGF.getTypeLowering(silOptType));
  }
  cond.exitFalse(SGF);
  cond.complete(SGF);

  ManagedValue managedValue = SGF.emitLoad(
      loc, allocatedOptional, SGF.getTypeLowering(silOptType), C, IsNotTake);

  return RValue(SGF, E, managedValue);
}

RValue SILGenFunction::emitRValue(Expr *E, SGFContext C) {
  assert(E->getType()->isMaterializable() &&
         "l-values must be emitted with emitLValue");
  return RValueEmitter(*this).visit(E, C);
}

// Evaluate the expression as an lvalue or rvalue, discarding the result.
void SILGenFunction::emitIgnoredExpr(Expr *E) {
  // If this is a tuple expression, recursively ignore its elements.
  // This may let us recursively avoid work.
  if (auto *TE = dyn_cast<TupleExpr>(E)) {
    for (auto *elt : TE->getElements())
      emitIgnoredExpr(elt);
    return;
  }
  
  // TODO: Could look through arbitrary implicit conversions that don't have
  // side effects, or through tuple shuffles, by emitting ignored default
  // arguments.
  
  FullExpr scope(Cleanups, CleanupLocation(E));
  if (!E->getType()->isMaterializable()) {
    // Emit the l-value, but don't perform an access.
    emitLValue(E, AccessKind::Read);
    return;
  }

  // If this is a load expression, we try hard not to actually do the load
  // (which could materialize a potentially expensive value with cleanups).
  if (auto *LE = dyn_cast<LoadExpr>(E)) {
    LValue lv = emitLValue(LE->getSubExpr(), AccessKind::Read);
    // If the lvalue is purely physical, then it won't have any side effects,
    // and we don't need to drill into it.
    if (lv.isPhysical())
      return;

    // If the last component is physical, then we just need to drill through
    // side effects in the lvalue, but don't need to perform the final load.
    if (lv.isLastComponentPhysical()) {
      emitAddressOfLValue(E, std::move(lv), AccessKind::Read);
      return;
    }

    // Otherwise, we must call the ultimate getter to get its potential side
    // effect.
    emitLoadOfLValue(E, std::move(lv), SGFContext::AllowImmediatePlusZero);
    return;
  }

  // Otherwise, emit the result (to get any side effects), but produce it at +0
  // if that allows simplification.
  emitRValue(E, SGFContext::AllowImmediatePlusZero);
}

/// Emit the given expression as an r-value, then (if it is a tuple), combine
/// it together into a single ManagedValue.
ManagedValue SILGenFunction::emitRValueAsSingleValue(Expr *E, SGFContext C) {
  RValue &&rv = emitRValue(E, C);
  if (rv.isUsed()) return ManagedValue::forInContext();
  return std::move(rv).getAsSingleValue(*this, E);
}

static ManagedValue emitUndef(SILGenFunction &gen, SILLocation loc,
                              const TypeLowering &undefTL) {
  SILValue undef = SILUndef::get(undefTL.getLoweredType(), gen.SGM.M);
  return gen.emitManagedRValueWithCleanup(undef, undefTL);
}

ManagedValue SILGenFunction::emitUndef(SILLocation loc, Type type) {
  return ::emitUndef(*this, loc, getTypeLowering(type));
}

ManagedValue SILGenFunction::emitUndef(SILLocation loc, SILType type) {
  return ::emitUndef(*this, loc, getTypeLowering(type));
}
