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
#include "swift/AST/Types.h"
#include "swift/AST/TypeRefinementContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Unicode.h"
#include "swift/Basic/type_traits.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "ExitableFullExpr.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace Lowering;

SILGenFunction::OpaqueValueRAII::~OpaqueValueRAII() {
  // Destroy the value, unless it was both uniquely referenced and consumed.
  auto entry = Self.OpaqueValues.find(OpaqueValue);
  if (Destroy && 
      (!OpaqueValue->isUniquelyReferenced() || !entry->second.second)) {
    SILValue &value = entry->second.first;
    auto &lowering = Self.getTypeLowering(value.getType().getSwiftRValueType());
    if (lowering.isTrivial()) {
      // Nothing to do.
    } else if (lowering.isAddressOnly()) {
      Self.B.emitDestroyAddr(OpaqueValue, value);
    } else {
      lowering.emitDestroyRValue(Self.B, OpaqueValue, value);
    }
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
    RValue visitIsaExpr(IsaExpr *E, SGFContext C);
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

SILValue SILGenFunction::emitGlobalFunctionRef(SILLocation loc,
                                               SILDeclRef constant,
                                               SILConstantInfo constantInfo) {
  assert(constantInfo == getConstantInfo(constant));

  assert(!LocalFunctions.count(constant) &&
         "emitting ref to local constant without context?!");
  // Builtins must be fully applied at the point of reference.
  if (constant.hasDecl() &&
      isa<BuiltinUnit>(constant.getDecl()->getDeclContext())) {
    SGM.diagnose(loc.getSourceLoc(), diag::not_implemented,
                 "delayed application of builtin");
    return SILUndef::get(constantInfo.getSILType(), SGM.M);
  }
  
  // If the constant is a curry thunk we haven't emitted yet, emit it.
  if (constant.isCurried) {
    if (!SGM.hasFunction(constant)) {
      // Non-functions can't be referenced uncurried.
      FuncDecl *fd = cast<FuncDecl>(constant.getDecl());
      
      // Getters and setters can't be referenced uncurried.
      assert(!fd->isAccessor());
      
      // FIXME: Thunks for instance methods of generics.
      assert(!(fd->isInstanceMember() && isa<ProtocolDecl>(fd->getDeclContext()))
             && "currying generic method not yet supported");

      // FIXME: Curry thunks for generic methods don't work right yet, so skip
      // emitting thunks for them
      assert(!(fd->getType()->is<AnyFunctionType>() &&
               fd->getType()->castTo<AnyFunctionType>()->getResult()
                 ->is<PolymorphicFunctionType>()));
      
      // Reference the next uncurrying level of the function.
      SILDeclRef next = SILDeclRef(fd, SILDeclRef::Kind::Func,
                                 SILDeclRef::ConstructAtBestResilienceExpansion,
                                   constant.uncurryLevel + 1);
      // If the function is fully uncurried and natively foreign, reference its
      // foreign entry point.
      if (!next.isCurried && fd->hasClangNode())
        next = next.asForeign();
      
      SGM.emitCurryThunk(constant, next, fd);
    }
  }
  // Otherwise, if this is a foreign thunk we haven't emitted yet, emit it.
  else if (constant.isForeignThunk()) {
    if (!SGM.hasFunction(constant))
      SGM.emitForeignThunk(constant);
  }
  
  return B.createFunctionRef(loc, SGM.getFunction(constant, NotForDefinition));
}

SILFunction *SILGenModule::getDynamicThunk(SILDeclRef constant,
                                           SILConstantInfo constantInfo) {
  // Mangle the constant with a _TTD header.
  llvm::SmallString<32> name;
  constant.mangle(name, "_TTD");
  
  auto F = M.getOrCreateFunction(constant.getDecl(), name, SILLinkage::Shared,
                            constantInfo.getSILType().castTo<SILFunctionType>(),
                            IsBare, IsTransparent,
                            makeModuleFragile ? IsFragile : IsNotFragile);

  if (F->empty()) {
    // Emit the thunk if we haven't yet.
    // Currently a dynamic thunk looks just like a foreign-to-native thunk around
    // an ObjC method. This would change if we introduced a native
    // runtime-hookable mechanism.
    SILGenFunction SGF(*this, *F);
    SGF.emitForeignThunk(constant);
  }

  return F;
}

static constexpr unsigned OTKPair(OptionalTypeKind a, OptionalTypeKind b) {
  return unsigned(a) << 8 | unsigned(b);
}

SILFunction *
SILGenModule::emitVTableMethod(SILDeclRef derived, SILDeclRef base) {
  // As a fast path, if there is no override, definitely no thunk is necessary.
  if (!M.getOptions().EnableVTableThunks ||
      derived == base)
    return getFunction(derived, NotForDefinition);
  
  auto origDerivedTy = getConstantType(derived).castTo<SILFunctionType>();
  auto baseTy = getConstantType(base).castTo<SILFunctionType>();

  bool needsThunk = false;
  
  // Introduce optionality into the derived signature to match the base.
  // TODO: Handle other forms of reabstraction.
  SmallVector<SILParameterInfo, 4> vtableParams;
  SILResultInfo vtableResult;
  SmallVector<VTableParamThunk, 4> paramActions;
  VTableResultThunk resultAction;

  // The derived result may be less optional than the base.
  {
    SILType vtableResultTy;
    OptionalTypeKind baseOTK, derivedOTK;
    auto baseObj = baseTy->getSemanticResultSILType().getSwiftRValueType()
      ->getAnyOptionalObjectType(baseOTK);
    origDerivedTy->getSemanticResultSILType().getSwiftRValueType()
      ->getAnyOptionalObjectType(derivedOTK);
    
    switch (OTKPair(baseOTK, derivedOTK)) {
    case OTKPair(OTK_ImplicitlyUnwrappedOptional, OTK_None):
    case OTKPair(OTK_Optional, OTK_None):
      // Optionalize the return value.
      // This only requires a thunk if the underlying type isn't an object
      // reference.
      vtableResultTy = baseTy->getSemanticResultSILType();
      resultAction = VTableResultThunk::MakeOptional;
      needsThunk |= !baseObj->isBridgeableObjectType();
      break;
      
    case OTKPair(OTK_None, OTK_None):
    case OTKPair(OTK_Optional, OTK_Optional):
    case OTKPair(OTK_Optional, OTK_ImplicitlyUnwrappedOptional):
    case OTKPair(OTK_ImplicitlyUnwrappedOptional, OTK_Optional):
    case OTKPair(OTK_ImplicitlyUnwrappedOptional,
                 OTK_ImplicitlyUnwrappedOptional):
      // The return value doesn't need to change.
      vtableResultTy = origDerivedTy->getSemanticResultSILType();
      resultAction = VTableResultThunk::None;
      break;
      
    default:
      llvm_unreachable("derived return can't be more optional than base");
    }
    
    assert(origDerivedTy->hasIndirectResult() == baseTy->hasIndirectResult()
           && "return type reabstraction for override not implemented");
    if (origDerivedTy->hasIndirectResult()) {
      vtableParams.push_back(
                           SILParameterInfo(vtableResultTy.getSwiftRValueType(),
                                            ParameterConvention::Indirect_Out));
      vtableResult = SILResultInfo(TupleType::getEmpty(getASTContext()),
                                   ResultConvention::Unowned);
    } else {
      vtableResult = SILResultInfo(vtableResultTy.getSwiftRValueType(),
                                   origDerivedTy->getResult().getConvention());
    }
  }

  // The parameters may be either more optional than the base, or if the base
  // is IUO, may force off optionality.
  auto baseParams = baseTy->getParametersWithoutIndirectResult();
  auto derivedParams = origDerivedTy->getParametersWithoutIndirectResult();
  assert(baseParams.size() == derivedParams.size()
         && "explosion reabstraction for override not implemented");
  for (unsigned i : indices(baseParams)) {
    OptionalTypeKind baseOTK, derivedOTK;
    baseParams[i].getSILType().getSwiftRValueType()
      ->getAnyOptionalObjectType(baseOTK);
    auto derivedObj = derivedParams[i].getSILType().getSwiftRValueType()
      ->getAnyOptionalObjectType(derivedOTK);
    
    VTableParamThunk paramAction;
    
    switch (OTKPair(baseOTK, derivedOTK)) {
    case OTKPair(OTK_None, OTK_Optional):
    case OTKPair(OTK_None, OTK_ImplicitlyUnwrappedOptional):
      // De-optionalize the parameter.
      // This only requires a thunk if the underlying type isn't an object
      // reference.
      vtableParams.push_back(baseParams[i]);
      paramAction = VTableParamThunk::MakeOptional;
      needsThunk |= !derivedObj->isBridgeableObjectType();
      break;
      
    case OTKPair(OTK_ImplicitlyUnwrappedOptional, OTK_None):
      // Optionalize the parameter. We'll force it in the thunk.
      vtableParams.push_back(baseParams[i]);
      paramAction = VTableParamThunk::ForceIUO;
      needsThunk = true;
      break;
      
    case OTKPair(OTK_None, OTK_None):
    case OTKPair(OTK_Optional, OTK_Optional):
    case OTKPair(OTK_Optional, OTK_ImplicitlyUnwrappedOptional):
    case OTKPair(OTK_ImplicitlyUnwrappedOptional, OTK_Optional):
    case OTKPair(OTK_ImplicitlyUnwrappedOptional,
                 OTK_ImplicitlyUnwrappedOptional):
      // The parameter doesn't need to change.
      vtableParams.push_back(derivedParams[i]);
      paramAction = VTableParamThunk::None;
      break;
      
    default:
      llvm_unreachable("derived param can't be less optional than base");
    }
    
    paramActions.push_back(paramAction);
  }
  
  // If no thunk action was necessary, just emit the method as is.
  if (!needsThunk)
    return getFunction(derived, NotForDefinition);
  
  // Mangle the constant with a _TTV header.
  // TODO: If we allocated a new vtable slot for the derived method, then
  // further derived methods would potentially need multiple thunks, and we
  // would need to mangle the base method into the symbol as well.
  llvm::SmallString<32> name;
  derived.mangle(name, "_TTV");
  
  assert(!M.lookUpFunction(name)
         && "vtable thunk already exists");
  
  auto vtableDerivedTy = SILFunctionType::get(
                                          origDerivedTy->getGenericSignature(),
                                          origDerivedTy->getExtInfo(),
                                          origDerivedTy->getCalleeConvention(),
                                          vtableParams, vtableResult,
                                          getASTContext());
  SILLocation loc(derived.getDecl());
  auto thunk = SILFunction::create(M, SILLinkage::Private, name, vtableDerivedTy,
                         cast<FuncDecl>(derived.getDecl())->getGenericParams(),
                         loc, IsBare, IsNotTransparent, IsNotFragile);
  thunk->setDebugScope(new (M) SILDebugScope(loc, *thunk));
  
  SILGenFunction(*this, *thunk)
    .emitVTableThunk(derived, base, paramActions, resultAction);
  
  return thunk;
}

SILValue SILGenFunction::emitDynamicMethodRef(SILLocation loc,
                                              SILDeclRef constant,
                                              SILConstantInfo constantInfo) {
  // If the method is foreign, its foreign thunk will handle the dynamic
  // dispatch for us.
  if (constant.isForeignThunk()) {
    if (!SGM.hasFunction(constant))
      SGM.emitForeignThunk(constant);
    return B.createFunctionRef(loc, SGM.getFunction(constant, NotForDefinition));
  }
  
  // Otherwise, we need a dynamic dispatch thunk.
  SILFunction *F = SGM.getDynamicThunk(constant, constantInfo);
  
  return B.createFunctionRef(loc, F);
}

SILValue SILGenFunction::emitUnmanagedFunctionRef(SILLocation loc,
                                               SILDeclRef constant) {
  // If this is a reference to a local constant, grab it.
  if (LocalFunctions.count(constant)) {
    return LocalFunctions[constant];
  }
  
  // Otherwise, use a global FunctionRefInst.
  return emitGlobalFunctionRef(loc, constant);
}

ManagedValue SILGenFunction::emitFunctionRef(SILLocation loc,
                                             SILDeclRef constant) {
  return emitFunctionRef(loc, constant, getConstantInfo(constant));
}

ManagedValue SILGenFunction::emitFunctionRef(SILLocation loc,
                                             SILDeclRef constant,
                                             SILConstantInfo constantInfo) {
  // If this is a reference to a local constant, grab it.
  if (LocalFunctions.count(constant)) {
    SILValue v = LocalFunctions[constant];
    return emitManagedRetain(loc, v);
  }
  
  // Otherwise, use a global FunctionRefInst.
  SILValue c = emitGlobalFunctionRef(loc, constant, constantInfo);
  return ManagedValue::forUnmanaged(c);
}

/// True if the global stored property requires lazy initialization.
static bool isGlobalLazilyInitialized(VarDecl *var) {
  assert(!var->getDeclContext()->isLocalContext() &&
         "not a global variable!");
  assert(var->hasStorage() &&
         "not a stored global variable!");

  // Imports from C are never lazily initialized.
  if (var->hasClangNode())
    return false;
  
  // Top-level global variables in the main source file and in the REPL are not
  // lazily initialized.
  auto sourceFileContext = dyn_cast<SourceFile>(var->getDeclContext());
  if (!sourceFileContext)
    return true;
  
  return !sourceFileContext->isScriptMode();
}

static ManagedValue emitGlobalVariableRef(SILGenFunction &gen,
                                          SILLocation loc, VarDecl *var) {
  if (isGlobalLazilyInitialized(var)) {
    // Call the global accessor to get the variable's address.
    SILFunction *accessorFn = gen.SGM.getFunction(
                            SILDeclRef(var, SILDeclRef::Kind::GlobalAccessor),
                                                  NotForDefinition);
    SILValue accessor = gen.B.createFunctionRef(loc, accessorFn);
    auto accessorTy = accessor.getType().castTo<SILFunctionType>();
    (void)accessorTy;
    assert(!accessorTy->isPolymorphic()
           && "generic global variable accessors not yet implemented");
    SILValue addr = gen.B.createApply(loc, accessor, accessor.getType(),
                              accessor.getType().castTo<SILFunctionType>()
                                      ->getResult().getSILType(),
                              {}, {});
    // FIXME: It'd be nice if the result of the accessor was natively an
    // address.
    addr = gen.B.createPointerToAddress(loc, addr,
                          gen.getLoweredType(var->getType()).getAddressType());
    return ManagedValue::forLValue(addr);
  }
  
  // Global variables can be accessed directly with global_addr.  Emit this
  // instruction into the prolog of the function so we can memoize/CSE it in
  // VarLocs.
  auto &entryBB = gen.getFunction().getBlocks().front();
  SILBuilder B(&entryBB, entryBB.begin());
  B.setTrackingList(gen.getBuilder().getTrackingList());

  auto *silG = gen.SGM.getSILGlobalVariable(var, NotForDefinition);
  SILValue addr = B.createGlobalAddr(var, silG);

  gen.VarLocs[var] = SILGenFunction::VarLoc::get(addr);
  return ManagedValue::forLValue(addr);
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
    return emitGlobalVariableRef(*this, loc, var);

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
      
      return emitLoad(loc, Result.getLValueAddress(), getTypeLowering(refType),
                      C, takes);
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
      return C.isPlusZeroOk() ? Result : Result.copyUnmanaged(*this, loc);
    }

    assert(var->hasAccessorFunctions() && "Unknown rvalue case");

    bool isDirectAccessorUse = (semantics == AccessSemantics::DirectToAccessor);
    SILDeclRef getter = getGetterDeclRef(var, isDirectAccessorUse);

    RValueSource selfSource;
    
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
      selfSource = RValueSource(loc, std::move(metatypeRV));
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
                                          FunctionType::Representation::Thick);

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

static AbstractionPattern getOrigFormalRValueType(Type formalStorageType) {
  auto type =
    formalStorageType->getReferenceStorageReferent()->getCanonicalType();
  return AbstractionPattern(type);
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
                       RValueSource &&baseRV, RValue &&subscriptRV,
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
    SGF.B.emitStrongRelease(loc, addressorResult.second.forward(SGF));
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
    RValueSource baseRV = prepareAccessorBaseArg(loc, base, accessor);

    AbstractionPattern origFormalType =
      getOrigFormalRValueType(field->getType());
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
    LValue LV = emitDirectIVarLValue(loc, base, field, AccessKind::Read);
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
    getOrigFormalRValueType(field->getType());
  bool hasAbstractionChange = false;
  if (origFormalType.getAsType() != substFormalType) {
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

    } else if (hasAbstractionChange || !C.isPlusZeroOk()) {
      // If we have an abstraction change or if we have to produce a result at
      // +1, then emit a RetainValue.
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
static SILValue getAddressForInPlaceInitialization(Initialization *I) {
  if (!I) return SILValue();
  
  SILValue address;
  switch (I->kind) {
  case Initialization::Kind::LetValue:
    // Emit into the buffer that 'let's produce for address-only values if
    // we have it.
    if (I->hasAddress())
      address = I->getAddress();
    break;
  case Initialization::Kind::Translating:
  case Initialization::Kind::Ignored:
    break;
    
  case Initialization::Kind::Tuple:
    // FIXME: For a single-element tuple, we could emit into the single field.
    
    // The tuple initialization isn't contiguous, so we can't emit directly
    // into it.
    break;
    
  case Initialization::Kind::SingleBuffer:
    // Emit into the buffer.
    address = I->getAddress();
    break;
  }
  return address;
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

/// Emit a collection downcast expression.
///
/// \param conditional Whether to emit a conditional downcast; if
/// false, this will emit a forced downcast.
static RValue emitCollectionDowncastExpr(SILGenFunction &SGF,
                                         Expr *source,
                                         SILLocation loc,
                                         Type destType,
                                         SGFContext C,
                                         bool conditional,
                                         bool bridgesFromObjC) {
  // Get the sub expression argument as a managed value
  auto mv = SGF.emitRValueAsSingleValue(source);
  
  // Compute substitutions for the intrinsic call.
  auto fromCollection = cast<BoundGenericStructType>(
                          source->getType()->getCanonicalType());
  auto toCollection = cast<BoundGenericStructType>(
                        destType->getCanonicalType());
  // Get the intrinsic function.
  auto &ctx = SGF.getASTContext();
  FuncDecl *fn = nullptr;
  if (fromCollection->getDecl() == ctx.getArrayDecl()) {
    fn = conditional ? ctx.getArrayConditionalCast(nullptr)
                          : ctx.getArrayForceCast(nullptr);
  } else if (fromCollection->getDecl() == ctx.getDictionaryDecl()) {
    fn = bridgesFromObjC
           ? (conditional 
                ? ctx.getDictionaryBridgeFromObjectiveCConditional(nullptr)
                : ctx.getDictionaryBridgeFromObjectiveC(nullptr))
           : (conditional 
                ? ctx.getDictionaryDownCastConditional(nullptr)
                : ctx.getDictionaryDownCast(nullptr));
  } else if (fromCollection->getDecl() == ctx.getSetDecl()) {
    fn = bridgesFromObjC
           ? (conditional 
                ? ctx.getSetBridgeFromObjectiveCConditional(nullptr)
                : ctx.getSetBridgeFromObjectiveC(nullptr))
           : (conditional 
                ? ctx.getSetDownCastConditional(nullptr)
                : ctx.getSetDownCast(nullptr));
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
  
  Type resultType = destType;
  if (conditional)
    resultType = OptionalType::get(resultType);
  return RValue(SGF, loc, resultType->getCanonicalType(), emitApply);
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

static void buildFuncToBlockInvokeBody(SILGenFunction &gen,
                                       SILLocation loc,
                                       CanSILFunctionType blockTy,
                                       CanSILBlockStorageType blockStorageTy,
                                       CanSILFunctionType funcTy) {
  Scope scope(gen.Cleanups, CleanupLocation::getCleanupLocation(loc));
  SILBasicBlock *entry = gen.F.begin();
  
  // Get the captured native function value out of the block.
  auto storageAddrTy = SILType::getPrimitiveAddressType(blockStorageTy);
  auto storage = new (gen.SGM.M) SILArgument(entry, storageAddrTy);
  auto capture = gen.B.createProjectBlockStorage(loc, storage);
  auto &funcTL = gen.getTypeLowering(funcTy);
  auto fn = gen.emitLoad(loc, capture, funcTL, SGFContext(), IsNotTake);
  
  // Collect the block arguments, which may have nonstandard conventions.
  assert(blockTy->getParameters().size()
         == funcTy->getParameters().size()
         && "block and function types don't match");
  
  SmallVector<ManagedValue, 4> args;
  for (unsigned i : indices(funcTy->getParameters())) {
    auto &funcParam = funcTy->getParameters()[i];
    auto &param = blockTy->getParameters()[i];
    SILValue v = new (gen.SGM.M) SILArgument(entry, param.getSILType());
    
    ManagedValue mv;
    switch (param.getConvention()) {
    case ParameterConvention::Direct_Owned:
      // Consume owned parameters at +1.
      mv = gen.emitManagedRValueWithCleanup(v);
      break;
        
    case ParameterConvention::Direct_Guaranteed:
    case ParameterConvention::Direct_Unowned:
      // We need to independently retain the value.
      mv = gen.emitManagedRetain(loc, v);
      break;

    case ParameterConvention::Indirect_In_Guaranteed:
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_Out:
      llvm_unreachable("indirect arguments to blocks not supported");
    }
    
    args.push_back(gen.emitBridgedToNativeValue(loc, mv, AbstractCC::C,
                                                funcParam.getType()));
  }
  
  // Call the native function.
  assert(!funcTy->hasIndirectResult()
         && "block thunking func with indirect result not supported");
  ManagedValue result = gen.emitMonomorphicApply(loc, fn, args,
                         funcTy->getSILResult().getSwiftRValueType());
  
  // Bridge the result back to ObjC.
  result = gen.emitNativeToBridgedValue(loc, result, AbstractCC::C,
                        result.getType().getSwiftRValueType(),
                        result.getType().getSwiftRValueType(),
                        blockTy->getSILResult().getSwiftRValueType());
  
  auto resultVal = result.forward(gen);
  scope.pop();
  
  // Handle the result convention.
  switch (blockTy->getResult().getConvention()) {
  case ResultConvention::UnownedInnerPointer:
  case ResultConvention::Unowned:
    assert(gen.getTypeLowering(resultVal.getType()).isTrivial()
           && "nontrivial result is returned unowned?!");
    gen.B.createReturn(loc, resultVal);
    break;
  case ResultConvention::Autoreleased:
    gen.B.createAutoreleaseReturn(loc, resultVal);
    break;
  case ResultConvention::Owned:
    gen.B.createReturn(loc, resultVal);
    break;
  }
}

/// Bridge a native function to a block with a thunk.
static ManagedValue emitFuncToBlock(SILGenFunction &gen,
                                    SILLocation loc,
                                    ManagedValue fn,
                                    CanSILFunctionType blockTy) {
  // Build the invoke function signature. The block will capture the original
  // function value.
  auto fnTy = fn.getType().castTo<SILFunctionType>();
  auto storageTy = SILBlockStorageType::get(fnTy);
  
  // Build the invoke function type.
  SmallVector<SILParameterInfo, 4> params;
  params.push_back(
              SILParameterInfo(storageTy, ParameterConvention::Indirect_Inout));
  std::copy(blockTy->getParameters().begin(),
            blockTy->getParameters().end(),
            std::back_inserter(params));
  
  auto invokeTy =
    SILFunctionType::get(nullptr,
                     FunctionType::ExtInfo()
                       .withCallingConv(AbstractCC::C)
                       .withRepresentation(FunctionType::Representation::Thin),
                     ParameterConvention::Direct_Unowned,
                     params,
                     blockTy->getResult(),
                     gen.getASTContext());
  
  // Create the invoke function. Borrow the mangling scheme from reabstraction
  // thunks, which is what we are in spirit.
  auto thunk = gen.SGM.getOrCreateReabstractionThunk(loc,
                                                     nullptr,
                                                     invokeTy,
                                                     fnTy,
                                                     blockTy,
                                                     gen.F.isFragile());
  
  // Build it if necessary.
  if (thunk->empty()) {
    SILGenFunction thunkSGF(gen.SGM, *thunk);
    buildFuncToBlockInvokeBody(thunkSGF, loc, blockTy, storageTy, fnTy);
  }
  
  // Form the block on the stack.
  auto storageAddrTy = SILType::getPrimitiveAddressType(storageTy);
  auto storage = gen.emitTemporaryAllocation(loc, storageAddrTy);
  auto capture = gen.B.createProjectBlockStorage(loc, storage);
  // Store the function to the block without claiming it, so that it still
  // gets cleaned up in scope. Copying the block will create an independent
  // reference.
  gen.B.createStore(loc, fn.getValue(), capture);
  auto invokeFn = gen.B.createFunctionRef(loc, thunk);
  auto stackBlock = gen.B.createInitBlockStorageHeader(loc, storage, invokeFn,
                                      SILType::getPrimitiveObjectType(blockTy));
  
  // Copy the block so we have an independent heap object we can hand off.
  auto heapBlock = gen.B.createCopyBlock(loc, stackBlock);
  return gen.emitManagedRValueWithCleanup(heapBlock);
}

static void buildBlockToFuncThunkBody(SILGenFunction &gen,
                                      SILLocation loc,
                                      CanSILFunctionType blockTy,
                                      CanSILFunctionType funcTy) {
  // Collect the native arguments, which should all be +1.
  Scope scope(gen.Cleanups, CleanupLocation::getCleanupLocation(loc));
  
  assert(blockTy->getParameters().size()
           == funcTy->getParameters().size()
         && "block and function types don't match");
  
  SmallVector<ManagedValue, 4> args;
  SILBasicBlock *entry = gen.F.begin();
  for (unsigned i : indices(funcTy->getParameters())) {
    auto &param = funcTy->getParameters()[i];
    auto &blockParam = blockTy->getParameters()[i];
    
    auto &tl = gen.getTypeLowering(param.getSILType());
    assert((tl.isTrivial()
              ? param.getConvention() == ParameterConvention::Direct_Unowned
              : param.getConvention() == ParameterConvention::Direct_Owned)
           && "nonstandard conventions for native functions not implemented");
    SILValue v = new (gen.SGM.M) SILArgument(entry, param.getSILType());
    auto mv = gen.emitManagedRValueWithCleanup(v, tl);
    args.push_back(gen.emitNativeToBridgedValue(loc, mv, AbstractCC::C,
                                        param.getType(), param.getType(),
                                        blockParam.getType()));
  }
  
  // Add the block argument.
  SILValue blockV
    = new (gen.SGM.M) SILArgument(entry,
                                  SILType::getPrimitiveObjectType(blockTy));
  ManagedValue block = gen.emitManagedRValueWithCleanup(blockV);
  
  // Call the block.
  assert(!funcTy->hasIndirectResult()
         && "block thunking func with indirect result not supported");
  ManagedValue result = gen.emitMonomorphicApply(loc, block, args,
                         funcTy->getSILResult().getSwiftRValueType(),
                         /*transparent*/ false,
                         /*override CC*/ AbstractCC::C);
  
  // Return the result at +1.
  auto &resultTL = gen.getTypeLowering(funcTy->getSILResult());
  auto convention = funcTy->getResult().getConvention();
  assert((resultTL.isTrivial()
           ? convention == ResultConvention::Unowned
           : convention == ResultConvention::Owned)
         && "nonstandard conventions for return not implemented");
  (void)convention;
  (void)resultTL;
  
  auto r = result.forward(gen);
  scope.pop();
  gen.B.createReturn(loc, r);
}

/// Bridge a native function to a block with a thunk.
ManagedValue
SILGenFunction::emitBlockToFunc(SILLocation loc,
                                ManagedValue block,
                                CanSILFunctionType funcTy) {
  CanSILFunctionType substFnTy;
  SmallVector<Substitution, 4> subs;
  
  // Declare the thunk.
  auto blockTy = block.getType().castTo<SILFunctionType>();
  auto thunkTy = buildThunkType(block, funcTy, substFnTy, subs);
  auto thunk = SGM.getOrCreateReabstractionThunk(loc,
                                                 F.getContextGenericParams(),
                                                 thunkTy,
                                                 blockTy,
                                                 funcTy,
                                                 F.isFragile());
  
  // Build it if necessary.
  if (thunk->empty()) {
    SILGenFunction thunkSGF(SGM, *thunk);
    buildBlockToFuncThunkBody(thunkSGF, loc, blockTy, funcTy);
  }
  
  // Create it in the current function.
  auto thunkValue = B.createFunctionRef(loc, thunk);
  auto thunkedFn = B.createPartialApply(loc, thunkValue,
                                    SILType::getPrimitiveObjectType(substFnTy),
                                    subs, block.forward(*this),
                                    SILType::getPrimitiveObjectType(funcTy));
  return emitManagedRValueWithCleanup(thunkedFn);
}

RValue RValueEmitter::visitFunctionConversionExpr(FunctionConversionExpr *e,
                                                  SGFContext C)
{
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
  if (origRep != destTy->getRepresentation())
    destTy = adjustFunctionType(destTy, origRep);

  SILType resultType = SGF.getLoweredType(destTy);
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
    auto resultFTy = resultType.castTo<SILFunctionType>();
    // The only currently possible representation changes are block -> thick and
    // thick -> block.
    switch (destRepTy->getRepresentation()) {
    case AnyFunctionType::Representation::Block:
      switch (resultFTy->getRepresentation()) {
      case AnyFunctionType::Representation::Thin: {
        // Make thick first.
        auto v = SGF.B.createThinToThickFunction(e, result.getValue(),
          SILType::getPrimitiveObjectType(
           adjustFunctionType(resultFTy, FunctionType::Representation::Thick)));
        result = ManagedValue(v, result.getCleanup());
        SWIFT_FALLTHROUGH;
      }
      case AnyFunctionType::Representation::Thick:
        // Convert to a block.
        result = emitFuncToBlock(SGF, e, result,
                       SGF.getLoweredType(destRepTy).castTo<SILFunctionType>());
        break;
      case AnyFunctionType::Representation::Block:
        llvm_unreachable("should not try block-to-block repr change");
      }
      break;
    case AnyFunctionType::Representation::Thick: {
      assert(resultFTy->getRepresentation()
               == FunctionType::Representation::Block
             && "only block-to-thick repr changes supported");
      result = SGF.emitBlockToFunc(e, result,
                       SGF.getLoweredType(destRepTy).castTo<SILFunctionType>());
      break;
    }
    case AnyFunctionType::Representation::Thin:
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

namespace {
  /// An Initialization representing the concrete value buffer inside an
  /// existential container.
  class ExistentialValueInitialization : public SingleBufferInitialization {
    SILValue valueAddr;
  public:
    ExistentialValueInitialization(SILValue valueAddr)
      : valueAddr(valueAddr)
    {}
    
    SILValue getAddressOrNull() const override {
      return valueAddr;
    }
    
    void finishInitialization(SILGenFunction &gen) {
      // FIXME: Disable the DeinitExistential cleanup and enable the
      // DestroyAddr cleanup for the existential container.
    }
  };
}

static RValue emitClassBoundedErasure(SILGenFunction &gen, ErasureExpr *E) {
  ManagedValue sub = gen.emitRValueAsSingleValue(E->getSubExpr());
  SILType resultTy = gen.getLoweredLoadableType(E->getType());
  
  SILValue v;
  
  Type subType = E->getSubExpr()->getType();
  
  if (subType->isExistentialType()) {
    // If the source value is already of protocol type, open the value so we can
    // take it.
    subType = ArchetypeType::getOpened(E->getSubExpr()->getType());
    auto openedTy = gen.getLoweredLoadableType(subType);
    SILValue openedVal
      = gen.B.createOpenExistentialRef(E, sub.forward(gen), openedTy);
    sub = gen.emitManagedRValueWithCleanup(openedVal);
  }
  
  // Create a new existential container value around the class
  // instance.
  v = gen.B.createInitExistentialRef(E, resultTy,
                               subType->getCanonicalType(),
                               sub.getValue(), E->getConformances());

  return RValue(gen, E, ManagedValue(v, sub.getCleanup()));
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
    // If the source value is already of a protocol type, open the existential
    // container so we can steal its value.
    // TODO: Have a way to represent this operation in-place. The supertype
    // should be able to fit in the memory of the subtype existential.
    ManagedValue subExistential = gen.emitRValueAsSingleValue(E->getSubExpr());
    Type subFormalTy = ArchetypeType::getOpened(subType);
    SILType subLoweredTy = gen.getLoweredType(subFormalTy);
    bool isTake = subExistential.hasCleanup();
    SILValue subPayload;
    if (subExistential.getValue().getType().isAddress()) {
      subPayload = gen.B.createOpenExistential(E, subExistential.forward(gen),
                                               subLoweredTy);
    } else {
      subPayload = gen.B.createOpenExistentialRef(E,subExistential.forward(gen),
                                                  subLoweredTy);
    }
    ManagedValue subMV = isTake
      ? gen.emitManagedRValueWithCleanup(subPayload)
      : ManagedValue::forUnmanaged(subPayload);
    
    // Set up the destination existential, and forward the payload into it.
    SILValue destAddr = gen.B.createInitExistential(E, existential,
                                                subFormalTy->getCanonicalType(),
                                                subLoweredTy,
                                                E->getConformances());
    
    subMV.forwardInto(gen, E, destAddr);
  } else {
    // Otherwise, we need to initialize a new existential container from
    // scratch.
    
    // Allocate the concrete value inside the container.
    auto concreteFormalType = E->getSubExpr()->getType()->getCanonicalType();
    
    auto archetype = ArchetypeType::getOpened(E->getType());
    AbstractionPattern abstractionPattern(archetype);
    auto &concreteTL = gen.getTypeLowering(abstractionPattern,
                                           concreteFormalType);
    
    SILValue valueAddr = gen.B.createInitExistential(E, existential,
                                concreteFormalType,
                                concreteTL.getLoweredType(),
                                E->getConformances());
    // Initialize the concrete value in-place.
    InitializationPtr init(new ExistentialValueInitialization(valueAddr));
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

RValue RValueEmitter::visitErasureExpr(ErasureExpr *E, SGFContext C) {
  if (E->getType()->isClassExistentialType())
    return emitClassBoundedErasure(SGF, E);
  return emitAddressOnlyErasure(SGF, E, C);
}

/// Given an existential type or metatype, produce the type that
/// results from opening the underlying existential type.
static CanType getOpenedTypeForExistential(CanType type) {
  assert(type.isAnyExistentialType());
  if (auto metatype = dyn_cast<ExistentialMetatypeType>(type)) {
    auto instance = getOpenedTypeForExistential(metatype.getInstanceType());
    return CanMetatypeType::get(instance);
  }
  return CanType(ArchetypeType::getOpened(type));
}

static SILType
getOpenedTypeForLoweredExistentialMetatype(SILType type) {
  auto metatype = type.castTo<ExistentialMetatypeType>();
  auto instanceType = getOpenedTypeForExistential(metatype.getInstanceType());
  auto resultType =
    CanMetatypeType::get(instanceType, metatype->getRepresentation());
  return SILType::getPrimitiveObjectType(resultType);
}

static SILValue emitOpenExistentialMetatype(SILGenFunction &SGF,
                                            SILLocation loc,
                                            SILValue metatype) {
  SILType resultType =
    getOpenedTypeForLoweredExistentialMetatype(metatype.getType());
  return SGF.B.createOpenExistentialMetatype(loc, metatype, resultType);
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

namespace {
  class CleanupUsedExistentialContainer : public Cleanup {
    SILValue existential;
  public:
    CleanupUsedExistentialContainer(SILValue existential)
      : existential(existential) {}
    
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      gen.B.createDeinitExistential(l, existential);
    }
  };

  class CheckedCastEmitter {
    SILGenFunction &SGF;
    SILLocation Loc;
    CanType SourceType;
    CanType TargetType;

    enum class CastStrategy : uint8_t {
      Address,
      Scalar,
    };
    CastStrategy Strategy;

  public:
    CheckedCastEmitter(SILGenFunction &SGF, SILLocation loc,
                       Type sourceType, Type targetType)
      : SGF(SGF), Loc(loc), SourceType(sourceType->getCanonicalType()),
        TargetType(targetType->getCanonicalType()),
        Strategy(getStrategy()) {
    }

    bool isOperandIndirect() const {
      return Strategy == CastStrategy::Address;
    }

    ManagedValue emitOperand(Expr *operand) {
      AbstractionPattern mostGeneral = SGF.SGM.Types.getMostGeneralAbstraction();
      auto &origSourceTL = SGF.getTypeLowering(mostGeneral, SourceType);

      SGFContext ctx;

      std::unique_ptr<TemporaryInitialization> temporary;
      if (isOperandIndirect()) {
        temporary = SGF.emitTemporary(Loc, origSourceTL);
        ctx = SGFContext(temporary.get());
      }

      auto result = SGF.emitRValueAsOrig(operand, mostGeneral,
                                         origSourceTL, ctx);

      if (isOperandIndirect()) {
        // Force the result into the temporary if it's not already there.
        if (!result.isInContext()) {
          result.forwardInto(SGF, Loc, temporary->getAddress());
          temporary->finishInitialization(SGF);
        }
        return temporary->getManagedAddress();
      }

      return result;
    }

    RValue emitUnconditionalCast(ManagedValue operand, SGFContext ctx) {
      // The cast functions don't know how to work with anything but
      // the most general possible abstraction level.
      AbstractionPattern abstraction = SGF.SGM.Types.getMostGeneralAbstraction();
      auto &origTargetTL = SGF.getTypeLowering(abstraction, TargetType);
      auto &substTargetTL = SGF.getTypeLowering(TargetType);
      bool hasAbstraction =
        (origTargetTL.getLoweredType() != substTargetTL.getLoweredType());

      // If we're using checked_cast_addr, take the operand (which
      // should be an address) and build into the destination buffer.
      ManagedValue abstractResult;
      if (Strategy == CastStrategy::Address) {
        SILValue resultBuffer =
          createAbstractResultBuffer(hasAbstraction, origTargetTL, ctx);
        SGF.B.createUnconditionalCheckedCastAddr(Loc,
                                             CastConsumptionKind::TakeAlways,
                                             operand.forward(SGF), SourceType,
                                             resultBuffer, TargetType);
        return RValue(SGF, Loc, TargetType,
                      finishFromResultBuffer(hasAbstraction, resultBuffer,
                                             abstraction, origTargetTL, ctx));
      }

      SILValue resultScalar =
        SGF.B.createUnconditionalCheckedCast(Loc, operand.forward(SGF),
                                             origTargetTL.getLoweredType());
      return RValue(SGF, Loc, TargetType,
                    finishFromResultScalar(hasAbstraction, resultScalar,
                                           CastConsumptionKind::TakeAlways,
                                           abstraction, origTargetTL, ctx));
    }

    /// Emit a conditional cast.
    void emitConditional(ManagedValue operand, CastConsumptionKind consumption,
                         SGFContext ctx,
                         const std::function<void(ManagedValue)> &handleTrue,
                         const std::function<void()> &handleFalse) {
      // The cast instructions don't know how to work with anything
      // but the most general possible abstraction level.
      AbstractionPattern abstraction = SGF.SGM.Types.getMostGeneralAbstraction();
      auto &origTargetTL = SGF.getTypeLowering(abstraction, TargetType);
      auto &substTargetTL = SGF.getTypeLowering(TargetType);
      bool hasAbstraction =
        (origTargetTL.getLoweredType() != substTargetTL.getLoweredType());

      SILBasicBlock *falseBB = SGF.B.splitBlockForFallthrough();
      SILBasicBlock *trueBB = SGF.B.splitBlockForFallthrough();

      // Emit the branch.
      SILValue scalarOperandValue;
      SILValue resultBuffer;
      if (Strategy == CastStrategy::Address) {
        assert(operand.getType().isAddress());
        resultBuffer =
          createAbstractResultBuffer(hasAbstraction, origTargetTL, ctx);
        SGF.B.createCheckedCastAddrBranch(Loc, consumption,
                                          operand.forward(SGF), SourceType,
                                          resultBuffer, TargetType,
                                          trueBB, falseBB);
      } else {
        // Tolerate being passed an address here.  It comes up during switch
        //emission.
        scalarOperandValue = operand.forward(SGF);
        if (scalarOperandValue.getType().isAddress()) {
          scalarOperandValue = SGF.B.createLoad(Loc, scalarOperandValue);
        }
        SGF.B.createCheckedCastBranch(Loc, /*exact*/ false, scalarOperandValue,
                                      origTargetTL.getLoweredType(),
                                      trueBB, falseBB);
      }

      // Emit the success block.
      SGF.B.setInsertionPoint(trueBB); {
        FullExpr scope(SGF.Cleanups, CleanupLocation::getCleanupLocation(Loc));

        ManagedValue result;
        if (Strategy == CastStrategy::Address) {
          result = finishFromResultBuffer(hasAbstraction, resultBuffer,
                                          abstraction, origTargetTL, ctx);
        } else {
          SILValue argument = new (SGF.F.getModule())
            SILArgument(trueBB, origTargetTL.getLoweredType());
          result = finishFromResultScalar(hasAbstraction, argument, consumption,
                                          abstraction, origTargetTL, ctx);
        }

        handleTrue(result);
        assert(!SGF.B.hasValidInsertionPoint() && "handler did not end block");
      }

      // Emit the failure block.
      SGF.B.setInsertionPoint(falseBB); {
        FullExpr scope(SGF.Cleanups, CleanupLocation::getCleanupLocation(Loc));

        // If we're using the scalar strategy, handle the consumption rules.
        if (Strategy != CastStrategy::Address &&
            shouldDestroyOnFailure(consumption)) {
          SGF.B.emitReleaseValueOperation(Loc, scalarOperandValue);
        }

        handleFalse();
        assert(!SGF.B.hasValidInsertionPoint() && "handler did not end block");
      }
    }

    SILValue createAbstractResultBuffer(bool hasAbstraction,
                                        const TypeLowering &origTargetTL,
                                        SGFContext ctx) {
      if (!hasAbstraction) {
        if (auto emitInto = ctx.getEmitInto()) {
          if (SILValue addr = emitInto->getAddressOrNull()) {
            return addr;
          }
        }
      }

      return SGF.emitTemporaryAllocation(Loc, origTargetTL.getLoweredType());
    }

    ManagedValue finishFromResultBuffer(bool hasAbstraction,
                                        SILValue buffer,
                                        AbstractionPattern abstraction,
                                        const TypeLowering &origTargetTL,
                                        SGFContext ctx) {
      if (!hasAbstraction) {
        if (auto emitInto = ctx.getEmitInto()) {
          if (emitInto->getAddressOrNull()) {
            emitInto->finishInitialization(SGF);
            return ManagedValue::forInContext();
          }
        }
      }

      ManagedValue result;
      if (!origTargetTL.isAddressOnly()) {
        result = SGF.emitLoad(Loc, buffer, origTargetTL, ctx, IsTake);
      } else {
        result = SGF.emitManagedBufferWithCleanup(buffer, origTargetTL);
      }
      
      if (hasAbstraction) {
        result = SGF.emitOrigToSubstValue(Loc, result, abstraction,
                                          TargetType, ctx);
      }
      return result;
    }

    /// Our cast succeeded and gave us this abstracted value.
    ManagedValue finishFromResultScalar(bool hasAbstraction, SILValue value,
                                        CastConsumptionKind consumption,
                                        AbstractionPattern abstraction,
                                        const TypeLowering &origTargetTL,
                                        SGFContext ctx) {
      // Retain the result if this is copy-on-success.
      if (!shouldTakeOnSuccess(consumption))
        origTargetTL.emitRetainValue(SGF.B, Loc, value);

      // Enter a cleanup for the +1 result.
      ManagedValue result
        = SGF.emitManagedRValueWithCleanup(value, origTargetTL);

      // Re-abstract if necessary.
      if (hasAbstraction) {
        result = SGF.emitOrigToSubstValue(Loc, result, abstraction,
                                          TargetType, ctx);
      }
      return result;
    }

  private:
    CastStrategy getStrategy() const {
      if (canUseScalarCheckedCastInstructions(SGF.SGM.M, SourceType, TargetType))
        return CastStrategy::Scalar;
      return CastStrategy::Address;
    }
  };
}

static RValue emitUnconditionalCheckedCast(SILGenFunction &SGF,
                                           SILLocation loc,
                                           Expr *operand,
                                           Type targetType,
                                           CheckedCastKind castKind,
                                           SGFContext C) {
  // Handle collection downcasts directly; they have specific library
  // entry points.
  if (castKind == CheckedCastKind::ArrayDowncast ||
      castKind == CheckedCastKind::DictionaryDowncast ||
      castKind == CheckedCastKind::DictionaryDowncastBridged ||
      castKind == CheckedCastKind::SetDowncast ||
      castKind == CheckedCastKind::SetDowncastBridged) {
    bool bridgesFromObjC
      = (castKind == CheckedCastKind::DictionaryDowncastBridged ||
         castKind == CheckedCastKind::SetDowncastBridged);
    return emitCollectionDowncastExpr(SGF, operand, loc, targetType, C,
                                      /*conditional=*/false,
                                      bridgesFromObjC);
  }

  CheckedCastEmitter emitter(SGF, loc, operand->getType(),
                             targetType);
  ManagedValue operandValue = emitter.emitOperand(operand);
  return emitter.emitUnconditionalCast(operandValue, C);
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

void SILGenFunction::emitCheckedCastBranch(SILLocation loc, Expr *source,
                                           Type targetType,
                                           SGFContext ctx,
                                std::function<void(ManagedValue)> handleTrue,
                                           std::function<void()> handleFalse) {
  CheckedCastEmitter emitter(*this, loc, source->getType(), targetType);
  ManagedValue operand = emitter.emitOperand(source);
  emitter.emitConditional(operand, CastConsumptionKind::TakeAlways, ctx,
                          handleTrue, handleFalse);
}

void SILGenFunction::emitCheckedCastBranch(SILLocation loc,
                                           ConsumableManagedValue src,
                                           CanType sourceType,
                                           CanType targetType,
                                           SGFContext ctx,
                                std::function<void(ManagedValue)> handleTrue,
                                           std::function<void()> handleFalse) {
  CheckedCastEmitter emitter(*this, loc, sourceType, targetType);
  emitter.emitConditional(src.getFinalManagedValue(), src.getFinalConsumption(),
                          ctx, handleTrue, handleFalse);
}

RValue RValueEmitter::visitForcedCheckedCastExpr(ForcedCheckedCastExpr *E,
                                                 SGFContext C) {
  return emitUnconditionalCheckedCast(SGF, E, E->getSubExpr(), E->getType(),
                                      E->getCastKind(), C);
}

RValue RValueEmitter::
visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *E,
                                SGFContext C) {
  // Handle collection downcasts directly; they have specific library
  // entry points.
  auto castKind = E->getCastKind();
  if (castKind == CheckedCastKind::ArrayDowncast ||
      castKind == CheckedCastKind::DictionaryDowncast ||
      castKind == CheckedCastKind::DictionaryDowncastBridged ||
      castKind == CheckedCastKind::SetDowncast ||
      castKind == CheckedCastKind::SetDowncastBridged) {
    bool bridgesFromObjC
      = (castKind == CheckedCastKind::DictionaryDowncastBridged ||
         castKind == CheckedCastKind::SetDowncastBridged);
    return emitCollectionDowncastExpr(SGF, E->getSubExpr(), SILLocation(E), 
                                      E->getCastTypeLoc().getType(), C,
                                      /*conditional=*/true,
                                      bridgesFromObjC);
  }

  // Drill into the result type.
  OptionalTypeKind optKind;
  CanType resultObjectType =
    E->getType()->getCanonicalType().getAnyOptionalObjectType(optKind);
  assert(resultObjectType);
  auto someDecl = SGF.getASTContext().getOptionalSomeDecl(optKind);

  auto &resultTL = SGF.getTypeLowering(E->getType());

  // Optional<T> currently fully abstracts its object.
  auto abstraction = SGF.SGM.Types.getMostGeneralAbstraction();
  auto &abstractResultObjectTL =
    SGF.getTypeLowering(abstraction, resultObjectType);
  auto &resultObjectTL = SGF.getTypeLowering(resultObjectType);
  bool hasAbstraction =
    (resultObjectTL.getLoweredType() != abstractResultObjectTL.getLoweredType());

  // Set up a result buffer if desirable/required.
  SILValue resultBuffer;
  SILValue resultObjectBuffer;
  Optional<TemporaryInitialization> resultObjectTemp;
  SGFContext resultObjectCtx;
  if (resultTL.isAddressOnly() ||
      (!hasAbstraction && C.getEmitInto() &&
       C.getEmitInto()->getAddressOrNull())) {
    resultBuffer = SGF.getBufferForExprResult(E, resultTL.getLoweredType(), C);
    resultObjectBuffer = SGF.B.createInitEnumDataAddr(E, resultBuffer, someDecl,
                     abstractResultObjectTL.getLoweredType().getAddressType());
    resultObjectTemp.emplace(resultObjectBuffer, CleanupHandle::invalid());

    if (!hasAbstraction)
      resultObjectCtx = SGFContext(&resultObjectTemp.getValue());
  }

  // Prepare a jump destination here.
  ExitableFullExpr scope(SGF, CleanupLocation::getCleanupLocation(E));

  SGF.emitCheckedCastBranch(E, E->getSubExpr(),
                            resultObjectType, resultObjectCtx,
    // The success path.
    [&](ManagedValue objectValue) {
      // If we're not emitting into a temporary, just wrap up the result
      // in Some and go to the continuation block.
      if (!resultObjectTemp) {
        if (hasAbstraction)
          objectValue = SGF.emitSubstToOrigValue(E, objectValue, abstraction,
                                                 resultObjectType);
        auto some = SGF.B.createEnum(E, objectValue.forward(SGF),
                                     someDecl, resultTL.getLoweredType());
        SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), E, { some });
        return;
      }

      // Otherwise, make sure the value is in the context.
      if (!objectValue.isInContext() && hasAbstraction) {
        objectValue = SGF.emitSubstToOrigValue(E, objectValue, abstraction,
                                               resultObjectType,
                                   SGFContext(&resultObjectTemp.getValue()));
      }
      if (!objectValue.isInContext()) {
        objectValue.forwardInto(SGF, E, resultObjectBuffer);
      }
      SGF.B.createInjectEnumAddr(E, resultBuffer, someDecl);
      SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), E);
    },
    // The failure path.
    [&] {
      auto noneDecl = SGF.getASTContext().getOptionalNoneDecl(optKind);

      // If we're not emitting into a temporary, just wrap up the result
      // in None and go to the continuation block.
      if (!resultObjectTemp) {
        auto none =
          SGF.B.createEnum(E, nullptr, noneDecl, resultTL.getLoweredType());
        SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), E, { none });

      // Just construct the enum directly in the context.
      } else {
        SGF.B.createInjectEnumAddr(E, resultBuffer, noneDecl);
        SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), E);
      }
    });

  // Enter the continuation block.
  auto contBB = scope.exit();

  ManagedValue result;
  if (resultObjectTemp) {
    result = SGF.manageBufferForExprResult(resultBuffer, resultTL, C);
  } else {
    auto argument =
      new (SGF.F.getModule()) SILArgument(contBB, resultTL.getLoweredType());
    result = SGF.emitManagedRValueWithCleanup(argument, resultTL);
  }

  return RValue(SGF, E, result);
}

RValue RValueEmitter::visitIsaExpr(IsaExpr *E, SGFContext C) {
  // Handle collection downcasts separately.
  auto castKind = E->getCastKind();
  if (castKind == CheckedCastKind::ArrayDowncast ||
      castKind == CheckedCastKind::DictionaryDowncast ||
      castKind == CheckedCastKind::DictionaryDowncastBridged ||
      castKind == CheckedCastKind::SetDowncast ||
      castKind == CheckedCastKind::SetDowncastBridged) {
    bool bridgesFromObjC
      = (castKind == CheckedCastKind::DictionaryDowncastBridged ||
         castKind == CheckedCastKind::SetDowncastBridged);
    SILLocation loc(E);
    ManagedValue optValue = emitCollectionDowncastExpr(
                              SGF, E->getSubExpr(), loc,
                              E->getCastTypeLoc().getType(), 
                              C, /*conditional=*/true,
                              bridgesFromObjC)
      .getAsSingleValue(SGF, loc);

    // Materialize the input.
    SILValue optValueTemp;
    if (optValue.getType().isAddress()) {
      optValueTemp = optValue.forward(SGF);
    } else {
      optValueTemp = SGF.emitTemporaryAllocation(loc, optValue.getType());
      optValue.forwardInto(SGF, loc, optValueTemp);
    }

    SILValue isa = SGF.emitDoesOptionalHaveValue(E, optValueTemp);
    
    // Call the _getBool library intrinsic.
    ASTContext &ctx = SGF.SGM.M.getASTContext();
    auto result =
      SGF.emitApplyOfLibraryIntrinsic(E, ctx.getGetBoolDecl(nullptr), {},
                                      ManagedValue::forUnmanaged(isa),
                                      C);
    return RValue(SGF, E, result);
  }

  // Prepare a jump destination here.
  ExitableFullExpr scope(SGF, CleanupLocation::getCleanupLocation(E));

  auto i1Ty = SILType::getBuiltinIntegerType(1, SGF.getASTContext());

  SGF.emitCheckedCastBranch(E, E->getSubExpr(), E->getCastTypeLoc().getType(),
                            SGFContext(),
    [&](ManagedValue value) {
      SILValue yes = SGF.B.createIntegerLiteral(E, i1Ty, 1);
      SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), E, yes);
    },
    [&] {
      SILValue no = SGF.B.createIntegerLiteral(E, i1Ty, 0);
      SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), E, no);
    });

  auto contBB = scope.exit();
  auto isa = new (SGF.SGM.M) SILArgument(contBB, i1Ty);
  
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

static ManagedValue emitVarargs(SILGenFunction &gen,
                                SILLocation loc,
                                Type baseTy,
                                ArrayRef<ManagedValue> elements,
                                Type arrayTy) {
  // Reabstract the base type against the array element type.
  AbstractionPattern baseAbstraction(
    arrayTy->getNominalOrBoundGenericNominal()
           ->getGenericParams()->getPrimaryArchetypes()[0]);
  auto baseCanTy = baseTy->getCanonicalType();
  auto &baseTL = gen.getTypeLowering(baseAbstraction, baseCanTy);
  
  // Allocate the array.
  SILValue numEltsVal = gen.B.createIntegerLiteral(loc,
                             SILType::getBuiltinWordType(gen.F.getASTContext()),
                             elements.size());
  // The first result is the array value.
  ManagedValue array;
  // The second result is a RawPointer to the base address of the array.
  SILValue basePtr;
  std::tie(array, basePtr)
    = gen.emitUninitializedArrayAllocation(arrayTy, numEltsVal, loc);
  
  // Turn the pointer into an address.
  basePtr = gen.B.createPointerToAddress(loc, basePtr,
                                     baseTL.getLoweredType().getAddressType());
  
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
    v = gen.emitSubstToOrigValue(loc, v, baseAbstraction, baseCanTy);
    v.forwardInto(gen, loc, eltPtr);
  }
  
  return array;
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
      for (unsigned i = 0, size = subInitializations.size(); i < size; ++i) {
        SGF.emitExprInto(E->getElements()[i], subInitializations[i].get());
      }
      return RValue();
    }
  }
    
  RValue result(type);
  for (Expr *elt : E->getElements())
    result.addElement(SGF.emitRValue(elt));
  return result;
}

std::tuple<ManagedValue, SILType, ArrayRef<Substitution>>
SILGenFunction::emitSiblingMethodRef(SILLocation loc,
                                     SILValue selfValue,
                                     SILDeclRef methodConstant,
                                     ArrayRef<Substitution> subs) {
  SILValue methodValue;
  
  // If the method is dynamic, access it through runtime-hookable virtual
  // dispatch (viz. objc_msgSend for now).
  if (methodConstant.hasDecl()
      && methodConstant.getDecl()->getAttrs().hasAttribute<DynamicAttr>())
    methodValue = emitDynamicMethodRef(loc, methodConstant,
                                     SGM.Types.getConstantInfo(methodConstant));
  else
    methodValue = emitGlobalFunctionRef(loc, methodConstant);

  SILType methodTy = methodValue.getType();
  
  if (!subs.empty()) {
    // Specialize the generic method.
    methodTy = getLoweredLoadableType(
                    methodTy.castTo<SILFunctionType>()
                      ->substGenericArgs(SGM.M, SGM.SwiftModule, subs));
  }
  
  return std::make_tuple(ManagedValue::forUnmanaged(methodValue),
                         methodTy, subs);
}

RValue RValueEmitter::visitMemberRefExpr(MemberRefExpr *E, SGFContext C) {
  // Any writebacks for this access are tightly scoped.
  WritebackScope scope(SGF);
  
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

  // TODO: unify the rest of this with l-value emission.

  auto FieldDecl = cast<VarDecl>(E->getMember().getDecl());

  // Evaluate the base of the member reference.  Since emitRValueForPropertyLoad
  // works with +0 bases correctly, we ask for them to come back.
  ManagedValue base = SGF.emitRValueAsSingleValue(E->getBase(),
                                                  SGFContext::AllowPlusZero);
  ManagedValue res = SGF.emitRValueForPropertyLoad(E, base, E->isSuper(), 
                                                   FieldDecl,
                                             E->getMember().getSubstitutions(),
                                                   E->getAccessSemantics(),
                                                   E->getType(), C);
  return RValue(SGF, E, res);
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
  SGFContext SubContext;
  if (C.isPlusZeroOk())
    SubContext = SGFContext::AllowPlusZero;
  
  return visit(E->getBase(), SubContext).extractElement(E->getFieldNumber());
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
  
  auto outerFields = E->getType()->castTo<TupleType>()->getFields();
  auto shuffleIndexIterator = E->getElementMapping().begin();
  auto shuffleIndexEnd = E->getElementMapping().end();
  unsigned callerDefaultArgIndex = 0;
  for (auto &field : outerFields) {
    assert(shuffleIndexIterator != shuffleIndexEnd &&
           "ran out of shuffle indexes before running out of fields?!");
    int shuffleIndex = *shuffleIndexIterator++;
    
    // If the shuffle index is DefaultInitialize, we're supposed to use the
    // default value.
    if (shuffleIndex == TupleShuffleExpr::DefaultInitialize) {
      unsigned destIndex
        = shuffleIndexIterator - E->getElementMapping().begin() - 1;
      SILDeclRef generator 
        = SILDeclRef::getDefaultArgGenerator(E->getDefaultArgsOwner().getDecl(),
                                              destIndex);
      
      auto fnRef = SGF.emitFunctionRef(E, generator);
      auto resultType = field.getType()->getCanonicalType();
      
      auto fnType = fnRef.getType().castTo<SILFunctionType>();
      auto substFnType = fnType->substGenericArgs(SGF.SGM.M,
                                   SGF.SGM.M.getSwiftModule(),
                                   E->getDefaultArgsOwner().getSubstitutions());
      auto origResultType = AbstractionPattern(
        E->getDefaultArgsOwner().getDecl()->getType()->getCanonicalType());
      
      auto apply = SGF.emitApply(E, fnRef, E->getDefaultArgsOwner().getSubstitutions(),
                                 {}, substFnType,
                                 origResultType, resultType,
                                 generator.isTransparent(),
                                 None,
                                 SGFContext());
      
      result.addElement(SGF, apply, resultType, E);
      continue;
    }

    // If the shuffle index is CallerDefaultInitialize, we're supposed to
    // use the caller-provided default value. This is used only in special
    // cases, e.g., __FILE__, __LINE__, and __COLUMN__.
    if (shuffleIndex == TupleShuffleExpr::CallerDefaultInitialize) {
      auto arg = E->getCallerDefaultArgs()[callerDefaultArgIndex++];
      result.addElement(visit(arg));
      continue;
    }

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
  auto outerFields = tupleType->getFields();
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

    auto &element = E->getElements()[i];
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
                                            generator.isTransparent());
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
  auto outerFields = E->getType()->castTo<TupleType>()->getFields();
  for (unsigned i = 0, e = outerFields.size(); i != e; ++i) {
    // Handle the variadic argument. If we didn't emit the scalar field yet,
    // it goes into the variadic array; otherwise, the variadic array is empty.
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

    auto &element = E->getElements()[i];

    // A null element indicates that this is the position of the scalar. Add
    // the scalar here.
    if (element.isNull()) {
      result.addElement(std::move(scalar));
      continue;
    }

    // If this element comes from a default argument generator, emit a call to
    // that generator.
    assert(outerFields[i].hasInit() &&
           "no default initializer in non-scalar field of scalar-to-tuple?!");
    if (auto defaultArgOwner = element.dyn_cast<ConcreteDeclRef>()) {
      SILDeclRef generator
        = SILDeclRef::getDefaultArgGenerator(defaultArgOwner.getDecl(), i);
      auto fnRef = SGF.emitFunctionRef(E, generator);
      auto resultType = outerFields[i].getType()->getCanonicalType();
      
      auto fnType = fnRef.getType().castTo<SILFunctionType>();
      auto substFnType = fnType->substGenericArgs(SGF.SGM.M,
                                   SGF.SGM.M.getSwiftModule(),
                                   defaultArgOwner.getSubstitutions());
      auto origResultType = AbstractionPattern(
        defaultArgOwner.getDecl()->getType()->getCanonicalType());
      
      auto apply = SGF.emitApply(E, fnRef, defaultArgOwner.getSubstitutions(),
                                 {}, substFnType,
                                 origResultType, resultType,
                                 generator.isTransparent(),
                                 None,
                                 SGFContext());

      result.addElement(SGF, apply, resultType, E);
      continue;
    }

    // We have an caller-side default argument. Emit it.
    Expr *defArg = element.get<Expr *>();
    result.addElement(visit(defArg));
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
                                        SGFContext::AllowPlusZero).getValue();
    return B.createExistentialMetatype(loc, metaTy, base);
  }

  SILType metaTy = getLoweredLoadableType(CanMetatypeType::get(baseTy));
  // If the lowered metatype has a thick representation, we need to derive it
  // dynamically from the instance.
  if (metaTy.castTo<MetatypeType>()->getRepresentation()
          != MetatypeRepresentation::Thin) {
    auto base = emitRValueAsSingleValue(baseExpr,
                                        SGFContext::AllowPlusZero).getValue();
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

ManagedValue
SILGenFunction::emitClosureValue(SILLocation loc, SILDeclRef constant,
                                 ArrayRef<Substitution> forwardSubs,
                                 AnyFunctionRef TheClosure) {
  // FIXME: Stash the capture args somewhere and curry them on demand rather
  // than here.
  assert(((constant.uncurryLevel == 1 &&
           TheClosure.getCaptureInfo().hasLocalCaptures()) ||
          (constant.uncurryLevel == 0 &&
           !TheClosure.getCaptureInfo().hasLocalCaptures())) &&
         "curried local functions not yet supported");

  auto constantInfo = getConstantInfo(constant);
  SILValue functionRef = emitGlobalFunctionRef(loc, constant, constantInfo);
  SILType functionTy = functionRef.getType();

  auto expectedType =
    cast<FunctionType>(TheClosure.getType()->getCanonicalType());

  // Forward substitutions from the outer scope.
  
  auto pft = constantInfo.SILFnType;

  bool wasSpecialized = false;
  if (pft->isPolymorphic() && !forwardSubs.empty()) {
    auto specialized = pft->substGenericArgs(F.getModule(),
                                                F.getModule().getSwiftModule(),
                                                forwardSubs);
    functionTy = SILType::getPrimitiveObjectType(specialized);
    wasSpecialized = true;
  }

  if (!TheClosure.getCaptureInfo().hasLocalCaptures() && !wasSpecialized) {
    auto result = ManagedValue::forUnmanaged(functionRef);
    return emitGeneralizedFunctionValue(loc, result,
                             AbstractionPattern(expectedType), expectedType);
  }

  SmallVector<CaptureInfo::LocalCaptureTy, 4> captures;
  TheClosure.getLocalCaptures(captures);
  SmallVector<SILValue, 4> capturedArgs;
  for (auto capture : captures) {
    auto *vd = capture.getPointer();
    
    switch (SGM.Types.getDeclCaptureKind(capture, TheClosure)) {
    case CaptureKind::None:
      break;

    case CaptureKind::Constant: {
      // let declarations.
      auto Entry = VarLocs[vd];

      // Non-address-only constants are passed at +1.
      auto &tl = getTypeLowering(vd->getType()->getReferenceStorageReferent());
      SILValue Val = Entry.value;

      if (!Val.getType().isAddress()) {
        // Just retain a by-val let.
        B.emitRetainValueOperation(loc, Val);
      } else {
        // If we have a mutable binding for a 'let', such as 'self' in an
        // 'init' method, load it.
        Val = emitLoad(loc, Val, tl, SGFContext(), IsNotTake).forward(*this);
      }
      
      // Use an RValue to explode Val if it is a tuple.
      RValue RV(*this, loc, vd->getType()->getCanonicalType(),
                ManagedValue::forUnmanaged(Val));

      // If we're capturing an unowned pointer by value, we will have just
      // loaded it into a normal retained class pointer, but we capture it as
      // an unowned pointer.  Convert back now.
      if (vd->getType()->is<ReferenceStorageType>()) {
        auto type = getTypeLowering(vd->getType()).getLoweredType();
        auto val = std::move(RV).forwardAsSingleStorageValue(*this, type,loc);
        capturedArgs.push_back(val);
      } else {
        std::move(RV).forwardAll(*this, capturedArgs);
      }
      break;
    }

    case CaptureKind::NoEscape: {
      // No-escaping stored declarations are captured as the
      // address of the value.
      assert(VarLocs.count(vd) && "no location for captured var!");
      VarLoc vl = VarLocs[vd];
      assert(vl.value.getType().isAddress() && "no address for captured var!");
      capturedArgs.push_back(vl.value);
      break;
    }

    case CaptureKind::Box: {
      // LValues are captured as both the box owning the value and the
      // address of the value.
      assert(VarLocs.count(vd) && "no location for captured var!");
      VarLoc vl = VarLocs[vd];
      assert(vl.value.getType().isAddress() && "no address for captured var!");

      // If this is a boxed variable, we can use it directly.
      if (vl.box) {
        B.createStrongRetain(loc, vl.box);
        capturedArgs.push_back(vl.box);
        capturedArgs.push_back(vl.value);
      } else {
        // Address only 'let' values are passed by box.  This isn't great, in
        // that a variable captured by multiple closures will be boxed for each
        // one.  This could be improved by doing an "isCaptured" analysis when
        // emitting address-only let constants, and emit them into a alloc_box
        // like a variable instead of into an alloc_stack.
        AllocBoxInst *allocBox =
          B.createAllocBox(loc, vl.value.getType().getObjectType());
        auto boxAddress = SILValue(allocBox, 1);
        B.createCopyAddr(loc, vl.value, boxAddress, IsNotTake,IsInitialization);
        capturedArgs.push_back(SILValue(allocBox, 0));
        capturedArgs.push_back(boxAddress);
      }

      break;
    }
    case CaptureKind::LocalFunction: {
      // SILValue is a constant such as a local func. Pass on the reference.
      ManagedValue v = emitRValueForDecl(loc, vd, vd->getType(),
                                         AccessSemantics::Ordinary);
      capturedArgs.push_back(v.forward(*this));
      break;
    }
    case CaptureKind::GetterSetter: {
      // Pass the setter and getter closure references on.
      auto *Setter = cast<AbstractStorageDecl>(vd)->getSetter();
      ManagedValue v = emitFunctionRef(loc, SILDeclRef(Setter,
                                                       SILDeclRef::Kind::Func));
      capturedArgs.push_back(v.forward(*this));
      SWIFT_FALLTHROUGH;
    }
    case CaptureKind::Getter: {
      // Pass the getter closure reference on.
      auto *Getter = cast<AbstractStorageDecl>(vd)->getGetter();
      ManagedValue v = emitFunctionRef(loc, SILDeclRef(Getter,
                                                       SILDeclRef::Kind::Func));
      capturedArgs.push_back(v.forward(*this));
      break;
    }
    }
  }
  
  SILType closureTy =
    SILBuilder::getPartialApplyResultType(functionRef.getType(),
                                          capturedArgs.size(), SGM.M,
                                          forwardSubs);
  auto toClosure =
    B.createPartialApply(loc, functionRef, functionTy,
                         forwardSubs, capturedArgs, closureTy);
  auto result = emitManagedRValueWithCleanup(toClosure);

  return emitGeneralizedFunctionValue(loc, result,
                                      AbstractionPattern(expectedType),
                                      expectedType);
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
  return RValue(SGF, e, SGF.emitClosureValue(e, SILDeclRef(e),
                                          SGF.getForwardingSubstitutions(), e));
}

// Get the __FUNCTION__ name for a declaration.
static DeclName getMagicFunctionName(DeclContext *dc) {
  // For closures, use the parent name.
  if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
    return getMagicFunctionName(closure->getParent());
  }
  if (auto absFunc = dyn_cast<AbstractFunctionDecl>(dc)) {
    // If this is an accessor, use the name of the storage.
    if (auto func = dyn_cast<FuncDecl>(absFunc)) {
      if (auto storage = func->getAccessorStorageDecl())
        return storage->getFullName();
    }

    return absFunc->getFullName();
  }
  if (auto init = dyn_cast<Initializer>(dc)) {
    return getMagicFunctionName(init->getParent());
  }
  if (auto nominal = dyn_cast<NominalTypeDecl>(dc)) {
    return nominal->getName();
  }
  if (auto tl = dyn_cast<TopLevelCodeDecl>(dc)) {
    return tl->getModuleContext()->Name;
  }
  if (auto fu = dyn_cast<FileUnit>(dc)) {
    return fu->getParentModule()->Name;
  }
  if (auto m = dyn_cast<Module>(dc)) {
    return m->Name;
  }
  llvm_unreachable("unexpected __FUNCTION__ context");
}

static DeclName getMagicFunctionName(SILDeclRef ref) {
  switch (ref.kind) {
  case SILDeclRef::Kind::Func:
    if (auto closure = ref.getAbstractClosureExpr())
      return getMagicFunctionName(closure);
    return getMagicFunctionName(cast<FuncDecl>(ref.getDecl()));
  case SILDeclRef::Kind::Initializer:
  case SILDeclRef::Kind::Allocator:
    return getMagicFunctionName(cast<ConstructorDecl>(ref.getDecl()));
  case SILDeclRef::Kind::Deallocator:
  case SILDeclRef::Kind::Destroyer:
    return getMagicFunctionName(cast<DestructorDecl>(ref.getDecl()));
  case SILDeclRef::Kind::GlobalAccessor:
  case SILDeclRef::Kind::GlobalGetter:
    return getMagicFunctionName(cast<VarDecl>(ref.getDecl())->getDeclContext());
  case SILDeclRef::Kind::DefaultArgGenerator:
    return getMagicFunctionName(cast<AbstractFunctionDecl>(ref.getDecl()));
  case SILDeclRef::Kind::IVarInitializer:
    return getMagicFunctionName(cast<ClassDecl>(ref.getDecl()));
  case SILDeclRef::Kind::IVarDestroyer:
    return getMagicFunctionName(cast<ClassDecl>(ref.getDecl()));
  case SILDeclRef::Kind::EnumElement:
    return getMagicFunctionName(cast<EnumElementDecl>(ref.getDecl())
                                  ->getDeclContext());
  }
}

void SILGenFunction::emitFunction(FuncDecl *fd) {
  MagicFunctionName = getMagicFunctionName(fd);
  
  Type resultTy = fd->getResultType();
  emitProlog(fd, fd->getBodyParamPatterns(), resultTy);
  prepareEpilog(resultTy, CleanupLocation(fd));
  visit(fd->getBody());
  emitEpilog(fd);
}

void SILGenFunction::emitClosure(AbstractClosureExpr *ace) {
  MagicFunctionName = getMagicFunctionName(ace);
  
  emitProlog(ace, ace->getParams(), ace->getResultType());
  prepareEpilog(ace->getResultType(), CleanupLocation(ace));
  if (auto *ce = dyn_cast<ClosureExpr>(ace))
    visit(ce->getBody());
  else {
    auto *autoclosure = cast<AutoClosureExpr>(ace);
    // Closure expressions implicitly return the result of their body
    // expression.
    emitReturnExpr(ImplicitReturnLocation(ace),
                   autoclosure->getSingleExpressionBody());
  }
  emitEpilog(ace);
}

void SILGenFunction::emitArtificialTopLevel(ClassDecl *mainClass) {
  // Load argc and argv from the entry point arguments.
  SILValue argc = F.begin()->getBBArg(0);
  SILValue argv = F.begin()->getBBArg(1);
  
  switch (mainClass->getArtificialMainKind()) {
  case ArtificialMainKind::UIApplicationMain: {
    // Emit a UIKit main.
    // return UIApplicationMain(C_ARGC, C_ARGV, nil, ClassName);
    
    CanType NSStringTy = SGM.Types.getNSStringType();
    CanType IUOptNSStringTy
      = ImplicitlyUnwrappedOptionalType::get(NSStringTy)->getCanonicalType();
    
    // Get the class name as a string using NSStringFromClass.
    CanType mainClassTy = mainClass->getDeclaredTypeInContext()->getCanonicalType();
    CanType mainClassMetaty = CanMetatypeType::get(mainClassTy,
                                                   MetatypeRepresentation::ObjC);
    ProtocolDecl *anyObjectProtocol =
      getASTContext().getProtocol(KnownProtocolKind::AnyObject);
    auto mainClassAnyObjectConformance =
      getASTContext().getConformsTo(mainClassTy, anyObjectProtocol)
        ->getPointer();
    CanType anyObjectTy = anyObjectProtocol
      ->getDeclaredTypeInContext()
      ->getCanonicalType();
    CanType anyObjectMetaTy = CanExistentialMetatypeType::get(anyObjectTy,
                                                  MetatypeRepresentation::ObjC);
    CanType IUOptAnyObjectMetaty
      = ImplicitlyUnwrappedOptionalType::get(anyObjectMetaTy)
          ->getCanonicalType();
    
    auto NSStringFromClassType = SILFunctionType::get(nullptr,
                  AnyFunctionType::ExtInfo()
                    .withRepresentation(AnyFunctionType::Representation::Thin)
                    .withCallingConv(AbstractCC::C),
                  ParameterConvention::Direct_Unowned,
                  SILParameterInfo(IUOptAnyObjectMetaty,
                                   ParameterConvention::Direct_Unowned),
                  SILResultInfo(IUOptNSStringTy,
                                ResultConvention::Autoreleased),
                  getASTContext());
    auto NSStringFromClassFn
      = SGM.M.getOrCreateFunction(mainClass, "NSStringFromClass",
                                  SILLinkage::PublicExternal,
                                  NSStringFromClassType,
                                  IsBare, IsTransparent, IsNotFragile);
    auto NSStringFromClass = B.createFunctionRef(mainClass, NSStringFromClassFn);
    SILValue metaTy = B.createMetatype(mainClass,
                             SILType::getPrimitiveObjectType(mainClassMetaty));
    metaTy = B.createInitExistentialMetatype(mainClass, metaTy,
                          SILType::getPrimitiveObjectType(anyObjectMetaTy),
                          getASTContext().AllocateCopy(
                            llvm::makeArrayRef(mainClassAnyObjectConformance)));
    SILValue iuoptMetaTy =
      B.createEnum(mainClass, metaTy,
                   getASTContext().getImplicitlyUnwrappedOptionalSomeDecl(),
                   SILType::getPrimitiveObjectType(IUOptAnyObjectMetaty));
    SILValue iuoptName = B.createApply(mainClass,
                               NSStringFromClass,
                               NSStringFromClass->getType(),
                               SILType::getPrimitiveObjectType(IUOptNSStringTy),
                               {}, iuoptMetaTy);
    
    // Call UIApplicationMain.
    SILParameterInfo argTypes[] = {
      SILParameterInfo(argc.getType().getSwiftRValueType(),
                       ParameterConvention::Direct_Unowned),
      SILParameterInfo(argv.getType().getSwiftRValueType(),
                       ParameterConvention::Direct_Unowned),
      SILParameterInfo(IUOptNSStringTy, ParameterConvention::Direct_Unowned),
      SILParameterInfo(IUOptNSStringTy, ParameterConvention::Direct_Unowned),
    };
    auto UIApplicationMainType = SILFunctionType::get(nullptr,
                  AnyFunctionType::ExtInfo()
                    .withRepresentation(AnyFunctionType::Representation::Thin)
                    .withCallingConv(AbstractCC::C),
                  ParameterConvention::Direct_Unowned,
                  argTypes,
                  SILResultInfo(argc.getType().getSwiftRValueType(),
                                ResultConvention::Unowned),
                  getASTContext());
    
    auto UIApplicationMainFn
      = SGM.M.getOrCreateFunction(mainClass, "UIApplicationMain",
                                  SILLinkage::PublicExternal,
                                  UIApplicationMainType,
                                  IsBare, IsTransparent, IsNotFragile);
    
    auto UIApplicationMain = B.createFunctionRef(mainClass, UIApplicationMainFn);
    auto nil = B.createEnum(mainClass, SILValue(),
                      getASTContext().getImplicitlyUnwrappedOptionalNoneDecl(),
                      SILType::getPrimitiveObjectType(IUOptNSStringTy));

    SILValue args[] = { argc, argv, nil, iuoptName };
    
    B.createApply(mainClass, UIApplicationMain,
                  UIApplicationMain->getType(),
                  argc.getType(), {}, args);
    SILValue r = B.createIntegerLiteral(mainClass,
                        SILType::getBuiltinIntegerType(32, getASTContext()), 0);
    if (r.getType() != F.getLoweredFunctionType()->getResult().getSILType())
      r = B.createStruct(mainClass,
                       F.getLoweredFunctionType()->getResult().getSILType(), r);
    
    B.createReturn(mainClass, r);
    return;
  }
  
  case ArtificialMainKind::NSApplicationMain: {
    // Emit an AppKit main.
    // return NSApplicationMain(C_ARGC, C_ARGV);

    SILParameterInfo argTypes[] = {
      SILParameterInfo(argc.getType().getSwiftRValueType(),
                       ParameterConvention::Direct_Unowned),
      SILParameterInfo(argv.getType().getSwiftRValueType(),
                       ParameterConvention::Direct_Unowned),
    };
    auto NSApplicationMainType = SILFunctionType::get(nullptr,
                  AnyFunctionType::ExtInfo()
                    .withRepresentation(AnyFunctionType::Representation::Thin)
                    // Should be C calling convention, but NSApplicationMain
                    // has an overlay to fix the type of argv.
                    .withCallingConv(AbstractCC::Freestanding),
                  ParameterConvention::Direct_Unowned,
                  argTypes,
                  SILResultInfo(argc.getType().getSwiftRValueType(),
                                ResultConvention::Unowned),
                  getASTContext());
    
    auto NSApplicationMainFn
      = SGM.M.getOrCreateFunction(mainClass, "NSApplicationMain",
                                  SILLinkage::PublicExternal,
                                  NSApplicationMainType,
                                  IsBare, IsTransparent, IsNotFragile);
    
    auto NSApplicationMain = B.createFunctionRef(mainClass, NSApplicationMainFn);
    SILValue args[] = { argc, argv };
    
    B.createApply(mainClass, NSApplicationMain,
                  NSApplicationMain->getType(),
                  argc.getType(), {}, args);
    SILValue r = B.createIntegerLiteral(mainClass,
                        SILType::getBuiltinIntegerType(32, getASTContext()), 0);
    if (r.getType() != F.getLoweredFunctionType()->getResult().getSILType())
      r = B.createStruct(mainClass,
                       F.getLoweredFunctionType()->getResult().getSILType(), r);
    B.createReturn(mainClass, r);
    return;
  }
  }
}

std::pair<Optional<SILValue>, SILLocation>
SILGenFunction::emitEpilogBB(SILLocation TopLevel) {
  assert(ReturnDest.getBlock() && "no epilog bb prepared?!");
  SILBasicBlock *epilogBB = ReturnDest.getBlock();
  SILLocation ImplicitReturnFromTopLevel =
    ImplicitReturnLocation::getImplicitReturnLoc(TopLevel);
  SILValue returnValue;
  Optional<SILLocation> returnLoc = None;

  // If the current BB isn't terminated, and we require a return, then we
  // are not allowed to fall off the end of the function and can't reach here.
  if (NeedsReturn && B.hasValidInsertionPoint()) {
    B.createUnreachable(ImplicitReturnFromTopLevel);
  }
  
  if (epilogBB->pred_empty()) {
    bool hadArg = !epilogBB->bbarg_empty();
    
    // If the epilog was not branched to at all, kill the BB and
    // just emit the epilog into the current BB.
    epilogBB->eraseFromParent();

    // If the current bb is terminated then the epilog is just unreachable.
    if (!B.hasValidInsertionPoint())
      return { None, TopLevel };
    // We emit the epilog at the current insertion point.
    assert(!hadArg && "NeedsReturn is false but epilog had argument?!");
    (void)hadArg;
    returnLoc = ImplicitReturnFromTopLevel;

  } else if (std::next(epilogBB->pred_begin()) == epilogBB->pred_end()
             && !B.hasValidInsertionPoint()) {
    // If the epilog has a single predecessor and there's no current insertion
    // point to fall through from, then we can weld the epilog to that
    // predecessor BB.

    bool needsArg = false;
    if (!epilogBB->bbarg_empty()) {
      assert(epilogBB->bbarg_size() == 1 && "epilog should take 0 or 1 args");
      needsArg = true;
    }
    
    epilogBB->eraseFromParent();

    // Steal the branch argument as the return value if present.
    SILBasicBlock *pred = *epilogBB->pred_begin();
    BranchInst *predBranch = cast<BranchInst>(pred->getTerminator());
    assert(predBranch->getArgs().size() == (needsArg ? 1 : 0)
           && "epilog predecessor arguments does not match block params");
    if (needsArg)
      returnValue = predBranch->getArgs()[0];

    // If we are optimizing, we should use the return location from the single,
    // previously processed, return statement if any.
    if (predBranch->getLoc().is<ReturnLocation>()) {
      returnLoc = predBranch->getLoc();
    } else {
      returnLoc = ImplicitReturnFromTopLevel;
    }

    // Kill the branch to the now-dead epilog BB.
    pred->getInstList().erase(predBranch);

    // Emit the epilog into its former predecessor.
    B.setInsertionPoint(pred);
  } else {
    // Emit the epilog into the epilog bb. Its argument is the return value.
    if (!epilogBB->bbarg_empty()) {
      assert(epilogBB->bbarg_size() == 1 && "epilog should take 0 or 1 args");
      returnValue = epilogBB->bbarg_begin()[0];
    }

    // If we are falling through from the current block, the return is implicit.
    B.emitBlock(epilogBB, ImplicitReturnFromTopLevel);
  }
  
  // Emit top-level cleanups into the epilog block.
  assert(!Cleanups.hasAnyActiveCleanups(getCleanupsDepth(),
                                        ReturnDest.getDepth())
         && "emitting epilog in wrong scope");         

  auto cleanupLoc = CleanupLocation::getCleanupLocation(TopLevel);
  Cleanups.emitCleanupsForReturn(cleanupLoc);
  
  // If the return location is known to be that of an already
  // processed return, use it. (This will get triggered when the
  // epilog logic is simplified.)
  //
  // Otherwise make the ret instruction part of the cleanups.
  if (!returnLoc) returnLoc = cleanupLoc;

  return { returnValue, *returnLoc };
}

void SILGenFunction::emitEpilog(SILLocation TopLevel, bool AutoGen) {
  Optional<SILValue> maybeReturnValue;
  SILLocation returnLoc(TopLevel);

  // Construct the appropriate SIL Location for the return instruction.
  if (AutoGen)
    TopLevel.markAutoGenerated();

  std::tie(maybeReturnValue, returnLoc) = emitEpilogBB(TopLevel);

  // If the epilog is unreachable, we're done.
  if (!maybeReturnValue)
    return;
  
  // Otherwise, return the return value, if any.
  SILValue returnValue = *maybeReturnValue;

  // Return () if no return value was given.
  if (!returnValue)
    returnValue = emitEmptyTuple(CleanupLocation::getCleanupLocation(TopLevel));

  B.createReturn(returnLoc, returnValue);
  if (!MainScope)
    MainScope = F.getDebugScope();
  setDebugScopeForInsertedInstrs(MainScope);
}

void SILGenFunction::emitDestroyingDestructor(DestructorDecl *dd) {
  MagicFunctionName = DeclName(SGM.M.getASTContext().getIdentifier("deinit"));

  RegularLocation Loc(dd);
  if (dd->isImplicit())
    Loc.markAutoGenerated();

  auto cd = cast<ClassDecl>(dd->getDeclContext());
  SILValue selfValue = emitSelfDecl(dd->getImplicitSelfDecl());

  // Create a basic block to jump to for the implicit destruction behavior
  // of releasing the elements and calling the superclass destructor.
  // We won't actually emit the block until we finish with the destructor body.
  prepareEpilog(Type(), CleanupLocation::getCleanupLocation(Loc));
  
  // Emit the destructor body.
  visit(dd->getBody());

  Optional<SILValue> maybeReturnValue;
  SILLocation returnLoc(Loc);
  std::tie(maybeReturnValue, returnLoc) = emitEpilogBB(Loc);

  if (!maybeReturnValue)
    return;

  auto cleanupLoc = CleanupLocation::getCleanupLocation(Loc);
    
  // If we have a superclass, invoke its destructor.
  SILValue resultSelfValue;
  SILType objectPtrTy = SILType::getNativeObjectType(F.getASTContext());
  if (cd->hasSuperclass()) {
    Type superclassTy
      = ArchetypeBuilder::mapTypeIntoContext(dd, cd->getSuperclass());
    ClassDecl *superclass = superclassTy->getClassOrBoundGenericClass();
    auto superclassDtorDecl = superclass->getDestructor();
    SILDeclRef dtorConstant =
      SILDeclRef(superclassDtorDecl, SILDeclRef::Kind::Destroyer);
    SILType baseSILTy = getLoweredLoadableType(superclassTy);
    SILValue baseSelf = B.createUpcast(cleanupLoc, selfValue, baseSILTy);
    ManagedValue dtorValue;
    SILType dtorTy;
    ArrayRef<Substitution> subs
      = superclassTy->gatherAllSubstitutions(SGM.M.getSwiftModule(), nullptr);
    std::tie(dtorValue, dtorTy, subs)
      = emitSiblingMethodRef(cleanupLoc, baseSelf, dtorConstant, subs);
    resultSelfValue = B.createApply(cleanupLoc, dtorValue.forward(*this), 
                                    dtorTy, objectPtrTy, subs, baseSelf);
  } else {
    resultSelfValue = B.createUncheckedRefCast(cleanupLoc, selfValue,
                                                 objectPtrTy);
  }

  // Release our members.
  emitClassMemberDestruction(selfValue, cd, cleanupLoc);

  B.createReturn(returnLoc, resultSelfValue);
}

void SILGenFunction::emitDeallocatingDestructor(DestructorDecl *dd) {
  MagicFunctionName = DeclName(SGM.M.getASTContext().getIdentifier("deinit"));

  // The deallocating destructor is always auto-generated.
  RegularLocation loc(dd);
  loc.markAutoGenerated();
  
  // Emit the prolog.
  auto cd = cast<ClassDecl>(dd->getDeclContext());
  SILValue selfValue = emitSelfDecl(dd->getImplicitSelfDecl());

  // Form a reference to the destroying destructor.
  SILDeclRef dtorConstant(dd, SILDeclRef::Kind::Destroyer);
  auto classTy = cd->getDeclaredTypeInContext();
  ManagedValue dtorValue;
  SILType dtorTy;
  ArrayRef<Substitution> subs
    = classTy->gatherAllSubstitutions(SGM.M.getSwiftModule(), nullptr);
  std::tie(dtorValue, dtorTy, subs)
    = emitSiblingMethodRef(loc, selfValue, dtorConstant, subs);

  // Call the destroying destructor.
  SILType objectPtrTy = SILType::getNativeObjectType(F.getASTContext());
  selfValue = B.createApply(loc, dtorValue.forward(*this), 
                            dtorTy, objectPtrTy, subs, selfValue);

  // Deallocate the object.
  selfValue = B.createUncheckedRefCast(loc, selfValue, 
                                         getLoweredType(classTy));
  B.createDeallocRef(loc, selfValue);

  // Return.
  B.createReturn(loc, emitEmptyTuple(loc));
}

static SILValue emitConstructorMetatypeArg(SILGenFunction &gen,
                                           ValueDecl *ctor) {
  // In addition to the declared arguments, the constructor implicitly takes
  // the metatype as its first argument, like a static function.
  Type metatype = ctor->getType()->castTo<AnyFunctionType>()->getInput();
  auto &AC = gen.getASTContext();
  auto VD = new (AC) ParamDecl(/*IsLet*/ true, SourceLoc(),
                               AC.getIdentifier("$metatype"), SourceLoc(),
                               AC.getIdentifier("$metatype"), metatype,
                               ctor->getDeclContext());
  return new (gen.F.getModule()) SILArgument(gen.F.begin(),
                                             gen.getLoweredType(metatype),
                                             VD);
}

static RValue emitImplicitValueConstructorArg(SILGenFunction &gen,
                                              SILLocation loc,
                                              CanType ty,
                                              DeclContext *DC) {
  // Restructure tuple arguments.
  if (CanTupleType tupleTy = dyn_cast<TupleType>(ty)) {
    RValue tuple(ty);
    for (auto fieldType : tupleTy.getElementTypes())
      tuple.addElement(emitImplicitValueConstructorArg(gen, loc, fieldType, DC));

    return tuple;
  } else {
    auto &AC = gen.getASTContext();
    auto VD = new (AC) ParamDecl(/*IsLet*/ true, SourceLoc(),
                                 AC.getIdentifier("$implicit_value"), 
                                 SourceLoc(),
                                 AC.getIdentifier("$implicit_value"), ty, DC);
    SILValue arg = new (gen.F.getModule()) SILArgument(gen.F.begin(),
                                                       gen.getLoweredType(ty),
                                                       VD);
    return RValue(gen, loc, ty, ManagedValue::forUnmanaged(arg));
  }
}

namespace {
  class ImplicitValueInitialization : public SingleBufferInitialization {
    SILValue slot;
  public:
    ImplicitValueInitialization(SILValue slot) : slot(slot) {}
    
    SILValue getAddressOrNull() const override {
      return slot;
    }
  };
}

static void emitImplicitValueConstructor(SILGenFunction &gen,
                                         ConstructorDecl *ctor) {
  RegularLocation Loc(ctor);
  Loc.markAutoGenerated();
  // FIXME: Handle 'self' along with the other arguments.
  auto *TP = cast<TuplePattern>(ctor->getBodyParamPatterns()[1]);
  auto selfTyCan = ctor->getImplicitSelfDecl()->getType()->getInOutObjectType();
  SILType selfTy = gen.getLoweredType(selfTyCan);

  // Emit the indirect return argument, if any.
  SILValue resultSlot;
  if (selfTy.isAddressOnly(gen.SGM.M)) {
    auto &AC = gen.getASTContext();
    auto VD = new (AC) ParamDecl(/*IsLet*/ false, SourceLoc(),
                                 AC.getIdentifier("$return_value"),
                                 SourceLoc(),
                                 AC.getIdentifier("$return_value"), selfTyCan,
                                 ctor);
    resultSlot = new (gen.F.getModule()) SILArgument(gen.F.begin(), selfTy, VD);
  }
  
  // Emit the elementwise arguments.
  SmallVector<RValue, 4> elements;
  for (size_t i = 0, size = TP->getFields().size(); i < size; ++i) {
    auto *P = cast<TypedPattern>(TP->getFields()[i].getPattern());
    
    elements.push_back(
      emitImplicitValueConstructorArg(gen, Loc,
                                      P->getType()->getCanonicalType(), ctor));
  }

  emitConstructorMetatypeArg(gen, ctor);

  auto *decl = selfTy.getStructOrBoundGenericStruct();
  assert(decl && "not a struct?!");
  
  // If we have an indirect return slot, initialize it in-place.
  if (resultSlot) {
    
    auto elti = elements.begin(), eltEnd = elements.end();
    for (VarDecl *field : decl->getStoredProperties()) {
      assert(elti != eltEnd && "number of args does not match number of fields");
      (void)eltEnd;
      auto fieldTy = selfTy.getFieldType(field, gen.SGM.M);
      auto &fieldTL = gen.getTypeLowering(fieldTy);
      SILValue slot = gen.B.createStructElementAddr(Loc, resultSlot, field,
                                    fieldTL.getLoweredType().getAddressType());
      InitializationPtr init(new ImplicitValueInitialization(slot));
      std::move(*elti).forwardInto(gen, init.get(), Loc);
      ++elti;
    }
    gen.B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(Loc),
                       gen.emitEmptyTuple(Loc));
    return;
  }
  
  // Otherwise, build a struct value directly from the elements.
  SmallVector<SILValue, 4> eltValues;
  
  auto elti = elements.begin(), eltEnd = elements.end();
  for (VarDecl *field : decl->getStoredProperties()) {
    assert(elti != eltEnd && "number of args does not match number of fields");
    (void)eltEnd;
    auto fieldTy = selfTy.getFieldType(field, gen.SGM.M);
    
    SILValue v
      = std::move(*elti).forwardAsSingleStorageValue(gen, fieldTy, Loc);
    
    eltValues.push_back(v);
    
    ++elti;
  }
  
  SILValue selfValue = gen.B.createStruct(Loc, selfTy, eltValues);
  gen.B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(Loc),
                     selfValue);
  return;
}

void SILGenFunction::emitValueConstructor(ConstructorDecl *ctor) {
  MagicFunctionName = getMagicFunctionName(ctor);
  
  // If there's no body, this is the implicit elementwise constructor.
  if (!ctor->getBody())
    return emitImplicitValueConstructor(*this, ctor);

  // True if this constructor delegates to a peer constructor with self.init().
  bool isDelegating = ctor->getDelegatingOrChainedInitKind(nullptr) ==
    ConstructorDecl::BodyInitKind::Delegating;

  // Get the 'self' decl and type.
  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  auto &lowering = getTypeLowering(selfDecl->getType()->getInOutObjectType());
  SILType selfTy = lowering.getLoweredType();
  (void)selfTy;
  assert(!selfTy.hasReferenceSemantics() && "can't emit a ref type ctor here");
  
  // Emit a local variable for 'self'.
  // FIXME: The (potentially partially initialized) variable would need to be
  // cleaned up on an error unwind.
  emitLocalVariable(selfDecl,
                    isDelegating ? MarkUninitializedInst::DelegatingSelf
                                 : MarkUninitializedInst::RootSelf);

  // Mark self as being uninitialized so that DI knows where it is and how to
  // check for it.
  SILValue selfLV, selfBox;
  {
    auto &SelfVarLoc = VarLocs[selfDecl];
    selfBox = SelfVarLoc.box;
    selfLV = SelfVarLoc.value;
  }

  // FIXME: Handle 'self' along with the other body patterns.

  // Emit the prolog.
  emitProlog(ctor->getBodyParamPatterns()[1],
             ctor->getResultType(),
             ctor);
  emitConstructorMetatypeArg(*this, ctor);
  
  // Create a basic block to jump to for the implicit 'self' return.
  // We won't emit this until after we've emitted the body.
  // The epilog takes a void return because the return of 'self' is implicit.
  prepareEpilog(Type(), CleanupLocation(ctor));
  
  // If the constructor can fail, set up an alternative epilog for constructor
  // failure.
  SILBasicBlock *failureExitBB = nullptr;
  SILArgument *failureExitArg = nullptr;
  auto &resultLowering = getTypeLowering(ctor->getResultType());

  if (ctor->getFailability() != OTK_None) {
    SILBasicBlock *failureBB = createBasicBlock();
    failureExitBB = createBasicBlock();
    
    // On failure, we'll clean up everything (except self, which should have
    // been cleaned up before jumping here) and return nil instead.
    auto curBB = B.getInsertionBB();
    B.setInsertionPoint(failureBB);
    Cleanups.emitCleanupsForReturn(ctor);
    // Return nil.
    if (lowering.isAddressOnly()) {
      // Inject 'nil' into the indirect return.
      B.createInjectEnumAddr(ctor, IndirectReturnAddress,
                   getASTContext().getOptionalNoneDecl(ctor->getFailability()));
      B.createBranch(ctor, failureExitBB);

      B.setInsertionPoint(failureExitBB);
      B.createReturn(ctor, emitEmptyTuple(ctor));
    } else {
      // Pass 'nil' as the return value to the exit BB.
      failureExitArg = new (F.getModule())
        SILArgument(failureExitBB, resultLowering.getLoweredType());
      SILValue nilResult = B.createEnum(ctor, {},
                    getASTContext().getOptionalNoneDecl(ctor->getFailability()),
                    resultLowering.getLoweredType());
      B.createBranch(ctor, failureExitBB, nilResult);

      B.setInsertionPoint(failureExitBB);
      B.createReturn(ctor, failureExitArg);
    }
    
    B.setInsertionPoint(curBB);
    FailDest = JumpDest(failureBB, Cleanups.getCleanupsDepth(), ctor);
    FailSelfDecl = selfDecl;
  }

  // If this is not a delegating constructor, emit member initializers.
  if (!isDelegating) {
    auto nominal = ctor->getDeclContext()->getDeclaredTypeInContext()
                     ->getNominalOrBoundGenericNominal();
    emitMemberInitializers(selfDecl, nominal);
  }

  // Emit the constructor body.
  visit(ctor->getBody());

  Optional<SILValue> maybeReturnValue;
  SILLocation returnLoc(ctor);
  std::tie(maybeReturnValue, returnLoc) = emitEpilogBB(ctor);

  // Return 'self' in the epilog.
  if (!maybeReturnValue)
    return;

  auto cleanupLoc = CleanupLocation::getCleanupLocation(ctor);

  assert(selfBox && "self should be a mutable box");
  
  // If 'self' is address-only, copy 'self' into the indirect return slot.
  if (lowering.isAddressOnly()) {
    assert(IndirectReturnAddress &&
           "no indirect return for address-only ctor?!");
    
    // Get the address to which to store the result.
    SILValue returnAddress;
    switch (ctor->getFailability()) {
    // For non-failable initializers, store to the return address directly.
    case OTK_None:
      returnAddress = IndirectReturnAddress;
      break;
    // If this is a failable initializer, project out the payload.
    case OTK_Optional:
    case OTK_ImplicitlyUnwrappedOptional:
      returnAddress = B.createInitEnumDataAddr(ctor, IndirectReturnAddress,
          getASTContext().getOptionalSomeDecl(ctor->getFailability()),
          selfLV.getType());
      break;
    }
    
    // We have to do a non-take copy because someone else may be using the box.
    B.createCopyAddr(cleanupLoc, selfLV, returnAddress,
                     IsNotTake, IsInitialization);
    B.emitStrongRelease(cleanupLoc, selfBox);
    
    // Inject the enum tag if the result is optional because of failability.
    switch (ctor->getFailability()) {
    case OTK_None:
      // Not optional.
      break;

    case OTK_Optional:
    case OTK_ImplicitlyUnwrappedOptional:
      // Inject the 'Some' tag.
      B.createInjectEnumAddr(ctor, IndirectReturnAddress,
                   getASTContext().getOptionalSomeDecl(ctor->getFailability()));
      break;
    }
    
    if (failureExitBB) {
      B.createBranch(returnLoc, failureExitBB);
    } else {
      B.createReturn(returnLoc, emitEmptyTuple(ctor));
    }
    return;
  }

  // Otherwise, load and return the final 'self' value.
  SILValue selfValue = B.createLoad(cleanupLoc, selfLV);

  // We have to do a retain because someone else may be using the box.
  lowering.emitRetainValue(B, cleanupLoc, selfValue);

  // Release the box.
  B.emitStrongRelease(cleanupLoc, selfBox);
  
  // Inject the self value into an optional if the constructor is failable.
  switch (ctor->getFailability()) {
  case OTK_None:
    // Not optional.
    break;
      
  case OTK_Optional:
  case OTK_ImplicitlyUnwrappedOptional:
    selfValue = B.createEnum(ctor, selfValue,
                   getASTContext().getOptionalSomeDecl(ctor->getFailability()),
                   getLoweredLoadableType(ctor->getResultType()));
    break;
  }
  
  if (failureExitBB) {
    B.createBranch(returnLoc, failureExitBB, selfValue);
  } else {
    B.createReturn(returnLoc, selfValue);
  }
}

static void emitAddressOnlyEnumConstructor(SILGenFunction &gen,
                                           SILType enumTy,
                                           EnumElementDecl *element) {
  RegularLocation Loc(element);
  Loc.markAutoGenerated();

  // Emit the indirect return slot.
  auto &AC = gen.getASTContext();
  auto VD = new (AC) ParamDecl(/*IsLet*/ false, SourceLoc(),
                               AC.getIdentifier("$return_value"),
                               SourceLoc(),
                               AC.getIdentifier("$return_value"),
                               enumTy.getSwiftType(),
                               element->getDeclContext());
  SILValue resultSlot
    = new (gen.F.getModule()) SILArgument(gen.F.begin(), enumTy, VD);
  
  // Emit the exploded constructor argument.
  ManagedValue argValue;
  if (element->hasArgumentType()) {
    RValue arg = emitImplicitValueConstructorArg
      (gen, Loc, element->getArgumentType()->getCanonicalType(),
       element->getDeclContext());
    argValue = std::move(arg).getAsSingleValue(gen, Loc);
  }
  emitConstructorMetatypeArg(gen, element);
  
  // Store the data, if any.
  if (element->hasArgumentType()) {
    SILValue resultData = gen.B.createInitEnumDataAddr(element, resultSlot,
      element, gen.getLoweredType(element->getArgumentType()).getAddressType());
    argValue.forwardInto(gen, element, resultData);
  }
  
  // Apply the tag.
  gen.B.createInjectEnumAddr(Loc, resultSlot, element);
  gen.Cleanups.emitCleanupsForReturn(CleanupLocation::getCleanupLocation(Loc));
  gen.B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(Loc),
                     gen.emitEmptyTuple(element));
}

static void emitLoadableEnumConstructor(SILGenFunction &gen, SILType enumTy,
                                        EnumElementDecl *element) {
  RegularLocation Loc(element);
  Loc.markAutoGenerated();

  // Emit the exploded constructor argument.
  SILValue argValue;
  if (element->hasArgumentType()) {
    RValue arg = emitImplicitValueConstructorArg
      (gen, Loc,
       element->getArgumentType()->getCanonicalType(),
       element->getDeclContext());
    argValue = std::move(arg).forwardAsSingleValue(gen, Loc);
  }
  
  emitConstructorMetatypeArg(gen, element);
  
  // Create and return the enum value.
  SILValue result = gen.B.createEnum(Loc, argValue, element, enumTy);
  gen.Cleanups.emitCleanupsForReturn(CleanupLocation::getCleanupLocation(Loc));
  gen.B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(Loc), result);
}

void SILGenFunction::emitEnumConstructor(EnumElementDecl *element) {
  Type enumTy = element->getType()->getAs<AnyFunctionType>()->getResult();
  if (element->hasArgumentType())
    enumTy = enumTy->getAs<AnyFunctionType>()->getResult();
  auto &enumTI = getTypeLowering(enumTy);
  
  if (enumTI.isAddressOnly()) {
    return emitAddressOnlyEnumConstructor(*this, enumTI.getLoweredType(),
                                           element);
  }
  return emitLoadableEnumConstructor(*this, enumTI.getLoweredType(),
                                      element);
}

namespace {
  // Unlike the ArgumentInitVisitor, this visitor generates arguments but leaves
  // them destructured instead of storing them to lvalues so that the
  // argument set can be easily forwarded to another function.
  class ArgumentForwardVisitor
    : public PatternVisitor<ArgumentForwardVisitor>
  {
    SILGenFunction &gen;
    SmallVectorImpl<SILValue> &args;
  public:
    ArgumentForwardVisitor(SILGenFunction &gen,
                           SmallVectorImpl<SILValue> &args)
      : gen(gen), args(args) {}
    
    void makeArgument(Type ty, VarDecl *varDecl) {
      assert(ty && "no type?!");
      // Destructure tuple arguments.
      if (TupleType *tupleTy = ty->getAs<TupleType>()) {
        for (auto fieldType : tupleTy->getElementTypes())
          makeArgument(fieldType, varDecl);
      } else {
        SILValue arg =
          new (gen.F.getModule()) SILArgument(gen.F.begin(),
                                              gen.getLoweredType(ty),
                                              varDecl);
        args.push_back(arg);
      }
    }

    void visitParenPattern(ParenPattern *P) {
      visit(P->getSubPattern());
    }
    void visitVarPattern(VarPattern *P) {
      visit(P->getSubPattern());
    }
    
    void visitTypedPattern(TypedPattern *P) {
      // FIXME: work around a bug in visiting the "self" argument of methods
      if (auto NP = dyn_cast<NamedPattern>(P->getSubPattern()))
        makeArgument(P->getType(), NP->getDecl());
      else
        visit(P->getSubPattern());
    }
    
    void visitTuplePattern(TuplePattern *P) {
      for (auto &elt : P->getFields())
        visit(elt.getPattern());
    }
    
    void visitAnyPattern(AnyPattern *P) {
      llvm_unreachable("unnamed parameters should have a ParamDecl");
    }
    
    void visitNamedPattern(NamedPattern *P) {
      makeArgument(P->getType(), P->getDecl());
    }
    
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) \
    void visit##Id##Pattern(Id##Pattern *) { \
      llvm_unreachable("pattern not valid in argument binding"); \
    }
#include "swift/AST/PatternNodes.def"

  };
} // end anonymous namespace

ArrayRef<Substitution>
SILGenFunction::buildForwardingSubstitutions(GenericParamList *params) {
  if (!params)
    return {};
  
  ASTContext &C = F.getASTContext();
  
  SmallVector<Substitution, 4> subs;
  
  // TODO: IRGen wants substitutions for secondary archetypes.
  //for (auto &param : params->getNestedGenericParams()) {
  //  ArchetypeType *archetype = param.getAsTypeParam()->getArchetype();
  
  for (auto archetype : params->getAllNestedArchetypes()) {
    // "Check conformance" on each declared protocol to build a
    // conformance map.
    SmallVector<ProtocolConformance*, 2> conformances;
    
    for (ProtocolDecl *conformsTo : archetype->getConformsTo()) {
      (void)conformsTo;
      conformances.push_back(nullptr);
    }
    
    // Build an identity mapping with the derived conformances.
    auto replacement = SubstitutedType::get(archetype, archetype, C);
    subs.push_back({archetype, replacement,
                    C.AllocateCopy(conformances)});
  }
  
  return C.AllocateCopy(subs);
}

bool Lowering::usesObjCAllocator(ClassDecl *theClass) {
  while (true) {
    // If the root class was implemented in Objective-C, use Objective-C's
    // allocation methods because they may have been overridden.
    if (!theClass->hasSuperclass())
      return theClass->hasClangNode();

    theClass = theClass->getSuperclass()->getClassOrBoundGenericClass();
  }
}

void SILGenFunction::emitClassConstructorAllocator(ConstructorDecl *ctor) {
  assert(!ctor->isFactoryInit() && "factories should not be emitted here");
  
  // Emit the prolog. Since we're just going to forward our args directly
  // to the initializer, don't allocate local variables for them.
  RegularLocation Loc(ctor);
  Loc.markAutoGenerated();

  SmallVector<SILValue, 8> args;
  
  // Forward the constructor arguments.
  // FIXME: Handle 'self' along with the other body patterns.
  ArgumentForwardVisitor(*this, args)
    .visit(ctor->getBodyParamPatterns()[1]);

  SILValue selfMetaValue = emitConstructorMetatypeArg(*this, ctor);

  // Allocate the "self" value.
  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  SILType selfTy = getLoweredType(selfDecl->getType());
  assert(selfTy.hasReferenceSemantics() &&
         "can't emit a value type ctor here");

  // Use alloc_ref to allocate the object.
  // TODO: allow custom allocation?
  // FIXME: should have a cleanup in case of exception
  auto selfTypeContext = ctor->getDeclContext()->getDeclaredTypeInContext();
  auto selfClassDecl =
    cast<ClassDecl>(selfTypeContext->getNominalOrBoundGenericNominal());

  SILValue selfValue;

  // Allocate the 'self' value.
  bool useObjCAllocation = usesObjCAllocator(selfClassDecl);

  if (ctor->isConvenienceInit() || ctor->hasClangNode()) {
    // For a convenience initializer or an initializer synthesized
    // for an Objective-C class, allocate using the metatype.
    SILValue allocArg = selfMetaValue;
    
    // When using Objective-C allocation, convert the metatype
    // argument to an Objective-C metatype.
    if (useObjCAllocation) {
      auto metaTy = allocArg.getType().castTo<MetatypeType>();
      metaTy = CanMetatypeType::get(metaTy.getInstanceType(),
                                    MetatypeRepresentation::ObjC);
      allocArg = B.createThickToObjCMetatype(Loc, allocArg,
                                             getLoweredType(metaTy));
    }

    selfValue = B.createAllocRefDynamic(Loc, allocArg, selfTy, 
                                        useObjCAllocation);
  } else {
    // For a designated initializer, we know that the static type being
    // allocated is the type of the class that defines the designated
    // initializer.
    selfValue = B.createAllocRef(Loc, selfTy, useObjCAllocation);
  }
  args.push_back(selfValue);

  // Call the initializer. Always use the Swift entry point, which will be a
  // bridging thunk if we're calling ObjC.
  SILDeclRef initConstant =
    SILDeclRef(ctor,
               SILDeclRef::Kind::Initializer,
               SILDeclRef::ConstructAtBestResilienceExpansion,
               SILDeclRef::ConstructAtNaturalUncurryLevel,
               /*isObjC=*/false);

  ManagedValue initVal;
  SILType initTy;
  
  ArrayRef<Substitution> subs;
  // Call the initializer.
  auto forwardingSubs
    = buildForwardingSubstitutions(ctor->getGenericParamsOfContext());
  std::tie(initVal, initTy, subs)
    = emitSiblingMethodRef(Loc, selfValue, initConstant, forwardingSubs);
  SILType resultTy = getLoweredLoadableType(ctor->getResultType());

  SILValue initedSelfValue
    = B.createApply(Loc, initVal.forward(*this), initTy, resultTy, subs, args,
                    initConstant.isTransparent());

  // Return the initialized 'self'.
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(Loc),
                 initedSelfValue);
}

void SILGenFunction::emitClassConstructorInitializer(ConstructorDecl *ctor) {
  MagicFunctionName = getMagicFunctionName(ctor);
  
  assert(ctor->getBody() && "Class constructor without a body?");

  // True if this constructor delegates to a peer constructor with self.init().
  bool isDelegating = false;
  if (!ctor->hasStubImplementation()) {
    isDelegating = ctor->getDelegatingOrChainedInitKind(nullptr) ==
      ConstructorDecl::BodyInitKind::Delegating;
  }

  // FIXME: The (potentially partially initialized) value here would need to be
  // cleaned up on a constructor failure unwinding, if we were to support
  // failing before total initialization.

  // Set up the 'self' argument.  If this class has a superclass, we set up
  // self as a box.  This allows "self reassignment" to happen in super init
  // method chains, which is important for interoperating with Objective-C
  // classes.  We also use a box for delegating constructors, since the
  // delegated-to initializer may also replace self.
  //
  // TODO: If we could require Objective-C classes to have an attribute to get
  // this behavior, we could avoid runtime overhead here.
  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  auto selfTypeContext = ctor->getDeclContext()->getDeclaredTypeInContext();
  auto selfClassDecl =
    cast<ClassDecl>(selfTypeContext->getNominalOrBoundGenericNominal());
  bool NeedsBoxForSelf = isDelegating ||
    (selfClassDecl->hasSuperclass() && !ctor->hasStubImplementation());
  bool usesObjCAllocator = Lowering::usesObjCAllocator(selfClassDecl);

  // If needed, mark 'self' as uninitialized so that DI knows to
  // enforce its DI properties on stored properties.
  MarkUninitializedInst::Kind MUKind;

  if (isDelegating)
    MUKind = MarkUninitializedInst::DelegatingSelf;
  else if (selfClassDecl->requiresStoredPropertyInits() &&
           usesObjCAllocator) {
    // Stored properties will be initialized in a separate
    // .cxx_construct method called by the Objective-C runtime.
    assert(selfClassDecl->hasSuperclass() &&
           "Cannot use ObjC allocation without a superclass");
    MUKind = MarkUninitializedInst::DerivedSelfOnly;
  } else if (selfClassDecl->hasSuperclass())
    MUKind = MarkUninitializedInst::DerivedSelf;
  else
    MUKind = MarkUninitializedInst::RootSelf;

  if (NeedsBoxForSelf)
    emitLocalVariable(selfDecl, MUKind);

  // Emit the prolog for the non-self arguments.
  // FIXME: Handle self along with the other body patterns.
  emitProlog(ctor->getBodyParamPatterns()[1],
             TupleType::getEmpty(F.getASTContext()), ctor);

  SILType selfTy = getLoweredLoadableType(selfDecl->getType());
  SILValue selfArg = new (SGM.M) SILArgument(F.begin(), selfTy, selfDecl);

  if (!NeedsBoxForSelf) {
    SILLocation PrologueLoc(selfDecl);
    PrologueLoc.markAsPrologue();
    B.createDebugValue(PrologueLoc, selfArg);
  }
  
  if (!ctor->hasStubImplementation()) {
    assert(selfTy.hasReferenceSemantics() &&
           "can't emit a value type ctor here");
    if (NeedsBoxForSelf) {
      SILLocation prologueLoc = RegularLocation(ctor);
      prologueLoc.markAsPrologue();
      B.createStore(prologueLoc, selfArg, VarLocs[selfDecl].value);
    } else {
      selfArg = B.createMarkUninitialized(selfDecl, selfArg, MUKind);
      VarLocs[selfDecl] = VarLoc::get(selfArg);
    }
  }

  // Prepare the end of initializer location.
  SILLocation endOfInitLoc = RegularLocation(ctor);
  endOfInitLoc.pointToEnd();

  // Create a basic block to jump to for the implicit 'self' return.
  // We won't emit the block until after we've emitted the body.
  prepareEpilog(Type(), CleanupLocation::getCleanupLocation(endOfInitLoc));
  
  // If the constructor can fail, set up an alternative epilog for constructor
  // failure.
  SILBasicBlock *failureExitBB = nullptr;
  SILArgument *failureExitArg = nullptr;
  auto &resultLowering = getTypeLowering(ctor->getResultType());

  if (ctor->getFailability() != OTK_None) {
    SILBasicBlock *failureBB = createBasicBlock();
    failureExitBB = createBasicBlock();
    failureExitArg = new (F.getModule())
      SILArgument(failureExitBB, resultLowering.getLoweredType());

    RegularLocation loc(ctor);
    loc.markAutoGenerated();
   
    // On failure, we'll clean up everything (except self, which should already
    // have been released) and return nil instead.
    auto curBB = B.getInsertionBB();
    B.setInsertionPoint(failureBB);
    Cleanups.emitCleanupsForReturn(ctor);
    SILValue nilResult = B.createEnum(loc, {},
                    getASTContext().getOptionalNoneDecl(ctor->getFailability()),
                    resultLowering.getLoweredType());
    B.createBranch(loc, failureExitBB, nilResult);
    
    B.setInsertionPoint(failureExitBB);
    B.createReturn(loc, failureExitArg)->setDebugScope(F.getDebugScope());
    
    B.setInsertionPoint(curBB);
    FailDest = JumpDest(failureBB, Cleanups.getCleanupsDepth(), ctor);
    FailSelfDecl = selfDecl;
  }

  // Handle member initializers.
  if (isDelegating) {
    // A delegating initializer does not initialize instance
    // variables.
  } else if (ctor->hasStubImplementation()) {
    // Nor does a stub implementation.
  } else if (selfClassDecl->requiresStoredPropertyInits() &&
             usesObjCAllocator) {
    // When the class requires all stored properties to have initial
    // values and we're using Objective-C's allocation, stored
    // properties are initialized via the .cxx_construct method, which
    // will be called by the runtime.

    // Note that 'self' has been fully initialized at this point.
  } else {
    // Emit the member initializers.
    emitMemberInitializers(selfDecl, selfClassDecl);
  }

  // Emit the constructor body.
  visit(ctor->getBody());

  // Return 'self' in the epilog.
  Optional<SILValue> maybeReturnValue;
  SILLocation returnLoc(ctor);
  std::tie(maybeReturnValue, returnLoc) = emitEpilogBB(ctor);

  if (!maybeReturnValue)
    return;

  // If we're using a box for self, reload the value at the end of the init
  // method.
  if (NeedsBoxForSelf) {
    // Emit the call to super.init() right before exiting from the initializer.
    if (Expr *SI = ctor->getSuperInitCall())
      emitRValue(SI);

    auto cleanupLoc = CleanupLocation(ctor);
    selfArg = B.createLoad(cleanupLoc, VarLocs[selfDecl].value);
    SILValue selfBox = VarLocs[selfDecl].box;
    assert(selfBox);

    // We have to do a retain because someone else may be using the box.
    B.emitRetainValueOperation(cleanupLoc, selfArg);
    B.emitStrongRelease(cleanupLoc, selfBox);
  }
  
  // Inject the self value into an optional if the constructor is failable.
  switch (ctor->getFailability()) {
  case OTK_None:
    // Not optional.
    break;
      
  case OTK_Optional:
  case OTK_ImplicitlyUnwrappedOptional: {
    RegularLocation loc(ctor);
    loc.markAutoGenerated();
    selfArg = B.createEnum(loc, selfArg,
                   getASTContext().getOptionalSomeDecl(ctor->getFailability()),
                   getLoweredLoadableType(ctor->getResultType()));
    break;
  }
  }
  
  // Return the final 'self'.
  if (failureExitBB) {
    B.createBranch(returnLoc, failureExitBB, selfArg);
  } else {
    B.createReturn(returnLoc, selfArg)
      ->setDebugScope(F.getDebugScope());
  }
}

/// Emit a member initialization for the members described in the
/// given pattern from the given source value.
static void emitMemberInit(SILGenFunction &SGF, VarDecl *selfDecl, 
                           Pattern *pattern, RValue &&src) {
  switch (pattern->getKind()) {
  case PatternKind::Paren:
    return emitMemberInit(SGF, selfDecl, 
                          cast<ParenPattern>(pattern)->getSubPattern(), 
                          std::move(src));

  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(pattern);
    auto fields = tuple->getFields();

    SmallVector<RValue, 4> elements;
    std::move(src).extractElements(elements);
    for (unsigned i = 0, n = fields.size(); i != n; ++i) {
      emitMemberInit(SGF, selfDecl, fields[i].getPattern(), 
                     std::move(elements[i]));
    }
    break;
  }

  case PatternKind::Named: {
    auto named = cast<NamedPattern>(pattern);
    // Form the lvalue referencing this member.
    SILLocation loc = pattern;
    ManagedValue self;
    if (selfDecl->getType()->hasReferenceSemantics())
      self = SGF.emitRValueForDecl(loc, selfDecl, selfDecl->getType(),
                                   AccessSemantics::DirectToStorage,
                                   SGFContext::AllowPlusZero);
    else
      self = SGF.emitLValueForDecl(loc, selfDecl, src.getType(),
                                   AccessKind::Write,
                                   AccessSemantics::DirectToStorage);

    LValue memberRef =
      SGF.emitDirectIVarLValue(loc, self, named->getDecl(), AccessKind::Write);

    // Assign to it.
    SGF.emitAssignToLValue(loc, std::move(src), std::move(memberRef));
    return;
  }
    
  case PatternKind::Any:
    return;

  case PatternKind::Typed:
    return emitMemberInit(SGF, selfDecl, 
                          cast<TypedPattern>(pattern)->getSubPattern(), 
                          std::move(src));

  case PatternKind::Var:
    return emitMemberInit(SGF, selfDecl, 
                          cast<VarPattern>(pattern)->getSubPattern(), 
                          std::move(src));

#define PATTERN(Name, Parent)
#define REFUTABLE_PATTERN(Name, Parent) case PatternKind::Name:
#include "swift/AST/PatternNodes.def"
    llvm_unreachable("Refutable pattern in pattern binding");
  }
}

void SILGenFunction::emitMemberInitializers(VarDecl *selfDecl, 
                                            NominalTypeDecl *nominal) {
  for (auto member : nominal->getMembers()) {
    // Find pattern binding declarations that have initializers.
    auto pbd = dyn_cast<PatternBindingDecl>(member);
    if (!pbd || pbd->isStatic()) continue;

    auto init = pbd->getInit();
    if (!init) continue;

    // Cleanup after this initialization.
    FullExpr scope(Cleanups, pbd->getPattern());
    emitMemberInit(*this, selfDecl, pbd->getPattern(), emitRValue(init));
  }
}

void SILGenFunction::emitIVarInitializer(SILDeclRef ivarInitializer) {
  auto cd = cast<ClassDecl>(ivarInitializer.getDecl());
  RegularLocation loc(cd);
  loc.markAutoGenerated();

  // Emit 'self', then mark it uninitialized.
  auto selfDecl = cd->getDestructor()->getImplicitSelfDecl();
  SILType selfTy = getLoweredLoadableType(selfDecl->getType());
  SILValue selfArg = new (SGM.M) SILArgument(F.begin(), selfTy, selfDecl);
  SILLocation PrologueLoc(selfDecl);
  PrologueLoc.markAsPrologue();
  B.createDebugValue(PrologueLoc, selfArg);
  selfArg = B.createMarkUninitialized(selfDecl, selfArg,
                                      MarkUninitializedInst::RootSelf);
  assert(selfTy.hasReferenceSemantics() && "can't emit a value type ctor here");
  VarLocs[selfDecl] = VarLoc::get(selfArg);

  auto cleanupLoc = CleanupLocation::getCleanupLocation(loc);
  prepareEpilog(TupleType::getEmpty(getASTContext()), cleanupLoc);

  // Emit the initializers.
  emitMemberInitializers(cd->getDestructor()->getImplicitSelfDecl(), cd);

  // Return 'self'.
  B.createReturn(loc, selfArg);

  emitEpilog(loc);
}

void SILGenFunction::emitIVarDestroyer(SILDeclRef ivarDestroyer) {
  auto cd = cast<ClassDecl>(ivarDestroyer.getDecl());
  RegularLocation loc(cd);
  loc.markAutoGenerated();

  SILValue selfValue = emitSelfDecl(cd->getDestructor()->getImplicitSelfDecl());

  auto cleanupLoc = CleanupLocation::getCleanupLocation(loc);
  prepareEpilog(TupleType::getEmpty(getASTContext()), cleanupLoc);
  emitClassMemberDestruction(selfValue, cd, cleanupLoc);
  B.createReturn(loc, emitEmptyTuple(loc));
  emitEpilog(loc);
}

void SILGenFunction::emitClassMemberDestruction(SILValue selfValue,
                                                ClassDecl *cd,
                                                CleanupLocation cleanupLoc) {
  for (VarDecl *vd : cd->getStoredProperties()) {
    const TypeLowering &ti = getTypeLowering(vd->getType());
    if (!ti.isTrivial()) {
      SILValue addr = B.createRefElementAddr(cleanupLoc, selfValue, vd,
                                         ti.getLoweredType().getAddressType());
      B.emitDestroyAddr(cleanupLoc, addr);
    }
  }
}

static void forwardCaptureArgs(SILGenFunction &gen,
                               SmallVectorImpl<SILValue> &args,
                               CaptureInfo::LocalCaptureTy capture,
                               AnyFunctionRef theClosure) {
  ASTContext &c = gen.getASTContext();
  
  auto addSILArgument = [&](SILType t, ValueDecl *d) {
    args.push_back(new (gen.SGM.M) SILArgument(gen.F.begin(), t, d));
  };

  auto *vd = capture.getPointer();
  
  switch (gen.SGM.Types.getDeclCaptureKind(capture, theClosure)) {
  case CaptureKind::None:
    break;

  case CaptureKind::Constant:
    addSILArgument(gen.getLoweredType(vd->getType()), vd);
    break;

  case CaptureKind::Box: {
    SILType ty = gen.getLoweredType(vd->getType()->getRValueType())
      .getAddressType();
    // Forward the captured owning NativeObject.
    addSILArgument(SILType::getNativeObjectType(c), vd);
    // Forward the captured value address.
    addSILArgument(ty, vd);
    break;
  }

  case CaptureKind::NoEscape: {
    SILType ty = gen.getLoweredType(vd->getType()->getRValueType())
      .getAddressType();
    // Forward the captured value address.
    addSILArgument(ty, vd);
    break;
  }
  case CaptureKind::LocalFunction:
    // Forward the captured value.
    addSILArgument(gen.getLoweredType(vd->getType()), vd);
    break;
  case CaptureKind::GetterSetter: {
    // Forward the captured setter.
    Type setTy = cast<AbstractStorageDecl>(vd)->getSetter()->getType();
    addSILArgument(gen.getLoweredType(setTy), vd);
    SWIFT_FALLTHROUGH;
  }
  case CaptureKind::Getter: {
    // Forward the captured getter.
    Type getTy = cast<AbstractStorageDecl>(vd)->getGetter()->getType();
    addSILArgument(gen.getLoweredType(getTy), vd);
    break;
  }
  }
}

static SILValue getNextUncurryLevelRef(SILGenFunction &gen,
                                       SILLocation loc,
                                       SILDeclRef next,
                                       ArrayRef<SILValue> curriedArgs,
                                       ArrayRef<Substitution> curriedSubs) {
  // For a foreign function, reference the native thunk.
  if (next.isForeign)
    return gen.emitGlobalFunctionRef(loc, next.asForeign(false));
  
  // If the fully-uncurried reference is to a native dynamic class method, emit
  // the dynamic dispatch.
  auto fullyAppliedMethod = !next.isCurried && !next.isForeign &&
    next.kind == SILDeclRef::Kind::Func &&
    next.hasDecl();
  
  auto constantInfo = gen.SGM.Types.getConstantInfo(next);
  SILValue thisArg;
  if (!curriedArgs.empty())
      thisArg = curriedArgs.back();
  
  if (fullyAppliedMethod &&
      gen.getMethodDispatch(cast<AbstractFunctionDecl>(next.getDecl()))
        == MethodDispatch::Class) {
    SILValue thisArg = curriedArgs.back();
    
    // Use the dynamic thunk if dynamic.
    if (next.getDecl()->isDynamic()) {
      auto dynamicThunk = gen.SGM.getDynamicThunk(next, constantInfo);
      return gen.B.createFunctionRef(loc, dynamicThunk);
    }
    
    return gen.B.createClassMethod(loc, thisArg, next,
                                   constantInfo.getSILType());
  }
  
  // If the fully-uncurried reference is to a generic method, look up the
  // witness.
  if (fullyAppliedMethod &&
      constantInfo.SILFnType->getAbstractCC() == AbstractCC::WitnessMethod) {
    auto thisType = curriedSubs[0].getReplacement()->getCanonicalType();
    assert(isa<ArchetypeType>(thisType) && "no archetype for witness?!");
    SILValue OpenedExistential;
    if (!cast<ArchetypeType>(thisType)->getOpenedExistentialType().isNull())
      OpenedExistential = thisArg;
    return gen.B.createWitnessMethod(loc, thisType, nullptr, next,
                                     constantInfo.getSILType(),
                                     OpenedExistential);
  }

  // Otherwise, emit a direct call.
  return gen.emitGlobalFunctionRef(loc, next);
}

void SILGenFunction::emitCurryThunk(FuncDecl *fd,
                                    SILDeclRef from, SILDeclRef to) {
  SmallVector<SILValue, 8> curriedArgs;
  
  unsigned paramCount = from.uncurryLevel + 1;
  
  // Forward implicit closure context arguments.
  bool hasCaptures = fd->getCaptureInfo().hasLocalCaptures();
  if (hasCaptures)
    --paramCount;

  // Forward the curried formal arguments.
  auto forwardedPatterns = fd->getBodyParamPatterns().slice(0, paramCount);
  ArgumentForwardVisitor forwarder(*this, curriedArgs);
  for (auto *paramPattern : reversed(forwardedPatterns))
    forwarder.visit(paramPattern);

  // Forward captures.
  if (hasCaptures) {
    SmallVector<CaptureInfo::LocalCaptureTy, 4> LocalCaptures;
    fd->getLocalCaptures(LocalCaptures);
    for (auto capture : LocalCaptures)
      forwardCaptureArgs(*this, curriedArgs, capture, fd);
  }

  // Forward substitutions.
  ArrayRef<Substitution> subs;
  if (auto gp = getConstantInfo(to).ContextGenericParams) {
    subs = buildForwardingSubstitutions(gp);
  }
  
  SILValue toFn = getNextUncurryLevelRef(*this, fd, to, curriedArgs, subs);
  SILType resultTy
    = SGM.getConstantType(from).castTo<SILFunctionType>()
         ->getResult().getSILType();
  resultTy = F.mapTypeIntoContext(resultTy);
  auto toTy = toFn.getType();
  
  // Forward archetypes and specialize if the function is generic.
  if (!subs.empty()) {
    auto toFnTy = toFn.getType().castTo<SILFunctionType>();
    toTy = getLoweredLoadableType(
              toFnTy->substGenericArgs(SGM.M, SGM.SwiftModule, subs));
  }
  
  // Partially apply the next uncurry level and return the result closure.
  auto closureTy =
    SILBuilder::getPartialApplyResultType(toFn.getType(), curriedArgs.size(),
                                          SGM.M, subs);
  SILInstruction *toClosure =
    B.createPartialApply(fd, toFn, toTy, subs, curriedArgs, closureTy);
  if (resultTy != closureTy)
    toClosure = B.createConvertFunction(fd, toClosure, resultTy);
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(fd), toClosure);
}

static SILValue
getThunkedForeignFunctionRef(SILGenFunction &gen,
                             SILLocation loc,
                             SILDeclRef foreign,
                             ArrayRef<ManagedValue> args) {
  assert(!foreign.isCurried
         && "should not thunk calling convention when curried");
  
  // Produce a class_method when thunking ObjC methods.
  auto foreignTy = gen.SGM.getConstantType(foreign);
  if (foreignTy.castTo<SILFunctionType>()->getAbstractCC() == AbstractCC::ObjCMethod) {
    SILValue thisArg = args.back().getValue();
    
    return gen.B.createClassMethod(loc, thisArg, foreign,
                                   gen.SGM.getConstantType(foreign),
                                   /*volatile*/ true);
  }
  // Otherwise, emit a function_ref.
  return gen.emitGlobalFunctionRef(loc, foreign);
}

void SILGenFunction::emitForeignThunk(SILDeclRef thunk) {
  // FIXME: native-to-foreign thunk
  assert(!thunk.isForeign && "native to foreign thunk not implemented");
  
  // Wrap the function in its original form.

  auto fd = cast<AbstractFunctionDecl>(thunk.getDecl());
  auto ci = getConstantInfo(thunk);
  auto resultTy = ci.LoweredInterfaceType->getResult();
  
  // Forward the arguments.
  // FIXME: For native-to-foreign thunks, use emitObjCThunkArguments to retain
  // inputs according to the foreign convention.
  auto forwardedPatterns = fd->getBodyParamPatterns();
  // For allocating constructors, 'self' is a metatype, not the 'self' value
  // formally present in the constructor body.
  Type allocatorSelfType;
  if (thunk.kind == SILDeclRef::Kind::Allocator) {
    allocatorSelfType = forwardedPatterns[0]->getType();
    forwardedPatterns = forwardedPatterns.slice(1);
  }
  
  SmallVector<SILValue, 8> args;
  ArgumentForwardVisitor forwarder(*this, args);
  for (auto *paramPattern : reversed(forwardedPatterns))
    forwarder.visit(paramPattern);
  
  if (allocatorSelfType) {
    auto selfMetatype = CanMetatypeType::get(allocatorSelfType->getCanonicalType(),
                                             MetatypeRepresentation::Thick);
    auto selfArg = new (F.getModule()) SILArgument(
                                 F.begin(),
                                 SILType::getPrimitiveObjectType(selfMetatype),
                                 fd->getImplicitSelfDecl());
    args.push_back(selfArg);
  }
  
  SILValue result;
  {
    CleanupLocation cleanupLoc(fd);
    Scope scope(Cleanups, fd);
    
    SILDeclRef original = thunk.asForeign(!thunk.isForeign);
    auto originalInfo = getConstantInfo(original);
    auto thunkFnTy = ci.getSILType().castTo<SILFunctionType>();
    auto originalFnTy = originalInfo.getSILType().castTo<SILFunctionType>();
    
    // Bridge all the arguments.
    SmallVector<ManagedValue, 8> managedArgs;
    for (unsigned i : indices(args)) {
      auto arg = args[i];
      auto thunkParam = thunkFnTy->getParameters()[i];
      // Bring the argument to +1.
      // TODO: Could avoid a retain if the bridged parameter is also +0 and
      // doesn't require a bridging conversion.
      ManagedValue mv;
      switch (thunkParam.getConvention()) {
      case ParameterConvention::Direct_Owned:
        mv = emitManagedRValueWithCleanup(arg);
        break;
      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Direct_Unowned:
        mv = emitManagedRetain(fd, arg);
        break;
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Guaranteed:
      case ParameterConvention::Indirect_Out:
      case ParameterConvention::Indirect_Inout:
        llvm_unreachable("indirect args in foreign thunked method not implemented");
      }
      
      auto origArg = originalFnTy->getParameters()[i].getSILType();
      
      managedArgs.push_back(emitNativeToBridgedValue(fd, mv, AbstractCC::C,
                                                 mv.getSwiftType(),
                                                 mv.getSwiftType(),
                                                 origArg.getSwiftRValueType()));
    }

    // Call the original.
    auto fn = getThunkedForeignFunctionRef(*this, fd, original,
                                           managedArgs);
    result = emitMonomorphicApply(fd, ManagedValue::forUnmanaged(fn),
                                  managedArgs, resultTy->getCanonicalType())
      .forward(*this);
  }
  // FIXME: use correct convention for native-to-foreign return
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(fd), result);
}

void
SILGenFunction::emitVTableThunk(SILDeclRef derived, SILDeclRef base,
                                ArrayRef<VTableParamThunk> paramThunks,
                                VTableResultThunk resultThunk) {
  auto fd = cast<AbstractFunctionDecl>(derived.getDecl());

  SILLocation loc(fd);
  loc.markAutoGenerated();
  CleanupLocation cleanupLoc(fd);
  cleanupLoc.markAutoGenerated();
  Scope scope(Cleanups, cleanupLoc);

  // Collect the parameters.
  SmallVector<ManagedValue, 8> thunkArgs;
  SILValue indirectReturn;
  for (auto param : F.getLoweredFunctionType()->getParameters()) {
    auto paramTy = F.mapTypeIntoContext(param.getSILType());
    auto arg = new (F.getModule()) SILArgument(F.begin(), paramTy);
    if (param.getConvention() == ParameterConvention::Indirect_Out)
      indirectReturn = arg;
    else
      thunkArgs.push_back(emitManagedRValueWithCleanup(arg));
  }

  auto origFn = SGM.getFunction(derived, NotForDefinition);
  auto origTy = origFn->getLoweredFunctionType();

  // Process the arguments.
  SmallVector<SILValue, 8> origArgs;
  
  // Handle the indirect return, if we have one.
  if (indirectReturn.isValid()) {
    switch (resultThunk) {
    case VTableResultThunk::None:
      // Emit into the indirect return directly.
      origArgs.push_back(indirectReturn);
      break;

    case VTableResultThunk::MakeOptional:
      // Emit into the payload of the optional indirect return.
      OptionalTypeKind OTK;
      indirectReturn.getType().getSwiftRValueType()
        ->getAnyOptionalObjectType(OTK);
      auto payloadTy = F.mapTypeIntoContext(origTy->getSemanticResultSILType());
      auto returnPayload = B.createInitEnumDataAddr(loc, indirectReturn,
                                      getASTContext().getOptionalSomeDecl(OTK),
                                      payloadTy);
      origArgs.push_back(returnPayload);
      break;
    }
  }
  
  auto origParams = origTy->getParametersWithoutIndirectResult();
  for (unsigned i : indices(paramThunks)) {
    switch (paramThunks[i]) {
    // Forward this argument as-is.
    case VTableParamThunk::None:
      origArgs.push_back(thunkArgs[i].forward(*this));
      break;

    // Wrap this argument up in an optional.
    case VTableParamThunk::MakeOptional: {
      OptionalTypeKind OTK;
      origParams[i].getType()->getAnyOptionalObjectType(OTK);
      auto someDecl = getASTContext().getOptionalSomeDecl(OTK);
      auto optTy = F.mapTypeIntoContext(origParams[i].getSILType());
      if (thunkArgs[i].getType().isAddress()) {
        auto buf = emitTemporaryAllocation(loc, optTy);
        auto payload = B.createInitEnumDataAddr(loc, buf, someDecl,
                                                thunkArgs[i].getType());
        B.createCopyAddr(loc, thunkArgs[i].forward(*this), payload,
                         IsTake, IsInitialization);
        B.createInjectEnumAddr(loc, buf, someDecl);
        origArgs.push_back(buf);
      } else {
        auto some = B.createEnum(loc, thunkArgs[i].forward(*this),
                                 someDecl, optTy);
        origArgs.push_back(some);
      }
      break;
    }
    
    // Force-unwrap this optional argument.
    case VTableParamThunk::ForceIUO: {
      auto &tl = getTypeLowering(thunkArgs[i].getType());
      auto payload =
        emitCheckedGetOptionalValueFrom(loc, thunkArgs[i], tl, SGFContext());
      origArgs.push_back(payload.forward(*this));
      break;
    }
    }
  }

  // Call the underlying function.
  auto origFnRef = B.createFunctionRef(loc, origFn);
  auto subs = getForwardingSubstitutions();
  auto substTy = origTy->substGenericArgs(SGM.M, SGM.M.getSwiftModule(), subs);
  SILValue result = B.createApply(loc, origFnRef,
                              SILType::getPrimitiveObjectType(substTy),
                              origTy->getResult().getSILType(), subs, origArgs);

  // Process the result.
  switch (resultThunk) {
  case VTableResultThunk::None:
    break;
  case VTableResultThunk::MakeOptional:
    // Wrap the result in an optional.
    OptionalTypeKind OTK;
    if (indirectReturn) {
      indirectReturn.getType().getSwiftRValueType()
        ->getAnyOptionalObjectType(OTK);

      // We emitted the payload in-place above. We just need to inject
      // the tag.
      B.createInjectEnumAddr(loc, indirectReturn,
                             getASTContext().getOptionalSomeDecl(OTK));
    } else {
      // Wrap up the immediate value in an optional.
      auto thunkResultTy
        = F.getLoweredFunctionType()->getResult().getSILType();
      
      thunkResultTy.getSwiftRValueType()
        ->getAnyOptionalObjectType(OTK);
      
      result = B.createEnum(loc, result,
                            getASTContext().getOptionalSomeDecl(OTK),
                            thunkResultTy);
    }
    break;
  }
  
  scope.pop();
  
  B.createReturn(loc, result);
}

void SILGenFunction::emitGeneratorFunction(SILDeclRef function, Expr *value) {
  MagicFunctionName = getMagicFunctionName(function);
  
  RegularLocation Loc(value);
  Loc.markAutoGenerated();

  // Override location for __FILE__ __LINE__ etc. to an invalid one so that we
  // don't put extra strings into the defaut argument generator function that
  // is not going to be ever used anyway.
  overrideLocationForMagicIdentifiers = SourceLoc();

  emitProlog({ }, value->getType(), function.getDecl()->getDeclContext());
  prepareEpilog(value->getType(), CleanupLocation::getCleanupLocation(Loc));
  emitReturnExpr(Loc, value);
  emitEpilog(Loc);
}

void SILGenFunction::emitLazyGlobalInitializer(PatternBindingDecl *binding) {
  {
    Scope scope(Cleanups, binding);

    // Emit the initialization sequence.
    visit(binding);
  }
  
  // Return void.
  auto ret = emitEmptyTuple(binding);
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(binding), ret);
}

static void emitOnceCall(SILGenFunction &gen, VarDecl *global,
                         SILGlobalVariable *onceToken, SILFunction *onceFunc) {
  SILType rawPointerSILTy
    = gen.getLoweredLoadableType(gen.getASTContext().TheRawPointerType);

  // Emit a reference to the global token.
  SILValue onceTokenAddr = gen.B.createGlobalAddr(global, onceToken);
  onceTokenAddr = gen.B.createAddressToPointer(global, onceTokenAddr,
                                               rawPointerSILTy);

  // Emit a reference to the function to execute once, then thicken
  // that reference as Builtin.once expects.
  SILValue onceFuncRef = gen.B.createFunctionRef(global, onceFunc);
  auto onceFuncThickTy
    = adjustFunctionType(onceFunc->getLoweredFunctionType(),
                         FunctionType::Representation::Thick);
  auto onceFuncThickSILTy = SILType::getPrimitiveObjectType(onceFuncThickTy);
  onceFuncRef = gen.B.createThinToThickFunction(global, onceFuncRef,
                                                onceFuncThickSILTy);

  // Call Builtin.once.
  SILValue onceArgs[] = {onceTokenAddr, onceFuncRef};
  gen.B.createBuiltin(global, gen.getASTContext().getIdentifier("once"),
                      gen.SGM.Types.getEmptyTupleType(), {}, onceArgs);
}

void SILGenFunction::emitGlobalAccessor(VarDecl *global,
                                        SILGlobalVariable *onceToken,
                                        SILFunction *onceFunc) {
  emitOnceCall(*this, global, onceToken, onceFunc);

  // Return the address of the global variable.
  // FIXME: It'd be nice to be able to return a SIL address directly.
  auto *silG = SGM.getSILGlobalVariable(global, NotForDefinition);
  SILValue addr = B.createGlobalAddr(global, silG);

  SILType rawPointerSILTy
    = getLoweredLoadableType(getASTContext().TheRawPointerType);
  addr = B.createAddressToPointer(global, addr, rawPointerSILTy);
  B.createReturn(global, addr);
  if (!MainScope)
    MainScope = F.getDebugScope();
  setDebugScopeForInsertedInstrs(MainScope);
}

void SILGenFunction::emitGlobalGetter(VarDecl *global,
                                      SILGlobalVariable *onceToken,
                                      SILFunction *onceFunc) {
  emitOnceCall(*this, global, onceToken, onceFunc);

  auto *silG = SGM.getSILGlobalVariable(global, NotForDefinition);
  SILValue addr = B.createGlobalAddr(global, silG);

  auto refType = global->getType()->getCanonicalType();
  ManagedValue value = emitLoad(global, addr, getTypeLowering(refType),
                                SGFContext(), IsNotTake);
  SILValue result = value.forward(*this);
  B.createReturn(global, result);
}

RValue RValueEmitter::
visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                   SGFContext C) {
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

  // If the delegated-to initializer can fail, check for the potential failure.
  switch (failability) {
  case OTK_None:
    // Not failable.
    break;

  case OTK_Optional:
  case OTK_ImplicitlyUnwrappedOptional: {
    // Materialize the value so we can pass it indirectly to optional
    // intrinsics.
    auto mat = SGF.emitMaterialize(E, newSelf);
    
    // If the current constructor is not failable, abort.
    switch (ctorDecl->getFailability()) {

    case OTK_Optional:
    case OTK_ImplicitlyUnwrappedOptional: {
      SILBasicBlock *noneBB = SGF.createBasicBlock();
      SILBasicBlock *someBB = SGF.createBasicBlock();

      auto hasValue = SGF.emitDoesOptionalHaveValue(E, mat.address);
      SGF.B.createCondBranch(E, hasValue, someBB, noneBB);
      
      // If the delegate result is nil, clean up and propagate 'nil' from our
      // constructor.
      SGF.B.emitBlock(noneBB);
      assert(SGF.FailDest.isValid() && SGF.FailSelfDecl && "too big to fail");
      // If the delegation consumed self, then our box for 'self' is
      // uninitialized, so we have to deallocate it without triggering a
      // destructor using dealloc_box.
      auto selfBox = SGF.VarLocs[SGF.FailSelfDecl].box;
      assert(selfBox.isValid() && "self not boxed in constructor delegation?!");
      switch (SGF.SelfInitDelegationState) {
      case SILGenFunction::NormalSelf:
        llvm_unreachable("self isn't normal in a constructor delegation");
        
      case SILGenFunction::WillConsumeSelf:
        // We didn't consume, so release the box normally.
        SGF.B.createStrongRelease(E, selfBox);
        break;
      
      case SILGenFunction::DidConsumeSelf:
        // We did consume. Deallocate the box. This is safe because any capture
        // of 'self' prior to full initialization would be a DI violation, so
        // we can count on the box having a retain count of exactly 1 here.
        SGF.B.createDeallocBox(E, SGF.getLoweredType(SGF.FailSelfDecl->getType()),
                               selfBox);
      }
      SGF.Cleanups.emitBranchAndCleanups(SGF.FailDest, E);
      
      // Otherwise, project out the value and carry on.
      SGF.B.emitBlock(someBB);
      
      SWIFT_FALLTHROUGH;
    }
    case OTK_None: {
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

  // We know that self is a box, so get its address.
  SILValue selfAddr =
    SGF.emitLValueForDecl(E, selfDecl, selfTy->getCanonicalType(),
                          AccessKind::Write).getLValueAddress();
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
    cond.enterTrue(SGF.B);
    SGF.Cleanups.emitBranchAndCleanups(SGF.ReturnDest, E, { });
    cond.exitTrue(SGF.B);

    cond.complete(SGF.B);
  }

  return SGF.emitEmptyTupleRValue(E);
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
    auto result = SGF.emitRValue(E->getSubExpr())
      .getAsSingleValue(SGF, E->getSubExpr());
    auto optType = SGF.getLoweredLoadableType(E->getType());
    SILValue bitcast = SGF.B.createUncheckedRefBitCast(E, result.getValue(),
                                                       optType);
    ManagedValue bitcastMV = ManagedValue(bitcast, result.getCleanup());
    return RValue(SGF, E, bitcastMV);
  }

  // Create a buffer for the result.  Abstraction difference will
  // force this to be returned indirectly from
  // _injectValueIntoOptional anyway, so there's not much point
  // avoiding that.
  auto &optTL = SGF.getTypeLowering(E->getType());
  SILValue optAddr = SGF.getBufferForExprResult(E, optTL.getLoweredType(), C);

  SGF.emitInjectOptionalValueInto(E, E->getSubExpr(), optAddr, optTL);

  ManagedValue result = SGF.manageBufferForExprResult(optAddr, optTL, C);
  if (result.isInContext()) return RValue();

  // If we're not address-only, the caller will expect a non-address value.
  if (!optTL.isAddressOnly()) {
    auto optValue = optTL.emitLoadOfCopy(SGF.B, E, result.forward(SGF), IsTake);
    result = SGF.emitManagedRValueWithCleanup(optValue, optTL);
  }
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

namespace {
  /// An Initialization representing the result of an address-only ternary.
  class TernaryInitialization : public SingleBufferInitialization {
    SILValue valueAddr;
  public:
    TernaryInitialization(SILValue valueAddr)
      : valueAddr(valueAddr)
    {}
    
    SILValue getAddressOrNull() const override {
      return valueAddr;
    }
    
    void finishInitialization(SILGenFunction &gen) {
    }
  };
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
    
    cond.enterTrue(SGF.B);
    SILValue trueValue;
    {
      auto TE = E->getThenExpr();
      FullExpr trueScope(SGF.Cleanups, CleanupLocation(TE));
      trueValue = visit(TE).forwardAsSingleValue(SGF, TE);
    }
    cond.exitTrue(SGF.B, trueValue);
    
    cond.enterFalse(SGF.B);
    SILValue falseValue;
    {
      auto EE = E->getElseExpr();
      FullExpr falseScope(SGF.Cleanups, CleanupLocation(EE));
      falseValue = visit(EE).forwardAsSingleValue(SGF, EE);
    }
    cond.exitFalse(SGF.B, falseValue);
    
    SILBasicBlock *cont = cond.complete(SGF.B);
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
    cond.enterTrue(SGF.B);
    {
      auto TE = E->getThenExpr();
      FullExpr trueScope(SGF.Cleanups, CleanupLocation(TE));
      TernaryInitialization init(resultAddr);
      SGF.emitExprInto(TE, &init);
    }
    cond.exitTrue(SGF.B);
    
    cond.enterFalse(SGF.B);
    {
      auto EE = E->getElseExpr();
      FullExpr trueScope(SGF.Cleanups, CleanupLocation(EE));
      TernaryInitialization init(resultAddr);
      SGF.emitExprInto(EE, &init);
    }
    cond.exitFalse(SGF.B);
    
    cond.complete(SGF.B);

    return RValue(SGF, E,
                  SGF.manageBufferForExprResult(resultAddr, lowering, C));
  }
}

RValue RValueEmitter::visitDefaultValueExpr(DefaultValueExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

//
// Bridging
//

static ManagedValue emitBridgeStringToNSString(SILGenFunction &gen,
                                               SILLocation loc,
                                               ManagedValue str) {
  // func _convertStringToNSString(String) -> NSString
  SILValue stringToNSStringFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getStringToNSStringFn());
  
  SILValue nsstr = gen.B.createApply(loc, stringToNSStringFn,
                           stringToNSStringFn.getType(),
                           gen.getLoweredType(gen.SGM.Types.getNSStringType()),
                           {}, str.forward(gen));
  return gen.emitManagedRValueWithCleanup(nsstr);
}

static ManagedValue emitBridgeNSStringToString(SILGenFunction &gen,
                                               SILLocation loc,
                                               ManagedValue nsstr) {
  // func _convertNSStringToString(NSString) -> String
  SILValue nsstringToStringFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getNSStringToStringFn());
  
  SILValue str = gen.B.createApply(loc, nsstringToStringFn,
                    nsstringToStringFn.getType(),
                    gen.getLoweredType(gen.SGM.Types.getStringType()),
                    {}, nsstr.forward(gen));
  
  return gen.emitManagedRValueWithCleanup(str);
}

static ManagedValue emitBridgeArrayToNSArray(SILGenFunction &gen,
                                             SILLocation loc,
                                             ManagedValue arr) {
  SILValue arrToNSArrFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getArrayToNSArrayFn());

  // Figure out the key and value types.
  auto arrTy
    = arr.getType().getSwiftRValueType()->castTo<BoundGenericType>();
  auto subs = arrTy->getSubstitutions(gen.SGM.M.getSwiftModule(), nullptr);
  auto substFnType
    = arrToNSArrFn.getType().substGenericArgs(gen.SGM.M, subs);
  SILValue nsarr = gen.B.createApply(loc, arrToNSArrFn,
                                     substFnType,
                                     gen.SGM.getLoweredType(
                                       gen.SGM.Types.getNSArrayType()),
                                     subs,
                                     { arr.forward(gen) });

  return gen.emitManagedRValueWithCleanup(nsarr);
}

static ManagedValue emitBridgeNSArrayToArray(SILGenFunction &gen,
                                             SILLocation loc,
                                             ManagedValue nsarr,
                                             SILType nativeTy) {
  SILValue nsarrToArrFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getNSArrayToArrayFn());

  auto arrTy = nativeTy.getSwiftRValueType()->castTo<BoundGenericType>();
  auto subs = arrTy->getSubstitutions(gen.SGM.M.getSwiftModule(), nullptr);
  auto substFnType
    = nsarrToArrFn.getType().substGenericArgs(gen.SGM.M, subs);

  SILValue arr = gen.B.createApply(loc, nsarrToArrFn,
                                   substFnType,
                                   nativeTy,
                                   subs,
                                   { nsarr.forward(gen) });

  return gen.emitManagedRValueWithCleanup(arr);
}

static ManagedValue emitBridgeDictionaryToNSDictionary(SILGenFunction &gen,
                                                       SILLocation loc,
                                                       ManagedValue dict) {
  SILValue dictToNSDictFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getDictionaryToNSDictionaryFn());

  // Figure out the key and value types.
  auto dictTy
    = dict.getType().getSwiftRValueType()->castTo<BoundGenericType>();
  auto subs = dictTy->getSubstitutions(gen.SGM.M.getSwiftModule(), nullptr);
  auto substFnType
    = dictToNSDictFn.getType().substGenericArgs(gen.SGM.M, subs);
  SILValue nsdict = gen.B.createApply(loc, dictToNSDictFn,
                                      substFnType,
                                      gen.SGM.getLoweredType(
                                        gen.SGM.Types.getNSDictionaryType()),
                                      subs,
                                      { dict.forward(gen) });

  return gen.emitManagedRValueWithCleanup(nsdict);
}

static ManagedValue emitBridgeNSDictionaryToDictionary(SILGenFunction &gen,
                                                       SILLocation loc,
                                                       ManagedValue nsdict,
                                                       SILType nativeTy) {
  SILValue nsdictToDictFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getNSDictionaryToDictionaryFn());

  auto dictTy = nativeTy.getSwiftRValueType()->castTo<BoundGenericType>();
  auto subs = dictTy->getSubstitutions(gen.SGM.M.getSwiftModule(), nullptr);
  auto substFnType
    = nsdictToDictFn.getType().substGenericArgs(gen.SGM.M, subs);

  SILValue dict = gen.B.createApply(loc, nsdictToDictFn,
                                    substFnType,
                                    nativeTy,
                                    subs,
                                    { nsdict.forward(gen) });

  return gen.emitManagedRValueWithCleanup(dict);
}

static ManagedValue emitBridgeSetToNSSet(SILGenFunction &gen, SILLocation loc,
                                         ManagedValue set) {
  SILValue setToNSSetFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getSetToNSSetFn());

  // Figure out the key and value types.
  auto setTy = set.getType().getSwiftRValueType()->castTo<BoundGenericType>();
  auto subs = setTy->getSubstitutions(gen.SGM.M.getSwiftModule(), nullptr);
  auto substFnType
    = setToNSSetFn.getType().substGenericArgs(gen.SGM.M, subs);
  SILValue nsset = gen.B.createApply(loc, setToNSSetFn,
                                     substFnType,
                                     gen.SGM.getLoweredType(
                                       gen.SGM.Types.getNSSetType()),
                                     subs,
                                     { set.forward(gen) });

  return gen.emitManagedRValueWithCleanup(nsset);
}

static ManagedValue emitBridgeNSSetToSet(SILGenFunction &gen,
                                         SILLocation loc,
                                         ManagedValue nsset,
                                         SILType nativeTy) {
  SILValue nssetToSetFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getNSSetToSetFn());

  auto setTy = nativeTy.getSwiftRValueType()->castTo<BoundGenericType>();
  auto subs = setTy->getSubstitutions(gen.SGM.M.getSwiftModule(), nullptr);
  auto substFnType
    = nssetToSetFn.getType().substGenericArgs(gen.SGM.M, subs);

  SILValue set = gen.B.createApply(loc, nssetToSetFn,
                                   substFnType,
                                   nativeTy,
                                   subs,
                                   { nsset.forward(gen) });

  return gen.emitManagedRValueWithCleanup(set);
}

static ManagedValue emitBridgeBoolToObjCBool(SILGenFunction &gen,
                                             SILLocation loc,
                                             ManagedValue swiftBool) {
  // func _convertBoolToObjCBool(Bool) -> ObjCBool
  SILValue boolToObjCBoolFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getBoolToObjCBoolFn());
  
  SILType resultTy =gen.getLoweredLoadableType(gen.SGM.Types.getObjCBoolType());
  
  SILValue result = gen.B.createApply(loc, boolToObjCBoolFn,
                                      boolToObjCBoolFn.getType(),
                                      resultTy, {}, swiftBool.forward(gen));
  return gen.emitManagedRValueWithCleanup(result);
}

static ManagedValue emitBridgeObjCBoolToBool(SILGenFunction &gen,
                                             SILLocation loc,
                                             ManagedValue objcBool) {
  // func _convertObjCBoolToBool(ObjCBool) -> Bool
  SILValue objcBoolToBoolFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getObjCBoolToBoolFn());
  
  SILType resultTy = gen.getLoweredLoadableType(gen.SGM.Types.getBoolType());
  
  SILValue result = gen.B.createApply(loc, objcBoolToBoolFn,
                                      objcBoolToBoolFn.getType(),
                                      resultTy, {}, objcBool.forward(gen));
  return gen.emitManagedRValueWithCleanup(result);
}

/// Emit an optional-to-optional transformation.
ManagedValue
SILGenFunction::emitOptionalToOptional(SILLocation loc,
                                       ManagedValue input,
                                       SILType resultTy,
                                       const ValueTransform &transformValue) {
  auto isNotPresentBB = createBasicBlock();
  auto isPresentBB = createBasicBlock();
  auto contBB = createBasicBlock();

  // Create a temporary for the output optional.
  auto &resultTL = getTypeLowering(resultTy);
  auto resultTemp = emitTemporaryAllocation(loc, resultTy);

  // Materialize the input.
  SILValue inputTemp;
  if (input.getType().isAddress()) {
    inputTemp = input.forward(*this);
  } else {
    inputTemp = emitTemporaryAllocation(loc, input.getType());
    input.forwardInto(*this, loc, inputTemp);
  }

  // Branch on whether the input is optional.
  auto isPresent = emitDoesOptionalHaveValue(loc, inputTemp);
  B.createCondBranch(loc, isPresent, isPresentBB, isNotPresentBB);

  // If it's present, apply the recursive transformation to the value.
  B.emitBlock(isPresentBB);
  {
    // Don't allow cleanups to escape the conditional block.
    FullExpr presentScope(Cleanups, CleanupLocation::getCleanupLocation(loc));

    CanType resultValueTy =
      resultTy.getSwiftRValueType().getAnyOptionalObjectType();
    assert(resultValueTy);
    SILType loweredResultValueTy = getLoweredType(resultValueTy);

    // Pull the value out.  This will load if the value is not address-only.
    auto &inputTL = getTypeLowering(input.getType());
    auto inputValue = emitUncheckedGetOptionalValueFrom(loc,
                                          ManagedValue::forUnmanaged(inputTemp),
                                          inputTL, SGFContext());

    // Transform it.
    auto resultValue = transformValue(*this, loc, inputValue,
                                      loweredResultValueTy);

    // Inject that into the result type.
    RValueSource resultValueRV(loc, RValue(resultValue, resultValueTy));
    emitInjectOptionalValueInto(loc, std::move(resultValueRV),
                                resultTemp, resultTL);
  }
  B.createBranch(loc, contBB);

  // If it's not present, inject 'nothing' into the result.
  B.emitBlock(isNotPresentBB);
  {
    emitInjectOptionalNothingInto(loc, resultTemp, resultTL);
  }
  B.createBranch(loc, contBB);

  // Continue.
  B.emitBlock(contBB);
  if (resultTL.isAddressOnly()) {
    return emitManagedBufferWithCleanup(resultTemp, resultTL);
  } else {
    return emitLoad(loc, resultTemp, resultTL, SGFContext(), IsTake);
  }
}

namespace {
  // A cleanup that emits a fix_lifetime instruction on a value.
  class FixLifetimeCleanup : public Cleanup {
    SILValue value;
  public:
    FixLifetimeCleanup(SILValue value) : value(value) {}
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      gen.B.emitFixLifetime(l, value);
    }
  };
}

static ManagedValue emitNativeToCBridgedValue(SILGenFunction &gen,
                                              SILLocation loc,
                                              ManagedValue v,
                                              SILType bridgedTy) {
  CanType loweredBridgedTy = bridgedTy.getSwiftRValueType();
  CanType loweredNativeTy = v.getType().getSwiftRValueType();
  if (loweredNativeTy == loweredBridgedTy)
    return v;

  if (loweredNativeTy.getAnyOptionalObjectType()) {
    return gen.emitOptionalToOptional(loc, v, bridgedTy,
                                      emitNativeToCBridgedValue);
  }

  // If the input is a native type with a bridged mapping, convert it.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType,Opt) \
  if (loweredNativeTy == gen.SGM.Types.get##NativeType##Type()              \
      && loweredBridgedTy == gen.SGM.Types.get##BridgedType##Type()) {      \
    return emitBridge##NativeType##To##BridgedType(gen, loc, v);            \
  }
#include "swift/SIL/BridgedTypes.def"

  // Bridge thick to Objective-C metatypes.
  if (auto bridgedMetaTy = dyn_cast<AnyMetatypeType>(loweredBridgedTy)) {
    if (bridgedMetaTy->getRepresentation() == MetatypeRepresentation::ObjC) {
      SILValue native = gen.B.emitThickToObjCMetatype(loc, v.getValue(),
                           SILType::getPrimitiveObjectType(loweredBridgedTy));
      return ManagedValue(native, v.getCleanup());
    }
  }
  
  // Bridge native functions to blocks.
  auto bridgedFTy = dyn_cast<SILFunctionType>(loweredBridgedTy);
  if (bridgedFTy
      && bridgedFTy->getRepresentation() == FunctionType::Representation::Block){
    auto nativeFTy = cast<SILFunctionType>(loweredNativeTy);
    
    if (nativeFTy->getRepresentation() != FunctionType::Representation::Block)
      return emitFuncToBlock(gen, loc, v, bridgedFTy);
  }

  // Bridge Array to NSArray.
  if (auto arrayDecl = gen.getASTContext().getArrayDecl()) {
    if (v.getType().getSwiftRValueType().getAnyNominal() == arrayDecl) {
      return emitBridgeArrayToNSArray(gen, loc, v);
    }
  }

  // Bridge Dictionary to NSDictionary.
  if (auto dictDecl = gen.getASTContext().getDictionaryDecl()) {
    if (v.getType().getSwiftRValueType().getAnyNominal() == dictDecl) {
      return emitBridgeDictionaryToNSDictionary(gen, loc, v);
    }
  }

  // Bridge Set to NSSet.
  if (auto setDecl = gen.getASTContext().getSetDecl()) {
    if (v.getType().getSwiftRValueType().getAnyNominal() == setDecl) {
      return emitBridgeSetToNSSet(gen, loc, v);
    }
  }

  return v;
}

ManagedValue SILGenFunction::emitNativeToBridgedValue(SILLocation loc,
                                                      ManagedValue v,
                                                      AbstractCC destCC,
                                                      CanType origNativeTy,
                                                      CanType substNativeTy,
                                                      CanType loweredBridgedTy){
  switch (destCC) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    // No additional bridging needed for native functions.
    return v;
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    return emitNativeToCBridgedValue(*this, loc, v,
                           SILType::getPrimitiveObjectType(loweredBridgedTy));
  }
  llvm_unreachable("bad CC");
}

static ManagedValue emitCBridgedToNativeValue(SILGenFunction &gen,
                                              SILLocation loc,
                                              ManagedValue v,
                                              SILType nativeTy) {
  CanType loweredNativeTy = nativeTy.getSwiftRValueType();
  CanType loweredBridgedTy = v.getType().getSwiftRValueType();
  if (loweredNativeTy == loweredBridgedTy)
    return v;

  if (loweredNativeTy.getAnyOptionalObjectType()) {
    return gen.emitOptionalToOptional(loc, v, nativeTy,
                                      emitCBridgedToNativeValue);
  }

  // If the output is a bridged type, convert it back to a native type.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType,Opt) \
  if (loweredNativeTy == gen.SGM.Types.get##NativeType##Type() &&           \
      loweredBridgedTy == gen.SGM.Types.get##BridgedType##Type()) {         \
    return emitBridge##BridgedType##To##NativeType(gen, loc, v);            \
  }
#include "swift/SIL/BridgedTypes.def"

  // Bridge Objective-C to thick metatypes.
  if (auto bridgedMetaTy = dyn_cast<AnyMetatypeType>(loweredBridgedTy)){
    if (bridgedMetaTy->getRepresentation() == MetatypeRepresentation::ObjC) {
      SILValue native = gen.B.emitObjCToThickMetatype(loc, v.getValue(),
                                        gen.getLoweredType(loweredNativeTy));
      return ManagedValue(native, v.getCleanup());
    }
  }
  
  // Bridge blocks back into native function types.
  auto bridgedFTy = dyn_cast<SILFunctionType>(loweredBridgedTy);
  if (bridgedFTy
      && bridgedFTy->getRepresentation() == FunctionType::Representation::Block){
    auto nativeFTy = cast<SILFunctionType>(loweredNativeTy);
    
    if (nativeFTy->getRepresentation() != FunctionType::Representation::Block)
      return gen.emitBlockToFunc(loc, v, nativeFTy);
  }

  // Bridge NSArray to Array.
  if (auto arrayDecl = gen.getASTContext().getArrayDecl()) {
    if (nativeTy.getSwiftRValueType()->getAnyNominal() == arrayDecl) {
      return emitBridgeNSArrayToArray(gen, loc, v, nativeTy);
    }
  }

  // Bridge NSDictionary to Dictionary.
  if (auto dictDecl = gen.getASTContext().getDictionaryDecl()) {
    if (nativeTy.getSwiftRValueType()->getAnyNominal() == dictDecl) {
      return emitBridgeNSDictionaryToDictionary(gen, loc, v, nativeTy);
    }
  }

  // Bridge NSSet to Set.
  if (auto setDecl = gen.getASTContext().getSetDecl()) {
    if (nativeTy.getSwiftRValueType()->getAnyNominal() == setDecl) {
      return emitBridgeNSSetToSet(gen, loc, v, nativeTy);
    }
  }
  
  return v;
}

ManagedValue SILGenFunction::emitBridgedToNativeValue(SILLocation loc,
                                                      ManagedValue v,
                                                      AbstractCC srcCC,
                                                      CanType nativeTy) {
  switch (srcCC) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    // No additional bridging needed for native functions.
    return v;

  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    return emitCBridgedToNativeValue(*this, loc, v, getLoweredType(nativeTy));
  }
  llvm_unreachable("bad CC");
}

RValue SILGenFunction::emitEmptyTupleRValue(SILLocation loc) {
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
  return SGF.emitEmptyTupleRValue(E);
}

void SILGenFunction::emitBindOptional(SILLocation loc,
                                      SILValue optionalAddr,
                                      unsigned depth) {
  assert(depth < BindOptionalFailureDests.size());
  auto failureDest = BindOptionalFailureDests[BindOptionalFailureDests.size()
                                                - depth - 1];

  // Check whether the optional has a value.
  SILBasicBlock *hasValueBB = createBasicBlock();
  SILBasicBlock *hasNoValueBB = createBasicBlock();
  SILValue hasValue = emitDoesOptionalHaveValue(loc, optionalAddr);
  B.createCondBranch(loc, hasValue, hasValueBB, hasNoValueBB);

  // If not, thread out through a bunch of cleanups.
  B.emitBlock(hasNoValueBB);
  Cleanups.emitBranchAndCleanups(failureDest, loc);

  // If so, continue.
  B.emitBlock(hasValueBB);
}

RValue RValueEmitter::visitBindOptionalExpr(BindOptionalExpr *E, SGFContext C) {
  // Create a temporary of type Optional<T>.
  auto &optTL = SGF.getTypeLowering(E->getSubExpr()->getType());
  auto temp = SGF.emitTemporary(E, optTL);

  // Emit the operand into the temporary.
  SGF.emitExprInto(E->getSubExpr(), temp.get());

  SILValue addr = temp->getAddress();

  // Branch out if the thing is nil.
  SGF.emitBindOptional(E, addr, E->getDepth());
  
  // If we continued, get the value out as the result of the expression.
  auto optValue = temp->getManagedAddress();
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
  // Allocate a temporary for the Optional<T> if we didn't get one
  // from the context.  This needs to happen outside of the cleanups
  // scope we're about to push.
  auto &optTL = SGF.getTypeLowering(E->getType());

  std::unique_ptr<TemporaryInitialization> optTemp;
  Initialization *optInit = C.getEmitInto();
  bool usingProvidedContext = optInit && optInit->canForwardInBranch();
  if (!usingProvidedContext) {
    optTemp = SGF.emitTemporary(E, optTL);
    optInit = optTemp.get();
  }

  // Enter a cleanups scope.
  FullExpr scope(SGF.Cleanups, E);

  // Install a new optional-failure destination just outside of the
  // cleanups scope.
  SILBasicBlock *failureBB = SGF.createBasicBlock();
  RestoreOptionalFailureDest restoreFailureDest(SGF,
                    JumpDest(failureBB, SGF.Cleanups.getCleanupsDepth(), E));

  // Emit the operand into the temporary.
  SGF.emitExprInto(E->getSubExpr(), optInit);

  // We fell out of the normal result, which generated a T? as either
  // a scalar in subResult or directly into optInit.

  // This concludes the conditional scope.
  scope.pop();

  // Branch to the continuation block.
  SILBasicBlock *contBB = SGF.createBasicBlock();
  SGF.B.createBranch(E, contBB);

  // If control branched to the failure block, inject .None into the
  // result type.
  SGF.B.emitBlock(failureBB);

  // FIXME: reset optInit here?

  SILValue resultAddr = optInit->getAddressOrNull();
  assert(resultAddr || optInit->kind == Initialization::Kind::Ignored);
  if (resultAddr) {
    SGF.emitInjectOptionalNothingInto(E, resultAddr, optTL);
  }

  // FIXME: finish optInit within a conditional scope.

  SGF.B.createBranch(E, contBB);

  // Emit the continuation block.
  SGF.B.emitBlock(contBB);

  // If we emitted into the provided context, we're done.
  if (usingProvidedContext)
    return RValue();

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
    CleanupLocation cleanupLoc = CleanupLocation::getCleanupLocation(loc);
    SILBasicBlock *failureBB;
    JumpDest failureDest(cleanupLoc);

    // Set up an optional-failure scope (which cannot actually return).
    // We can just borrow the enclosing one if we're in a nested context.
    if (numOptionalEvaluations) {
      failureBB = nullptr; // remember that we did this
      failureDest = SGF.BindOptionalFailureDests.back();
    } else {
      failureBB = SGF.createBasicBlock();
      failureDest = JumpDest(failureBB, SGF.Cleanups.getCleanupsDepth(),
                             cleanupLoc);
    }
    RestoreOptionalFailureDest restoreFailureDest(SGF, std::move(failureDest));
    RValue result = emitForceValue(loc, eval->getSubExpr(),
                                   numOptionalEvaluations + 1, C);

    // Emit the failure destination, but only if actually used.
    if (failureBB) {
      if (failureBB->pred_empty()) {
        failureBB->eraseFromParent();
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

RValue RValueEmitter::visitOpenExistentialExpr(OpenExistentialExpr *E, 
                                               SGFContext C) {
  // Emit the existential value.
  ManagedValue existentialValue
    = SGF.emitRValueAsSingleValue(E->getExistentialValue());
  
  // Open the existential value into the opened archetype value.
  SILValue archetypeValue;
  if (existentialValue.getValue().getType().isAddress()) {
    archetypeValue = SGF.B.createOpenExistential(
                       E, existentialValue.forward(SGF),
                       SGF.getLoweredType(E->getOpaqueValue()->getType()));
  } else {
    assert(existentialValue.getValue().getType().isObject());
    archetypeValue = SGF.B.createOpenExistentialRef(
                       E, existentialValue.forward(SGF),
                       SGF.getLoweredType(E->getOpaqueValue()->getType()));
  }
  
  // Register the opaque value for the projected existential.
  SILGenFunction::OpaqueValueRAII opaqueValueRAII(SGF, E->getOpaqueValue(), 
                                                  archetypeValue,
                                                  /*destroy=*/false);

  return visit(E->getSubExpr(), C);
}

RValue RValueEmitter::visitOpaqueValueExpr(OpaqueValueExpr *E, SGFContext C) {
  assert(SGF.OpaqueValues.count(E) && "Didn't bind OpaqueValueExpr");

  auto &entry = SGF.OpaqueValues[E];

  // If the opaque value is uniquely referenced, we can just return the
  // value with a cleanup. There is no need to retain it separately.
  if (E->isUniquelyReferenced()) {
    assert(!entry.second &&"Uniquely-referenced opaque value already consumed");
    entry.second = true;
    return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(entry.first));
  }

  // Retain the value.
  entry.second = true;
  return RValue(SGF, E, SGF.emitManagedRetain(E, entry.first));
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
        CanUnmanagedStorageType::get(typeData.OrigFormalType.getAsType())),
      CanUnmanagedStorageType::get(typeData.SubstFormalType),
      rvalueType);
    lv.add<AutoreleasingWritebackComponent>(unownedTypeData);
    break;
  }
  }
  
  // Get the lvalue address as a raw pointer.
  SILValue address =
    SGF.emitAddressOfLValue(E, std::move(lv), accessKind).getUnmanagedValue();
  address = SGF.B.createAddressToPointer(E, address,
                               SILType::getRawPointerType(SGF.getASTContext()));
  
  // Disable nested writeback scopes for any calls evaluated during the
  // conversion intrinsic.
  InOutConversionScope scope(SGF);
  
  // Invoke the conversion intrinsic.
  auto &Ctx = SGF.getASTContext();
  FuncDecl *converter = Ctx.getConvertInOutToPointerArgument(nullptr);
  Substitution sub = SGF.getPointerSubstitution(E->getType(),
                          converter->getGenericParams()->getAllArchetypes()[0]);
  auto result = SGF.emitApplyOfLibraryIntrinsic(E, converter, sub,
                                        ManagedValue::forUnmanaged(address), C);
  return RValue(SGF, E, result);
}
RValue RValueEmitter::visitArrayToPointerExpr(ArrayToPointerExpr *E,
                                              SGFContext C) {
  auto &Ctx = SGF.getASTContext();
  FuncDecl *converter;
  // Convert the array mutably if it's being passed inout.
  if (E->getSubExpr()->getType()->is<InOutType>()) {
    converter = Ctx.getConvertMutableArrayToPointerArgument(nullptr);
  } else {
    converter = Ctx.getConvertConstArrayToPointerArgument(nullptr);
  }
  auto converterArchetypes = converter->getGenericParams()->getAllArchetypes();

  // Get the original value.
  ManagedValue orig = SGF.emitRValueAsSingleValue(E->getSubExpr());
  
  // Invoke the conversion intrinsic, which will produce an owner-pointer pair.
  Substitution subs[2] = {
    Substitution{
      converterArchetypes[0],
      E->getSubExpr()->getType()->getInOutObjectType()
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
  cond.enterTrue(SGF.B);
  {
    RValueSource source;
    if (E->getSubExpr()->getType()->getAs<LValueType>()) {
      // If the unavailable expression is an lvalue, we will load it.
      auto lval = SGF.emitLValue(unavailExpr, AccessKind::Read);
      ManagedValue loadedValue = SGF.emitLoadOfLValue(loc, std::move(lval), C);
      source = RValueSource(
          loc, RValue(SGF, loc, lval.getSubstFormalType(), loadedValue));
    } else {
      source = RValueSource(unavailExpr);
    }

    SGF.emitInjectOptionalValueInto(loc, std::move(source), allocatedOptional,
                                    SGF.getTypeLowering(silOptType));
  }
  cond.exitTrue(SGF.B);

  cond.enterFalse(SGF.B);
  {
    // If the declaration is not available, inject .None.
    SGF.emitInjectOptionalNothingInto(loc, allocatedOptional,
                                      SGF.getTypeLowering(silOptType));
  }
  cond.exitFalse(SGF.B);
  cond.complete(SGF.B);

  ManagedValue managedValue = SGF.emitLoad(
      loc, allocatedOptional, SGF.getTypeLowering(silOptType), C, IsNotTake);

  return RValue(SGF, E, managedValue);
}

RValue SILGenFunction::emitRValue(Expr *E, SGFContext C) {
  return RValueEmitter(*this).visit(E, C);
}

// Evaluate the expression as an lvalue or rvalue, discarding the result.
void SILGenFunction::emitIgnoredExpr(Expr *E) {
  FullExpr scope(Cleanups, CleanupLocation(E));
  if (E->getType()->is<LValueType>()) {
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
    emitLoadOfLValue(E, std::move(lv), SGFContext::AllowPlusZero);
    return;
  }

  // Otherwise, emit the result (to get any side effects), but produce it at +0
  // if that allows simplification.
  emitRValue(E, SGFContext::AllowPlusZero);
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
