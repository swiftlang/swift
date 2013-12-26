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
#include "Scope.h"
#include "swift/AST/AST.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/type_traits.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebuggerClient.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace Lowering;

void SILDebuggerClient::anchor() {}

namespace {
  class CleanupRValue : public Cleanup {
    const TypeLowering &Lowering;
    SILValue Value;
  public:
    CleanupRValue(const TypeLowering &lowering, SILValue value)
      : Lowering(lowering), Value(value) {}
    
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      Lowering.emitDestroyRValue(gen.B, l, Value);
    }
  };
  
  class CleanupMaterializedValue : public Cleanup {
    SILValue Address;
  public:
    CleanupMaterializedValue(SILValue address)
      : Address(address) {}
    
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      gen.B.emitDestroyAddr(l, Address);
    }
  };
} // end anonymous namespace

SILGenFunction::OpaqueValueRAII::~OpaqueValueRAII() {
  // Destroy the value, unless it was both uniquely referenced and consumed.
  auto entry = Self.OpaqueValues.find(OpaqueValue);
  if (!OpaqueValue->isUniquelyReferenced() || !entry->second.second) {
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
    return ManagedValue(v, ManagedValue::Unmanaged);
  assert(!lowering.isAddressOnly() && "cannot retain an unloadable type");

  v = lowering.emitCopyValue(B, loc, v);
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v) {
  auto &lowering = getTypeLowering(v.getType());
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType() == v.getType());
  if (lowering.isTrivial()) {
    return ManagedValue(v, ManagedValue::Unmanaged);
  } else if (lowering.isAddressOnly()) {
    Cleanups.pushCleanup<CleanupMaterializedValue>(v);
    return ManagedValue(v, getTopCleanup());
  } else {
    Cleanups.pushCleanup<CleanupRValue>(lowering, v);
    return ManagedValue(v, getTopCleanup());
  }
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

  Cleanups.pushCleanup<CleanupMaterializedValue>(v);
  return ManagedValue(v, getTopCleanup());
}

static void destroyRValue(SILGenFunction &SGF, CleanupLocation loc,
                          SILValue value, const TypeLowering &valueTL) {
  if (valueTL.isTrivial()) return;
  if (valueTL.isAddressOnly()) {
    SGF.B.emitDestroyAddr(loc, value);
  } else {
    valueTL.emitDestroyRValue(SGF.B, loc, value);
  }
}

static VarDecl *isLoadPropagatedValue(Expr *SubExpr, SILGenFunction &SGF) {
    // Look through parens.
  while (auto *PE = dyn_cast<ParenExpr>(SubExpr))
    SubExpr = PE->getSubExpr();
  
  // If this is a load of a local constant decl, just produce the value.
  if (auto *DRE = dyn_cast<DeclRefExpr>(SubExpr)) {
    if (auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) {
      // FIXME: This should be a bit on vardecl, not presence in VarLocs.
      auto It = SGF.VarLocs.find(VD);
      if (It != SGF.VarLocs.end() && It->second.isConstant())
        return VD;
    }
  }
  
  // If this is a use of super that is a constant, just produce the value.
  if (auto *SRE = dyn_cast<SuperRefExpr>(SubExpr)) {
    if (auto *VD = dyn_cast<VarDecl>(SRE->getSelf())) {
      // FIXME: This should be a bit on vardecl, not presence in VarLocs.
      auto It = SGF.VarLocs.find(VD);
      if (It != SGF.VarLocs.end() && It->second.isConstant())
        return VD;
    }
  }
  return nullptr;
}


void SILGenFunction::emitExprInto(Expr *E, Initialization *I) {
  // Handle the special case of copying an lvalue.
  if (auto load = dyn_cast<LoadExpr>(E))
    if (!isLoadPropagatedValue(load->getSubExpr(), *this)) {
      auto lv = emitLValue(load->getSubExpr());
      emitCopyLValueInto(E, lv, I);
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
             "RValueEmitter shouldn't be called on lvalues");
      return visit(E, SGFContext());
    }

    // These always produce lvalues.
    RValue visitAddressOfExpr(AddressOfExpr *E, SGFContext C) {
      abort();
    }
    RValue visitMaterializeExpr(MaterializeExpr *E, SGFContext C) {
      abort();
    }
    RValue visitSubscriptExpr(SubscriptExpr *E, SGFContext C);
    
    RValue visitApplyExpr(ApplyExpr *E, SGFContext C);

    RValue visitDiscardAssignmentExpr(DiscardAssignmentExpr *E, SGFContext C);
    RValue visitDeclRefExpr(DeclRefExpr *E, SGFContext C);
    RValue visitSuperRefExpr(SuperRefExpr *E, SGFContext C);
    RValue visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E,
                                            SGFContext C);
  
    RValue visitIntegerLiteralExpr(IntegerLiteralExpr *E, SGFContext C);
    RValue visitFloatLiteralExpr(FloatLiteralExpr *E, SGFContext C);
    RValue visitCharacterLiteralExpr(CharacterLiteralExpr *E, SGFContext C);
        
    RValue emitStringLiteral(Expr *E, StringRef Str, SGFContext C);
        
    RValue visitStringLiteralExpr(StringLiteralExpr *E, SGFContext C);
    RValue visitLoadExpr(LoadExpr *E, SGFContext C);
    RValue visitDerivedToBaseExpr(DerivedToBaseExpr *E, SGFContext C);
    RValue visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                       SGFContext C);
    RValue visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E, SGFContext C);
    RValue visitRequalifyExpr(RequalifyExpr *E, SGFContext C);
    RValue visitFunctionConversionExpr(FunctionConversionExpr *E,
                                       SGFContext C);
    RValue visitErasureExpr(ErasureExpr *E, SGFContext C);
    RValue visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *E,
                                           SGFContext C);
    RValue visitIsaExpr(IsaExpr *E, SGFContext C);
    RValue visitCoerceExpr(CoerceExpr *E, SGFContext C);
    RValue visitTupleExpr(TupleExpr *E, SGFContext C);
    RValue visitScalarToTupleExpr(ScalarToTupleExpr *E, SGFContext C);
    RValue visitMemberRefExpr(MemberRefExpr *E, SGFContext C);
    RValue visitDynamicMemberRefExpr(DynamicMemberRefExpr *E, SGFContext C);
    RValue visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E,
                                       SGFContext C);
    RValue visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E,
                                         SGFContext C);
    RValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E,
                                         SGFContext C);
    RValue visitModuleExpr(ModuleExpr *E, SGFContext C);
    RValue visitTupleElementExpr(TupleElementExpr *E, SGFContext C);
    RValue visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E,
                                       SGFContext C);
    RValue visitDynamicSubscriptExpr(DynamicSubscriptExpr *E,
                                     SGFContext C);
    RValue visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E,
                                         SGFContext C);
    RValue visitTupleShuffleExpr(TupleShuffleExpr *E, SGFContext C);
    RValue visitNewArrayExpr(NewArrayExpr *E, SGFContext C);
    RValue visitMetatypeExpr(MetatypeExpr *E, SGFContext C);
    RValue visitClosureExpr(ClosureExpr *E, SGFContext C);
    RValue visitAutoClosureExpr(AutoClosureExpr *E, SGFContext C);
    RValue visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                              SGFContext C);
    RValue visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E,
                                           SGFContext C);
    RValue visitCollectionExpr(CollectionExpr *E, SGFContext C);
    RValue visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E,
                                            SGFContext C);
    RValue visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *E, SGFContext C);
    RValue visitBridgeToBlockExpr(BridgeToBlockExpr *E, SGFContext C);
    RValue visitIfExpr(IfExpr *E, SGFContext C);
    RValue visitDefaultValueExpr(DefaultValueExpr *E, SGFContext C);
    RValue visitAssignExpr(AssignExpr *E, SGFContext C);

    RValue visitBindOptionalExpr(BindOptionalExpr *E, SGFContext C);
    RValue visitOptionalEvaluationExpr(OptionalEvaluationExpr *E,
                                       SGFContext C);
    RValue visitForceValueExpr(ForceValueExpr *E, SGFContext C);

    RValue visitOpaqueValueExpr(OpaqueValueExpr *E, SGFContext C);

    RValue emitUnconditionalCheckedCast(Expr *source,
                                        SILLocation loc,
                                        Type destType,
                                        CheckedCastKind castKind,
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
  if (constant.hasDecl() &&
      isa<BuiltinUnit>(constant.getDecl()->getDeclContext())) {
    return B.createBuiltinFunctionRef(loc, constant.getDecl()->getName(),
                                      constantInfo.getSILType());
  }
  
  // If the constant is a curry thunk we haven't emitted yet, emit it.
  if (constant.isCurried) {
    if (!SGM.hasFunction(constant)) {
      // Non-functions can't be referenced uncurried.
      FuncDecl *fd = cast<FuncDecl>(constant.getDecl());
      
      // Getters and setters can't be referenced uncurried.
      assert(!fd->isGetterOrSetter());
      
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
  
  return B.createFunctionRef(loc, SGM.getFunction(constant));
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
  return ManagedValue(c, ManagedValue::Unmanaged);
}

/// True if the global stored property requires lazy initialization.
static bool isGlobalLazilyInitialized(VarDecl *var) {
  assert(!var->getDeclContext()->isLocalContext() &&
         "not a global variable!");
  assert(!var->isComputed() &&
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
                            SILDeclRef(var, SILDeclRef::Kind::GlobalAccessor));
    SILValue accessor = gen.B.createFunctionRef(loc, accessorFn);
    SILValue addr = gen.B.createApply(loc, accessor, accessor.getType(),
                              accessor.getType().castTo<SILFunctionType>()
                                      ->getResult().getSILType(),
                              {}, {});
    // FIXME: It'd be nice if the result of the accessor was natively an address.
    addr = gen.B.createPointerToAddress(loc, addr,
                          gen.getLoweredType(var->getType()).getAddressType());
    return ManagedValue(addr, ManagedValue::LValue);
  }
  
  // Global variables in main source files can be accessed directly.
  // FIXME: And all global variables when lazy initialization is disabled.
  SILValue addr = gen.B.createGlobalAddr(loc, var,
                          gen.getLoweredType(var->getType()).getAddressType());
  return ManagedValue(addr, ManagedValue::LValue);
}

ManagedValue SILGenFunction::emitReferenceToDecl(SILLocation loc,
                                                 ConcreteDeclRef declRef,
                                                 Type ncRefType,
                                                 unsigned uncurryLevel) {
  ValueDecl *decl = declRef.getDecl();
  if (!ncRefType) ncRefType = decl->getType();
  CanType refType = ncRefType->getCanonicalType();
  
  // If this is a reference to a type, produce a metatype.
  if (isa<TypeDecl>(decl)) {
    assert(!declRef.isSpecialized() &&
           "Cannot handle specialized type references");
    assert(decl->getType()->is<MetatypeType>() &&
           "type declref does not have metatype type?!");
    assert((uncurryLevel == SILDeclRef::ConstructAtNaturalUncurryLevel
            || uncurryLevel == 0)
           && "uncurry level doesn't make sense for types");
    return ManagedValue(B.createMetatype(loc, getLoweredType(refType)),
                        ManagedValue::Unmanaged);
  }
  
  // If this is a reference to a var, produce an address or value.
  if (auto *var = dyn_cast<VarDecl>(decl)) {
    if (var->isDebuggerVar()) {
      DebuggerClient *DebugClient = SGM.SwiftModule->getDebugClient();
      assert (DebugClient && "Debugger variables with no debugger client");
      SILDebuggerClient *SILDebugClient = DebugClient->getAsSILDebuggerClient();
      assert (SILDebugClient && "Debugger client doesn't support SIL");
      return ManagedValue(SILDebugClient->emitReferenceToDecl(loc, declRef,
                                                              ncRefType,
                                                              uncurryLevel, B),
                          ManagedValue::Unmanaged);
    }

    assert(!declRef.isSpecialized() &&
           "Cannot handle specialized variable references");
    assert((uncurryLevel == SILDeclRef::ConstructAtNaturalUncurryLevel
            || uncurryLevel == 0)
           && "uncurry level doesn't make sense for vars");

    // For local decls, use the address we allocated or the value if we have it.
    auto It = VarLocs.find(decl);
    if (It != VarLocs.end()) {
      if (It->second.isConstant()) {
        SILValue V = It->second.getConstant();
        auto &TL = getTypeLowering(refType);
        // The value must be copied for the duration of the expression.
        V = TL.emitCopyValue(B, loc, V);
        return emitManagedRValueWithCleanup(V);
      }

      return ManagedValue(It->second.getAddress(), ManagedValue::LValue);
    }

    if (var->isComputed()) {
      assert(!var->isSettable() &&
             "computed lvalue decls are handled by lvalue machinery");
      // Global properties have no base or subscript.
      
      SILDeclRef getter(var, SILDeclRef::Kind::Getter,
                        SILDeclRef::ConstructAtNaturalUncurryLevel,
                        SGM.requiresObjCDispatch(var));
      
      return emitGetAccessor(loc, getter,
                             ArrayRef<Substitution>(),
                             RValueSource(),
                             RValueSource(), ncRefType->getCanonicalType(),
                             SGFContext());
    }
    
    // If this is a global variable, invoke its accessor function to get its
    // address.
    return emitGlobalVariableRef(*this, loc, var);
  }
  
  // If the referenced decl isn't a VarDecl, it should be a constant of some
  // sort.

  // If the referenced decl is a local func with context, then the SILDeclRef
  // uncurry level is one deeper (for the context vars).
  bool hasLocalCaptures = false;
  if (auto *fd = dyn_cast<FuncDecl>(decl)) {
    hasLocalCaptures = fd->getCaptureInfo().hasLocalCaptures();
    if (hasLocalCaptures &&
        uncurryLevel != SILDeclRef::ConstructAtNaturalUncurryLevel)
      ++uncurryLevel;
  }

  auto silDeclRef = SILDeclRef(decl, uncurryLevel);
  auto constantInfo = getConstantInfo(silDeclRef);

  unsigned uncurryLevelWithoutCaptures =
    silDeclRef.uncurryLevel - unsigned(hasLocalCaptures);

  ManagedValue result = emitFunctionRef(loc, silDeclRef, constantInfo);

  // Get the lowered AST types:
  //  - the original type
  auto origLoweredFormalType = AbstractionPattern(constantInfo.LoweredType);
  if (hasLocalCaptures) {
    auto formalTypeWithoutCaptures =
      cast<AnyFunctionType>(constantInfo.FormalType.getResult());
    origLoweredFormalType =
      AbstractionPattern(
              SGM.Types.getLoweredASTFunctionType(formalTypeWithoutCaptures,
                                                  uncurryLevelWithoutCaptures));
  }

  // - the substituted type
  auto substFormalType = cast<AnyFunctionType>(refType);
  auto substLoweredFormalType =
    SGM.Types.getLoweredASTFunctionType(substFormalType,
                                        uncurryLevelWithoutCaptures);

  // If the declaration reference is specialized, create the partial
  // application.
  if (declRef.isSpecialized()) {
    // Substitute the function type.
    auto origFnType = result.getType().castTo<SILFunctionType>();
    auto substFnType = origFnType->substGenericArgs(SGM.M, SGM.SwiftModule,
                                                    declRef.getSubstitutions());
    auto closureType = getThickFunctionType(substFnType);

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

RValue RValueEmitter::visitDiscardAssignmentExpr(DiscardAssignmentExpr *E,
                                                 SGFContext C) {
  llvm_unreachable("cannot appear in rvalue");
}

RValue RValueEmitter::visitDeclRefExpr(DeclRefExpr *E, SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");

  // Emit the reference to the decl.
  ManagedValue Reference =
    SGF.emitReferenceToDecl(E, E->getDeclRef(), E->getType(), 0);
  
  // If it comes back as an LValue-style RValue, emit a load to get a real
  // RValue.
  if (Reference.isLValue())
    Reference = SGF.emitLoad(E, Reference.getUnmanagedValue(),
                             SGF.getTypeLowering(E->getType()), C, IsNotTake);
  
  return RValue(SGF, E, Reference);
}


RValue RValueEmitter::visitSuperRefExpr(SuperRefExpr *E, SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");
  return RValue(SGF, E,
                SGF.emitReferenceToDecl(E, E->getSelf(), E->getType(), 0));
}

RValue RValueEmitter::visitOtherConstructorDeclRefExpr(
                                OtherConstructorDeclRefExpr *E, SGFContext C) {
  // This should always be a child of an ApplyExpr and so will be emitted by
  // SILGenApply.
  llvm_unreachable("unapplied reference to constructor?!");
}

RValue RValueEmitter::visitIntegerLiteralExpr(IntegerLiteralExpr *E,
                                               SGFContext C) {
  return RValue(SGF, E, ManagedValue(SGF.B.createIntegerLiteral(E),
                                     ManagedValue::Unmanaged));
}
RValue RValueEmitter::visitFloatLiteralExpr(FloatLiteralExpr *E,
                                             SGFContext C) {
  return RValue(SGF, E, ManagedValue(SGF.B.createFloatLiteral(E),
                                     ManagedValue::Unmanaged));
}
RValue RValueEmitter::visitCharacterLiteralExpr(CharacterLiteralExpr *E,
                                                 SGFContext C) {
  return RValue(SGF, E, ManagedValue(SGF.B.createIntegerLiteral(E),
                                     ManagedValue::Unmanaged));
}

RValue RValueEmitter::emitStringLiteral(Expr *E, StringRef Str,
                                        SGFContext C) {
  StringLiteralInst *string = SGF.B.createStringLiteral(E, Str);
  CanType ty = E->getType()->getCanonicalType();
  
  ManagedValue Elts[] = {
    ManagedValue(SILValue(string, 0), ManagedValue::Unmanaged),
    ManagedValue(SILValue(string, 1), ManagedValue::Unmanaged),
    ManagedValue(SILValue(string, 2), ManagedValue::Unmanaged)
  };
  
  return RValue(Elts, ty);
}


RValue RValueEmitter::visitStringLiteralExpr(StringLiteralExpr *E,
                                             SGFContext C) {
  return emitStringLiteral(E, E->getValue(), C);
}

RValue RValueEmitter::visitLoadExpr(LoadExpr *E, SGFContext C) {
  // If we can and must fold this, do so.
  if (VarDecl *VD = isLoadPropagatedValue(E->getSubExpr(), SGF)) {
    auto Entry = SGF.VarLocs[VD];
    assert(Entry.isConstant() && "Not a load propagated vardecl");
    SILValue V = Entry.getConstant();

    auto &TL = SGF.getTypeLowering(VD->getType());
    // The value must be copied for the duration of the expression.
    V = TL.emitCopyValue(SGF.B, E, V);
    return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(V, TL));
  }

  LValue lv = SGF.emitLValue(E->getSubExpr());
  auto result = SGF.emitLoadOfLValue(E, lv, C);
  return (result ? RValue(SGF, E, result) : RValue());
}

SILValue SILGenFunction::emitTemporaryAllocation(SILLocation loc,
                                                 SILType ty) {
  ty = ty.getObjectType();
  auto alloc = B.createAllocStack(loc, ty);
  enterDeallocStackCleanup(loc, alloc->getContainerResult());
  return alloc->getAddressResult();
}

SILValue SILGenFunction::getBufferForExprResult(
                                    SILLocation loc, SILType ty, SGFContext C) {
  // If you change this, change manageBufferForExprResult below as well.

  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (Initialization *I = C.getEmitInto()) {
    switch (I->kind) {
    case Initialization::Kind::AddressBinding:
      llvm_unreachable("can't emit into address binding");
    case Initialization::Kind::LetValue:
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
      return I->getAddress();
    }
  }
  
  // If we couldn't emit into an Initialization, emit into a temporary
  // allocation.
  return emitTemporaryAllocation(loc, ty.getObjectType());
}

ManagedValue SILGenFunction::manageBufferForExprResult(SILValue buffer,
                                                 const TypeLowering &bufferTL,
                                                       SGFContext C) {
  if (Initialization *I = C.getEmitInto()) {
    switch (I->kind) {
    case Initialization::Kind::AddressBinding:
      llvm_unreachable("can't emit into address binding");
    case Initialization::Kind::Ignored:
    case Initialization::Kind::Translating:
    case Initialization::Kind::Tuple:
    case Initialization::Kind::LetValue:
      break;
    case Initialization::Kind::SingleBuffer:
      I->finishInitialization(*this);
      return ManagedValue();
    }
  }

  // Add a cleanup for the temporary we allocated.
  if (bufferTL.isTrivial())
    return ManagedValue::forUnmanaged(buffer);

  Cleanups.pushCleanup<CleanupMaterializedValue>(buffer);
  return ManagedValue(buffer, getTopCleanup());
}

Materialize SILGenFunction::emitMaterialize(SILLocation loc, ManagedValue v) {
  assert(!v.isLValue() && "materializing an lvalue?!");
  // Address-only values are already materialized.
  if (v.getType().isAddress()) {
    assert(v.getType().isAddressOnly(SGM.M) && "can't materialize an l-value");
    return Materialize{v.getValue(), v.getCleanup()};
  }

  auto &lowering = getTypeLowering(v.getType().getSwiftType());
  
  // We don't use getBufferForExprResult here because the result of a
  // MaterializeExpr is *not* the value, but an lvalue reference to the value.
  SILValue tmpMem = emitTemporaryAllocation(loc, v.getType());
  v.forwardInto(*this, loc, tmpMem);
  
  CleanupHandle valueCleanup = CleanupHandle::invalid();
  if (!lowering.isTrivial()) {
    Cleanups.pushCleanup<CleanupMaterializedValue>(tmpMem);
    valueCleanup = getTopCleanup();
  }
  
  return Materialize{tmpMem, valueCleanup};
}

RValue RValueEmitter::visitDerivedToBaseExpr(DerivedToBaseExpr *E,
                                             SGFContext C) {
  ManagedValue original = visit(E->getSubExpr()).getAsSingleValue(SGF,
                                                              E->getSubExpr());
  SILValue converted = SGF.B.createUpcast(E,
                                   original.getValue(),
                                   SGF.getLoweredType(E->getType()));
  return RValue(SGF, E, ManagedValue(converted, original.getCleanup()));
}

RValue RValueEmitter::visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                                   SGFContext C) {
  SILValue metaBase = visit(E->getSubExpr()).getUnmanagedSingleValue(SGF,
                                                              E->getSubExpr());
  return RValue(SGF, E,
                ManagedValue(SGF.B.createUpcast(E, metaBase,
                                    SGF.getLoweredLoadableType(E->getType())),
                             ManagedValue::Unmanaged));
}

RValue RValueEmitter::visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E,
                                                 SGFContext C) {
  ManagedValue archetype = visit(E->getSubExpr()).getAsSingleValue(SGF,
                                                              E->getSubExpr());
  // Replace the cleanup with a new one on the superclass value so we always use
  // concrete retain/release operations.
  SILValue base = SGF.B.createArchetypeRefToSuper(E,
                                    archetype.forward(SGF),
                                    SGF.getLoweredLoadableType(E->getType()));
  return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(base));
}

RValue RValueEmitter::visitRequalifyExpr(RequalifyExpr *E, SGFContext C) {
  assert(E->getType()->is<LValueType>() && "non-lvalue requalify");
  // Ignore lvalue qualifiers.
  return visit(E->getSubExpr());
}

RValue RValueEmitter::visitFunctionConversionExpr(FunctionConversionExpr *e,
                                                  SGFContext C)
{
  ManagedValue original = visit(e->getSubExpr()).getAsSingleValue(SGF,
                                                              e->getSubExpr());
  
  // Retain the thinness of the original function type.
  CanAnyFunctionType destTy =
    cast<AnyFunctionType>(e->getType()->getCanonicalType());
  if (original.getType().castTo<SILFunctionType>()->isThin())
    destTy = getThinFunctionType(destTy);

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
    
    SILValue getAddressOrNull() override {
      return valueAddr;
    }
    
    void finishInitialization(SILGenFunction &gen) {
      // FIXME: Disable the DeinitExistential cleanup and enable the
      // DestroyAddr cleanup for the existential container.
    }
  };
}

static RValue emitClassBoundErasure(SILGenFunction &gen, ErasureExpr *E) {
  ManagedValue sub = gen.emitRValue(E->getSubExpr()).getAsSingleValue(gen,
                                                              E->getSubExpr());
  SILType resultTy = gen.getLoweredLoadableType(E->getType());
  
  SILValue v;
  
  if (E->getSubExpr()->getType()->isExistentialType())
    // If the source value is already of protocol type, we can use
    // upcast_existential_ref to steal the already-initialized witness tables
    // and concrete value.
    v = gen.B.createUpcastExistentialRef(E, sub.getValue(), resultTy);
  else
    // Otherwise, create a new existential container value around the class
    // instance.
    v = gen.B.createInitExistentialRef(E, resultTy, sub.getValue(),
                                       E->getConformances());

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
  
  if (E->getSubExpr()->getType()->isExistentialType()) {
    // If the source value is already of a protocol type, we can use
    // upcast_existential to steal its already-initialized witness tables and
    // concrete value.
    ManagedValue subExistential
      = gen.emitRValue(E->getSubExpr()).getAsSingleValue(gen, E->getSubExpr());

    IsTake_t isTake = IsTake_t(subExistential.hasCleanup());

    gen.B.createUpcastExistential(E, subExistential.getValue(), existential,
                                  isTake);
  } else {
    // Otherwise, we need to initialize a new existential container from
    // scratch.
    
    // Allocate the concrete value inside the container.
    SILValue valueAddr = gen.B.createInitExistential(E, existential,
                                gen.getLoweredType(E->getSubExpr()->getType()),
                                E->getConformances());
    // Initialize the concrete value in-place.
    InitializationPtr init(new ExistentialValueInitialization(valueAddr));
    gen.emitExprInto(E->getSubExpr(), init.get());
  }

  auto result = gen.manageBufferForExprResult(existential, existentialTL, C);
  return (result ? RValue(gen, E, result) : RValue());
}

RValue RValueEmitter::visitErasureExpr(ErasureExpr *E, SGFContext C) {
  if (E->getType()->isClassExistentialType())
    return emitClassBoundErasure(SGF, E);
  return emitAddressOnlyErasure(SGF, E, C);
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
}

SILValue SILGenFunction::emitUnconditionalCheckedCast(SILLocation loc,
                                                      SILValue original,
                                                      Type origTy,
                                                      Type castTy,
                                                      CheckedCastKind kind) {
  auto &origLowering = getTypeLowering(origTy);
  auto &castLowering = getTypeLowering(castTy);

  // Spill to a temporary if casting from loadable to address-only.
  if (origLowering.isLoadable() && castLowering.isAddressOnly()) {
    SILValue temp = emitTemporaryAllocation(loc, origLowering.getLoweredType());
    B.createStore(loc, original, temp);
    original = temp;
  }
  
  // Get the cast destination type at the least common denominator abstraction
  // level.
  SILType destTy = castLowering.getLoweredType();
  if (origLowering.isAddressOnly())
    destTy = destTy.getAddressType();

  // Emit the cast.
  SILValue result = B.createUnconditionalCheckedCast(loc, kind, original,
                                                     destTy);
  
  // Load from the address if casting from address-only to loadable.
  if (origLowering.isAddressOnly() && castLowering.isLoadable())
    result = B.createLoad(loc, result);
  
  return result;
}

SILValue
SILGenFunction::emitCheckedCastAbstractionChange(SILLocation loc,
                                       SILValue original,
                                       const TypeLowering &origTL,
                                       ArrayRef<const TypeLowering *> castTLs) {
  // If the original type is already address-only, we don't need to abstract
  // further.
  if (origTL.isAddressOnly()) {
    return SILValue();
  }
  
  // If any of the cast-to types are address-only, spill to a temporary.
  if (std::find_if(castTLs.begin(), castTLs.end(),
                   [](const TypeLowering *tl){ return tl->isAddressOnly(); })
        != castTLs.end()) {
    SILValue temp = emitTemporaryAllocation(loc, origTL.getLoweredType());
    B.createStore(loc, original, temp);
    return temp;
  }
  
  // Otherwise, no abstraction change is needed.
  return SILValue();
}

std::pair<SILBasicBlock*, SILBasicBlock*>
SILGenFunction::emitCheckedCastBranch(SILLocation loc,
                                      SILValue original,
                                      SILValue originalAbstracted,
                                      const TypeLowering &origLowering,
                                      const TypeLowering &castLowering,
                                      CheckedCastKind kind) {
  // Spill to a temporary if casting from loadable to address-only.
  if (origLowering.isLoadable() && castLowering.isAddressOnly()) {
    assert(originalAbstracted && "no abstracted value for cast");
    original = originalAbstracted;
  }
  
  // Get the cast destination type at the least common denominator abstraction
  // level.
  SILType destTy = castLowering.getLoweredType();
  if (origLowering.isAddressOnly())
    destTy = destTy.getAddressType();

  // Set up BBs for the cast.
  auto success = createBasicBlock();
  new (SGM.M) SILArgument(destTy, success);
  auto failure = createBasicBlock();

  // Emit the cast.
  B.createCheckedCastBranch(loc, kind, original, destTy,
                            success, failure);
  
  return {success, failure};
}

RValue RValueEmitter::visitConditionalCheckedCastExpr(
                                                  ConditionalCheckedCastExpr *E,
                                                  SGFContext C) {
  ManagedValue original = visit(E->getSubExpr()).getAsSingleValue(SGF,
                                                              E->getSubExpr());

  SILBasicBlock *contBB = SGF.createBasicBlock();

  // The optional injection intrinsics return indirectly regardless of
  // whether the result is address-only, so go ahead and always emit
  // to a temporary.
  auto &resultTL = SGF.getTypeLowering(E->getType());
  SILValue resultBuffer =
    SGF.getBufferForExprResult(E, resultTL.getLoweredType(), C);

  SILValue origVal = original.forward(SGF);
  SILBasicBlock *success, *failure;
  auto &origTL = SGF.getTypeLowering(E->getSubExpr()->getType());
  auto castTy = E->getCastTypeLoc().getType()->getCanonicalType();
  auto &castTL = SGF.getTypeLowering(castTy);
  SILValue origAbs = SGF.emitCheckedCastAbstractionChange(E, origVal,
                                                          origTL,
                                                          &castTL);
  std::tie(success, failure) = SGF.emitCheckedCastBranch(E, origVal, origAbs,
                                                         origTL, castTL,
                                                         E->getCastKind());

  // Handle the cast success case.
  {
    SGF.B.emitBlock(success);
    SILValue castResult = success->bbarg_begin()[0];

    // Load the BB argument if casting from address-only to loadable type.
    if (castResult.getType().isAddress() && !castTL.isAddressOnly())
      castResult = SGF.B.createLoad(E, castResult);

    RValue castRV(SGF, E, castTy,
                  SGF.emitManagedRValueWithCleanup(castResult, castTL));

    // Wrap it in an Optional.
    SGF.emitInjectOptionalValueInto(E, { E->getSubExpr(), std::move(castRV) },
                                    resultBuffer, resultTL);
    SGF.B.createBranch(E, contBB);
  }
  
  // Handle the cast failure case.
  {
    SGF.B.emitBlock(failure);

    // Destroy the original value.
    destroyRValue(SGF, E, origVal, origTL);

    SGF.emitInjectOptionalNothingInto(E, resultBuffer, resultTL);
    SGF.B.createBranch(E, contBB);
  }

  SGF.B.emitBlock(contBB);

  // Manage the optional buffer.
  auto result = SGF.manageBufferForExprResult(resultBuffer, resultTL, C);
  if (!result) return RValue();

  if (!resultTL.isAddressOnly()) {
    auto resultValue = SGF.B.createLoad(E, result.forward(SGF));
    result = SGF.emitManagedRValueWithCleanup(resultValue, resultTL);
  }

  return RValue(SGF, E, result);
}

RValue RValueEmitter::emitUnconditionalCheckedCast(Expr *source,
                                                   SILLocation loc,
                                                   Type destType,
                                                   CheckedCastKind castKind,
                                                   SGFContext C) {
  ManagedValue original = visit(source).getAsSingleValue(SGF, source);

  // Disable the original cleanup because the cast-to type is more specific and
  // should have a more efficient cleanup.
  SILValue originalVal = original.forward(SGF);
  SILValue cast = SGF.emitUnconditionalCheckedCast(loc, originalVal,
                                                   source->getType(),
                                                   destType,
                                                   castKind);

  // If casting from an opaque existential, we'll forward the concrete value,
  // but the existential container husk still needs cleanup.
  if (originalVal.getType().isExistentialType()
      && !originalVal.getType().isClassExistentialType())
    SGF.Cleanups.pushCleanup<CleanupUsedExistentialContainer>(originalVal);

  return RValue(SGF, loc, destType->getCanonicalType(),
                SGF.emitManagedRValueWithCleanup(cast));
}

RValue RValueEmitter::visitIsaExpr(IsaExpr *E, SGFContext C) {
  // Cast the value using a conditional cast.
  ManagedValue original = visit(E->getSubExpr()).getAsSingleValue(SGF,
                                                              E->getSubExpr());
  auto &origTL = SGF.getTypeLowering(E->getSubExpr()->getType());
  auto &castTL = SGF.getTypeLowering(E->getCastTypeLoc().getType());
  SILValue origAbs = SGF.emitCheckedCastAbstractionChange(E,original.getValue(),
                                                          origTL,
                                                          &castTL);
  
  SILBasicBlock *success, *failure;
  std::tie(success, failure)
    = SGF.emitCheckedCastBranch(E, original.getValue(), origAbs,
                                origTL, castTL,
                                E->getCastKind());

  // Join the branches into an i1 value representing the success of the cast.
  auto contBB = SGF.createBasicBlock();
  auto i1Ty = SILType::getBuiltinIntegerType(1, SGF.getASTContext());
  auto isa = new (SGF.SGM.M) SILArgument(i1Ty, contBB);
  
  SGF.B.emitBlock(success);
  SILValue yes = SGF.B.createIntegerLiteral(E, i1Ty, 1);
  SGF.B.createBranch(E, contBB, yes);
  
  SGF.B.emitBlock(failure);
  SILValue no = SGF.B.createIntegerLiteral(E, i1Ty, 0);
  SGF.B.createBranch(E, contBB, no);
  
  SGF.B.emitBlock(contBB);
  
  // Call the _getBool library intrinsic.
  ASTContext &ctx = SGF.SGM.M.getASTContext();
  auto result =
    SGF.emitApplyOfLibraryIntrinsic(E, ctx.getGetBoolDecl(nullptr), {},
                                    ManagedValue(isa,
                                                 ManagedValue::Unmanaged),
                                    C);
  return (result ? RValue(SGF, E, result) : RValue());
}

RValue RValueEmitter::visitCoerceExpr(CoerceExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

static ManagedValue emitVarargs(SILGenFunction &gen,
                                SILLocation loc,
                                Type baseTy,
                                ArrayRef<ManagedValue> elements,
                                Expr *VarargsInjectionFn) {
  SILValue numEltsVal = gen.B.createIntegerLiteral(loc,
                      SILType::getBuiltinIntegerType(64, gen.F.getASTContext()),
                      elements.size());
  AllocArrayInst *allocArray = gen.B.createAllocArray(loc,
                                                  gen.getLoweredType(baseTy),
                                                  numEltsVal);
  // The first result is the owning ObjectPointer for the array.
  ManagedValue objectPtr
    = gen.emitManagedRValueWithCleanup(SILValue(allocArray, 0));
  // The second result is a RawPointer to the base address of the array.
  SILValue basePtr(allocArray, 1);

  for (size_t i = 0, size = elements.size(); i < size; ++i) {
    SILValue eltPtr = basePtr;
    if (i != 0) {
      SILValue index = gen.B.createIntegerLiteral(loc,
                  SILType::getBuiltinIntegerType(64, gen.F.getASTContext()), i);
      eltPtr = gen.B.createIndexAddr(loc, basePtr, index);
    }
    ManagedValue v = elements[i];
    v.forwardInto(gen, loc, eltPtr);
  }

  return gen.emitArrayInjectionCall(objectPtr, basePtr,
                                    numEltsVal, VarargsInjectionFn, loc);
}

RValue RValueEmitter::visitTupleExpr(TupleExpr *E, SGFContext C) {
  auto type = cast<TupleType>(E->getType()->getCanonicalType());

  // If we have an Initialization, emit the tuple elements into its elements.
  if (Initialization *I = C.getEmitInto()) {
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
  
  RValue result(type);
  for (Expr *elt : E->getElements()) {
    // FIXME: Remove this when tuple elements cannot be inout.
    result.addElement(SGF.emitLValueOrRValueAsRValue(elt));
  }
  return result;
}

/// Retrieve the outer substitutions
static ArrayRef<Substitution> 
getOuterSubstitutions(Type type, Module *module,
                      SmallVectorImpl<Substitution> &allSubstitutions) {
  if (!type)
    return { };

  // For a (non-generic) nominal type, just get the outer substitutions.
  if (auto nominal = type->getAs<NominalType>())
    return getOuterSubstitutions(nominal->getParent(), module, 
                                 allSubstitutions);

  // If we don't have a bound generic type, there are no substitutions.
  auto bound = type->getAs<BoundGenericType>();
  if (!bound)
    return { };

  // Retrieve the parent's substitutions.
  auto outer = getOuterSubstitutions(bound->getParent(), 
                                     module, allSubstitutions);

  // If the parent had no substitutions, just use our substitutions.
  auto subs = bound->getSubstitutions(module, nullptr);
  if (outer.empty())
    return subs;

  // If the parent had substitutions, add our own substitutions to it.
  if (allSubstitutions.empty())
    allSubstitutions.append(outer.begin(), outer.end());
  allSubstitutions.append(subs.begin(), subs.end());
  return allSubstitutions;
}

std::tuple<ManagedValue, SILType, ArrayRef<Substitution>>
SILGenFunction::emitSiblingMethodRef(SILLocation loc,
                                     SILValue selfValue,
                                     SILDeclRef methodConstant,
                                     ArrayRef<Substitution> innerSubs) {
  SILValue methodValue = B.createFunctionRef(loc,
                                             SGM.getFunction(methodConstant));

  /// Collect substitutions from a generic 'self' type, so we can
  /// specialize the method with its forwarding substitutions.
  SmallVector<Substitution, 4> allSubsVec;
  auto subs = getOuterSubstitutions(selfValue.getType().getSwiftType(),
                                    F.getDeclContext()->getParentModule(),
                                    allSubsVec);

  // Add the inner substitutions, if we have any.
  if (!innerSubs.empty()) {
    if (subs.empty())
      subs = innerSubs;
    else {
      if (allSubsVec.empty())
        allSubsVec.append(subs.begin(), subs.end());
      allSubsVec.append(innerSubs.begin(), innerSubs.end());
      subs = F.getASTContext().AllocateCopy(allSubsVec);
    }
  }

  SILType methodTy = methodValue.getType();
  
  if (!subs.empty()) {
    // Specialize the generic method.
    methodTy = getLoweredLoadableType(
                    methodTy.castTo<SILFunctionType>()
                      ->substGenericArgs(SGM.M, SGM.SwiftModule, subs));
  }
  
  return std::make_tuple(ManagedValue(methodValue, ManagedValue::Unmanaged),
                         methodTy, subs);
}

RValue RValueEmitter::visitMemberRefExpr(MemberRefExpr *E,
                                         SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");
  
  if (E->getType()->is<MetatypeType>()) {
    // Emit the metatype for the associated type.
    assert(E->getType()->is<MetatypeType>() &&
           "generic_member_ref of metatype should give metatype");
    visit(E->getBase());
    return RValue(SGF, E,
                  ManagedValue(SGF.B.createMetatype(E,
                                      SGF.getLoweredLoadableType(E->getType())),
                               ManagedValue::Unmanaged));
  }
  
  // If this is a get-only computed property being accessed, call the getter,
  // passing in self as an lvalue (for now!).
  auto FieldDecl = cast<VarDecl>(E->getMember().getDecl());
  if (FieldDecl->isComputed()) {
    SILValue baseVal;
    if (!E->getBase()->getType()->is<MetatypeType>() &&
        !E->getBase()->getType()->hasReferenceSemantics()) {
      baseVal = SGF.emitLValueAsRValue(E->getBase())
        .getAsSingleValue(SGF,E).getValue();
    } else {
      baseVal = SGF.emitRValue(E->getBase()).getAsSingleValue(SGF,E).getValue();
    }
    RValueSource baseRV = SGF.prepareAccessorBaseArg(E, baseVal);
  
    SILDeclRef getter(FieldDecl, SILDeclRef::Kind::Getter,
                      SILDeclRef::ConstructAtNaturalUncurryLevel,
                      SGF.SGM.requiresObjCDispatch(FieldDecl));
    
    return RValue(SGF, E,
                  SGF.emitGetAccessor(E, getter,
                                      E->getMember().getSubstitutions(),
                                      std::move(baseRV),
                                      RValueSource(),
                                      E->getType()->getCanonicalType(), C));
  }

  // rvalue MemberRefExprs are produces in a two cases: when accessing a 'let'
  // decl member (TODO), and when the base is a struct.
  assert(isa<StructDecl>(E->getBase()->getType()->getAnyNominal()) &&
         "The base of an rvalue MemberRefExpr should be an rvalue value");

  // If the accessed field is stored, emit a StructExtract on the base.

  // Evaluate the base of the member reference.
  ManagedValue base =
    visit(E->getBase()).getAsSingleValue(SGF, E->getBase());

  
  assert(!FieldDecl->isComputed() &&
         "Don't handle computed rvalue memberrefs yet");

  // For non-address-only structs, we emit a struct_extract sequence.
  auto &lowering = SGF.getTypeLowering(E->getType());
  if (!base.getType().isAddress()) {
    SILValue ElementVal =
      SGF.B.createStructExtract(E, base.getValue(), FieldDecl);

    // Emit a copyvalue to +1 the returned element, since the entire aggregate
    // will be destroyed.
    ElementVal = lowering.emitCopyValue(SGF.B, E, ElementVal);

    return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(ElementVal));
  }
  
  // For address-only sequences, the base is in memory.  Emit a
  // struct_element_addr to get to the field, and then load the element as an
  // rvalue.
  SILValue ElementPtr =
    SGF.B.createStructElementAddr(E, base.getValue(), FieldDecl);
  
  return RValue(SGF, E,
                SGF.emitLoad(E, ElementPtr, lowering, C, IsNotTake));
}

RValue RValueEmitter::visitDynamicMemberRefExpr(DynamicMemberRefExpr *E,
                                                SGFContext C) {
  return SGF.emitDynamicMemberRefExpr(E, C);
}

RValue RValueEmitter::visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E,
                                                  SGFContext C) {
  SILValue archetype = visit(E->getBase()).getUnmanagedSingleValue(SGF,
                                                                  E->getBase());
  assert((archetype.getType().isAddress() ||
          archetype.getType().is<MetatypeType>()) &&
         "archetype must be an address or metatype");
  // FIXME: curried archetype
  // FIXME: archetype properties
  (void)archetype;
  llvm_unreachable("unapplied archetype method not implemented");
}

RValue RValueEmitter::visitExistentialMemberRefExpr(
                                                 ExistentialMemberRefExpr *E,
                                                 SGFContext C) {
  SILValue existential = visit(E->getBase()).getUnmanagedSingleValue(SGF,
                                                                  E->getBase());
  //SILValue projection = B.createProjectExistential(E, existential);
  //SILValue method = emitProtocolMethod(E, existential);
  // FIXME: curried existential
  // FIXME: existential properties
  (void)existential;
  llvm_unreachable("unapplied protocol method not implemented");
}

RValue RValueEmitter::
visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E, SGFContext C) {
  visit(E->getLHS());
  return visit(E->getRHS());
}

RValue RValueEmitter::visitSubscriptExpr(SubscriptExpr *E, SGFContext C) {
  // rvalue subscript expressions are produced for get-only subscript
  // operations.  Emit a call to the getter.
  auto decl = cast<SubscriptDecl>(E->getDecl().getDecl());

  // Emit the base.
  SILValue baseVal;
  if (!E->getBase()->getType()->is<MetatypeType>() &&
      !E->getBase()->getType()->hasReferenceSemantics()) {
    baseVal = SGF.emitLValueAsRValue(E->getBase())
    .getAsSingleValue(SGF,E).getValue();
  } else {
    baseVal = SGF.emitRValue(E->getBase()).getAsSingleValue(SGF,E).getValue();
  }
  RValueSource baseRV = SGF.prepareAccessorBaseArg(E, baseVal);

  // Emit the indices.
  RValueSource subscriptRV = RValueSource(E, SGF.emitRValue(E->getIndex()));
  
  SILDeclRef getter(decl, SILDeclRef::Kind::Getter,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    SGF.SGM.requiresObjCDispatch(decl));
    
  return RValue(SGF, E,
                SGF.emitGetAccessor(E, getter,
                                    E->getDecl().getSubstitutions(),
                                    std::move(baseRV),
                                    std::move(subscriptRV),
                                    E->getType()->getCanonicalType(), C));
}


RValue RValueEmitter::visitModuleExpr(ModuleExpr *E, SGFContext C) {
  // Produce an undef value. The module value should never actually be used.
  SILValue module = SILUndef::get(SGF.getLoweredLoadableType(E->getType()),
                                  SGF.SGM.M);
  return RValue(SGF, E, ManagedValue(module, ManagedValue::Unmanaged));
}

RValue RValueEmitter::visitTupleElementExpr(TupleElementExpr *E,
                                            SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");
  return visit(E->getBase()).extractElement(E->getFieldNumber());
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
        = SILDeclRef::getDefaultArgGenerator(E->getDefaultArgsOwner(),
                                              destIndex);
      auto fnRef = SGF.emitFunctionRef(E, generator);
      auto resultType = field.getType()->getCanonicalType();
      auto apply = SGF.emitMonomorphicApply(E, fnRef, {}, resultType,
                                            generator.isTransparent());
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
    // into the varargs portion of this, which is then constructed into a Slice
    // through an informal protocol captured by the InjectionFn in the
    // TupleShuffleExpr.
    assert(E->getVarargsInjectionFunction() &&
           "no injection function for varargs tuple?!");
    SmallVector<ManagedValue, 4> variadicValues;
    
    while (shuffleIndexIterator != shuffleIndexEnd) {
      unsigned sourceField = *shuffleIndexIterator++;
      variadicValues.push_back(
                     std::move(elements[sourceField]).getAsSingleValue(SGF, E));
    }
    
    ManagedValue varargs = emitVarargs(SGF, E, field.getVarargBaseTy(),
                                       variadicValues,
                                       E->getVarargsInjectionFunction());
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
    ManagedValue scalar = gen.emitRValue(E->getSubExpr()).getAsSingleValue(gen,
                                                              E->getSubExpr());
    ManagedValue varargs = emitVarargs(gen, E, E->getSubExpr()->getType(),
                                       scalar,E->getVarargsInjectionFunction());
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
                                         {}, E->getVarargsInjectionFunction());
      varargs.forwardInto(gen, E, subInitializations[i]->getAddress());
      subInitializations[i]->finishInitialization(gen);
      continue;
    }

    auto &element = E->getElements()[i];
    // If this element comes from a default argument generator, emit a call to
    // that generator in-place.
    assert(outerFields[i].hasInit() &&
           "no default initializer in non-scalar field of scalar-to-tuple?!");
    if (auto defaultArgOwner = element.dyn_cast<ValueDecl *>()) {
      SILDeclRef generator
        = SILDeclRef::getDefaultArgGenerator(defaultArgOwner, i);
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
    emitScalarToTupleExprInto(SGF, E, I);
    return RValue();
  }

  // Emit the scalar member.
  RValue scalar = SGF.emitLValueOrRValueAsRValue(E->getSubExpr());

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
                              E->getVarargsInjectionFunction());
      else
        varargs = emitVarargs(SGF, E, outerFields[i].getVarargBaseTy(),
                              {}, E->getVarargsInjectionFunction());
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
    if (auto defaultArgOwner = element.dyn_cast<ValueDecl *>()) {
      SILDeclRef generator
        = SILDeclRef::getDefaultArgGenerator(defaultArgOwner, i);
      auto fnRef = SGF.emitFunctionRef(E, generator);
      auto resultType = outerFields[i].getType()->getCanonicalType();
      auto apply = SGF.emitMonomorphicApply(E, fnRef, {}, resultType,
                                            generator.isTransparent());
      result.addElement(SGF, apply, resultType, E);
      continue;
    }

    // We have an caller-side default argument. Emit it.
    Expr *defArg = element.get<Expr *>();
    result.addElement(visit(defArg));
  }

  return result;
}

RValue RValueEmitter::visitNewArrayExpr(NewArrayExpr *E, SGFContext C) {
  SILValue NumElements = visit(E->getBounds()[0].Value)
    .getAsSingleValue(SGF, E->getBounds()[0].Value)
    .getValue();

  // Allocate the array.
  AllocArrayInst *AllocArray = SGF.B.createAllocArray(E,
                                            SGF.getLoweredType(E->getElementType()),
                                            NumElements);

  ManagedValue ObjectPtr
    = SGF.emitManagedRValueWithCleanup(SILValue(AllocArray, 0));
  SILValue BasePtr(AllocArray, 1);

  // FIXME: We need to initialize the elements of the array that are now
  // allocated.

  // Finally, build and return a Slice instance using the object
  // header/base/count.
  return RValue(SGF, E,
                SGF.emitArrayInjectionCall(ObjectPtr, BasePtr, NumElements,
                                           E->getInjectionFunction(), E));
}

SILValue SILGenFunction::emitMetatypeOfValue(SILLocation loc, SILValue base) {
  // For class, archetype, and protocol types, look up the dynamic metatype.
  SILType metaTy = getLoweredLoadableType(
    MetatypeType::get(base.getType().getSwiftRValueType(), F.getASTContext()));
  if (base.getType().getSwiftType()->getClassOrBoundGenericClass()) {
    return B.createClassMetatype(loc, metaTy, base);
  } else if (base.getType().getSwiftRValueType()->is<ArchetypeType>()) {
    return B.createArchetypeMetatype(loc, metaTy, base);
  } else if (base.getType().getSwiftRValueType()->isExistentialType()) {
    return B.createProtocolMetatype(loc, metaTy, base);
  }
  // Otherwise, ignore the base and return the static metatype.
  return B.createMetatype(loc, metaTy);
}

RValue RValueEmitter::visitMetatypeExpr(MetatypeExpr *E, SGFContext C) {
  // Evaluate the base if present.
  SILValue metatype;
  
  if (E->getBase()) {
    SILValue base = visit(E->getBase()).getAsSingleValue(SGF,
                                                      E->getBase()).getValue();
    metatype = SGF.emitMetatypeOfValue(E, base);
  } else {
    metatype = SGF.B.createMetatype(E, SGF.getLoweredLoadableType(E->getType()));
  }
  
  return RValue(SGF, E, ManagedValue(metatype, ManagedValue::Unmanaged));
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
  
  // FIXME: AutoClosureExprs appear to always have null parent decl contexts,
  // so getFunctionTypeWithCaptures is unable to find contextual generic
  // parameters for them. The getAs null check here should be unnecessary.
  auto pft = constantInfo.SILFnType;
  
  if (pft->isPolymorphic() && !forwardSubs.empty()) {
    auto info = FunctionType::ExtInfo()
                  .withCallingConv(pft->getAbstractCC())
                  .withIsThin(true);

    auto specialized = SILFunctionType::get(nullptr,
                                            info,
                                            pft->getCalleeConvention(),
                                            pft->getParameters(),
                                            pft->getResult(),
                                            F.getASTContext());
    functionTy = getLoweredLoadableType(specialized);
  }

  if (!TheClosure.getCaptureInfo().hasLocalCaptures()) {
    auto result = ManagedValue::forUnmanaged(functionRef);
    return emitGeneralizedFunctionValue(loc, result,
                             AbstractionPattern(expectedType), expectedType);
  }

  SmallVector<ValueDecl*, 4> captures;
  TheClosure.getCaptureInfo().getLocalCaptures(captures);
  SmallVector<SILValue, 4> capturedArgs;
  for (ValueDecl *capture : captures) {
    switch (getDeclCaptureKind(capture)) {
    case CaptureKind::None:
      break;

    case CaptureKind::Constant:
      if (!getTypeLowering(capture->getType()).isAddressOnly()) {
        // LValues captured by value.
        auto Entry = VarLocs[cast<VarDecl>(capture)];
        ManagedValue v;
        if (Entry.isConstant()) {
          SILValue Val = B.createCopyValue(loc, Entry.getConstant());
          v = ManagedValue(Val, ManagedValue::Unmanaged);
        } else {
          SILValue addr = Entry.getAddress();
          B.createStrongRetain(loc, Entry.box);
          auto &TL = getTypeLowering(capture->getType());
          v = emitLoad(loc, addr, TL, SGFContext(), IsNotTake);
        }

        capturedArgs.push_back(v.forward(*this));
        break;
      }
      SWIFT_FALLTHROUGH;

    case CaptureKind::Box: {
      // LValues are captured as both the box owning the value and the
      // address of the value.
      assert(VarLocs.count(capture) && "no location for captured var!");
      VarLoc vl = VarLocs[capture];
      assert(vl.box && "no box for captured var!");
      assert(vl.isAddress() && vl.getAddress() &&
             "no address for captured var!");
      B.createStrongRetain(loc, vl.box);
      capturedArgs.push_back(vl.box);
      capturedArgs.push_back(vl.getAddress());
      break;
    }
    case CaptureKind::LocalFunction: {
      // SILValue is a constant such as a local func. Pass on the reference.
      ManagedValue v = emitReferenceToDecl(loc, capture);
      capturedArgs.push_back(v.forward(*this));
      break;
    }
    case CaptureKind::GetterSetter: {
      // Pass the setter and getter closure references on.
      ManagedValue v = emitFunctionRef(loc, SILDeclRef(capture,
                                               SILDeclRef::Kind::Setter));
      capturedArgs.push_back(v.forward(*this));
      SWIFT_FALLTHROUGH;
    }
    case CaptureKind::Getter: {
      // Pass the getter closure reference on.
      ManagedValue v = emitFunctionRef(loc, SILDeclRef(capture,
                                               SILDeclRef::Kind::Getter));
      capturedArgs.push_back(v.forward(*this));
      break;
    }
    }
  }
  
  SILType closureTy =
    SILBuilder::getPartialApplyResultType(functionRef.getType(),
                                          capturedArgs.size(), SGM.M);
  auto toClosure =
    B.createPartialApply(loc, functionRef, functionTy,
                         forwardSubs, capturedArgs, closureTy);
  auto result = emitManagedRValueWithCleanup(toClosure);

  return emitGeneralizedFunctionValue(loc, result,
                                      AbstractionPattern(expectedType),
                                      expectedType);
}

RValue RValueEmitter::visitClosureExpr(ClosureExpr *e, SGFContext C) {
  // Generate the closure function.
  SGF.SGM.emitClosure(e);

  // Generate the closure value (if any) for the closure expr's function
  // reference.
  return RValue(SGF, e, SGF.emitClosureValue(e, SILDeclRef(e),
                                          SGF.getForwardingSubstitutions(), e));
}

RValue RValueEmitter::visitAutoClosureExpr(AutoClosureExpr *e,
                                           SGFContext C) {
  // Generate the closure body.
  SGF.SGM.emitClosure(e);
  
  // Generate the closure value (if any) for the closure expr's function
  // reference.
  return RValue(SGF, e, SGF.emitClosureValue(e, SILDeclRef(e),
                                          SGF.getForwardingSubstitutions(), e));
}

void SILGenFunction::emitFunction(FuncDecl *fd) {
  Type resultTy = fd->getResultType();
  emitProlog(fd, fd->getBodyParamPatterns(), resultTy);
  prepareEpilog(resultTy, CleanupLocation(fd));
  visit(fd->getBody());
  emitEpilog(fd);
}

void SILGenFunction::emitClosure(ClosureExpr *ce) {
  emitProlog(ce, ce->getParams(), ce->getResultType());
  prepareEpilog(ce->getResultType(), CleanupLocation(ce));
  visit(ce->getBody());
  emitEpilog(ce);
}

void SILGenFunction::emitClosure(AutoClosureExpr *ce) {
  Type resultTy = ce->getType()->castTo<FunctionType>()->getResult();
  emitProlog(ce, ce->getParamPatterns(), resultTy);
  prepareEpilog(resultTy, CleanupLocation(ce));

  // Closure expressions implicitly return the result of their body expression.
  emitReturnExpr(ImplicitReturnLocation(ce), ce->getSingleExpressionBody());
  emitEpilog(ce);
}

std::pair<Optional<SILValue>, SILLocation>
SILGenFunction::emitEpilogBB(SILLocation TopLevel) {
  assert(ReturnDest.getBlock() && "no epilog bb prepared?!");
  SILBasicBlock *epilogBB = ReturnDest.getBlock();
  SILLocation ImplicitReturnFromTopLevel =
    ImplicitReturnLocation::getImplicitReturnLoc(TopLevel);
  SILValue returnValue;
  Optional<SILLocation> returnLoc = Nothing;

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
      return { Nothing, TopLevel };
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
  assert(getCleanupsDepth() == ReturnDest.getDepth() &&
         "emitting epilog in wrong scope");

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

  llvm::tie(maybeReturnValue, returnLoc) = emitEpilogBB(TopLevel);

  // If the epilog is unreachable, we're done.
  if (!maybeReturnValue)
    return;
  
  // Otherwise, return the return value, if any.
  SILValue returnValue = *maybeReturnValue;

  // Return () if no return value was given.
  if (!returnValue)
    returnValue = emitEmptyTuple(CleanupLocation::getCleanupLocation(TopLevel));

  B.createReturn(returnLoc, returnValue)
    ->setDebugScope(F.getDebugScope());
}

void SILGenFunction::emitDestructor(ClassDecl *cd, DestructorDecl *dd) {
  // Always emit physical property accesses in destructors.
  AlwaysDirectStoredPropertyAccess = true;
  
  RegularLocation Loc(dd);
  if (dd->isImplicit())
    Loc.markAutoGenerated();

  SILValue selfValue = emitDestructorProlog(cd, dd);

  // Create a basic block to jump to for the implicit destruction behavior
  // of releasing the elements and calling the superclass destructor.
  // We won't actually emit the block until we finish with the destructor body.
  prepareEpilog(Type(), CleanupLocation::getCleanupLocation(Loc));
  
  // Emit the destructor body.
  visit(dd->getBody());

  Optional<SILValue> maybeReturnValue;
  SILLocation returnLoc(Loc);
  llvm::tie(maybeReturnValue, returnLoc) = emitEpilogBB(Loc);

  if (!maybeReturnValue)
    return;

  auto cleanupLoc = CleanupLocation::getCleanupLocation(Loc);
  
  // Release our members.
  // FIXME: generic params
  // FIXME: Can a destructor always consider its fields fragile like this?
  for (Decl *member : cd->getMembers()) {
    if (VarDecl *vd = dyn_cast<VarDecl>(member)) {
      if (vd->isComputed())
        continue;
      const TypeLowering &ti = getTypeLowering(vd->getType());
      if (!ti.isTrivial()) {
        SILValue addr = B.createRefElementAddr(Loc, selfValue, vd,
                                          ti.getLoweredType().getAddressType());
        B.emitDestroyAddr(cleanupLoc, addr);
      }
    }
  }
  
  // If we have a superclass, invoke its destructor.
  SILType objectPtrTy = SILType::getObjectPointerType(F.getASTContext());
  if (Type superclassTy = cd->getSuperclass()) {
    ClassDecl *superclass = superclassTy->getClassOrBoundGenericClass();
    
    // FIXME: We can't sensibly call up to ObjC dealloc methods right now
    // because they aren't really destroying destructors.
    if (superclass->hasClangNode() && superclass->isObjC()) {
      selfValue = B.createRefToObjectPointer(cleanupLoc, selfValue, objectPtrTy);
      B.createReturn(returnLoc, selfValue);
      return;
    }
    
    SILDeclRef dtorConstant =
      SILDeclRef(superclass, SILDeclRef::Kind::Destroyer);
    SILType baseSILTy = getLoweredLoadableType(superclassTy);
    SILValue baseSelf = B.createUpcast(Loc, selfValue, baseSILTy);
    ManagedValue dtorValue;
    SILType dtorTy;
    ArrayRef<Substitution> subs;
    std::tie(dtorValue, dtorTy, subs)
      = emitSiblingMethodRef(cleanupLoc, baseSelf, dtorConstant,
                             /*innerSubstitutions*/ {});
    selfValue = B.createApply(cleanupLoc, dtorValue.forward(*this), dtorTy,
                              objectPtrTy,
                              subs, baseSelf);
  } else {
    selfValue = B.createRefToObjectPointer(cleanupLoc, selfValue, objectPtrTy);
  }
  B.createReturn(returnLoc, selfValue);
}

static void emitConstructorMetatypeArg(SILGenFunction &gen,
                                       ValueDecl *ctor) {
  // In addition to the declared arguments, the constructor implicitly takes
  // the metatype as its first argument, like a static function.
  Type metatype = ctor->getType()->castTo<AnyFunctionType>()->getInput();
  auto &AC = gen.getASTContext();
  auto VD = new (AC) VarDecl(/*static*/ false, /*IsLet*/ true, SourceLoc(),
                             AC.getIdentifier("$metatype"), metatype,
                             ctor->getDeclContext());
  new (gen.F.getModule()) SILArgument(gen.getLoweredType(metatype),
                                      gen.F.begin(), VD);
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
    auto VD = new (AC) VarDecl(/*static*/ false, /*IsLet*/ true, SourceLoc(),
                                 AC.getIdentifier("$implicit_value"), ty, DC);
    SILValue arg = new (gen.F.getModule()) SILArgument(gen.getLoweredType(ty),
                                                       gen.F.begin(), VD);
    return RValue(gen, loc, ty, ManagedValue::forUnmanaged(arg));
  }
}

namespace {
  class ImplicitValueInitialization : public SingleBufferInitialization {
    SILValue slot;
  public:
    ImplicitValueInitialization(SILValue slot) : slot(slot) {}
    
    SILValue getAddressOrNull() override {
      return slot;
    };
  };
}

static void emitImplicitValueConstructor(SILGenFunction &gen,
                                         ConstructorDecl *ctor) {
  RegularLocation Loc(ctor);
  Loc.markAutoGenerated();
  auto *TP = cast<TuplePattern>(ctor->getBodyParams());
  auto selfTyCan = ctor->getImplicitSelfDecl()->getType()->getRValueType();
  SILType selfTy = gen.getLoweredType(selfTyCan);

  // Emit the indirect return argument, if any.
  SILValue resultSlot;
  if (selfTy.isAddressOnly(gen.SGM.M)) {
    auto &AC = gen.getASTContext();
    auto VD = new (AC) VarDecl(/*static*/ false, /*IsLet*/ false, SourceLoc(),
                               AC.getIdentifier("$return_value"), selfTyCan,
                               ctor);
    resultSlot = new (gen.F.getModule()) SILArgument(selfTy, gen.F.begin(), VD);
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
  // Always emit physical property accesses in destructors.
  AlwaysDirectStoredPropertyAccess = true;

  // If there's no body, this is the implicit elementwise constructor.
  if (!ctor->getBody())
    return emitImplicitValueConstructor(*this, ctor);

  // Get the 'self' decl and type.
  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  auto &lowering = getTypeLowering(selfDecl->getType()->getRValueType());
  SILType selfTy = lowering.getLoweredType();
  (void)selfTy;
  assert(!selfTy.hasReferenceSemantics() && "can't emit a ref type ctor here");

  // Emit a local variable for 'self'.
  // FIXME: The (potentially partially initialized) variable would need to be
  // cleaned up on an error unwind.
  emitLocalVariable(selfDecl);
  
  // Mark self as being uninitialized so that DI knows where it is and how to
  // check for it.
  SILValue selfLV;
  {
    auto &SelfVarLoc = VarLocs[selfDecl];
    selfLV = B.createMarkUninitializedRootSelf(selfDecl,
                                               SelfVarLoc.getAddress());
    SelfVarLoc = VarLoc::getAddress(selfLV, SelfVarLoc.box);
  }

  // Emit the prolog.
  emitProlog(ctor->getBodyParams(),
             ctor->getImplicitSelfDecl()->getType()->getRValueType(),
             ctor);
  emitConstructorMetatypeArg(*this, ctor);
  
  // Create a basic block to jump to for the implicit 'self' return.
  // We won't emit this until after we've emitted the body.
  // The epilog takes a void return because the return of 'self' is implicit.
  prepareEpilog(Type(), CleanupLocation(ctor));

  // Emit the constructor body.
  visit(ctor->getBody());

  Optional<SILValue> maybeReturnValue;
  SILLocation returnLoc(ctor);
  llvm::tie(maybeReturnValue, returnLoc) = emitEpilogBB(ctor);

  // Return 'self' in the epilog.
  if (!maybeReturnValue)
    return;

  auto cleanupLoc = CleanupLocation::getCleanupLocation(ctor);

  // If 'self' is address-only, copy 'self' into the indirect return slot.
  if (lowering.isAddressOnly()) {
    assert(IndirectReturnAddress &&
           "no indirect return for address-only ctor?!");
    SILValue selfBox = VarLocs[selfDecl].box;
    assert(selfBox &&
           "address-only non-heap this should have been allocated in-place");
    // We have to do a non-take copy because someone else may be using the box.
    B.createCopyAddr(cleanupLoc, selfLV, IndirectReturnAddress,
                     IsNotTake, IsInitialization);
    B.emitStrongRelease(cleanupLoc, selfBox);
    B.createReturn(returnLoc, emitEmptyTuple(ctor));
    return;
  }

  // Otherwise, load and return the final 'self' value.
  SILValue selfValue = B.createLoad(cleanupLoc, selfLV);
  SILValue selfBox = VarLocs[selfDecl].box;
  assert(selfBox);

  // We have to do a retain because someone else may be using the box.
  selfValue = lowering.emitCopyValue(B, cleanupLoc, selfValue);

  // Release the box.
  B.emitStrongRelease(cleanupLoc, selfBox);

  B.createReturn(returnLoc, selfValue);
}

static void emitAddressOnlyEnumConstructor(SILGenFunction &gen,
                                           SILType enumTy,
                                           EnumElementDecl *element) {
  RegularLocation Loc(element);
  Loc.markAutoGenerated();

  // Emit the indirect return slot.
  auto &AC = gen.getASTContext();
  auto VD = new (AC) VarDecl(/*static*/ false, /*IsLet*/ false, SourceLoc(),
                             AC.getIdentifier("$return_value"),
                             enumTy.getSwiftType(),
                             element->getDeclContext());
  SILValue resultSlot
    = new (gen.F.getModule()) SILArgument(enumTy, gen.F.begin(), VD);
  
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
    SILValue resultData = gen.B.createEnumDataAddr(element, resultSlot,
      element, gen.getLoweredType(element->getArgumentType()).getAddressType());
    argValue.forwardInto(gen, element, resultData);
  }
  
  // Apply the tag.
  gen.B.createInjectEnumAddr(Loc, resultSlot, element);
  gen.Cleanups.emitCleanupsForReturn(CleanupLocation::getCleanupLocation(Loc));
  gen.B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(Loc),
                     gen.emitEmptyTuple(element));
}

static void emitLoadableEnumConstructor(SILGenFunction &gen,
                                        SILType enumTy,
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
  // Always emit physical property accesses in destructors.
  AlwaysDirectStoredPropertyAccess = true;

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
    DeclContext *DC;
  public:
    ArgumentForwardVisitor(SILGenFunction &gen,
                           SmallVectorImpl<SILValue> &args,
                           DeclContext *DC)
      : gen(gen), args(args), DC(DC) {}
    
    void makeArgument(Type ty, VarDecl *varDecl) {
      assert(ty && "no type?!");
      // Destructure tuple arguments.
      if (TupleType *tupleTy = ty->getAs<TupleType>()) {
        for (auto fieldType : tupleTy->getElementTypes())
          makeArgument(fieldType, varDecl);
      } else {
        SILValue arg = new (gen.F.getModule()) SILArgument(gen.getLoweredType(ty),
                                                           gen.F.begin(), varDecl);
        args.push_back(arg);
      }
    }

    void visitParenPattern(ParenPattern *P) {
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
      auto &AC = gen.getASTContext();
      auto VD = new (AC) VarDecl(/*static*/ false, /*IsLet*/ true, SourceLoc(),
                                 // FIXME: we should probably number them.
                                 AC.getIdentifier("_"), P->getType(), DC);
      makeArgument(P->getType(), VD);
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
SILGenFunction::buildForwardingSubstitutions(ArrayRef<ArchetypeType *> params) {
  if (params.empty())
    return {};
  
  ASTContext &C = F.getASTContext();
  
  size_t paramCount = params.size();
  MutableArrayRef<Substitution> results = C.Allocate<Substitution>(paramCount);
  
  for (size_t i = 0; i < paramCount; ++i) {
    // FIXME: better way to do this?
    ArchetypeType *archetype = params[i];
    // "Check conformance" on each declared protocol to build a
    // conformance map.
    SmallVector<ProtocolConformance*, 2> conformances;
    
    for (ProtocolDecl *conformsTo : archetype->getConformsTo()) {
      (void)conformsTo;
      conformances.push_back(nullptr);
    }
    
    // Build an identity mapping with the derived conformances.
    auto replacement = SubstitutedType::get(archetype, archetype, C);
    results[i] = {archetype, replacement,
                  C.AllocateCopy(conformances)};
  }
  
  return results;
}

void SILGenFunction::emitClassConstructorAllocator(ConstructorDecl *ctor) {
  // Always emit physical property accesses in destructors.
  AlwaysDirectStoredPropertyAccess = true;

  // Emit the prolog. Since we're just going to forward our args directly
  // to the initializer, don't allocate local variables for them.
  RegularLocation Loc(ctor);
  Loc.markAutoGenerated();

  SmallVector<SILValue, 8> args;
  
  // Forward the constructor arguments.
  ArgumentForwardVisitor(*this, args, ctor).visit(ctor->getBodyParams());

  emitConstructorMetatypeArg(*this, ctor);

  // Allocate the "self" value.
  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  SILType selfTy = getLoweredType(selfDecl->getType());
  assert(selfTy.hasReferenceSemantics() &&
         "can't emit a value type ctor here");

  // Use alloc_ref to allocate the constructor.
  // TODO: allow custom allocation?
  // FIXME: should have a cleanup in case of exception
  SILValue selfValue = B.createAllocRef(Loc, selfTy);
  args.push_back(selfValue);

  // Call the initializer.
  SILDeclRef initConstant =
    SILDeclRef(ctor, SILDeclRef::Kind::Initializer,
               SILDeclRef::ConstructAtNaturalUncurryLevel,
               /*isObjC=*/ctor->hasClangNode());

  ManagedValue initVal;
  SILType initTy;
  
  ArrayRef<Substitution> subs;
  if (ctor->hasClangNode()) {
    // If the constructor was imported from Clang, we perform dynamic dispatch
    // to it because we can't refer directly to the Objective-C method.
    auto method = initConstant.atUncurryLevel(1);
    auto objcInfo = getConstantInfo(method);
    SILValue methodRef = B.createClassMethod(Loc, selfValue, initConstant,
                                             objcInfo.getSILType());
    initVal = ManagedValue(methodRef, ManagedValue::Unmanaged);
    initTy = initVal.getType();

    // Bridge arguments.
    Scope scope(Cleanups, CleanupLocation::getCleanupLocation(Loc));

    auto objcFnType = objcInfo.SILFnType;

    unsigned idx = 0;
    for (auto &arg : args) {
      auto nativeTy = arg.getType().getSwiftType();// FIXME: wrong for functions
      auto bridgedTy =
        objcFnType->getParameters()[idx++].getSILType().getSwiftType();
      arg = emitNativeToBridgedValue(Loc,
                                     ManagedValue(arg, ManagedValue::Unmanaged),
                                     AbstractCC::ObjCMethod,
                                     nativeTy, nativeTy,
                                     bridgedTy).forward(*this);
    }
  } else {
    // Otherwise, directly call the constructor.
    ArrayRef<ArchetypeType *> archetypes;
    if (auto genericParams = ctor->getGenericParams())
      archetypes = genericParams->getAllArchetypes();
    auto forwardingSubs = buildForwardingSubstitutions(archetypes);
    std::tie(initVal, initTy, subs)
      = emitSiblingMethodRef(Loc, selfValue, initConstant, forwardingSubs);
  }

  SILValue initedSelfValue
    = B.createApply(Loc, initVal.forward(*this), initTy, selfTy, subs, args,
                    initConstant.isTransparent());
  
  // Return the initialized 'self'.
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(Loc),
                 initedSelfValue);
}

void SILGenFunction::emitClassConstructorInitializer(ConstructorDecl *ctor) {
  // Always emit physical property accesses in destructors.
  AlwaysDirectStoredPropertyAccess = true;

  // If there's no body, this is the implicit constructor.
  assert(ctor->getBody() && "Class constructor without a body?");

  // FIXME: The (potentially partially initialized) value here would need to be
  // cleaned up on a constructor failure unwinding.

  // Set up the 'self' argument.  If this class has a superclass, we set up
  // self as a box.  This allows "self reassignment" to happen in super init
  // method chains, which is important for interoperating with Objective-C
  // classes.
  // TODO: If we could require Objective-C classes to have an attribute to get
  // this behavior, we could avoid runtime overhead here.
  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  auto nominalDecl = ctor->getDeclContext()->getDeclaredTypeInContext()
    ->getNominalOrBoundGenericNominal();
  bool NeedsBoxForSelf = cast<ClassDecl>(nominalDecl)->hasSuperclass();

  if (NeedsBoxForSelf)
    emitLocalVariable(selfDecl);

  // Emit the prolog for the non-self arguments.
  emitProlog(ctor->getBodyParams(), TupleType::getEmpty(F.getASTContext()),
             ctor);

  SILType selfTy = getLoweredLoadableType(selfDecl->getType());
  SILValue selfArg = new (SGM.M) SILArgument(selfTy, F.begin(), selfDecl);

  // Mark 'self' as uninitialized so that DI knows to enforce its DI properties
  // on ivars.
  auto MUKind = cast<ClassDecl>(nominalDecl)->hasSuperclass()
          ? MarkUninitializedInst::DerivedSelf :
            MarkUninitializedInst::RootSelf;
  selfArg = B.createMarkUninitialized(selfDecl, selfArg, MUKind);

  assert(selfTy.hasReferenceSemantics() && "can't emit a value type ctor here");

  if (NeedsBoxForSelf) {
    SILLocation prologueLoc = RegularLocation(ctor);
    prologueLoc.markAsPrologue();
    B.createStore(prologueLoc, selfArg, VarLocs[selfDecl].getAddress());
  } else {
    VarLocs[selfDecl] = VarLoc::getConstant(selfArg);
  }

  // Prepare the end of initializer location.
  SILLocation endOfInitLoc = RegularLocation(ctor);
  endOfInitLoc.pointToEnd();

  // Create a basic block to jump to for the implicit 'self' return.
  // We won't emit the block until after we've emitted the body.
  prepareEpilog(Type(), CleanupLocation::getCleanupLocation(endOfInitLoc));
  
  // Emit the constructor body.
  visit(ctor->getBody());
  
  // Return 'self' in the epilog.
  Optional<SILValue> maybeReturnValue;
  SILLocation returnLoc(ctor);
  llvm::tie(maybeReturnValue, returnLoc) = emitEpilogBB(ctor);

  if (!maybeReturnValue)
    return;

  auto cleanupLoc = CleanupLocation::getCleanupLocation(ctor);

  // If we're using a box for self, reload the value at the end of the init
  // method.
  if (NeedsBoxForSelf) {
    // Emit the call to super.init() right before exiting from the initializer.
    if (Expr* SI = ctor->getSuperInitCall())
      emitIgnoredRValue(SI);

    selfArg = B.createLoad(cleanupLoc, VarLocs[selfDecl].getAddress());
    SILValue selfBox = VarLocs[selfDecl].box;
    assert(selfBox);

    // We have to do a retain because someone else may be using the box.
    selfArg = B.emitCopyValueOperation(cleanupLoc, selfArg);
    B.emitStrongRelease(cleanupLoc, selfBox);
  }

  // Return the final 'self'.
  B.createReturn(returnLoc, selfArg)
    ->setDebugScope(F.getDebugScope());
}

static void forwardCaptureArgs(SILGenFunction &gen,
                               SmallVectorImpl<SILValue> &args,
                               ValueDecl *capture) {
  ASTContext &c = capture->getASTContext();
  
  auto addSILArgument = [&](SILType t) {
    args.push_back(new (gen.SGM.M) SILArgument(t, gen.F.begin()));
  };

  switch (getDeclCaptureKind(capture)) {
  case CaptureKind::None:
    break;

  case CaptureKind::Constant:
    if (!gen.getTypeLowering(capture->getType()).isAddressOnly()) {
      addSILArgument(gen.getLoweredType(capture->getType()));
      break;
    }
    SWIFT_FALLTHROUGH;

  case CaptureKind::Box: {
    SILType ty = gen.getLoweredType(capture->getType()->getRValueType())
      .getAddressType();
    // Forward the captured owning ObjectPointer.
    addSILArgument(SILType::getObjectPointerType(c));
    // Forward the captured value address.
    addSILArgument(ty);
    break;
  }
  case CaptureKind::LocalFunction:
    // Forward the captured value.
    addSILArgument(gen.getLoweredType(capture->getType()));
    break;
  case CaptureKind::GetterSetter: {
    // Forward the captured setter.
    Type setTy;
    if (auto subscript = dyn_cast<SubscriptDecl>(capture))
      setTy = subscript->getSetterType();
    else
      setTy = cast<VarDecl>(capture)->getSetterType();
    addSILArgument(gen.getLoweredType(setTy));
    SWIFT_FALLTHROUGH;
  }
  case CaptureKind::Getter: {
    // Forward the captured getter.
    Type getTy;
    if (auto subscript = dyn_cast<SubscriptDecl>(capture))
      getTy = subscript->getGetterType();
    else
      getTy = cast<VarDecl>(capture)->getGetterType();
    addSILArgument(gen.getLoweredType(getTy));
    break;
  }
  }
}

static SILValue getNextUncurryLevelRef(SILGenFunction &gen,
                                       SILLocation loc,
                                       SILDeclRef next,
                                       ArrayRef<SILValue> curriedArgs) {
  // For the fully-uncurried reference to a class method, emit the dynamic
  // dispatch.
  // FIXME: We should always emit dynamic dispatch at uncurry level 1,
  // to support overriding a curried method with a non-curried,
  // function-returning method.
  if (!next.isCurried
      && next.kind == SILDeclRef::Kind::Func
      && next.hasDecl() && isa<ClassDecl>(next.getDecl()->getDeclContext())) {
    SILValue thisArg;
    thisArg = curriedArgs.back();
    
    return gen.B.createClassMethod(loc, thisArg, next,
                                   gen.SGM.getConstantType(next));
  }
  
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
  ArgumentForwardVisitor forwarder(*this, curriedArgs, fd);
  for (auto *paramPattern : reversed(forwardedPatterns))
    forwarder.visit(paramPattern);

  // Forward captures.
  if (hasCaptures) {
    SmallVector<ValueDecl*, 4> LocalCaptures;
    fd->getCaptureInfo().getLocalCaptures(LocalCaptures);
    for (auto capture : LocalCaptures)
      forwardCaptureArgs(*this, curriedArgs, capture);
  }

  SILValue toFn = getNextUncurryLevelRef(*this, fd, to, curriedArgs);
  SILType resultTy
    = SGM.getConstantType(from).castTo<SILFunctionType>()->getResult().getSILType();
  auto toTy = toFn.getType();
  
  // Forward archetypes and specialize if the function is generic.
  ArrayRef<Substitution> subs;
  {
    auto toFnTy = toFn.getType().castTo<SILFunctionType>();
    if (toFnTy->isPolymorphic()) {
      subs = buildForwardingSubstitutions(toFnTy->getGenericParams()->getAllArchetypes());
      toTy = getLoweredLoadableType(toFnTy->substGenericArgs(SGM.M, SGM.SwiftModule, subs));
    }
  }
  
  // Partially apply the next uncurry level and return the result closure.
  auto closureTy =
    SILBuilder::getPartialApplyResultType(toFn.getType(), curriedArgs.size(),
                                          SGM.M);
  SILInstruction *toClosure =
    B.createPartialApply(fd, toFn, toTy, subs, curriedArgs, closureTy);
  if (resultTy != closureTy)
    toClosure = B.createConvertFunction(fd, toClosure, resultTy);
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(fd), toClosure);
}

void SILGenFunction::emitForeignThunk(SILDeclRef thunk) {
  // FIXME: native-to-foreign thunk
  assert(!thunk.isForeign && "native to foreign thunk not implemented");
  
  // Wrap the function in its original form.

  auto fd = cast<FuncDecl>(thunk.getDecl());
  
  // Forward the arguments.
  // FIXME: For native-to-foreign thunks, use emitObjCThunkArguments to retain
  // inputs according to the foreign convention.
  auto forwardedPatterns = fd->getBodyParamPatterns();
  SmallVector<SILValue, 8> args;
  ArgumentForwardVisitor forwarder(*this, args, fd);
  for (auto *paramPattern : reversed(forwardedPatterns))
    forwarder.visit(paramPattern);
  
  SILValue result;
  {
    CleanupLocation cleanupLoc(fd);
    Scope scope(Cleanups, fd);
    
    // Set up cleanups on all the arguments, which should be at +1 now.
    SmallVector<ManagedValue, 8> managedArgs;
    for (auto arg : args)
      managedArgs.push_back(emitManagedRValueWithCleanup(arg));

    // Call the original.
    SILDeclRef original = thunk.asForeign(!thunk.isForeign);
    auto originalInfo = getConstantInfo(original);
    auto fn = emitGlobalFunctionRef(fd, original, originalInfo);
    result = emitMonomorphicApply(fd, ManagedValue(fn, ManagedValue::Unmanaged),
                                  managedArgs,
                                  fd->getBodyResultType()->getCanonicalType())
      .forward(*this);
  }
  // FIXME: use correct convention for native-to-foreign return
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(fd), result);
}

void SILGenFunction::emitGeneratorFunction(SILDeclRef function, Expr *value) {
  RegularLocation Loc(value);
  Loc.markAutoGenerated();

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

void SILGenFunction::emitGlobalAccessor(VarDecl *global,
                                        FuncDecl *builtinOnceDecl,
                                        SILGlobalVariable *onceToken,
                                        SILFunction *onceFunc) {
  // Emit a reference to Builtin.once.
  SILDeclRef builtinOnceConstant(builtinOnceDecl, SILDeclRef::Kind::Func);
  auto builtinOnceSILTy = SGM.Types.getConstantType(builtinOnceConstant);
  auto builtinOnce = B.createBuiltinFunctionRef(global,
                                                builtinOnceDecl->getName(),
                                                builtinOnceSILTy);

  SILType rawPointerSILTy
    = getLoweredLoadableType(getASTContext().TheRawPointerType);

  // Emit a reference to the global token.
  SILValue onceTokenAddr = B.createSILGlobalAddr(global, onceToken);
  onceTokenAddr = B.createAddressToPointer(global, onceTokenAddr,
                                           rawPointerSILTy);

  // Emit a reference to the function to execute once, then thicken
  // that reference as Builtin.once expects.
  SILValue onceFuncRef = B.createFunctionRef(global, onceFunc);
  auto onceFuncThickTy
    = getThickFunctionType(onceFunc->getLoweredFunctionType());
  auto onceFuncThickSILTy = SILType::getPrimitiveObjectType(onceFuncThickTy);
  onceFuncRef = B.createThinToThickFunction(global, onceFuncRef,
                                            onceFuncThickSILTy);

  // Call Builtin.once.
  SILValue onceArgs[] = {onceTokenAddr, onceFuncRef};
  auto resultTy = builtinOnceSILTy.castTo<SILFunctionType>()
                                                  ->getResult().getSILType();

  B.createApply(global, builtinOnce, builtinOnceSILTy, resultTy,
                {}, onceArgs);
  
  // Return the address of the global variable.
  // FIXME: It'd be nice to be able to return a SIL address directly.
  SILValue addr = B.createGlobalAddr(global, global,
                           getLoweredType(global->getType()).getAddressType());
  addr = B.createAddressToPointer(global, addr, rawPointerSILTy);
  B.createReturn(global, addr);
}

RValue RValueEmitter::
visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                   SGFContext C) {
  return visit(E->getSemanticExpr(), C);
}

RValue RValueEmitter::
visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E, SGFContext C) {
  ASTContext &Ctx = SGF.SGM.M.getASTContext();
  SILType Ty = SGF.getLoweredLoadableType(E->getType());
  SourceLoc Loc;
  
  // If "overrideLocationForMagicIdentifiers" is set, then we use it as the
  // location point for these magic identifiers.
  if (SGF.overrideLocationForMagicIdentifiers.isValid())
    Loc = SGF.overrideLocationForMagicIdentifiers;
  else
    Loc = E->getStartLoc();
  
  switch (E->getKind()) {
  case MagicIdentifierLiteralExpr::File: {
    unsigned BufferID = Ctx.SourceMgr.findBufferContainingLoc(Loc);

    StringRef Value =
      Ctx.SourceMgr->getMemoryBuffer(BufferID)->getBufferIdentifier();

    return emitStringLiteral(E, Value, C);
  }
  case MagicIdentifierLiteralExpr::Line: {
    unsigned Value = Ctx.SourceMgr.getLineAndColumn(Loc).first;
    return RValue(SGF, E, ManagedValue(SGF.B.createIntegerLiteral(E, Ty, Value),
                                       ManagedValue::Unmanaged));
  }
  case MagicIdentifierLiteralExpr::Column: {
    unsigned Value = Ctx.SourceMgr.getLineAndColumn(Loc).second;
    return RValue(SGF, E, ManagedValue(SGF.B.createIntegerLiteral(E, Ty, Value),
                                       ManagedValue::Unmanaged));
  }
  }
}

RValue RValueEmitter::visitCollectionExpr(CollectionExpr *E, SGFContext C) {
  return visit(E->getSemanticExpr());
}

RValue RValueEmitter::visitRebindSelfInConstructorExpr(
                                RebindSelfInConstructorExpr *E, SGFContext C) {
  // FIXME: Use a different instruction from 'downcast'. IRGen can make
  // "rebind this" into a no-op if the called constructor is a Swift one.
  ManagedValue newSelf = visit(E->getSubExpr()).getAsSingleValue(SGF,
                                                              E->getSubExpr());
  if (!newSelf.getType().getSwiftRValueType()
        ->isEqual(E->getSelf()->getType())) {
    assert(newSelf.getType().isObject() &&
           newSelf.getType().hasReferenceSemantics() &&
           "delegating ctor type mismatch for non-reference type?!");
    CleanupHandle newSelfCleanup = newSelf.getCleanup();
    SILValue newSelfValue = SGF.B.createUnconditionalCheckedCast(E,
                           CheckedCastKind::Downcast,
                           newSelf.getValue(),
                           SGF.getLoweredLoadableType(E->getSelf()->getType()));
    newSelf = ManagedValue(newSelfValue, newSelfCleanup);
  }
  
  SILValue selfAddr = SGF.emitReferenceToDecl(E, E->getSelf()).getUnmanagedValue();
  newSelf.assignInto(SGF, E, selfAddr);
  
  return SGF.emitEmptyTupleRValue(E);
}

RValue RValueEmitter::visitArchetypeSubscriptExpr(
                                     ArchetypeSubscriptExpr *E, SGFContext C) {
  llvm_unreachable("not implemented");
}

RValue RValueEmitter::visitDynamicSubscriptExpr(
                                     DynamicSubscriptExpr *E, SGFContext C) {
  return SGF.emitDynamicSubscriptExpr(E, C);
}

RValue RValueEmitter::visitExistentialSubscriptExpr(
                                   ExistentialSubscriptExpr *E, SGFContext C) {
  llvm_unreachable("not implemented");
}

RValue RValueEmitter::visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *E,
                                                  SGFContext C) {
  // Create a buffer for the result.  Abstraction difference will
  // force this to be returned indirectly from
  // _injectValueIntoOptional anyway, so there's not much point
  // avoiding that.
  auto &optTL = SGF.getTypeLowering(E->getType());
  SILValue optAddr =
    SGF.getBufferForExprResult(E, optTL.getLoweredType(), C);

  SGF.emitInjectOptionalValueInto(E, E->getSubExpr(), optAddr, optTL);

  ManagedValue result = SGF.manageBufferForExprResult(optAddr, optTL, C);
  if (!result) return RValue();

  // If we're not address-only, the caller will expect a non-address value.
  if (!optTL.isAddressOnly()) {
    auto optValue = optTL.emitLoadOfCopy(SGF.B, E, result.forward(SGF), IsTake);
    result = SGF.emitManagedRValueWithCleanup(optValue, optTL);
  }
  return RValue(SGF, E, result);
}

RValue RValueEmitter::visitBridgeToBlockExpr(BridgeToBlockExpr *E,
                                             SGFContext C) {
  auto func = visit(E->getSubExpr()).getScalarValue();

  // Emit the bridge_to_block instruction.
  SILValue block = SGF.B.createBridgeToBlock(E, func.forward(SGF),
                                    SGF.getLoweredLoadableType(E->getType()));
  return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(block));
}

namespace {
  /// An Initialization representing the result of an address-only ternary.
  class TernaryInitialization : public SingleBufferInitialization {
    SILValue valueAddr;
  public:
    TernaryInitialization(SILValue valueAddr)
      : valueAddr(valueAddr)
    {}
    
    SILValue getAddressOrNull() override {
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

    auto result = SGF.manageBufferForExprResult(resultAddr, lowering, C);
    return (result ? RValue(SGF, E, result) : RValue());
  }
}

RValue RValueEmitter::visitDefaultValueExpr(DefaultValueExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

static ManagedValue emitBridgeStringToNSString(SILGenFunction &gen,
                                               SILLocation loc,
                                               ManagedValue str) {
  // func convertStringToNSString([inout] String) -> NSString
  SILValue stringToNSStringFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getStringToNSStringFn());
  
  // Materialize the string so we can pass a reference.
  // Assume StringToNSString won't consume or modify the string, so leave the
  // cleanup on the original value intact.
  SILValue strTemp = gen.emitTemporaryAllocation(loc,
                                                 str.getType());
  gen.B.createStore(loc, str.getValue(), strTemp);
  
  SILValue nsstr = gen.B.createApply(loc, stringToNSStringFn,
                           stringToNSStringFn.getType(),
                           gen.getLoweredType(gen.SGM.Types.getNSStringType()),
                           {}, strTemp);
  return gen.emitManagedRValueWithCleanup(nsstr);
}

static ManagedValue emitBridgeNSStringToString(SILGenFunction &gen,
                                               SILLocation loc,
                                               ManagedValue nsstr) {
  // func convertNSStringToString(NSString, [inout] String) -> ()
  SILValue nsstringToStringFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getNSStringToStringFn());
  
  // Allocate and initialize a temporary to receive the result String.
  SILValue strTemp = gen.emitTemporaryAllocation(loc,
                             gen.getLoweredType(gen.SGM.Types.getStringType()));
  // struct String { init() }
  SILValue strInitFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getStringDefaultInitFn());
  SILValue strMetaty
    = gen.B.createMetatype(loc, gen.getLoweredLoadableType(
                   MetatypeType::get(gen.SGM.Types.getStringType(),
                                     gen.getASTContext())->getCanonicalType()));
  SILValue strInit = gen.B.createApply(loc, strInitFn,
                      strInitFn.getType(),
                      gen.getLoweredLoadableType(gen.SGM.Types.getStringType()),
                      {}, strMetaty);
  gen.B.createStore(loc, strInit, strTemp);
  
  SILValue args[2] = {nsstr.forward(gen), strTemp};
  gen.B.createApply(loc, nsstringToStringFn,
                    nsstringToStringFn.getType(),
                    gen.SGM.Types.getEmptyTupleType(),
                    {}, args);
  
  // Load the result string, taking ownership of the value. There's no cleanup
  // on the value in the temporary allocation.
  SILValue str = gen.B.createLoad(loc, strTemp);
  return gen.emitManagedRValueWithCleanup(str);
}

static ManagedValue emitBridgeBoolToObjCBool(SILGenFunction &gen,
                                             SILLocation loc,
                                             ManagedValue swiftBool) {
  // func convertBoolToObjCBool(Bool) -> ObjCBool
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
  // func convertObjCBoolToBool(ObjCBool) -> Bool
  SILValue objcBoolToBoolFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getObjCBoolToBoolFn());
  
  SILType resultTy = gen.getLoweredLoadableType(gen.SGM.Types.getBoolType());
  
  SILValue result = gen.B.createApply(loc, objcBoolToBoolFn,
                                      objcBoolToBoolFn.getType(),
                                      resultTy, {}, objcBool.forward(gen));
  return gen.emitManagedRValueWithCleanup(result);
}

ManagedValue SILGenFunction::emitNativeToBridgedValue(SILLocation loc,
                                                      ManagedValue v,
                                                      AbstractCC destCC,
                                                      CanType origNativeTy,
                                                      CanType substNativeTy,
                                                      CanType bridgedTy) {
  switch (destCC) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    // No additional bridging needed for native functions.
    return v;
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    // If the input is a native type with a bridged mapping, convert it.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType) \
    if (substNativeTy == SGM.Types.get##NativeType##Type() \
        && bridgedTy == SGM.Types.get##BridgedType##Type()) {           \
      return emitBridge##NativeType##To##BridgedType(*this, loc, v);    \
    }
#include "swift/SIL/BridgedTypes.def"
    return v;
  }
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
    // If the output is a bridged type, convert it back to a native type.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType)     \
    if (nativeTy == SGM.Types.get##NativeType##Type() &&                    \
        v.getType().getSwiftType() == SGM.Types.get##BridgedType##Type()) { \
      return emitBridge##BridgedType##To##NativeType(*this, loc, v);        \
    }
#include "swift/SIL/BridgedTypes.def"
    return v;
  }
}

RValue SILGenFunction::emitEmptyTupleRValue(SILLocation loc) {
  return RValue(CanType(TupleType::getEmpty(F.getASTContext())));
}

/// Destructure (potentially) recursive assignments into tuple expressions
/// down to their scalar stores.
static void emitAssignExprRecursive(AssignExpr *S, RValue &&Src,
                                    Expr *Dest, SILGenFunction &Gen) {
  // If the destination is a tuple, recursively destructure.
  if (auto *TE = dyn_cast<TupleExpr>(Dest)) {
    SmallVector<RValue, 4> elements;
    std::move(Src).extractElements(elements);
    unsigned EltNo = 0;
    for (Expr *DestElem : TE->getElements()) {
      emitAssignExprRecursive(S,
                              std::move(elements[EltNo++]),
                              DestElem, Gen);
    }
    return;
  }
  
  // If the destination is '_', do nothing.
  if (isa<DiscardAssignmentExpr>(Dest))
    return;
  
  // Otherwise, emit the scalar assignment.
  LValue DstLV = Gen.emitLValue(Dest);
  Gen.emitAssignToLValue(S, {S, std::move(Src)}, DstLV);
}

RValue RValueEmitter::visitAssignExpr(AssignExpr *E, SGFContext C) {
  FullExpr scope(SGF.Cleanups, CleanupLocation(E));

  // Handle lvalue-to-lvalue assignments with a high-level copy_addr instruction
  // if possible.
  if (auto *LE = dyn_cast<LoadExpr>(E->getSrc())) {
    if (!isa<TupleExpr>(E->getDest())
        && E->getDest()->getType()->isEqual(LE->getSubExpr()->getType()) &&
        !isLoadPropagatedValue(LE->getSubExpr(), SGF)) {
      auto SrcLV = SGF.emitLValue(cast<LoadExpr>(E->getSrc())->getSubExpr());
      SGF.emitAssignLValueToLValue(E, SrcLV,
                                   SGF.emitLValue(E->getDest()));
      return SGF.emitEmptyTupleRValue(E);
    }
  }

  // Handle tuple destinations by destructuring them if present.
  emitAssignExprRecursive(E, visit(E->getSrc()), E->getDest(), SGF);
  return SGF.emitEmptyTupleRValue(E);
}

RValue RValueEmitter::visitBindOptionalExpr(BindOptionalExpr *E, SGFContext C) {
  assert(SGF.BindOptionalFailureDest.isValid());

  // Create a temporary of type Optional<T>.
  auto &optTL = SGF.getTypeLowering(E->getSubExpr()->getType());
  auto temp = SGF.emitTemporary(E, optTL);

  // Emit the operand into the temporary.
  SGF.emitExprInto(E->getSubExpr(), temp.get());

  SILValue addr = temp->getAddress();

  // Check whether the optional has a value.
  SILBasicBlock *hasValueBB = SGF.createBasicBlock();
  SILBasicBlock *hasNoValueBB = SGF.createBasicBlock();
  SILValue hasValue = SGF.emitDoesOptionalHaveValue(E, addr);
  SGF.B.createCondBranch(E, hasValue, hasValueBB, hasNoValueBB);

  // If not, thread out through a bunch of cleanups.
  SGF.B.emitBlock(hasNoValueBB);
  SGF.Cleanups.emitBranchAndCleanups(SGF.BindOptionalFailureDest, E);

  // If so, get that value as the result of our expression.
  SGF.B.emitBlock(hasValueBB);
  auto result = temp->getManagedAddress();
  return RValue(SGF, E, SGF.emitGetOptionalValueFrom(E, result, optTL, C));
}

namespace {
  /// A RAII object to save and restore BindOptionalFailureDest.
  class RestoreOptionalFailureDest {
    SILGenFunction &SGF;
    JumpDest Prev;
  public:
    RestoreOptionalFailureDest(SILGenFunction &SGF)
      : SGF(SGF), Prev(SGF.BindOptionalFailureDest) {
    }
    ~RestoreOptionalFailureDest() {
      SGF.BindOptionalFailureDest = Prev;
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
  if (!optInit) {
    optTemp = SGF.emitTemporary(E, optTL);
    optInit = optTemp.get();
  }

  // Enter a cleanups scope.
  FullExpr scope(SGF.Cleanups, E);

  // Install a new optional-failure destination just outside of the
  // cleanups scope.
  RestoreOptionalFailureDest restoreFailureDest(SGF);
  SILBasicBlock *failureBB = SGF.createBasicBlock();
  SGF.BindOptionalFailureDest =
    JumpDest(failureBB, SGF.Cleanups.getCleanupsDepth(), E);

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

  // If we had a destination address, we're done.
  if (C.getEmitInto())
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
  // If the subexpression is a conditional checked cast, emit an unconditional
  // cast, which drastically simplifies the generated SIL for something like:
  //
  //   (x as Foo)!
  if (auto checkedCast = dyn_cast<ConditionalCheckedCastExpr>(
        E->getSubExpr()->getSemanticsProvidingExpr())) {
    return emitUnconditionalCheckedCast(checkedCast->getSubExpr(),
                                        E, E->getType(),
                                        checkedCast->getCastKind(),
                                        C);
  }

  const TypeLowering &optTL = SGF.getTypeLowering(E->getSubExpr()->getType());
  auto optTemp = SGF.emitTemporary(E, optTL);
  SGF.emitExprInto(E->getSubExpr(), optTemp.get());

  ManagedValue V = SGF.emitGetOptionalValueFrom(E, optTemp->getManagedAddress(),
                                                optTL, C);
  
  return V ? RValue(SGF, E, V) : RValue();
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

void SILGenFunction::emitIgnoredRValue(Expr *E) {
  emitRValue(E, SGFContext::Ignored);
}

RValue SILGenFunction::emitUngeneralizedRValue(Expr *E) {
  return emitRValue(E, SGFContext::Ungeneralized);
}

RValue SILGenFunction::emitRValue(Expr *E, SGFContext C) {
  return RValueEmitter(*this).visit(E, C);
}
