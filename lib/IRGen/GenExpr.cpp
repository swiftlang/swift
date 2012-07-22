//===--- GenExpr.cpp - Miscellaneous IR Generation for Expressions --------===//
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
//
//  This file implements general IR generation for Swift expressions.
//  Expressions which naturally belong to a specific type kind, such
//  as TupleExpr, are generally implemented in the type-specific file.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ExprHandle.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Target/TargetData.h"

#include "ASTVisitor.h"
#include "GenArray.h"
#include "GenClass.h"
#include "GenClosure.h"
#include "GenFunc.h"
#include "GenInit.h"
#include "GenLValue.h"
#include "GenMeta.h"
#include "GenOneOf.h"
#include "GenProto.h"
#include "GenTuple.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "Explosion.h"
#include "TypeInfo.h"
#include "TypeVisitor.h"

using namespace swift;
using namespace irgen;

/// Is the given l-value type heap or non-heap?
static OnHeap_t isOnHeap(Type type) {
  return (type->castTo<LValueType>()->isHeap() ? OnHeap : NotOnHeap);
}

/// Emit an integer literal expression.
static llvm::Value *emitIntegerLiteralExpr(IRGenFunction &IGF,
                                           IntegerLiteralExpr *E) {
  assert(E->getType()->is<BuiltinIntegerType>());
  return llvm::ConstantInt::get(IGF.IGM.LLVMContext, E->getValue());
}

/// Emit an float literal expression.
static llvm::Value *emitFloatLiteralExpr(IRGenFunction &IGF,
                                         FloatLiteralExpr *E) {
  assert(E->getType()->is<BuiltinFloatType>());
  return llvm::ConstantFP::get(IGF.IGM.LLVMContext, E->getValue());
}

/// Emit an string literal expression.
static llvm::Value *emitCharacterLiteralExpr(IRGenFunction &IGF,
                                             CharacterLiteralExpr *E) {
  assert(E->getType()->is<BuiltinIntegerType>());
  return llvm::ConstantInt::get(IGF.IGM.Int32Ty, E->getValue());
}

/// Emit an string literal expression.
static void emitStringLiteralExpr(IRGenFunction &IGF,
                                  StringLiteralExpr *E, 
                                  Explosion &explosion) {
  // CreateGlobalStringPtr adds our nul terminator.
  if (E->getType()->is<BuiltinRawPointerType>()) {
    explosion.addUnmanaged(IGF.Builder.CreateGlobalStringPtr(E->getValue()));
  } else {
    assert(E->getType()->is<TupleType>());
    explosion.addUnmanaged(IGF.Builder.CreateGlobalStringPtr(E->getValue()));
    explosion.addUnmanaged(IGF.Builder.getInt64(E->getValue().size()));
  }
}

static LValue emitDeclRefLValue(IRGenFunction &IGF, DeclRefExpr *E) {
  ValueDecl *D = E->getDecl();
  switch (D->getKind()) {
#define VALUE_DECL(id, parent)
#define DECL(id, parent) case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("decl is not a value decl");

  case DeclKind::TypeAlias:
  case DeclKind::OneOf:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
  case DeclKind::Func:
  case DeclKind::OneOfElement:
    llvm_unreachable("decl cannot be emitted as an l-value");

  case DeclKind::Var:
    if (D->getDeclContext()->isLocalContext())
      return IGF.emitAddressLValue(IGF.getLocalVar(cast<VarDecl>(D)));
    return IGF.getGlobal(cast<VarDecl>(D));
      
  case DeclKind::Subscript:
    llvm_unreachable("subscript decl cannot be referenced");

  case DeclKind::Constructor:
    llvm_unreachable("constructor decl cannot be referenced");

  case DeclKind::Destructor:
    llvm_unreachable("destructor decl cannot be referenced");
  }
  llvm_unreachable("bad decl kind");
}

/// Emit a declaration reference as an exploded r-value.
static void emitDeclRef(IRGenFunction &IGF, DeclRefExpr *E,
                        Explosion &explosion) {
  ValueDecl *D = E->getDecl();
  switch (D->getKind()) {
#define VALUE_DECL(id, parent)
#define DECL(id, parent) case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("decl is not a value decl");

  case DeclKind::TypeAlias:
  case DeclKind::OneOf:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
    emitMetaTypeRef(IGF, D->getType(), explosion);
    return;

  case DeclKind::Var:
    return IGF.emitLValueAsScalar(emitDeclRefLValue(IGF, E),
                                  OnHeap, explosion);

  case DeclKind::Func:
    return emitRValueForFunction(IGF, cast<FuncDecl>(D), explosion);

  case DeclKind::OneOfElement:
    return emitOneOfElementRef(IGF, cast<OneOfElementDecl>(D), explosion);

  case DeclKind::Subscript:
    llvm_unreachable("subscript decl cannot be referenced");

  case DeclKind::Constructor:
    llvm_unreachable("constructor decl cannot be referenced");

  case DeclKind::Destructor:
    llvm_unreachable("destructor decl cannot be referenced");
  }
  llvm_unreachable("bad decl kind");
}

/// Emit the given expression, which must have primitive scalar type,
/// as that primitive scalar value.  This is just a convenience method
/// for not needing to construct and destroy an Explosion.
llvm::Value *IRGenFunction::emitAsPrimitiveScalar(Expr *E) {
  Explosion explosion(ExplosionKind::Minimal);
  emitRValue(E, explosion);

  llvm::Value *result = explosion.claimUnmanagedNext();
  assert(explosion.empty());
  return result;
}

/// Emit a rvalue-to-lvalue conversion.
static OwnedAddress emitMaterializeExpr(IRGenFunction &IGF,
                                        MaterializeExpr *E) {
  // Do we need a heap object?
  OnHeap_t onHeap = isOnHeap(E->getType());

  // Compute the object type.
  Expr *subExpr = E->getSubExpr();
  const TypeInfo &objectTI = IGF.getFragileTypeInfo(subExpr->getType());

  // Begin the initialization.
  Initialization I;
  InitializedObject object = I.getObjectForTemporary();
  I.registerObject(IGF, object, onHeap, objectTI);

  // Allocate.
  OwnedAddress addr =
    I.emitLocalAllocation(IGF, object, onHeap, objectTI,
                          "materialized-temporary");

  // Emit the initializer.
  I.emitInit(IGF, object, addr, subExpr, objectTI);

  // We're done.
  return addr;
}

namespace {
  /// A visitor for emitting a value into an explosion.  We call this
  /// r-value emission, but do note that it's valid to emit an
  /// expression of l-value type in this way; the effect is that of
  /// emitLValueAsScalar.
  class RValueEmitter : public irgen::ExprVisitor<RValueEmitter> {
    IRGenFunction &IGF;
    Explosion &Out;

  public:
    RValueEmitter(IRGenFunction &IGF, Explosion &out) : IGF(IGF), Out(out) {}

    void visitLoadExpr(LoadExpr *E) {
      const TypeInfo &type = IGF.getFragileTypeInfo(E->getType());
      return IGF.emitLoad(IGF.emitLValue(E->getSubExpr()),
                          type, Out);
    }

    void visitMaterializeExpr(MaterializeExpr *E) {
      OwnedAddress addr = emitMaterializeExpr(IGF, E);
      Out.addUnmanaged(addr.getAddressPointer());
    }

    void visitRequalifyExpr(RequalifyExpr *E) {
      emitRequalify(IGF, E, Out);
    }

    void visitTupleExpr(TupleExpr *E) {
      emitTupleLiteral(IGF, E, Out);
    }

    void visitSubscriptExpr(SubscriptExpr *E) {
      IGF.emitLValueAsScalar(emitSubscriptLValue(IGF, E),
                             isOnHeap(E->getType()), Out);
    }

    void visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E) {
      IGF.unimplemented(E->getLBracketLoc(), "existential subscripts");
      IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), Out);
    }

    void visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E) {
      IGF.unimplemented(E->getLBracketLoc(), "archetype subscripts");
      IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), Out);
    }

    void visitGenericSubscriptExpr(GenericSubscriptExpr *E) {
      IGF.unimplemented(E->getLBracketLoc(), "generic subscripts");
      IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), Out);
    }

    void visitTupleShuffleExpr(TupleShuffleExpr *E) {
      emitTupleShuffle(IGF, E, Out);
    }

    void visitFunctionConversionExpr(FunctionConversionExpr *E) {
      IGF.emitRValue(E->getSubExpr(), Out);
    }
    void visitErasureExpr(ErasureExpr *E) {
      emitErasure(IGF, E, Out);
    }
    void visitSpecializeExpr(SpecializeExpr *E) {
      IGF.unimplemented(E->getLoc(), "specialize expressions");
      IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), Out);
    }
    void visitTupleElementExpr(TupleElementExpr *E) {
      emitTupleElement(IGF, E, Out);
    }

    void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      IGF.emitIgnored(E->getLHS());
      IGF.emitRValue(E->getRHS(), Out);
    }
    
    void visitCoerceExpr(CoerceExpr *E) {
      IGF.emitIgnored(E->getLHS());
      IGF.emitRValue(E->getRHS(), Out);
    }

    void visitConstructExpr(ConstructExpr *E) {
      IGF.constructObject(E, Out);
    }

    void visitNewArrayExpr(NewArrayExpr *E) {
      emitNewArrayExpr(IGF, E, Out);
    }

    void visitNewReferenceExpr(NewReferenceExpr *E) {
      emitNewReferenceExpr(IGF, E, Out);
    }

    void visitApplyExpr(ApplyExpr *E) {
      emitApplyExpr(IGF, E, Out);
    }

    void visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
      Out.addUnmanaged(emitIntegerLiteralExpr(IGF, E));
    }

    void visitFloatLiteralExpr(FloatLiteralExpr *E) {
      Out.addUnmanaged(emitFloatLiteralExpr(IGF, E));
    }

    void visitCharacterLiteralExpr(CharacterLiteralExpr *E) {
      Out.addUnmanaged(emitCharacterLiteralExpr(IGF, E));
    }
    void visitStringLiteralExpr(StringLiteralExpr *E) {
      emitStringLiteralExpr(IGF, E, Out);
    }
    void visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E) {
      visit(E->getSemanticExpr());
    }

    void visitDeclRefExpr(DeclRefExpr *E) {
      emitDeclRef(IGF, E, Out);
    }

    void visitMemberRefExpr(MemberRefExpr *E) {
      IGF.emitLValueAsScalar(emitMemberRefLValue(IGF, E),
                             isOnHeap(E->getType()), Out);
    }
    
    void visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E) {
      if (isa<VarDecl>(E->getDecl()) || isa<SubscriptDecl>(E->getDecl())) {
        assert(E->getType()->is<LValueType>());
        return IGF.emitLValueAsScalar(emitExistentialMemberRefLValue(IGF, E),
                                      isOnHeap(E->getType()), Out);
      }

      assert(!E->getType()->is<LValueType>());
      emitExistentialMemberRef(IGF, E, Out);
    }

    void visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E) {
      IGF.unimplemented(E->getLoc(), "archetype member reference");
      IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), Out);
    }

    void visitGenericMemberRefExpr(GenericMemberRefExpr *E) {
      IGF.unimplemented(E->getLoc(), "generic member reference");
      IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), Out);
    }

    void visitCapturingExpr(CapturingExpr *E) {
      emitClosure(IGF, E, Out);
    }

    void visitModuleExpr(ModuleExpr *E) {
      // Nothing to do: modules have no runtime representation.
    }
  };
}

void IRGenFunction::emitRValue(Expr *E, Explosion &explosion) {
  RValueEmitter(*this, explosion).visit(E);
}

/// Emit the given expression as a temporary, casting the address to
/// the given pointer type at the last minute.
static void emitAsCastTemporary(IRGenFunction &IGF, Expr *E,
                                llvm::PointerType *castTy,
                                Explosion &out) {
  auto &actualTI = IGF.getFragileTypeInfo(E->getType());

  // Set up the temporary.
  Initialization init;
  auto object = init.getObjectForTemporary();
  auto cleanup = init.registerObject(IGF, object, NotOnHeap, actualTI);
  auto addr = init.emitLocalAllocation(IGF, object, NotOnHeap, actualTI,
                                       "substitution.temp").getAddress();

  // Initialize into it.
  init.emitInit(IGF, object, addr, E, actualTI);

  // Cast to the expected pointer type.
  addr = IGF.Builder.CreateBitCast(addr, castTy, "temp.cast");

  // Add that to the output explosion.
  out.add(ManagedValue(addr.getAddress(), cleanup));
}

enum class DBAKind : bool{
  Ordinary,
  Argument
};

static bool differsByAbstraction(IRGenModule &IGM,
                                 AnyFunctionType *origTy,
                                 AnyFunctionType *substTy);

/// Does the representation of the first type "differ by abstraction"
/// from the second type, which is the result of applying a
/// substitution to it?
///
/// Because we support rich value types, and because we don't want to
/// always coerce value types into a universal representation (as a
/// dynamically-typed language would have to), the representation of a
/// type with an abstract component may differ from the representation
/// of a type that's fully concrete.
///
/// The fundamental cause of this complication is function types.  For
/// example, a function that returns an Int will return it directly in
/// a register, but a function that returns an abstracted type T will
/// return it indirectly (via a hidden out-parameter); a similar rule
/// applies to parameters.
///
/// This difference then propagates through other structural types,
/// creating a set of general rules for translating values.
///
/// The following is a complete list of the canonical type forms
/// which can contain generic parameters:
///   - generic parameters, e.g. T
///   - tuples, e.g. (T, Int)
///   - functions, e.g. T -> Int
///   - l-values, e.g. [byref] T
///   - generic bindings, e.g. Vector<T>
///   - metatypes ?
///
/// Given a type T and a substitution S, T "differs by
/// abstraction" under S if, informally, its representation
/// is different from that of S(T).
///
/// Note S(T) == T if T is not dependent.  Note also that some
/// substitutions don't cause a change in representation: e.g.
/// if T := U -> Int and S := (T=>Printable), the substitution
/// doesn't change representation because an existential type
/// like Printable is always passed indirectly.
///
/// More formally, T differs by abstraction under S if:
///   - T == (T_1, ..., T_k) and T_i differs by abstraction under S
///     for some i;
///   - T == [byref] U and U differs by abstraction under S;
///   - T == U -> V and either
///       - U differs by abstraction as an argument under S or
///       - V differs by abstraction as a result under S; or
///   - T == U.class and U is dependent but S(U) is not.
/// T differs by abstraction as an argument under S if:
///   - T differs by abstraction under S; or
///   - T is a generic parameter and S(T) is not passed indirectly; or
///   - T == (T_1, ..., T_k) and T_i differs by abstraction as
///     an argument under S for some i.
/// T differs by abstraction as a result under S if:
///   - T differs by abstraction under S or
///   - T is returned indirectly but S(T) is not.
///
/// ** Variables **
///
/// All accessors to a variable must agree on its representation.
/// This is generally okay, because most accesses to a variable
/// are direct accesses, i.e. occur in code where its declaration
/// is known, and that declaration determines its abstraction.
///
/// For example, suppose we have a generic type:
///   class Producer<T> {
///     var f : () -> T
///   }
/// Code that accesses Producer<Int>.f directly will know how the
/// functions stored there are meant to be abstracted because the
/// declaration of 'f' spells it out.  They will know that they
/// cannot store a () -> Int function in that variable; it must
/// first be "thunked" so that it returns indirectly.
///
/// The same rule applies to local variables, which are contained
/// and declared in the context of a possibly-generic function.
///
/// There is (currently) one way in which a variable can be accessed
/// indirectly, without knowledge of how it was originally declared,
/// and that is when it is passed [byref].  A variable cannot be
/// passed directly by reference when the target l-value type
/// differs by abstraction from the variable's type.  However, the
/// mechanics and relatively weak guarantees of [byref] make it
/// legal to instead pass a properly-abstracted temporary variable,
/// thunking the current value as it's passed in and "un-thunking"
/// it on the way out.  Of course, that ain't free.
///
/// \param orig - T in the definition;  the type which the
///   substitution was performed on
/// \param subst - S(T)
static bool differsByAbstraction(IRGenModule &IGM,
                                 CanType orig, CanType subst,
                                 DBAKind diffKind,
                                 ExplosionKind explosionKind) {
  if (orig == subst) return false;

  // Archetypes vary by what we're consider this for.
  if (isa<ArchetypeType>(orig)) {
    if (diffKind == DBAKind::Ordinary) return false;

    // For function arguments, consider whether the substituted type
    // is passed indirectly under the indirect-call conventions.
    return !IGM.isSingleIndirectValue(subst, explosionKind);
  }

  // Tuples are considered element-wise.
  if (auto origTuple = dyn_cast<TupleType>(orig)) {
    auto substTuple = cast<TupleType>(subst);
    for (unsigned i = 0, e = origTuple->getFields().size(); i != e; ++i) {
      if (differsByAbstraction(IGM, CanType(origTuple->getElementType(i)),
                               CanType(substTuple->getElementType(i)),
                               diffKind, explosionKind))
        return true;
    }
    return false;
  }

  // L-values go by the object type;  note that we ask the ordinary
  // question, not the argument question.
  if (auto origLV = dyn_cast<LValueType>(orig)) {
    auto substLV = cast<LValueType>(subst);
    return differsByAbstraction(IGM, CanType(origLV->getObjectType()),
                                CanType(substLV->getObjectType()),
                                DBAKind::Ordinary, ExplosionKind::Minimal);
  }

  // Functions follow complicated rules.
  // FIXME: is this right for PolymorphicFunctionTypes?
  if (auto origFn = dyn_cast<AnyFunctionType>(orig)) {
    auto substFn = cast<AnyFunctionType>(subst);
    return differsByAbstraction(IGM, origFn, substFn);
  }
      
  /// FIXME: don't punt on generic applications for now.
  if (auto origGeneric = dyn_cast<BoundGenericType>(orig)) {
    (void) origGeneric;
    IGM.unimplemented(SourceLoc(), "difference in bound generic");
    return false;
  }

  llvm_unreachable("difference for nominal type?");
}

static bool differsByAbstraction(IRGenModule &IGM,
                                 AnyFunctionType *origTy,
                                 AnyFunctionType *substTy) {
  assert(origTy->isCanonical());
  assert(substTy->isCanonical());

  // Arguments use a different rule for archetypes.
  if (differsByAbstraction(IGM, CanType(origTy->getInput()),
                           CanType(substTy->getInput()),
                           DBAKind::Argument, ExplosionKind::Minimal))
    return true;

  // Results consider ordinary rules...
  if (differsByAbstraction(IGM, CanType(origTy->getResult()),
                           CanType(substTy->getResult()),
                           DBAKind::Ordinary, ExplosionKind::Minimal))
    return true;

  // ...and then we also have to decide whether the substituted
  // makes us stop passing something indirectly.

  // If the abstract type doesn't pass indirectly, the substituted
  // type doesn't either.
  if (!IGM.requiresIndirectResult(origTy->getResult(),
                                  ExplosionKind::Minimal))
    return false;

  // Otherwise, if the substituted type doesn't pass indirectly,
  // we've got a mismatch.
  return !IGM.requiresIndirectResult(substTy->getResult(),
                                     ExplosionKind::Minimal);
}

namespace {
  /// A visitor for emitting substituted r-values.
  class SubstRValueReemitter : public SubstTypeVisitor<SubstRValueReemitter> {
    IRGenFunction &IGF;
    ArrayRef<Substitution> Subs;
    Explosion &In;
    Explosion &Out;
  public:
    SubstRValueReemitter(IRGenFunction &IGF, ArrayRef<Substitution> subs,
                         Explosion &in, Explosion &out)
      : IGF(IGF), Subs(subs), In(in), Out(out) {
      assert(in.getKind() == out.getKind());
    }

    void visitLeafType(CanType origTy, CanType substTy) {
      assert(origTy == substTy);
      Out.transferInto(In, IGF.IGM.getExplosionSize(origTy, In.getKind()));
    }

    void visitArchetypeType(ArchetypeType *origTy, CanType substTy) {
      // Handle the not-unlikely case that the input is a single
      // indirect value.
      if (IGF.IGM.isSingleIndirectValue(substTy, In.getKind())) {
        ManagedValue inValue = In.claimNext();
        auto addr = IGF.Builder.CreateBitCast(inValue.getValue(),
                                              IGF.IGM.OpaquePtrTy,
                                              "substitution.reinterpret");
        Out.add(ManagedValue(addr, inValue.getCleanup()));
        return;
      }

      // Otherwise, we need to make a temporary.
      auto &substTI = IGF.getFragileTypeInfo(substTy);
      initIntoTemporary(substTI);
    }

    void initIntoTemporary(const TypeInfo &substTI) {
      Initialization init;
      auto object = init.getObjectForTemporary();
      auto cleanup = init.registerObject(IGF, object, NotOnHeap, substTI);
      auto addr = init.emitLocalAllocation(IGF, object, NotOnHeap, substTI,
                                           "substitution.temp").getAddress();

      // Initialize into it.
      substTI.initialize(IGF, In, addr);
      init.markInitialized(IGF, object);

      // Cast to the expected pointer type.
      addr = IGF.Builder.CreateBitCast(addr, IGF.IGM.OpaquePtrTy, "temp.cast");

      // Add that to the output explosion.
      Out.add(ManagedValue(addr.getAddress(), cleanup));
    }

    void visitArrayType(ArrayType *origTy, ArrayType *substTy) {
      llvm_unreachable("remapping values of array type");
    }

    void visitBoundGenericType(BoundGenericType *origTy,
                               BoundGenericType *substTy) {
      assert(origTy->getDecl() == substTy->getDecl());

      // If the base type has reference semantics, we can just copy
      // that reference into the output explosion.
      if (origTy->hasReferenceSemantics())
        return In.transferInto(Out, 1);

      // Otherwise, this gets more complicated.
      IGF.unimplemented(SourceLoc(), "remapping bound generic value types");
    }

    void visitAnyFunctionType(AnyFunctionType *origTy,
                              AnyFunctionType *substTy) {
      if (differsByAbstraction(IGF.IGM, origTy, substTy))
        IGF.unimplemented(SourceLoc(), "remapping bound function type");
      In.transferInto(Out, 2);
    }

    void visitLValueType(LValueType *origTy, LValueType *substTy) {
      if (differsByAbstraction(IGF.IGM, CanType(origTy), CanType(substTy),
                               DBAKind::Ordinary, In.getKind()))
        IGF.unimplemented(SourceLoc(), "remapping l-values");
      In.transferInto(Out, 1);
    }

    void visitMetaTypeType(MetaTypeType *origTy, MetaTypeType *substTy) {
      // There's actually nothing to do here right now, but eventually
      // we'll actually implement metatypes with representations.
      IGF.unimplemented(SourceLoc(), "remapping metatypes");
    }

    void visitTupleType(TupleType *origTy, TupleType *substTy) {
      assert(origTy->getFields().size() == substTy->getFields().size());
      for (unsigned i = 0, e = origTy->getFields().size(); i != e; ++i) {
        visit(CanType(origTy->getElementType(i)),
              CanType(substTy->getElementType(i)));
      }
    }
  };
}

/// Re-emit a value under a set of substitutions.
///
/// The explosions have the same kind;  the input obeys
/// the constraints of expectedTy, and the output needs to obey
/// the constraints of substTy.
static void reemitUnderSubstitutions(IRGenFunction &IGF,
                                     ArrayRef<Substitution> subs,
                                     CanType expectedTy, CanType substTy,
                                     Explosion &in, Explosion &out) {
}

static void emitUnderSubstitutions(IRGenFunction &IGF, Expr *E,
                                   CanType expectedType,
                                   ArrayRef<Substitution> subs,
                                   Explosion &out);

namespace {
  /// A visitor for emitting a value under substitution rules.
  ///
  /// Invariants:
  ///  - Substitutions(ExpectedTy) == E->getType().
  class SubstitutedRValueEmitter
      : public irgen::ExprVisitor<SubstitutedRValueEmitter> {
    typedef irgen::ExprVisitor<SubstitutedRValueEmitter> super;

    IRGenFunction &IGF;
    CanType ExpectedTy;
    ArrayRef<Substitution> Substitutions;
    Explosion &Out;
  public:
    SubstitutedRValueEmitter(IRGenFunction &IGF, CanType expected,
                             ArrayRef<Substitution> subs, Explosion &out)
      : IGF(IGF), ExpectedTy(expected), Substitutions(subs), Out(out) {}

    void visitTupleExpr(TupleExpr *E) {
      // The only way that the substituted type can be a tuple when
      // the abstracted type isn't is when the abstracted type is an
      // archetype, which is filtered out already.  Otherwise, we're
      // much down to passing these things indirectly.
      auto expectedTuple = cast<TupleType>(ExpectedTy);
      assert(expectedTuple->getFields().size() == E->getElements().size());
      for (unsigned i = 0, e = expectedTuple->getFields().size(); i != e; ++i) {
        emitUnderSubstitutions(IGF, E->getElements()[i],
                               CanType(expectedTuple->getElementType(i)),
                               Substitutions, Out);
      }
    }

    // TODO: shuffles?

    void visitExpr(Expr *E) {
      CanType substTy = E->getType()->getCanonicalType();

      // TODO: when we're converting to an abstracted function, try to
      // emit a direct-call thunk.

      // Go ahead and emit to an explosion.
      Explosion temp(Out.getKind());
      IGF.emitRValue(E, temp);

      // Re-emit under substitution.
      reemitUnderSubstitutions(IGF, Substitutions, ExpectedTy, substTy,
                               temp, Out);
    }
  };

  /// The definition of "dependence" that we care about is "can it be
  /// a legitimate target of a substitution"?
  struct IsDependent : irgen::TypeVisitor<IsDependent, bool> {
    llvm::SmallPtrSet<ArchetypeType*, 4> BoundTypes;

    bool visitBuiltinType(BuiltinType *T) { return false; }
    bool visitNominalType(NominalType *T) { return false; }
    bool visitModuleType(ModuleType *T) { return false; }

    // FIXME: Some of these are actually rigid and therefore can't be
    // substituted.
    bool visitArchetypeType(ArchetypeType *T) {
      return !BoundTypes.count(T);
    }

    bool visitArrayType(ArrayType *T) {
      return visit(CanType(T->getBaseType()));
    }

    bool visitBoundGenericType(BoundGenericType *T) {
      for (Type arg : T->getGenericArgs())
        if (visit(CanType(arg)))
          return true;
      return false;
    }

    bool visitFunctionType(FunctionType *T) {
      return visit(CanType(T->getInput())) || visit(CanType(T->getResult()));
    }

    bool visitLValueType(LValueType *T) {
      return visit(CanType(T->getObjectType()));
    }

    bool visitMetaTypeType(MetaTypeType *T) {
      return visit(CanType(T->getInstanceType()));
    }

    bool visitPolymorphicFunctionType(PolymorphicFunctionType *T) {
      for (auto &param : T->getGenericParams().getParams()) {
        auto type = param.getAsTypeParam()->getUnderlyingType();
        BoundTypes.insert(cast<ArchetypeType>(type));
      }
      return visit(CanType(T->getInput())) || visit(CanType(T->getResult()));
    }

    // Can this become dependent due to constraints?
    bool visitProtocolCompositionType(ProtocolCompositionType *T) {
      return false;
    }

    bool visitTupleType(TupleType *T) {
      for (auto &elt : T->getFields())
        if (visit(CanType(elt.getType())))
          return true;
      return false;
    }
  };
}

/// Is the given type dependent?
///
/// This really ought to be provided efficiently by every type,
/// but it isn't, and it's not clear that our definition here
/// isn't idiosyncratic.
static bool isDependentType(CanType type) {
  return IsDependent().visit(type);
}

/// A helper routine that does some quick, common filtering before
/// falling back to the general emitter.
static void emitUnderSubstitutions(IRGenFunction &IGF, Expr *E,
                                   CanType expectedType,
                                   ArrayRef<Substitution> subs,
                                   Explosion &out) {
  // If the expected type isn't dependent, just use the normal emitter.
  if (!isDependentType(expectedType))
    return IGF.emitRValue(E, out);

  // It's fairly common to be targetting an archetype.  Filter that
  // out here.  This is also useful because it removes the need for
  // some of the specialized emitters to worry about things like
  // abstracting an entire tuple as a unit.
  if (isa<ArchetypeType>(expectedType)) {
    return emitAsCastTemporary(IGF, E, IGF.IGM.OpaquePtrTy, out);
  }

  // Otherwise, use the specialized emitter.
  SubstitutedRValueEmitter(IGF, expectedType, subs, out).visit(E);  
}

/// Emit a value under substitution rules.
void IRGenFunction::emitRValueUnderSubstitutions(Expr *E, Type expectedType,
                                                 ArrayRef<Substitution> subs,
                                                 Explosion &out) {
  // It's convenient to call this with no substitutions sometimes.
  // Just ignore that now.
  if (subs.empty()) return emitRValue(E, out);

  emitUnderSubstitutions(*this, E, expectedType->getCanonicalType(), subs, out);
}

namespace {
  /// A visitor for emitting a value into memory.  Like r-value
  /// emission, this can actually emit an l-value, with the result
  /// that the address (and possibly the owner) of the l-value are
  /// stored.
  class RValueInitEmitter : public irgen::ExprVisitor<RValueInitEmitter> {
    IRGenFunction &IGF;
    const TypeInfo &AddrTI;
    Address Addr;

  public:
    RValueInitEmitter(IRGenFunction &IGF, const TypeInfo &addrTI, Address addr)
      : IGF(IGF), AddrTI(addrTI), Addr(addr) {}

    void visitExpr(Expr *E) {
      // The default behavior is to emit as an explosion and then
      // initialize from that.
      Explosion explosion(ExplosionKind::Maximal);
      IGF.emitRValue(E, explosion);
      AddrTI.initialize(IGF, explosion, Addr);
    }

    void visitApplyExpr(ApplyExpr *E) {
      emitApplyExprToMemory(IGF, E, Addr, AddrTI);
    }

    void visitLoadExpr(LoadExpr *E) {
      return emitLoadAsInit(IGF, IGF.emitLValue(E->getSubExpr()),
                            Addr, AddrTI);
    }

    void visitErasureExpr(ErasureExpr *E) {
      emitErasureAsInit(IGF, E, Addr, AddrTI);
    }

    // TODO: Implement some other interesting cases that could
    // benefit from this:
    //   TupleExpr
    //   TupleShuffleExpr
  };
}

/// Emit the given expression as the initializer for an object at the
/// given address.  A FullExpr has already been pushed, and a cleanup
/// for the address will be activated immediately after completion.
void IRGenFunction::emitRValueAsInit(Expr *E, Address addr,
                                     const TypeInfo &addrTI) {
  RValueInitEmitter(*this, addrTI, addr).visit(E);
}

namespace {
  class LValueEmitter : public irgen::ExprVisitor<LValueEmitter, LValue> {
    IRGenFunction &IGF;

  public:
    LValueEmitter(IRGenFunction &IGF) : IGF(IGF) {}

#define NOT_LVALUE_EXPR(Id) \
    LValue visit##Id##Expr(Id##Expr *E) { \
      llvm_unreachable("these expression kinds should never be l-values"); \
    }
    NOT_LVALUE_EXPR(Apply)
    NOT_LVALUE_EXPR(IntegerLiteral)
    NOT_LVALUE_EXPR(FloatLiteral)
    NOT_LVALUE_EXPR(CharacterLiteral)
    NOT_LVALUE_EXPR(StringLiteral)
    NOT_LVALUE_EXPR(InterpolatedStringLiteral)
    NOT_LVALUE_EXPR(TupleShuffle)
    NOT_LVALUE_EXPR(Erasure)
    NOT_LVALUE_EXPR(Specialize) // FIXME: Generic subscripts?
    NOT_LVALUE_EXPR(Func)
    NOT_LVALUE_EXPR(Closure)
    NOT_LVALUE_EXPR(Load)
    NOT_LVALUE_EXPR(Tuple)
    NOT_LVALUE_EXPR(NewArray)
    NOT_LVALUE_EXPR(NewReference)
    NOT_LVALUE_EXPR(DotSyntaxBaseIgnored)
    NOT_LVALUE_EXPR(Construct)
    NOT_LVALUE_EXPR(Coerce)
    NOT_LVALUE_EXPR(Module)
#undef NOT_LVALUE_EXPR

    LValue visitTupleElementExpr(TupleElementExpr *E) {
      return emitTupleElementLValue(IGF, E);
    }

    // Qualification never affects emission as an l-value.
    LValue visitRequalifyExpr(RequalifyExpr *E) {
      return visit(E->getSubExpr());
    }

    LValue visitMaterializeExpr(MaterializeExpr *E) {
      OwnedAddress addr = emitMaterializeExpr(IGF, E);
      return IGF.emitAddressLValue(addr);
    }

    LValue visitFunctionConversionExpr(FunctionConversionExpr *E) {
      return visit(E->getSubExpr());
    }
    
    LValue visitDeclRefExpr(DeclRefExpr *E) {
      return emitDeclRefLValue(IGF, E);
    }
    
    LValue visitMemberRefExpr(MemberRefExpr *E) {
      return emitMemberRefLValue(IGF, E);
    }
    
    LValue visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E) {
      return emitExistentialMemberRefLValue(IGF, E);
    }

    LValue visitSubscriptExpr(SubscriptExpr *E) {
      return emitSubscriptLValue(IGF, E);
    }

    LValue visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E) {
      IGF.unimplemented(E->getLBracketLoc(), "existential subscripts");
      return LValue();
    }

    LValue visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E) {
      IGF.unimplemented(E->getLBracketLoc(), "archetype subscripts");
      return LValue();
    }

    LValue visitGenericSubscriptExpr(GenericSubscriptExpr *E) {
      IGF.unimplemented(E->getLBracketLoc(), "generic subscripts");
      return LValue();
    }

    LValue visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E) {
      IGF.unimplemented(E->getLoc(), "archetype member reference");
      return LValue();
    }

    LValue visitGenericMemberRefExpr(GenericMemberRefExpr *E) {
      IGF.unimplemented(E->getLoc(), "generic member reference");
      return LValue();
    }

  };
}

/// Emit the given expression as an l-value.  The expression must
/// actually have l-value kind; to try to find an address for an
/// expression as an aggressive local optimization, use
/// tryEmitAsAddress.
LValue IRGenFunction::emitLValue(Expr *E) {
  assert(E->getType()->is<LValueType>());
  return LValueEmitter(*this).visit(E);
}

namespace {
  class AddressEmitter : public irgen::ASTVisitor<AddressEmitter,
                                                  Optional<Address>,
                                                  void,
                                                  Optional<Address> > {
    IRGenFunction &IGF;
    const TypeInfo &ObjectType;

  public:
    AddressEmitter(IRGenFunction &IGF, const TypeInfo &objectType)
      : IGF(IGF), ObjectType(objectType) {}

#define NON_LOCATEABLE(T) \
    Optional<Address> visit##T(T *D) { return Nothing; }

    // Look through loads without further ado.
    Optional<Address> visitLoadExpr(LoadExpr *E) {
      return visit(E->getSubExpr());
    }

    // We can find addresses for some locals.
    Optional<Address> visitDeclRefExpr(DeclRefExpr *E) {
      return visit(E->getDecl());
    }

    // Visiting a decl is equivalent to visiting a reference to it.

    // Ignore non-value decls.
#define DECL(Id, Parent) \
    Optional<Address> visit##Id##Decl(Id##Decl *D) { \
      llvm_unreachable("not a value decl!"); \
    }
#define VALUE_DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

    // These are r-values.
    NON_LOCATEABLE(FuncDecl)
    NON_LOCATEABLE(OneOfElementDecl)

    // These are potentially supportable.
    NON_LOCATEABLE(TypeAliasDecl)
    NON_LOCATEABLE(OneOfDecl)
    NON_LOCATEABLE(StructDecl)
    NON_LOCATEABLE(ClassDecl)
    NON_LOCATEABLE(ProtocolDecl)
                                                    
    // FIXME: Not really a ValueDecl.
    NON_LOCATEABLE(SubscriptDecl);
    NON_LOCATEABLE(ConstructorDecl);
    NON_LOCATEABLE(DestructorDecl);
                                                    
    // These we support.
    Optional<Address> visitVarDecl(VarDecl *D) {
      // For now, only bother with locals.
      if (!D->getDeclContext()->isLocalContext())
        return Nothing;

      return IGF.getLocalVar(D);
    }

    // Some call results will naturally come back in memory.
    Optional<Address> visitApplyExpr(ApplyExpr *E) {
      return tryEmitApplyAsAddress(IGF, E, ObjectType);
    }

    // Changes in qualification are unimportant for this.
    Optional<Address> visitRequalifyExpr(RequalifyExpr *E) {
      return visit(E->getSubExpr());
    }
    Optional<Address> visitFunctionConversionExpr(FunctionConversionExpr *E) {
      return visit(E->getSubExpr());
    }
    Optional<Address> visitAddressOfExpr(AddressOfExpr *E) {
      return visit(E->getSubExpr());
    }

    // We can locate a tuple element if we can locate the tuple.
    Optional<Address> visitTupleElementExpr(TupleElementExpr *E) {
      return tryEmitTupleElementAsAddress(IGF, E);
    }

    // Materializations are always in memory.
    Optional<Address> visitMaterializeExpr(MaterializeExpr *E) {
      return emitMaterializeExpr(IGF, cast<MaterializeExpr>(E));
    }

    Optional<Address> visitMemberRefExpr(MemberRefExpr *E) {
      return tryEmitMemberRefAsAddress(IGF, E);
    }

    // These expressions aren't naturally already in memory.
    NON_LOCATEABLE(TupleExpr)
    NON_LOCATEABLE(IntegerLiteralExpr)
    NON_LOCATEABLE(FloatLiteralExpr)
    NON_LOCATEABLE(CharacterLiteralExpr)
    NON_LOCATEABLE(StringLiteralExpr)
    NON_LOCATEABLE(InterpolatedStringLiteralExpr)
    NON_LOCATEABLE(TupleShuffleExpr)
    NON_LOCATEABLE(ErasureExpr)
    NON_LOCATEABLE(SpecializeExpr) // FIXME: Generic subscripts?
    NON_LOCATEABLE(CapturingExpr)
    NON_LOCATEABLE(ModuleExpr)
    NON_LOCATEABLE(DotSyntaxBaseIgnoredExpr)
    NON_LOCATEABLE(NewReferenceExpr)
    NON_LOCATEABLE(NewArrayExpr)
    NON_LOCATEABLE(CoerceExpr)
    NON_LOCATEABLE(ConstructExpr)
    NON_LOCATEABLE(ExistentialMemberRefExpr)
    NON_LOCATEABLE(ArchetypeMemberRefExpr)
    NON_LOCATEABLE(GenericMemberRefExpr)

    // FIXME: We may want to specialize IR generation for array subscripts.
    NON_LOCATEABLE(SubscriptExpr);
    NON_LOCATEABLE(ExistentialSubscriptExpr)
    NON_LOCATEABLE(ArchetypeSubscriptExpr)
    NON_LOCATEABLE(GenericSubscriptExpr)
#undef NON_LOCATEABLE
  };
}

/// Try to emit the given expression as an entity with an address.
/// This is useful for local optimizations.
Optional<Address>
IRGenFunction::tryEmitAsAddress(Expr *E, const TypeInfo &type) {
  return AddressEmitter(*this, type).visit(E);
}

namespace {
  struct IgnoredExprEmitter : irgen::ASTVisitor<IgnoredExprEmitter> {
    IRGenFunction &IGF;
    IgnoredExprEmitter(IRGenFunction &IGF) : IGF(IGF) {}

#define EXPR(Id, Parent)
#define UNCHECKED_EXPR(Id, Parent) \
    void visit##Id##Expr(Id##Expr *E) { \
      llvm_unreachable("expression should not have survived to IR-gen"); \
    }
#include "swift/AST/ExprNodes.def"
    void visitErrorExpr(ErrorExpr *E) {
      llvm_unreachable("expression should not have survived to IR-gen");
    }
    void visitIntegerLiteralExpr(IntegerLiteralExpr *E) {}
    void visitFloatLiteralExpr(FloatLiteralExpr *E) {}
    void visitDeclRefExpr(DeclRefExpr *E) {}
    void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      visit(E->getLHS());
      visit(E->getRHS());
    }
    void visitTupleExpr(TupleExpr *E) {
      for (auto elt : E->getElements())
        visit(elt);
    }
    void visitTupleElementExpr(TupleElementExpr *E) {
      visit(E->getBase());
    }
    void visitFuncExpr(FuncExpr *E) {}
    void visitClosureExpr(ClosureExpr *E) {}
    void visitModuleExpr(ModuleExpr *E) {}

#define USING_SUBEXPR(Id) \
    void visit##Id##Expr(Id##Expr *E) { \
      return visit(E->getSubExpr()); \
    }
    USING_SUBEXPR(Paren)
    USING_SUBEXPR(AddressOf)
    USING_SUBEXPR(Requalify)
    USING_SUBEXPR(Materialize)

    void visitTupleShuffleExpr(TupleShuffleExpr *E) {
      // First, evaluate the base expression.
      visit(E->getSubExpr());

      // Then evaluate any defaulted elements.
      TupleType *TT = E->getType()->castTo<TupleType>();
      for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
        if (E->getElementMapping()[i] == -1)
          visit(TT->getFields()[i].getInit()->getExpr());
      }
    }

    void visitExpr(Expr *E) {
      // If all else fails, emit it as an r-value.
      Explosion explosion(ExplosionKind::Maximal);
      IGF.emitRValue(E, explosion);

      // Ignore all the values.
      explosion.ignoreAndDestroy(IGF, explosion.size());
    }
  };
}

/// Emit an expression whose value is being ignored.
void IRGenFunction::emitIgnored(Expr *E) {
  IgnoredExprEmitter(*this).visit(E);
}

/// Emit a fake l-value which obeys the given specification.  This
/// should only ever be used for error recovery.
LValue IRGenFunction::emitFakeLValue(const TypeInfo &type) {
  llvm::Value *fakeAddr =
    llvm::UndefValue::get(type.getStorageType()->getPointerTo());
  return emitAddressLValue(OwnedAddress(Address(fakeAddr, type.StorageAlignment),
                                        IGM.RefCountedNull));
}

void IRGenFunction::emitFakeExplosion(const TypeInfo &type, Explosion &explosion) {
  ExplosionSchema schema(explosion.getKind());
  type.getSchema(schema);
  for (auto &element : schema) {
    llvm::Type *elementType;
    if (element.isAggregate()) {
      elementType = element.getAggregateType()->getPointerTo();
    } else {
      elementType = element.getScalarType();
    }

    explosion.addUnmanaged(llvm::UndefValue::get(elementType));
  }
}
