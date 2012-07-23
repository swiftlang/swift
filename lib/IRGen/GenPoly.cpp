//===--- GenPoly.cpp - Swift IR Generation for Polymorphism ---------------===//
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
//  This file implements IR generation for polymorphic operations in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "llvm/DerivedTypes.h"

#include "ASTVisitor.h"
#include "Explosion.h"
#include "GenInit.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "TypeInfo.h"
#include "TypeVisitor.h"

#include "GenPoly.h"

using namespace swift;
using namespace irgen;

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

/// Answer the differs-by-abstraction question for the given
/// function types.  See the comment below.
static bool differsByAbstraction(IRGenModule &IGM,
                                 AnyFunctionType *origTy,
                                 AnyFunctionType *substTy) {
  assert(origTy->isCanonical());
  assert(substTy->isCanonical());

  // Arguments use a different rule for archetypes.
  if (differsByAbstraction(IGM, CanType(origTy->getInput()),
                           CanType(substTy->getInput()),
                           AbstractionDifference::Argument))
    return true;

  // Results consider ordinary rules...
  if (differsByAbstraction(IGM, CanType(origTy->getResult()),
                           CanType(substTy->getResult()),
                           AbstractionDifference::Memory))
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
namespace {
  class DiffersByAbstraction
      : public SubstTypeVisitor<DiffersByAbstraction, bool> {
    IRGenModule &IGM;
    AbstractionDifference DiffKind;
  public:
    DiffersByAbstraction(IRGenModule &IGM, AbstractionDifference kind)
      : IGM(IGM), DiffKind(kind) {}

    bool visit(CanType origTy, CanType substTy) {
      if (origTy == substTy) return false;
      return super::visit(origTy, substTy);
    }

    bool visitLeafType(CanType origTy, CanType substTy) {
      // The check in visit should make this impossible.
      llvm_unreachable("difference with leaf types");
    }

    bool visitArchetypeType(ArchetypeType *origTy, CanType substTy) {
      // Archetypes vary by what we're considering this for.

      // Archetypes are laid out in memory in the same way as a
      // concrete type would be.
      if (DiffKind == AbstractionDifference::Memory) return false;

      // For function arguments, consider whether the substituted type
      // is passed indirectly under the abstract-call convention.
      // We only ever care about the abstract-call convention.
      return !IGM.isSingleIndirectValue(substTy, ExplosionKind::Minimal);
    }

    bool visitArrayType(ArrayType *origTy, ArrayType *substTy) {
      return visit(CanType(origTy->getBaseType()),
                   CanType(substTy->getBaseType()));
    }

    bool visitBoundGenericType(BoundGenericType *origTy,
                               BoundGenericType *substTy) {
      assert(origTy->getDecl() == substTy->getDecl());
      if (origTy->hasReferenceSemantics())
        return false;

      // Not really sure what to do here?
      IGM.unimplemented(SourceLoc(),
              "testing differ-by-abstraction for bound generic value types");
      return false;
    }

    /// Functions use a more complicated algorithm which calls back
    /// into this.
    bool visitAnyFunctionType(AnyFunctionType *origTy,
                              AnyFunctionType *substTy) {
      return differsByAbstraction(IGM, origTy, substTy);
    }

    // L-values go by the object type;  note that we ask the ordinary
    // question, not the argument question.
    bool visitLValueType(LValueType *origTy, LValueType *substTy) {
      return differsByAbstraction(IGM, CanType(origTy->getObjectType()),
                                  CanType(substTy->getObjectType()),
                                  AbstractionDifference::Memory);
    }

    bool visitMetaTypeType(MetaTypeType *origTy, MetaTypeType *substTy) {
      // There's actually nothing to do here right now, but eventually
      // we'll actually implement metatypes with representations.
      IGM.unimplemented(SourceLoc(),
                        "testing differ-by-abstraction for metatypes");
      return false;
    }

    /// Tuples are considered element-wise.
    bool visitTupleType(TupleType *origTy, TupleType *substTy) {
      assert(origTy->getFields().size() == substTy->getFields().size());
      for (unsigned i = 0, e = origTy->getFields().size(); i != e; ++i)
        if (visit(CanType(origTy->getElementType(i)),
                  CanType(substTy->getElementType(i))))
          return true;
      return false;
    }
  };
}

bool irgen::differsByAbstraction(IRGenModule &IGM,
                                 CanType origTy, CanType substTy,
                                 AbstractionDifference diffKind) {
  return DiffersByAbstraction(IGM, diffKind).visit(origTy, substTy);
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
      if (differsByAbstraction(IGF.IGM,
                               CanType(origTy->getObjectType()),
                               CanType(substTy->getObjectType()),
                               AbstractionDifference::Memory))
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
