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
#include "llvm/IR/DerivedTypes.h"

#include "ASTVisitor.h"
#include "Explosion.h"
#include "GenInit.h"
#include "GenMeta.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "TypeInfo.h"
#include "TypeVisitor.h"

#include "GenPoly.h"

using namespace swift;
using namespace irgen;

/// Ways in which we can test two types differ by abstraction.
enum class AbstractionDifference : bool {
  Memory,
  Explosion
};


/// Answer the differs-by-abstraction question for the given
/// function types.  See the comment below.
bool irgen::differsByAbstractionAsFunction(IRGenModule &IGM,
                                           AnyFunctionType *origTy,
                                           AnyFunctionType *substTy,
                                           ExplosionKind explosionLevel,
                                           unsigned uncurryLevel) {
  assert(origTy->isCanonical());
  assert(substTy->isCanonical());

  // Note that our type-system does not allow types to differ by
  // polymorphism.

  while (true) {
    // Arguments will be exploded.
    if (differsByAbstractionInExplosion(IGM, CanType(origTy->getInput()),
                                        CanType(substTy->getInput()),
                                        explosionLevel))
      return true;

    // Stop processing things as arguments if we're out of uncurryings.
    if (uncurryLevel == 0) break;

    uncurryLevel--;
    origTy = cast<AnyFunctionType>(CanType(origTy->getResult()));
    substTy = cast<AnyFunctionType>(CanType(substTy->getResult()));

    // Fast path.
    if (origTy == substTy) return false;
  }

  // For the result, consider whether it will be passed in memory or
  // exploded.
  CanType origResult = CanType(origTy->getResult());
  CanType substResult = CanType(substTy->getResult());
  if (origResult == substResult) return false;

  // If the abstract type isn't passed indirectly, the substituted
  // type won't be, either.
  if (!IGM.requiresIndirectResult(origResult, explosionLevel)) {
    assert(!IGM.requiresIndirectResult(substResult, explosionLevel));
    // In this case, we must consider whether the exploded difference
    // will matter.
    return differsByAbstractionInExplosion(IGM, origResult, substResult,
                                           explosionLevel);
  }

  // Otherwise, if the substituted type isn't passed indirectly,
  // we've got a mismatch.
  if (!IGM.requiresIndirectResult(substResult, explosionLevel))
    return true;

  // Otherwise, we're passing indirectly, so use memory rules.
  return differsByAbstractionInMemory(IGM, origResult, substResult);
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
    ExplosionKind ExplosionLevel;
    AbstractionDifference DiffKind;
  public:
    DiffersByAbstraction(IRGenModule &IGM, ExplosionKind explosionLevel,
                         AbstractionDifference kind)
      : IGM(IGM), ExplosionLevel(explosionLevel), DiffKind(kind) {}

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
      return !IGM.isSingleIndirectValue(substTy, ExplosionLevel);
    }

    bool visitArrayType(ArrayType *origTy, ArrayType *substTy) {
      return visit(CanType(origTy->getBaseType()),
                   CanType(substTy->getBaseType()));
    }

    bool visitBoundGenericType(BoundGenericType *origTy,
                               BoundGenericType *substTy) {
      assert(origTy->getDecl() == substTy->getDecl());

      // Bound generic types with reference semantics will never
      // differ by abstraction.  Bound generic types with value
      // semantics might someday, if we want things like Optional<T>
      // to have an efficient representation.  For now, though, they
      // don't.
      return false;
    }

    /// Functions use a more complicated algorithm which calls back
    /// into this.
    bool visitAnyFunctionType(AnyFunctionType *origTy,
                              AnyFunctionType *substTy) {
      return differsByAbstractionAsFunction(IGM, origTy, substTy,
                                            ExplosionKind::Minimal,
                                            /*uncurry*/ 0);
    }

    // L-values go by the object type;  note that we ask the ordinary
    // question, not the argument question.
    bool visitLValueType(LValueType *origTy, LValueType *substTy) {
      return differsByAbstractionInMemory(IGM,
                                          CanType(origTy->getObjectType()),
                                          CanType(substTy->getObjectType()));
    }

    bool visitMetaTypeType(MetaTypeType *origTy, MetaTypeType *substTy) {
      // Metatypes can differ by abstraction if the substitution
      // reveals that the type is actually not a class type.
      return (IGM.hasTrivialMetatype(CanType(substTy->getInstanceType())) &&
              !IGM.hasTrivialMetatype(CanType(origTy->getInstanceType())));
    }

    /// Whether we're checking for memory or for an explosion, tuples
    /// are considered element-wise.
    ///
    /// TODO: unless the original tuple contains a variadic explosion,
    /// in which case that portion of the tuple is passed indirectly
    /// in an explosion!
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

bool irgen::differsByAbstractionInMemory(IRGenModule &IGM,
                                         CanType origTy, CanType substTy) {
  return DiffersByAbstraction(IGM, ExplosionKind::Minimal,
                              AbstractionDifference::Memory)
           .visit(origTy, substTy);
}

bool irgen::differsByAbstractionInExplosion(IRGenModule &IGM,
                                            CanType origTy, CanType substTy,
                                            ExplosionKind explosionLevel) {
  return DiffersByAbstraction(IGM, explosionLevel,
                              AbstractionDifference::Explosion)
           .visit(origTy, substTy);
}

/// A class for testing whether a type directly stores an archetype.
struct EmbedsArchetype : irgen::DeclVisitor<EmbedsArchetype, bool>,
                         irgen::TypeVisitor<EmbedsArchetype, bool> {
  IRGenModule &IGM;
  EmbedsArchetype(IRGenModule &IGM) : IGM(IGM) {}

  using irgen::DeclVisitor<EmbedsArchetype, bool>::visit;
  using irgen::TypeVisitor<EmbedsArchetype, bool>::visit;

  bool visitTupleType(TupleType *type) {
    for (auto &field : type->getFields())
      if (visit(CanType(field.getType())))
        return true;
    return false;
  }
  bool visitArchetypeType(ArchetypeType *type) {
    return true;
  }
  bool visitBoundGenericType(BoundGenericType *type) {
    return visit(type->getDecl());
  }
#define FOR_NOMINAL_TYPE(Kind)                 \
  bool visit##Kind##Type(Kind##Type *type) {   \
    return visit##Kind##Decl(type->getDecl()); \
  }
  FOR_NOMINAL_TYPE(Protocol)
  FOR_NOMINAL_TYPE(Struct)
  FOR_NOMINAL_TYPE(Class)
  FOR_NOMINAL_TYPE(OneOf)
#undef FOR_NOMINAL_TYPE

  bool visitArrayType(ArrayType *type) {
    return visit(CanType(type->getBaseType()));
  }

  // All these types are leaves, in the sense that they don't directly
  // store any other types.
  bool visitBuiltinType(BuiltinType *type) { return false; }
  bool visitMetaTypeType(MetaTypeType *type) { return false; }
  bool visitModuleType(ModuleType *type) { return false; }
  bool visitAnyFunctionType(AnyFunctionType *type) { return false; }
  bool visitLValueType(LValueType *type) { return false; }
  bool visitProtocolCompositionType(ProtocolCompositionType *type) {
    return false;
  }

  bool visitProtocolDecl(ProtocolDecl *decl) { return false; }
  bool visitClassDecl(ClassDecl *decl) { return false; }
  bool visitStructDecl(StructDecl *decl) {
    if (IGM.isResilient(decl, ResilienceScope::Local)) return true;
    return visitMembers(decl->getMembers());
  }
  bool visitOneOfDecl(OneOfDecl *decl) {
    if (IGM.isResilient(decl, ResilienceScope::Local)) return true;
    return visitMembers(decl->getMembers());
  }
  bool visitVarDecl(VarDecl *var) {
    if (var->isProperty()) return false;
    return visit(var->getType()->getCanonicalType());
  }
  bool visitOneOfElementDecl(OneOfElementDecl *decl) {
    return visit(decl->getType()->getCanonicalType());
  }
  bool visitDecl(Decl *decl) { return false; }

  bool visitMembers(ArrayRef<Decl*> members) {
    for (auto member : members)
      if (visit(member))
        return true;
    return false;
  }
};

namespace {
  /// A CRTP class for translating substituted explosions into
  /// unsubstituted ones, or in other words, emitting them at a higher
  /// (less concrete) abstraction level.
  class ReemitAsUnsubstituted : public SubstTypeVisitor<ReemitAsUnsubstituted> {
    IRGenFunction &IGF;
    ArrayRef<Substitution> Subs;
    Explosion &In;
    Explosion &Out;
  public:
    ReemitAsUnsubstituted(IRGenFunction &IGF, ArrayRef<Substitution> subs,
                          Explosion &in, Explosion &out)
      : IGF(IGF), Subs(subs), In(in), Out(out) {
      assert(in.getKind() == out.getKind());
    }

    void visitLeafType(CanType origTy, CanType substTy) {
      assert(origTy == substTy);
      In.transferInto(Out, IGF.IGM.getExplosionSize(origTy, Out.getKind()));
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
      if (EmbedsArchetype(IGF.IGM).visitBoundGenericType(origTy))
        IGF.unimplemented(SourceLoc(),
               "remapping bound generic value types with archetype members");

      // FIXME: This is my first shot at implementing this, but it doesn't
      // handle cases which actually need remapping.
      auto n = IGF.IGM.getExplosionSize(CanType(origTy), In.getKind());
      In.transferInto(Out, n);
    }

    void visitAnyFunctionType(AnyFunctionType *origTy,
                              AnyFunctionType *substTy) {
      if (differsByAbstractionAsFunction(IGF.IGM, origTy, substTy,
                                         ExplosionKind::Minimal,
                                         /*uncurry*/ 0))
        IGF.unimplemented(SourceLoc(), "remapping bound function type");
      In.transferInto(Out, 2);
    }

    void visitLValueType(LValueType *origTy, LValueType *substTy) {
      CanType origObjectTy = CanType(origTy->getObjectType());
      CanType substObjectTy = CanType(substTy->getObjectType());
      if (differsByAbstractionInMemory(IGF.IGM, origObjectTy, substObjectTy))
        IGF.unimplemented(SourceLoc(), "remapping l-values");

      ManagedValue substMV = In.claimNext();
      if (origObjectTy == substObjectTy)
        return Out.add(substMV);

      // A bitcast will be sufficient.
      auto &origObjectTI = IGF.IGM.getFragileTypeInfo(origObjectTy);
      auto origPtrTy = origObjectTI.getStorageType()->getPointerTo();

      auto substValue = substMV.getValue();
      auto origValue =
        IGF.Builder.CreateBitCast(substValue, origPtrTy,
                                  substValue->getName() + ".reinterpret");
      Out.add(ManagedValue(origValue, substMV.getCleanup()));
    }

    void visitMetaTypeType(MetaTypeType *origTy, MetaTypeType *substTy) {
      CanType origInstanceTy = CanType(origTy->getInstanceType());
      CanType substInstanceTy = CanType(substTy->getInstanceType());

      // The only metatypes with non-trivial representations are those
      // for archetypes and class types.  A type can't lose the class
      // nature under substitution, so if the substituted type is
      // trivial, the original type either must also be or must be an
      // archetype.
      if (IGF.IGM.hasTrivialMetatype(substInstanceTy)) {
        assert(IGF.IGM.hasTrivialMetatype(origInstanceTy) ||
               isa<ArchetypeType>(origInstanceTy));
        if (isa<ArchetypeType>(origInstanceTy))
          Out.addUnmanaged(emitTypeMetadataRef(IGF, substInstanceTy));
        return;
      }

      // Otherwise, the original type is either a class type or an
      // archetype, and in either case it has a non-trivial representation.
      assert(!IGF.IGM.hasTrivialMetatype(origInstanceTy));
      In.transferInto(Out, 1);
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

/// Given a substituted explosion, re-emit it as an unsubstituted one.
///
/// For example, given an explosion which begins with the
/// representation of an (Int, Float), consume that and produce the
/// representation of an (Int, T).
///
/// The substitutions must carry origTy to substTy.
void irgen::reemitAsUnsubstituted(IRGenFunction &IGF,
                                  CanType expectedTy, CanType substTy,
                                  ArrayRef<Substitution> subs,
                                  Explosion &in, Explosion &out) {
  ReemitAsUnsubstituted(IGF, subs, in, out).visit(expectedTy, substTy);
}

namespace {
  /// A CRTP class for translating unsubstituted explosions into
  /// substituted ones, or in other words, emitting them at a lower
  /// (more concrete) abstraction level.
  class ReemitAsSubstituted : public SubstTypeVisitor<ReemitAsSubstituted> {
    IRGenFunction &IGF;
    ArrayRef<Substitution> Subs;
    Explosion &In;
    Explosion &Out;
  public:
    ReemitAsSubstituted(IRGenFunction &IGF, ArrayRef<Substitution> subs,
                          Explosion &in, Explosion &out)
      : IGF(IGF), Subs(subs), In(in), Out(out) {
      assert(in.getKind() == out.getKind());
    }

    void visitLeafType(CanType origTy, CanType substTy) {
      assert(origTy == substTy);
      In.transferInto(Out, IGF.IGM.getExplosionSize(origTy, In.getKind()));
    }

    /// The unsubstituted type is an archetype.  In explosion terms,
    /// that makes it a single pointer-to-opaque.
    void visitArchetypeType(ArchetypeType *origTy, CanType substTy) {
      auto &substTI = IGF.getFragileTypeInfo(substTy);

      ManagedValue inValue = In.claimNext();
      auto inAddr = IGF.Builder.CreateBitCast(inValue.getValue(),
                                    substTI.getStorageType()->getPointerTo(),
                                              "substitution.reinterpret");

      // If the substituted type is still a single indirect value,
      // just pass it on without reinterpretation.
      if (IGF.IGM.isSingleIndirectValue(substTy, In.getKind())) {
        Out.add(ManagedValue(inAddr, inValue.getCleanup()));
        return;
      }

      // Otherwise, load as a take and then kill the cleanup attached
      // to the archetype value.
      substTI.loadAsTake(IGF, substTI.getAddressForPointer(inAddr), Out);
      if (inValue.hasCleanup())
        IGF.setCleanupState(inValue.getCleanup(), CleanupState::Dead);
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
      if (EmbedsArchetype(IGF.IGM).visitBoundGenericType(origTy))
        IGF.unimplemented(SourceLoc(),
               "remapping bound generic value types with archetype members");

      // FIXME: This is my first shot at implementing this, but it doesn't
      // handle cases which actually need remapping.
      auto n = IGF.IGM.getExplosionSize(CanType(origTy), In.getKind());
      In.transferInto(Out, n);
    }

    void visitAnyFunctionType(AnyFunctionType *origTy,
                              AnyFunctionType *substTy) {
      if (differsByAbstractionAsFunction(IGF.IGM, origTy, substTy,
                                         ExplosionKind::Minimal,
                                         /*uncurry*/ 0))
        IGF.unimplemented(SourceLoc(), "remapping bound function type");
      In.transferInto(Out, 2);
    }

    void visitLValueType(LValueType *origTy, LValueType *substTy) {
      CanType origObjectTy = CanType(origTy->getObjectType());
      CanType substObjectTy = CanType(substTy->getObjectType());
      if (differsByAbstractionInMemory(IGF.IGM, origObjectTy, substObjectTy))
        IGF.unimplemented(SourceLoc(), "remapping l-values");

      ManagedValue origMV = In.claimNext();
      if (origObjectTy == substObjectTy)
        return Out.add(origMV);

      // A bitcast will be sufficient.
      auto &substObjectTI = IGF.IGM.getFragileTypeInfo(substObjectTy);
      auto substPtrTy = substObjectTI.getStorageType()->getPointerTo();

      auto origValue = origMV.getValue();
      auto substValue =
        IGF.Builder.CreateBitCast(origValue, substPtrTy,
                                  origValue->getName() + ".reinterpret");
      Out.add(ManagedValue(substValue, origMV.getCleanup()));
    }

    void visitMetaTypeType(MetaTypeType *origTy, MetaTypeType *substTy) {
      CanType origInstanceTy = CanType(origTy->getInstanceType());
      CanType substInstanceTy = CanType(substTy->getInstanceType());

      // The only metatypes with non-trivial representations are those
      // for archetypes and class types.  A type can't lose the class
      // nature under substitution, so if the substituted type is
      // trivial, the original type either must also be or must be an
      // archetype.
      if (IGF.IGM.hasTrivialMetatype(substInstanceTy)) {
        assert(IGF.IGM.hasTrivialMetatype(origInstanceTy) ||
               isa<ArchetypeType>(origInstanceTy));
        if (isa<ArchetypeType>(origInstanceTy))
          In.ignoreUnmanaged(1);
        return;
      }

      // Otherwise, the original type is either a class type or an
      // archetype, and in either case it has a non-trivial representation.
      assert(!IGF.IGM.hasTrivialMetatype(origInstanceTy));
      In.transferInto(Out, 1);
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

/// Given an unsubstituted explosion, re-emit it as a substituted one.
///
/// For example, given an explosion which begins with the
/// representation of an (Int, T), consume that and produce the
/// representation of an (Int, Float).
///
/// The substitutions must carry origTy to substTy.
void irgen::reemitAsSubstituted(IRGenFunction &IGF,
                                CanType origTy, CanType substTy,
                                ArrayRef<Substitution> subs,
                                Explosion &in, Explosion &out) {
  ReemitAsSubstituted(IGF, subs, in, out).visit(origTy, substTy);
}


void IRGenFunction::emitSupertoArchetypeConversion(Explosion &input,
                                                   CanType destType,
                                                   Address outputArchetype) {
  assert(destType->is<ArchetypeType>() && "expected archetype type");
  
  llvm::Value *superObject = input.forwardNext(*this);
  superObject = Builder.CreateBitCast(superObject, IGM.Int8PtrTy);
  
  // Retrieve the metadata.
  llvm::Value *metadataRef = emitTypeMetadataRef(*this, destType);
  if (metadataRef->getType() != IGM.Int8PtrTy)
    metadataRef = Builder.CreateBitCast(metadataRef, IGM.Int8PtrTy);
  
  // Call the (unconditional) dynamic cast.
  auto call
    = Builder.CreateCall2(IGM.getDynamicCastUnconditionalFn(),
                          superObject, metadataRef);

  // FIXME: Eventually, we may want to throw.
  call->setDoesNotThrow();
  
  // Store the result into the archetype.
  llvm::Value *addr = Builder.CreateBitCast(outputArchetype.getAddress(),
                                            IGM.Int8PtrPtrTy);
  Builder.CreateStore(call, addr, outputArchetype.getAlignment());  
}
