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

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "llvm/IR/DerivedTypes.h"

#include "Explosion.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "TypeVisitor.h"
#include "GenTuple.h"
#include "GenPoly.h"
#include "GenType.h"

using namespace swift;
using namespace irgen;

/// Ways in which we can test two types differ by abstraction.
enum class AbstractionDifference : bool {
  Memory,
  Explosion
};

/// Function abstraction changes should have been handled in SILGen.
/// This function checks that SIL function types are call-compatible.
void checkFunctionsAreCompatible(IRGenModule &IGM,
                                 CanSILFunctionType origTy,
                                 CanSILFunctionType substTy) {
#ifndef NDEBUG
  assert(origTy->getGenericSignature() == substTy->getGenericSignature()
         && "types have different generic signatures");
  
  GenericContextScope scope(IGM, origTy->getGenericSignature());
  
  auto getContextType = [&](CanType t) -> CanType {
    if (t->isDependentType())
      return IGM.getContextArchetypes().substDependentType(t)
        ->getCanonicalType();
    return t;
  };
  
  // The result types must either both be reference types with the same
  // convention, or must be equivalent value types.
  auto origResultTy = getContextType(origTy->getResult().getType());
  auto substResultTy = getContextType(substTy->getResult().getType());
  
  if (origResultTy->hasReferenceSemantics()) {
    assert(substResultTy->hasReferenceSemantics()
           && "result abstraction difference survived to IRGen");
    assert(origTy->getResult().getConvention()
             == substTy->getResult().getConvention()
           && "result abstraction difference survived to IRGen");
  } else {
    // FIXME: Assert that the substTy is a valid substitution of origTy.
    //assert(origTy->getResult() == substTy->getResult()
    //       && "result abstraction difference survived to IRGen");
  }
  assert(origTy->getParameters().size()
           == substTy->getParameters().size()
         && "parameter abstraction difference survived to IRGen");
  for (unsigned i = 0, e = origTy->
       getParameters().size(); i < e; ++i) {
    auto &origParam = origTy->getParameters()[i];
    auto &substParam = substTy->getParameters()[i];
    auto origParamTy = getContextType(origParam.getType());
    auto substParamTy = getContextType(substParam.getType());
    // Direct parameters must be both reference types or matching value types.
    if (!origParam.isIndirect()) {
      if (origParamTy->hasReferenceSemantics()) {
        assert(substParamTy->hasReferenceSemantics()
               && "parameter abstraction difference survived to IRGen");
        assert(origParam.getConvention() == substParam.getConvention()
               && "parameter abstraction difference survived to IRGen");
      } else {
        // FIXME: Assert that the substTy is a valid substitution of origTy.
        //assert(origParam == substParam
        //       && "parameter abstraction difference survived to IRGen");
      }
    }
    // Indirect parameters can differ in type; they're just pointers.
    // The convention must still match.
    else {
      assert(origParam.getConvention() == substParam.getConvention()
             && "parameter abstraction difference survived to IRGen");
    }
  }
#endif
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
///   - l-values, e.g. [inout] T
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
///   - T == [inout] U and U differs by abstraction under S;
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
/// and that is when it is passed 'inout'.  A variable cannot be
/// passed directly by reference when the target l-value type
/// differs by abstraction from the variable's type.  However, the
/// mechanics and relatively weak guarantees of 'inout' make it
/// legal to instead pass a properly-abstracted temporary variable,
/// thunking the current value as it's passed in and "un-thunking"
/// it on the way out.  Of course, that ain't free.
///
/// In the functions below, parameters named \c orig refer to the type T in the
/// definition -- substitution has been performed on this type. Parameters named
/// \c subst refer to a type after substitution, i.e. S(T).
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
      
      // Contextualize dependent types.
      if (origTy->isDependentType())
        origTy = IGM.getContextArchetypes().substDependentType(origTy)
          ->getCanonicalType();
      if (substTy->isDependentType())
        substTy = IGM.getContextArchetypes().substDependentType(substTy)
          ->getCanonicalType();
      
      return super::visit(origTy, substTy);
    }

    bool visitLeafType(CanType origTy, CanType substTy) {
      if (origTy == substTy) return false;
      // The check in visit should make this impossible.
      llvm_unreachable("difference with leaf types");
    }

    // We assume that all reference storage types have equivalent
    // representation.  This may not be true.
    bool visitReferenceStorageType(CanReferenceStorageType origTy,
                                   CanReferenceStorageType substTy) {
      return false;
    }
        
    CanType getArchetypeReprType(CanArchetypeType a) {
      if (Type super = a->getSuperclass())
        return CanType(super);
      return CanType(IGM.Context.TheUnknownObjectType);
    }

    bool visitArchetypeType(CanArchetypeType origTy, CanType substTy) {
      // Archetypes vary by what we're considering this for.

      if (origTy->requiresClass()) {
        // Class archetypes are represented as some refcounted
        // pointer type that needs to be bitcast.
        return origTy != substTy;
      }
      
      // Archetypes are laid out in memory in the same way as a
      // concrete type would be.
      if (DiffKind == AbstractionDifference::Memory) return false;

      auto substType = SILType::getPrimitiveObjectType(substTy);

      // For function arguments, consider whether the substituted type
      // is passed indirectly under the abstract-call convention.
      // We only ever care about the abstract-call convention.
      return !IGM.isSingleIndirectValue(substType);
    }

    bool visitBoundGenericType(CanBoundGenericType origTy,
                               CanBoundGenericType substTy) {
      assert(origTy->getDecl() == substTy->getDecl());

      // Bound generic types with reference semantics will never
      // differ by abstraction.  Bound generic types with value
      // semantics might someday, if we want things like Optional<T>
      // to have an efficient representation.  For now, though, they
      // don't.
      return false;
    }

    bool visitAnyFunctionType(CanAnyFunctionType origTy,
                              CanAnyFunctionType substTy) {
      llvm_unreachable("should have been lowered by SILGen");
    }

    bool visitSILFunctionType(CanSILFunctionType origTy,
                              CanSILFunctionType substTy) {
      // Function abstraction changes should have been handled in SILGen.
      checkFunctionsAreCompatible(IGM, origTy, substTy);
      return false;
    }

    // L-values go by the object type;  note that we ask the ordinary
    // question, not the argument question.
    bool visitLValueType(CanLValueType origTy, CanLValueType substTy) {
      llvm_unreachable("should have been lowered by SILGen");
    }
        
    // inout go by the object type;  note that we ask the ordinary
    // question, not the argument question.
    bool visitInOutType(CanInOutType origTy, CanInOutType substTy) {
      return differsByAbstractionInMemory(IGM,
                                          origTy.getObjectType(),
                                          substTy.getObjectType());
    }

    bool visitMetatypeType(CanMetatypeType origTy, CanMetatypeType substTy) {
      // Metatypes can differ by abstraction if the substitution
      // reveals that the type is actually not a class type.
      return (IGM.isTrivialMetatype(substTy) &&
              !IGM.isTrivialMetatype(origTy));
    }

    /// Whether we're checking for memory or for an explosion, tuples
    /// are considered element-wise.
    ///
    /// TODO: unless the original tuple contains a variadic explosion,
    /// in which case that portion of the tuple is passed indirectly
    /// in an explosion!
    bool visitTupleType(CanTupleType origTy, CanTupleType substTy) {
      assert(origTy->getNumElements() == substTy->getNumElements());
      for (unsigned i = 0, e = origTy->getNumElements(); i != e; ++i)
        if (visit(origTy.getElementType(i), substTy.getElementType(i)))
          return true;
      return false;
    }
        
    /// We shouldn't use block storage pointers in a way that requires
    /// abstraction difference.
    bool visitSILBlockStorageType(CanSILBlockStorageType origTy,
                                  CanSILBlockStorageType substTy) {
      assert(!visit(origTy->getCaptureType(), substTy->getCaptureType())
             && "block storage should not differ by abstraction");
      return false;
    }

    bool visitSILBoxType(CanSILBoxType origTy,
                         CanSILBoxType substTy) {
      // Will be a refcounted pointer regardless of box payload.
      return false;
    }
  };
}

bool irgen::differsByAbstractionInMemory(IRGenModule &IGM,
                                         CanType origTy, CanType substTy) {
  return DiffersByAbstraction(IGM, AbstractionDifference::Memory)
           .visit(origTy, substTy);
}

bool irgen::differsByAbstractionInExplosion(IRGenModule &IGM,
                                            CanType origTy, CanType substTy) {
  return DiffersByAbstraction(IGM, AbstractionDifference::Explosion)
           .visit(origTy, substTy);
}

/// A class for testing whether a type directly stores an archetype.
struct EmbedsArchetype : DeclVisitor<EmbedsArchetype, bool>,
                         CanTypeVisitor<EmbedsArchetype, bool> {
  IRGenModule &IGM;
  EmbedsArchetype(IRGenModule &IGM) : IGM(IGM) {}

  using DeclVisitor<EmbedsArchetype, bool>::visit;
  using CanTypeVisitor<EmbedsArchetype, bool>::visit;

  bool visitTupleType(CanTupleType type) {
    for (auto eltType : type.getElementTypes())
      if (visit(eltType))
        return true;
    return false;
  }
  bool visitArchetypeType(CanArchetypeType type) {
    return true;
  }
  bool visitBoundGenericType(CanBoundGenericType type) {
    return visit(type->getDecl());
  }
#define FOR_NOMINAL_TYPE(Kind)                     \
  bool visit##Kind##Type(Can##Kind##Type type) {   \
    return visit##Kind##Decl(type->getDecl());     \
  }
  FOR_NOMINAL_TYPE(Protocol)
  FOR_NOMINAL_TYPE(Struct)
  FOR_NOMINAL_TYPE(Class)
  FOR_NOMINAL_TYPE(Enum)
#undef FOR_NOMINAL_TYPE

  // All these types are leaves, in the sense that they don't directly
  // store any other types.
  bool visitBuiltinType(CanBuiltinType type) { return false; }
  bool visitAnyMetatypeType(CanAnyMetatypeType type) { return false; }
  bool visitModuleType(CanModuleType type) { return false; }
  bool visitDynamicSelfType(CanDynamicSelfType type) { return false; }
  bool visitAnyFunctionType(CanAnyFunctionType type) { return false; }
  bool visitSILFunctionType(CanSILFunctionType type) { return false; }

  bool visitLValueType(CanLValueType type) { return false; }
  bool visitInOutType(CanInOutType type) { return false; }
  bool visitProtocolCompositionType(CanProtocolCompositionType type) {
    return false;
  }
  bool visitReferenceStorageType(CanReferenceStorageType type) {
    return visit(type.getReferentType());
  }
  bool visitGenericTypeParamType(CanGenericTypeParamType type) {
    // FIXME: These might map down to an archetype.
    return false;
  }
  bool visitDependentMemberType(CanDependentMemberType type) {
    // FIXME: These might map down to an archetype.
    return false;
  }
  bool visitSILBlockStorageType(CanSILBlockStorageType type) {
    return visit(type->getCaptureType());
  }
  bool visitSILBoxType(CanSILBoxType type) {
    return false;
  }
  bool visitProtocolDecl(ProtocolDecl *decl) { return false; }
  bool visitClassDecl(ClassDecl *decl) { return false; }
  bool visitStructDecl(StructDecl *decl) {
    if (IGM.isResilient(decl, ResilienceScope::Local)) return true;
    return visitMembers(decl->getMembers());
  }
  bool visitEnumDecl(EnumDecl *decl) {
    if (IGM.isResilient(decl, ResilienceScope::Local)) return true;
    return visitMembers(decl->getMembers());
  }
  bool visitVarDecl(VarDecl *var) {
    if (!var->hasStorage()) return false;
    return visit(var->getType()->getCanonicalType());
  }
  bool visitEnumElementDecl(EnumElementDecl *decl) {
    return visit(decl->getType()->getCanonicalType());
  }
  bool visitDecl(Decl *decl) { return false; }

  bool visitMembers(DeclRange members) {
    for (auto member : members)
      if (visit(member))
        return true;
    return false;
  }
};

static SILType applyContextArchetypes(IRGenFunction &IGF,
                                      SILType type) {
  if (!type.isDependentType()) {
    return type;
  }

  auto substType =
    IGF.IGM.getContextArchetypes().substDependentType(type.getSwiftRValueType())
      ->getCanonicalType();
  return SILType::getPrimitiveType(substType, type.getCategory());
}

/// Given a substituted explosion, re-emit it as an unsubstituted one.
///
/// For example, given an explosion which begins with the
/// representation of an (Int, Float), consume that and produce the
/// representation of an (Int, T).
///
/// The substitutions must carry origTy to substTy.
void irgen::reemitAsUnsubstituted(IRGenFunction &IGF,
                                  SILType expectedTy, SILType substTy,
                                  Explosion &in, Explosion &out) {
  expectedTy = applyContextArchetypes(IGF, expectedTy);

  ExplosionSchema expectedSchema = IGF.IGM.getSchema(expectedTy);
  assert(expectedSchema.size() ==
         IGF.IGM.getExplosionSize(applyContextArchetypes(IGF, substTy)));
  for (ExplosionSchema::Element &elt : expectedSchema) {
    llvm::Value *value = in.claimNext();
    assert(elt.isScalar());

    // The only type differences we expect here should be due to
    // substitution of class archetypes.
    if (value->getType() != elt.getScalarType()) {
      value = IGF.Builder.CreateBitCast(value, elt.getScalarType(),
                                        value->getName() + ".asUnsubstituted");
    }
    out.add(value);
  }
}
