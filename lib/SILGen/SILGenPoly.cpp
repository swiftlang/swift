//===--- SILGenPoly.cpp - Function Type Thunks ----------------------------===//
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
//
// Swift function types can be equivalent or have a subtyping relationship even
// if the SIL-level lowering of the calling convention is different. The
// routines in this file implement thunking between lowered function types.
//
//
// Re-abstraction thunks
// =====================
// After SIL type lowering, generic substitutions become explicit, for example
// the AST type (Int) -> Int passes the Ints directly, whereas (T) -> T with Int
// substituted for T will pass the Ints like a T, as an address-only value with
// opaque type metadata. Such a thunk is called a "re-abstraction thunk" -- the
// AST-level type of the function value does not change, only the manner in
// which parameters and results are passed. See the comment in
// AbstractionPattern.h for details.
//
// Function conversion thunks
// ==========================
// In Swift's AST-level type system, certain types have a subtype relation
// involving a representation change. For example, a concrete type is always
// a subtype of any protocol it conforms to. The upcast from the concrete
// type to an existential type for the protocol requires packaging the
// payload together with type metadata and witness tables.
//
// Between function types, the type (A) -> B is defined to be a subtype of
// (A') -> B' iff A' is a subtype of A, and B is a subtype of B' -- parameters
// are contravariant, and results are covariant.
//
// A subtype conversion of a function value (A) -> B is performed by wrapping
// the function value in a thunk of type (A') -> B'. The thunk takes an A' and
// converts it into an A, calls the inner function value, and converts the
// result from B to B'.
//
// VTable thunks
// =============
//
// If a base class is generic and a derived class substitutes some generic
// parameter of the base with a concrete type, the derived class can override
// methods in the base that involved generic types. In the derived class, a
// method override that involves substituted types will have a different
// SIL lowering than the base method. In this case, the overridden vtable entry
// will point to a thunk which transforms parameters and results and invokes
// the derived method.
//
// Some limited forms of subtyping are also supported for method overrides;
// namely, a derived method's parameter can be a superclass of, or more
// optional than, a parameter of the base, and result can be a subclass of,
// or less optional than, the result of the base.
//
// Witness thunks
// ==============
//
// Protocol witness methods are called with an additional generic parameter
// bound to the Self type, and thus always require a thunk. Thunks are also
// required for conditional conformances, since the extra requirements aren't
// part of the protocol and so any witness tables need to be loaded from the
// original protocol's witness table and passed into the real witness method.
//
// Thunks for class method witnesses dispatch through the vtable allowing
// inherited witnesses to be overridden in subclasses. Hence a witness thunk
// might require two levels of abstraction difference -- the method might
// override a base class method with more generic types, and the protocol
// requirement may involve associated types which are always concrete in the
// conforming class.
//
// Other thunks
// ============
//
// Foreign-to-native, native-to-foreign thunks for declarations and function
// values are implemented in SILGenBridging.cpp.
//
//===----------------------------------------------------------------------===//

#include "SILGen.h"
#include "SILGenFunction.h"
#include "Scope.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "llvm/Support/Compiler.h"

using namespace swift;
using namespace Lowering;

/// A helper function that pulls an element off the front of an array.
template <class T>
static const T &claimNext(ArrayRef<T> &array) {
  assert(!array.empty() && "claiming next from empty array!");
  const T &result = array.front();
  array = array.slice(1);
  return result;
}

namespace {
  /// An abstract class for transforming first-class SIL values.
  class Transform {
  private:
    SILGenFunction &SGF;
    SILLocation Loc;

  public:
    Transform(SILGenFunction &SGF, SILLocation loc) : SGF(SGF), Loc(loc) {}
    virtual ~Transform() = default;

    /// Transform an arbitrary value.
    RValue transform(RValue &&input,
                     AbstractionPattern inputOrigType,
                     CanType inputSubstType,
                     AbstractionPattern outputOrigType,
                     CanType outputSubstType,
                     SGFContext ctxt);

    /// Transform an arbitrary value.
    ManagedValue transform(ManagedValue input,
                           AbstractionPattern inputOrigType,
                           CanType inputSubstType,
                           AbstractionPattern outputOrigType,
                           CanType outputSubstType,
                           SGFContext ctxt);

    /// Transform a metatype value.
    ManagedValue transformMetatype(ManagedValue fn,
                                   AbstractionPattern inputOrigType,
                                   CanMetatypeType inputSubstType,
                                   AbstractionPattern outputOrigType,
                                   CanMetatypeType outputSubstType);

    /// Transform a tuple value.
    ManagedValue transformTuple(ManagedValue input,
                                AbstractionPattern inputOrigType,
                                CanTupleType inputSubstType,
                                AbstractionPattern outputOrigType,
                                CanTupleType outputSubstType,
                                SGFContext ctxt);

    /// Transform a function value.
    ManagedValue transformFunction(ManagedValue fn,
                                   AbstractionPattern inputOrigType,
                                   CanAnyFunctionType inputSubstType,
                                   AbstractionPattern outputOrigType,
                                   CanAnyFunctionType outputSubstType,
                                   const TypeLowering &expectedTL);
  };
} // end anonymous namespace
;

static ArrayRef<ProtocolConformanceRef>
collectExistentialConformances(ModuleDecl *M, CanType fromType, CanType toType) {
  assert(!fromType.isAnyExistentialType());

  auto layout = toType.getExistentialLayout();
  auto protocols = layout.getProtocols();
  
  SmallVector<ProtocolConformanceRef, 4> conformances;
  for (auto proto : protocols) {
    auto conformance =
      M->lookupConformance(fromType, proto->getDecl());
    conformances.push_back(*conformance);
  }
  
  return M->getASTContext().AllocateCopy(conformances);
}

static ArchetypeType *getOpenedArchetype(CanType openedType) {
  while (auto metatypeTy = dyn_cast<MetatypeType>(openedType))
    openedType = metatypeTy.getInstanceType();
  return cast<ArchetypeType>(openedType);
}

static ManagedValue emitTransformExistential(SILGenFunction &SGF,
                                             SILLocation loc,
                                             ManagedValue input,
                                             CanType inputType,
                                             CanType outputType,
                                             SGFContext ctxt) {
  assert(inputType != outputType);

  SILGenFunction::OpaqueValueState state;
  ArchetypeType *openedArchetype = nullptr;

  if (inputType->isAnyExistentialType()) {
    CanType openedType = ArchetypeType::getAnyOpened(inputType);
    SILType loweredOpenedType = SGF.getLoweredType(openedType);

    // Unwrap zero or more metatype levels
    openedArchetype = getOpenedArchetype(openedType);

    state = SGF.emitOpenExistential(loc, input, openedArchetype,
                                    loweredOpenedType, AccessKind::Read);
    inputType = openedType;
  }

  // Build conformance table
  CanType fromInstanceType = inputType;
  CanType toInstanceType = outputType;
  
  // Look through metatypes
  while (isa<MetatypeType>(fromInstanceType) &&
         isa<ExistentialMetatypeType>(toInstanceType)) {
    fromInstanceType = cast<MetatypeType>(fromInstanceType)
      .getInstanceType();
    toInstanceType = cast<ExistentialMetatypeType>(toInstanceType)
      .getInstanceType();
  }

  ArrayRef<ProtocolConformanceRef> conformances =
      collectExistentialConformances(SGF.SGM.M.getSwiftModule(),
                                     fromInstanceType,
                                     toInstanceType);

  // Build result existential
  AbstractionPattern opaque = AbstractionPattern::getOpaque();
  const TypeLowering &concreteTL = SGF.getTypeLowering(opaque, inputType);
  const TypeLowering &expectedTL = SGF.getTypeLowering(outputType);
  input = SGF.emitExistentialErasure(
                   loc, inputType, concreteTL, expectedTL,
                   conformances, ctxt,
                   [&](SGFContext C) -> ManagedValue {
                     if (openedArchetype)
                       return SGF.manageOpaqueValue(state, loc, C);
                     return input;
                   });
  
  return input;
}

/// Apply this transformation to an arbitrary value.
RValue Transform::transform(RValue &&input,
                            AbstractionPattern inputOrigType,
                            CanType inputSubstType,
                            AbstractionPattern outputOrigType,
                            CanType outputSubstType,
                            SGFContext ctxt) {
  // Fast path: we don't have a tuple.
  auto inputTupleType = dyn_cast<TupleType>(inputSubstType);
  if (!inputTupleType) {
    assert(!isa<TupleType>(outputSubstType) &&
           "transformation introduced a tuple?");
    auto result = transform(std::move(input).getScalarValue(),
                            inputOrigType, inputSubstType,
                            outputOrigType, outputSubstType, ctxt);
    return RValue(SGF, Loc, outputSubstType, result);
  }

  // Okay, we have a tuple.  The output type will also be a tuple unless
  // there's a subtyping conversion that erases tuples, but that's currently
  // not allowed by the typechecker, which considers existential erasure to
  // be a conversion relation, not a subtyping one.  Anyway, it would be
  // possible to support that here, but since it's not currently required...
  assert(isa<TupleType>(outputSubstType) &&
         "subtype constraint erasing tuple is not currently implemented");
  auto outputTupleType = cast<TupleType>(outputSubstType);
  assert(inputTupleType->getNumElements() == outputTupleType->getNumElements());

  // Pull the r-value apart.
  SmallVector<RValue, 8> inputElts;
  std::move(input).extractElements(inputElts);

  // Emit into the context initialization if it's present and possible
  // to split.
  SmallVector<InitializationPtr, 4> eltInitsBuffer;
  MutableArrayRef<InitializationPtr> eltInits;
  auto tupleInit = ctxt.getEmitInto();
  if (!ctxt.getEmitInto()
      || !ctxt.getEmitInto()->canSplitIntoTupleElements()) {
    tupleInit = nullptr;
  } else {
    eltInits = tupleInit->splitIntoTupleElements(SGF, Loc, outputTupleType,
                                                 eltInitsBuffer);
  }

  // At this point, if tupleInit is non-null, we must emit all of the
  // elements into their corresponding contexts.
  assert(tupleInit == nullptr ||
         eltInits.size() == inputTupleType->getNumElements());

  SmallVector<ManagedValue, 8> outputExpansion;
  for (auto eltIndex : indices(inputTupleType->getElementTypes())) {
    // Determine the appropriate context for the element.
    SGFContext eltCtxt;
    if (tupleInit) eltCtxt = SGFContext(eltInits[eltIndex].get());

    // Recurse.
    RValue outputElt = transform(std::move(inputElts[eltIndex]),
                                 inputOrigType.getTupleElementType(eltIndex),
                                 inputTupleType.getElementType(eltIndex),
                                 outputOrigType.getTupleElementType(eltIndex),
                                 outputTupleType.getElementType(eltIndex),
                                 eltCtxt);

    // Force the r-value into its context if necessary.
    assert(!outputElt.isInContext() || tupleInit != nullptr);
    if (tupleInit && !outputElt.isInContext()) {
      std::move(outputElt).forwardInto(SGF, Loc, eltInits[eltIndex].get());
    } else {
      std::move(outputElt).getAll(outputExpansion);
    }
  }

  // If we emitted into context, be sure to finish the overall initialization.
  if (tupleInit) {
    tupleInit->finishInitialization(SGF);
    return RValue::forInContext();
  }

  return RValue(SGF, outputExpansion, outputTupleType);
}

// Single @objc protocol value metatypes can be converted to the ObjC
// Protocol class type.
static bool isProtocolClass(Type t) {
  auto classDecl = t->getClassOrBoundGenericClass();
  if (!classDecl)
    return false;

  ASTContext &ctx = classDecl->getASTContext();
  return (classDecl->getName() == ctx.Id_Protocol &&
          classDecl->getModuleContext()->getName() == ctx.Id_ObjectiveC);
};

static ManagedValue emitManagedLoad(SILGenFunction &SGF, SILLocation loc,
                                    ManagedValue addr,
                                    const TypeLowering &addrTL) {
  // SEMANTIC ARC TODO: When the verifier is finished, revisit this.
  if (!addr.hasCleanup())
    return SGF.B.createLoadBorrow(loc, addr);

  auto loadedValue = addrTL.emitLoad(SGF.B, loc, addr.forward(SGF),
                                     LoadOwnershipQualifier::Take);
  return SGF.emitManagedRValueWithCleanup(loadedValue, addrTL);
}

/// Apply this transformation to an arbitrary value.
ManagedValue Transform::transform(ManagedValue v,
                                  AbstractionPattern inputOrigType,
                                  CanType inputSubstType,
                                  AbstractionPattern outputOrigType,
                                  CanType outputSubstType,
                                  SGFContext ctxt) {
  // Load if the result isn't address-only.  All the translation routines
  // expect this.
  if (v.getType().isAddress()) {
    auto &inputTL = SGF.getTypeLowering(v.getType());
    if (!inputTL.isAddressOnly()) {
      v = emitManagedLoad(SGF, Loc, v, inputTL);
    }
  }

  const TypeLowering &expectedTL = SGF.getTypeLowering(outputOrigType,
                                                       outputSubstType);
  auto loweredResultTy = expectedTL.getLoweredType();

  // Nothing to convert
  if (v.getType() == loweredResultTy)
    return v;

  CanType inputObjectType = inputSubstType.getOptionalObjectType();
  bool inputIsOptional = (bool) inputObjectType;

  CanType outputObjectType = outputSubstType.getOptionalObjectType();
  bool outputIsOptional = (bool) outputObjectType;

  // If the value is less optional than the desired formal type, wrap in
  // an optional.
  if (outputIsOptional && !inputIsOptional) {
    return SGF.emitInjectOptional(
        Loc, expectedTL, ctxt, [&](SGFContext objectCtxt) {
          return transform(v, inputOrigType, inputSubstType,
                           outputOrigType.getOptionalObjectType(),
                           outputObjectType, objectCtxt);
        });
  }

  // If the value is an optional, but the desired formal type isn't an
  // optional or Any, force it.
  if (inputIsOptional && !outputIsOptional &&
      !outputSubstType->isExistentialType()) {
    // isImplicitUnwrap is hardcoded true because the looseness in types of
    // @objc witnesses/overrides that we're handling here only allows IUOs,
    // not explicit Optionals.
    v = SGF.emitCheckedGetOptionalValueFrom(Loc, v,
                                            /*isImplicitUnwrap*/ true, 
                                            SGF.getTypeLowering(v.getType()),
                                            SGFContext());

    // Check if we have any more conversions remaining.
    if (v.getType() == loweredResultTy)
      return v;

    inputIsOptional = false;
  }

  // Optional-to-optional conversion.
  if (inputIsOptional && outputIsOptional) {
    // If the conversion is trivial, just cast.
    if (SGF.SGM.Types.checkForABIDifferences(v.getType(), loweredResultTy)
          == TypeConverter::ABIDifference::Trivial) {
      if (v.getType().isAddress())
        return SGF.B.createUncheckedAddrCast(Loc, v, loweredResultTy);
      return SGF.B.createUncheckedBitCast(Loc, v, loweredResultTy);
    }

    auto transformOptionalPayload =
        [&](SILGenFunction &SGF, SILLocation loc, ManagedValue input,
            SILType loweredResultTy, SGFContext context) -> ManagedValue {
      return transform(input, inputOrigType.getOptionalObjectType(),
                       inputObjectType, outputOrigType.getOptionalObjectType(),
                       outputObjectType, context);
    };

    return SGF.emitOptionalToOptional(Loc, v, loweredResultTy,
                                      transformOptionalPayload);
  }
  
  // Abstraction changes:

  //  - functions
  if (auto outputFnType = dyn_cast<AnyFunctionType>(outputSubstType)) {
    auto inputFnType = cast<AnyFunctionType>(inputSubstType);
    return transformFunction(v,
                             inputOrigType, inputFnType,
                             outputOrigType, outputFnType,
                             expectedTL);
  }

  //  - tuples of transformable values
  if (auto outputTupleType = dyn_cast<TupleType>(outputSubstType)) {
    auto inputTupleType = cast<TupleType>(inputSubstType);
    return transformTuple(v,
                          inputOrigType, inputTupleType,
                          outputOrigType, outputTupleType,
                          ctxt);
  }

  //  - metatypes
  if (auto outputMetaType = dyn_cast<MetatypeType>(outputSubstType)) {
    if (auto inputMetaType = dyn_cast<MetatypeType>(inputSubstType)) {
      return transformMetatype(v,
                               inputOrigType, inputMetaType,
                               outputOrigType, outputMetaType);
    }
  }

  // Subtype conversions:

  //  - upcasts for classes
  if (outputSubstType->getClassOrBoundGenericClass() &&
      inputSubstType->getClassOrBoundGenericClass()) {
    auto class1 = inputSubstType->getClassOrBoundGenericClass();
    auto class2 = outputSubstType->getClassOrBoundGenericClass();

    // CF <-> Objective-C via toll-free bridging.
    if ((class1->getForeignClassKind() == ClassDecl::ForeignKind::CFType) ^
        (class2->getForeignClassKind() == ClassDecl::ForeignKind::CFType)) {
      return SGF.B.createUncheckedRefCast(Loc, v, loweredResultTy);
    }

    if (outputSubstType->isExactSuperclassOf(inputSubstType)) {
      // Upcast to a superclass.
      return SGF.B.createUpcast(Loc, v, loweredResultTy);
    } else {
      // Unchecked-downcast to a covariant return type.
      assert(inputSubstType->isExactSuperclassOf(outputSubstType)
             && "should be inheritance relationship between input and output");
      return SGF.B.createUncheckedRefCast(Loc, v, loweredResultTy);
    }
  }

  // - upcasts for collections
  if (outputSubstType->getStructOrBoundGenericStruct() &&
      inputSubstType->getStructOrBoundGenericStruct()) {
    auto *inputStruct = inputSubstType->getStructOrBoundGenericStruct();
    auto *outputStruct = outputSubstType->getStructOrBoundGenericStruct();

    // Attempt collection upcast only if input and output declarations match.
    if (inputStruct == outputStruct) {
      FuncDecl *fn = nullptr;
      auto &ctx = SGF.getASTContext();
      if (inputStruct == ctx.getArrayDecl()) {
        fn = SGF.SGM.getArrayForceCast(Loc);
      } else if (inputStruct == ctx.getDictionaryDecl()) {
        fn = SGF.SGM.getDictionaryUpCast(Loc);
      } else if (inputStruct == ctx.getSetDecl()) {
        fn = SGF.SGM.getSetUpCast(Loc);
      } else {
        llvm_unreachable("unsupported collection upcast kind");
      }

      return SGF.emitCollectionConversion(Loc, fn, inputSubstType,
                                          outputSubstType, v, ctxt)
                .getScalarValue();
    }
  }

  //  - upcasts from an archetype
  if (outputSubstType->getClassOrBoundGenericClass()) {
    if (auto archetypeType = dyn_cast<ArchetypeType>(inputSubstType)) {
      if (archetypeType->getSuperclass()) {
        // Replace the cleanup with a new one on the superclass value so we
        // always use concrete retain/release operations.
        return SGF.B.createUpcast(Loc, v, loweredResultTy);
      }
    }
  }

  // - metatype to Protocol conversion
  if (isProtocolClass(outputSubstType)) {
    if (auto metatypeTy = dyn_cast<MetatypeType>(inputSubstType)) {
      return SGF.emitProtocolMetatypeToObject(Loc, metatypeTy,
                                   SGF.getLoweredLoadableType(outputSubstType));
    }
  }

  // - metatype to AnyObject conversion
  if (outputSubstType->isAnyObject() &&
      isa<MetatypeType>(inputSubstType)) {
    return SGF.emitClassMetatypeToObject(Loc, v,
                                   SGF.getLoweredLoadableType(outputSubstType));
  }
  
  // - existential metatype to AnyObject conversion
  if (outputSubstType->isAnyObject() &&
      isa<ExistentialMetatypeType>(inputSubstType)) {
    return SGF.emitExistentialMetatypeToObject(Loc, v,
                                   SGF.getLoweredLoadableType(outputSubstType));
  }

  // - block to AnyObject conversion (under ObjC interop)
  if (outputSubstType->isAnyObject() &&
      SGF.getASTContext().LangOpts.EnableObjCInterop) {
    if (auto inputFnType = dyn_cast<AnyFunctionType>(inputSubstType)) {
      if (inputFnType->getRepresentation() == FunctionTypeRepresentation::Block)
        return SGF.B.createBlockToAnyObject(Loc, v, loweredResultTy);
    }
  }

  //  - existentials
  if (outputSubstType->isAnyExistentialType()) {
    // We have to re-abstract payload if its a metatype or a function
    v = SGF.emitSubstToOrigValue(Loc, v, AbstractionPattern::getOpaque(),
                                 inputSubstType);
    return emitTransformExistential(SGF, Loc, v,
                                    inputSubstType, outputSubstType,
                                    ctxt);
  }

  // - upcasting class-constrained existentials or metatypes thereof
  if (inputSubstType->isAnyExistentialType()) {
    auto instanceType = inputSubstType;
    while (auto metatypeType = dyn_cast<ExistentialMetatypeType>(instanceType))
      instanceType = metatypeType.getInstanceType();

    auto layout = instanceType.getExistentialLayout();
    if (layout.explicitSuperclass) {
      CanType openedType = ArchetypeType::getAnyOpened(inputSubstType);
      SILType loweredOpenedType = SGF.getLoweredType(openedType);

      // Unwrap zero or more metatype levels
      auto openedArchetype = getOpenedArchetype(openedType);

      auto state = SGF.emitOpenExistential(Loc, v, openedArchetype,
                                           loweredOpenedType,
                                           AccessKind::Read);
      auto payload = SGF.manageOpaqueValue(state, Loc, SGFContext());
      return transform(payload,
                       AbstractionPattern::getOpaque(),
                       openedType,
                       outputOrigType,
                       outputSubstType,
                       ctxt);
    }
  }

  // - T : Hashable to AnyHashable
  if (isa<StructType>(outputSubstType) &&
      outputSubstType->getAnyNominal() ==
        SGF.getASTContext().getAnyHashableDecl()) {
    auto *protocol = SGF.getASTContext().getProtocol(
        KnownProtocolKind::Hashable);
    auto conformance = SGF.SGM.M.getSwiftModule()->lookupConformance(
        inputSubstType, protocol);
    auto addr = v.getType().isAddress() ? v : v.materialize(SGF, Loc);
    auto result = SGF.emitAnyHashableErasure(Loc, addr,
                                             inputSubstType, *conformance,
                                             ctxt);
    if (result.isInContext())
      return ManagedValue::forInContext();
    return std::move(result).getAsSingleValue(SGF, Loc);
  }

  // Should have handled the conversion in one of the cases above.
  llvm_unreachable("Unhandled transform?");
}

ManagedValue Transform::transformMetatype(ManagedValue meta,
                                          AbstractionPattern inputOrigType,
                                          CanMetatypeType inputSubstType,
                                          AbstractionPattern outputOrigType,
                                          CanMetatypeType outputSubstType) {
  assert(!meta.hasCleanup() && "metatype with cleanup?!");

  auto expectedType = SGF.getTypeLowering(outputOrigType,
                                          outputSubstType).getLoweredType();
  auto wasRepr = meta.getType().castTo<MetatypeType>()->getRepresentation();
  auto willBeRepr = expectedType.castTo<MetatypeType>()->getRepresentation();
  
  SILValue result;

  if ((wasRepr == MetatypeRepresentation::Thick &&
       willBeRepr == MetatypeRepresentation::Thin) ||
      (wasRepr == MetatypeRepresentation::Thin &&
       willBeRepr == MetatypeRepresentation::Thick)) {
    // If we have a thin-to-thick abstraction change, cook up new a metatype
    // value out of nothing -- thin metatypes carry no runtime state.
    result = SGF.B.createMetatype(Loc, expectedType);
  } else {
    // Otherwise, we have a metatype subtype conversion of thick metatypes.
    assert(wasRepr == willBeRepr && "Unhandled metatype conversion");
    result = SGF.B.createUpcast(Loc, meta.getUnmanagedValue(), expectedType);
  }

  return ManagedValue::forUnmanaged(result);
}

/// Explode a managed tuple into a bunch of managed elements.
///
/// If the tuple is in memory, the result elements will also be in
/// memory.
static void explodeTuple(SILGenFunction &SGF, SILLocation loc,
                         ManagedValue managedTuple,
                         SmallVectorImpl<ManagedValue> &out) {
  // If the tuple is empty, there's nothing to do.
  if (managedTuple.getType().castTo<TupleType>()->getNumElements() == 0)
    return;

  SmallVector<SILValue, 16> elements;
  bool isPlusOne = managedTuple.hasCleanup();

  if (managedTuple.getType().isAddress()) {
    SGF.B.emitShallowDestructureAddressOperation(loc, managedTuple.forward(SGF),
                                                 elements);
  } else {
    SGF.B.emitShallowDestructureValueOperation(loc, managedTuple.forward(SGF),
                                               elements);
  }

  for (auto element : elements) {
    if (!isPlusOne)
      out.push_back(ManagedValue::forUnmanaged(element));
    else if (element->getType().isAddress())
      out.push_back(SGF.emitManagedBufferWithCleanup(element));
    else
      out.push_back(SGF.emitManagedRValueWithCleanup(element));
  }
}

/// Apply this transformation to all the elements of a tuple value,
/// which just entails mapping over each of its component elements.
ManagedValue Transform::transformTuple(ManagedValue inputTuple,
                                       AbstractionPattern inputOrigType,
                                       CanTupleType inputSubstType,
                                       AbstractionPattern outputOrigType,
                                       CanTupleType outputSubstType,
                                       SGFContext ctxt) {
  const TypeLowering &outputTL =
    SGF.getTypeLowering(outputOrigType, outputSubstType);
  assert((outputTL.isAddressOnly() == inputTuple.getType().isAddress() ||
          !SGF.silConv.useLoweredAddresses()) &&
         "expected loadable inputs to have been loaded");

  // If there's no representation difference, we're done.
  if (outputTL.getLoweredType() == inputTuple.getType())
    return inputTuple;

  assert(inputOrigType.matchesTuple(outputSubstType));
  assert(outputOrigType.matchesTuple(outputSubstType));

  auto inputType = inputTuple.getType().castTo<TupleType>();
  assert(outputSubstType->getNumElements() == inputType->getNumElements());

  // If the tuple is address only, we need to do the operation in memory.
  SILValue outputAddr;
  if (outputTL.isAddressOnly() && SGF.silConv.useLoweredAddresses())
    outputAddr = SGF.getBufferForExprResult(Loc, outputTL.getLoweredType(),
                                            ctxt);

  // Explode the tuple into individual managed values.
  SmallVector<ManagedValue, 4> inputElts;
  explodeTuple(SGF, Loc, inputTuple, inputElts);

  // Track all the managed elements whether or not we're actually
  // emitting to an address, just so that we can disable them after.
  SmallVector<ManagedValue, 4> outputElts;

  for (auto index : indices(inputType->getElementTypes())) {
    auto &inputEltTL = SGF.getTypeLowering(inputElts[index].getType());
    ManagedValue inputElt = inputElts[index];
    if (inputElt.getType().isAddress() && !inputEltTL.isAddressOnly()) {
      inputElt = emitManagedLoad(SGF, Loc, inputElt, inputEltTL);
    }

    auto inputEltOrigType = inputOrigType.getTupleElementType(index);
    auto inputEltSubstType = inputSubstType.getElementType(index);
    auto outputEltOrigType = outputOrigType.getTupleElementType(index);
    auto outputEltSubstType = outputSubstType.getElementType(index);

    // If we're emitting to memory, project out this element in the
    // destination buffer, then wrap that in an Initialization to
    // track the cleanup.
    Optional<TemporaryInitialization> outputEltTemp;
    if (outputAddr) {
      SILValue outputEltAddr =
        SGF.B.createTupleElementAddr(Loc, outputAddr, index);
      auto &outputEltTL = SGF.getTypeLowering(outputEltAddr->getType());
      assert(outputEltTL.isAddressOnly() == inputEltTL.isAddressOnly());
      auto cleanup =
        SGF.enterDormantTemporaryCleanup(outputEltAddr, outputEltTL);
      outputEltTemp.emplace(outputEltAddr, cleanup);
    }

    SGFContext eltCtxt =
      (outputEltTemp ? SGFContext(&outputEltTemp.getValue()) : SGFContext());
    auto outputElt = transform(inputElt,
                               inputEltOrigType, inputEltSubstType,
                               outputEltOrigType, outputEltSubstType,
                               eltCtxt);

    // If we're not emitting to memory, remember this element for
    // later assembly into a tuple.
    if (!outputEltTemp) {
      assert(outputElt);
      assert(!inputEltTL.isAddressOnly() || !SGF.silConv.useLoweredAddresses());
      outputElts.push_back(outputElt);
      continue;
    }

    // Otherwise, make sure we emit into the slot.
    auto &temp = outputEltTemp.getValue();
    auto outputEltAddr = temp.getManagedAddress();

    // That might involve storing directly.
    if (!outputElt.isInContext()) {
      outputElt.forwardInto(SGF, Loc, outputEltAddr.getValue());
      temp.finishInitialization(SGF);
    }

    outputElts.push_back(outputEltAddr);
  }

  // Okay, disable all the individual element cleanups and collect
  // the values for a potential tuple aggregate.
  SmallVector<SILValue, 4> outputEltValues;
  for (auto outputElt : outputElts) {
    SILValue value = outputElt.forward(SGF);
    if (!outputAddr) outputEltValues.push_back(value);
  }

  // If we're emitting to an address, just manage that.
  if (outputAddr)
    return SGF.manageBufferForExprResult(outputAddr, outputTL, ctxt);

  // Otherwise, assemble the tuple value and manage that.
  auto outputTuple =
    SGF.B.createTuple(Loc, outputTL.getLoweredType(), outputEltValues);
  return SGF.emitManagedRValueWithCleanup(outputTuple, outputTL);
}

void SILGenFunction::collectThunkParams(
    SILLocation loc, SmallVectorImpl<ManagedValue> &params,
    SmallVectorImpl<SILArgument *> *indirectResults) {
  // Add the indirect results.
  for (auto resultTy : F.getConventions().getIndirectSILResultTypes()) {
    auto paramTy = F.mapTypeIntoContext(resultTy);
    SILArgument *arg = F.begin()->createFunctionArgument(paramTy);
    if (indirectResults)
      indirectResults->push_back(arg);
  }

  // Add the parameters.
  auto paramTypes = F.getLoweredFunctionType()->getParameters();
  for (auto param : paramTypes) {
    auto paramTy = F.mapTypeIntoContext(F.getConventions().getSILType(param));
    params.push_back(B.createInputFunctionArgument(paramTy, loc));
  }
}

/// Force a ManagedValue to be stored into a temporary initialization
/// if it wasn't emitted that way directly.
static void emitForceInto(SILGenFunction &SGF, SILLocation loc,
                          ManagedValue result, TemporaryInitialization &temp) {
  if (result.isInContext()) return;
  result.ensurePlusOne(SGF, loc).forwardInto(SGF, loc, temp.getAddress());
  temp.finishInitialization(SGF);
}

namespace {
  class TranslateIndirect : public Cleanup {
    AbstractionPattern InputOrigType, OutputOrigType;
    CanType InputSubstType, OutputSubstType;
    SILValue Input, Output;

  public:
    TranslateIndirect(AbstractionPattern inputOrigType, CanType inputSubstType,
                      AbstractionPattern outputOrigType, CanType outputSubstType,
                      SILValue input, SILValue output)
      : InputOrigType(inputOrigType), OutputOrigType(outputOrigType),
        InputSubstType(inputSubstType), OutputSubstType(outputSubstType),
        Input(input), Output(output) {
      assert(input->getType().isAddress());
      assert(output->getType().isAddress());
    }

    void emit(SILGenFunction &SGF, CleanupLocation loc,
              ForUnwind_t forUnwind) override {
      FullExpr scope(SGF.Cleanups, loc);

      // Re-assert ownership of the input value.
      auto inputMV = SGF.emitManagedBufferWithCleanup(Input);

      // Set up an initialization of the output buffer.
      auto &outputTL = SGF.getTypeLowering(Output->getType());
      auto outputInit = SGF.useBufferAsTemporary(Output, outputTL);

      // Transform into the output buffer.
      auto mv = SGF.emitTransformedValue(loc, inputMV,
                                         InputOrigType, InputSubstType,
                                         OutputOrigType, OutputSubstType,
                                         SGFContext(outputInit.get()));
      emitForceInto(SGF, loc, mv, *outputInit);

      // Disable the cleanup; we've kept our promise to leave the inout
      // initialized.
      outputInit->getManagedAddress().forward(SGF);
    }

    void dump(SILGenFunction &SGF) const override {
      llvm::errs() << "TranslateIndirect("
        << InputOrigType << ", " << InputSubstType << ", "
        << OutputOrigType << ", " << OutputSubstType << ", "
        << Output << ", " << Input << ")\n";
    }
  };

  class TranslateArguments {
    SILGenFunction &SGF;
    SILLocation Loc;
    ArrayRef<ManagedValue> Inputs;
    SmallVectorImpl<ManagedValue> &Outputs;
    ArrayRef<SILParameterInfo> OutputTypes;
  public:
    TranslateArguments(SILGenFunction &SGF, SILLocation loc,
                       ArrayRef<ManagedValue> inputs,
                       SmallVectorImpl<ManagedValue> &outputs,
                       ArrayRef<SILParameterInfo> outputTypes)
      : SGF(SGF), Loc(loc), Inputs(inputs), Outputs(outputs),
        OutputTypes(outputTypes) {}

    void translate(AbstractionPattern inputOrigFunctionType,
                   AnyFunctionType::CanParamArrayRef inputSubstTypes,
                   AbstractionPattern outputOrigFunctionType,
                   AnyFunctionType::CanParamArrayRef outputSubstTypes) {
      if (inputSubstTypes.size() == 1 &&
          outputSubstTypes.size() != 1) {
        // SE-0110 tuple splat. Turn the output into a single value of tuple
        // type, and translate.
        auto inputOrigType = inputOrigFunctionType.getFunctionParamType(0);
        auto inputSubstType = inputSubstTypes[0].getPlainType();

        // Build an abstraction pattern for the output.
        SmallVector<AbstractionPattern, 8> outputOrigTypes;
        for (unsigned i = 0, e = outputOrigFunctionType.getNumFunctionParams();
             i < e; ++i) {
          outputOrigTypes.push_back(
            outputOrigFunctionType.getFunctionParamType(i));
        }
        auto outputOrigType = AbstractionPattern::getTuple(outputOrigTypes);

        // Build the substituted output tuple type. Note that we deliberately
        // don't use composeInput() because we want to drop ownership
        // qualifiers.
        SmallVector<TupleTypeElt, 8> elts;
        for (auto param : outputSubstTypes) {
          assert(!param.isVariadic());
          assert(!param.isInOut());
          elts.emplace_back(param.getParameterType());
        }
        auto outputSubstType = cast<TupleType>(
          TupleType::get(elts, SGF.getASTContext())
            ->getCanonicalType());

        // Translate the input tuple value into the output tuple value. Note
        // that the output abstraction pattern is a tuple, and we explode tuples
        // into multiple parameters, so if the input abstraction pattern is
        // opaque, this will explode the input value. Otherwise, the input
        // parameters will be mapped one-to-one to the output parameters.
        translate(inputOrigType, inputSubstType,
                  outputOrigType, outputSubstType);
        return;
      }

      // Otherwise, parameters are always reabstracted one-to-one.
      assert(inputSubstTypes.size() == outputSubstTypes.size());

      SmallVector<AbstractionPattern, 8> inputOrigTypes;
      SmallVector<AbstractionPattern, 8> outputOrigTypes;
      for (auto i : indices(inputSubstTypes)) {
        inputOrigTypes.push_back(inputOrigFunctionType.getFunctionParamType(i));
        outputOrigTypes.push_back(outputOrigFunctionType.getFunctionParamType(i));
      }

      translate(inputOrigTypes, inputSubstTypes,
                outputOrigTypes, outputSubstTypes);
    }

    void translate(ArrayRef<AbstractionPattern> inputOrigTypes,
                   AnyFunctionType::CanParamArrayRef inputSubstTypes,
                   ArrayRef<AbstractionPattern> outputOrigTypes,
                   AnyFunctionType::CanParamArrayRef outputSubstTypes) {
      assert(inputOrigTypes.size() == inputSubstTypes.size());
      assert(outputOrigTypes.size() == outputSubstTypes.size());
      assert(inputOrigTypes.size() == outputOrigTypes.size());

      for (auto i : indices(inputOrigTypes)) {
        translate(inputOrigTypes[i], inputSubstTypes[i],
                  outputOrigTypes[i], outputSubstTypes[i]);
      }
    }

    void translate(AbstractionPattern inputOrigType,
                   AnyFunctionType::CanParam inputSubstType,
                   AbstractionPattern outputOrigType,
                   AnyFunctionType::CanParam outputSubstType) {
      // Note that it's OK for the input to be inout but not the output;
      // this means we're just going to load the inout and pass it on as a
      // scalar.
      if (outputSubstType.isInOut()) {
        assert(inputSubstType.isInOut());
        auto inputValue = claimNextInput();
        auto outputLoweredTy = claimNextOutputType();

        translateInOut(inputOrigType, inputSubstType.getParameterType(),
                       outputOrigType, outputSubstType.getParameterType(),
                       inputValue, outputLoweredTy);
      } else {
        translate(inputOrigType, inputSubstType.getParameterType(),
                  outputOrigType, outputSubstType.getParameterType());
      }
    }


    void translate(AbstractionPattern inputOrigType,
                   CanType inputSubstType,
                   AbstractionPattern outputOrigType,
                   CanType outputSubstType) {
      // Most of this function is about tuples: tuples can be represented
      // as one or many values, with varying levels of indirection.
      auto inputTupleType = dyn_cast<TupleType>(inputSubstType);
      auto outputTupleType = dyn_cast<TupleType>(outputSubstType);

      // Case where the input type is an exploded tuple.
      if (inputOrigType.isTuple()) {
        if (outputOrigType.isTuple()) {
          // Both input and output are exploded tuples, easy case.
          return translateParallelExploded(inputOrigType,
                                           inputTupleType,
                                           outputOrigType,
                                           outputTupleType);
        }

        // Tuple types are subtypes of their optionals
        if (auto outputObjectType = outputSubstType.getOptionalObjectType()) {
          auto outputOrigObjectType = outputOrigType.getOptionalObjectType();

          if (auto outputTupleType = dyn_cast<TupleType>(outputObjectType)) {
            // The input is exploded and the output is an optional tuple.
            // Translate values and collect them into a single optional
            // payload.

            auto result =
                translateAndImplodeIntoOptional(inputOrigType,
                                                inputTupleType,
                                                outputOrigObjectType,
                                                outputTupleType);
            Outputs.push_back(result);
            return;
          }

          // Tuple types are subtypes of optionals of Any, too.
          assert(outputObjectType->isAny());

          // First, construct the existential.
          auto result =
              translateAndImplodeIntoAny(inputOrigType,
                                         inputTupleType,
                                         outputOrigObjectType,
                                         outputObjectType);

          // Now, convert it to an optional.
          translateSingle(outputOrigObjectType, outputObjectType,
                          outputOrigType, outputSubstType,
                          result, claimNextOutputType());
          return;
        }

        if (outputSubstType->isAny()) {
          claimNextOutputType();

          auto result =
              translateAndImplodeIntoAny(inputOrigType,
                                         inputTupleType,
                                         outputOrigType,
                                         outputSubstType);
          Outputs.push_back(result);
          return;
        }

        if (outputTupleType) {
          // The input is exploded and the output is not. Translate values
          // and store them to a result tuple in memory.
          assert(outputOrigType.isTypeParameter() &&
                 "Output is not a tuple and is not opaque?");

          auto outputTy = SGF.getSILType(claimNextOutputType());
          auto &outputTL = SGF.getTypeLowering(outputTy);
          if (SGF.silConv.useLoweredAddresses()) {
            auto temp = SGF.emitTemporary(Loc, outputTL);
            translateAndImplodeInto(inputOrigType, inputTupleType,
                                    outputOrigType, outputTupleType, *temp);

            Outputs.push_back(temp->getManagedAddress());
          } else {
            auto result = translateAndImplodeIntoValue(
                inputOrigType, inputTupleType, outputOrigType, outputTupleType,
                outputTL.getLoweredType());
            Outputs.push_back(result);
          }
          return;
        }

        llvm_unreachable("Unhandled conversion from exploded tuple");
      }

      // Handle output being an exploded tuple when the input is opaque.
      if (outputOrigType.isTuple()) {
        if (inputTupleType) {
          // The input is exploded and the output is not. Translate values
          // and store them to a result tuple in memory.
          assert(inputOrigType.isTypeParameter() &&
                 "Input is not a tuple and is not opaque?");

          return translateAndExplodeOutOf(inputOrigType,
                                          inputTupleType,
                                          outputOrigType,
                                          outputTupleType,
                                          claimNextInput());
        }

        // FIXME: IUO<Tuple> to Tuple
        llvm_unreachable("Unhandled conversion to exploded tuple");
      }

      // Okay, we are now working with a single value turning into a
      // single value.
      auto inputElt = claimNextInput();
      auto outputEltType = claimNextOutputType();
      translateSingle(inputOrigType, inputSubstType,
                      outputOrigType, outputSubstType,
                      inputElt, outputEltType);
    }

  private:
    /// Take a tuple that has been exploded in the input and turn it into
    /// a tuple value in the output.
    ManagedValue translateAndImplodeIntoValue(AbstractionPattern inputOrigType,
                                              CanTupleType inputType,
                                              AbstractionPattern outputOrigType,
                                              CanTupleType outputType,
                                              SILType loweredOutputTy) {
      assert(loweredOutputTy.is<TupleType>());

      SmallVector<ManagedValue, 4> elements;
      assert(outputType->getNumElements() == inputType->getNumElements());
      for (unsigned i : indices(outputType->getElementTypes())) {
        auto inputOrigEltType = inputOrigType.getTupleElementType(i);
        auto inputEltType = inputType.getElementType(i);
        auto outputOrigEltType = outputOrigType.getTupleElementType(i);
        auto outputEltType = outputType.getElementType(i);
        SILType loweredOutputEltTy = loweredOutputTy.getTupleElementType(i);

        ManagedValue elt;
        if (auto inputEltTupleType = dyn_cast<TupleType>(inputEltType)) {
          elt = translateAndImplodeIntoValue(inputOrigEltType,
                                             inputEltTupleType,
                                             outputOrigEltType,
                                             cast<TupleType>(outputEltType),
                                             loweredOutputEltTy);
        } else {
          elt = claimNextInput();

          // Load if necessary.
          if (elt.getType().isAddress()) {
            elt = SGF.emitLoad(Loc, elt.forward(SGF),
                               SGF.getTypeLowering(elt.getType()),
                               SGFContext(), IsTake);
          }
        }

        if (elt.getType() != loweredOutputEltTy)
          elt = translatePrimitive(inputOrigEltType, inputEltType,
                                   outputOrigEltType, outputEltType,
                                   elt);

        elements.push_back(elt);
      }

      SmallVector<SILValue, 4> forwarded;
      for (auto &elt : elements)
        forwarded.push_back(elt.forward(SGF));

      auto tuple = SGF.B.createTuple(Loc, loweredOutputTy, forwarded);
      return SGF.emitManagedRValueWithCleanup(tuple);
    }

    /// Handle a tuple that has been exploded in the input but wrapped in
    /// an optional in the output.
    ManagedValue
    translateAndImplodeIntoOptional(AbstractionPattern inputOrigType,
                                    CanTupleType inputTupleType,
                                    AbstractionPattern outputOrigType,
                                    CanTupleType outputTupleType) {
      assert(!inputTupleType->hasElementWithOwnership() &&
             !outputTupleType->hasElementWithOwnership());
      assert(inputTupleType->getNumElements() ==
             outputTupleType->getNumElements());

      // Collect the tuple elements.
      auto &loweredTL = SGF.getTypeLowering(outputOrigType, outputTupleType);
      auto loweredTy = loweredTL.getLoweredType();
      auto optionalTy = SGF.getSILType(claimNextOutputType());
      auto someDecl = SGF.getASTContext().getOptionalSomeDecl();
      if (loweredTL.isLoadable() || !SGF.silConv.useLoweredAddresses()) {
        auto payload =
          translateAndImplodeIntoValue(inputOrigType, inputTupleType,
                                       outputOrigType, outputTupleType,
                                       loweredTy);

        return SGF.B.createEnum(Loc, payload, someDecl, optionalTy);
      } else {
        auto optionalBuf = SGF.emitTemporaryAllocation(Loc, optionalTy);
        auto tupleBuf = SGF.B.createInitEnumDataAddr(Loc, optionalBuf, someDecl,
                                                     loweredTy);
        
        auto tupleTemp = SGF.useBufferAsTemporary(tupleBuf, loweredTL);

        translateAndImplodeInto(inputOrigType, inputTupleType,
                                outputOrigType, outputTupleType,
                                *tupleTemp);

        SGF.B.createInjectEnumAddr(Loc, optionalBuf, someDecl);

        auto payload = tupleTemp->getManagedAddress();
        if (payload.hasCleanup()) {
          payload.forward(SGF);
          return SGF.emitManagedBufferWithCleanup(optionalBuf);
        }
        return ManagedValue::forUnmanaged(optionalBuf);
      }
    }

    /// Handle a tuple that has been exploded in the input but wrapped
    /// in an existential in the output.
    ManagedValue
    translateAndImplodeIntoAny(AbstractionPattern inputOrigType,
                               CanTupleType inputTupleType,
                               AbstractionPattern outputOrigType,
                               CanType outputSubstType) {
      auto existentialTy = SGF.getLoweredType(outputOrigType, outputSubstType);
      auto existentialBuf = SGF.emitTemporaryAllocation(Loc, existentialTy);

      auto opaque = AbstractionPattern::getOpaque();
      auto &concreteTL = SGF.getTypeLowering(opaque, inputTupleType);

      auto tupleBuf =
        SGF.B.createInitExistentialAddr(Loc, existentialBuf,
                                        inputTupleType,
                                        concreteTL.getLoweredType(),
                                        /*conformances=*/{});

      auto tupleTemp = SGF.useBufferAsTemporary(tupleBuf, concreteTL);
      translateAndImplodeInto(inputOrigType, inputTupleType,
                              opaque, inputTupleType,
                              *tupleTemp);

      auto payload = tupleTemp->getManagedAddress();
      if (SGF.silConv.useLoweredAddresses()) {
        // We always need to return the existential buf with a cleanup even if
        // we stored trivial values, since SILGen maintains the invariant that
        // forwarding a non-trivial value (i.e. an Any) into memory must be done
        // at +1.
        payload.forward(SGF);
        return SGF.emitManagedBufferWithCleanup(existentialBuf);
      }

      // We are under opaque value(s) mode - load the any and init an opaque
      auto loadedPayload = SGF.B.createLoadCopy(Loc, payload);
      auto &anyTL = SGF.getTypeLowering(opaque, outputSubstType);
      return SGF.B.createInitExistentialValue(
          Loc, anyTL.getLoweredType(), inputTupleType, loadedPayload,
          /*Conformances=*/{});
    }

    /// Handle a tuple that has been exploded in both the input and
    /// the output.
    void translateParallelExploded(AbstractionPattern inputOrigType,
                                   CanTupleType inputSubstType,
                                   AbstractionPattern outputOrigType,
                                   CanTupleType outputSubstType) {
      assert(inputOrigType.matchesTuple(inputSubstType));
      assert(outputOrigType.matchesTuple(outputSubstType));
      assert(!inputSubstType->hasElementWithOwnership() &&
             !outputSubstType->hasElementWithOwnership());
      assert(inputSubstType->getNumElements() ==
             outputSubstType->getNumElements());

      for (auto index : indices(outputSubstType.getElementTypes())) {
        translate(inputOrigType.getTupleElementType(index),
                  inputSubstType.getElementType(index),
                  outputOrigType.getTupleElementType(index),
                  outputSubstType.getElementType(index));
      }
    }

    /// Given that a tuple value is being passed indirectly in the
    /// input, explode it and translate the elements.
    void translateAndExplodeOutOf(AbstractionPattern inputOrigType,
                                  CanTupleType inputSubstType,
                                  AbstractionPattern outputOrigType,
                                  CanTupleType outputSubstType,
                                  ManagedValue inputTupleAddr) {
      assert(inputOrigType.isTypeParameter());
      assert(outputOrigType.matchesTuple(outputSubstType));
      assert(!inputSubstType->hasElementWithOwnership() &&
             !outputSubstType->hasElementWithOwnership());
      assert(inputSubstType->getNumElements() ==
             outputSubstType->getNumElements());

      SmallVector<ManagedValue, 4> inputEltAddrs;
      explodeTuple(SGF, Loc, inputTupleAddr, inputEltAddrs);
      assert(inputEltAddrs.size() == outputSubstType->getNumElements());

      for (auto index : indices(outputSubstType.getElementTypes())) {
        auto inputEltOrigType = inputOrigType.getTupleElementType(index);
        auto inputEltSubstType = inputSubstType.getElementType(index);
        auto outputEltOrigType = outputOrigType.getTupleElementType(index);
        auto outputEltSubstType = outputSubstType.getElementType(index);
        auto inputEltAddr = inputEltAddrs[index];
        assert(inputEltAddr.getType().isAddress() ||
               !SGF.silConv.useLoweredAddresses());

        if (auto outputEltTupleType = dyn_cast<TupleType>(outputEltSubstType)) {
          assert(outputEltOrigType.isTuple());
          auto inputEltTupleType = cast<TupleType>(inputEltSubstType);
          translateAndExplodeOutOf(inputEltOrigType,
                                   inputEltTupleType,
                                   outputEltOrigType,
                                   outputEltTupleType,
                                   inputEltAddr);
        } else {
          auto outputType = claimNextOutputType();
          translateSingle(inputEltOrigType,
                          inputEltSubstType,
                          outputEltOrigType,
                          outputEltSubstType,
                          inputEltAddr,
                          outputType);
        }
      }
    }

    /// Given that a tuple value is being passed indirectly in the
    /// output, translate the elements and implode it.
    void translateAndImplodeInto(AbstractionPattern inputOrigType,
                                 CanTupleType inputSubstType,
                                 AbstractionPattern outputOrigType,
                                 CanTupleType outputSubstType,
                                 TemporaryInitialization &tupleInit) {
      assert(inputOrigType.matchesTuple(inputSubstType));
      assert(outputOrigType.matchesTuple(outputSubstType));
      assert(!inputSubstType->hasElementWithOwnership() &&
             !outputSubstType->hasElementWithOwnership());
      assert(inputSubstType->getNumElements() ==
             outputSubstType->getNumElements());

      SmallVector<CleanupHandle, 4> cleanups;

      for (auto index : indices(outputSubstType.getElementTypes())) {
        auto inputEltOrigType = inputOrigType.getTupleElementType(index);
        auto inputEltSubstType = inputSubstType.getElementType(index);
        auto outputEltOrigType = outputOrigType.getTupleElementType(index);
        auto outputEltSubstType = outputSubstType.getElementType(index);
        auto eltAddr =
          SGF.B.createTupleElementAddr(Loc, tupleInit.getAddress(), index);

        auto &outputEltTL = SGF.getTypeLowering(eltAddr->getType());
        CleanupHandle eltCleanup =
          SGF.enterDormantTemporaryCleanup(eltAddr, outputEltTL);
        if (eltCleanup.isValid()) cleanups.push_back(eltCleanup);

        TemporaryInitialization eltInit(eltAddr, eltCleanup);
        if (auto outputEltTupleType = dyn_cast<TupleType>(outputEltSubstType)) {
          auto inputEltTupleType = cast<TupleType>(inputEltSubstType);
          translateAndImplodeInto(inputEltOrigType, inputEltTupleType,
                                  outputEltOrigType, outputEltTupleType,
                                  eltInit);
        } else {
          // Otherwise, we come from a single value.
          auto input = claimNextInput();
          translateSingleInto(inputEltOrigType, inputEltSubstType,
                              outputEltOrigType, outputEltSubstType,
                              input, eltInit);
        }
      }

      // Deactivate all the element cleanups and activate the tuple cleanup.
      for (auto cleanup : cleanups)
        SGF.Cleanups.forwardCleanup(cleanup);
      tupleInit.finishInitialization(SGF);
    }

    // Translate into a temporary.
    void translateIndirect(AbstractionPattern inputOrigType,
                           CanType inputSubstType,
                           AbstractionPattern outputOrigType,
                           CanType outputSubstType, ManagedValue input,
                           SILType resultTy) {
      auto &outputTL = SGF.getTypeLowering(resultTy);
      auto temp = SGF.emitTemporary(Loc, outputTL);
      translateSingleInto(inputOrigType, inputSubstType, outputOrigType,
                          outputSubstType, input, *temp);
      Outputs.push_back(temp->getManagedAddress());
    }

    // Translate into an owned argument.
    void translateIntoOwned(AbstractionPattern inputOrigType,
                            CanType inputSubstType,
                            AbstractionPattern outputOrigType,
                            CanType outputSubstType, ManagedValue input) {
      auto output = translatePrimitive(inputOrigType, inputSubstType,
                                       outputOrigType, outputSubstType, input);

      // If our output is guaranteed or unowned, we need to create a copy here.
      if (output.getOwnershipKind() != ValueOwnershipKind::Owned)
        output = output.copyUnmanaged(SGF, Loc);

      Outputs.push_back(output);
    }

    // Translate into a guaranteed argument.
    void translateIntoGuaranteed(AbstractionPattern inputOrigType,
                                 CanType inputSubstType,
                                 AbstractionPattern outputOrigType,
                                 CanType outputSubstType, ManagedValue input) {
      auto output = translatePrimitive(inputOrigType, inputSubstType,
                                       outputOrigType, outputSubstType, input);

      // If our output value is not guaranteed, we need to:
      //
      // 1. Unowned - Copy + Borrow.
      // 2. Owned - Borrow.
      // 3. Trivial - do nothing.
      //
      // This means we can first transition unowned => owned and then handle
      // the new owned value using the same code path as values that are
      // initially owned.
      if (output.getOwnershipKind() == ValueOwnershipKind::Unowned) {
        assert(!output.hasCleanup());
        output = SGF.emitManagedRetain(Loc, output.getValue());
      }

      // If the output is unowned or owned, create a borrow.
      if (output.getOwnershipKind() != ValueOwnershipKind::Guaranteed) {
        output = SGF.emitManagedBeginBorrow(Loc, output.getValue());
      }

      Outputs.push_back(output);
    }

    /// Translate a single value and add it as an output.
    void translateSingle(AbstractionPattern inputOrigType,
                         CanType inputSubstType,
                         AbstractionPattern outputOrigType,
                         CanType outputSubstType,
                         ManagedValue input,
                         SILParameterInfo result) {
      // Easy case: we want to pass exactly this value.
      if (input.getType() == SGF.getSILType(result)) {
        switch (result.getConvention()) {
        case ParameterConvention::Direct_Owned:
        case ParameterConvention::Indirect_In:
          if (!input.hasCleanup() &&
              input.getOwnershipKind() != ValueOwnershipKind::Trivial)
            input = input.copyUnmanaged(SGF, Loc);
          break;

        default:
          break;
        }

        Outputs.push_back(input);
        return;
      }

      switch (result.getConvention()) {
      // Direct translation is relatively easy.
      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Unowned:
        translateIntoOwned(inputOrigType, inputSubstType, outputOrigType,
                           outputSubstType, input);
        assert(Outputs.back().getType() == SGF.getSILType(result));
        return;
      case ParameterConvention::Direct_Guaranteed:
        translateIntoGuaranteed(inputOrigType, inputSubstType, outputOrigType,
                                outputSubstType, input);
        return;
      case ParameterConvention::Indirect_In: {
        if (SGF.silConv.useLoweredAddresses()) {
          translateIndirect(inputOrigType, inputSubstType, outputOrigType,
                            outputSubstType, input, SGF.getSILType(result));
          return;
        }
        translateIntoOwned(inputOrigType, inputSubstType, outputOrigType,
                           outputSubstType, input);
        assert(Outputs.back().getType() == SGF.getSILType(result));
        return;
      }
      case ParameterConvention::Indirect_In_Guaranteed: {
        if (SGF.silConv.useLoweredAddresses()) {
          translateIndirect(inputOrigType, inputSubstType, outputOrigType,
                            outputSubstType, input, SGF.getSILType(result));
          return;
        }
        translateIntoGuaranteed(inputOrigType, inputSubstType, outputOrigType,
                                outputSubstType, input);
        assert(Outputs.back().getType() == SGF.getSILType(result));
        return;
      }
      case ParameterConvention::Indirect_Inout:
        llvm_unreachable("inout reabstraction handled elsewhere");
      case ParameterConvention::Indirect_InoutAliasable:
        llvm_unreachable("abstraction difference in aliasable argument not "
                         "allowed");
      case ParameterConvention::Indirect_In_Constant:
        llvm_unreachable("in_constant convention not allowed in SILGen");
      }

      llvm_unreachable("Covered switch isn't covered?!");
    }

    void translateInOut(AbstractionPattern inputOrigType,
                        CanType inputSubstType,
                        AbstractionPattern outputOrigType,
                        CanType outputSubstType,
                        ManagedValue input,
                        SILParameterInfo result) {
      assert(input.isLValue());
      if (input.getType() == SGF.getSILType(result)) {
        Outputs.push_back(input);
        return;
      }

      // Create a temporary of the right type.
      auto &temporaryTL = SGF.getTypeLowering(result.getType());
      auto temporary = SGF.emitTemporary(Loc, temporaryTL);

      // Take ownership of the input value.  This leaves the input l-value
      // effectively uninitialized, but we'll push a cleanup that will put
      // a value back into it.
      FullExpr scope(SGF.Cleanups, CleanupLocation::get(Loc));
      auto ownedInput =
        SGF.emitManagedBufferWithCleanup(input.getLValueAddress());

      // Translate the input value into the temporary.
      translateSingleInto(inputOrigType, inputSubstType,
                          outputOrigType, outputSubstType,
                          ownedInput, *temporary);

      // Forward the cleanup on the temporary.  We're about to push a new
      // cleanup that will re-assert ownership of this value.
      auto temporaryAddr = temporary->getManagedAddress().forward(SGF);

      // Leave the scope in which we did the forward translation.  This
      // ensures that the value in the input buffer is destroyed
      // immediately rather than (potentially) arbitrarily later
      // at a point where we want to put new values in the input buffer.
      scope.pop();

      // Push the cleanup to perform the reverse translation.  This cleanup
      // asserts ownership of the value of the temporary.
      SGF.Cleanups.pushCleanup<TranslateIndirect>(outputOrigType,
                                                  outputSubstType,
                                                  inputOrigType,
                                                  inputSubstType,
                                                  temporaryAddr,
                                                  input.getLValueAddress());

      // Add the temporary as an l-value argument.
      Outputs.push_back(ManagedValue::forLValue(temporaryAddr));
    }

    /// Translate a single value and initialize the given temporary with it.
    void translateSingleInto(AbstractionPattern inputOrigType,
                             CanType inputSubstType,
                             AbstractionPattern outputOrigType,
                             CanType outputSubstType,
                             ManagedValue input,
                             TemporaryInitialization &temp) {
      auto output = translatePrimitive(inputOrigType, inputSubstType,
                                       outputOrigType, outputSubstType,
                                       input, SGFContext(&temp));
      forceInto(output, temp);
    }

    /// Apply primitive translation to the given value.
    ManagedValue translatePrimitive(AbstractionPattern inputOrigType,
                                    CanType inputSubstType,
                                    AbstractionPattern outputOrigType,
                                    CanType outputSubstType,
                                    ManagedValue input,
                                    SGFContext context = SGFContext()) {
      return SGF.emitTransformedValue(Loc, input,
                                      inputOrigType, inputSubstType,
                                      outputOrigType, outputSubstType,
                                      context);
    }

    /// Force the given result into the given initialization.
    void forceInto(ManagedValue result, TemporaryInitialization &temp) {
      emitForceInto(SGF, Loc, result, temp);
    }

    ManagedValue claimNextInput() {
      return claimNext(Inputs);
    }

    SILParameterInfo claimNextOutputType() {
      return claimNext(OutputTypes);
    }
  };
} // end anonymous namespace

/// Forward arguments according to a function type's ownership conventions.
static void forwardFunctionArguments(SILGenFunction &SGF,
                                     SILLocation loc,
                                     CanSILFunctionType fTy,
                                     ArrayRef<ManagedValue> managedArgs,
                                     SmallVectorImpl<SILValue> &forwardedArgs) {
  auto argTypes = fTy->getParameters();
  for (auto index : indices(managedArgs)) {
    auto &arg = managedArgs[index];
    auto argTy = argTypes[index];
    if (argTy.isConsumed()) {
      forwardedArgs.push_back(arg.forward(SGF));
      continue;
    }

    if (isGuaranteedParameter(argTy.getConvention())) {
      forwardedArgs.push_back(
          SGF.emitManagedBeginBorrow(loc, arg.getValue()).getValue());
      continue;
    }

    forwardedArgs.push_back(arg.getValue());
  }
}

namespace {
  class YieldInfo {
    SmallVector<AbstractionPattern, 1> OrigTypes;
    SmallVector<AnyFunctionType::Param, 1> Yields;
    ArrayRef<SILYieldInfo> LoweredInfos;
  public:
    YieldInfo(SILGenModule &SGM, SILDeclRef function,
              CanSILFunctionType loweredType, SubstitutionMap subs) {
      LoweredInfos = loweredType->getYields();

      auto accessor = cast<AccessorDecl>(function.getDecl());
      auto storage = accessor->getStorage();

      OrigTypes.push_back(
        SGM.Types.getAbstractionPattern(storage, /*nonobjc*/ true));

      SmallVector<AnyFunctionType::Yield, 1> yieldsBuffer;
      auto yields = AnyFunctionRef(accessor).getYieldResults(yieldsBuffer);
      assert(yields.size() == 1);
      Yields.push_back(yields[0].getCanonical().subst(subs).asParam());
    }

    ArrayRef<AbstractionPattern> getOrigTypes() const { return OrigTypes; }
    AnyFunctionType::CanParamArrayRef getSubstTypes() const {
      return AnyFunctionType::CanParamArrayRef(Yields);
    }
    ArrayRef<SILYieldInfo> getLoweredTypes() const { return LoweredInfos; }
  };
}

static ManagedValue manageYield(SILGenFunction &SGF, SILValue value,
                                SILYieldInfo info) {
  switch (info.getConvention()) {
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    return ManagedValue::forLValue(value);
  case ParameterConvention::Direct_Owned:
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Constant:
    return SGF.emitManagedRValueWithCleanup(value);
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Direct_Unowned:
    if (value.getOwnershipKind() == ValueOwnershipKind::Trivial)
      return ManagedValue::forTrivialObjectRValue(value);
    return ManagedValue::forBorrowedObjectRValue(value);
  case ParameterConvention::Indirect_In_Guaranteed:
    return ManagedValue::forBorrowedAddressRValue(value);
  }
  llvm_unreachable("bad kind");
}

static void manageYields(SILGenFunction &SGF, ArrayRef<SILValue> yields,
                         ArrayRef<SILYieldInfo> yieldInfos,
                         SmallVectorImpl<ManagedValue> &yieldMVs) {
  assert(yields.size() == yieldInfos.size());
  for (auto i : indices(yields)) {
    yieldMVs.push_back(manageYield(SGF, yields[i], yieldInfos[i]));
  }
}

/// Translate the values yielded to us back out to the caller.
static void translateYields(SILGenFunction &SGF, SILLocation loc,
                            ArrayRef<SILValue> innerYields,
                            const YieldInfo &innerInfos,
                            const YieldInfo &outerInfos) {
  assert(innerInfos.getOrigTypes().size() == innerInfos.getSubstTypes().size());
  assert(outerInfos.getOrigTypes().size() == outerInfos.getSubstTypes().size());
  assert(innerInfos.getOrigTypes().size() == outerInfos.getOrigTypes().size());

  SmallVector<ManagedValue, 4> innerMVs;
  manageYields(SGF, innerYields, innerInfos.getLoweredTypes(), innerMVs);

  FullExpr scope(SGF.Cleanups, CleanupLocation::get(loc));

  // Map the SILYieldInfos into the local context and incidentally turn
  // them into SILParameterInfos.
  SmallVector<SILParameterInfo, 4> outerLoweredTypesAsParameters;
  for (auto unmappedInfo : outerInfos.getLoweredTypes()) {
    auto mappedTy = SGF.F.mapTypeIntoContext(unmappedInfo.getSILStorageType());
    outerLoweredTypesAsParameters.push_back({mappedTy.getASTType(),
                                             unmappedInfo.getConvention()});
  }

  // Translate the yields as if they were arguments.
  SmallVector<ManagedValue, 4> outerMVs;
  TranslateArguments translator(SGF, loc, innerMVs, outerMVs,
                                outerLoweredTypesAsParameters);

  translator.translate(innerInfos.getOrigTypes(), innerInfos.getSubstTypes(),
                       outerInfos.getOrigTypes(), outerInfos.getSubstTypes());

  // Prepare a destination for the unwind; use the current cleanup stack
  // as the depth so that we branch right to it.
  SILBasicBlock *unwindBB = SGF.createBasicBlock(FunctionSection::Postmatter);
  JumpDest unwindDest(unwindBB, SGF.Cleanups.getCleanupsDepth(),
                      CleanupLocation::get(loc));

  // Emit the yield.
  SGF.emitRawYield(loc, outerMVs, unwindDest, /*unique*/ true);

  // Emit the unwind block.
  {
    SILGenSavedInsertionPoint savedIP(SGF, unwindBB,
                                      FunctionSection::Postmatter);

    // Emit all active cleanups.
    SGF.Cleanups.emitCleanupsForReturn(CleanupLocation::get(loc), IsForUnwind);
    SGF.B.createUnwind(loc);
  }
}

namespace {

/// A helper class to translate the inner results to the outer results.
///
/// Creating a result-translation plan involves three basic things:
///   - building SILArguments for each of the outer indirect results
///   - building a list of SILValues for each of the inner indirect results
///   - building a list of Operations to perform which will reabstract
///     the inner results to match the outer.
class ResultPlanner {
  SILGenFunction &SGF;
  SILLocation Loc;

  /// A single result-translation operation.
  struct Operation {
    enum Kind {
      /// Take the last N direct outer results, tuple them, and make that a
      /// new direct outer result.
      ///
      /// Valid: NumElements, OuterResult
      TupleDirect,

      /// Take the last direct outer result, inject it into an optional
      /// type, and make that a new direct outer result.
      ///
      /// Valid: SomeDecl, OuterResult
      InjectOptionalDirect,

      /// Finish building an optional Some in the given address.
      ///
      /// Valid: SomeDecl, OuterResultAddr
      InjectOptionalIndirect,

      /// Take the next direct inner result and just make it a direct
      /// outer result.
      ///
      /// Valid: InnerResult, OuterResult.
      DirectToDirect,

      /// Take the next direct inner result and store it into an
      /// outer result address.
      ///
      /// Valid: InnerDirect, OuterResultAddr.
      DirectToIndirect,

      /// Take from an indirect inner result and make it the next outer
      /// direct result.
      ///
      /// Valid: InnerResultAddr, OuterResult.
      IndirectToDirect,

      /// Take from an indirect inner result into an outer indirect result.
      ///
      /// Valid: InnerResultAddr, OuterResultAddr.
      IndirectToIndirect,

      /// Take a value out of the source inner result address, reabstract
      /// it, and initialize the destination outer result address.
      ///
      /// Valid: reabstraction info, InnerAddress, OuterAddress.
      ReabstractIndirectToIndirect,

      /// Take a value out of the source inner result address, reabstract
      /// it, and add it as the next direct outer result.
      ///
      /// Valid: reabstraction info, InnerAddress, OuterResult.
      ReabstractIndirectToDirect,

      /// Take the next direct inner result, reabstract it, and initialize
      /// the destination outer result address.
      ///
      /// Valid: reabstraction info, InnerResult, OuterAddress.
      ReabstractDirectToIndirect,

      /// Take the next direct inner result, reabstract it, and add it as
      /// the next direct outer result.
      ///
      /// Valid: reabstraction info, InnerResult, OuterResult.
      ReabstractDirectToDirect,
    };

    Operation(Kind kind) : TheKind(kind) {}

    Kind TheKind;

    // Reabstraction information.  Only valid for reabstraction kinds.
    AbstractionPattern InnerOrigType = AbstractionPattern::getInvalid();
    AbstractionPattern OuterOrigType = AbstractionPattern::getInvalid();
    CanType InnerSubstType, OuterSubstType;

    union {
      SILValue InnerResultAddr;
      SILResultInfo InnerResult;
      unsigned NumElements;
      EnumElementDecl *SomeDecl;
    };

    union {
      SILValue OuterResultAddr;
      SILResultInfo OuterResult;
    };
  };

  struct PlanData {
    ArrayRef<SILResultInfo> OuterResults;
    ArrayRef<SILResultInfo> InnerResults;
    SmallVectorImpl<SILValue> &InnerIndirectResultAddrs;
    size_t NextOuterIndirectResultIndex;
  };

  SmallVector<Operation, 8> Operations;
public:
  ResultPlanner(SILGenFunction &SGF, SILLocation loc) : SGF(SGF), Loc(loc) {}

  void plan(AbstractionPattern innerOrigType, CanType innerSubstType,
            AbstractionPattern outerOrigType, CanType outerSubstType,
            CanSILFunctionType innerFnType, CanSILFunctionType outerFnType,
            SmallVectorImpl<SILValue> &innerIndirectResultAddrs) {
    // Assert that the indirect results are set up like we expect.
    assert(innerIndirectResultAddrs.empty());
    assert(SGF.F.begin()->args_size()
           >= SILFunctionConventions(outerFnType, SGF.SGM.M)
                  .getNumIndirectSILResults());

    innerIndirectResultAddrs.reserve(
        SILFunctionConventions(innerFnType, SGF.SGM.M)
            .getNumIndirectSILResults());

    PlanData data = {outerFnType->getResults(), innerFnType->getResults(),
                     innerIndirectResultAddrs, 0};

    // Recursively walk the result types.
    plan(innerOrigType, innerSubstType, outerOrigType, outerSubstType, data);

    // Assert that we consumed and produced all the indirect result
    // information we needed.
    assert(data.OuterResults.empty());
    assert(data.InnerResults.empty());
    assert(data.InnerIndirectResultAddrs.size() ==
           SILFunctionConventions(innerFnType, SGF.SGM.M)
               .getNumIndirectSILResults());
    assert(data.NextOuterIndirectResultIndex
           == SILFunctionConventions(outerFnType, SGF.SGM.M)
                  .getNumIndirectSILResults());
  }

  SILValue execute(SILValue innerResult);

private:
  void execute(ArrayRef<SILValue> innerDirectResults,
               SmallVectorImpl<SILValue> &outerDirectResults);
  void executeInnerTuple(SILValue innerElement,
                         SmallVector<SILValue, 4> &innerDirectResults);

  void plan(AbstractionPattern innerOrigType, CanType innerSubstType,
            AbstractionPattern outerOrigType, CanType outerSubstType,
            PlanData &planData);

  void planIntoIndirectResult(AbstractionPattern innerOrigType,
                              CanType innerSubstType,
                              AbstractionPattern outerOrigType,
                              CanType outerSubstType,
                              PlanData &planData,
                              SILValue outerResultAddr);
  void planTupleIntoIndirectResult(AbstractionPattern innerOrigType,
                                   CanTupleType innerSubstType,
                                   AbstractionPattern outerOrigType,
                                   CanType outerSubstType,
                                   PlanData &planData,
                                   SILValue outerResultAddr);
  void planScalarIntoIndirectResult(AbstractionPattern innerOrigType,
                                    CanType innerSubstType,
                                    AbstractionPattern outerOrigType,
                                    CanType outerSubstType,
                                    PlanData &planData,
                                    SILResultInfo innerResult,
                                    SILValue outerResultAddr);

  void planIntoDirectResult(AbstractionPattern innerOrigType,
                            CanType innerSubstType,
                            AbstractionPattern outerOrigType,
                            CanType outerSubstType,
                            PlanData &planData,
                            SILResultInfo outerResult);
  void planScalarIntoDirectResult(AbstractionPattern innerOrigType,
                                  CanType innerSubstType,
                                  AbstractionPattern outerOrigType,
                                  CanType outerSubstType,
                                  PlanData &planData,
                                  SILResultInfo innerResult,
                                  SILResultInfo outerResult);
  void planTupleIntoDirectResult(AbstractionPattern innerOrigType,
                                 CanTupleType innerSubstType,
                                 AbstractionPattern outerOrigType,
                                 CanType outerSubstType,
                                 PlanData &planData,
                                 SILResultInfo outerResult);

  void planFromIndirectResult(AbstractionPattern innerOrigType,
                              CanType innerSubstType,
                              AbstractionPattern outerOrigType,
                              CanType outerSubstType,
                              PlanData &planData,
                              SILValue innerResultAddr);
  void planTupleFromIndirectResult(AbstractionPattern innerOrigType,
                                   CanTupleType innerSubstType,
                                   AbstractionPattern outerOrigType,
                                   CanTupleType outerSubstType,
                                   PlanData &planData,
                                   SILValue innerResultAddr);
  void planTupleFromDirectResult(AbstractionPattern innerOrigType,
                                 CanTupleType innerSubstType,
                                 AbstractionPattern outerOrigType,
                                 CanTupleType outerSubstType,
                                 PlanData &planData, SILResultInfo innerResult);
  void planScalarFromIndirectResult(AbstractionPattern innerOrigType,
                                    CanType innerSubstType,
                                    AbstractionPattern outerOrigType,
                                    CanType outerSubstType,
                                    SILValue innerResultAddr,
                                    SILResultInfo outerResult,
                                    SILValue optOuterResultAddr);

  /// Claim the next inner result from the plan data.
  SILResultInfo claimNextInnerResult(PlanData &data) {
    return claimNext(data.InnerResults);
  }

  /// Claim the next outer result from the plan data.  If it's indirect,
  /// grab its SILArgument.
  std::pair<SILResultInfo, SILValue> claimNextOuterResult(PlanData &data) {
    SILResultInfo result = claimNext(data.OuterResults);

    SILValue resultAddr;
    if (SGF.silConv.isSILIndirect(result)) {
      resultAddr =
          SGF.F.begin()->getArgument(data.NextOuterIndirectResultIndex++);
    }

    return { result, resultAddr };
  }

  /// Create a temporary address suitable for passing to the given inner
  /// indirect result and add it as an inner indirect result.
  SILValue addInnerIndirectResultTemporary(PlanData &data,
                                           SILResultInfo innerResult) {
    assert(SGF.silConv.isSILIndirect(innerResult) ||
           !SGF.silConv.useLoweredAddresses());
    auto temporary =
        SGF.emitTemporaryAllocation(Loc, SGF.getSILType(innerResult));
    data.InnerIndirectResultAddrs.push_back(temporary);
    return temporary;
  }

  /// Cause the next inner indirect result to be emitted directly into
  /// the given outer result address.
  void addInPlace(PlanData &data, SILValue outerResultAddr) {
    data.InnerIndirectResultAddrs.push_back(outerResultAddr);
    // Does not require an Operation.
  }

  Operation &addOperation(Operation::Kind kind) {
    Operations.emplace_back(kind);
    return Operations.back();
  }

  void addDirectToDirect(SILResultInfo innerResult, SILResultInfo outerResult) {
    auto &op = addOperation(Operation::DirectToDirect);
    op.InnerResult = innerResult;
    op.OuterResult = outerResult;
  }

  void addDirectToIndirect(SILResultInfo innerResult,
                           SILValue outerResultAddr) {
    auto &op = addOperation(Operation::DirectToIndirect);
    op.InnerResult = innerResult;
    op.OuterResultAddr = outerResultAddr;
  }

  void addIndirectToDirect(SILValue innerResultAddr,
                           SILResultInfo outerResult) {
    auto &op = addOperation(Operation::IndirectToDirect);
    op.InnerResultAddr = innerResultAddr;
    op.OuterResult = outerResult;
  }

  void addIndirectToIndirect(SILValue innerResultAddr,
                             SILValue outerResultAddr) {
    auto &op = addOperation(Operation::IndirectToIndirect);
    op.InnerResultAddr = innerResultAddr;
    op.OuterResultAddr = outerResultAddr;
  }

  void addTupleDirect(unsigned numElements, SILResultInfo outerResult) {
    auto &op = addOperation(Operation::TupleDirect);
    op.NumElements = numElements;
    op.OuterResult = outerResult;
  }

  void addInjectOptionalDirect(EnumElementDecl *someDecl,
                               SILResultInfo outerResult) {
    auto &op = addOperation(Operation::InjectOptionalDirect);
    op.SomeDecl = someDecl;
    op.OuterResult = outerResult;
  }

  void addInjectOptionalIndirect(EnumElementDecl *someDecl,
                                 SILValue outerResultAddr) {
    auto &op = addOperation(Operation::InjectOptionalIndirect);
    op.SomeDecl = someDecl;
    op.OuterResultAddr = outerResultAddr;
  }

  void addReabstractDirectToDirect(AbstractionPattern innerOrigType,
                                   CanType innerSubstType,
                                   AbstractionPattern outerOrigType,
                                   CanType outerSubstType,
                                   SILResultInfo innerResult,
                                   SILResultInfo outerResult) {
    auto &op = addOperation(Operation::ReabstractDirectToDirect);
    op.InnerResult = innerResult;
    op.OuterResult = outerResult;
    op.InnerOrigType = innerOrigType;
    op.InnerSubstType = innerSubstType;
    op.OuterOrigType = outerOrigType;
    op.OuterSubstType = outerSubstType;
  }

  void addReabstractDirectToIndirect(AbstractionPattern innerOrigType,
                                     CanType innerSubstType,
                                     AbstractionPattern outerOrigType,
                                     CanType outerSubstType,
                                     SILResultInfo innerResult,
                                     SILValue outerResultAddr) {
    auto &op = addOperation(Operation::ReabstractDirectToIndirect);
    op.InnerResult = innerResult;
    op.OuterResultAddr = outerResultAddr;
    op.InnerOrigType = innerOrigType;
    op.InnerSubstType = innerSubstType;
    op.OuterOrigType = outerOrigType;
    op.OuterSubstType = outerSubstType;
  }

  void addReabstractIndirectToDirect(AbstractionPattern innerOrigType,
                                     CanType innerSubstType,
                                     AbstractionPattern outerOrigType,
                                     CanType outerSubstType,
                                     SILValue innerResultAddr,
                                     SILResultInfo outerResult) {
    auto &op = addOperation(Operation::ReabstractIndirectToDirect);
    op.InnerResultAddr = innerResultAddr;
    op.OuterResult = outerResult;
    op.InnerOrigType = innerOrigType;
    op.InnerSubstType = innerSubstType;
    op.OuterOrigType = outerOrigType;
    op.OuterSubstType = outerSubstType;
  }

  void addReabstractIndirectToIndirect(AbstractionPattern innerOrigType,
                                       CanType innerSubstType,
                                       AbstractionPattern outerOrigType,
                                       CanType outerSubstType,
                                       SILValue innerResultAddr,
                                       SILValue outerResultAddr) {
    auto &op = addOperation(Operation::ReabstractIndirectToIndirect);
    op.InnerResultAddr = innerResultAddr;
    op.OuterResultAddr = outerResultAddr;
    op.InnerOrigType = innerOrigType;
    op.InnerSubstType = innerSubstType;
    op.OuterOrigType = outerOrigType;
    op.OuterSubstType = outerSubstType;
  }
};

} // end anonymous namespace

/// Plan the reabstraction of a call result.
void ResultPlanner::plan(AbstractionPattern innerOrigType,
                         CanType innerSubstType,
                         AbstractionPattern outerOrigType,
                         CanType outerSubstType,
                         PlanData &planData) {
  // The substituted types must match up in tuple-ness and arity.
  assert(
      isa<TupleType>(innerSubstType) == isa<TupleType>(outerSubstType) ||
      (isa<TupleType>(innerSubstType) &&
       (outerSubstType->isAny() || outerSubstType->getOptionalObjectType())));
  assert(!isa<TupleType>(outerSubstType) ||
         cast<TupleType>(innerSubstType)->getNumElements() ==
           cast<TupleType>(outerSubstType)->getNumElements());

  // If the inner abstraction pattern is a tuple, that result will be expanded.
  if (innerOrigType.isTuple()) {
    auto innerSubstTupleType = cast<TupleType>(innerSubstType);

    // If the outer abstraction pattern is also a tuple, that result will also
    // be expanded, in parallel with the inner pattern.
    if (outerOrigType.isTuple()) {
      auto outerSubstTupleType = cast<TupleType>(outerSubstType);
      assert(innerSubstTupleType->getNumElements()
               == outerSubstTupleType->getNumElements());

      // Otherwise, recursively descend into the tuples.
      for (auto eltIndex : indices(innerSubstTupleType.getElementTypes())) {
        plan(innerOrigType.getTupleElementType(eltIndex),
             innerSubstTupleType.getElementType(eltIndex),
             outerOrigType.getTupleElementType(eltIndex),
             outerSubstTupleType.getElementType(eltIndex),
             planData);
      }
      return;      
    }

    // Otherwise, the next outer result must be either opaque or optional.
    // In either case, it corresponds to a single result.
    auto outerResult = claimNextOuterResult(planData);

    // Base the plan on whether the single result is direct or indirect.
    if (SGF.silConv.isSILIndirect(outerResult.first)) {
      assert(outerResult.second);
      planTupleIntoIndirectResult(innerOrigType, innerSubstTupleType,
                                  outerOrigType, outerSubstType,
                                  planData, outerResult.second);
    } else {
      planTupleIntoDirectResult(innerOrigType, innerSubstTupleType,
                                outerOrigType, outerSubstType,
                                planData, outerResult.first);
    }
    return;
  }

  // Otherwise, the inner pattern is a scalar; claim the next inner result.
  SILResultInfo innerResult = claimNextInnerResult(planData);

  assert((!outerOrigType.isTuple() || innerResult.isFormalIndirect()) &&
         "outer pattern is a tuple, inner pattern is not, but inner result is "
         "not indirect?");

  // If the inner result is a tuple, we need to expand from a temporary.
  if (innerResult.isFormalIndirect() && outerOrigType.isTuple()) {
    if (SGF.silConv.isSILIndirect(innerResult)) {
      SILValue innerResultAddr =
          addInnerIndirectResultTemporary(planData, innerResult);
      planTupleFromIndirectResult(
          innerOrigType, cast<TupleType>(innerSubstType), outerOrigType,
          cast<TupleType>(outerSubstType), planData, innerResultAddr);
    } else {
      assert(!SGF.silConv.useLoweredAddresses() &&
             "Formal Indirect Results that are not SIL Indirect are only "
             "allowed in opaque values mode");
      planTupleFromDirectResult(innerOrigType, cast<TupleType>(innerSubstType),
                                outerOrigType, cast<TupleType>(outerSubstType),
                                planData, innerResult);
    }
    return;
  }

  // Otherwise, the outer pattern is a scalar; claim the next outer result.
  auto outerResult = claimNextOuterResult(planData);

  // If the outer result is indirect, plan to emit into that.
  if (SGF.silConv.isSILIndirect(outerResult.first)) {
    assert(outerResult.second);
    planScalarIntoIndirectResult(innerOrigType, innerSubstType,
                                 outerOrigType, outerSubstType,
                                 planData, innerResult, outerResult.second);

  } else {
    planScalarIntoDirectResult(innerOrigType, innerSubstType,
                               outerOrigType, outerSubstType,
                               planData, innerResult, outerResult.first);
  }
}

/// Plan the emission of a call result into an outer result address.
void ResultPlanner::planIntoIndirectResult(AbstractionPattern innerOrigType,
                                           CanType innerSubstType,
                                           AbstractionPattern outerOrigType,
                                           CanType outerSubstType,
                                           PlanData &planData,
                                           SILValue outerResultAddr) {
  // outerOrigType can be a tuple if we're also injecting into an optional.

  // If the inner pattern is a tuple, expand it.
  if (innerOrigType.isTuple()) {
    planTupleIntoIndirectResult(innerOrigType, cast<TupleType>(innerSubstType),
                                outerOrigType, outerSubstType,
                                planData, outerResultAddr);

  // Otherwise, it's scalar.
  } else {
    // Claim the next inner result.
    SILResultInfo innerResult = claimNextInnerResult(planData);

    planScalarIntoIndirectResult(innerOrigType, innerSubstType,
                                 outerOrigType, outerSubstType,
                                 planData, innerResult, outerResultAddr);
  }
}

/// Plan the emission of a call result into an outer result address,
/// given that the inner abstraction pattern is a tuple.
void
ResultPlanner::planTupleIntoIndirectResult(AbstractionPattern innerOrigType,
                                           CanTupleType innerSubstType,
                                           AbstractionPattern outerOrigType,
                                           CanType outerSubstType,
                                           PlanData &planData,
                                           SILValue outerResultAddr) {
  assert(innerOrigType.isTuple());
  // outerOrigType can be a tuple if we're doing something like
  // injecting into an optional tuple.

  auto outerSubstTupleType = dyn_cast<TupleType>(outerSubstType);

  // If the outer type is not a tuple, it must be optional.
  if (!outerSubstTupleType) {
    // Figure out what kind of optional it is.
    CanType outerSubstObjectType = outerSubstType.getOptionalObjectType();
    if (outerSubstObjectType) {
      auto someDecl = SGF.getASTContext().getOptionalSomeDecl();

      // Prepare the value slot in the optional value.
      SILType outerObjectType =
          outerResultAddr->getType().getOptionalObjectType();
      SILValue outerObjectResultAddr
        = SGF.B.createInitEnumDataAddr(Loc, outerResultAddr, someDecl,
                                       outerObjectType);

      // Emit into that address.
      planTupleIntoIndirectResult(
          innerOrigType, innerSubstType, outerOrigType.getOptionalObjectType(),
          outerSubstObjectType, planData, outerObjectResultAddr);

      // Add an operation to finish the enum initialization.
      addInjectOptionalIndirect(someDecl, outerResultAddr);
      return;
    }

    assert(outerSubstType->isAny());

    // Prepare the value slot in the existential.
    auto opaque = AbstractionPattern::getOpaque();
    SILValue outerConcreteResultAddr
      = SGF.B.createInitExistentialAddr(Loc, outerResultAddr, innerSubstType,
                                        SGF.getLoweredType(opaque, innerSubstType),
                                        /*conformances=*/{});

    // Emit into that address.
    planTupleIntoIndirectResult(innerOrigType, innerSubstType,
                                innerOrigType, innerSubstType,
                                planData, outerConcreteResultAddr);
    return;
  }

  assert(innerSubstType->getNumElements()
           == outerSubstTupleType->getNumElements());

  for (auto eltIndex : indices(innerSubstType.getElementTypes())) {
    // Project the address of the element.
    SILValue outerEltResultAddr =
      SGF.B.createTupleElementAddr(Loc, outerResultAddr, eltIndex);

    // Plan to emit into that location.
    planIntoIndirectResult(innerOrigType.getTupleElementType(eltIndex),
                           innerSubstType.getElementType(eltIndex),
                           outerOrigType.getTupleElementType(eltIndex),
                           outerSubstTupleType.getElementType(eltIndex),
                           planData, outerEltResultAddr);
  }
}

/// Plan the emission of a call result as a single outer direct result.
void
ResultPlanner::planIntoDirectResult(AbstractionPattern innerOrigType,
                                    CanType innerSubstType,
                                    AbstractionPattern outerOrigType,
                                    CanType outerSubstType,
                                    PlanData &planData,
                                    SILResultInfo outerResult) {
  assert(!outerOrigType.isTuple() || !SGF.silConv.useLoweredAddresses());

  // If the inner pattern is a tuple, expand it.
  if (innerOrigType.isTuple()) {
    planTupleIntoDirectResult(innerOrigType, cast<TupleType>(innerSubstType),
                              outerOrigType, outerSubstType,
                              planData, outerResult);

  // Otherwise, it's scalar.
  } else {
    // Claim the next inner result.
    SILResultInfo innerResult = claimNextInnerResult(planData);

    planScalarIntoDirectResult(innerOrigType, innerSubstType,
                               outerOrigType, outerSubstType,
                               planData, innerResult, outerResult);
  }
}

/// Plan the emission of a call result as a single outer direct result,
/// given that the inner abstraction pattern is a tuple.
void
ResultPlanner::planTupleIntoDirectResult(AbstractionPattern innerOrigType,
                                         CanTupleType innerSubstType,
                                         AbstractionPattern outerOrigType,
                                         CanType outerSubstType,
                                         PlanData &planData,
                                         SILResultInfo outerResult) {
  assert(innerOrigType.isTuple());

  auto outerSubstTupleType = dyn_cast<TupleType>(outerSubstType);

  // If the outer type is not a tuple, it must be optional or we are under
  // opaque value mode
  if (!outerSubstTupleType) {
    CanType outerSubstObjectType = outerSubstType.getOptionalObjectType();

    if (outerSubstObjectType) {
      auto someDecl = SGF.getASTContext().getOptionalSomeDecl();
      SILType outerObjectType =
          SGF.getSILType(outerResult).getOptionalObjectType();
      SILResultInfo outerObjectResult(outerObjectType.getASTType(),
                                      outerResult.getConvention());

      // Plan to leave the tuple elements as a single direct outer result.
      planTupleIntoDirectResult(
          innerOrigType, innerSubstType, outerOrigType.getOptionalObjectType(),
          outerSubstObjectType, planData, outerObjectResult);

      // Take that result and inject it into an optional.
      addInjectOptionalDirect(someDecl, outerResult);
      return;
    } else {
      assert(!SGF.silConv.useLoweredAddresses() &&
             "inner type was a tuple but outer type was neither a tuple nor "
             "optional nor are we under opaque value mode");
      assert(outerSubstType->isAny());

      auto opaque = AbstractionPattern::getOpaque();
      auto anyType = SGF.getLoweredType(opaque, outerSubstType);
      auto outerResultAddr = SGF.emitTemporaryAllocation(Loc, anyType);

      SILValue outerConcreteResultAddr = SGF.B.createInitExistentialAddr(
          Loc, outerResultAddr, innerSubstType,
          SGF.getLoweredType(opaque, innerSubstType), /*conformances=*/{});

      planTupleIntoIndirectResult(innerOrigType, innerSubstType, innerOrigType,
                                  innerSubstType, planData,
                                  outerConcreteResultAddr);

      addReabstractIndirectToDirect(innerOrigType, innerSubstType,
                                    outerOrigType, outerSubstType,
                                    outerConcreteResultAddr, outerResult);
      return;
    }
  }

  // Otherwise, the outer type is a tuple.
  assert(innerSubstType->getNumElements()
           == outerSubstTupleType->getNumElements());

  // Create direct outer results for each of the elements.
  for (auto eltIndex : indices(innerSubstType.getElementTypes())) {
    auto outerEltType =
        SGF.getSILType(outerResult).getTupleElementType(eltIndex);
    SILResultInfo outerEltResult(outerEltType.getASTType(),
                                 outerResult.getConvention());

    planIntoDirectResult(innerOrigType.getTupleElementType(eltIndex),
                         innerSubstType.getElementType(eltIndex),
                         outerOrigType.getTupleElementType(eltIndex),
                         outerSubstTupleType.getElementType(eltIndex),
                         planData, outerEltResult);
  }

  // Bind them together into a single tuple.
  addTupleDirect(innerSubstType->getNumElements(), outerResult);
}

/// Plan the emission of a call result as a single outer direct result,
/// given that the inner abstraction pattern is not a tuple.
void ResultPlanner::planScalarIntoDirectResult(AbstractionPattern innerOrigType,
                                               CanType innerSubstType,
                                               AbstractionPattern outerOrigType,
                                               CanType outerSubstType,
                                               PlanData &planData,
                                               SILResultInfo innerResult,
                                               SILResultInfo outerResult) {
  assert(!innerOrigType.isTuple());
  assert(!outerOrigType.isTuple());

  // If the inner result is indirect, plan to emit from that.
  if (SGF.silConv.isSILIndirect(innerResult)) {
    SILValue innerResultAddr =
      addInnerIndirectResultTemporary(planData, innerResult);
    planScalarFromIndirectResult(innerOrigType, innerSubstType,
                                 outerOrigType, outerSubstType,
                                 innerResultAddr, outerResult, SILValue());
    return;
  }

  // Otherwise, we have two direct results.

  // If there's no abstraction difference, it's just returned directly.
  if (SGF.getSILType(innerResult) == SGF.getSILType(outerResult)) {
    addDirectToDirect(innerResult, outerResult);

  // Otherwise, we need to reabstract.
  } else {
    addReabstractDirectToDirect(innerOrigType, innerSubstType,
                                outerOrigType, outerSubstType,
                                innerResult, outerResult);
  }
}

/// Plan the emission of a call result into an outer result address,
/// given that the inner abstraction pattern is not a tuple.
void
ResultPlanner::planScalarIntoIndirectResult(AbstractionPattern innerOrigType,
                                            CanType innerSubstType,
                                            AbstractionPattern outerOrigType,
                                            CanType outerSubstType,
                                            PlanData &planData,
                                            SILResultInfo innerResult,
                                            SILValue outerResultAddr) {
  assert(!innerOrigType.isTuple());
  assert(!outerOrigType.isTuple());

  bool hasAbstractionDifference =
    (innerResult.getType() != outerResultAddr->getType().getASTType());

  // If the inner result is indirect, we need some memory to emit it into.
  if (SGF.silConv.isSILIndirect(innerResult)) {
    // If there's no abstraction difference, that can just be
    // in-place into the outer result address.
    if (!hasAbstractionDifference) {
      addInPlace(planData, outerResultAddr);

    // Otherwise, we'll need a temporary.
    } else {
      SILValue innerResultAddr =
        addInnerIndirectResultTemporary(planData, innerResult);
      addReabstractIndirectToIndirect(innerOrigType, innerSubstType,
                                      outerOrigType, outerSubstType,
                                      innerResultAddr, outerResultAddr);
    }

  // Otherwise, the inner result is direct.
  } else {
    // If there's no abstraction difference, we just need to store.
    if (!hasAbstractionDifference) {
      addDirectToIndirect(innerResult, outerResultAddr);

    // Otherwise, we need to reabstract and store.
    } else {
      addReabstractDirectToIndirect(innerOrigType, innerSubstType,
                                    outerOrigType, outerSubstType,
                                    innerResult, outerResultAddr);      
    }
  }
}

/// Plan the emission of a call result from an inner result address.
void ResultPlanner::planFromIndirectResult(AbstractionPattern innerOrigType,
                                           CanType innerSubstType,
                                           AbstractionPattern outerOrigType,
                                           CanType outerSubstType,
                                           PlanData &planData,
                                           SILValue innerResultAddr) {
  assert(!innerOrigType.isTuple());

  if (outerOrigType.isTuple()) {
    planTupleFromIndirectResult(innerOrigType, cast<TupleType>(innerSubstType),
                                outerOrigType, cast<TupleType>(outerSubstType),
                                planData, innerResultAddr);
  } else {
    auto outerResult = claimNextOuterResult(planData);
    planScalarFromIndirectResult(innerOrigType, innerSubstType,
                                 outerOrigType, outerSubstType,
                                 innerResultAddr,
                                 outerResult.first, outerResult.second);
  }
}

/// Plan the emission of a call result from an inner result address, given
/// that the outer abstraction pattern is a tuple.
void
ResultPlanner::planTupleFromIndirectResult(AbstractionPattern innerOrigType,
                                           CanTupleType innerSubstType,
                                           AbstractionPattern outerOrigType,
                                           CanTupleType outerSubstType,
                                           PlanData &planData,
                                           SILValue innerResultAddr) {
  assert(!innerOrigType.isTuple());
  assert(innerSubstType->getNumElements() == outerSubstType->getNumElements());
  assert(outerOrigType.isTuple());

  for (auto eltIndex : indices(innerSubstType.getElementTypes())) {
    // Project the address of the element.
    SILValue innerEltResultAddr =
      SGF.B.createTupleElementAddr(Loc, innerResultAddr, eltIndex);

    // Plan to expand from that location.
    planFromIndirectResult(innerOrigType.getTupleElementType(eltIndex),
                           innerSubstType.getElementType(eltIndex),
                           outerOrigType.getTupleElementType(eltIndex),
                           outerSubstType.getElementType(eltIndex),
                           planData, innerEltResultAddr);
  }
}

void ResultPlanner::planTupleFromDirectResult(AbstractionPattern innerOrigType,
                                              CanTupleType innerSubstType,
                                              AbstractionPattern outerOrigType,
                                              CanTupleType outerSubstType,
                                              PlanData &planData,
                                              SILResultInfo innerResult) {

  assert(!innerOrigType.isTuple());
  auto outerSubstTupleType = dyn_cast<TupleType>(outerSubstType);

  assert(outerSubstTupleType && "Outer type must be a tuple");
  assert(innerSubstType->getNumElements() ==
         outerSubstTupleType->getNumElements());

  // Create direct outer results for each of the elements.
  for (auto eltIndex : indices(innerSubstType.getElementTypes())) {
    AbstractionPattern newOuterOrigType =
        outerOrigType.getTupleElementType(eltIndex);
    AbstractionPattern newInnerOrigType =
        innerOrigType.getTupleElementType(eltIndex);
    if (newOuterOrigType.isTuple()) {
      planTupleFromDirectResult(
          newInnerOrigType,
          cast<TupleType>(innerSubstType.getElementType(eltIndex)),
          newOuterOrigType,
          cast<TupleType>(outerSubstTupleType.getElementType(eltIndex)),
          planData, innerResult);
      continue;
    }

    auto outerResult = claimNextOuterResult(planData);
    auto elemType = outerSubstTupleType.getElementType(eltIndex);
    SILResultInfo eltResult(elemType, outerResult.first.getConvention());
    planScalarIntoDirectResult(
        newInnerOrigType, innerSubstType.getElementType(eltIndex),
        newOuterOrigType, outerSubstTupleType.getElementType(eltIndex),
        planData, eltResult, outerResult.first);
  }
}

/// Plan the emission of a call result from an inner result address,
/// given that the outer abstraction pattern is not a tuple.
void
ResultPlanner::planScalarFromIndirectResult(AbstractionPattern innerOrigType,
                                            CanType innerSubstType,
                                            AbstractionPattern outerOrigType,
                                            CanType outerSubstType,
                                            SILValue innerResultAddr,
                                            SILResultInfo outerResult,
                                            SILValue optOuterResultAddr) {
  assert(!innerOrigType.isTuple());
  assert(!outerOrigType.isTuple());
  assert(SGF.silConv.isSILIndirect(outerResult) == bool(optOuterResultAddr));

  bool hasAbstractionDifference =
    (innerResultAddr->getType().getASTType() != outerResult.getType());

  // The outer result can be indirect, and it doesn't necessarily have an
  // abstraction difference.  Note that we should only end up in this path
  // in cases where simply forwarding the outer result address wasn't possible.

  if (SGF.silConv.isSILIndirect(outerResult)) {
    assert(optOuterResultAddr);
    if (!hasAbstractionDifference) {
      addIndirectToIndirect(innerResultAddr, optOuterResultAddr);
    } else {
      addReabstractIndirectToIndirect(innerOrigType, innerSubstType,
                                      outerOrigType, outerSubstType,
                                      innerResultAddr, optOuterResultAddr);
    }
  } else {
    if (!hasAbstractionDifference) {
      addIndirectToDirect(innerResultAddr, outerResult);
    } else {
      addReabstractIndirectToDirect(innerOrigType, innerSubstType,
                                    outerOrigType, outerSubstType,
                                    innerResultAddr, outerResult);
    }
  }
}

void ResultPlanner::executeInnerTuple(
    SILValue innerElement, SmallVector<SILValue, 4> &innerDirectResults) {
  // NOTE: We know that our value is at +1 here.
  assert(innerElement->getType().getAs<TupleType>() &&
         "Only supports tuple inner types");

  SGF.B.emitDestructureValueOperation(
      Loc, innerElement, [&](unsigned index, SILValue elt) {
        if (elt->getType().is<TupleType>())
          return executeInnerTuple(elt, innerDirectResults);
        innerDirectResults.push_back(elt);
      });
}

SILValue ResultPlanner::execute(SILValue innerResult) {
  // The code emission here assumes that we don't need to have
  // active cleanups for all the result values we're not actively
  // transforming.  In other words, it's not "exception-safe".

  // Explode the inner direct results.
  SmallVector<SILValue, 4> innerDirectResults;
  auto innerResultTupleType = innerResult->getType().getAs<TupleType>();
  if (!innerResultTupleType) {
    innerDirectResults.push_back(innerResult);
  } else {
    {
      Scope S(SGF.Cleanups, CleanupLocation::get(Loc));

      // First create an rvalue cleanup for our direct result.
      assert(innerResult.getOwnershipKind() == ValueOwnershipKind::Owned ||
             innerResult.getOwnershipKind() == ValueOwnershipKind::Trivial);
      executeInnerTuple(innerResult, innerDirectResults);
      // Then allow the cleanups to be emitted in the proper reverse order.
    }
  }

  // Translate the result values.
  SmallVector<SILValue, 4> outerDirectResults;
  execute(innerDirectResults, outerDirectResults);

  // Implode the outer direct results.
  SILValue outerResult;
  if (outerDirectResults.size() == 1) {
    outerResult = outerDirectResults[0];
  } else {
    outerResult = SGF.B.createTuple(Loc, outerDirectResults);
  }

  return outerResult;
}

void ResultPlanner::execute(ArrayRef<SILValue> innerDirectResults,
                            SmallVectorImpl<SILValue> &outerDirectResults) {
  // A helper function to claim an inner direct result.
  auto claimNextInnerDirectResult = [&](SILResultInfo result) -> ManagedValue {
    auto resultValue = claimNext(innerDirectResults);
    assert(resultValue->getType() == SGF.getSILType(result));
    auto &resultTL = SGF.getTypeLowering(result.getType());
    switch (result.getConvention()) {
    case ResultConvention::Indirect:
      assert(!SGF.silConv.isSILIndirect(result)
             && "claiming indirect result as direct!");
      LLVM_FALLTHROUGH;
    case ResultConvention::Owned:
    case ResultConvention::Autoreleased:
      return SGF.emitManagedRValueWithCleanup(resultValue, resultTL);
    case ResultConvention::UnownedInnerPointer:
      // FIXME: We can't reasonably lifetime-extend an inner-pointer result
      // through a thunk. We don't know which parameter to the thunk was
      // originally 'self'.
      SGF.SGM.diagnose(Loc.getSourceLoc(), diag::not_implemented,
                       "reabstraction of returns_inner_pointer function");
      LLVM_FALLTHROUGH;
    case ResultConvention::Unowned:
      return SGF.emitManagedRetain(Loc, resultValue, resultTL);
    }
    llvm_unreachable("bad result convention!");
  };

  // A helper function to add an outer direct result.
  auto addOuterDirectResult = [&](ManagedValue resultValue,
                                  SILResultInfo result) {
    assert(resultValue.getType()
           == SGF.F.mapTypeIntoContext(SGF.getSILType(result)));
    outerDirectResults.push_back(resultValue.forward(SGF));
  };

  auto emitReabstract =
      [&](Operation &op, bool innerIsIndirect, bool outerIsIndirect) {
    // Set up the inner result.
    ManagedValue innerResult;
    if (innerIsIndirect) {
      innerResult = SGF.emitManagedBufferWithCleanup(op.InnerResultAddr);
    } else {
      innerResult = claimNextInnerDirectResult(op.InnerResult);
    }

    // Set up the context into which to emit the outer result.
    SGFContext outerResultCtxt;
    Optional<TemporaryInitialization> outerResultInit;
    if (outerIsIndirect) {
      outerResultInit.emplace(op.OuterResultAddr, CleanupHandle::invalid());
      outerResultCtxt = SGFContext(&*outerResultInit);
    }

    // Perform the translation.
    auto translated =
      SGF.emitTransformedValue(Loc, innerResult,
                               op.InnerOrigType, op.InnerSubstType,
                               op.OuterOrigType, op.OuterSubstType,
                               outerResultCtxt);

    // If the outer is indirect, force it into the context.
    if (outerIsIndirect) {
      if (!translated.isInContext()) {
        translated.forwardInto(SGF, Loc, op.OuterResultAddr);
      }

    // Otherwise, it's a direct result.
    } else {
      addOuterDirectResult(translated, op.OuterResult);
    }
  };

  // Execute each operation.
  for (auto &op : Operations) {
    switch (op.TheKind) {
    case Operation::DirectToDirect: {
      auto result = claimNextInnerDirectResult(op.InnerResult);
      addOuterDirectResult(result, op.OuterResult);
      continue;
    }

    case Operation::DirectToIndirect: {
      auto result = claimNextInnerDirectResult(op.InnerResult);
      SGF.B.emitStoreValueOperation(Loc, result.forward(SGF),
                                    op.OuterResultAddr,
                                    StoreOwnershipQualifier::Init);
      continue;
    }

    case Operation::IndirectToDirect: {
      auto resultAddr = op.InnerResultAddr;
      auto &resultTL = SGF.getTypeLowering(resultAddr->getType());
      auto result = SGF.emitManagedRValueWithCleanup(
          resultTL.emitLoad(SGF.B, Loc, resultAddr,
                            LoadOwnershipQualifier::Take),
          resultTL);
      addOuterDirectResult(result, op.OuterResult);
      continue;
    }

    case Operation::IndirectToIndirect: {
      // The type could be address-only; just take.
      SGF.B.createCopyAddr(Loc, op.InnerResultAddr, op.OuterResultAddr,
                           IsTake, IsInitialization);
      continue;
    }

    case Operation::ReabstractIndirectToIndirect:
      emitReabstract(op, /*indirect source*/ true, /*indirect dest*/ true);
      continue;
    case Operation::ReabstractIndirectToDirect:
      emitReabstract(op, /*indirect source*/ true, /*indirect dest*/ false);
      continue;
    case Operation::ReabstractDirectToIndirect:
      emitReabstract(op, /*indirect source*/ false, /*indirect dest*/ true);
      continue;
    case Operation::ReabstractDirectToDirect:
      emitReabstract(op, /*indirect source*/ false, /*indirect dest*/ false);
      continue;

    case Operation::TupleDirect: {
      auto firstEltIndex = outerDirectResults.size() - op.NumElements;
      auto elts = makeArrayRef(outerDirectResults).slice(firstEltIndex);
      auto tupleType = SGF.F.mapTypeIntoContext(SGF.getSILType(op.OuterResult));
      auto tuple = SGF.B.createTuple(Loc, tupleType, elts);
      outerDirectResults.resize(firstEltIndex);
      outerDirectResults.push_back(tuple);
      continue;
    }

    case Operation::InjectOptionalDirect: {
      SILValue value = outerDirectResults.pop_back_val();
      auto tupleType = SGF.F.mapTypeIntoContext(SGF.getSILType(op.OuterResult));
      SILValue optValue = SGF.B.createEnum(Loc, value, op.SomeDecl, tupleType);
      outerDirectResults.push_back(optValue);
      continue;
    }

    case Operation::InjectOptionalIndirect:
      SGF.B.createInjectEnumAddr(Loc, op.OuterResultAddr, op.SomeDecl);
      continue;
    }
    llvm_unreachable("bad operation kind");
  }

  assert(innerDirectResults.empty() && "didn't consume all inner results?");
}

/// Build the body of a transformation thunk.
///
/// \param inputOrigType Abstraction pattern of function value being thunked
/// \param inputSubstType Formal AST type of function value being thunked
/// \param outputOrigType Abstraction pattern of the thunk
/// \param outputSubstType Formal AST type of the thunk
static void buildThunkBody(SILGenFunction &SGF, SILLocation loc,
                           AbstractionPattern inputOrigType,
                           CanAnyFunctionType inputSubstType,
                           AbstractionPattern outputOrigType,
                           CanAnyFunctionType outputSubstType) {
  PrettyStackTraceSILFunction stackTrace("emitting reabstraction thunk in",
                                         &SGF.F);
  auto thunkType = SGF.F.getLoweredFunctionType();

  FullExpr scope(SGF.Cleanups, CleanupLocation::get(loc));

  SmallVector<ManagedValue, 8> params;
  SGF.collectThunkParams(loc, params);

  ManagedValue fnValue = params.pop_back_val();
  auto fnType = fnValue.getType().castTo<SILFunctionType>();
  assert(!fnType->isPolymorphic());
  auto argTypes = fnType->getParameters();

  // Translate the argument values.  Function parameters are
  // contravariant: we want to switch the direction of transformation
  // on them by flipping inputOrigType and outputOrigType.
  //
  // For example, a transformation of (Int,Int)->Int to (T,T)->T is
  // one that should take an (Int,Int)->Int value and make it be
  // abstracted like a (T,T)->T value.  This must be done with a thunk.
  // Within the thunk body, the result of calling the inner function
  // needs to be translated from Int to T (we receive a normal Int
  // and return it like a T), but the parameters are translated in the
  // other direction (the thunk receives an Int like a T, and passes it
  // like a normal Int when calling the inner function).
  SmallVector<ManagedValue, 8> args;
  TranslateArguments(SGF, loc, params, args, argTypes)
    .translate(outputOrigType,
               outputSubstType.getParams(),
               inputOrigType,
               inputSubstType.getParams());

  SmallVector<SILValue, 8> argValues;

  // Plan the results.  This builds argument values for all the
  // inner indirect results.
  ResultPlanner resultPlanner(SGF, loc);
  resultPlanner.plan(inputOrigType.getFunctionResultType(),
                     inputSubstType.getResult(),
                     outputOrigType.getFunctionResultType(),
                     outputSubstType.getResult(),
                     fnType, thunkType, argValues);

  // Add the rest of the arguments.
  forwardFunctionArguments(SGF, loc, fnType, args, argValues);

  auto fun = fnType->isCalleeGuaranteed() ? fnValue.borrow(SGF, loc).getValue()
                                          : fnValue.forward(SGF);
  SILValue innerResult =
      SGF.emitApplyWithRethrow(loc, fun,
                               /*substFnType*/ fnValue.getType(),
                               /*substitutions*/ {}, argValues);

  // Reabstract the result.
  SILValue outerResult = resultPlanner.execute(innerResult);

  scope.pop();
  SGF.B.createReturn(loc, outerResult);
}

/// Build a generic signature and environment for a re-abstraction thunk.
///
/// Most thunks share the generic environment with their original function.
/// The one exception is if the thunk type involves an open existential,
/// in which case we "promote" the opened existential to a new generic parameter.
///
/// \param SGF - the parent function
/// \param openedExistential - the opened existential to promote to a generic
//  parameter, if any
/// \param inheritGenericSig - whether to inherit the generic signature from the
/// parent function.
/// \param genericEnv - the new generic environment
/// \param contextSubs - map old archetypes to new archetypes
/// \param interfaceSubs - map interface types to old archetypes
static CanGenericSignature
buildThunkSignature(SILGenFunction &SGF,
                    bool inheritGenericSig,
                    ArchetypeType *openedExistential,
                    GenericEnvironment *&genericEnv,
                    SubstitutionMap &contextSubs,
                    SubstitutionMap &interfaceSubs,
                    ArchetypeType *&newArchetype) {
  auto *mod = SGF.F.getModule().getSwiftModule();
  auto &ctx = mod->getASTContext();

  // If there's no opened existential, we just inherit the generic environment
  // from the parent function.
  if (openedExistential == nullptr) {
    auto genericSig = SGF.F.getLoweredFunctionType()->getGenericSignature();
    genericEnv = SGF.F.getGenericEnvironment();
    interfaceSubs = SGF.F.getForwardingSubstitutionMap();
    contextSubs = interfaceSubs;
    return genericSig;
  }

  GenericSignatureBuilder builder(ctx);

  // Add the existing generic signature.
  int depth = 0;
  if (inheritGenericSig) {
    if (auto genericSig = SGF.F.getLoweredFunctionType()->getGenericSignature()) {
      builder.addGenericSignature(genericSig);
      depth = genericSig->getGenericParams().back()->getDepth() + 1;
    }
  }

  // Add a new generic parameter to replace the opened existential.
  auto *newGenericParam = GenericTypeParamType::get(depth, 0, ctx);

  builder.addGenericParameter(newGenericParam);
  Requirement newRequirement(RequirementKind::Conformance, newGenericParam,
                             openedExistential->getOpenedExistentialType());
  auto source =
    GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
  builder.addRequirement(newRequirement, source, nullptr);

  GenericSignature *genericSig =
    std::move(builder).computeGenericSignature(SourceLoc(),
                                    /*allowConcreteGenericParams=*/true);
  genericEnv = genericSig->createGenericEnvironment();

  newArchetype = genericEnv->mapTypeIntoContext(newGenericParam)
    ->castTo<ArchetypeType>();

  // Calculate substitutions to map the caller's archetypes to the thunk's
  // archetypes.
  if (auto calleeGenericSig = SGF.F.getLoweredFunctionType()
          ->getGenericSignature()) {
    contextSubs = SubstitutionMap::get(
      calleeGenericSig,
      [&](SubstitutableType *type) -> Type {
        return genericEnv->mapTypeIntoContext(type);
      },
      MakeAbstractConformanceForGenericType());
  }

  // Calculate substitutions to map interface types to the caller's archetypes.
  interfaceSubs = SubstitutionMap::get(
    genericSig,
    [&](SubstitutableType *type) -> Type {
      if (type->isEqual(newGenericParam))
        return openedExistential;
      return SGF.F.mapTypeIntoContext(type);
    },
    MakeAbstractConformanceForGenericType());

  return genericSig->getCanonicalSignature();
}

/// Build the type of a function transformation thunk.
CanSILFunctionType SILGenFunction::buildThunkType(
    CanSILFunctionType &sourceType,
    CanSILFunctionType &expectedType,
    CanType &inputSubstType,
    CanType &outputSubstType,
    GenericEnvironment *&genericEnv,
    SubstitutionMap &interfaceSubs,
    bool withoutActuallyEscaping) {
  assert(!expectedType->isPolymorphic());
  assert(!sourceType->isPolymorphic());

  // Can't build a thunk without context, so we require ownership semantics
  // on the result type.
  assert(expectedType->getExtInfo().hasContext());

  // This may inherit @noescape from the expectedType. The @noescape attribute
  // is only stripped when using this type to materialize a new decl.
  auto extInfo = expectedType->getExtInfo()
    .withRepresentation(SILFunctionType::Representation::Thin);

  if (withoutActuallyEscaping)
    extInfo = extInfo.withNoEscape(false);

  // Does the thunk type involve archetypes other than opened existentials?
  bool hasArchetypes = false;
  // Does the thunk type involve an open existential type?
  CanArchetypeType openedExistential;
  auto archetypeVisitor = [&](CanType t) {
    if (auto archetypeTy = dyn_cast<ArchetypeType>(t)) {
      if (archetypeTy->getOpenedExistentialType()) {
        assert((openedExistential == CanArchetypeType() ||
                openedExistential == archetypeTy) &&
               "one too many open existentials");
        openedExistential = archetypeTy;
      } else
        hasArchetypes = true;
    }
  };

  // Use the generic signature from the context if the thunk involves
  // generic parameters.
  CanGenericSignature genericSig;
  SubstitutionMap contextSubs;
  ArchetypeType *newArchetype = nullptr;

  if (expectedType->hasArchetype() || sourceType->hasArchetype()) {
    expectedType.visit(archetypeVisitor);
    sourceType.visit(archetypeVisitor);

    genericSig = buildThunkSignature(*this,
                                     hasArchetypes,
                                     openedExistential,
                                     genericEnv,
                                     contextSubs,
                                     interfaceSubs,
                                     newArchetype);
  }

  // Utility function to apply contextSubs, and also replace the
  // opened existential with the new archetype.
  auto substIntoThunkContext = [&](CanType t) -> CanType {
    return t.subst(
      [&](SubstitutableType *type) -> Type {
        if (CanType(type) == openedExistential)
          return newArchetype;
        return Type(type).subst(contextSubs);
      },
      LookUpConformanceInSubstitutionMap(contextSubs),
      SubstFlags::AllowLoweredTypes)
        ->getCanonicalType();
  };

  sourceType = cast<SILFunctionType>(
    substIntoThunkContext(sourceType));
  expectedType = cast<SILFunctionType>(
    substIntoThunkContext(expectedType));

  if (inputSubstType) {
    inputSubstType = cast<AnyFunctionType>(
      substIntoThunkContext(inputSubstType));
  }

  if (outputSubstType) {
    outputSubstType = cast<AnyFunctionType>(
      substIntoThunkContext(outputSubstType));
  }

  // If our parent function was pseudogeneric, this thunk must also be
  // pseudogeneric, since we have no way to pass generic parameters.
  if (genericSig)
    if (F.getLoweredFunctionType()->isPseudogeneric())
      extInfo = extInfo.withIsPseudogeneric();

  // Add the function type as the parameter.
  auto contextConvention =
      SILType::getPrimitiveObjectType(sourceType).isTrivial(this->getModule())
          ? ParameterConvention::Direct_Unowned
          : ParameterConvention::Direct_Guaranteed;
  SmallVector<SILParameterInfo, 4> params;
  params.append(expectedType->getParameters().begin(),
                expectedType->getParameters().end());
  params.push_back({sourceType,
                    sourceType->getExtInfo().hasContext()
                      ? contextConvention
                      : ParameterConvention::Direct_Unowned});

  // Map the parameter and expected types out of context to get the interface
  // type of the thunk.
  SmallVector<SILParameterInfo, 4> interfaceParams;
  interfaceParams.reserve(params.size());
  for (auto &param : params) {
    auto paramIfaceTy = param.getType()->mapTypeOutOfContext();
    interfaceParams.push_back(
      SILParameterInfo(paramIfaceTy->getCanonicalType(genericSig),
                       param.getConvention()));
  }

  SmallVector<SILYieldInfo, 4> interfaceYields;
  for (auto &yield : expectedType->getYields()) {
    auto yieldIfaceTy = yield.getType()->mapTypeOutOfContext();
    auto interfaceYield =
      yield.getWithType(yieldIfaceTy->getCanonicalType(genericSig));
    interfaceYields.push_back(interfaceYield);
  }

  SmallVector<SILResultInfo, 4> interfaceResults;
  for (auto &result : expectedType->getResults()) {
    auto resultIfaceTy = result.getType()->mapTypeOutOfContext();
    auto interfaceResult =
      result.getWithType(resultIfaceTy->getCanonicalType(genericSig));
    interfaceResults.push_back(interfaceResult);
  }

  Optional<SILResultInfo> interfaceErrorResult;
  if (expectedType->hasErrorResult()) {
    auto errorResult = expectedType->getErrorResult();
    auto errorIfaceTy = errorResult.getType()->mapTypeOutOfContext();
    interfaceErrorResult = SILResultInfo(
        errorIfaceTy->getCanonicalType(genericSig),
        expectedType->getErrorResult().getConvention());
  }
  
  // The type of the thunk function.
  return SILFunctionType::get(genericSig, extInfo,
                              expectedType->getCoroutineKind(),
                              ParameterConvention::Direct_Unowned,
                              interfaceParams, interfaceYields,
                              interfaceResults, interfaceErrorResult,
                              getASTContext());
}

/// Create a reabstraction thunk.
static ManagedValue createThunk(SILGenFunction &SGF,
                                SILLocation loc,
                                ManagedValue fn,
                                AbstractionPattern inputOrigType,
                                CanAnyFunctionType inputSubstType,
                                AbstractionPattern outputOrigType,
                                CanAnyFunctionType outputSubstType,
                                const TypeLowering &expectedTL) {
  auto sourceType = fn.getType().castTo<SILFunctionType>();
  auto expectedType = expectedTL.getLoweredType().castTo<SILFunctionType>();

  // We can't do bridging here.
  assert(expectedType->getLanguage() ==
         fn.getType().castTo<SILFunctionType>()->getLanguage() &&
         "bridging in re-abstraction thunk?");

  // Declare the thunk.
  SubstitutionMap interfaceSubs;
  GenericEnvironment *genericEnv = nullptr;
  auto toType = expectedType->getWithExtInfo(
      expectedType->getExtInfo().withNoEscape(false));
  auto thunkType = SGF.buildThunkType(sourceType, toType,
                                      inputSubstType,
                                      outputSubstType,
                                      genericEnv,
                                      interfaceSubs);
  auto thunk = SGF.SGM.getOrCreateReabstractionThunk(
                                       thunkType,
                                       sourceType,
                                       toType,
                                       SGF.F.isSerialized());

  // Build it if necessary.
  if (thunk->empty()) {
    thunk->setGenericEnvironment(genericEnv);
    SILGenFunction thunkSGF(SGF.SGM, *thunk, SGF.FunctionDC);
    auto loc = RegularLocation::getAutoGeneratedLocation();
    buildThunkBody(thunkSGF, loc,
                   inputOrigType,
                   inputSubstType,
                   outputOrigType,
                   outputSubstType);
  }

  CanSILFunctionType substFnType = thunkType;

  if (thunkType->getGenericSignature()) {
    substFnType = thunkType->substGenericArgs(SGF.F.getModule(),
                                              interfaceSubs);
  }

  // Create it in our current function.
  auto thunkValue = SGF.B.createFunctionRef(loc, thunk);
  ManagedValue thunkedFn =
    SGF.B.createPartialApply(loc, thunkValue,
                             SILType::getPrimitiveObjectType(substFnType),
                             interfaceSubs, fn.ensurePlusOne(SGF, loc),
                             SILType::getPrimitiveObjectType(toType));

  if (!expectedType->isNoEscape()) {
    return thunkedFn;
  }

  // Handle the escaping to noescape conversion.
  assert(expectedType->isNoEscape());
  return SGF.B.createConvertEscapeToNoEscape(
      loc, thunkedFn, SILType::getPrimitiveObjectType(expectedType), false);
}

static CanSILFunctionType buildWithoutActuallyEscapingThunkType(
    SILGenFunction &SGF, CanSILFunctionType &noEscapingType,
    CanSILFunctionType &escapingType, GenericEnvironment *&genericEnv,
    SubstitutionMap &interfaceSubs) {

  assert(escapingType->getExtInfo() ==
         noEscapingType->getExtInfo().withNoEscape(false));

  CanType inputSubstType, outputSubstType;
  return SGF.buildThunkType(noEscapingType, escapingType,
                            inputSubstType, outputSubstType,
                            genericEnv, interfaceSubs,
                            /*withoutActuallyEscaping=*/true);
}

static void buildWithoutActuallyEscapingThunkBody(SILGenFunction &SGF) {
  PrettyStackTraceSILFunction stackTrace(
      "emitting withoutAcutallyEscaping thunk in", &SGF.F);

  auto loc = RegularLocation::getAutoGeneratedLocation();

  FullExpr scope(SGF.Cleanups, CleanupLocation::get(loc));

  SmallVector<ManagedValue, 8> params;
  SmallVector<SILArgument*, 8> indirectResults;
  SGF.collectThunkParams(loc, params, &indirectResults);

  ManagedValue fnValue = params.pop_back_val();
  auto fnType = fnValue.getType().castTo<SILFunctionType>();

  SmallVector<SILValue, 8> argValues;
  if (!indirectResults.empty()) {
    for (auto *result : indirectResults)
      argValues.push_back(SILValue(result));
  }

  // Forward indirect result arguments.

   // Add the rest of the arguments.
  forwardFunctionArguments(SGF, loc, fnType, params, argValues);

  auto fun = fnType->isCalleeGuaranteed() ? fnValue.borrow(SGF, loc).getValue()
                                          : fnValue.forward(SGF);
  SILValue result =
      SGF.emitApplyWithRethrow(loc, fun,
                               /*substFnType*/ fnValue.getType(),
                               /*substitutions*/ {}, argValues);

  scope.pop();
  SGF.B.createReturn(loc, result);
}

ManagedValue
SILGenFunction::createWithoutActuallyEscapingClosure(
    SILLocation loc, ManagedValue noEscapingFunctionValue, SILType escapingTy) {

  auto escapingFnTy = escapingTy.castTo<SILFunctionType>();
  // TODO: maybe this should use a more explicit instruction.
  assert(escapingFnTy->getExtInfo() == noEscapingFunctionValue.getType()
                                           .castTo<SILFunctionType>()
                                           ->getExtInfo()
                                           .withNoEscape(false));

  SubstitutionMap interfaceSubs;
  GenericEnvironment *genericEnv = nullptr;
  auto noEscapingFnTy =
      noEscapingFunctionValue.getType().castTo<SILFunctionType>();

  auto thunkType = buildWithoutActuallyEscapingThunkType(
      *this, noEscapingFnTy, escapingFnTy, genericEnv, interfaceSubs);

  auto *thunk = SGM.getOrCreateReabstractionThunk(
      thunkType, noEscapingFnTy, escapingFnTy, F.isSerialized());

  if (thunk->empty()) {
    thunk->setWithoutActuallyEscapingThunk();
    thunk->setGenericEnvironment(genericEnv);
    SILGenFunction thunkSGF(SGM, *thunk, FunctionDC);
    buildWithoutActuallyEscapingThunkBody(thunkSGF);
  }
  assert(thunk->isWithoutActuallyEscapingThunk());

  CanSILFunctionType substFnTy = thunkType;
  // Use the subsitution map in the context of the current function.
  // thunk->getForwardingSubstitutionMap() / thunk might have been created in a
  // different function's generic enviroment.
  if (thunkType->getGenericSignature()) {
    substFnTy = thunkType->substGenericArgs(F.getModule(), interfaceSubs);
  }

  // Create it in our current function.
  auto thunkValue = B.createFunctionRef(loc, thunk);
  SILValue noEscapeValue =
      noEscapingFunctionValue.ensurePlusOne(*this, loc).forward(*this);
  SingleValueInstruction *thunkedFn = B.createPartialApply(
      loc, thunkValue,
      SILType::getPrimitiveObjectType(substFnTy),
      interfaceSubs,
      noEscapeValue,
      SILType::getPrimitiveObjectType(escapingFnTy));
  // We need to ensure the 'lifetime' of the trivial values context captures. As
  // long as we rerpresent these captures by the same value the following works.
  thunkedFn = B.createMarkDependence(loc, thunkedFn, noEscapeValue);

  return emitManagedRValueWithCleanup(thunkedFn);
}

ManagedValue Transform::transformFunction(ManagedValue fn,
                                          AbstractionPattern inputOrigType,
                                          CanAnyFunctionType inputSubstType,
                                          AbstractionPattern outputOrigType,
                                          CanAnyFunctionType outputSubstType,
                                          const TypeLowering &expectedTL) {
  assert(fn.getType().isObject() &&
         "expected input to emitTransformedFunctionValue to be loaded");

  auto expectedFnType = expectedTL.getLoweredType().castTo<SILFunctionType>();

  auto fnType = fn.getType().castTo<SILFunctionType>();
  assert(expectedFnType->getExtInfo().hasContext()
         || !fnType->getExtInfo().hasContext());

  // If there's no abstraction difference, we're done.
  if (fnType == expectedFnType) {
    return fn;
  }

  // Check if we require a re-abstraction thunk.
  if (SGF.SGM.Types.checkForABIDifferences(
                              SILType::getPrimitiveObjectType(fnType),
                              SILType::getPrimitiveObjectType(expectedFnType))
        == TypeConverter::ABIDifference::NeedsThunk) {
    assert(expectedFnType->getExtInfo().hasContext()
           && "conversion thunk will not be thin!");
    return createThunk(SGF, Loc, fn,
                       inputOrigType, inputSubstType,
                       outputOrigType, outputSubstType,
                       expectedTL);
  }

  // We do not, conversion is trivial.
  auto expectedEI = expectedFnType->getExtInfo();
  auto newEI = expectedEI.withRepresentation(fnType->getRepresentation())
                   .withNoEscape(fnType->getRepresentation() ==
                                         SILFunctionType::Representation::Thick
                                     ? fnType->isNoEscape()
                                     : expectedFnType->isNoEscape());
  auto newFnType =
      adjustFunctionType(expectedFnType, newEI, fnType->getCalleeConvention(),
                         fnType->getWitnessMethodConformanceOrNone());

  // Apply any ABI-compatible conversions before doing thin-to-thick or
  // escaping->noescape conversion.
  if (fnType != newFnType) {
    SILType resTy = SILType::getPrimitiveObjectType(newFnType);
    fn = SGF.B.createConvertFunction(Loc, fn, resTy);
  }

  // Now do thin-to-thick if necessary.
  if (newFnType != expectedFnType &&
      fnType->getRepresentation() == SILFunctionTypeRepresentation::Thin) {
    assert(expectedEI.getRepresentation() ==
           SILFunctionTypeRepresentation::Thick &&
           "all other conversions should have been handled by "
           "FunctionConversionExpr");
    SILType resTy = SILType::getPrimitiveObjectType(expectedFnType);
    fn = SGF.emitManagedRValueWithCleanup(
        SGF.B.createThinToThickFunction(Loc, fn.forward(SGF), resTy));
  } else if (newFnType != expectedFnType) {
    // Escaping to noescape conversion.
    SILType resTy = SILType::getPrimitiveObjectType(expectedFnType);
    fn = SGF.B.createConvertEscapeToNoEscape(Loc, fn, resTy, false);
  }

  return fn;
}

/// Given a value with the abstraction patterns of the original formal
/// type, give it the abstraction patterns of the substituted formal type.
ManagedValue
SILGenFunction::emitOrigToSubstValue(SILLocation loc, ManagedValue v,
                                     AbstractionPattern origType,
                                     CanType substType,
                                     SGFContext ctxt) {
  
  return emitTransformedValue(loc, v,
                              origType, substType,
                              AbstractionPattern(substType), substType,
                              ctxt);
}

/// Given a value with the abstraction patterns of the original formal
/// type, give it the abstraction patterns of the substituted formal type.
RValue SILGenFunction::emitOrigToSubstValue(SILLocation loc, RValue &&v,
                                            AbstractionPattern origType,
                                            CanType substType,
                                            SGFContext ctxt) {
  return emitTransformedValue(loc, std::move(v),
                              origType, substType,
                              AbstractionPattern(substType), substType,
                              ctxt);
}

/// Given a value with the abstraction patterns of the substituted
/// formal type, give it the abstraction patterns of the original
/// formal type.
ManagedValue
SILGenFunction::emitSubstToOrigValue(SILLocation loc, ManagedValue v,
                                     AbstractionPattern origType,
                                     CanType substType,
                                     SGFContext ctxt) {
  return emitTransformedValue(loc, v,
                              AbstractionPattern(substType), substType,
                              origType, substType,
                              ctxt);
}

/// Given a value with the abstraction patterns of the substituted
/// formal type, give it the abstraction patterns of the original
/// formal type.
RValue SILGenFunction::emitSubstToOrigValue(SILLocation loc, RValue &&v,
                                            AbstractionPattern origType,
                                            CanType substType,
                                            SGFContext ctxt) {
  return emitTransformedValue(loc, std::move(v),
                              AbstractionPattern(substType), substType,
                              origType, substType,
                              ctxt);
}

ManagedValue
SILGenFunction::emitMaterializedRValueAsOrig(Expr *expr,
                                             AbstractionPattern origType) {  
  // Create a temporary.
  auto &origTL = getTypeLowering(origType, expr->getType());
  auto temporary = emitTemporary(expr, origTL);

  // Emit the reabstracted r-value.
  auto result =
    emitRValueAsOrig(expr, origType, origTL, SGFContext(temporary.get()));

  // Force the result into the temporary.
  if (!result.isInContext()) {
    temporary->copyOrInitValueInto(*this, expr, result, /*init*/ true);
    temporary->finishInitialization(*this);
  }

  return temporary->getManagedAddress();
}

ManagedValue
SILGenFunction::emitRValueAsOrig(Expr *expr, AbstractionPattern origPattern,
                                 const TypeLowering &origTL, SGFContext ctxt) {
  auto outputSubstType = expr->getType()->getCanonicalType();
  auto &substTL = getTypeLowering(outputSubstType);
  if (substTL.getLoweredType() == origTL.getLoweredType())
    return emitRValueAsSingleValue(expr, ctxt);

  ManagedValue temp = emitRValueAsSingleValue(expr);
  return emitSubstToOrigValue(expr, temp, origPattern,
                              outputSubstType, ctxt);
}

ManagedValue
SILGenFunction::emitTransformedValue(SILLocation loc, ManagedValue v,
                                     CanType inputType,
                                     CanType outputType,
                                     SGFContext ctxt) {
  return emitTransformedValue(loc, v,
                              AbstractionPattern(inputType), inputType,
                              AbstractionPattern(outputType), outputType,
                              ctxt);
}

ManagedValue
SILGenFunction::emitTransformedValue(SILLocation loc, ManagedValue v,
                                     AbstractionPattern inputOrigType,
                                     CanType inputSubstType,
                                     AbstractionPattern outputOrigType,
                                     CanType outputSubstType,
                                     SGFContext ctxt) {
  return Transform(*this, loc).transform(v,
                                         inputOrigType,
                                         inputSubstType,
                                         outputOrigType,
                                         outputSubstType, ctxt);
}

RValue
SILGenFunction::emitTransformedValue(SILLocation loc, RValue &&v,
                                     AbstractionPattern inputOrigType,
                                     CanType inputSubstType,
                                     AbstractionPattern outputOrigType,
                                     CanType outputSubstType,
                                     SGFContext ctxt) {
  return Transform(*this, loc).transform(std::move(v),
                                         inputOrigType,
                                         inputSubstType,
                                         outputOrigType,
                                         outputSubstType, ctxt);
}

//===----------------------------------------------------------------------===//
// vtable thunks
//===----------------------------------------------------------------------===//

void
SILGenFunction::emitVTableThunk(SILDeclRef derived,
                                SILFunction *implFn,
                                AbstractionPattern inputOrigType,
                                CanAnyFunctionType inputSubstType,
                                CanAnyFunctionType outputSubstType) {
  auto fd = cast<AbstractFunctionDecl>(derived.getDecl());

  SILLocation loc(fd);
  loc.markAutoGenerated();
  CleanupLocation cleanupLoc(fd);
  cleanupLoc.markAutoGenerated();
  Scope scope(Cleanups, cleanupLoc);

  auto fTy = implFn->getLoweredFunctionType();
  
  SubstitutionMap subs;
  if (auto *genericEnv = fd->getGenericEnvironment()) {
    F.setGenericEnvironment(genericEnv);
    subs = getForwardingSubstitutionMap();
    fTy = fTy->substGenericArgs(SGM.M, subs);

    inputSubstType = cast<FunctionType>(
        cast<GenericFunctionType>(inputSubstType)
            ->substGenericArgs(subs)->getCanonicalType());
    outputSubstType = cast<FunctionType>(
        cast<GenericFunctionType>(outputSubstType)
            ->substGenericArgs(subs)->getCanonicalType());
  }

  // Emit the indirect return and arguments.
  auto thunkTy = F.getLoweredFunctionType();

  SmallVector<ManagedValue, 8> thunkArgs;
  collectThunkParams(loc, thunkArgs);

  SmallVector<ManagedValue, 8> substArgs;

  AbstractionPattern outputOrigType(outputSubstType);

  // Reabstract the arguments.
  TranslateArguments(*this, loc, thunkArgs, substArgs, fTy->getParameters())
    .translate(inputOrigType,
               inputSubstType.getParams(),
               outputOrigType,
               outputSubstType.getParams());
  
  // Collect the arguments to the implementation.
  SmallVector<SILValue, 8> args;

  // First, indirect results.
  ResultPlanner resultPlanner(*this, loc);
  resultPlanner.plan(outputOrigType.getFunctionResultType(),
                     outputSubstType.getResult(),
                     inputOrigType.getFunctionResultType(),
                     inputSubstType.getResult(),
                     fTy, thunkTy, args);

  // Then, the arguments.
  forwardFunctionArguments(*this, loc, fTy, substArgs, args);

  // Create the call.
  auto implRef = B.createFunctionRef(loc, implFn);
  SILValue implResult = emitApplyWithRethrow(loc, implRef,
                                SILType::getPrimitiveObjectType(fTy),
                                subs, args);

  // Reabstract the return.
  SILValue result = resultPlanner.execute(implResult);
  
  scope.pop();
  B.createReturn(loc, result);

  // Now that the thunk body has been completely emitted, verify the
  // body early.
  F.verify();
}

//===----------------------------------------------------------------------===//
// Protocol witnesses
//===----------------------------------------------------------------------===//

enum class WitnessDispatchKind {
  Static,
  Dynamic,
  Class
};

static WitnessDispatchKind getWitnessDispatchKind(SILDeclRef witness) {
  auto *decl = witness.getDecl();

  ClassDecl *C = decl->getDeclContext()->getSelfClassDecl();
  if (!C)
    return WitnessDispatchKind::Static;

  // If the witness is dynamic, go through dynamic dispatch.
  if (decl->isDynamic()) {
    // For initializers we still emit a static allocating thunk around
    // the dynamic initializing entry point.
    if (witness.kind == SILDeclRef::Kind::Allocator)
      return WitnessDispatchKind::Static;
    return WitnessDispatchKind::Dynamic;
  }

  bool isFinal = (decl->isFinal() || C->isFinal());
  if (auto fnDecl = dyn_cast<AbstractFunctionDecl>(witness.getDecl()))
    isFinal |= fnDecl->hasForcedStaticDispatch();

  bool isExtension = isa<ExtensionDecl>(decl->getDeclContext());

  // If we have a final method or a method from an extension that is not
  // Objective-C, emit a static reference.
  // A natively ObjC method witness referenced this way will end up going
  // through its native thunk, which will redispatch the method after doing
  // bridging just like we want.
  if (isFinal || isExtension || witness.isForeignToNativeThunk())
    return WitnessDispatchKind::Static;

  if (witness.kind == SILDeclRef::Kind::Allocator) {
    // Non-required initializers can witness a protocol requirement if the class
    // is final, so we can statically dispatch to them.
    if (!cast<ConstructorDecl>(decl)->isRequired())
      return WitnessDispatchKind::Static;

    // We emit a static thunk for ObjC allocating constructors.
    if (decl->hasClangNode())
      return WitnessDispatchKind::Static;
  }

  // Otherwise emit a class method.
  return WitnessDispatchKind::Class;
}

static CanSILFunctionType
getWitnessFunctionType(SILGenModule &SGM,
                       SILDeclRef witness,
                       WitnessDispatchKind witnessKind) {
  switch (witnessKind) {
  case WitnessDispatchKind::Static:
  case WitnessDispatchKind::Dynamic:
    return SGM.Types.getConstantInfo(witness).SILFnType;
  case WitnessDispatchKind::Class:
    return SGM.Types.getConstantOverrideType(witness);
  }

  llvm_unreachable("Unhandled WitnessDispatchKind in switch.");
}

static SILValue
getWitnessFunctionRef(SILGenFunction &SGF,
                      SILDeclRef witness,
                      CanSILFunctionType witnessFTy,
                      WitnessDispatchKind witnessKind,
                      SmallVectorImpl<ManagedValue> &witnessParams,
                      SILLocation loc) {
  switch (witnessKind) {
  case WitnessDispatchKind::Static:
    return SGF.emitGlobalFunctionRef(loc, witness);
  case WitnessDispatchKind::Dynamic:
    return SGF.emitDynamicMethodRef(loc, witness, witnessFTy).getValue();
  case WitnessDispatchKind::Class: {
    SILValue selfPtr = witnessParams.back().getValue();
    return SGF.emitClassMethodRef(loc, selfPtr, witness, witnessFTy);
  }
  }

  llvm_unreachable("Unhandled WitnessDispatchKind in switch.");
}

namespace {
  class EmitAbortApply : public Cleanup {
    SILValue Token;
  public:
    EmitAbortApply(SILValue token) : Token(token) {}
    void emit(SILGenFunction &SGF, CleanupLocation loc,
              ForUnwind_t forUnwind) override {
      SGF.B.createAbortApply(loc, Token);
    }
    void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
      llvm::errs() << "EmitAbortApply\n"
                   << "State:" << getState() << "\n"
                   << "Token:" << Token << "\n";
#endif
    }
  };
}

void SILGenFunction::emitProtocolWitness(AbstractionPattern reqtOrigTy,
                                         CanAnyFunctionType reqtSubstTy,
                                         SILDeclRef requirement,
                                         SubstitutionMap reqtSubs,
                                         SILDeclRef witness,
                                         SubstitutionMap witnessSubs,
                                         IsFreeFunctionWitness_t isFree) {
  // FIXME: Disable checks that the protocol witness carries debug info.
  // Should we carry debug info for witnesses?
  F.setBare(IsBare);

  SILLocation loc(witness.getDecl());
  loc.markAutoGenerated();

  CleanupLocation cleanupLoc(witness.getDecl());
  cleanupLoc.markAutoGenerated();

  FullExpr scope(Cleanups, cleanupLoc);
  FormalEvaluationScope formalEvalScope(*this);

  auto witnessKind = getWitnessDispatchKind(witness);
  auto thunkTy = F.getLoweredFunctionType();

  SmallVector<ManagedValue, 8> origParams;
  collectThunkParams(loc, origParams);

  // Get the type of the witness.
  auto witnessInfo = getConstantInfo(witness);
  CanAnyFunctionType witnessSubstTy = witnessInfo.LoweredType;
  if (auto genericFnType = dyn_cast<GenericFunctionType>(witnessSubstTy)) {
    witnessSubstTy = cast<FunctionType>(genericFnType
                                          ->substGenericArgs(witnessSubs)
                                          ->getCanonicalType());
  }

  if (auto genericFnType = dyn_cast<GenericFunctionType>(reqtSubstTy)) {
    auto forwardingSubs = F.getForwardingSubstitutionMap();
    reqtSubstTy = cast<FunctionType>(genericFnType
                                          ->substGenericArgs(forwardingSubs)
                                          ->getCanonicalType());
  } else {
    reqtSubstTy = cast<FunctionType>(F.mapTypeIntoContext(reqtSubstTy)
                                          ->getCanonicalType());
  }

  auto reqtSubstParams = reqtSubstTy.getParams();
  auto witnessSubstParams = witnessSubstTy.getParams();

  // For a free function witness, discard the 'self' parameter of the
  // requirement.
  if (isFree) {
    origParams.pop_back();
    reqtSubstParams = reqtSubstParams.drop_back();
  }

  // Translate the argument values from the requirement abstraction level to
  // the substituted signature of the witness.
  auto origWitnessFTy = getWitnessFunctionType(SGM, witness, witnessKind);
  auto witnessFTy = origWitnessFTy;
  if (!witnessSubs.empty())
    witnessFTy = origWitnessFTy->substGenericArgs(SGM.M, witnessSubs);

  SmallVector<ManagedValue, 8> witnessParams;
  AbstractionPattern witnessOrigTy(witnessInfo.LoweredType);
  TranslateArguments(*this, loc,
                     origParams, witnessParams,
                     witnessFTy->getParameters())
    .translate(reqtOrigTy,
               reqtSubstParams,
               witnessOrigTy,
               witnessSubstParams);

  SILValue witnessFnRef = getWitnessFunctionRef(*this, witness,
                                                origWitnessFTy,
                                                witnessKind,
                                                witnessParams, loc);

  auto coroutineKind =
    witnessFnRef->getType().castTo<SILFunctionType>()->getCoroutineKind();
  assert(coroutineKind == F.getLoweredFunctionType()->getCoroutineKind() &&
         "coroutine-ness mismatch between requirement and witness");

  // Collect the arguments.
  SmallVector<SILValue, 8> args;

  Optional<ResultPlanner> resultPlanner;
  if (coroutineKind == SILCoroutineKind::None) {
    //   - indirect results
    resultPlanner.emplace(*this, loc);
    resultPlanner->plan(witnessOrigTy.getFunctionResultType(),
                        witnessSubstTy.getResult(),
                        reqtOrigTy.getFunctionResultType(),
                        reqtSubstTy.getResult(),
                        witnessFTy, thunkTy, args);
  }

  //   - the rest of the arguments
  forwardFunctionArguments(*this, loc, witnessFTy, witnessParams, args);

  // Perform the call.
  SILType witnessSILTy = SILType::getPrimitiveObjectType(witnessFTy);

  SILValue reqtResultValue;
  switch (coroutineKind) {
  case SILCoroutineKind::None: {
    SILValue witnessResultValue =
      emitApplyWithRethrow(loc, witnessFnRef, witnessSILTy, witnessSubs, args);

    // Reabstract the result value.
    reqtResultValue = resultPlanner->execute(witnessResultValue);
    break;
  }

  case SILCoroutineKind::YieldOnce: {
    SmallVector<SILValue, 4> witnessYields;
    auto token =
      emitBeginApplyWithRethrow(loc, witnessFnRef, witnessSILTy, witnessSubs,
                                args, witnessYields);

    // Push a cleanup to abort the inner coroutine.
    Cleanups.pushCleanup<EmitAbortApply>(token);
    auto abortCleanup = Cleanups.getTopCleanup();

    YieldInfo witnessYieldInfo(SGM, witness, witnessFTy, witnessSubs);
    YieldInfo reqtYieldInfo(SGM, requirement, thunkTy,
                            reqtSubs.subst(getForwardingSubstitutionMap()));

    translateYields(*this, loc, witnessYields, witnessYieldInfo, reqtYieldInfo);

    // Kill the abort cleanup without emitting it.
    Cleanups.setCleanupState(abortCleanup, CleanupState::Dead);

    // End the inner coroutine normally.
    emitEndApplyWithRethrow(loc, token);

    reqtResultValue = B.createTuple(loc, {});
    break;
  }

  case SILCoroutineKind::YieldMany:
    SGM.diagnose(loc, diag::unimplemented_generator_witnesses);
    reqtResultValue = B.createTuple(loc, {});
    break;
  }

  scope.pop();
  B.createReturn(loc, reqtResultValue);
}

//===----------------------------------------------------------------------===//
// Conversion to Canonical SILFunctionType Thunks
//===----------------------------------------------------------------------===//

static void translateParametersForCanonicalFunctionThunk(
    SILGenFunction &SGF, SILLocation loc,
    ArrayRef<ManagedValue> origParamValues,
    ArrayRef<SILParameterInfo> newParamInfos,
    SmallVectorImpl<ManagedValue> &newParams) {
  assert(origParamValues.size() == newParamInfos.size());

  for (auto T : llvm::zip(origParamValues, newParamInfos)) {
    ManagedValue origParam;
    SILParameterInfo newParamInfo;
    std::tie(origParam, newParamInfo) = T;

    if (origParam.getType().isTrivial(SGF.getModule())) {
      newParams.emplace_back(origParam);
      continue;
    }

    if (origParam.hasCleanup()) {
      // If we have a +1 value and the non-canonical function expects a
      // guaranteed parameter, borrow the parameter. Otherwise just pass off the
      // +1 value.
      if (newParamInfo.isGuaranteed()) {
        origParam = origParam.borrow(SGF, loc);
      }
      newParams.emplace_back(origParam);
      continue;
    }

    // Otherwise, if we have a +0 value and we want to pass it off as a +1
    // value, perform the copy.
    if (newParamInfo.isConsumed()) {
      origParam = origParam.copy(SGF, loc);
    }
    newParams.emplace_back(origParam);
  }
}

static void buildCanonicalFunctionThunkBody(SILGenFunction &SGF,
                                            SILLocation loc,
                                            CanSILFunctionType nonCanonicalTy,
                                            CanSILFunctionType canonicalTy) {
  SGF.F.setBare(IsBare);
  SGF.F.setThunk(IsThunk);

  FullExpr scope(SGF.Cleanups, CleanupLocation::get(loc));
  FormalEvaluationScope formalEvalScope(SGF);

  // Collect the thunk params, creating arguments for each parameter.
  SmallVector<ManagedValue, 8> origParams;
  SGF.collectThunkParams(loc, origParams);

  // Then translate our parameters into new params.
  SmallVector<ManagedValue, 8> newParams;
  translateParametersForCanonicalFunctionThunk(
      SGF, loc,
      // We drop the front so we don't process the thunked function here. We
      // handle that later.
      llvm::makeArrayRef(origParams).drop_back(1),
      nonCanonicalTy->getParameters(), newParams);

  // Then grab the function we are going to call from the last parameter.
  ManagedValue fn = origParams.back();

  // Collect the arguments.
  SmallVector<SILValue, 8> args;

  // Add all of the indirect results. We can just add the SILValues directly to
  // the args array since we do not need to perform any transformations upon
  // them because:
  //
  // 1. Reabstraction can not occur as a result of a canonical/non-canonical
  // mismatch.
  // 2. SILGenFunction::collectThunkParams(...) does not create cleanups when it
  // creates arguments.
  SILFunctionConventions fnConv(canonicalTy, SGF.SGM.M);
  transform(range(fnConv.getNumIndirectSILResults()), std::back_inserter(args),
            [&](unsigned Index) -> SILValue {
              return SGF.F.begin()->getArgument(Index);
            });

  // and then the rest of the arguments besides the first argument. Here we have
  // to forward the arguments since collectThunkParams /does/ create cleanups
  // for the parameters.
  forwardFunctionArguments(SGF, loc, nonCanonicalTy, newParams, args);

  // Perform the call.
  SILValue result =
      SGF.emitApplyWithRethrow(loc, fn.forward(SGF), fn.getType(), {}, args);

  formalEvalScope.pop();
  scope.pop();
  SGF.B.createReturn(loc, result);
}

ManagedValue
SILGenFunction::emitCanonicalFunctionThunk(SILLocation loc, ManagedValue fn,
                                           CanSILFunctionType nonCanonicalTy,
                                           CanSILFunctionType canonicalTy) {
  canonicalTy = canonicalTy->getWithRepresentation(
      SILFunctionType::Representation::Thick);

  SubstitutionMap contextSubs, interfaceSubs;
  GenericEnvironment *genericEnv = nullptr;

  // These two are not used here -- but really, bridging thunks
  // should be emitted using the formal AST type, not the lowered
  // type
  CanType inputSubstType;
  CanType outputSubstType;
  auto thunkTy = buildThunkType(nonCanonicalTy, canonicalTy, inputSubstType,
                                outputSubstType, genericEnv, interfaceSubs);
  auto thunk = SGM.getOrCreateReabstractionThunk(thunkTy, nonCanonicalTy,
                                                 canonicalTy, F.isSerialized());
  if (thunk->empty()) {
    thunk->setGenericEnvironment(genericEnv);
    SILGenFunction thunkSGF(SGM, *thunk, FunctionDC);
    auto loc = RegularLocation::getAutoGeneratedLocation();
    buildCanonicalFunctionThunkBody(thunkSGF, loc, nonCanonicalTy, canonicalTy);
  }

  CanSILFunctionType substFnTy = thunkTy;

  if (thunkTy->getGenericSignature()) {
    substFnTy = thunkTy->substGenericArgs(F.getModule(), interfaceSubs);
  }

  // Create it in the current function.
  auto thunkValue = B.createFunctionRef(loc, thunk);
  ManagedValue thunkedFn = B.createPartialApply(
      loc, thunkValue, SILType::getPrimitiveObjectType(substFnTy),
      interfaceSubs, {fn},
      SILType::getPrimitiveObjectType(canonicalTy));
  if (canonicalTy->isNoEscape()) {
    auto &funcTL = getTypeLowering(canonicalTy);
    thunkedFn =
        B.createConvertFunction(loc, thunkedFn, funcTL.getLoweredType());
  }
  return thunkedFn;
}
