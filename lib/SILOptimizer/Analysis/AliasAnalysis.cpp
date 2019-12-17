//===--- AliasAnalysis.cpp - SIL Alias Analysis ---------------------------===//
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

#define DEBUG_TYPE "sil-aa"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/EscapeAnalysis.h"
#include "swift/SILOptimizer/Analysis/SideEffectAnalysis.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;


// The AliasAnalysis Cache must not grow beyond this size.
// We limit the size of the AA cache to 2**14 because we want to limit the
// memory usage of this cache.
static const int AliasAnalysisMaxCacheSize = 16384;


//===----------------------------------------------------------------------===//
//                                AA Debugging
//===----------------------------------------------------------------------===//

#ifndef NDEBUG

namespace {

enum class AAKind : unsigned {
  None=0,
  BasicAA=1,
  TypedAccessTBAA=2,
  All=3,
};

} // end anonymous namespace

static llvm::cl::opt<AAKind>
DebugAAKinds("aa-kind", llvm::cl::desc("Alias Analysis Kinds:"),
             llvm::cl::init(AAKind::All),
             llvm::cl::values(clEnumValN(AAKind::None,
                                         "none",
                                         "Do not perform any AA"),
                              clEnumValN(AAKind::BasicAA,
                                         "basic-aa",
                                         "basic-aa"),
                              clEnumValN(AAKind::TypedAccessTBAA,
                                         "typed-access-tb-aa",
                                         "typed-access-tb-aa"),
                              clEnumValN(AAKind::All,
                                         "all",
                                         "all")));

static inline bool shouldRunAA() {
  return unsigned(AAKind(DebugAAKinds));
}

static inline bool shouldRunTypedAccessTBAA() {
  return unsigned(AAKind(DebugAAKinds)) & unsigned(AAKind::TypedAccessTBAA);
}

static inline bool shouldRunBasicAA() {
  return unsigned(AAKind(DebugAAKinds)) & unsigned(AAKind::BasicAA);
}

#endif

//===----------------------------------------------------------------------===//
//                                 Utilities
//===----------------------------------------------------------------------===//

using AliasResult = AliasAnalysis::AliasResult;

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &OS, AliasResult R) {
  switch (R) {
  case AliasResult::NoAlias:      return OS << "NoAlias";
  case AliasResult::MayAlias:     return OS << "MayAlias";
  case AliasResult::PartialAlias: return OS << "PartialAlias";
  case AliasResult::MustAlias:    return OS << "MustAlias";
  }

  llvm_unreachable("Unhandled AliasResult in switch.");
}

// Return the address of the directly accessed memory. If either the address is
// unknown, or any other memory is accessed via indirection, return an invalid
// SILValue.
SILValue getDirectlyAccessedMemory(SILInstruction *User) {
  if (auto *LI = dyn_cast<LoadInst>(User)) {
    return LI->getOperand();
  }

  if (auto *SI = dyn_cast<StoreInst>(User)) {
    return SI->getDest();
  }

  return SILValue();
}

//===----------------------------------------------------------------------===//
//                           Unequal Base Object AA
//===----------------------------------------------------------------------===//

/// Return true if the given SILArgument is an argument to the first BB of a
/// function.
static bool isFunctionArgument(SILValue V) {
  return isa<SILFunctionArgument>(V);
}

/// Return true if V is an object that at compile time can be uniquely
/// identified.
static bool isIdentifiableObject(SILValue V) {
  if (isa<AllocationInst>(V) || isa<LiteralInst>(V))
    return true;
  if (isExclusiveArgument(V))
    return true;
  return false;
}

/// Return true if V1 and V2 are distinct objects that can be uniquely
/// identified at compile time.
static bool areDistinctIdentifiableObjects(SILValue V1, SILValue V2) {
  // Do both values refer to the same global variable?
  if (auto *GA1 = dyn_cast<GlobalAddrInst>(V1)) {
    if (auto *GA2 = dyn_cast<GlobalAddrInst>(V2)) {
      return GA1->getReferencedGlobal() != GA2->getReferencedGlobal();
    }
  }
  if (isIdentifiableObject(V1) && isIdentifiableObject(V2))
    return V1 != V2;
  return false;
}

/// Returns true if both values are equal or yield the address of the same
/// global variable.
static bool isSameValueOrGlobal(SILValue V1, SILValue V2) {
  if (V1 == V2)
    return true;
  // Do both values refer to the same global variable?
  if (auto *GA1 = dyn_cast<GlobalAddrInst>(V1)) {
    if (auto *GA2 = dyn_cast<GlobalAddrInst>(V2)) {
      return GA1->getReferencedGlobal() == GA2->getReferencedGlobal();
    }
  }
  return false;
}

/// Is this a literal which we know cannot refer to a global object?
///
/// FIXME: function_ref?
static bool isLocalLiteral(SILValue V) {
  switch (V->getKind()) {
  case ValueKind::IntegerLiteralInst:
  case ValueKind::FloatLiteralInst:
  case ValueKind::StringLiteralInst:
    return true;
  default:
    return false;
  }
}

/// Is this a value that can be unambiguously identified as being defined at the
/// function level.
static bool isIdentifiedFunctionLocal(SILValue V) {
  return isa<AllocationInst>(*V) || isExclusiveArgument(V) || isLocalLiteral(V);
}

/// Returns true if we can prove that the two input SILValues which do not equal
/// cannot alias.
static bool aliasUnequalObjects(SILValue O1, SILValue O2) {
  assert(O1 != O2 && "This function should only be called on unequal values.");

  // If O1 and O2 do not equal and they are both values that can be statically
  // and uniquely identified, they cannot alias.
  if (areDistinctIdentifiableObjects(O1, O2)) {
    LLVM_DEBUG(llvm::dbgs() << "            Found two unequal identified "
               "objects.\n");
    return true;
  }

  // Function arguments can't alias with things that are known to be
  // unambiguously identified at the function level.
  //
  // Note that both function arguments must be identified. For example, an @in
  // argument may be an interior pointer into a box that is passed separately as
  // @owned. We must consider uses on the @in argument as potential uses of the
  // @owned object.
  if ((isFunctionArgument(O1) && isIdentifiedFunctionLocal(O2)) ||
      (isFunctionArgument(O2) && isIdentifiedFunctionLocal(O1))) {
    LLVM_DEBUG(llvm::dbgs() << "            Found unequal function arg and "
               "identified function local!\n");
    return true;
  }

  // We failed to prove that the two objects are different.
  return false;
}

//===----------------------------------------------------------------------===//
//                           Projection Address AA
//===----------------------------------------------------------------------===//

/// Uses a bunch of ad-hoc rules to disambiguate a GEP instruction against
/// another pointer. We know that V1 is a GEP, but we don't know anything about
/// V2. O1, O2 are getUnderlyingObject of V1, V2 respectively.
AliasResult AliasAnalysis::aliasAddressProjection(SILValue V1, SILValue V2,
                                                  SILValue O1, SILValue O2) {

  // If V2 is also a gep instruction with a must-alias or not-aliasing base
  // pointer, figure out if the indices of the GEPs tell us anything about the
  // derived pointers.
  if (!Projection::isAddressProjection(V2)) {
    // Ok, V2 is not an address projection. See if V2 after stripping casts
    // aliases O1. If so, then we know that V2 must partially alias V1 via a
    // must alias relation on O1. This ensures that given an alloc_stack and a
    // gep from that alloc_stack, we say that they partially alias.
    if (isSameValueOrGlobal(O1, stripCasts(V2)))
      return AliasResult::PartialAlias;

    return AliasResult::MayAlias;
  }
  
  assert(!Projection::isAddressProjection(O1) &&
         "underlying object may not be a projection");
  assert(!Projection::isAddressProjection(O2) &&
         "underlying object may not be a projection");

  // Do the base pointers alias?
  AliasResult BaseAlias = aliasInner(O1, O2);

  // If the underlying objects are not aliased, the projected values are also
  // not aliased.
  if (BaseAlias == AliasResult::NoAlias)
    return AliasResult::NoAlias;

  // Let's do alias checking based on projections.
  auto V1Path = ProjectionPath::getProjectionPath(O1, V1);
  auto V2Path = ProjectionPath::getProjectionPath(O2, V2);

  // getUnderlyingPath and findAddressProjectionPathBetweenValues disagree on
  // what the base pointer of the two values are. Be conservative and return
  // MayAlias.
  //
  // FIXME: The only way this should happen realistically is if there are
  // casts in between two projection instructions. getUnderlyingObject will
  // ignore that, while findAddressProjectionPathBetweenValues wont. The two
  // solutions are to make address projections variadic (something on the wee
  // horizon) or enable Projection to represent a cast as a special sort of
  // projection.
  if (!V1Path || !V2Path)
    return AliasResult::MayAlias;

  auto R = V1Path->computeSubSeqRelation(*V2Path);

  // If all of the projections are equal (and they have the same base pointer),
  // the two GEPs must be the same.
  if (BaseAlias == AliasResult::MustAlias &&
      R == SubSeqRelation_t::Equal)
    return AliasResult::MustAlias;

  // The two GEPs do not alias if they are accessing different fields, even if
  // we don't know the base pointers. Different fields should not overlap.
  //
  // TODO: Replace this with a check on the computed subseq relation. See the
  // TODO in computeSubSeqRelation.
  if (V1Path->hasNonEmptySymmetricDifference(V2Path.getValue()))
    return AliasResult::NoAlias;

  // If one of the GEPs is a super path of the other then they partially
  // alias.
  if (BaseAlias == AliasResult::MustAlias &&
      isStrictSubSeqRelation(R))
    return AliasResult::PartialAlias;

  // We failed to prove anything. Be conservative and return MayAlias.
  return AliasResult::MayAlias;
}


//===----------------------------------------------------------------------===//
//                                TBAA
//===----------------------------------------------------------------------===//

/// Is this an instruction that can act as a type "oracle" allowing typed access
/// TBAA to know what the real types associated with the SILInstruction are.
static bool isTypedAccessOracle(SILInstruction *I) {
  switch (I->getKind()) {
  case SILInstructionKind::RefElementAddrInst:
  case SILInstructionKind::RefTailAddrInst:
  case SILInstructionKind::StructElementAddrInst:
  case SILInstructionKind::TupleElementAddrInst:
  case SILInstructionKind::UncheckedTakeEnumDataAddrInst:
  case SILInstructionKind::LoadInst:
  case SILInstructionKind::StoreInst:
  case SILInstructionKind::AllocStackInst:
  case SILInstructionKind::AllocBoxInst:
  case SILInstructionKind::ProjectBoxInst:
  case SILInstructionKind::DeallocStackInst:
  case SILInstructionKind::DeallocBoxInst:
    return true;
  default:
    return false;
  }
}

/// Return true if the given value is an instruction or block argument that is
/// known to produce a nonaliasing address with respect to TBAA rules (i.e. the
/// pointer is not type punned). The only way to produce an aliasing typed
/// address is with pointer_to_address (via UnsafePointer) or
/// unchecked_addr_cast (via Builtin.reinterpretCast). Consequently, if the
/// given value is directly derived from a memory location, it cannot
/// alias. Call arguments also cannot alias because they must follow \@in, @out,
/// @inout, or \@in_guaranteed conventions.
static bool isAddressRootTBAASafe(SILValue V) {
  if (isa<SILFunctionArgument>(V))
    return true;

  if (auto *PtrToAddr = dyn_cast<PointerToAddressInst>(V))
    return PtrToAddr->isStrict();

  switch (V->getKind()) {
  default:
    return false;
  case ValueKind::AllocStackInst:
  case ValueKind::ProjectBoxInst:
  case ValueKind::RefElementAddrInst:
  case ValueKind::RefTailAddrInst:
    return true;
  }
}

/// Look at the origin/user ValueBase of V to see if any of them are
/// TypedAccessOracle which enable one to ascertain via undefined behavior the
/// "true" type of the instruction.
static SILType findTypedAccessType(SILValue V) {
  // First look at the origin of V and see if we have any instruction that is a
  // typed oracle.
  // TODO: MultiValueInstruction
  if (auto *I = dyn_cast<SingleValueInstruction>(V))
    if (isTypedAccessOracle(I))
      return V->getType();

  // Then look at any uses of V that potentially could act as a typed access
  // oracle.
  for (auto Use : V->getUses())
    if (isTypedAccessOracle(Use->getUser()))
      return V->getType();

  // Otherwise return an empty SILType
  return SILType();
}

SILType swift::computeTBAAType(SILValue V) {
  if (isAddressRootTBAASafe(getUnderlyingAddressRoot(V)))
    return findTypedAccessType(V);

  // FIXME: add ref_element_addr check here. TBAA says that objects cannot be
  // type punned.

  return SILType();
}

static bool typedAccessTBAABuiltinTypesMayAlias(SILType LTy, SILType RTy) {
  assert(LTy != RTy && "LTy should have already been shown to not equal RTy to "
         "call this function.");

  // If either of our types are raw pointers, they may alias any builtin.
  if (LTy.is<BuiltinRawPointerType>() || RTy.is<BuiltinRawPointerType>())
    return true;

  // At this point, we have 3 possibilities:
  //
  // 1. (Pointer, Scalar): A pointer to a pointer can never alias a scalar.
  //
  // 2. (Pointer, Pointer): If we have two pointers to pointers, since we know
  // that the two values do not equal due to previous AA calculations, one must
  // be a native object and the other is an unknown object type (i.e. an objc
  // object) which cannot alias.
  //
  // 3. (Scalar, Scalar): If we have two scalar pointers, since we know that the
  // types are already not equal, we know that they cannot alias. For those
  // unfamiliar even though BuiltinIntegerType/BuiltinFloatType are single
  // classes, the AST represents each integer/float of different bit widths as
  // different types, so equality of SILTypes allows us to know that they have
  // different bit widths.
  //
  // Thus we can just return false since in none of the aforementioned cases we
  // cannot alias, so return false.
  return false;
}

/// return True if the types \p LTy and \p RTy may alias.
///
/// Currently this only implements typed access based TBAA. See the TBAA section
/// in the SIL reference manual.
static bool typedAccessTBAAMayAlias(SILType LTy, SILType RTy,
                                    const SILFunction &F) {
#ifndef NDEBUG
  if (!shouldRunTypedAccessTBAA())
    return true;
#endif

  // If the two types are the same they may alias.
  if (LTy == RTy)
    return true;

  // Typed access based TBAA only occurs on pointers. If we reach this point and
  // do not have a pointer, be conservative and return that the two types may
  // alias.
  if (!LTy.isAddress() || !RTy.isAddress())
    return true;

  // If the types have unbound generic arguments then we don't know
  // the possible range of the type. A type such as $Array<Int> may
  // alias $Array<T>.  Right now we are conservative and we assume
  // that $UnsafeMutablePointer<T> and $Int may alias.
  if (LTy.hasArchetype() || RTy.hasArchetype())
    return true;

  // If either type is a protocol type, we don't know the underlying type so
  // return may alias.
  //
  // FIXME: We could be significantly smarter here by using the protocol
  // hierarchy.
  if (LTy.isAnyExistentialType() || RTy.isAnyExistentialType())
    return true;

  // If either type is an address only type, bail so we are conservative.
  if (LTy.isAddressOnly(F) || RTy.isAddressOnly(F))
    return true;

  // If both types are builtin types, handle them separately.
  if (LTy.is<BuiltinType>() && RTy.is<BuiltinType>())
    return typedAccessTBAABuiltinTypesMayAlias(LTy, RTy);

  // Otherwise, we know that at least one of our types is not a builtin
  // type. If we have a builtin type, canonicalize it on the right.
  if (LTy.is<BuiltinType>())
    std::swap(LTy, RTy);

  // If RTy is a builtin raw pointer type, it can alias anything.
  if (RTy.is<BuiltinRawPointerType>())
    return true;

  ClassDecl *LTyClass = LTy.getClassOrBoundGenericClass();

  // The Builtin reference types can alias any class instance.
  if (LTyClass) {
    if (RTy.is<BuiltinNativeObjectType>()  ||
        RTy.is<BuiltinBridgeObjectType>()) {
      return true;
    }
  }

  auto &Mod = F.getModule();

  // If one type is an aggregate and it contains the other type then the record
  // reference may alias the aggregate reference.
  if (LTy.aggregateContainsRecord(RTy, Mod, F.getTypeExpansionContext()) ||
      RTy.aggregateContainsRecord(LTy, Mod, F.getTypeExpansionContext()))
    return true;

  // FIXME: All the code following could be made significantly more aggressive
  // by saying that aggregates of the same type that do not contain each other
  // cannot alias.

  // Tuples do not alias non-tuples.
  bool LTyTT = LTy.is<TupleType>();
  bool RTyTT = RTy.is<TupleType>();
  if ((LTyTT && !RTyTT) || (!LTyTT && RTyTT))
    return false;

  // Structs do not alias non-structs.
  StructDecl *LTyStruct = LTy.getStructOrBoundGenericStruct();
  StructDecl *RTyStruct = RTy.getStructOrBoundGenericStruct();
  if ((LTyStruct && !RTyStruct) || (!LTyStruct && RTyStruct))
    return false;

  // Enums do not alias non-enums.
  EnumDecl *LTyEnum = LTy.getEnumOrBoundGenericEnum();
  EnumDecl *RTyEnum = RTy.getEnumOrBoundGenericEnum();
  if ((LTyEnum && !RTyEnum) || (!LTyEnum && RTyEnum))
    return false;

  // Classes do not alias non-classes.
  ClassDecl *RTyClass = RTy.getClassOrBoundGenericClass();
  if ((LTyClass && !RTyClass) || (!LTyClass && RTyClass))
    return false;

  // Classes with separate class hierarchies do not alias.
  if (!LTy.isBindableToSuperclassOf(RTy) && !RTy.isBindableToSuperclassOf(LTy))
    return false;

  // Otherwise be conservative and return that the two types may alias.
  return true;
}

bool AliasAnalysis::typesMayAlias(SILType T1, SILType T2,
                                  const SILFunction &F) {
  // Both types need to be valid.
  if (!T2 || !T1)
    return true;

  // Check if we've already computed the TBAA relation.
  auto Key = std::make_pair(T1, T2);
  auto Res = TypesMayAliasCache.find(Key);
  if (Res != TypesMayAliasCache.end()) {
    return Res->second;
  }

  bool MA = typedAccessTBAAMayAlias(T1, T2, F);
  TypesMayAliasCache[Key] = MA;
  return MA;
}

//===----------------------------------------------------------------------===//
//                                Entry Points
//===----------------------------------------------------------------------===//

/// The main AA entry point. Performs various analyses on V1, V2 in an attempt
/// to disambiguate the two values.
AliasResult AliasAnalysis::alias(SILValue V1, SILValue V2,
                                 SILType TBAAType1, SILType TBAAType2) {
  AliasKeyTy Key = toAliasKey(V1, V2, TBAAType1, TBAAType2);

  // Check if we've already computed this result.
  auto It = AliasCache.find(Key);
  if (It != AliasCache.end()) {
    return It->second;
  }

  // Flush the cache if the size of the cache is too large.
  if (AliasCache.size() > AliasAnalysisMaxCacheSize) {
    AliasCache.clear();
    AliasValueBaseToIndex.clear();

    // Key is no longer valid as we cleared the AliasValueBaseToIndex.
    Key = toAliasKey(V1, V2, TBAAType1, TBAAType2);
  }

  // Calculate the aliasing result and store it in the cache.
  auto Result = aliasInner(V1, V2, TBAAType1, TBAAType2);
  AliasCache[Key] = Result;
  return Result;
}

/// The main AA entry point. Performs various analyses on V1, V2 in an attempt
/// to disambiguate the two values.
AliasResult AliasAnalysis::aliasInner(SILValue V1, SILValue V2,
                                      SILType TBAAType1,
                                      SILType TBAAType2) {
#ifndef NDEBUG
  // If alias analysis is disabled, always return may alias.
  if (!shouldRunAA())
    return AliasResult::MayAlias;
#endif

  // If the two values equal, quickly return must alias.
  if (isSameValueOrGlobal(V1, V2))
    return AliasResult::MustAlias;

  LLVM_DEBUG(llvm::dbgs() << "ALIAS ANALYSIS:\n    V1: " << *V1
             << "    V2: " << *V2);

  // If this is SILUndef, return may alias.
  if (!V1->getFunction())
    return AliasResult::MayAlias;

  // Pass in both the TBAA types so we can perform typed access TBAA and the
  // actual types of V1, V2 so we can perform class based TBAA.
  if (!typesMayAlias(TBAAType1, TBAAType2, *V1->getFunction()))
    return AliasResult::NoAlias;

#ifndef NDEBUG
  if (!shouldRunBasicAA())
    return AliasResult::MayAlias;
#endif

  // Strip off any casts on V1, V2.
  V1 = stripCasts(V1);
  V2 = stripCasts(V2);
  LLVM_DEBUG(llvm::dbgs() << "        After Cast Stripping V1:" << *V1);
  LLVM_DEBUG(llvm::dbgs() << "        After Cast Stripping V2:" << *V2);

  // Ok, we need to actually compute an Alias Analysis result for V1, V2. Begin
  // by finding the "base" of V1, V2 by stripping off all casts and GEPs.
  SILValue O1 = getUnderlyingObject(V1);
  SILValue O2 = getUnderlyingObject(V2);
  LLVM_DEBUG(llvm::dbgs() << "        Underlying V1:" << *O1);
  LLVM_DEBUG(llvm::dbgs() << "        Underlying V2:" << *O2);

  // If O1 and O2 do not equal, see if we can prove that they cannot be the
  // same object. If we can, return No Alias.
  if (O1 != O2 && aliasUnequalObjects(O1, O2))
    return AliasResult::NoAlias;

  // Ok, either O1, O2 are the same or we could not prove anything based off of
  // their inequality.
  // Next: ask escape analysis. This catches cases where we compare e.g. a
  // non-escaping pointer with another (maybe escaping) pointer. Escape analysis
  // uses the connection graph to check if the pointers may point to the same
  // content.
  //
  // canPointToSameMemory must take the original pointers used for memory
  // access, not the underlying object, because objects projections can be
  // modeled by escape analysis as different content, and canPointToSameMemory
  // assumes that only the pointer itself may be accessed here, not any other
  // address that can be derived from this pointer.
  if (!EA->canPointToSameMemory(V1, V2)) {
    LLVM_DEBUG(llvm::dbgs() << "            Found not-aliased objects based on "
                               "escape analysis\n");
    return AliasResult::NoAlias;
  }

  // Now we climb up use-def chains and attempt to do tricks based off of GEPs.

  // First if one instruction is a gep and the other is not, canonicalize our
  // inputs so that V1 always is the instruction containing the GEP.
  if (!Projection::isAddressProjection(V1) &&
       Projection::isAddressProjection(V2)) {
    std::swap(V1, V2);
    std::swap(O1, O2);
  }

  // If V1 is an address projection, attempt to use information from the
  // aggregate type tree to disambiguate it from V2.
  if (Projection::isAddressProjection(V1)) {
    AliasResult Result = aliasAddressProjection(V1, V2, O1, O2);
    if (Result != AliasResult::MayAlias)
      return Result;
  }

  // We could not prove anything. Be conservative and return that V1, V2 may
  // alias.
  return AliasResult::MayAlias;
}

bool AliasAnalysis::canApplyDecrementRefCount(FullApplySite FAS, SILValue Ptr) {
  // Treat applications of no-return functions as decrementing ref counts. This
  // causes the apply to become a sink barrier for ref count increments.
  if (FAS.isCalleeNoReturn())
    return true;

  /// If the pointer cannot escape to the function we are done.
  if (!EA->canEscapeTo(Ptr, FAS))
    return false;

  FunctionSideEffects ApplyEffects;
  SEA->getCalleeEffects(ApplyEffects, FAS);

  auto &GlobalEffects = ApplyEffects.getGlobalEffects();
  if (ApplyEffects.mayReadRC() || GlobalEffects.mayRelease())
    return true;

  /// The function has no unidentified releases, so let's look at the arguments
  // in detail.
  for (unsigned Idx = 0, End = FAS.getNumArguments(); Idx < End; ++Idx) {
    auto &ArgEffect = ApplyEffects.getParameterEffects()[Idx];
    if (ArgEffect.mayRelease()) {
      // The function may release this argument, so check if the pointer can
      // escape to it.
      if (EA->canEscapeToValue(Ptr, FAS.getArgument(Idx)))
        return true;
    }
  }
  return false;
}

bool AliasAnalysis::canBuiltinDecrementRefCount(BuiltinInst *BI, SILValue Ptr) {
  for (SILValue Arg : BI->getArguments()) {

    // Exclude some types of arguments where Ptr can never escape to.
    if (isa<MetatypeInst>(Arg))
      continue;
    if (Arg->getType().is<BuiltinIntegerType>())
      continue;

    // A builtin can only release an object if it can escape to one of the
    // builtin's arguments.
    if (EA->canEscapeToValue(Ptr, Arg))
      return true;
  }
  return false;
}


bool AliasAnalysis::mayValueReleaseInterfereWithInstruction(SILInstruction *User,
                                                            SILValue Ptr) {
  // TODO: Its important to make this as precise as possible.
  //
  // TODO: Eventually we can plug in some analysis on the what the release of
  // the Ptr can do, i.e. be more precise about Ptr's deinit.
  //
  // TODO: If we know the specific release instruction, we can potentially do
  // more.
  //
  // If this instruction can not read or write any memory. Its OK.
  if (!User->mayReadOrWriteMemory())
    return false;

  // These instructions do read or write memory, get memory directly
  // accessed. 'V' must be the only memory accessed by User, and it must be
  // directly accessed. Any memory indirectly accessed via 'User' may have
  // escaped.
  SILValue V = getDirectlyAccessedMemory(User);
  if (!V)
    return true;

  // If the 'User' instruction's memory is uniquely identified and does not
  // escape in the local scope, then it can't be accessed by a deinit in the
  // local scope. Note that an exclusive argument's content may have escaped in
  // the caller, but the argument value itself can't be accessed via aliasing
  // references and we know that User doesn't see through any indirection.
  if (!isUniquelyIdentified(V))
    return true;

  // This is a scoped allocation.
  // The most important check: does the object escape the current function?
  auto LO = getUnderlyingObject(V);
  auto *ConGraph = EA->getConnectionGraph(User->getFunction());
  auto *Content = ConGraph->getValueContent(LO);
  if (Content && !Content->escapes())
    return false;

  // This is either a non-local allocation or a scoped allocation that escapes.
  // We failed to prove anything, it could be read or written by the deinit.
  return true;
}

bool swift::isLetPointer(SILValue V) {
  // Traverse the "access" path for V and check that it starts with "let"
  // and everything along this path is a value-type (i.e. struct or tuple).

  // Is this an address of a "let" class member?
  if (auto *REA = dyn_cast<RefElementAddrInst>(V))
    return REA->getField()->isLet();

  // Is this an address of a global "let"?
  if (auto *GAI = dyn_cast<GlobalAddrInst>(V)) {
    auto *GlobalDecl = GAI->getReferencedGlobal()->getDecl();
    return GlobalDecl && GlobalDecl->isLet();
  }

  // Is this an address of a struct "let" member?
  if (auto *SEA = dyn_cast<StructElementAddrInst>(V))
    // Check if it is a "let" in the parent struct.
    // Check if its parent is a "let".
    return isLetPointer(SEA->getOperand());


  // Check if a parent of a tuple is a "let"
  if (auto *TEA = dyn_cast<TupleElementAddrInst>(V))
    return isLetPointer(TEA->getOperand());

  return false;
}

void AliasAnalysis::initialize(SILPassManager *PM) {
  SEA = PM->getAnalysis<SideEffectAnalysis>();
  EA = PM->getAnalysis<EscapeAnalysis>();
}

SILAnalysis *swift::createAliasAnalysis(SILModule *M) {
  return new AliasAnalysis(M);
}

AliasKeyTy AliasAnalysis::toAliasKey(SILValue V1, SILValue V2,
                                     SILType Type1, SILType Type2) {
  size_t idx1 = AliasValueBaseToIndex.getIndex(V1);
  assert(idx1 != std::numeric_limits<size_t>::max() &&
         "~0 index reserved for empty/tombstone keys");
  size_t idx2 = AliasValueBaseToIndex.getIndex(V2);
  assert(idx2 != std::numeric_limits<size_t>::max() &&
         "~0 index reserved for empty/tombstone keys");
  void *t1 = Type1.getOpaqueValue();
  void *t2 = Type2.getOpaqueValue();
  return {idx1, idx2, t1, t2};
}
