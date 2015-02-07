//===-------------- AliasAnalysis.cpp - SIL Alias Analysis ----------------===//
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

#define DEBUG_TYPE "sil-aa"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

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
DebugAAKinds("aa", llvm::cl::desc("Alias Analysis Kinds:"),
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
                                         "all"),
                              clEnumValEnd));

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

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &OS,
                                     AliasAnalysis::AliasResult R) {
  switch (R) {
  case AliasAnalysis::AliasResult::NoAlias:
    return OS << "NoAlias";
  case AliasAnalysis::AliasResult::MayAlias:
    return OS << "MayAlias";
  case AliasAnalysis::AliasResult::PartialAlias:
    return OS << "PartialAlias";
  case AliasAnalysis::AliasResult::MustAlias:
    return OS << "MustAlias";
  }
}

//===----------------------------------------------------------------------===//
//                           Unequal Base Object AA
//===----------------------------------------------------------------------===//

/// Return true if the given SILArgument is an argument to the first BB of a
/// function.
static bool isFunctionArgument(SILValue V) {
  auto *Arg = dyn_cast<SILArgument>(V);
  if (!Arg)
    return false;
  return Arg->isFunctionArg();
}

/// A no alias argument is an argument that is an address type of the entry
/// basic block of a function.
static bool isNoAliasArgument(SILValue V) {
  return isFunctionArgument(V) && V.getType().isAddress();
}

/// Return true if V is an object that at compile time can be uniquely
/// identified.
static bool isIdentifiableObject(SILValue V) {
  if (isa<AllocationInst>(V) || isa<LiteralInst>(V))
    return true;
  if (isNoAliasArgument(V))
    return true;
  if (isa<GlobalAddrInst>(V))
    return true;
  return false;
}

/// Is this a literal which we know can not refer to a global object?
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
  return isa<AllocationInst>(*V) || isNoAliasArgument(V) || isLocalLiteral(V);
}

/// Returns true if V is a function argument that is not an address implying
/// that we do not have the gaurantee that it will not alias anything inside the
/// function.
static bool isAliasingFunctionArgument(SILValue V) {
  return isFunctionArgument(V) && !V.getType().isAddress();
}

/// Returns true if V is an apply inst that may read or write to memory.
static bool isReadWriteApplyInst(SILValue V) {
  // See if this is a normal function application.
  if (isa<ApplyInst>(V)) {
    // FIXME: Function attributes.
    return true;
  }
  
  // Next, see if this is a builtin.
  if (auto *BI = dyn_cast<BuiltinInst>(V)) {
    return !isReadNone(BI);
  }

  // If we fail, bail...
  return false;
}

/// Return true if the pointer is one which would have been considered an escape
/// by isNonEscapingLocalObject.
static bool isEscapeSource(SILValue V) {
  if (isReadWriteApplyInst(V))
    return true;

  if (isAliasingFunctionArgument(V))
    return true;

  // The LoadInst case works since valueMayBeCaptured always assumes stores are
  // escapes.
  if (isa<LoadInst>(*V))
    return true;

  // We could not prove anything, be conservative and return false.
  return false;
}

/// Returns true if we can prove that the two input SILValues which do not equal
/// can not alias.
static bool aliasUnequalObjects(SILValue O1, SILValue O2) {
  assert(O1 != O2 && "This function should only be called on unequal values.");

  // If O1 and O2 do not equal and they are both values that can be statically
  // and uniquely identified, they can not alias.
  if (isIdentifiableObject(O1) && isIdentifiableObject(O2)) {
    DEBUG(llvm::dbgs() << "            Found two unequal identified "
          "objects.\n");
    return true;
  }

  // Function arguments can't alias with things that are known to be
  // unambigously identified at the function level.
  //
  // Note that both function arguments must be identified. For example, an @in
  // argument may be an interior pointer into a box that is passed separately as
  // @owned. We must consider uses on the @in argument as potential uses of the
  // @owned object.
  if ((isFunctionArgument(O1) && isIdentifiedFunctionLocal(O2)) ||
      (isFunctionArgument(O2) && isIdentifiedFunctionLocal(O1))) {
    DEBUG(llvm::dbgs() << "            Found unequal function arg and "
          "identified function local!\n");
    return true;
  }

  // If one pointer is the result of an apply or load and the other is a
  // non-escaping local object within the same function, then we know the object
  // couldn't escape to a point where the call could return it.
  if ((isEscapeSource(O1) && isNonEscapingLocalObject(O2)) ||
      (isEscapeSource(O2) && isNonEscapingLocalObject(O1))) {
    DEBUG(llvm::dbgs() << "            Found unequal escape source and non "
          "escaping local object!\n");
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
static
AliasAnalysis::AliasResult
aliasAddressProjection(AliasAnalysis &AA, SILValue V1, SILValue V2, SILValue O1,
                       SILValue O2) {

  // If V2 is also a gep instruction with a must-alias or not-aliasing base
  // pointer, figure out if the indices of the GEPs tell us anything about the
  // derived pointers.
  if (Projection::isAddrProjection(V2)) {
    assert(!Projection::isAddrProjection(O1) &&
           "underlying object may not be a projection");
    assert(!Projection::isAddrProjection(O2) &&
           "underlying object may not be a projection");

    // Do the base pointers alias?
    AliasAnalysis::AliasResult BaseAlias = AA.alias(O1, O2);

    // If we get a NoAlias or a MayAlias, then there is nothing we can do here
    // so just return the base alias value.
    if (BaseAlias != AliasAnalysis::AliasResult::MustAlias)
      return BaseAlias;

    // Otherwise, we have a MustAlias result. Since the base pointers alias each
    // other exactly, see if computing offsets from the common pointer tells us
    // about the relation of the resulting pointer.
    auto V1Path = ProjectionPath::getAddrProjectionPath(O1, V1, true);
    auto V2Path = ProjectionPath::getAddrProjectionPath(O1, V2, true);

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
      return AliasAnalysis::AliasResult::MayAlias;

    auto R = V1Path->computeSubSeqRelation(*V2Path);

    // If all of the projections are equal, the two GEPs must be the same.
    if (R == SubSeqRelation_t::Equal)
      return AliasAnalysis::AliasResult::MustAlias;

    // The two GEPs do not alias if they are accessing different fields of
    // the same object, since different fields of the same object should not
    // overlap.
    //
    // TODO: Replace this with a check on the computed subseq relation. See the
    // TODO in computeSubSeqRelation.
    if (V1Path->hasNonEmptySymmetricDifference(V2Path.getValue()))
      return AliasAnalysis::AliasResult::NoAlias;

    // If one of the GEPs is a super path of the other then they partially
    // alias. W
    if (isStrictSubSeqRelation(R))
      return AliasAnalysis::AliasResult::PartialAlias;
  } else {
    // Ok, V2 is not an address projection. See if V2 after stripping casts
    // aliases O1. If so, then we know that V2 must partially alias V1 via a
    // must alias relation on O1. This ensures that given an alloc_stack and a
    // gep from that alloc_stack, we say that they partially alias.
    if (O1 == V2.stripCasts())
      return AliasAnalysis::AliasResult::PartialAlias;
  }

  // We failed to prove anything. Be conservative and return MayAlias.
  return AliasAnalysis::AliasResult::MayAlias;
}


//===----------------------------------------------------------------------===//
//                                TBAA
//===----------------------------------------------------------------------===//

static bool typedAccessTBAABuiltinTypesMayAlias(SILType LTy, SILType RTy,
                                                SILModule &Mod) {
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
  // object) which can not alias.
  //
  // 3. (Scalar, Scalar): If we have two scalar pointers, since we know that the
  // types are already not equal, we know that they can not alias. For those
  // unfamiliar even though BuiltinIntegerType/BuiltinFloatType are single
  // classes, the AST represents each integer/float of different bit widths as
  // different types, so equality of SILTypes allows us to know that they have
  // different bit widths.
  //
  // Thus we can just return false since in none of the aforementioned cases we
  // can not alias, so return false.
  return false;
}

/// \brief return True if the types \p LTy and \p RTy may alias.
///
/// Currently this only implements typed access based TBAA. See the TBAA section
/// in the SIL reference manual.
static bool typedAccessTBAAMayAlias(SILType LTy, SILType RTy, SILModule &Mod) {
#ifndef NDEBUG
  if (!shouldRunTypedAccessTBAA())
    return true;
#endif

  // If the two types are the same they may alias.
  if (LTy == RTy)
    return true;

  // Typed access based TBAA only occurs on pointers. If we reach this point and
  // do not have a pointer, be conservative and return that the two types may
  // alias. *NOTE* This ensures we return may alias for local_storage.
  if(!LTy.isAddress() || !RTy.isAddress())
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
  if (LTy.isAddressOnly(Mod) || RTy.isAddressOnly(Mod))
    return true;

  // If both types are builtin types, handle them separately.
  if (LTy.is<BuiltinType>() && RTy.is<BuiltinType>())
    return typedAccessTBAABuiltinTypesMayAlias(LTy, RTy, Mod);

  // Otherwise, we know that at least one of our types is not a builtin
  // type. If we have a builtin type, canonicalize it on the right.
  if (LTy.is<BuiltinType>())
    std::swap(LTy, RTy);

  // If RTy is a builtin raw pointer type, it can alias anything.
  if (RTy.is<BuiltinRawPointerType>())
    return true;

  ClassDecl *LTyClass = LTy.getClassOrBoundGenericClass();

  // The Builtin reference types can alias any class instance.
  if (RTy.is<BuiltinUnknownObjectType>() && LTyClass)
    return true;
  if (RTy.is<BuiltinNativeObjectType>() && LTyClass)
    return true;
  if (RTy.is<BuiltinBridgeObjectType>() && LTyClass)
    return true;
  
  // If one type is an aggregate and it contains the other type then the record
  // reference may alias the aggregate reference.
  if (LTy.aggregateContainsRecord(RTy, Mod) ||
      RTy.aggregateContainsRecord(LTy, Mod))
    return true;

  // FIXME: All the code following could be made significantly more aggressive
  // by saying that aggregates of the same type that do not contain each other
  // can not alias.

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
  if (!LTy.isSuperclassOf(RTy) && !RTy.isSuperclassOf(LTy))
    return false;

  // Otherwise be conservative and return that the two types may alias.
  return true;
}

static bool typesMayAlias(SILType T1, SILType T2, SILType TBAA1Ty,
                          SILType TBAA2Ty, SILModule &Mod) {
  // Perform type access based TBAA if we have TBAA info.
  if (TBAA1Ty && TBAA2Ty)
    return typedAccessTBAAMayAlias(TBAA1Ty, TBAA2Ty, Mod);

  // Otherwise perform class based TBAA on the passed in refs.
  //
  // FIXME: Implement class based TBAA.
  return true;
}

//===----------------------------------------------------------------------===//
//                                Entry Points
//===----------------------------------------------------------------------===//

/// The main AA entry point. Performs various analyses on V1, V2 in an attempt
/// to disambiguate the two values.
AliasAnalysis::AliasResult AliasAnalysis::alias(SILValue V1, SILValue V2,
                                                SILType TBAAType1,
                                                SILType TBAAType2) {
#ifndef NDEBUG
  // If alias analysis is disabled, always return may alias.
  if (!shouldRunAA())
    return AliasResult::MayAlias;
#endif

  // If the two values equal, quickly return must alias.
  if (V1 == V2)
    return AliasResult::MustAlias;

  DEBUG(llvm::dbgs() << "ALIAS ANALYSIS:\n    V1: " << *V1.getDef()
        << "    V2: " << *V2.getDef());

  // Pass in both the TBAA types so we can perform typed access TBAA and the
  // actual types of V1, V2 so we can perform class based TBAA.
  if (!typesMayAlias(V1.getType(), V2.getType(), TBAAType1, TBAAType2, *Mod))
    return AliasResult::NoAlias;

#ifndef NDEBUG
  if (!shouldRunBasicAA())
    return AliasResult::MayAlias;
#endif

  // Strip off any casts on V1, V2.
  V1 = V1.stripCasts();
  V2 = V2.stripCasts();
  DEBUG(llvm::dbgs() << "        After Cast Stripping V1:" << *V1.getDef());
  DEBUG(llvm::dbgs() << "        After Cast Stripping V2:" << *V2.getDef());

  // Create a key to lookup if we have already computed an alias result for V1,
  // V2. Canonicalize our cache keys so that the pointer with the lower address
  // is always the first element of the pair. This ensures we do not pollute our
  // cache with two entries with the same key, albeit with the key's fields
  // swapped.
  auto Key = V1 < V2? std::make_pair(V1, V2) : std::make_pair(V2, V1);

  // If we find our key in the cache, just return the alias result.
  auto Pair = AliasCache.find(Key);
  if (Pair != AliasCache.end()) {
    DEBUG(llvm::dbgs() << "      Found value in the cache: "
          << Pair->second << "\n");

    return Pair->second;
  }

  // Ok, we need to actually compute an Alias Analysis result for V1, V2. Begin
  // by finding the "base" of V1, V2 by stripping off all casts and GEPs.
  SILValue O1 = getUnderlyingObject(V1);
  SILValue O2 = getUnderlyingObject(V2);
  DEBUG(llvm::dbgs() << "        Underlying V1:" << *O1.getDef());
  DEBUG(llvm::dbgs() << "        Underlying V2:" << *O2.getDef());


  // If O1 and O2 do not equal, see if we can prove that they can not be the
  // same object. If we can, return No Alias.
  if (O1 != O2 && aliasUnequalObjects(O1, O2))
    return AliasCache[Key] = AliasResult::NoAlias;

  // Ok, either O1, O2 are the same or we could not prove anything based off of
  // their inequality. Now we climb up use-def chains and attempt to do tricks
  // based off of GEPs.

  // First if one instruction is a gep and the other is not, canonicalize our
  // inputs so that V1 always is the instruction containing the GEP.
  if (!Projection::isAddrProjection(V1) && Projection::isAddrProjection(V2)) {
    std::swap(V1, V2);
    std::swap(O1, O2);
  }

  // If V1 is an address projection, attempt to use information from the
  // aggregate type tree to disambiguate it from V2.
  if (Projection::isAddrProjection(V1)) {
    AliasResult Result = aliasAddressProjection(*this, V1, V2, O1, O2);
    if (Result != AliasResult::MayAlias)
      return AliasCache[Key] = Result;
  }


  // We could not prove anything. Be conservative and return that V1, V2 may
  // alias.
  return AliasResult::MayAlias;
}

namespace {

using MemBehavior = SILInstruction::MemoryBehavior;

/// Visitor that determines the memory behavior of an instruction relative to a
/// specific SILValue (i.e. can the instruction cause the value to be read,
/// etc.).
class MemoryBehaviorVisitor
    : public SILInstructionVisitor<MemoryBehaviorVisitor, MemBehavior> {

  /// The alias analysis for any queries we may need.
  AliasAnalysis &AA;

  /// The value we are attempting to discover memory behavior relative to.
  SILValue V;

  /// Should we treat instructions that increment ref counts as None instead of
  /// MayHaveSideEffects.
  bool IgnoreRefCountIncrements;

public:
  MemoryBehaviorVisitor(AliasAnalysis &AA, SILValue V, bool IgnoreRefCountIncs)
      : AA(AA), V(V), IgnoreRefCountIncrements(IgnoreRefCountIncs) {}

  MemBehavior visitValueBase(ValueBase *V) {
    llvm_unreachable("unimplemented");
  }

  MemBehavior visitSILInstruction(SILInstruction *Inst) {
    // If we do not have any more information, just use the general memory
    // behavior implementation.
    return Inst->getMemoryBehavior();
  }

  MemBehavior visitLoadInst(LoadInst *LI);
  MemBehavior visitStoreInst(StoreInst *SI);
  MemBehavior visitApplyInst(ApplyInst *AI);
  MemBehavior visitBuiltinInst(BuiltinInst *BI);

  // Instructions which are none if our SILValue does not alias one of its
  // arguments. If we can not prove such a thing, return the relevant memory
  // behavior.
#define OPERANDALIAS_MEMBEHAVIOR_INST(Name)                             \
  MemBehavior visit##Name(Name *I) {                                    \
    for (Operand &Op : I->getAllOperands()) {                           \
      if (!AA.isNoAlias(Op.get(), V)) {                                 \
        DEBUG(llvm::dbgs() << "  " #Name                                \
              " does alias inst. Returning  Normal behavior.\n");       \
        return I->getMemoryBehavior();                                  \
      }                                                                 \
    }                                                                   \
                                                                        \
    DEBUG(llvm::dbgs() << "  " #Name " does not alias inst. Returning " \
          "None.\n");                                                   \
    return MemBehavior::None;                                           \
  }

  OPERANDALIAS_MEMBEHAVIOR_INST(InjectEnumAddrInst)
  OPERANDALIAS_MEMBEHAVIOR_INST(UncheckedTakeEnumDataAddrInst)
  OPERANDALIAS_MEMBEHAVIOR_INST(InitExistentialInst)
  OPERANDALIAS_MEMBEHAVIOR_INST(DeinitExistentialInst)
  OPERANDALIAS_MEMBEHAVIOR_INST(DeallocStackInst)
  OPERANDALIAS_MEMBEHAVIOR_INST(FixLifetimeInst)
#undef OPERANDALIAS_MEMBEHAVIOR_INST

  // Override simple behaviors where MayHaveSideEffects is too general and
  // encompasses other behavior that is not read/write/ref count decrement
  // behavior we care about.
#define SIMPLE_MEMBEHAVIOR_INST(Name, Behavior)                         \
  MemBehavior visit##Name(Name *I) { return MemBehavior::Behavior; }
  SIMPLE_MEMBEHAVIOR_INST(CondFailInst, None)
#undef SIMPLE_MEMBEHAVIOR_INST

  // If we are asked to treat ref count increments as being inert, return None
  // for these.
  //
  // FIXME: Once we separate the notion of ref counts from reading/writing
  // memory this will be unnecessary.
#define REFCOUNTINC_MEMBEHAVIOR_INST(Name)                                     \
  MemBehavior visit##Name(Name *I) {                                           \
    if (IgnoreRefCountIncrements)                                              \
      return MemBehavior::None;                                                \
    return I->getMemoryBehavior();                                             \
  }
  REFCOUNTINC_MEMBEHAVIOR_INST(StrongRetainInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(StrongRetainAutoreleasedInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(StrongRetainUnownedInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(UnownedRetainInst)
  REFCOUNTINC_MEMBEHAVIOR_INST(RetainValueInst)
#undef REFCOUNTINC_MEMBEHAVIOR_INST

};

} // end anonymous namespace

/// Is this an instruction that can act as a type "oracle" allowing typed access
/// TBAA to know what the real types associated with the SILInstruction are.
static bool isTypedAccessOracle(SILInstruction *I) {
  switch (I->getKind()) {
  case ValueKind::RefElementAddrInst:
  case ValueKind::StructElementAddrInst:
  case ValueKind::TupleElementAddrInst:
  case ValueKind::UncheckedTakeEnumDataAddrInst:
  case ValueKind::LoadInst:
  case ValueKind::StoreInst:
  case ValueKind::AllocStackInst:
  case ValueKind::AllocBoxInst:
  case ValueKind::DeallocStackInst:
  case ValueKind::DeallocBoxInst:
    return true;
  default:
    return false;
  }
}

/// Look at the origin/user ValueBase of V to see if any of them are
/// TypedAccessOracle which enable one to ascertain via undefined behavior the
/// "true" type of the instruction.
SILType swift::findTypedAccessType(SILValue V) {
  // First look at the origin of V and see if we have any instruction that is a
  // typed oracle.
  if (auto *I = dyn_cast<SILInstruction>(V))
    if (isTypedAccessOracle(I))
      return V.getType();

  // Then look at any uses of V that potentially could act as a typed access
  // oracle.
  for (auto Use : V.getUses())
    if (isTypedAccessOracle(Use->getUser()))
      return V.getType();

  // Otherwise return an empty SILType
  return SILType();
}

MemBehavior MemoryBehaviorVisitor::visitLoadInst(LoadInst *LI) {
  if (AA.isNoAlias(LI->getOperand(), V, LI->getOperand().getType(),
                   findTypedAccessType(V))) {
    DEBUG(llvm::dbgs() << "  Load Operand does not alias inst. Returning "
                          "None.\n");
    return MemBehavior::None;
  }

  DEBUG(llvm::dbgs() << "  Could not prove that load inst does not alias "
        "pointer. Returning may read.");
  return MemBehavior::MayRead;
}

MemBehavior MemoryBehaviorVisitor::visitStoreInst(StoreInst *SI) {
  // If the store dest cannot alias the pointer in question, then the
  // specified value can not be modified by the store.
  if (AA.isNoAlias(SI->getDest(), V, SI->getDest().getType(),
                   findTypedAccessType(V))) {
    DEBUG(llvm::dbgs() << "  Store Dst does not alias inst. Returning "
                          "None.\n");
    return MemBehavior::None;
  }

  // Otherwise, a store just writes.
  DEBUG(llvm::dbgs() << "  Could not prove store does not alias inst. "
                        "Returning MayWrite.\n");
  return MemBehavior::MayWrite;
}
  
MemBehavior MemoryBehaviorVisitor::visitBuiltinInst(BuiltinInst *BI) {
  // If our callee is not a builtin, be conservative and return may have side
  // effects.
  if (!BI) {
    return MemBehavior::MayHaveSideEffects;
  }

  // If the builtin is read none, it does not read or write memory.
  if (isReadNone(BI)) {
    DEBUG(llvm::dbgs() << "  Found apply of read none builtin. Returning"
                          " None.\n");
    return MemBehavior::None;
  }

  // If the builtin is side effect free, then it can only read memory.
  if (isSideEffectFree(BI)) {
    DEBUG(llvm::dbgs() << "  Found apply of side effect free builtin. "
                          "Returning MayRead.\n");
    return MemBehavior::MayRead;
  }

  // FIXME: If the value (or any other values from the instruction that the
  // value comes from) that we are tracking does not escape and we don't alias
  // any of the arguments of the apply inst, we should be ok.

  // Otherwise be conservative and return that we may have side effects.
  DEBUG(llvm::dbgs() << "  Found apply of side effect builtin. "
                        "Returning MayHaveSideEffects.\n");
  return MemBehavior::MayHaveSideEffects;
}

MemBehavior MemoryBehaviorVisitor::visitApplyInst(ApplyInst *AI) {
    DEBUG(llvm::dbgs() << "  Found apply we don't understand returning "
                          "MHSF.\n");
  return MemBehavior::MayHaveSideEffects;
}

SILInstruction::MemoryBehavior
AliasAnalysis::getMemoryBehavior(SILInstruction *Inst, SILValue V,
                                 bool IgnoreRefCountIncrements) {
  DEBUG(llvm::dbgs() << "GET MEMORY BEHAVIOR FOR:\n    " << *Inst << "    "
        << *V.getDef());
  return MemoryBehaviorVisitor(*this, V, IgnoreRefCountIncrements).visit(Inst);
}

SILAnalysis *swift::createAliasAnalysis(SILModule *M) {
  return new AliasAnalysis(M);
}
