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
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

#ifndef NDEBUG
/// This is meant to be used during AA bring up. If AA has been brought up, feel
/// free to remove this.
static llvm::cl::opt<bool>
DisableAliasAnalysis("disable-aa", llvm::cl::init(false),
                     llvm::cl::Hidden,
                     llvm::cl::desc("Always return most conservative AA "
                                    "result."));
#endif

//===----------------------------------------------------------------------===//
//                                 Utilities
//===----------------------------------------------------------------------===//

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
static SILValue getUnderlyingObject(SILValue V) {
  while (true) {
    SILValue V2 = V.stripCasts().stripAddressProjections().stripIndexingInsts();
    if (V2 == V)
      return V2;
    V = V2;
  }
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &OS,
                                     AliasAnalysis::AliasResult R) {
  switch (R) {
  case AliasAnalysis::AliasResult::NoAlias:
    return OS << "NoAlias";
  case AliasAnalysis::AliasResult::MayAlias:
    return OS << "MayAlias";
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
  auto *Arg = dyn_cast<SILArgument>(V.getDef());
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
  return isa<AllocationInst>(*V) || isNoAliasArgument(V) ||
    isa<LiteralInst>(*V);
}

/// Is this a literal which we know can not refer to a global object?
///
/// FIXME: function_ref?, builtin_function_ref?
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

/// Returns true if the ValueBase inside V is an apply whose callee is a no read
/// builtin_function_ref.
static bool isNoReadApplyInst(SILValue V) {
  auto *AI = dyn_cast<ApplyInst>(V.getDef());
  if (!AI)
    return false;

  auto *BI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee());

  return BI && isReadNone(BI);
}

namespace {

/// Are there any uses that should be ignored as capture uses.
///
/// TODO: Expand this if we ever do the store of pointer analysis mentioned in
/// Basic AA.
enum CaptureException : unsigned {
  None=0,
  ReturnsCannotCapture=1,
};

} // end anonymous namespace

/// Is Inst an instruction which escapes if and only if one of its results
/// escape?
static bool isTransitiveEscapeInst(SILInstruction *Inst) {
  switch (Inst->getKind()) {
  case ValueKind::AllocArrayInst:
  case ValueKind::AllocBoxInst:
  case ValueKind::AllocRefInst:
  case ValueKind::AllocStackInst:
  case ValueKind::ApplyInst:
  case ValueKind::ArchetypeMethodInst:
  case ValueKind::BuiltinFunctionRefInst:
  case ValueKind::CopyAddrInst:
  case ValueKind::DeallocBoxInst:
  case ValueKind::DeallocRefInst:
  case ValueKind::DeallocStackInst:
  case ValueKind::DebugValueAddrInst:
  case ValueKind::DebugValueInst:
  case ValueKind::DestroyAddrInst:
  case ValueKind::DestroyValueInst:
  case ValueKind::FloatLiteralInst:
  case ValueKind::FunctionRefInst:
  case ValueKind::GlobalAddrInst:
  case ValueKind::IntegerLiteralInst:
  case ValueKind::LoadInst:
  case ValueKind::LoadWeakInst:
  case ValueKind::MetatypeInst:
  case ValueKind::SILGlobalAddrInst:
  case ValueKind::StoreInst:
  case ValueKind::StoreWeakInst:
  case ValueKind::StringLiteralInst:
  case ValueKind::StrongReleaseInst:
  case ValueKind::StrongRetainAutoreleasedInst:
  case ValueKind::StrongRetainInst:
  case ValueKind::StrongRetainUnownedInst:
  case ValueKind::UnownedReleaseInst:
  case ValueKind::UnownedRetainInst:
  case ValueKind::InjectEnumAddrInst:
  case ValueKind::DeinitExistentialInst:
  case ValueKind::UnreachableInst:
  case ValueKind::IsNonnullInst:
  case ValueKind::CondFailInst:
  case ValueKind::DynamicMethodBranchInst:
  case ValueKind::ReturnInst:
  case ValueKind::AutoreleaseReturnInst:
  case ValueKind::UpcastExistentialInst:
    return false;

  case ValueKind::AddressToPointerInst:
  case ValueKind::ValueMetatypeInst:
  case ValueKind::BranchInst:
  case ValueKind::BridgeToBlockInst:
  case ValueKind::CheckedCastBranchInst:
  case ValueKind::ClassMethodInst:
  case ValueKind::CondBranchInst:
  case ValueKind::ConvertFunctionInst:
  case ValueKind::CopyValueInst:
  case ValueKind::DynamicMethodInst:
  case ValueKind::EnumInst:
  case ValueKind::IndexAddrInst:
  case ValueKind::IndexRawPointerInst:
  case ValueKind::InitEnumDataAddrInst:
  case ValueKind::InitExistentialInst:
  case ValueKind::InitExistentialRefInst:
  case ValueKind::ObjCToThickMetatypeInst:
  case ValueKind::ObjectPointerToRefInst:
  case ValueKind::OpenExistentialInst:
  case ValueKind::OpenExistentialRefInst:
  case ValueKind::PartialApplyInst:
  case ValueKind::PeerMethodInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::ProjectExistentialInst:
  case ValueKind::ProjectExistentialRefInst:
  case ValueKind::ProtocolMetatypeInst:
  case ValueKind::ProtocolMethodInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::RefElementAddrInst:
  case ValueKind::RefToObjectPointerInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RefToUnownedInst:
  case ValueKind::StructElementAddrInst:
  case ValueKind::StructExtractInst:
  case ValueKind::StructInst:
  case ValueKind::SuperMethodInst:
  case ValueKind::SwitchEnumAddrInst:
  case ValueKind::SwitchEnumInst:
  case ValueKind::SwitchIntInst:
  case ValueKind::TakeEnumDataAddrInst:
  case ValueKind::ThickToObjCMetatypeInst:
  case ValueKind::ThinToThickFunctionInst:
  case ValueKind::TupleElementAddrInst:
  case ValueKind::TupleExtractInst:
  case ValueKind::TupleInst:
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::UnownedToRefInst:
  case ValueKind::UpcastExistentialRefInst:
  case ValueKind::UpcastInst:
    return true;

  case ValueKind::AssignInst:
  case ValueKind::MarkFunctionEscapeInst:
  case ValueKind::MarkUninitializedInst:
    llvm_unreachable("Invalid in canonical SIL.");

  case ValueKind::SILArgument:
  case ValueKind::SILUndef:
    llvm_unreachable("These do not use other values.");
  }
}

/// Maximum amount of ValueCapture queries.
static unsigned const Threshold = 32;

/// Returns true if V is a value that is used in a manner such that we know its
/// captured or we don't understand whether or not it was captured. In such a
/// case to be conservative, we must assume it is captured.
static bool valueMayBeCaptured(SILValue V, CaptureException Exception) {
  llvm::SmallVector<Operand *, Threshold> Worklist;
  llvm::SmallPtrSet<Operand *, Threshold> Visited;
  unsigned Count = 0;

  DEBUG(llvm::dbgs() << "        Checking for capture.\n");
  

  // All all uses of V to the worklist.
  for (auto *UI : V.getUses()) {
    // If we have more uses than the threshold, be conservative and bail so we
    // don't use too much compile time.
    if (Count++ >= Threshold)
      return true;
    Visited.insert(UI);
    Worklist.push_back(UI);
  }

  // Until the worklist is empty...
  while (!Worklist.empty()) {
    // Pop off an operand and grab the operand's user...
    Operand *Op = Worklist.pop_back_val();
    SILInstruction *Inst = Op->getUser();

    DEBUG(llvm::dbgs() << "            Visiting: " << *Inst);

    // If Inst is an instruction with the transitive escape property, V escapes
    // if and only if the results of Inst escape as well.
    if (isTransitiveEscapeInst(Inst)) {
      DEBUG(llvm::dbgs() << "                Found transitive escape "
            "instruction!");
      for (auto *UI : Inst->getUses()) {
        // If we have more uses than the threshold, be conservative and bail
        // so we don't use too much compile time.
        if (Count++ >= Threshold)
          return true;

        if (Visited.insert(UI)) {
          Worklist.push_back(UI);
        }
      }
      continue;
    }

    // An apply of a builtin that does not read memory can not capture a value.
    //
    // TODO: Use analysis of the other function perhaps to see if it captures
    // memory in some manner?
    // TODO: Add in knowledge about how parameters work on swift to make this
    // more aggressive.
    if (isNoReadApplyInst(Inst))
      continue;

    // Loading from a pointer does not cause it to be captured.
    if (isa<LoadInst>(Inst))
      continue;

    // If we have a store and are storing into the pointer, this is not a
    // capture. Otherwise it is safe.
    if (auto *SI = dyn_cast<StoreInst>(Inst)) {
      if (SI->getDest() == Op->get()) {
        continue;
      } else {
        return true;
      }
    }

    // Deallocation instructions don't capture.
    if (isa<DeallocationInst>(Inst))
      continue;

    // Debug instructions don't capture.
    if (isa<DebugValueInst>(Inst) || isa<DebugValueAddrInst>(Inst))
      continue;

    // RefCountOperations don't capture.
    //
    // The release case is true since Swift does not allow destructors to
    // resurrent objects. This is enforced via a runtime failure.
    if (isa<RefCountingInst>(Inst))
      continue;

    // If we have a return instruction and we are assuming that returns don't
    // capture, we are safe.
    if (Exception == CaptureException::ReturnsCannotCapture &&
        (isa<ReturnInst>(Inst) || isa<AutoreleaseReturnInst>(Inst)))
      continue;

    // We could not prove that Inst does not capture V. Be conservative and
    // return true.
    return true;
  }

  // We successfully proved that V is not captured. Return false.
  return false;
}

/// Return true if the pointer is to a function-local object that never escapes
/// from the function.
static bool isNonEscapingLocalObject(SILValue V) {
  // If this is a local allocation, or the result of a no read apply inst (which
  // can not affect memory in the caller), check to see if the allocation
  // escapes.
  if (isa<AllocationInst>(*V) || isNoReadApplyInst(V))
    return !valueMayBeCaptured(V, CaptureException::ReturnsCannotCapture);

  // If this is a no alias argument then it has not escaped before entering the
  // function. Check if it escapes inside the function.
  if (isNoAliasArgument(V))
      return !valueMayBeCaptured(V, CaptureException::ReturnsCannotCapture);

  // Otherwise we could not prove that V is a non escaping local object. Be
  // conservative and return false.
  return false;
}

/// Returns true if V is a function argument that is not an address implying
/// that we do not have the gaurantee that it will not alias anything inside the
/// function.
static bool isAliasingFunctionArgument(SILValue V) {
  return isFunctionArgument(V) && !V.getType().isAddress();
}

/// Returns true if V is an apply inst that may read or write to memory.
static bool isReadWriteApplyInst(SILValue V) {
  return isa<ApplyInst>(*V) && !isNoReadApplyInst(V.getDef());
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
  if ((isFunctionArgument(O1.getDef()) && isIdentifiedFunctionLocal(O2)) ||
      (isFunctionArgument(O2.getDef()) && isIdentifiedFunctionLocal(O1))) {
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

/// Returns true if every projection in V1Path and V2Path equal. Returns false
/// otherwise.
static bool projectionListsEqual(llvm::SmallVectorImpl<Projection> &V1Path,
                                 llvm::SmallVectorImpl<Projection> &V2Path) {
  if (V1Path.size() != V2Path.size())
    return false;

  for (unsigned i = 0, e = V1Path.size(); i != e; ++i)
    if (V1Path[i] != V2Path[i])
      return false;

  return true;
}

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
  if (Projection::isAddressProjection(V2)) {
    // Do the base pointers alias?
    AliasAnalysis::AliasResult BaseAlias = AA.alias(O1, O2);

    // If we get a NoAlias or a MayAlias, then there is nothing we can do here
    // so just return the base alias value.
    if (BaseAlias != AliasAnalysis::AliasResult::MustAlias)
      return BaseAlias;

    // Otherwise, we have a MustAlias result. Since the base pointers alias each
    // other exactly, see if computing offsets from the common pointer tells us
    // about the relation of the resulting pointer.
    llvm::SmallVector<Projection, 4> V1Path, V2Path;
    bool Result = findAddressProjectionPathBetweenValues(O1, V1, V1Path);
    Result &= findAddressProjectionPathBetweenValues(O1, V2, V2Path);

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
    if (!Result)
      return AliasAnalysis::AliasResult::MayAlias;

    // If all of the projections are equal, the two GEPs must be the same.
    if (projectionListsEqual(V1Path, V2Path))
      return AliasAnalysis::AliasResult::MustAlias;
  }

  // We failed to prove anything. Be conservative and return MayAlias.
  return AliasAnalysis::AliasResult::MayAlias;
}

//===----------------------------------------------------------------------===//
//                                Entry Points
//===----------------------------------------------------------------------===//

/// The main AA entry point. Performs various analyses on V1, V2 in an attempt
/// to disambiguate the two values.
AliasAnalysis::AliasResult AliasAnalysis::alias(SILValue V1, SILValue V2) {
#ifndef NDEBUG
  // If alias analysis is disabled, always return may alias.
  if (DisableAliasAnalysis)
    return AliasResult::MayAlias;
#endif

  // If the two values equal, quickly return must alias.
  if (V1 == V2)
    return AliasResult::MustAlias;

  DEBUG(llvm::dbgs() << "ALIAS ANALYSIS:\n    V1: " << *V1.getDef()
        << "    V2: " << *V2.getDef());
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
  if (!Projection::isAddressProjection(V1) &&
      Projection::isAddressProjection(V2)) {
    std::swap(V1, V2);
    std::swap(O1, O2);
  }

  // If V1 is an address projection, attempt to use information from the
  // aggregate type tree to disambiguate it from V2.
  if (Projection::isAddressProjection(V1)) {
    AliasResult Result = aliasAddressProjection(*this, V1, V2, O1, O2);
    if (Result != AliasResult::MayAlias)
      return AliasCache[Key] = Result;
  }


  // We could not prove anything. Be conservative and return that V1, V2 may
  // alias.
  return AliasResult::MayAlias;
}

SILInstruction::MemoryBehavior
AliasAnalysis::getMemoryBehavior(SILInstruction *Inst, SILValue V) {
  DEBUG(llvm::dbgs() << "GET MEMORY BEHAVIOR FOR:\n    " << *Inst << "    "
        << *V.getDef());

  // If we already know that we do not read or write memory, just return None.
  if (!Inst->mayReadOrWriteMemory()) {
    DEBUG(llvm::dbgs() << "  Inst does not write memory. Returning None.\n");
    return MemoryBehavior::None;
  }

  switch (Inst->getKind()) {
  case ValueKind::LoadInst:
    // If the load address doesn't alias the given address, it doesn't read or
    // write the specified memory.
    if (isNoAlias(Inst->getOperand(0), V)) {
      DEBUG(llvm::dbgs() << "  Load does not alias inst. Returning None.\n");
      return MemoryBehavior::None;
    }

    // Otherwise be conservative and just return reads since loads can only
    // read.
    DEBUG(llvm::dbgs() << "  Could not prove load does not alias inst. "
          "Returning MayRead.\n");
    return MemoryBehavior::MayRead;
  case ValueKind::StoreInst:
    // If the store dest cannot alias the pointer in question, then the
    // specified value can not be modified by the store.
    if (isNoAlias(cast<StoreInst>(Inst)->getDest(), V)) {
      DEBUG(llvm::dbgs() << "  Store Dst does not alias inst. Returning "
            "None.\n");
      return MemoryBehavior::None;
    }

    // Otherwise, a store just writes.
    DEBUG(llvm::dbgs() << "  Could not prove store does not alias inst. "
          "Returning MayWrite.\n");
    return MemoryBehavior::MayWrite;
  case ValueKind::ApplyInst: {
    // If the ApplyInst is from a no-read builtin it can not read or write and
    // if it comes from a no-side effect builtin, it can only read.
    auto *AI = cast<ApplyInst>(Inst);
    auto *BFR = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee().getDef());

    // If our callee is not a builtin, be conservative and return may have side
    // effects.
    if (!BFR) {
      DEBUG(llvm::dbgs() << "  Found apply we don't understand returning "
            "MHSF.\n");
      return MemoryBehavior::MayHaveSideEffects;
    }

    // If the builtin is read none, it does not read or write memory.
    if (isReadNone(BFR)) {
      DEBUG(llvm::dbgs() << "  Found apply of read none builtin. Returning"
            " None.\n");
      return MemoryBehavior::None;
    }
    // If the builtin is side effect free, then it can only read memory.
    if (isSideEffectFree(BFR)) {
      DEBUG(llvm::dbgs() << "  Found apply of side effect free builtin. "
            "Returning MayRead.\n");
      return MemoryBehavior::MayRead;
    }

    // Otherwise be conservative and return that we may have side effects.
    DEBUG(llvm::dbgs() << "  Found apply of side effect builtin. "
          "Returning MayHaveSideEffects.\n");
    return MemoryBehavior::MayHaveSideEffects;
  }
  default:
    // If we do not have a special case, just return the generic memory
    // behavior of Inst.
    return Inst->getMemoryBehavior();
  }
}

SILAnalysis *swift::createAliasAnalysis(SILModule *M) {
  return new AliasAnalysis(M);
}
