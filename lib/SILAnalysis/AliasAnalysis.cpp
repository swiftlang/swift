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
  if (isa<GlobalAddrInst>(V) || isa<SILGlobalAddrInst>(V))
    return true;
  return false;
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

/// Returns true if V is a function argument that is not an address implying
/// that we do not have the gaurantee that it will not alias anything inside the
/// function.
static bool isAliasingFunctionArgument(SILValue V) {
  return isFunctionArgument(V) && !V.getType().isAddress();
}

/// Returns true if V is an apply inst that may read or write to memory.
static bool isReadWriteApplyInst(SILValue V) {
  // Attempt to convert the ValueBase inside of V to an ApplyInst.
  auto *AI = dyn_cast<ApplyInst>(V);

  // If we fail, bail...
  if (!AI)
    return false;

  // If we succeed, check if AI's callee is a builtin that is not read none.
  auto *BI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee());
  return !BI || !isReadNone(BI);
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
//                                TBAA
//===----------------------------------------------------------------------===//

/// \brief return True if the aggregate type \p Aggregate contains the type
/// \p Record.
static bool aggregateContainsRecord(NominalTypeDecl *Aggregate, Type Record,
                                    SILModule &Mod) {
  CanType CanRecordType = Record->getCanonicalType();
  llvm::SmallVector<NominalTypeDecl *, 8> Worklist;
  Worklist.push_back(Aggregate);
  while (!Worklist.empty()) {
    NominalTypeDecl *T = Worklist.back();
    Worklist.pop_back();
    for (auto Var : T->getStoredProperties()) {
      // The record type could be generic. In here we find the the substituted
      // record type.
      Type RecTy =
          T->getType()->getTypeOfMember(Mod.getSwiftModule(), Var, nullptr);
      CanType CanRecTy = RecTy->getCanonicalType();

      // Is this the record we were looking for ?
      if (CanRecTy == CanRecordType)
        return true;

      // If the record is a nominal type add it to the worklist.
      if (auto D = CanRecTy->getNominalOrBoundGenericNominal()) {
        Worklist.push_back(D);
        continue;
      }

      // We don't handle unbound generic typed records.
      if (hasUnboundGenericTypes(CanRecTy))
        return true;
    }
  }

  // Could not find the record in the aggregate.
  return false;
}

/// \brief return True if the types \p T1 and \p T2 may alias.
/// See the TBAA section in the SIL reference manual.
static bool typesMayAlias(SILType T1, SILType T2, SILModule &Mod) {
  if (T1 == T2)
    return true;

  // We only operate on address types.
  if(!T1.isAddress() || !T2.isAddress())
    return true;

  CanType CT1 = T1.getSwiftRValueType();
  CanType CT2 = T2.getSwiftRValueType();

  bool IsObjPtr1 = isa<BuiltinNativeObjectType>(CT1);
  bool IsRawPtr1 = isa<BuiltinRawPointerType>(CT1);
  NominalTypeDecl *AsNominal1 = CT1.getNominalOrBoundGenericNominal();
  ClassDecl *AsClass1 = CT1.getClassOrBoundGenericClass();
  StructDecl *AsStruct1 = CT1.getStructOrBoundGenericStruct();
  EnumDecl *AsEnum1 = CT1.getEnumOrBoundGenericEnum();

  bool IsObjPtr2 = isa<BuiltinNativeObjectType>(CT2);
  bool IsRawPtr2 = isa<BuiltinRawPointerType>(CT2);
  NominalTypeDecl *AsNominal2 = CT2.getNominalOrBoundGenericNominal();
  ClassDecl *AsClass2 = CT2.getClassOrBoundGenericClass();
  StructDecl *AsStruct2 = CT2.getStructOrBoundGenericStruct();
  EnumDecl *AsEnum2 = CT2.getEnumOrBoundGenericEnum();

  // Raw pointers may alias anything.
  if (IsRawPtr1 || IsRawPtr2)
    return true;

  // If the types have unbound generic arguments then we don't know the possible
  // range of the type. A type such as $Array<Int> may alias $Array<T>.
  // Right now we are conservative and we assume that $UnsafePointer<T> and $Int
  // may alias.
  if (hasUnboundGenericTypes(CT1) || hasUnboundGenericTypes(CT2))
    return true;

  // Builtin.NativeObject is the root of the class hierarchy may alias classes.
  if ((IsObjPtr1 && AsClass2)||
      (IsObjPtr2 && AsClass1))
    return true;

  // If one type is an aggregate and it contains the other type then
  // the record reference may alias the aggregate reference.
  if ((AsNominal1 && aggregateContainsRecord(AsNominal1, CT2, Mod)) ||
      (AsNominal2 && aggregateContainsRecord(AsNominal2, CT1, Mod)))
    return true;

  // Structs don't alias non-structs.
  if (AsStruct1 || AsStruct2)
    return false;

  // Enums don't alias non-enums.
  if (AsEnum1 || AsEnum2)
    return false;

  // Classes don't alias non-classes. At the moment we don't follow the
  // class hierarchy so we can't tell if two classes inherit from one another.
  if ((AsClass1 && !AsClass2) ||
      (AsClass2 && !AsClass1))
    return false;

  // MayAlias.
  return true;
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

  if (!typesMayAlias(V1.getType(), V2.getType(), *Mod))
   return AliasResult::NoAlias;

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

namespace {

using MemBehavior = SILInstruction::MemoryBehavior;

class MemoryBehaviorVisitor
    : public SILInstructionVisitor<MemoryBehaviorVisitor, MemBehavior> {

  // The alias analysis for any queries we may need.
  AliasAnalysis &AA;

  // The value we are attempting to discover memory behavior relative to.
  SILValue V;

public:
  MemoryBehaviorVisitor(AliasAnalysis &AA, SILValue V) : AA(AA), V(V) {}

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
};

} // end anonymous namespace

MemBehavior MemoryBehaviorVisitor::visitLoadInst(LoadInst *LI) {
  // If the load address doesn't alias the given address, it doesn't read or
  // write the specified memory.
  if (AA.isNoAlias(LI->getOperand(), V)) {
    DEBUG(llvm::dbgs() << "  Load does not alias inst. Returning None.\n");
    return MemBehavior::None;
  }

  // Otherwise be conservative and just return reads since loads can only
  // read.
  DEBUG(llvm::dbgs() << "  Could not prove load does not alias inst. "
                        "Returning MayRead.\n");
  return MemBehavior::MayRead;
}

MemBehavior MemoryBehaviorVisitor::visitStoreInst(StoreInst *SI) {
  // If the store dest cannot alias the pointer in question, then the
  // specified value can not be modified by the store.
  if (AA.isNoAlias(SI->getDest(), V)) {
    DEBUG(llvm::dbgs() << "  Store Dst does not alias inst. Returning "
                          "None.\n");
    return MemBehavior::None;
  }

  // Otherwise, a store just writes.
  DEBUG(llvm::dbgs() << "  Could not prove store does not alias inst. "
                        "Returning MayWrite.\n");
  return MemBehavior::MayWrite;
}

MemBehavior MemoryBehaviorVisitor::visitApplyInst(ApplyInst *AI) {
  // If the ApplyInst is from a no-read builtin it can not read or write and
  // if it comes from a no-side effect builtin, it can only read.
  auto *BFR = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee());

  // If our callee is not a builtin, be conservative and return may have side
  // effects.
  if (!BFR) {
    DEBUG(llvm::dbgs() << "  Found apply we don't understand returning "
                          "MHSF.\n");
    return MemBehavior::MayHaveSideEffects;
  }

  // If the builtin is read none, it does not read or write memory.
  if (isReadNone(BFR)) {
    DEBUG(llvm::dbgs() << "  Found apply of read none builtin. Returning"
                          " None.\n");
    return MemBehavior::None;
  }
  // If the builtin is side effect free, then it can only read memory.
  if (isSideEffectFree(BFR)) {
    DEBUG(llvm::dbgs() << "  Found apply of side effect free builtin. "
                          "Returning MayRead.\n");
    return MemBehavior::MayRead;
  }

  // Otherwise be conservative and return that we may have side effects.
  DEBUG(llvm::dbgs() << "  Found apply of side effect builtin. "
                        "Returning MayHaveSideEffects.\n");
  return MemBehavior::MayHaveSideEffects;
}

SILInstruction::MemoryBehavior
AliasAnalysis::getMemoryBehavior(SILInstruction *Inst, SILValue V) {
  DEBUG(llvm::dbgs() << "GET MEMORY BEHAVIOR FOR:\n    " << *Inst << "    "
        << *V.getDef());
  return MemoryBehaviorVisitor(*this, V).visit(Inst);
}

SILAnalysis *swift::createAliasAnalysis(SILModule *M) {
  return new AliasAnalysis(M);
}
