//===--- ArrayElementValuePropagation.cpp - Propagate values of arrays ----===//
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
#define DEBUG_TYPE "array-element-propagation"

#include "llvm/ADT/SetVector.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

/// Propagate the elements of array values to calls of the array's get_element
/// method.
///
/// Array literal construction and array initialization of array values
/// associates element values with the array value. These values can be
/// propagated to the get_element method if we can prove that the array value
/// has not changed until reading the array value's element.
///
/// Propagation of the elements of one array allocation.
///
/// We propagate the elements associated with calls of
///
/// * Array.init(count:repeatedValue:)
///   The 'repeatedValue'.
///   TODO: this is not yet implemented.
///
/// * Array._adoptStorage(storage:count:)
///   The stores on the returned array element buffer pointer.
///
namespace {
class ArrayAllocation {
  /// The array allocation call.
  ApplyInst *Alloc;
  /// The array value returned by the allocation call.
  SILValue ArrayValue;

  /// The pointer to the returned array element buffer pointer.
  SILValue ElementBuffer;

  // The calls to Array get_element that use this array allocation.
  llvm::SmallSetVector<ApplyInst *, 16> GetElementCalls;
  llvm::DenseMap<uint64_t, SILValue> ElementValueMap;

  // Array get_element calls and their matching array element value for later
  // replacement.
  llvm::SmallVectorImpl<std::pair<ApplyInst *, SILValue>> &ReplacementMap;

  ArrayAllocation(
      ApplyInst *AI,
      llvm::SmallVectorImpl<std::pair<ApplyInst *, SILValue>> &Replacements)
      : Alloc(AI), ReplacementMap(Replacements) {}

  bool findValueReplacements();
  bool isInitializationWithKnownElements();
  bool mapInitializationStores();
  bool analyzeArrayValueUses();
  bool recursivelyCollectUses(ValueBase *Def);
  bool collectForwardableValues();

public:

  /// Find a set of get_element calls that can be replace by the initialization
  /// value of the array allocation call.
  ///
  /// Returns true if an access can be replaced. The replacements are stored in
  /// the \p ReplacementMap.
  static bool findValueReplacements(
      ApplyInst *Inst,
      llvm::SmallVectorImpl<std::pair<ApplyInst *, SILValue>> &Replacements) {
    return ArrayAllocation(Inst, Replacements).findValueReplacements();
  }
};
} // end anonymous namespace


/// Map the indices of array element initialization stores to their values.
bool ArrayAllocation::mapInitializationStores() {
  assert(ElementBuffer &&
         "Must have identified an array element storage pointer");

  // Match initialization stores.
  // %83 = struct_extract %element_buffer : $UnsafeMutablePointer<Int>
  // %84 = pointer_to_address %83 : $Builtin.RawPointer to strict $*Int
  // store %85 to %84 : $*Int
  // %87 = integer_literal $Builtin.Word, 1
  // %88 = index_addr %84 : $*Int, %87 : $Builtin.Word
  // store %some_value to %88 : $*Int

  auto *UnsafeMutablePointerExtract =
      dyn_cast_or_null<StructExtractInst>(getSingleNonDebugUser(ElementBuffer));
  if (!UnsafeMutablePointerExtract)
    return false;
  auto *PointerToAddress = dyn_cast_or_null<PointerToAddressInst>(
      getSingleNonDebugUser(UnsafeMutablePointerExtract));
  if (!PointerToAddress)
    return false;

  // Match the stores. We can have either a store directly to the address or
  // to an index_addr projection.
  for (auto *Op : PointerToAddress->getUses()) {
    auto *Inst = Op->getUser();

    // Store to the base.
    auto *SI = dyn_cast<StoreInst>(Inst);
    if (SI && SI->getDest() == PointerToAddress) {
      // We have already seen an entry for this index bail.
      if (ElementValueMap.count(0))
        return false;
      ElementValueMap[0] = SI->getSrc();
      continue;
    } else if (SI)
      return false;

    // Store an index_addr projection.
    auto *IndexAddr = dyn_cast<IndexAddrInst>(Inst);
    if (!IndexAddr)
      return false;
    SI = dyn_cast_or_null<StoreInst>(getSingleNonDebugUser(IndexAddr));
    if (!SI || SI->getDest() != IndexAddr)
      return false;
    auto *Index = dyn_cast<IntegerLiteralInst>(IndexAddr->getIndex());
    if (!Index)
      return false;
    auto IndexVal = Index->getValue();
    // Let's not blow up our map.
    if (IndexVal.getActiveBits() > 16)
      return false;
    // Already saw an entry.
    if (ElementValueMap.count(IndexVal.getZExtValue()))
      return false;

    ElementValueMap[IndexVal.getZExtValue()] = SI->getSrc();
  }
  return !ElementValueMap.empty();
}

/// Check that we have an array initialization call with known elements.
///
/// The returned array value is known not to be aliased since it was just
/// allocated.
bool ArrayAllocation::isInitializationWithKnownElements() {
  ArraySemanticsCall Uninitialized(Alloc, "array.uninitialized");
  if (Uninitialized &&
      (ArrayValue = Uninitialized.getArrayValue()) &&
      (ElementBuffer = Uninitialized.getArrayElementStoragePointer()))
    return mapInitializationStores();

  return false;
}

/// Propagate the elements of an array literal to get_element method calls on
/// the same array.
///
/// We have to prove that the array value is not changed in between the
/// creation and the method call to get_element.
bool ArrayAllocation::findValueReplacements() {
  if (!isInitializationWithKnownElements())
    return false;

  // The array value was stored or has escaped.
  if (!analyzeArrayValueUses())
    return false;

  // No count users.
  if (GetElementCalls.empty())
    return false;

  return collectForwardableValues();
}

/// Collect all get_element users and check that there are no escapes or uses
/// that could change the array value.
bool ArrayAllocation::analyzeArrayValueUses() {
  return recursivelyCollectUses(ArrayValue);
}

/// Recursively look at all uses of this definition. Abort if the array value
/// could escape or be changed. Collect all uses that are calls to array.count.
bool ArrayAllocation::recursivelyCollectUses(ValueBase *Def) {
  for (auto *Opd : Def->getUses()) {
    auto *User = Opd->getUser();
    // Ignore reference counting and debug instructions.
    if (isa<RefCountingInst>(User) || isa<DebugValueInst>(User))
      continue;

    // Array value projection.
    if (auto *SEI = dyn_cast<StructExtractInst>(User)) {
      if (!recursivelyCollectUses(SEI))
        return false;
      continue;
    }

    // Check array semantic calls.
    ArraySemanticsCall ArrayOp(User);
    if (ArrayOp && ArrayOp.doesNotChangeArray()) {
      if (ArrayOp.getKind() == ArrayCallKind::kGetElement)
        GetElementCalls.insert(ArrayOp);
      continue;
    }

    // An operation that escapes or modifies the array value.
    return false;
  }
  return true;
}

/// Look at the get_element calls and match them to values by index.
bool ArrayAllocation::collectForwardableValues() {
  bool FoundForwardableValue = false;
  for (auto *GetElementCall : GetElementCalls) {
    ArraySemanticsCall GetElement(GetElementCall);
    assert(GetElement.getKind() == ArrayCallKind::kGetElement);

    auto ConstantIndex = GetElement.getConstantIndex();
    if (ConstantIndex == None)
      continue;

    assert(*ConstantIndex >= 0 && "Must have a positive index");

    auto EltValueIt = ElementValueMap.find(*ConstantIndex);
    if (EltValueIt == ElementValueMap.end())
      continue;

    ReplacementMap.push_back(
        std::make_pair(GetElementCall, EltValueIt->second));
    FoundForwardableValue = true;
  }
  return FoundForwardableValue;
}

// =============================================================================
//                                 Driver
// =============================================================================

namespace {

class ArrayElementPropagation : public SILFunctionTransform {
public:
  ArrayElementPropagation() {}

  StringRef getName() override {
    return "Array Element Propagation";
  }

  void run() override {
    auto &Fn = *getFunction();

    bool Changed = false;

    // Propagate the elements an of array value to its users.
    SmallVector<std::pair<ApplyInst *, SILValue>, 16> ValueReplacements;
    for (auto &BB :Fn) {
      for (auto &Inst : BB) {
        if (auto *Apply = dyn_cast<ApplyInst>(&Inst))
          Changed |=
              ArrayAllocation::findValueReplacements(Apply, ValueReplacements);
      }
    }
    DEBUG(if (Changed) {
      llvm::dbgs() << "Array elements replaced in " << Fn.getName() << " ("
                   << ValueReplacements.size() << ")\n";
    });
    // Perform the actual replacement of the get_element call by its value.
    for (auto &Repl : ValueReplacements) {
      ArraySemanticsCall GetElement(Repl.first);
      GetElement.replaceByValue(Repl.second);
    }

    if (Changed) {
      PM->invalidateAnalysis(
          &Fn, SILAnalysis::InvalidationKind::CallsAndInstructions);
    }
  }
};
} // end anonymous namespace

SILTransform *swift::createArrayElementPropagation() {
  return new ArrayElementPropagation();
}
