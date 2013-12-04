//===--- DIMemoryUseCollector.h - Memory use information for DI -*- C++ -*-===//
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
// This file declares logic used by definitive analysis related passes that look
// at all the instructions that access a memory object.  This is quite specific
// to definitive analysis in that it is tuple element sensitive instead of
// relying on SROA.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILPASSES_DI_MEMORY_USE_COLLECTOR_H
#define SWIFT_SILPASSES_DI_MEMORY_USE_COLLECTOR_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/APInt.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILType.h"

namespace swift {
  class SILBuilder;
  
/// This is a collection of utilities for reasoning about (potentially
/// recursively) flattened tuples, and computing access paths and indexes into
/// the flattened namespace.
///
/// The flattened namespace is assigned lexicographically.  For example, in:
///   (Int, ((Float, (), Double)))
/// the Int member is numbered 0, the Float is numbered 1, and the Double is
/// numbered 2.  Empty tuples don't get numbered since they contain no state.
namespace TF {
  /// getElementCount - Return the number of elements in the flattened type.
  /// For tuples, this is the (recursive) count of the fields it contains,
  /// otherwise this is 1.
  unsigned getElementCount(CanType T);
  
  /// emitElementAddress - Given a tuple element number (in the flattened
  /// sense) return a pointer to a leaf element of the specified number.
  SILValue emitElementAddress(SILValue Ptr, unsigned TupleEltNo,
                              SILLocation Loc, SILBuilder &B);

  /// Push the symbolic path name to the specified element number onto the
  /// specified std::string.
  void getPathStringToElement(CanType T, unsigned Element,
                              std::string &Result);
}
  
enum DIUseKind {
  // The instruction is a Load.
  Load,
  
  // The instruction is either an initialization or an assignment, we don't
  // know which.  This classification only happens with values of trivial type
  // where the different isn't significant.
  InitOrAssign,
  
  // The instruction is an initialization of the tuple element.
  Initialization,
  
  // The instruction is an assignment, overwriting an already initialized
  // value.
  Assign,
  
  // The instruction is a store to a member of a larger struct value.
  PartialStore,
  
  /// An indirecet 'inout' parameter of an Apply instruction.
  InOutUse,
  
  /// An indirect 'in' parameter of an Apply instruction.
  IndirectIn,
  
  /// This instruction is a general escape of the value, e.g. a call to a
  /// closure that captures it.
  Escape
};

/// This struct represents a single classified access to the memory object
/// being analyzed, along with classification information about the access.
struct DIMemoryUse {
  /// This is the instruction accessing the memory.
  SILInstruction *Inst;
  
  /// This is what kind of access it is, load, store, escape, etc.
  DIUseKind Kind;
  
  /// For memory objects of (potentially recursive) tuple type, this keeps
  /// track of which tuple elements are affected.
  unsigned short FirstTupleElement, NumTupleElements;
  
  DIMemoryUse(SILInstruction *Inst, DIUseKind Kind, unsigned FTE, unsigned NTE)
  : Inst(Inst), Kind(Kind), FirstTupleElement(FTE), NumTupleElements(NTE) {
    assert(FTE == FirstTupleElement && NumTupleElements == NTE &&
           "more than 65K tuple elements not supported yet");
  }
  
  DIMemoryUse() : Inst(nullptr) {}
  
  bool isInvalid() const { return Inst == nullptr; }
  bool isValid() const { return Inst != nullptr; }
  
  bool usesElement(unsigned i) const {
    return i >= FirstTupleElement && i < FirstTupleElement+NumTupleElements;
  }
  
  /// onlyTouchesTrivialElements - Return true if all of the accessed elements
  /// have trivial type.
  bool onlyTouchesTrivialElements(SILType MemoryType) const;
  
  /// getElementBitmask - Return a bitmask with the touched tuple elements
  /// set.
  APInt getElementBitmask(unsigned NumMemoryTupleElements) const {
    return APInt::getBitsSet(NumMemoryTupleElements, FirstTupleElement,
                             FirstTupleElement+NumTupleElements);
  }
};

/// collectDIElementUsesFromAllocation - Analyze all uses of the specified
/// allocation instruction (alloc_box or alloc_stack), classifying them and
/// storing the information found into the Uses and Releases lists.
void collectDIElementUsesFromAllocation(SILInstruction *AllocInst,
                                        SmallVectorImpl<DIMemoryUse> &Uses,
                                    SmallVectorImpl<SILInstruction*> &Releases,
                                        bool isDefiniteInitFinished);
  
/// collectDIElementUsesFromMarkUninit - Analyze all uses of the specified
/// mark_uninitialized instruction, classifying them and storing the information
/// found into the Uses and Releases lists.
void collectDIElementUsesFromMarkUninit(MarkUninitializedInst *AllocInst,
                                        SmallVectorImpl<DIMemoryUse> &Uses,
                                    SmallVectorImpl<SILInstruction*> &Releases,
                                        bool isDefiniteInitFinished);
  
  
} // end namespace swift

#endif
