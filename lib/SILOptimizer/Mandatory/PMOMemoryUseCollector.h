//===- PMOMemoryUseCollector.h - Memory use information for PMO -*- C++ -*-===//
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
///
/// \file
///
/// High Level Overview
/// -------------------
///
/// This file declares logic that is used by the predictable memory optimization
/// pass. Like definite initialization, it is tuple element sensitive instead of
/// relying on an SROA optimization or the like to chop up structs.
///
/// NOTE: PMO is an abbreviation for "Predictable Memory Optimizations".
///
/// Historical Note
/// ---------------
///
/// This file looks very similar to DIMemoryUseCollector.* and has many
/// connections to Definite Initialization. This is because early in the
/// development of Swift, Predictable Memory Optimizations and Definite
/// Initialization were actually the same pass. The pass grew really big and the
/// two passes were split, but still used similra utility code. This became the
/// original DIMemoryUseCollector.*. This code was full of conditional logic for
/// all of the various cases that made it difficult to understand which code was
/// needed for Predictable Mem Opts and what was needed for DI. The introduction
/// of the SIL ownership model to SIL was used as an opportunity to split the
/// two, flatten the sphagetti conditional code so the logic was clear, and
/// allow the two passes to diverge and hew their form closer to their function.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_PMOMEMORYUSECOLLECTOR_H
#define SWIFT_SILOPTIMIZER_MANDATORY_PMOMEMORYUSECOLLECTOR_H

#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILType.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Support/Compiler.h"

namespace swift {

class SILBuilder;

/// PMOMemoryObjectInfo - This struct holds information about the memory object
/// being analyzed that is required to correctly break it down into elements.
///
/// This includes a collection of utilities for reasoning about (potentially
/// recursively) exploded aggregate elements, and computing access paths and
/// indexes into the flattened namespace.
///
/// The flattened namespace is assigned lexicographically.  For example, in:
///   (Int, ((Float, (), Double)))
/// the Int member is numbered 0, the Float is numbered 1, and the Double is
/// numbered 2.  Empty tuples don't get numbered since they contain no state.
///
/// Structs and classes have their elements exploded when we are analyzing the
/// 'self' member in an initializer for the aggregate.
///
/// Derived classes have an additional field at the end that models whether or
/// not super.init() has been called or not.
class PMOMemoryObjectInfo {
public:
  /// This is the instruction that represents the memory.  It is either an
  /// allocation (alloc_box, alloc_stack) or a mark_uninitialized.
  SingleValueInstruction *MemoryInst;

  /// This is the base type of the memory allocation.
  SILType MemorySILType;

  /// True if the memory object being analyzed represents a 'let', which is
  /// initialize-only (reassignments are not allowed).
  bool IsLet = false;

  /// This is the count of elements being analyzed.  For memory objects that are
  /// tuples, this is the flattened element count.  For 'self' members in init
  /// methods, this is the local field count (+1 for derive classes).
  unsigned NumElements;

public:
  PMOMemoryObjectInfo(SingleValueInstruction *MemoryInst);

  SILLocation getLoc() const { return MemoryInst->getLoc(); }
  SILFunction &getFunction() const { return *MemoryInst->getFunction(); }

  /// Return the first instruction of the function containing the memory object.
  SILInstruction *getFunctionEntryPoint() const;

  CanType getType() const { return MemorySILType.getASTType(); }

  SingleValueInstruction *getAddress() const {
    if (isa<AllocStackInst>(MemoryInst))
      return MemoryInst;
    assert(false);
    return nullptr;
  }

  AllocBoxInst *getContainer() const {
    return dyn_cast<AllocBoxInst>(MemoryInst);
  }

  /// getNumMemoryElements - Return the number of elements, without the extra
  /// "super.init" tracker in initializers of derived classes.
  unsigned getNumMemoryElements() const {
    return NumElements - unsigned(false);
  }

  /// getElementType - Return the swift type of the specified element.
  SILType getElementType(unsigned EltNo) const;

  /// Push the symbolic path name to the specified element number onto the
  /// specified std::string.  If the actual decl (or a subelement thereof) can
  /// be determined, return it.  Otherwise, return null.
  ValueDecl *getPathStringToElement(unsigned Element,
                                    std::string &Result) const;

  /// If the specified value is a 'let' property in an initializer, return true.
  bool isElementLetProperty(unsigned Element) const;
};

enum PMOUseKind {
  /// The instruction is a Load.
  Load,

  /// The instruction is either an initialization or an assignment, we don't
  /// know which.  This classification only happens with values of trivial type
  /// where the different isn't significant.
  InitOrAssign,

  /// The instruction is an initialization of the tuple element.
  Initialization,

  /// The instruction is an assignment, overwriting an already initialized
  /// value.
  Assign,

  /// The instruction is a store to a member of a larger struct value.
  PartialStore,

  /// An indirect 'inout' parameter of an Apply instruction.
  InOutUse,

  /// An indirect 'in' parameter of an Apply instruction.
  IndirectIn,

  /// This instruction is a general escape of the value, e.g. a call to a
  /// closure that captures it.
  Escape,

  /// This instruction is a call to 'super.init' in a 'self' initializer of a
  /// derived class.
  SuperInit,

  /// This instruction is a call to 'self.init' in a delegating initializer.
  SelfInit
};

/// This struct represents a single classified access to the memory object
/// being analyzed, along with classification information about the access.
struct PMOMemoryUse {
  /// This is the instruction accessing the memory.
  SILInstruction *Inst;

  /// This is what kind of access it is, load, store, escape, etc.
  PMOUseKind Kind;

  /// For memory objects of (potentially recursive) tuple type, this keeps
  /// track of which tuple elements are affected.
  unsigned short FirstElement, NumElements;

  PMOMemoryUse(SILInstruction *Inst, PMOUseKind Kind, unsigned FE, unsigned NE)
      : Inst(Inst), Kind(Kind), FirstElement(FE), NumElements(NE) {
    assert(FE == FirstElement && NumElements == NE &&
           "more than 64K elements not supported yet");
  }

  PMOMemoryUse() : Inst(nullptr) {}

  bool isInvalid() const { return Inst == nullptr; }
  bool isValid() const { return Inst != nullptr; }

  bool usesElement(unsigned i) const {
    return i >= FirstElement &&
           i < static_cast<unsigned>(FirstElement + NumElements);
  }

  /// onlyTouchesTrivialElements - Return true if all of the accessed elements
  /// have trivial type.
  bool onlyTouchesTrivialElements(const PMOMemoryObjectInfo &MemoryInfo) const;

  /// getElementBitmask - Return a bitmask with the touched tuple elements
  /// set.
  APInt getElementBitmask(unsigned NumMemoryTupleElements) const {
    return APInt::getBitsSet(NumMemoryTupleElements, FirstElement,
                             FirstElement + NumElements);
  }
};

/// collectPMOElementUsesFrom - Analyze all uses of the specified allocation
/// instruction (alloc_box, alloc_stack or mark_uninitialized), classifying them
/// and storing the information found into the Uses and Releases lists.
LLVM_NODISCARD bool
collectPMOElementUsesFrom(const PMOMemoryObjectInfo &MemoryInfo,
                          SmallVectorImpl<PMOMemoryUse> &Uses,
                          SmallVectorImpl<SILInstruction *> &Releases);

} // end namespace swift

#endif
