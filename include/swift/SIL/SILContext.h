//===--- SILContext.h -------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILBRIDGINGCONTEXT_H
#define SWIFT_SIL_SILBRIDGINGCONTEXT_H

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/NodeBits.h"
#include "swift/SIL/OperandBits.h"

namespace swift {

class SILSSAUpdater;

/// The abstract base class for the C++ implementation of Context in SwiftCompilerSources.
/// Referenced in BridgedContext.
///
/// Everything which only needs SIL is directly implemented in this class.
/// Everything which needs the SILPassManager is implemented in the derived class SwiftPassInvocation.
///
class SILContext {
public:
  enum NotificationKind : unsigned {
    Nothing = 0,
    Instructions = 0x1,
    Calls = 0x2,
    Branches = 0x4,
    Effects = 0x8,
    FunctionTables = 0x10
  };

protected:
  SILContext(SILFunction *f) : function(f), mod(&f->getModule()) {}
  SILContext(SILModule *mod) : mod(mod) {}

  virtual ~SILContext();

  /// The currently optimized function or null if this is the context of a module pass.
  SILFunction *function = nullptr;

  SILModule *mod = nullptr;

  /// All slabs, allocated by the pass.
  SILModule::SlabList allocatedSlabs;

  static constexpr int BlockSetCapacity = SILBasicBlock::numCustomBits;
  char blockSetStorage[sizeof(BasicBlockSet) * BlockSetCapacity];
  bool aliveBlockSets[BlockSetCapacity];
  int numBlockSetsAllocated = 0;

  static constexpr int NodeSetCapacity = SILNode::numCustomBits;
  char nodeSetStorage[sizeof(NodeSet) * NodeSetCapacity];
  bool aliveNodeSets[NodeSetCapacity];
  int numNodeSetsAllocated = 0;

  static constexpr int OperandSetCapacity = Operand::numCustomBits;
  char operandSetStorage[sizeof(OperandSet) * OperandSetCapacity];
  bool aliveOperandSets[OperandSetCapacity];
  int numOperandSetsAllocated = 0;

  int numClonersAllocated = 0;

  SILSSAUpdater *ssaUpdater = nullptr;
  SmallVector<SILPhiArgument *, 4> insertedPhisBySSAUpdater;

  /// Change notifications, collected during a pass run.
  NotificationKind changeNotifications = NotificationKind::Nothing;

  void verifyEverythingIsCleared();

public:
  SILModule *getModule() const { return mod; }
  SILFunction *getFunction() const {
    ASSERT(function != nullptr && "not in a function pass");
    return function;
  }

  /// Called by the pass when changes are made to the SIL.
  void notifyChanges(NotificationKind notification) {
    changeNotifications = (NotificationKind)(changeNotifications | notification);
  }

  FixedSizeSlab *allocSlab(FixedSizeSlab *afterSlab);

  FixedSizeSlab *freeSlab(FixedSizeSlab *slab);

  BasicBlockSet *allocBlockSet();

  void freeBlockSet(BasicBlockSet *set);

  NodeSet *allocNodeSet();

  void freeNodeSet(NodeSet *set);

  OperandSet *allocOperandSet();

  void freeOperandSet(OperandSet *set);

  void notifyNewCloner() { numClonersAllocated++; }
  void notifyClonerDestroyed() { numClonersAllocated--; }

  virtual void eraseInstruction(SILInstruction *inst, bool salvageDebugInfo) = 0;
  virtual SILFunction *createEmptyFunction(StringRef name, ArrayRef<SILParameterInfo> params,
                                          bool hasSelfParam, SILFunction *fromFn) = 0;
  virtual void moveFunctionBody(SILFunction *sourceFn, SILFunction *destFn) = 0;

  virtual SILFunction *lookupStdlibFunction(StringRef name) = 0;

  // The SILSSAUpdater is implemented in the Optimizer. Therefore all the APIs need to take
  // the indirection through virtual functions to SwiftPassInvocation.
  virtual void initializeSSAUpdater(SILFunction *function, SILType type, ValueOwnershipKind ownership) = 0;
  virtual void SSAUpdater_addAvailableValue(SILBasicBlock *block, SILValue value) = 0;
  virtual SILValue SSAUpdater_getValueAtEndOfBlock(SILBasicBlock *block) = 0;
  virtual SILValue SSAUpdater_getValueInMiddleOfBlock(SILBasicBlock *block) = 0;
  ArrayRef<SILPhiArgument*> SSAUpdater_getInsertedPhis() { return insertedPhisBySSAUpdater; }
};

} // namespace swift

#endif
