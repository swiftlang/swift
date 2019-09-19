//===--- MemoryLifetime.h ---------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file Contains utilities for calculating and verifying memory lifetime.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_MEMORY_LIFETIME_H
#define SWIFT_SIL_MEMORY_LIFETIME_H

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"

namespace swift {

/// The MemoryLocations utility provides functions to analyze memory locations.
///
/// Memory locations are limited to addresses which are guaranteed to
/// be not aliased, like @in/inout parameters and alloc_stack.
/// Currently only a certain set of address instructions are supported:
/// Specifically those instructions which are going to be included when SIL
/// supports opaque values.
/// TODO: Support more address instructions, like cast instructions.
///
/// The MemoryLocations works well together with MemoryDataflow, which can be
/// used to calculate global dataflow of location information.
class MemoryLocations {
public:

  using Bits = llvm::SmallBitVector;

  /// Represents a not-aliased memory location: either an indirect function
  /// parameter or an alloc_stack.
  ///
  /// Each location has a unique number which is index in the
  /// MemoryLifetime::locations array and the bit number in the bit sets.
  ///
  /// Locations can have sub-locations in case the parent location is a struct
  /// or tuple with fields/elements. So, each top-level location forms a
  /// tree-like data structure. Sub-locations are only created lazily, i.e. if
  /// struct/tuple elements are really accessed with struct/tuple_element_addr.
  ///
  /// As most alloc_stack locations only live within a single block, such
  /// single-block locations are not included in the "regular" data flow
  /// analysis (to not blow up the bit vectors). They are handled separately
  /// with a simple single-block data flow analysis, which runs independently
  /// for each block.
  struct Location {

    /// The SIL value of the memory location.
    ///
    /// For top-level locations this is either a function argument or an
    /// alloc_stack. For sub-locations it's the struct/tuple_element_addr.
    /// In case there are multiple struct/tuple_element_addr for a single
    /// field, this is only one representative instruction out of the set.
    SILValue representativeValue;

    /// All tracked sub-locations.
    ///
    /// If all tracked sub-locations cover the whole memory location, the "self"
    /// bit is not set. In other words: the "self" bit represents all
    /// sublocations, which are not explicitly tracked as locations.
    /// For example:
    /// \code
    ///   struct Inner {
    ///     var a: T
    ///     var b: T
    ///   }
    ///   struct Outer {
    ///     var x: T
    ///     var y: Inner
    ///     var z: T      // not accessed in the analyzed function
    ///   }
    /// \endcode
    ///
    /// If the analyzed function contains:
    /// \code
    ///   %a = alloc_stack $Outer                 // = location 0
    ///   %ox = struct_element_adr %a, #Outer.x   // = location 1
    ///   %oy = struct_element_adr %a, #Outer.y   // = location 2
    ///   %ia = struct_element_adr %oy, #Inner.a  // = location 3
    ///   %ib = struct_element_adr %oy, #Inner.b  // = location 4
    /// \endcode
    ///
    /// the ``subLocations`` bits are:
    /// \code
    ///    location 0 (alloc_stack): [0, 1,   3, 4]
    ///    location 1 (Outer.x):     [   1        ]
    ///    location 2 (Outer.y):     [        3, 4]
    ///    location 3 (Inner.a):     [        3   ]
    ///    location 4 (Inner.b):     [           4]
    /// \endcode
    ///
    /// Bit 2 is never set because Inner is completly represented by its
    /// sub-locations 3 and 4. But bit 0 is set in location 0 (the "self" bit),
    /// because it represents the untracked field ``Outer.z``.
    Bits subLocations;

    /// The accumulated parent bits, including the "self" bit.
    ///
    /// For the example given for ``subLocations``, the ``selfAndParents`` bits
    /// are:
    /// \code
    ///    location 0 (alloc_stack): [0           ]
    ///    location 1 (Outer.x):     [0, 1        ]
    ///    location 2 (Outer.y):     [0,   2      ]
    ///    location 3 (Inner.a):     [0,   2, 3   ]
    ///    location 4 (Inner.b):     [0,   2,    4]
    /// \endcode
    Bits selfAndParents;

    /// The location index of the parent, or -1 if it's a top-level location.
    ///
    /// For the example given for ``subLocations``, the ``parentIdx`` indices
    /// are:
    /// \code
    ///    location 0 (alloc_stack): -1
    ///    location 1 (Outer.x):     0
    ///    location 2 (Outer.y):     0
    ///    location 3 (Inner.a):     2
    ///    location 4 (Inner.b):     2
    /// \endcode
    int parentIdx;

    /// Used to decide if a location is completely covered by its sub-locations.
    ///
    /// -1 means: not yet initialized.
    int numFieldsNotCoveredBySubfields = -1;

    Location(SILValue val, unsigned index, int parentIdx = -1);
  };

private:
  /// The array of locations.
  llvm::SmallVector<Location, 64> locations;

  /// Mapping from SIL values (function arguments and alloc_stack) to location
  /// indices.
  ///
  /// In case there are multiple struct/tuple_element_addr for a single
  /// field, this map contains multiple entries mapping to the same location.
  llvm::DenseMap<SILValue, unsigned> addr2LocIdx;

  /// Memory locations (e.g. alloc_stack) which live in a single basic block.
  ///
  /// Those locations are excluded from the locations to keep the bit sets
  /// small. They can be handled separately with handleSingleBlockLocations().
  llvm::SmallVector<SingleValueInstruction *, 16> singleBlockLocations;

public:
  MemoryLocations() {}

  MemoryLocations(const MemoryLocations &) = delete;
  MemoryLocations &operator=(const MemoryLocations &) = delete;

  /// Returns the number of collected locations, except single-block locations.
  unsigned getNumLocations() const { return locations.size(); }

  /// Returns the location index corresponding to a memory address or -1, if
  /// \p addr is not associated with a location.
  int getLocationIdx(SILValue addr) const;

  /// Returns the location corresponding to a memory address or null, if
  /// \p addr is not associated with a location.
  const Location *getLocation(SILValue addr) const {
    int locIdx = getLocationIdx(addr);
    if (locIdx >= 0)
      return &locations[locIdx];
    return nullptr;
  }

  /// Returns the location with a given \p index.
  const Location *getLocation(unsigned index) const {
    return &locations[index];
  }

  /// Registers an address projection instruction for a location.
  void registerProjection(SingleValueInstruction *projection, unsigned locIdx) {
    addr2LocIdx[projection] = locIdx;
  }

  /// Sets the location bits os \p addr in \p bits, if \p addr is associated
  /// with a location.
  void setBits(Bits &bits, SILValue addr) {
    if (auto *loc = getLocation(addr))
      bits |= loc->subLocations;
  }

  /// Clears the location bits os \p addr in \p bits, if \p addr is associated
  /// with a location.
  void clearBits(Bits &bits, SILValue addr) {
    if (auto *loc = getLocation(addr))
      bits.reset(loc->subLocations);
  }

  /// Analyzes all locations in a function.
  ///
  /// Single-block locations are not analyzed, but added to singleBlockLocations.
  void analyzeLocations(SILFunction *function);

  /// Analyze a single top-level location.
  ///
  /// If all uses of \p loc are okay, the location and its sub-locations are
  /// added to the data structures.
  void analyzeLocation(SILValue loc);

  /// Do a block-local processing for all locations in singleBlockLocations.
  ///
  /// First, initializes all locations which are alive in a block and then
  /// calls \p handlerFunc for the block.
  void handleSingleBlockLocations(
                       std::function<void (SILBasicBlock *block)> handlerFunc);

  /// Debug dump the MemoryLifetime internals.
  void dump() const;

  /// Debug dump a bit set .
  static void dumpBits(const Bits &bits);

private:
  /// Clears all datastructures, except singleBlockLocations;
  void clear();

  // (locationIdx, fieldNr) -> subLocationIdx
  using SubLocationMap = llvm::DenseMap<std::pair<unsigned, unsigned>, unsigned>;

  /// Helper function called by analyzeLocation to check all uses of the
  /// location recursively.
  ///
  /// The \p subLocationMap is a temporary cache to speed up sub-location lookup.
  bool analyzeLocationUsesRecursively(SILValue V, unsigned locIdx,
                                      SmallVectorImpl<SILValue> &collectedVals,
                                      SubLocationMap &subLocationMap);

  /// Helper function called by analyzeLocation to create a sub-location for
  /// and address projection and check all of its uses.
  bool analyzeAddrProjection(
    SingleValueInstruction *projection, unsigned parentLocIdx,unsigned fieldNr,
    SmallVectorImpl<SILValue> &collectedVals, SubLocationMap &subLocationMap);

  /// Calculates Location::numFieldsNotCoveredBySubfields
  void initFieldsCounter(Location &loc);

  /// Only memory locations which store a non-trivial type are considered.
  bool shouldTrackLocation(SILType type, SILFunction *inFunction) {
    return !type.isTrivial(*inFunction);
  }
};

/// The MemoryDataflow utility calculates global dataflow of memory locations.
///
/// The MemoryDataflow works well together with MemoryLocations, which can be
/// used to analyze locations as input to the dataflow.
/// TODO: Actuall this utility can be used for any kind of dataflow, not just
/// for memory locations. Consider renaming it.
class MemoryDataflow {

public:
  using Bits = MemoryLocations::Bits;

  /// Basic-block specific information used for dataflow analysis.
  struct BlockState {
    /// The backlink to the SILBasicBlock.
    SILBasicBlock *block;

    /// The bits valid at the entry (i.e. the first instruction) of the block.
    Bits entrySet;

    /// The bits valid at the exit (i.e. after the terminator) of the block.
    Bits exitSet;

    /// Generated bits of the block.
    Bits genSet;

    /// Killed bits of the block.
    Bits killSet;

    /// True, if this block is reachable from the entry block, i.e. is not an
    /// unreachable block.
    ///
    /// This flag is only computed if entryReachabilityAnalysis is called.
    bool reachableFromEntry = false;

    /// True, if any function-exit block can be reached from this block, i.e. is
    /// not a block which eventually ends in an unreachable instruction.
    ///
    /// This flag is only computed if exitReachableAnalysis is called.
    bool exitReachable = false;

    BlockState(SILBasicBlock *block = nullptr) : block(block) { }

    // Utility functions for setting and clearing gen- and kill-bits.

    void genBits(SILValue addr, const MemoryLocations &locs) {
      if (auto *loc = locs.getLocation(addr)) {
        killSet.reset(loc->subLocations);
        genSet |= loc->subLocations;
      }
    }

    void killBits(SILValue addr, const MemoryLocations &locs) {
      if (auto *loc = locs.getLocation(addr)) {
        genSet.reset(loc->subLocations);
        killSet |= loc->subLocations;
      }
    }
  };

private:
  /// All block states.
  std::vector<BlockState> blockStates;

  /// Getting from SILBasicBlock to BlockState.
  llvm::DenseMap<SILBasicBlock *, BlockState *> block2State;

public:
  /// Sets up the BlockState datastructures and associates all basic blocks with
  /// a state.
  MemoryDataflow(SILFunction *function, unsigned numLocations);

  MemoryDataflow(const MemoryDataflow &) = delete;
  MemoryDataflow &operator=(const MemoryDataflow &) = delete;

  using iterator = std::vector<BlockState>::iterator;

  iterator begin() { return blockStates.begin(); }
  iterator end() { return blockStates.end(); }

  /// Returns the state of a block.
  BlockState *getState(SILBasicBlock *block) {
    return block2State[block];
  }

  /// Calculates the BlockState::reachableFromEntry flags.
  void entryReachabilityAnalysis();

  /// Calculates the BlockState::exitReachable flags.
  void exitReachableAnalysis();

  using JoinOperation = std::function<void (Bits &dest, const Bits &src)>;

  /// Derives the block exit sets from the entry sets by applying the gen and
  /// kill sets.
  /// At control flow joins, the \p join operation is applied.
  void solveForward(JoinOperation join);

  /// Calls solveForward() with a bit-intersection as join operation.
  void solveForwardWithIntersect();

  /// Calls solveForward() with a bit-union as join operation.
  void solveForwardWithUnion();

  /// Derives the block entry sets from the exit sets by applying the gen and
  /// kill sets.
  /// At control flow joins, the \p join operation is applied.
  void solveBackward(JoinOperation join);

  /// Calls solveBackward() with a bit-intersection as join operation.
  void solveBackwardWithIntersect();

  /// Calls solveBackward() with a bit-union as join operation.
  void solveBackwardWithUnion();

  /// Debug dump the MemoryLifetime internals.
  void dump() const;
};

/// Verifies the lifetime of memory locations in a function.
void verifyMemoryLifetime(SILFunction *function);

} // end swift namespace

#endif
