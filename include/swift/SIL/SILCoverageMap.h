//===--- SILCoverageMap.h - Defines the SILCoverageMap class ----*- C++ -*-===//
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
// This file defines the SILCoverageMap class, which is used to relay coverage
// mapping information from the AST to lower layers of the compiler.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILCOVERAGEMAP_H
#define SWIFT_SIL_SILCOVERAGEMAP_H

#include "swift/Basic/SourceLoc.h"
#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ProfileData/CoverageMapping.h"

namespace llvm {
namespace coverage {
struct CounterExpression;
struct Counter;
}
}

namespace swift {

/// A mapping from source locations to expressions made up of profiling
/// counters. This is used to embed information in build products for use with
/// coverage tools later.
class SILCoverageMap : public llvm::ilist_node<SILCoverageMap>,
                       public SILAllocated<SILCoverageMap> {
public:
  struct MappedRegion {
    unsigned StartLine;
    unsigned StartCol;
    unsigned EndLine;
    unsigned EndCol;
    llvm::coverage::Counter Counter;

    MappedRegion(unsigned StartLine, unsigned StartCol, unsigned EndLine,
                 unsigned EndCol, llvm::coverage::Counter Counter)
        : StartLine(StartLine), StartCol(StartCol), EndLine(EndLine),
          EndCol(EndCol), Counter(Counter) {}
  };

private:
  // The mangled name of the function covered by this mapping.
  SILFunction &Fn;

  // The coverage hash of the function covered by this mapping.
  uint64_t Hash;

  // The number of mapped regions.
  unsigned NumMappedRegions;

  // The number of counter expressions.
  unsigned NumExpressions;

  // Tail-allocated region mappings.
  MappedRegion *MappedRegions;

  // Tail-allocated expression list.
  llvm::coverage::CounterExpression *Expressions;

  // Disallow copying into temporary objects.
  SILCoverageMap(const SILCoverageMap &other) = delete;
  SILCoverageMap &operator=(const SILCoverageMap &) = delete;

  /// Private constructor. Create these using SILCoverageMap::create.
  SILCoverageMap(SILFunction &Fn, uint64_t Hash);

public:
  ~SILCoverageMap();

  static SILCoverageMap *
  create(SILModule &M, SILFunction &Fn, uint64_t Hash,
         ArrayRef<MappedRegion> MappedRegions,
         ArrayRef<llvm::coverage::CounterExpression> Expressions);

  /// Return the mangled name of the function this mapping covers.
  StringRef getName() const { return Fn.getName(); }

  /// Return the coverage hash for function this mapping covers.
  uint64_t getHash() const { return Hash; }

  /// Return all of the mapped regions.
  ArrayRef<MappedRegion> getMappedRegions() const {
    return {MappedRegions, NumMappedRegions};
  }

  /// Return all of the counter expressions.
  ArrayRef<llvm::coverage::CounterExpression> getExpressions() const {
    return {Expressions, NumExpressions};
  }

  void printCounter(llvm::raw_ostream &OS, llvm::coverage::Counter C) const;

  /// Print the coverage map.
  void print(llvm::raw_ostream &OS, bool ShouldSort = false,
             bool Verbose = false) const;
  void dump() const;
};

} // end swift namespace

namespace llvm {

//===----------------------------------------------------------------------===//
// ilist_traits for SILCoverageMap
//===----------------------------------------------------------------------===//

template <>
struct ilist_traits<::swift::SILCoverageMap> :
public ilist_default_traits<::swift::SILCoverageMap> {
  typedef ::swift::SILCoverageMap SILCoverageMap;

private:
  mutable ilist_half_node<SILCoverageMap> Sentinel;

public:
  SILCoverageMap *createSentinel() const {
    return static_cast<SILCoverageMap*>(&Sentinel);
  }
  void destroySentinel(SILCoverageMap *) const {}

  SILCoverageMap *provideInitialHead() const { return createSentinel(); }
  SILCoverageMap *ensureHead(SILCoverageMap*) const { return createSentinel(); }
  static void noteHead(SILCoverageMap*, SILCoverageMap*) {}
  static void deleteNode(SILCoverageMap *VT) { VT->~SILCoverageMap(); }

private:
  void createNode(const SILCoverageMap &);
};

} // end llvm namespace

#endif // SWIFT_SIL_SILCOVERAGEMAP_H
