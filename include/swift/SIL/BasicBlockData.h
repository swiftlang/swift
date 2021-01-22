//===--- BasicBlockData.h - Defines the BasicBlockData utility --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the BasicBlockData utility
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_BASICBLOCKDATA_H
#define SWIFT_SIL_BASICBLOCKDATA_H

#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {

/// Manage additional data for a function's basic blocks.
///
/// This can be used by transforms to store temporary data per basic block.
/// It is very efficient: only a single memory allocation is needed and no maps
/// are used to lookup data.
///
/// It is still permitted to use a BasicBlockData after the block list of a
/// function has changed. With two exceptions:
///  * If new blocks are added during the lifetime of a BasicBlockData, no data
///    can be queried for a new block.
///  * If a new BasicBlockData is created after the block list changed, then any
///    older BasicBlockData (which was created before the change) cannot be
///    used anymore.
///
/// This means, it's totally fine to store a BasicBlockData in an Analysis, as
/// long as the invalidation mechanism ensures that the analysis data is not
/// retrieved after the function's block list changed.
///
/// The template argument \p N specifies how many Data elements can be stored
/// inline in the data vector. The default value of 32 avoids malloc for about
/// 90% of all functions.
template <typename Data, unsigned N = 32>
class BasicBlockData {
  SILFunction *function;
  llvm::SmallVector<Data, N> data;

  /// The data is valid if validForBlockOrder matches the function's
  /// BlockListChangeIdx.
  unsigned validForBlockOrder = 0;

  int getIndex(SILBasicBlock *block) const {
    assert(block->index >= 0 && "Cannot get data for a new block");
    assert(validForBlockOrder == function->BlockListChangeIdx &&
           "BasicBlockData invalid because the function's block list changed");
    return block->index;
  }

public:

  /// For iterating over the function's blocks, yielding the block and its data.
  template <typename BlockIterTy, bool IsConst> class IteratorTy {
    using DataTy = typename std::conditional<IsConst, const Data, Data>::type;
    using BasicBlockDataTy = typename std::conditional<IsConst,
      const BasicBlockData<Data>, BasicBlockData<Data>>::type;

    /// The underlying SILBasicBlock iterator.
    BlockIterTy blockIter;

    /// Reference to the constructing BasicBlockData. Used to retrieve the data.
    BasicBlockDataTy &blockData;

    friend class BasicBlockData<Data>;

    IteratorTy(BlockIterTy blockIter, BasicBlockDataTy &blockData) :
      blockIter(blockIter), blockData(blockData) {}

  public:
    /// Result of re-referencing the iterator.
    struct BlockAndData {
      SILBasicBlock &block;
      DataTy &data;

      BlockAndData(SILBasicBlock &block, DataTy &data) :
        block(block), data(data) {}
    };

    IteratorTy & operator++() {  // Preincrement
      blockIter++; return *this;
    }
    IteratorTy & operator++(int) {  // Postincrement
      IteratorTy tmp = *this; ++*this; return tmp;
    }

    SILBasicBlock &block() const { return *blockIter; }
    DataTy &data() const { return blockData[&block()]; }

    BlockAndData operator*() const { return BlockAndData(block(), data()); }

    bool operator==(const IteratorTy &rhs) const {
      return blockIter == rhs.blockIter;
    }
    bool operator!=(const IteratorTy &rhs) const {
      return blockIter != rhs.blockIter;
    }
  };

  using iterator = IteratorTy<SILFunction::iterator, false>;
  using const_iterator = IteratorTy<SILFunction::iterator, true>;
  using reverse_iterator = IteratorTy<SILFunction::reverse_iterator, false>;
  using const_reverse_iterator = IteratorTy<SILFunction::reverse_iterator, true>;

  static Data constructDefaultData(SILBasicBlock *) { return Data(); }

  /// Initialize Data for all basic blocks.
  ///
  /// The \p init function must return the initial data for a block. If not
  /// provided, all data is constructed with the Data() default constructor.
  BasicBlockData(SILFunction *function,
                 llvm::function_ref<Data(SILBasicBlock *block)> init =
                   // Would be nice to simply write constructDefaultData in
                   // closure syntax, but I didn't get it compile under Windows.
                   constructDefaultData) :
      function(function) {
    // Reserve enough space. Though SILFunction::size() iterates over all
    // blocks it is still better than to risk multiple mallocs.
    data.reserve(function->size());
    bool blockListChanged = false;
    for (auto blockAndIdx : llvm::enumerate(*function)) {
      SILBasicBlock &block = blockAndIdx.value();
      int idx = blockAndIdx.index();
      if (block.index != idx) {
        blockListChanged = true;
        block.index = idx;
      }
      data.push_back(init(&block));
    }
    function->incrementRefCount();

    // If we assigned new block indices, it invalidates all older BasicBlockData
    // instances.
    if (blockListChanged)
      ++function->BlockListChangeIdx;
    validForBlockOrder = function->BlockListChangeIdx;
  }

  ~BasicBlockData() {
    function->decrementRefCount();
  }

  // For iterating over the function's basic blocks, yielding the block and its
  // data. For example:
  //
  //    BasicBlockData<Mydata> blockData;
  //    for (auto bd : blockData) {
  //      SILBasicBlock &block = bd.block;
  //      Mydata &dataForBlock = bd.data;
  //    }

  iterator begin() { return iterator(function->begin(), *this); }
  iterator end() { return iterator(function->end(), *this); }
  const_iterator begin() const {
    return const_iterator(function->begin(), *this);
  }
  const_iterator end() const {
    return const_iterator(function->end(), *this);
  }
  reverse_iterator rbegin() {
    return reverse_iterator(function->rbegin(), *this);
  }
  reverse_iterator rend() {
    return reverse_iterator(function->rend(), *this);
  }
  const_reverse_iterator rbegin() const {
    return const_reverse_iterator(function->rbegin(), *this);
  }
  const_reverse_iterator rend() const {
    return const_reverse_iterator(function->rend(), *this);
  }

  /// Returns the function's entry block and its data.
  typename iterator::BlockAndData entry() { return *begin(); }

  /// Returns the function's entry block and its data (const-version).
  typename const_iterator::BlockAndData entry() const { return *begin(); }

  /// Returns the underlying function.
  SILFunction *getFunction() const { return function; }

  /// Gets a mutable reference to a block's data.
  Data &operator[] (SILBasicBlock *block) {
    return data[getIndex(block)];
  }

  /// Gets a constant reference to a block's data.
  const Data &operator[] (SILBasicBlock *block) const {
    return data[getIndex(block)];
  }
  
  /// If \p block is a new block, i.e. created after this BasicBlockData was
  /// constructed, creates a new Data with \p init and returns it.
  Data &get(SILBasicBlock *block, llvm::function_ref<Data()> init) {
    if (block->index < 0) {
      assert(validForBlockOrder == function->BlockListChangeIdx &&
             "BasicBlockData invalid because the function's block list changed");
      validForBlockOrder = ++function->BlockListChangeIdx;
      block->index = data.size();
      data.push_back(init());
    }
    return data[getIndex(block)];
  }
};

} // end swift namespace

#endif
