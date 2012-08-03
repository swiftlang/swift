//===--- CFG.h - Defines the CFG and CFG library umbrella header -*- C++ -*-==//
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
// This file defines the CFG and includes the CFG components headers.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CFG_H
#define SWIFT_CFG_H

#include "llvm/Support/Allocator.h"
#include "llvm/ADT/GraphTraits.h"
#include "swift/CFG/Instruction.h"
#include "swift/CFG/BasicBlock.h"

namespace swift {

class Instruction;
class Stmt;
class TranslationUnit;

class CFG {
public:
  typedef llvm::iplist<BasicBlock> BlockListType;

  /// The collection of all BasicBlocks in the CFG.
  BlockListType blocks;

  /// The entrance of the CFG.
  BasicBlock *entryBlock;

private:
  friend class BasicBlock;

  /// Allocator that manages the memory of all the pieces of the CFG.
  mutable llvm::BumpPtrAllocator BPA;

  // Intentionally marked private so that we need to use 'build()'
  // to construct a CFG.
  CFG();

public:
  ~CFG();

  /// Construct a CFG from a given statement.  It is the caller's responsibility
  /// to 'delete' this object.  This can return nullptr if the CFG cannot
  /// be constructed.
  static CFG *constructCFG(const Stmt *S);

  /// Pretty-print the CFG.
  void dump() const;

  /// Dump the CFGs of an entire translation unit.
  static void dump(TranslationUnit *TU);

  /// Pretty-print the CFG with the designated stream.
  void print(llvm::raw_ostream &OS) const;

  /// Allocate memory using CFG's internal allocator.
  void *allocate(unsigned Size, unsigned Align) const {
    return BPA.Allocate(Size, Align);
  }

  /// \brief Provides a custom implementation of the iterator class to have the
  /// same interface as Function::iterator - iterator returns BasicBlock
  /// (not a pointer to BasicBlock).
  class graph_iterator {
  public:
    typedef const BasicBlock value_type;
    typedef value_type& reference;
    typedef value_type* pointer;
    typedef BlockListType::iterator ImplTy;

    graph_iterator(const ImplTy &i) : I(i) {}

    bool operator==(const graph_iterator &X) const { return I == X.I; }
    bool operator!=(const graph_iterator &X) const { return I != X.I; }
    reference operator*() const { return *I; }
    pointer operator->() const { return  &*I; }
    operator BasicBlock*() { return  &*I; }

    graph_iterator &operator++() { ++I; return *this; }
    graph_iterator &operator--() { --I; return *this; }

  private:
    ImplTy I;
  };
  
  class const_graph_iterator {
  public:
    typedef const BasicBlock value_type;
    typedef value_type& reference;
    typedef value_type* pointer;
    typedef BlockListType::const_iterator ImplTy;

    const_graph_iterator(const ImplTy &i) : I(i) {}

    bool operator==(const const_graph_iterator &X) const { return I == X.I; }
    bool operator!=(const const_graph_iterator &X) const { return I != X.I; }

    reference operator*() const { return *I; }
    pointer operator->() const { return  &*I; }
    operator const BasicBlock*() const { return  &*I; }

    const_graph_iterator &operator++() { ++I; return *this; }
    const_graph_iterator &operator--() { --I; return *this; }

  private:
    ImplTy I;
  };

  graph_iterator nodes_begin() {
    return graph_iterator(blocks.begin());
  }
  graph_iterator nodes_end() {
    return graph_iterator(blocks.end());
  }
  const_graph_iterator nodes_begin() const {
    return const_graph_iterator(blocks.begin());
  }
  const_graph_iterator nodes_end() const {
    return const_graph_iterator(blocks.end());
  }
};

} // end swift namespace

namespace llvm {

template <> struct GraphTraits<::swift::CFG *>
  : public GraphTraits<::swift::BasicBlock *>
{
  static NodeType *getEntryNode(::swift::CFG *C) { return C->entryBlock; }
  typedef ::swift::CFG::BlockListType::iterator nodes_iterator;
  static nodes_iterator nodes_begin(::swift::CFG *C) {return C->blocks.begin();}
  static nodes_iterator nodes_end(::swift::CFG *C) { return C->blocks.end(); }
  static unsigned size(::swift::CFG *C) { return C->blocks.size(); }
};

} // end llvm namespace

// operator new and delete aren't allowed inside namespaces.

/// @brief Placement new for using the CFG's allocator.
///
/// This placement form of operator new uses the CFG's allocator for
/// obtaining memory.
inline void *operator new(size_t Bytes, const swift::CFG &C,
                          size_t Alignment) {
  return C.allocate(Bytes, Alignment);
}

/// @brief Placement delete companion to the new above.
///
/// This operator is just a companion to the new above. There is no way of
/// invoking it directly; see the new operator for more details. This operator
/// is called implicitly by the compiler if a placement new expression using
/// the CFG throws in the object constructor.
inline void operator delete(void *Ptr, const swift::CFG &C, size_t) {}

/// This placement form of operator new[] uses the CFG's allocator for
/// obtaining memory.
///
/// We intentionally avoid using a nothrow specification here so that the calls
/// to this operator will not perform a null check on the result -- the
/// underlying allocator never returns null pointers.
inline void *operator new[](size_t Bytes, const swift::CFG& C,
                            size_t Alignment = 8) {
  return C.allocate(Bytes, Alignment);
}

/// @brief Placement delete[] companion to the new[] above.
///
/// This operator is just a companion to the new[] above. There is no way of
/// invoking it directly; see the new[] operator for more details. This operator
/// is called implicitly by the compiler if a placement new[] expression using
/// the CFG throws in the object constructor.
inline void operator delete[](void *Ptr, const swift::CFG &C, size_t) {}

#endif
