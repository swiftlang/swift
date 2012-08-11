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

#include "swift/CFG/Instruction.h"
#include "swift/CFG/BasicBlock.h"

namespace swift {

class Instruction;
class Stmt;
class TranslationUnit;

class CFG : public CFGBase {
public:
  typedef llvm::iplist<BasicBlock> BlockListType;

  /// The collection of all BasicBlocks in the CFG.
  BlockListType blocks;

private:
  friend class BasicBlock;

  // Intentionally marked private so that we need to use 'build()'
  // to construct a CFG.
  CFG();

public:
  ~CFG();

  /// Construct a CFG from a given statement.  It is the caller's responsibility
  /// to 'delete' this object.  This can return nullptr if the CFG cannot
  /// be constructed.
  static CFG *constructCFG(Stmt *S);

  /// Pretty-print the CFG.
  void dump() const;

  /// Dump the CFGs of an entire translation unit.
  static void dump(TranslationUnit *TU);

  /// Pretty-print the CFG with the designated stream.
  void print(raw_ostream &OS, CFGPrintContext &PC, unsigned Indent = 0) const;

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
  static NodeType *getEntryNode(::swift::CFG *C) { return &C->blocks.front(); }
  typedef ::swift::CFG::BlockListType::iterator nodes_iterator;
  static nodes_iterator nodes_begin(::swift::CFG *C) {return C->blocks.begin();}
  static nodes_iterator nodes_end(::swift::CFG *C) { return C->blocks.end(); }
  static unsigned size(::swift::CFG *C) { return C->blocks.size(); }
};

} // end llvm namespace

#endif
