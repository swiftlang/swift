//===--- CFG.h - SIL control-flow graph utilities ---------------*- C++ -*-===//
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
//
// This file provides basic declarations and utilities for working with
// SIL basic blocks as a control-flow graph.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_CFG_H
#define SWIFT_SIL_CFG_H

#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/GraphTraits.h"

namespace llvm {

//===----------------------------------------------------------------------===//
// GraphTraits for SILBasicBlock
//===----------------------------------------------------------------------===//

template <> struct GraphTraits<swift::SILBasicBlock *> {
  using ChildIteratorType = swift::SILBasicBlock::succblock_iterator;
  using Node = swift::SILBasicBlock;
  using NodeRef = Node *;

  static NodeRef getEntryNode(NodeRef BB) { return BB; }

  static ChildIteratorType child_begin(NodeRef N) {
    return N->succblock_begin();
  }
  static ChildIteratorType child_end(NodeRef N) { return N->succblock_end(); }
};

template <> struct GraphTraits<const swift::SILBasicBlock*> {
  using ChildIteratorType = swift::SILBasicBlock::const_succblock_iterator;
  using Node = const swift::SILBasicBlock;
  using NodeRef = Node *;

  static NodeRef getEntryNode(NodeRef BB) { return BB; }

  static ChildIteratorType child_begin(NodeRef N) {
    return N->succblock_begin();
  }
  static ChildIteratorType child_end(NodeRef N) { return N->succblock_end(); }
};

template <> struct GraphTraits<Inverse<swift::SILBasicBlock*> > {
  using ChildIteratorType = swift::SILBasicBlock::pred_iterator;
  using Node = swift::SILBasicBlock;
  using NodeRef = Node *;
  static NodeRef getEntryNode(Inverse<swift::SILBasicBlock *> G) {
    return G.Graph;
  }
  static inline ChildIteratorType child_begin(NodeRef N) {
    return N->pred_begin();
  }
  static inline ChildIteratorType child_end(NodeRef N) {
    return N->pred_end();
  }
};

template <> struct GraphTraits<Inverse<const swift::SILBasicBlock*> > {
  using ChildIteratorType = swift::SILBasicBlock::pred_iterator;
  using Node = const swift::SILBasicBlock;
  using NodeRef = Node *;

  static NodeRef getEntryNode(Inverse<const swift::SILBasicBlock *> G) {
    return G.Graph;
  }
  static inline ChildIteratorType child_begin(NodeRef N) {
    return N->pred_begin();
  }
  static inline ChildIteratorType child_end(NodeRef N) {
    return N->pred_end();
  }
};

template <>
struct GraphTraits<swift::SILFunction *>
    : public GraphTraits<swift::SILBasicBlock *> {
  using GraphType = swift::SILFunction *;
  using NodeRef = swift::SILBasicBlock *;

  static NodeRef getEntryNode(GraphType F) { return &F->front(); }

  using nodes_iterator = pointer_iterator<swift::SILFunction::iterator>;
  static nodes_iterator nodes_begin(GraphType F) {
    return nodes_iterator(F->begin());
  }
  static nodes_iterator nodes_end(GraphType F) {
    return nodes_iterator(F->end());
  }
  static unsigned size(GraphType F) { return F->size(); }
};

template <> struct GraphTraits<Inverse<swift::SILFunction*> >
    : public GraphTraits<Inverse<swift::SILBasicBlock*> > {
  using GraphType = Inverse<swift::SILFunction *>;
  using NodeRef = NodeRef;

  static NodeRef getEntryNode(GraphType F) { return &F.Graph->front(); }

  using nodes_iterator = pointer_iterator<swift::SILFunction::iterator>;
  static nodes_iterator nodes_begin(GraphType F) {
    return nodes_iterator(F.Graph->begin());
  }
  static nodes_iterator nodes_end(GraphType F) {
    return nodes_iterator(F.Graph->end());
  }
  static unsigned size(GraphType F) { return F.Graph->size(); }
};

} // end llvm namespace

#endif

