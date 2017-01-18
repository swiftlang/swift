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
template <> struct GraphTraits<swift::SILBasicBlock*> {
  typedef swift::SILBasicBlock::SuccessorListTy::iterator ChildIteratorType;
  typedef swift::SILBasicBlock *NodeRef;

  static NodeRef getEntryNode(NodeRef BB) { return BB; }

  static ChildIteratorType child_begin(NodeRef N) {
    return N->getSuccessors().begin();
  }
  static ChildIteratorType child_end(NodeRef N) {
    return N->getSuccessors().end();
  }
};

template <> struct GraphTraits<const swift::SILBasicBlock*> {
  typedef swift::SILBasicBlock::ConstSuccessorListTy::iterator ChildIteratorType;
  typedef const swift::SILBasicBlock *NodeRef;

  static NodeRef getEntryNode(NodeRef BB) { return BB; }

  static ChildIteratorType child_begin(NodeRef N) {
    return N->getSuccessors().begin();
  }
  static ChildIteratorType child_end(NodeRef N) {
    return N->getSuccessors().end();
  }
};

template <> struct GraphTraits<Inverse<swift::SILBasicBlock*> > {
  typedef swift::SILBasicBlock::pred_iterator ChildIteratorType;
  typedef swift::SILBasicBlock *NodeRef;

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
  typedef swift::SILBasicBlock::pred_iterator ChildIteratorType;
  typedef const swift::SILBasicBlock *NodeRef;

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

template <> struct GraphTraits<swift::SILFunction*>
    : public GraphTraits<swift::SILBasicBlock*> {
  typedef swift::SILFunction *GraphType;
  typedef swift::SILBasicBlock *NodeRef;

  static NodeRef getEntryNode(GraphType F) { return &F->front(); }

  typedef pointer_iterator<swift::SILFunction::iterator> nodes_iterator;
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
  typedef Inverse<swift::SILFunction *> GraphType;
  typedef NodeRef NodeRef;

  static NodeRef getEntryNode(GraphType F) { return &F.Graph->front(); }

  typedef pointer_iterator<swift::SILFunction::iterator> nodes_iterator;
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

