//===--- TFCanonicalizeCFG.h - Transform CFG into SESE regions ------------===//
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
// This defines the data structures used to represent single-entry-single-exit
// (SESE) regions, and defines the function that canonicalizes a SIL function
// into this form.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_TFCANONICALIZECFG_H
#define SWIFT_SILOPTIMIZER_TFCANONICALIZECFG_H

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/LoopInfo.h"

namespace swift {
namespace tf {
  /// This is the base class for the nodes that make up the "Single-Entry
  /// Single-Exit" (SESE) region tree that canonicalizeCFGForXLA produces.  As
  /// such, each node in the tree has a single start node and a single end node.
  class SESERegionTree {
  public:
    enum KindTy {
      SingleBlock,
      Sequence,
      WhileLoop,
      Conditional,
      Function
    };
  protected:
    KindTy kind;
    SESERegionTree(KindTy kind) : kind(kind) {}
  public:
    virtual ~SESERegionTree();
    KindTy getKind() const { return kind; }


    void dump() const;
    void print(llvm::raw_ostream &OS, unsigned indent = 0) const;
  };

  /// This region subset is a single basic block.
  class SingleBlockSESERegion : public SESERegionTree {
    SILBasicBlock *BB;
  public:
    SingleBlockSESERegion(SILBasicBlock *BB)
    : SESERegionTree(SingleBlock), BB(BB) {
    }

    SILBasicBlock *getBB() const { return BB; }

    void print(llvm::raw_ostream &OS, unsigned indent = 0) const;

    static bool classof(const SESERegionTree *n) {
      return n->getKind() == SingleBlock;
    }
  };

  /// This region represents a sequential series of regions, e.g. a single block
  /// followed by an 'if' diamond, followed by a while.
  class SequenceSESERegion : public SESERegionTree {
    SmallVector<std::unique_ptr<SESERegionTree>, 4> nodes;
  public:
    SequenceSESERegion(MutableArrayRef<std::unique_ptr<SESERegionTree>> nodes)
      : SESERegionTree(Sequence) {
      this->nodes.reserve(nodes.size());
      for (auto &n : nodes)
        this->nodes.push_back(std::move(n));
    }

    ArrayRef<std::unique_ptr<SESERegionTree>> getNodes() const {
      return nodes;
    }

    MutableArrayRef<std::unique_ptr<SESERegionTree>> takeNodes() {
      return nodes;
    }

    void addNode(std::unique_ptr<SESERegionTree> node) {
      nodes.push_back(std::move(node));
    }

    void print(llvm::raw_ostream &OS, unsigned indent = 0) const;

    static bool classof(const SESERegionTree *n) {
      return n->getKind() == Sequence;
    }
  };

  /// This region represents a sequence region that is shared between different
  /// nodes.
  class FunctionSESERegion : public SESERegionTree {
    std::shared_ptr<SESERegionTree> functionRegionTree;

  public:
    FunctionSESERegion(
        const std::shared_ptr<SESERegionTree> &functionRegionTree)
        : SESERegionTree(Function), functionRegionTree(functionRegionTree) {}

    SESERegionTree* getFunctionRegion() const { return functionRegionTree.get(); }

    void print(llvm::raw_ostream &OS, unsigned indent = 0) const;

    static bool classof(const SESERegionTree *n) {
      return n->getKind() == Function;
    }
  };

  /// This represents an XLA-style while loop.  We know that it is properly
  /// structured for conversion to XLA.  This means that the while loop has a
  /// header block.  That header block ends with a conditional jump to the exit
  /// block and the body of the loop.  The body of the loop ends with a backedge
  /// to the header.
  class WhileLoopSESERegion : public SESERegionTree {
    SILBasicBlock *preheader, *header, *exit;
    std::unique_ptr<SESERegionTree> body;
  public:
    WhileLoopSESERegion(SILBasicBlock *preheader, SILBasicBlock *header,
                        SILBasicBlock *exit,
                        std::unique_ptr<SESERegionTree> body)
      : SESERegionTree(WhileLoop), preheader(preheader), header(header),
        exit(exit), body(std::move(body)) {
    }

    SILBasicBlock *getPreheader() const { return preheader; }
    SILBasicBlock *getHeader() const { return header; }
    SILBasicBlock *getExit() const { return exit; }
    SESERegionTree *getBody() const { return body.get(); }

    void print(llvm::raw_ostream &OS, unsigned indent = 0) const;

    static bool classof(const SESERegionTree *n) {
      return n->getKind() == WhileLoop;
    }
  };

  /// This represents a conditional SESE region.  Conditional regions start with
  /// a basic block that contains a conditional branch, and may optionally have
  /// a true or false region.  The end block (which is the merge) is not
  /// considered part of this region.
  class ConditionalSESERegion : public SESERegionTree {
    SILBasicBlock *branchBB;
    std::unique_ptr<SESERegionTree> trueRegion, falseRegion;
  public:
    ConditionalSESERegion(SILBasicBlock *branchBB,
                          std::unique_ptr<SESERegionTree> trueRegion,
                          std::unique_ptr<SESERegionTree> falseRegion)
      : SESERegionTree(Conditional), branchBB(branchBB),
        trueRegion(std::move(trueRegion)),
        falseRegion(std::move(falseRegion)) {
    }

    SILBasicBlock *getBranchBB() const { return branchBB; }

    /// Return the region if the condition is true.
    /// NOTE: This returns null if there is no true region.
    SESERegionTree *getTrue() const {
      return trueRegion.get();
    }
    /// Return the region if the condition is false.
    /// NOTE: This returns null if there is no false region.
    SESERegionTree *getFalse() const {
      return falseRegion.get();
    }
    void print(llvm::raw_ostream &OS, unsigned indent = 0) const;

    static bool classof(const SESERegionTree *n) {
      return n->getKind() == Conditional;
    }
  };

  // Our partitioning and other transformations can leave around lots of
  // unconditional branches between blocks that formerly had control edges.  Go
  // through and merge those to make later passes simpler.
  bool contractUncondBranches(SILFunction *fn, DominanceInfo *DI,
                              SILLoopInfo *LI);

  /// Transform the function into a properly nested series of
  /// single-entry-single-exit regions and return the data structure that
  /// reflects it.
  std::unique_ptr<SESERegionTree> canonicalizeCFGForXLA(SILFunction *F);

} // end namespace tf
} // end namespace swift
#endif
