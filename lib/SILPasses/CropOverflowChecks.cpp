//===-- CropOverflowChecks.cpp - Delete masked overflow checkes -*- C++ -*-===//
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
// Remove overflow checks that are guarded by control flow or other
// overflow checks.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "crop-overflow-checks"

#include "swift/SIL/Dominance.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

class CropOverflowChecksPass : public SILFunctionTransform {
public:
  CropOverflowChecksPass() {}

  /// This enum represents a relationship between two operands.
  /// The relationship represented by arithmetic operators represent the
  /// information that the operation did not trap.
  ///
  /// The following code translate (with the correct signedness prefix):
  ///
  /// if (x > 2) { x }           -> LT(2, x)
  /// if (x > 2) {} else { x }   -> LE(x, 2)
  /// x - 2                      -> Sub(x, 2)
  /// 2 - x                      -> Sub(2, x)
  /// 2 * x                      -> Mul(2, x)
  /// x + y                      -> Add(x, y)
  enum class ValueRelation {EQ, ULT, ULE, UAdd, USub, UMul,
                                SLT, SLE, SAdd, SSub, SMul};

  /// This struct represents a constraint on the range of some values in some
  /// basic blocks in the program.
  /// For example, it can represent the constraint "X < 2" for some blocks in
  /// the function.
  struct Constraint {
    Constraint(SILBasicBlock *BB, SILValue L, SILValue R, ValueRelation Rel) :
      DominatingBlock(BB), Left(L), Right(R), Relationship(Rel) {}

    /// The constraint is valid blocks dominated by this block.
    SILBasicBlock *DominatingBlock;
    /// The first operand.
    SILValue Left;
    /// The second operand.
    SILValue Right;
    /// Describes the relationship between the operands.
    ValueRelation Relationship;

    /// Print the content of the constraint.
    void dump() {
      llvm::dbgs()<<"Constraint [" << DominatingBlock <<"]\n";
      llvm::dbgs()<<"  Relationship:";
      switch (Relationship) {
        case ValueRelation::EQ:   llvm::dbgs()<<"Equal\n"; break;
        case ValueRelation::SLT:  llvm::dbgs()<<"SLT\n"; break;
        case ValueRelation::ULT:  llvm::dbgs()<<"ULT\n"; break;
        case ValueRelation::SLE:  llvm::dbgs()<<"SLE\n"; break;
        case ValueRelation::ULE:  llvm::dbgs()<<"ULE\n"; break;
        case ValueRelation::SMul: llvm::dbgs()<<"SMul\n"; break;
        case ValueRelation::SSub: llvm::dbgs()<<"SSub\n"; break;
        case ValueRelation::SAdd: llvm::dbgs()<<"SAdd\n"; break;
        case ValueRelation::UMul: llvm::dbgs()<<"UMul\n"; break;
        case ValueRelation::USub: llvm::dbgs()<<"USub\n"; break;
        case ValueRelation::UAdd: llvm::dbgs()<<"UAdd\n"; break;
      }
      llvm::dbgs()<<"  Left:"; Left->dump();
      llvm::dbgs()<<"  Right:"; Right->dump();
    }
  };

  void run() override {
  }

  StringRef getName() override { return "Removes overflow checks that are proven to be redundant"; }
};
}

SILTransform *swift::createCropOverflowChecks() {
  return new CropOverflowChecksPass();
}
