//===--- IRGenSIL.h - IR Generation from SIL --------------------*- C++ -*-===//
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
// This file defines the structure used to generate the IR body of a
// function from its SIL representation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGENSIL_H
#define SWIFT_IRGEN_IRGENSIL_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Type.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/CallingConv.h"
#include "IRBuilder.h"
#include "IRGenFunction.h"
#include "JumpDest.h"
#include <map>

namespace swift {
namespace irgen {

struct PartialCall {
  CallEmission emission;
  unsigned remainingCurryLevels;
};
  
/// Represents a SIL value lowered to IR, either as an Explosion of (unmanaged)
/// LLVM Values, or as a CallEmission in progress. (The CallEmission case is
/// a kludge to deal with the impedance mismatch between irgen's current
/// handling of CallEmissions as complete expressions vs. SIL's CFG-like
/// representation.)
struct LoweredValue {
  enum class Kind {
    Explosion,
    PartialCall
  };
  
  Kind kind;
  
private:
  union {
    Explosion explosion;
    PartialCall partialCall;
  };

public:
  LoweredValue(Explosion &&explosion)
    : kind(Kind::Explosion), explosion(std::move(explosion))
  {}
  
  LoweredValue(PartialCall &&call)
    : kind(Kind::PartialCall), partialCall(std::move(call))
  {}
  
  LoweredValue(LoweredValue &&lv)
    : kind(lv.kind) {
    switch (kind) {
    case Kind::Explosion:
      new (&explosion) Explosion(std::move(lv.explosion));
      break;
    case Kind::PartialCall:
      new (&partialCall) PartialCall(std::move(lv.partialCall));
      break;
    }
  }
  
  Explosion &getExplosion() {
    assert(kind == Kind::Explosion && "not an explosion");
    return explosion;
  }
  
  PartialCall &getPartialCall() {
    assert(kind == Kind::PartialCall && "not a partial call");
    return partialCall;
  }
  
  ~LoweredValue() {
    switch (kind) {
    case Kind::Explosion:
      // Work around Explosion's "empty" sanity check by discarding all the
      // managed values.
      explosion.claimAll();
      explosion.~Explosion();
      break;
    case Kind::PartialCall:
      partialCall.~PartialCall();
      break;
    }
  }
};

/// Visits a SIL Function and generates LLVM IR.
class IRGenSILFunction :
  public IRGenFunction, public SILVisitor<IRGenSILFunction>
{
  // FIXME: using std::map because DenseMap doesn't handle non-copyable value
  // types.
  ::std::map<swift::Value, LoweredValue> loweredValues;
  
public:
  IRGenSILFunction(IRGenModule &IGM,
                   CanType t,
                   ArrayRef<Pattern*> p,
                   llvm::Function *fn);
  ~IRGenSILFunction();
  
  /// Generate IR for the given SIL Function.
  void emitSILFunction(swift::Function *f);
  
  /// Create a new Explosion corresponding to the given SIL value.
  Explosion &newLoweredExplosion(swift::Value v) {
    auto inserted = loweredValues.emplace(v, LoweredValue{
      Explosion(ExplosionKind::Minimal)
    });
    assert(inserted.second && "already had lowered value for sil value?!");
    return inserted.first->second.getExplosion();
  }
  
  /// Create a new PartialCall corresponding to the given SIL value.
  PartialCall &newLoweredPartialCall(swift::Value v,
                                     unsigned naturalCurryLevel,
                                     CallEmission &&emission) {
    auto inserted = loweredValues.emplace(v, LoweredValue{
      PartialCall{std::move(emission), naturalCurryLevel+1}
    });
    assert(inserted.second && "already had lowered value for sil value?!");
    return inserted.first->second.getPartialCall();
  }
  
  PartialCall &moveLoweredPartialCall(swift::Value v,
                                      PartialCall &&parent) {
    auto inserted = loweredValues.emplace(v, LoweredValue{std::move(parent)});
    assert(inserted.second && "already had lowered value for sil value?!");
    return inserted.first->second.getPartialCall();
  }
  
  /// Get the Explosion corresponding to the given SIL value, which must
  /// previously exist.
  LoweredValue &getLoweredValue(swift::Value v) {
    auto foundExplosion = loweredValues.find(v);
    assert(foundExplosion != loweredValues.end() &&
           "no lowered explosion for sil value!");
    return foundExplosion->second;
  }
  
  Explosion &getLoweredExplosion(swift::Value v) {
    return getLoweredValue(v).getExplosion();
  }
  PartialCall &getLoweredPartialCall(swift::Value v) {
    return getLoweredValue(v).getPartialCall();
  }
  
  //===--------------------------------------------------------------------===//
  // SIL instruction lowering
  //===--------------------------------------------------------------------===//

  void visitBasicBlock(BasicBlock *BB);
  
  void visitInstruction(Instruction *i) {
    i->dump();
    llvm_unreachable("irgen for SIL instruction not yet implemented");
  }
  
  /* A minimal set of instructions to implement to make hello world work. */
  void visitTupleInst(TupleInst *i);
  void visitConstantRefInst(ConstantRefInst *i);
  void visitMetatypeInst(MetatypeInst *i);
  void visitApplyInst(ApplyInst *i);
  void visitStringLiteralInst(StringLiteralInst *i);
  void visitExtractInst(ExtractInst *i);
  void visitReturnInst(ReturnInst *i);
  
  //===--------------------------------------------------------------------===//
  // Helpers
  //===--------------------------------------------------------------------===//
  void emitCall(LoweredValue &lv, Explosion &out);
};

} // end namespace irgen
} // end namespace swift

#endif
