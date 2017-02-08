//===--- SwitchBuilder.h ----------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_IRGEN_SWITCHBUILDER_H
#define SWIFT_IRGEN_SWITCHBUILDER_H

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

#include "IRBuilder.h"
#include "IRGenFunction.h"

namespace swift {
namespace irgen {

/// A builder that produces an LLVM branch or switch instruction for a set
/// of destinations.
class SwitchBuilder {
private:
#ifndef NDEBUG
  /// Track whether we added as many cases as we expected to.
  unsigned CasesToAdd;
#endif

protected:
  IRBuilder Builder;
  llvm::Value *Subject;

  /// Protected initializer. Clients should use the `create` factory method.
  SwitchBuilder(IRGenFunction &IGF, llvm::Value *Subject, unsigned NumCases)
      :
#ifndef NDEBUG
        CasesToAdd(NumCases),
#endif
        // Create our own IRBuilder, so that the SwitchBuilder is always able to
        // generate the branch instruction at the right point, even if it needs
        // to collect multiple cases before building an instruction.
        Builder(IGF.IGM.getLLVMContext(), /*DebugInfo*/ false),
        Subject(Subject) {
    // Start our builder off at IGF's current insertion point.
    Builder.SetInsertPoint(IGF.Builder.GetInsertBlock(),
                           IGF.Builder.GetInsertPoint());
  }

public:
#ifndef NDEBUG
  virtual ~SwitchBuilder() {
    assert(CasesToAdd == 0 && "Did not add enough cases");
  }
#endif

  // Create a SwitchBuilder instance for a switch that will have the given
  // number of cases.
  static std::unique_ptr<SwitchBuilder> create(IRGenFunction &IGF,
                                               llvm::Value *Subject,
                                               SwitchDefaultDest Default,
                                               unsigned NumCases);

  /// Add a case to the switch.
  virtual void addCase(llvm::ConstantInt *value, llvm::BasicBlock *dest) {
#ifndef NDEBUG
    assert(CasesToAdd > 0 && "Added too many cases");
    --CasesToAdd;
#endif
  }
};

/// A builder that emits an unconditional branch for a "switch" with only
/// one destination.
class BrSwitchBuilder final : public SwitchBuilder {
private:
  friend class SwitchBuilder;

  BrSwitchBuilder(IRGenFunction &IGF, llvm::Value *Subject, unsigned NumCases)
      : SwitchBuilder(IGF, Subject, NumCases) {
    assert(NumCases == 1 && "should have only one branch");
  }

public:
  void addCase(llvm::ConstantInt *value, llvm::BasicBlock *dest) override {
    SwitchBuilder::addCase(value, dest);
    Builder.CreateBr(dest);
  }
};

/// A builder that produces a conditional branch for a "switch" with two
/// defined destinations.
class CondBrSwitchBuilder final : public SwitchBuilder {
private:
  friend class SwitchBuilder;
  llvm::BasicBlock *FirstDest;

  CondBrSwitchBuilder(IRGenFunction &IGF, llvm::Value *Subject,
                      SwitchDefaultDest Default, unsigned NumCases)
      : SwitchBuilder(IGF, Subject, NumCases),
        FirstDest(Default.getInt() == IsUnreachable ? nullptr
                                                    : Default.getPointer()) {
    assert(NumCases + (Default.getInt() == IsNotUnreachable) == 2 &&
           "should have two branches total");
  }

public:
  void addCase(llvm::ConstantInt *value, llvm::BasicBlock *dest) override {
    SwitchBuilder::addCase(value, dest);

    // If we don't have a first destination yet, save it.
    // We don't need to save the value in this case since we assume that the
    // subject must be one of the case values. We only have to test one or
    // the other.
    //
    // TODO: It may make sense to save both case values, and pick which one
    // to compare based on which value is more likely to be efficiently
    // representable in the target machine language. For example, zero
    // or a small immediate is usually cheaper to materialize than a larger
    // value that may require multiple instructions or a bigger encoding.
    if (!FirstDest) {
      FirstDest = dest;
      return;
    }

    // Otherwise, we have both destinations for the branch and a value to
    // test against. We can make the instruction now.
    if (cast<llvm::IntegerType>(Subject->getType())->getBitWidth() == 1) {
      // If the subject is already i1, we can use it directly as the branch
      // condition.
      if (value->isZero())
        Builder.CreateCondBr(Subject, FirstDest, dest);
      else
        Builder.CreateCondBr(Subject, dest, FirstDest);
      return;
    }
    // Otherwise, compare against the case value we have.
    auto test = Builder.CreateICmpNE(Subject, value);
    Builder.CreateCondBr(test, FirstDest, dest);
  }
};

/// A builder that produces a switch instruction for a switch with many
/// destinations.
class SwitchSwitchBuilder final : public SwitchBuilder {
private:
  friend class SwitchBuilder;
  llvm::SwitchInst *TheSwitch;

  SwitchSwitchBuilder(IRGenFunction &IGF, llvm::Value *Subject,
                      SwitchDefaultDest Default, unsigned NumCases)
      : SwitchBuilder(IGF, Subject, NumCases) {
    TheSwitch =
        IGF.Builder.CreateSwitch(Subject, Default.getPointer(), NumCases);
  }

public:
  void addCase(llvm::ConstantInt *value, llvm::BasicBlock *dest) override {
    SwitchBuilder::addCase(value, dest);
    TheSwitch->addCase(value, dest);
  }
};

std::unique_ptr<SwitchBuilder> SwitchBuilder::create(IRGenFunction &IGF,
                                                     llvm::Value *Subject,
                                                     SwitchDefaultDest Default,
                                                     unsigned NumCases) {
  // Pick a builder based on how many total reachable destinations we intend
  // to have.
  switch (NumCases + (Default.getInt() == IsNotUnreachable)) {
  case 0:
    // No reachable destinations. We can emit an unreachable and go about
    // our business.
    IGF.Builder.CreateUnreachable();
    return nullptr;
  case 1:
    // One reachable destination. We can emit an unconditional branch.
    // If that one branch is the default, we're done.
    if (Default.getInt() == IsNotUnreachable) {
      IGF.Builder.CreateBr(Default.getPointer());
      return nullptr;
    }

    // Otherwise, use a builder to emit the one case.
    return std::unique_ptr<SwitchBuilder>(
        new BrSwitchBuilder(IGF, Subject, NumCases));
  case 2:
    // Two reachable destinations. We can emit a single conditional branch.
    return std::unique_ptr<SwitchBuilder>(
        new CondBrSwitchBuilder(IGF, Subject, Default, NumCases));
  default:
    // Anything more, fall over into a switch.
    // TODO: Since fast isel doesn't support switch insns, we may want to
    // also support a "pre-lowered-switch" builder that builds a jump tree
    // for unoptimized builds.
    return std::unique_ptr<SwitchBuilder>(
        new SwitchSwitchBuilder(IGF, Subject, Default, NumCases));
  }
}

} // end irgen namespace
} // end swift namespace

#endif
