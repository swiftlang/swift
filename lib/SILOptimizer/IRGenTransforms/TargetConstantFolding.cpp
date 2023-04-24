//===--- TargetConstantFolding.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// This pass lowers loadable SILTypes. On completion, the SILType of every
// function argument is an address instead of the type itself.
// This reduces the code size.
// Consequently, this pass is required for IRGen.
// It is a mandatory IRGen preparation pass (not a diagnostic pass).
//
//===----------------------------------------------------------------------===//
///
/// This file contains a pass for target specific constant folding:
/// `TargetConstantFolding`. For details see the comments there.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "target-constant-folding"
#include "../../IRGen/IRGenModule.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::irgen;

namespace {

/// Performs constant folding for target-specific values.
///
/// Specifically, this optimization constant folds
/// ```
///   MemoryLayout<S>.size
///   MemoryLayout<S>.alignment
///   MemoryLayout<S>.stride
/// ```
/// Constant folding those expressions in the middle of the SIL pipeline
/// enables other optimizations to e.g. allow such expressions in statically
/// allocated global variables (done by the GlobalOpt pass).
class TargetConstantFolding : public SILModuleTransform {
private:
  /// The entry point to the transformation.
  void run() override {
    SILModule *module = getModule();

    auto *irgenOpts = module->getIRGenOptionsOrNull();
    if (!irgenOpts)
      return;
    
    // We need an IRGenModule to get the actual sizes from type lowering.
    // Creating an IRGenModule involves some effort. Therefore this is a
    // module pass rather than a function pass so that this one-time setup
    // only needs to be done once and not for all functions in a module.
    IRGenerator irgen(*irgenOpts, *module);
    auto targetMachine = irgen.createTargetMachine();
    if (!targetMachine)
      return;
    IRGenModule IGM(irgen, std::move(targetMachine));

    // Scan all instructions in the module for constant foldable instructions.
    for (SILFunction &function : *module) {
    
      bool changed = false;
      for (SILBasicBlock &block : function) {
        InstructionDeleter deleter;

        for (SILInstruction &inst : block.deletableInstructions()) {
          if (constFold(&inst, IGM)) {
            deleter.forceDelete(&inst);
            changed = true;
          }
        }
        deleter.cleanupDeadInstructions();
      }
      if (changed) {
        invalidateAnalysis(&function, SILAnalysis::InvalidationKind::Instructions);
      }
    }
  }
 
  /// Constant fold a single instruction.
  ///
  /// Returns true if `inst` was replaced and can be deleted.
  bool constFold(SILInstruction *inst, IRGenModule &IGM) {
    auto *bi = dyn_cast<BuiltinInst>(inst);
    if (!bi)
      return false;
    
    llvm::Constant *c = nullptr;
    uint64_t offset = 0;

    switch (bi->getBuiltinInfo().ID) {
      case BuiltinValueKind::Sizeof:
        c = getTypeInfoOfBuiltin(bi, IGM).getStaticSize(IGM);
        break;
      case BuiltinValueKind::Alignof:
        c = getTypeInfoOfBuiltin(bi, IGM).getStaticAlignmentMask(IGM);
        // The constant is the alignment _mask_. We get the actual alignment by
        // adding 1.
        offset = 1;
        break;
      case BuiltinValueKind::Strideof:
        c = getTypeInfoOfBuiltin(bi, IGM).getStaticStride(IGM);
        break;
      default:
        return false;
    }
    auto *intConst = dyn_cast_or_null<llvm::ConstantInt>(c);
    if (!intConst)
      return false;

    APInt value = intConst->getValue();
    value += APInt(value.getBitWidth(), offset);
    auto intTy = bi->getType().getAs<BuiltinIntegerType>();
    if (!intTy)
      return false;

    // The bit widths can differ if we are compiling for a 32 bit target.
    if (value.getActiveBits() > intTy->getGreatestWidth()) {
      // It's unlikely that a size/stride overflows 32 bits, but let's be on
      // the safe side and catch a potential overflow.
      return false;
    }
    value = value.sextOrTrunc(intTy->getGreatestWidth());

    // Replace the builtin by an integer literal.
    SILBuilderWithScope builder(bi);
    auto *intLit = builder.createIntegerLiteral(bi->getLoc(), bi->getType(),
                                                value);
    bi->replaceAllUsesWith(intLit);
    return true;
  }

  const TypeInfo &getTypeInfoOfBuiltin(BuiltinInst *bi, IRGenModule &IGM) {
    SubstitutionMap subs = bi->getSubstitutions();
    SILType lowered = IGM.getLoweredType(AbstractionPattern::getOpaque(),
                                         subs.getReplacementTypes()[0]);
    return IGM.getTypeInfo(lowered);
  }
};

} // end anonymous namespace

SILTransform *swift::createTargetConstantFolding() {
  return new TargetConstantFolding();
}
