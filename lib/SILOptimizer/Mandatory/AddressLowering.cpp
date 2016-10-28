//===--- AddressLowering.cpp - Lower SIL address-only types. --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// This pass lowers SILTypes. On completion, the SILType of every SILValue is
// its SIL storage type. A SIL storage type is always an address type for values
// that require indirect storage at the LLVM IR level. Consequently, this pass
// is required for IRGen. It is a mandatory IRGen preparation pass (not a
// diagnostic pass).
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "address-lowering"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// AddressLowering: Top Level Function Transform.
//===----------------------------------------------------------------------===//

namespace {
class AddressLowering : public SILModuleTransform {
  /// The entry point to this function transformation.
  void run() override;

  StringRef getName() override { return "Address Lowering"; }
};
} // end anonymous namespace

/// The entry point to this function transformation.
void AddressLowering::run() {
  // Set the SIL state before the PassManager has a chance to run
  // verification.
  getModule()->setStage(SILStage::Lowered);
}

SILTransform *swift::createAddressLowering() { return new AddressLowering(); }
