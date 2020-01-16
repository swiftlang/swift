//===--- DifferentiationInvoker.cpp ---------------------------*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// SWIFT_ENABLE_TENSORFLOW
//
// Class that represents the invoker of a differentiation task.
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/Differentiation/DifferentiationInvoker.h"
#include "swift/SIL/SILDifferentiabilityWitness.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {
namespace autodiff {

SourceLoc DifferentiationInvoker::getLocation() const {
  switch (kind) {
  case Kind::DifferentiableFunctionInst:
    return getDifferentiableFunctionInst()->getLoc().getSourceLoc();
  case Kind::IndirectDifferentiation:
    return getIndirectDifferentiation().first->getLoc().getSourceLoc();
  case Kind::SILDifferentiabilityWitnessInvoker:
    return getSILDifferentiabilityWitnessInvoker()
        ->getOriginalFunction()
        ->getLocation()
        .getSourceLoc();
  }
}

void DifferentiationInvoker::print(llvm::raw_ostream &os) const {
  os << "(differentiation_invoker ";
  switch (kind) {
  case Kind::DifferentiableFunctionInst:
    os << "differentiable_function_inst=(" << *getDifferentiableFunctionInst()
       << ")";
    break;
  case Kind::IndirectDifferentiation: {
    auto indDiff = getIndirectDifferentiation();
    os << "indirect_differentiation=(" << *std::get<0>(indDiff) << ')';
    // TODO: Enable printing parent invokers.
    // May require storing a `DifferentiableInvoker *` in the
    // `IndirectDifferentiation` case.
    /*
    SILInstruction *inst;
    SILDifferentiableAttr *attr;
    std::tie(inst, attr) = getIndirectDifferentiation();
    auto invokerLookup = invokers.find(attr); // No access to ADContext?
    assert(invokerLookup != invokers.end() && "Expected parent invoker");
    */
    break;
  }
  case Kind::SILDifferentiabilityWitnessInvoker: {
    auto witness = getSILDifferentiabilityWitnessInvoker();
    os << "sil_differentiability_witness_invoker=(witness=(";
    witness->print(os);
    os << ") function=" << witness->getOriginalFunction()->getName();
    break;
  }
  }
  os << ')';
}

} // end namespace autodiff
} // end namespace swift
