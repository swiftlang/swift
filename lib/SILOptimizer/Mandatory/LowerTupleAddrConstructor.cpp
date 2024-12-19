//===--- LowerTupleAddrConstructor.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                         MARK: Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

static bool peepholeTupleDestructorOperand(TupleAddrConstructorInst *ctor) {

  // (%7, ...) = destructure_tuple %6 : $(A, ...)
  // tuple_addr_constructor [assign] %9 : $*(A, ...) with (%7 : $A, ...)
  //   =>
  // store [assign] %6 to %9

  auto numTupleElts = ctor->getNumElements();
  if (ctor->getNumElements() == 0)
    return false;

  auto multiVal = dyn_cast<MultipleValueInstructionResult>(ctor->getElement(0));
  if (!multiVal) {
    return false;
  }
  auto destructure = dyn_cast<DestructureTupleInst>(multiVal->getParent());
  if (!destructure) {
    return false;
  }

  if (destructure->getNumResults() != numTupleElts) {
    return false;
  }

  for (unsigned i = 0; i < numTupleElts; ++i) {
    if (destructure->getResult(i) != ctor->getElement(i)) {
      return false;
    }
    if (!destructure->getResult(i)->getSingleUse()) {
      return false;
    }
  }
  if (ctor->getDest()->getType().getObjectType() !=
      destructure->getOperand()->getType())
    return false;

  // Okay now we can peephole this to an assign.
  SILBuilderWithScope b(ctor);
  b.emitStoreValueOperation(ctor->getLoc(),
                            destructure->getOperand(), ctor->getDest(),
                            bool(ctor->isInitializationOfDest()) ?
                              StoreOwnershipQualifier::Init
                            : StoreOwnershipQualifier::Assign);
  ctor->eraseFromParent();
  destructure->eraseFromParent();
  return true;
}

class LowerTupleAddrConstructorTransform : public SILFunctionTransform {
  void run() override {
    SILFunction *function = getFunction();

    // Once we have finished, lower all tuple_addr_constructor that we see.
    bool deletedInst = false;
    for (auto &block : *function) {
      for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
        auto *inst = dyn_cast<TupleAddrConstructorInst>(&*ii);
        ++ii;

        if (!inst)
          continue;

        // (tuple_addr_constructor [assign/init] %addr,
        //                         (destructure_tuple %tuple))
        // ->
        // (store [assign/init] %tuple to %addr)
        if (peepholeTupleDestructorOperand(inst)) {
          continue;
        }

        SILBuilderWithScope builder(inst);

        unsigned count = 0;
        visitExplodedTupleValue(
            inst->getDest(),
            [&](SILValue value, std::optional<unsigned> index) -> SILValue {
              if (!index) {
                SILValue elt = inst->getElement(count);
                if (elt->getType().isAddress()) {
                  builder.createCopyAddr(inst->getLoc(), elt, value, IsTake,
                                         inst->isInitializationOfDest());
                } else {
                  builder.emitStoreValueOperation(
                      inst->getLoc(), elt, value,
                      bool(inst->isInitializationOfDest())
                          ? StoreOwnershipQualifier::Init
                          : StoreOwnershipQualifier::Assign);
                }
                ++count;
                return value;
              }
              auto *teai =
                  builder.createTupleElementAddr(inst->getLoc(), value, *index);
              return teai;
            });
        inst->eraseFromParent();
        deletedInst = true;
      }
    }

    if (deletedInst)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

} // end anonymous namespace

SILTransform *swift::createLowerTupleAddrConstructor() {
  return new LowerTupleAddrConstructorTransform();
}
