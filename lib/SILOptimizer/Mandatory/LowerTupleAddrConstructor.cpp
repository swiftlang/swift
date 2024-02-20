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
