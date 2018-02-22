//===--- TFDeabstraction.cpp - Lowering & canonicalization for tensor ops -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This psss is in charge of lowering general code coming out of the mandatory
// SIL passes and producing a canonicalized and deabstracted standard form.
// It combines together standard techniques like inlining, generics
// specialization, and scalarization of structs and tuples.
//
// This is intended to be part of the mandatory passes, so its behavior is
// defined to be as simple and predictable as possible.  We don't want to use
// heuristic techniques to resolve virtual calls for example, we'd rather leave
// them, so the user has a simple and predictable model for what this can
// handle.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "tf-deabstraction"
#include "TFUtilities.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
using namespace swift;
using namespace tf;

namespace {
  /// This class wraps the state and logic necessary to deabstract code into one
  /// specific SIL function, which has been designated as a potential top-level
  /// host for tensor code.
  class TFDeabstraction {
    SILFunction &fn;
    TensorFunctionClassifier &tfc;
    SILPassManager *passManager;
  public:
    TFDeabstraction(SILFunction &fn, TensorFunctionClassifier &tfc,
                    SILPassManager *PM)
      : fn(fn), tfc(tfc), passManager(PM) {}

    /// Deabstract the specified top level function as a deabstraction context.
    void doIt();
  private:
    void inlineCalls();
    bool promoteToSSA();
    void prepareStackAllocForPromotion(AllocStackInst *alloc);
  };
}  // end anonymous namespace


/// Scan the function looking for call sites that should be inlined to expose
/// tensor operations, and inline them to expose those ops.  Also keep track of
/// whether we see any tensor ops - if not, there is no reason to further
/// process this function, so we can stop and exit early from other
/// canonicalizations.
void TFDeabstraction::inlineCalls() {
  // Use the mandatory inlining algorithm to expose call sites that contain
  // TensorHandle values as their argument or result lists.
  inlineForTFDeabstraction(fn,
     [&](FullApplySite site, const SILFunction &callee) -> bool {
       // Get the type of the function being called after applying substitutions
       // at the call site.
       auto type = site.getSubstCalleeType();

       // If the call we found is to something that processes TensorHandles,
       // then we want it inlined.
       bool result = tfc.containsTensorHandle(type);

       // TODO: What about Array<Tensor>'s that appear in Tensor ops?  It would
       // be nice to avoid inlining them so we can apply the higher level array
       // semantic hooks to them, and to improve compile time / avoid code
       // bloat.
       return result;
     }
  );

}

/// Scan the function looking for TensorHandle AllocStack instructions to
/// promote.  While we are scanning, notice whether we see any tensor operations
/// in the function.  If not, we return false and early exit out of the
/// transformation since we'll never have any partitioning to do.
bool TFDeabstraction::promoteToSSA() {
  SmallVector<AllocStackInst*, 16> stackAllocs;

  bool containsTensorOps = false;
  for (auto &block : fn)
    for (auto &inst : block) {
      // Check to see if this is a tensor op.
      if (!containsTensorOps)
        containsTensorOps = SILTensorOpInfo::decode(&inst).hasValue();

      // Otherwise, look for allocations of types that contain TensorHandle's.
      auto *asi = dyn_cast<AllocStackInst>(&inst);
      if (!asi) continue;

      if (tfc.containsTensorHandle(asi->getElementType().getSwiftRValueType()))
        stackAllocs.push_back(asi);
    }

  // If we didn't find any tensor ops in the function, then there is no reason
  /// to continue processing it.  Let our caller know that we're done with the
  /// function.
  if (!containsTensorOps)
    return false;

  // If there is nothing to promote, don't bother calculating dominator info.
  if (stackAllocs.empty())
    return true;

  // Do any necessary preprocessing of the stack allocations before promoting
  // them.
  for (auto alloc : stackAllocs)
    prepareStackAllocForPromotion(alloc);

  // Otherwise the function does have tensor operations, so lets promote any
  // stack allocations out of the way so we can do simple dataflow analysis.
  auto domInfo = passManager->getAnalysis<DominanceAnalysis>()->get(&fn);
  promoteAllocsToSSA(stackAllocs, domInfo);
  return true;
}

/// Preprocess the specified allocation instruction to make it more suitable for
/// promotion to SSA.  In particularly, we eliminate CopyAddrInst and other
/// uses that could prevent us from promoting this.
void TFDeabstraction::prepareStackAllocForPromotion(AllocStackInst *alloc) {
  for (auto UI = alloc->use_begin(); UI != alloc->use_end(); ) {
    auto *inst = (*UI)->getUser();

    auto &module = alloc->getModule();

    // See if this is an instruction we need to explode.
    if (auto *copyAddr = dyn_cast<CopyAddrInst>(inst)) {
      // TODO: We will eventually need to do real SRoA to handle the case when
      // we have tensor values mixed in with other random values that shouldn't
      // (or can't) be loaded.  For now, we can just fail to deabstract these
      // cases.  When we handle the general case, we should investigate whether
      // it makes sense to use TypeLowering as
      // AvailableValueDataflowContext::explodeCopyAddr does.
      if (copyAddr->getSrc()->getType().isLoadable(module)) {
        // Lower the copy into a load and store.
        SILBuilder B(copyAddr);

        auto loadOwnership = LoadOwnershipQualifier::Unqualified;
        auto storeOwnership = StoreOwnershipQualifier::Unqualified;
        if (B.getFunction().hasQualifiedOwnership()) {
          if (copyAddr->getSrc()->getType().isTrivial(module)) {
            loadOwnership = LoadOwnershipQualifier::Trivial;
          } else {
            if (copyAddr->isTakeOfSrc())
              loadOwnership = LoadOwnershipQualifier::Take;
            else
              loadOwnership = LoadOwnershipQualifier::Copy;
            if (copyAddr->isInitializationOfDest())
              storeOwnership = StoreOwnershipQualifier::Init;
            else
              storeOwnership = StoreOwnershipQualifier::Assign;
          }
        }

        auto tmp = B.createLoad(copyAddr->getLoc(), copyAddr->getSrc(),
                                loadOwnership);
        B.createStore(copyAddr->getLoc(), tmp, copyAddr->getDest(),
                      storeOwnership);
        ++UI;
        copyAddr->eraseFromParent();
        continue;
      }
    }

    ++UI;
  }
}


/// Process the specified top level function as a deabstraction context: if it
/// contains Tensor operations simplify the code using predictable rules until
/// the tensor operations are exposed in a canonical form inside of this
/// function.
///
/// We currently make use of the following techniques to do this:
///   1) Inlining.  We look for direct calls to functions that take and return
///      values of TensorHandle type, possibly wrapped by structs and tuples.
///   2) SSA Promotion of TensorHandle's.
///
/// TODO:
///   0) Move tensor op canonicalization up from tf-partition.
///   1) Generics specialization.
///   2) Struct and tuple scalarization.
///   3) Enums.  What can we reliably do with them?  Should they be out of
///      model?  We can definitely do ones without payload values.
///
void TFDeabstraction::doIt() {
  // Start by inlining functions that take and return Tensor values.
  inlineCalls();

  /// Promote stack allocations to SSA, this allows us to do dataflow analysis,
  /// and eliminates mutation from tensor values.
  if (!promoteToSSA())
    return;

  // TODO: This should give us a lot of tuples and structs that wrap a
  // TensorHandle.  Explode each of them.

  // TODO: Canonicalize the tensor ops.
}


namespace {
  class TFDeabstractionPass : public SILModuleTransform {
    TensorFunctionClassifier tfc;
  public:

    /// The entry point to the transformation, runs deabstraction on an entire
    /// module.
    void run() override;
  };
}  // end anonymous namespace

void TFDeabstractionPass::run() {
  SILModule *module = getModule();
  auto &ctx = module->getASTContext();

  // If the TensorFlow module hasn't been imported by the program, don't do
  // anything.  This avoids impacting compile time for non-TensorFlow using
  // Swift programs by doing extraneous analysis.
  auto tfModule = ctx.getLoadedModule(ctx.getIdentifier("TensorFlow"));
  if (!tfModule)
    return;


  // Loop over all of the functions in the current module processing them -
  // iff they look like they could be the top level of a deabstraction
  // context.
  for (auto &fn : *module) {

    // If this function is a building block of larger tensor programs (e.g.
    // the ops defined in the TensorFlow module), then don't transform it in
    // isolation.
    if (!tfc.shouldBePartitioned(&fn))
      return;

    // Okay, process this function.
    TFDeabstraction(fn, tfc, PM).doIt();

    // TODO(clattner): This should eventually be the driver that kicks off
    // the partitioning pass as part of it, and the partitioning and later
    // passes are just function passes that are invoked by this one.  Until
    // we are ready for that, let them run later in the pipeline after the
    // other optimization and cleanup passes.
  }
}

SILTransform *swift::createTFDeabstraction() {
  return new TFDeabstractionPass();
}
