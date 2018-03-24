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
#include "TFConstExpr.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/PrettyStackTrace.h"

using namespace swift;
using namespace tf;
using llvm::DenseMap;

static llvm::cl::opt<bool>
TFDumpDeabstractionDetails("tf-dump-deabstraction-details",
                           llvm::cl::init(false),
           llvm::cl::desc("Dump extra details about TensorFlow deabstraction"));

static llvm::cl::opt<bool>
TFCheckDeabstraction("tf-check-deabstraction", llvm::cl::init(false),
           llvm::cl::desc("Check that the output of deabstraction is valid"));


template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag, U &&...args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

/// Delete the specified instruction (e.g. like inst->eraseFromParent()), but
/// also check to see if this instruction was the last use of any code that can
/// be trivially deleted.  If so, remove that trivially dead code.
static void deleteInstAndAbandonedUses(SILInstruction *inst) {
  for (auto &operand : inst->getAllOperands()) {
    auto opInst = operand.get()->getDefiningInstruction();
    operand.drop();

    if (opInst && !opInst->hasUsesOfAnyResult())
      recursivelyDeleteTriviallyDeadInstructions(opInst);
  }

  // Finally, delete the instruction itself.
  inst->eraseFromParent();
}

namespace {
  /// This class wraps the state and logic necessary to deabstract code into one
  /// specific SIL function, which has been designated as a potential top-level
  /// host for tensor code.
  class TFDeabstraction {
    SILFunction &fn;
    TensorFunctionClassifier &tfc;
    ConstExprEvaluator &constantEvaluator;
    SILPassManager *passManager;

    /// This is set to true by the early inlining phase if the function was
    /// forcibly flattened to make all references to global variables visible
    /// within the current function.  This is done for top level code in
    /// Playgrounds and the REPL.
    bool forciblyFlattened = false;

    /// This keeps track of whether we've ever changed this function through the
    /// 'aboutToChangeFunction' method.  This enables it to print debug log info
    /// only for interesting functions.
    bool changedFunction = false;

    /// This is the list of tensor operations in the current function, filled in
    /// by simplifyTensorOperands.  This contains both the builtin instructions
    /// that reflect the #tfop() invocations, as well as any retain/release
    /// instructions using TensorHandle values.
    SmallVector<SILInstruction*, 32> tensorOps;
  public:
    TFDeabstraction(SILFunction &fn, TensorFunctionClassifier &tfc,
                    ConstExprEvaluator &constantEvaluator, SILPassManager *PM)
      : fn(fn), tfc(tfc), constantEvaluator(constantEvaluator), passManager(PM){
    }

    /// Deabstract the specified top level function as a deabstraction context.
    void doIt();

    /// This function is called on key entrypoints that mutate the SIL function.
    /// This just exists to reduce the amount of debug spew to focus on the
    /// functions that matter.
    void aboutToChangeFunction() {
      // If we already changed the function then no need to print again.
      if (changedFunction) return;
      changedFunction = true;

      logCurrentState("Input", /*detailed*/false);
    }
  private:
    void logCurrentState(const char *name, bool isDetailed);
    void inlineCalls();
    void simplifyTensorOperands();

    void promoteGlobalsToStack(ArrayRef<SILGlobalVariable*> globals,
                               SmallVectorImpl<AllocStackInst*> &stackAllocs);
    void promoteToSSA(MutableArrayRef<AllocStackInst*> allocs);
    void prepareStackAllocForPromotion(AllocStackInst *alloc);
    void propagateTensorValues();
    void checkAndCanonicalizeAttributes();
  };
}  // end anonymous namespace

void TFDeabstraction::logCurrentState(const char *name, bool isDetailed) {
  // If this is detailed information and no-one asked for it, early out.
  if (isDetailed && !TFDumpDeabstractionDetails)
    return;

  auto outs = getTFDumpIntermediateStream();
  if (!outs) return;

  *outs << "--- TFDeabstraction " << name << ": " << fn.getName() << "\n";
  fn.print(*outs);
  *outs << "----\n";
  outs->flush();
}


/// Return true if this is a "array.uninitialized" call, which creates an array
/// and returns it with uninitialized elements for the caller to fill in.
static bool isArrayUninitialized(SILInstruction *call) {
  auto *apply = dyn_cast<ApplyInst>(call);
  if (!apply) return false;
  auto semantics = ArraySemanticsCall(apply, "array.uninitialized");
  return semantics.getKind() == ArrayCallKind::kArrayUninitialized;
}

/// Scan the function looking for call sites that should be inlined to expose
/// tensor operations, and inline them to expose those ops.
void TFDeabstraction::inlineCalls() {
  llvm::PrettyStackTraceFormat X("TFDeabstraction::inlineCalls");

  // We generally want to carefully and deliberately choose which functions to
  // inline into our 'fn' function, but if this is a main function with top
  // level code (e.g. in a playground) then we want to aggressively inline
  // when/if we see any global_addr's with a TensorHandle in them.  This allows
  // us to promote these global_addrs to registers safely.
  //
  // TODO: This should be enough for now, but isn't really the right long term
  // approach.  Long term we should build a full callgraph and look for call
  // paths that can touch tensor flavored global variables.  If a function
  // doesn't do so, then there is no reason to inline it.  This can start to
  // matter for larger examples.
  //
  // TODO: This should handle playgrounds and #! scripts, but probably isn't
  // enough to handle REPL generated code.  How do we identify the functions it
  // produces for each entered statement?  Matching on __repl or whatever prefix
  // LLDB and the integrated REPL use is probably enough.
  //
  if (fn.getName() == SWIFT_ENTRY_POINT_FUNCTION) {
    forciblyFlattened = [&]() -> bool {
      for (auto &bb : fn)
        for (auto &i : bb)
          if (auto *inst = dyn_cast<GlobalAddrInst>(&i)) {
            if (tfc.containsTensorFlowValue(inst->getType()))
              return true;
          }
      return false;
    }();
  }

  /// This predicate decides whether we should mandatory inline the specified
  /// call site.
  auto shouldInline = [&](FullApplySite site) -> bool {
    // If this is a call of an explicitly noinline function, don't inline it!
    if (auto *apply = dyn_cast<ApplyInst>(site.getInstruction()))
      if (auto *callee = apply->getCalleeFunction())
        if (callee->getInlineStrategy() == NoInline)
          return false;

    // Check for array internals which we could be inlined, but prefer to
    // leave in abstracted form for easier analysis.  For things like
    // Tensor<Float>([[1,2],[3,4]]), we prefer to see higher level array
    // construction calls beacuse we end up removing them anyway.
    if (isArrayUninitialized(site.getInstruction()))
      return false;

    // If we're forcibly flattening code into the top level function, and if the
    // callee is in the same source file as that top-level function (and thus
    // has visibility into its global variables) then force inline it.
    if (forciblyFlattened) {
      if (auto *apply = dyn_cast<ApplyInst>(site.getInstruction())) {
        if (auto *callee = apply->getCalleeFunction()) {
          // FIXME: We will miscompile functions that use variables in top level
          // code right now.  We need to implement this properly.
#if 0
          if (shouldBeForciblyFlattened(*callee)) {
            // Recognize that we're about to change this function.
            aboutToChangeFunction();
            return true;
          }
#endif
        }
      }
    }

    // Get the type of the function being called after applying substitutions
    // at the call site.
    auto type = site.getSubstCalleeType();

    // If the call we found is to something that processes TensorFlow values,
    // then we want it inlined.
    if (!tfc.containsTensorFlowValue(type))
      return false;

    // Recognize that we're about to change this function.
    aboutToChangeFunction();
    return true;
  };

  SmallPtrSet<SILFunction*, 16> inlinedCallees;

  // Use the mandatory inlining algorithm to expose call sites that contain
  // TensorFlow values as their argument or result lists.
  inlineForTFDeabstraction(fn,
     [&](FullApplySite site, const SILFunction &callee) -> bool {
       if (!shouldInline(site))
         return false;

       // Recognize that we're about to change this function.
       aboutToChangeFunction();
       inlinedCallees.insert(const_cast<SILFunction*>(&callee));
       return true;
     }
  );

  auto &module = fn.getModule();
  module.invalidateSILLoaderCaches();

  // Now that we've inlined some functions, clean them up to avoid burning
  // compile time in later passes.  We do this with a simple linear scan,
  // because functions that reference each other have already been flattened
  // so there should be no interdependencies.
  for (auto *callee : inlinedCallees) {
    // We shouldn't be trying to delete the thing we're inlining into, doing so
    // would invalidate iterators.
    assert(callee != &fn && "inlining self into self??");

    passManager->invalidateAnalysis(callee,
                                    SILAnalysis::InvalidationKind::Everything);

    // We can't delete this function if something is still using it.  That could
    // be because there is some other tensor program in this module that is
    // using it or (most likely) that there is a now-dead witness table.
    //
    // TODO: Build infra to find unused witness tables and remove them.
    if (callee->getRefCount() != 0) {

      // FIXME: As a super hack, disable all optimization of the inlined
      // methods that are defined and kept alive by the DifferentiableModule
      // abstraction in the TensorFlow module.  These don't get removed because
      // of the witness tables for DifferentiableModule, but we know that they
      // don't matter.  Don't burn time optimizing them.
      if (callee->getName().contains("adjoint3for4with") || //adjoint(for:with:
          callee->getName().contains("6primal3for")) // primal(for:)
        callee->setOptimizationMode(OptimizationMode::NoOptimization);

      continue;
    }

    // If this is a public function then we can't remove it either.
    if (callee->isPossiblyUsedExternally())
      continue;

    // ObjC functions are called through the runtime and are therefore alive
    // even if not referenced inside SIL.
    if (callee->getRepresentation() ==SILFunctionTypeRepresentation::ObjCMethod)
      continue;

    passManager->notifyDeleteFunction(callee);

    // Okay, erase the function from the module.
    module.eraseFunction(callee);
  }
}

/// If the specified value is a StructInst that has one operand, or potentially
/// a chain of them, dig through and return the underlying value inside of it.
static SILValue lookThroughSingleElementStructInsts(SILValue value) {
  if (auto *str = dyn_cast_or_null<StructInst>(value->getDefiningInstruction()))
    if (str->getNumOperands() == 1)
      return lookThroughSingleElementStructInsts(str->getOperand(0));
  return value;
}

/// Scan the operand list of the builtin.  If any operand is passed indirectly
/// (i.e., an address of a stack location is passed instead of the value itself)
/// then rewrite the builtin to use a loaded version of that value.
///
/// Similarly, if a primitive integer or floating point value is passed as a
/// struct value, extract out the underlying integer or float value.
///
static BuiltinInst *simplifyOperands(BuiltinInst *inst, TFDeabstraction &TFDA) {
  /// Return a VarDecl if this is a struct wrapping a single field which is a
  /// primitive integer or floating point value.  We accept multiple layers of
  /// struct wrappers as well, but return the decl for the top level field
  /// type.  This returns null in any other case.
  auto getPrimitiveStructField = [&](Type type) -> VarDecl* {
    VarDecl *result = nullptr;
    while (1) {
      auto decl = type->getAnyNominal();
      if (!decl || !isa<StructDecl>(decl)) return nullptr;

      // Check to see if there is a single stored field.
      auto fieldIt = decl->getStoredProperties().begin();
      if (fieldIt == decl->getStoredProperties().end()) return nullptr;

      // If this is the top level of the struct, retain the field decl.
      if (result == nullptr) result = *fieldIt;

      type = (*fieldIt++)->getType();
      if (fieldIt != decl->getStoredProperties().end()) return nullptr;

      // If we unwrapped a level and got to a builtin type, then this is a
      // wrapper.
      if (type->is<BuiltinIntegerType>() ||
          type->is<BuiltinFloatType>())
        return result;
    }
  };

  // Predicate that returns true if the specified type is an address type for
  // a loadable (non-address-only) value.
  auto isLoadableAddressType = [&](SILType type) -> bool {
    return type.isAddress() && type.isLoadable(inst->getModule());
  };

  // Predicate that returns true if an operand of the specified type should be
  // rewritten - either to load an address argument or expand a struct
  // parameter.
  auto canSimplifyOperand = [&](SILType type) -> bool {
    return isLoadableAddressType(type) ||
           getPrimitiveStructField(type.getSwiftRValueType()) != nullptr;
  };

  // If we don't have to change any operands, don't rewrite the builtin.
  bool mustChangeBuiltin = false;
  for (auto &op : inst->getAllOperands()) {
    if (canSimplifyOperand(op.get()->getType())) {
      mustChangeBuiltin = true;
      break;
    }
  }

  if (!mustChangeBuiltin) return inst;

  // Mark the function as being mutated.
  TFDA.aboutToChangeFunction();

  // Okay, we do have to simplify something.  Scan through and rewrite operands.
  SILBuilder B(inst);
  SmallVector<SILValue, 8> operands;
  for (auto &op : inst->getAllOperands()) {
    auto operand = op.get();
    // If this is an address operand, emit a load of the value.
    if (isLoadableAddressType(operand->getType())) {
      bool hasOwnership = inst->getFunction()->hasQualifiedOwnership();
      auto loadOwnership = hasOwnership ? LoadOwnershipQualifier::Trivial
                                        : LoadOwnershipQualifier::Unqualified;
      auto load = B.createLoad(inst->getLoc(), operand, loadOwnership);

      load->setDebugLocation(inst->getDebugLocation());
      operand = load;
    }

    // If the operand is a StructInst building the value that we want to
    // extract, just get the element out of it, to avoid generating bloated IR.
    operand = lookThroughSingleElementStructInsts(operand);

    // If this is a struct value, emit struct extraction instruction(s).
    while (auto fieldDecl = getPrimitiveStructField(
                                     operand->getType().getSwiftRValueType())) {
      auto extract = B.createStructExtract(inst->getLoc(), operand, fieldDecl);
      extract->setDebugLocation(inst->getDebugLocation());
      operand = extract;
    }

    operands.push_back(operand);
  }

  // Now that we've rebuilt the operand list, create a new builtin and replace
  // the old one.
  auto *newInst =
    B.createBuiltin(inst->getLoc(), inst->getName(),
                    inst->getType(), /*no substitions*/{}, operands);
  newInst->setDebugLocation(inst->getDebugLocation());

  // Replace the old with the new and delete the old instruction.
  inst->replaceAllUsesPairwiseWith(newInst);

  // Remove the StructInst and other random values that we leave around in the
  // program, now that we directly refer to the TensorFlow values.
  deleteInstAndAbandonedUses(inst);
  return newInst;
}

/// If the specified instruction is an high-level aggregate operation like
/// copy_addr or destroy_addr, and if it is working on a type that contains a
/// TensorFlow value, break it down into its more primitive operations and
/// return true.  Otherwise, return false.  This leaves the input instruction in
/// place and inserts the additional instructions immediately after the input
/// instruction that is exploded.
static bool explodeAggregateInst(SILInstruction *inst,
                                 TensorFunctionClassifier &tfc) {
  // Check to see if this is an instruction we can handle below, early exiting
  // if not.
  if (!isa<CopyAddrInst>(inst) &&
      !isa<DestroyAddrInst>(inst) &&
      !isa<RetainValueInst>(inst) &&
      !isa<ReleaseValueInst>(inst) &&
      !isa<StrongRetainInst>(inst) &&
      !isa<StrongReleaseInst>(inst))
    return false;

  // Check to make sure that this operation is doing something on a value
  // containing a TensorFlow value.  If not, just leave it alone.
  auto type = inst->getOperand(0)->getType();
  if (!tfc.containsTensorFlowValue(type))
    return false;

  // TODO: This is currently just handling loadable types.  We should be able to
  // scalarize address-only elements, by turning them into by-address operations
  // on each element.  This can occur when a struct/tuple contains tensors and
  // also has some address-only type.
  auto &TL = inst->getModule().getTypeLowering(type);
  if (!TL.isLoadable())
    return false;

  // Insert any new instructions right after the one we're going to explode.
  if (isa<TermInst>(inst)) return false;
  SILBuilder B(++SILBasicBlock::iterator(inst));
  B.setCurrentDebugScope(inst->getDebugScope());

  // Lower a copy_addr into a load and store + retain/release instructions.
  if (auto *copyAddr = dyn_cast<CopyAddrInst>(inst)) {
    // Note, we don't use TL.emitCopyInto because that will produce a copy_addr.
    auto loc = copyAddr->getLoc();
    SILValue value =
      TL.emitLoadOfCopy(B, loc, copyAddr->getSrc(), copyAddr->isTakeOfSrc());
    TL.emitStoreOfCopy(B, loc, value, copyAddr->getDest(),
                       copyAddr->isInitializationOfDest());
  } else if (auto *destroy = dyn_cast<DestroyAddrInst>(inst)) {
    /// Turn a destroy_addr into a load+release_value pair.
    TL.emitDestroyAddress(B, destroy->getLoc(), destroy->getOperand());
  } else if (isa<RetainValueInst>(inst) || isa<StrongRetainInst>(inst)) {
    // Turn a retain_value into a retain_value on its elements.  We peephole
    // StructInst values because they are so common and this generates cleaner
    // IR and faster compile times.
    auto op = lookThroughSingleElementStructInsts(inst->getOperand(0));
    if (op != inst->getOperand(0) && op->getType().isAnyClassReferenceType())
      B.createStrongRetain(inst->getLoc(), op, Atomicity::Atomic);
    else
      TL.emitLoweredCopyValueMostDerivedDescendents(B, inst->getLoc(),
                                                    inst->getOperand(0));
  } else if (isa<ReleaseValueInst>(inst) || isa<StrongReleaseInst>(inst)) {
    // Turn a retain_value into a retain_value on its elements.  We peephole
    // StructInst values because they are so common and this generates cleaner
    // IR and faster compile times.
    auto op = lookThroughSingleElementStructInsts(inst->getOperand(0));
    if (op != inst->getOperand(0) && op->getType().isAnyClassReferenceType())
      B.createStrongRelease(inst->getLoc(), op, Atomicity::Atomic);
    else
      TL.emitLoweredDestroyValueMostDerivedDescendents(B, inst->getLoc(),
                                                       inst->getOperand(0));
  } else {
    llvm_unreachable("unhandled instructions should be filtered above");
  }

  return true;
}


/// Identify all of the tensor operations in the current function, and scan them
/// to see if there are any indirect arguments, where the address of a stack
/// allocation is passed to the builtin.  These occur when the tensor op was in
/// a generic context and was passed a scalar attribute value of generic type.
///
/// If we find one of these indirect values, transform it into a load of the
/// address and a use of the loaded value.  This allows the stack allocation to
/// be promoted, allowing us to construct SSA def-use chains.
///
/// Similarly, if we see a struct operand that wraps a primitive value, we
/// extract out the underlying scalar value until we get to a builtin integer or
/// floating point value.
///
/// Since we're scanning the function, keep track of all of the tensor
/// operations to avoid additional linear scans over the function.
///
void TFDeabstraction::simplifyTensorOperands() {
  llvm::PrettyStackTraceFormat X("TFDeabstraction::simplifyTensorOperands");
  bool containsOpBuiltin = false;

  bool alreadyPrinted = false;
  auto logIfFirstChange = [&]() {
    if (alreadyPrinted) return;
    logCurrentState("After Inlining", /*detailed*/true);
    alreadyPrinted = true;
  };

  for (auto &BB : fn) {
    for (auto I = BB.begin(), E = BB.end(); I != E; ) {
      // Manually move iterator to avoid invalidation if we replace 'inst'.
      auto *inst = &*I++;

      // Try to decode this instruction as an op.  If it isn't one, ignore it.
      if (auto opInfo = SILTensorOpInfo::decode(inst)) {
        logIfFirstChange();

        // Simplify operands if possible.
        opInfo->inst = simplifyOperands(opInfo->inst, *this);

        // Remember this for later passes.
        tensorOps.push_back(opInfo->inst);
        containsOpBuiltin = true;
        continue;
      }

      // If we have a call to a function that is conditionally promotable to a
      // tensor op, we add it to the set of tensor operations we're trying to
      // deabstract.  This ensures that we deabstract its operands, which makes
      // it possible to tell if it is getting a variable or constant value.
      if (auto *apply = dyn_cast<ApplyInst>(inst)) {
        if (SILTensorOpInfo::isDecodableApply(apply)) {
          logIfFirstChange();
          // Remember this for later passes.
          tensorOps.push_back(apply);
          containsOpBuiltin = true;
          continue;
        }
      }

      // Find retain and release instructions that directly use TensorFlow
      // values.  We treat them as tensorOps to ensure that their operands are
      // deabstracted.
      if (isa<StrongRetainInst>(inst) || isa<StrongReleaseInst>(inst)) {
        if (isTensorFlowValue(inst->getOperand(0)->getType())) {
          tensorOps.push_back(inst);
          continue;
        }
      }

      // Check to see if this is an aggregate operation (like a copy_addr, a
      // retain or release, etc) that involves a TensorFlow value.  If so,
      // explode it out into its components and reprocess the components.  This
      // ensures that nothing later in deabstraction or partitioning have to
      // worry about them.
      if (explodeAggregateInst(inst, tfc)) {
        logIfFirstChange();

        // Reset our iterator to the first instruction we just produced so we
        // walk through them and recursively expand or remember them as
        // appropriate.
        I = ++SILBasicBlock::iterator(inst);

        // We frequently produce dead code by exploding things, for example a
        // retain of a StructInst value will end up being a retain of the
        // original value, and therefore strand the StructInst.  Clean this
        // stuff up as we go.  This is better for compile time and it makes it
        // a lot easier to read the debugging dumps.
        deleteInstAndAbandonedUses(inst);
        continue;
      }

      // Otherwise we leave the instruction alone.
    }
  }

  // If the tensorOps list just contained retain/release instructions but had
  // no actual tensor builtins, we'll ignore the function because there is
  // nothing to partition out of it.  This is probably something actually
  // working on the host-side tensor operation.
  if (!containsOpBuiltin)
    tensorOps.clear();
}

namespace {
  /// This helper is used to find promotable memory in the operand chains of
  /// tensor operations.  This operates on the pre-deabstraction code, so it has
  /// to be able to look through the various cases that will be eliminated
  /// later.
  class PromotableMemoryFinder {
    SmallVectorImpl<SILGlobalVariable*> &globals;
    SmallVectorImpl<AllocStackInst*> &stackAllocs;
    SmallPtrSet<SILInstruction*, 32> visited;
    TensorFunctionClassifier &tfc;
  public:

    PromotableMemoryFinder(SmallVectorImpl<SILGlobalVariable*> &globals,
                           SmallVectorImpl<AllocStackInst*> &stackAllocs,
                           TensorFunctionClassifier &tfc)
      : globals(globals), stackAllocs(stackAllocs), tfc(tfc) {}

    void run(ArrayRef<SILInstruction*> tensorOps);
  private:
    void findPromotableMemoryFromValue(SILValue value);
    void findPromotableMemoryFromLoadedAddress(SILValue pointer);
  };
} // end anonymous namespace



/// Analyze the dataflow values feeding into the specified tensor operations in
/// order to find promotable stack and global references.
void PromotableMemoryFinder::run(ArrayRef<SILInstruction*> tensorOps) {
  llvm::PrettyStackTraceFormat X("PromotableMemoryFinder::run");

  // Find all the promotable memory reachable from tensor ops.  This ensures
  // we can directly connect their use-def edges together.
  for (auto *op : tensorOps) {
    for (auto &operand : op->getAllOperands())
      findPromotableMemoryFromValue(operand.get());
  }

  // If we're in the main function processing top level code, scan the function
  // to collect any global_addr instructions.  We want to promote tensor-related
  // globals and the values that feed into them.
  auto &fn = *tensorOps.front()->getFunction();
  if (fn.getName() != SWIFT_ENTRY_POINT_FUNCTION)
    return;

  // Find all global addrs in the function, whether or not they involve tensor
  // operations: they could involve tensor values but not be directly used in
  // the ops.  If we find a global tensor, make sure to add it to our set.  It
  // may be a use of a tensor op, but not being used by one.
  DenseMap<SILGlobalVariable*, SmallVector<SILValue, 4>> allGlobalAddrs;
  for (auto &bb : fn) {
    for (auto &inst : bb)
      if (auto ga = dyn_cast<GlobalAddrInst>(&inst))
        if (tfc.containsTensorFlowValue(ga->getType()))
          allGlobalAddrs[ga->getReferencedGlobal()].push_back(ga);
  }

  // If we've found references to global variables, we need to make sure to
  // analyze the computation that feeds into the globals as well.  This is an
  // iterative process since new references can be discovered as new globals are
  // found.
  for (auto &entry : allGlobalAddrs) {
    auto global = entry.first;

    bool canPromoteGlobal = true;

    // Process the global_addrs that are referencing this global, using the
    // array as a worklist.
    SmallVector<SILValue, 4> addrWorklist(std::move(entry.second));

    while (!addrWorklist.empty()) {
      auto addr = addrWorklist.pop_back_val();

      // Walk the use chains of the addr, looking for stores to it.  Any store
      // to it produces a value that feeds it, which we can contain global
      // variable references and stack allocation references.
      for (auto *use : addr->getUses()) {
        auto user = use->getUser();

        // Take an extremely conservative approach to handling accesses of the
        // global, whitelisting specific sorts of uses.  If we find anything
        // we can't handle, we abort promotion of this global.
        if (isa<EndAccessInst>(user)) // Just a marker.
          continue;

        // Anything that dives into an element of the global can continue to
        // dive into the promoted value.
        if (isa<StructElementAddrInst>(user) || isa<TupleElementAddrInst>(user))
          continue;

        // If this is a store to the global, analyze the input value.
        if (auto *si = dyn_cast<StoreInst>(user)) {
          if (use->getOperandNumber() == 1) {
            findPromotableMemoryFromValue(si->getOperand(0));
            continue;
          }
        }

        // Loads are fine.
        if (isa<LoadInst>(user))
          continue;

        // If this is a begin_access instruction, then it is a projection/copy
        // of the address.  Analyze it too.
        if (auto *begin = dyn_cast<BeginAccessInst>(user)) {
          addrWorklist.push_back(begin);
          continue;
        }

        // Some other unexpected user of the address is left around.  We should
        // handle this some day, but for now just leave the global access
        // unchanged, to avoid miscompiling code.
        canPromoteGlobal = false;

        // Make this a hard error in the testsuite.
        if (getTFDumpIntermediateStream() == &llvm::outs()) {
          llvm::errs() << "unexpected global_addr user in top level code"
                       << " promotion: " << *user << "\n\n";
          llvm::errs() << *user->getFunction();
          llvm::errs() << "unexpected global_addr user in top level code"
                       << " promotion: " << *user << "\n\n";
          abort();
        }
        break;
      }
    }

    // Tell the caller that this global is promotable.
    if (canPromoteGlobal)
      globals.push_back(global);
  }
}



/// Scan upward through the def-use chains of the specified operand value,
/// looking through operations that we can deabstract.  If we find stack
/// allocations along the way, add them to our set.
void PromotableMemoryFinder::findPromotableMemoryFromValue(SILValue value) {
  // If we found a non-instruction operand, or an instruction we've already
  // visited, then we're done scanning it.
  auto *inst = value->getDefiningInstruction();
  if (!inst || !visited.insert(inst).second)
    return;

  // If this is one of the instructions we can deabstract by scalarizing, just
  // look through it.
  if (isa<TupleInst>(inst) || isa<StructInst>(inst) ||
      isa<StructExtractInst>(inst) || isa<TupleExtractInst>(inst)) {
    for (auto &operand : inst->getAllOperands())
      findPromotableMemoryFromValue(operand.get());
    return;
  }

  // If this is a load, then we can deabstract it if it is a SRoA'able pointer
  // to a stack allocation.
  if (auto *load = dyn_cast<LoadInst>(inst))
    findPromotableMemoryFromLoadedAddress(load->getOperand());
}

/// The specific pointer is being loaded by a tensor operation operand.
/// Recursively process the pointer - if it is to a stack allocation that we can
/// deabstract, then recursively process any stores into it as values that feed
/// the tensor operation.
void PromotableMemoryFinder::
findPromotableMemoryFromLoadedAddress(SILValue pointer) {
  while (isa<TupleElementAddrInst>(pointer) ||
         isa<StructElementAddrInst>(pointer) ||
         isa<BeginAccessInst>(pointer)) {
    pointer = cast<SingleValueInstruction>(pointer)->getOperand(0);
  }

  // If we've already processed this instruction, then we're done.
  auto *pointerInst = pointer->getDefiningInstruction();
  if (!pointerInst || !visited.insert(pointerInst).second)
    return;

  // If the base of the pointer is something other than a stack allocation or if
  // we already processed this, then we're done.
  auto *alloc = dyn_cast<AllocStackInst>(pointerInst);
  if (!alloc)
    return;

  // Ok, this is a stack allocation we want to promote, remember it.
  stackAllocs.push_back(alloc);

  // Walk the use-def chains of the allocation, finding any stores that feed
  // into it, and recursively processing the values that are store into it.
  SmallVector<SILInstruction*, 4> instrsToProcess;
  instrsToProcess.push_back(alloc);

  while (!instrsToProcess.empty()) {
    auto *inst = instrsToProcess.pop_back_val();

    for (auto result : inst->getResults())
      for (auto use : result->getUses()) {
        auto *user = use->getUser();
        // If we found a store instruction on the upward pass, and if the store
        // is *to* the alloc then we can recursively process the value stored
        // into it.
        if (auto *store = dyn_cast<StoreInst>(user)) {
          // If this is a store *to* the address, then process the stored value
          // as an input.
          if (use->getOperandNumber() == 1)
            findPromotableMemoryFromValue(store->getSrc());
          continue;
        }

        // copy_addr's are a load+store pair.
        if (auto *copyaddr = dyn_cast<CopyAddrInst>(user)) {
          // If we found a copy_addr into this address during an upward scan,
          // then this is a load of the other operand.
          if (use->getOperandNumber() == 1)
            findPromotableMemoryFromLoadedAddress(copyaddr->getSrc());
        }

        // If this is the original allocation or an SRoA'able projection of its
        // address, then recursively process users.
        if (isa<TupleElementAddrInst>(inst) ||
            isa<StructElementAddrInst>(inst)) {
          instrsToProcess.push_back(user);
          continue;
        }

        // Otherwise we don't know what kind of user this is, ignore it.
      }
  }
}


/// Our dataflow analysis of tensor operations has decided that some number of
/// global variables need to be promoted to SSA in order to perform
/// deabstraction.  We know that all calls within this function have been
/// forcibly inlined, so we can switch them to stack allocations and then allow
/// SSA promotion to take care of converting them to registers.
void TFDeabstraction::
promoteGlobalsToStack(ArrayRef<SILGlobalVariable*> globals,
                      SmallVectorImpl<AllocStackInst*> &stackAllocs) {
  llvm::PrettyStackTraceFormat X("PromotableMemoryFinder::promoteGlobals");

  DenseMap<SILGlobalVariable*, AllocStackInst*> stackAllocForGlobal;

  // Promote each global by making a stack allocation that corresponds to them,
  // inserting loads and stores to the real global, and replacing the uses of
  // the global_addr instructions with the stack allocation.
  for (auto *global : globals) {
    // Create a stack allocation in the entry block for the function.
    SILBuilder B(&fn.getEntryBlock()->front());
    auto stackAlloc = B.createAllocStack(fn.getLocation(),
                                         global->getLoweredType());
    stackAllocForGlobal[global] = stackAlloc;

    // Make sure to convert the generated alloc_stack to SSA.
    stackAllocs.push_back(stackAlloc);
  }

  // Scan the function to find any alloc_global instructions, and replace any
  // global_addr instructions with our stack allocation.
  DenseMap<SILGlobalVariable*, AllocGlobalInst*> allocGlobals;
  SmallVector<SILBasicBlock*, 4> exitBlocks;

  for (auto &bb : fn) {
    // Find all exit blocks from the function.
    if (isa<ReturnInst>(bb.getTerminator()) ||
        isa<ThrowInst>(bb.getTerminator()) ||
        isa<UnwindInst>(bb.getTerminator()))
      exitBlocks.push_back(&bb);

    for (auto i = bb.begin(), e = bb.end(); i != e;) {
      auto inst = &*i++;

      if (auto *alloc = dyn_cast<AllocGlobalInst>(inst)) {
        assert(allocGlobals[alloc->getReferencedGlobal()] == 0 &&
               "more than one alloc_global instruction in the function?");
        allocGlobals[alloc->getReferencedGlobal()] = alloc;
      }

      // If this is a global_addr for a global we're referencing, replace it
      // with a use of the stack allocation.
      if (auto *addr = dyn_cast<GlobalAddrInst>(inst)) {
        auto stack = stackAllocForGlobal[addr->getReferencedGlobal()];
        if (!stack) continue;

        addr->replaceAllUsesWith(stack);
        addr->eraseFromParent();
      }
    }
  }


  // Insert a stack deallocation plus cleanup in all of the exit blocks.
  for (auto *global : globals) {
    auto stackAlloc = stackAllocForGlobal[global];
    assert(stackAlloc && "where'd our alloc_stack go?");

    // If there was an alloc_global for the global, then this is the code that
    // initializes the global, otherwise it is just something that uses it
    // (REPL case).  In the later case, we start the stack allocation off with
    // a load.
    if (!allocGlobals.count(global)) {
      // Insert after the stack alloc.
      SILBuilder B(++SILBasicBlock::iterator(stackAlloc));
      auto loc = fn.getLocation();

      auto *addr = B.createGlobalAddr(loc, global);
      auto &TL = B.getTypeLowering(stackAlloc->getType());
      TL.emitCopyInto(B, loc, addr, stackAlloc, IsTake_t::IsNotTake,
                      IsInitialization_t::IsInitialization);
    }


    // Process each exit block, inserting epilog code.
    for (auto *exit : exitBlocks) {
      SILBuilder B(exit->getTerminator());
      auto loc = fn.getLocation();

      // Load from the stack allocation and store to the global, leaving it
      // initialized with our final state.
      auto addr = B.createGlobalAddr(loc, global);

      // If this function defines the global, then this is an initialization
      // of it, otherwise this is a reassignment of it.
      bool isInit = allocGlobals.count(global);

      auto &TL = B.getTypeLowering(stackAlloc->getType());
      TL.emitCopyInto(B, loc, addr, stackAlloc, IsTake_t::IsTake,
                      IsInitialization_t(isInit));

      B.createDeallocStack(loc, stackAlloc);
    }
  }
}

/// Scan the function looking for TensorFlow value AllocStack instructions to
/// promote.
void TFDeabstraction::promoteToSSA(MutableArrayRef<AllocStackInst*> allocs) {
  // If there is nothing to promote, don't bother calculating dominator info.
  if (allocs.empty())
    return;

  llvm::PrettyStackTraceFormat X("PromotableMemoryFinder::promoteToSSA");

  // Do any necessary preprocessing of the stack allocations before promoting
  // them.
  for (auto alloc : allocs)
    prepareStackAllocForPromotion(alloc);

  // Otherwise the function does have tensor operations, so lets promote any
  // stack allocations out of the way so we can do simple dataflow analysis.
  auto domInfo = passManager->getAnalysis<DominanceAnalysis>()->get(&fn);
  promoteAllocsToSSA(allocs, domInfo);
}

/// Preprocess the specified allocation instruction to make it more suitable for
/// promotion to SSA.  In particularly, we eliminate CopyAddrInst and other
/// uses that could prevent us from promoting this.
void TFDeabstraction::prepareStackAllocForPromotion(AllocStackInst *alloc) {
  // TODO: We will eventually need to do real SRoA to handle the case when
  // we have tensor values mixed in with other random values that shouldn't
  // (or can't) be loaded.  For now, we can just fail to deabstract these
  // cases.
  SmallVector<SILInstruction*, 4> users;

  for (auto UI = alloc->use_begin(); UI != alloc->use_end();) {
    auto inst = (*UI++)->getUser();

    // If we have an instruction other than begin_access, remember it.
    auto *begin = dyn_cast<BeginAccessInst>(inst);
    if (!begin) {
      users.push_back(inst);
      continue;
    }

    // If we have a begin_access instruction, look through it.  Add all of the
    // users to the users list, and replace uses of begin_access with uses of
    // the original value.  Finally, ignore and remove the end_access.
    for (auto UI = begin->use_begin(); UI != begin->use_end();) {
      auto *use = *UI++;
      auto inst = use->getUser();
      if (isa<EndAccessInst>(inst)) {
        inst->eraseFromParent();
      } else {
        use->set(begin->getOperand());
        users.push_back(inst);
      }
    }

    begin->eraseFromParent();
  }

  for (auto user : users) {
    if (isa<EndAccessInst>(user))
      user->eraseFromParent();
  }
}

/// The specified argument has tuple type that deabstraction needs to scalarize.
/// Explode it into its deabstracted elements, rebuilding it and the branch
/// instructions that feed it.  This returns a value of the original type that
/// can be used for further analysis.
static SILValue explodeSILTupleArgument(SILPHIArgument *arg) {
  SmallVector<SILValue, 4> newArgs;

  auto *argBB = arg->getParent();

  // Collect all the fields and add new BB arguments to the block for each of
  // them.
  auto tuple = arg->getType();
  unsigned numElements = tuple.castTo<TupleType>()->getNumElements();
  for (unsigned i = 0; i != numElements; ++i) {
    auto newArg = argBB->createPHIArgument(tuple.getTupleElementType(i),
                                           arg->getOwnershipKind());
    newArgs.push_back(newArg);
  }

  // Now that we have created all of the BB arguments, we can create a new
  // tuple inst, replace the old argument, and remove it.
  SILBuilder B(&argBB->front());
  auto replacement = B.createTuple(argBB->front().getLoc(),
                                   arg->getType(), newArgs);
  arg->replaceAllUsesWith(replacement);
  unsigned argNo = arg->getIndex();
  argBB->eraseArgument(argNo);

  // Ok, now that we've exploded the BB argument itself, we need to explode the
  // values passed in the predecessor blocks.
  for (auto pi : argBB->getPredecessorBlocks()) {
    auto *br = cast<BranchInst>(pi->getTerminator());
    SmallVector<SILValue, 8> operands;
    for (unsigned i = 0, e = br->getNumOperands(); i != e; ++i)
      if (i != argNo)
        operands.push_back(br->getOperand(i));

    auto origValue = br->getOperand(argNo);

    B.setInsertionPoint(br);

    // Add all of the extracted versions of the elements.
    for (unsigned i = 0; i != numElements; ++i)
      operands.push_back(B.createTupleExtract(br->getLoc(), origValue, i));

    // Replace the branch itself.
    SILBuilder(br).createBranch(br->getLoc(), br->getDestBB(), operands);
    br->eraseFromParent();
  }

  // Ok, we're done.  Return the generated StructInst that aggregates the
  // arguments back to the caller.
  return replacement;
}

/// The specified argument has struct type that deabstraction needs to
/// scalarize. Explode it into its deabstracted elements, rebuilding it and the
/// branch instructions that feed it.  This returns a value of the original type
/// that can be used for further analysis.
static SILValue explodeSILStructArgument(SILPHIArgument *arg) {
  SmallVector<VarDecl*, 4> elementDecls;
  SmallVector<SILValue, 4> newArgs;

  auto &M = arg->getFunction()->getModule();
  auto *argBB = arg->getParent();
  auto fnLoc = argBB->getParent()->getLocation();

  // Collect all the fields and add new BB arguments to the block for each of
  // them.
  auto structType = arg->getType();
  auto decl = structType.getStructOrBoundGenericStruct();
  for (auto fieldDecl : decl->getStoredProperties()) {
    elementDecls.push_back(fieldDecl);
    auto fieldTy = structType.getFieldType(fieldDecl, M);

    auto newArg = argBB->createPHIArgument(fieldTy, arg->getOwnershipKind());
    newArgs.push_back(newArg);
  }

  // Now that we have created all of the BB arguments, we can create a new
  // struct inst, replace the old argument, and remove it.
  SILBuilder B(&argBB->front());
  auto replacement = B.createStruct(fnLoc, arg->getType(), newArgs);
  arg->replaceAllUsesWith(replacement);
  unsigned argNo = arg->getIndex();
  argBB->eraseArgument(argNo);

  // Ok, now that we've exploded the BB argument itself, we need to explode the
  // values passed in the predecessor blocks.
  for (auto pi : argBB->getPredecessorBlocks()) {
    auto *br = cast<BranchInst>(pi->getTerminator());
    SmallVector<SILValue, 8> operands;
    for (unsigned i = 0, e = br->getNumOperands(); i != e; ++i)
      if (i != argNo)
        operands.push_back(br->getOperand(i));

    B.setInsertionPoint(br);

    // Add all of the extracted versions of the elements.
    auto origValue = br->getOperand(argNo);
    for (auto fieldDecl : elementDecls)
      operands.push_back(B.createStructExtract(fnLoc, origValue, fieldDecl));

    // Replace the branch itself.
    SILBuilder(br).createBranch(br->getLoc(), br->getDestBB(), operands);
    br->eraseFromParent();
  }

  // Ok, we're done.  Return the generated StructInst that aggregates the
  // arguments back to the caller.
  return replacement;
}

/// We've promoted any stack allocations that are in the way of tensor operands
/// so we now have proper SSA.  Look through struct and tuple injection and
/// projection instructions to find the underlying value that feeds the tensor
/// operation.  This is typically another tensor operation or a constant (for
/// attributes) but may be variables or other things that cause a send.
///
static SILValue
propagateTensorOperand(SILValue v,
                       SmallPtrSet<SILPHIArgument*, 8> &checkedPhis) {
  // This is the series of struct/tuple extract indices that the value is
  // currently being projected through.  Consider an access like this:
  //     B = struct { #1, #2 }
  //     C = tuple { #3, B }
  //     Y = tuple_extract C, 1
  //     Z = struct_extract Y, 0
  // We start analysis at Z, and add the access indices of Z and Y.  When we get
  // to C, we know that we're accessing element 1 from the tuple because that is
  // the top of our access path.  When we get to B, we know we're accessing
  // element 0 from the access path, so we return the #1 value.
  SmallVector<unsigned, 4> accessPath;

  SILValue lastRootValue;
  while (1) {
    // If our access path is empty, this is a candidate that we could return.
    if (accessPath.empty())
      lastRootValue = v;

    if (auto *arg = dyn_cast<SILPHIArgument>(v)) {
      // Don't reprocess a PHI argument if we've already seen it.
      if (!checkedPhis.insert(arg).second)
        break;

      // If this is an aggregate basic block argument, explode it into its
      // component values.
      if (!accessPath.empty()) {

        // Do a quick pass over all of the predecessors to see if they are
        // unconditional branches.  If not, we can't explode them.
        // TODO: We should handle things like switch_enum someday.
        for (auto pi : arg->getParent()->getPredecessorBlocks()) {
          if (!isa<BranchInst>(pi->getTerminator()))
            // Cannot explode this BB argument.
            return lastRootValue;
        }

        // We're going to erase 'arg', so don't leave dangling pointers in the
        // set.
        checkedPhis.erase(arg);
        if (arg->getType().is<TupleType>())
          v = explodeSILTupleArgument(arg);
        else if (arg->getType().is<StructType>() ||
                 arg->getType().is<BoundGenericStructType>())
          v = explodeSILStructArgument(arg);
        else
          return lastRootValue; // Cannot handle this.
        continue;
      }

      // Otherwise simplify inputs in predecessor blocks.
      for (auto pi : arg->getParent()->getPredecessorBlocks()) {
        if (auto *br = dyn_cast<BranchInst>(pi->getTerminator())) {
          // We intentionally recalculate arg->getIndex() because its index can
          // shift.  We know that recursive processing won't delete the bb arg
          // though, as it is in checkedPhis.
          auto incomingVal = br->getOperand(arg->getIndex());
          incomingVal = propagateTensorOperand(incomingVal, checkedPhis);
          br->setOperand(arg->getIndex(), incomingVal);
        }
      }

      continue;
    }

    // Otherwise, peer through instructions.
    auto inst = v->getDefiningInstruction();
    if (!inst)
      break;

    // Extractions add to the access path.
    if (auto extract = dyn_cast<TupleExtractInst>(inst)) {
      accessPath.push_back(extract->getFieldNo());
      v = extract->getOperand();
      continue;
    }
    if (auto extract = dyn_cast<StructExtractInst>(inst)) {
      accessPath.push_back(extract->getFieldNo());
      v = extract->getOperand();
      continue;
    }

    // Constructions provide values to extract from if we have an access inside
    // of it.
    if (!accessPath.empty()) {
      if (auto str = dyn_cast<StructInst>(inst)) {
        v = str->getOperand(accessPath.pop_back_val());
        continue;
      }
      if (auto tuple = dyn_cast<TupleInst>(inst)) {
        v = tuple->getOperand(accessPath.pop_back_val());
        continue;
      }
    }

    // Otherwise, this is an unhandled instruction - we're done.
    break;
  }

  return lastRootValue;
}

/// Propagate the operand values for all tensors: this ensures that all tensor
/// operands and results are directly linked together in the SSA graph at the
/// TensorFlow value level, without going through intervening struct/tuple
/// wrappers.
void TFDeabstraction::propagateTensorValues() {
  llvm::PrettyStackTraceFormat X("TFDeabstraction::propagateTensorValues");

  // Now that we have directly exposed retain/release instructions and tensor
  // operations, go through and make sure they are directly linked to each
  // other.
  SmallPtrSet<SILPHIArgument*, 8> checkedPhis;
  for (auto *op : tensorOps) {
    for (auto &operand : op->getAllOperands()) {
      // Get the propagated value.  This call can change the tensor operand.
      auto newVal = propagateTensorOperand(operand.get(), checkedPhis);

      // Get the (possibly-changed) instruction that used to be feeding the
      // tensor operation and set the new value.
      auto opInst = operand.get()->getDefiningInstruction();
      operand.set(newVal);

      // If the old instruction is unused, try to clean up the code.
      if (opInst && !opInst->hasUsesOfAnyResult())
        recursivelyDeleteTriviallyDeadInstructions(opInst);
    }
  }
}

/// Decode the specified array constant value (which should be an
/// array of constant integer or fp values) and add it as an expanded operand
/// to the specified op that is being built up.
static void expandArrayConstant(ArrayLatticeValue &arrayVal, StringRef attrName,
                                SILTensorOpInfo::OperandClass attrKind,
                                std::string &name,
                                SmallVectorImpl<SILValue> &operands,
                                SILInstruction *forInst) {
  SILBuilder B(forInst);

  // Add the first operand, which is the metatype for the element.  If it was
  // a 'Normal' operand, change it to an Array so we can distinguish it in the
  // case of an empty array.
  if (attrKind == SILTensorOpInfo::OperandClass::Normal)
    attrKind = SILTensorOpInfo::OperandClass::Array;
  name += ","+attrName.str();
  name += SILTensorOpInfo::getOperandClassSuffix(attrKind);

  auto eltType =
    SILType::getPrimitiveObjectType(arrayVal.elementType->getCanonicalType());

  auto metatypeType =
    MetatypeType::get(arrayVal.elementType, MetatypeRepresentation::Thin)
      ->getCanonicalType();
  operands.push_back(B.createMetatype(forInst->getLoc(),
                              SILType::getPrimitiveObjectType(metatypeType)));

  // Add all of the operands as explicit values.  If the instructions came
  // from an out of line array initializer, make sure to clone them over to
  // our function.
  for (auto eltVal : arrayVal.getElements()) {
    auto elt = eltVal.getConstantInstIfPresent();

    if (!elt || elt->getFunction() != forInst->getFunction()) {
      // Make a copy of the value, it may be computed.
      switch (eltVal.getTypeKind()) {
      case LatticeValue::Array:
      case LatticeValue::String:
        llvm_unreachable("unknown type to initialize array");

      case LatticeValue::Integer:
        elt = B.createIntegerLiteral(forInst->getLoc(), eltType,
                                     eltVal.getIntegerValue());
        break;
      case LatticeValue::Float:
        elt = B.createFloatLiteral(forInst->getLoc(), eltType,
                                   eltVal.getFloatValue());
        break;
      }

      elt->setDebugLocation(B.getSILDebugLocation(forInst->getLoc()));
    }

    operands.push_back(elt);
    name += ",";
    auto eltKind = SILTensorOpInfo::OperandClass::ArrayElement;
    name += SILTensorOpInfo::getOperandClassSuffix(eltKind);
  }
}

/// If all the operands to a call to __tf_tensor_from_scalars are constants, we
/// can promote this to a 'Const' node with an attached TF_Tensor attribute.
/// It takes a 1D array of scalars and a shape as a 1D array of integers.
///
/// On success, this removes the ApplyInst and returns a pointer to the new
/// BuiltinInst that is created.  On failure, it returns a nullptr.
///
/// FIXME: This is a near duplication of the logic used by TFPartitioning in
/// SILTensorOpInfo::decodeTensorFromScalars.  When constexpr propagation is
/// done, we should remove the logic in SILTensorOpInfo.
static BuiltinInst *
tryToPromoteTensorFromScalars(ApplyInst *inst,
                            const DenseMap<SILValue, LatticeValue> &constants) {
  assert(inst->getNumOperands() == 3 && isTensorHandle(inst->getType()) &&
         "Unexpected type signature for __tf_tensor_from_scalars");

  // If we can't analyze the operands as arrays of constants, give up.
  auto scalarIt = constants.find(inst->getOperand(1));
  if (scalarIt == constants.end()) return nullptr;
  auto shapeIt = constants.find(inst->getOperand(2));
  if (shapeIt == constants.end()) return nullptr;

  // Okay, we were able to resolve the two arrays of constants.  Transform this
  // into the correct Const operation.

  // We transform this into a __tfop_Const instruction, where the values are
  // part of the 'value' tensor attribute and the shape is specified as a shape
  // attribute.
  SmallVector<SILValue, 8> operands;
  std::string name = "__tfop_Const";

  // Try to expand the array and the shape into their scalars.
  expandArrayConstant(scalarIt->second.getArrayValue(), "value",
                      SILTensorOpInfo::OperandClass::Tensor,
                      name, operands, inst);
  unsigned numElements = operands.size()-1;
  expandArrayConstant(shapeIt->second.getArrayValue(), "value",
                      SILTensorOpInfo::OperandClass::Shape,
                      name, operands, inst);

  // Verify we have the right number of scalars.  If not, emit an error and
  // leave the broken code without promoting it to an op.
  uint64_t scalarCount = 1;
  std::string errorInfo;
  for (auto elt : ArrayRef<SILValue>(operands).drop_front(numElements+2)) {
    auto *eltCst = cast<IntegerLiteralInst>(elt);
    scalarCount *= eltCst->getValue().getLimitedValue();
  }
  if (scalarCount != numElements && errorInfo.empty()) {
    errorInfo = "tensor literal should have " + llvm::utostr(scalarCount) +
          " scalars for this shape, but has " + llvm::utostr(numElements);
  }

  if (!errorInfo.empty()) {
    auto loc = getUserSourceLocation(inst);
    diagnose(inst->getType().getSwiftRValueType()->getASTContext(),
             loc.getSourceLoc(), diag::tf_op_misuse, errorInfo)
      .highlight(loc.getSourceRange());
    return nullptr;
  }

  // This takes a Tensor and a Shape operand, but needs a DType added.  The
  // dtype is the type of the Tensor elements, which we conveniently already
  // have available as the first operand.
  operands.push_back(operands[0]);
  name += ",dtype";

  auto scalarV = inst->getOperand(1);
  auto shapeV = inst->getOperand(2);

  SILBuilder B(inst);
  // Finally build a new builtin instruction with the simplified operands.
  auto newInst =
    B.createBuiltin(inst->getLoc(),
                    B.getASTContext().getIdentifier(name),
                    inst->getType(), /*no substitions*/{},
                    operands);
  newInst->setDebugLocation(inst->getDebugLocation());
  inst->replaceAllUsesPairwiseWith(newInst);
  inst->eraseFromParent();

  // We are dropping a reference to the element and shape array initializers, so
  // we need to remove the arrays themselves or at least release them.
  SILTensorOpInfo::removeOrDestroyArrayValue(scalarV, inst->getLoc(), B);
  SILTensorOpInfo::removeOrDestroyArrayValue(shapeV, inst->getLoc(), B);
  return newInst;
}

/// If all the operands to a call to __tf_tensor_from_scalars_1d are constants,
/// we can promote this to a 'Const' node with an attached TF_Tensor attribute.
/// This is a specialized form of __tf_tensor_from_scalars, because the later is
/// defined in terms of a shape of "[scalars.count]" but the performance
/// optimizer is not reliably constant propagating this.  When we have a
/// reliable deabstraction pass we can re-evaluate this and hopefully eliminate
/// it in favor of library code in the TensorFlow module.
///
/// On success, this removes the applyexpr and returns a pointer to the new
/// BuiltinInst that is created.  On failure, it returns a nullptr.
///
/// FIXME: This is a near duplication of the logic used by TFPartitioning in
/// SILTensorOpInfo::decodeTensorFromScalars1D.  When constexpr propagation is
/// done, we should remove the logic in SILTensorOpInfo.
static BuiltinInst *
tryToPromoteTensorFromScalars1D(ApplyInst *inst,
                            const DenseMap<SILValue, LatticeValue> &constants) {
  assert(inst->getNumOperands() == 2 && isTensorHandle(inst->getType()) &&
         "Unexpected type signature for __tf_tensor_from_scalars_1d");

  // If we can't analyze the scalars as an arrays of constants, give up.
  auto scalarIt = constants.find(inst->getOperand(1));
  if (scalarIt == constants.end())
    return nullptr;

  // We transform this into a __tfop_Const instruction, where the values are
  // part of the 'value' tensor attribute and the shape is hard coded.
  SmallVector<SILValue, 8> operands;
  std::string name = "__tfop_Const";

  // Try to expand the array into its scalars.
  expandArrayConstant(scalarIt->second.getArrayValue(), "value",
                      SILTensorOpInfo::OperandClass::Tensor,
                      name, operands, inst);

  SILBuilder B(inst);

  // This takes a Tensor operand, but needs a Shape and a DType added.  At
  // this point, the operands list will have a metatype for the tensor as
  // the first operand then all the elements.
  uint64_t scalarCount = operands.size()-1;

  // The shape needs a metatype to be well formed, but nothing actually
  // cares what it is.  Just re-push the metatype for the tensor elements,
  // even though it might be floating point or something else weird.
  operands.push_back(operands[0]);
  name += ",shape";
  auto shapeKind = SILTensorOpInfo::OperandClass::Shape;
  name += SILTensorOpInfo::getOperandClassSuffix(shapeKind);

  // The shape of a 1d tensor is just the count of elements.
  auto &ctx = inst->getFunction()->getASTContext();
  auto scalarCountVal =
    B.createIntegerLiteral(inst->getLoc(),
                           SILType::getBuiltinIntegerType(64, ctx),
                           scalarCount);
  operands.push_back(scalarCountVal);
  name += ",";
  auto arrayEltKind = SILTensorOpInfo::OperandClass::ArrayElement;
  name += SILTensorOpInfo::getOperandClassSuffix(arrayEltKind);

  // The  dtype is the type of the Tensor elements, which we conveniently
  // already have available as the first operand.
  operands.push_back(operands[0]);
  name += ",dtype";

  auto arrayValue = inst->getOperand(1);

  // Finally build a new builtin instruction with the simplified operands.
  auto newInst =
    B.createBuiltin(inst->getLoc(),
                    B.getASTContext().getIdentifier(name),
                    inst->getType(), /*no substitions*/{},
                    operands);
  newInst->setDebugLocation(inst->getDebugLocation());
  inst->replaceAllUsesPairwiseWith(newInst);
  inst->eraseFromParent();

  // We dropped a reference to the element initializer, so we need to
  // remove the array itself or at least release it.  This happens after
  // creating the replacement builtin, so that element initializers aren't
  // dropped.
  B.setInsertionPoint(newInst);
  SILTensorOpInfo::removeOrDestroyArrayValue(arrayValue, inst->getLoc(), B);
  return newInst;
}


/// Canonicalize tensor ops, validating their attribute arguments have
/// constants, and flattening array parameters.
void TFDeabstraction::checkAndCanonicalizeAttributes() {
  llvm::PrettyStackTraceFormat
  X("TFDeabstraction::checkAndCanonicalizeAttributes");

  // Do a big sweep over all of the operands to tensor values, collecting ones
  // that we might be interested in being constants into a single list.
  SmallVector<SymbolicValue, 32> valuesToCheck;

  for (auto *op : tensorOps) {
    for (auto &operand : op->getAllOperands()) {
      // Dump anything that might be an attribute into the list without too much
      // filtering.  We take out TensorFlow values since they are the most
      // obvious ones we don't care about later, but there may be other minor
      // things we over-query on.
      auto value = operand.get();
      if (!isTensorFlowValue(value->getType()))
        valuesToCheck.push_back({value, 0});
    }
  }

  // Eliminate duplicates and sort the array of values so we have an efficient
  // way to query it later.
  llvm::array_pod_sort(valuesToCheck.begin(), valuesToCheck.end());
  valuesToCheck.erase(std::unique(valuesToCheck.begin(), valuesToCheck.end()),
                      valuesToCheck.end());

  SmallVector<LatticeValue, 32> results;
  constantEvaluator.computeConstantValues(fn, valuesToCheck, results);

  // Transform the returned information about constants into a map that we can
  // query.  The results list should correspond directly to the values we asked
  // about.
  DenseMap<SILValue, LatticeValue> constants;

  assert(valuesToCheck.size() == results.size() && "incorrect values returned");
  for (unsigned i = 0, e = valuesToCheck.size(); i != e; ++i) {
    // If this value is a non-constant, just ignore it.
    if (!results[i].isConstant())
      continue;

    // Otherwise, add the information about it to our map.
    assert(valuesToCheck[i].index == 0 && "we only query for index #0");
    constants.insert({valuesToCheck[i].value, results[i]});
  }


  // Now that we've computed whether any of the operands are constants,
  // substitute them into the operations that we have, eliminating abstractions.
  // This makes it immediately obvious to partitioning what is and isn't a
  // constant.
  //
  // TODO: When something *isn't* a constant, we should diagnose why: is it a
  // use of a non-constexpr function that caused the problem, or the use of
  // some other thing that we can't analyze, like passing through a class?
  //
  for (auto *&inst : tensorOps) {
    // Take a look at the various well known function calls that we can promote
    // to tensor operations.  We can promote them if we are able to constant
    // fold all of the operands to these calls.  If so, we rewrite them in terms
    // of a proper op, and partitioning will continue to treat them that way.
    if (auto apply = dyn_cast<ApplyInst>(inst)) {
      // FIXME: Move this upgrading logic out of SILTensorOpInfo into
      // Deabstraction once partitioning is moved up to the mandatory passes.
      if (!SILTensorOpInfo::isDecodableApply(apply))
        continue;

      auto name = apply->getCalleeFunction()->getName();
      BuiltinInst *result;
      if (name == "__tf_tensor_from_scalars")
        result = tryToPromoteTensorFromScalars(apply, constants);
      else if (name == "__tf_tensor_from_scalars_1d")
        result = tryToPromoteTensorFromScalars1D(apply, constants);
      else
        llvm_unreachable("out of sync with isDecodableApply");

      // If promotion failed, no change is necessary.
      if (!result) continue;

      // Otherwise, we got a new instruction, so remember it in our tensor op
      // list.
      inst = result;

      // Fall into the normal op processing code.
    }

    // TODO: Handle normal tensor ops with their attributes, subsuming the
    // following loop.
  }

  for (auto &BB : fn) {
    for (auto I = BB.begin(), E = BB.end(); I != E; ) {
      // Manually move iterator to avoid invalidation if we replace 'inst'.
      auto *inst = &*I++;

      // If this is a well known function that can be transformed into an op, do
      // so first.
      // FIXME: This should take into consideration the constants we just
      // computed!
      if (auto apply = dyn_cast<ApplyInst>(inst))
        inst = SILTensorOpInfo::decodeApply(apply);

      // Try to decode this instruction as an op.  If it isn't one, ignore it.
      auto opInfo = SILTensorOpInfo::decode(inst);
      if (!opInfo)
        continue;

      // Use the constants we just computed to substitute into parameter values.



      // TODO: Deabstraction isn't fully handling all constant expressions and
      // other canonicalizations that we expect, so for now we depend on the
      // performance optimizer.  When deabstraction is done, we will run the
      // partitioner as part of deabstraction (including at -O0).  Until we are
      // ready for that, we gate the validation of tensor operations on a flag.
      // This allows us to write testcases without breaking current use of the
      // compiler.
      if (!TFCheckDeabstraction)
        continue;


      // Check to see if the usage of this op looks ok.  If not, reject it with
      // an error and ignore it.
      auto error = opInfo->checkAndDiagnoseOperands();
      if (!error.empty()) {
        // TODO: improve the diagnostic to talk about the parameter label in the
        // user code, not the internal op attribute.  The bookkeeping for this
        // isn't obvious though.
        auto loc = getUserSourceLocation(inst);
        diagnose(fn.getModule().getASTContext(), loc.getSourceLoc(),
                 diag::tf_op_misuse, error)
          .highlight(loc.getSourceRange());
        continue;
      }

      // If the tensor operation uses array parameters or has scalar values that
      // are passed through memory, promote them to being simple arguments to
      // make all subsequent analyses and promotion of the tensor operations
      // simpler.
      opInfo->canonicalizeOperands();
    }
  }
}

/// Process the specified top level function as a deabstraction context: if it
/// contains Tensor operations simplify the code using predictable rules until
/// the tensor operations are exposed in a canonical form inside of this
/// function.
///
/// We currently make use of the following techniques to do this:
///   1) Inlining.  We look for direct calls to functions that take and return
///      values of TensorFlow values, possibly wrapped by structs and tuples.
///   2) Promotion of globals to stack allocations for Playgrounds, REPL, and
///      top level code in scripts.
///   3) SSA Promotion of stack values to registers.
///   4) Scalarization of struct/tuple values.
///
/// TODO:
///   *) Move tensor op canonicalization up from tf-partition.
///   *) Enums.  What can we reliably do with them?  Should they be out of
///      model?  We can definitely do ones without payload values.
///
void TFDeabstraction::doIt() {
  // Start by inlining functions that take and return Tensor values.
  inlineCalls();

  // Scan for any Tensor operations, removing indirect operands and structs that
  // interfere with SSA construction.
  simplifyTensorOperands();

  // If we didn't find any ops, early exit processing of this function to save
  // compile time.
  if (tensorOps.empty())
    return;

  logCurrentState("After simplifyTensorOperands", /*detailed*/true);

  // Scan over all of the operands of the tensor ops, finding stack allocations
  // and global variables that must be promoted to SSA.
  SmallVector<SILGlobalVariable*, 8> globals;
  SmallVector<AllocStackInst*, 16> stackAllocs;
  PromotableMemoryFinder(globals, stackAllocs, tfc).run(tensorOps);

  // If any global variable references were found in the operation chain, try
  // to predictably promote their references to unblock analysis.  This is only
  // valid in cases that we have forcibly flattened the function (e.g.
  // Playgrounds, REPL, and other top-level-code situations).
  if (forciblyFlattened && !globals.empty()) {
    promoteGlobalsToStack(globals, stackAllocs);
    logCurrentState("After promoteGlobalsToStack", /*detailed*/true);
  }

  // Promote stack allocations to SSA, this allows us to do dataflow analysis,
  // and eliminates mutation from tensor values.
  promoteToSSA(stackAllocs);

  logCurrentState("After promoteToSSA", /*detailed*/true);

  // Now that we've promoted all the allocations in the way of our dataflow,
  // go through and propagate any tuple/struct values that are in the way of
  // our analysis.
  propagateTensorValues();

  logCurrentState("After propagateTensorValues", /*detailed*/true);

  // Canonicalize attribute arguments, checking that they have constants, and
  // flattening array attributes.
  checkAndCanonicalizeAttributes();

  logCurrentState("Result", /*detailed*/false);

  // We're currently relying on the performance optimizer to do some stuff, but
  // for large testcacses it is doing bad stuff.
  // FIXME: Should be eliminated when partitioning happens as part of
  // deabstraction, because then the optimizer won't be seeing all of our tensor
  // stuff.
  fn.getModule().getOptions().EnableARCOptimizations = false;
}


namespace {
  struct TFDeabstractionPass : public SILModuleTransform {
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

  // If we are running on the TensorFlow module itself, do not perform
  // deabstraction.  It contains a lot of code that processes TensorHandle and
  // other types as host values, and we do not want to force inline all of these
  // things together.
  //
  // TODO: Rework the heuristics in inlineCalls() to be smarter.  In an ideal
  // world, we would be lazy about inlining, and only inline calls due to actual
  // inter-op value uses.
  if (module->getSwiftModule() == tfModule)
    return;

  TensorFunctionClassifier tfc;
  ConstExprEvaluator constantEvaluator;

  // Loop over all of the functions in the current module processing them -
  // iff they look like they could be the top level of a deabstraction
  // context.
  for (auto &fn : *module) {
    // If this function is a building block of larger tensor programs (e.g.
    // the ops defined in the TensorFlow module), then don't transform it in
    // isolation.
    if (!tfc.shouldBePartitioned(&fn))
      continue;

    // If something crashes, make sure the pretty stack trace says what we
    // were doing.
    llvm::PrettyStackTraceFormat X("TFDeabstraction on function %s",
                                   fn.getName().str().c_str());

    TFDeabstraction(fn, tfc, constantEvaluator, PM).doIt();

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
