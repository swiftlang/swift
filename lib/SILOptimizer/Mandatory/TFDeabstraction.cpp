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
#include "swift/SIL/SILConstants.h"
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
TFStrictDeabstraction("tf-strict-deabstraction", llvm::cl::init(false),
       llvm::cl::desc("Verify #tfop's are valid without the perf optimizer"));

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

/// Return true if this apply instruction is to a function that can be
/// conditionally hoisted into the graph, but don't check the operands to
/// see if they are actually constants we can handle.
static bool isDecodableApply(ApplyInst *apply) {
  auto fn = apply->getCalleeFunction();
  if (!fn) return false;

  auto name = fn->getName();
  return name == "__tf_tensor_from_scalars" ||
         name == "__tf_tensor_from_scalars_1d";
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

    void promoteToSSA(MutableArrayRef<AllocStackInst*> allocs);
    void prepareStackAllocForPromotion(AllocStackInst *alloc);
    void propagateTensorValues();
    void checkAttributesAndFormGraphOps();
    void formGraphOp(SILTensorOpInfo &opInfo,
                     DenseMap<SILValue, SymbolicValue> &constants,
                     GraphFunctionDeviceInfo &deviceInfo);
    void cleanupDeadInstructions();
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

  if (auto fn = apply->getCalleeFunction())
    return fn->hasSemanticsAttr("array.uninitialized");
  return false;
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
  auto shouldInline = [&](FullApplySite site,
                          const SILFunction &callee) -> bool {
    // If this is a call of an explicitly noinline function, don't inline it!
    if (callee.getInlineStrategy() == NoInline)
      return false;

    // Check for array internals which we could be inlined, but prefer to
    // leave in abstracted form for easier analysis.  For things like
    // Tensor<Float>([[1,2],[3,4]]), we prefer to see higher level array
    // construction calls beacuse we end up removing them anyway.
    if (isArrayUninitialized(site.getInstruction()))
      return false;

    // Never inline _allocateUninitializedArray (even of Tensors).  It is the
    // entrypoint used by SILGen to represent array allocations.
    if (callee.getName().contains("_allocateUninitializedArray"))
      return false;

    // FIXME: This is a specific hack to inline literal conversion operations
    // that prevent SSA promotion and bloat code.  This should be eliminated
    // when we have a proper constexpr model and can just constant fold through
    // the memory indirections.
    if (callee.getName().contains("ExpressibleByBuiltinIntegerLiteral") ||
        callee.getName().contains("ExpressibleByIntegerLiteral") ||
        callee.getName().contains("SSfs13FloatingPoint") ||
        callee.getName().contains("S10TensorFlow0A0VAAs13FloatingPointRzrlE12"
                                  "randomNormal4mean6stddev5s") ||
        callee.getName().contains("S10TensorFlow0A5ShapeV12arrayLiteral"
                                  "ACs5Int32Vd_tcfC"))
      if (!TFStrictDeabstraction)
        return true;

    // If we're forcibly flattening code into the top level function, and if the
    // callee is in the same source file as that top-level function (and thus
    // has visibility into its global variables) then force inline it.
    if (forciblyFlattened) {
      if (auto *apply = dyn_cast<ApplyInst>(site.getInstruction())) {
        if (auto *callee = apply->getCalleeFunction()) {
          // FIXME: We will miscompile functions that use variables in top level
          // code right now.  We need to implement this properly.
#if 0
          if (shouldBeForciblyFlattened(*callee))
            return true;
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

    return true;
  };

  SmallPtrSet<SILFunction*, 16> inlinedCallees;

  // Use the mandatory inlining algorithm to expose call sites that contain
  // TensorFlow values as their argument or result lists.
  inlineForTFDeabstraction(fn,
     [&](FullApplySite site, SILFunction &callee) -> bool {
       if (callee.empty() &&
           !site.getModule().linkFunction(&callee,
                                          SILModule::LinkingMode::LinkAll))
         return false;

       if (!shouldInline(site, callee))
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
           getPrimitiveStructField(type.getASTType()) != nullptr;
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
                                     operand->getType().getASTType())) {
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
                    inst->getType(), SubstitutionMap(), operands);
  newInst->setDebugLocation(inst->getDebugLocation());

  // Replace the old with the new and delete the old instruction.
  inst->replaceAllUsesPairwiseWith(newInst);

  // Remove the StructInst and other random values that we leave around in the
  // program, now that we directly refer to the TensorFlow values.
  deleteInstAndAbandonedUses(inst);
  return newInst;
}

/// If the specified instruction is an high-level aggregate operation like
/// copy_addr or destroy_addr, break it down into its more primitive operations
/// and return true.  Otherwise, return false.
///
/// If 'tfc' is non-null, this will only promote ops working on a type that
/// contains a TensorFlow value.
///
/// This leaves the input instruction in place and inserts the additional
/// instructions immediately after the input instruction that is exploded.
static bool explodeAggregateInst(SILInstruction *inst,
                                 TensorFunctionClassifier *tfc) {
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
  if (tfc && !tfc->containsTensorFlowValue(type))
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
        if (isDecodableApply(apply)) {
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
      if (explodeAggregateInst(inst, &tfc)) {
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

// Return true if this is a standard LLVM arithmetic operation.  We often see
// silly allocas in the way of them.
// FIXME: This is only necessary because we don't have a constexpr model.
// When that is in place and working well, this should be removed.
static bool isSimpleBuiltinArithmeticOp(BuiltinInst *builtin) {
  if (TFStrictDeabstraction)
    return false;

  switch (builtin->getBuiltinInfo().ID) {
  default: return false;
  case BuiltinValueKind::Trunc:
  case BuiltinValueKind::ZExt:
  case BuiltinValueKind::SExt:
  case BuiltinValueKind::FPToUI:
  case BuiltinValueKind::FPToSI:
  case BuiltinValueKind::UIToFP:
  case BuiltinValueKind::SIToFP:
  case BuiltinValueKind::FPTrunc:
  case BuiltinValueKind::FPExt:
  case BuiltinValueKind::TruncOrBitCast:
  case BuiltinValueKind::ZExtOrBitCast:
  case BuiltinValueKind::SExtOrBitCast:
  case BuiltinValueKind::Add:
  case BuiltinValueKind::FAdd:
  case BuiltinValueKind::And:
  case BuiltinValueKind::AShr:
  case BuiltinValueKind::LShr:
  case BuiltinValueKind::Or:
  case BuiltinValueKind::FDiv:
  case BuiltinValueKind::Mul:
  case BuiltinValueKind::FMul:
  case BuiltinValueKind::SDiv:
  case BuiltinValueKind::ExactSDiv:
  case BuiltinValueKind::Shl:
  case BuiltinValueKind::SRem:
  case BuiltinValueKind::Sub:
  case BuiltinValueKind::FSub:
  case BuiltinValueKind::UDiv:
  case BuiltinValueKind::ExactUDiv:
  case BuiltinValueKind::URem:
  case BuiltinValueKind::FRem:
  case BuiltinValueKind::Xor:
  case BuiltinValueKind::SAddOver:
  case BuiltinValueKind::UAddOver:
  case BuiltinValueKind::SSubOver:
  case BuiltinValueKind::USubOver:
  case BuiltinValueKind::SMulOver:
  case BuiltinValueKind::UMulOver:
  case BuiltinValueKind::FNeg:
  case BuiltinValueKind::AssumeNonNegative:
  case BuiltinValueKind::ICMP_EQ:
  case BuiltinValueKind::ICMP_NE:
  case BuiltinValueKind::ICMP_SLE:
  case BuiltinValueKind::ICMP_SLT:
  case BuiltinValueKind::ICMP_SGE:
  case BuiltinValueKind::ICMP_SGT:
  case BuiltinValueKind::ICMP_ULE:
  case BuiltinValueKind::ICMP_ULT:
  case BuiltinValueKind::ICMP_UGE:
  case BuiltinValueKind::ICMP_UGT:
  case BuiltinValueKind::FCMP_OEQ:
  case BuiltinValueKind::FCMP_OGT:
  case BuiltinValueKind::FCMP_OGE:
  case BuiltinValueKind::FCMP_OLT:
  case BuiltinValueKind::FCMP_OLE:
  case BuiltinValueKind::FCMP_ONE:
  case BuiltinValueKind::FCMP_ORD:
  case BuiltinValueKind::FCMP_UEQ:
  case BuiltinValueKind::FCMP_UGT:
  case BuiltinValueKind::FCMP_UGE:
  case BuiltinValueKind::FCMP_ULT:
  case BuiltinValueKind::FCMP_ULE:
  case BuiltinValueKind::FCMP_UNE:
  case BuiltinValueKind::FCMP_UNO:
    return true;
  }
}

namespace {
  /// This helper is used to find promotable memory in the operand chains of
  /// tensor operations.  This operates on the pre-deabstraction code, so it has
  /// to be able to look through the various cases that will be eliminated
  /// later.
  class PromotableMemoryFinder {
    SILFunction &fn;
    SmallVectorImpl<AllocStackInst*> &stackAllocs;
    SmallPtrSet<SILInstruction*, 32> visited;
    TensorFunctionClassifier &tfc;
  public:

    PromotableMemoryFinder(SmallVectorImpl<AllocStackInst*> &stackAllocs,
                           TensorFunctionClassifier &tfc, SILFunction &fn)
      : fn(fn), stackAllocs(stackAllocs), tfc(tfc) {}

    bool run(ArrayRef<SILInstruction*> tensorOps);
  private:
    void findPromotableMemoryFromValue(SILValue value);
    void findPromotableMemoryFromLoadedAddress(SILValue pointer);

    void findMainFunctionGlobalAddressRootCandidates(
                     SmallVectorImpl<std::pair<SILValue, bool>> &addressRoots);
    bool canAddressRootBeReliablyPromoted(SILValue root);

    void promoteAddressRootsToStack(
                        ArrayRef<std::pair<SILValue, bool>> addressRoots);

  };
} // end anonymous namespace



/// Analyze the dataflow values feeding into the specified tensor operations in
/// order to find promotable stack values and address root references.
///
/// This returns true if any address roots were promoted to stack values.
///
bool PromotableMemoryFinder::run(ArrayRef<SILInstruction*> tensorOps) {
  llvm::PrettyStackTraceFormat X("PromotableMemoryFinder::run");

  // Find all the promotable memory reachable from tensor ops.  This ensures
  // we can directly connect their use-def edges together.
  for (auto *op : tensorOps) {
    for (auto &operand : op->getAllOperands())
      findPromotableMemoryFromValue(operand.get());
  }

  // Next we collect address roots, which are pointers that are not stack
  // allocations that we need to promote.  We start by collecting candidate
  // pointers, then validating them.  We keep track of the root pointer as well
  // as whether the value starts out uninitialized (which is the case for many
  // global roots).
  SmallVector<std::pair<SILValue, bool>, 8> addressRoots;

  // Check the arguments to the SIL function for any indirect structs/tuples
  // that contain tensors.  Such functions are generally inlined into the caller
  // but can appear this way when the user explicitly specifies @noinline.  In
  // this case we want to promote the pointer as a root because this allows
  // turning the entire body into SSA.
  for (auto arg : fn.getArguments()) {
    auto convention = cast<SILFunctionArgument>(arg)->getArgumentConvention();
    // If this is an indirect argument working on tensors, it is a candidate.
    if (convention.isIndirectConvention() &&
        tfc.containsTensorFlowValue(arg->getType()))
      addressRoots.push_back({ arg, /*startsUninitialized*/false });
  }


  // If we're in the main function processing top level code, scan the function
  // to collect any global_addr instructions which provide address roots.  We
  // want to promote tensor-related globals and the values that feed into them.
  if (fn.getName() == SWIFT_ENTRY_POINT_FUNCTION)
    findMainFunctionGlobalAddressRootCandidates(addressRoots);

  if (addressRoots.empty())
    return false;

  // If we've found any address roots, check to see if the computation that
  // feeds into them can be reliably promoted.
  for (unsigned i = 0; i != addressRoots.size(); ++i) {
    if (canAddressRootBeReliablyPromoted(addressRoots[i].first))
      continue;

    // If we can't promote this root, remove it from our set.
    std::swap(addressRoots[i], addressRoots.back());
    addressRoots.pop_back();
    --i;
  }

  if (addressRoots.empty())
    return false;

  // If any address roots were found, predictably promote them to the stack to
  // unblock analysis.
  promoteAddressRootsToStack(addressRoots);
  return true;
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


  // Look through standard LLVM arithmetic operations.  We often see silly
  // allocas in the way of them.
  // FIXME: This is only necessary because we don't have a constexpr model.
  // When that is in place and working well, this should be removed.
  if (!TFStrictDeabstraction)
    if (auto builtin = dyn_cast<BuiltinInst>(inst))
      if (isSimpleBuiltinArithmeticOp(builtin))
        findPromotableMemoryFromValue(builtin->getOperand(0));

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

/// Find all global addrs in the function, whether or not they involve tensor
/// operations: they could involve tensor values but not be directly used in
/// the ops.  If we find a global tensor, make sure to add it to our set.  It
/// may be a use of a tensor op, but not being used by one.
///
/// The representation of global addresses is also a bit wonky:  There can be
/// multiple global_addr instructions for each global.  Later code wants to
/// have a single pointer to reason about, so we canonicalize to one of them.
///
void PromotableMemoryFinder::
findMainFunctionGlobalAddressRootCandidates(
                    SmallVectorImpl<std::pair<SILValue, bool>> &addressRoots) {
  // First collect all the alloc_globals that may be present in the function,
  // to ensure we have them all when we start scanning for global_addr's.
  DenseMap<SILGlobalVariable*, AllocGlobalInst*> allocGlobals;
  for (auto &bb : fn) {
    for (auto &inst : bb) {
      // If we see an alloc global, remember where it is.
      if (auto agi = dyn_cast<AllocGlobalInst>(&inst)) {
        auto gv = agi->getReferencedGlobal();
        if (tfc.containsTensorFlowValue(gv->getLoweredType())) {
          assert(allocGlobals[agi->getReferencedGlobal()] == 0 &&
                 "more than one alloc_global instruction in the function?");

          allocGlobals[gv] = agi;
        }
      }
    }
  }

  // FIXME: We are missing an important validity check here that checks to
  // verify that there are no references to the global *other* than from the
  // main function.  This is generally true because we inline tensor ops
  // aggressively, but can be incorrect in some cases: e.g. a tensor-using
  // function is marked @noinline, or such a function just contains a copy.
  DenseMap<SILGlobalVariable*, GlobalAddrInst*> globalAddrRoots;
  for (auto &bb : fn) {
    for (auto bbi = bb.begin(), e = bb.end(); bbi != e; ) {
      auto &inst = *(bbi++);

      // Process GlobalAddrInst's.
      auto ga = dyn_cast<GlobalAddrInst>(&inst);
      if (!ga || !tfc.containsTensorFlowValue(ga->getType()))
        continue;

      // Check to see if this is the first global_addr for this global
      // variable.  If not, we reuse the existing one, which we know dominates
      // our current code.
      auto &entry = globalAddrRoots[ga->getReferencedGlobal()];
      if (entry) {
        ga->replaceAllUsesWith(entry);
        ga->eraseFromParent();
        continue;
      }

      // Otherwise, this is the first one, and it will be our canonical
      // pointer.  If we have a global_alloc, then it starts out uninitialized
      // but if we don't (as in the case of the REPL) it is known to be
      // previously initialized.
      auto allocGlobal = allocGlobals[ga->getReferencedGlobal()];
      entry = ga;
      addressRoots.push_back({ ga, /*isUninit*/allocGlobal != nullptr });

      // If this global_addr is in the entry block, then it will dominate any
      // other ones: we know it is the first in the entry block (because we
      // scan top to bottom) and we know the entry block dominates everything
      // else.
      if (ga->getParent() == fn.getEntryBlock())
        continue;

      // Otherwise, we aren't sure it will dominate all uses.  If we saw an
      // alloc_global instruction, move it right after that.  We know it will
      // dominate all uses.
      if (allocGlobal) {
        ga->moveAfter(allocGlobal);
        continue;
      }

      // Otherwise, move this to the entry block.
      ga->moveBefore(fn.getEntryBlock()->getTerminator());
    }
  }
}

/// Once we've found address roots that we're interested in, walk their uses to
/// see if they are doing things we have confidence in promoting.  Notably, we
/// cannot promote something that escapes the pointer.
///
bool PromotableMemoryFinder::canAddressRootBeReliablyPromoted(SILValue root) {
  // Check all uses of the root, including direct aliases formed by things
  // like begin_access.
  SmallVector<SILValue, 4> addrWorklist;
  addrWorklist.push_back(root);

  while (!addrWorklist.empty()) {
    auto addr = addrWorklist.pop_back_val();

    // Walk the use chains of the addr, looking for stores to it.  Any store
    // to it produces a value that feeds it, which can add new stack allocations
    // to our set.
    for (auto *use : addr->getUses()) {
      auto user = use->getUser();

      // Take an extremely conservative approach to handling accesses of the
      // global, whitelisting specific sorts of uses.  If we find anything
      // we can't handle, we abort promotion of this root.
      if (isa<EndAccessInst>(user) ||    // Just a marker.
          isa<LoadInst>(user) ||         // Reads are always ok.
          isa<DebugValueAddrInst>(user)) // Debug info is ok.
        continue;

      // Anything that dives into an element of the global can continue to
      // dive into the promoted value.
      if (isa<StructElementAddrInst>(user) || isa<TupleElementAddrInst>(user))
        continue;

      // If this is a store *to* the global, analyze the input value.
      if (auto *si = dyn_cast<StoreInst>(user)) {
        if (use->getOperandNumber() == 1) {
          findPromotableMemoryFromValue(si->getOperand(0));
          continue;
        }
      }

      // If this is a begin_access instruction, then it is a projection/copy
      // of the address.  Analyze it too.
      if (auto *begin = dyn_cast<BeginAccessInst>(user)) {
        addrWorklist.push_back(begin);
        continue;
      }

      // If this is an apply_inst passing the global's address as an indirect
      // operand, then we are ok.  These generally get inlined, but can occur
      // when the user specifies @noinline on a method, for example.
      //
      if (auto *apply = dyn_cast<ApplyInst>(user)) {
        // FIXME: This seems wrong, because it is not counting indirect results.
        // See DIMemoryUseCollector's use of getSubstCalleeConv for an example.
        auto conventions = apply->getSubstCalleeConv();
        assert(conventions.getNumIndirectSILResults() == 0 &&
               "FIXME: Handle this");

        unsigned opIdx = use->getOperandNumber();
        if (auto argIndex = apply->getArgumentIndexForOperandIndex(opIdx)) {
          auto paramConvention =
            conventions.getParameters()[argIndex.getValue()].getConvention();
          if (isIndirectFormalParameter(paramConvention))
            continue;
        }
      }


      // Some other unexpected user of the address is left around.  We should
      // handle this some day, but for now just leave the global access
      // unchanged, to avoid miscompiling code.
      if (getTFDumpIntermediateStream() == &llvm::outs()) {
        // Make this a hard error in the testsuite.
        llvm::errs() << "unexpected global_addr user in top level code"
                     << " promotion: " << *user << "\n\n";
        llvm::errs() << *user->getFunction();
        llvm::errs() << "unexpected global_addr user in top level code"
                     << " promotion: " << *user << "\n\n";
        abort();
      }

      return false;
    }
  }

  return true;
}


/// Our dataflow analysis of tensor operations has decided that some number of
/// address roots need to be promoted to SSA in order to perform deabstraction,
/// and has verified that this is safe.  Perform this transformation now.
void PromotableMemoryFinder::
promoteAddressRootsToStack(ArrayRef<std::pair<SILValue, bool>> addressRoots) {
  llvm::PrettyStackTraceFormat X("PromotableMemoryFinder::"
                                 "promoteAddressRootsToStack");

  DenseMap<SILValue, AllocStackInst*> stackAllocForRoot;

  // Promote each root by making a stack allocation that corresponds to them,
  // inserting loads and stores to the real root, and replacing the uses of
  // the root instructions with the stack allocation.
  for (auto rootInfo : addressRoots) {
    auto root = rootInfo.first;

    // Create a stack allocation in the entry block for the function.
    SILBuilder B(&fn.getEntryBlock()->front());
    auto stackAlloc = B.createAllocStack(fn.getLocation(),
                                         root->getType().getObjectType());
    stackAllocForRoot[root] = stackAlloc;

    // Make sure to convert the generated alloc_stack to SSA.
    stackAllocs.push_back(stackAlloc);

    // Replace all uses of the root with the stack value.
    root->replaceAllUsesWith(stackAlloc);
  }

  // Find all exit blocks from the function.
  SmallVector<SILBasicBlock*, 4> exitBlocks;
  for (auto &bb : fn) {
    if (isa<ReturnInst>(bb.getTerminator()) ||
        isa<ThrowInst>(bb.getTerminator()) ||
        isa<UnwindInst>(bb.getTerminator()))
      exitBlocks.push_back(&bb);
  }


  // Insert a stack deallocation plus cleanup in all of the exit blocks.
  for (auto rootInfo : addressRoots) {
    auto root = rootInfo.first;
    auto stackAlloc = stackAllocForRoot[root];
    assert(stackAlloc && "where'd our alloc_stack go?");

    // In some cases like global variables in top level code, the root will
    // start out uninitialized.  In other cases, it is already initialized - as
    // in indirect arguments to functions or REPL code that reuses a global.
    // If it is initialize, emit code to do so.
    if (!rootInfo.second) {
      auto insertionPoint = rootInfo.first->getDefiningInstruction();

      // Insert the initialization after the root or stack alloc.
      if (!insertionPoint) insertionPoint = stackAlloc;
      SILBuilder B(++SILBasicBlock::iterator(insertionPoint));
      auto loc = fn.getLocation();

      auto &TL = B.getTypeLowering(stackAlloc->getType());
      TL.emitCopyInto(B, loc, root, stackAlloc, IsTake_t::IsNotTake,
                      IsInitialization_t::IsInitialization);
    }

    // Process each exit block, inserting epilog code.
    for (auto *exit : exitBlocks) {
      SILBuilder B(exit->getTerminator());
      auto loc = fn.getLocation();

      // Load from the stack allocation and store to the root, leaving it
      // initialized with our final state.

      // If the root started out uninitialized, then this is an initialization
      // of it, otherwise this is a reassignment of it.
      auto &TL = B.getTypeLowering(stackAlloc->getType());
      TL.emitCopyInto(B, loc, stackAlloc, root, IsTake_t::IsTake,
                      IsInitialization_t(rootInfo.second));

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
  for (auto UI = alloc->use_begin(); UI != alloc->use_end();) {
    auto inst = (*UI)->getUser();

    if (auto sea = dyn_cast<StructElementAddrInst>(inst))
      if (auto *use = sea->getSingleUse()) {
        // If we have a load(struct_element_addr(alloc)) turn it into
        // struct_extract(load(alloc)).
        if (auto *load = dyn_cast<LoadInst>(use->getUser())) {
          SILBuilder B(load);
          auto *newLoad = B.createLoad(load->getLoc(), sea->getOperand(),
                                       load->getOwnershipQualifier());
          auto *newVal = B.createStructExtract(load->getLoc(), newLoad,
                                               sea->getField(),
                                               load->getType());
          load->replaceAllUsesWith(newVal);
          load->eraseFromParent();
          ++UI;
          sea->eraseFromParent();
          continue;
        }
      }

    // Explode aggregate by-address instructions like copy-addr.
    if (explodeAggregateInst(inst, /*all types*/nullptr)) {
      ++UI;
      inst->eraseFromParent();
      continue;
    }

    // If we have an instruction other than begin_access, remember it.
    auto *begin = dyn_cast<BeginAccessInst>(inst);
    if (!begin) {
      ++UI;
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
        use->set(alloc);
      }
    }

    ++UI;
    begin->eraseFromParent();
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
static GraphOperationInst *tryToPromoteTensorFromScalars(
    ApplyInst *inst, const DenseMap<SILValue, SymbolicValue> &constants,
    GraphFunctionDeviceInfo &deviceInfo) {
  assert(inst->getNumOperands() == 3 && isTensorHandle(inst->getType()) &&
         "Unexpected type signature for __tf_tensor_from_scalars");

  auto scalarsValue = inst->getOperand(1);
  auto shapeValue = inst->getOperand(2);

  // If we can't analyze the operands as arrays of constants, give up.
  auto scalarIt = constants.find(scalarsValue);
  if (scalarIt == constants.end() || !scalarIt->second.isConstant())
    return nullptr;
  auto scalars = scalarIt->second;
  CanType scalarsElementType;
  auto scalarsElements = scalars.getArrayValue(scalarsElementType);

  auto shapeIt = constants.find(shapeValue);
  if (shapeIt == constants.end() || !shapeIt->second.isConstant())
    return nullptr;
  auto shape = shapeIt->second;

  CanType shapeElementType;
  auto shapeElements = shape.getArrayValue(shapeElementType);

  // Verify we have the right number of scalars.  If not, emit an error and
  // leave the broken code without promoting it to an op.
  uint64_t scalarCount = 1;
  for (auto elt : shapeElements) {
    if (!elt.isConstant()) return nullptr;
    elt = elt.lookThroughSingleElementAggregates();
    scalarCount *= elt.getIntegerValue().getLimitedValue();
  }
  uint64_t numElements = scalarsElements.size();
  if (scalarCount != numElements) {
    std::string errorInfo =
      "tensor literal should have " + llvm::utostr(scalarCount) +
      " scalars for this shape, but has " + llvm::utostr(numElements);

    auto loc = getUserSourceLocation(inst);
    auto &context = inst->getType().getASTContext();
    diagnose(context, loc.getSourceLoc(), diag::tf_op_misuse, errorInfo)
      .highlight(loc.getSourceRange());
    return nullptr;
  }

  // Okay, we were able to resolve the two arrays of constants.  Transform this
  // into the correct Const operation.
  SILBuilder B(inst);
  B.setCurrentDebugScope(inst->getDebugScope());
  auto result =
      createConstTensor(scalarsElementType, scalars, shape, inst->getType(),
                        inst->getLoc(), deviceInfo.primaryDeviceType, B);

  // Replace the old instruction with the new one.
  inst->replaceAllUsesPairwiseWith(result);

  // Clean up the IR.
  auto callee = dyn_cast<FunctionRefInst>(inst->getOperand(0));
  inst->eraseFromParent();
  if (callee && callee->use_empty())
    callee->eraseFromParent();
  return result;
}

/// If all the operands to a call to __tf_tensor_from_scalars_1d are constants,
/// we can promote this to a 'Const' node with an attached TF_Tensor attribute.
/// This is a specialized form of __tf_tensor_from_scalars, because the later is
/// defined in terms of a shape of "[scalars.count]" but the performance
/// optimizer is not reliably constant propagating this.
///
/// When we have a the ability constexpr fold array.count, we should be able to
/// eliminate this in favor of library code in the TensorFlow module.
///
/// On success, this removes the applyexpr and returns a pointer to the new
/// instruction that it created.  On failure, it returns a nullptr.
///
static GraphOperationInst *tryToPromoteTensorFromScalars1D(
    ApplyInst *inst, const DenseMap<SILValue, SymbolicValue> &constants,
    GraphFunctionDeviceInfo &deviceInfo) {
  assert(inst->getNumOperands() == 2 && isTensorHandle(inst->getType()) &&
         "Unexpected type signature for __tf_tensor_from_scalars_1d");

  auto arrayValue = inst->getOperand(1);

  // If we can't analyze the scalars as an arrays of constants, give up.
  auto scalarIt = constants.find(arrayValue);
  if (scalarIt == constants.end() || !scalarIt->second.isConstant())
    return nullptr;

  auto scalars = scalarIt->second;
  assert(scalars.getKind() == SymbolicValue::Array &&
         "Unexpected value for array constant");
  CanType scalarElementType;
  auto scalarElements = scalars.getArrayValue(scalarElementType);

  auto &context = inst->getType().getASTContext();
  auto &allocator = context.getAllocator();

  // This takes a Tensor operand, but needs a shape added.  Since this is 1d,
  // our shape is just a single entry array with the length of scalars, like
  // [i32 42] if the scalars list is 42 entries in size.
  auto shape = SymbolicValue::getArray({
    SymbolicValue::getInteger(scalarElements.size(), /*bitwidth*/ 32)
  }, context.getInt32Decl()->getDeclaredType()->getCanonicalType(), allocator);

  SILBuilder B(inst);
  B.setCurrentDebugScope(inst->getDebugScope());
  auto result =
      createConstTensor(scalarElementType, scalars, shape, inst->getType(),
                        inst->getLoc(), deviceInfo.primaryDeviceType, B);

  // Replace the old instruction with the new one.
  inst->replaceAllUsesPairwiseWith(result);

  // Clean up the IR.
  auto callee = dyn_cast<FunctionRefInst>(inst->getOperand(0));
  inst->eraseFromParent();
  if (callee && callee->use_empty())
    callee->eraseFromParent();
  return result;
}

/// Canonicalize tensor ops, validating that attribute arguments are constant
/// expressions, and transform the IR to use GraphOperationInst.
void TFDeabstraction::checkAttributesAndFormGraphOps() {
  llvm::PrettyStackTraceFormat
  X("TFDeabstraction::checkAttributesAndFormGraphOps");

  auto deviceInfo =
      GraphFunctionDeviceInfo::getForFunction(fn, /*removeConfigInst*/ false);

  // Do a big sweep over all of the operands to tensor values, collecting ones
  // that we might be interested in being constants into a single list.
  SmallVector<SILValue, 32> valuesToCheck;

  for (auto *op : tensorOps) {
    for (auto &operand : op->getAllOperands()) {
      // Dump anything that might be an attribute into the list without too much
      // filtering.  We take out TensorFlow values since they are the most
      // obvious ones we don't care about later, but there may be other minor
      // things we over-query on.
      auto value = operand.get();
      if (!isTensorFlowValue(value->getType()))
        valuesToCheck.push_back(value);
    }
  }

  // Eliminate duplicates and sort the array of values so we have an efficient
  // way to query it later.
  llvm::array_pod_sort(valuesToCheck.begin(), valuesToCheck.end());
  valuesToCheck.erase(std::unique(valuesToCheck.begin(), valuesToCheck.end()),
                      valuesToCheck.end());

  // Determine whether each value is a constant or not.
  // TODO: Capture information about *WHY* values are not constants, e.g. the
  // first SIL instruction that could not be folded.
  SmallVector<SymbolicValue, 32> results;
  constantEvaluator.computeConstantValues(valuesToCheck, results);
  assert(valuesToCheck.size() == results.size() && "incorrect values returned");

  // Transform the returned information about constants into a map that we can
  // query.  The results list should correspond directly to the values we asked
  // about.
  DenseMap<SILValue, SymbolicValue> constants;
  for (unsigned i = 0, e = valuesToCheck.size(); i != e; ++i)
    constants.insert({valuesToCheck[i], results[i]});

  // Now that we've computed whether any of the operands are constants,
  // substitute them into the operations that we have, eliminating abstractions.
  // This makes it immediately obvious to partitioning what is and isn't a
  // constant.
  for (auto *&inst : tensorOps) {
    // If this is a normal tensor operation, validate it and transform it into a
    // graphOp instruction.
    if (TFStrictDeabstraction) {
      if (auto opInfo = SILTensorOpInfo::decode(inst)) {
        // Do not translate this special inst into a graph op, since it will get
        // removed at the beginning of the partition pass.
        // TODO: remove this inst in the getForFunction() call above, once
        // the partition pass is folded into deabstraction.
        if (opInfo->opName == "tfc.configureTPU" ||
            opInfo->opName == "tfc.configureGPU")
          continue;
        formGraphOp(*opInfo, constants, deviceInfo);
      }
    }

    // Take a look at the various well known function calls that we can promote
    // to tensor operations.  We can promote them if we are able to constant
    // fold all of the operands to these calls.  If so, we rewrite them in terms
    // of a proper op, and partitioning will continue to treat them that way.
    if (auto apply = dyn_cast<ApplyInst>(inst)) {
      auto callee = apply->getCalleeFunction();

      if (callee && callee->getName() == "__tf_tensor_from_scalars") {
        if (auto result =
                tryToPromoteTensorFromScalars(apply, constants, deviceInfo))
          inst = result;
        continue;
      }
      if (callee && callee->getName() == "__tf_tensor_from_scalars_1d") {
        if (auto result =
                tryToPromoteTensorFromScalars1D(apply, constants, deviceInfo))
          inst = result;
        continue;
      }
    }
  }
}

/// If the specified type is a Swift.Array or some element type, then return the
/// element type.  Otherwise, return a null Type.
static Type getArrayElementType(Type ty) {
  if (auto bgst = ty->getAs<BoundGenericStructType>())
    if (bgst->getDecl() == bgst->getASTContext().getArrayDecl())
      return bgst->getGenericArgs()[0];
  return Type();
}

/// Return true if this is a reference to the _allocateUninitialized helper
/// in array in the standard library allocating zero elements.
/// TODO: Move to deabstraction when other clients are removed.
bool isArrayAllocUninit(SILValue op, SILValue &numElements);

namespace {
/// This is a little helper for working with literal arrays that may want to get
/// deleted if all references to them are removed.
struct ArrayElementDecoder {
  SmallVector<SILValue, 4> elements;
  SmallPtrSet<SILInstruction*, 8> arrayInsts;

  /// Given a SILValue that may be an array, attempt to decode it into the
  /// literal values that make up its elements.  This returns the element type
  /// of the array if it succeeds, otherwise a null type.
  Type decode(SILValue value) {
    auto elementType = getArrayElementType(value->getType().getASTType());
    if (!elementType) return Type();

    // The only pattern we support involves a call to
    // _allocateUninitializedArray.  The array value will be a tuple extract
    // from the 0th result of the call.
    auto *teiValue = dyn_cast<TupleExtractInst>(value);
    if (!teiValue || teiValue->getFieldNo() != 0 ||
        !isa<ApplyInst>(teiValue->getOperand()))
      return Type();

    // Figure out the number of elements, which must be a constant integer.
    auto *apply = cast<ApplyInst>(teiValue->getOperand());

    if (decodeApply(apply))
      return elementType;
    return Type();
  }

  /// Given an applyinst for _allocateUninitialized, try to decode it.  This
  /// returns true on success or false on failure.
  bool decodeApply(ApplyInst *apply) {
    // Verify we have a call to _allocateUninitializedArray.
    SILValue numElementsVal;
    if (!isArrayAllocUninit(apply, numElementsVal) ||
        !isa<IntegerLiteralInst>(numElementsVal))
      return false;
    uint64_t numElements =
    cast<IntegerLiteralInst>(numElementsVal)->getValue().getLimitedValue();

    return !tf::ConstExprEvaluator::
    decodeAllocUninitializedArray(apply, numElements, elements, &arrayInsts);
  }

  /// Try to remove the instructions that make up the array initialization.
  void removeInstructionsIfPossible(SILBasicBlock::iterator &it) {
    if (arrayInsts.empty())
      return;

    // If the iterator is pointing to an instruction we're about to remove,
    // advance it to avoid invalidation.
    while (arrayInsts.count(&*it))
      ++it;

    // If we can remove it, drop all inter-dependent references.
    for (auto inst : arrayInsts)
      inst->dropAllReferences();
    // Then erase the instructions themselves.
    for (auto inst : arrayInsts)
      inst->eraseFromParent();
  }
};
} // end anonymous namespace



/// A reference to the specified array was just dropped.  If it was a literal
/// array and this was the last use, clean up the instructions that fed it.
///
static void removeArrayValueIfPossible(ApplyInst *array,
                                       SILBasicBlock::iterator &it) {
  // If this array is a literal that we can decode, remove the instructions that
  // it came from.
  ArrayElementDecoder decoder;
  if (decoder.decodeApply(array))
    decoder.removeInstructionsIfPossible(it);
}

/// Deabstraction can leave around lots of dead instructions from the attributes
/// that get promoted, including heavy weight types that inline into a lot of
/// code, like arrays.  Do a quick pass to remove these, improving compile time
/// of subsequent passes.
void TFDeabstraction::cleanupDeadInstructions() {
  llvm::PrettyStackTraceFormat
  X("TFDeabstraction::cleanupDeadInstructions");

  for (auto &block : fn) {
    for (auto instIt = block.begin(), e = block.end(); instIt != e; ) {
      // Manually move iterator to avoid invalidation if we remove 'inst'.
      auto *inst = &*instIt++;

      // Do a quick job to remove obvious dead instructions.  Don't delete debug
      // instructions.
      if (isa<SingleValueInstruction>(inst) &&
          isInstructionTriviallyDead(inst)) {
        recursivelyDeleteTriviallyDeadInstructions(inst);
        continue;
      }

      // The original call took the array at +0, so we don't need to destroy it.
      // however, we want to clean up the mess to make the resultant IR simpler.
      // Do so now if possible.
      if (auto *apply = dyn_cast<ApplyInst>(inst)) {
        removeArrayValueIfPossible(apply, instIt);
        continue;
      }

      // TODO: Remove dead stack slots for tensors being passed as input lists.

      // TODO: need to delete TensorShape.init and other methods that we
      // shouldn't bake in special knowledge of.  Needs integration with
      // ConstExpr.

      // TODO: Handle String.init and other stuff.  Refactor this when ConstExpr
      // handling subsumes ConstantPropagation.
    }
  }
}


// TODO: move this from TFUtilities when deabstraction is done.
SILValue getTensorProtocolHandleMember(SILValue v, SILLocation loc,
                                       SILBuilder &B);

/// Replace the specified tensor operation with a GraphOperation instruction,
/// emitting errors if attribute arguments could not be constant folded, or if
/// the operand/attribute types are incorrect.
void TFDeabstraction::formGraphOp(SILTensorOpInfo &opInfo,
                                  DenseMap<SILValue, SymbolicValue> &constants,
                                  GraphFunctionDeviceInfo &deviceInfo) {
  auto *inst = opInfo.inst;
  auto &context = inst->getFunction()->getASTContext();
  auto &allocator = context.getAllocator();
  SILBuilder B(opInfo.inst);

  // This is a helper function to emit a diagnostic.
  auto diagnoseInvalid = [&](const Twine &message) {
    auto loc = getUserSourceLocation(inst);
    diagnose(fn.getModule().getASTContext(), loc.getSourceLoc(),
             diag::tf_op_misuse, message.str())
      .highlight(loc.getSourceRange());
  };

  std::string opName = opInfo.opName.str();
  SmallVector<SILValue, 4> inputs;
  SmallVector<GraphOperationAttribute, 4> attributes;

  // Find the device attribute specified for the instruction if present.
  StringRef opDevice;

  for (unsigned i = 0, e = opInfo.operandClasses.size(); i != e; ++i) {
    auto operand = inst->getOperand(i);
    auto operandTy = operand->getType();
    auto operandClass = opInfo.operandClasses[i];

    // Collect and validate input operands.
    if (opInfo.isInput(i)) {

      // Each input gets a marker mangled into the op name, because we need to
      // be able to distinguish between normal inputs and elements of an input
      // list.

      // If this is tfc.scalarToTensor, then the input must be a valid scalar.
      if (opInfo.opName == "tfc.scalarToTensor") {
        auto scalarType = operandTy.getASTType();
        if (convertSwiftTypeToTF(scalarType) == 0) {
          diagnoseInvalid("scalarToTensor requires scalar value; unrecognized"
                          " type '" + scalarType->getString() +
                          "' is not allowed");
          return;
        }
        opName +=
          GraphOperationInfo::getInputMarker(GraphOperationInfo::IM_Scalar);
        inputs.push_back(operand);
        continue;
      }

      // Tensor operand type's are ok.
      if (isTensorFlowValue(operandTy)) {
        opName +=
          GraphOperationInfo::getInputMarker(GraphOperationInfo::IM_Normal);
        inputs.push_back(operand);
        continue;
      }

      // Remove the operand so decodeArrayElements doesn't see the use.
      inst->setOperand(i, SILUndef::get(operand->getType(),
                                        fn.getModule()));

      // Otherwise this must be an array, representing an input list.
      ArrayElementDecoder arrayDecoder;
      auto elementType = arrayDecoder.decode(operand);
      if (!elementType) {
        diagnoseInvalid("operand has unrecognized type '" +
                        operandTy.getASTType()->getString() + "'");
        return;
      }

      // Remember that we have an input list, this marker is important because
      // the list may be empty, and we need to know it exists in graph lowering.
      opName +=
        GraphOperationInfo::getInputMarker(GraphOperationInfo::IM_InputList);
      const char *elementMarker =
        GraphOperationInfo::getInputMarker(GraphOperationInfo::IM_InputListElt);

      // Add each element to the input list we're building, drilling down
      // through any struct wrappers (like Tensor/Tensor2D, etc) that may be
      // around it.
      //
      // It is common to have arrays with repeated elements.  These will
      // generally be uniqued on entry to this routine.  If so, make sure to
      // reuse them as we project out the .handle members to avoid code bloat.
      llvm::DenseMap<SILValue, SILValue> loweredElts;
      for (auto elt : arrayDecoder.elements) {
        auto &eltVal = loweredElts[elt];
        if (!eltVal) {
          eltVal = getTensorProtocolHandleMember(elt, inst->getLoc(), B);
          if (!eltVal) {
            diagnoseInvalid("elements to input list have invalid type '" +
                            elementType->getString() + "'");
            return;
          }
        }
        elt = eltVal;

        opName += elementMarker;
        inputs.push_back(elt);
      }
      continue;
    }

    // Helper to diagnose invalid attributes.
    auto diagnoseInvalidAttr = [&](const Twine &message) {
      diagnoseInvalid(Twine("attribute '") + operandClass.first.str() +
                      "' " + message.str());
    };

    // Ok, we have an attribute operand, we should have been able to fold it
    // through our constexpr evaluation logic.
    auto it = constants.find(operand);
    assert(it != constants.end() && "out of sync with constant scanning loop");

    if (!it->second.isConstant()) {
      // TODO: improve the diagnostic to talk about the parameter label in
      // the user code, not the internal op attribute.  The bookkeeping for
      // this isn't obvious though.
      diagnoseInvalidAttr("requires a constant argument");

      // If we have more specific information about what went wrong, emit
      // notes.
      if (it->second.getKind() == SymbolicValue::Unknown)
        it->second.emitUnknownDiagnosticNotes(opInfo.inst->getLoc());
      return;
    }

    // Name mangle the attribute classification into the operand list so we can
    // know the difference between a shape and an array, and a shape array, etc.
    auto attrName = operandClass.first.str() +
       SILTensorOpInfo::getOperandClassSuffix(operandClass.second);

    // Get the constant, ignoring struct wrappers.
    auto constValue = it->second.lookThroughSingleElementAggregates();

    // Clone it out of the ConstExpr pool into the global SILModule pool.
    constValue = constValue.cloneInto(allocator);

    auto attrIdentifier = context.getIdentifier(attrName);
    attributes.push_back({ attrIdentifier, constValue });

    // FIXME: Do we detect and reject duplicate attribute names already?

    // If it's a device attribute, get the device value.
    if (operandClass.first == DEVICE_ATTR) {
      if (constValue.getKind() != SymbolicValue::String)
        return diagnoseInvalidAttr("must be a string");
      opDevice = constValue.getStringValue();
      // User code should not specify this pseudo device.
      if (opDevice == ALL_DEVICES)
        return diagnoseInvalidAttr("may not use this device name");
    }

    auto verifyShapeAttr = [&](SymbolicValue constValue) -> bool {
      // strip away the possible aggregate wrapper.
      constValue = constValue.lookThroughSingleElementAggregates();
      if (constValue.getKind() != SymbolicValue::Array) {
        diagnoseInvalidAttr("requires an array");
        return true;
      }
      CanType eltType;
      auto elements = constValue.getArrayValue(eltType);
      if (!StringRef(eltType->getString()).startswith("Int")) {
        diagnoseInvalidAttr("requires an array of ints");
        return true;
      }
      for (auto elt : elements) {
        // strip away the possible aggregate wrapper.
        elt = elt.lookThroughSingleElementAggregates();
        if (elt.getKind() != SymbolicValue::Integer) {
          diagnoseInvalidAttr("requires an array of ints");
          return true;
        }
      }
      return false;
    };
    // Verify that the type of this attribute is ok for the OperandClass we
    // have.
    switch (operandClass.second) {
    case SILTensorOpInfo::OperandClass::Input:
    case SILTensorOpInfo::OperandClass::InputElt:
    case SILTensorOpInfo::OperandClass::Normal:  // No modifier.
      break;
    case SILTensorOpInfo::OperandClass::DType:
      // This integer value is a dtype.
      if (constValue.getKind() != SymbolicValue::Integer)
        return diagnoseInvalidAttr("requires a constant integer");
      break;
    case SILTensorOpInfo::OperandClass::Array:
      if (constValue.getKind() != SymbolicValue::Array)
        return diagnoseInvalidAttr("requires an array");
      break;
    case SILTensorOpInfo::OperandClass::Shape:
      if (verifyShapeAttr(constValue))
        return; // error already emitted.
      break;
    case SILTensorOpInfo::OperandClass::ShapeArray: {
      if (constValue.getKind() != SymbolicValue::Array)
        return diagnoseInvalidAttr("requires an array");
      CanType eltType;
      auto shapes = constValue.getArrayValue(eltType);
      if (eltType->getString() != "TensorShape")
        return diagnoseInvalidAttr("requires an array of TensorShape values");
      for (auto shape : shapes) {
        if (verifyShapeAttr(shape))
          return; // error already emitted.
      }
      break;
    }
    case SILTensorOpInfo::OperandClass::ArrayElement:
      // Match logic from checkAndDiagnoseOperands.
      inst->dump();
      assert(0 && "FIXME: array elem exprs aren't handled yet");
    case SILTensorOpInfo::OperandClass::Tensor:
      // FIXME: There needs to be a dtype attribute before this.

      if (constValue.getKind() == SymbolicValue::Integer ||
          constValue.getKind() == SymbolicValue::Float)
        break;

      if (constValue.getKind() != SymbolicValue::Array)
        return diagnoseInvalidAttr("requires a constant that is an integer,"
                                   " floating point, or array thereof");

      CanType eltType;
      auto elements = constValue.getArrayValue(eltType);

      /// Tensor array arguments must always be followed by a shape.
      if (i+1 >= opInfo.operandClasses.size() ||
          opInfo.operandClasses[i+1].second !=
                      SILTensorOpInfo::OperandClass::Shape)
        return diagnoseInvalidAttr("tensor attributes must be followed by "
                                   "a shape attribute");

      // TODO: Decode the shape and validate that it matches the # elements
      // we have.

      // Empty tensor value is ok.
      if (elements.empty()) break;

      auto firstElt = elements.front().lookThroughSingleElementAggregates();

      // Verify that all the elements are the same type, and that they are
      // either integer or FP.
      if (firstElt.getKind() == SymbolicValue::Integer) {
        for (auto elt : elements) {
          elt = elt.lookThroughSingleElementAggregates();
          if (elt.getKind() != SymbolicValue::Integer ||
              elt.getIntegerValueBitWidth() !=
              firstElt.getIntegerValueBitWidth())
            return diagnoseInvalidAttr("array values must be the same type");
        }
      } else if (firstElt.getKind() == SymbolicValue::Float) {
        for (auto elt : elements) {
          elt = elt.lookThroughSingleElementAggregates();
          if (elt.getKind() != SymbolicValue::Float ||
              elt.getFloatValueSemantics() != firstElt.getFloatValueSemantics())
            return diagnoseInvalidAttr("array values must be the same type");
        }
      } else {
        return diagnoseInvalidAttr("requires a constant that is an integer,"
                                   " floating point, or array thereof");
      }
      break;
    }
  }

  // Finally, set a device attribute for this if there wasn't already one
  // specified.
  if (opDevice.empty()) {
    deviceInfo.handleDevicePlacement(opInfo.opName, opDevice, context,
                                     attributes);
  }
  // Okay, if we got this far then we have all valid attributes and inputs.
  // Figure out the result list.
  SmallVector<SILType, 4> resultTypes;
  if (auto tuple = inst->getType().getAs<TupleType>()) {
    for (auto elt : tuple->getElements()) {
      auto eltTy = elt.getType()->getCanonicalType();
      resultTypes.push_back(SILType::getPrimitiveObjectType(eltTy));
    }
  } else {
    resultTypes.push_back(inst->getType());
  }

  auto op = B.createGraphOperation(inst->getLoc(),
                                   context.getIdentifier(opName), inputs,
                                   attributes, resultTypes);
  op->setDebugLocation(inst->getDebugLocation());
  
  if (auto tuple = inst->getType().getAs<TupleType>()) {
    SmallVector<SILValue, 4> elts;
    for (unsigned i = 0, e = tuple->getNumElements(); i != e; ++i)
      elts.push_back(op->getResult(i));
    auto tupleResult = B.createTuple(inst->getLoc(), elts);
    tupleResult->setDebugLocation(inst->getDebugLocation());

    inst->replaceAllUsesWith(tupleResult);
    inst->eraseFromParent();
  } else {
    inst->replaceAllUsesWith(op->getResult(0));
    inst->eraseFromParent();
  }

  // TODO: Analyze the operands to the instruction and remove them if they are
  // now dead.
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
  // that we want to promote to SSA.
  SmallVector<AllocStackInst*, 16> stackAllocs;
  if (PromotableMemoryFinder(stackAllocs, tfc, fn).run(tensorOps)) {
    logCurrentState("After promoteAddressRootsToStack",
                    /*detailed*/true);
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

  // Canonicalize attribute arguments, check that they have constants,
  // flatten array attributes, and form graph_op instructions.
  checkAttributesAndFormGraphOps();

  logCurrentState("Before Cleanup", /*detailed*/true);

  // Remove code that is dead now that tensor operations are formed.
  cleanupDeadInstructions();

  logCurrentState("Result", /*detailed*/false);

  // We're currently relying on the performance optimizer to do some stuff, but
  // for large testcacses it is doing bad stuff.
  // FIXME: Should be eliminated when partitioning happens as part of
  // deabstraction, because then the optimizer won't be seeing all of our tensor
  // stuff.
  if (!TFStrictDeabstraction)
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
  ConstExprEvaluator constantEvaluator(*module);

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
