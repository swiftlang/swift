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
#include "TFConstExpr.h"
#include "TFUtilities.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Module.h"
#include "swift/SIL/GraphOperationBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/PrettyStackTrace.h"

using namespace swift;
using namespace tf;
using llvm::DenseMap;

static llvm::cl::opt<bool>
TFDumpDeabstractionDetails("tf-dump-deabstraction-details",
                           llvm::cl::init(false),
           llvm::cl::desc("Dump extra details about TensorFlow deabstraction"));

// When this code path is enabled, it currently only works in some
// scenarios. The comments around the relevant code below provide more context.
static llvm::cl::opt<bool> TFPromoteGlobalVariables(
    "tf-promote-global-variables", llvm::cl::init(false),
    llvm::cl::desc(
        "If enabled, promote global variables into SSA with a best "
        "effort to minimize sends/recvs. This is a performance optimization."));

// TODO(marcrasi): This is a very temporary option allowing us to
// incrementally add IRGen support for non-deabstracted functions. As soon as
// IRGen fully supports non-deabstracted functions, remove this flag and make
// TFDeabstraction always be off for non-graph-only functions in dynamic
// compilation mode.
static llvm::cl::opt<bool>
TFDisableDeabstraction(
    "tf-disable-deabstraction", llvm::cl::init(false),
    llvm::cl::desc(
        "Disables deabstraction for everything except graph-only functions. "
        "-tf-dynamic-compilation must also be enabled when this is on."));

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
  return name == "__tf_tensor_from_scalar" ||
         name == "__tf_tensor_from_scalars" ||
         name == "__tf_tensor_from_scalars_1d";
}


namespace {
  /// This class wraps the state and logic necessary to deabstract code into one
  /// specific SIL function, which has been designated as a potential top-level
  /// host for tensor code.
  class TFDeabstraction {
    SILTransform &transform;
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
    /// by simplifyTensorOperands.  This contains both the graph_op instructions
    /// that reflect the #tfop() invocations, as well as any retain/release
    /// instructions using TensorHandle values.
    SmallVector<SILInstruction*, 32> tensorOps;
  public:
    TFDeabstraction(SILTransform &transform, SILFunction &fn,
                    TensorFunctionClassifier &tfc,
                    ConstExprEvaluator &constantEvaluator, SILPassManager *PM)
      : transform(transform), fn(fn), tfc(tfc),
        constantEvaluator(constantEvaluator), passManager(PM) {
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
    bool simplifyTensorOperands();

    void promoteToSSA(ArrayRef<AllocStackInst *> allocs);
    void prepareStackAllocForPromotion(AllocStackInst *alloc);
    void propagateSSAValues();
    void checkAttributesAndFormGraphOps();
    void evaluateAttributesAndDoPacking(
        GraphOperationInfo &opInfo,
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
  if (TFPromoteGlobalVariables && fn.getName() == SWIFT_ENTRY_POINT_FUNCTION) {
    forciblyFlattened = [&]() -> bool {
      for (auto &bb : fn)
        for (auto &i : bb)
          if (auto *inst = dyn_cast<GlobalAddrInst>(&i)) {
            if (tfc.containsTensorFlowValue(
                    inst->getType(), /*checkHigherOrderFunctions*/ false))
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

    // In dynamic compliation mode, do not inline functions from the same
    // module. (We still inline functions from other modules, because there is a
    // risk that the other module was not compiled in dynamic compilation mode
    // and therefore its functions are not deabstracted or lowered. For
    // example, we do not compile the standard library in dynamic compilation
    // mode.)
    if (llvm::TFDynamicCompilation && !isAcceleratorOnly(fn) &&
        !callee.isAvailableExternally())
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
    if (!tfc.containsTensorFlowValue(type, /*checkHigherOrderFunctions*/ true))
      return false;

    return true;
  };

  SmallPtrSet<SILFunction*, 16> inlinedCallees;

  // Use the mandatory inlining algorithm to expose call sites that contain
  // TensorFlow values as their argument or result lists.
  SILOptFunctionBuilder funcBuilder(transform);
  inlineForTFDeabstraction(funcBuilder, fn,
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

    passManager->notifyWillDeleteFunction(callee);

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

/// Scan the argument list of the graph_op.  If any argument is passed indirectly
/// (i.e., an address of a stack location is passed instead of the value itself)
/// then rewrite the graph_op to use a loaded version of that value.
///
/// Similarly, if an argument is an indirect output, then rewrite the graph_op to
/// return the value directly, and emit an instruction that stores the direct
/// value to the indirect output address.
///
/// Also, if a primitive integer or floating point value is passed as a struct
/// value, extract out the underlying integer or float value.
///
/// Returns nullptr on error.
///
static GraphOperationInst *simplifyOperands(GraphOperationInst *origInst,
                                            TFDeabstraction &TFDA) {
  auto *fn = origInst->getFunction();
  auto &ctx = fn->getASTContext();
  GraphOperationInfo opInfo(origInst);

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
      auto field = tf::getFieldIfContainsSingleField(decl);
      if (!field) return nullptr;

      // If this is the top level of the struct, retain the field decl.
      if (result == nullptr) result = field;

      type = field->getType();

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
    return type.isAddress() && type.isLoadable(origInst->getModule());
  };

  // Predicate that returns true if an argument of the specified type should be
  // rewritten to load an address argument or expand a struct parameter.
  auto canSimplifyArgumentType = [&](SILType type) -> bool {
    return isLoadableAddressType(type) ||
           getPrimitiveStructField(type.getASTType()) != nullptr;
  };

  // Predicate that returns true if an argument should be rewritten.
  auto canSimplifyArgument =
      [&](const GraphOperationInfo::StructuredArgument &argument) -> bool {
    switch (argument.getKind()) {
    case GraphOperationInfo::SAK_Single:
      return canSimplifyArgumentType(argument.getSingleArgument()->getType()) ||
             std::get<1>(argument.getArgumentNameAndLowering()) ==
                 GraphOperationInfo::ArgumentLowering::Out;
    case GraphOperationInfo::SAK_List:
      // We can get SAK_List arguments from inlining functions that have already
      // been deabstracted. These arguments do not need further simplification.
      return false;
    }
  };

  // If we don't have to change any arguments, don't rewrite the graph_op.
  bool mustChangeGraphOp = false;
  for (auto &argument : opInfo.getStructuredArguments()) {
    if (canSimplifyArgument(argument)) {
      mustChangeGraphOp = true;
      break;
    }
  }

  if (!mustChangeGraphOp) return origInst;

  // Mark the function as being mutated.
  TFDA.aboutToChangeFunction();

  // Okay, we do have to simplify something.  Scan through and rewrite arguments.
  SILBuilder B(origInst);
  GraphOperationBuilder opBuilder(opInfo.getOperationName());
  // Pass attributes through.
  for (auto &attr : origInst->getAttributes())
    opBuilder.addAttribute(attr);
  SILValue outParameterAddress;
  for (auto &argument : opInfo.getStructuredArguments()) {
    if (argument.getKind() == GraphOperationInfo::SAK_List) {
      // We can get SAK_List arguments from inlining functions that have already
      // been deabstracted. Pass these arguments through.
      opBuilder.addListArgument(argument.getArgumentList(),
                                argument.getArgumentNameWithSuffix());
      continue;
    }

    assert(argument.getKind() == GraphOperationInfo::SAK_Single &&
           "should have already handled all other argument kinds");
    auto argumentValue = argument.getSingleArgument();
    auto argumentLowering = std::get<1>(argument.getArgumentNameAndLowering());

    // If this is an out parameter, take note of it (so that we can deal with it
    // later), and exclude it from the list of rewritten arguments.
    if (argumentLowering == GraphOperationInfo::ArgumentLowering::Out) {
      auto argumentType = argumentValue->getType();
      assert(argumentType.isAddress());
      if (!argumentType.isLoadable(origInst->getModule())) {
        // When we're in dynamic compilation mode, it is okay to keep the
        // generic out parameter because dynamic compilation knows how to deal
        // with generic out parameters.
        if (llvm::TFDynamicCompilation && !isAcceleratorOnly(*fn)) {
          opBuilder.addArgument(argumentValue,
                                argument.getArgumentNameWithSuffix());
          continue;
        }

        auto astType = argumentType.getASTType();
        if (astType->hasArchetype()) {
          diagnose(ctx, getUserSourceLocation(origInst).getSourceLoc(),
                   diag::tf_op_result_generic, astType);
        } else {
          diagnose(ctx, getUserSourceLocation(origInst).getSourceLoc(),
                   diag::tf_op_result_not_value_or_aggregate, astType);
        }
        return nullptr;
      }
      assert(origInst->getNumResults() == 0 &&
             "graph_op with out parameter must have 0 results");
      // There should only be one output parameter because output parameter is
      // still a single value or single aggregate.
      assert(!outParameterAddress && "there is more than one out parameter");
      outParameterAddress = argumentValue;
      continue;
    }

    // If this is an address argument, emit a load of the value.
    if (isLoadableAddressType(argumentValue->getType())) {
      bool hasOwnership = origInst->getFunction()->hasQualifiedOwnership();
      auto loadOwnership = hasOwnership ? LoadOwnershipQualifier::Trivial
                                        : LoadOwnershipQualifier::Unqualified;
      auto load = B.createLoad(origInst->getLoc(), argumentValue, loadOwnership);
      load->setDebugLocation(origInst->getDebugLocation());
      argumentValue = load;
    }

    // If the argument is a StructInst building the value that we want to
    // extract, just get the element out of it, to avoid generating bloated IR.
    argumentValue = lookThroughSingleElementStructInsts(argumentValue);

    // If this is a struct value, emit struct extraction instruction(s).
    while (auto fieldDecl = getPrimitiveStructField(
                                     argumentValue->getType().getASTType())) {
      auto extract = B.createStructExtract(origInst->getLoc(), argumentValue,
                                           fieldDecl);
      extract->setDebugLocation(origInst->getDebugLocation());
      argumentValue = extract;
    }

    opBuilder.addArgument(argumentValue, argument.getArgumentNameWithSuffix());
  }

  // Now that we've rebuilt the argument list, create a new graph_op and replace
  // the old one.
  GraphOperationInst *newInst;
  if (outParameterAddress) {
    newInst = opBuilder.build(B, ctx, origInst->getLoc(),
                              {outParameterAddress->getType().getObjectType()});

    // Store the new instruction's result to the outParameterAddress.
    auto hasOwnership = newInst->getFunction()->hasQualifiedOwnership();
    auto storeOwnership = hasOwnership ? StoreOwnershipQualifier::Trivial
                                       : StoreOwnershipQualifier::Unqualified;
    B.createStore(origInst->getLoc(), newInst->getResult(0),
                  outParameterAddress, storeOwnership);
  } else {
    SmallVector<SILType, 4> origResultTypes(origInst->getResultTypes());
    newInst = opBuilder.build(B, ctx, origInst->getLoc(), origResultTypes);
  }
  newInst->setDebugLocation(origInst->getDebugLocation());

  // Replace the old with the new and delete the old instruction.
  if (origInst->getNumResults() > 0)
    origInst->replaceAllUsesPairwiseWith(newInst);

  // Remove the StructInst and other random values that we leave around in the
  // program, now that we directly refer to the TensorFlow values.
  deleteInstAndAbandonedUses(origInst);

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
  if (tfc &&
      !tfc->containsTensorFlowValue(type, /*checkHigherOrderFunctions*/ false))
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
      TL.emitLoweredCopyValueDirectChildren(B, inst->getLoc(),
                                            inst->getOperand(0));
  } else if (isa<ReleaseValueInst>(inst) || isa<StrongReleaseInst>(inst)) {
    // Turn a retain_value into a retain_value on its elements.  We peephole
    // StructInst values because they are so common and this generates cleaner
    // IR and faster compile times.
    auto op = lookThroughSingleElementStructInsts(inst->getOperand(0));
    if (op != inst->getOperand(0) && op->getType().isAnyClassReferenceType())
      B.createStrongRelease(inst->getLoc(), op, Atomicity::Atomic);
    else
      TL.emitLoweredDestroyValueDirectChildren(B, inst->getLoc(),
                                               inst->getOperand(0));
  } else {
    llvm_unreachable("unhandled instructions should be filtered above");
  }

  return true;
}

/// Identify all of the tensor operations in the current function, and scan them
/// to see if there are any indirect arguments, where the address of a stack
/// allocation is passed to the graph_op.  These occur when the tensor op was in
/// a generic context and was passed a scalar attribute value of generic type.
///
/// If we find one of these indirect values, transform it into a load of the
/// address and a use of the loaded value.  This allows the stack allocation to
/// be promoted, allowing us to construct SSA def-use chains.
///
/// Similarly, if we find an indirect output, replace it with a direct output
/// and a store of the outputted value.  This occurs when the tfop has been
/// inlined from a context where its output is address-only.
///
/// Also, if we see a struct operand that wraps a primitive value, we extract
/// out the underlying scalar value until we get to a builtin integer or
/// floating point value.
///
/// Since we're scanning the function, keep track of all of the tensor
/// operations to avoid additional linear scans over the function.
///
/// Returns true on error.
///
bool TFDeabstraction::simplifyTensorOperands() {
  llvm::PrettyStackTraceFormat X("TFDeabstraction::simplifyTensorOperands");
  bool containsGraphOp = false;
  bool hasError = false;

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
      if (auto *graphOpInst = dyn_cast<GraphOperationInst>(inst)) {
        logIfFirstChange();

        // Simplify operands if possible.
        auto newInst = simplifyOperands(graphOpInst, *this);

        if (!newInst) {
          hasError = true;
          continue;
        }

        // Remember this for later passes.
        tensorOps.push_back(newInst);
        containsGraphOp = true;
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
          containsGraphOp = true;
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
  // no actual tensor graph_ops, we'll ignore the function because there is
  // nothing to partition out of it.  This is probably something actually
  // working on the host-side tensor operation.
  if (!containsGraphOp)
    tensorOps.clear();

  return hasError;
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
        tfc.containsTensorFlowValue(arg->getType(),
                                    /*checkHigherOrderFunctions*/ true))
      addressRoots.push_back({arg, /*startsUninitialized*/ false});
  }


  // If we're in the main function processing top level code, scan the function
  // to collect any global_addr instructions which provide address roots.  We
  // want to promote tensor-related globals and the values that feed into them.
  if (TFPromoteGlobalVariables && fn.getName() == SWIFT_ENTRY_POINT_FUNCTION)
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
        if (tfc.containsTensorFlowValue(gv->getLoweredType(),
                                        /*checkHigherOrderFunctions*/ false)) {
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
      if (!ga || !tfc.containsTensorFlowValue(
                     ga->getType(), /*checkHigherOrderFunctions*/ false))
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
    auto stackAlloc = B.createAllocStack(root.getLoc(),
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
    auto loc = root.getLoc();
    auto stackAlloc = stackAllocForRoot[root];
    assert(stackAlloc && "where'd our alloc_stack go?");

    // In some cases like global variables in top level code, the root will
    // start out uninitialized.  In other cases, it is already initialized - as
    // in indirect arguments to functions or REPL code that reuses a global.
    // If it is initialized, emit code to do so.
    if (!rootInfo.second) {
      auto insertionPoint = rootInfo.first->getDefiningInstruction();

      // Insert the initialization after the root or stack alloc.
      if (!insertionPoint) insertionPoint = stackAlloc;
      SILBuilder B(++SILBasicBlock::iterator(insertionPoint));

      auto &TL = B.getTypeLowering(stackAlloc->getType());
      TL.emitCopyInto(B, loc, root, stackAlloc, IsTake_t::IsNotTake,
                      IsInitialization_t::IsInitialization);
    }

    // Process each exit block, inserting epilog code.
    for (auto *exit : exitBlocks) {
      SILBuilder B(exit->getTerminator());

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
void TFDeabstraction::promoteToSSA(ArrayRef<AllocStackInst *> allocs) {
  // If there is nothing to promote, don't bother calculating dominator info.
  if (allocs.empty())
    return;

  llvm::PrettyStackTraceFormat X("PromotableMemoryFinder::promoteToSSA");

  // Our first scan will look for begin/end access instructions and remove them,
  // allowing later passes to be simpler.
  // This is done repeatedly, since a begin_access itself can have another
  // begin_access user.
  for (auto *alloc : allocs) {
    while (true) {
      bool changed = false;
      for (auto UI = alloc->use_begin(); UI != alloc->use_end();) {
        auto *begin = dyn_cast<BeginAccessInst>((*UI++)->getUser());
        if (!begin)
          continue;

        // If we have a begin_access instruction, replace uses of begin_access
        // with uses of the original value and remove the end_access.
        for (auto UI = begin->use_begin(); UI != begin->use_end();) {
          auto *use = *UI++;
          auto inst = use->getUser();
          if (isa<EndAccessInst>(inst))
            inst->eraseFromParent();
          else
            use->set(alloc);
        }
        begin->eraseFromParent();
        changed = true;
      }
      if (!changed)
        break;
    }
  }

  // Now we explode the alloc / dealloc / load / store operations of aggregate
  // values into per-field operations. For those tfop attr types that we will
  // const-evaluate later (only "TensorShape" for now), stop exploding them, so
  // that we can properly propagate SSA values for them in the subsequent
  // propagateSSAValues() call.
  (void)runSROAOnInsts(allocs, [](AllocStackInst *alloc) {
    auto ty = alloc->getType().getASTType();
    auto *structTy = ty->getAs<StructType>();
    return !structTy || structTy->getDecl()->getNameStr() != "TensorShape";
  });

  // Since the SROA pass above may have mutate alloc insts, we again scan over
  // all of the operands of the tensor ops (including tf op attributes), finding
  // stack allocations that we want to promote to SSA.
  SmallVector<AllocStackInst *, 16> newStackAllocs;
  if (PromotableMemoryFinder(newStackAllocs, tfc, fn).run(tensorOps)) {
    logCurrentState("After running SROA",
                    /*detailed*/ true);
  }

  for (auto alloc : newStackAllocs)
    prepareStackAllocForPromotion(alloc);

  // Otherwise the function does have tensor operations, so lets promote any
  // stack allocations out of the way so we can do simple dataflow analysis.
  auto domInfo = passManager->getAnalysis<DominanceAnalysis>()->get(&fn);
  promoteAllocsToSSA(newStackAllocs, domInfo);
}

/// Preprocess the specified allocation instruction to make it more suitable for
/// promotion to SSA.  In particularly, we eliminate CopyAddrInst and other
/// uses that could prevent us from promoting this.
void TFDeabstraction::prepareStackAllocForPromotion(AllocStackInst *alloc) {
  // Our second pass looks for aggregate operations and struct_element_addrs
  // that poke inside the allocation.
  for (auto UI = alloc->use_begin(); UI != alloc->use_end();) {
    auto *inst = (*UI)->getUser();

    if (auto *sea = dyn_cast<StructElementAddrInst>(inst)) {
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

        // If we have a store(x ->struct_element_addr(alloc)), turn it into a
        // load of the whole value, a bunch of extracts, then a struct_inst
        // to rebuild the whole value, then a store of the whole thing.
        //
        // TODO: For now, we only handle a single element struct, which is
        // considerably simpler.
        //
        if (auto *store = dyn_cast<StoreInst>(use->getUser())) {
          if (use->getOperandNumber() == 1 &&  // store TO the alloca.
              tf::getFieldIfContainsSingleField(sea->getStructDecl())) {
            SILBuilder B(store);
            auto *newStruct = B.createStruct(store->getLoc(),
                                             alloc->getType().getObjectType(),
                                             store->getOperand(0));
            B.createStore(store->getLoc(), newStruct, sea->getOperand(),
                          store->getOwnershipQualifier());
            store->eraseFromParent();
            ++UI;
            sea->eraseFromParent();
            continue;
          }
        }
      }
    }

    // Explode aggregate by-address instructions like copy-addr.
    if (explodeAggregateInst(inst, /*all types*/nullptr)) {
      ++UI;
      inst->eraseFromParent();
      continue;
    }

    // Otherwise we have something else, leave it alone.
    ++UI;
  }
}

/// The specified argument has tuple type that deabstraction needs to scalarize.
/// Explode it into its deabstracted elements, rebuilding it and the branch
/// instructions that feed it.  This returns a value of the original type that
/// can be used for further analysis.
static SILValue explodeSILTupleArgument(SILPhiArgument *arg) {
  SmallVector<SILValue, 4> newArgs;

  auto *argBB = arg->getParent();

  // Collect all the fields and add new BB arguments to the block for each of
  // them.
  auto tuple = arg->getType();
  unsigned numElements = tuple.castTo<TupleType>()->getNumElements();
  for (unsigned i = 0; i != numElements; ++i) {
    auto newArg = argBB->createPhiArgument(tuple.getTupleElementType(i),
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
static SILValue explodeSILStructArgument(SILPhiArgument *arg) {
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

    auto newArg = argBB->createPhiArgument(fieldTy, arg->getOwnershipKind());
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

/// If the specified type is a Swift.Array or some element type, then return the
/// element type.  Otherwise, return a null Type.
static Type getArrayElementType(Type ty) {
  if (auto bgst = ty->getAs<BoundGenericStructType>())
    if (bgst->getDecl() == bgst->getASTContext().getArrayDecl())
      return bgst->getGenericArgs()[0];
  return Type();
}

/// If the specified value is a single-element struct_inst wrapper, look through
/// them.  We special case arrays, and return Array<T> values as themselves.
static SILValue getValueInsideStructInst(SILValue value) {
  // Dig through one-argument struct insts.
  while (auto structVal = dyn_cast<StructInst>(value)) {
    // If this is an ArrayType, don't dig in.
    if (getArrayElementType(structVal->getType().getASTType()))
      break;

    if (structVal->getNumOperands() != 1)
      break;
    value = structVal->getOperand(0);
  }
  return value;
}

/// Return true if this is a reference to the _allocateUninitialized helper
/// in array in the standard library allocating zero elements.
bool isArrayAllocUninit(SILValue op, SILValue &numElements) {
  auto *apply = dyn_cast<ApplyInst>(op->getDefiningInstruction());
  if (!apply)
    return false;
  auto *callee = dyn_cast<FunctionRefInst>(apply->getOperand(0));
  if (!callee)
    return false;

  auto calleeName = callee->getReferencedFunction()->getName();
  // FIXME: Gross hack because this is specialized by perf optimizer.  Remove
  // when deabstraction does arrays.
  if (!calleeName.contains("_allocateUninitializedArray"))
    return false;

  numElements = getValueInsideStructInst(apply->getOperand(1));
  return true;
}

namespace {
/// This is a little helper for working with literal arrays that may want to get
/// deleted if all references to them are removed.
struct ArrayElementDecoder {
  SmallVector<Operand *, 4> elementsAtInit;
  SmallPtrSet<SILInstruction *, 8> arrayInsts;

  /// Given a SILValue that may be an array, attempt to decode it into the
  /// literal values that make up its elements.  This returns the element type
  /// of the array if it succeeds, otherwise a null type.
  Type decode(SILValue value) {
    auto elementType = getArrayElementType(value->getType().getASTType());
    if (!elementType)
      return Type();

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

    return !ConstExprEvaluator::decodeAllocUninitializedArray(
        apply, numElements, elementsAtInit, &arrayInsts);
  }

  /// Try to remove the instructions that make up the array initialization.
  void removeInstructionsIfPossible() {
    if (arrayInsts.empty())
      return;

    // If we can remove it, drop all inter-dependent references.
    for (auto inst : arrayInsts)
      inst->dropAllReferences();
    // Then erase the instructions themselves.
    for (auto inst : arrayInsts)
      inst->eraseFromParent();
  }
};
} // end anonymous namespace

/// We've promoted any stack allocations that are in the way of tensor operands
/// so we now have proper SSA.  Look through struct and tuple injection and
/// projection instructions to find the underlying value that can feed the
/// tensor operation or attribute.  This is typically another tensor operation
/// or a constant (for attributes) but may be variables or other things that
/// cause a send.
///
static SILValue
propagateSSAOperand(SILValue v, SmallPtrSet<SILPhiArgument *, 8> &checkedPhis) {
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

    if (auto *arg = dyn_cast<SILPhiArgument>(v)) {
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
          incomingVal = propagateSSAOperand(incomingVal, checkedPhis);
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

      auto *apply = dyn_cast_or_null<ApplyInst>(v->getDefiningInstruction());
      if (!apply)
        continue;

      // Handle the case of deabstracting an array, such as the tfop attr %181
      // below. In this example, we need to propagate %188 to %190, which
      // eventually feeds %181. Note %189 is not a const struct. The goal of
      // SSA value propagation here is to have const expr eval only process
      // the (const) struct field %188, and not the struct %189.
      //
      // function_ref _allocateUninitializedArray<A>(_:)
      // %179 = function_ref @$Ss27_allocateUninitializedArrayySayxG_BptBwlF ...
      // %180 = apply %179<TensorShape>(%178) ...
      // %181 = tuple_extract %180 : $(Array<TensorShape>, Builtin.RawPointer),0
      // %183 = tuple_extract %180 : $(Array<TensorShape>, Builtin.RawPointer),1
      // %185 = pointer_to_address %183
      // %188 = struct $TensorShape (%187 : $Array<Int32>)
      // %189 = struct $SimpleIter(...: $ResourceHandle, %188 :$TensorShape)
      // %190 = struct_extract %189 : $SimpleIter, #SimpleIter.elementShape
      // store %190 to %185 : $*TensorShape
      // %193 = graph_op "Foo"(..., shapes %181 : $Array<TensorShape>
      ArrayElementDecoder arrayDecoder;
      if (!arrayDecoder.decode(extract))
        continue;

      for (auto *use : arrayDecoder.elementsAtInit) {
        auto *store = dyn_cast<StoreInst>(use->getUser());
        if (!store) {
          // TODO: May need to handle other inst types too, such as CopyAddr.
          continue;
        }

        auto newSrc = propagateSSAOperand(store->getOperand(0), checkedPhis);
        store->setOperand(0, newSrc);
      }

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
/// operands (including attributes) and results are directly linked together in
/// the SSA graph at the TensorFlow value level, without going through
/// intervening struct/tuple wrappers.
/// This is essential in deabstracting constant tfop attribute values, and also
/// helps reduce sends/recvs involving tensor operands.
void TFDeabstraction::propagateSSAValues() {
  llvm::PrettyStackTraceFormat X("TFDeabstraction::propagateSSAValues");

  SmallPtrSet<SILPhiArgument*, 8> checkedPhis;
  for (auto *op : tensorOps) {
    for (auto &operand : op->getAllOperands()) {
      // Get the propagated value.
      auto newVal = propagateSSAOperand(operand.get(), checkedPhis);

      if (newVal == operand.get())
        continue;

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

/// This function transform the apply of the "__tf_tensor_from_scalar" function
/// to a graph node (graph_op inst).
///
/// Similar to tryToPromoteTensorFromScalars(), the resulting graph node can be
/// a "const", if its input scalar operand is a compile-time constant.
/// Different from tryToPromoteTensorFromScalars(), we can also transform the
/// apply into a "tfc.scalarToTensor" graph_op, which the partition pass then
/// uses to do scalar promotion. See the comment above "func
/// _TFTensorFromScalar<Scalar>" in Tensor.swift for more details.
static GraphOperationInst *
transformTensorFromScalar(ApplyInst *apply,
                          const DenseMap<SILValue, SymbolicValue> &constants,
                          GraphFunctionDeviceInfo &deviceInfo) {
  assert(apply->getNumResults() == 1 &&
         "tfc.scalarToTensor must have 1 result");
  auto origResult = apply->getResults()[0];

  assert(apply->getNumOperands() == 2);
  auto scalarOperandAddr = apply->getOperand(1);
  LLVM_DEBUG(llvm::dbgs() << "Converting __tf_tensor_from_scalar inst "
                          << *apply
                          << " with scalar operand : " << scalarOperandAddr
                          << " of type " << scalarOperandAddr->getType()
                          << " to a graph_op\n");
  SILBuilder B(apply);
  auto &ctx = B.getASTContext();

  // First check to see if it's constant foldable.  If so, we can turn this into
  // a Const node.
  auto it = constants.find(scalarOperandAddr);
  if (it != constants.end() && it->second.isConstant()) {
    // Dig the element type out of the TensorHandle result type.
    auto eltType =
        getTensorHandleElementType(origResult->getType().getASTType());
    // We use int32 as the element type of the zero-d shape array.
    auto int32Ty =
        ctx.getInt32Decl()->getDeclaredType()->getCanonicalType();
    assert(it->second.getKind() == SymbolicValue::Address);

    SmallVector<unsigned, 4> accessPath;
    auto *scalarMemoryObject = it->second.getAddressValue(accessPath);
    assert(accessPath.empty());
    auto scalarValue = scalarMemoryObject->getValue();
    assert(scalarValue.isConstant());
    scalarValue = scalarValue.lookThroughSingleElementAggregates();
    LLVM_DEBUG(llvm::dbgs()
               << "  The scalar const value is: " << scalarValue << "\n");

    auto constant = createConstTensor(
        eltType, scalarValue,
        SymbolicValue::getArray({}, int32Ty, ctx.getAllocator()),
        origResult->getType(), getUserSourceLocation(apply), DeviceType::ALL,
        B);
    LLVM_DEBUG(llvm::dbgs()
               << "  The resulting const node is: " << *constant << "\n");
    origResult->replaceAllUsesWith(constant->getResult(0));
    apply->eraseFromParent();
    return constant;
  }

  // convert to tfc.scalarToTensor.
  auto loc = getUserSourceLocation(apply);
  // Create a load inst to convert a scalar type like Float* to Float.
  auto hasOwnership = apply->getFunction()->hasQualifiedOwnership();
  auto loadOwnership = hasOwnership ? LoadOwnershipQualifier::Trivial
                                    : LoadOwnershipQualifier::Unqualified;
  auto *loadScalar = B.createLoad(loc, scalarOperandAddr, loadOwnership);
  auto scalarTy = loadScalar->getType();
  assert(convertSwiftTypeToTF(scalarTy.getASTType()) > 0 &&
         "Input value must be a scalar.");

  // The marking and scalar promotion code in the TF partition pass does not
  // work when we feed a stdlib type like $Float to tfc.scalarToTensor. So we
  // get the underlying built-in type like $Builtin.FPIEEE32.
  // TODO: Extend the partition code so that we can feed stdlib types directly.
  SingleValueInstruction *loadScalarBuiltin = nullptr;
  {
    auto *decl = scalarTy.getStructOrBoundGenericStruct();
    assert(decl);
    for (auto *field : decl->getStoredProperties()) {
      // Check the struct only has 1 field.
      assert(!loadScalarBuiltin);
      loadScalarBuiltin = B.createStructExtract(loc, loadScalar, field);
    }
    assert(loadScalarBuiltin);
  }
  GraphOperationBuilder opBuilder("tfc.scalarToTensor");
  opBuilder.addArgument(loadScalarBuiltin);
  // Place on the primary device.
  deviceInfo.handleDevicePlacement("tfc.scalarToTensor", /*opDevice*/ "", ctx,
                                   &opBuilder);

  auto op = opBuilder.build(B, ctx, loc,
                            /*resultSILTypes*/ {origResult->getType()});
  LLVM_DEBUG(llvm::dbgs() << "  The resulting tfc.scalarToTensor op is: " << *op
                          << "\n");
  auto newResult = getSingleValueResult(op);
  origResult->replaceAllUsesWith(newResult);
  apply->eraseFromParent();
  return op;
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
    if (auto *graphOpInst = dyn_cast<GraphOperationInst>(inst)) {
      GraphOperationInfo opInfo(graphOpInst);
      // Do not translate this special inst into a graph op, since it will get
      // removed at the beginning of the partition pass.
      // TODO: remove this inst in the getForFunction() call above, once
      // the partition pass is folded into deabstraction.
      // FIXME: consider defining constexpr strings for these literals.
      if (opInfo.getOperationName() == "tfc.configureTPU" ||
          opInfo.getOperationName() == "tfc.configureGPU" ||
          opInfo.getOperationName() == "tfc.configureCPU")
        continue;
      evaluateAttributesAndDoPacking(opInfo, constants, deviceInfo);
      // evaluateAttributesAndDoPacking deletes inst. So, continue as the rest
      // of the loop is irrelevant. (This also avoid memory errors.)
      continue;
    }

    // Do not perform the following graph_op optimization/promotion unless
    // we are in graph mode.
    if (llvm::TFDynamicCompilation && !isAcceleratorOnly(fn)) continue;

    // Take a look at the various well known function calls that we can promote
    // to tensor operations.  We can promote them if we are able to constant
    // fold all of the operands to these calls.  If so, we rewrite them in terms
    // of a proper op, and partitioning will continue to treat them that way.
    if (auto apply = dyn_cast<ApplyInst>(inst)) {
      auto callee = apply->getCalleeFunction();

      LLVM_DEBUG(llvm::dbgs() << "processing apply inst with func name "
                              << callee->getName() << "\n");

      if (callee && callee->getName() == "__tf_tensor_from_scalar") {
          inst = transformTensorFromScalar(apply, constants, deviceInfo);
          assert(inst);
        continue;
      }
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

/// A reference to the specified array was just dropped.  If it was a literal
/// array and this was the last use, clean up the instructions that fed it.
static void removeArrayValueIfPossible(ApplyInst *array) {
  // If this array is a literal that we can decode, remove the instructions that
  // it came from.
  ArrayElementDecoder decoder;
  if (decoder.decodeApply(array))
    decoder.removeInstructionsIfPossible();
}

/// Deabstraction can leave around lots of dead instructions from the attributes
/// that get promoted, including heavy weight types that inline into a lot of
/// code, like arrays.  Do a quick pass to remove these, improving compile time
/// of subsequent passes.
void TFDeabstraction::cleanupDeadInstructions() {
  llvm::PrettyStackTraceFormat
  X("TFDeabstraction::cleanupDeadInstructions");

  SmallVector<SILInstruction *, 16> deadInsts;
  SmallVector<ApplyInst *, 8> arrayAllocUninitInsts;

  auto markInstructionForRemoval = [&](SILInstruction *inst) {
    // Mark trivially dead instructions.
    if (isa<SingleValueInstruction>(inst) &&
        isInstructionTriviallyDead(inst)) {
      deadInsts.push_back(inst);
      return;
    }

    // Mark instructions applying the _allocateUninitialized function.
    if (auto *apply = dyn_cast<ApplyInst>(inst)) {
      SILValue tmp;
      if (isArrayAllocUninit(apply, tmp)) {
        arrayAllocUninitInsts.push_back(apply);
        return;
      }
    }

    // TODO: Mark dead stack slots for tensors being passed as input lists.

    // TODO: Mark TensorShape.init and other methods that we shouldn't bake in
    // special knowledge of. Needs integration with ConstExpr.

    // TODO: Handle String.init and other stuff. Refactor this when ConstExpr
    // handling subsumes ConstantPropagation.
  };

  // Mark instructions for removal.
  for (auto &bb : fn)
    for (auto &inst : bb)
      markInstructionForRemoval(&inst);

  // Remove dead instructions. Debug instructions are not deleted.
  for (auto *inst : deadInsts)
    recursivelyDeleteTriviallyDeadInstructions(inst);

  // Clean up instructions related to array literal initialization, if possible.
  for (auto *inst : arrayAllocUninitInsts)
    removeArrayValueIfPossible(inst);
}

/// Recursively unwraps a sequence of aggregate construction/extraction
/// instructions to find the earliest definition of `value`.  May return
/// `value` if there is nothing to unwrap.
static SILValue
unwrapAggregateInstructions(SILValue value,
                            llvm::DenseMap<SILValue, SILValue> &cache) {
  auto &unwrapped = cache[value];
  if (unwrapped)
    return unwrapped;

  if (auto *tei = dyn_cast<TupleExtractInst>(value)) {
    auto tupleValue = unwrapAggregateInstructions(tei->getOperand(), cache);
    if (auto *ti = dyn_cast<TupleInst>(tupleValue)) {
      unwrapped =
          unwrapAggregateInstructions(ti->getOperand(tei->getFieldNo()),
                                      cache);
      return unwrapped;
    }
  }

  if (auto *sei = dyn_cast<StructExtractInst>(value)) {
    auto structValue = unwrapAggregateInstructions(sei->getOperand(), cache);
    if (auto *si = dyn_cast<StructInst>(structValue)) {
      unwrapped =
          unwrapAggregateInstructions(si->getFieldValue(sei->getField()),
                                      cache);
      return unwrapped;
    }
  }

  unwrapped = value;
  return unwrapped;
}

/// Recursively unpacks aggregates of TensorFlow values to `inputList`, using
/// the already-lowered values when possible to avoid code bloat.  Returns true
/// to represent error if it detects a non-TensorFlow leaf field.
///
/// If `acceptTensorGroupConformingLeaves` is true, then we accept types that
/// conform to TensorGroup as allowed leaf fields that get unpacked to
/// `inputList`. Otherwise, we only accept types as leaf fields when they are
/// one of our well-known TensorFlow types.
static bool unpackTensorAggregates(
    const ASTContext &ctx, SILLocation loc, SILBuilder &B, SILValue rootAggregate,
    bool acceptTensorGroupConformingLeaves,
    llvm::DenseMap<std::pair<SILValue, unsigned>, SILValue> &loweredTupleElts,
    llvm::DenseMap<std::pair<SILValue, VarDecl*>, SILValue>
        &loweredStructFields,
    llvm::DenseMap<SILValue, SILValue> &unwrapCache,
    SmallVectorImpl<SILValue> &inputList) {

  // Extracts a field from a tuple.  If the tuple comes from a `tuple`
  // instruction, uses the appropriate operand instead of emitting a new
  // `tuple_extract`.
  auto extractElement = [&](SILValue tupleValue, unsigned i) -> SILValue {
    if (auto *ti = dyn_cast<TupleInst>(tupleValue))
      return ti->getOperand(i);
    return B.createTupleExtract(loc, tupleValue, i);
  };

  // Extracts a field from a struct.  If the struct comes from a `struct`
  // instruction, uses the appropriate operand instead of emitting a new
  // `struct_extract`.
  auto extractField = [&](SILValue structValue, VarDecl *field) -> SILValue {
    if (auto *si = dyn_cast<StructInst>(structValue))
      return si->getFieldValue(field);
    return B.createStructExtract(loc, structValue, field);
  };

  auto tfModule = ctx.getLoadedModule(ctx.Id_TensorFlow);
  assert(tfModule && "could not find TensorFlow module");
  auto inputTensorGroupProto =
      ctx.getProtocol(KnownProtocolKind::TensorArrayProtocol);
  assert(inputTensorGroupProto &&
         "could not find TensorArrayProtocol protocol");

  std::function<bool(SILValue)> recurse;
  recurse = [&](SILValue aggregate) -> bool {
    auto aggregateTy = aggregate->getType();
    if (isTensorFlowValue(aggregateTy)) {
      inputList.push_back(aggregate);
      return false;
    }
    if (acceptTensorGroupConformingLeaves &&
        tfModule->lookupConformance(aggregateTy.getASTType(),
                                    inputTensorGroupProto)) {
      inputList.push_back(aggregate);
      return false;
    }
    if (auto tupleTy = aggregateTy.getAs<TupleType>()) {
      for (auto i : range(tupleTy->getNumElements())) {
        auto eltIdx = std::pair<SILValue, unsigned>(aggregate, i);
        auto &eltValue = loweredTupleElts[eltIdx];
        if (!eltValue) {
          eltValue =
              unwrapAggregateInstructions(extractElement(aggregate, i),
                                          unwrapCache);
        }
        if (recurse(eltValue))
          return true;
      }
      return false;
    }
    if (auto *decl = aggregateTy.getStructOrBoundGenericStruct()) {
      for (auto *field : decl->getStoredProperties()) {
        auto fieldIdx = std::pair<SILValue, VarDecl*>(aggregate, field);
        auto &fieldValue = loweredStructFields[fieldIdx];
        if (!fieldValue) {
          fieldValue =
              unwrapAggregateInstructions(extractField(aggregate, field),
                                          unwrapCache);
        }
        if (recurse(fieldValue))
          return true;
      }
      return false;
    }
    return true;
  };

  return recurse(unwrapAggregateInstructions(rootAggregate, unwrapCache));
}

/// Given a type that is an aggregate of TF types, recursively collect all
/// innermost TF types as SymbolicValue integers representing TF_DataTypes.
static bool collectInnermostTensorFlowDTypes(
    CanType type, SmallVectorImpl<SymbolicValue> &result) {

  if (isTensorFlowDType(type)) {
    result.push_back(convertSwiftTypeToConstantTFDataType(type));
    return true;
  }
  if (tf::isTensorHandle(type)) {
    auto eltType = getTensorHandleElementType(type)->getCanonicalType();
    assert(tf::isTensorFlowDType(eltType));
    result.push_back(convertSwiftTypeToConstantTFDataType(eltType));
    return true;
  }
  if (auto tupleTy = type->getAs<TupleType>())
    return llvm::all_of(tupleTy->getElementTypes(),
                        [&](Type eltTy) {
                          return collectInnermostTensorFlowDTypes(
                              eltTy->getCanonicalType(), result);
                        });
  if (auto *decl = type->getStructOrBoundGenericStruct())
    return llvm::all_of(decl->getStoredProperties(),
                        [&](VarDecl *member) {
                          auto subMap = type->getMemberSubstitutionMap(
                              member->getModuleContext(), member);
                          auto eltTy = member->getType().subst(subMap);
                          return collectInnermostTensorFlowDTypes(
                              eltTy->getCanonicalType(), result);
                        });
  return false;
}

/// Replace the specified GraphOperationInst with a new GraphOperationInst
/// with the following transformations applied:
///  - attribute arguments with known constant values become GraphOperationInst
///    attributes.
///  - inputs get unpacked to raw tensor types before being fed into the
///    GraphOperationInst.
///  - outputs are raw tensor types, which get packed into whatever types the
///    original GraphOperationInst was returning.
///
/// This deletes the underlying inst in `opInfo` when a GraphOperation is
/// created successfully.
void TFDeabstraction::evaluateAttributesAndDoPacking(
    GraphOperationInfo &opInfo, DenseMap<SILValue, SymbolicValue> &constants,
    GraphFunctionDeviceInfo &deviceInfo) {
  auto *origInst = opInfo.getInst();
  auto &context = origInst->getFunction()->getASTContext();
  auto &allocator = context.getAllocator();
  SILBuilder B(origInst);
  auto origInstLoc = getUserSourceLocation(origInst);

  // This is a helper function to emit a diagnostic.
  auto diagnoseInvalid = [&](SILLocation loc, const Twine &message,
                             SILLocation userNoteLoc = (Expr *)nullptr) {
    diagnose(fn.getModule().getASTContext(), loc.getSourceLoc(),
             diag::tf_op_misuse, message.str())
      .highlight(loc.getSourceRange());
    if (userNoteLoc)
      diagnose(context, userNoteLoc.getSourceLoc(), diag::tf_value_used_here);
  };

  GraphOperationBuilder opBuilder(opInfo.getOperationName());

  // Find the device attribute specified for the instruction if present.
  StringRef opDevice;

  // Pass attributes through.
  for (auto &attr : origInst->getAttributes()) {
    if (attr.name.str() == TF_DEVICE_ATTR) {
      opDevice = attr.value.getStringValue();
    }
    opBuilder.addAttribute(attr);
  }

  // It is common to have input lists with repeated elements.  These will
  // generally be uniqued on entry to this routine.  We cache the projections in
  // these maps so that we can reuse them and avoid code bloat.
  llvm::DenseMap<std::pair<SILValue, unsigned>, SILValue> loweredTupleElts;
  llvm::DenseMap<std::pair<SILValue, VarDecl*>, SILValue> loweredStructFields;

  // A cache for calls to `unwrapAggregateInstructions`.
  llvm::DenseMap<SILValue, SILValue> unwrapCache;

  for (auto i : range(opInfo.getStructuredArguments().size())) {
    auto argument = opInfo.getStructuredArguments()[i];

    if (argument.getKind() == GraphOperationInfo::SAK_List) {
      // We can get SAK_List arguments from inlining functions that have already
      // been deabstracted. Pass these arguments through.
      opBuilder.addListArgument(argument.getArgumentList(),
                                argument.getArgumentNameWithSuffix());
      continue;
    }

    assert(argument.getKind() == GraphOperationInfo::SAK_Single &&
           "should have already handled all other argument kinds");
    auto argumentValue = argument.getSingleArgument();
    auto argumentTy = argumentValue->getType();
    auto argumentNameAndLowering = argument.getArgumentNameAndLowering();
    auto argumentLoc = getUserSourceLocation(argumentValue);

    // Collect and validate input arguments.
    if (std::get<1>(argumentNameAndLowering) ==
        GraphOperationInfo::ArgumentLowering::Input) {
      // Remove the argument so decodeArrayElements doesn't see the use.
      origInst->setOperand(i, SILUndef::get(argumentTy, fn.getModule()));

      // Handle the case where this is an array, representing an input list.
      ArrayElementDecoder arrayDecoder;
      auto elementType = arrayDecoder.decode(argumentValue);
      SmallVector<SILValue, 4> inputList;
      if (elementType) {
        // Add each element to the input list we're building, drilling down
        // through any aggregates that may be around it.
        for (auto *use : arrayDecoder.elementsAtInit) {
          SILValue element;
          if (auto *store = dyn_cast<StoreInst>(use->getUser()))
            element = store->getSrc();
          else if (auto *copyAddr = dyn_cast<CopyAddrInst>(use->getUser()))
            element = copyAddr->getSrc();

          if (!element) {
            diagnoseInvalid(argumentLoc,
                            "argument of type '" + elementType->getString() +
                            "' is not a TensorFlow value or an aggregate of "
                            "TensorFlow values");
            return;
          }

          // In dynamic compilation mode, we can handle inputs that conform to
          // TensorGroup even if we cannot completely unpack them.
          bool acceptTensorGroupConformingLeaves =
              llvm::TFDynamicCompilation && !isAcceleratorOnly(fn);

          if (unpackTensorAggregates(context, origInst->getLoc(), B, element,
                                     acceptTensorGroupConformingLeaves,
                                     loweredTupleElts, loweredStructFields,
                                     unwrapCache, inputList)) {
            diagnoseInvalid(argumentLoc,
                            "argument of type '" + elementType->getString() +
                            "' is not a TensorFlow value or an aggregate of "
                            "TensorFlow values");
            return;
          }
        }
        opBuilder.addListArgument(inputList);
        continue;
      }

      // Otherwise, this must be a single input.
      if (unpackTensorAggregates(context, origInst->getLoc(), B, argumentValue,
                                 /*acceptTensorGroupConformingLeaves=*/false,
                                 loweredTupleElts, loweredStructFields,
                                 unwrapCache, inputList)) {
        // FIXME: Since TFDeabstraction happens after mandatory inlining,
        // numeric arguments will appear as having a builtin type such as
        // `Builtin.FPIEEE32`. This is undesirable for user-facing diagnostics.
        auto astTypeString = argumentTy.getASTType().getString();
        diagnoseInvalid(argumentLoc,
                        "argument of type '" + astTypeString +
                        "' is not a TensorFlow value or an aggregate of "
                        "TensorFlow values");
        return;
      }
      if (inputList.size() != 1) {
        // We could accept this situation and unpack the input into an input
        // list.  However, we want users to explicitly indicate that they want
        // unpacking to an input list by wrapping their input in an array, so we
        // reject this situation.
        auto astTypeString = argumentTy.getASTType().getString();
        // FIXME: We could use `argumentLoc`, but the error would show up on the
        // type declaration instead. Need to investigate how to get the argument
        // location.
        diagnoseInvalid(argumentLoc, "argument of type '" + astTypeString +
                        "' must contain exactly one TensorFlow value",
                        origInstLoc);
        return;
      }
      opBuilder.addArgument(inputList[0]);
      continue;
    }

    // Helper to diagnose invalid attributes.
    auto diagnoseInvalidAttr = [&](const Twine &message) {
      diagnoseInvalid(origInstLoc,
                      Twine("attribute '") +
                      std::get<0>(argumentNameAndLowering) +
                      "' " + message.str());
    };

    // Ok, we have an attribute argument, we should have been able to fold it
    // through our constexpr evaluation logic -- unless we're doing dynamic
    // compilation.
    auto it = constants.find(argumentValue);
    if (it == constants.end() || !it->second.isConstant()) {
      if (llvm::TFDynamicCompilation && !isAcceleratorOnly(fn)
          && !argument.mustBeLoweredToConstant()) {
        // Since we're doing dynamic compilation, we can simply pass this
        // unknown attribute as a named argument to the graph_op, and IRGen will
        // deal with it.
        opBuilder.addArgument(argumentValue, argument.getArgumentNameWithSuffix());
        continue;
      } else {
        // TODO: improve the diagnostic to talk about the parameter label in
        // the user code, not the internal op attribute.  The bookkeeping for
        // this isn't obvious though.
        diagnoseInvalidAttr("requires a constant argument");

        // If we have more specific information about what went wrong, emit
        // notes.
        if (it != constants.end() &&
            it->second.getKind() == SymbolicValue::Unknown)
          it->second.emitUnknownDiagnosticNotes(origInstLoc);
        return;
      }
    }

    // Get the constant, ignoring struct wrappers.
    auto constValue = it->second.lookThroughSingleElementAggregates();

    // Clone it out of the ConstExpr pool into the global SILModule pool.
    constValue = constValue.cloneInto(allocator);

    auto attrIdentifier = context.getIdentifier(argument.getArgumentNameWithSuffix());
    opBuilder.addAttribute({ attrIdentifier, constValue });

    // FIXME: Do we detect and reject duplicate attribute names already?

    // If it's a device attribute, get the device value.
    if (std::get<0>(argumentNameAndLowering) == TF_DEVICE_ATTR) {
      if (constValue.getKind() != SymbolicValue::String)
        return diagnoseInvalidAttr("must be a string");
      opDevice = constValue.getStringValue();
      // User code should not specify this pseudo device.
      if (opDevice == TF_ALL_DEVICES)
        return diagnoseInvalidAttr("may not use this device name");
    }

    // Emits a diagnostic and returns true if the value is invalid for a shape
    // attr.
    auto verifyNormalAttr = [&](SymbolicValue constValue) -> bool {
      switch (constValue.getKind()) {
      case SymbolicValue::Unknown:
      case SymbolicValue::UninitMemory:
        llvm_unreachable(
            "earlier code should have ruled out non-constant values");

      case SymbolicValue::Address:
        llvm_unreachable("it's impossible to pass an address as an attr");

      case SymbolicValue::Enum:
      case SymbolicValue::EnumWithPayload:
      case SymbolicValue::Aggregate:
        diagnoseInvalidAttr("cannot be an enum, struct, or tuple");
        return true;

      case SymbolicValue::Metatype:
        diagnoseInvalidAttr("cannot be a metatype");
        return true;

      case SymbolicValue::Integer:
      case SymbolicValue::Float:
      case SymbolicValue::String:
      case SymbolicValue::Function:
        break;

      case SymbolicValue::Array:
        // Best effort check for common problems.
        CanType eltTy;
        for (auto elt : constValue.getArrayValue(eltTy)) {
          elt = elt.lookThroughSingleElementAggregates();
          if (elt.getKind() == SymbolicValue::Metatype) {
            diagnoseInvalidAttr("cannot be an array of metatypes");
            return true;
          }
        }
        break;
      }
      return false;
    };

    // Emits a diagnostic and returns a negative number if the value is not an
    // int array.  Otherwise returns the product of the array element
    // values. e.g. For array [2, 3], return 6.
    auto getIntArrayProduct = [&](SymbolicValue constValue) -> long long {
      // strip away the possible aggregate wrapper.
      constValue = constValue.lookThroughSingleElementAggregates();
      if (constValue.getKind() != SymbolicValue::Array) {
        diagnoseInvalidAttr("requires an array");
        return -1;
      }
      CanType eltType;
      auto elements = constValue.getArrayValue(eltType);
      if (!StringRef(eltType->getString()).startswith("Int")) {
        diagnoseInvalidAttr("requires an array of ints");
        return -1;
      }
      long long ret = 1;
      for (auto elt : elements) {
        // strip away the possible aggregate wrapper.
        elt = elt.lookThroughSingleElementAggregates();
        if (elt.getKind() != SymbolicValue::Integer) {
          diagnoseInvalidAttr("requires an array of ints");
          return -1;
        }
        // It is possible for this to overflow, but the resulting compilation
        // will still be correct, so for simplicity we do not guard against
        // overflow. More specifically, when it overflows, we would either
        // return a negative value and thus reject the input program, or
        // return an incorrect positive value. In both cases, the input
        // program will still be eventually rejected (e.g. in TF graph
        // compiler), since the specified shape is too large. As such, the
        // correctness of compilation is preserved.
        ret *= elt.getIntegerValue().getLimitedValue();
      }
      return ret;
    };
    // Verify that the type of this attribute is ok for the ArgumentLowering we
    // have.
    switch (std::get<1>(argumentNameAndLowering)) {
    case GraphOperationInfo::ArgumentLowering::Input:
      llvm_unreachable("Attributes cannot be inputs");
    case GraphOperationInfo::ArgumentLowering::Out:
      llvm_unreachable("Attributes cannot be output parameters");
    case GraphOperationInfo::ArgumentLowering::NormalAttribute:
      if (verifyNormalAttr(constValue))
        return; // error already emitted.
      break;
    case GraphOperationInfo::ArgumentLowering::ShapeAttribute:
      // A shape attr must be an int array.
      if (getIntArrayProduct(constValue) < 0)
        return; // error already emitted.
      break;
    case GraphOperationInfo::ArgumentLowering::TensorAttribute: {
      if (constValue.getKind() == SymbolicValue::Integer ||
          constValue.getKind() == SymbolicValue::Float   ||
          constValue.getKind() == SymbolicValue::String)
        break;

      if (constValue.getKind() != SymbolicValue::Array)
        return diagnoseInvalidAttr("requires a constant that is an integer,"
                                   " floating point, or array thereof");

      // Returns a negative number if the next attr is not a shape.  Otherwise
      // returns the number of elements as given by the shape attr. e.g. a
      // tensor with shape [2, 3] has 6 elements.
      auto getNumEltsFromShapeAttr = [&]() -> long long {
        if (i + 1 >= opInfo.getStructuredArguments().size())
          return -1;
        auto nextOperand = opInfo.getStructuredArguments()[i + 1];
        auto nextArgumentLowering = std::get<1>(
            nextOperand.getArgumentNameAndLowering());
        if (nextArgumentLowering !=
            GraphOperationInfo::ArgumentLowering::ShapeAttribute)
          return -1;
        assert(nextOperand.getKind() == GraphOperationInfo::SAK_Single &&
               "SILGen should not have generated a list argument");
        auto it = constants.find(nextOperand.getSingleArgument());
        if (it == constants.end() || !it->second.isConstant())
          return -1;
        auto constValue = it->second.lookThroughSingleElementAggregates();
        return getIntArrayProduct(constValue);
      };
      // Shape attr is an int array.
      auto numEltsFromShapeAttr = getNumEltsFromShapeAttr();
      if (numEltsFromShapeAttr < 0)
        return diagnoseInvalidAttr("must be followed by a shape attribute");

      // Validate that the shape matches the # elements we have.
      CanType eltType;
      auto elements = constValue.getArrayValue(eltType);

      if ((size_t)numEltsFromShapeAttr != elements.size())
        return diagnoseInvalidAttr("does not match the shape attribute in "
                                   "the number of scalar elements");

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
      } else if (firstElt.getKind() == SymbolicValue::String) {
        for (auto elt : elements) {
          elt = elt.lookThroughSingleElementAggregates();
          if (elt.getKind() != SymbolicValue::String)
            return diagnoseInvalidAttr("array values must be the same type");
        }
      }
      else {
        return diagnoseInvalidAttr("requires a constant that is an integer,"
                                   " floating point, string, or array thereof");
      }
      break;
    }
    case GraphOperationInfo::ArgumentLowering::TFDataTypeAttribute: {
      // Integers are ok.
      if (constValue.getKind() == SymbolicValue::Integer)
        break;

      // Arrays of integers are ok.
      if (constValue.getKind() == SymbolicValue::Array) {
        CanType eltTy;
        for (auto elt : constValue.getArrayValue(eltTy)) {
          elt = elt.lookThroughSingleElementAggregates();
          if (elt.getKind() != SymbolicValue::Integer)
            return diagnoseInvalidAttr("requires an integer or array of "
                                       "integers");
        }
        break;
      }

      // Everything else is bad.
      return diagnoseInvalidAttr("requires an integer or array of integers");
    }
    }
  }

  // Finally, set a device attribute for this if there wasn't already one
  // specified.
  if (opDevice.empty()) {
    deviceInfo.handleDevicePlacement(opInfo.getOperationName(), opDevice,
                                     context, &opBuilder);
  }
  // Okay, if we got this far then we have all valid attributes and inputs.

  // Time to create the op and deal with results.
  auto loc = getUserSourceLocation(origInst);

  // If there is only one result, it may be an aggregate that we have to pack.
  if (origInst->getNumResults() == 1) {
    auto origResult = origInst->getResults()[0];
    SmallVector<Type, 4> resultTypes;
    auto userResultTy = origResult->getType().getASTType();
    if (!tf::flattenTensorFlowValueAggregate(userResultTy, resultTypes)) {
      auto loc = getUserSourceLocation(origInst);
      diagnose(fn.getModule().getASTContext(), loc.getSourceLoc(),
               diag::tf_op_result_not_value_or_aggregate, userResultTy)
        .highlight(loc.getSourceRange());
      return;
    }
    auto resultSILTypes = map<SmallVector<SILType, 8>>(resultTypes, [&](Type ty) {
        return SILType::getPrimitiveObjectType(ty->getCanonicalType()); });

    auto op = opBuilder.build(B, context, loc, resultSILTypes);

    // Recursively pack results to a value with the user-specified aggregate type.
    auto resultIt = op->getResults().begin();
    std::function<SILValue(SILType)> packResultsToAggregate;
    packResultsToAggregate = [&](SILType aggregateTy) -> SILValue {
      if (isTensorFlowValue(aggregateTy))
        return *resultIt++;
      if (auto tupleTy = aggregateTy.getAs<TupleType>()) {
        SmallVector<SILValue, 8> elts;
        for (auto i : range(tupleTy->getNumElements()))
          elts.push_back(
              packResultsToAggregate(aggregateTy.getTupleElementType(i)));
        return B.createTuple(loc, aggregateTy, elts);
      }
      if (auto *decl = aggregateTy.getStructOrBoundGenericStruct()) {
        SmallVector<SILValue, 8> elts;
        for (auto *field : decl->getStoredProperties())
          elts.push_back(packResultsToAggregate(
              aggregateTy.getFieldType(field, B.getModule())));
        return B.createStruct(loc, aggregateTy, elts);
      }
      llvm_unreachable("Non-TF type should have been diagnosed");
    };
    auto result = packResultsToAggregate(origResult->getType());
    assert(resultIt == op->getResults().end()
           && "All result types should've been processed");
    // Replace any use of the old graph_op result with the newly packed aggregate.
    origResult->replaceAllUsesWith(result);
  }
  // There are multiple results, so each one must be a TensorFlow value. Simply
  // replace users, without doing any packing.
  else {
    SmallVector<SILType, 4> origResultTypes(origInst->getResultTypes());
    for (auto resultType : origResultTypes)
      assert(isTensorFlowValue(resultType) &&
             "when there are multiple results, they should be tf types");
    auto op = opBuilder.build(B, context, loc, origResultTypes);
    origInst->replaceAllUsesPairwiseWith(op);
  }

  origInst->eraseFromParent();

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
  if (simplifyTensorOperands())
    return;

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
  propagateSSAValues();

  logCurrentState("After propagateSSAValues", /*detailed*/ true);

  // Canonicalize attribute arguments, check that they have constants,
  // flatten array attributes, and form graph_op instructions.
  checkAttributesAndFormGraphOps();

  logCurrentState("Before Cleanup", /*detailed*/true);

  // Remove code that is dead now that tensor operations are formed.
  cleanupDeadInstructions();

  logCurrentState("Result", /*detailed*/false);
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

  assert((llvm::TFDynamicCompilation || !TFDisableDeabstraction) &&
         "If deabstraction is disabled, dynamic compilation must be enabled.");

  // If the TensorFlow module hasn't been imported by the program, don't do
  // anything.  This avoids impacting compile time for non-TensorFlow using
  // Swift programs by doing extraneous analysis.
  auto tfModule = ctx.getLoadedModule(ctx.Id_TensorFlow);
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

  SmallPtrSet<SILFunction*, 16> partitionedFunctions;

  // Loop over all of the functions in the current module processing them -
  // iff they look like they could be the top level of a deabstraction
  // context.
  for (auto &fn : *module) {
    if (TFDisableDeabstraction && !isAcceleratorOnly(fn))
      continue;

    // There's no point in deabstracting things defined in other modules,
    // because we won't lower them.
    if (fn.isAvailableExternally())
      continue;

    // If this function is a building block of larger tensor programs (e.g.
    // the ops defined in the TensorFlow module), then don't transform it in
    // isolation.
    // However, in dynamic compilation mode, deabstract everything in this
    // module because dynamic compilation mode executes functions individually
    // rather than inlining them into large tensor programs.
    //
    // TODO(marcrasi): IRGen should be able to handle non-deabstracted code.
    // Once it can handle non-deabstracted code, turn deabstraction off entirely
    // in dynamic compilation mode.
    if (!tfc.shouldBePartitioned(&fn, /*forceTFFunctions*/false) &&
        !llvm::TFDynamicCompilation)
      continue;

    // If something crashes, make sure the pretty stack trace says what we
    // were doing.
    llvm::PrettyStackTraceFormat X("TFDeabstraction on function %s",
                                   fn.getName().str().c_str());

    TFDeabstraction(*this, fn, tfc, constantEvaluator, PM).doIt();
    partitionedFunctions.insert(&fn);

    // TODO(clattner): This should eventually be the driver that kicks off
    // the partitioning pass as part of it, and the partitioning and later
    // passes are just function passes that are invoked by this one.  Until
    // we are ready for that, let them run later in the pipeline after the
    // other optimization and cleanup passes.
  }

  // Deabstract stragglers that were left out in the previous iteration. These
  // are functions that are *still* referred to in the code and operate on
  // tensor values, but have not been partitioned. It can happen in the
  // following case, for instance, where `foo` is an external function that has
  // no body and takes or returns Tensors:
  //   main() {
  //     foo() { $0 -= 0.1 * $1 }
  //   }
  //
  // In the common case where we have the body of foo(), it gets inlined, and
  // therefore, the closure gets inlined too. However, if the body of foo() is
  // not available, the closure never gets inlined. To ensure that the call to
  // the closure within foo() works correctly, we will have to deabstract and
  // partition the closure.
  //
  // (Note the body of a function may be missing when we are linking against a
  // library or in the REPL context where the function was defined on a
  // different REPL line.)
  //
  // We are doing this in two phases because we do not want to partition a
  // function unless it is absolutely necessary. In the first round, we
  // inline as much of the functions as possible during deabstraction. Many
  // of the functions would be dead after the first round, but some stragglers
  // remain as in the example above.
  for (auto &fn : *module) {
    if (TFDisableDeabstraction && !isAcceleratorOnly(fn))
      continue;

    // Skip if it is already partitioned, or if it was ignored only because it
    // operated on tensor values.
    if (partitionedFunctions.count(&fn) > 0 ||
        !tfc.shouldBePartitioned(&fn, /*forceTFFunctions=*/true))
      continue;
    TFDeabstraction(*this, fn, tfc, constantEvaluator, PM).doIt();
  }
}

SILTransform *swift::createTFDeabstraction() {
  return new TFDeabstractionPass();
}
