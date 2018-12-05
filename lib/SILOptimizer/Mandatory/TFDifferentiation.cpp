//===--- TFDifferentiation.cpp - SIL Automatic Differentiation --*- C++ -*-===//
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
//
// SWIFT_ENABLE_TENSORFLOW
//
// This file implements reverse-mode automatic differentiation.
//
// NOTE: Although the AD feature is developed as part of the Swift for
// TensorFlow project, it is completely independent from TensorFlow support.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/AutoDiff.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/ADT/DenseSet.h"

using namespace swift;
using llvm::DenseMap;
using llvm::SmallDenseMap;
using llvm::SmallDenseSet;
using llvm::SmallSet;

//===----------------------------------------------------------------------===//
// Helpers
//===----------------------------------------------------------------------===//

/// Prints an "[AD] " prefix to `llvm::dbgs()` and returns the debug stream.
/// This is being used to print short debug messages within the AD pass.
static raw_ostream &getADDebugStream() { return llvm::dbgs() << "[AD] "; }

/// Given a dumpable value, dumps it to `llvm::dbgs()`.
template <typename T> static inline void debugDump(T &v) {
  LLVM_DEBUG(llvm::dbgs() << "\n==== BEGIN DEBUG DUMP ====\n"
                          << v << "\n==== END DEBUG DUMP ====\n");
}

/// Given a set of AD indices, mangles it into a textual form.
static std::string mangleADIndices(const SILAutoDiffIndices &indices) {
  std::string result = "src_" + llvm::utostr(indices.source) + "_wrt_";
  interleave(indices.parameters.set_bits(),
             [&](unsigned idx) { result += llvm::utostr(idx); },
             [&] { result += '_'; });
  return result;
}

/// Mangles an AD configuration. The mangling rule looks like
///   "grad_src_<src_idx>_wrt_<param_idx0>_<param_idx1>_..._<options>"
/// ... where options mangle as the following:
///   "_s" : seedable
///   "_p" : preserving_result
///   "_d" : delayed
static std::string mangleADConfig(const SILAutoDiffConfig &config) {
  std::string result = "grad_" + mangleADIndices(config.indices);
  if (config.isSeedable())
    result += "_s";
  if (config.isPreservingResult())
    result += "_p";
  if (config.isDelayed())
    result += "_d";
  return result;
}

/// Creates arguments in the entry block based on the function type.
static void createEntryArguments(SILFunction *f) {
  auto *entry = f->getEntryBlock();
  auto conv = f->getConventions();
  assert((entry->getNumArguments() == 0 || conv.getNumSILArguments() == 0) &&
         "Entry already has arguments?!");
  for (auto indResultTy : conv.getIndirectSILResultTypes())
    entry->createFunctionArgument(
        f->mapTypeIntoContext(indResultTy).getAddressType());
  for (auto paramTy : conv.getParameterSILTypes())
    entry->createFunctionArgument(f->mapTypeIntoContext(paramTy));
}

/// Looks up a function in the current module. If it exists, returns it.
/// Otherwise, attempt to link it from imported modules. Returns null if such
/// function name does not exist.
static SILFunction *lookUpOrLinkFunction(StringRef name, SILModule &module) {
  assert(!name.empty());
  if (auto *localFn = module.lookUpFunction(name))
    return localFn;
  return module.findFunction(name, SILLinkage::PublicExternal);
}

/// Given a function, gather all of its formal results (both direct and
/// indirect) in an order defined by its result type. Note that "formal results"
/// refer to result values in the body of the function, not at call sites.
static void
collectAllFormalResultsInTypeOrder(SILFunction &function,
                                   SmallVectorImpl<SILValue> &results) {
  SILFunctionConventions convs(function.getLoweredFunctionType(),
                               function.getModule());
  auto indResults = function.getIndirectResults();
  auto *retInst = cast<ReturnInst>(function.findReturnBB()->getTerminator());
  auto retVal = retInst->getOperand();
  SmallVector<SILValue, 8> dirResults;
  if (auto *tupleInst =
          dyn_cast_or_null<TupleInst>(retVal->getDefiningInstruction()))
    dirResults.append(tupleInst->getElements().begin(),
                      tupleInst->getElements().end());
  else
    dirResults.push_back(retVal);
  unsigned indResIdx = 0, dirResIdx = 0;
  for (auto &resInfo : convs.getResults())
    results.push_back(resInfo.isFormalDirect() ? dirResults[dirResIdx++]
                                               : indResults[indResIdx++]);
}

/// Given a function call site, gather all of its actual results (both direct
/// and indirect) in an order defined by its result type.
template <typename IndResRange>
static void collectAllActualResultsInTypeOrder(
    ApplyInst *ai, ArrayRef<SILValue> extractedDirectResults,
    IndResRange &&indirectResults, SmallVectorImpl<SILValue> &results) {
  auto callee = ai->getCallee();
  SILFunctionConventions calleeConvs(
      callee->getType().castTo<SILFunctionType>(), ai->getModule());
  unsigned indResIdx = 0, dirResIdx = 0;
  for (auto &resInfo : calleeConvs.getResults()) {
    results.push_back(resInfo.isFormalDirect()
                          ? extractedDirectResults[dirResIdx++]
                          : indirectResults[indResIdx++]);
  }
}

/// Given a range of types, joins these into a single type. If there's exactly
/// one element type, returns that element type. Otherwise, creates a tuple type
/// of all element types.
template <typename TypeRange>
static CanType joinElementTypes(TypeRange &&range, const ASTContext &ctx) {
  if (range.size() == 1)
    return range.front();
  auto typeElts =
      map<SmallVector<TupleTypeElt, 8>>(range, [&](Type type) { return type; });
  return TupleType::get(typeElts, ctx);
}

/// Given a range of SIL values, retrives the canonical types of these values,
/// and joins these types into a single type.
template <typename SILValueRange>
static CanType joinElementTypesFromValues(SILValueRange &&range,
                                          const ASTContext &ctx) {
  if (range.size() == 1)
    return range.front()->getType().getASTType();
  SmallVector<TupleTypeElt, 8> elts;
  transform(range, elts.begin(),
            [&](SILValue val) { return val->getType().getASTType(); });
  return TupleType::get(elts, ctx)->getCanonicalType();
}

/// Looks through the definition of a function value. If the source that
/// produced this function value is `function_ref` and the function is visible
/// (either in the same module or is serialized), returns the instruction.
/// Otherwise, returns null.
static FunctionRefInst *findReferenceToVisibleFunction(SILValue value) {
  auto *inst = value->getDefiningInstruction();
  if (!inst)
    return nullptr;
  if (auto *fri = dyn_cast<FunctionRefInst>(inst)) {
    auto *fn = fri->getReferencedFunction();
    if (&fn->getModule() == &inst->getModule() ||
        fn->isSerialized() == IsSerialized)
      return fri;
  }
  if (auto *thinToThick = dyn_cast<ThinToThickFunctionInst>(inst))
    return findReferenceToVisibleFunction(thinToThick->getOperand());
  if (auto *convertFn = dyn_cast<ConvertFunctionInst>(inst))
    return findReferenceToVisibleFunction(convertFn->getOperand());
  if (auto *partialApply = dyn_cast<PartialApplyInst>(inst))
    return findReferenceToVisibleFunction(partialApply->getCallee());
  return nullptr;
}

/// Given an operator name, such as "+", and a protocol, returns the
/// "+" operator with type `(Self, Self) -> Self`. If the operator does not
/// exist in the protocol, returns null.
static FuncDecl *findAssociativeOperatorDeclInProtocol(DeclName operatorName,
                                                       ProtocolDecl *protocol) {
  assert(operatorName.isOperator());
  // Find the operator requirement in the `VectorNumeric` protocol
  // declaration and cache it.
  auto opLookUp = protocol->lookupDirect(operatorName);
  // Find the `+` with type siguature `(Self, Self) -> Self`.
  for (auto *decl : opLookUp) {
    auto *fd = dyn_cast<FuncDecl>(decl);
    if (!fd || !fd->isBinaryOperator() || !fd->isStatic())
      continue;
    auto *methodType = fd->getMethodInterfaceType()->castTo<AnyFunctionType>();
    // Make sure there are two parameters.
    if (methodType->getNumParams() != 2)
      continue;
    return fd;
  }
  // Not found.
  return nullptr;
}

/// Assuming the buffer is for indirect passing, returns the store ownership
/// qualifier for creating a `store` instruction into the buffer.
static StoreOwnershipQualifier getBufferSOQ(Type type, SILFunction &fn) {
  if (fn.hasQualifiedOwnership())
    return fn.getModule().Types.getTypeLowering(type).isTrivial()
               ? StoreOwnershipQualifier::Trivial
               : StoreOwnershipQualifier::Init;
  return StoreOwnershipQualifier::Unqualified;
}

/// Assuming the buffer is for indirect passing, returns the load ownership
/// qualified for creating a `load` instruction from the buffer.
static LoadOwnershipQualifier getBufferLOQ(Type type, SILFunction &fn) {
  if (fn.hasQualifiedOwnership())
    return fn.getModule().Types.getTypeLowering(type).isTrivial()
               ? LoadOwnershipQualifier::Trivial
               : LoadOwnershipQualifier::Take;
  return LoadOwnershipQualifier::Unqualified;
}

//===----------------------------------------------------------------------===//
// Auxiliary data structures
//===----------------------------------------------------------------------===//

namespace {
class ADContext;
class DifferentiationTask;

/// The invoker of a differentiation task. It can be some user syntax, e.g.
/// `#gradient` expression, the differentiation pass, or nothing at all. This
/// will be used to emit informative diagnostics.
struct DifferentiationInvoker {
public:
  /// The kind of the invoker of a differentiation task.
  enum class Kind {
    // No known invoker. This is the case when the differentiation is requested
    // from SIL source via a `gradient` instruction **without** being linked to
    // a Swift AST node.
    GradientInst,

    // Invoked by the indirect application of differentiation. This case has an
    // associated differentiation task reference.
    IndirectDifferentiation,

    // Invoked by a differential operator, such as `#gradient`, in the Swift
    // source. This case has an associated differential operator, i.e. a
    // `ReverseAutoDiffExpr`.
    DifferentialOperator,

    // Invoked by a `@differentiable` attribute in the Swift source. This case
    // has an associated `@differentiable` attribute.
    DifferentiableAttribute,

    // Invoker by a `[differentiable]` attribute in SIL **without** being lined
    // to a Swift AST attribute. This case has an associated `[differentiable]`
    // attribute.
    SILDifferentiableAttribute
  };

private:
  Kind kind;
  union Value {
    /// The instruction associated with the `SILSource` case.
    GradientInst *gradientInst;
    Value(GradientInst *inst) : gradientInst(inst) {}

    /// The parent differentiation task associated with the
    /// `IndirectDifferentiation` case.
    std::pair<ApplyInst *, DifferentiationTask *> indirectDifferentiation;
    Value(ApplyInst *applyInst, DifferentiationTask *parentTask)
        : indirectDifferentiation({applyInst, parentTask}) {}

    /// The differential operator associated with the `DifferentialOperator`
    /// case.
    ReverseAutoDiffExpr *differentialOperator;
    Value(ReverseAutoDiffExpr *expr) : differentialOperator(expr) {}

    /// The `@differentiable` attribute associated with the
    /// `DifferentiableAttribute` case.
    std::pair<DifferentiableAttr *, FuncDecl *> differentiableAttribute;
    Value(DifferentiableAttr *attr, FuncDecl *fd)
        : differentiableAttribute({attr, fd}) {}

    /// The `[differentiable]` attribute associated with the
    /// `SILDifferentiableAttribute` case.
    std::pair<SILDifferentiableAttr *, SILFunction *>
        silDifferentiableAttribute;
    Value(SILDifferentiableAttr *attr, SILFunction *f)
        : silDifferentiableAttribute({attr, f}) {}
  } value;

  /*implicit*/
  DifferentiationInvoker(Kind kind, Value value) : kind(kind), value(value) {}

public:
  DifferentiationInvoker(GradientInst *inst)
      : kind(Kind::GradientInst), value(inst) {}
  DifferentiationInvoker(ApplyInst *applyInst, DifferentiationTask *task)
      : kind(Kind::IndirectDifferentiation), value(applyInst, task) {}
  DifferentiationInvoker(ReverseAutoDiffExpr *expr)
      : kind(Kind::DifferentialOperator), value(expr) {}
  DifferentiationInvoker(DifferentiableAttr *attr, FuncDecl *fd)
      : kind(Kind::DifferentiableAttribute), value(attr, fd) {}
  DifferentiationInvoker(SILDifferentiableAttr *attr, SILFunction *f)
      : kind(Kind::SILDifferentiableAttribute), value(attr, f) {}

  Kind getKind() const { return kind; }

  GradientInst *getGradientInst() const {
    assert(kind == Kind::GradientInst);
    return value.gradientInst;
  }

  std::pair<ApplyInst *, DifferentiationTask *>
  getIndirectDifferentiation() const {
    assert(kind == Kind::IndirectDifferentiation);
    return value.indirectDifferentiation;
  }

  ReverseAutoDiffExpr *getDifferentialOperator() const {
    assert(kind == Kind::DifferentialOperator);
    return value.differentialOperator;
  }

  std::pair<DifferentiableAttr *, FuncDecl *>
  getDifferentiableAttribute() const {
    assert(kind == Kind::DifferentiableAttribute);
    return value.differentiableAttribute;
  }

  std::pair<SILDifferentiableAttr *, SILFunction *>
  getSILDifferentiableAttribute() const {
    assert(kind == Kind::SILDifferentiableAttribute);
    return value.silDifferentiableAttribute;
  }

  bool isAnyDifferentialOperator() const {
    return kind == Kind::DifferentialOperator || kind == Kind::GradientInst;
  }

  void print(llvm::raw_ostream &os) const;
};

/// Information about the primal function produced by PrimalGen, e.g.
/// mappings from the original values to their corresponding ones in the primal
/// value struct produced by the primal function.
///
/// A primal value struct is an aggregate value containing intermediate values
/// checkpointed during the primal computation. During PrimalGen, such a struct
/// will be generated for each function being differentiated, and each primal
/// function will return such a struct value for the adjoint function to
/// consume.
///
/// There are two kinds of primal values: control-independent ones (static) and
/// control-dependent ones (taped). The control-independent ones are stored in
/// the struct as normal members, each having a separate stored property
/// declaration. The control-dependent ones are stored per type in a
/// reference-typed stack data structure called `_AutoDiffTape`.
///
/// Beyond primal values, the primal value struct contains a special tape: the
/// predecessor trace tape. During execution of the primal, after each branch to
/// a basic block, a unique ID of the predecessor block will be pushed to this
/// stack. In the adjoint function, each basic block (except the exit block)
/// pops a unique ID from this tape and branches to the corresponding adjoint
/// block.
///
/// If the original function has the form:
///
///     sil @foo : ... {
///     bb0(%0):
///       %1 = ... [CHECKPOINT]           // $Float
///       %2 = ... [TO_MATERIALIZE]       // $Double
///       cond_br ... bb1(%2), bb2(%1)
///     bb1(%3):
///       %4 = ... [CHECKPOINT]           // $Float
///       br bb3
///     bb2(%5):
///       %6 = ... [TO_MATERIALIZE]       // $Double
///       %7 = ... [CHECKPOINT]           // $Int
///     bb3:
///       %8 = ... [CHECKPOINT]           // $Float
///       return
///
/// Then the primal value struct will look like the following:
///
///     struct foo__Type {
///       var v0: Float    // corresponding to %0
///       var v1: Float    // corresponding to %8
///
///       // Control-dependent values of type Float.
///       var t0: _AutoDiffTape<Float>
///
///       // Control-dependent values of type Double.
///       var t1: _AutoDiffTape<Double>
///
///       // The predecessor trace stack.
///       var pred_trace: _AutoDiffTape<Builtin.Word>
///     }
///
class PrimalInfo {
private:
  /// The primal value struct declaration.
  StructDecl *primalValueStruct = nullptr;

  /// The SIL module;
  const SILModule &module;

  /// The corresponding type of the primal value struct. This is initially
  /// null. After this field is computed, mutation of primal value will lead to
  /// unexpected behavior.
  StructType *primalValueStructType = nullptr;

  /// Mapping from original values that are preserved as non-control-dependent
  /// primal values to declaration references in the primal value struct.
  DenseMap<SILValue, VarDecl *> staticPrimalValueMap;

  /// Mapping from types of control-dependent direct primal values to distinct
  /// tapes. Tapes are uniqued by the element
  /// type.
  DenseMap<CanType, VarDecl *> directTapeTypeMap;

  /// Mapping from non-control-dependent `apply` instructions in the original
  /// function to the primal values returned by the corresponding call in the
  /// primal function.
  ///
  /// For example, in the original function:
  ///     %orig_res = apply %f(%x)
  ///
  /// This will be transformed into the following in the primal function:
  ///     %tuple = apply %f(%x)
  ///     %prim_val_0 = tuple_extract %tuple, 0
  ///     ...
  ///     %prim_val_n = tuple_extract %tuple, n
  ///     %orig_res_0 = tuple_extract %tuple, 0
  ///     ...
  ///     %orig_res_n = tuple_extract %tuple, n
  ///     %prim_vals = tuple (%prim_val_0, ..., %prim_val_n)     [CHECKPOINT]
  ///     %orig_results = tuple (%orig_res_0, ..., %orig_res_n)  [CHECKPOINT]
  ///
  /// If this function is non-control-dependent, primal values will be
  /// checkpointed into the primal value struct as a tuple member, and get
  /// inserted into `nestedStaticPrimalValueMap`. Otherwise, it'll go to the
  /// corresponding tape of its type.
  DenseMap<ApplyInst *, VarDecl *> nestedStaticPrimalValueMap;

  /// Mapping from types of control-dependent nested primal values to district
  /// tapes.
  DenseMap<CanType, VarDecl *> nestedTapeTypeMap;

  /// Set of control-dependent primal values that have been checkpointed.
  SmallPtrSet<SILValue, 16> tapedDirectPrimalValueSet;

  /// Mapping from original basic blocks to their associated IDs. In the primal
  /// function, we push the predecessor block ID for each basic block that has
  /// 2 or more incoming edges to the tape that traces control predecessors. In
  /// the adjoint function, we pop the ID from the tape and do a `switch_value`
  /// on it to go to the adjoint block corresponding to the original precessor
  /// block. This hash map will be populated during primal synthesis.
  DenseMap<SILBasicBlock *, unsigned> originalBlockIDs;

  /// Declaration reference of the tape in the primal value struct that stores
  /// a trace of predecessors for each block in the original function with 2 or
  /// more predecessors. This is non-null when the original function has control
  /// flow. This tape is guaranteed to have type
  /// `$Swift._AutoDiffTape<Builtin.Int64>`.
  VarDecl *predecessorTraceTapeDecl = nullptr;

  /// Mangler for mangling types.
  Mangle::ASTMangler mangler;

private:
  VarDecl *addVarDecl(StringRef name, Type type) {
    auto &ctx = primalValueStruct->getASTContext();
    auto id = ctx.getIdentifier(name);
    auto *varDecl = new (ctx) VarDecl(
        /*IsStatic*/ false, VarDecl::Specifier::Var,
        /*IsCaptureList*/ false, SourceLoc(), id, primalValueStruct);
    if (primalValueStruct->getEffectiveAccess() < AccessLevel::Public)
      varDecl->getAttrs().add(
          new (ctx) UsableFromInlineAttr(/*implicit*/ true));
    else
      varDecl->setAccess(AccessLevel::Public);
    varDecl->setInterfaceType(type);
    primalValueStruct->addMember(varDecl);
    return varDecl;
  }

public:
  PrimalInfo(const PrimalInfo &) = delete;
  PrimalInfo &operator=(const PrimalInfo &) = delete;

  explicit PrimalInfo(StructDecl *primalValueStruct, const SILModule &module)
      : primalValueStruct(&*primalValueStruct), module(module) {}

  /// Returns the primal value struct that the primal info is established
  /// around.
  StructDecl *getPrimalValueStruct() const { return primalValueStruct; }

  /// Computes the primal value struct type.
  StructType *computePrimalValueStructType() {
    assert(!primalValueStructType &&
           "The primal value struct type has been computed before");
    primalValueStructType = StructType::get(primalValueStruct, Type(),
                                            primalValueStruct->getASTContext());
    return primalValueStructType;
  }

  /// Returns the primal value struct type, assuming the primal value struct
  /// type has already been computed before.
  StructType *getPrimalValueStructType() const {
    assert(primalValueStructType &&
           "The primal value struct type has not been computed");
    return primalValueStructType;
  }

  /// Returns the lowered SIL type for the primal value struct.
  SILType getLoweredPrimalValueStructType() const {
    return module.Types.getLoweredType(getPrimalValueStructType());
  }

  /// Add a primal value decl for a non-control-dependent (static) value in the
  /// original function.
  VarDecl *addStaticPrimalValueDecl(SILValue originalValue) {
    auto *decl = addVarDecl("v_" + llvm::itostr(staticPrimalValueMap.size()),
                            originalValue->getType().getASTType());
    staticPrimalValueMap.insert({originalValue, decl});
    return decl;
  }

  /// Add a nested primal value decl for a non-control-dependent (static) primal
  /// value returned by the corresponding instruction in the primal function
  /// of an `apply` instruction in the original function.
  VarDecl *addNestedStaticPrimalValueDecl(ApplyInst *inst,
                                          CanType primalValueType) {
    auto *decl = addVarDecl(
        "pv_" + llvm::itostr(nestedStaticPrimalValueMap.size()),
        primalValueType);
    nestedStaticPrimalValueMap.insert({inst, decl});
    return decl;
  }

  /// Finds the primal value decl in the primal value struct for a static primal
  /// value in the original function.
  VarDecl *lookupDirectStaticPrimalValueDecl(SILValue originalValue) const {
    auto lookup = staticPrimalValueMap.find(originalValue);
    return lookup == staticPrimalValueMap.end() ? nullptr : lookup->getSecond();
  }

  /// Finds the primal value decl in the primal value struct for an `apply` in
  /// the original function.
  VarDecl *lookUpNestedStaticPrimalValueDecl(ApplyInst *inst) {
    auto lookup = nestedStaticPrimalValueMap.find(inst);
    return lookup == nestedStaticPrimalValueMap.end() ? nullptr
                                                      : lookup->getSecond();
  }

  /// Retrieves the tape decl in the primal value struct for the specified type.
  VarDecl *getOrCreateTapeDeclForType(CanType type) {
    auto &astCtx = primalValueStruct->getASTContext();
    auto insertion = directTapeTypeMap.try_emplace(type, nullptr);
    auto &tapeDecl = insertion.first->getSecond();
    if (insertion.second) {
      auto tapeType =
          BoundGenericType::get(astCtx.get_AutoDiffTapeDecl(), Type(), {type});
      tapeDecl = addVarDecl("t_" + mangler.mangleTypeAsUSR(type), tapeType);
    }
    return tapeDecl;
  }

  /// Retrieves the tape decl in the primal value struct for a value in the
  /// original function. Tapes are uniqued by the element type.
  VarDecl *getOrCreateTapeDeclForValue(SILValue value) {
    return getOrCreateTapeDeclForType(value->getType().getASTType());
  }

  /// Retrieves the 'predecessor trace' tape decl in the primal value struct for
  /// control flow support.
  VarDecl *getOrCreatePredecessorTraceTapeDecl() {
    if (predecessorTraceTapeDecl)
      return predecessorTraceTapeDecl;
    auto &ctx = primalValueStruct->getASTContext();
    auto tapeType = BoundGenericType::get(ctx.get_AutoDiffTapeDecl(), Type(),
                                          {getBuiltinType(ctx, "Int64")});
    predecessorTraceTapeDecl = addVarDecl("pred_trace", tapeType);
    return predecessorTraceTapeDecl;
  }
};

/// Tracks the progress of primal/adjoint synthesis for a task.
enum class FunctionSynthesisState {
  /// We do not need to synthesize this function.
  NotNeeded,

  /// We need to synthesize this function.
  Needed,

  /// The function has been added to the PrimalGen/AdjointGen worklist, but not
  /// yet synthesized.
  Pending,

  /// Synthesis is done: either the function has been synthesized, or an error
  /// occurred durng synthesis.
  Done
};

/// A differentiation task, specifying the original function and the
/// `[differentiable]` attribute on the function. PrimalGen and AdjointGen
/// will synthesize the primal and the adjoint for this task, filling the primal
/// and adjoint fields in the attribute.
///
/// NOTE: A task instance manages a `[differentiable]` SIL attribute and shall
/// be the only one that modifies this attribute.
class DifferentiationTask {
  friend llvm::DenseMapInfo<DifferentiationTask>;
  friend class ADContext;

private:
  ADContext &context;

  /// The original function to be differentiated.
  SILFunction *original;

  /// The `[differentiable]` attribute on the original function. Since
  /// attribute synthesis is part of differentiation, a `[differentiable]`
  /// attribute must be available when a `DifferentiationTask` is created. The
  /// AD configuration resides within the attribute. This is guaranteed to be
  /// non-null.
  SILDifferentiableAttr *attr;

  /// The invoker of this differentiation task.
  DifferentiationInvoker invoker;

  /// Primal info. If this is `nullptr`, then there is no primal values between
  /// the primal and the adjoint.
  std::unique_ptr<PrimalInfo> primalInfo = nullptr;

  /// Mapping from original `apply` instructions to their corresponding
  /// differentiation tasks, if it's active. This is filled during primal
  /// synthesis, so that adjoint synthesis does not need to recompute the
  /// original function and differentiation indices.
  DenseMap<ApplyInst *, DifferentiationTask *> associatedTasks;

  /// Cache for associated functions.
  SILFunction *primal = nullptr;
  SILFunction *adjoint = nullptr;
  SILFunction *vjp = nullptr;

  /// Tracks the progress of primal synthesis for this task.
  FunctionSynthesisState primalSynthesisState;

  /// Tracks the progress of adjoint synthesis for this task.
  FunctionSynthesisState adjointSynthesisState;

  /// Asserts that a transition from one state to another is valid.
  void validateTransition(FunctionSynthesisState from,
                          FunctionSynthesisState to) {
    switch (from) {
    case FunctionSynthesisState::NotNeeded:
      llvm_unreachable("should not change state from NotNeeded");
    case FunctionSynthesisState::Needed:
      assert(to == FunctionSynthesisState::Pending &&
             "Needed must transition to Pending");
      break;
    case FunctionSynthesisState::Pending:
      assert(to == FunctionSynthesisState::Done &&
             "Pending must transition to Done");
      break;
    case FunctionSynthesisState::Done:
      llvm_unreachable("should not change state from Done");
    }
  }

  /// Creates an empty primal for this task.
  void createEmptyPrimal();
  /// Creates an empty adjoint for this task.
  void createEmptyAdjoint();

  /// Creates a VJP for this task. Primal and adjoint (possibly empty) must
  /// exist.
  void createVJP();

protected:
  /// Create a differentiation task.
  ///
  /// Creates empty primal and adjoint functions, if this task needs to
  /// synthesize them. Creates fully-synthesized VJP if this task needs to
  /// synthesize it.
  ///
  /// @param context The ADContext where differentiation happens.
  /// @param original The original function to be differentiated.
  /// @param attr The [differentiable] attribute to take control of.
  /// @param invoker The invoker of this differentiation task.
  explicit DifferentiationTask(ADContext &context,
                               SILFunction *original,
                               SILDifferentiableAttr *&&attr,
                               DifferentiationInvoker invoker);

public:
  DifferentiationTask(const DifferentiationTask &) = delete;
  DifferentiationTask &operator=(const DifferentiationTask &) = delete;

  SILFunction *getOriginal() const { return original; }
  SILDifferentiableAttr *getAttribute() const { return attr; }
  DifferentiationInvoker getInvoker() const { return invoker; }

  PrimalInfo *getPrimalInfo() const { return primalInfo.get(); }

  const SILAutoDiffIndices &getIndices() const {
    return attr->getIndices();
  }

  FunctionSynthesisState getPrimalSynthesisState() const {
    return primalSynthesisState;
  }
  void setPrimalSynthesisState(FunctionSynthesisState newState) {
    validateTransition(primalSynthesisState, newState);
    primalSynthesisState = newState;
  }
  FunctionSynthesisState getAdjointSynthesisState() const {
    return adjointSynthesisState;
  }
  void setAdjointSynthesisState(FunctionSynthesisState newState) {
    validateTransition(adjointSynthesisState, newState);
    adjointSynthesisState = newState;
  }

  SILFunction *getPrimal() const { return primal; }
  SILFunction *getAdjoint() const { return adjoint; }
  SILFunction *getVJP() const { return vjp; }

  DenseMap<ApplyInst *, DifferentiationTask *> &getAssociatedTasks() {
    return associatedTasks;
  }

  bool isEqual(const DifferentiationTask &other) const {
    return original == other.original && attr == other.attr;
  }

  SILAutoDiffConfig getMasterConfig() const {
    return SILAutoDiffConfig::getMaster(getIndices());
  }

  void print(llvm::raw_ostream &os) const;
};

static inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                            DifferentiationInvoker invoker) {
  invoker.print(os);
  return os;
}

void DifferentiationInvoker::print(llvm::raw_ostream &os) const {
  os << "(differentiation_invoker ";
  switch (kind) {
  case Kind::GradientInst:
    os << "gradient_inst=(" << *getGradientInst() << ")";
    break;
  case Kind::IndirectDifferentiation: {
    auto indDiff = getIndirectDifferentiation();
    os << "indirect_differentiation=(apply_inst=(" << *indDiff.first
       << ") task=" << indDiff.second << ')';
    break;
  }
  case Kind::DifferentialOperator: {
    StreamPrinter printer(os);
    PrintOptions options;
    os << "differential_operator=(";
    getDifferentialOperator()->print(printer, options);
    os << ')';
    break;
  }
  case Kind::DifferentiableAttribute: {
    auto diffAttr = getDifferentiableAttribute();
    os << "differentiable_attribute=(attr=(";
    diffAttr.first->print(os);
    os << ") func_decl=" << diffAttr.second->getFullName();
    break;
  }
  case Kind::SILDifferentiableAttribute: {
    auto diffAttr = getSILDifferentiableAttribute();
    os << "sil_differentiable_attribute=(attr=(";
    diffAttr.first->print(os);
    os << ") function=" << diffAttr.second->getName();
    break;
  }
  }
  os << ')';
}

void DifferentiationTask::print(llvm::raw_ostream &os) const {
  os << "(differentiation_task original=@" << original->getName()
     << " attribute=";
  attr->print(os);
  os << " invoker=" << invoker << ")";
}

/// A task specifies the empty primal/adjoint function to be filled in, and what
/// its corresponding original function and differentiation indices are.
struct FunctionSynthesisItem {
  /// The original function that the new function will be cloned and synthesized
  /// based on.
  SILFunction *original;

  /// The function to be synthesized.
  SILFunction *target;

  /// The indices of reverse automatic differentiation.
  SILAutoDiffIndices indices;

  /// The parent differentiation task. This will be used for diagnostics.
  DifferentiationTask *task;
};

/// The kind of SIL value in the primal function.
enum class PrimalValueKind {
  /// The value can be converted from its arguments using the same instruction.
  Conversion,
  /// Intentially discarded for rematerialization.
  ToRematerialize,
  /// The value is statically grouped into the primal value struct and can be
  /// accessed directly using `struct_extract`.
  StaticCheckpoint,
  /// The value is pushed onto the differentiation tape in the struct and can be
  /// accessed when popped from the tape.
  TapeCheckpoint
};

using GradientLookUpKey = std::pair<SILFunction *, SILAutoDiffConfig>;

//===----------------------------------------------------------------------===//
// ADContext - Per-module contextual information for the Differentiation pass.
//===----------------------------------------------------------------------===//

class ADContext {
private:
  /// Reference to the main transform.
  SILModuleTransform &transform;

  /// The module where Differentiation is performed on.
  SILModule &module;

  /// AST context.
  ASTContext &astCtx = module.getASTContext();

  /// Shared pass manager.
  SILPassManager &passManager;

  /// A mapping from functions and AD configurations to gradient functions.
  ///
  /// NOTE: The parameter index array is hashed by reference, which is expected
  /// to point to [differentiable wrt ...]'s trailing index storage.
  DenseMap<GradientLookUpKey, SILFunction *> gradientMap;
  /// Canonical gradients to be filled.
  SmallVector<std::pair<GradientLookUpKey, SILFunction *>, 32>
      canonicalGradients;

  /// Queue of differentiation tasks.
  SmallVector<std::unique_ptr<DifferentiationTask>, 32> differentiationTasks;
  /// Mapping from enqueued differentiation tasks to their indices in
  /// `differentiationTasks`.
  SmallDenseMap<std::pair<SILFunction *, SILAutoDiffIndices>, unsigned>
      enqueuedTaskIndices;

  /// The SIL loader owned by the module.
  SerializedSILLoader *silLoader = module.getSILLoader();

  /// The VectorNumeric protocol in the standard library.
  ProtocolDecl *vectorNumericProtocol =
      astCtx.getProtocol(KnownProtocolKind::VectorNumeric);
  /// The Numeric protocol in the standard library.
  ProtocolDecl *numericProtocol =
      astCtx.getProtocol(KnownProtocolKind::Numeric);
  /// The AdditiveArithmetic protocol in the standard library.
  ProtocolDecl *additiveArithmeticProtocol =
    astCtx.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  /// The FloatingPoint protocol in the stanard library.
  ProtocolDecl *floatingPointProtocol =
      astCtx.getProtocol(KnownProtocolKind::FloatingPoint);

  /// `AdditiveArithmetic.+` declaration.
  mutable FuncDecl *cachedPlusFn = nullptr;

public:
  /// Construct an ADContext for the given module.
  explicit ADContext(SILModuleTransform &transform);

  SILModuleTransform &getTransform() const { return transform; }
  SILModule &getModule() const { return module; }
  ASTContext &getASTContext() const { return module.getASTContext(); }
  SILPassManager &getPassManager() const { return passManager; }
  Lowering::TypeConverter &getTypeConverter() { return module.Types; }

  ArrayRef<std::unique_ptr<DifferentiationTask>>
  getDifferentiationTasks() const {
    return differentiationTasks;
  }

  ArrayRef<std::pair<GradientLookUpKey, SILFunction *>>
  getCanonicalGradients() const {
    return canonicalGradients;
  }

  ProtocolDecl *getVectorNumericProtocol() const {
    return vectorNumericProtocol;
  }

  ProtocolDecl *getNumericProtocol() const {
    return numericProtocol;
  }

  ProtocolDecl *getAdditiveArithmeticProtocol() const {
    return additiveArithmeticProtocol;
  }

  ProtocolDecl *getFloatingPointProtocol() const {
    return floatingPointProtocol;
  }

  FuncDecl *getPlusDecl() const {
    if (!cachedPlusFn) {
      cachedPlusFn = findAssociativeOperatorDeclInProtocol(
          astCtx.getIdentifier("+"), additiveArithmeticProtocol);
      assert(cachedPlusFn && "AdditiveArithmetic.+ not found");
    }
    return cachedPlusFn;
  }

  /// Retrieves the file unit that contains implicit declarations in the
  /// current Swift module. If it does not exist, create one.
  ///
  // FIXME: Currently it defaults to any file unit in the module. To handle this
  // more properly, we should make a DerivedFileUnit class to contain all
  // synthesized implicit type declarations.
  SourceFile &getPrimalValueDeclContainer() {
    for (auto *file : module.getSwiftModule()->getFiles())
      if (auto *src = dyn_cast<SourceFile>(file))
        return *src;
    llvm_unreachable("No files?");
  }

  /// Creates a struct declaration (without contents) for storing primal values
  /// of a function. The newly created struct will have the same generic
  /// parameters as the function.
  StructDecl *createPrimalValueStruct(const DifferentiationTask *task);

  void insertGradient(const GradientLookUpKey &key, SILFunction *gradient) {
    gradientMap.insert({key, gradient});
    if (key.second.isMaster())
      canonicalGradients.push_back({key, gradient});
  }

  SILFunction *lookUpGradient(const GradientLookUpKey &key) const {
    auto lookup = gradientMap.find(key);
    return lookup == gradientMap.end() ? nullptr : lookup->getSecond();
  }

  SILFunction *lookUpCanonicalGradient(const DifferentiationTask *task) const {
    return lookUpGradient({task->original, task->getMasterConfig()});
  }

  /// Finds the `[differentiable]` attribute on the specified original function
  /// corresponding to the specified parameter indices. Returns nullptr if it
  /// does not exist.
  ///
  /// TODO: Currently we are doing a O(n) lookup. This could be improved by
  /// hashing on SILFunction's side or maintaining a dictionary in ADContext.
  /// In any case, this is not performance-critical.
  SILDifferentiableAttr *lookUpDifferentiableAttr(
      SILFunction *original, const SILAutoDiffIndices &indices) const {
    for (auto *attr : original->getDifferentiableAttrs())
      if (attr->getIndices() == indices)
        return attr;
    return nullptr;
  }

  SILDifferentiableAttr *createDifferentiableAttr(
      SILFunction *original, const SILAutoDiffIndices &indices) const {
    assert(!lookUpDifferentiableAttr(original, indices));
    auto *attr =
        SILDifferentiableAttr::create(getModule(), indices,
                                      /*primalName*/ StringRef(),
                                      /*adjointName*/ StringRef(),
                                      /*primitive*/ false);
    original->addDifferentiableAttr(attr);
    return attr;
  }

  /// Finds or creates a `[differentiable]` attribute on the specified
  /// original function corresponding to the specified parameter indices.
  SILDifferentiableAttr *getOrCreateDifferentiableAttr(
      SILFunction *original, const SILAutoDiffIndices &indices) {
    if (auto *attr = lookUpDifferentiableAttr(original, indices))
      return attr;
    return createDifferentiableAttr(original, indices);
  }

  /// Finds a differentiation task on a function such that the task produces
  /// adjoints for the specified indices.
  DifferentiationTask *
  lookUpDifferentiationTask(SILFunction *original,
                            const SILAutoDiffIndices &indices) {
    auto existing = enqueuedTaskIndices.find({original, indices});
    if (existing == enqueuedTaskIndices.end())
      return nullptr;
    return differentiationTasks[existing->getSecond()].get();
  }

  /// Finds a differentiation task on a function such that the task produces
  /// adjoints for the specified indices or, if such a task is not present,
  /// for the task with the least number of parameters that is a superset of
  /// the parameter indices in `indices`, and which corresponds to a
  /// primitive adjoint function.
  DifferentiationTask *
  lookUpMinimalDifferentiationTask(SILFunction *original,
                                   const SILAutoDiffIndices &indices) {
    auto supersetParamIndices = llvm::SmallBitVector();
    const auto &indexSet = indices.parameters;
    if (auto *existingTask = lookUpDifferentiationTask(original, indices))
      return existingTask;
    for (auto *rda : original->getDifferentiableAttrs()) {
      const auto &rdaIndexSet = rda->getIndices().parameters;
      // If all indices in indexSet are in rdaIndexSet, and it has fewer
      // indices than our current candidate and a primitive adjoint, rda is our
      // new candidate.
      if (!indexSet.test(rdaIndexSet) && // all indexSet indices in rdaIndexSet
          (supersetParamIndices.empty() || // fewer parameters than before
           rdaIndexSet.count() < supersetParamIndices.count()) &&
          (indexSet == rdaIndexSet || rda->isAdjointPrimitive()))
        supersetParamIndices = rda->getIndices().parameters;
    }
    auto existing = enqueuedTaskIndices.find(
        {original, {indices.source, supersetParamIndices}});
    if (existing == enqueuedTaskIndices.end())
      return nullptr;
    return differentiationTasks[existing->getSecond()].get();
  }

  /// Register a differentiation task in the global worklist. This will ensure
  /// that a `[differentiable]` attribute will be generated for the specified
  /// indices, and that primal/adjoint synthesis will be run in the
  /// Differentiation pass.
  ///
  /// The function must either be a definition or be serialized.
  DifferentiationTask *
  registerDifferentiationTask(SILFunction *original,
                              const SILAutoDiffIndices &indices,
                              DifferentiationInvoker invoker) {
    // Make sure this pair of original and indices is unique.
    assert(!lookUpDifferentiationTask(original, indices));
    // Make sure this function either has a body or has a
    // `[differentiable]` attribute that is a superset of all the
    if (original->isExternalDeclaration()) {
      // If it's serialized, deserialize it.
      assert(original->isSerialized() &&
             "Differentiation task cannot be on a function without a body");
      auto *deserializedFn = silLoader->lookupSILFunction(original);
      assert(deserializedFn && "Cannot deserialize original function");
      (void)deserializedFn;
    }
    auto *attr = getOrCreateDifferentiableAttr(original, indices);
    std::unique_ptr<DifferentiationTask> task(
        new DifferentiationTask(*this, original, std::move(attr), invoker));
    differentiationTasks.push_back(std::move(task));
    enqueuedTaskIndices.insert(
        {{original, indices}, differentiationTasks.size() - 1});
    return differentiationTasks.back().get();
  }

  DifferentiationTask *
  lookUpOrRegisterDifferentiationTask(SILFunction *original,
                                      const SILAutoDiffIndices &indices,
                                      DifferentiationInvoker invoker) {
    if (auto *existingTask = lookUpMinimalDifferentiationTask(original, indices))
      return existingTask;
    return registerDifferentiationTask(original, indices, invoker);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnose(SourceLoc loc, Diag<T...> diag,
                              U &&... args) const {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  /// Given an instruction and a differentiation task associated with the
  /// parent function, emits a "not differentiable" error based on the task. If
  /// the task is indirect, emits notes all the way up to the outermost task,
  /// and emits an error at the outer task. Otherwise, emits an error directly.
  void emitNondifferentiabilityError(SILInstruction *inst,
                                     const DifferentiationTask *task,
                                     Optional<Diag<>> diag = None);

  /// Given a value and a differentiation task associated with the parent
  /// function, emits a "not differentiable" error based on the task. If the
  /// task is indirect, emits notes all the way up to the outermost task, and
  /// emits an error at the outer task. Otherwise, emits an error directly.
  void emitNondifferentiabilityError(SILValue value,
                                     const DifferentiationTask *task,
                                     Optional<Diag<>> diag = None) {
    emitNondifferentiabilityError(value->getDefiningInstruction(), task, diag);
  }
};
} // end anonymous namespace

ADContext::ADContext(SILModuleTransform &transform)
    : transform(transform), module(*transform.getModule()),
      passManager(*transform.getPassManager()) {}

void ADContext::emitNondifferentiabilityError(SILInstruction *inst,
                                              const DifferentiationTask *task,
                                              Optional<Diag<>> diag) {
  // Location of the instruction.
  auto opLoc = inst->getLoc().getSourceLoc();
  auto invoker = task->getInvoker();
  LLVM_DEBUG(getADDebugStream()
             << "Diagnosing non-differentiability for value \n\t" << *inst
             << "\n"
             << "while performing differentiation task\n\t" << task << '\n');
  switch (invoker.getKind()) {
  // For a gradient instruction or a `[differentiable]` attribute that is not
  // associated with any source location, we emit a diagnostic at the
  // instruction source location.
  case DifferentiationInvoker::Kind::GradientInst:
  case DifferentiationInvoker::Kind::SILDifferentiableAttribute:
    diagnose(opLoc,
             diag.getValueOr(diag::autodiff_expression_is_not_differentiable));
    break;

  // For indirect differentiation, emit a "not differentiable" note on the
  // expression first. Then emit an error at the source invoker of
  // differentiation, and a "when differentiating this"  note at each indirect
  // invoker.
  case DifferentiationInvoker::Kind::IndirectDifferentiation: {
    std::tie(inst, task) = task->getInvoker().getIndirectDifferentiation();
    emitNondifferentiabilityError(inst, task, None);
    diagnose(opLoc, diag.getValueOr(
                        diag::autodiff_when_differentiating_function_call));
    break;
  }

  // For a differential operator, emit a "not differentiable" error on the
  // attribute first and a note on the non-differentiable operation.
  case DifferentiationInvoker::Kind::DifferentialOperator: {
    auto *expr = invoker.getDifferentialOperator();
    diagnose(expr->getLoc(), diag::autodiff_function_not_differentiable)
        .highlight(expr->getOriginalExpr()->getSourceRange());
    diagnose(opLoc,
             diag.getValueOr(diag::autodiff_expression_is_not_differentiable));
    break;
  }

  // For a `@differentiable` attribute, emit a "not differentiable" error on the
  // attribute first and a note on the non-differentiable operation.
  case DifferentiationInvoker::Kind::DifferentiableAttribute: {
    auto diffAttr = invoker.getDifferentiableAttribute();
    diagnose(diffAttr.first->getLocation(),
             diag::autodiff_function_not_differentiable)
        .highlight(diffAttr.second->getNameLoc());
    diagnose(opLoc,
             diag.getValueOr(diag::autodiff_expression_is_not_differentiable));
    break;
  }
  }
}

//===----------------------------------------------------------------------===//
// Control flow canonicalization
//===----------------------------------------------------------------------===//

namespace {
class ControlFlowCanonicalization {
private:
  SILFunction &function;
  SILBuilder builder = SILBuilder(function);
  DominanceInfo &domInfo;
  SILLoopInfo &loopInfo;

public:
  explicit ControlFlowCanonicalization(SILFunction &function,
                                       DominanceInfo &domInfo,
                                       SILLoopInfo &loopInfo)
      : function(function), domInfo(domInfo), loopInfo(loopInfo) {}

  /// Run control flow canonicalization on the function. Returns true if the
  /// program changed.
  bool run();
};
} // namespace

bool ControlFlowCanonicalization::run() {
  LLVM_DEBUG(getADDebugStream() << "Running control flow canonicalization on "
                                   "function "
                                << function.getName() << '\n');
  bool changed = false;
  assert(!function.isNoReturnFunction() && !function.isExternalDeclaration());
  assert(function.findReturnBB().getNodePtr());
  // Canonicalize loops.
  canonicalizeAllLoops(&domInfo, &loopInfo);
  // TODO: Handle multiple loop exits.
  return changed;
}

//===----------------------------------------------------------------------===//
// Activity Analysis
//===----------------------------------------------------------------------===//

namespace {
class DifferentiableActivityInfo;

/// In many real situations, the end-users of AD need only the derivatives of
/// some selected outputs of `P` with respect to some selected inputs of `P`.
/// Whatever the differentiation mode (tangent, reverse,...), these restrictions
/// allow the AD tool to produce a much more efficient differentiated program.
/// Essentially, fixing some inputs and neglecting some outputs allows AD to
/// just forget about several intermediate differentiated variables.
///
/// Activity analysis is the specific analysis that detects these situations,
/// therefore allowing for a better differentiated code. Activity analysis is
/// present in all transformation-based AD tools.
///
/// To begin with, the end-user specifies that only some output variables (the
/// “dependent”) must be differentiated with respect to only some input
/// variables (the “independent”). We say that variable `y` depends on `x` when
/// the derivative of `y` with respect to `x` is not trivially null. We say that
/// a variable is “varied” if it depends on at least one independent. Conversely
/// we say that a variable is “useful” if at least one dependent depends on it.
/// Finally, we say that a variable is “active” if it is at the same time varied
/// and useful. In the special case of the tangent mode, it is easy to check
/// that when variable `v` is not varied at some place in the program, then its
/// derivative `v̇` at this place is certainly null. Conversely when variable `v`
/// is not useful, then whatever the value of `v̇`, this value does not matter
/// for the final result. Symmetric reasoning applies for the reverse mode of
/// AD: observing that differentiated variables go upstream, we see that a
/// useless variable has a null derivative, in other words the partial
/// derivative of the output with respect to this variable is null. Conversely
/// when variable `v` is not varied, then whatever the value of `v`, this value
/// does not matter for the final result.
///
/// Reference:
/// Laurent Hascoët. Automatic Differentiation by Program Transformation. 2017.
class DifferentiableActivityAnalysis
    : public FunctionAnalysisBase<DifferentiableActivityInfo> {
private:
  DominanceAnalysis *dominanceAnalysis = nullptr;

public:
  explicit DifferentiableActivityAnalysis()
      : FunctionAnalysisBase(SILAnalysisKind::DifferentiableActivity) {}

  static bool classof(const SILAnalysis *s) {
    return s->getKind() == SILAnalysisKind::DifferentiableActivity;
  }

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind k) override {
    return k & InvalidationKind::Everything;
  }

  virtual std::unique_ptr<DifferentiableActivityInfo>
  newFunctionAnalysis(SILFunction *f) override;

  virtual void initialize(SILPassManager *pm) override;
};
} // end anonymous namespace

namespace {
/// Represents the differentiation activity associated with a SIL value.
enum class ActivityFlags : unsigned {
  /// The value depends on a function parameter.
  Varied = 1 << 1,
  /// The value contributes to a result.
  Useful = 1 << 2,
  /// The value is both varied and useful.
  Active = Varied | Useful,
};

using Activity = OptionSet<ActivityFlags>;

/// Result of activity analysis on a function. Accepts queries for whether a
/// value is "varied", "useful" or "active" against certain differentiation
/// indices.
class DifferentiableActivityInfo {
private:
  SILFunction &function;

  /// Input values, i.e. parameters (both direct and indirect).
  SmallVector<SILValue, 4> inputValues;
  /// Output values, i.e. individual values (not the final tuple) being returned
  /// by the `return` instruction.
  SmallVector<SILValue, 4> outputValues;

  /// The set of useful variables, indexed by the corresponding dependent value
  /// (output) index.
  SmallVector<SmallDenseSet<SILValue>, 4> usefulValueSets;
  /// The set of useful variables, indexed by the corresponding independent
  /// value (input) index.
  SmallVector<SmallDenseSet<SILValue>, 4> variedValueSets;

  /// Perform analysis and populate sets.
  void analyze();

public:
  explicit DifferentiableActivityInfo(SILFunction &f);

  bool isIndependent(SILValue value,
                     const SILAutoDiffIndices &indices) const;
  bool isDependent(SILValue value,
                   const SILAutoDiffIndices &indices) const;
  bool isVaried(SILValue value, unsigned independentVariableIndex) const;
  bool isUseful(SILValue value, unsigned dependentVariableIndex) const;
  bool isVaried(SILValue value,
                const llvm::SmallBitVector &parameterIndices) const;
  bool isActive(SILValue value, const SILAutoDiffIndices &indices) const;

  Activity getActivity(SILValue value,
                       const SILAutoDiffIndices &indices) const;
  Activity getActivity(SILInstruction *inst,
                       const SILAutoDiffIndices &indices) const;
};
} // end anonymous namespace

std::unique_ptr<DifferentiableActivityInfo>
DifferentiableActivityAnalysis::newFunctionAnalysis(SILFunction *f) {
  assert(dominanceAnalysis && "Expect a valid dominance anaysis");
  return llvm::make_unique<DifferentiableActivityInfo>(*f);
}

void DifferentiableActivityAnalysis::initialize(SILPassManager *pm) {
  dominanceAnalysis = pm->getAnalysis<DominanceAnalysis>();
}

SILAnalysis *swift::createDifferentiableActivityAnalysis(SILModule *m) {
  return new DifferentiableActivityAnalysis();
}

DifferentiableActivityInfo::DifferentiableActivityInfo(SILFunction &f)
    : function(f) {
  analyze();
}

/// Recursively find all "varied" values relative to the given value.
///
/// NOTE: The given value will **not** be considered varied.
static void collectVariedValues(SILValue value,
                                SmallDenseSet<SILValue> &variedValues,
                                unsigned inputIndex,
                                SmallDenseSet<SILValue> &visited) {
  auto insertion = visited.insert(value);
  if (!insertion.second)
    return;
  for (auto use : value->getUses()) {
    auto *inst = use->getUser();
    // If there's a `store` of this value, we consider the destination varied.
    if (auto *storeInst = dyn_cast<StoreInst>(inst)) {
      SILValue buffer = storeInst->getDest();
      // If the def is `begin_access`, then its operand is the actual buffer.
      if (auto *def = dyn_cast_or_null<BeginAccessInst>(
              buffer->getDefiningInstruction()))
        buffer = def->getOperand();
      LLVM_DEBUG(getADDebugStream() << "VARIED @ " << inputIndex << ":\n"
                                    << buffer << '\n');
      variedValues.insert(buffer);
      visited.insert(buffer);
      collectVariedValues(buffer, variedValues, inputIndex, visited);
      continue;
    }
    // For other instructions, consider their results varied.
    for (auto val : inst->getResults()) {
      LLVM_DEBUG(getADDebugStream() << "VARIED @ " << inputIndex << ":\n"
                                    << val << '\n');
      variedValues.insert(val);
      // Recursively collect.
      collectVariedValues(val, variedValues, inputIndex, visited);
    }
  }
}

/// Recursively find all "useful" values relative to the given value.
///
/// NOTE: The given value will be considered useful.
static void collectUsefulValues(SILValue value,
                                SmallDenseSet<SILValue> &usefulValues,
                                unsigned outputIndex) {
  LLVM_DEBUG(getADDebugStream() << "USEFUL @ " << outputIndex << ":\n"
                                << value << '\n');
  usefulValues.insert(value);
  if (auto *def = value->getDefiningInstruction())
    for (auto &op : def->getAllOperands())
      collectUsefulValues(op.get(), usefulValues, outputIndex);
}

void DifferentiableActivityInfo::analyze() {
  LLVM_DEBUG(getADDebugStream()
             << "Running activity analysis on @" << function.getName() << '\n');
  // Inputs are just function's arguments, count `n`.
  auto paramArgs = function.getArgumentsWithoutIndirectResults();
  for (auto valueAndIndex : enumerate(paramArgs)) {
    inputValues.push_back(valueAndIndex.first);
  }
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Inputs in @" << function.getName() << ":\n";
    for (auto val : inputValues)
      s << val << '\n';
  });
  // Outputs are indirect result buffers and return values, count `m`.
  collectAllFormalResultsInTypeOrder(function, outputValues);
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Outputs in @" << function.getName() << ":\n";
    for (auto val : outputValues)
      s << val << '\n';
  });
  // Initialize sets to store useful values and varied values.
  usefulValueSets.append(outputValues.size(), {});
  variedValueSets.append(inputValues.size(), {});
  // Mark varied values for each independent varible.
  for (auto valAndIdx : enumerate(inputValues)) {
    SmallDenseSet<SILValue> visitedVariedValues;
    collectVariedValues(valAndIdx.first, variedValueSets[valAndIdx.second],
                        valAndIdx.second, visitedVariedValues);
  }
  // Mark useful values for each dependent variable.
  for (auto valAndIdx : enumerate(outputValues))
    collectUsefulValues(valAndIdx.first, usefulValueSets[valAndIdx.second],
                        valAndIdx.second);
}

bool DifferentiableActivityInfo::isIndependent(
    SILValue value, const SILAutoDiffIndices &indices) const {
  for (auto paramIdx : indices.parameters.set_bits())
    if (inputValues[paramIdx] == value)
      return true;
  return false;
}

bool DifferentiableActivityInfo::isDependent(
    SILValue value, const SILAutoDiffIndices &indices) const {
  return inputValues[indices.source] == value;
}

bool DifferentiableActivityInfo::isVaried(
    SILValue value, unsigned independentVariableIndex) const {
  auto &set = variedValueSets[independentVariableIndex];
  return set.count(value);
}

bool DifferentiableActivityInfo::isVaried(
    SILValue value, const llvm::SmallBitVector &parameterIndices) const {
  for (auto paramIdx : parameterIndices.set_bits())
    if (isVaried(value, paramIdx))
      return true;
  return false;
}

bool DifferentiableActivityInfo::isUseful(
    SILValue value, unsigned dependentVariableIndex) const {
  auto &set = usefulValueSets[dependentVariableIndex];
  return set.count(value);
}

bool DifferentiableActivityInfo::isActive(
    SILValue value, const SILAutoDiffIndices &indices) const {
  return isVaried(value, indices.parameters) && isUseful(value, indices.source);
}

Activity DifferentiableActivityInfo::getActivity(
    SILValue value, const SILAutoDiffIndices &indices) const {
  Activity activity;
  if (isVaried(value, indices.parameters))
    activity |= ActivityFlags::Varied;
  if (isUseful(value, indices.source))
    activity |= ActivityFlags::Useful;
  return activity;
}

Activity DifferentiableActivityInfo::getActivity(
    SILInstruction *inst, const SILAutoDiffIndices &indices) const {
  Activity activity;
  for (auto result : inst->getResults())
    activity |= getActivity(result, indices);
  return activity;
}

static void dumpActivityInfo(SILValue value,
                             const SILAutoDiffIndices &indices,
                             const DifferentiableActivityInfo &activityInfo,
                             llvm::raw_ostream &s = llvm::dbgs()) {
  s << '[';
  auto activity = activityInfo.getActivity(value, indices);
  switch (activity.toRaw()) {
  case 0: s << "NONE"; break;
  case (unsigned)ActivityFlags::Varied: s << "VARIED"; break;
  case (unsigned)ActivityFlags::Useful: s << "USEFUL"; break;
  case (unsigned)ActivityFlags::Active: s << "ACTIVE"; break;
  }
  s << "] " << value;
}

static void dumpActivityInfo(SILFunction &fn,
                             const SILAutoDiffIndices &indices,
                             DifferentiableActivityInfo &activityInfo,
                             llvm::raw_ostream &s = llvm::dbgs()) {
  s << "Activity info for " << fn.getName() << " at " << indices << '\n';
  for (auto &bb : fn) {
    for (auto *arg : bb.getArguments())
      dumpActivityInfo(arg, indices, activityInfo, s);
    for (auto &inst : bb)
      for (auto res : inst.getResults())
        dumpActivityInfo(res, indices, activityInfo, s);
  }
}

//===----------------------------------------------------------------------===//
// Code emission utilities
//===----------------------------------------------------------------------===//

/// Given a value, extracts all elements to `result` from this value if it's a
/// tuple. Otherwise, add this value directly to `result`.
static void extractAllElements(SILValue val, SILBuilder &builder,
                               SmallVectorImpl<SILValue> &result) {
  if (auto tupleType = val->getType().getAs<TupleType>())
    for (auto i : range(tupleType->getNumElements()))
      result.push_back(builder.createTupleExtract(val.getLoc(), val, i));
  else
    result.push_back(val);
}

/// Given a range of elements, joins these into a single value. If there's
/// exactly one element, returns that element. Otherwise, creates a tuple using
/// a `tuple` instruction.
static SILValue joinElements(ArrayRef<SILValue> elements, SILBuilder &builder,
                             SILLocation loc) {
  if (elements.size() == 1)
    return elements.front();
  return builder.createTuple(loc, elements);
}

/// When a function value is used in an instruction (usually `apply`), there's
/// some conversion instruction in between, e.g. `thin_to_thick_function`. Given
/// a new function value and an old function value, this helper function
/// recursively converts the new function just like how the old function is
/// converted.
static SILValue
reapplyFunctionConversion(SILValue newFunc, SILValue oldFunc,
                          SILValue oldConvertedFunc, SILBuilder &builder,
                          SILLocation loc,
                          std::function<SILValue(SILValue)> substituteOperand =
                              [](SILValue v) { return v; }) {
  // If the old func is the new func, then there's no conversion.
  if (oldFunc == oldConvertedFunc)
    return newFunc;
  // Handle a few instruction cases.
  // thin_to_thick_function
  if (auto *tttfi = dyn_cast<ThinToThickFunctionInst>(oldConvertedFunc)) {
    auto innerNewFunc = reapplyFunctionConversion(
        newFunc, oldFunc, tttfi->getOperand(), builder, loc, substituteOperand);
    auto operandFnTy = innerNewFunc->getType().castTo<SILFunctionType>();
    auto thickTy = operandFnTy->getWithRepresentation(
        SILFunctionTypeRepresentation::Thick);
    auto silTy = SILType::getPrimitiveObjectType(thickTy);

    return builder.createThinToThickFunction(loc, innerNewFunc, silTy);
  }
  // partial_apply
  if (auto *pai = dyn_cast<PartialApplyInst>(oldConvertedFunc)) {
    SmallVector<SILValue, 8> newArgs;
    newArgs.reserve(pai->getNumArguments());
    for (auto arg : pai->getArguments())
      newArgs.push_back(substituteOperand(arg));
    auto innerNewFunc = reapplyFunctionConversion(
        newFunc, oldFunc, pai->getCallee(), builder, loc, substituteOperand);
    return builder.createPartialApply(
        loc, innerNewFunc, pai->getSubstitutionMap(), newArgs,
        ParameterConvention::Direct_Guaranteed);
  }
  llvm_unreachable("Unhandled function convertion instruction");
}

/// Create a builtin floating point value. The specified type must be a builtin
/// float type.
static SILValue createBuiltinFPScalar(intmax_t scalar, CanType type,
                                      SILLocation loc, SILBuilder &builder) {
  auto *fpType = type->castTo<BuiltinFloatType>();
  return builder.createFloatLiteral(
      loc, SILType::getPrimitiveObjectType(type),
      APFloat(fpType->getAPFloatSemantics(), scalar));
}

/// Convert an integer literal to a type that is expressible by integer literal.
///
/// The root address derivation of `resultBufAccess` must be the result of a
/// `begin_access`.
static void convertIntToIndirectExpressible(intmax_t scalar,
                                            NominalTypeDecl *targetTypeDecl,
                                            SILValue resultBufAccess,
                                            SILLocation loc,
                                            SILBuilder &builder,
                                            ADContext &context) {
  auto &module = builder.getModule();
  auto &astCtx = module.getASTContext();
  auto targetTy =
      targetTypeDecl->getDeclaredInterfaceType()->getCanonicalType();
  // Step 1. Initialize a value of type `<target type>.IntegerLiteralType` from
  // the given value.
  DeclName intLitTypeName(astCtx.Id_IntegerLiteralType);
  SmallVector<ValueDecl *, 1> intLitTypeLookUpResults;
  targetTypeDecl->lookupQualified(targetTy, intLitTypeName, NL_OnlyTypes,
                                  /*typeResolver*/ nullptr,
                                  intLitTypeLookUpResults);
  assert(intLitTypeLookUpResults.size() == 1);
  auto intLitTypeAliasDecl =
      cast<TypeAliasDecl>(intLitTypeLookUpResults[0]);
  // Now we have the IntegerLiteralType type.
  auto intLitTy = intLitTypeAliasDecl
      ->getUnderlyingTypeLoc().getType()->getCanonicalType();
  auto *intLitTypeDecl = intLitTy->getAnyNominal();
  assert(intLitTypeDecl);
  // %1 = float_literal $Builtin.FPIEEE80, <value>
  auto builtinIntegerLiteralTy = SILType::getBuiltinIntegerLiteralType(astCtx);
  auto builtinInteger =
      builder.createIntegerLiteral(loc, builtinIntegerLiteralTy, scalar);
  // %2 = metatype $@thin <target type>.IntegerLiteralType.Type
  auto intLitMetatypeTy = SILType::getPrimitiveObjectType(
      CanMetatypeType::get(intLitTy, MetatypeRepresentation::Thick));
  auto *intLitMetatype = builder.createMetatype(loc, intLitMetatypeTy);
  // ExpressibleByBuiltinIntegerLiteral
  auto *ebilProto =
      astCtx.getProtocol(KnownProtocolKind::ExpressibleByBuiltinIntegerLiteral);
  // `init(_builtinIntegerLiteral:)`
  DeclName builtinLitInitName(astCtx, DeclBaseName::createConstructor(),
                              {astCtx.Id_builtinIntegerLiteral});
  auto *initBILDecl =
      cast<ConstructorDecl>(ebilProto->lookupDirect(builtinLitInitName)[0]);
  SILDeclRef initBILDeclRef(initBILDecl);
  auto initBILType = context.getTypeConverter().getConstantType(initBILDeclRef);
  // Look up `IntegerLiteralType : _ExpressibleByBuiltinIntegerLiteral`. This is
  // guaranteed to be a normal conformance.
  auto *ebilConf = astCtx.getConformance(intLitTy, ebilProto,
                                         intLitTypeDecl->getLoc(),
                                         intLitTypeDecl,
                                         ProtocolConformanceState::Complete);
  ProtocolConformanceRef ebilConfRef(ebilConf);
  // %3 = witness_method ...
  auto initBILFn = builder.createWitnessMethod(loc, intLitTy, ebilConfRef,
                                               initBILDeclRef, initBILType);
  // Ensure the witness method is linked.
  module.lookUpFunctionInWitnessTable(ebilConfRef, initBILDeclRef);
  // Get substitutions.
  auto intLitSubMap = SubstitutionMap::getProtocolSubstitutions(
      ebilProto, intLitTy, ebilConfRef);
  // Allocate result buffer.
  // %intLitBuf = alloc_stack $IntegerLiteralType
  auto *intLitBuf = builder.createAllocStack(
      loc, SILType::getPrimitiveObjectType(intLitTy));
  SWIFT_DEFER {
    // dealloc_stack %intLitBuf : $*IntegerLiteralType
    builder.createDeallocStack(loc, intLitBuf);
  };
  // %4 = apply %3 <...>(%intLitBuf, %1, %2)
  builder.createApply(loc, initBILFn, intLitSubMap,
                      {intLitBuf, builtinInteger, intLitMetatype},
                      /*isNonThrowing*/ false);
  // Step 2. Initialize a value of type `<target type>` by calling
  // `ExpressibleByIntegerLiteral.init(integerLiteral: %4)`.
  // %5 = metatype $@thin <target type>.IntegerLiteralType.Type
  auto targetMetatypeTy = SILType::getPrimitiveObjectType(
      CanMetatypeType::get(targetTy, MetatypeRepresentation::Thick));
  auto *targetMetatype = builder.createMetatype(loc, targetMetatypeTy);
  auto *eilProto =
      astCtx.getProtocol(KnownProtocolKind::ExpressibleByIntegerLiteral);
  DeclName intLitInitName(astCtx, DeclBaseName::createConstructor(),
                          {astCtx.Id_integerLiteral});
  auto *initILDecl =
      cast<ConstructorDecl>(eilProto->lookupDirect(intLitInitName)[0]);
  SILDeclRef initILDeclRef(initILDecl);
  auto initILType = context.getTypeConverter().getConstantType(initILDeclRef);
  // Lookup `<target type> : ExpressibleByIntegerLiteral`.
  auto *parentModule = targetTypeDecl->getModuleContext();
  auto eilConf = *parentModule->lookupConformance(targetTy, eilProto);
  ProtocolConformanceRef eilConfRef(eilConf);
  // context.lookUpOrLinkWitnessTable(eilConfRef);
  // %6 = witness_method ...
  auto initILFn = builder.createWitnessMethod(loc, targetTy, eilConfRef,
                                              initILDeclRef, initILType);
  // Ensure the witness method is linked.
  module.lookUpFunctionInWitnessTable(eilConfRef, initILDeclRef);
  // Get substitutions.
  auto targetSubMap =
      SubstitutionMap::getProtocolSubstitutions(eilProto, targetTy, eilConfRef);
  // %7 = apply %6 <...>(%resultBuf, %intLitBuf, %5)
  builder.createApply(loc, initILFn, targetSubMap,
                      {resultBufAccess, intLitBuf, targetMetatype},
                      /*isNonThrowing*/ false);
}

/// Create a scalar value in the specified type indirectly in a buffer.
///
/// The specified type must satisfy **one** of these requirements:
/// - It is a builtin floating point type like `Builtin.FPIEEE32`.
/// - It conforms to `FloatingPoint`.
/// - It conforms `VectorNumeric` and its nested type `Scalar` conforms
///   to `FloatingPoint`.
///
/// The root address derivation of `seedBufAccess` must be the result of
/// a `begin_access`.
static void createScalarValueIndirect(intmax_t scalar, CanType type,
                                      SILValue seedBufAccess, SILLocation loc,
                                      SILBuilder &builder, ADContext &context) {
  auto &astCtx = context.getASTContext();
  auto *swiftMod = context.getModule().getSwiftModule();
  auto tangentSpace = astCtx.getTangentSpace(type, swiftMod);
  assert(tangentSpace && "No tangent space for this type");
  // See if the type is a builtin float. If so, we don't do protocol
  // conformance-based conversion.
  switch (tangentSpace->getKind()) {
  // Builtin scalar is just a `float_literal` instruction.
  case TangentSpace::Kind::BuiltinRealScalar: {
    auto *fpType = type->castTo<BuiltinFloatType>();
    auto scalarVal =
        createBuiltinFPScalar(scalar, fpType->getCanonicalType(), loc, builder);
    builder.createStore(loc, scalarVal, seedBufAccess,
                        getBufferSOQ(type, builder.getFunction()));
    return;
  }
  // Real scalar gets initialized through
  // `<target_type>.IntegerLiteralType.init(_builtinIntegerLiteral:)` and
  // `<target_type>.init(integerLiteral:)`.
  case TangentSpace::Kind::RealScalar: {
    auto *decl = tangentSpace->getRealScalarSpace();
    convertIntToIndirectExpressible(scalar, decl, seedBufAccess, loc, builder,
                                    context);
    return;
  }
  // Real vector gets initialized through
  // `<target_type>.Scalar.IntegerLiteralType
  //     .init(_builtinIntegerLiteral:)`,
  // `<target_type>.Scalar.init(integerLiteral:)` and
  // `<target_type>.init(_:)`.
  // TODO: Remove and reject this case. Real vectors should never be a seed.
  case TangentSpace::Kind::RealVector: {
    auto *targetTypeDecl = tangentSpace->getRealVectorSpace();
    auto &astCtx = context.getASTContext();
    auto &module = context.getModule();
    // Create a scalar value from the specified integer literal.
    DeclName scalarDeclName(astCtx.getIdentifier("Scalar"));
    auto currencyDeclLookUpResult =
        targetTypeDecl->lookupDirect(scalarDeclName);
    auto *scalarElemAlias = cast<TypeAliasDecl>(currencyDeclLookUpResult[0]);
    auto scalarTy =
        scalarElemAlias->getDeclaredInterfaceType()->getCanonicalType();
    auto currencySubMap =
        type->getMemberSubstitutionMap(module.getSwiftModule(),
                                       scalarElemAlias);
    scalarTy = scalarTy.subst(currencySubMap)->getCanonicalType();
    auto *scalarTyDecl = scalarTy.getAnyNominal();
    assert(scalarTyDecl && "Scalar must be a nominal type");
    // %0 = ... : $<scalar type>
    auto scalarBuf = builder.createAllocStack(
        loc, SILType::getPrimitiveObjectType(scalarTy));
    auto scalarBufInitAccess = builder.createBeginAccess(
        loc, scalarBuf, SILAccessKind::Init, SILAccessEnforcement::Static,
        /*noNestedConflict*/ true, /*fromBuiltin*/ false);
    convertIntToIndirectExpressible(scalar, scalarTyDecl, scalarBufInitAccess,
                                    loc, builder, context);
    builder.createEndAccess(loc, scalarBufInitAccess, /*aborted*/ false);
    // Prepare to call `VectorNumeric.init(_:)` on the scalar.
    auto *vecNumProto = context.getVectorNumericProtocol();
    DeclName initName(astCtx, DeclBaseName::createConstructor(),
                      {Identifier()});
    auto *reqr =
        cast<ConstructorDecl>(vecNumProto->lookupDirect(initName).front());
    SILDeclRef reqrRef(reqr, SILDeclRef::Kind::Allocator);
    auto silInitTy = context.getTypeConverter().getConstantType(reqrRef);
    LLVM_DEBUG({
      auto &s = getADDebugStream();
      s << "VectorNumeric.init(_:) definition: ";
      reqr->print(s);
      s << '\n' << reqrRef << " : " << silInitTy << '\n';
    });

    // Get the target type's conformance to `VectorNumeric`.
    llvm::SmallVector<ProtocolConformance*, 1> confs;
    targetTypeDecl->lookupConformance(module.getSwiftModule(), vecNumProto,
                                      confs);
    assert(confs.size() == 1);
    auto *genericConf = confs[0];
    auto typeSubMap = type->getContextSubstitutionMap(
        module.getSwiftModule(), genericConf->getDeclContext());
    auto *conf = astCtx.getSpecializedConformance(type, genericConf,
                                                  typeSubMap);
    LLVM_DEBUG({
      auto &s = getADDebugStream() << "Found conformance ";
      conf->dump(s);
      s << '\n';
    });
    ProtocolConformanceRef confRef(conf);

    // Make sure the witness table is linked.
    module.lookUpFunctionInWitnessTable(confRef, reqrRef);
    // $4 = witness_method ...
    auto initFnRef =
        builder.createWitnessMethod(loc, type, confRef, reqrRef, silInitTy);
    // debugDump(builder.getModule());
    LLVM_DEBUG({
      auto &s = getADDebugStream();
      s << "Real vector initialization function: " << *initFnRef << '\n';
    });
    auto initSubMap =
        SubstitutionMap::getProtocolSubstitutions(vecNumProto, type, confRef);
    LLVM_DEBUG({
      auto &s = getADDebugStream()
          << "Substitution map for real vector " << type << ": ";
      initSubMap.dump(s);
      s << '\n';
    });
    // %type = metatype $<scalar type>.Type
    auto metatypeTy = SILType::getPrimitiveObjectType(
        CanMetatypeType::get(type, MetatypeRepresentation::Thick));
    auto *metatype = builder.createMetatype(loc, metatypeTy);
    auto scalarReadAccess = builder.createBeginAccess(
        loc, scalarBuf, SILAccessKind::Read, SILAccessEnforcement::Static,
        /*noNestedConflict*/ true, /*fromBuiltin*/ false);
    // %5 = apply %4(%3, %2, %type)
    builder.createApply(loc, initFnRef, initSubMap,
                        {seedBufAccess, scalarReadAccess, metatype},
                        /*isNonThrowing*/ false);
    builder.createEndAccess(loc, scalarReadAccess, /*aborted*/ false);
    // dealloc_stack %0 : $*<scalar type>
    builder.createDeallocStack(loc, scalarBuf);
    return;
  }
  // Struct gets member-wise initialized.
  case TangentSpace::Kind::ProductStruct: {
    auto *decl = tangentSpace->getProductStruct();
    SmallVector<SILValue, 8> elements;
    for (auto *field : decl->getStoredProperties()) {
      auto *eltAddr =
          builder.createStructElementAddr(loc, seedBufAccess, field);
      createScalarValueIndirect(scalar, eltAddr->getType().getASTType(),
                                eltAddr, loc, builder, context);
    }
    return;
  }
  // Tuple gets member-wise initialized.
  case TangentSpace::Kind::ProductTuple: {
    auto tupleType = tangentSpace->getProductTuple();
    SmallVector<SILValue, 8> elements;
    for (auto i : indices(tupleType->getElementTypes())) {
      auto *eltAddr = builder.createTupleElementAddr(loc, seedBufAccess, i);
      createScalarValueIndirect(scalar, eltAddr->getType().getASTType(),
                                eltAddr, loc, builder, context);
    }
    return;
  }
  case TangentSpace::Kind::Sum: {
    llvm_unreachable("Differentiating sum types is not supported yet");
  }
  }
}

/// Creates and returns a scalar value in the specified type. The specified type
/// must be a loadable type and must have a tangent space.
static SILValue createScalarValueDirect(intmax_t scalar, CanType type,
                                        SILLocation loc,
                                        SILBuilder &builder,
                                        ADContext &context) {
  LLVM_DEBUG(getADDebugStream() << "Creating a scalar value " << scalar <<
        " of type " << type << '\n');
  auto &astCtx = context.getASTContext();
  auto *swiftMod = context.getModule().getSwiftModule();
  auto tangentSpace = astCtx.getTangentSpace(type, swiftMod);
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case TangentSpace::Kind::BuiltinRealScalar:
    return createBuiltinFPScalar(scalar, type, loc, builder);
  case TangentSpace::Kind::RealScalar:
  case TangentSpace::Kind::RealVector: {
    // Otherwise, initiailize the value through protocol calls.
    auto *buffer =
        builder.createAllocStack(loc, SILType::getPrimitiveObjectType(type));
    auto *access =
        builder.createBeginAccess(loc, buffer, SILAccessKind::Init,
                                  SILAccessEnforcement::Static,
                                  /*noNestedConflict*/ true,
                                  /*fromBuiltin*/ false);
    createScalarValueIndirect(scalar, type, access, loc, builder, context);
    builder.createEndAccess(loc, access, /*aborted*/ false);
    access = builder.createBeginAccess(loc, buffer, SILAccessKind::Read,
                                       SILAccessEnforcement::Static,
                                       /*noNestedConflict*/ true,
                                       /*fromBuiltin*/ false);
    auto *loadedValue = builder.createLoad(
        loc, access, getBufferLOQ(type, builder.getFunction()));
    builder.createEndAccess(loc, access, /*aborted*/ false);
    builder.createDeallocStack(loc, buffer);
    return loadedValue;
  }
  case TangentSpace::Kind::ProductStruct: {
    auto *structDecl = tangentSpace->getProductStruct();
    SmallVector<SILValue, 8> elements;
    for (auto *field : structDecl->getStoredProperties()) {
      auto eltVal = createScalarValueDirect(
          scalar, field->getType()->getCanonicalType(), loc, builder, context);
      elements.push_back(eltVal);
    }
    return builder.createStruct(
        loc, SILType::getPrimitiveObjectType(type), elements);
  }
  case TangentSpace::Kind::ProductTuple: {
    auto tupleType = tangentSpace->getProductTuple();
    SmallVector<SILValue, 8> elements;
    for (auto eltType : tupleType->getElementTypes()) {
      auto eltVal = createScalarValueDirect(
          scalar, eltType->getCanonicalType(), loc, builder, context);
      elements.push_back(eltVal);
    }
    return builder.createTuple(
        loc, SILType::getPrimitiveObjectType(type), elements);
  }
  case TangentSpace::Kind::Sum: {
    llvm_unreachable("Differentiating sum types is not supported yet");
  }
  }
}

//===----------------------------------------------------------------------===//
// PrimalGen - generates primal functions for each differentiation task in
// the SIL module.
//===----------------------------------------------------------------------===//

namespace {
class PrimalGen {
  friend class PrimalGenCloner;

private:
  /// The global AD context.
  ADContext &context;

  /// A worklist of primal synthesis items, each of which specifies a the
  /// original function, the target primal function, AD indices, and the primal
  /// value struct.
  SmallVector<FunctionSynthesisItem, 16> worklist;

  /// Flag indicating there was an error during primal generation.
  bool errorOccurred = false;

public:
  explicit PrimalGen(ADContext &context) : context(context) {}

  /// Performs primal synthesis for all differentiation tasks. Returns true if
  /// any error occurs.
  bool run();

protected:
  /// Get the primal, and lazily schedule a task to synthesize its body.
  SILFunction *lookUpPrimalAndMaybeScheduleSynthesis(DifferentiationTask *task);

private:
  /// Processes a synthesis item. Returns true if any error occurs.
  bool performSynthesis(FunctionSynthesisItem task);
};
} // end anonymous namespace

StructDecl *
ADContext::createPrimalValueStruct(const DifferentiationTask *task) {
  auto *function = task->getOriginal();
  assert(&function->getModule() == &module &&
         "The function must be in the same module");
  auto &file = getPrimalValueDeclContainer();
  // Create a `<fn_name>__Type` struct.
  std::string pvStructName = "AD__" + function->getName().str() + "__Type__" +
                             mangleADIndices(task->getIndices());
  auto structId = astCtx.getIdentifier(pvStructName);
  SourceLoc loc = function->getLocation().getSourceLoc();
  auto pvStruct =
      new (astCtx) StructDecl(/*StructLoc*/ loc, /*Name*/ structId,
                              /*NameLoc*/ loc, /*Inherited*/ {},
                              /*GenericParams*/ nullptr, // to be set later
                              /*DC*/ &file);
  pvStruct->computeType();
  if (auto *dc = function->getDeclContext()) {
    if (auto *afd = dyn_cast<AbstractFunctionDecl>(dc)) {
      auto funcAccess = afd->getEffectiveAccess();
      if (funcAccess >= AccessLevel::Public) {
        pvStruct->getAttrs().add(
            new (astCtx) FixedLayoutAttr(/*implicit*/ true));
      }
      pvStruct->setAccess(funcAccess);
    }
  } else {
    pvStruct->setAccess(AccessLevel::Internal);
    pvStruct->getAttrs().add(
        new (astCtx) UsableFromInlineAttr(/*implicit*/ true));
  }
  pvStruct->setGenericEnvironment(task->getOriginal()->getGenericEnvironment());
  file.addVisibleDecl(pvStruct);
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Primal value struct created for function " << function->getName()
      << '\n';
    pvStruct->print(s);
    s << '\n';
  });
  return pvStruct;
}

/// Given an parameter argument (not indirect result) and some differentiation
/// indices, figure out whether the parent function is being differentiated with
/// respect to this parameter, according to the indices.
static bool isDifferentiationParameter(SILArgument *argument,
                                       llvm::SmallBitVector indices) {
  if (!argument) return false;
  auto *function = argument->getFunction();
  auto paramArgs = function->getArgumentsWithoutIndirectResults();
  for (unsigned i : indices.set_bits())
    if (paramArgs[i] == argument)
      return true;
  return false;
}

/// For a nested function call whose result tuple is active on the
/// differentiation path, compute the set of minimal indices for differentiating
/// this function as required by the data flow.
static void collectMinimalIndicesForFunctionCall(
    ApplyInst *ai, const SILAutoDiffIndices &parentIndices,
    const DifferentiableActivityInfo &activityInfo,
    SmallVectorImpl<unsigned> &paramIndices,
    SmallVectorImpl<unsigned> &resultIndices) {
  // Make sure the function call result is active.
  assert(activityInfo.isActive(ai, parentIndices));
  auto fnTy = ai->getCallee()->getType().castTo<SILFunctionType>();
  SILFunctionConventions convs(fnTy, ai->getModule());
  auto arguments = ai->getArgumentOperands();
  // Parameter indices are indices (in the type signature) of parameter
  // arguments that are varied or are arguments.
  unsigned currentParamIdx = 0;
  for (auto applyArg : ai->getArgumentsWithoutIndirectResults()) {
    if (activityInfo.isVaried(applyArg, parentIndices.parameters) ||
        isDifferentiationParameter(dyn_cast<SILArgument>(applyArg),
                                   parentIndices.parameters))
      paramIndices.push_back(currentParamIdx);
    ++currentParamIdx;
  }
  // Result indices are indices (in the type signature) of results that are
  // useful.
  //
  // If the function returns only one result, then we just see if that is
  // useful.
  if (fnTy->getNumDirectFormalResults() == 1) {
    if (activityInfo.isUseful(ai, parentIndices.source))
      resultIndices.push_back(0);
    return;
  }
  // If the function returns more than 1 results, the return type is a tuple. We
  // need to find all `tuple_extract`s on that tuple, and determine if each
  // found extracted element is useful.
  // Collect direct results being retrieved using `tuple_extract`.
  SmallVector<SILValue, 8> usedDirectResults(convs.getNumDirectSILResults());
  for (auto *use : ai->getUses())
    if (auto *tei = dyn_cast<TupleExtractInst>(use->getUser()))
      usedDirectResults[tei->getFieldNo()] = tei;
  // Add differentiation indices based on activity analysis.
  unsigned dirResIdx = 0;
  unsigned indResIdx = convs.getSILArgIndexOfFirstIndirectResult();
  for (auto &resAndIdx : enumerate(convs.getResults())) {
    auto &res = resAndIdx.value();
    unsigned idx = resAndIdx.index();
    if (res.isFormalDirect()) {
      if (auto dirRes = usedDirectResults[dirResIdx])
        if (dirRes && activityInfo.isUseful(dirRes, parentIndices.source))
          resultIndices.push_back(idx);
      ++dirResIdx;
    } else {
      if (activityInfo.isUseful(arguments[indResIdx].get(),
                                parentIndices.source))
        resultIndices.push_back(idx);
      ++indResIdx;
    }
  }
}

/// If the original function in the differentiation task has more than one basic
/// blocks, emit a "control flow unsupported" error at appropriate source
/// locations. Returns true if error is emitted.
static bool diagnoseUnsupportedControlFlow(ADContext &context,
                                           DifferentiationTask *task) {
  if (task->getOriginal()->getBlocks().size() <= 1)
    return false;
  // Find any control flow node and diagnose.
  for (auto &bb : *task->getOriginal()) {
    auto *term = bb.getTerminator();
    if (term->isBranch()) {
      context.emitNondifferentiabilityError(
          term, task, diag::autodiff_control_flow_not_supported);
      return true;
    }
  }
  return false;
}

/// Given the original function and a call to the corresponding primal function,
/// collect primal values and original results returned by the primal call.
static void collectPrimalValuesAndOriginalResults(
    SILFunctionType *origFnTy, ApplyInst *primalCall,
    ArrayRef<SILValue> extractedDirRes, SmallVectorImpl<SILValue> &primVals,
    SmallVectorImpl<SILValue> &origRes) {
  SmallVector<SILValue, 8> allResults;
  collectAllActualResultsInTypeOrder(primalCall, extractedDirRes,
                                     primalCall->getIndirectSILResults(),
                                     allResults);
  ArrayRef<SILValue> allResultsRef(allResults);
  auto numOrigRes = origFnTy->getNumResults();
  auto primValsRef = allResultsRef.drop_back(numOrigRes);
  auto origResRef = allResultsRef.take_back(numOrigRes);
  primVals.append(primValsRef.begin(), primValsRef.end());
  origRes.append(origResRef.begin(), origResRef.end());
}

namespace {
class PrimalGenCloner final : public SILClonerWithScopes<PrimalGenCloner> {
private:
  /// A reference to this function synthesis item.
  const FunctionSynthesisItem &synthesis;

  /// Info from activity analysis on the original function.
  const DifferentiableActivityInfo &activityInfo;

  /// The dominator tree of the original function.
  /// const DominanceInfo &domInfo;

  /// The postdominator tree of the original function.
  const PostDominanceInfo &postDomInfo;

  bool errorOccurred = false;

  // To be used for control flow support.
  // const SILLoopInfo &loopInfo;

  /// Global PrimalGen.
  PrimalGen &primalGen;

  /// Global context.
  ADContext &getContext() { return primalGen.context; }

  /// The non-control-dependent static primal values. This will be used to
  /// gather all primal values in instruction order as they are created and
  /// added to the primal value struct. These will then form a primal value
  /// struct value along with tapes, to be returned as the first result of the
  /// primal.
  SmallVector<SILValue, 8> staticPrimalValues;

  ASTContext &getASTContext() const {
    return synthesis.target->getASTContext();
  }

  DifferentiationTask *getDifferentiationTask() const { return synthesis.task; }

  SILFunction *getOriginal() const { return synthesis.original; }
  SILFunction *getPrimal() const { return synthesis.target; }

  PrimalInfo &getPrimalInfo() const {
    return *getDifferentiationTask()->getPrimalInfo();
  }

protected:
  /// Determine the kind of the given primal value. It is a BB argument, a
  /// cost-free conversion like `struct_extract`, a value to be recomputed in
  /// the adjoint, an control-independent checkpoint, or a tape checkpoint.
  PrimalValueKind classifyPrimalValue(SILInstruction *inst) {
    assert(inst->getFunction() == getOriginal());
    auto *entry = getOriginal()->getEntryBlock();
    auto *bb = inst->getParentBlock();
    switch (inst->getKind()) {
    case SILInstructionKind::IntegerLiteralInst:
    case SILInstructionKind::FloatLiteralInst:
    case SILInstructionKind::StringLiteralInst:
    case SILInstructionKind::TupleInst:
    case SILInstructionKind::StructInst:
    case SILInstructionKind::TupleExtractInst:
    case SILInstructionKind::TupleElementAddrInst:
    case SILInstructionKind::StructExtractInst:
    case SILInstructionKind::StructElementAddrInst:
    case SILInstructionKind::EnumInst:
    case SILInstructionKind::FunctionRefInst:
    case SILInstructionKind::ConvertFunctionInst:
    case SILInstructionKind::ThinToThickFunctionInst:
    case SILInstructionKind::PartialApplyInst:
    case SILInstructionKind::GlobalValueInst:
    case SILInstructionKind::KeyPathInst:
    case SILInstructionKind::MetatypeInst:
    case SILInstructionKind::GradientInst:
      return PrimalValueKind::Conversion;
    case SILInstructionKind::BuiltinInst: {
      auto *bi = cast<BuiltinInst>(inst);
      auto kind = bi->getBuiltinKind();
      if (!kind)
        return PrimalValueKind::Conversion;
      switch (*kind) {
      case BuiltinValueKind::FAdd:
      case BuiltinValueKind::FSub:
      case BuiltinValueKind::FNeg:
      case BuiltinValueKind::FMul:
      case BuiltinValueKind::FDiv:
        goto checkpoint;
      default:
        return PrimalValueKind::Conversion;
      }
    }
    default:
    checkpoint:
      return postDomInfo.dominates(bb, entry)
                 ? PrimalValueKind::StaticCheckpoint
                 : PrimalValueKind::TapeCheckpoint;
    }
  }

public:
  explicit PrimalGenCloner(const FunctionSynthesisItem &synthesis,
                           const DifferentiableActivityInfo &activityInfo,
                           const DominanceInfo &domInfo,
                           const PostDominanceInfo &pdomInfo,
                           const SILLoopInfo &loopInfo, PrimalGen &primalGen,
                           ADContext &context)
      : SILClonerWithScopes(*synthesis.target), synthesis(synthesis),
        activityInfo(activityInfo), /*domInfo(domInfo),*/ postDomInfo(pdomInfo),
        /*loopInfo(loopInfo),*/
        primalGen(primalGen) {}

  void postProcess(SILInstruction *orig, SILInstruction *cloned) {
    if (errorOccurred)
      return;
    SILClonerWithScopes::postProcess(orig, cloned);
    auto &indices = getDifferentiationTask()->getIndices();
    if (!activityInfo.getActivity(orig, indices)
            .contains(ActivityFlags::Active))
      return;
    switch (classifyPrimalValue(orig)) {
    case PrimalValueKind::Conversion:
      break;
    case PrimalValueKind::ToRematerialize:
      break;
    case PrimalValueKind::TapeCheckpoint:
      // FIXME: Get or create typed tape, and emit push-to-tape builtin.
      llvm_unreachable("Unhandled tape checkpoint");
    case PrimalValueKind::StaticCheckpoint:
      // Checkpointing active function applications is handled within
      // `PrimalGenCloner::visitApplyInst`.
      if (isa<ApplyInst>(orig))
        break;
      for (auto resultPair :
           llvm::zip(orig->getResults(), cloned->getResults())) {
        LLVM_DEBUG(getADDebugStream()
                   << "Found static checkpoint " << *cloned << '\n');
        SILValue origRes, clonedRes;
        std::tie(origRes, clonedRes) = resultPair;
        getPrimalInfo().addStaticPrimalValueDecl(origRes);
        staticPrimalValues.push_back(clonedRes);
      }
      break;
    }
    LLVM_DEBUG(getADDebugStream() << "Post-processing the clone of \n"
                                  << *orig << "as\n"
                                  << *cloned << '\n');
  }

  // Run primal generation. Returns true on error.
  bool run() {
    auto *original = getOriginal();
    LLVM_DEBUG(getADDebugStream()
               << "Cloning original @" << getOriginal()->getName()
               << " to primal @" << synthesis.target->getName() << '\n');
    // Create entry BB and arguments.
    auto *entry = getPrimal()->createBasicBlock();
    // Map the original's arguments to the new function's arguments.
    SmallVector<SILValue, 8> entryArgs;
    for (auto *origArg : original->getArguments()) {
      auto *newArg = entry->createFunctionArgument(origArg->getType());
      entryArgs.push_back(newArg);
    }
    // Clone.
    cloneFunctionBody(original, entry, entryArgs);
    // If errors occurred, back out.
    if (errorOccurred)
      return true;
    auto *origExit = &*original->findReturnBB();
    auto *exit = BBMap.lookup(origExit);
    assert(exit->getParent() == getPrimal());
    // Get the original's return value's corresponsing value in the primal.
    auto *origRetInst = cast<ReturnInst>(origExit->getTerminator());
    auto origRetVal = origRetInst->getOperand();
    auto origResInPrimal = getOpValue(origRetVal);
    // Create a primal value struct containing all static primal values and
    // tapes.
    auto loc = getPrimal()->getLocation();
    auto structTy =
        getPrimalInfo().getPrimalValueStruct()->getDeclaredInterfaceType();
    auto &builder = getBuilder();
    builder.setInsertionPoint(exit);
    auto structLoweredTy =
        getContext().getTypeConverter().getLoweredType(structTy);
    auto primValsVal =
        builder.createStruct(loc, structLoweredTy, staticPrimalValues);
    // FIXME: Handle tapes.
    //
    // If the original result was a tuple, return a tuple of all elements in the
    // original result tuple and the primal value struct value.
    auto origResTy = origResInPrimal->getType();
    SILValue retVal;
    if (auto origResTupTy = origResTy.getAs<TupleType>()) {
      auto eltTypes = origResTupTy.getElementTypes();
      auto numElts = eltTypes.size();
      SmallVector<SILValue, 8> elts;
      elts.reserve(numElts + 1);
      elts.push_back(primValsVal);
      for (unsigned i : range(numElts))
        elts.push_back(builder.emitTupleExtract(loc, origResInPrimal, i));
      retVal = builder.createTuple(loc, elts);
    }
    // If the original result was a single value, return a tuple of the primal
    // value struct value and the original result.
    else {
      retVal = builder.createTuple(loc, {primValsVal, origResInPrimal});
    }
    builder.createReturn(loc, retVal);
    LLVM_DEBUG({
      auto &s = getADDebugStream()
                << "Primal values in $"
                << getPrimalInfo().getPrimalValueStruct()->getName() << ":\n";
      for (auto *var : getPrimalInfo().getPrimalValueStruct()->getMembers()) {
        var->dump(s);
        s << '\n';
      }
    });
    LLVM_DEBUG(getADDebugStream() << "Finished PrimalGen for function "
                                  << original->getName() << ":\n"
                                  << *getPrimal());
    debugDump(*getPrimal());
    return errorOccurred;
  }

  /// General visitor for all instruction. If there is any error emitted by
  /// previous visits, bail out.
  void visit(SILInstruction *inst) {
    if (errorOccurred)
      return;
    SILClonerWithScopes::visit(inst);
  }
  
  void visitReturnInst(ReturnInst *ri) {
    // The original return is not to be cloned.
    return;
  }
  
  void visitReleaseValueInst(ReleaseValueInst *rvi) {
    // Checkpoints are not to be released.
    if (auto *inst = rvi->getOperand()->getDefiningInstruction())
      if (classifyPrimalValue(inst) == PrimalValueKind::StaticCheckpoint)
        return;
    SILClonerWithScopes::visitReleaseValueInst(rvi);
  }

  /// Handle the primal transformation of an `apply` instruction. We do not
  /// always transform `apply`. When we do, we do not just blindly differentiate
  /// from all results w.r.t. all parameters. Instead, we let activity analysis
  /// decide whether to transform and what differentiation indices to use.
  void visitApplyInst(ApplyInst *ai) {
    // Special handling logic only applies when `apply` is active. If not, just
    // do standard cloning.
    if (!activityInfo.isActive(ai, synthesis.indices)) {
      LLVM_DEBUG(getADDebugStream() << "Not active:\n" << *ai << '\n');
      SILClonerWithScopes::visitApplyInst(ai);
      return;
    }
    // This instruction is active, replace it with a call to its primal.
    // Get the indices required for differentiating this function.
    LLVM_DEBUG(getADDebugStream() << "Primal-transforming:\n" << *ai << '\n');
    SmallVector<unsigned, 8> activeParamIndices;
    SmallVector<unsigned, 8> activeResultIndices;
    collectMinimalIndicesForFunctionCall(ai, synthesis.indices, activityInfo,
                                         activeParamIndices,
                                         activeResultIndices);
    assert(!activeParamIndices.empty() && "Parameter indices cannot be empty");
    assert(!activeResultIndices.empty() && "Result indices cannot be empty");
    LLVM_DEBUG(auto &s = getADDebugStream() << "Active indices: params={";
               interleave(activeParamIndices.begin(), activeParamIndices.end(),
                          [&s](unsigned i) { s << i; }, [&s] { s << ", "; });
               s << "}, results={"; interleave(
                   activeResultIndices.begin(), activeResultIndices.end(),
                   [&s](unsigned i) { s << i; }, [&s] { s << ", "; });
               s << "}\n";);
    auto &context = getContext();
    // FIXME: If there are multiple active results, we don't support it yet.
    // To support this, we need to emit a primal call for each active result.
    if (activeResultIndices.size() > 1) {
      context.emitNondifferentiabilityError(ai, synthesis.task);
      errorOccurred = true;
      return;
    }
    // Form expected indices by assuming there's only one result.
    SILAutoDiffIndices indices(activeResultIndices.front(), activeParamIndices);
    // Retrieve the original function being called before conversion.
    auto calleeOrigin = ai->getCalleeOrigin();
    auto *calleeOriginFnRef = dyn_cast<FunctionRefInst>(calleeOrigin);
    // If callee does not trace back to a `function_ref`, it is an opaque
    // function. Emit a "not differentiable" diagnostic here.
    // FIXME: Handle `partial_apply`.
    if (!calleeOriginFnRef) {
      context.emitNondifferentiabilityError(ai, synthesis.task);
      errorOccurred = true;
      return;
    }
    // Find or register a differentiation task for this function.
    auto *newTask = context.lookUpOrRegisterDifferentiationTask(
        calleeOriginFnRef->getReferencedFunction(), indices,
        /*invoker*/ {ai, synthesis.task});
    // Associate the new differentiation task with this `apply` instruction, so
    // that adjoint synthesis can pick it up.
    getDifferentiationTask()->getAssociatedTasks().insert({ai, newTask});
    // Get the primal function from the task. If the task was newly created,
    // then we need to schedule a synthesis item for the primal.
    auto *primalFn = primalGen.lookUpPrimalAndMaybeScheduleSynthesis(newTask);
    // Now that we have the primal, get ready to call it.
    // But before calling it, we need to convert the primal function like how
    // the original function is converted.
    SILBuilder &builder = getBuilder();
    // %0 = function_ref <primal>
    auto *primal =
        builder.createFunctionRef(ai->getCallee().getLoc(), primalFn);
    // %1 = ... convert %0 ...
    auto convertedPrimal = reapplyFunctionConversion(
        primal, calleeOrigin, ai->getCallee(), builder, primal->getLoc(),
        [&](SILValue v) { return getOpValue(v); });
    // Call the primal function using the original parameters.
    SmallVector<SILValue, 8> newArgs;
    auto primalFnTy = primalFn->getLoweredFunctionType();
    auto numPrimalParams = primalFnTy->getNumParameters();
    auto numPrimalIndRes = primalFnTy->getNumIndirectFormalResults();
    auto numArgs = numPrimalParams + numPrimalIndRes;
    newArgs.reserve(numArgs);
    // Handle indirect primal values.
    if (newArgs.size() > ai->getNumArguments()) {
      llvm_unreachable("FIXME: Some primal values are indirect");
    }
    // Collect substituted arguments.
    LLVM_DEBUG(getADDebugStream() << "Retrieving original arguments:\n");
    for (auto origArg : ai->getArguments()) {
      LLVM_DEBUG(getADDebugStream() << "Original argument " << origArg);
      newArgs.push_back(getOpValue(origArg));
    }
    // %2 = apply %1(...)
    auto *primalCall = builder.createApply(ai->getLoc(), convertedPrimal,
                                           ai->getSubstitutionMap(), newArgs,
                                           ai->isNonThrowing());
    LLVM_DEBUG(getADDebugStream()
               << "Applied primal function\n" << *primalCall);
    // After applying the primal, we need to handle the primal's direct results.
    // These results include direct primal values and direct original results.
    SmallVector<SILValue, 8> primVals, origResults, allDirResults;
    extractAllElements(primalCall, builder, allDirResults);
    collectPrimalValuesAndOriginalResults(calleeOriginFnRef->getFunctionType(),
                                          primalCall, allDirResults, primVals,
                                          origResults);
    LLVM_DEBUG({
      auto &s = getADDebugStream();
      s << "All direct results returned by the primal function: \n";
      llvm::for_each(allDirResults, [&](SILValue v) { s << v; });
      s << "Primal values returned by the primal function: \n";
      llvm::for_each(primVals, [&](SILValue v) { s << v; });
      s << "Original results returned by the primal function: \n";
      llvm::for_each(origResults, [&](SILValue v) { s << v; });
    });

    // Get original direct results for cloning.
    SmallVector<SILValue, 8> origDirResults;
    for (auto origRes : origResults)
      if (origRes->getType().isObject())
        origDirResults.push_back(origRes);
    auto origDirResultFromPrimal =
        joinElements(origDirResults, builder, primalCall->getLoc());
    // Store the original result from primal to the value map.
    ValueMap.insert({ai, origDirResultFromPrimal});

    // FIXME: Handle indirect passing. One possible way is to scan the entire
    // data flow to determine whether the primal value struct should be
    // indirect. Then use a flag to determine whether we'll use SSA operations
    // or address operations to perform checkpointing.

    // Checkpoint nested primal values as a tuple.
    if (!primVals.empty()) {
      auto nestedPrimValDeclTy =
          joinElementTypesFromValues(primVals, getASTContext());
      getPrimalInfo().addNestedStaticPrimalValueDecl(ai, nestedPrimValDeclTy);
      auto primValAggr = joinElements(primVals, builder, primalCall->getLoc());
      staticPrimalValues.push_back(primValAggr);
    }

    // Checkpoint original results as a tuple.
    getPrimalInfo().addStaticPrimalValueDecl(ai);
    auto origResAggr = joinElements(origResults, builder, primalCall->getLoc());
    staticPrimalValues.push_back(origResAggr);

    // Some instructions that produce the callee may have been cloned.
    // If the original callee did not have any users beyond this `apply`,
    // recursively kill the cloned callee.
    if (auto *origCallee = cast_or_null<SingleValueInstruction>(
            ai->getCallee()->getDefiningInstruction()))
      if (origCallee->hasOneUse())
        recursivelyDeleteTriviallyDeadInstructions(
            getOpValue(origCallee)->getDefiningInstruction());
  }

  /// Handle the primal transformation of a `gradient` instruction. The only
  /// case that will incur visiting `gradient` instruction is nested
  /// differentiation, which is not supported yet.
  void visitGradientInst(GradientInst *gi) {
    getContext().emitNondifferentiabilityError(
        gi, getDifferentiationTask(), diag::autodiff_nested_not_supported);
    errorOccurred = true;
  }

  /// Primal has qualified ownership. We assign store ownership qualifier while
  /// cloning the `store` instruction.
  void visitStoreInst(StoreInst *si) {

    auto destTy = si->getDest()->getType().getASTType();
    auto loc = remapLocation(si->getLoc());
    auto soq = getBufferSOQ(getOpASTType(destTy), *getPrimal());
    getBuilder().createStore(loc, getOpValue(si->getSrc()),
                             getOpValue(si->getDest()), soq);
  }

  /// Primal has qualified ownership. We assign load ownership qualified while
  /// cloning the `load` instruction.
  void visitLoadInst(LoadInst *li) {
    auto srcTy = li->getOperand()->getType().getASTType();
    auto loc = remapLocation(li->getLoc());
    auto loq = getBufferLOQ(getOpASTType(srcTy), *getPrimal());
    ValueMap.insert(
        {li, getBuilder().createLoad(loc, getOpValue(li->getOperand()), loq)});
  }
};
} // end anonymous namespace

bool PrimalGen::performSynthesis(FunctionSynthesisItem item) {
  LLVM_DEBUG(getADDebugStream() << "Performing primal synthesis for original"
             << item.original->getName() << " and its corresponding adjoint "
             << item.target->getName() << '\n');
  // FIXME: If the original function has multiple basic blocks, bail out since
  // AD does not support control flow yet.
  if (diagnoseUnsupportedControlFlow(context, item.task)) {
    errorOccurred = true;
    return true;
  }
  // FIXME: Support generics.
  auto *original = item.original;
  if (original->getLoweredFunctionType()->getGenericSignature()) {
    context.diagnose(original->getLocation().getSourceLoc(),
                     diag::autodiff_function_generic_functions_unsupported);
    context.diagnose(original->getLocation().getSourceLoc(),
                     diag::autodiff_function_not_differentiable);
    errorOccurred = true;
    return true;
  }
  // Compute necessary analyses on the original function.
  auto &passManager = context.getPassManager();
  auto *activityAnalysis =
      passManager.getAnalysis<DifferentiableActivityAnalysis>();
  auto *domAnalysis = passManager.getAnalysis<DominanceAnalysis>();
  auto *pdomAnalysis = passManager.getAnalysis<PostDominanceAnalysis>();
  auto *loopAnalysis = passManager.getAnalysis<SILLoopAnalysis>();
  auto &activityInfo = *activityAnalysis->get(item.original);
  auto &domInfo = *domAnalysis->get(item.original);
  auto &pdomInfo = *pdomAnalysis->get(item.original);
  auto &loopInfo = *loopAnalysis->get(item.original);
  // Canonicalize the orignal function's control flow.
  ControlFlowCanonicalization(*item.original, domInfo, loopInfo).run();
  // For debugging, dump the original function's activity analysis.
  LLVM_DEBUG(dumpActivityInfo(*item.original, item.task->getIndices(),
                              activityInfo, getADDebugStream()));
  // Synthesize primal.
  PrimalGenCloner cloner(item, activityInfo, domInfo, pdomInfo, loopInfo, *this,
                         context);
  return cloner.run();
}

SILFunction *
PrimalGen::lookUpPrimalAndMaybeScheduleSynthesis(DifferentiationTask *task) {
  auto *primal = task->getPrimal();
  assert(primal && "requesting primal from task without primal");

  if (task->getPrimalSynthesisState() == FunctionSynthesisState::Needed) {
    FunctionSynthesisItem synthesis{task->getOriginal(), primal,
                                    task->getIndices(), task};
    worklist.push_back(synthesis);
    task->setPrimalSynthesisState(FunctionSynthesisState::Pending);
  }

  return primal;
}

bool PrimalGen::run() {
  // Push everything to the list of primal synthesis items.
  for (auto &task : context.getDifferentiationTasks())
    lookUpPrimalAndMaybeScheduleSynthesis(task.get());
  // Process each item until empty.
  while (!worklist.empty()) {
    auto synthesis = worklist.back();
    worklist.pop_back();
    errorOccurred |= performSynthesis(synthesis);
    synthesis.task->getPrimalInfo()->computePrimalValueStructType();
    synthesis.task->setPrimalSynthesisState(FunctionSynthesisState::Done);
  }
  return errorOccurred;
}

//===----------------------------------------------------------------------===//
// AdjointGen - generates an adjoint function for each differentiation task
// in a SIL module.
//===----------------------------------------------------------------------===//

/// The adjoint generator for all gradient functions. Executed after PrimalGen.
namespace {

class AdjointGen {
  friend class AdjointEmitter;

private:
  /// The global AD context.
  ADContext &context;

  /// Work list of synthesis items.
  SmallVector<FunctionSynthesisItem, 16> worklist;

  /// Flag indicating whether an error has occurred.
  bool errorOccurred = false;

public:
  explicit AdjointGen(ADContext &context) : context(context) {}

  /// Performs adjoint generation for all differentiation tasks. Returns true if
  /// any error occurs.
  bool run();

private:
  /// Do the synthesis item. Returns true if any error occurs.
  bool performSynthesis(FunctionSynthesisItem item);
};
} // end anonymous namespace

bool AdjointGen::run() {
  // Push everything to the worklist.
  for (auto &task : context.getDifferentiationTasks()) {
    if (task->getAdjointSynthesisState() == FunctionSynthesisState::Needed) {
      FunctionSynthesisItem synthesis{task->getOriginal(), task->getAdjoint(),
                                      task->getIndices(), task.get()};
      worklist.push_back(synthesis);
      task->setAdjointSynthesisState(FunctionSynthesisState::Pending);
    }
  }
  // Iterate over the worklist, look up existing adjoint. If an adjoint exists
  // for the task, do nothing. Otherwise, create a function and process it.
  while (!worklist.empty()) {
    auto synthesis = worklist.back();
    worklist.pop_back();
    errorOccurred |= performSynthesis(synthesis);
    synthesis.task->setAdjointSynthesisState(FunctionSynthesisState::Done);
  }
  return errorOccurred;
}

//===----------------------------------------------------------------------===//
// AdjointValue - a symbolic representation for adjoint values that allows
// for efficient differentiation of aggregates.
//===----------------------------------------------------------------------===//

namespace {

/// A symbolic adjoint value that is capable of representing zero gradient 0 and
/// 1, in addition to a materialized SILValue. This is expected to be passed
/// around by value in most cases, as it's two words long.
class AdjointValue {
public:
  enum Kind {
    /// An empty adjoint, i.e. zero. This case exists due to its special
    /// mathematical properties: `0 + x = x`. This is a guaranteed optimization
    /// when we combine a zero adjoint with another (e.g. differentiating a
    /// fanout).
    Zero,

    /// An aggregate of adjoint values.
    Aggregate,

    /// A materialized SIL value.
    Materialized,
  };

private:
  union Value {
    ArrayRef<AdjointValue> aggregate;
    SILValue materialized;
    Value(ArrayRef<AdjointValue> v) : aggregate(v) {}
    Value(SILValue v) : materialized(v) {}
    Value() {}
  };

  /// The kind of this adjoint value.
  Kind kind;

  /// The type of this value as if it were materialized as a SIL value.
  SILType type;

  /// The underlying value.
  Value value;

  AdjointValue(Kind kind, SILType type, Value value)
      : kind(kind), type(type), value(value) {
  }

public:
  AdjointValue(SILValue materializedValue)
      : AdjointValue(Kind::Materialized, materializedValue->getType(),
                     materializedValue) {}
  AdjointValue(SingleValueInstruction *svi) : AdjointValue(SILValue(svi)) {}

  Kind getKind() const { return kind; }
  SILType getType() const { return type; }
  CanType getSwiftType() const { return type.getASTType(); }

  NominalTypeDecl *getAnyNominal() const {
    return getSwiftType()->getAnyNominal();
  }

  bool isZero() const { return kind == Kind::Zero; }
  bool isAggregate() const { return kind == Kind::Aggregate; }
  bool isMaterialized() const { return kind == Kind::Materialized; }

  static AdjointValue getZero(SILType type) { return {Kind::Zero, type, {}}; }

  static AdjointValue getMaterialized(SILValue value) {
    assert(value);
    return {Kind::Materialized, value->getType(), value};
  }

  static AdjointValue getAggregate(SILType type,
                                   ArrayRef<AdjointValue> elements,
                                   llvm::BumpPtrAllocator &allocator) {
    // Tuple type elements must match the type of each adjoint value element.
    assert(isLegalAggregate(elements, type));
    AdjointValue *buf = reinterpret_cast<AdjointValue *>(allocator.Allocate(
        elements.size() * sizeof(AdjointValue), alignof(AdjointValue)));
    MutableArrayRef<AdjointValue> array(buf, elements.size());
    std::uninitialized_copy(elements.begin(), elements.end(), array.begin());
    return {Kind::Aggregate, type, array};
  }

  ArrayRef<AdjointValue> getAggregateElements() const {
    assert(isAggregate());
    return value.aggregate;
  }

  SILValue getMaterializedValue() const {
    assert(isMaterialized());
    return value.materialized;
  }

private:
  static bool isLegalAggregate(ArrayRef<AdjointValue> elements, SILType type) {
    if (auto *structDecl =
            dyn_cast_or_null<StructDecl>(type.getASTType()->getAnyNominal())) {
      // TODO: Check whether this struct is @_fixed_layout and ABI public.
      for (auto pair : llvm::zip(structDecl->getStoredProperties(), elements))
        if (!std::get<0>(pair)->getType()->getCanonicalType()
                ->isEqual(std::get<1>(pair).getSwiftType()))
          return false;
    } else if (auto tupleTy = type.getAs<TupleType>()) {
      for (auto pair : llvm::zip(tupleTy->getElementTypes(), elements))
        if (!std::get<0>(pair)->isEqual(std::get<1>(pair).getSwiftType()))
          return false;
    } else {
      llvm_unreachable("Not an aggregate type");
    }
    return true;
  }

public:
  void print(llvm::raw_ostream &s = llvm::outs()) const {
    switch (kind) {
    case Kind::Zero:
      s << "Zero";
      break;
    case Kind::Aggregate:
      s << "Aggregate<";
      if (auto *decl = dyn_cast_or_null<StructDecl>(
              type.getASTType()->getAnyNominal())) {
        s << "Struct>(";
        interleave(llvm::zip(decl->getStoredProperties(),
                             getAggregateElements()),
                   [&s](std::tuple<VarDecl *, AdjointValue> elt) {
                     s << std::get<0>(elt)->getName() << ": ";
                     std::get<1>(elt).print(s);
                   }, [&s] { s << ", "; });
      } else if (auto tupleType = type.getAs<TupleType>()) {
        s << "Tuple>(";
        interleave(getAggregateElements(),
                   [&s](AdjointValue elt) { elt.print(s); },
                   [&s] { s << ", "; });
      } else {
        llvm_unreachable("Invalid aggregate");
      }
      s << ')';
      break;
    case Kind::Materialized:
      s << "Materialized(" << getMaterializedValue() << ')';
      break;
    }
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const AdjointValue &adjVal) {
  adjVal.print(os);
  return os;
}

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// AdjointRematCloner and AdjointEmitter - visitors on the original function
// for adjoint code generation
//===----------------------------------------------------------------------===//

namespace {
/// `AdjointRematCloner` is used to clone non-checkpointed computation
/// that is to be rematerialized from the original function to the adjoint
/// function. It's called by `AdjointEmitter` during adjoint code generation.
/// During cloning, this cloner will record instructions that introduce data
/// flow scopes, such as `alloc_stack` and `begin_access`. `AdjointEmitter` will
/// emit the corresponding end-scope instructions.
class AdjointRematCloner final
    : public SILClonerWithScopes<AdjointRematCloner> {
  friend class AdjointEmitter;
      
  SmallVector<std::pair<SILInstruction *, SILInstruction *>, 8>
      scopeMarkingInstructions;

private:
  SILFunction &getOriginal() { return getBuilder().getFunction(); }

public:
  AdjointRematCloner(SILFunction &fn) : SILClonerWithScopes(fn) {}

  void insertValueMapping(SILValue valueInOriginal, SILValue valueInAdjoint) {
    auto insertion = ValueMap.insert({valueInOriginal, valueInAdjoint});
    assert(insertion.second && "The original value was mapped before");
  }

  SILValue lookUpValue(SILValue valueInOriginal) {
    auto lookup = ValueMap.find(valueInOriginal);
    if (lookup == ValueMap.end())
      return nullptr;
    return lookup->getSecond();
  }

  void setInsertionPointBeforeAnyTerminator(SILBasicBlock *bb) {
    if (!bb->empty())
      if (auto *ti = dyn_cast<TermInst>(&bb->back()))
        getBuilder().setInsertionPoint(ti);
    getBuilder().setInsertionPoint(bb);
  }
      
  void postProcess(SILInstruction *orig, SILInstruction *cloned) {
    // If the instruction is a scope-marking instruction, record it so that the
    // adjoint emitter can emit their end-scope counterparts.
    switch (orig->getKind()) {
    // FIXME: Handle other cases such as `begin_apply`.
    case swift::SILInstructionKind::BeginAccessInst:
    case swift::SILInstructionKind::AllocStackInst:
      scopeMarkingInstructions.push_back({orig, cloned});
      break;
    default:
      break;
    }
    SILClonerWithScopes::postProcess(orig, cloned);
  }
};

class AdjointEmitter final : public SILInstructionVisitor<AdjointEmitter> {
private:
  /// A reference to this function synthesis item.
  const FunctionSynthesisItem &synthesis;

  /// Info from activity analysis on the original function.
  const DifferentiableActivityInfo &activityInfo;

  /// The dominator tree of the original function.
  const DominanceInfo &domInfo;

  /// The post-dominator tree of the original function.
  const PostDominanceInfo &postDomInfo;

  // To be used for control flow support.
  // const SILLoopInfo &loopInfo;

  /// Global AdjointGen.
  AdjointGen &adjointGen;

  /// Rematerialization cloner.
  AdjointRematCloner rematCloner;

  /// Mapping from original values to their corresponding adjoint values.
  DenseMap<SILValue, AdjointValue> adjointMap;

  /// Mapping from original basic blocks to their corresponding adjoint basic
  /// blocks.
  DenseMap<SILBasicBlock *, SILBasicBlock *> adjointBBMap;

  /// Original parameters passed to the adjoint function, in the same order as
  /// they appear in the adjoint function.
  ArrayRef<SILArgument *> originalParametersInAdj;

  /// The primal value aggregate passed to the adjoint function.
  SILArgument *primalValueAggregateInAdj = nullptr;

  /// Original results passed to the adjoint function.
  ArrayRef<SILArgument *> originalResultsInAdj;

  /// The seed argument in the adjoint function.
  SILArgument *seed = nullptr;

  /// The main builder.
  SILBuilder builder;

  llvm::BumpPtrAllocator allocator;

  bool errorOccurred = false;

  ADContext &getContext() const { return adjointGen.context; }

  SILModule &getModule() const { return getContext().getModule(); }

  ASTContext &getASTContext() const {
    return synthesis.target->getASTContext();
  }

  PrimalInfo &getPrimalInfo() const {
    return *getDifferentiationTask()->getPrimalInfo();
  }

  SILFunction &getOriginal() const { return *synthesis.original; }
  SILFunction &getAdjoint() const { return *synthesis.target; }
  SILBuilder &getBuilder() { return builder; }

  DifferentiationTask *getDifferentiationTask() const { return synthesis.task; }

public:
  explicit AdjointEmitter(const FunctionSynthesisItem &item,
                          DifferentiableActivityInfo &activityInfo,
                          DominanceInfo &domInfo, PostDominanceInfo &pdomInfo,
                          SILLoopInfo &loopInfo, AdjointGen &adjointGen)
      : synthesis(item), activityInfo(activityInfo), domInfo(domInfo),
        postDomInfo(pdomInfo), adjointGen(adjointGen),
        rematCloner(getAdjoint()), builder(getAdjoint()) {}

protected:
  /// Rematerialize an original instruction. Depending on how its result is used
  /// in the original function, rematerialization will be emitted to the nearest
  /// common dominator of all use sites in the adjoint. If there are no uses,
  /// just clone this instruction at the corresponding basic block.
  void rematerializeOriginalInstruction(SILInstruction *inst);

  /// Materialize an adjoint value. The type of the given adjoint value must be
  /// loadable.
  SILValue materializeAdjointDirect(AdjointValue val, SILLocation loc);

  /// Materialize an adjoint value indirectly to a SIL buffer.
  void materializeAdjointIndirect(AdjointValue val, SILValue destBuffer);

  /// Given an two adjoint values, accumulate them.
  AdjointValue accumulateAdjointsDirect(AdjointValue lhs, AdjointValue rhs);

  /// Given two materialized adjoint values, accumulate them. These two
  /// adjoints must be objects of loadable type.
  SILValue accumulateMaterializedAdjointsDirect(SILValue lhs, SILValue rhs);

  /// Given two materialized adjoint values, accumulate them using
  /// `VectorNumeric.+` or `FloatingPoint.+`, depending on the differentiation
  /// mode.
  void accumulateMaterializedAdjointsIndirect(SILValue lhsBufAccess,
                                              SILValue rhsBufAccess,
                                              SILValue resultBufAcess);

  /// Get the adjoint for an original value. The given value must be in the
  /// original function.
  ///
  /// This method first tries to find an entry in `adjointMap`. If an adjoint
  /// doesn't exist, create a zero adjoint.
  AdjointValue getAdjointValue(SILValue originalValue) {
    assert(originalValue->getFunction() == &getOriginal());
    auto insertion = adjointMap.try_emplace(
        originalValue, AdjointValue::getZero(originalValue->getType()));
    return insertion.first->getSecond();
  }

  /// Add an adjoint value for the given original value. If there's no previous
  /// value
  AdjointValue &addAdjointValue(SILValue originalValue,
                                AdjointValue adjointValue) {
    assert(originalValue->getFunction() == &getOriginal());
    LLVM_DEBUG(getADDebugStream() << "Adding adjoint for " << originalValue);
    auto insertion = adjointMap.try_emplace(originalValue, adjointValue);
    auto inserted = insertion.second;
    auto &value = insertion.first->getSecond();
    // If adjoint already exists, accumulate the adjoint onto the existing
    // adjoint.
    if (!inserted)
      value = accumulateAdjointsDirect(value, adjointValue);
    return value;
  }

  SILBasicBlock *getAdjointBlock(SILBasicBlock *originalBlock) {
    return adjointBBMap.lookup(originalBlock);
  }

public:
  /// Performs adjoint synthesis on the empty adjoint function. Returns true if
  /// any error occurs.
  bool run() {
    auto &original = getOriginal();
    auto &adjoint = getAdjoint();
    auto adjLoc = getAdjoint().getLocation();
    auto *task = getDifferentiationTask();
    LLVM_DEBUG(getADDebugStream() << "Running AdjointGen on\n" << original);
    auto origTy = original.getLoweredFunctionType();
    // Create entry BB and arguments.
    auto *adjointEntry = getAdjoint().createBasicBlock();
    createEntryArguments(&adjoint);
    auto *origRetBB = &*original.findReturnBB();
    adjointBBMap.insert({origRetBB, adjointEntry});
    SILFunctionConventions origConv(origTy, getModule());
    // Initialize `originalParameters`, `primalValueAggregate`,
    // `originalResults` and `seed`.
    auto adjParamArgs = getAdjoint().getArgumentsWithoutIndirectResults();
    auto origNumParams = origConv.getNumParameters();
    auto origNumResults = origTy->getNumResults();
    // The adjoint function has type
    //   (seed, pv0, ..., pvn, origres, arg0, ..., argn, [self])
    //       -> ([self], [arg0], ..., [argn]).
    // Square brackets denote [] elements that are not always in the signature:
    //   * "self" is present in the argument list when it's present in the
    //      original's argument list.
    //   * Results are present when the adjoint is with respect to the
    //     corresponding original argument.
    //
    // We get each range of arguments by shifting the `paramArgsData` pointer.
    auto *paramArgsData = adjParamArgs.data();
    seed = *paramArgsData++;
    primalValueAggregateInAdj = *paramArgsData++;
    originalResultsInAdj = {paramArgsData, origNumResults};
    paramArgsData += origNumResults;
    originalParametersInAdj = {paramArgsData, origNumParams};
    paramArgsData += origNumParams;
    assert(paramArgsData == adjParamArgs.data() + adjParamArgs.size());

    // Map the original's parameters to the adjoint's corresponding "original
    // parameters".
    for (auto pair : zip(original.getArgumentsWithoutIndirectResults(),
                         originalParametersInAdj))
      rematCloner.insertValueMapping(std::get<0>(pair), std::get<1>(pair));

    // Assign adjoint to the return value.
    //   y = tuple (y0, ..., yn)
    //   return y
    //   adj[y] =
    //     if the source result is direct
    //     then tuple (0, ..., seed, ..., 0) where seed is at the direct
    //          result index corresponding to the source index
    //     else zeros
    SmallVector<SILValue, 8> formalResults;
    collectAllFormalResultsInTypeOrder(original, formalResults);
    auto srcIdx = task->getIndices().source;
    addAdjointValue(formalResults[srcIdx], AdjointValue::getMaterialized(seed));
    LLVM_DEBUG(getADDebugStream()
               << "Assigned seed " << *seed << " as the adjoint of "
               << formalResults[srcIdx]);

    // From the original exit, emit a reverse control flow graph and perform
    // differentiation in each block.
    // NOTE: For now we just assume single basic block.
    for (auto *bb : llvm::breadth_first(origRetBB)) {
      if (errorOccurred)
        break;
      auto indices = getDifferentiationTask()->getIndices();
      // Get the corresponding adjoint basic block.
      auto adjBB = getAdjointBlock(bb);
      // Prepare the remat cloner for on-demand rematerialization.
      rematCloner.setInsertionPointBeforeAnyTerminator(bb);
      getBuilder().setInsertionPoint(adjBB);
      // Visit each instruction in reverse order.
      for (auto &inst : reversed(*bb)) {
        // If any results are active on the differentiation path, we'll
        // differentiate it.
        if (isa<NonValueInstruction>(inst))
          continue;
        auto needsDiff = llvm::any_of(inst.getResults(), [&](SILValue val) {
          return activityInfo.isActive(val, indices);
        });
        if (!needsDiff)
          continue;
        // Differentiate instruction.
        visit(&inst);
      }
    }
    
    for (auto &pair : rematCloner.scopeMarkingInstructions) {
      auto *original = pair.first;
      auto *cloned = pair.second;
      // The end-scope counterpart should be emitted to the adjoint block
      // corresponding to the original begin-scope instruction.
      auto *endBB = getAdjointBlock(original->getParent());
      getBuilder().setInsertionPoint(endBB);
      if (auto *bai = dyn_cast<BeginAccessInst>(original))
        getBuilder().createEndAccess(bai->getLoc(),
                                     cast<BeginAccessInst>(cloned),
                                     /*aborted*/ false);
      else if (auto *asi = dyn_cast<AllocStackInst>(original))
        getBuilder().createDeallocStack(asi->getLoc(),
                                        cast<AllocStackInst>(cloned));
      // FIXME: Handle other cases.
    }

    // If errors occurred, back out.
    if (errorOccurred)
      return true;

    // Place the builder at the adjoint block corresponding to the original
    // entry. This block is going to be our exit block and we emit a `return`
    // there.
    getBuilder().setInsertionPoint(adjointEntry);

    // This vector will contain all the materialized return elements.
    SmallVector<SILValue, 8> retElts;
    auto origParams = original.getArgumentsWithoutIndirectResults();

    // Materializes the return element corresponding to the parameter
    // `parameterIndex` into the `retElts` vector.
    auto addRetElt = [&](unsigned parameterIndex) -> void {
      auto origParam = origParams[parameterIndex];
      auto adjParam = originalParametersInAdj[parameterIndex];
      auto adjVal = getAdjointValue(origParam);
      if (adjParam->getType().isObject())
        retElts.push_back(materializeAdjointDirect(adjVal, adjLoc));
      else
        materializeAdjointIndirect(adjVal, adjParam);
    };

    // The original's self parameter, if present, is the last parameter. But we
    // want its gradient, if present, to be the first return element.
    auto selfParamIndex = origParams.size() - 1;
    if (origTy->hasSelfParam() &&
        task->getIndices().isWrtParameter(selfParamIndex))
      addRetElt(selfParamIndex);

    // Add the non-self parameters that are differentiated with respect to.
    for (auto i : task->getIndices().parameters.set_bits()) {
      // Do not add the self parameter because we have already added it at the
      // beginning.
      if (origTy->hasSelfParam() && i == selfParamIndex)
        continue;
      addRetElt(i);
    }
    getBuilder().createReturn(adjLoc, joinElements(retElts, builder, adjLoc));
    return errorOccurred;
  }

  void visit(SILInstruction *inst) {
    if (errorOccurred)
      return;
    SILInstructionVisitor::visit(inst);
  }

  void visitSILInstruction(SILInstruction *inst) {
    llvm_unreachable("Unsupport instruction visited");
  }

  SILLocation remapLocation(SILLocation loc) { return loc; }

  SILType remapType(SILType type) { return type; }

  /// Determines if the given value is a control-dependent value in the original
  /// function. This helps determine whether to retrieve a primal value from
  /// the struct directly or from a tape.
  bool isControlDependent(SILValue value) const {
    auto &fn = getOriginal();
    assert(value->getFunction() == &fn && "Value not in the original function");
    return !postDomInfo.dominates(value->getParentBlock(), fn.getEntryBlock());
  }

  // Given a primal value in the original function, extract this value from the
  // primal value struct either by struct extraction or tape operation.
  //
  // The boolean flag `nested` determines for an `apply` instruction whether we
  // are extracting its nested primal values (as an aggregate) instead of its
  // original results. When `nested` is true, `value` must be an `apply`
  // instruction.
  SILValue extractPrimalValueIfAny(SILValue value, bool nested = false) {
    // If the non-nested primal value has been extracted before, use that.
    if (!nested)
      if (auto val = rematCloner.lookUpValue(value))
        return val;
    // Do primal value extraction.
    auto &pi = getPrimalInfo();
    VarDecl *field = nullptr;
    SILValue extracted;
    auto loc = remapLocation(value.getLoc());
    if (isControlDependent(value)) {
      // TODO: handle control-dependent values through tape operations. The
      // general strategy is to emit an `autodiffPopFromTape` builtin in the
      // corresponding adjoint block of the dominator of all uses of the value.
      llvm_unreachable("Control-dependent primal value are not handled yet");
    }
    // For non-control-dependent primal values, we just look up the struct
    // directly.
    else {
      if (nested) {
        auto *applyInst = cast<ApplyInst>(value);
        field = pi.lookUpNestedStaticPrimalValueDecl(applyInst);
      } else {
        field = pi.lookupDirectStaticPrimalValueDecl(value);
      }
      // No field found, so this primal value was not checkpointed.
      if (!field)
        return nullptr;
      // If it's an address, use `struct_element_addr` to get the element
      // address.
      if (pi.getLoweredPrimalValueStructType().isAddress())
        extracted = getBuilder().createStructElementAddr(
            loc, primalValueAggregateInAdj, field);
      // Otherwise, the primal value struct is a normal SSA value. Emit a
      // `struct_extract`.
      else
        extracted = getBuilder().createStructExtract(
            remapLocation(value.getLoc()), primalValueAggregateInAdj, field);
    }
    // Memorize this value mapping in the remat cloner, when we are not
    // extracting a nested primal value aggregate.
    if (!nested)
      rematCloner.insertValueMapping(value, extracted);
    return extracted;
  }

  /// Remap a value in the original function.
  SILValue getMappedValue(SILValue value) {
    // If `value` is a checkpointed primal value, extract it from the primal
    // value aggregate.
    if (auto extractedPV = extractPrimalValueIfAny(value))
      return extractedPV;
    // Otherwise, `value` is a non-checkpointed primal value. Recursively
    // rematerialize it in the adjoint function.
    if (auto *inst = value->getDefiningInstruction())
      rematerializeOriginalInstruction(inst);
    return rematCloner.getMappedValue(value);
  }

  /// Handle `apply` instruction. If it's active (on the differentiation path),
  /// we replace it with its corresponding adjoint.
  void visitApplyInst(ApplyInst *ai) {
    // Replace a call to the function with a call to its adjoint.
    auto &assocTasks = getDifferentiationTask()->getAssociatedTasks();
    auto assocTaskLookUp = assocTasks.find(ai);
    // If no task was found, then this task doesn't need to be differentiated.
    if (assocTaskLookUp == assocTasks.end()) {
      // Must not be active.
      assert(
          !activityInfo.isActive(ai, getDifferentiationTask()->getIndices()));
      return;
    }
    // When we have a differentiation task, just get the adjoint and call it.
    auto *otherTask = assocTaskLookUp->getSecond();
    auto origTy = otherTask->getOriginal()->getLoweredFunctionType();
    SILFunctionConventions origConvs(origTy, getModule());
    auto *adjoint = otherTask->getAdjoint();
    auto loc = remapLocation(ai->getLoc());
    // Prepare arguments for calling the corresponding adjoint.
    // Parameters: (seed..., prim_val_struct?, orig_res..., orig_args...)
    // Results: (derivatives...)
    SmallVector<SILValue, 8> args;
    // For each indirect result, allocate a local buffer and add it to the
    // argument list.
    SmallVector<SILValue, 8> allocsToCleanUp;
    for (auto param : ai->getIndirectSILResults()) {
      // FIXME: Emit `dealloc_stack` somewhere!
      auto *buf = getBuilder().createAllocStack(loc, param->getType());
      args.push_back(buf);
      allocsToCleanUp.push_back(buf);
    }

    // Add seed.
    auto seed = getAdjointValue(ai);
    auto *seedBuf = getBuilder().createAllocStack(loc, seed.getType());
    materializeAdjointIndirect(seed, seedBuf);
    if (seed.getType().isAddressOnly(getModule()))
      args.push_back(seedBuf);
    else {
      auto access = getBuilder().createBeginAccess(
          loc, seedBuf, SILAccessKind::Read, SILAccessEnforcement::Static,
          /*noNestedConflict*/ true,
          /*fromBuiltin*/ false);
      args.push_back(getBuilder().createLoad(
          loc, access, getBufferLOQ(seed.getSwiftType(), getAdjoint())));
      getBuilder().createEndAccess(loc, access, /*aborted*/ false);
    }

    // Add nested primal values.
    if (auto nestedPrimValAggr = extractPrimalValueIfAny(ai, /*nested*/ true)) {
      SmallVector<SILValue, 8> nestedPrimVals;
      extractAllElements(nestedPrimValAggr, builder, nestedPrimVals);
      args.append(nestedPrimVals.begin(), nestedPrimVals.end());
    }
    // Add original results.
    auto origResultAggr = getMappedValue(ai);
    SmallVector<SILValue, 8> origResults;
    extractAllElements(origResultAggr, builder, origResults);
    args.append(origResults.begin(), origResults.end());

    // Add the original parameters.
    auto originalParams = ai->getArgumentsWithoutIndirectResults();
    for (auto param : originalParams)
      args.push_back(getMappedValue(param));

    // Call the adjoint function.
    auto *adjointRef = getBuilder().createFunctionRef(ai->getLoc(), adjoint);
    auto origFnRef = ai->getCalleeOrigin();
    auto convertedAdjFn = reapplyFunctionConversion(
        adjointRef, origFnRef, ai->getCallee(), getBuilder(), ai->getLoc());
    auto *applyAdj = getBuilder().createApply(ai->getLoc(), convertedAdjFn,
                                              ai->getSubstitutionMap(), args,
                                              /*isNonThrowing*/ false);
    // Clean up seed allocation.
    getBuilder().createDeallocStack(loc, seedBuf);
    // If `applyAdj` is a tuple, extract all results.
    SmallVector<SILValue, 8> dirResults;
    extractAllElements(applyAdj, builder, dirResults);
    // Get all results in type-defined order.
    SmallVector<SILValue, 8> allResults;
    collectAllActualResultsInTypeOrder(
        applyAdj, dirResults, applyAdj->getIndirectSILResults(), allResults);
    LLVM_DEBUG({
      auto &s = getADDebugStream();
      s << "All direct results of the nested adjoint call: \n";
      interleave(dirResults, [&](SILValue v) { s << v; }, [&]{});
      s << "All indirect results of the nested adjoint call: \n";
      interleave(applyAdj->getIndirectSILResults(),
                 [&](SILValue v) { s << v; }, [&]{});
      s << "All results of the nested adjoint call: \n";
      interleave(allResults, [&](SILValue v) { s << v; }, [&]{});
    });
    // Set adjoints for all original parameters.
    auto origNumIndRes = origConvs.getNumIndirectSILResults();
    auto allResultsIt = allResults.begin();
    // If the applied adjoint returns the adjoint of the original self
    // parameter, then it returns it first. Set the adjoint of the original
    // self parameter.
    auto selfParamIndex = originalParams.size() - 1;
    if (ai->hasSelfArgument() &&
        otherTask->getIndices().isWrtParameter(selfParamIndex))
      addAdjointValue(ai->getArgument(origNumIndRes + selfParamIndex),
                      AdjointValue::getMaterialized(*allResultsIt++));
    // Set adjoints for the remaining non-self original parameters.
    for (unsigned i : otherTask->getIndices().parameters.set_bits()) {
      // Do not set the adjoint of the original self parameter because we
      // already added it at the beginning.
      if (ai->hasSelfArgument() && i == selfParamIndex)
        continue;
      addAdjointValue(ai->getArgument(origNumIndRes + i),
                      AdjointValue::getMaterialized(*allResultsIt++));
    }
  }

  /// Handle `gradient` instruction.
  ///
  /// NOTE: Nested differentiation is not supported yet.
  void visitGradientInst(GradientInst *gi) {
    // Rejected by PrimalGen already.
    llvm_unreachable("Should've been rejected by PrimalGen");
  }

  /// Handle `struct` instruction.
  ///   y = struct (x0, x1, x2, ...)
  ///   adj[x0] = struct_extract #0, adj[y]
  ///   adj[x1] = struct_extract #1, adj[y]
  ///   adj[x2] = struct_extract #2, adj[y]
  ///   ...
  void visitStructInst(StructInst *si) {
    auto *decl = si->getStructDecl();
    auto av = getAdjointValue(si);
    auto loc = si->getLoc();
    switch (av.getKind()) {
    case AdjointValue::Zero:
      for (auto *field : decl->getStoredProperties()) {
        auto fv = si->getFieldValue(field);
        addAdjointValue(fv, AdjointValue::getZero(fv->getType()));
      }
      break;
    case AdjointValue::Materialized: {
      auto adjY = av.getMaterializedValue();
      for (auto *field : decl->getStoredProperties())
        addAdjointValue(si->getFieldValue(field),
                        getBuilder().createStructExtract(loc, adjY, field));
      break;
    }
    case AdjointValue::Aggregate: {
      for (auto pair : llvm::zip(si->getElements(), av.getAggregateElements()))
        addAdjointValue(std::get<0>(pair), std::get<1>(pair));
      break;
    }
    }
  }

  /// Handle `struct_extract` instruction.
  ///   y = struct_extract <key>, x
  ///   adj[x] = struct (0, ..., key: adj[y], ..., 0)
  void visitStructExtractInst(StructExtractInst *sei) {
    auto *structDecl = sei->getStructDecl();
    auto av = getAdjointValue(sei);
    switch (av.getKind()) {
    case AdjointValue::Kind::Zero:
      addAdjointValue(sei->getOperand(),
                      AdjointValue::getZero(sei->getOperand()->getType()));
      break;
    case AdjointValue::Kind::Materialized:
    case AdjointValue::Kind::Aggregate: {
      SmallVector<AdjointValue, 8> eltVals;
      for (auto *field : structDecl->getStoredProperties()) {
        if (field == sei->getField())
          eltVals.push_back(av);
        else
          eltVals.push_back(AdjointValue::getZero(
              SILType::getPrimitiveObjectType(
                  field->getType()->getCanonicalType())));
      }
      addAdjointValue(sei->getOperand(),
          AdjointValue::getAggregate(sei->getOperand()->getType(),
                                     eltVals, allocator));
    }
    }
  }

  /// Handle `tuple` instruction.
  ///   y = tuple (x0, x1, x2, ...)
  ///   adj[x0] = tuple_extract 0, adj[y]
  ///   ...
  void visitTupleInst(TupleInst *ti) {
    auto av = getAdjointValue(ti);
    switch (av.getKind()) {
    case AdjointValue::Kind::Zero:
      for (auto eltVal : ti->getElements())
        addAdjointValue(eltVal, AdjointValue::getZero(eltVal->getType()));
      break;
    case AdjointValue::Kind::Materialized:
      for (auto i : range(ti->getNumOperands()))
        addAdjointValue(ti->getOperand(i),
                        getBuilder().createTupleExtract(ti->getLoc(), ti, i));
      break;
    case AdjointValue::Kind::Aggregate:
      for (auto pair : llvm::zip(ti->getElements(), av.getAggregateElements()))
        addAdjointValue(std::get<0>(pair), std::get<1>(pair));
      break;
    }
  }

  /// Handle `tuple_extract` instruction.
  ///   y = tuple_extract <n>, x
  ///                                 |--- n-th element
  ///   adj[x] = tuple (0, 0, ..., adj[y], ..., 0, 0)
  void visitTupleExtractInst(TupleExtractInst *tei) {
    auto *tupleTy = tei->getTupleType();
    auto av = getAdjointValue(tei);
    switch (av.getKind()) {
    case AdjointValue::Kind::Zero:
      addAdjointValue(tei->getOperand(),
                      AdjointValue::getZero(SILType::getPrimitiveObjectType(
                          tupleTy->getCanonicalType())));
      break;
    case AdjointValue::Kind::Aggregate:
    case AdjointValue::Kind::Materialized: {
      SmallVector<AdjointValue, 8> elements;
      for (unsigned i : range(tupleTy->getNumElements())) {
        if (tei->getFieldNo() == i)
          elements.push_back(av);
        else {
          auto eltTy = SILType::getPrimitiveObjectType(
              tupleTy->getElementType(i)->getCanonicalType());
          elements.push_back(AdjointValue::getZero(eltTy));
        }
      }
      addAdjointValue(tei->getOperand(),
          AdjointValue::getAggregate(tei->getOperand()->getType(),
                                     elements, allocator));
      break;
    }
    }
  }

  /// Handle floating-point arithmetics: `fadd`, `fsub`, `fneg`, `fmul`, and
  /// `fdiv`.
  void visitBuiltinInst(BuiltinInst *bi) {
    LLVM_DEBUG(getADDebugStream() << "Differentiating " << *bi);
    auto &info = bi->getBuiltinInfo();
    auto adj = getAdjointValue(bi);
    auto &builder = getBuilder();
    auto opType = remapType(bi->getType());
    auto opLoc = remapLocation(bi->getLoc());
    switch (info.ID) {
    case BuiltinValueKind::FAdd:
      addAdjointValue(bi->getOperand(0), adj);
      addAdjointValue(bi->getOperand(1), adj);
      break;
    case BuiltinValueKind::FSub: {
      auto adjVal = materializeAdjointDirect(adj, opLoc);
      addAdjointValue(bi->getOperand(0), adjVal);
      // NOTE: `createBuiltinBinaryFunction` is general enough to work on
      // builtin functions with arbitrary arity.
      auto *neg = builder.createBuiltinBinaryFunction(opLoc, "fneg", opType,
                                                      opType, {adjVal});
      addAdjointValue(bi->getOperand(1), neg);
      break;
    }
    case BuiltinValueKind::FNeg: {
      auto adjVal = materializeAdjointDirect(adj, opLoc);
      auto *neg = builder.createBuiltinBinaryFunction(opLoc, "fneg", opType,
                                                      opType, {adjVal});
      addAdjointValue(bi->getOperand(0), neg);
      break;
    }
    case BuiltinValueKind::FMul: {
      auto adjVal = materializeAdjointDirect(adj, opLoc);
      auto *adjLHS = builder.createBuiltinBinaryFunction(
          opLoc, "fmul", opType, opType,
          {adjVal, getMappedValue(bi->getOperand(1))});
      addAdjointValue(bi->getOperand(0), adjLHS);
      auto *adjRHS = builder.createBuiltinBinaryFunction(
          opLoc, "fmul", opType, opType,
          {adjVal, getMappedValue(bi->getOperand(0))});
      addAdjointValue(bi->getOperand(1), adjRHS);
      break;
    }
    case BuiltinValueKind::FDiv: {
      auto adjVal = materializeAdjointDirect(adj, opLoc);
      auto lhs = getMappedValue(bi->getOperand(0));
      auto rhs = getMappedValue(bi->getOperand(1));
      // x' = seed / y
      auto adjLHS = builder.createBuiltinBinaryFunction(opLoc, "fdiv", opType,
                                                        opType, {adjVal, rhs});
      addAdjointValue(bi->getOperand(0), adjLHS);
      // y' = -x / y^2 * seed
      auto minusLHS = builder.createBuiltinBinaryFunction(opLoc, "fneg", opType,
                                                          opType, {lhs});
      auto squareRHS = builder.createBuiltinBinaryFunction(
          opLoc, "fmul", opType, opType, {rhs, rhs});
      auto div = builder.createBuiltinBinaryFunction(
          opLoc, "fdiv", opType, opType, {minusLHS, squareRHS});
      auto adjRHS = builder.createBuiltinBinaryFunction(opLoc, "fmul", opType,
                                                        opType, {adjVal, div});
      addAdjointValue(bi->getOperand(1), adjRHS);
      break;
    }
    default:
      getContext().emitNondifferentiabilityError(bi, getDifferentiationTask());
      errorOccurred = true;
      return;
    }
  }
};
} // end anonymous namespace

void AdjointEmitter::rematerializeOriginalInstruction(SILInstruction *inst) {
  assert(inst->getFunction() == &getOriginal());
  auto lookup = rematCloner.ValueMap.find(inst->getResults()[0]);
  if (lookup != rematCloner.ValueMap.end())
    return;
  // Find the nearest common dominator of the corresponding adjoint blocks of
  // all user blocks of the value.
  SmallPtrSet<SILBasicBlock *, 8> userBlocks;
  for (auto res : inst->getResults())
    for (auto *use : res->getUses())
      userBlocks.insert(getAdjointBlock(use->get()->getParentBlock()));
  auto *ncd = accumulate(userBlocks, getAdjointBlock(inst->getParentBlock()),
                         [&](SILBasicBlock *acc, SILBasicBlock *next) {
                           return domInfo.findNearestCommonDominator(acc, next);
                         });
  // Ensure that all operands have a corresponding value in the adjoint.
  for (auto &op : inst->getAllOperands())
    getMappedValue(op.get());
  rematCloner.setInsertionPointBeforeAnyTerminator(ncd);
  rematCloner.visit(inst);
}

SILValue AdjointEmitter::materializeAdjointDirect(AdjointValue val,
                                                  SILLocation loc) {
  auto &builder = getBuilder();
  auto &ctx = getContext();
  auto *swiftMod = ctx.getModule().getSwiftModule();
  LLVM_DEBUG(getADDebugStream() <<
             "Materializing adjoints for " << val << '\n');
  auto tangentSpace =
      getASTContext().getTangentSpace(val.getType().getASTType(), swiftMod);
  assert(tangentSpace && "No tangent space for this type");
  switch (val.getKind()) {
  case AdjointValue::Kind::Zero:
    return createScalarValueDirect(
        0, val.getType().getASTType(), loc, builder, ctx);
  case AdjointValue::Kind::Aggregate: {
    SmallVector<SILValue, 8> elements;
    for (auto &eltAdjVal : val.getAggregateElements())
      elements.push_back(materializeAdjointDirect(eltAdjVal, loc));
    if (auto tupleTy = val.getType().getAs<TupleType>()) {
      return builder.createTuple(loc, val.getType(), elements);
    } else {
      return builder.createStruct(loc, val.getType(), elements);
    }
  }
  case AdjointValue::Kind::Materialized:
    return val.getMaterializedValue();
  }
}

/// Materialize the given adjoint value indirectly to the specified buffer.
/// The root address derivation of `seedBufAccess` must be the result of
/// a `begin_access`.
static void materializeAdjointIndirectHelper(AdjointValue val,
                                             SILValue destBufferAccess,
                                             SILBuilder &builder,
                                             ADContext &context) {
  auto loc = destBufferAccess.getLoc();
  auto soq = getBufferSOQ(val.getType().getASTType(), builder.getFunction());
  switch (val.getKind()) {
  /// Given a `%buf : *T, emit instructions that produce a zero or an aggregate
  /// of zeros of the expected type. When `T` conforms to
  /// `ExpressibleByIntegerLiteral`, we use literal conversion directly.
  /// Otherwise, we assert that `T` must be an aggregate where each element is
  /// `ExpressibleByIntegerLiteral`. We expect to emit a zero for each element
  /// and use the appropriate aggregate constructor instruction (in this case,
  /// `tuple`) to produce a tuple. But currently, since we need indirect
  /// passing for aggregate instruction, we just use `tuple_element_addr` to get
  /// element buffers and write elements to them.
  case AdjointValue::Kind::Zero:
    if (auto tupleTy = val.getType().getAs<TupleType>()) {
      SmallVector<SILValue, 8> eltVals;
      for (unsigned i : range(tupleTy->getNumElements())) {
        auto eltAddr = builder.createTupleElementAddr(loc, destBufferAccess, i);
        createScalarValueIndirect(0, tupleTy, eltAddr, loc, builder, context);
      }
    } else {
      createScalarValueIndirect(0, val.getSwiftType(),
                                destBufferAccess, loc, builder, context);
    }
    break;
  /// Given a `%buf : *(T0, T1, T2, ...)` or `%buf : *Struct` recursively emit
  /// instructions to materialize the symbolic tuple or struct, filling the
  /// buffer.
  case AdjointValue::Kind::Aggregate: {
    if (auto *tupTy = val.getSwiftType()->getAs<TupleType>()) {
      for (auto eltAndIdx : enumerate(val.getAggregateElements())) {
        auto idx = eltAndIdx.index();
        auto eltTy = SILType::getPrimitiveObjectType(tupTy->getCanonicalType());
        auto *eltBuf =
            builder.createTupleElementAddr(loc, destBufferAccess, idx, eltTy);
        materializeAdjointIndirectHelper(
            eltAndIdx.value(), eltBuf, builder, context);
      }
    } else if (auto *structDecl = dyn_cast_or_null<StructDecl>(
                   val.getSwiftType()->getAnyNominal())) {
      for (auto eltAndField : zip(val.getAggregateElements(),
                                  structDecl->getStoredProperties())) {
        auto elt = std::get<0>(eltAndField);
        auto field = std::get<1>(eltAndField);
        auto *eltBuf = builder.createStructElementAddr(loc, destBufferAccess,
                                                       field);
        materializeAdjointIndirectHelper(elt, eltBuf, builder, context);
      }
    } else {
      llvm_unreachable("Not an aggregate type");
    }
    break;
  }
  /// Value is already materialized!
  case AdjointValue::Kind::Materialized:
    auto *access = builder.createBeginAccess(
        loc, destBufferAccess, SILAccessKind::Init,
        SILAccessEnforcement::Static, /*noNestedConflict*/ true,
        /*fromBuiltin*/ false);
    builder.createStore(loc, val.getMaterializedValue(), access, soq);
    builder.createEndAccess(loc, access, /*aborted*/ false);
    break;
  }
}

void AdjointEmitter::materializeAdjointIndirect(AdjointValue val,
                                                SILValue destBuffer) {
  auto *access = getBuilder().createBeginAccess(
      destBuffer.getLoc(), destBuffer, SILAccessKind::Init,
      SILAccessEnforcement::Static, /*noNestedConflict*/ true,
      /*fromBuiltin*/ false);
  materializeAdjointIndirectHelper(val, access, builder, getContext());
  getBuilder().createEndAccess(destBuffer.getLoc(), access, /*aborted*/ false);
}

AdjointValue AdjointEmitter::accumulateAdjointsDirect(AdjointValue lhs,
                                                      AdjointValue rhs) {
  switch (lhs.getKind()) {
  // x
  case AdjointValue::Kind::Materialized: {
    auto lhsVal = lhs.getMaterializedValue();
    switch (rhs.getKind()) {
    // x + y
    case AdjointValue::Kind::Materialized:
      return AdjointValue::getMaterialized(
          accumulateMaterializedAdjointsDirect(lhsVal,
                                               rhs.getMaterializedValue()));
    // x + 0 => x
    case AdjointValue::Kind::Zero:
      return lhs;
    // x + (y, z) => (x.0 + y, x.1 + z)
    case AdjointValue::Kind::Aggregate:
      SmallVector<AdjointValue, 8> newElements;
      auto rhsElements = rhs.getAggregateElements();
      for (auto i : indices(rhsElements)) {
        auto lhsElt =
            getBuilder().createTupleExtract(lhsVal.getLoc(), lhsVal, i);
        auto newElt = accumulateAdjointsDirect(
            AdjointValue::getMaterialized(lhsElt), rhsElements[i]);
        newElements.push_back(newElt);
      }
      return AdjointValue::getAggregate(
          lhsVal->getType(), newElements, allocator);
    }
  }
  // 0
  case AdjointValue::Kind::Zero:
    // 0 + x => x
    return rhs;
  // (x, y)
  case AdjointValue::Kind::Aggregate:
    switch (rhs.getKind()) {
    // (x, y) + z => (x + z.0, y + z.1)
    case AdjointValue::Kind::Materialized:
    // x + 0 => x
    case AdjointValue::Kind::Zero:
      return lhs;
    // (x, y) + (z, w) => (x + z, y + w)
    case AdjointValue::Kind::Aggregate: {
      SmallVector<AdjointValue, 8> newElements;
      for (auto elt : llvm::zip(lhs.getAggregateElements(),
                                rhs.getAggregateElements()))
        newElements.push_back(
            accumulateAdjointsDirect(std::get<0>(elt), std::get<1>(elt)));
      return AdjointValue::getAggregate(lhs.getType(), newElements, allocator);
    }
    }
  }
}

SILValue AdjointEmitter::accumulateMaterializedAdjointsDirect(SILValue lhs,
                                                              SILValue rhs) {
  // TODO: Optimize for the case when lhs == rhs.
  assert(lhs->getType() == rhs->getType() && "Adjoints must have equal types!");
  assert(lhs->getType().isObject() && rhs->getType().isObject() &&
         "Adjoint types must be both object types!");
  auto adjointTy = lhs->getType();
  auto adjointASTTy = adjointTy.getASTType();
  auto loc = lhs.getLoc();
  auto &builder = getBuilder();
  auto &astCtx = getContext().getASTContext();
  auto *swiftMod = getModule().getSwiftModule();
  auto tangentSpace = astCtx.getTangentSpace(adjointASTTy, swiftMod);
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case TangentSpace::Kind::BuiltinRealScalar:
    return builder.createBuiltinBinaryFunction(
        loc, "fadd", adjointTy, adjointTy, {lhs, rhs});
    case TangentSpace::Kind::RealScalar:
    case TangentSpace::Kind::RealVector: {
      // Handle any nominal type value.

      // Allocate buffers for inputs and output.
      auto *resultBuf = builder.createAllocStack(loc, adjointTy);
      auto *lhsBuf = builder.createAllocStack(loc, adjointTy);
      auto *rhsBuf = builder.createAllocStack(loc, adjointTy);

      // Initialize input buffers.
      auto *lhsBufInitAccess = builder.createBeginAccess(
          loc, lhsBuf, SILAccessKind::Init, SILAccessEnforcement::Static,
          /*noNestedConflict*/ true, /*fromBuiltin*/ false);
      auto *rhsBufInitAccess = builder.createBeginAccess(
          loc, rhsBuf, SILAccessKind::Init, SILAccessEnforcement::Static,
          /*noNestedConflict*/ true, /*fromBuiltin*/ false);
      builder.createStore(loc, lhs, lhsBufInitAccess,
                          getBufferSOQ(adjointASTTy, getAdjoint()));
      builder.createStore(loc, rhs, rhsBufInitAccess,
                          getBufferSOQ(adjointASTTy, getAdjoint()));
      builder.createEndAccess(loc, lhsBufInitAccess, /*aborted*/ false);
      builder.createEndAccess(loc, rhsBufInitAccess, /*aborted*/ false);

      // Accumulate the adjoints.
      auto *resultBufAccess = builder.createBeginAccess(
          loc, resultBuf, SILAccessKind::Init, SILAccessEnforcement::Static,
          /*noNestedConflict*/ true, /*fromBuiltin*/ false);
      auto *lhsBufReadAccess = builder.createBeginAccess(loc, lhsBuf,
          SILAccessKind::Read, SILAccessEnforcement::Static,
          /*noNestedConflict*/ true, /*fromBuiltin*/ false);
      auto *rhsBufReadAccess = builder.createBeginAccess(loc, rhsBuf,
          SILAccessKind::Read, SILAccessEnforcement::Static,
          /*noNestedConflict*/ true, /*fromBuiltin*/ false);
      accumulateMaterializedAdjointsIndirect(lhsBufReadAccess, rhsBufReadAccess,
                                             resultBufAccess);
      builder.createEndAccess(loc, resultBufAccess, /*aborted*/ false);
      builder.createEndAccess(loc, rhsBufReadAccess, /*aborted*/ false);
      builder.createEndAccess(loc, lhsBufReadAccess, /*aborted*/ false);

      // Deallocate input buffers.
      builder.createDeallocStack(loc, rhsBuf);
      builder.createDeallocStack(loc, lhsBuf);

      // Load result.
      resultBufAccess = builder.createBeginAccess(loc, resultBuf,
          SILAccessKind::Read, SILAccessEnforcement::Static,
          /*noNestedConflict*/ true, /*fromBuiltin*/ false);
      auto val = builder.createLoad(loc, resultBufAccess,
          getBufferLOQ(lhs->getType().getASTType(), getAdjoint()));
      builder.createEndAccess(loc, resultBufAccess, /*aborted*/ false);

      // Deallocate result buffer.
      builder.createDeallocStack(loc, resultBuf);

      return val;
    }
    case TangentSpace::Kind::ProductStruct: {
      auto *structDecl = tangentSpace->getProductStruct();
      SmallVector<SILValue, 8> adjElements;
      for (auto *field : structDecl->getStoredProperties()) {
        auto *eltLHS = builder.createStructExtract(loc, lhs, field);
        auto *eltRHS = builder.createStructExtract(loc, rhs, field);
        adjElements.push_back(
            accumulateMaterializedAdjointsDirect(eltLHS, eltRHS));
      }
      return builder.createStruct(loc, adjointTy, adjElements);
    }
    case TangentSpace::Kind::ProductTuple: {
      auto tupleType = tangentSpace->getProductTuple();
      SmallVector<SILValue, 8> adjElements;
      for (unsigned i : range(tupleType->getNumElements())) {
        auto *eltLHS = builder.createTupleExtract(loc, lhs, i);
        auto *eltRHS = builder.createTupleExtract(loc, rhs, i);
        adjElements.push_back(
            accumulateMaterializedAdjointsDirect(eltLHS, eltRHS));
      }
      return builder.createTuple(loc, adjointTy, adjElements);
    }
    case TangentSpace::Kind::Sum: {
      llvm_unreachable("Differentiating sum types is not supported yet");
    }
  }
}

void AdjointEmitter::accumulateMaterializedAdjointsIndirect(
    SILValue lhsBufAccess, SILValue rhsBufAccess, SILValue resultBufAccess) {
  // TODO: Optimize for the case when lhs == rhs.
  assert(lhsBufAccess->getType() == rhsBufAccess->getType()
         && "Adjoints must have equal types!");
  assert(lhsBufAccess->getType().isAddress() &&
         rhsBufAccess->getType().isAddress()
         && "Adjoint types must be both address types!");
  auto loc = resultBufAccess.getLoc();
  auto adjointTy = lhsBufAccess->getType();
  auto adjointASTTy = adjointTy.getASTType();
  auto &context = getContext();
  auto &astCtx = context.getASTContext();
  auto *swiftMod = getModule().getSwiftModule();
  auto tangentSpace = astCtx.getTangentSpace(adjointASTTy, swiftMod);
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case TangentSpace::Kind::BuiltinRealScalar: {
    auto *sum = builder.createBuiltinBinaryFunction(
        loc, "fadd", lhsBufAccess->getType(), lhsBufAccess->getType(),
        {lhsBufAccess, rhsBufAccess});
    builder.createStore(loc, sum, resultBufAccess,
                             getBufferSOQ(adjointASTTy, getAdjoint()));
    builder.createEndAccess(loc, resultBufAccess, /*aborted*/ false);
    return;
  }
  case TangentSpace::Kind::RealScalar:
  case TangentSpace::Kind::RealVector: {
    auto *adjointTyDecl = tangentSpace->getRealScalarOrVectorSpace();
    auto *proto = getContext().getAdditiveArithmeticProtocol();
    auto *combinerFuncDecl = getContext().getPlusDecl();
    // Call the combiner function and return.
    auto adjointParentModule = adjointTyDecl->getModuleContext();
    auto confRef = *adjointParentModule->lookupConformance(adjointASTTy, proto);
    SILDeclRef declRef(combinerFuncDecl, SILDeclRef::Kind::Func);
    auto silFnTy = getContext().getTypeConverter().getConstantType(declRef);
    // %0 = witness_method @+
    auto witnessMethod = builder.createWitnessMethod(loc, adjointASTTy,
                                                     confRef, declRef, silFnTy);
    // Ensure the witness method is linked.
    getModule().lookUpFunctionInWitnessTable(confRef, declRef);
    auto subMap =
        SubstitutionMap::getProtocolSubstitutions(proto, adjointASTTy, confRef);
    // %1 = metatype $T.Type
    auto metatypeType =
        CanMetatypeType::get(adjointASTTy, MetatypeRepresentation::Thick);
    auto metatypeSILType = SILType::getPrimitiveObjectType(metatypeType);
    auto metatype = builder.createMetatype(loc, metatypeSILType);
    // %2 = apply $0(%result, %new, %old, %1)
    builder.createApply(loc, witnessMethod, subMap,
                        {resultBufAccess, rhsBufAccess, lhsBufAccess, metatype},
                        /*isNonThrowing*/ false);
    return;
  }
  case TangentSpace::Kind::ProductTuple: {
    auto tupleType = tangentSpace->getProductTuple();
    for (unsigned i : range(tupleType->getNumElements())) {
      auto *eltAddrLHS = builder.createTupleElementAddr(loc, lhsBufAccess, i);
      auto *eltAddrRHS = builder.createTupleElementAddr(loc, rhsBufAccess, i);
      auto *destAddr = builder.createTupleElementAddr(loc, resultBufAccess, i);
      accumulateMaterializedAdjointsIndirect(eltAddrLHS, eltAddrRHS, destAddr);
    }
    return;
  }
  case TangentSpace::Kind::ProductStruct: {
    auto *structDecl = tangentSpace->getProductStruct();
    for (auto *field : structDecl->getStoredProperties()) {
      auto *eltAddrLHS =
          builder.createStructElementAddr(loc, lhsBufAccess, field);
      auto *eltAddrRHS =
          builder.createStructElementAddr(loc, rhsBufAccess, field);
      auto *destAddr =
          builder.createStructElementAddr(loc, resultBufAccess, field);
      accumulateMaterializedAdjointsIndirect(eltAddrLHS, eltAddrRHS, destAddr);
    }
    return;
  }
  case TangentSpace::Kind::Sum: {
    llvm_unreachable("Differentiating a sum type is not supported yet");
  }
  }
}

bool AdjointGen::performSynthesis(FunctionSynthesisItem item) {
  LLVM_DEBUG(getADDebugStream() << "Performing adjoint synthesis for original"
             << item.original->getName() << " and its corresponding adjoint "
             << item.target->getName() << '\n');
  auto &passManager = context.getPassManager();
  auto *activityAnalysis =
      passManager.getAnalysis<DifferentiableActivityAnalysis>();
  auto *domAnalysis = passManager.getAnalysis<DominanceAnalysis>();
  auto *pdomAnalysis = passManager.getAnalysis<PostDominanceAnalysis>();
  auto *loopAnalysis = passManager.getAnalysis<SILLoopAnalysis>();
  // Generate primal code.
  AdjointEmitter emitter(item, *activityAnalysis->get(item.original),
                         *domAnalysis->get(item.original),
                         *pdomAnalysis->get(item.original),
                         *loopAnalysis->get(item.original), *this);
  return emitter.run();
}

//===----------------------------------------------------------------------===//
// DifferentiationTask
//===----------------------------------------------------------------------===//

DifferentiationTask::DifferentiationTask(ADContext &context,
                                         SILFunction *original,
                                         SILDifferentiableAttr *&&attr,
                                         DifferentiationInvoker invoker)
    : context(context), original(original), attr(attr), invoker(invoker) {
  if (attr->hasPrimal())
    primal = lookUpOrLinkFunction(attr->getPrimalName(), context.getModule());
  if (attr->hasAdjoint())
    adjoint = lookUpOrLinkFunction(attr->getAdjointName(), context.getModule());
  if (attr->hasVJP())
    vjp = lookUpOrLinkFunction(attr->getVJPName(), context.getModule());

  if (adjoint) {
    // If we already have an adjoint, then we don't need to synthesize the
    // primal or the adjoint.
    primalSynthesisState = FunctionSynthesisState::NotNeeded;
    adjointSynthesisState = FunctionSynthesisState::NotNeeded;
  } else {
    assert(!attr->hasPrimal() &&
           "[differentiable] attr without adjoint should not have primal");

    // We don't have the primal or adjoint, so we need to synthesize them.
    // TODO: Once the AD pass uses VJP, we don't need to synthesize primal or
    // adjoint if VJP exists.
    primalSynthesisState = FunctionSynthesisState::Needed;
    adjointSynthesisState = FunctionSynthesisState::Needed;
    createEmptyPrimal();
    createEmptyAdjoint();
  }

  // If there is no VJP, synthesize it.
  if (!vjp)
    createVJP();
}

void DifferentiationTask::createEmptyPrimal() {
  assert(primalSynthesisState == FunctionSynthesisState::Needed);
  assert(!primalInfo);
  assert(!primal);

  auto indices = getIndices();
  auto *original = getOriginal();
  auto &module = context.getModule();
  std::string primalName =
      "AD__" + original->getName().str() + "__primal_" + mangleADIndices(indices);
  StructDecl *primalValueStructDecl = context.createPrimalValueStruct(this);
  primalInfo = std::unique_ptr<PrimalInfo>(new PrimalInfo(primalValueStructDecl, module));
  auto pvType = primalValueStructDecl->getDeclaredType()->getCanonicalType();
  auto objTy = SILType::getPrimitiveObjectType(pvType);
  auto resultConv = objTy.isLoadable(module) ? ResultConvention::Owned
                                             : ResultConvention::Indirect;
  auto origResults = original->getLoweredFunctionType()->getResults();
  SmallVector<SILResultInfo, 8> results;
  results.push_back({pvType, resultConv});
  results.append(origResults.begin(), origResults.end());
  // Create result info for checkpoints.
  auto originalTy = original->getLoweredFunctionType();
  auto primalTy = SILFunctionType::get(
      originalTy->getGenericSignature(), originalTy->getExtInfo(),
      originalTy->getCoroutineKind(), originalTy->getCalleeConvention(),
      originalTy->getParameters(), originalTy->getYields(), results,
      originalTy->getOptionalErrorResult(), context.getASTContext());
  SILOptFunctionBuilder fb(context.getTransform());
  auto linkage = original->getLinkage();
  if (linkage == SILLinkage::Public)
    linkage = SILLinkage::PublicNonABI;
  primal = fb.getOrCreateFunction(
      original->getLocation(), primalName, linkage, primalTy,
      original->isBare(), original->isTransparent(), original->isSerialized());
  primal->setUnqualifiedOwnership();
  LLVM_DEBUG(getADDebugStream() << "Primal function created \n"
                                << *primal << '\n');

  attr->setPrimalName(primal->getName());
}

void DifferentiationTask::createEmptyAdjoint() {
  assert(adjointSynthesisState == FunctionSynthesisState::Needed);
  assert(!adjoint);

  auto &module = context.getModule();
  auto *original = getOriginal();

  // RAII that pushes the original function's generic signature to
  // `module.Types` so that the calls `module.Types.getTypeLowering()` below
  // can understand the original function's generic parameter types.
  Lowering::GenericContextScope genericContextScope(
      module.Types, original->getLoweredFunctionType()->getGenericSignature());

  // Given a type, returns its formal SIL parameter info.
  auto getFormalParameterInfo = [&](CanType type) -> SILParameterInfo {
    auto &typeLowering = module.Types.getTypeLowering(type);
    ParameterConvention conv;
    if (typeLowering.isFormallyPassedIndirectly())
      conv = ParameterConvention::Indirect_In_Guaranteed;
    else if (typeLowering.isTrivial())
      conv = ParameterConvention::Direct_Unowned;
    else
      conv = ParameterConvention::Direct_Guaranteed;
    return {type, conv};
  };
  // Given a type, returns its formal SIL result info.
  auto getFormalResultInfo = [&](CanType type) -> SILResultInfo {
    auto &typeLowering = module.Types.getTypeLowering(type);
    ResultConvention conv;
    if (typeLowering.isFormallyReturnedIndirectly())
      conv = ResultConvention::Indirect;
    else if (typeLowering.isTrivial())
      conv = ResultConvention::Unowned;
    else
      conv = ResultConvention::Owned;
    return {type, conv};
  };

  // Parameters of the adjoint are:
  // - a seed,
  // - a primal value struct,
  // - original results, and
  // - the original parameters.
  // Results of the adjoint have the same type as the original parameters.
  SmallVector<SILParameterInfo, 8> adjParams;
  SmallVector<SILResultInfo, 8> adjResults;
  auto origTy = original->getLoweredFunctionType();
  auto origParams = origTy->getParameters();

  // Add adjoint parameter for the seed.
  adjParams.push_back(getFormalParameterInfo(
      origTy->getResults()[getIndices().source].getType()));

  // If there's a generated primal, accept a primal value struct in the adjoint
  // parameter list.
  if (auto *pi = getPrimalInfo()) {
    auto pvType = pi->getPrimalValueStruct()
                    ->getDeclaredInterfaceType()
                    ->getCanonicalType();
    adjParams.push_back(getFormalParameterInfo(pvType));
  }

  // Add adjoint parameters for the original results.
  for (auto &origRes : origTy->getResults())
    adjParams.push_back(getFormalParameterInfo(origRes.getType()));

  // Add adjoint parameters for the original parameters.
  for (auto &param : origParams)
    adjParams.push_back(param);

  // Add adjoint result for the wrt self parameter, if it was requested.
  auto selfParamIndex = origParams.size() - 1;
  if (origTy->hasSelfParam() &&
      getIndices().isWrtParameter(selfParamIndex))
    adjResults.push_back(getFormalResultInfo(
        origParams[selfParamIndex].getType()));

  // Add adjoint results for the requested non-self wrt parameters.
  for (auto i : getIndices().parameters.set_bits()) {
    if (origTy->hasSelfParam() && i == selfParamIndex)
      continue;
    adjResults.push_back(getFormalResultInfo(origParams[i].getType()));
  }

  auto adjName = "AD__" + original->getName().str() + "__adjoint_" +
                 mangleADIndices(getIndices());
  auto adjType = SILFunctionType::get(
      origTy->getGenericSignature(), origTy->getExtInfo(),
      origTy->getCoroutineKind(), origTy->getCalleeConvention(), adjParams, {},
      adjResults, None, original->getASTContext());
  SILOptFunctionBuilder fb(context.getTransform());
  auto linkage = original->getLinkage();
  if (linkage == SILLinkage::Public)
    linkage = SILLinkage::PublicNonABI;
  adjoint = fb.createFunction(linkage, adjName, adjType,
      original->getGenericEnvironment(), original->getLocation(),
      original->isBare(), original->isTransparent(), original->isSerialized());
  adjoint->setUnqualifiedOwnership();
  adjoint->setDebugScope(new (module)
                             SILDebugScope(original->getLocation(), adjoint));

  attr->setAdjointName(adjoint->getName(), /*primitive*/ false);
}

void DifferentiationTask::createVJP() {
  assert(!vjp);
  assert(adjoint);
  assert(primal);

  LLVM_DEBUG(llvm::dbgs() << "Creating VJP:\n"
                          << "  original type: "
                          << original->getLoweredFunctionType() << "\n"
                          << "  primal type: "
                          << primal->getLoweredFunctionType() << "\n"
                          << "  adjoint type: "
                          <<  adjoint->getLoweredFunctionType() << "\n");

  auto &module = context.getModule();
  auto originalTy = original->getLoweredFunctionType();

  // === Create an empty VJP. ===
  auto vjpName = "AD__" + original->getName().str() + "__vjp_" +
                 mangleADIndices(getIndices());
  auto vjpType = originalTy->getAutoDiffAssociatedFunctionType(
      getIndices().parameters, getIndices().source, 1,
      AutoDiffAssociatedFunctionKind::VJP, module,
      LookUpConformanceInModule(module.getSwiftModule()));

  SILOptFunctionBuilder fb(context.getTransform());
  auto linkage = original->getLinkage();
  if (linkage == SILLinkage::Public)
    linkage = SILLinkage::PublicNonABI;
  vjp = fb.createFunction(
      linkage, vjpName, vjpType, original->getGenericEnvironment(),
      original->getLocation(), original->isBare(), original->isTransparent(),
      original->isSerialized());
  vjp->setUnqualifiedOwnership();
  vjp->setDebugScope(new (module)
                         SILDebugScope(original->getLocation(), vjp));
  attr->setVJPName(vjp->getName());

  LLVM_DEBUG(llvm::dbgs() << "  vjp type: "
                          << vjp->getLoweredFunctionType() << "\n");

  // We'll use these conventions frequently.
  auto originalConv = original->getConventions();
  auto primalConv = primal->getConventions();

  // Validate signatures.
#ifndef NDEBUG
  auto adjointConv = adjoint->getConventions();
  auto vjpConv = vjp->getConventions();

  unsigned numOriginalParameters = originalConv.getNumParameters();
  unsigned numOriginalResults = originalConv.getResults().size();
  unsigned numCheckpoints = primalConv.getResults().size() - numOriginalResults;
  unsigned numSeeds = 1;
  unsigned numWrt = getIndices().parameters.count();

  LLVM_DEBUG(llvm::dbgs() << "  numOriginalParameters: "
                          << numOriginalParameters << "\n"
                          << "  numOriginalResults: "
                          << numOriginalResults << "\n"
                          << "  numCheckpoints: "
                          << numCheckpoints << "\n"
                          << "  numSeeds: "
                          << numSeeds << "\n");

  assert(primalConv.getNumParameters() == numOriginalParameters &&
         "unexpected number of primal parameters");
  assert(vjpConv.getNumParameters() == numOriginalParameters &&
         "unexpected number of vjp parameters");
  assert(vjpConv.getResults().size() == numOriginalResults + 1 &&
         "unexpected number of vjp results");
  assert(adjointConv.getNumParameters() ==
             numSeeds + numCheckpoints + numOriginalResults +
             numOriginalParameters &&
         "unexpected number of adjoint parameters");
  assert(adjointConv.getResults().size() == numWrt &&
         "unexpected number of adjoint results");

  // We assume that primal result conventions (for all results but the optional
  // checkpoints struct) match the vjp result conventions (for all results but
  // the pullback), so check that assumption.
  for (unsigned resultIndex : range(numOriginalResults)) {
    auto &primalResultInfo =
        primalConv.getResults()[numCheckpoints + resultIndex];
    auto &vjpResultInfo = vjpConv.getResults()[resultIndex];
    assert(primalResultInfo == vjpResultInfo &&
           "primal result info does not match vjp result info");
  }

  // We assume that the primal result conventions for checkpoints and original
  // results match the corresponding adjoint parameter conventions for
  // checkpoints and original results, so check that assumption.
  for (unsigned resultIndex : indices(primalConv.getResults())) {
    auto &primalResultInfo = primalConv.getResults()[resultIndex];
    auto &adjointParameterInfo =
        adjointConv.getParameters()[numSeeds + resultIndex];
    assert(primalResultInfo.isFormalIndirect() ==
               adjointParameterInfo.isFormalIndirect() &&
           "primal result directness does not match adjoint parameter "
           "directness");
    assert(primalResultInfo.getType() == adjointParameterInfo.getType() &&
           "primal result type does not match adjoint parameter type");
  }
#endif

  // Create the entry block with indirect results and parameters.
  auto *entry = vjp->createBasicBlock();
  createEntryArguments(vjp);
  SILBuilder builder(entry);
  auto loc = vjp->getLocation();

  // === Call primal with original arguments. ===
  SmallVector<SILValue, 8> primalArgs;

  // Tell the primal to put its indirect results in the vjp indirect result
  // buffers. This assumes that the primal indirect results are exactly the vjp
  // indirect results, an assumption that we check in assertions above.
  for (auto indRes : vjp->getIndirectResults())
    primalArgs.push_back(indRes);

  // Add original parameters.
  for (auto arg : vjp->getArgumentsWithoutIndirectResults())
    primalArgs.push_back(arg);

  // Get and call the primal.
  auto *primalRef = builder.createFunctionRef(loc, primal);
  auto *primalApply = builder.createApply(
      loc, primalRef, vjp->getForwardingSubstitutionMap(), primalArgs,
      /*isNonThrowing*/ false);

  // Collect the primal's direct results.
  SmallVector<SILValue, 8> primalDirectResults;
  if (primalConv.getNumDirectSILResults() == 1)
    primalDirectResults.push_back(primalApply);
  else {
    auto tupleSILTy = primalConv.getSILResultType();
    for (unsigned i : range(primalConv.getNumDirectSILResults())) {
      auto val = builder.createTupleExtract(loc, primalApply, i,
                                            tupleSILTy.getTupleElementType(i));
      primalDirectResults.push_back(val);
    }
  }

  // === Partially apply the adjoint. ===
  SmallVector<SILValue, 8> partialAdjointArgs;

  // Add primal values and original results.
  unsigned indResIdx = 0, dirResIdx = 0;
  for (auto &resInfo : primalConv.getResults()) {
    if (resInfo.isFormalDirect()) {
      // This assumes that the primal direct results correspond exactly to the
      // adjoint's direct parameters, an assumption that we check in assertions
      // above.
      partialAdjointArgs.push_back(primalDirectResults[dirResIdx++]);
    } else {
      // This assumes that the primal indirect results correspond exactly to the
      // adjoint's indirect parameters, an assumption that we check in
      // assertions above.
      partialAdjointArgs.push_back(primalArgs[indResIdx++]);
    }
  }

  // Add original parameters.
  for (auto arg : vjp->getArgumentsWithoutIndirectResults())
    partialAdjointArgs.push_back(arg);

  // Get and partially apply the adjoint.
  auto *adjointRef = builder.createFunctionRef(loc, adjoint);
  auto *adjointPartialApply = builder.createPartialApply(
      loc, adjointRef, vjp->getForwardingSubstitutionMap(), partialAdjointArgs,
      ParameterConvention::Direct_Guaranteed);

  // === Return the direct results. ===
  // (Note that indirect results have already been filled in by the application
  // of the primal).
  SmallVector<SILValue, 8> directResults;
  auto originalDirectResults = ArrayRef<SILValue>(primalDirectResults)
      .take_back(originalConv.getNumDirectSILResults());
  for (auto originalDirectResult : originalDirectResults)
    directResults.push_back(originalDirectResult);
  directResults.push_back(adjointPartialApply);
  if (directResults.size() > 1) {
    auto tupleRet = builder.createTuple(loc, directResults);
    builder.createReturn(loc, tupleRet);
  } else {
    builder.createReturn(loc, directResults[0]);
  }
}

//===----------------------------------------------------------------------===//
// Differentiation pass implementation
//===----------------------------------------------------------------------===//

/// Given a `gradient` instruction, find the corresponding differential operator
/// used in the AST. If no differential operator is found, return nullptr.
static ReverseAutoDiffExpr *findDifferentialOperator(GradientInst *inst) {
  return inst->getLoc().getAsASTNode<ReverseAutoDiffExpr>();
}

// Retrieve or create an empty gradient function based on a `gradient`
// instruction and replace all users of the `gradient` instruction with the
// gradient function. Returns the gradient function.
static SILFunction *lookUpOrSynthesizeGradient(ADContext &context,
                                               GradientInst *gradInst,
                                               SILFunction *original) {
  auto &module = original->getModule();
  auto &astCtx = module.getASTContext();
  auto origTy = original->getLoweredFunctionType();
  auto &config = gradInst->getConfig();

  // Creates a gradient function based on the configuration.
  auto createGradFunction = [&](const SILAutoDiffConfig &config) {
    auto gradType = origTy->getGradientType(config, module);
    std::string gradName =
        "AD__" + original->getName().str() + "__" + mangleADConfig(config);
    auto gradNameId = astCtx.getIdentifier(gradName);
    SILOptFunctionBuilder fb(context.getTransform());
    auto linkage = original->getLinkage();
    if (linkage == SILLinkage::Public)
      linkage = SILLinkage::PublicNonABI;
    auto *gradFn =
        fb.createFunction(linkage, gradNameId.str(), gradType,
                          original->getGenericEnvironment(),
                          original->getLocation(), original->isBare(),
                          original->isTransparent(), original->isSerialized());
    gradFn->setUnqualifiedOwnership();
    gradFn->setDebugScope(new (module)
                              SILDebugScope(original->getLocation(), gradFn));
    return gradFn;
  };

  // Find the canonical gradient.
  SILFunction *canonicalGrad = nullptr;
  // The master AD config corresponds to the canonical gradient.
  auto masterConfig = config.getWithCanonicalOptions();
  // If the canonical gradient already exists, we'll simply use it. No
  // differentiation is needed.
  if (auto *existingGrad = context.lookUpGradient({original, masterConfig}))
    canonicalGrad = existingGrad;
  // Otherwise, create a canonical gradient and enqueue a differentiation task.
  else {
    // Create a canonical gradient.
    canonicalGrad = createGradFunction(masterConfig);
    context.insertGradient({original, masterConfig}, canonicalGrad);
    // Enqueue a new differentiation task in the global context.
    if (auto *diffOp = findDifferentialOperator(gradInst))
      context.lookUpOrRegisterDifferentiationTask(
          original, config.indices, diffOp);
    else
      context.lookUpOrRegisterDifferentiationTask(
          original, config.indices, gradInst);
  }

  // If the requested gradient is not *both seedable and result-preserving*,
  // emit wrapper function, emit a call to the canonical gradient function
  // inside, and cache it. Otherwise, it's just the canonical gradient.
  SILFunction *gradFn = nullptr;
  if (config.isMaster())
    gradFn = canonicalGrad;
  else if (auto *existingGradFn = context.lookUpGradient({original, config}))
    gradFn = existingGradFn;
  else {
    gradFn = createGradFunction(config);
    // Create entry basic block.
    auto *entry = gradFn->createBasicBlock();
    createEntryArguments(gradFn);
    // Build a call to the canonical gradient function.
    SILBuilder builder(entry);
    auto loc = gradFn->getLocation();
    SILFunctionConventions gradConv(gradFn->getLoweredFunctionType(), module),
        origConv(origTy, module),
        canGradConv(canonicalGrad->getLoweredFunctionType(), module);
    SmallVector<SILValue, 8> args;
    SmallVector<SILValue, 1> stackAllocsToCleanUp;
    // Prepare arguments.
    // The first few arguments are the original arguments.
    for (auto arg : gradFn->getArguments())
      args.push_back(arg);
    // If it's not seedable, we need to create a default seed.
    if (!config.isSeedable()) {
      auto seedTy = origTy->getResults()[config.getSourceIndex()].getType();
      auto seedSILTy = SILType::getPrimitiveObjectType(seedTy);
      // Call `<seed type>.init(1)` to create a default
      // seed to feed into the canonical gradient.
      auto *seedBuf = builder.createAllocStack(loc, seedSILTy);
      createScalarValueIndirect(1, seedTy, seedBuf, loc, builder, context);
      // If seed is address only, we'll clean up the buffer after calling the
      // canonical gradient Otherwise, we just load the seed and deallocate the
      // buffer.
      if (seedSILTy.isAddressOnly(module)) {
        stackAllocsToCleanUp.push_back(seedBuf);
      } else {
        auto loq = getBufferLOQ(seedSILTy.getASTType(), *gradFn);
        auto seedBufAccess = builder.createBeginAccess(
            loc, seedBuf, SILAccessKind::Read, SILAccessEnforcement::Static,
            /*noNestedConflict*/ false, /*fromBuiltin=*/false);
        auto seed = builder.createLoad(loc, seedBufAccess, loq);
        builder.createEndAccess(loc, seedBufAccess, /*aborted*/ false);
        args.push_back(seed);
        builder.createDeallocStack(loc, seedBuf);
      }
    }
    // Call the canonical gradient function.
    // %0 = function_ref ...
    auto *canGradFnRef = builder.createFunctionRef(loc, canonicalGrad);
    SubstitutionMap subMap;
    if (auto *genEnv = gradFn->getGenericEnvironment())
      subMap = genEnv->getForwardingSubstitutionMap();
    // %1 = apply %0(...)
    auto *resultAndGrad = builder.createApply(loc, canGradFnRef, subMap, args,
                                              /*isNonThrowing*/ false);
    // Clean up stack allocations made by seed passing when seed is addr-only.
    for (auto alloc : stackAllocsToCleanUp)
      builder.createDeallocStack(loc, alloc);
    // If the config is result-preserving, or if all original results are
    // indirect, we can just return whatever direct results the canonical
    // gradient produces.
    if (config.isPreservingResult() || origConv.getNumDirectSILResults() == 0) {
      builder.createReturn(loc, resultAndGrad);
    }
    // Otherwise, return every tuple element of `resultAndGrad` except the
    // first. For this, we have to build a bunch of `tuple_extract`s and
    // re-gather them using `tuple`.
    else {
      unsigned numDirResults = canGradConv.getNumDirectSILResults();
      SILValue result;
      if (numDirResults == 2)
        result = builder.createTupleExtract(loc, resultAndGrad, 1);
      else {
        SmallVector<SILValue, 8> grads;
        for (unsigned i : range(1, numDirResults))
          grads.push_back(builder.createTupleExtract(loc, resultAndGrad, i));
        result = builder.createTuple(loc, gradConv.getSILResultType(), grads);
      }
      builder.createReturn(loc, result);
    }

    // Cache the newly created gradient wrapper (non-canonical).
    context.insertGradient({original, config}, gradFn);
  }

  return gradFn;
}

/// Finish the canonical gradient function.
///
/// For the following original function type:
///   (a1, a2, ..., an) -> r
///
/// The canonical gradient has the following type:
///   (a1, a2, ..., an, seed) -> (r, a1, a0, ..., an)
///
/// In the canonical gradient function, we simply call the primal and the
/// adjoint, and return a tuple of the original result and the gradient values.
/// If the adjoint provides more gradient values than we need, we use the
/// config to extract the ones we want.
static void fillCanonicalGradient(SILFunction &canGrad,
                                  const DifferentiationTask *task,
                                  const SILAutoDiffConfig &config,
                                  ADContext &context) {
  assert(canGrad.empty() && "The gradient function must be empty");
  auto &module = context.getModule();
  auto canGradTy = canGrad.getLoweredFunctionType();
  auto loc = canGrad.getLocation();
  auto *primal = task->getPrimal();
  assert(primal && "Primal does not exist?");
  auto primalTy = primal->getLoweredFunctionType();
  auto *adjoint = task->getAdjoint();
  assert(adjoint && "Adjoint does not exist?");
  auto adjointTy = adjoint->getLoweredFunctionType();
  SILFunctionConventions primalConv(primalTy, module),
      adjointConv(adjointTy, module), canGradConv(canGradTy, module);
  // Create an entry basic block.
  auto *entry = canGrad.createBasicBlock();
  createEntryArguments(&canGrad);
  // Initialize arguments.
  SILBuilder builder(entry);
  // Call primal with original arguments.
  SmallVector<SILValue, 8> stackAllocsToCleanUp;
  SmallVector<SILValue, 8> primalArgs;
  // Add indirect results.
  for (auto indResInfo : primalTy->getIndirectFormalResults()) {
    auto objTy = SILType::getPrimitiveObjectType(indResInfo.getType());
    auto resultBuf = builder.createAllocStack(loc, objTy);
    stackAllocsToCleanUp.push_back(resultBuf);
    primalArgs.push_back(resultBuf);
  }
  // Add original parameters. These are the canonical gradient's parameter
  // arguments except the seed, which is the last argument.
  for (auto arg : canGrad.getArgumentsWithoutIndirectResults().drop_back())
    primalArgs.push_back(arg);
  // %0 = function_ref @primal
  auto *primalRef = builder.createFunctionRef(loc, primal);
  // %1 = apply %0(...)
  auto *primalApply = builder.createApply(
      loc, primalRef, canGrad.getForwardingSubstitutionMap(), primalArgs,
      /*isNonThrowing*/ false);
  // Collect the primal's direct results.
  SmallVector<SILValue, 8> primalResults;
  if (primalConv.getNumDirectSILResults() == 1)
    primalResults.push_back(primalApply);
  else {
    auto tupleSILTy = primalConv.getSILResultType();
    for (unsigned i : range(primalConv.getNumDirectSILResults())) {
      auto val = builder.createTupleExtract(loc, primalApply, i,
                                            tupleSILTy.getTupleElementType(i));
      primalResults.push_back(val);
    }
  }
  // Call adjoint with original arguments, the checkpoints value and the seed.
  SmallVector<SILValue, 8> adjointArgs;
  // Add indirect results.
  for (auto indRes : canGrad.getIndirectResults())
    adjointArgs.push_back(indRes);
  // Add seed.
  adjointArgs.push_back(canGrad.getArguments().back());
  // Add primal values and the original result (all returned by primal).
  unsigned indResIdx = 0, dirResIdx = 0;
  for (auto &resInfo : primalConv.getResults())
    adjointArgs.push_back(resInfo.isFormalDirect() ? primalResults[dirResIdx++]
                                                   : primalArgs[indResIdx++]);
  // Add original parameters. These are the canonical gradient's parameters
  // except the seed, which is the last.
  for (auto arg : canGrad.getArgumentsWithoutIndirectResults().drop_back())
    adjointArgs.push_back(arg);
  // %2 = function_ref @adjoint
  auto *adjRef = builder.createFunctionRef(loc, adjoint);
  // %3 = apply %2(...)
  auto *adjApply =
      builder.createApply(loc, adjRef, canGrad.getForwardingSubstitutionMap(),
                          adjointArgs, /*isNonThrowing*/ false);
  // Release primal results. This includes primal values and original results.
  for (unsigned i : indices(primalResults))
    if (primalTy->getResults()[i].getConvention() == ResultConvention::Owned)
      builder.createReleaseValue(loc, primalResults[i],
                                 builder.getDefaultAtomicity());
  // Clean up stack allocations.
  for (auto val : reversed(stackAllocsToCleanUp))
    builder.createDeallocStack(loc, val);
  // Return the original result and the adjoint result as a tuple. If either one
  // of the primal or the adjoint returns a tuple, join them in a flat tuple.
  SmallVector<SILValue, 8> directResults;
  // If the original result is a direct return, add it to the direct return list
  // of the canonical gradient.
  if (primalConv.getResults().back().isFormalDirect())
    directResults.push_back(primalResults.back());
  // Add the adjoint's results to the direct return list.
  // If the adjoint returns one result, it's the subset we're looking for.
  if (adjointConv.getNumDirectSILResults() == 1)
    directResults.push_back(adjApply);
  else {
    auto tupleSILTy = adjApply->getType();
    for (unsigned i : range(adjointConv.getNumDirectSILResults())) {
      if (config.indices.isWrtParameter(i)) {
        auto val = builder.createTupleExtract(
          loc, adjApply, i, tupleSILTy.getTupleElementType(i));
        directResults.push_back(val);
      }
    }
  }
  // Return these results as a tuple.
  auto tupleRet =
      builder.createTuple(loc, canGradConv.getSILResultType(), directResults);
  builder.createReturn(loc, tupleRet);
}

/// The automatic differentiation pass.
namespace {
class Differentiation : public SILModuleTransform {
public:
  Differentiation() : SILModuleTransform() {}
  void run() override;

private:
  /// Processes the given gradient instruction. This includes transforming it
  /// into a normal `function_ref` to the synthesized gradient function, and
  /// pushing a differentiation task onto the global list. Returns true if any
  /// error occurred.
  bool processGradientInst(GradientInst *gi, ADContext &context);
};
} // end anonymous namespace

bool Differentiation::processGradientInst(GradientInst *gi,
                                          ADContext &context) {
  SILFunction *parent = gi->getFunction();
  auto operand = gi->getOperand(0);
  SILFunction *gradFn = nullptr;
  // If it traces back to a `function_ref`, differentiate that.
  if (auto *originalFRI = findReferenceToVisibleFunction(operand)) {
    auto *original = originalFRI->getReferencedFunction();
    gradFn = lookUpOrSynthesizeGradient(context, gi, original);

    // Replace the `gradient` instruction with the reference to the specified
    // function.
    SILBuilder builder(gi);
    auto loc = parent->getLocation();
    SILValue gradRef = builder.createFunctionRef(loc, gradFn);
    // Traverse from the `gradient` instruction to the original
    // `function_ref`. If there's any function convertion, do the same
    // convertion for the gradient.
    auto convertedGradFn = reapplyFunctionConversion(
        gradRef, originalFRI, gi->getOriginal(), builder, loc);
    // Replace uses of the `gradient` instruction with the converted (if
    // necessary) gradient function value.
    gi->replaceAllUsesWith(convertedGradFn);
  }
  // Differentiating opaque functions is not supported yet.
  else {
    // Find the original differential operator expression. Show an error at the
    // operator, highlight the argument, and show a note at the definition site
    // of the argument.
    if (auto *expr = findDifferentialOperator(gi))
      context
          .diagnose(expr->getLoc(), diag::autodiff_opaque_function_unsupported)
          .highlight(expr->getOriginalExpr()->getSourceRange());
    else
      context.diagnose(gi->getLoc().getSourceLoc(),
                       diag::autodiff_opaque_function_unsupported);
    // Emit a note at the definition site.
    context.diagnose(gi->getOriginal().getLoc().getSourceLoc(),
                     diag::autodiff_value_defined_here);
    return true;
  }
  // We invalidate analyses on the parent function because the `gradient`
  // instruction is transformed.
  PM->invalidateAnalysis(parent, SILAnalysis::InvalidationKind::FunctionBody);
  return false;
}

/// AD pass entry.
void Differentiation::run() {
  auto &module = *getModule();
  auto &astCtx = module.getASTContext();
  debugDump(module);

  // Collect [differentiable] attributes and gradient instructions to process.
  SmallVector<std::pair<SILFunction *,
                        SILDifferentiableAttr *>, 8> diffAttrs;
  SmallVector<GradientInst *, 16> gradInsts;
  // Handle each `gradient` instruction and each `differentiable`
  // attribute in the module.
  for (SILFunction &f : module) {
    // If `f` has a `[differentiable]` attribute, it should become a
    // differentiation task.
    for (auto *diffAttr : f.getDifferentiableAttrs()) {
      if (diffAttr->hasPrimal() == diffAttr->hasAdjoint()) {
        diffAttrs.push_back({&f, diffAttr});
        continue;
      }
      // If only primal or adjoint is specified, it's an incomplete attribute.
      astCtx.Diags.diagnose(f.getLocation().getSourceLoc(),
                            diag::autodiff_incomplete_differentiable_attr);
    }
    // Not look for `gradient` instructions in transparent functions because
    // they are to be inlined.
    if (f.isTransparent())
      continue;
    for (SILBasicBlock &bb : f) {
      for (SILInstruction &i : bb) {
        // If `i` is a `gradient` instruction, i.e. the SIL-level differential
        // operator, push it to the work list.
        if (auto *gi = dyn_cast<GradientInst>(&i))
          gradInsts.push_back(gi);
      }
    }
  }

  // If there's no `gradient` instruction or no `[differentiable]` attributes,
  // there's no AD to do.
  if (gradInsts.empty() && diffAttrs.empty())
    return;

  // AD relies on stdlib (the Swift module). If it's not imported, it's an
  // internal error.
  if (!astCtx.getStdlibModule()) {
    astCtx.Diags.diagnose(SourceLoc(),
                          diag::autodiff_internal_swift_not_imported);
    return;
  }

  // A global differentiation context.
  ADContext context(*this);

  // For every `[differentiable]` attribute, create a differentiation task. If
  // the attribute has a primal and adjoint, this task will not synthesize
  // anything, but it's still needed as a lookup target.
  for (auto &fnAndAttr : diffAttrs) {
    context.registerDifferentiationTask(
        fnAndAttr.first, fnAndAttr.second->getIndices(),
        DifferentiationInvoker(fnAndAttr.second, fnAndAttr.first));
  }

  // Lower each gradient instruction to a function reference and replaces its
  // uses with a function reference to its gradient.
  //
  // If the operand to the instruction traces back to a function reference that
  // the compiler can see into, then we do further processing, i.e. retrieving
  // or creating its gradient. Otherwise, it's differentiating an opaque
  // function whose body isn't visible to the compiler. We don't have
  // infrastructure support for this yet and currently it'll error out, but
  // we'll look into adding a new function convention so that the primal and the
  // adjoint can be passed along with the function.
  //
  // If any error occurs during `gradient` instruction processing, it won't be
  // turned into a differentiation task. But we don't back out just yet - primal
  // synthesis and adjoint synthesis for newly created differentiation tasks
  // should still run because they will diagnose more errors.
  bool errorProcessingGradInsts = false;
  for (auto *gi : gradInsts)
    errorProcessingGradInsts |= processGradientInst(gi, context);

  // Run primal generation for newly created differentiation tasks. If any error
  // occurs, back out.
  PrimalGen primalGen(context);
  if (primalGen.run())
    return;

  // Run adjoint generation for differentiation tasks. If any error occurs, back
  // out.
  AdjointGen adjointGen(context);
  if (adjointGen.run())
    return;

  // If there was any error that occurred during `gradient` instruction
  // processing, back out.
  if (errorProcessingGradInsts)
    return;

  // Fill the body of each empty canonical gradient function.
  for (auto &canonicalGrad : context.getCanonicalGradients()) {
    auto *original = canonicalGrad.first.first;
    auto config = canonicalGrad.first.second;
    auto *canGradFn = canonicalGrad.second;
    auto *task = context.lookUpMinimalDifferentiationTask(
        original, config.indices);
    fillCanonicalGradient(*canGradFn, task, config, context);
  }

  // Remove all remaining `gradient` instructions.
  for (auto *gi : gradInsts)
    recursivelyDeleteTriviallyDeadInstructions(gi);

  LLVM_DEBUG(getADDebugStream() << "All differentiation finished\n");
}

//===----------------------------------------------------------------------===//
// Pass creation
//===----------------------------------------------------------------------===//

SILTransform *swift::createDifferentiation() {
  return new Differentiation;
}
