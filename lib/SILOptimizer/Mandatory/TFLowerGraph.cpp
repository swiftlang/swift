//===--- TFLowerGraph.cpp - Lower a TensorFlow graph to GraphDef ----------===//
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
// This pass creates a TensorFlow graph for the specified computation and then
// returns it as an encoded binary protobuf.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "tf-lower-graph"
#include "TFUtilities.h"
#include "llvm/Support/CommandLine.h"
#ifdef SWIFT_ENABLE_TENSORFLOW
#include "TFCanonicalizeCFG.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Module.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/GraphFunctionDeviceInfo.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "tensorflow/c/c_api.h"
#include "tensorflow/c/c_api_experimental.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#endif
using namespace swift;
using namespace tf;

static llvm::cl::opt<bool>
    TFDumpGraph("tf-dump-graph", llvm::cl::init(false),
                llvm::cl::desc("Dump generated tensorflow graphs to /tmp"));

#ifdef SWIFT_ENABLE_TENSORFLOW
template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &ctx, SILLocation loc,
                                   Diag<T...> diag, U &&... args) {
  auto &diags = ctx.Diags;
  return diags.diagnose(loc.getSourceLoc(), diag, std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(SILFunction &fn, SILLocation loc,
                                   Diag<T...> diag, U &&... args) {
  return diagnose(fn.getASTContext(), loc, diag, std::forward<U>(args)...);
}

static const char DEVICE_TPU_REPLICATED_CORE[] = "TPU_REPLICATED_CORE";
static const char TPU_CLUSTER_ATTR_VALUE[] = "TPUReplicate/cluster";
// Set a small number to exercise the bounded queue capacity more, increasing
// test coverage.
// FIXME: Tune the default value for performance, and/or make it configurable.
static const int NAMED_TENSOR_QUEUE_CAPACITY = 1;

// The send device incarnation for TF sends/recvs.
// FIXME: revisit whether using a fixed value is good enough.
static const int DEVICE_INCARNATION_ID = 1;

/// When generating a TF TPU graph, call this function to place an eligible TF
/// graph node onto TPU device. Some nodes such as Placeholder and
/// Dataset/Iterator nodes are not eligible for TPU.
static void markNodeAsTPUReplicated(TF_OperationDescription *desc) {
  static const char *const TPU_CLUSTER_ATTR_NAME = "_tpu_replicate";
  TF_SetAttrString(desc, TPU_CLUSTER_ATTR_NAME, TPU_CLUSTER_ATTR_VALUE,
                   strlen(TPU_CLUSTER_ATTR_VALUE));
}

namespace {
/// Each op is represented by a single SILValue, but we then also have
/// tuple_extract values that project multiple-result values out.  A
/// SILOpResult holds this, allowing us to refer to each result of a
/// multi-output op.
///
/// FIXME: Eliminate this when GraphOperationInst has taken over the world.
///
typedef std::pair<SILValue, unsigned> SILOpResult;

/// As we lower SIL instructions to tensorflow graph nodes, we traverse the
/// SESE region tree.  Nodes that produce while loops and conditions turn into
/// scopes.
typedef llvm::ScopedHashTable<SILOpResult, std::pair<TF_Output, unsigned>>
    ValueMappingScopedHashTable;

/// This represents a single input parameter to a graph function.  In addition
/// to representing the Parameter node in the graph function itself, it also
/// knows the actual value to be passed in to fulfill that parameter from the
/// enclosing scope.
struct GraphFunctionInput {
  /// This is the Parameter node inside of the graph function.
  TF_Output parameter;

  /// This is the value passed into the function from the enclosing scope.  If
  /// this is the top level graph function, then the passed values are null.
  TF_Output passedValue;

  /// This is the SILValue that the parameter corresponds to.
  SILOpResult value;
};

/// This represents a TensorFlow TF_Function that is being constructed,
/// including its inputs, outputs, live-in values used by the loop and the
/// ops created that make it up.
struct GraphFunctionBody {
  const DeviceType thisDeviceType;
  const GraphFunctionDeviceInfo &deviceInfo;

  /// This is the temporary Graph we use to build up the body of this
  /// function.  When we're done building the graph, we transform it into a
  /// graph function, then copy it out into a resultGraph, deleting both the
  /// temporary graph and the temporary graph function.
  ///
  std::unique_ptr<TF_Graph, decltype(&TF_DeleteGraph)> graph;

  /// These are inputs to the graph function.
  SmallVector<GraphFunctionInput, 4> inputs;

  /// These are outputs from the function, and the SILArgument* (if non-null)
  /// specifies which SILArgument the output corresponds to.  This gets filled
  /// in in a few different ways:
  ///  1) If this is the top level of the function, then this is filled in by
  ///     a return instruction and the SILArgument*'s are null.
  ///  2) In a conditional region, this is filled in by a BranchInst to a
  ///     merge point and the SILArgument's indicate which BB arguments are
  ///     provided by the branch.
  ///  3) The condition function for a While region has one output (with a
  ///     null SILArgument) corresponding to the boolean result of the
  ///     function.
  ///  4) The body function for a While region has an output for each
  ///     SILArgument in the loop header block, and also has outputs for the
  ///     live-in values used within the loop (with no SILArgument specified).
  SmallVector<std::pair<SILArgument *, TF_Output>, 4> outputs;

  /// If this graph has any side-effecting operations, this is the most
  /// recently emitted operation that had side effects, and this operation
  /// should get executed before the function returns.  Otherwise, it is null.
  TF_Operation *controlDependenceValue = nullptr;

  /// If true, this graph function contains some side-effecting ops.
  bool funcHasSideEffects = false;

  /// This is a list of all of the operations that make up this function.
  std::vector<const TF_Operation *> operations;

  // When true, lower effectful ops (e.g. TF->Swift send ops), if any, in the
  // corresponding TF function. Currently in a While op context, these ops
  // should not be run in the cond function.
  // TODO(b/78472806): Add a more thorough and proper fix for effectful ops in
  // the cond function.
  bool shouldLowerEffectfulOps = true;

public:
  GraphFunctionBody(DeviceType thisDeviceType,
                    const GraphFunctionDeviceInfo &deviceInfo)
      : thisDeviceType(thisDeviceType), deviceInfo(deviceInfo),
        graph(TF_NewGraph(), &TF_DeleteGraph) {}

  TF_Graph *getGraph() const { return graph.get(); }

  /// "Finish" a tensorflow op under construction, and remember that it is
  /// part of this graph function.
  TF_Operation *finishOp(TF_OperationDescription *desc, bool opHasSideEffects,
                         bool isEligibleForTPU, TF_Status *status) {
    funcHasSideEffects |= opHasSideEffects;
    // If this node has side effects and we've already emitted another node
    // that does, make sure to connect them with control dependencies to
    // preserve ordering.
    if (opHasSideEffects && controlDependenceValue)
      TF_AddControlInput(desc, controlDependenceValue);

    // If this node should be put onto TPU, mark it with an attribute.
    if (thisDeviceType == DeviceType::TPU && isEligibleForTPU) {
      markNodeAsTPUReplicated(desc);
    }

    auto result = TF_FinishOperation(desc, status);
    operations.push_back(result);

    // If this op has side effects, remember it in case we need to chain it
    // to another one later.
    if (opHasSideEffects)
      controlDependenceValue = result;

    return result;
  }

  // If there is a control dependence value, run it before producing an output
  // tensor in GraphFunctionBody.
  TF_Output maybeRunEffectfulOp(TF_Output result, TF_Status *status) {
    if (!controlDependenceValue)
      return result;

    std::string nodeName = "RunControlDependency";
    auto *desc = TF_NewOperation(getGraph(), "Identity", nodeName.c_str());
    TF_AddControlInput(desc, controlDependenceValue);
    TF_AddInput(desc, result);
    TF_Operation *newResult = finishOp(desc, /*opHasSideEffects*/ false,
                                       /*isEligibleForTPU*/ false, status);
    controlDependenceValue = nullptr;
    return {newResult, 0};
  }
};
} // namespace

namespace {
/// Status in the process of graph lowering. This is used for
/// error propagation and handling in the TFGraphLowering class.
enum class GLStatus {
  Success,
  Error,
};

struct TFGraphFunctionLowering
    : public SILInstructionVisitor<TFGraphFunctionLowering, GLStatus> {
  SILFunction &SILFn;
  // The TF device to which the generated graph is targeting.
  const DeviceType thisDeviceType;
  const std::string thisDeviceTypeStr;
  const GraphFunctionDeviceInfo &deviceInfo;
  const std::string &funcNodeBaseName;
  llvm::DenseMap<StringRef, std::unique_ptr<LoweredGraphFunction>>
      &graphFunctions;
  TF_Graph *resultGraph;
  TF_Status *status;

  /// This is a stack of the currently active TF_Function's that are being
  /// constructed.  Nodes that are created are added to the innermost function,
  /// and we use this to keep track of live-in values and other state.
  std::vector<GraphFunctionBody> functionStack;

  // As we walk the SESE region tree, we expect each referenced value to have
  // already been lowered.  However, it may be in an outer loop nest.  As such,
  // this scoped hash table keeps track of which tensorflow graph node result
  // a value corresponds to, along with the scope ID of the value.
  ValueMappingScopedHashTable valueMapping;

  // Track those tensor ids that have been lowered to graph ops respectively for
  // TF->Swift tensor sends and receives.
  llvm::SmallSet<int, 4> processedTensorIdsForSend;
  llvm::SmallSet<int, 4> processedTensorIdsForReceive;

  /// Mapping from declarations to the number to times a TF_Function took the
  /// name from the declaration. This will be used in `getUniqueName` to produce
  /// uniqued graph node names.
  llvm::SmallDenseMap<ValueDecl *, unsigned> uniqueNames;
  /// To be used to generate unique undef names.
  llvm::SmallDenseMap<SILType, unsigned> uniqueUndefNames;

  /// The set of graph functions that the generated graph use as
  /// function-typed attributes, but their definitions are not yet available,
  /// and should be provided later.
  SmallVectorImpl<std::pair<StringRef, SILLocation>> &pendingGraphFnNames;

public:
  /// Generate one or more TF graph functions from `fn` targeting
  /// `thisDeviceType`, and add them to `resultGraph`.
  TFGraphFunctionLowering(
      SILFunction &fn, DeviceType thisDeviceType,
      const GraphFunctionDeviceInfo &deviceInfo,
      const std::string &funcNodeBaseName,
      llvm::DenseMap<StringRef, std::unique_ptr<LoweredGraphFunction>>
          &graphFunctions,
      TF_Graph *resultGraph,
      SmallVectorImpl<std::pair<StringRef, SILLocation>> &pendingGraphFnNames,
      TF_Status *status)
      : SILFn(fn), thisDeviceType(thisDeviceType),
        thisDeviceTypeStr(getDeviceString(thisDeviceType)),
        deviceInfo(deviceInfo), funcNodeBaseName(funcNodeBaseName),
        graphFunctions(graphFunctions), resultGraph(resultGraph),
        status(status), pendingGraphFnNames(pendingGraphFnNames) {}

  /// Return the current graph function that is being set up.
  GraphFunctionBody &getCurrentGraphFunction() { return functionStack.back(); }

  ~TFGraphFunctionLowering() {}

  /// Check whether the specified TensorFlow status object is valid or not.  If
  /// valid return false.  If invalid, emit a diagnostic and return true.
  bool checkStatus(SILLocation loc,
                   Diag<StringRef> id = diag::tf_lowering_error) {
    if (TF_GetCode(status) == TF_OK)
      return false;
    internalError(loc, TF_Message(status), id);
    return true;
  }

  void internalError(SILLocation loc, std::string message,
                     Diag<StringRef> id = diag::tf_lowering_error) {
    diagnose(SILFn, loc, id, message);
  }

  /// Given a GraphFunctionBody, which encapsulates all the information
  /// necessary to represent a tensorflow TF_Function, perform the final steps
  /// to generate the TF_Function itself and put it into the resultGraph.
  ///
  /// Also populate `inputTypes` and `outputTypes` based on the input and result
  /// parameters of the function, if they are non-NULL.
  ///
  /// When `isTopLevelFunction` is true, also adds a set of graph nodes to
  /// support calling that function.
  ///
  /// Note: `name` starts with _, where the name of a graph node cannot start
  /// with _.
  ///
  /// `funcHasSideEffects` is set to true if the body of the graph function has
  /// any side effecting operations.  This allows the caller to this graph
  /// function to wire up control dependence edges properly. In particular, the
  /// graph node that runs this function in the call-site is treated as a
  /// side-effecting node, and will be threaded via control edges with other
  /// side-effecting nodes in the caller. This is true unless we are building a
  /// top level graph function, in which case we ignore `funcHasSideEffects`, as
  /// the top level function gets called in a `TF_SessionRun()` call.
  ///
  /// This emits an error and returns true on error.
  bool buildGraphFunction(const GraphFunctionBody &graphBody, StringRef name,
                          bool &funcHasSideEffects,
                          SmallVectorImpl<TF_DataType> *inputTypes,
                          SmallVectorImpl<TF_DataType> *outputTypes);

  /// Builds TF graph nodes for the top level TF function call, where
  /// `funcOpType` is the TF graph function name, and `funcNodeBaseName` is the
  /// node base name for calling that function. `inputs` and `outputs`
  /// respectively specify the inputs and outputs to the
  /// function. `metadataNodeForTPU` is a graph node needed when we target TPU
  /// -- if it's NULL, it'll be set to one such node that's created in the call;
  /// otherwise no such node is created.
  ///
  /// Context of `isPrimaryFn`: An accelerator SIL function produced by
  /// TFPartition is to be partitioned into a set of N SIL functions, one per
  /// device, if the graph execution involves N TF devices. One of the functions
  /// is the *primary* function, whose device is specified via a configure stmt
  /// like Tensorflow.enableGPU() or enableTPU() (defaulting to CPU if there is
  /// no such stmt), and the other functions are the *helper" functions, which
  /// will be executed together with the primary one, for cross-device
  /// sends/recvs. The helper functions do not take input or output tensors, and
  /// are executed for their side-effects (e.g. sending/receiving tensors across
  /// devices).
  ///
  /// The created nodes follow this naming convention shared with the runtime:
  /// - The graph node for the function call has op type set <funcOpType>, and
  ///   name set to tfc_func_<funcNodeBaseName> for the primary function, and
  ///   tfc_func_<funcNodeBaseName>_helper_{i} for the i-th helper function.
  /// - The i-th input node is named tfc_input_{i}_<funcNodeBaseName>
  /// - The j-th output node is named tfc_output_{j}_<funcNodeBaseName>
  /// - If TPU infeed is enabled, the graph contains an 'InfeedEnqueueTuple'
  ///   node for caller feed input tensors.
  ///
  /// Additional graph nodes may be added for TPU graph creation, but they are
  /// implementation details that runtime need not be aware of.
  ///
  /// This emits an error and returns true on error.
  bool buildGraphNodesForTopLevelFunctionCall(
      StringRef funcOpType, bool isPrimaryFn, ArrayRef<TF_DataType> inputTypes,
      ArrayRef<TF_DataType> outputTypes, TF_Operation *&metadataNodeForTPU);

private: // Helpers to create TensorFlow graph nodes.
  unsigned OpID = 0;
  llvm::StringSet<> usedOpNames;

  /// Adds TPU config-related nodes to the graph, and sets `*metadataNode` to
  /// the created TPUReplicateMetadata node.
  ///
  /// This emits an error and returns true on error.
  bool addTopLevelTPUConfigLogic(TF_Operation **metadataNode);

public: // Lowering functionality.
  std::string getUniqueName(SILDebugLocation loc, const char *baseName);

  TF_DataType getTensorFlowDataType(SILType type, SILLocation loc);

  TF_Output createParameter(SILOpResult value, TF_Output passedValue,
                            GraphFunctionBody &fn);

  /// Add an available value for the specified SILOpResult (a SILValue+result#)
  /// to the value mapping.  This defaults to setting its scope to the current
  /// function depth, but can be customized, so long as the value isn't added
  /// "under" an existing value.
  void addValueMapping(SILOpResult value, TF_Output result,
                       unsigned depth = ~0U) {
    // If an explicit depth isn't specified, then this is added to the current
    // function.
    ValueMappingScopedHashTable::ScopeTy *scope = valueMapping.getCurScope();
    if (depth == ~0U) {
      depth = functionStack.size() - 1;
    } else {
      // This isn't particularly efficient, but this happens infrequently and
      // the scope stack should be shallow.
      SmallVector<ValueMappingScopedHashTable::ScopeTy *, 4> scopes;
      for (auto tmp = scope; tmp != nullptr; tmp = tmp->getParentScope())
        scopes.push_back(tmp);
      scope = scopes[scopes.size() - 1 - depth];
    }

#ifndef NDEBUG
    auto it = valueMapping.begin(value);
    assert((it == valueMapping.end() || it->second < depth) &&
           "value introduced multiple times in the same scope");
#endif
    valueMapping.insertIntoScope(scope, value, {result, depth});
  }

  /// Return the TensorFlow operand for the specified value.  Note that this can
  /// return a null value if an error occurred lowering the operand in question.
  /// On error, return a "NULL" value, and emit an error diagnostic.
  TF_Output getOperandValue(SILValue v) {
    // ops can produce multiple values, and we support tuple_extract
    // instructions that access them.  Check to see if this is referring to a
    // tuple extract instruction, and if so, get the value it is using.
    unsigned idx = 0;
    if (auto *tei = dyn_cast<TupleExtractInst>(v)) {
      v = tei->getOperand();
      idx = tei->getFieldNo();
    } else {
      assert(!v->getType().is<TupleType>() &&
             "Directly referring to multiple result value!");
    }
    return getOperandValue({v, idx});
  }

  /// Return the TensorFlow operand for the specified SILOpResult (a
  /// SILValue+result #).  Note that this can return a null value if an error
  /// occurred lowering the operand in question.
  /// On error, return a "NULL" value, and emit an error diagnostic.
  TF_Output getOperandValue(SILOpResult v) {
    assert(!isa<SILUndef>(v.first) && "Undef value found during graph lowering!");

    std::pair<TF_Output, unsigned> valueInfo = valueMapping.lookup(v);
    assert(valueInfo.first.oper != nullptr && "didn't find live-in value?");

    auto value = valueInfo.first;

    // If the value was from an outer while loop / function, we need to add it
    // as a parameter to all of the inner functions as well, so they can each
    // pass it down to this use point.  Consider the use inside the loop in this
    // example:
    //    x = ...
    //    while
    //      while
    //        x+42
    //
    // When the node for 'x' is requested inside the two while loops, this will
    // add x as parameters to the two while loops so it is available inside.
    //
    for (unsigned depth = valueInfo.second + 1; depth != functionStack.size();
         ++depth) {
      // Create placeholder, add it as input to each function.
      value = createParameter(v, value, functionStack[depth]);
      if (value.oper == nullptr)
        return {};

      // Remember that it is the available version of this value at that depth.
      addValueMapping(v, value, depth);
    }

    return value;
  }

  // These get special handling, they are only used as operands to tfops.
  GLStatus visitIntegerLiteralInst(IntegerLiteralInst *inst) {
    return GLStatus::Success;
  }
  GLStatus visitFloatLiteralInst(FloatLiteralInst *inst) {
    return GLStatus::Success;
  }
  GLStatus visitMetatypeInst(MetatypeInst *inst) { return GLStatus::Success; }
  GLStatus visitStringLiteralInst(StringLiteralInst *inst) {
    return GLStatus::Success;
  }
  GLStatus visitFunctionRefInst(FunctionRefInst *inst) {
    return GLStatus::Success;
  }

  GLStatus visitGraphOperationInst(GraphOperationInst *inst);

  GLStatus visitTupleInst(TupleInst *inst);
  GLStatus visitUncheckedRefCastInst(UncheckedRefCastInst *inst);

  GLStatus visitReturnInst(ReturnInst *inst);
  GLStatus visitBranchInst(BranchInst *inst);

  // visitSILInstruction is the bottom level of the instruction visitor, where
  // unhandled instructions bottom out in.
  GLStatus visitSILInstruction(SILInstruction *inst) {
    internalError(inst->getLoc(), "GraphGen cannot lower this instruction yet");
    llvm::errs() << "Unhandled SIL instruction in TFGraphFunctionLowering:\n";
    inst->dump();
    return GLStatus::Error;
  }

  GraphFunctionBody lowerToFunction(const std::function<void()> &body);

  GLStatus lowerArgumentsToParams(ArrayRef<SILArgument *> args,
                                  ArrayRef<TF_Output> passedValues,
                                  SILLocation loc);

  GLStatus lowerBasicBlock(SILBasicBlock *bb, bool skipTerminator = false);
  GLStatus lowerRegion(SESERegionTree *region);
  GLStatus lowerSequenceRegion(SequenceSESERegion *r);
  GLStatus lowerSharedRegion(SharedSESERegion *r);
  GLStatus lowerWhileLoopRegion(WhileLoopSESERegion *r);
  GLStatus lowerConditionalRegion(ConditionalSESERegion *r);

private: // Helpers for lowering.
  GLStatus visitGraphOpSendToHostInst(GraphOperationInfo &graphOpInfo);
  GLStatus visitGraphOpRecvFromHostInst(GraphOperationInfo &graphOpInfo);
  // D2D means device-to-device.
  GLStatus visitGraphOpD2DTensorRecvInst(GraphOperationInfo &graphOpInfo);
  GLStatus visitGraphOpD2DTensorSendInst(GraphOperationInfo &graphOpInfo);

  // Helper functions to add different flavors of send/recv TF ops.
  GLStatus addTFRecvOp(const SILInstruction *inst, int transferId,
                       StringRef srcDevice);
  GLStatus addTFSendOp(const SILInstruction *inst, int transferId,
                       StringRef destDevice);
  // For the TPU infeed/outfeed related ops, the shape array of the tensor being
  // transferred is given by `dims`, `numDims` and `dimPtrs`.
  GLStatus addTPUDequeueOp(const SILInstruction *inst, bool isInfeed,
                           int transferId, ArrayRef<int64_t> dims,
                           ArrayRef<int> numDims, ArrayRef<int64_t *> dimPtrs);
  GLStatus addTPUEnqueueOp(const SILInstruction *inst, bool isInfeed,
                           int transferId, ArrayRef<int64_t> dims,
                           ArrayRef<int> numDims, ArrayRef<int64_t *> dimPtrs);

  // For `op` with `opName` under construction, set a function-typed attribute
  // with a graph function name derived from `silFuncName` under the following
  // naming convention: Let `silFuncName` be "$foo", then the corresponding
  // graph function name is "foo.tf_only".
  void handleFunctionAttribute(TF_OperationDescription *op,
                               const std::string &opName, SILLocation loc,
                               StringRef silFuncName);
};
} // namespace

// Escape the specified string to be a valid TensorFlow OpDef name.
// Op names must match the regex "[A-Za-z0-9.][A-Za-z0-9_./]*".
static void escapeOpName(std::string &name) {
  // Currently, invalid characters are simply replaced with underscores.
  // TODO: Use a more robust escaping transformation. It should handle unicode
  // characters (using llvm::UTF8 or some other means) and be reversible.
  for (auto i : indices(name)) {
    char &c = name[i];
    if (!llvm::isAlnum(c) && c != '.')
      if (i == 0 || (c != '_' && c != '/'))
        c = '_';
  }
}

/// Given a DeclName, returns an escaped (TF-compatible)
/// name that replaces parentheses with '.' and colons with '_', for example:
/// `foo(x:y:z:)` -> `foo.x_y_z_.`.
static std::string escapeDeclName(DeclName name) {
  SmallVector<char, 8> buffer;
  auto newName = name.getString(buffer, /*skipEmptyArgumentNames*/ true);
  for (char &c : buffer) {
    if (c == '(' || c == ')')
      c = '.';
    else if (!llvm::isAlnum(c))
      c = '_';
  }
  return newName.str();
}

/// Produce a "stack trace" for the specified location, producing it in a form
/// that we can use as a unique op name.
std::string TFGraphFunctionLowering::getUniqueName(SILDebugLocation loc,
                                                   const char *baseName) {
  std::string name = baseName;

  // Skip over internal implementation details of the Tensor library.
  loc = skipInternalLocations(loc);

  auto &SM = SILFn.getASTContext().SourceMgr;

  // Form a name for this op based on the user's source location and "stack
  // trace" of where it got inlined in user code.  We use the form
  // "file:line:col".
  //
  // FIXME: InlinedCallSite is always nullptr even if we use the performance
  // inliner, so it currently does not track the inlined call site.
  for (auto ds = loc.getScope(); ds; ds = ds->InlinedCallSite) {
    // If the call site location is invalid, stop scanning.
    if (!ds->Loc.getSourceLoc().isValid())
      break;

    if (SILFunction *F = ds->getInlinedFunction()) {
      if (!F->getLocation().getSourceLoc().isValid())
        break;

      auto lineCol = SM.getLineAndColumn(ds->Loc.getSourceLoc());
      // Separate functions using '/' so that TensorBoard can treat it as a
      // hierarchical separator.
      name += '/';

      // If the SIL function is backed by a Swift decl, use the decl name.
      // Otherwise, use the SIL name.
      std::string funcName;
      auto *dc = SILFn.getDeclContext();
      if (auto *afd = dyn_cast_or_null<AbstractFunctionDecl>(dc)) {
        funcName = escapeDeclName(afd->getEffectiveFullName());
        // Make sure the name is unique.
        auto declCountLookup = uniqueNames.find(afd);
        if (declCountLookup != uniqueNames.end())
          funcName += "_" + llvm::itostr(declCountLookup->getSecond()++);
        else
          uniqueNames.insert({afd, 1});
      } else {
        auto fnName = F->getName();
        // Drop ".device_partition" suffix off function names.
        if (fnName.endswith(".device_partition")) {
          fnName = fnName.drop_back(strlen(".device_partition"));
        }
        funcName = fnName.str();
      }

      name += funcName + "." + llvm::utostr(lineCol.first);
      name += "." + llvm::utostr(lineCol.second);
    }
  }

  // If the debug location didn't have any other information, produce something
  // with the raw location.
  if (!loc.getScope() || loc.getScope()->Loc.isNull()) {
    auto sourceLoc = loc.getLocation().getSourceLoc();
    if (sourceLoc.isValid()) {
      auto lineCol = SM.getLineAndColumn(sourceLoc);
      auto bufferID = SM.findBufferContainingLoc(sourceLoc);
      name += "/" + llvm::utostr(bufferID) + "." + llvm::utostr(lineCol.first);
      name += "." + llvm::utostr(lineCol.second);
    }
  }

  // Append device type to ensure the name is unique across all devices.
  name += ':';
  name += thisDeviceTypeStr;

  // Append module name to ensure the name is unique across all modules.
  // (Duplicate names happen across execution units in the lldb repl).
  auto module = SILFn.getModule().getAssociatedContext()->getParentModule();
  name += ':';
  name += *module->getReverseFullModuleName();

  // Escape op name.
  escapeOpName(name);

  // If we've already used this name, rename it to make it unique.
  while (!usedOpNames.insert(name).second) {
    name += "_" + llvm::utostr(OpID++);
  }

  return name;
}

TF_DataType TFGraphFunctionLowering::getTensorFlowDataType(SILType type,
                                                           SILLocation loc) {
  // Handle things like TensorHandle<Float>.
  switch (classifyTensorFlowValue(type)) {
  case TFValueKind::TensorHandle: {
    auto elt = getTensorHandleElementType(type.getASTType());
    assert(elt && "We know this is TensorHandle!");
    if (auto ty = (TF_DataType)convertSwiftTypeToTF(elt))
      return ty;
    break;
  }
  case TFValueKind::ResourceHandle:
    return TF_RESOURCE;
  case TFValueKind::VariantHandle:
    return TF_VARIANT;
  case TFValueKind::Nope:
    // Otherwise this must be a scalar type we're promoting to a tensor.
    if (auto ty = (TF_DataType)convertSwiftTypeToTF(type.getASTType()))
      return ty;
    break;
  }

  internalError(loc, "Unknown Swift type to lower to TensorFlow: " +
                         type.getAsString());
  return TF_DataType(-1);
}

//===----------------------------------------------------------------------===//
// Helpers to create TensorFlow graph nodes.
//===----------------------------------------------------------------------===//

/// A default deallocator function to pass in `TF_NewTensor`.
static void tensorDataDeallocator(void *data, size_t len, void *arg) {
  free(data);
};

/// Convert given elements to a tensor with the given shape and dtype.
/// Tensor element values can only be Integer, Float or String, and must
/// be consistent with the specified dtype.
static TF_Tensor *convertValuesToTensor(ArrayRef<SymbolicValue> elts,
                                        ArrayRef<int64_t> shape,
                                        TF_DataType dtype) {
  assert(dtype != TF_DataType() && "Expected to get a type!");
  // If dtype is not string, we can directly compute the total alloc size from
  // the dtype and the shape.
  if (dtype != TF_STRING) {
    auto dtypeSize = TF_DataTypeSize(dtype);

    // Compute the total memory size of the tensor value.
    unsigned totalElements = 1;
    for (auto dim : shape)
      totalElements *= dim;

    // Make an uninitialized tensor that is big enough for our value.
    auto *tensor = TF_AllocateTensor(dtype, shape.data(), shape.size(),
                                     dtypeSize * totalElements);

    // Set up its contents, element-wise.
    // FIXME: This will need a byte swap for big endian hosts.
    auto *ptr = (char *)TF_TensorData(tensor);

    for (auto elt : elts) {
      switch (elt.getKind()) {
      case SymbolicValue::Integer: {
        auto intVal = elt.getIntegerValue();
        // Swift.Bool is represented as a 1-bit value, but TF_BOOL is 1 byte
        // or more.
        if (dtype == TF_BOOL)
          intVal = intVal.zext(dtypeSize * 8);
        assert(intVal.getBitWidth() == dtypeSize * 8);
        memcpy(ptr, intVal.getRawData(), dtypeSize);
        break;
      }
      case SymbolicValue::Float: {
        auto floatVal = elt.getFloatValue();
        bool losesInfo = false;
        // Convert to float if necessary.
        if (dtype == TF_FLOAT)
          floatVal.convert(APFloat::IEEEsingle(),
                           APFloat::rmNearestTiesToEven, &losesInfo);
        memcpy(ptr, floatVal.bitcastToAPInt().getRawData(), dtypeSize);
        break;
      }
      default:
        llvm_unreachable("Tensor element values can only be Integer or Float");
      }
      ptr += dtypeSize;
    }

    return tensor;
  }

  // When dtype is string, strings are stored as varint encodings. The buffer
  // starts with uint64_t offsets for each string, followed by all strings'
  // encodings.
  size_t offsetsSize = elts.size() * sizeof(uint64_t);
  // Compute the total size.
  size_t totalSize = offsetsSize;
  for (auto elt : elts) {
    auto string = elt.getStringValue();
    totalSize += TF_StringEncodedSize(string.size());
  }
  // Allocate tensor.
  void *baseAddr = malloc(totalSize);
  auto *tensor = TF_NewTensor(dtype, shape.data(), shape.size(), baseAddr,
                              totalSize, tensorDataDeallocator, nullptr);
  auto *status = TF_NewStatus();
  // Populate the buffer with strings and record each string's offset.
  uint64_t *offsets = (uint64_t *)baseAddr;
  char *dataStart = (char *)baseAddr + offsetsSize;
  char *curData = dataStart;
  for (unsigned i = 0, n = elts.size(); i < n; i++) {
    auto string = elts[i].getStringValue();
    auto encodingSize = TF_StringEncode(string.data(), string.size(), curData,
                                        totalSize, status);
    assert(TF_GetCode(status) == TF_OK);
    offsets[i] = curData - dataStart;
    curData += encodingSize;
  }
  TF_DeleteStatus(status);
  return tensor;
}

//===----------------------------------------------------------------------===//
// Helpers to create TensorFlow graph nodes.
//===----------------------------------------------------------------------===//

/// If `type` is Tensor<T> or TensorHandle<T>, return the TF_DataType
/// corresponding to element type T. Otherwise, return 0.
static unsigned getTFDataTypeFromTensorGenericType(Type type) {
  auto *genTy = type->getAs<BoundGenericType>();
  if (!genTy || genTy->getGenericArgs().size() != 1) {
    return 0;
  }
  return convertSwiftTypeToTF(genTy->getGenericArgs()[0]);
}

/// Decode the shape array attribute at attr `attrIdx` in the graph_op
/// instruction, into `dims`, `numDims` and `dimPtrs`.
static void decodeShapeArrayAtAttr(const ASTContext &ctx,
                                   const GraphOperationInfo &graphOpInfo,
                                   StringRef attrName, unsigned attrIdx,
                                   SmallVectorImpl<int64_t> &dims,
                                   SmallVectorImpl<int> &numDims,
                                   SmallVectorImpl<int64_t *> &dimPtrs) {
  auto *inst = graphOpInfo.getInst();
  auto attr = inst->getAttribute(attrIdx);
  auto attrInfo = GraphOperationInfo::decodeArgumentName(attr.name.str());
  assert(attrInfo && "attribute has malformed name");
  assert(attrInfo->second == GraphOperationInfo::ArgumentLowering::NormalAttribute);
  assert(attrInfo->first == attrName);
  decodeShapeArray(ctx, attr.value, dims, numDims, dimPtrs);
}

GLStatus TFGraphFunctionLowering::visitGraphOpSendToHostInst(
    GraphOperationInfo &graphOpInfo) {
  auto &graphFn = getCurrentGraphFunction();
  // TODO(b/78472806): Add a more thorough and proper fix for effectful ops in
  // the while cond function.
  if (!graphFn.shouldLowerEffectfulOps)
    return GLStatus::Success;

  // Type check and process the parameters.
  // SendToHost has type <T> (input$T, tensorId$int, device$str) -> ()
  auto *inst = graphOpInfo.getInst();
  assert(inst->getNumResults() == 1);
  assert(inst->getNumOperands() == 1);
  assert(inst->getNumAttributes() == 2);

  TF_Output inputOp;
  TF_DataType inputType;
  {
    auto operand = inst->getOperand(0);
    inputOp = getOperandValue(operand);
    if (!inputOp.oper)
      return GLStatus::Error;
    inputType = getTensorFlowDataType(operand->getType(), inst->getLoc());
  }

  assert(getDeviceString(graphOpInfo) == TF_DEFAULT_CPU_DEVICE &&
         "SendToHost must run on CPU device");
  int tensorId = inst->getAttributeNamed("tensorId")
                     .getValue()
                     .getIntegerValue()
                     .getLimitedValue();
  // Add enqueue to the local graph function, and the corresponding dequeue to
  // the top level function, so that caller can dequeue tensors via SessionRun.
  TF_Operation *queueOp;
  {
    auto opName = "fifo_queue_" + llvm::itostr(tensorId);
    auto *desc =
        TF_NewOperation(graphFn.getGraph(), "FIFOQueueV2", opName.c_str());
    TF_SetDevice(desc, TF_DEFAULT_CPU_DEVICE);
    TF_SetAttrInt(desc, "capacity", NAMED_TENSOR_QUEUE_CAPACITY);
    TF_SetAttrTypeList(desc, "component_types", &inputType, 1);
    TF_SetAttrString(desc, "shared_name", opName.data(), opName.size());
    queueOp = graphFn.finishOp(desc, /*opHasSideEffects*/ false,
                               /*isEligibleForTPU*/ false, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return GLStatus::Error;
  }

  {
    auto opName = "fifo_queue_enqueue_" + llvm::itostr(tensorId);
    auto *desc =
        TF_NewOperation(graphFn.getGraph(), "QueueEnqueueV2", opName.c_str());
    TF_AddInput(desc, {queueOp, 0});
    TF_AddInputList(desc, &inputOp, 1);
    TF_SetDevice(desc, TF_DEFAULT_CPU_DEVICE);
    TF_SetAttrTypeList(desc, "Tcomponents", &inputType, 1);

    graphFn.finishOp(desc, /*opHasSideEffects*/ true,
                     /*isEligibleForTPU*/ false, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return GLStatus::Error;
  }

  // Now add dequeue to the top level graph function.
  // Multiple graph functions can have an enqueue op over the same tensorId.
  // One example is to enqueue tensors both within the while op's body
  // function, and also right after the while op is executed.
  // In that case, we only generate a single dequeue op at the top level.
  if (!processedTensorIdsForSend.insert(tensorId).second)
    return GLStatus::Success;

  // The code here is different enough from the above that it's not worth
  // extracting common code into functions.
  TF_Operation *globalQueueOp;
  {
    auto opName = "fifo_queue_" + llvm::itostr(tensorId);
    auto *desc = TF_NewOperation(resultGraph, "FIFOQueueV2", opName.c_str());
    TF_SetDevice(desc, TF_DEFAULT_CPU_DEVICE);
    TF_SetAttrInt(desc, "capacity", NAMED_TENSOR_QUEUE_CAPACITY);
    TF_SetAttrTypeList(desc, "component_types", &inputType, 1);
    // FIXME: Revisit whether to populate "shared_name".
    TF_SetAttrString(desc, "shared_name", opName.data(), opName.size());
    globalQueueOp = TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return GLStatus::Error;
  }

  {
    auto opName = "fifo_queue_dequeue_" + llvm::itostr(tensorId);
    auto *desc = TF_NewOperation(resultGraph, "QueueDequeueV2", opName.c_str());
    TF_AddInput(desc, {globalQueueOp, 0});
    TF_SetDevice(desc, TF_DEFAULT_CPU_DEVICE);
    TF_SetAttrTypeList(desc, "component_types", &inputType, 1);
    TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return GLStatus::Error;
  }
  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::visitGraphOpRecvFromHostInst(
    GraphOperationInfo &graphOpInfo) {
  auto &graphFn = getCurrentGraphFunction();
  // TODO(b/78472806): Add a more thorough and proper fix for effectful ops in
  // the while cond function.
  if (!graphFn.shouldLowerEffectfulOps) {
    internalError(
        getUserSourceLocation(graphOpInfo.getInst()->getDebugLocation()),
        "FIXME: cannot lower a Host->TF tensor transfer in a loop header",
        diag::tfop_invalid_tfop);
    return GLStatus::Error;
  }

  // Type check and process the parameters.
  // recvFromHost has type <T> (tensorId$int, device$string) -> (T)
  // Optionally it can carry a shape array attr, only used for shape propagation
  // in XLA compilation.
  auto *inst = graphOpInfo.getInst();
  assert(inst->getNumResults() == 1);
  assert(inst->getNumOperands() == 0);
  assert(inst->getNumAttributes() >= 2);

  assert(getDeviceString(graphOpInfo) == TF_DEFAULT_CPU_DEVICE &&
         "SendToHost must run on CPU device");
  int tensorId = inst->getAttributeNamed("tensorId")
                     .getValue()
                     .getIntegerValue()
                     .getLimitedValue();

  TF_DataType outputType =
      getTensorFlowDataType(inst->getResults()[0]->getType(), inst->getLoc());

  // Add dequeue to the local graph function, and the corresponding enqueue to
  // the top level function, so that caller can enqueue tensors via SessionRun.
  TF_Operation *queueOp;
  {
    auto opName = "fifo_queue_" + llvm::itostr(tensorId);
    auto *desc =
        TF_NewOperation(graphFn.getGraph(), "FIFOQueueV2", opName.c_str());
    TF_SetDevice(desc, TF_DEFAULT_CPU_DEVICE);
    TF_SetAttrInt(desc, "capacity", NAMED_TENSOR_QUEUE_CAPACITY);
    TF_SetAttrTypeList(desc, "component_types", &outputType, 1);
    TF_SetAttrString(desc, "shared_name", opName.data(), opName.size());
    queueOp = graphFn.finishOp(desc, /*opHasSideEffects*/ false,
                               /*isEligibleForTPU*/ false, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return GLStatus::Error;
  }

  {
    auto opName = "fifo_queue_dequeue_" + llvm::itostr(tensorId);
    auto *desc =
        TF_NewOperation(graphFn.getGraph(), "QueueDequeueV2", opName.c_str());
    TF_AddInput(desc, {queueOp, 0});
    TF_SetDevice(desc, TF_DEFAULT_CPU_DEVICE);
    TF_SetAttrTypeList(desc, "component_types", &outputType, 1);

    auto dequeueOp = graphFn.finishOp(desc, /*opHasSideEffects*/ true,
                                      /*isEligibleForTPU*/ false, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return GLStatus::Error;
    addValueMapping({inst->getResult(0), 0}, {dequeueOp, 0});
  }

  // Now add enqueue to the top level graph function. Multiple graph functions
  // can have their own dequeue ops over the same tensorId.
  // One example is to dequeue tensors both within the while op's body
  // function, and also right after the while op is executed.
  // In that case, we only generate a single enqueue op at the top level.
  if (!processedTensorIdsForReceive.insert(tensorId).second)
    return GLStatus::Success;

  // The code here is different enough from the above that it's not worth
  // extracting common code into functions.
  TF_Operation *globalQueueOp;
  {
    auto opName = "fifo_queue_" + llvm::itostr(tensorId);
    auto *desc = TF_NewOperation(resultGraph, "FIFOQueueV2", opName.c_str());
    TF_SetDevice(desc, TF_DEFAULT_CPU_DEVICE);
    TF_SetAttrInt(desc, "capacity", NAMED_TENSOR_QUEUE_CAPACITY);
    TF_SetAttrTypeList(desc, "component_types", &outputType, 1);
    // FIXME: Revisit whether to populate "shared_name".
    TF_SetAttrString(desc, "shared_name", opName.data(), opName.size());
    globalQueueOp = TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return GLStatus::Error;
  }

  TF_Operation *inputTensorPlaceholder;
  {
    auto opName = "arg_tensor_enqueue_" + llvm::itostr(tensorId);
    auto *desc = TF_NewOperation(resultGraph, "Placeholder", opName.c_str());
    TF_SetAttrType(desc, "dtype", outputType);

    inputTensorPlaceholder = TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return GLStatus::Error;
  }

  {
    auto opName = "fifo_queue_enqueue_" + llvm::itostr(tensorId);
    auto *desc = TF_NewOperation(resultGraph, "QueueEnqueueV2", opName.c_str());
    TF_AddInput(desc, {globalQueueOp, 0});
    TF_Output inputTensor{inputTensorPlaceholder, 0};
    TF_AddInputList(desc, &inputTensor, 1);
    TF_SetDevice(desc, TF_DEFAULT_CPU_DEVICE);
    TF_SetAttrTypeList(desc, "Tcomponents", &outputType, 1);
    TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return GLStatus::Error;
  }
  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::addTFRecvOp(const SILInstruction *inst,
                                              int transferId,
                                              StringRef srcDevice) {
  auto opName = "tf_recv_" + llvm::itostr(transferId);
  auto &graphFn = getCurrentGraphFunction();
  auto *desc = TF_NewOperation(graphFn.getGraph(), "_Recv", opName.c_str());
  TF_SetDevice(desc, thisDeviceTypeStr.c_str());

  auto outputFromRecvVal = inst->getResults()[0];
  auto tfType =
      getTensorFlowDataType(outputFromRecvVal->getType(), inst->getLoc());
  assert(tfType > 0);
  TF_SetAttrType(desc, "tensor_type", (TF_DataType)tfType);

  auto tensorName = "tensor_transfer_" + llvm::itostr(transferId);
  TF_SetAttrString(desc, "tensor_name", tensorName.data(), tensorName.size());
  TF_SetAttrString(desc, "send_device", srcDevice.data(), srcDevice.size());
  TF_SetAttrInt(desc, "send_device_incarnation", DEVICE_INCARNATION_ID);
  TF_SetAttrString(desc, "recv_device", thisDeviceTypeStr.data(),
                   thisDeviceTypeStr.size());
  auto *recvOp = graphFn.finishOp(desc, /*opHasSideEffects*/ true,
                                  /*isEligibleForTPU*/ false, status);
  if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
    return GLStatus::Error;
  addValueMapping({outputFromRecvVal, 0}, {recvOp, 0});
  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::addTPUDequeueOp(const SILInstruction *inst,
                                                  bool isInfeed, int transferId,
                                                  ArrayRef<int64_t> dims,
                                                  ArrayRef<int> numDims,
                                                  ArrayRef<int64_t *> dimPtrs) {
  // Infeed dequeue runs on TPU, while outfeed dequeue runs on CPU.
  // Otherwise they have the same op signature.
  if (numDims.size() != 1) {
    if (isInfeed)
      internalError(getUserSourceLocation(inst->getDebugLocation()),
                    "TPU infeed dequeue supports dequeuing a single tensor -- "
                    "did you specify shape?",
                    diag::tfop_invalid_tfop);
    else
      internalError(getUserSourceLocation(inst->getDebugLocation()),
                    "TPU outfeed dequeue supports dequeuing a single tensor -- "
                    "did you specify shape?",
                    diag::tfop_invalid_tfop);
    return GLStatus::Error;
  }
  if (isInfeed) {
    assert(thisDeviceType == DeviceType::TPU);
  } else {
    if (thisDeviceType != DeviceType::CPU) {
      internalError(getUserSourceLocation(inst->getDebugLocation()),
                    "TPU outfeed dequeue cannot run on this device",
                    diag::tfop_invalid_tfop);
      return GLStatus::Error;
    }
  }
  std::string opName = isInfeed ? "tf_infeed_dequeue_" : "tf_outfeed_dequeue_";
  opName += llvm::itostr(transferId);
  auto &graphFn = getCurrentGraphFunction();
  auto *desc = TF_NewOperation(
      graphFn.getGraph(),
      isInfeed ? "InfeedDequeueTuple" : "OutfeedDequeueTuple", opName.c_str());
  if (isInfeed) {
    markNodeAsTPUReplicated(desc);
  } else {
    TF_SetDevice(desc, thisDeviceTypeStr.c_str());
    TF_SetAttrInt(desc, "device_ordinal", 0);
  }

  SmallVector<TF_DataType, 4> types;
  for (auto result : inst->getResults()) {
    TF_DataType tfType =
        getTensorFlowDataType(result->getType(), inst->getLoc());
    assert(tfType > 0);
    types.push_back(tfType);
  }
  TF_SetAttrTypeList(desc, "dtypes", types.data(), types.size());
  TF_SetAttrShapeList(desc, "shapes", dimPtrs.data(), numDims.data(),
                      numDims.size());
  // Infeed dequeue is placed on TPU; outfeed dequeue isn't.
  bool isEligibleForTPU = isInfeed;
  auto *dequeue = graphFn.finishOp(desc, /*opHasSideEffects*/ true,
                                   isEligibleForTPU, status);
  if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
    return GLStatus::Error;
  for (int i = 0, n = numDims.size(); i != n; ++i) {
    addValueMapping({inst->getResults()[i], 0}, {dequeue, i});
  }

  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::visitGraphOpD2DTensorRecvInst(
    GraphOperationInfo &graphOpInfo) {
  // Signature: "tfc.D2DTensorRecv {transferId,srcDevice,device}"
  // Can also carry an optional shape array.
  auto *inst = graphOpInfo.getInst();
  assert(inst->getNumResults() == 1);
  assert(inst->getNumOperands() == 0);
  assert(inst->getNumAttributes() == 3 || inst->getNumAttributes() == 4);
  assert(getDeviceString(graphOpInfo) == thisDeviceTypeStr);

  int transferId = graphOpInfo.getIntAttr(0, "transferId");
  auto srcDeviceStr = graphOpInfo.getStringAttr(1, "srcDevice");
  auto srcDevice = getOpDeviceType(srcDeviceStr);
  assert(thisDeviceType != srcDevice);

  SmallVector<int64_t, 8> dims;
  SmallVector<int, 3> numDims;
  SmallVector<int64_t *, 8> dimPtrs;
  if (inst->getNumAttributes() == 4) {
    // 3 is the attr idx for the optional shape array attr.
    decodeShapeArrayAtAttr(SILFn.getASTContext(), graphOpInfo,
                           TF_SHAPE_ARRAY_ATTR, /*attrIdx*/ 3, dims, numDims,
                           dimPtrs);
  }
  if (thisDeviceType == DeviceType::TPU) {
    return addTPUDequeueOp(inst, /* isInfeed */ true, transferId, dims, numDims,
                           dimPtrs);
  } else if (srcDevice == DeviceType::TPU) {
    return addTPUDequeueOp(inst, /* isInfeed */ false, transferId, dims,
                           numDims, dimPtrs);
  } else {
    return addTFRecvOp(inst, transferId, srcDeviceStr);
  }
}

GLStatus TFGraphFunctionLowering::addTFSendOp(const SILInstruction *inst,
                                              int transferId,
                                              StringRef destDevice) {
  auto opName = "tf_send_" + llvm::itostr(transferId);
  auto &graphFn = getCurrentGraphFunction();
  auto *desc = TF_NewOperation(graphFn.getGraph(), "_Send", opName.c_str());
  TF_SetDevice(desc, thisDeviceTypeStr.c_str());

  auto inputToSendVal = inst->getOperand(0);
  auto inputToSendOp = getOperandValue(inputToSendVal);
  if (!inputToSendOp.oper)
    return GLStatus::Error;
  TF_AddInput(desc, inputToSendOp);
  auto tfType =
      getTensorFlowDataType(inputToSendVal->getType(), inst->getLoc());
  assert(tfType > 0);
  TF_SetAttrType(desc, "T", (TF_DataType)tfType);
  auto tensorName = "tensor_transfer_" + llvm::itostr(transferId);
  TF_SetAttrString(desc, "tensor_name", tensorName.data(), tensorName.size());
  TF_SetAttrString(desc, "send_device", thisDeviceTypeStr.data(),
                   thisDeviceTypeStr.size());
  TF_SetAttrInt(desc, "send_device_incarnation", DEVICE_INCARNATION_ID);
  TF_SetAttrString(desc, "recv_device", destDevice.data(), destDevice.size());
  /* sendOp = */ graphFn.finishOp(desc, /*opHasSideEffects*/ true,
                                  /*isEligibleForTPU*/ false, status);
  if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
    return GLStatus::Error;
  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::addTPUEnqueueOp(const SILInstruction *inst,
                                                  bool isInfeed, int transferId,
                                                  ArrayRef<int64_t> dims,
                                                  ArrayRef<int> numDims,
                                                  ArrayRef<int64_t *> dimPtrs) {
  // Infeed enqueue runs on CPU, while outfeed enqueue runs on TPU.
  if (isInfeed) {
    if (thisDeviceType != DeviceType::CPU) {
      internalError(getUserSourceLocation(inst->getDebugLocation()),
                    "TPU infeed enqueue cannot run on this device",
                    diag::tfop_invalid_tfop);
      return GLStatus::Error;
    }
    if (numDims.size() != 1) {
      // For InfeedEnqueueTuple, the shapes attr is only used for
      // error checking but is still a required attr.
      // Interestingly, OutfeedEnqueueTuple does not take a shapes attr
      // (b/110538524).
      internalError(getUserSourceLocation(inst->getDebugLocation()),
                    "TPU infeed enqueue supports enqueuing a single tensor -- "
                    "did you specify shape?",
                    diag::tfop_invalid_tfop);
      return GLStatus::Error;
    }
  } else {
    assert(thisDeviceType == DeviceType::TPU);
  }
  std::string opName = isInfeed ? "tf_infeed_enqueue_" : "tf_outfeed_enqueue_";
  opName += llvm::itostr(transferId);
  auto &graphFn = getCurrentGraphFunction();
  auto *desc = TF_NewOperation(
      graphFn.getGraph(),
      isInfeed ? "InfeedEnqueueTuple" : "OutfeedEnqueueTuple", opName.c_str());
  if (isInfeed) {
    TF_SetDevice(desc, thisDeviceTypeStr.c_str());
    TF_SetAttrInt(desc, "device_ordinal", 0);
  } else {
    markNodeAsTPUReplicated(desc);
  }

  auto inputToSendVal = inst->getOperand(0);
  auto inputToSendOp = getOperandValue(inputToSendVal);
  if (!inputToSendOp.oper)
    return GLStatus::Error;
  TF_AddInputList(desc, &inputToSendOp, 1);

  TF_DataType tfType =
      getTensorFlowDataType(inputToSendVal->getType(), inst->getLoc());
  assert(tfType > 0);
  TF_SetAttrTypeList(desc, "dtypes", &tfType, 1);
  if (isInfeed && !numDims.empty()) {
    TF_SetAttrShapeList(desc, "shapes", dimPtrs.data(), numDims.data(),
                        numDims.size());
  }
  // Infeed enqueue is not placed on TPU; outfeed enqueue is.
  bool isEligibleForTPU = !isInfeed;
  /*auto *enqueueOp = */ graphFn.finishOp(desc, /*opHasSideEffects*/ true,
                                          isEligibleForTPU, status);
  if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
    return GLStatus::Error;
  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::visitGraphOpD2DTensorSendInst(
    GraphOperationInfo &graphOpInfo) {
  // Signature: "tfc.D2DTensorSend,$in {transferId,destDevice,device}"
  // Can also carry an optional shape array.
  auto *inst = graphOpInfo.getInst();
  assert(inst->getNumResults() == 0);
  assert(inst->getNumOperands() == 1);
  assert(inst->getNumAttributes() == 3 || inst->getNumAttributes() == 4);
  assert(getDeviceString(graphOpInfo) == thisDeviceTypeStr);

  int transferId = graphOpInfo.getIntAttr(0, "transferId");
  auto destDeviceStr = graphOpInfo.getStringAttr(1, "destDevice");
  auto destDevice = getOpDeviceType(destDeviceStr);
  assert(thisDeviceType != destDevice);

  SmallVector<int64_t, 8> dims;
  SmallVector<int, 3> numDims;
  SmallVector<int64_t *, 8> dimPtrs;
  if (inst->getNumAttributes() == 4)
    // 3 is the attr idx for the optional shape array attr.
    decodeShapeArrayAtAttr(SILFn.getASTContext(), graphOpInfo,
                           TF_SHAPE_ARRAY_ATTR, /*attrIdx*/ 3, dims, numDims,
                           dimPtrs);
  if (thisDeviceType == DeviceType::TPU) {
    return addTPUEnqueueOp(inst, /* isInfeed */ false, transferId, dims,
                           numDims, dimPtrs);
  } else if (destDevice == DeviceType::TPU) {
    return addTPUEnqueueOp(inst, /* isInfeed */ true, transferId, dims, numDims,
                           dimPtrs);
  } else {
    return addTFSendOp(inst, transferId, destDeviceStr);
  }
}

void TFGraphFunctionLowering::handleFunctionAttribute(
    TF_OperationDescription *op, const std::string &opName, SILLocation loc,
    StringRef silFuncName) {
  auto graphFnName = getGraphFuncNameForFuncAttr(silFuncName);
  TF_SetAttrFuncName(op, opName.c_str(), graphFnName.data(),
                     graphFnName.size());
}

/// Lower a graph_op into the TensorFlow op node.
///
GLStatus
TFGraphFunctionLowering::visitGraphOperationInst(GraphOperationInst *inst) {
  // If this is the magic tf_tensor_to_i1 graph_op, then we completely ignore it.
  // the only user of it are things that take conditional branches, and they
  // handle it directly.
  if (inst->getName().str() == "tf_tensor_to_i1,i")
    return GLStatus::Success;

  // Decode information about the graph_op.
  GraphOperationInfo decoder(inst);
  auto opName = decoder.getOperationName();
  auto &structuredArguments = decoder.getStructuredArguments();

  // Swift host <-> TF device sends/recvs.
  if (opName == "tfc.RecvFromHost")
    return visitGraphOpRecvFromHostInst(decoder);
  else if (opName == "tfc.SendToHost")
    return visitGraphOpSendToHostInst(decoder);

  // Device-to-device sends/recvs.
  if (opName == "tfc.D2DTensorRecv")
    return visitGraphOpD2DTensorRecvInst(decoder);
  else if (opName == "tfc.D2DTensorSend")
    return visitGraphOpD2DTensorSendInst(decoder);

  auto &graphFn = getCurrentGraphFunction();

  // The name label we put on the op is summarized from the "stack trace" of
  // the operation's source location.
  auto opLocString = getUniqueName(inst->getDebugLocation(), "op");

  auto *op = TF_NewOperation(graphFn.getGraph(), opName.str().c_str(),
                             opLocString.c_str());

  // TODO: We compute the "opHasSideEffects" bit solely based on whether or not
  // the op has Resource inputs.  This is a good starting point but is
  // insufficient.  It would be much nicer to have a TensorFlow C function that
  // returns the "SetIsStateful" bit from a TF_OperationDescription.
  bool opHasSideEffects = false;
  bool hasDevice = false;

  // Process all inputs.
  for (auto structuredArgument : structuredArguments) {
    assert(structuredArgument.getArgumentNameWithSuffix().empty() &&
           "cannot lower named arguments");
    switch (structuredArgument.getKind()) {
    case GraphOperationInfo::SAK_Single: {
      // Normal tensor inputs.
      auto argument = structuredArgument.getSingleArgument();
      auto valueKind = classifyTensorFlowValue(argument->getType());

      // Keep track of whether we have any resource inputs.
      opHasSideEffects |= valueKind == TFValueKind::ResourceHandle;
      assert(valueKind != TFValueKind::Nope &&
             "all op inputs should be TensorFlow values");
      auto opValue = getOperandValue(argument);
      if (!opValue.oper)
        return GLStatus::Error;
      TF_AddInput(op, opValue);
      break;
    }
    case GraphOperationInfo::SAK_List: {
      // Collect all of the elements of the input list.
      SmallVector<TF_Output, 4> elements;
      for (auto argument : structuredArgument.getArgumentList()) {
        auto valueKind = classifyTensorFlowValue(argument->getType());

        // Keep track of whether we have any resource inputs.
        opHasSideEffects |= valueKind == TFValueKind::ResourceHandle;
        assert(valueKind != TFValueKind::Nope &&
               "all op inputs should be TensorFlow values");
        auto opValue = getOperandValue(argument);
        if (!opValue.oper)
          return GLStatus::Error;
        elements.push_back(opValue);
      }
      TF_AddInputList(op, elements.data(), elements.size());
      break;
    }
    }
  }

  // Process all of the attributes.
  // For an inst like:
  //   graph_op "Const"() {dtype: $Builtin.Int64, value$tensor: i1 0
  // We will use the `dtype` attr value to lower the `value` attr, so we
  // remember the dtype info here.
  // We use unsigned instead of TF_DataType, to express the invalid initial
  // value 0.
  unsigned dtypeAttr = 0;
  for (unsigned nextAttributeNumber = 0, e = inst->getNumAttributes();
       nextAttributeNumber != e;) {
    // Look at which attribute comes next.
    auto attr = inst->getAttribute(nextAttributeNumber++);
    auto attrInfo = GraphOperationInfo::decodeArgumentName(attr.name.str());
    assert(attrInfo && "attribute has malformed name");
    auto attrValue = attr.value;

    // Convert the not-necessarily-nul-terminated StringRef to an std::string
    // so we can guarantee null termination for the "const char*" taking APIs.
    std::string name = attrInfo->first.str();

    switch (attrInfo->second) {
    case GraphOperationInfo::ArgumentLowering::Input:
      assert(0 && "Input classes cannot exist for attributes");
    case GraphOperationInfo::ArgumentLowering::Out:
      assert(0 && "Attributes cannot be output parameters");

    case GraphOperationInfo::ArgumentLowering::NormalAttribute: // No modifier.
      // We add attributes based on what the type of the value is.
      switch (attrValue.getKind()) {
      case SymbolicValue::Unknown:
      case SymbolicValue::UninitMemory:
      case SymbolicValue::Enum:
      case SymbolicValue::EnumWithPayload:
      case SymbolicValue::Address:
      case SymbolicValue::Aggregate: // Tuples and structs
      case SymbolicValue::Metatype:
        llvm::dbgs() << name << " " << attrValue << "\n";
        assert(0 && "These attribute kinds cannot happen here");
      case SymbolicValue::Integer: {
        auto value = attrValue.getIntegerValue();
        if (value.getBitWidth() == 1)
          TF_SetAttrBool(op, name.c_str(), value.isAllOnesValue());
        else
          TF_SetAttrInt(op, name.c_str(), (int64_t)value.getLimitedValue());
        break;
      }
      case SymbolicValue::Float: {
        auto value = attrValue.getFloatValue();
        // TensorFlow only supports 32-bit float attributes.  If we got a 16 or
        // 64 bit one, convert it to float.
        bool losesInfo = false;
        value.convert(APFloat::IEEEsingle(), APFloat::rmNearestTiesToEven,
                      &losesInfo);
        TF_SetAttrFloat(op, name.c_str(), value.convertToFloat());
        break;
      }
      case SymbolicValue::String: {
        auto value = attrValue.getStringValue();
        if (name != TF_DEVICE_ATTR) {
          TF_SetAttrString(op, name.c_str(), value.data(), value.size());
        } else {
          if (value == TF_ALL_DEVICES)
            value = thisDeviceTypeStr;

          if (value.str() != TF_DEFAULT_TPU_DEVICE) {
            TF_SetDevice(op, value.str().c_str());
          } else {
            // TPU device placement is not done via TF_SetDevice().
            markNodeAsTPUReplicated(op);
          }
          hasDevice = true;
        }
        break;
      }
      case SymbolicValue::Function: {
        auto silFuncName = attrValue.getFunctionValue()->getName();
        handleFunctionAttribute(op, name, inst->getLoc(), silFuncName);
        break;
      }
      case SymbolicValue::Array: {
        // TF_SHAPE_ARRAY_ATTR is a pseudo-attribute used by the compiler's
        // partitioning and graph lowering passes to propagate shape info for
        // XLA compilation (e.g. feed shape info to infeed / outfeed ops), and
        // will not be lowered into this graph op itself.
        if (isShapeArrayPseudoAttr(name, attrValue))
          break;

        CanType elementType;
        auto rawElements = attrValue.getArrayValue(elementType);
        auto elementTypeString = elementType->getString();

        if (elementTypeString == "TensorShape" ||
            elementTypeString == "Optional<TensorShape>") {
          SmallVector<int64_t, 8> dims;
          SmallVector<int, 3> numDims;
          SmallVector<int64_t *, 8> dimPtrs;
          decodeShapeArray(SILFn.getASTContext(), attrValue, dims, numDims, dimPtrs);
          TF_SetAttrShapeList(op, name.c_str(), dimPtrs.data(), numDims.data(),
                              numDims.size());
          break;
        }

        SmallVector<SymbolicValue, 4> elements;
        elements.reserve(rawElements.size());
        for (auto elt : rawElements)
          elements.push_back(elt.lookThroughSingleElementAggregates());

        if (elementTypeString == "String") {
          SmallVector<const void *, 4> pointers;
          SmallVector<size_t, 4> sizes;
          pointers.reserve(elements.size());
          sizes.reserve(elements.size());
          for (auto elt : elements) {
            auto bytes = elt.getStringValue();
            pointers.push_back(bytes.data());
            sizes.push_back(bytes.size());
          }
          TF_SetAttrStringList(op, name.c_str(), pointers.data(), sizes.data(),
                               elements.size());
          break;
        }
        if (StringRef(elementTypeString).startswith("Int")) {
          SmallVector<int64_t, 4> values;
          values.reserve(elements.size());
          for (auto elt : elements)
            values.push_back(elt.getIntegerValue().getLimitedValue());
          TF_SetAttrIntList(op, name.c_str(), values.data(), values.size());
          break;
        }
        if (elementTypeString == "Float" || elementTypeString == "Double") {
          SmallVector<float, 4> values;
          values.reserve(elements.size());
          for (auto elt : elements) {
            auto value = elt.getFloatValue();
            // TensorFlow only supports float32 attributes, and isn't designed
            // for high precision, so we force truncate down.
            bool losesInfo = false;
            value.convert(APFloat::IEEEsingle(), APFloat::rmNearestTiesToEven,
                          &losesInfo);
            values.push_back(value.convertToFloat());
          }
          TF_SetAttrFloatList(op, name.c_str(), values.data(), values.size());
          break;
        }
        if (elementTypeString == "Bool") {
          SmallVector<unsigned char, 4> values;
          values.reserve(elements.size());
          for (auto elt : elements)
            values.push_back(elt.getIntegerValue().getLimitedValue() != 0);
          TF_SetAttrBoolList(op, name.c_str(), values.data(), values.size());
          break;
        }

        internalError(getUserSourceLocation(inst->getDebugLocation()),
                      "unknown array attribute");
        return GLStatus::Error;
      }
      }
      // Done with normal attributes.
      break;
    case GraphOperationInfo::ArgumentLowering::TensorAttribute: {
      if (!dtypeAttr) {
        inst->dump();
        llvm_unreachable("dtype attr must have been processed!");
      }
      auto dtype = (TF_DataType)dtypeAttr;

      // Add a scalar to the elements list, checking that it is the right size
      // for our dtype, and adjusting for a couple of special cases we
      // intentionally support.
      auto addScalar = [&](SymbolicValue value,
                           SmallVectorImpl<SymbolicValue> &elements) -> bool {
        value = value.lookThroughSingleElementAggregates();
        elements.push_back(value);
        return false;
      };

      // Tensor can support two cases: an array case, and a scalar case.
      SmallVector<SymbolicValue, 4> elements;

      // The scalar case is very simple, the shape of a scalar is 0d, and the
      // data type comes from an attr that should already be processed.
      SmallVector<int64_t, 4> shape;
      attrValue = attrValue.lookThroughSingleElementAggregates();
      if (attrValue.getKind() == SymbolicValue::Integer ||
          attrValue.getKind() == SymbolicValue::Float ||
          attrValue.getKind() == SymbolicValue::String) {
        if (addScalar(attrValue, elements))
          return GLStatus::Error;
      } else {
        // Add all the elements to the elements list.
        CanType eltType;
        for (auto elt : attrValue.getArrayValue(eltType)) {
          if (addScalar(elt, elements))
            return GLStatus::Error;
        }

        // Decode the shape attribute which must come next.
        auto shapeAttr = inst->getAttribute(nextAttributeNumber++).value;
        auto rank = decodeShapeAttr(SILFn.getASTContext(), shapeAttr, shape);
        (void)rank;
        assert(rank != -1 && "we generated a shape with unknown rank");
      }
      // Set the tensor as the attribute on the graph node.
      auto tensor = convertValuesToTensor(elements, shape, dtype);
      TF_SetAttrTensor(op, name.c_str(), tensor, status);
      TF_DeleteTensor(tensor);
      if (checkStatus(inst->getLoc()))
        return GLStatus::Error;
      break;
    }
    case GraphOperationInfo::ArgumentLowering::ShapeAttribute: {
      SmallVector<int64_t, 4> shape;
      auto rank = decodeShapeAttr(SILFn.getASTContext(), attrValue, shape);
      TF_SetAttrShape(op, name.c_str(), shape.data(), rank);
      break;
    }
    case GraphOperationInfo::ArgumentLowering::TFDataTypeAttribute:
      switch (attrValue.getKind()) {
      case SymbolicValue::Integer:
        dtypeAttr = getTFDataType(attrValue);
        TF_SetAttrType(op, name.c_str(), (TF_DataType)dtypeAttr);
        break;
      case SymbolicValue::Array: {
        CanType eltTy;
        SmallVector<TF_DataType, 4> types;
        for (auto elt : attrValue.getArrayValue(eltTy))
          types.push_back((TF_DataType)getTFDataType(elt));
        TF_SetAttrTypeList(op, name.c_str(), types.data(), types.size());
        break;
      }
      default:
        llvm_unreachable(
            "only integers and arrays are possible for TF_DataType attrs");
      }
    }
  }

  if (!hasDevice) {
    inst->dump();
    llvm_unreachable("The above tensor op has no device set");
  }

  auto *result =
      graphFn.finishOp(op, opHasSideEffects, /*isEligibleForTPU*/ true, status);

  // If the node builder failed, then something is wrong.  Handle a few special
  // cases to improve the diagnostics.
  if (TF_GetCode(status) != TF_OK) {
    auto loc = getUserSourceLocation(inst->getDebugLocation());

    StringRef message = TF_Message(status);
    if (message.startswith("Op type not registered")) {
      internalError(loc, opName, diag::tf_lowering_unknown_op);
      return GLStatus::Error;
    }

    // Otherwise, emit a generic error message.
    internalError(loc, message);
    return GLStatus::Error;
  }

  // Check to make sure that the operation produces the number of results we
  // expect, and wire them into the valueMapping result table.
  unsigned numOpResults = TF_OperationNumOutputs(result);
  if (numOpResults != inst->getNumResults()) {
    diagnose(SILFn, inst->getLoc(), diag::tfop_incorrect_definition,
             "TensorFlow op '" + opName.str() + "' produces " +
                 llvm::utostr(numOpResults) +
                 " result, but Swift function "
                 "produces " +
                 llvm::utostr(inst->getNumResults()));
    return GLStatus::Error;
  }

  // Remember each of the results.
  for (unsigned i = 0; i != numOpResults; ++i) {
    addValueMapping({inst->getResult(i), 0}, {result, (int)i});
  }

  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::visitTupleInst(TupleInst *inst) {
  // Tuples never exist in the graph except when they are the argument to
  // the return instruction.
  assert(inst->hasOneUse() &&
         isa<ReturnInst>(inst->getSingleUse()->getUser()) &&
         "Unexpected tuple_inst in GraphGen");
  return GLStatus::Success;
}

GLStatus
TFGraphFunctionLowering::visitUncheckedRefCastInst(UncheckedRefCastInst *inst) {
  // UncheckedBitwiseCast's get generated between two identical TensorHandle's
  // when one is using a Swift type like Int32 and one is using Builtin.Int32.
  // None of this matters for graph lowering.
  auto opValue = getOperandValue(inst->getOperand());
  if (!opValue.oper)
    return GLStatus::Error;

  addValueMapping({inst, 0}, opValue);
  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::visitReturnInst(ReturnInst *inst) {
  auto &graphFn = getCurrentGraphFunction();
  assert(graphFn.outputs.empty() &&
         "Should only have one return per graph function");

  // The return is either using a single value or a tuple of values (which
  // could be empty).  These become the results of the graph.
  if (auto *ti = dyn_cast<TupleInst>(inst->getOperand())) {
    for (auto &operand : ti->getAllOperands()) {
      auto result = getOperandValue(operand.get());
      if (!result.oper)
        return GLStatus::Error;
      result = graphFn.maybeRunEffectfulOp(result, status);
      if (checkStatus(SILFn.getLocation()))
        return GLStatus::Error;
      graphFn.outputs.push_back({/*SILArgument*/ nullptr, result});
    }
  } else {
    auto result = getOperandValue(inst->getOperand());
    if (!result.oper)
      return GLStatus::Error;
    result = graphFn.maybeRunEffectfulOp(result, status);
    if (checkStatus(SILFn.getLocation()))
      return GLStatus::Error;
    graphFn.outputs.push_back({/*SILArgument*/ nullptr, result});
  }
  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::visitBranchInst(BranchInst *inst) {
  auto &graphFn = getCurrentGraphFunction();
  assert(graphFn.outputs.empty() &&
         "Should only have one exit branch per graph function");

  if (inst->getNumArgs() == 0)
    return GLStatus::Success;

  auto destBB = inst->getDestBB();

  // Walk the BB arguments of the branch - each one is an output of the
  // enclosing graph function that we're building.  Match up the output values
  // with the BB argument being computed.
  for (unsigned i = 0, e = inst->getNumArgs(); i != e; ++i) {
    auto result = getOperandValue(inst->getArg(i));
    if (!result.oper)
      return GLStatus::Error;
    result = graphFn.maybeRunEffectfulOp(result, status);
    if (checkStatus(SILFn.getLocation()))
      return GLStatus::Error;
    graphFn.outputs.push_back({destBB->getArgument(i), result});
  }
  return GLStatus::Success;
}

/// Lower all of the instructions in the specified basic block.  If
/// skipTerminator is set to true, then the terminator instruction isn't
/// lowered.
GLStatus TFGraphFunctionLowering::lowerBasicBlock(SILBasicBlock *bb,
                                                  bool skipTerminator) {
  // Visit all of the instructions other than the terminator.
  auto I = bb->begin(), E = bb->end();

  // Ignore the terminator instruction if requested.
  if (skipTerminator)
    E = std::prev(E);

  for (; I != E; ++I) {
    GLStatus S = visit(&*I);

    // If we produced an error lowering an instruction, give up hope and return.
    if (S != GLStatus::Success)
      return S;
  }

  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::lowerSequenceRegion(SequenceSESERegion *r) {
  for (auto &child : r->getNodes()) {
    // The outputs for a sequence corresponds to the outputs of the last region
    // in the sequence. Hence, clear outputs for the current function if any.
    // Do not clear the outputs if the next region is a function as the outputs
    // are required to process that function region.
    auto &graphFn = getCurrentGraphFunction();
    if (child->getKind() == SESERegionTree::Shared) {
      for (int i = 0, e = graphFn.outputs.size(); i != e; ++i) {
        addValueMapping({graphFn.outputs[i].first, 0},
                        graphFn.outputs[i].second);
      }
    }
    graphFn.outputs.clear();
    GLStatus S = lowerRegion(child.get());
    if (S != GLStatus::Success)
      return S;
  }
  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::lowerSharedRegion(SharedSESERegion *r) {
  return lowerRegion(r->getSharedRegionTree());
}

/// Given a conditional branch, produce the TF_Output for its branch condition.
static TF_Output getCondition(CondBranchInst *condBr,
                              TFGraphFunctionLowering &lowering) {
  auto cond = condBr->getCondition();
  SILInstruction *tensorToI1 = nullptr;
  auto *graphOpResult = cast<GraphOperationResult>(cond);
  auto *graphOpInst = graphOpResult->getParent();
  assert(graphOpInst->getNumResults() == 1);
  assert(graphOpInst->getName().str() == "tf_tensor_to_i1,i");
  tensorToI1 = graphOpInst;
  assert(tensorToI1->getNumOperands() == 1 &&
         "unexpected branch condition in graph lowering");
  cond = tensorToI1->getOperand(0);

  // Get the graph node that corresponds to the condition.
  return lowering.getOperandValue(cond);
}

// Given a boolean value, create a 'not' operation to invert it, returning the
// inverted result.
static TF_Output createNotOp(TF_Output input, SILDebugLocation loc,
                             TFGraphFunctionLowering &lowering) {
  auto opLocString = lowering.getUniqueName(loc, "not");
  auto &graphFn = lowering.getCurrentGraphFunction();
  auto *op =
      TF_NewOperation(graphFn.getGraph(), "LogicalNot", opLocString.c_str());
  TF_AddInput(op, input);

  // For this synthesized op, let TF figure out the best device placement.
  // FIXME: Revisit this.
  auto *result = graphFn.finishOp(op, /*side effects*/ false,
                                  /*isEligibleForTPU*/ true, lowering.status);
  if (lowering.checkStatus(loc.getLocation()))
    return {nullptr, 0};
  return {result, 0};
}

/// Given a boolean value, create a 'cast' operation to convert it to int32.
static TF_Output castBoolToInt32(TF_Output input, SILDebugLocation loc,
                                 TFGraphFunctionLowering &lowering) {
  auto opLocString = lowering.getUniqueName(loc, "cast");
  auto &graphFn = lowering.getCurrentGraphFunction();
  auto *op = TF_NewOperation(graphFn.getGraph(), "Cast", opLocString.c_str());
  TF_AddInput(op, input);
  TF_SetAttrType(op, "SrcT", TF_BOOL);
  TF_SetAttrType(op, "DstT", TF_INT32);

  // For this synthesized op, let TF figure out the best device placement.
  // FIXME: Revisit this.
  auto *result = graphFn.finishOp(op, /*side effects*/ false,
                                  /*isEligibleForTPU*/ true, lowering.status);
  if (lowering.checkStatus(loc.getLocation()))
    return {nullptr, 0};
  return {result, 0};
}

/// Get the SILType for the specified SILOpResult.
static SILType getOpResultType(SILOpResult r) {
  if (auto *inst = dyn_cast<SILInstruction>((SILNode *)r.first))
    return inst->getResults()[r.second]->getType();
  return r.first->getType();
}

// Our WhileLoopSESERegion has been structurized into a canonical form that
// matches up pretty closely to the XLA while loop: we know we have a
// preheader, a header block, that the header block has one exit edge, and
// that there are no other exits out of the loop.
//
// This means that we can turn the computation that produces the bool for the
// termination condition into the loop exit check.
GLStatus TFGraphFunctionLowering::lowerWhileLoopRegion(WhileLoopSESERegion *r) {
  // Emit the preheader block.  The preheader ends with a branch that sets BB
  // arguments, which we will handle specially later.  They provide the passed
  // values to the loop function that we will create.
  GLStatus S = lowerBasicBlock(r->getPreheader(), /*skipTerminator:*/ true);
  if (S != GLStatus::Success)
    return S;

  auto phBranch = cast<BranchInst>(r->getPreheader()->getTerminator());

  // Get all the values that the preheader passes in for the SILArguments in
  // the loop body.
  SmallVector<TF_Output, 4> preheaderInputs;
  for (auto argValue : phBranch->getArgs()) {
    auto result = getOperandValue(argValue);
    if (!result.oper)
      return GLStatus::Error;
    preheaderInputs.push_back(result);
  }

  // We know that the header block of the loop ends with a conditional branch,
  // which is the sole exit from the loop.
  auto headerBr = cast<CondBranchInst>(r->getHeader()->getTerminator());
  SILBasicBlock *headerBB = r->getHeader();

  // Start by lowering the loop header and body of the loop to a function that
  // is the body of the while loop.  We do this before lowering the condition,
  // because the body is a superset of the code in the condition, and thus will
  // have all of the live inputs present.
  auto loopBodyFn = lowerToFunction([&]() {
    // Process each of the SILArguments in the header block.  These are the
    // values that are live across loop iterations and are the outputs of the
    // body.
    auto brLoc = r->getPreheader()->getTerminator()->getLoc();
    S = lowerArgumentsToParams(headerBB->getArguments(), preheaderInputs,
                               brLoc);
    if (S != GLStatus::Success)
      return;

    // The loop body consists of two logical regions: the code in the header
    // itself (which controls the exit condition) and the code in the body
    // region.
    //
    // Of course, the header block dominates the loop body, and it is possible
    // that some computation in the header block is used by *both* the exit
    // condition and the loop body.  Unfortunately, due to the way that XLA
    // structures its While loop into a separate function for the condition and
    // body, we are required to emit the computation into both functions, and
    // rely on XLA to CSE it where possible (which I suspect it doesn't do).
    //
    // This will also be problematic when the condition is allowed to have side
    // effects (e.g. because of send and recv) because they cannot be reissued
    // in general.
    //
    // A better model for while loop is to change the condition to be a function
    // "T -> (U, bool)" and have the loop body be "U -> T".  This structure
    // would also allow the result of the loop to be U, which is necessary to
    // support live outputs (e.g. in repeat/while loops).  This will take
    // significant TensorFlow and XLA changes though so we can survive without
    // it for quite some time.

    // Lower any code in the header block, which may be used by the body of the
    // loop.  It ends with a conditional branch (which conditionally exits the
    // loop) that we don't want to lower.
    S = lowerBasicBlock(headerBB, /*skipTerminator:*/ true);
    if (S != GLStatus::Success)
      return;

    // Lower all the code in the body of the loop.  This region ends with a
    // branch back to the loop header that passes arguments, and these arguments
    // will be installed as "exit values" on the loop by the normal BranchInst
    // lowering code.
    S = lowerRegion(r->getBody());
  });
  if (S != GLStatus::Success)
    return S;

  // Okay, at this point, the loop body should have all of the SILArguments
  // installed as inputs and outputs (in guaranteed matching order) and will
  // have all of the live inputs added as inputs to the function.  XLA While
  // loops require a T->T function, so we need to add the live inputs as outputs
  // as well.
  assert(loopBodyFn.outputs.size() == headerBB->getArguments().size() &&
         "loop body result values didn't get lowered properly");

  for (unsigned i = loopBodyFn.outputs.size(), e = loopBodyFn.inputs.size();
       i != e; ++i) {
    auto result =
        loopBodyFn.maybeRunEffectfulOp(loopBodyFn.inputs[i].parameter, status);
    if (checkStatus(SILFn.getLocation()))
      return GLStatus::Error;
    loopBodyFn.outputs.push_back({/*SILArgument*/ nullptr, result});
  }

  // Next, lower the condition function into a 'stop predicate' for the loop.
  auto condFn = lowerToFunction([&]() {
    // The condition function takes the same set of inputs as the loop body.
    auto brLoc = r->getPreheader()->getTerminator()->getLoc();
    S = lowerArgumentsToParams(headerBB->getArguments(), preheaderInputs,
                               brLoc);
    if (S != GLStatus::Success)
      return;

    // Copy the live-in set over to the condition by requesting the values be
    // live.  This ensures that the condition and body functions agree on their
    // inputs.
    auto &graphFn = getCurrentGraphFunction();
    for (unsigned i = graphFn.inputs.size(), e = loopBodyFn.inputs.size();
         i != e; ++i) {
      auto opValue = getOperandValue(loopBodyFn.inputs[i].value);
      if (!opValue.oper) {
        S = GLStatus::Error;
        return;
      }
    }

    // Lower any code in the header block, which may be used by the termination
    // condition.  It ends with a conditional branch which we handle manually.
    graphFn.shouldLowerEffectfulOps = false;
    S = lowerBasicBlock(r->getHeader(), /*skipTerminator:*/ true);
    if (S != GLStatus::Success)
      return;

    // Lower the condition, which always produces a boolean value.
    auto condValue = getCondition(headerBr, *this);
    if (!condValue.oper) {
      S = GLStatus::Error;
      return;
    }

    // If the condition is true when the loop should continue, invert the
    // condition.
    // TODO: add a unit test to cover this case.
    if (headerBr->getTrueBB() == r->getExit()) {
      condValue = createNotOp(condValue, headerBr->getDebugLocation(), *this);
      if (!condValue.oper) {
        S = GLStatus::Error;
        return;
      }
    }

    // For non TPU/XLA case, cast the boolean value to int32, a workaround as
    // needed to get while loop to run on GPU (b/65752372).
    if (thisDeviceType != DeviceType::TPU) {
      // FIXME: this added cast may not work for XlaWhile. Revisit whether/how
      // to support loops in XLA GPU.
      condValue =
          castBoolToInt32(condValue, headerBr->getDebugLocation(), *this);
      if (!condValue.oper) {
        S = GLStatus::Error;
        return;
      }
    }

    // The result of the function is our condition value.
    graphFn.outputs.push_back({/*SILArgument*/ nullptr, condValue});
  });
  if (S != GLStatus::Success)
    return S;

  // We are going to need the input values and types for the op creation: build
  // these lists now.
  SmallVector<TF_Output, 4> inputs;
  for (auto &input : loopBodyFn.inputs) {
    inputs.push_back(input.passedValue);
  }

  // Create TF_Function's for our condition and body.
  auto loc = headerBr->getDebugLocation();
  auto loopBodyFnName = getUniqueName(loc, "whilebody");
  SmallVector<TF_DataType, 4> inputTypes, outputTypes;
  bool bodyHasSideEffects = false;
  if (buildGraphFunction(loopBodyFn, loopBodyFnName, bodyHasSideEffects,
                         &inputTypes, &outputTypes))
    return GLStatus::Error;
  auto condFnName = getUniqueName(loc, "whilecond");
  bool condHasSideEffects = false;
  if (buildGraphFunction(condFn, condFnName, condHasSideEffects,
                         /*inputTypes*/ nullptr, /*outputTypes*/ nullptr))
    return GLStatus::Error;
  // This is needed for correctness, since we lower the header code again right
  // after creating the While op towards the end of this function.
  assert(!condHasSideEffects && "The loop should have been canonicalized by "
                                "now, where the cond has no side effects!");

  auto &graphFn = getCurrentGraphFunction();

  // Now we can create the actual operation itself.  This is the Tensorflow
  // op description that we are generating:
  // REGISTER_OP("While")
  //   .Input("input: T")
  //   .Output("output: T")
  //   .Attr("T: list(type) >= 0")
  //   .Attr("cond: func")
  //   .Attr("body: func")
  auto opLocString = getUniqueName(loc, "op");
  auto *op = TF_NewOperation(graphFn.getGraph(),
                             bodyHasSideEffects ? "While" : "StatelessWhile",
                             opLocString.c_str());
  TF_AddInputList(op, inputs.data(), inputs.size());
  TF_SetAttrTypeList(op, "T", inputTypes.data(), inputTypes.size());
  TF_SetAttrFuncName(op, "cond", condFnName.c_str(), condFnName.size());
  TF_SetAttrFuncName(op, "body", loopBodyFnName.c_str(), loopBodyFnName.size());

  auto *result = graphFn.finishOp(op, bodyHasSideEffects,
                                  /*isEligibleForTPU*/ true, status);
  if (checkStatus(getUserSourceLocation(loc)))
    return GLStatus::Error;

  // The live-out value from the while loop was the state of the SILArgument's
  // at the time that the termination program stopped.  Those SILArgument values
  // dominate the exit branch, so they may be used by code after the while.
  // Install them in our name lookup table.
  for (unsigned i = 0, e = headerBB->getArguments().size(); i != e; ++i) {
    addValueMapping(SILOpResult(headerBB->getArgument(i), 0), {result, (int)i});
  }

  // In addition to the SIL arguments themselves, all of the code in the header
  // block dominates the exit as well and may well be used by code outside the
  // loop.  We've already emit it into the condition function and the while loop
  // body, so emit it one more time outside the loop for good measure.  We
  // should be able to remove this when/if we get a proper model for loops as
  // described above.
  return lowerBasicBlock(r->getHeader(), /*skipTerminator:*/ true);
}

GLStatus
TFGraphFunctionLowering::lowerConditionalRegion(ConditionalSESERegion *r) {
  // Start by lowering any code that exists in the block that leads up to the
  // conditional branch.  This ensures that the condition bool is available.
  GLStatus S = lowerBasicBlock(r->getBranchBB(), /*skipTerminator:*/ true);
  if (S != GLStatus::Success)
    return S;

  // The branch block should end with a conditional branch on a tf_tensor_to_i1
  // invocation.
  auto condBr = cast<CondBranchInst>(r->getBranchBB()->getTerminator());
  auto loc = condBr->getDebugLocation();

  auto condValue = getCondition(condBr, *this);
  if (!condValue.oper)
    return GLStatus::Error;

  // Lower the true and false bodies to graph functions.
  auto trueCodeFn = lowerToFunction([&]() {
    // Lower all of the code inside the region (which can of course recursively
    // create functions and call them as ops.
    if (auto trueRegion = r->getTrue()) {
      S = lowerRegion(trueRegion);
      if (S != GLStatus::Success)
        return;
    }
  });
  if (S != GLStatus::Success)
    return S;

  auto falseCodeFn = lowerToFunction([&]() {
    // Lower all of the code inside the region (which can of course recursively
    // create functions and call them as ops.
    if (auto falseRegion = r->getFalse()) {
      S = lowerRegion(falseRegion);
      if (S != GLStatus::Success)
        return;
    }
  });
  if (S != GLStatus::Success)
    return S;

  // We are generating the "If" TensorFlow node, which takes an input
  // condition as a bool, and functions to run for the true/false branch that
  // are controlled by the condition.  The operation takes a type list for the
  // inputs that are fed into both the true/false functions - this is the union
  // of the live in sets for the true/false region.  It also takes a type list
  // that captures the union of the outputs of the two functions, this directly
  // corresponds to SIL BB parameters in the merge block.

  // Unify live input values.  Make sure the input lists are in a single
  // consistent order, and remember the inputs for when we build the op later.
  // Our approach on this is to build a set of the true inputs, then add any
  // unique-to-the-false-function entries, then rearrange the false list to
  // match the true list.
  llvm::SmallSet<SILOpResult, 4> trueInputs;
  for (auto &input : trueCodeFn.inputs) {
    bool inserted = trueInputs.insert(input.value).second;
    (void)inserted;
    assert(inserted && "A passed value shouldn't be added multiple times");
  }

  // Scan the false function, adding entries to the true fn input list if it
  // lacks them, and build the false function index.
  llvm::SmallDenseMap<SILOpResult, unsigned> falseInputIndex;
  for (unsigned i = 0, e = falseCodeFn.inputs.size(); i != e; ++i) {
    auto &input = falseCodeFn.inputs[i];

    // Keep track of the all the false function entries (and their index) in the
    // falseInputIndex.
    auto &entry = falseInputIndex[input.value];
    assert(entry == 0 && "A passed value shouldn't be added multiple times");
    entry = i + 1; // Entry in the map is 1-biased.

    // Check to see if the true function already has this passed value.
    if (!trueInputs.insert(input.value).second)
      continue; // Ignore common entries.

    // If not, add the parameter to the true list.
    auto result = createParameter(input.value, input.passedValue, trueCodeFn);
    if (result.oper == nullptr)
      return GLStatus::Error;
  }

  // Okay, we now know that the true function has all of the input parameters,
  // and that the false function has a subset of them.  We also know where they
  // are in the false input list if present.  Rebuild the false function's input
  // list in the right order to match the true function, adding any missing
  // entries as we go.
  auto falseInputList = std::move(falseCodeFn.inputs);
  falseCodeFn.inputs.clear();

  // Since we're walking the canonical list, also collect the info we need to
  // create the op node later.
  SmallVector<TF_Output, 4> inputs;

  for (auto &input : trueCodeFn.inputs) {
    // Build info we need to create the op node later.
    inputs.push_back(input.passedValue);

    // Figure out where the false node parameter should come from.
    auto entry = falseInputIndex[input.value];

    // If the false function already had this parameter set up, use it.
    if (entry != 0) {
      falseCodeFn.inputs.push_back(falseInputList[entry - 1]);
    } else {
      // Otherwise, we need to create a new parameter and add it.  Fortunately
      // this automatically adds it to the false function's input list for us.
      auto result =
          createParameter(input.value, input.passedValue, falseCodeFn);
      if (result.oper == nullptr)
        return GLStatus::Error;
    }
  }

  // The output lists for the true/false branch should have exactly the same
  // number of outputs, because they are based on the BB arguments that are
  // being filled in by the conditional region.  That said, the lists could
  // be in different orders.  Canonicalize the false region to match the true
  // region, and keep track of the output types for later consumption.
  assert(trueCodeFn.outputs.size() == falseCodeFn.outputs.size() &&
         "True and false region should produce same set of result values");
  for (unsigned i = 0, e = trueCodeFn.outputs.size(); i != e; ++i) {
    auto arg = trueCodeFn.outputs[i].first;

    // If the false code function has the results in the wrong order, reorder
    // them.  This isn't an efficient algorithm, but should suffice for now.
    if (arg == falseCodeFn.outputs[i].first)
      continue;

    // Scan until we find the matching argument list, then swap it into the
    // right place, to make it consistent with the trueCode function.
    for (unsigned j = i + 1;; ++j) {
      assert(j != e && "true/false code don't common argument list");
      if (arg == falseCodeFn.outputs[j].first) {
        std::swap(falseCodeFn.outputs[i], falseCodeFn.outputs[j]);
        break;
      }
    }
  }

  // Create the graph functions for the true/false code.
  auto trueFnName = getUniqueName(loc, "true");
  bool trueFnHasSideEffects = false;
  SmallVector<TF_DataType, 4> inputTypes, outputTypes;
  if (buildGraphFunction(trueCodeFn, trueFnName, trueFnHasSideEffects,
                         &inputTypes, &outputTypes))
    return GLStatus::Error;
  bool falseFnHasSideEffects = false;
  auto falseFnName = getUniqueName(loc, "false");
  if (buildGraphFunction(falseCodeFn, falseFnName, falseFnHasSideEffects,
                         /*inputTypes*/ nullptr, /*outputTypes*/ nullptr))
    return GLStatus::Error;

  auto &graphFn = getCurrentGraphFunction();

  // Finally, we can create the actual operation itself.  This is the Tensorflow
  // op description that we are generating:
  // REGISTER_OP("If")
  //   .Input("cond: Tcond")
  //   .Input("inputs: Tin")
  //   .Output("output: Tout")
  //   .Attr("Tcond: type")
  //   .Attr("then_branch: func")
  //   .Attr("else_branch: func")
  //   .Attr("Tin: list(type) >= 0")
  //   .Attr("Tout: list(type) >= 0")
  auto opLocString = getUniqueName(loc, "op");
  bool useSideEffectingIf = trueFnHasSideEffects || falseFnHasSideEffects;
  auto *op = TF_NewOperation(graphFn.getGraph(),
                             useSideEffectingIf ? "If" : "StatelessIf",
                             opLocString.c_str());
  TF_AddInput(op, condValue);
  TF_AddInputList(op, inputs.data(), inputs.size());
  TF_SetAttrTypeList(op, "Tin", inputTypes.data(), inputTypes.size());
  TF_SetAttrTypeList(op, "Tout", outputTypes.data(), outputTypes.size());
  TF_SetAttrFuncName(op, "then_branch", trueFnName.c_str(), trueFnName.size());
  TF_SetAttrFuncName(op, "else_branch", falseFnName.c_str(),
                     falseFnName.size());

  auto *result = graphFn.finishOp(op, useSideEffectingIf,
                                  /*isEligibleForTPU*/ true, status);
  if (checkStatus(getUserSourceLocation(loc)))
    return GLStatus::Error;

  // Remember each of the results so that any references to the SIL BBArguments
  // that got defined end up referring to this node.
  // Also set the graph function outputs. Note they may have been set by the
  // lowering of a prior conditional region.
  for (int i = 0, e = trueCodeFn.outputs.size(); i != e; ++i) {
    TF_Output outputNode = {result, i};
    addValueMapping({trueCodeFn.outputs[i].first, 0}, outputNode);
    graphFn.outputs.push_back({trueCodeFn.outputs[i].first, outputNode});
  }
  return GLStatus::Success;
}

GLStatus TFGraphFunctionLowering::lowerRegion(SESERegionTree *region) {
  switch (region->getKind()) {
  case SESERegionTree::SingleBlock:
    return lowerBasicBlock(cast<SingleBlockSESERegion>(region)->getBB());
  case SESERegionTree::Sequence:
    return lowerSequenceRegion(cast<SequenceSESERegion>(region));
  case SESERegionTree::Shared:
    return lowerSharedRegion(cast<SharedSESERegion>(region));
  case SESERegionTree::WhileLoop:
    return lowerWhileLoopRegion(cast<WhileLoopSESERegion>(region));
  case SESERegionTree::Conditional:
    return lowerConditionalRegion(cast<ConditionalSESERegion>(region));
  }
  llvm_unreachable("unknown region kind");
}

//===----------------------------------------------------------------------===//
// Top Level driver
//===----------------------------------------------------------------------===//

/// Create a "Placeholder" op parameter input on the specified function for the
/// specified SIL Value.  When this is created on inner functions, passedValue
/// indicates the value that is passed in to fulfill this parameter from the
/// next outer scope.  For the top-level parameters to the SIL function, the
/// passedValue can be null.
/// On error, return a "NULL" value, and emit an error diagnostic.
TF_Output TFGraphFunctionLowering::createParameter(SILOpResult value,
                                                   TF_Output passedValue,
                                                   GraphFunctionBody &fn) {
  auto opName = "arg_" + llvm::utostr(OpID++);
  auto *desc = TF_NewOperation(fn.getGraph(), "Placeholder", opName.c_str());
  auto loc = value.first.getLoc();
  auto type = getTensorFlowDataType(getOpResultType(value), loc);
  if (!type) {
    internalError(loc, "use of unknown dtype!");
    return {nullptr, 0};
  }
  TF_SetAttrType(desc, "dtype", type);

  // Placeholder nodes are never placed on TPU.
  auto result = fn.finishOp(desc, /*side effects*/ false,
                            /*isEligibleForTPU*/ false, status);
  if (checkStatus(loc))
    return {nullptr, 0};

#ifndef NDEBUG
  // Verify we haven't seen this value yet.
  if (value.first) {
    for (auto i : fn.inputs)
      assert(i.value != value && "adding redundant value");
  }
#endif

  assert(result != nullptr &&
         "expect a non-NULL result whenever the TensorFlow status is OK");

  // Success!  Remember this parameter, and the value that is passed in.
  fn.inputs.push_back({{result, 0}, passedValue, value});
  return {result, 0};
}

/// Lower the specified list of SIL arguments to a bunch of parameters, filling
/// the inputs list for the current function.  If the passedValues array is
/// non-empty, it specifies the passed values to add to the input.
GLStatus TFGraphFunctionLowering::lowerArgumentsToParams(
    ArrayRef<SILArgument *> args, ArrayRef<TF_Output> passedValues,
    SILLocation loc) {
  auto &graphFn = getCurrentGraphFunction();
  unsigned idx = 0;
  for (auto arg : args) {
    auto passedValue = TF_Output();
    if (!passedValues.empty())
      passedValue = passedValues[idx++];

    auto result = createParameter({arg, 0}, passedValue, graphFn);
    if (result.oper == nullptr)
      return GLStatus::Error;

    addValueMapping({arg, 0}, result);
  }
  return GLStatus::Success;
}

/// Build a function around the code produced by the specified std::function.
GraphFunctionBody
TFGraphFunctionLowering::lowerToFunction(const std::function<void()> &body) {
  // Push a scope, allowing us to keep track of any live-in values in the
  // true code.  These will need to become tuple elements live across the
  // loop.
  ValueMappingScopedHashTable::ScopeTy scope(valueMapping);

  /// Start a new graph function.
  functionStack.push_back(GraphFunctionBody(thisDeviceType, deviceInfo));

  // Lower the code in the body however the caller wants to do it.
  body();

  auto result = std::move(functionStack.back());
  functionStack.pop_back();
  return result;
}

bool TFGraphFunctionLowering::addTopLevelTPUConfigLogic(
    TF_Operation **metadataNode) {
  {
    auto *desc = TF_NewOperation(resultGraph, "TPUReplicateMetadata",
                                 "TPUReplicate/TPUReplicateMetadata");
    TF_SetAttrInt(desc, "num_replicas", 1);
    markNodeAsTPUReplicated(desc);
    *metadataNode = TF_FinishOperation(desc, status);
    if (checkStatus(SILFn.getLocation()))
      return true;
  }

  {
    auto *desc = TF_NewOperation(resultGraph, "ConfigureDistributedTPU",
                                 "ConfigureDistributedTPU");
    TF_SetDevice(desc, TF_DEFAULT_TPU_DEVICE);
    TF_SetAttrBool(desc, "is_global_init", true);
    TF_FinishOperation(desc, status);
    if (checkStatus(SILFn.getLocation()))
      return true;
  }

  {
    auto *desc = TF_NewOperation(resultGraph, "TPUCompilationResult",
                                 "TPUCompilationResult");
    TF_SetDevice(desc, TF_DEFAULT_CPU_DEVICE);
    TF_SetAttrString(desc, "_tpu_compilation_status", TPU_CLUSTER_ATTR_VALUE,
                     strlen(TPU_CLUSTER_ATTR_VALUE));
    TF_AddControlInput(desc, *metadataNode);
    TF_FinishOperation(desc, status);
    if (checkStatus(SILFn.getLocation()))
      return true;
  }

  return false;
}

bool TFGraphFunctionLowering::buildGraphNodesForTopLevelFunctionCall(
    StringRef funcOpType, bool isPrimaryFn, ArrayRef<TF_DataType> inputTypes,
    ArrayRef<TF_DataType> outputTypes, TF_Operation *&metadataNodeForTPU) {
  if (isPrimaryFn && thisDeviceType == DeviceType::TPU && !metadataNodeForTPU) {
    if (addTopLevelTPUConfigLogic(&metadataNodeForTPU)) {
      // An error has occurred. Abort graph generation.
      return true;
    }
    assert(metadataNodeForTPU);
  }

  // Now we create top level graph nodes to invoke the function.
  // Say the function F takes 1 input I and produces 1 output O:
  //
  // - In the non-TPU case, create the following 3 nodes: I (Placeholder)
  //   -> F (with op type being F) -> O (Identity).
  //
  // - In the TPU case, create additional input and output nodes as follows:
  //  - Input rewrite:
  //   a) Without infeed: I feeds a TPUReplicatedInput node (both running on
  //     CPU), which in turn feeds an Identity node IN, and IN feeds F (IN and
  //     F run on TPU).
  //   b) With infeed: I feeds an InfeedEnqueueTuple node (both running on
  //     CPU). Add a corresponding InfeedDequeueTuple, feeding F (both running
  //     on TPU).
  //  - Output rewrite: F outputs to an Identity node ON (running on TPU),
  //     which feeds a TPUReplicatedOutput node, which in turn feeds another
  //     Identity node. The last node is the output node to run the TF graph.
  //
  // Recall naming convention: The graph nodes corresponding to I, F and O above
  // are respectively named tfc_input_0_F, tfc_func_F and tfc_output_0_F.
  //
  // The above discussion generalizes to multiple inputs and/or outputs.
  // FIXME: Lift the current restriction that the # TPU replicas is always 1.
  std::string funcOpTypeStr = funcOpType.str();
  std::string funcNodeName = "tfc_func_" + funcNodeBaseName;
  TF_OperationDescription *funcDesc =
      TF_NewOperation(resultGraph, /*op_type*/ funcOpTypeStr.c_str(),
                      /*op_name*/ funcNodeName.c_str());
  if (thisDeviceType == DeviceType::TPU) {
    markNodeAsTPUReplicated(funcDesc);
  } else {
    TF_SetDevice(funcDesc, thisDeviceTypeStr.c_str());
  }

  // Handle inputs.
  for (unsigned i = 0, e = inputTypes.size(); i != e; ++i) {
    std::string inputNodeName =
        "tfc_input_" + std::to_string(i) + "_" + funcNodeBaseName;
    TF_OperationDescription *inputDesc =
        TF_NewOperation(resultGraph, "Placeholder", inputNodeName.c_str());
    TF_SetAttrType(inputDesc, "dtype", inputTypes[i]);
    TF_Operation *placeholder = TF_FinishOperation(inputDesc, status);
    if (checkStatus(SILFn.getLocation()))
      return true;
    TF_Output inputNode{placeholder, 0};
    if (thisDeviceType != DeviceType::TPU) {
      // Feed I directly into F.
      TF_AddInput(funcDesc, inputNode);
    } else {
      // Add some intermediate nodes between I and F.
      TF_Operation *replicatedInput;
      {
        std::string nodeName =
            funcNodeBaseName + "/TPUReplicate/input" + std::to_string(i);
        auto *desc = TF_NewOperation(resultGraph, "TPUReplicatedInput",
                                     nodeName.c_str());
        SmallVector<TF_Output, 1> input;
        input.push_back(inputNode);
        // This node requires an input list.
        TF_AddInputList(desc, input.data(), input.size());
        replicatedInput = TF_FinishOperation(desc, status);
        if (checkStatus(SILFn.getLocation()))
          return true;
      }
      {
        std::string nodeName = funcNodeBaseName +
                               "/TPUReplicate/replicated_input_" +
                               std::to_string(i);
        auto *desc = TF_NewOperation(resultGraph, "Identity", nodeName.c_str());
        TF_AddControlInput(desc, metadataNodeForTPU);
        TF_AddInput(desc, {replicatedInput, 0});
        markNodeAsTPUReplicated(desc);
        TF_Operation *idInput = TF_FinishOperation(desc, status);
        if (checkStatus(SILFn.getLocation()))
          return true;
        TF_AddInput(funcDesc, {idInput, 0});
      }
    }
  }

  // Finish constructing the function node.
  TF_Operation *funcNode = TF_FinishOperation(funcDesc, status);
  if (checkStatus(SILFn.getLocation()))
    return true;

  // Now handle outputs.
  for (unsigned i = 0, e = outputTypes.size(); i != e; ++i) {
    std::string outputNodeName =
        "tfc_output_" + std::to_string(i) + "_" + funcNodeBaseName;
    TF_OperationDescription *outputDesc =
        TF_NewOperation(resultGraph, "Identity", outputNodeName.c_str());
    TF_Output funcOutputNode{funcNode, static_cast<int>(i)};
    if (thisDeviceType != DeviceType::TPU) {
      // Feed F directly into O.
      TF_AddInput(outputDesc, funcOutputNode);
    } else {
      // Add some intermediate nodes between F and O.
      TF_Operation *outputIdNode;
      {
        const std::string nodeName =
            funcNodeBaseName + "/TPUReplicate/Identity_" + std::to_string(i);
        auto *desc = TF_NewOperation(resultGraph, "Identity", nodeName.c_str());
        TF_AddInput(desc, funcOutputNode);
        const auto deviceName =
            std::string("/device:") + DEVICE_TPU_REPLICATED_CORE;
        TF_SetDevice(desc, deviceName.c_str());
        markNodeAsTPUReplicated(desc);
        outputIdNode = TF_FinishOperation(desc, status);
        if (checkStatus(SILFn.getLocation()))
          return true;
      }
      {
        const std::string nodeName =
            funcNodeBaseName + "/TPUReplicate/output" + std::to_string(i);
        auto *desc = TF_NewOperation(resultGraph, "TPUReplicatedOutput",
                                     nodeName.c_str());
        TF_AddInput(desc, {outputIdNode, 0});
        TF_SetAttrInt(desc, "num_replicas", 1);
        TF_Operation *replicatedOutputNode = TF_FinishOperation(desc, status);
        if (checkStatus(SILFn.getLocation()))
          return true;
        TF_AddInput(outputDesc, {replicatedOutputNode, 0});
      }
    }
    /*TF_Operation *outputNode =*/TF_FinishOperation(outputDesc, status);
    if (checkStatus(SILFn.getLocation()))
      return true;
  }

  // Everything is good!
  return false;
}

bool TFGraphFunctionLowering::buildGraphFunction(
    const GraphFunctionBody &graphBody, StringRef funcName,
    bool &funcHasSideEffects, SmallVectorImpl<TF_DataType> *inputTypes,
    SmallVectorImpl<TF_DataType> *outputTypes) {
  // Inform our callers whether this function contains side effects or not.
  funcHasSideEffects = graphBody.funcHasSideEffects;

  SmallVector<TF_Output, 4> ins, outs;
  ins.reserve(graphBody.inputs.size());
  if (inputTypes) {
    inputTypes->clear();
    inputTypes->reserve(graphBody.inputs.size());
  }
  for (unsigned i = 0, e = graphBody.inputs.size(); i != e; ++i) {
    ins.push_back(graphBody.inputs[i].parameter);
    if (inputTypes)
      inputTypes->push_back(TF_OperationOutputType(ins[i]));
  }
  outs.reserve(graphBody.outputs.size());
  if (outputTypes) {
    outputTypes->clear();
    outputTypes->reserve(graphBody.outputs.size());
  }
  for (unsigned i = 0, e = graphBody.outputs.size(); i != e; ++i) {
    outs.push_back(graphBody.outputs[i].second);
    if (outputTypes)
      outputTypes->push_back(TF_OperationOutputType(outs[i]));
  }

  LLVM_DEBUG(llvm::dbgs() << "Creating graph function " << funcName << "\n");
  auto resultFn =
      TF_GraphToFunction(graphBody.getGraph(), funcName.str().c_str(),
                         /*append_hash_to_fn_name*/ false,
                         /*num_opers*/ -1,
                         /*opers*/ nullptr,
                         /*numinputs*/ ins.size(),
                         /*inputs*/ ins.data(),
                         /*noutputs*/ outs.size(),
                         /*outputs*/ outs.data(),
                         /*outputnames*/ nullptr,
                         /*functionoptions*/ nullptr, "", status);
  // Diagnose any error that occurred if it happened building the graph.
  if (checkStatus(SILFn.getLocation()))
    return true;
  SWIFT_DEFER { TF_DeleteFunction(resultFn); };

  // Now that we have a function, copy it into the result graph.  We do this
  // multi-stage create-then-form-function-then-copy-function approach because
  // we don't want the nodes for the original function body to end up in the
  // result graph.
  TF_GraphCopyFunction(resultGraph, resultFn, /*gradient*/ nullptr, status);
  if (checkStatus(SILFn.getLocation()))
    return true;

  // Everything is good!
  return false;
}

bool TFGraphLowering::serializeGraphProtoBuf(ASTContext &ctx,
                                             SILLocation errorLoc,
                                             std::vector<char> &bytes) {
  TF_Status *status = TF_NewStatus();
  // Create a buffer to hold the result.
  auto buffer = TF_NewBuffer();
  SWIFT_DEFER {
    TF_DeleteBuffer(buffer);
    TF_DeleteStatus(status);
  };

  // Serialize the graph into the buffer.
  TF_GraphToGraphDef(graph.get(), buffer, status);
  if (TF_GetCode(status) != TF_OK) {
    diagnose(ctx, errorLoc, diag::tf_lowering_error, TF_Message(status));
    return true;
  }

  // If the user wants a copy of the graph in /tmp, emit it now.
  if (TFDumpGraph) {
    int resultFD = -1;
    SmallString<64> resultPath;
    auto error = llvm::sys::fs::createTemporaryFile("tf-dump-graph", "pb",
                                                    resultFD, resultPath);
    if (error) {
      llvm::errs() << "error opening '" << resultPath.str()
                   << "' for -tf-dump-graph emission!\n";
    } else {
      llvm::outs() << "wrote binary graph of " << buffer->length
                   << " bytes to '" << resultPath.str() << "'\n";
      llvm::raw_fd_ostream file(resultFD, /*shouldClose*/ true,
                                /*unbuffered*/ false);
      file.write((const char *)buffer->data, buffer->length);
    }

    // Also write in a textual format.
    error = llvm::sys::fs::createTemporaryFile("tf-dump-graph", "pbtxt",
                                               resultFD, resultPath);
    if (error) {
      llvm::errs() << "error opening '" << resultPath.str()
                   << "' for -tf-dump-graph emission!\n";
    } else {
      size_t len;
      const char *content = TF_GraphDebugString(graph.get(), &len);
      llvm::outs() << "wrote textual graph of " << len << " bytes to '"
                   << resultPath.str() << "'\n";
      llvm::raw_fd_ostream file(resultFD, /*shouldClose*/ true,
                                /*unbuffered*/ false);
      file.write(content, len);

      llvm::outs() << "--- TFPartition GraphDef Proto: \n";
      llvm::outs() << content << "\n";
      llvm::outs() << "----\n";
      llvm::outs().flush();

      free((void *)content);
    }
  }

  auto bufPtr = (const char *)buffer->data;
  bytes = std::vector<char>(bufPtr, bufPtr + buffer->length);
  return false;
}

#endif // SWIFT_ENABLE_TENSORFLOW

/// Gets a function name that can be used as a TF op name. This name is based on
/// fn's name, with
/// - disallowed characters removed, and
/// - fn's module name added, so that identically-named functions in different
///   modules do not clash (this happens for the "main" function in the lldb
///   repl)
std::string getTFCompatibleFuncName(SILFunction *fn) {
  auto fnName = fn->getName();
  if (fnName.startswith("$"))
    fnName = fnName.substr(1);

  auto module = fn->getModule().getAssociatedContext()->getParentModule();
  return (*module->getReverseFullModuleName() + "_" + fnName).str();
}

bool TFGraphLowering::lowerTFGraphOrFunction(
    StringRef hostFnName, SILFunction *fn,
    const std::string &graphFnNameForCaller, bool isAcceleratorOnly,
    const GraphFunctionDeviceInfo &deviceInfo) {
#ifndef SWIFT_ENABLE_TENSORFLOW
  // This should never be called if TensorFlow support isn't enabled, but just
  // in case, emit an error message so a misconfiguration is diagnosable.
  llvm::errs() << "TensorFlow support is not built into this Swift compiler.\n";
  return true;
#else
  // If we're generating a graph for XLA, we need to structurize the CFG into
  // single-entry-single-exit regions.
  auto structure = canonicalizeCFGForXLA(fn);

  if (auto outs = getTFDumpIntermediateStream()) {
    *outs << "--- XLA CFG Canonicalize: " << fn->getName() << "\n";
    structure->print(*outs);
    *outs << "\n--- XLA CFG Canonicalize end\n";
    *outs << "----\n";
    outs->flush();
  }

  // TensorFlow likes to print out lots of informational messages to the
  // console, which are just noise.  This is apparently controlled through
  // an environment variable, so we set it to silence these informational logs.
  //
  // TODO: is there a better way to silence this?  We could forcibly change
  // the file descriptor mapping...
  setenv("TF_CPP_MIN_LOG_LEVEL", "2", 1);

  TF_Status *status = TF_NewStatus();

  SWIFT_DEFER { TF_DeleteStatus(status); };

  DevicePartitioner partitioner(parentTransform, *fn, deviceInfo,
                                nextTensorTransferId);
  auto entryFnBaseName = graphFnNameForCaller;
  unsigned helperFuncId = 0;
  SmallVector<std::pair<StringRef, SILLocation>, 1> pendingGraphFnNames;
  for (auto deviceType : deviceInfo.getUsedDeviceTypes()) {
    auto *perDeviceFn = partitioner.extractFunctionForDevice(deviceType);
    SWIFT_DEFER {
      // Remove the partitioned function so it doesn't go through the normal
      // compiler flow.
      parentTransform.getPassManager()->notifyWillDeleteFunction(perDeviceFn);
      perDeviceFn->getModule().eraseFunction(perDeviceFn);
    };
    bool isPrimaryFn = deviceType == deviceInfo.primaryDeviceType;

    // The func op type is `graphFnName`, with the caller node name being
    // based on `funcNodeBaseName`.
    std::string funcNodeBaseName = entryFnBaseName;
    if (!isPrimaryFn) {
      funcNodeBaseName += "_helper_" + llvm::utostr(helperFuncId);
      ++helperFuncId;
    }
    TFGraphFunctionLowering graphFuncGen(
        *perDeviceFn, deviceType, deviceInfo, funcNodeBaseName, graphFunctions,
        graph.get(), pendingGraphFnNames, status);
    GLStatus S = GLStatus::Success;
    auto graphFnBody =
        graphFuncGen.lowerToFunction([&graphFuncGen, perDeviceFn, &S]() {
          // This is the top level of the function, add its formal arguments.
          S = graphFuncGen.lowerArgumentsToParams(
              perDeviceFn->getArguments(), {}, perDeviceFn->getLocation());
          if (S != GLStatus::Success)
            return;

          // Lower all of the code inside the function body (which can of course
          // recursively creates functions and call them as ops.
          auto structure = canonicalizeCFGForXLA(perDeviceFn);
          S = graphFuncGen.lowerRegion(structure.get());
        });
    if (S != GLStatus::Success)
      return true;

    std::string graphFnName =
        isAcceleratorOnly ? graphFnNameForCaller
                          : getTFCompatibleFuncName(perDeviceFn);
    assert(!graphFunctions.count(graphFnName));

    bool funcHasSideEffects = false;
    SmallVector<TF_DataType, 4> inputTypes, outputTypes;
    // Ignore `funcHasSideEffects` when the call returns, since this top level
    // graph function gets called directly in a TF_SessionRun() call.
    if (graphFuncGen.buildGraphFunction(graphFnBody, graphFnName,
                                        funcHasSideEffects, &inputTypes,
                                        &outputTypes))
      return true;

    if (!isPrimaryFn) {
      assert(inputTypes.empty());
      assert(outputTypes.empty());
    }

    // Create the top level graph nodes, only when we are lowering to a graph
    // for host-side invocation.
    if (!isAcceleratorOnly &&
        graphFuncGen.buildGraphNodesForTopLevelFunctionCall(
            graphFnName, isPrimaryFn, inputTypes, outputTypes,
            metadataNodeForTPU))
      return true;
  }

  // Ok, we're done!
  LLVM_DEBUG(llvm::dbgs() << "Inserting a graph functions entry with host fn "
                          << hostFnName << "\n");
  assert(!graphFunctions.count(hostFnName));
  auto graphFn =
      llvm::make_unique<LoweredGraphFunction>(hostFnName, entryFnBaseName);
  graphFunctions[graphFn->silHostFnName] = std::move(graphFn);
  // This confirms that the StringRef-typed map key has a proper backing buffer.
  assert(graphFunctions.count(hostFnName));

  return false;
#endif
}

TFGraphLowering::TFGraphLowering(
    SILTransform &parentTransform,
    llvm::DenseMap<StringRef, std::unique_ptr<LoweredGraphFunction>>
        &graphFunctions)
    : parentTransform(parentTransform),
      graphFunctions(graphFunctions), graph(TF_NewGraph(), &TF_DeleteGraph) {}

bool TFGraphLowering::lowerTFFunction(
    StringRef hostFnName, SILFunction *fn,
    const GraphFunctionDeviceInfo &deviceInfo) {
  std::string graphFnName = getGraphFuncNameForFuncAttr(hostFnName);
  LLVM_DEBUG(llvm::dbgs() << "Lowering accelerator-only host fn " << hostFnName
                          << " to graph function " << graphFnName << "\n");
  return lowerTFGraphOrFunction(hostFnName, fn, graphFnName,
                                /*isAcceleratorOnly*/ true, deviceInfo);
}

bool TFGraphLowering::lowerTFGraph(StringRef hostFnName, SILFunction *fn,
                                   const GraphFunctionDeviceInfo &deviceInfo) {
  std::string entryFnBaseName = getTFCompatibleFuncName(fn);
  return lowerTFGraphOrFunction(hostFnName, fn, entryFnBaseName,
                                /*isAcceleratorOnly*/ false, deviceInfo);
}

//===----------------------------------------------------------------------===//
// TFLowerGraphTestPass
//===----------------------------------------------------------------------===//

namespace {
/// This is a SIL pass that drives the TF Graph generation process.  This is not
/// used by the compiler proper, it just exists so we can drive it through
/// sil-opt and write testcases.
struct TFLowerGraphTestPass : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    auto fn = getFunction();
    auto deviceInfo =
        GraphFunctionDeviceInfo::getForFunction(*fn, /*removeConfigInst*/ true);
    llvm::DenseMap<StringRef, std::unique_ptr<LoweredGraphFunction>>
        graphFunctions;
    TFGraphLowering graphLowering(*this, graphFunctions);
    if (graphLowering.lowerTFGraph(fn->getName(), fn, deviceInfo)) {
      llvm::errs() << "Failed to generate TFGraph for " << fn->getName()
                   << "\n";
      return;
    }
    size_t len;
    const char *content =
        TF_GraphDebugString(graphLowering.getGraphDebug(), &len);
    llvm::outs() << "--- TFPartition GraphDef Proto: " << fn->getName() << "\n";
    llvm::outs() << content << "\n";
    llvm::outs() << "----\n";
    llvm::outs().flush();
    free((void *)content);
  }
};

} // end anonymous namespace.

SILTransform *swift::createTFLowerGraph() { return new TFLowerGraphTestPass(); }
