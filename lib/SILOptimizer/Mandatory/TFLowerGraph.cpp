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

#include "TFUtilities.h"
#include "llvm/Support/CommandLine.h"
#ifdef SWIFT_ENABLE_TENSORFLOW
#include "TFCanonicalizeCFG.h"
#include "tensorflow/c/c_api.h"
#include "tensorflow/c/c_api_experimental.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Module.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#endif
using namespace swift;
using namespace tf;

static llvm::cl::opt<bool>
TFDumpGraph("tf-dump-graph", llvm::cl::init(false),
            llvm::cl::desc("Dump generated tensorflow graphs to /tmp"));

#ifdef SWIFT_ENABLE_TENSORFLOW
template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(SILFunction &fn, SILLocation loc, Diag<T...> diag, U &&...args) {
  auto &diags = fn.getASTContext().Diags;
  return diags.diagnose(loc.getSourceLoc(), diag, std::forward<U>(args)...);
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
    const GraphGlobalConfiguration& configuration;

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
    SmallVector<std::pair<SILArgument*, TF_Output>, 4> outputs;

    /// If this graph has any side-effecting operations, this is the most
    /// recently emitted operation that had side effects.  Otherwise, it is
    /// null.
    TF_Operation *controlDependenceValue = nullptr;

    /// This is a list of all of the operations that make up this function.
    std::vector<const TF_Operation*> operations;

    // When true, lower effectful ops (e.g. Swift->TF send ops), if any, in the
    // corresponding TF function. Currently in a While op context, these ops
    // should not be run in the cond function.
    // TODO(b/78472806): Add a more thorough and proper fix for effectful ops in
    // the cond function.
    bool shouldLowerEffectfulOps = true;

   public:
    GraphFunctionBody(DeviceType thisDeviceType,
                      const GraphGlobalConfiguration& configuration)
        : thisDeviceType(thisDeviceType),
          configuration(configuration),
          graph(TF_NewGraph(), &TF_DeleteGraph) {}

    TF_Graph *getGraph() const { return graph.get(); }

    /// "Finish" a tensorflow op under construction, and remember that it is
    /// part of this graph function.
    TF_Operation *finishOp(TF_OperationDescription *desc, bool hasSideEffects,
                           bool isEligibleForTPU, TF_Status *status) {
      // If this node has side effects and we've already emitted another node
      // that does, make sure to connect them with control dependencies to
      // preserve ordering.
      if (hasSideEffects && controlDependenceValue)
        TF_AddControlInput(desc, controlDependenceValue);

      // If this node should be put onto TPU, mark it with an attribute.
      if (thisDeviceType == DeviceType::TPU && isEligibleForTPU) {
        markNodeAsTPUReplicated(desc);
      }

      auto result = TF_FinishOperation(desc, status);
      operations.push_back(result);

      // If this op has side effects, remember it in case we need to chain it to
      // another one later.
      if (hasSideEffects)
        controlDependenceValue = result;

      return result;
    }

    // If there is a control dependence value, run it before producing an output
    // tensor in GraphFunctionBody.
    TF_Output maybeRunEffectfulOp(TF_Output result, TF_Status *status) {
      if (!controlDependenceValue) return result;

      std::string nodeName = "RunControlDependency";
      auto *desc = TF_NewOperation(getGraph(), "Identity", nodeName.c_str());
      TF_AddControlInput(desc, controlDependenceValue);
      TF_AddInput(desc, result);
      TF_Operation *newResult = finishOp(desc, /*hasSideEffects*/ false,
                                         /*isEligibleForTPU*/ false, status);
      controlDependenceValue = nullptr;
      return {newResult, 0};
    }
  };
}

namespace {
struct TFGraphLowering : public SILInstructionVisitor<TFGraphLowering> {
  SILFunction &SILFn;
  // The TF device to which the generated graph is targeting.
  const DeviceType thisDeviceType;
  const std::string thisDeviceTypeStr;
  const GraphGlobalConfiguration &configuration;
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

  /// This flag gets set if lowering code to the graph produces a TensorFlow
  /// error and emits a diagnostic.  This tells us to stop lowering and give up
  /// gracefully.
  bool errorOccurred = false;

 public:
  /// Generate one or more TF graph functions from `fn` targeting
  /// `thisDeviceType`, and add them to `resultGraph`.
  TFGraphLowering(SILFunction &fn, DeviceType thisDeviceType,
                  const GraphGlobalConfiguration &configuration,
                  TF_Graph *resultGraph, TF_Status *status)
      : SILFn(fn),
        thisDeviceType(thisDeviceType),
        thisDeviceTypeStr(getDeviceString(thisDeviceType)),
        configuration(configuration),
        resultGraph(resultGraph),
        status(status) {}

  /// Return the current graph function that is being set up.
  GraphFunctionBody &getCurrentGraphFunction() {
    return functionStack.back();
  }

  ~TFGraphLowering() {}

  /// Check whether the specified TensorFlow status object is valid or not.  If
  /// valid return false.  If invalid, emit a diagnostic and return true.
  bool checkStatus(SILLocation loc,
                   Diag<StringRef> id = diag::tf_lowering_error) {
    if (TF_GetCode(status) == TF_OK) return false;
    internalError(loc, TF_Message(status), id);
    return true;
  }

  void internalError(SILLocation loc, std::string message,
                     Diag<StringRef> id = diag::tf_lowering_error) {
    diagnose(SILFn, loc, id, message);
    errorOccurred = true;
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
  /// This sets hasSideEffects to true if the body of the graph had any side
  /// effecting operations.  This allows calls to the graph to set up control
  /// dependence edges properly.
  ///
  /// This emits an error and returns true on error.
  bool buildGraphFunction(const GraphFunctionBody &graphBody, StringRef name,
                          bool &hasSideEffects,
                          SmallVectorImpl<TF_DataType> *inputTypes,
                          SmallVectorImpl<TF_DataType> *outputTypes);

  /// Builds TF graph nodes for the top level TF function call, where
  /// `funcOpType` is the TF graph function name, and `funcNodeBaseName` is the
  /// node base name for calling that function. `inputs` and `outputs`
  /// respectively specify the inputs and outputs to the function.
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
      StringRef funcOpType, StringRef funcNodeBaseName, bool isPrimaryFn,
      ArrayRef<TF_DataType> inputTypes, ArrayRef<TF_DataType> outputTypes);

 private:  // Helpers to create TensorFlow graph nodes.
  unsigned OpID = 0;
  llvm::StringSet<> usedOpNames;

  /// Provides the context for creating the dataset / iterator node stack, along
  /// with an infeed enqueue node consuming the output of the iterator.
  struct DatasetCreationContext {
    /// The instruction corresponding to the builtin
    /// tfc.makeIteratorGetNextWithDatasets.
    BuiltinInst *datasetInst = nullptr;

    /// Specifies which (hard-coded) iterator stack to create.
    enum DataSource {
      // Use a fake iterator stack that's non-file-based, for unit testing only.
      FAKE = 0,
      // Read an MNIST file dataset.
      MNIST = 1,
      // Reads an Imagenet file dataset.
      IMAGENET = 2
    };
    DataSource dataSource;

    /// The file path for Imagenet or MNIST data. Olny defined when dataSource
    /// is not FAKE.
    StringRef filePath;

    /// The batch size for each IteratorGetNext call.
    int batchSize;

    /// Metadata for the infeed enqueue node creation.
    /// FIXME: Assess whether to use SmallVector.
    std::vector<int64_t> dims;
    std::vector<int> numDims;
    std::vector<int64_t*> dimPtrs;
    std::vector<TF_DataType> infeedInputDtypes;

   public:
    DatasetCreationContext(BuiltinInst *datasetInst, DataSource dataSource,
                           StringRef filePath, int batchSize,
                           ArrayRef<int64_t> dims, ArrayRef<int> numDims,
                           ArrayRef<TF_DataType> dTypes)
        : datasetInst(datasetInst),
          dataSource(dataSource),
          filePath(filePath),
          batchSize(batchSize),
          dims(dims),
          numDims(numDims),
          infeedInputDtypes(dTypes) {
      assert(numDims.size() == dTypes.size());
      auto dimPtr = this->dims.data();
      for (unsigned shape = 0; shape != numDims.size(); ++shape) {
        dimPtrs.push_back(dimPtr);
        dimPtr += numDims[shape];
      }
    }

    /// Returns the number of tensors produced by the dataset / iterator stack.
    int getNumTensors() const {
      return infeedInputDtypes.size();
    }

    /// `desc` can be the partially constructed infeed enqueue or dequeue node.
    void setInfeedTypeAndShapeList(TF_OperationDescription *desc) {
      TF_SetAttrTypeList(desc, "dtypes", infeedInputDtypes.data(),
                         infeedInputDtypes.size());
      TF_SetAttrShapeList(desc, "shapes", dimPtrs.data(), numDims.data(),
                          dimPtrs.size());
    }

    TF_Operation *makeIteratorGetNextWithDatasets(TF_Graph *resultGraph,
                                                  TF_Status *status) {
      if (dataSource == FAKE) {
        // For unit testing.
        return TF_MakeFakeIteratorGetNextWithDatasets(resultGraph, status);
      }
      return TF_MakeFileBasedIteratorGetNextWithDatasets(
          resultGraph, filePath.str().c_str(), batchSize, dataSource == MNIST,
          status);
    }
  };

  /// When non-NULL, uses TF dataset / iterators mechanism to feed input
  /// data. Current requirements / restrictions:
  /// 1. Only supported in the TPU TF graph generation mode with infeed support.
  /// 2. All input tensors of the TF graph must be supplied via this mechanism
  /// (in other words, the generated TF graph should require 0 input from other
  /// Placeholders, etc).
  std::unique_ptr<DatasetCreationContext> datasetCreationContext;

  /// Adds TPU config-related nodes to the graph, and sets `*metadataNode` to
  /// the created TPUReplicateMetadata node.
  ///
  /// This emits an error and returns true on error.
  bool addTopLevelTPUConfigLogic(TF_Operation **metadataNode);

 public:  // Lowering functionality.
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
      depth = functionStack.size()-1;
    } else {
      // This isn't particularly efficient, but this happens infrequently and
      // the scope stack should be shallow.
      SmallVector<ValueMappingScopedHashTable::ScopeTy *, 4> scopes;
      for (auto tmp = scope; tmp != nullptr; tmp = tmp->getParentScope())
        scopes.push_back(tmp);
      scope = scopes[scopes.size()-1-depth];
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
  TF_Output getOperandValue(SILOpResult v) {
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
    for (unsigned depth = valueInfo.second+1; depth != functionStack.size();
         ++depth) {
      // Create placeholder, add it as input to each function.
      value = createParameter(v, value, functionStack[depth]);
      if (errorOccurred) return {};

      // Remember that it is the available version of this value at that depth.
      addValueMapping(v, value, depth);
    }

    return value;
  }

  // These get special handling, they are only used as operands to tfops.
  void visitIntegerLiteralInst(IntegerLiteralInst *inst) {}
  void visitFloatLiteralInst(FloatLiteralInst *inst) {}
  void visitMetatypeInst(MetatypeInst *inst) {}
  void visitStringLiteralInst(StringLiteralInst *inst) {}

  void visitBuiltinInst(BuiltinInst *inst);

  void visitTupleInst(TupleInst *inst);
  void visitTupleExtractInst(TupleExtractInst *inst);
  void visitUncheckedRefCastInst(UncheckedRefCastInst *inst);

  void visitReturnInst(ReturnInst *inst);
  void visitBranchInst(BranchInst *inst);

  // visitSILInstruction is the bottom level of the instruction visitor, where
  // unhandled instructions bottom out in.
  void visitSILInstruction(SILInstruction *inst) {
    internalError(inst->getLoc(),
                  "GraphGen cannot lower this instruction yet");
    llvm::errs() << "Unhandled SIL instruction in TFGraphLowering:\n";
    inst->dump();
  }

  GraphFunctionBody lowerToFunction(const std::function<void()> &body);

  void lowerArgumentsToParams(ArrayRef<SILArgument *> args,
                              ArrayRef<TF_Output> passedValues,
                              SILLocation loc);

  void lowerBasicBlock(SILBasicBlock *bb, bool skipTerminator = false);
  void lowerRegion(SESERegionTree *region);
  void lowerSequenceRegion(SequenceSESERegion *r);
  void lowerWhileLoopRegion(WhileLoopSESERegion *r);
  void lowerConditionalRegion(ConditionalSESERegion *r);

 private:  // Helpers for lowering.
  /// Create a stack of TF dataset and iterator nodes up to IteratorGetNext.
  ///
  /// FIXME: Dissolve this builtin into a set of finer-grained, composable
  /// features.
  void visitTFDataset(BuiltinInst *inst);
  bool createDatasetIteratorNodesWithInfeedEnqueue();

  void visitTFOpInst(BuiltinInst *inst);

  void visitBuiltinSendToHostInst(SILTensorOpInfo &tfopInfo);
  void visitBuiltinRecvFromHostInst(SILTensorOpInfo &tfopInfo);
  // D2D means device-to-device.
  void visitBuiltinD2DTensorRecvInst(SILTensorOpInfo &tfopInfo);
  void visitBuiltinD2DTensorSendInst(SILTensorOpInfo &tfopInfo);

  // Helper functions to add different flavors of send/recv TF ops.
  void addTFRecvOp(BuiltinInst *inst, int transferId, StringRef srcDevice);
  void addTFSendOp(BuiltinInst *inst, int transferId, StringRef destDevice);
  // For the TPU infeed/outfeed related ops, the shape array of the tensor being
  // transferred is given by `dims`, `numDims` and `dimPtrs`.
  void addTPUDequeueOp(BuiltinInst *inst, bool isInfeed, int transferId,
                       ArrayRef<int64_t> dims, ArrayRef<int> numDims,
                       ArrayRef<int64_t *> dimPtrs);
  void addTPUEnqueueOp(BuiltinInst *inst, bool isInfeed, int transferId,
                       ArrayRef<int64_t> dims, ArrayRef<int> numDims,
                       ArrayRef<int64_t *> dimPtrs);
};
}

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
std::string TFGraphLowering::getUniqueName(SILDebugLocation loc,
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
      auto fnName = F->getName();

      // Drop ".device_partition" suffix off function names.
      if (fnName.endswith(".device_partition"))
        fnName = fnName.drop_back(strlen(".device_partition"));

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
      auto bufferID = SM.getBufferIdentifierForLoc(sourceLoc);
      name += "/" + bufferID.str() + "." + llvm::utostr(lineCol.first);
      name += "." + llvm::utostr(lineCol.second);
    }
  }

  // Escape op name.
  escapeOpName(name);

  // If we've already used this name, rename it to make it unique.
  while (!usedOpNames.insert(name).second) {
    name += "_" + llvm::utostr(OpID++);
  }

  return name;
}

TF_DataType TFGraphLowering::getTensorFlowDataType(SILType type,
                                                   SILLocation loc) {
  // Handle things like TensorHandle<Float>.
  switch (classifyTensorFlowValue(type)) {
  case TFValueKind::TensorHandle: {
    auto elt = getTensorHandleElementType(type.getSwiftRValueType());
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
    if (auto ty = (TF_DataType)convertSwiftTypeToTF(type.getSwiftRValueType()))
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

static TF_Tensor *convertValuesToTensor(ArrayRef<SingleValueInstruction*> elts,
                                        ArrayRef<int64_t> shape,
                                        TF_DataType dtype) {
  assert(dtype != TF_DataType() && "Expected to get a type!");
  auto dtypeSize = TF_DataTypeSize(dtype);

  // Compute the total memory size of the tensor value.
  unsigned totalElements = 1;
  for (auto dim : shape)
    totalElements *= dim;

  // Make an uninitialized tensor that is big enough for our value.
  auto *tensor = TF_AllocateTensor(dtype, shape.data(), shape.size(),
                                   dtypeSize*totalElements);

  // Set up its contents, element-wise.
  auto *ptr = (char*)TF_TensorData(tensor);
  APInt value;
  for (auto elt : elts) {
    if (auto *ILI = dyn_cast<IntegerLiteralInst>(elt)) {
      value = ILI->getValue();
    } else {
      value = cast<FloatLiteralInst>(elt)->getBits();
    }

    // FIXME: This will need a byte swap for big endian hosts.
    memcpy(ptr, value.getRawData(), dtypeSize);
    ptr += dtypeSize;
  }

  return tensor;
}

//===----------------------------------------------------------------------===//
// Helpers to create TensorFlow graph nodes.
//===----------------------------------------------------------------------===//

void TFGraphLowering::visitBuiltinInst(BuiltinInst *inst) {
  // If this is the magic tf_tensor_to_i1 builtin, then we completely ignore it.
  // the only user of it are things that take conditional branches, and they
  // handle it directly.
  if (inst->getName().str() == "tf_tensor_to_i1")
    return;
  if (inst->getName().str().startswith(
          "__tfop_tfc.makeIteratorGetNextWithDatasets"))
    return visitTFDataset(inst);
  if (inst->getName().str().startswith("__tfop_"))
    return visitTFOpInst(inst);

  assert(0 && "Unhandled builtin instruction");
}

bool TFGraphLowering::createDatasetIteratorNodesWithInfeedEnqueue() {
  assert(datasetCreationContext);
  TF_Operation *getnextOp =
      datasetCreationContext->makeIteratorGetNextWithDatasets(resultGraph,
                                                              status);

  // If the node builder failed, then the tfop definition is wrong, report
  // an error in a way that can hopefully be fixed - pointing to the op
  // definition in the Swift code, and emitting the TensorFlow error
  // information.
  if (checkStatus(getUserSourceLocation(
                      datasetCreationContext->datasetInst->getDebugLocation()),
                  diag::tfop_incorrect_definition))
    return true;

  // Add infeed enqueue to consume the output of the iterator.
  {
    auto *desc = TF_NewOperation(resultGraph, "InfeedEnqueueTuple",
                                 "InfeedEnqueueTuple");
    int numInputs = datasetCreationContext->getNumTensors();
    std::vector<TF_Output> infeedInputs;
    infeedInputs.reserve(numInputs);
    for (int i = 0; i < numInputs; ++i) {
      infeedInputs.push_back({getnextOp, i});
    }
    TF_AddInputList(desc, infeedInputs.data(), infeedInputs.size());
    TF_SetDevice(desc, DEFAULT_CPU_DEVICE);
    TF_SetAttrInt(desc, "device_ordinal", 0);
    datasetCreationContext->setInfeedTypeAndShapeList(desc);
    /*TF_Operation* enqueue =*/ TF_FinishOperation(desc, status);
    if (checkStatus(SILFn.getLocation())) return true;
  }

  return false;
}

/// If `type` is Tensor<T> or TensorHandle<T>, return the TF_DataType
/// corresponding to element type T. Otherwise, return 0.
static unsigned getTFDataTypeFromTensorGenericType(Type type) {
  auto *genTy = type->getAs<BoundGenericType>();
  if (!genTy || genTy->getGenericArgs().size() != 1) {
    return 0;
  }
  return convertSwiftTypeToTF(genTy->getGenericArgs()[0]);
}

/// Decode shape elements from the tfop builtin instruction, starting at
/// operand `operandIdx`, into `shape`.
static void decodeShapeElements(SILValue attrValue,
                                const SILTensorOpInfo &tfopInfo,
                                unsigned &operandIdx, unsigned operandEndIdx,
                                SmallVectorImpl<int64_t> &shape) {
  assert(isa<MetatypeInst>(attrValue) && "$shape should start with a metatype");
  while (operandIdx + 1 < operandEndIdx &&
         tfopInfo.operandClasses[operandIdx + 1].second ==
             SILTensorOpInfo::OperandClass::ArrayElement) {
    auto eltValue = tfopInfo.inst->getOperand(++operandIdx);
    auto intValue = cast<IntegerLiteralInst>(eltValue);
    shape.push_back(intValue->getValue().getLimitedValue());
  }
}

/// Decode the shape array attribute from the tfop builtin instruction, starting
/// at operand `operandIdx`, into `dims`, `numDims` and `dimPtrs`.
static void decodeShapeArray(const SILTensorOpInfo &tfopInfo,
                             unsigned &operandIdx, unsigned operandEndIdx,
                             SmallVectorImpl<int64_t> &dims,
                             SmallVectorImpl<int> &numDims,
                             SmallVectorImpl<int64_t *> &dimPtrs) {
  auto *inst = tfopInfo.inst;
  auto numShapes = cast<IntegerLiteralInst>(inst->getOperand(operandIdx))
                       ->getValue()
                       .getLimitedValue();
  for (unsigned shape = 0; shape != numShapes; ++shape) {
    auto prevNumDims = dims.size();
    ++operandIdx;  // We consumed an operand.
    auto nextOperand = inst->getOperand(operandIdx);
    assert(tfopInfo.operandClasses[operandIdx].second ==
               SILTensorOpInfo::OperandClass::Shape &&
           "expected a shape value");

    decodeShapeElements(nextOperand, tfopInfo, operandIdx, operandEndIdx, dims);
    numDims.push_back(int(dims.size() - prevNumDims));
  }

  // Now that we've build the array of dimensions, convert it to the array
  // of pointers that TensorFlow needs.  This is safe now that the vector
  // has finished its resizing.
  auto dimPtr = dims.data();
  for (unsigned shape = 0; shape != numShapes; ++shape) {
    dimPtrs.push_back(dimPtr);
    dimPtr += numDims[shape];
  }
}

void TFGraphLowering::visitBuiltinSendToHostInst(SILTensorOpInfo &tfopInfo) {
  auto &graphFn = getCurrentGraphFunction();
  // TODO(b/78472806): Add a more thorough and proper fix for effectful ops in
  // the while cond function.
  if (!graphFn.shouldLowerEffectfulOps) return;

  // Type check and process the parameters.
  // SendToHost has type <T> (input$T, tensorId$int, device$str) -> ()
  auto *inst = tfopInfo.inst;
  assert(inst->getNumResults() == 1);
  assert(inst->getNumOperands() == 3);
  assert(tfopInfo.isInput(0));

  TF_Output inputOp;
  TF_DataType inputType;
  {
    auto operand = inst->getOperand(0);
    inputOp = getOperandValue(operand);
    if (!inputOp.oper) return;  // Error occurred.
    inputType = getTensorFlowDataType(operand->getType(), inst->getLoc());
  }
  int tensorId = tfopInfo.getIntAttrOperand(1, "tensorId");
  assert(tfopInfo.getDeviceString() == DEFAULT_CPU_DEVICE &&
         "SendToHost must run on CPU device");

  // Add enqueue to the local graph function, and the corresponding dequeue to
  // the top level function, so that caller can dequeue tensors via SessionRun.
  TF_Operation *queueOp;
  {
    auto opName = "fifo_queue_" + llvm::itostr(tensorId);
    auto *desc =
        TF_NewOperation(graphFn.getGraph(), "FIFOQueueV2", opName.c_str());
    TF_SetDevice(desc, DEFAULT_CPU_DEVICE);
    TF_SetAttrInt(desc, "capacity", NAMED_TENSOR_QUEUE_CAPACITY);
    TF_SetAttrTypeList(desc, "component_types", &inputType, 1);
    TF_SetAttrString(desc, "shared_name", opName.data(), opName.size());
    queueOp = graphFn.finishOp(desc, /*hasSideEffects*/ false,
                               /*isEligibleForTPU*/ false, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return;
  }

  {
    auto opName = "fifo_queue_enqueue_" + llvm::itostr(tensorId);
    auto *desc =
        TF_NewOperation(graphFn.getGraph(), "QueueEnqueueV2", opName.c_str());
    TF_AddInput(desc, {queueOp, 0});
    TF_AddInputList(desc, &inputOp, 1);
    TF_SetDevice(desc, DEFAULT_CPU_DEVICE);
    TF_SetAttrTypeList(desc, "Tcomponents", &inputType, 1);

    graphFn.finishOp(desc, /*hasSideEffects*/ true,
                     /*isEligibleForTPU*/ false, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return;
  }

  // Now add dequeue to the top level graph function.
  // Multiple graph functions can have an enqueue op over the same tensorId.
  // One example is to enqueue tensors both within the while op's body
  // function, and also right after the while op is executed.
  // In that case, we only generate a single dequeue op at the top level.
  if (!processedTensorIdsForSend.insert(tensorId).second) return;

  // The code here is different enough from the above that it's not worth
  // extracting common code into functions.
  TF_Operation *globalQueueOp;
  {
    auto opName = "fifo_queue_" + llvm::itostr(tensorId);
    auto *desc = TF_NewOperation(resultGraph, "FIFOQueueV2", opName.c_str());
    TF_SetDevice(desc, DEFAULT_CPU_DEVICE);
    TF_SetAttrInt(desc, "capacity", NAMED_TENSOR_QUEUE_CAPACITY);
    TF_SetAttrTypeList(desc, "component_types", &inputType, 1);
    // FIXME: Revisit whether to populate "shared_name".
    TF_SetAttrString(desc, "shared_name", opName.data(), opName.size());
    globalQueueOp = TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return;
  }

  {
    auto opName = "fifo_queue_dequeue_" + llvm::itostr(tensorId);
    auto *desc = TF_NewOperation(resultGraph, "QueueDequeueV2", opName.c_str());
    TF_AddInput(desc, {globalQueueOp, 0});
    TF_SetDevice(desc, DEFAULT_CPU_DEVICE);
    TF_SetAttrTypeList(desc, "component_types", &inputType, 1);
    TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation()))) return;
  }
}

void TFGraphLowering::visitBuiltinRecvFromHostInst(SILTensorOpInfo &tfopInfo) {
  auto &graphFn = getCurrentGraphFunction();
  // TODO(b/78472806): Add a more thorough and proper fix for effectful ops in
  // the while cond function.
  if (!graphFn.shouldLowerEffectfulOps) return;

  // Type check and process the parameters.
  // recvFromHost has type <T> (tensorId$int, device$string) -> (T)
  auto *inst = tfopInfo.inst;
  assert(inst->getNumResults() == 1);
  assert(inst->getNumOperands() == 2);

  int tensorId = tfopInfo.getIntAttrOperand(0, "tensorId");
  assert(tfopInfo.getDeviceString() == DEFAULT_CPU_DEVICE &&
         "SendToHost must run on CPU device");

  TF_DataType outputType;
  {
    assert(inst->getNumResults() == 1);
    outputType =
        getTensorFlowDataType(inst->getResults()[0]->getType(), inst->getLoc());
  }

  // Add dequeue to the local graph function, and the corresponding enqueue to
  // the top level function, so that caller can enqueue tensors via SessionRun.
  TF_Operation *queueOp;
  {
    auto opName = "fifo_queue_" + llvm::itostr(tensorId);
    auto *desc =
        TF_NewOperation(graphFn.getGraph(), "FIFOQueueV2", opName.c_str());
    TF_SetDevice(desc, DEFAULT_CPU_DEVICE);
    TF_SetAttrInt(desc, "capacity", NAMED_TENSOR_QUEUE_CAPACITY);
    TF_SetAttrTypeList(desc, "component_types", &outputType, 1);
    TF_SetAttrString(desc, "shared_name", opName.data(), opName.size());
    queueOp = graphFn.finishOp(desc, /*hasSideEffects*/ false,
                               /*isEligibleForTPU*/ false, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return;
  }

  {
    auto opName = "fifo_queue_dequeue_" + llvm::itostr(tensorId);
    auto *desc =
        TF_NewOperation(graphFn.getGraph(), "QueueDequeueV2", opName.c_str());
    TF_AddInput(desc, {queueOp, 0});
    TF_SetDevice(desc, DEFAULT_CPU_DEVICE);
    TF_SetAttrTypeList(desc, "component_types", &outputType, 1);

    auto dequeueOp = graphFn.finishOp(desc, /*hasSideEffects*/ true,
                                      /*isEligibleForTPU*/ false, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return;
    addValueMapping({inst, 0}, {dequeueOp, 0});
  }

  // Now add enqueue to the top level graph function. Multiple graph functions
  // can have their own dequeue ops over the same tensorId.
  // One example is to dequeue tensors both within the while op's body
  // function, and also right after the while op is executed.
  // In that case, we only generate a single enqueue op at the top level.
  if (!processedTensorIdsForReceive.insert(tensorId).second) return;

  // The code here is different enough from the above that it's not worth
  // extracting common code into functions.
  TF_Operation *globalQueueOp;
  {
    auto opName = "fifo_queue_" + llvm::itostr(tensorId);
    auto *desc = TF_NewOperation(resultGraph, "FIFOQueueV2", opName.c_str());
    TF_SetDevice(desc, DEFAULT_CPU_DEVICE);
    TF_SetAttrInt(desc, "capacity", NAMED_TENSOR_QUEUE_CAPACITY);
    TF_SetAttrTypeList(desc, "component_types", &outputType, 1);
    // FIXME: Revisit whether to populate "shared_name".
    TF_SetAttrString(desc, "shared_name", opName.data(), opName.size());
    globalQueueOp = TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return;
  }

  TF_Operation *inputTensorPlaceholder;
  {
    auto opName = "arg_tensor_enqueue_" + llvm::itostr(tensorId);
    auto *desc = TF_NewOperation(resultGraph, "Placeholder", opName.c_str());
    TF_SetAttrType(desc, "dtype", outputType);

    inputTensorPlaceholder = TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return;
  }

  {
    auto opName = "fifo_queue_enqueue_" + llvm::itostr(tensorId);
    auto *desc = TF_NewOperation(resultGraph, "QueueEnqueueV2", opName.c_str());
    TF_AddInput(desc, {globalQueueOp, 0});
    TF_Output inputTensor{inputTensorPlaceholder, 0};
    TF_AddInputList(desc, &inputTensor, 1);
    TF_SetDevice(desc, DEFAULT_CPU_DEVICE);
    TF_SetAttrTypeList(desc, "Tcomponents", &outputType, 1);
    TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation())))
      return;
  }
}

void TFGraphLowering::addTFRecvOp(BuiltinInst *inst, int transferId,
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
  auto *recvOp = graphFn.finishOp(desc, /*hasSideEffects*/ true,
                                  /*isEligibleForTPU*/ false, status);
  if (checkStatus(getUserSourceLocation(inst->getDebugLocation()))) return;
  addValueMapping({inst, 0}, {recvOp, 0});
}

void TFGraphLowering::addTPUDequeueOp(BuiltinInst *inst, bool isInfeed,
                                      int transferId, ArrayRef<int64_t> dims,
                                      ArrayRef<int> numDims,
                                      ArrayRef<int64_t *> dimPtrs) {
  // Infeed dequeue runs on TPU, while outfeed dequeue runs on CPU.
  // OTherwise they have the same op signature.
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
    return;
  }
  if (isInfeed) {
    assert(thisDeviceType == DeviceType::TPU);
  } else {
    if (thisDeviceType != DeviceType::CPU) {
      internalError(getUserSourceLocation(inst->getDebugLocation()),
                    "TPU outfeed dequeue cannot run on this device",
                    diag::tfop_invalid_tfop);
      return;
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
  auto *dequeue = graphFn.finishOp(desc, /*hasSideEffects*/ true,
                                   isEligibleForTPU, status);
  if (checkStatus(getUserSourceLocation(inst->getDebugLocation()))) return;
  for (int i = 0, n = numDims.size(); i != n; ++i) {
    addValueMapping({inst, i}, {dequeue, i});
  }
}

void TFGraphLowering::visitBuiltinD2DTensorRecvInst(SILTensorOpInfo &tfopInfo) {
  // Signature: "__tfop_tfc.D2DTensorRecv,transferId,srcDevice,device"
  // Can also carry an optional shape array.
  auto *inst = tfopInfo.inst;
  assert(inst->getNumResults() == 1);
  assert(inst->getNumOperands() >= 3);
  assert(tfopInfo.getDeviceString() == thisDeviceTypeStr);

  int transferId = tfopInfo.getIntAttrOperand(0, "transferId");
  auto srcDeviceStr = tfopInfo.getStringAttrOperand(1, "srcDevice");
  auto srcDevice = OpDeviceType(srcDeviceStr);
  assert(thisDeviceType != srcDevice);

  SmallVector<int64_t, 8> dims;
  SmallVector<int, 3> numDims;
  SmallVector<int64_t*, 8> dimPtrs;
  if (inst->getNumOperands() > 3) {
    unsigned i = 3;  // 3 is the start operand idx for the shape attr.
    unsigned e = inst->getNumOperands();
    decodeShapeArray(tfopInfo, i, e, dims, numDims, dimPtrs);
    // We should be consuming all operands by now.
    assert(i+1 == e);
  }
  if (thisDeviceType == DeviceType::TPU) {
    addTPUDequeueOp(inst, /* isInfeed */ true, transferId, dims, numDims,
                    dimPtrs);
  } else if (srcDevice == DeviceType::TPU) {
    addTPUDequeueOp(inst, /* isInfeed */ false, transferId, dims, numDims,
                    dimPtrs);
  } else {
    addTFRecvOp(inst, transferId, srcDeviceStr);
  }
}

void TFGraphLowering::addTFSendOp(BuiltinInst *inst, int transferId,
                                  StringRef destDevice) {
  auto opName = "tf_send_" + llvm::itostr(transferId);
  auto &graphFn = getCurrentGraphFunction();
  auto *desc = TF_NewOperation(graphFn.getGraph(), "_Send", opName.c_str());
  TF_SetDevice(desc, thisDeviceTypeStr.c_str());

  auto inputToSendVal = inst->getOperand(0);
  auto inputToSendOp = getOperandValue(inputToSendVal);
  if (!inputToSendOp.oper) return;  // Error occurred.
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
  /* sendOp = */ graphFn.finishOp(desc, /*hasSideEffects*/ true,
                                  /*isEligibleForTPU*/ false, status);
  checkStatus(getUserSourceLocation(inst->getDebugLocation()));
}

void TFGraphLowering::addTPUEnqueueOp(BuiltinInst *inst, bool isInfeed,
                                      int transferId, ArrayRef<int64_t> dims,
                                      ArrayRef<int> numDims,
                                      ArrayRef<int64_t *> dimPtrs) {
  // Infeed enqueue runs on CPU, while outfeed enqueue runs on TPU.
  // Otherwise they have the same op signature (except for the "shapes" as
  // commented below).
  if (numDims.size() != 1) {
    if (isInfeed)
      internalError(
          getUserSourceLocation(inst->getDebugLocation()),
          "TPU infeed enqueue support enqueuing a single tensor -- did "
          "you specify shape?",
          diag::tfop_invalid_tfop);
    else
      internalError(
          getUserSourceLocation(inst->getDebugLocation()),
          "TPU outfeed enqueue support enqueuing a single tensor -- did "
          "you specify shape?",
          diag::tfop_invalid_tfop);
    return;
  }
  if (isInfeed) {
    if (thisDeviceType != DeviceType::CPU) {
      internalError(getUserSourceLocation(inst->getDebugLocation()),
                    "TPU infeed enqueue cannot run on this device",
                    diag::tfop_invalid_tfop);
      return;
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
  if (!inputToSendOp.oper) return;  // Error occurred.
  TF_AddInputList(desc, &inputToSendOp, 1);

  TF_DataType tfType =
      getTensorFlowDataType(inputToSendVal->getType(), inst->getLoc());
  assert(tfType > 0);
  TF_SetAttrTypeList(desc, "dtypes", &tfType, 1);
  if (isInfeed) {
    // Interestingly, OutfeedEnqueueTuple does not take a shapes attr, unlike
    // outfeed dequeue and infeed enqueue/dequeue tuple ops.
    TF_SetAttrShapeList(desc, "shapes", dimPtrs.data(), numDims.data(),
                        numDims.size());
  }
  // Infeed enqueue is not placed on TPU; outfeed enqueue is.
  bool isEligibleForTPU = !isInfeed;
  /*auto *enqueueOp = */ graphFn.finishOp(desc, /*hasSideEffects*/ true,
                                          isEligibleForTPU, status);
  if (checkStatus(getUserSourceLocation(inst->getDebugLocation()))) return;
}

void TFGraphLowering::visitBuiltinD2DTensorSendInst(SILTensorOpInfo &tfopInfo) {
  // Signature: "__tfop_tfc.D2DTensorSend,$in,transferId,destDevice,device"
  // Can also carry an optional shape array.
  auto *inst = tfopInfo.inst;
  assert(inst->getNumResults() == 1);
  assert(inst->getNumOperands() >= 4);
  assert(tfopInfo.getDeviceString() == thisDeviceTypeStr);

  assert(tfopInfo.isInput(0));
  int transferId = tfopInfo.getIntAttrOperand(1, "transferId");
  auto destDeviceStr = tfopInfo.getStringAttrOperand(2, "destDevice");
  auto destDevice = OpDeviceType(destDeviceStr);
  assert(thisDeviceType != destDevice);

  SmallVector<int64_t, 8> dims;
  SmallVector<int, 3> numDims;
  SmallVector<int64_t*, 8> dimPtrs;
  if (inst->getNumOperands() > 4) {
    unsigned i = 4;  // 4 is the start operand idx for the shape attr.
    unsigned e = inst->getNumOperands();
    decodeShapeArray(tfopInfo, i, e, dims, numDims, dimPtrs);
    // We should be consuming all operands by now.
    assert(i+1 == e);
  }
  if (thisDeviceType == DeviceType::TPU) {
    addTPUEnqueueOp(inst, /* isInfeed */ false, transferId, dims, numDims,
                    dimPtrs);
  } else if (destDevice == DeviceType::TPU) {
    addTPUEnqueueOp(inst, /* isInfeed */ true, transferId, dims, numDims,
                    dimPtrs);
  } else {
    addTFSendOp(inst, transferId, destDeviceStr);
  }
}

void TFGraphLowering::visitTFDataset(BuiltinInst *inst) {
  // FIXME: Also support dataset/iterator outside of TPU context.
  if (thisDeviceType != DeviceType::TPU || !configuration.isTPUInfeedEnabled) {
    internalError(
        getUserSourceLocation(inst->getDebugLocation()),
        "Builtin tfc.makeIteratorGetNextWithDatasets can only be used when "
        "generating TPU TF graphs with infeed support.",
        diag::tfop_invalid_tfop);
    return;
  }

  SILTensorOpInfo tfopInfo = SILTensorOpInfo::decode(inst).getValue();
  // Type check and process the first attribute: dataSource.
  DatasetCreationContext::DataSource dataSource;
  {
    auto operand = inst->getOperand(0);
    auto opInfo = tfopInfo.operandClasses[0];
    assert(opInfo.second == SILTensorOpInfo::OperandClass::Normal);
    auto *sli = cast<StringLiteralInst>(operand);
    assert(sli->getEncoding() == StringLiteralInst::Encoding::UTF8);
    if (sli->getValue().str() == "fake") {
      dataSource = DatasetCreationContext::FAKE;
    } else if (sli->getValue().str() == "mnist") {
      dataSource = DatasetCreationContext::MNIST;
    } else {
      dataSource = DatasetCreationContext::IMAGENET;
    }
  }

  // Type check and process the second attribute: filePath.
  // When dataSource is FAKE, this attribute needs to be present, but is not
  // used.
  StringRef filePath;
  if (dataSource != DatasetCreationContext::FAKE) {
    auto operand = inst->getOperand(1);
    auto *sli = cast<StringLiteralInst>(operand);
    assert(sli->getEncoding() == StringLiteralInst::Encoding::UTF8);
    filePath = sli->getValue();
  }

  // Type check and process the third attribute: batchSize
  int batchSize;
  {
    auto operand = inst->getOperand(2);
    auto opInfo = tfopInfo.operandClasses[2];
    assert(opInfo.second == SILTensorOpInfo::OperandClass::Normal);
    auto *ili = cast<IntegerLiteralInst>(operand);
    batchSize = ili->getValue().getLimitedValue();
  }

  // Type check and process the fourth attribute: outputShapes
  SmallVector<int64_t, 8> dims;
  SmallVector<int, 3> numDims;
  SmallVector<int64_t*, 8> dimPtrs;
  unsigned i = 3;
  unsigned e = inst->getNumOperands();
  decodeShapeArray(tfopInfo, i, e, dims, numDims, dimPtrs);

  // Even when this built-in returns multiple tensors, they are always presented
  // by a single tuple.
  assert(inst->getNumResults() == 1);

  std::vector<TF_DataType> outputTypes;
  auto outputType = inst->getType().getSwiftRValueType();
  if (auto tfType = getTFDataTypeFromTensorGenericType(outputType)) {
    outputTypes.push_back(static_cast<TF_DataType>(tfType));
  } else {
    // This must be a tuple type
    auto *tt = outputType->getAs<TupleType>();
    if (!tt) {
      internalError(getUserSourceLocation(inst->getDebugLocation()),
                    "The returned datatype of builtin "
                    "tfc.makeIteratorGetNextWithDatasets "
                    "should be a single element or a tuple of elements or type "
                    "Tensor<T> or TensorHandle<T>.",
                    diag::tfop_invalid_tfop);
      return;
    }
    for (unsigned i = 0, n = tt->getNumElements(); i != n; ++i) {
      auto tfType = getTFDataTypeFromTensorGenericType(tt->getElementType(i));
      assert(tfType && "Each output tuple element must be a TF type.");
      outputTypes.push_back(static_cast<TF_DataType>(tfType));
    }
  }
  if (outputTypes.size() != numDims.size()) {
    internalError(getUserSourceLocation(inst->getDebugLocation()),
                  "Must specify the same number of shapes and output tensors.",
                  diag::tfop_invalid_tfop);
    return;
  }

  // Defer the creation of the dataset / iterator related nodes, along with the
  // associated infeed enqueue till the creation of top level function
  // nodes. Here we fill in the dataset creation context, and then create an
  // infeed dequeue node to feed the user(s) of `inst`.
  datasetCreationContext.reset(new DatasetCreationContext(
      inst, dataSource, filePath, batchSize, dims, numDims, outputTypes));

  {
    auto &graphFn = getCurrentGraphFunction();
    auto *desc = TF_NewOperation(graphFn.getGraph(), "InfeedDequeueTuple",
                                 "InfeedDequeueTuple");
    markNodeAsTPUReplicated(desc);
    datasetCreationContext->setInfeedTypeAndShapeList(desc);
    auto *dequeue = TF_FinishOperation(desc, status);
    if (checkStatus(getUserSourceLocation(inst->getDebugLocation()))) return;

    for (int i = 0, n = outputTypes.size(); i != n; ++i) {
      addValueMapping({inst, i}, {dequeue, i});
    }
  }
}

/// Lower a builtin for a TFOp instruction into a TensorFlow op node.
///
void TFGraphLowering::visitTFOpInst(BuiltinInst *inst) {
  SILTensorOpInfo tfopInfo = SILTensorOpInfo::decode(inst).getValue();

  // Swift host <-> TF device sends/recvs.
  if (tfopInfo.opName == "tfc.RecvFromHost")
    return visitBuiltinRecvFromHostInst(tfopInfo);
  else if (tfopInfo.opName == "tfc.SendToHost")
    return visitBuiltinSendToHostInst(tfopInfo);

  // Device-to-device sends/recvs.
  if (tfopInfo.opName == "tfc.D2DTensorRecv")
    return visitBuiltinD2DTensorRecvInst(tfopInfo);
  else if (tfopInfo.opName == "tfc.D2DTensorSend")
    return visitBuiltinD2DTensorSendInst(tfopInfo);

  // Handle other TF ops.
  auto &graphFn = getCurrentGraphFunction();

  // The name label we put on the op is summarized from the "stack trace" of
  // the operation's source location.
  auto opLocString = getUniqueName(inst->getDebugLocation(), "op");

  auto *op = TF_NewOperation(graphFn.getGraph(), tfopInfo.opName.str().c_str(),
                             opLocString.c_str());

  // TODO: We compute the "hasSideEffects" bit solely based on whether or not
  // the op has Resource inputs.  This is a good starting point but is
  // insufficient.  It would be much nicer to have a TensorFlow C function that
  // returns the "SetIsStateful" bit from a TF_OperationDescription.
  bool hasSideEffects = false;
  bool hasDevice = false;

  for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
    auto operand = inst->getOperand(i);
    auto opInfo = tfopInfo.operandClasses[i];

    // Helper function to collect subsequent elements that contribute to an
    // array value.
    auto collectArrayElements =
                    [&](SILValue attrValue, Type &eltType,
                        SmallVectorImpl<SingleValueInstruction*> &elements) {
      // We have the metatype for the array element, which we need to know the
      // element type in case the array was empty.
      auto metatype = cast<MetatypeInst>(attrValue)->getType();
      eltType = metatype.getAs<AnyMetatypeType>()->getInstanceType();

      while (i+1 < e &&
             tfopInfo.operandClasses[i+1].second ==
                     SILTensorOpInfo::OperandClass::ArrayElement) {
        elements.push_back(cast<SingleValueInstruction>(inst->getOperand(++i)));
      }
    };

    // Convert the not-necessarily-nul-terminated StringRef to an std::string
    // so we can guarantee null termination for the "const char*" taking APIs.
    std::string name = opInfo.first.str();

    switch (opInfo.second) {
    case SILTensorOpInfo::OperandClass::Input: {
      // There are two things we support here: simple tensor inputs, and arrays
      // of tensors that get turned into InputList's.  Arrays are represented
      // with a metatype operand.
      if (operand->getType().is<MetatypeType>()) {
        // We don't actually care about the metatype, it is just a marker.
        // Collect all of the elements from the following InputElt operands if
        // any exist.
        SmallVector<TF_Output, 4> elements;

        while (i+1 < e && tfopInfo.operandClasses[i+1].second ==
                                   SILTensorOpInfo::OperandClass::InputElt) {
          auto eltValue = inst->getOperand(++i);
          auto valueKind = classifyTensorFlowValue(eltValue->getType());

          // Keep track of whether we have any resource inputs.
          hasSideEffects |= valueKind == TFValueKind::ResourceHandle;
          assert(valueKind != TFValueKind::Nope &&
                 "all op inputs should be TensorFlow values");
          auto opValue = getOperandValue(eltValue);
          if (!opValue.oper) return;  // Error occurred.
          elements.push_back(opValue);
        }
        TF_AddInputList(op, elements.data(), elements.size());
        break;
      }

      auto valueKind = classifyTensorFlowValue(operand->getType());

      // Keep track of whether we have any resource inputs.
      hasSideEffects |= valueKind == TFValueKind::ResourceHandle;
      assert(valueKind != TFValueKind::Nope &&
             "all op inputs should be TensorFlow values");
      auto opValue = getOperandValue(operand);
      if (!opValue.oper) return;  // Error occurred.
      TF_AddInput(op, opValue);
      break;
    }
    case SILTensorOpInfo::OperandClass::InputElt:
      assert(0 && "Handled by the Input that precedes it.");
    case SILTensorOpInfo::OperandClass::Normal:  // No modifier.
      // We add attributes based on what the type of the value is.
      if (auto *ili = dyn_cast<IntegerLiteralInst>(operand)) {
        uint64_t value = ili->getValue().getLimitedValue();
        if (ili->getValue().getBitWidth() == 1)
          TF_SetAttrBool(op, name.c_str(), (unsigned char)value);
        else
          TF_SetAttrInt(op, name.c_str(), (int64_t)value);
      } else if (auto *fli = dyn_cast<FloatLiteralInst>(operand)) {
        auto value = fli->getValue();
        // TensorFlow only supports 32-bit float attributes.  If we got a 16 or
        // 64 bit one, convert it to float.
        bool losesInfo = false;
        value.convert(APFloat::IEEEsingle(), APFloat::rmNearestTiesToEven,
                      &losesInfo);
        TF_SetAttrFloat(op, name.c_str(), value.convertToFloat());
      } else if (auto *sli = dyn_cast<StringLiteralInst>(operand)) {
        assert(sli->getEncoding() == StringLiteralInst::Encoding::UTF8 &&
               "only byte encodings are supported");
        auto value = sli->getValue();
        if (name != DEVICE_ATTR) {
          TF_SetAttrString(op, name.c_str(), value.data(), value.size());
        } else {
          if (value.str() == ALL_DEVICES) {
            value = thisDeviceTypeStr;
          }
          if (value.str() != DEFAULT_TPU_DEVICE) {
            TF_SetDevice(op, value.str().c_str());
          } else {
            // TPU device placement is not done via TF_SetDevice().
            markNodeAsTPUReplicated(op);
          }
          hasDevice = true;
        }
      } else if (auto *mti = dyn_cast<MetatypeInst>(operand)) {
        auto ty = mti->getType().castTo<AnyMetatypeType>()->getInstanceType();
        auto dtype = convertSwiftTypeToTF(ty);
        assert(dtype && "expected a valid tensorflow type");
        TF_SetAttrType(op, name.c_str(), (TF_DataType)dtype);
      } else {
        llvm_unreachable("unexpected attribute instruction");
      }
      break;
    case SILTensorOpInfo::OperandClass::DType: {
      // This integer value is a dtype.
      auto val = cast<IntegerLiteralInst>(operand)->getValue();
      TF_SetAttrType(op, name.c_str(), (TF_DataType)val.getLimitedValue());
      break;
    }
    case SILTensorOpInfo::OperandClass::Tensor: {
      // Tensor can support two cases: an array case, where 'attrValue' is a
      // metatype, and a scalar case.
      TF_DataType dtype;
      SmallVector<SingleValueInstruction*, 4> elements;
      SmallVector<int64_t, 4> shape;
      if (!isa<MetatypeInst>(operand)) {
        // The scalar case is very simple, the shape of a scalar is 0d.
        dtype = getTensorFlowDataType(operand->getType(), inst->getLoc());
        elements.push_back(cast<LiteralInst>(operand));
      } else {
        // Handle the array case by decoding the array itself, then decoding the
        // shape value that follows it.
        Type eltType;
        collectArrayElements(operand, eltType, elements);

        auto shapeAttr = tfopInfo.operandClasses[++i]; (void)shapeAttr;
        assert(shapeAttr.second == SILTensorOpInfo::OperandClass::Shape);
        auto shapeAttrValue = tfopInfo.getAttrOperand(i);
        decodeShapeElements(shapeAttrValue, tfopInfo, i, e, shape);
        auto eltSILType =
          SILType::getPrimitiveObjectType(eltType->getCanonicalType());
        dtype = getTensorFlowDataType(eltSILType, inst->getLoc());
      }

      // Set the tensor as the attribute on the graph node.
      auto tensor = convertValuesToTensor(elements, shape, dtype);
      TF_SetAttrTensor(op, name.c_str(), tensor, status);
      TF_DeleteTensor(tensor);
      if (checkStatus(inst->getLoc())) return;
      break;
    }
    case SILTensorOpInfo::OperandClass::Shape: {
      SmallVector<int64_t, 4> shape;
      decodeShapeElements(operand, tfopInfo, i, e, shape);
      TF_SetAttrShape(op, name.c_str(), shape.data(), shape.size());
      break;
    }
    case SILTensorOpInfo::OperandClass::ShapeArray: {
      SmallVector<int64_t, 8> dims;
      SmallVector<int, 3> numDims;
      SmallVector<int64_t*, 8> dimPtrs;
      decodeShapeArray(tfopInfo, i, e, dims, numDims, dimPtrs);
      if (name != SHAPE_ARRAY_ATTR) {
        // SHAPE_ARRAY_ATTR is a pseudo-attribute used by the compiler's
        // partitioning and graph lowering passes to propagate shape info for
        // XLA compilation (e.g. feed shape info to infeed / outfeed ops), and
        // will not be lowered into this graph op itself.
        TF_SetAttrShapeList(op, name.c_str(), dimPtrs.data(), numDims.data(),
                            numDims.size());
      }
      break;
    }
    case SILTensorOpInfo::OperandClass::Array: {
      // Get all of the elements that contribute to this array value.
      Type eltType;
      SmallVector<SingleValueInstruction*, 4> elements;
      collectArrayElements(operand, eltType, elements);

      if (eltType->is<MetatypeType>()) {
        SmallVector<TF_DataType, 4> types;
        for (auto elt : elements) {
          auto *mti = cast<MetatypeInst>(elt);
          auto ty = mti->getType().castTo<AnyMetatypeType>()->getInstanceType();
          auto dtype = convertSwiftTypeToTF(ty);
          assert(dtype && "expected a valid tensorflow type");
          types.push_back((TF_DataType)dtype);
        }

        TF_SetAttrTypeList(op, name.c_str(), types.data(), types.size());
        break;
      }

      if (eltType->getString() == "String") {
        SmallVector<const void*, 4> pointers;
        SmallVector<size_t, 4> sizes;
        for (auto elt : elements) {
          auto *sli = cast<StringLiteralInst>(elt);
          assert(sli->getEncoding() == StringLiteralInst::Encoding::UTF8 &&
                 "only byte encodings are supported");
          pointers.push_back(sli->getValue().data());
          sizes.push_back(sli->getValue().size());
        }
        TF_SetAttrStringList(op, name.c_str(), pointers.data(), sizes.data(),
                             elements.size());
        break;
      }

      auto typeName = eltType->getString();
      if (StringRef(typeName).startswith("Int")) {
        SmallVector<int64_t, 4> values;
        for (auto elt : elements) {
          auto *ili = cast<IntegerLiteralInst>(elt);
          values.push_back(ili->getValue().getLimitedValue());
        }
        TF_SetAttrIntList(op, name.c_str(), values.data(), values.size());
        break;
      }
      if (typeName == "Float" || typeName == "Double") {
        SmallVector<float, 4> values;
        for (auto elt : elements) {
          auto value = cast<FloatLiteralInst>(elt)->getValue();
          bool losesInfo = false;
          value.convert(APFloat::IEEEsingle(), APFloat::rmNearestTiesToEven,
                        &losesInfo);
          values.push_back(value.convertToFloat());
        }
        TF_SetAttrFloatList(op, name.c_str(), values.data(), values.size());
        break;
      }
      if (typeName == "Bool") {
        SmallVector<unsigned char, 4> values;
        for (auto elt : elements) {
          auto *ili = cast<IntegerLiteralInst>(elt);
          values.push_back(ili->getValue().getLimitedValue() != 0);
        }
        TF_SetAttrBoolList(op, name.c_str(), values.data(), values.size());
        break;
      }

      llvm_unreachable(("unknown attribute array type: " + typeName).c_str());
    }
    case SILTensorOpInfo::OperandClass::ArrayElement:
      llvm_unreachable("Array elements are handled by the array that "
                       "precedes them");
    }
  }

  if (!hasDevice) {
    inst->dump();
    llvm_unreachable("The above tensor op has no device set");
  }

  auto *result =
      graphFn.finishOp(op, hasSideEffects, /*isEligibleForTPU*/ true, status);

  // If the node builder failed, then something is wrong.  Handle a few special
  // cases to improve the diagnostics.
  if (TF_GetCode(status) != TF_OK) {
    auto loc = getUserSourceLocation(inst->getDebugLocation());

    StringRef message = TF_Message(status);
    if (message.startswith("Op type not registered")) {
      internalError(loc, tfopInfo.opName.str(), diag::tf_lowering_unknown_op);
      return;
    }

    // Otherwise, emit a generic error message.
    internalError(loc, message);
    return;
  }

  // Check to make sure that the operation produces the number of results we
  // expect, and wire them into the valueMapping result table.
  unsigned numOpResults = TF_OperationNumOutputs(result);

  unsigned numActualResults = 0;
  if (auto tuple = inst->getType().getAs<TupleType>()) {
    numActualResults = tuple->getNumElements();
  } else if (!inst->getType().isVoid()) {
    numActualResults = 1;
  }

  if (numOpResults != numActualResults) {
    diagnose(SILFn, inst->getLoc(),
             diag::tfop_incorrect_definition,
             "TensorFlow op '" + tfopInfo.opName.str() + "' produces " +
             llvm::utostr(numOpResults) + " result, but Swift function "
             "produces " + llvm::utostr(numActualResults));
    errorOccurred = true;
    return;
  }

  // Remember each of the results.
  for (int i = 0; i != (int)numActualResults; ++i) {
    addValueMapping({inst, i}, {result, i});
  }
}

void TFGraphLowering::visitTupleInst(TupleInst *inst) {
  // Tuples never exist in the graph except when they are the argument to
  // the return instruction.
  assert(inst->hasOneUse() && isa<ReturnInst>(inst->getSingleUse()->getUser())&&
         "Unexpected tuple_inst in GraphGen");
}

void TFGraphLowering::visitTupleExtractInst(TupleExtractInst *inst) {
  // tuple_extracts only exist as part of the handling for multi-result
  // tensor operations.  This is handled as part of the 'getOperandValue'
  // implementation.
}

void TFGraphLowering::
visitUncheckedRefCastInst(UncheckedRefCastInst *inst) {
  // UncheckedBitwiseCast's get generated between two identical TensorHandle's
  // when one is using a Swift type like Int32 and one is using Builtin.Int32.
  // None of this matters for graph lowering.
  auto opValue = getOperandValue(inst->getOperand());
  if (!opValue.oper) return;  // Error occurred.

  addValueMapping({inst, 0}, opValue);
}


void TFGraphLowering::visitReturnInst(ReturnInst *inst) {
  auto &graphFn = getCurrentGraphFunction();
  assert(graphFn.outputs.empty() &&
         "Should only have one return per graph function");

  // The return is either using a single value or a tuple of values (which
  // could be empty).  These become the results of the graph.
  if (auto *ti = dyn_cast<TupleInst>(inst->getOperand())) {
    for (auto &operand : ti->getAllOperands()) {
      auto result = getOperandValue(operand.get());
      if (!result.oper) return; // Error occurred.
      result = graphFn.maybeRunEffectfulOp(result, status);
      if (checkStatus(SILFn.getLocation())) return;
      graphFn.outputs.push_back({ /*SILArgument*/nullptr, result });
    }
  } else {
    auto result = getOperandValue(inst->getOperand());
    if (!result.oper) return; // Error occurred.
    result = graphFn.maybeRunEffectfulOp(result, status);
    if (checkStatus(SILFn.getLocation())) return;
    graphFn.outputs.push_back({ /*SILArgument*/nullptr, result });
  }
}

void TFGraphLowering::visitBranchInst(BranchInst *inst) {
  if (inst->getNumArgs() == 0) return;

  auto &graphFn = getCurrentGraphFunction();
  assert(graphFn.outputs.empty() &&
         "Should only have one exit branch per graph function");

  auto destBB = inst->getDestBB();

  // Walk the BB arguments of the branch - each one is an output of the
  // enclosing graph function that we're building.  Match up the output values
  // with the BB argument being computed.
  for (unsigned i = 0, e = inst->getNumArgs(); i != e; ++i) {
    auto result = getOperandValue(inst->getArg(i));
    if (!result.oper) return; // Error occurred.
    result = graphFn.maybeRunEffectfulOp(result, status);
    if (checkStatus(SILFn.getLocation())) return;
    graphFn.outputs.push_back({ destBB->getArgument(i), result });
  }
}


/// Lower all of the instructions in the specified basic block.  If
/// skipTerminator is set to true, then the terminator instruction isn't
/// lowered.
void TFGraphLowering::lowerBasicBlock(SILBasicBlock *bb, bool skipTerminator) {
  // Visit all of the instructions other than the terminator.
  auto I = bb->begin(), E = bb->end();

  // Ignore the terminator instruction if requested.
  if (skipTerminator)
    E = std::prev(E);

  for (; I != E; ++I) {
    visit(&*I);

    // If we produced an error lowering an instruction, give up hope and return.
    if (errorOccurred)
      return;
  }
}

void TFGraphLowering::lowerSequenceRegion(SequenceSESERegion *r) {
  for (auto &child : r->getNodes())
    lowerRegion(child.get());
}


/// Given a conditional branch, produce the TF_Output for its branch condition.
static TF_Output getCondition(CondBranchInst *condBr,
                              TFGraphLowering &lowering) {
  auto cond = condBr->getCondition();
  auto tensorToI1 = cast<BuiltinInst>(cond);
  assert(tensorToI1->getName().str() == "tf_tensor_to_i1" &&
         tensorToI1->getNumOperands() == 1 &&
         "unexpected branch condition in graph lowering");
  cond = tensorToI1->getOperand(0);

  // Get the graph node that corresponds to the condition.
  return lowering.getOperandValue(cond);
}

// Given a boolean value, create a 'not' operation to invert it, returning the
// inverted result.
static TF_Output createNotOp(TF_Output input, SILDebugLocation loc,
                             TFGraphLowering &lowering) {
  auto opLocString = lowering.getUniqueName(loc, "not");
  auto &graphFn = lowering.getCurrentGraphFunction();
  auto *op = TF_NewOperation(graphFn.getGraph(), "LogicalNot",
                             opLocString.c_str());
  TF_AddInput(op, input);

  // For this synthesized op, let TF figure out the best device placement.
  // FIXME: Revisit this.
  auto *result = graphFn.finishOp(op, /*side effects*/ false,
                                  /*isEligibleForTPU*/ true, lowering.status);
  if (lowering.checkStatus(loc.getLocation()))
    return { nullptr, 0 };
  return { result, 0 };
}

/// Given a boolean value, create a 'cast' operation to convert it to int32.
static TF_Output castBoolToInt32(TF_Output input, SILDebugLocation loc,
                                 TFGraphLowering &lowering) {
  auto opLocString = lowering.getUniqueName(loc, "cast");
  auto &graphFn = lowering.getCurrentGraphFunction();
  auto *op = TF_NewOperation(graphFn.getGraph(), "Cast",
                             opLocString.c_str());
  TF_AddInput(op, input);
  TF_SetAttrType(op, "SrcT", TF_BOOL);
  TF_SetAttrType(op, "DstT", TF_INT32);

  // For this synthesized op, let TF figure out the best device placement.
  // FIXME: Revisit this.
  auto *result = graphFn.finishOp(op, /*side effects*/ false,
                                  /*isEligibleForTPU*/ true, lowering.status);
  if (lowering.checkStatus(loc.getLocation()))
    return { nullptr, 0 };
  return { result, 0 };
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
void TFGraphLowering::lowerWhileLoopRegion(WhileLoopSESERegion *r) {
  // Emit the preheader block.  The preheader ends with a branch that sets BB
  // arguments, which we will handle specially later.  They provide the passed
  // values to the loop function that we will create.
  lowerBasicBlock(r->getPreheader(), /*skipTerminator:*/ true);
  if (errorOccurred) return;

  auto phBranch = cast<BranchInst>(r->getPreheader()->getTerminator());

  // Get all the values that the preheader passes in for the SILArguments in
  // the loop body.
  SmallVector<TF_Output, 4> preheaderInputs;
  for (auto argValue : phBranch->getArgs()) {
    auto result = getOperandValue(argValue);
    if (!result.oper) return; // Error occurred.
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
    lowerArgumentsToParams(headerBB->getArguments(), preheaderInputs, brLoc);
    if (errorOccurred) return;

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
    lowerBasicBlock(headerBB, /*skipTerminator:*/ true);
    if (errorOccurred) return;

    // Lower all the code in the body of the loop.  This region ends with a
    // branch back to the loop header that passes arguments, and these arguments
    // will be installed as "exit values" on the loop by the normal BranchInst
    // lowering code.
    lowerRegion(r->getBody());
  });
  if (errorOccurred) return;

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
    if (checkStatus(SILFn.getLocation())) return;
    loopBodyFn.outputs.push_back({/*SILArgument*/ nullptr, result});
  }

  // Next, lower the condition function into a 'stop predicate' for the loop.
  auto condFn = lowerToFunction([&]() {
    // The condition function takes the same set of inputs as the loop body.
    auto brLoc = r->getPreheader()->getTerminator()->getLoc();
    lowerArgumentsToParams(headerBB->getArguments(), preheaderInputs, brLoc);
    if (errorOccurred) return;

    // Copy the live-in set over to the condition by requesting the values be
    // live.  This ensures that the condition and body functions agree on their
    // inputs.
    auto &graphFn = getCurrentGraphFunction();
    for (unsigned i = graphFn.inputs.size(), e = loopBodyFn.inputs.size();
         i != e; ++i) {
      (void)getOperandValue(loopBodyFn.inputs[i].value);
    }

    // Lower any code in the header block, which may be used by the termination
    // condition.  It ends with a conditional branch which we handle manually.
    graphFn.shouldLowerEffectfulOps = false;
    lowerBasicBlock(r->getHeader(), /*skipTerminator:*/ true);
    if (errorOccurred) return;

    // Lower the condition, which always produces a boolean value.
    auto condValue = getCondition(headerBr, *this);
    if (!condValue.oper) return;  // Error occurred.

    // If the condition is true when the loop should continue, invert the
    // condition.
    // TODO: add a unit test to cover this case.
    if (headerBr->getTrueBB() == r->getExit()) {
      condValue = createNotOp(condValue, headerBr->getDebugLocation(), *this);
      if (!condValue.oper) return;   // Error occurred.
    }

    // For non TPU/XLA case, cast the boolean value to int32, a workaround as
    // needed to get while loop to run on GPU (b/65752372).
    if (thisDeviceType != DeviceType::TPU) {
      // FIXME: this added cast may not work for XlaWhile. Revisit whether/how
      // to support loops in XLA GPU.
      condValue =
          castBoolToInt32(condValue, headerBr->getDebugLocation(), *this);
      if (!condValue.oper) return;  // Error occurred.
    }

    // The result of the function is our condition value.
    graphFn.outputs.push_back({ /*SILArgument*/nullptr, condValue });
  });
  if (errorOccurred) return;


  // We are going to need the input values and types for the op creation: build
  // these lists now.
  SmallVector<TF_Output, 4> inputs;
  for (auto &input : loopBodyFn.inputs) {
    inputs.push_back(input.passedValue);
  }

  bool hasSideEffects = false;

  // Create TF_Function's for our condition and body.
  auto loc = headerBr->getDebugLocation();
  auto loopBodyFnName = getUniqueName(loc, "whilebody");
  SmallVector<TF_DataType, 4> inputTypes, outputTypes;
  if (buildGraphFunction(loopBodyFn, loopBodyFnName, hasSideEffects,
                         &inputTypes, &outputTypes))
    return;
  auto condFnName = getUniqueName(loc, "whilecond");
  if (buildGraphFunction(condFn, condFnName, hasSideEffects,
                         /*inputTypes*/ nullptr, /*outputTypes*/ nullptr))
    return;

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
  auto *op = TF_NewOperation(graphFn.getGraph(), "While", opLocString.c_str());
  TF_AddInputList(op, inputs.data(), inputs.size());
  TF_SetAttrTypeList(op, "T", inputTypes.data(), inputTypes.size());
  TF_SetAttrFuncName(op, "cond", condFnName.c_str(), condFnName.size());
  TF_SetAttrFuncName(op, "body",  loopBodyFnName.c_str(),
                     loopBodyFnName.size());

  auto *result =
      graphFn.finishOp(op, hasSideEffects, /*isEligibleForTPU*/ true, status);
  if (checkStatus(getUserSourceLocation(loc)))
    return;

  // The live-out value from the while loop was the state of the SILArgument's
  // at the time that the termination program stopped.  Those SILArgument values
  // dominate the exit branch, so they may be used by code after the while.
  // Install them in our name lookup table.
  for (unsigned i = 0, e = headerBB->getArguments().size(); i != e; ++i) {
    addValueMapping(SILOpResult(headerBB->getArgument(i), 0),
                    { result, (int)i });
  }

  // In addition to the SIL arguments themselves, all of the code in the header
  // block dominates the exit as well and may well be used by code outside the
  // loop.  We've already emit it into the condition function and the while loop
  // body, so emit it one more time outside the loop for good measure.  We
  // should be able to remove this when/if we get a proper model for loops as
  // described above.
  lowerBasicBlock(r->getHeader(), /*skipTerminator:*/ true);
}

void TFGraphLowering::lowerConditionalRegion(ConditionalSESERegion *r) {
  // Start by lowering any code that exists in the block that leads up to the
  // conditional branch.  This ensures that the condition bool is available.
  lowerBasicBlock(r->getBranchBB(), /*skipTerminator:*/ true);
  if (errorOccurred)
    return;

  // The branch block should end with a conditional branch on a tf_tensor_to_i1
  // invocation.
  auto condBr = cast<CondBranchInst>(r->getBranchBB()->getTerminator());
  auto loc = condBr->getDebugLocation();

  auto condValue = getCondition(condBr, *this);
  if (!condValue.oper) return;  // Error occurred.

  // Lower the true and false bodies to graph functions.
  auto trueCodeFn = lowerToFunction([&]() {
    // Lower all of the code inside the region (which can of course recursively
    // create functions and call them as ops.
    if (auto trueRegion = r->getTrue())
      lowerRegion(trueRegion);
  });
  if (errorOccurred) return;

  auto falseCodeFn = lowerToFunction([&]() {
    // Lower all of the code inside the region (which can of course recursively
    // create functions and call them as ops.
    if (auto falseRegion = r->getFalse())
      lowerRegion(falseRegion);
  });
  if (errorOccurred) return;

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
    bool inserted = trueInputs.insert(input.value).second; (void)inserted;
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
    entry = i+1;  // Entry in the map is 1-biased.

    // Check to see if the true function already has this passed value.
    if (!trueInputs.insert(input.value).second)
      continue;  // Ignore common entries.

    // If not, add the parameter to the true list.
    createParameter(input.value, input.passedValue, trueCodeFn);
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
      falseCodeFn.inputs.push_back(falseInputList[entry-1]);
    } else {
      // Otherwise, we need to create a new parameter and add it.  Fortunately
      // this automatically adds it to the false function's input list for us.
      createParameter(input.value, input.passedValue, falseCodeFn);
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
    for (unsigned j = i+1; ; ++j) {
      assert(j != e && "true/false code don't common argument list");
      if (arg == falseCodeFn.outputs[j].first) {
        std::swap(falseCodeFn.outputs[i], falseCodeFn.outputs[j]);
        break;
      }
    }
  }

  bool hasSideEffects = false;

  // Create the graph functions for the true/false code.
  auto trueFnName = getUniqueName(loc, "true");
  SmallVector<TF_DataType, 4> inputTypes, outputTypes;
  if (buildGraphFunction(trueCodeFn, trueFnName, hasSideEffects, &inputTypes,
                         &outputTypes))
    return;
  auto falseFnName = getUniqueName(loc, "false");
  if (buildGraphFunction(falseCodeFn, falseFnName, hasSideEffects,
                         /*inputTypes*/ nullptr, /*outputTypes*/ nullptr))
    return;

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
  auto *op = TF_NewOperation(graphFn.getGraph(), "If", opLocString.c_str());
  TF_AddInput(op, condValue);
  TF_AddInputList(op, inputs.data(), inputs.size());
  TF_SetAttrTypeList(op, "Tin", inputTypes.data(), inputTypes.size());
  TF_SetAttrTypeList(op, "Tout", outputTypes.data(), outputTypes.size());
  TF_SetAttrFuncName(op, "then_branch", trueFnName.c_str(), trueFnName.size());
  TF_SetAttrFuncName(op, "else_branch",  falseFnName.c_str(),
                     falseFnName.size());

  auto *result =
      graphFn.finishOp(op, hasSideEffects, /*isEligibleForTPU*/ true, status);
  if (checkStatus(getUserSourceLocation(loc)))
    return;

  // Remember each of the results so that any references to the SIL BBArguments
  // that got defined end up referring to this node.
  for (int i = 0, e = trueCodeFn.outputs.size(); i != e; ++i)
    addValueMapping({trueCodeFn.outputs[i].first, 0}, {result, i});
}

void TFGraphLowering::lowerRegion(SESERegionTree *region) {
  if (errorOccurred)
    return;

  switch (region->getKind()) {
  case SESERegionTree::SingleBlock:
    return lowerBasicBlock(cast<SingleBlockSESERegion>(region)->getBB());
  case SESERegionTree::Sequence:
    return lowerSequenceRegion(cast<SequenceSESERegion>(region));
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
TF_Output TFGraphLowering::
createParameter(SILOpResult value, TF_Output passedValue,
                GraphFunctionBody &fn) {
  auto opName = "arg_" + llvm::utostr(OpID++);
  auto *desc = TF_NewOperation(fn.getGraph(), "Placeholder", opName.c_str());
  auto loc = value.first.getLoc();
  auto type = getTensorFlowDataType(getOpResultType(value), loc);
  if (!type) {
    internalError(loc, "use of unknown dtype!");
    return { nullptr, 0 };
  }
  TF_SetAttrType(desc, "dtype", type);

  // Placeholder nodes are never placed on TPU.
  auto result = fn.finishOp(desc, /*side effects*/ false,
                            /*isEligibleForTPU*/ false, status);
  if (checkStatus(loc))
    return { nullptr, 0 };

#ifndef NDEBUG
  // Verify we haven't seen this value yet.
  if (value.first) {
    for (auto i : fn.inputs)
      assert(i.value != value && "adding redundant value");
  }
#endif

  // Success!  Remember this parameter, and the value that is passed in.
  fn.inputs.push_back({{ result, 0 }, passedValue, value });
  return { result, 0 };
}

/// Lower the specified list of SIL arguments to a bunch of parameters, filling
/// the inputs list for the current function.  If the passedValues array is
/// non-empty, it specifies the passed values to add to the input.
void TFGraphLowering::lowerArgumentsToParams(ArrayRef<SILArgument *> args,
                                             ArrayRef<TF_Output> passedValues,
                                             SILLocation loc) {
  auto &graphFn = getCurrentGraphFunction();
  unsigned idx = 0;
  for (auto arg : args) {
    auto passedValue = TF_Output();
    if (!passedValues.empty())
      passedValue = passedValues[idx++];

    auto result = createParameter({arg, 0}, passedValue, graphFn);
    if (result.oper == nullptr)
      return;

    addValueMapping({arg, 0}, result);
  }
}


/// Build a function around the code produced by the specified std::function.
GraphFunctionBody TFGraphLowering::
lowerToFunction(const std::function<void()> &body) {
  // Push a scope, allowing us to keep track of any live-in values in the
  // true code.  These will need to become tuple elements live across the
  // loop.
  ValueMappingScopedHashTable::ScopeTy scope(valueMapping);

  /// Start a new graph function.
  functionStack.push_back(GraphFunctionBody(thisDeviceType, configuration));

  // Lower the code in the body however the caller wants to do it.
  body();

  auto result = std::move(functionStack.back());
  functionStack.pop_back();
  return result;
}

bool TFGraphLowering::addTopLevelTPUConfigLogic(TF_Operation **metadataNode) {
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
    TF_SetDevice(desc, DEFAULT_TPU_DEVICE);
    TF_SetAttrBool(desc, "is_global_init", true);
    TF_FinishOperation(desc, status);
    if (checkStatus(SILFn.getLocation()))
      return true;
  }

  {
    auto *desc = TF_NewOperation(resultGraph, "TPUCompilationResult",
                                 "TPUCompilationResult");
    TF_SetDevice(desc, DEFAULT_CPU_DEVICE);
    TF_SetAttrString(desc, "_tpu_compilation_status", TPU_CLUSTER_ATTR_VALUE,
                     strlen(TPU_CLUSTER_ATTR_VALUE));
    TF_AddControlInput(desc, *metadataNode);
    TF_FinishOperation(desc, status);
    if (checkStatus(SILFn.getLocation()))
      return true;
  }

  return false;
}

bool TFGraphLowering::buildGraphNodesForTopLevelFunctionCall(
    StringRef funcOpType, StringRef funcNodeBaseName, bool isPrimaryFn,
    ArrayRef<TF_DataType> inputTypes, ArrayRef<TF_DataType> outputTypes) {
  TF_Operation *metadataNode = nullptr;
  if (isPrimaryFn && thisDeviceType == DeviceType::TPU) {
    if (addTopLevelTPUConfigLogic(&metadataNode)) {
      // An error has occurred. Abort graph generation.
      return true;
    }
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
  std::string funcNodeName = "tfc_func_" + funcNodeBaseName.str();
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
      "tfc_input_" + std::to_string(i) + "_" + funcNodeBaseName.str();
    TF_OperationDescription *inputDesc =
        TF_NewOperation(resultGraph, "Placeholder", inputNodeName.c_str());
    TF_SetAttrType(inputDesc, "dtype", inputTypes[i]);
    TF_Operation *placeholder = TF_FinishOperation(inputDesc, status);
    if (checkStatus(SILFn.getLocation())) return true;
    TF_Output inputNode{placeholder, 0};
    if (thisDeviceType != DeviceType::TPU) {
      // Feed I directly into F.
      TF_AddInput(funcDesc, inputNode);
    } else {
      // Add some intermediate nodes between I and F.
      TF_Operation *replicatedInput;
      {
        std::string nodeName = "TPUReplicate/input" + std::to_string(i);
        auto *desc = TF_NewOperation(resultGraph, "TPUReplicatedInput",
                                     nodeName.c_str());
        SmallVector<TF_Output, 1> input;
        input.push_back(inputNode);
        // This node requires an input list.
        TF_AddInputList(desc, input.data(), input.size());
        replicatedInput = TF_FinishOperation(desc, status);
        if (checkStatus(SILFn.getLocation())) return true;
      }
      {
        std::string nodeName =
          "TPUReplicate/replicated_input_" + std::to_string(i);
        auto *desc = TF_NewOperation(resultGraph, "Identity", nodeName.c_str());
        TF_AddControlInput(desc, metadataNode);
        TF_AddInput(desc, {replicatedInput, 0});
        markNodeAsTPUReplicated(desc);
        TF_Operation *idInput = TF_FinishOperation(desc, status);
        if (checkStatus(SILFn.getLocation())) return true;
        TF_AddInput(funcDesc, {idInput, 0});
      }
    }
  }

  // Finish constructing the function node.
  TF_Operation *funcNode = TF_FinishOperation(funcDesc, status);
  if (checkStatus(SILFn.getLocation())) return true;

  // Now handle outputs.
  for (unsigned i = 0, e = outputTypes.size(); i != e; ++i) {
    std::string outputNodeName =
        "tfc_output_" + std::to_string(i) + "_" + funcNodeBaseName.str();
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
            "TPUReplicate/Identity_" + std::to_string(i);
        auto *desc = TF_NewOperation(resultGraph, "Identity", nodeName.c_str());
        TF_AddInput(desc, funcOutputNode);
        const auto deviceName =
            std::string("/device:") + DEVICE_TPU_REPLICATED_CORE;
        TF_SetDevice(desc, deviceName.c_str());
        markNodeAsTPUReplicated(desc);
        outputIdNode = TF_FinishOperation(desc, status);
        if (checkStatus(SILFn.getLocation())) return true;
      }
      {
        const std::string nodeName = "TPUReplicate/output" + std::to_string(i);
        auto *desc = TF_NewOperation(resultGraph, "TPUReplicatedOutput",
                                     nodeName.c_str());
        TF_AddInput(desc, {outputIdNode, 0});
        TF_SetAttrInt(desc, "num_replicas", 1);
        TF_Operation *replicatedOutputNode = TF_FinishOperation(desc, status);
        if (checkStatus(SILFn.getLocation())) return true;
        TF_AddInput(outputDesc, {replicatedOutputNode, 0});
      }
    }
    /*TF_Operation *outputNode =*/ TF_FinishOperation(outputDesc, status);
    if (checkStatus(SILFn.getLocation())) return true;
  }

  if (datasetCreationContext) {
    if (createDatasetIteratorNodesWithInfeedEnqueue()) return true;
  }

  // Everything is good!
  return false;
}

bool TFGraphLowering::buildGraphFunction(
    const GraphFunctionBody &graphBody, StringRef funcName,
    bool &hasSideEffects, SmallVectorImpl<TF_DataType> *inputTypes,
    SmallVectorImpl<TF_DataType> *outputTypes) {
  if (errorOccurred)
    return true;

  // Inform our callers whether this function contains side effects or not.
  hasSideEffects = graphBody.controlDependenceValue != nullptr;

  SmallVector<TF_Output, 4> ins, outs;
  ins.reserve(graphBody.inputs.size());
  if (inputTypes) {
    inputTypes->clear();
    inputTypes->reserve(graphBody.inputs.size());
  }
  for (unsigned i = 0, e = graphBody.inputs.size(); i != e; ++i) {
    ins.push_back(graphBody.inputs[i].parameter);
    if (inputTypes) inputTypes->push_back(TF_OperationOutputType(ins[i]));
  }
  outs.reserve(graphBody.outputs.size());
  if (outputTypes) {
    outputTypes->clear();
    outputTypes->reserve(graphBody.outputs.size());
  }
  for (unsigned i = 0, e = graphBody.outputs.size(); i != e; ++i) {
    outs.push_back(graphBody.outputs[i].second);
    if (outputTypes) outputTypes->push_back(TF_OperationOutputType(outs[i]));
  }

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
  TF_GraphCopyFunction(resultGraph, resultFn, /*gradient*/nullptr, status);
  if (checkStatus(SILFn.getLocation()))
    return true;

  // Everything is good!
  return false;
}

/// Serialize our resultGraph into a binary protobuf and return its bytes.  On
/// error, this emits a diagnostic, and returns an empty buffer.
static std::vector<char> serializeGraphProtoBuf(SILFunction &SILFn,
                                                TF_Graph *resultGraph,
                                                TF_Status *status) {
  // Create a buffer to hold the result.
  auto buffer = TF_NewBuffer();
  SWIFT_DEFER { TF_DeleteBuffer(buffer); };

  // Serialize the graph into the buffer.
  TF_GraphToGraphDef(resultGraph, buffer, status);
  if (TF_GetCode(status) != TF_OK)  {
    diagnose(SILFn, SILFn.getLocation(), diag::tf_lowering_error,
             TF_Message(status));
    return {};
  }

  // If the user wants a copy of the graph in /tmp, emit it now.
  if (TFDumpGraph) {
    int resultFD = -1;
    SmallString<64> resultPath;
    auto error = llvm::sys::fs::createTemporaryFile(
        "tf-dump-graph-" + SILFn.getName().str(), "pb", resultFD, resultPath);
    if (error) {
      llvm::errs() << "error opening '" << resultPath.str()
                   << "' for -tf-dump-graph emission!\n";
    } else {
      llvm::outs() << "wrote binary graph of " << buffer->length
                   << " bytes to '" << resultPath.str() << "'\n";
      llvm::raw_fd_ostream file(resultFD, /*shouldClose*/ true,
                                /*unbuffered*/ false);
      file.write((const char*)buffer->data, buffer->length);
    }

    // Also write in a textual format.
    error = llvm::sys::fs::createTemporaryFile(
        "tf-dump-graph-" + SILFn.getName().str(), "pbtxt", resultFD,
        resultPath);
    if (error) {
      llvm::errs() << "error opening '" << resultPath.str()
                   << "' for -tf-dump-graph emission!\n";
    } else {
      size_t len;
      const char *content = TF_GraphDebugString(resultGraph, &len);
      llvm::outs() << "wrote textual graph of " << len << " bytes to '"
                   << resultPath.str() << "'\n";
      llvm::raw_fd_ostream file(resultFD, /*shouldClose*/ true,
                                /*unbuffered*/ false);
      file.write(content, len);

      // For debugging convenience, also write the graph proto to STDERR. If it
      // gets too large, we can flag-protect this mode.
      llvm::outs() << "The graph proto is: \n";
      llvm::outs() << content << "\n";
      free((void*)content);
    }
  }

  auto bufPtr = (const char*)buffer->data;
  return std::vector<char>(bufPtr, bufPtr + buffer->length);
}

#endif // SWIFT_ENABLE_TENSORFLOW

/// Gets a function name that can be used as a TF op name.
StringRef getTFCompatibleFuncName(SILFunction *fn) {
  auto fnName = fn->getName();
  if (fnName.startswith("$")) fnName = fnName.substr(1);
  return fnName;
}

/// Lower the specified SIL function (which was formed by the partitioner)
/// into a TensorFlow graph, and encode into a vector of bytes.
///
std::vector<char> tf::lowerTFGraph(
    SILFunction *fn, const GraphGlobalConfiguration &configuration,
    std::string &entryFnBaseName) {
#ifndef SWIFT_ENABLE_TENSORFLOW
  // This should never be called if TensorFlow support isn't enabled, but just
  // in case, emit an error message so a misconfiguration is diagnosable.
  llvm::errs() << "TensorFlow support is not built into this Swift compiler.\n";
  return {};
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

  TF_Graph *resultGraph = TF_NewGraph();
  TF_Status *status = TF_NewStatus();

  SWIFT_DEFER {
    TF_DeleteStatus(status);
    TF_DeleteGraph(resultGraph);
  };

  DevicePartitioner partitioner(*fn, configuration);
  entryFnBaseName = getTFCompatibleFuncName(fn);
  unsigned helperFuncId = 0;
  for (const auto deviceType : configuration.usedDeviceTypes) {
    assert(deviceType != DeviceType::ALL);
    auto *perDeviceFn = partitioner.extractFunctionForDevice(deviceType);
    bool isPrimaryFn = deviceType == configuration.primaryDeviceType;

    TFGraphLowering graphGen(*perDeviceFn, deviceType, configuration,
                             resultGraph, status);
    auto graphFnBody = graphGen.lowerToFunction([&graphGen, perDeviceFn]() {
      // This is the top level of the function, add its formal arguments.
      graphGen.lowerArgumentsToParams(perDeviceFn->getArguments(), {},
                                      perDeviceFn->getLocation());
      if (graphGen.errorOccurred) return;

      // Lower all of the code inside the function body (which can of course
      // recursively creates functions and call them as ops.
      auto structure = canonicalizeCFGForXLA(perDeviceFn);
      graphGen.lowerRegion(structure.get());
    });

    auto fnName = getTFCompatibleFuncName(perDeviceFn);

    bool hasSideEffects = false;
    SmallVector<TF_DataType, 4> inputTypes, outputTypes;
    if (graphGen.buildGraphFunction(graphFnBody, fnName, hasSideEffects,
                                    &inputTypes, &outputTypes))
      return {};

    // The func op type is `fnName`, with the caller node name being
    // based on `funcNodeBaseName`.
    std::string funcNodeBaseName = entryFnBaseName;
    if (!isPrimaryFn) {
      funcNodeBaseName += "_helper_" + llvm::utostr(helperFuncId);
      ++helperFuncId;
      assert(inputTypes.empty());
      assert(outputTypes.empty());
    }

    // Create the graph function for the top level code.
    if (graphGen.buildGraphNodesForTopLevelFunctionCall(
            fnName.str(), funcNodeBaseName, isPrimaryFn, inputTypes,
            outputTypes))
      return {};

    // Remove the partitioned function so it doesn't go through the normal
    // compiler flow.
    perDeviceFn->getModule().eraseFunction(perDeviceFn);
  }

  // Ok, we're done!  Serialize the resulting graph to a protobuf and return it.
  return serializeGraphProtoBuf(*fn, resultGraph, status);
#endif
}
