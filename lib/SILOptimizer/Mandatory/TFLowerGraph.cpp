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
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#endif
using namespace swift;
using namespace tf;

static llvm::cl::opt<bool>
TFDumpGraph("tf-dump-graph", llvm::cl::init(false),
            llvm::cl::desc("Dump generated tensorflow graphs to /tmp"));

static llvm::cl::opt<bool>
TFTargetTPU("tf-target-tpu", llvm::cl::init(false),
            llvm::cl::desc("If true, target TPU in the generated TF graph"));

#ifdef SWIFT_ENABLE_TENSORFLOW
template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(SILFunction &fn, SILLocation loc, Diag<T...> diag, U &&...args) {
  auto &diags = fn.getASTContext().Diags;
  return diags.diagnose(loc.getSourceLoc(), diag, std::forward<U>(args)...);
}


/// This struct holds information about the global configuration of the graph
/// we are generating.  This can be different between distinct graphs in the
/// same program though.
struct GraphGlobalConfiguration {
  bool isTPUEnabled = false;
  bool isTPUInfeedEnabled = false;
};

static const char DEVICE_TPU_REPLICATED_CORE[] = "TPU_REPLICATED_CORE";
static const char DEVICE_TPU_SYSTEM[] = "TPU_SYSTEM";

/// When generating a TF TPU graph, call this function to place an eligible TF
/// graph node onto TPU device. Some nodes such as Placeholder and
/// Dataset/Iterator nodes are not eligible for TPU.
static void markNodeAsTPUReplicated(TF_OperationDescription *desc) {
  static const char *const TPU_CLUSTER_ATTR_NAME = "_tpu_replicate";
  static const char *const TPU_CLUSTER_ATTR_VALUE = "TPUReplicate/cluster";
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
    GraphGlobalConfiguration configuration;

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

  public:
    GraphFunctionBody(GraphGlobalConfiguration configuration)
      : configuration(configuration), graph(TF_NewGraph(), &TF_DeleteGraph) {}

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
      if (configuration.isTPUEnabled && isEligibleForTPU) {
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
  };
}

namespace {
struct TFGraphLowering : public SILInstructionVisitor<TFGraphLowering> {
  SILFunction &SILFn;
  GraphGlobalConfiguration configuration;
  TF_Graph *resultGraph = TF_NewGraph();
  TF_Status *status = TF_NewStatus();

  /// This is a stack of the currently active TF_Function's that are being
  /// constructed.  Nodes that are created are added to the innermost function,
  /// and we use this to keep track of live-in values and other state.
  std::vector<GraphFunctionBody> functionStack;

  // As we walk the SESE region tree, we expect each referenced value to have
  // already been lowered.  However, it may be in an outer loop nest.  As such,
  // this scoped hash table keeps track of which tensorflow graph node result
  // a value corresponds to, along with the scope ID of the value.
  ValueMappingScopedHashTable valueMapping;

  /// This flag gets set if lowering code to the graph produces a TensorFlow
  /// error and emits a diagnostic.  This tells us to stop lowering and give up
  /// gracefully.
  bool errorOccurred = false;
public:
  TFGraphLowering(SILFunction &fn, GraphGlobalConfiguration configuration)
    : SILFn(fn), configuration(configuration) {}

  /// Return the current graph function that is being set up.
  GraphFunctionBody &getCurrentGraphFunction() {
    return functionStack.back();
  }

  /// Serialize our resultGraph into a binary protobuf and return its bytes.  On
  /// error, this emits a diagnostic, and returns an empty buffer.
  std::vector<char> serializeGraphProtoBuf();

  ~TFGraphLowering() {
    TF_DeleteStatus(status);
    TF_DeleteGraph(resultGraph);
  }

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
                          bool &hasSideEffects,bool isTopLevelFunction = false);

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

  /// Builds TF graph nodes for the top level TF function call, where `funcName`
  /// is the function name. `inputs` specifies the inputs to the function, and
  /// `numOutputs` specifies the number of outputs.
  ///
  /// The created nodes follow this naming convention shared with the runtime:
  /// - The graph node for the function call has op type set <funcName>, and
  ///   name set to tfc_func_<funcName>.
  /// - The i-th input node is named tfc_input_<i>_<funcName>
  /// - The j-th output node is named tfc_output_<j>_<funcName>
  /// - If TPU infeed is enabled, the graph contains an 'InfeedEnqueueTuple'
  ///   node for caller feed input tensors.
  ///
  /// Additional graph nodes may be added for TPU graph creation, but they are
  /// implementation details that runtime need not be aware of.
  ///
  /// This emits an error and returns true on error.
  bool buildGraphNodesForTopLevelFunctionCall(StringRef funcName,
                                              ArrayRef<TF_Output> inputs,
                                              unsigned numOutputs);

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
  void visitBuiltinTFSendInst(BuiltinInst *inst) {
    internalError(inst->getLoc(),
                  "GraphGen cannot lower a 'send' to the host yet");
  }
  void visitBuiltinTFReceiveInst(BuiltinInst *inst) {
    internalError(inst->getLoc(),
                  "GraphGen cannot lower a 'receive' from the host yet");
  }
  /// Create a stack of TF dataset and iterator nodes up to IteratorGetNext.
  ///
  /// FIXME: Dissolve this builtin into a set of finer-grained, composer
  /// features.
  void visitTFDataset(BuiltinInst *inst);
  bool createDatasetIteratorNodesWithInfeedEnqueue();

  void visitTFOpInst(BuiltinInst *inst);
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
};
}

/// Produce a "stack trace" for the specified location, producing it in a form
/// that we can use as a uniqued op name.
std::string TFGraphLowering::getUniqueName(SILDebugLocation loc,
                                           const char *baseName) {
  std::string name = baseName;

  // Skip over internal implementation details of the Tensor library.
  loc = skipInternalLocations(loc);

  auto &SM = SILFn.getASTContext().SourceMgr;

  // Form a name for this op based on the user's source location and "stack
  // trace" of where it got inlined in user code.  We use the form
  // file:line:col.
  for (auto ds = loc.getScope(); ds; ds = ds->InlinedCallSite) {
    // If the call site location is invalid, stop scanning.
    if (!ds->Loc.getSourceLoc().isValid())
      break;

    if (SILFunction *F = ds->getInlinedFunction()) {
      if (!F->getLocation().getSourceLoc().isValid())
        break;

      auto lineCol = SM.getLineAndColumn(ds->Loc.getSourceLoc());
      auto fnName = F->getName();

      // Drop .tf_partition suffix off function names.
      if (fnName.endswith(".tf_partition"))
        fnName = fnName.drop_back(strlen(".tf_partition"));

      // $ isn't a valid character in a tensorflow op name, and gets added to
      // the start of some symbols.  If we see it, drop it.
      if (fnName.startswith("$"))
        fnName = fnName.drop_front();

      name += "."+fnName.str()+"."+llvm::utostr(lineCol.first);
      name += "."+llvm::utostr(lineCol.second);
    }
  }

  // If the debug location didn't have any other information, produce something
  // with the raw location.
  if (!loc.getScope() || loc.getScope()->Loc.isNull()) {
    auto sourceLoc = loc.getLocation().getSourceLoc();
    if (sourceLoc.isValid()) {
      auto lineCol = SM.getLineAndColumn(sourceLoc);
      auto bufferID = SM.getBufferIdentifierForLoc(sourceLoc);
      name += "."+bufferID.str()+"."+llvm::utostr(lineCol.first);
      name += "."+llvm::utostr(lineCol.second);
    }
  }

  // If we've already used this name, rename it to make it unique.
  while (!usedOpNames.insert(name).second) {
    name += "_"+llvm::utostr(OpID++);
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
  if (inst->getName().str().startswith("tensorflowReceive_"))
    return visitBuiltinTFReceiveInst(inst);
  if (inst->getName().str().startswith("tensorflowSend_"))
    return visitBuiltinTFSendInst(inst);
  if (inst->getName().str().startswith(
          "__tfop_tfc.makeIteratorGetNextWithDatasets"))
    return visitTFDataset(inst);
  // This is a marker that doesn't actually directly generate graph logic.
  if (inst->getName().str().startswith("__tfop_tfc.configureTPU"))
    return;
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
    TF_SetDevice(desc, "/device:CPU:0");
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
static void decodeShapeElements(SILValue attrValue, SILInstruction *inst,
                                const SILTensorOpInfo &tfopInfo,
                                unsigned &operandIdx, unsigned operandEndIdx,
                                SmallVectorImpl<int64_t> &shape) {
  assert(isa<MetatypeInst>(attrValue) && "$shape should start with a metatype");
  while (operandIdx + 1 < operandEndIdx &&
         tfopInfo.operandClasses[operandIdx + 1].second ==
             SILTensorOpInfo::OperandClass::ArrayElement) {
    auto eltValue = inst->getOperand(++operandIdx);
    auto intValue = cast<IntegerLiteralInst>(eltValue);
    shape.push_back(intValue->getValue().getLimitedValue());
  }
}

/// Decode the shape array attribute from the tfop builtin instruction, starting
/// at operand `operandIdx`, into `dims`, `numDims` and `dimPtrs`.
static void decodeShapeArray(SILInstruction *inst,
                             const SILTensorOpInfo &tfopInfo,
                             unsigned &operandIdx, unsigned operandEndIdx,
                             SmallVectorImpl<int64_t> &dims,
                             SmallVectorImpl<int> &numDims,
                             SmallVectorImpl<int64_t *> &dimPtrs) {
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

    decodeShapeElements(nextOperand, inst, tfopInfo, operandIdx, operandEndIdx,
                        dims);
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

void TFGraphLowering::visitTFDataset(BuiltinInst *inst) {
  // FIXME: Also support dataset/iterator outside of TPU context.
  if(!configuration.isTPUEnabled || !configuration.isTPUInfeedEnabled) {
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
    auto opInfo = tfopInfo.operandClasses[1];
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
  decodeShapeArray(inst, tfopInfo, i, e, dims, numDims, dimPtrs);

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
        TF_SetAttrString(op, name.c_str(), value.data(), value.size());
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
        decodeShapeElements(shapeAttrValue, inst, tfopInfo, i, e, shape);
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
      decodeShapeElements(operand, inst, tfopInfo, i, e, shape);
      TF_SetAttrShape(op, name.c_str(), shape.data(), shape.size());
      break;
    }
    case SILTensorOpInfo::OperandClass::ShapeArray: {
      SmallVector<int64_t, 8> dims;
      SmallVector<int, 3> numDims;
      SmallVector<int64_t*, 8> dimPtrs;
      decodeShapeArray(inst, tfopInfo, i, e, dims, numDims, dimPtrs);
      TF_SetAttrShapeList(op, name.c_str(), dimPtrs.data(), numDims.data(),
                          numDims.size());
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
      graphFn.outputs.push_back({ /*SILArgument*/nullptr, result });
    }
  } else {
    auto result = getOperandValue(inst->getOperand());
    if (!result.oper) return; // Error occurred.
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
    // This will also be problematic when the condition is allowed to have
    // side effects (e.g. because of send and recv) because they cannot be
    // reissued in general.
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
    loopBodyFn.outputs.push_back({
      /*SILArgument*/nullptr, loopBodyFn.inputs[i].parameter
    });
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
    if (!configuration.isTPUEnabled) {
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
  SmallVector<TF_DataType, 4> inputTypes;
  for (auto &input : loopBodyFn.inputs) {
    inputs.push_back(input.passedValue);
    auto ty = getOpResultType(input.value);
    inputTypes.push_back(getTensorFlowDataType(ty, input.value.first.getLoc()));
  }

  bool hasSideEffects = false;

  // Create TF_Function's for our condition and body.
  auto loc = headerBr->getDebugLocation();
  auto loopBodyFnName = getUniqueName(loc, "whilebody");
  if (buildGraphFunction(loopBodyFn, loopBodyFnName, hasSideEffects))
    return;
  auto condFnName = getUniqueName(loc, "whilecond");
  if (buildGraphFunction(condFn, condFnName, hasSideEffects))
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
  SmallVector<TF_DataType, 4> inputTypes;

  for (auto &input : trueCodeFn.inputs) {
    // Build info we need to create the op node later.
    inputs.push_back(input.passedValue);
    auto ty = getOpResultType(input.value);
    inputTypes.push_back(getTensorFlowDataType(ty, loc.getLocation()));

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
  SmallVector<TF_DataType, 4> outputTypes;
  for (unsigned i = 0, e = trueCodeFn.outputs.size(); i != e; ++i) {
    auto arg = trueCodeFn.outputs[i].first;

    // Remember the result type for later when we're building the op node.
    outputTypes.push_back(getTensorFlowDataType(arg->getType(),
                                                SILFn.getLocation()));

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
  if (buildGraphFunction(trueCodeFn, trueFnName, hasSideEffects))
    return;
  auto falseFnName = getUniqueName(loc, "false");
  if (buildGraphFunction(falseCodeFn, falseFnName, hasSideEffects))
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
  functionStack.push_back(GraphFunctionBody(configuration));

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
    TF_SetDevice(desc, DEVICE_TPU_SYSTEM);
    TF_SetAttrBool(desc, "is_global_init", true);
    TF_FinishOperation(desc, status);
    if (checkStatus(SILFn.getLocation()))
      return true;
  }

  return false;
}

bool TFGraphLowering::buildGraphNodesForTopLevelFunctionCall(
    StringRef funcName, ArrayRef<TF_Output> inputs, unsigned numOutputs) {
  TF_Operation *metadataNode = nullptr;
  if (configuration.isTPUEnabled) {
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
  std::string funcNameStr = funcName.str();
  std::string funcNodeName = "tfc_func_" + funcNameStr;
  TF_OperationDescription *funcDesc =
      TF_NewOperation(resultGraph, /*op_type*/ funcNameStr.c_str(),
                      /*op_name*/ funcNodeName.c_str());
  if (configuration.isTPUEnabled)
    markNodeAsTPUReplicated(funcDesc);

  // FIXME: Revisit how to enable infeed outside the context of dataset /
  // iterators.
  // bool addTPUInfeedNodes = configuration.isTPUInfeedEnabled &&
  // !inputs.empty();
  bool addTPUInfeedNodes = false;

  // These vectors are only used when addTPUInfeedNodes is true.
  std::vector<TF_Output> infeedInputs;
  infeedInputs.reserve(inputs.size());
  std::vector<TF_DataType> infeedInputDtypes;
  infeedInputDtypes.reserve(inputs.size());
  std::vector<TF_Buffer *> infeedInputShapes;
  infeedInputShapes.reserve(inputs.size());
  SWIFT_DEFER {
    for (auto *shape : infeedInputShapes) {
      TF_DeleteBuffer(shape);
    }
  };

  // Handle inputs and finish constructing the function node.
  for (unsigned i = 0, e = inputs.size(); i != e; ++i) {
    std::string inputNodeName =
        "tfc_input_" + std::to_string(i) + "_" + funcNameStr;
    TF_OperationDescription *inputDesc =
        TF_NewOperation(resultGraph, "Placeholder", inputNodeName.c_str());
    TF_SetAttrType(inputDesc, "dtype", TF_OperationOutputType(inputs[i]));
    TF_Operation *placeholder = TF_FinishOperation(inputDesc, status);
    if (checkStatus(SILFn.getLocation())) return true;
    TF_Output inputNode{placeholder, 0};
    if (!configuration.isTPUEnabled) {
      // Feed I directly into F.
      TF_AddInput(funcDesc, inputNode);
    } else if (!addTPUInfeedNodes) {
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
    } else {
      // Collect the input tensors' metadata, to be used for infeed node
      // construction later.
      assert(!inputs.empty());
      TF_Output inputPlaceholder = {placeholder, 0};
      infeedInputs.push_back(inputPlaceholder);
      infeedInputDtypes.push_back(TF_OperationOutputType(inputPlaceholder));
      TF_Buffer *shapeProto = TF_NewBuffer();
      TF_OperationGetAttrTensorShapeProto(placeholder, "shape", shapeProto,
                                          status);
      if (checkStatus(SILFn.getLocation())) return true;
      infeedInputShapes.push_back(shapeProto);
    }
  }

  if (addTPUInfeedNodes) {
    // Create infeed enqueue and dequeue nodes.
    std::vector<const void *> shapeProtos;
    shapeProtos.reserve(inputs.size());
    std::vector<size_t> shapeProtoLens;
    shapeProtoLens.reserve(inputs.size());
    for (unsigned i = 0, e = inputs.size(); i != e; ++i) {
      shapeProtos.push_back(infeedInputShapes[i]->data);
      shapeProtoLens.push_back(infeedInputShapes[i]->length);
    }
    // The enqueue node runs on CPU.
    {
      auto *desc = TF_NewOperation(resultGraph, "InfeedEnqueueTuple",
                                   "InfeedEnqueueTuple");
      TF_AddInputList(desc, infeedInputs.data(), infeedInputs.size());
      TF_SetDevice(desc, "/device:CPU:0");
      TF_SetAttrInt(desc, "device_ordinal", 0);
      TF_SetAttrTypeList(desc, "dtypes", infeedInputDtypes.data(),
                         infeedInputDtypes.size());
      TF_SetAttrTensorShapeProtoList(desc, "shapes", shapeProtos.data(),
                                     shapeProtoLens.data(),
                                     shapeProtos.size(), status);
      if (checkStatus(SILFn.getLocation())) return true;
      TF_FinishOperation(desc, status);
      if (checkStatus(SILFn.getLocation())) return true;
    }
    // The enqueue node runs on TPU.
    {
      auto *desc = TF_NewOperation(resultGraph, "InfeedDequeueTuple",
                                   "InfeedDequeueTuple");
      TF_AddControlInput(desc, metadataNode);
      markNodeAsTPUReplicated(desc);
      TF_SetAttrTypeList(desc, "dtypes", infeedInputDtypes.data(),
                         infeedInputDtypes.size());
      TF_SetAttrTensorShapeProtoList(desc, "shapes", shapeProtos.data(),
                                     shapeProtoLens.data(),
                                     shapeProtos.size(), status);
      if (checkStatus(SILFn.getLocation())) return true;
      auto *dequeue = TF_FinishOperation(desc, status);
      if (checkStatus(SILFn.getLocation())) return true;
      for (unsigned i = 0, e = inputs.size(); i != e; ++i) {
        TF_AddInput(funcDesc, {dequeue, static_cast<int>(i)});
      }
    }
  }
  TF_Operation *funcNode = TF_FinishOperation(funcDesc, status);
  if (checkStatus(SILFn.getLocation())) return true;

  // Now handle outputs.
  for (unsigned i = 0; i != numOutputs; ++i) {
    std::string outputNodeName =
        "tfc_output_" + std::to_string(i) + "_" + funcNameStr;
    TF_OperationDescription *outputDesc =
        TF_NewOperation(resultGraph, "Identity", outputNodeName.c_str());
    TF_Output funcOutputNode{funcNode, static_cast<int>(i)};
    if (!configuration.isTPUEnabled) {
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

/// Given a GraphFunctionBody, which encapsulates all the information
/// necessary to represent a tensorflow TF_Function, perform the final steps
/// to generate the TF_Function itself and put it into the resultGraph.
///
/// This emits an error and returns true on error.
bool TFGraphLowering::buildGraphFunction(const GraphFunctionBody &graphBody,
                                         StringRef name, bool &hasSideEffects,
                                         bool isTopLevelFunction) {
  if (errorOccurred)
    return true;

  // Inform our callers whether this function contains side effects or not.
  hasSideEffects = graphBody.controlDependenceValue != nullptr;

  SmallVector<TF_Output, 4> ins, outs;
  ins.reserve(graphBody.inputs.size());
  for (auto &elt : graphBody.inputs)
    ins.push_back(elt.parameter);
  outs.reserve(graphBody.outputs.size());
  for (auto &elt : graphBody.outputs)
    outs.push_back(elt.second);

  auto resultFn = TF_GraphToFunction(graphBody.getGraph(), name.str().c_str(),
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

  if (isTopLevelFunction) {
    return buildGraphNodesForTopLevelFunctionCall(name.str(), ins,
                                                  outs.size());
  }

  // Everything is good!
  return false;
}


/// Serialize our resultGraph into a binary protobuf and return its bytes.  On
/// error, this emits a diagnostic, and returns an empty buffer.
std::vector<char> TFGraphLowering::serializeGraphProtoBuf() {
  // Create a buffer to hold the result.
  auto buffer = TF_NewBuffer();
  SWIFT_DEFER { TF_DeleteBuffer(buffer); };

  // Serialize the graph into the buffer.
  TF_GraphToGraphDef(resultGraph, buffer, status);
  if (checkStatus(SILFn.getLocation())) return {};

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
      llvm::errs() << "wrote binary graph of " << buffer->length
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
      llvm::errs() << "wrote textual graph of " << len << " bytes to '"
                   << resultPath.str() << "'\n";
      llvm::raw_fd_ostream file(resultFD, /*shouldClose*/ true,
                                /*unbuffered*/ false);
      file.write(content, len);

      // For debugging convenience, also write the graph proto to STDERR. If it
      // gets too large, we can flag-protect this mode.
      llvm::errs() << "The graph proto is: \n";
      llvm::errs() << content << "\n";
      free((void*)content);
    }
  }

  auto bufPtr = (const char*)buffer->data;
  return std::vector<char>(bufPtr, bufPtr + buffer->length);
}

/// Scan the specified function, looking for logic that configures the current
/// graph.
GraphGlobalConfiguration findConfigurationInfo(SILFunction &fn) {
  GraphGlobalConfiguration result;

  SILInstruction *alreadyFound = nullptr;
  for (auto &bb : fn)
    for (auto &inst : bb) {
      // Scan for the device configuration ops if present.
      auto tfopInfo = SILTensorOpInfo::decode(&inst);
      if (!tfopInfo ||
          tfopInfo->opName != "tfc.configureTPU")
        continue;

      // If we found one, make sure we don't have more than one.
      if (alreadyFound) {
        diagnose(fn, inst.getLoc(), diag::tf_lowering_multiple_device);
        diagnose(fn, alreadyFound->getLoc(),
                 diag::tf_lowering_multiple_device_prev);
        continue;
      }

      // Otherwise, remember this one and decode it.
      alreadyFound = &inst;

      // Eventually we'll support multiple different configuration ops, so
      // we recheck the opcode here.
      if (tfopInfo->opName == "tfc.configureTPU") {
        // Decode: tfc.configureTPU(isInfeedEnabled: bool)
        result.isTPUEnabled = true;
        auto infeedEnabled =
          cast<IntegerLiteralInst>(tfopInfo->getAttrOperand(0));
        result.isTPUInfeedEnabled = !infeedEnabled->getValue().isNullValue();

      } else {
        llvm_unreachable("unknown device configuration op");
      }
    }


  // If the program didn't specify, fall back to the command line option.
  // FIXME: We should remove this command line flag and convert the testsuite
  // over to using the new mechanism directly.
  if (!alreadyFound)
    result.isTPUEnabled = TFTargetTPU;

  return result;
}

#endif // SWIFT_ENABLE_TENSORFLOW


/// Lower the specified SIL function (which was formed by the partitioner)
/// into a TensorFlow graph, and encode into a vector of bytes.
///
std::vector<char> tf::lowerTFGraph(SILFunction *fn) {
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
    outs->flush();
  }

  // TensorFlow likes to print out lots of informational messages to the
  // console, which are just noise.  This is apparently controlled through
  // an environment variable, so we set it to silence these informational logs.
  //
  // TODO: is there a better way to silence this?  We could forcibly change
  // the file descriptor mapping...
  setenv("TF_CPP_MIN_LOG_LEVEL", "2", 1);

  GraphGlobalConfiguration configuration = findConfigurationInfo(*fn);

  TFGraphLowering graphGen(*fn, configuration);
  auto graphFnBody = graphGen.lowerToFunction([&]() {
    // This is the top level of the function, add its formal arguments.
    graphGen.lowerArgumentsToParams(fn->getArguments(), {}, fn->getLocation());
    if (graphGen.errorOccurred)
      return;

    // Lower all of the code inside the function body (which can of course
    // recursively creates functions and call them as ops.
    graphGen.lowerRegion(structure.get());
  });

  // Create the graph function for the top level code.
  auto fnName = graphGen.SILFn.getName();
  if (fnName.startswith("$"))
    fnName = fnName.substr(1);

  bool hasSideEffects = false;
  if (graphGen.buildGraphFunction(graphFnBody, fnName, hasSideEffects,
                                  /*isTopLevelFunction*/true))
    return {};

  // Ok, we're done!  Serialize the resulting graph to a protobuf and return it.
  return graphGen.serializeGraphProtoBuf();
#endif
}
