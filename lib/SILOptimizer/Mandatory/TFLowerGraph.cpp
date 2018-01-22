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
#ifdef CMAKE_INTDIR
#include "tensorflow/c/c_api.h"
#else
#include "tensorflow/c/c_api.h"
#endif
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

#ifdef SWIFT_ENABLE_TENSORFLOW
template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag, U &&...args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
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

    /// This is the type of the parameter.
    SILType type;
  };

  /// This represents a TensorFlow TF_Function that is being constructed,
  /// including its inputs, outputs, live-in values used by the loop and the
  /// ops created that make it up.
  struct GraphFunctionBody {
    /// This is the temporary Graph we use to build up the body of this
    /// function.  When we're done building the graph, we transform it into a
    /// graph function, then copy it out into a resultGraph, deleting both the
    /// temporary graph and the temporary graph function.
    ///
    std::unique_ptr<TF_Graph, decltype(&TF_DeleteGraph)> graph;

    /// These are inputs to the graph function.
    SmallVector<GraphFunctionInput, 4> inputs;

    /// These are outputs from the function.  If this is the top level of the
    /// function, then this is filled in by a return instruction and the
    /// SILArgument*'s are null.  Otherwise, this is filled in by a BranchInst
    /// and the SILArgument's indicate which BB arguments are provided by this.
    SmallVector<std::pair<SILArgument*, TF_Output>, 4> outputs;

    /// This is a list of all of the operations that make up this function.
    std::vector<const TF_Operation*> operations;

  public:
    GraphFunctionBody()
      : graph(TF_NewGraph(), &TF_DeleteGraph) {}

    TF_Graph *getGraph() const { return graph.get(); }

    /// "Finish" a tensorflow op under construction, and remember that it is
    /// part of this graph function.
    TF_Operation *finishOp(TF_OperationDescription *desc, TF_Status *status) {
      auto result = TF_FinishOperation(desc, status);
      operations.push_back(result);
      return result;
    }
  };
}

namespace {
struct TFGraphLowering : public SILInstructionVisitor<TFGraphLowering> {
  SILFunction *SILFn;
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
  TFGraphLowering(SILFunction *F) : SILFn(F) {}

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
    diagnose(SILFn->getASTContext(), loc.getSourceLoc(), id, message);
    errorOccurred = true;
  }

  /// Given a GraphFunctionBody, which encapsulates all the information
  /// necessary to represent a tensorflow TF_Function, perform the final steps
  /// to generate the TF_Function itself and put it into the resultGraph.
  ///
  /// This emits an error and returns true on error.
  bool buildGraphFunction(const GraphFunctionBody &graphBody, const char *name);

private:  // Helpers to create TensorFlow graph nodes.
  unsigned OpID = 0;
  llvm::StringSet<> usedOpNames;
  std::string getUniqueName(SILDebugLocation loc, const char *baseName);

  TF_DataType getTensorFlowDataType(SILType type, SILLocation loc);

  TF_Output createParameter(SILType ty, SILLocation loc,
                            TF_Output passedValue,
                            GraphFunctionBody &fn);
public:  // Lowering functionality.

  /// Add an available value for the specified SILOpResult (a SILValue+result#)
  /// to the value mapping.  This defaults to setting its scope to the current
  /// function depth, but can be customized, so long as the value isn't added
  /// "under" an existing value.
  void addValueMapping(SILOpResult value, TF_Output result,
                       unsigned depth = ~0U) {
    // If an explicit depth isn't specified, then this is added to the current
    // function.
    if (depth == ~0U)
      depth = functionStack.size()-1;

#ifndef NDEBUG
    auto it = valueMapping.begin(value);
    assert((it == valueMapping.end() || it->second < depth) &&
           "value introduced multiple times in the same scope");
#endif
    valueMapping.insert(value, {result, depth});
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

    std::pair<TF_Output, unsigned> valueInfo = valueMapping.lookup({v, idx});
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
      value = createParameter(v->getType(), v.getLoc(), value,
                              functionStack[depth]);
      // Remember that it is the available version of this value at that depth.
      addValueMapping({v, idx}, value, depth);
    }

    return value;
  }

  // These get special handling, they are only used as operands to tfops.
  void visitIntegerLiteralInst(IntegerLiteralInst *inst) {}
  void visitFloatLiteralInst(FloatLiteralInst *inst) {}
  void visitMetatypeInst(MetatypeInst *inst) {}

  void visitBuiltinInst(BuiltinInst *inst);
  void visitBuiltinTFSendInst(BuiltinInst *inst) {
    internalError(inst->getLoc(),
                  "GraphGen cannot lower a 'send' to the host yet");
  }
  void visitBuiltinTFReceiveInst(BuiltinInst *inst) {
    internalError(inst->getLoc(),
                  "GraphGen cannot lower a 'receive' from the host yet");
  }
  void visitTFOpInst(BuiltinInst *inst);
  void visitTupleInst(TupleInst *inst);
  void visitTupleExtractInst(TupleExtractInst *inst);

  void visitReturnInst(ReturnInst *inst);
  void visitBranchInst(BranchInst *inst);

  void visitCondBranchInst(CondBranchInst *inst) {
    // Handled by region lowering.
  }

  // visitSILInstruction is the bottom level of the instruction visitor, where
  // unhandled instructions bottom out in.
  void visitSILInstruction(SILInstruction *inst) {
    internalError(inst->getLoc(), "GraphGen cannot lower this instruction yet");
    llvm::errs() << "Unhandled SIL instruction in TFGraphLowering:\n";
    inst->dump();
  }

  GraphFunctionBody lowerToFunction(SESERegionTree *r, bool isTopLevel = false);

  void lowerArguments();
  void lowerBasicBlock(SILBasicBlock *bb);
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

  auto &SM = SILFn->getASTContext().SourceMgr;

  // Form a name for this op based on the user's source location and "stack
  // trace" of where it got inlined in user code.  We use the form
  // file:line:col.
  for (auto ds = loc.getScope(); ds; ds = ds->InlinedCallSite) {
    // If the call site location is invalid, stop scanning.
    if (ds->Loc.isNull())
      break;

    if (SILFunction *F = ds->getInlinedFunction()) {
      auto lineCol = SM.getLineAndColumn(ds->Loc.getSourceLoc());
      auto fnName = F->getName();

      // Drop .tf_partition suffix off function names.
      if (fnName.endswith(".tf_partition"))
        fnName = fnName.drop_back(strlen(".tf_partition"));

      name += "."+fnName.str()+"."+llvm::utostr(lineCol.first);
      name += "."+llvm::utostr(lineCol.second);
    }
  }

  // If the debug location didn't have any other information, produce something
  // with the raw location.
  if (!loc.getScope() || loc.getScope()->Loc.isNull())
    if (!loc.getLocation().isNull()) {
      auto sourceLoc = loc.getLocation().getSourceLoc();
      auto lineCol = SM.getLineAndColumn(sourceLoc);
      auto bufferID = SM.getBufferIdentifierForLoc(sourceLoc);
      name += "."+bufferID.str()+"."+llvm::utostr(lineCol.first);
      name += "."+llvm::utostr(lineCol.second);
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
  if (auto elt = tf::isTensorHandle(type.getSwiftRValueType())) {
    if (auto ty = (TF_DataType)convertSwiftTypeToTF(elt))
      return ty;
  }

  if (auto ty = (TF_DataType)convertSwiftTypeToTF(type.getSwiftRValueType()))
    return ty;

  internalError(loc, "Unknown Swift type to lower to TensorFlow: " +
                type.getAsString());
  return TF_DataType(-1);
}

//===----------------------------------------------------------------------===//
// Helpers to create TensorFlow graph nodes.
//===----------------------------------------------------------------------===//

static TF_Tensor *convertConstantToTensor(LiteralInst *LI, TF_DataType dtype) {
  assert(dtype != TF_DataType() && "Expected to get a type!");
  auto dtypeSize = TF_DataTypeSize(dtype);

  // Make an uninitialized tensor that is big enough for our value.
  int64_t dim = 1;
  auto *tensor = TF_AllocateTensor(dtype, &dim, 1, dtypeSize);

  // Set up its contents.
  APInt value;
  if (auto *ILI = dyn_cast<IntegerLiteralInst>(LI)) {
    value = ILI->getValue();
  } else {
    value = cast<FloatLiteralInst>(LI)->getBits();
  }

  // FIXME: This will need a byte swap for big endian hosts.
  memcpy(TF_TensorData(tensor), value.getRawData(), dtypeSize);
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
  if (inst->getName().str().startswith("__tfop_"))
    return visitTFOpInst(inst);

  assert(0 && "Unhandled builtin instruction");
}

/// Lower a builtin for a TFOp instruction into a TensorFlow op node.
///
void TFGraphLowering::visitTFOpInst(BuiltinInst *inst) {
  TensorOpInfo tfopInfo(*inst);
  bool isValid = tfopInfo.decode();
  assert(isValid && "Invalid tfop formed by partitioning?"); (void)isValid;

  auto &graphFn = getCurrentGraphFunction();

  // The name label we put on the op is summarized from the "stack trace" of
  // the operation's source location.
  auto opLocString = getUniqueName(inst->getDebugLocation(), "op");

  auto *op = TF_NewOperation(graphFn.getGraph(), tfopInfo.opName.str().c_str(),
                             opLocString.c_str());

  // Each function argument for the op is a parameter that is passed in.
  unsigned nextOperand = 0;
  for (auto operandInfo : tfopInfo.operandDescriptors) {
    switch (operandInfo) {
    case OpCommand::Tensor: {
      auto opValue = getOperandValue(inst->getOperand(nextOperand++));
      if (!opValue.oper) return;  // Error occurred.
      TF_AddInput(op, opValue);
      break;
    }
    case OpCommand::Scalar:
      llvm_unreachable("Scalar operands should never reach graphgen");

    case OpCommand::Constant: {
      auto lit = tfopInfo.getTensorConstantOperand(nextOperand++);

      auto dtype = getTensorFlowDataType(lit->getType(), inst->getLoc());

      // Set the tensor as the 'value' attribute on the graph node.
      auto tensor = convertConstantToTensor(lit, dtype);
      TF_SetAttrTensor(op, "value", tensor, status);
      TF_DeleteTensor(tensor);

      if (checkStatus(inst->getLoc())) return;
      break;
    }

    case OpCommand::AddDType: {
      // This command adds the dtype of the result tensor as a dtype attribute.
      auto type = isTensorHandle(inst->getType().getSwiftRValueType());
      assert(type && "Not a valid builtin");
      TF_SetAttrType(op, "dtype", (TF_DataType)convertSwiftTypeToTF(type));
      break;
    }
    }
  }

  auto *result = graphFn.finishOp(op, status);

  // If the node builder failed, then the tfop definition is wrong, report an
  // error in a way that can hopefully be fixed - pointing to the op definition
  // in the Swift code, and emitting the TensorFlow error information.
  if (checkStatus(inst->getLoc(), diag::tfop_incorrect_definition))
    return;

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
    diagnose(SILFn->getASTContext(), inst->getLoc().getSourceLoc(),
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



void TFGraphLowering::lowerBasicBlock(SILBasicBlock *bb) {
  for (auto &inst : *bb) {
    visit(&inst);

    // If we produced an error lowering an instruction, give up hope and return.
    if (errorOccurred)
      return;
  }
}

void TFGraphLowering::lowerSequenceRegion(SequenceSESERegion *r) {
  for (auto &child : r->getNodes())
    lowerRegion(child.get());
}

void TFGraphLowering::lowerWhileLoopRegion(WhileLoopSESERegion *r) {
  // Our WhileLoopSESERegion has been structurized into a canonical form that
  // matches up pretty closely to the XLA while loop: we know we have a
  // preheader, a header block, that the header block has one exit edge, and
  // that there are no other exits out of the loop.
  //
  // This means that we can turn the computation that produces the bool for the
  // termination condition into the loop exit check, and turn the loop body
  // overall - possibly duplicating computation in the exit computation :-( -
  // into the XLA While Loop body.


  // TODO: ...

  // Finally, we can create the actual operation itself.  This is the Tensorflow
  // op description that we are generating:
  // REGISTER_OP("While")
  //   .Input("input: T")
  //   .Output("output: T")
  //   .Attr("T: list(type) >= 0")
  //   .Attr("cond: func")
  //   .Attr("body: func")

  internalError(SILFn->getLocation(),
                "TFLowerGraph cannot handle loops yet");
}

void TFGraphLowering::lowerConditionalRegion(ConditionalSESERegion *r) {
  assert(r->getTrue() && r->getFalse() &&
         "FIXME: The only way we could be missing true or false is if there "
         "is nothing being computed, but they could have side effects.");

  // Start by lowering any code that exists in the block that leads up to the
  // conditional branch.  This ensures that the condition bool is available.
  lowerBasicBlock(r->getBranchBB());
  if (errorOccurred)
    return;

  // The branch block should end with a conditional branch on a tf_tensor_to_i1
  // invocation.
  auto condBr = cast<CondBranchInst>(r->getBranchBB()->getTerminator());
  auto loc = condBr->getDebugLocation();

  auto cond = condBr->getCondition();
  auto tensorToI1 = cast<BuiltinInst>(cond);
  assert(tensorToI1->getName().str() == "tf_tensor_to_i1" &&
         tensorToI1->getNumOperands() == 1 &&
         "unexpected branch condition in graph lowering");
  cond = tensorToI1->getOperand(0);

  // Get the graph node that corresponds to the condition.
  auto condValue = getOperandValue(cond);
  if (!condValue.oper) return;  // Error occurred.

  // Lower the true and false bodies to graph functions.
  auto trueCodeFn = lowerToFunction(r->getTrue());
  if (errorOccurred)
    return;
  auto falseCodeFn = lowerToFunction(r->getFalse());
  if (errorOccurred)
    return;

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
  llvm::SmallSet<std::pair<void*, int>, 4> trueInputs;
  for (auto &input : trueCodeFn.inputs) {
    bool inserted = trueInputs.insert({input.passedValue.oper,
                                       input.passedValue.index}).second;
    assert(inserted && "A passed value shouldn't be added multiple times");
    (void)inserted;
  }

  // Scan the false function, adding entries to the true fn input list if it
  // lacks them, and build the false function index.
  llvm::SmallDenseMap<std::pair<void*, int>, unsigned> falseInputIndex;
  for (unsigned i = 0, e = falseCodeFn.inputs.size(); i != e; ++i) {
    auto &input = falseCodeFn.inputs[i];

    // Keep track of the all the false function entries (and their index) in the
    // falseInputIndex.
    auto &entry = falseInputIndex[{input.passedValue.oper,
                                   input.passedValue.index}];
    assert(entry == 0 && "A passed value shouldn't be added multiple times");
    entry = i+1;  // Entry in the map is 1-biased.

    // Check to see if the true function already has this passed value.
    if (!trueInputs.insert({input.passedValue.oper,
                            input.passedValue.index}).second)
      continue;  // Ignore common entries.

    // If not, add the parameter to the true list.
    createParameter(input.type, loc.getLocation(), input.passedValue,
                    trueCodeFn);
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
    inputTypes.push_back(getTensorFlowDataType(input.type, loc.getLocation()));

    // Figure out where the false node parameter should come from.
    auto entry = falseInputIndex[{input.passedValue.oper,
                                  input.passedValue.index}];

    // If the false function already had this parameter set up, use it.
    if (entry != 0) {
      falseCodeFn.inputs.push_back(falseInputList[entry-1]);
    } else {
      // Otherwise, we need to create a new parameter and add it.  Fortunately
      // this automatically adds it to the false function's input list for us.
      createParameter(input.type, loc.getLocation(), input.passedValue,
                      falseCodeFn);
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
                                                SILFn->getLocation()));

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

  // Create the graph functions for the true/false code.
  auto trueFnName = getUniqueName(loc, "true");
  if (buildGraphFunction(trueCodeFn, trueFnName.c_str()))
    return;
  auto falseFnName = getUniqueName(loc, "false");
  if (buildGraphFunction(falseCodeFn, falseFnName.c_str()))
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

  auto *result = graphFn.finishOp(op, status);
  if (checkStatus(loc.getLocation()))
    return;

  // Remember each of the results so that any references to the SIL BBArguments
  // that got defined end up referring to this node.
  for (int i = 0, e = trueCodeFn.outputs.size(); i != e; ++i)
    addValueMapping({trueCodeFn.outputs[i].first, 0}, {result, i});

#if 0  // TODO: Remove when we get If working.

  // XLA does not have a usable if condition yet, so we may have to lower:
  //   if cond { trueCode } else { falseCode }
  // into:
  //   tmp = cond
  //   while tmp {
  //      trueCode
  //      tmp = 0
  //   }
  //   tmp = !cond
  //   while tmp {
  //      falseCode
  //      tmp = 0
  //   }
  //
  // The real problem with doing this though is that XlaWhile loops are overly
  // limited on the result types of the loop: they can only produce an output
  // value if it is an input.  In real code, you can have something like this:
  //    if cond {
  //      a = foo()
  //    } else {
  //      a = bar()
  //    }
  // The problem with this is that since "a" is created on both branches to the
  // if statement we have no idea what shape it is.  Promoting it to being an
  // input of the XlaWhile doesn't work because XLA requires static shapes.
#endif
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


#endif // SWIFT_ENABLE_TENSORFLOW

//===----------------------------------------------------------------------===//
// Top Level driver
//===----------------------------------------------------------------------===//

/// Create a "Placeholder" op parameter input on the specified function with the
/// specified type.  When this is created on inner functions, passedValue
/// indicates the value that is passed in to fulfill this parameter from the
/// next outer scope.
TF_Output TFGraphLowering::
createParameter(SILType ty, SILLocation loc, TF_Output passedValue,
                GraphFunctionBody &fn) {
  auto opName = "arg_" + llvm::utostr(OpID++);
  auto *desc = TF_NewOperation(fn.getGraph(), "Placeholder", opName.c_str());
  auto type = getTensorFlowDataType(ty, loc);
  if (!type) {
    internalError(loc, "use of unknown dtype!");
    return { nullptr, 0 };
  }
  TF_SetAttrType(desc, "dtype", type);

  auto result = fn.finishOp(desc, status);
  if (checkStatus(loc))
    return { nullptr, 0 };

  // Success!  Remember this parameter, and the value that is passed in.
  fn.inputs.push_back({{ result, 0 }, passedValue, ty });
  return { result, 0 };
}


void TFGraphLowering::lowerArguments() {
  auto &graphFn = getCurrentGraphFunction();
  auto loc = SILFn->getLocation();
  for (auto arg : SILFn->getArguments()) {
    auto result = createParameter(arg->getType(), loc, TF_Output(), graphFn);
    if (result.oper == nullptr)
      return;

    addValueMapping({arg, 0}, result);
  }
}


/// Lower the specified SESE region to a GraphFunctionBody.
GraphFunctionBody TFGraphLowering::
lowerToFunction(SESERegionTree *r, bool isTopLevel) {
  // Push a scope, allowing us to keep track of any live-in values in the
  // true code.  These will need to become tuple elements live across the
  // loop.
  ValueMappingScopedHashTable::ScopeTy scope(valueMapping);

  /// Start a new graph function.
  functionStack.push_back(GraphFunctionBody());

  // If this is the top level of the function, add its formal arguments.
  if (isTopLevel)
    lowerArguments();

  // Lower all of the code inside the region (which can of course recursively
  // create functions and call them as ops.
  lowerRegion(r);

  auto result = std::move(functionStack.back());
  functionStack.pop_back();
  return result;
}


/// Given a GraphFunctionBody, which encapsulates all the information
/// necessary to represent a tensorflow TF_Function, perform the final steps
/// to generate the TF_Function itself and put it into the resultGraph.
///
/// This emits an error and returns true on error.
bool TFGraphLowering::
buildGraphFunction(const GraphFunctionBody &graphBody, const char *name) {
  if (errorOccurred)
    return true;

  SmallVector<TF_Output, 4> ins, outs;
  ins.reserve(graphBody.inputs.size());
  for (auto &elt : graphBody.inputs)
    ins.push_back(elt.parameter);
  outs.reserve(graphBody.outputs.size());
  for (auto &elt : graphBody.outputs)
    outs.push_back(elt.second);

  auto resultFn =
    TF_GraphToFunction(graphBody.getGraph(), name,
                       /*append_hash_to_fn_name*/false,
                       /*num_opers*/graphBody.operations.size(),
                       /*opers*/graphBody.operations.data(),
                       /*numinputs*/ins.size(),
                       /*inputs*/ins.data(),
                       /*noutputs*/outs.size(),
                       /*outputs*/outs.data(),
                       /*outputnames*/nullptr,
                       /*functionoptions*/nullptr,
                       "", status);
  // Diagnose any error that occurred if it happened building the graph.
  if (checkStatus(SILFn->getLocation()))
    return true;
  SWIFT_DEFER { TF_DeleteFunction(resultFn); };

  // Now that we have a function, copy it into the result graph.  We do this
  // multi-stage create-then-form-function-then-copy-function approach because
  // we don't want the nodes for the original function body to end up in the
  // result graph.
  TF_GraphCopyFunction(resultGraph, resultFn, /*gradient*/nullptr, status);
  if (checkStatus(SILFn->getLocation()))
    return true;

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
  if (checkStatus(SILFn->getLocation())) return {};

  auto bufPtr = (const char*)buffer->data;
  return std::vector<char>(bufPtr, bufPtr + buffer->length);
}

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

  if (shouldDumpIntermediates()) {
    llvm::outs() << "--- XLA CFG Canonicalize: " << fn->getName() << "\n";
    structure->print(llvm::outs());
    llvm::outs() << "\n--- XLA CFG Canonicalize end\n";
  }

  // TensorFlow likes to print out lots of informational messages to the
  // console, which are just noise.  This is apparently controlled through
  // an environment variable, so we set it to silence these informational logs.
  //
  // TODO: is there a better way to silence this?  We could forcably change
  // the file descriptor mapping...
  setenv("TF_CPP_MIN_LOG_LEVEL", "2", 1);

  TFGraphLowering graphGen(fn);
  auto graphFnBody = graphGen.lowerToFunction(structure.get(),
                                              /*isTopLevel*/true);
  // Create the graph function for the top level code.
  if (graphGen.buildGraphFunction(graphFnBody, "the_function"))
    return {};

  // Ok, we're done!  Serialize the resulting graph to a protobuf and return it.
  return graphGen.serializeGraphProtoBuf();
#endif
}
