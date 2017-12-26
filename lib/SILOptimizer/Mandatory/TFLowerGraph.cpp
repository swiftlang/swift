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
// writes it out to disk as a protobuf.
//
//===----------------------------------------------------------------------===//

#include "TensorFlow.h"
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
struct TFGraphLowering : public SILInstructionVisitor<TFGraphLowering> {
  SILFunction *SILFn;
  TF_Graph *graph = TF_NewGraph();
  TF_Status *status = TF_NewStatus();

  llvm::DenseMap<std::pair<SILValue,unsigned>, TF_Output> valueMapping;
  SmallVector<TF_Output, 2> inputs, outputs;

  /// This flag gets set if lowering code to the graph produces a TensorFlow
  /// error and emits a diagnostic.  This tells us to stop lowering and give up
  /// gracefully.
  bool errorOcccurred = false;
public:
  TFGraphLowering(SILFunction *F) : SILFn(F) {}
  void lowerArguments();
  void lowerBlock(SILBasicBlock &BB);

  void writeFile(StringRef filename);

  ~TFGraphLowering() {
    TF_DeleteStatus(status);
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
    errorOcccurred = true;
  }

private:  // Helpers to create TensorFlow graph nodes.
  unsigned OpID = 0;
  llvm::StringSet<> usedOpNames;
  std::string getUniqueOpName(SILDebugLocation loc);

  TF_DataType getTensorFlowDataType(SILType type, SILLocation loc);

public:  // Lowering functionality.
  TF_Output getOperandValue(SILValue v) {
    // apply_inst's can produce multiple values, and we support tuple_extract
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

    auto it = valueMapping.find({v, idx});
    assert(it != valueMapping.end() && "Expected value not lowered?");
    return it->second;
  }

  // These get special handling, they are only used as operands to tfops.
  void visitIntegerLiteralInst(IntegerLiteralInst *inst) {}
  void visitFloatLiteralInst(FloatLiteralInst *inst) {}

  void visitBuiltinInst(BuiltinInst *inst);
  void visitBuiltinTFSendInst(BuiltinInst *inst);
  void visitBuiltinTFReceiveInst(BuiltinInst *inst);
  void visitTFOpInst(BuiltinInst *inst);
  void visitTupleInst(TupleInst *inst);
  void visitTupleExtractInst(TupleExtractInst *inst);
  void visitReturnInst(ReturnInst *inst);

  // visitSILInstruction is the bottom level of the instruction visitor, where
  // unhandled instructions bottom out in.
  void visitSILInstruction(SILInstruction *inst) {
    llvm::errs() << "Unhandled SIL instruction in TFGraphLowering:\n";
    inst->dump();
    exit(1);
  }
};
}

/// Produce a "stack trace" for the specified location, producing it in a form
/// that we can use as a uniqued op name.
std::string TFGraphLowering::getUniqueOpName(SILDebugLocation loc) {
  std::string name = "op";

  // Skip over internal implementation details of the Tensor library.
  loc = getUserSourceLocation(loc);

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

      name += ","+fnName.str()+":"+llvm::utostr(lineCol.first);
      name += ":"+llvm::utostr(lineCol.second);
    }
  }

  // If the debug location didn't have any other information, produce something
  // with the raw location.
  if (!loc.getScope() || loc.getScope()->Loc.isNull())
    if (!loc.getLocation().isNull()) {
      auto sourceLoc = loc.getLocation().getSourceLoc();
      auto lineCol = SM.getLineAndColumn(sourceLoc);
      auto bufferID = SM.getBufferIdentifierForLoc(sourceLoc);
      name += ","+bufferID.str()+":"+llvm::utostr(lineCol.first);
      name += ":"+llvm::utostr(lineCol.second);
    }

  // If we've already used this name, rename it to make it unique.
  while (!usedOpNames.insert(name).second) {
    name += "_"+llvm::utostr(OpID++);
  }

  return name;
}

static bool is64(const StructType *ty) {
  return ty->getDecl()->getASTContext().LangOpts.Target.isArch64Bit();
}

static TF_DataType convertSwiftPrimitiveTypeToTF(Type ty) {
  // Handle wrappers like Float, which come up in TensorHandle<Float>
  if (auto *s = ty->getAs<StructType>()) {
    // Make sure the type is defined inside the Swift module.
    auto context = s->getDecl()->getDeclContext()->getParentModule();
    if (!context || context->getName().str() != "Swift")
      return TF_DataType();

    return llvm::StringSwitch<TF_DataType>(s->getDecl()->getNameStr())
      .Case("Bool", TF_BOOL)
      .Case("Int8", TF_INT8)
      .Case("UInt8", TF_UINT8)
      .Case("Int16", TF_INT16)
      .Case("UInt16", TF_UINT16)
      .Case("Int32", TF_INT32)
      .Case("UInt32", TF_UINT32)
      .Case("Int64", TF_INT64)
      .Case("UInt64", TF_UINT64)
      .Case("Int8", TF_INT8)
      .Case("UInt8", TF_UINT8)
      .Case("Float", TF_FLOAT)
      .Case("Double", TF_DOUBLE)
      .Case("Int", is64(s) ? TF_INT64 : TF_INT32)
      .Case("UInt", is64(s) ? TF_UINT64 : TF_UINT32)
      .Default(TF_DataType());
  }

  // BuiltinIntegerType doesn't carry sign information, which TensorFlow needs,
  // so we can't rely on getting type information from the builtin types
  // themselves.

  if (auto *BIF = ty->getAs<BuiltinFloatType>()) {
    switch (BIF->getFPKind()) {
    case BuiltinFloatType::IEEE16: return TF_HALF;
    case BuiltinFloatType::IEEE32: return TF_FLOAT;
    case BuiltinFloatType::IEEE64: return TF_DOUBLE;
    case BuiltinFloatType::IEEE80:
    case BuiltinFloatType::IEEE128:
    case BuiltinFloatType::PPC128:
      return TF_DataType();
    }
  }

  return TF_DataType();
}

static TF_DataType convertSwiftPrimitiveTypeToTF(SILType ty) {
  return convertSwiftPrimitiveTypeToTF(ty.getSwiftRValueType());
}


TF_DataType TFGraphLowering::getTensorFlowDataType(SILType type,
                                                   SILLocation loc) {
  // Handle things like TensorHandle<Float>.
  if (auto elt = tf::isTensorHandle(type.getSwiftRValueType())) {
    if (auto ty = convertSwiftPrimitiveTypeToTF(elt))
      return ty;
  }

  if (auto ty = convertSwiftPrimitiveTypeToTF(type))
    return ty;

  internalError(loc, "Unknown Swift type to lower to TensorFlow: " +
                type.getAsString());
  return TF_DataType(-1);
}

void TFGraphLowering::writeFile(StringRef filename) {
  // Don't emit the file if an error occurred.
  if (errorOcccurred) return;

  auto *fn =
  TF_GraphToFunction(graph, filename.str().c_str(),
                     /*append_hash_to_fn_name,*/1,
                     // We don't pass in an explicit operation list, just have
                     // TF_GraphToFunction take everything in the graph.
                     /*num_opers*/-1, /*opers*/nullptr,
                     /*numinputs*/inputs.size(), /*inputs*/inputs.data(),
                     /*noutputs*/outputs.size(), /*outputs*/outputs.data(),
                     /*outputnames*/nullptr, /*functionoptions*/nullptr,
                     "", status);
  if (checkStatus(SILFn->getLocation())) return;

  SWIFT_DEFER { TF_DeleteFunction(fn); };

  // Now that we have a function, convert that into a serialized protobuf.
  auto buffer = TF_NewBuffer();
  SWIFT_DEFER { TF_DeleteBuffer(buffer); };

  TF_FunctionToFunctionDef(fn, buffer, status);
  if (checkStatus(SILFn->getLocation())) return;

  // Now that we have a serialized protobuf, write it to disk.
  std::error_code err;
  llvm::raw_fd_ostream of(filename.str()+".pb", err,
                          llvm::sys::fs::OpenFlags::F_None);
  if (err) {
    diagnose(SILFn->getASTContext(), SILFn->getLocation().getSourceLoc(),
             diag::tf_lowering_file_error, filename.str()+".pb");
    return;
  }

  of.write((const char*)buffer->data, buffer->length);
  of.close();
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

void TFGraphLowering::visitBuiltinTFSendInst(BuiltinInst *inst) {
  // Right now we just drop tensorflowSend_N on the floor.
  assert(inst->getNumOperands() == 1 && "Should send exactly one value");
}

void TFGraphLowering::visitBuiltinTFReceiveInst(BuiltinInst *inst) {
  assert(0 && "GraphGen cannot lower a receive instruction from the host");
  //valueMapping[{inst, 0}] = TF_Output{pi, 0};
  abort();
}

void TFGraphLowering::visitBuiltinInst(BuiltinInst *inst) {
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

  // The name label we put on the op is summarized from the "stack trace" of
  // the operation's source location.
  auto opLocString = getUniqueOpName(inst->getDebugLocation());

  auto *op = TF_NewOperation(graph, tfopInfo.opName.str().c_str(),
                             opLocString.c_str());

  SILValue lastTensorOperand;

  // This gets the dtype of the last tensor operand (or the operation as a
  // whole).
  auto getDType = [&]() -> TF_DataType {
    auto type = lastTensorOperand ? lastTensorOperand->getType()
                                  : inst->getType();
    return getTensorFlowDataType(type, inst->getLoc());
  };


  // Each function argument for the op is a parameter that is passed in.
  unsigned nextOperand = 0;
  for (auto operandInfo : tfopInfo.operandDescriptors) {
    switch (operandInfo) {
    case OpCommand::Tensor:
      lastTensorOperand = inst->getOperand(nextOperand++);
      TF_AddInput(op, getOperandValue(lastTensorOperand));
      break;
    case OpCommand::Constant: {
      auto lit = tfopInfo.getTensorConstantOperand(nextOperand++);

      // Set the tensor as the 'value' attribute on the graph node.
      auto tensor = convertConstantToTensor(lit, getDType());
      TF_SetAttrTensor(op, "value", tensor, status);
      TF_DeleteTensor(tensor);

      if (checkStatus(inst->getLoc())) return;
      break;
    }

    case OpCommand::AddDType: {
      // This command adds the dtype of the last tensor operand (or the
      // operation as a whole) as the dtype attribute.
      TF_SetAttrType(op, "dtype", getDType());
      break;
    }
    }
  }

  auto *result = TF_FinishOperation(op, status);

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
    errorOcccurred = true;
    return;
  }

  // Remember each of the results.
  for (unsigned i = 0; i != numActualResults; ++i) {
    TF_Output out;
    out.oper = result;
    out.index = i;
    valueMapping[{inst, i}] = out;
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
  assert((getOperandValue(inst), 1) && "Invalid tuple extract");
}


void TFGraphLowering::visitReturnInst(ReturnInst *inst) {
  assert(outputs.empty() && "Should only have one return per graph function");

  // The return is either using a single value or a tuple of values.  These
  // become the results of the graph.
  if (auto *ti = dyn_cast<TupleInst>(inst->getOperand())) {
    for (auto &operand : ti->getAllOperands())
      outputs.push_back(getOperandValue(operand.get()));
  } else {
    outputs.push_back(getOperandValue(inst->getOperand()));
  }
}

void TFGraphLowering::lowerArguments() {
  auto loc = SILFn->getLocation();
  for (auto arg : SILFn->getArguments()) {
    auto opName = "arg_" + llvm::utostr(OpID++);
    auto *desc = TF_NewOperation(graph, "Placeholder", opName.c_str());
    if (auto type = getTensorFlowDataType(arg->getType(), loc))
      TF_SetAttrType(desc, "dtype", type);
    else
      return;

    auto result = TF_FinishOperation(desc, status);
    if (checkStatus(loc)) return;

    inputs.push_back({ result, 0 });
    valueMapping[{arg, 0}] = inputs.back();
  }
}

void TFGraphLowering::lowerBlock(SILBasicBlock &BB) {
  if (errorOcccurred)
    return;

  for (auto &inst : BB) {
    visit(&inst);

    // If we produced an error lowering an instruction, give up hope and return.
    if (errorOcccurred)
      return;
  }
}
#endif // SWIFT_ENABLE_TENSORFLOW

//===----------------------------------------------------------------------===//
// Top Level driver
//===----------------------------------------------------------------------===//


/// Lower the specified SIL function (which was formed by the partitioner)
/// into a TensorFlow graph, and write it to disk.
void tf::emitTensorFlowGraph(SILFunction *fn, StringRef fnName) {
#ifndef SWIFT_ENABLE_TENSORFLOW
  // This should never be called if TensorFlow support isn't enabled, but just
  // in case, emit an error message so a misconfiguration is diagnosable.
  llvm::errs() << "TensorFlow support is not built into this Swift compiler.\n";
  exit(1);
#else
  // If we're generating a graph for XLA, we need to structurize the CFG into
  // single-entry-single-exit regions.
  auto structure = canonicalizeCFGForXLA(fn);


  llvm::outs() << "--- XLA CFG Canonicalize: " << fn->getName() << "\n";
  structure->print(llvm::outs());
  llvm::outs() << "\n--- XLA CFG Canonicalize end\n";

  // FIXME: Remove.
  return;

  // Right now we only support lowering graphs that are a single basic block.
  assert(fn->getBlocks().size() == 1 &&
         "TFLowerGraph can only handle single basic block programs");

  // TensorFlow likes to print out lots of informational messages to the
  // console, which are just noise.  This is apparently controlled through
  // an environment variable, so we set it to silence these informational logs.
  setenv("TF_CPP_MIN_LOG_LEVEL", "2", 1);

  TFGraphLowering graphGen(fn);
  graphGen.lowerArguments();
  graphGen.lowerBlock(*fn->getBlocks().begin());
  graphGen.writeFile(fnName);
#endif
}
