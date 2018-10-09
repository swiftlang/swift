//===--- TFUtilities.h - TensorFlow lowering utilities ----------*- C++ -*-===//
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
// This defines the shared code that implements the various TensorFlow related
// lowerings and other transformations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_TENSORFLOW_H
#define SWIFT_SILOPTIMIZER_TENSORFLOW_H

#include "TFDeviceSupport.h"
#include "swift/AST/TensorFlow.h"
#include "swift/SIL/GraphOperationInfo.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/CommandLine.h"
#ifdef SWIFT_ENABLE_TENSORFLOW
#include "tensorflow/c/c_api.h"
#endif

namespace llvm {
extern cl::opt<bool> TFDynamicCompilation;
}

namespace swift {
namespace tf {

// TODO: reformat the code below to remove indentation.

/// If the -tf-dump-intermediates flag has been passed, return a pointer to
/// the stream that we should print debug dump information to.  Otherwise,
/// return null.  This is used for integration unit tests and debugging.
llvm::raw_ostream *getTFDumpIntermediateStream();

/// If the specified decl has a single stored field, return it.  Otherwise
/// return null.
VarDecl *getFieldIfContainsSingleField(NominalTypeDecl *decl);

/// Return true if the specified type is the well-known TensorHandle<T> type.
bool isTensorHandle(SILType ty);
  
/// Return true if the specified type is the well-known opaque handle type such
/// as VariantHandle and ResourceHandle.
bool isOpaqueHandle(SILType ty);

/// `ty` must be a valid TensorFlow element type "T", like Builtin.Int32. Turn
/// it into a TensorHandle<T> type.
SILType convertElementTypeToTensorValueType(Type ty, const ASTContext &ctx);

/// If the specified type is a TensorFlow value type, return it.  Otherwise, it
/// must be a primitive type T.  In that case, wrap it to form TensorHandle<T>.
SILType convertElementTypeToTensorValueType(SILType ty);

/// Return true if the specified type is a valid tensor element type.  For
/// example, int128 and pointers are not.
///
/// TODO: This should eventually consider information about the target
/// deployment.
inline bool isValidTensorFlowElementType(Type ty) {
  return convertSwiftTypeToTF(ty) != 0;
}

/// Looks up a function by `name` in the context of `typeDecl`, `proto` and
/// `module`, and returns that function.
SILFunction *findSILFunctionForRequiredProtocolMember(NominalTypeDecl *typeDecl,
                                                      ProtocolDecl *proto,
                                                      DeclName name,
                                                      ModuleDecl *module,
                                                      SILModule &silModule);

/// Given an element type like `Float` and a generic signature with a single
/// type parameter, returns a substitution map suitable for calling a builtin
/// or function with such a substitution.
SubstitutionMap getSingleSubstitutionMapForElementTypeAndSignature(
    Type ty, GenericSignature *genericSig);

/// Given an element type like `Float`, returns a substitution map suitable
/// for calling a builtin or function with this single-entry substitution.
SubstitutionMap getSingleSubstitutionMapForElementType(Type ty,
                                                       ASTContext &ctx);

/// `inst` must have a single result, and return that result value.
static inline SILValue getSingleValueResult(GraphOperationInst *inst) {
  assert(inst->getNumResults() == 1);
  return inst->getResults()[0];
}

//===--------------------------------------------------------------------===//
// Source location helpers
//===--------------------------------------------------------------------===//

/// The SIL location for operations we process are usually deep in the bowels
/// of the tensor library code, which are all implementation details to the
/// user.  As such, walk the inlining location of the specified node to return
/// the first location *outside* of the tensor implementation goop.
SILDebugLocation skipInternalLocations(SILDebugLocation loc);

/// Skip over all the internal implementation details to get the source
///  location in user code.
inline SILLocation getUserSourceLocation(SILDebugLocation loc) {
  return skipInternalLocations(loc).getLocation();
}

/// Get the user's source location for the specified value.  If it is an
/// instruction, we can apply various heuristics to improve the precision of
/// the returned location information.
SILLocation getUserSourceLocation(SILValue value);
SILLocation getUserSourceLocation(SILInstruction *inst);

//===--------------------------------------------------------------------===//
// Other stuff
//===--------------------------------------------------------------------===//

/// Create a "Const" tensor operation containing the specified scalars, with
/// the specified shape and elementType (setting dtype).  The resultType is
/// the TensorHandle type to produce, and targetDevice is the device set for
/// the operation.
GraphOperationInst *createConstTensor(Type elementType, SymbolicValue scalars,
                                      SymbolicValue shape, SILType resultType,
                                      SILLocation loc, DeviceType targetDevice,
                                      SILBuilder &B);

/// Create a tf_tensor_to_i1 instruction with the given value as argument.
GraphOperationInst *createTensorToInt1Inst(SILValue value, SILBuilder &builder,
                                           SILLocation location,
                                           GraphFunctionDeviceInfo &deviceInfo);

/// Return true when this function must be entirely lowered to a TF graph
/// function, with no host-side logic remaining (i.e., no sends/recvs, and no
/// start/stop tensor computation on the host side). In other words, this
/// function uses the tensorflow calling convention.
///
/// The only way to call/use such a function is from a TF graph node (e.g. by
/// referencing the function in a function-typed op attribute).
bool isAcceleratorOnly(const SILFunction &hostFn);

/// This struct provides a an efficient implementation of a predicate that
/// determines whether a type is or contains a TensorHandle that will be
/// exposed after deabstraction.  This is a class instead of a simple
/// function because we memoize state to avoid rechecking types over and
/// over again.
class TensorFunctionClassifier {
  TypeContainsTensorFlowValue tctfc;

public:
  TensorFunctionClassifier() {}

  /// Return true if the specified function is the top-level context that
  /// tensor partitioning should be applied to.  This returns false (for
  /// example) for inlined functions that take and return tensors, since we
  /// know that they are either unreachable or will be inlined into any
  /// clients that use them.
  ///
  /// If the flag forceTFFunctions is true, forces partitioning of functions
  /// that operate on Tensors even if it would have been rejected otherwise.
  bool shouldBePartitioned(SILFunction *fn, bool forceTFFunctions);

  /// Return true if the specified function type has TensorFlow values in its
  /// argument or result list (and do so recursively, if `fnType` has an
  /// argument or result that is itself function-typed), even if they are
  /// abstracted by structs or tuples.
  bool containsTensorFlowValue(CanSILFunctionType fnType);

  /// Return true if the specified type contains a TensorFlow value type that
  /// will be exposed after deabstraction.
  /// If `checkHigherOrderFunctions`, also check for a function-typed `ty`, if
  /// its parameter or result contains any TensorFlow value type.
  bool containsTensorFlowValue(Type ty, bool checkHigherOrderFunctions) {
    return tctfc.containsTensorFlowValue(ty, checkHigherOrderFunctions);
  }

  /// Return true if the specified type contains a TensorFlow value type that
  /// will be exposed after deabstraction.
  /// If `checkHigherOrderFunctions`, also check for a function-typed `ty`, if
  /// its parameter or result contains any TensorFlow value type.
  bool containsTensorFlowValue(SILType ty, bool checkHigherOrderFunctions) {
    return containsTensorFlowValue(ty.getASTType(), checkHigherOrderFunctions);
  }
};

/// Represent the TF graph of a graph function named `graphFnName`, which
/// corresponds to the SIL host function `silHostFnName`. `graph` can contain
/// more functions beyond `graphFnName`, if that function calls into other
/// graph functions (e.g. if it has functional If/While ops).
struct LoweredGraphFunction {
  LoweredGraphFunction(const std::string &silHostFnName,
                       const std::string &graphFnName)
      : silHostFnName(silHostFnName), graphFnName(graphFnName) {}

  LoweredGraphFunction(LoweredGraphFunction &&) = delete;

  /// Used as the buffer to back a StringRef-typed map key value elsewhere.
  std::string silHostFnName;

  std::string graphFnName;
};

/// Each object lowers a set of accelerator functions into a single TF graph.
class TFGraphLowering {
  SILTransform &parentTransform;
  llvm::DenseMap<StringRef, std::unique_ptr<LoweredGraphFunction>>
      &graphFunctions;
  std::unique_ptr<TF_Graph, decltype(&TF_DeleteGraph)> graph;
  TF_Operation *metadataNodeForTPU = nullptr;

  /// This is a counter we use to give each cross-device send/receive
  /// operation a unique ID.
  int nextTensorTransferId = 0;

public:
  TFGraphLowering(
      SILTransform &parentTransform,
      llvm::DenseMap<StringRef, std::unique_ptr<LoweredGraphFunction>>
          &graphFunctions);

  /// Lower the accelerator-only function `fn` (which was formed by the
  /// partitioner) into a TensorFlow graph function, and add an entry to
  /// `graphFunctions`, keyed on `hostFnName`. This way another graph
  /// function foo() can call/use this function, if the corresponding SIL
  /// code of foo() calls/uses `hostFnName`.
  bool lowerTFFunction(StringRef hostFnName, SILFunction *fn,
                       const GraphFunctionDeviceInfo &deviceInfo);

  /// Similar to the function above, except it handles a
  /// non-accelerator-only function, which can be lowered to graph functions
  /// on a set of TF devices.
  ///
  /// When deviceInfo.usedDeviceTypes has N>1 devices, in addition to
  /// generating a graph function whose name is
  /// LoweredGraphFunction::graphFnName (referred to as `entryFnBaseName`),
  /// also generate another N-1 nodes named `entryFnBaseName_helper_{i}`,
  /// with i ranging from 0 to N-2. These N nodes correspond to the N
  /// per-device graph functions, and must be called by the runtime in a
  /// single SessionRun() call. Those N-1 helper functions take no input or
  /// output tensors, and are executed for their side-effects of
  /// sending/receiving tensors with the function of `entryFnBaseName`.
  bool lowerTFGraph(StringRef hostFnName, SILFunction *fn,
                    const GraphFunctionDeviceInfo &deviceInfo);

  /// Serialize `graph` into a binary protobuf into `bytes`.
  /// Return true on error, with an error diagnostic already emitted at
  /// `errorLoc`.
  bool serializeGraphProtoBuf(ASTContext &ctx, SILLocation errorLoc,
                              std::vector<char> &bytes);

  /// Return the graph for debug printing.
  TF_Graph *getGraphDebug() { return graph.get(); }

private:
  /// This is a helper function to unify the implementation of
  /// lowerTFFunction() and lowerTFGraph(). The former calls this method with
  /// `isAcceleratorOnly` set to true, and the latter false. See their doc
  /// comments on the semantics.
  ///
  ///  `graphFnNameForCaller` provides for the caller with a name to call this
  /// lowered graph function. If `isAcceleratorOnly` is true, it is the graph
  /// function name for a TF graph node to call; otherwise, it is a function
  /// name for the host runtime to call.
  bool lowerTFGraphOrFunction(StringRef hostFnName, SILFunction *fn,
                              const std::string &graphFnNameForCaller,
                              bool isAcceleratorOnly,
                              const GraphFunctionDeviceInfo &deviceInfo);
};

} // end namespace tf
} // end namespace swift
#endif
