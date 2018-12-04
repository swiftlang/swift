//===--- TFUtilities.cpp - TensorFlow lowering utilities ------------------===//
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

#define DEBUG_TYPE "TFUtilities"
#include "TFUtilities.h"
#include "TFConstExpr.h"
#include "TFDeviceSupport.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/Module.h"
#include "swift/SIL/GraphFunctionDeviceInfo.h"
#include "swift/SIL/GraphOperationBuilder.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#ifdef SWIFT_ENABLE_TENSORFLOW
#include "tensorflow/c/c_api.h"
#endif

using namespace swift;
using namespace tf;

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &Context, SourceLoc loc,
                                   Diag<T...> diag, U &&... args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

static llvm::cl::opt<bool> TFDumpIntermediates(
    "tf-dump-intermediates", llvm::cl::init(false),
    llvm::cl::desc("Dump intermediate results in TensorFlow passes"));

static llvm::cl::opt<bool> TFDumpIntermediatesToTmp(
    "tf-dump-intermediates-tmp", llvm::cl::init(false),
    llvm::cl::desc("Dump intermediate results in "
                   "TensorFlow passes to files in /tmp"));

// The flag below is referenced in multiple translation units.
namespace llvm {
// This flag is used as a crutch to develop and test IRGen code that handles
// graph_op insts.
// TODO: Fold this flag into -Onone mode.
llvm::cl::opt<bool> TFDynamicCompilation(
    "tf-dynamic-compilation", llvm::cl::init(false),
    llvm::cl::desc(
        "When true, skip the partitioning and lowering pass, so that graph_op "
        "instructions flow to IRGen. This flag should not be turned on by end "
        "users, due to many restrictions (e.g. it will not work with "
        "tensorflow convention functions)."));
} // namespace llvm

static raw_ostream &getTmpLoggingStream() {
  // If we are supposed to dump the intermediates into /tmp, set that up now.
  SmallString<64> resultPath;
  int resultFD = -1;
  auto error = llvm::sys::fs::createTemporaryFile("tf-dump-intermediates",
                                                  "txt", resultFD, resultPath);

  // If we had an error, print something to stderr, but keep going.
  if (error) {
    llvm::errs() << "error opening -tf-dump-intermediates logging file '"
                 << resultPath.str() << "'!\n";
    return llvm::errs();
  }

  // This file never gets closed since the raw_ostream is immortal.
  return *new llvm::raw_fd_ostream(resultFD, /*shouldClose*/ true,
                                   /*unbuffered*/ true);
}

/// If the -tf-dump-intermediates flag has been passed, return a pointer to
/// the stream that we should print debug dump information to.  Otherwise,
/// return null.  This is used for integration unit tests and debugging.
llvm::raw_ostream *tf::getTFDumpIntermediateStream() {
  // If the -tf-dump-intermediates flag was passed, dump to stdout.
  if (TFDumpIntermediates)
    return &llvm::outs();

  if (!TFDumpIntermediatesToTmp)
    return nullptr;

  // Lazily initialize the logging stream.
  static llvm::raw_ostream &fileStream = getTmpLoggingStream();
  return &fileStream;
}

/// Given a nominal type decl, collect all fields. If it's a class decl, collect
/// all fields along the inheritance hierarchy.
static void getAllFields(NominalTypeDecl *decl,
                         SmallVectorImpl<VarDecl *> &fields) {
  for (auto *field : decl->getStoredProperties())
    fields.push_back(field);
  if (auto *classdecl = decl->getSelfClassDecl())
    if (auto *superclass = classdecl->getSuperclassDecl())
      getAllFields(superclass, fields);
}

/// If the specified decl has a single stored field, return it.  If it's a class
/// type, return there's exactly one field in the entire inheritance hierarchy.
/// Otherwise return null.
VarDecl *tf::getFieldIfContainsSingleField(NominalTypeDecl *decl) {
  SmallVector<VarDecl *, 4> fields;
  getAllFields(decl, fields);
  if (fields.size() == 1)
    return fields.front();
  return nullptr;
}

bool tf::isTensorHandle(SILType ty) {
  return isTensorHandle(ty.getASTType());
}

bool tf::isOpaqueHandle(SILType ty) {
  return isOpaqueHandle(ty.getASTType());
}

/// `ty` must be a valid TensorFlow element type "T", like Builtin.Int32. Turn
/// it into a TensorHandle<T> type.
SILType tf::convertElementTypeToTensorValueType(Type ty,
                                                const ASTContext &ctx) {
  assert(isValidTensorFlowElementType(ty));
  auto decl = ctx.getTensorHandleDecl();
  auto tensorType = BoundGenericClassType::get(decl, /*parent*/ Type(), ty);

  return SILType::getPrimitiveObjectType(tensorType->getCanonicalType());
}

/// If the specified type is a TensorFlow value type, return it.  Otherwise, it
/// must be a primitive type T.  In that case, wrap it to form TensorHandle<T>.
SILType tf::convertElementTypeToTensorValueType(SILType ty) {
  // If this is already TensorHandle<T>, return it.
  if (isTensorFlowValue(ty))
    return ty;

  return convertElementTypeToTensorValueType(ty.getASTType(),
                                             ty.getASTContext());
}

/// Looks up members by `name` in the context of `typeDecl`, `proto` and
/// `module`, and populates `results`.
static void
lookupProtocolRequiredMembers(NominalTypeDecl *typeDecl, ProtocolDecl *proto,
                              DeclName name, ModuleDecl *module,
                              SmallVectorImpl<ValueDecl *> &results) {
  // Make sure the given type conforms to the given protocol.
  SmallVector<ProtocolConformance *, 2> conformances;
  auto type = typeDecl->getDeclaredInterfaceType();
  typeDecl->lookupConformance(module, proto, conformances);
  assert(!conformances.empty() && "Type doesn't conform to the protocol?");
  // Look up nominal type candidates and protocol requirement candidates.
  SmallVector<ValueDecl *, 2> lookupResults;
  typeDecl->lookupQualified(type, name, NLOptions::NL_ProtocolMembers, nullptr,
                            lookupResults);
  // Append matches to results.
  for (ValueDecl *decl : lookupResults)
    results.push_back(decl);
}

SILFunction *tf::findSILFunctionForRequiredProtocolMember(
    NominalTypeDecl *typeDecl, ProtocolDecl *proto, DeclName name,
    ModuleDecl *module, SILModule &silModule) {
  SmallVector<ValueDecl *, 4> results;
  lookupProtocolRequiredMembers(typeDecl, proto, name, module, results);
  for (auto *result : results) {
    std::string name = SILDeclRef(result).mangle();
    if (auto *fn = silModule.findFunction(name, SILLinkage::PublicExternal))
      return fn;
  }
  return nullptr;
}

SubstitutionMap tf::getSingleSubstitutionMapForElementTypeAndSignature(
    Type ty, GenericSignature *genericSig) {
  assert(genericSig->getGenericParams().size() == 1);
  return SubstitutionMap::get(genericSig,
                              [&](SubstitutableType *t) { return ty; },
                              LookUpConformanceInSignature(*genericSig));
}

SubstitutionMap tf::getSingleSubstitutionMapForElementType(Type ty,
                                                           ASTContext &ctx) {
  // FIXME: consider storing `genericSig` somewhere to avoid rebuilding it
  // each time.
  auto builder = GenericSignatureBuilder(ctx);
  builder.addGenericParameter(GenericTypeParamType::get(0, 0, ctx));
  auto *genericSig = std::move(builder).computeGenericSignature(SourceLoc());
  return getSingleSubstitutionMapForElementTypeAndSignature(ty, genericSig);
}

//===----------------------------------------------------------------------===//
// Source Location Manipulation Helpers
//===----------------------------------------------------------------------===//

/// The SIL location for operations we process are usually deep in the bowels
/// of the tensor library code, which are all implementation details to the
/// user.  As such, walk the inlining location of the specified node to return
/// the first location *outside* of the tensor implementation goop.
SILDebugLocation tf::skipInternalLocations(SILDebugLocation loc) {
  auto ds = loc.getScope();

  if (!ds || loc.getLocation().getSourceLoc().isValid())
    return loc;

  // Zip through inlined call site information that came from the
  // implementation guts of the tensor library.  We want to report the
  // message inside the user's code, not in the guts we inlined through.
  for (; auto ics = ds->InlinedCallSite; ds = ics) {
    // Stop if ds is already inside a valid function location.
    if (SILFunction *F = ds->getInlinedFunction()) {
      if (F->getLocation().getSourceLoc().isValid())
        break;
    }
    // If we found a valid inlined-into location, then we are good.
    if (ics->Loc.getSourceLoc().isValid())
      return SILDebugLocation(ics->Loc, ics);
  }

  return loc;
}

SILLocation tf::getUserSourceLocation(SILValue value) {
  if (auto *inst = value->getDefiningInstruction())
    return getUserSourceLocation(inst);
  return getUserSourceLocation(value.getDebugLocation());
}

/// Get the user's source location for the specified instruction.  Because it
/// is an instruction, we can apply various heuristics to improve the
/// precision of the returned location information.
SILLocation tf::getUserSourceLocation(const SILInstruction *inst) {
  // If we have a struct extract from a type like Int, Float, or Tensor of an
  // internal type like Builtin.i64 or TensorHandle, look through it to the
  // higher level type, which will have better source location information.
  //
  // The struct-extract came from the implementation of some operator in the
  // standard library like "+", and we want the source of the parameter.
  if (auto *sei = dyn_cast<StructExtractInst>(inst)) {
    auto outerType = sei->getType().getASTType();
    if (outerType->is<BuiltinType>() || isTensorHandle(outerType)) {
      if (sei->getOperand()) // Could be called after dropAllReferences().
        return getUserSourceLocation(sei->getOperand());
    }
  }

  return getUserSourceLocation(inst->getDebugLocation());
}

/// Create a "Const" tensor operation containing the specified scalars, with
/// the specified shape and elementType (setting dtype).  The resultType is
/// the TensorHandle type to produce, and targetDevice is the device set for
/// the operation.
GraphOperationInst *
tf::createConstTensor(Type elementType, SymbolicValue scalars,
                      SymbolicValue shape, SILType resultType, SILLocation loc,
                      DeviceType targetDevice, SILBuilder &B) {
  auto &context = B.getASTContext();
  auto &allocator = context.getAllocator();

  // Ensure that the array of initializer values is in the module's allocator.
  scalars = scalars.cloneInto(allocator);
  shape = shape.cloneInto(allocator);
  assert(shape.getKind() == SymbolicValue::Array &&
         "expected array constants for scalars and shape");

  GraphOperationBuilder opBuilder("Const");

  // Add a dtype attribute for the array element.
  opBuilder.addAttribute(
      {context.getIdentifier("dtype$dtype"),
       convertSwiftTypeToConstantTFDataType(elementType)});

  // Add an attribute for the value$tensor attribute.
  auto tensorSuffix = GraphOperationInfo::getArgumentLoweringSuffix(
      GraphOperationInfo::ArgumentLowering::TensorAttribute);
  opBuilder.addAttribute(
      {context.getIdentifier(std::string("value") + tensorSuffix), scalars});

  // Add the shape$shape attribute if we have an array value.
  if (scalars.getKind() == SymbolicValue::Array) {
    auto shapeId = GraphOperationInfo::ArgumentLowering::ShapeAttribute;
    auto shapeSuffix = GraphOperationInfo::getArgumentLoweringSuffix(shapeId);
    opBuilder.addAttribute(
        {context.getIdentifier(std::string("shape") + shapeSuffix), shape});
  }

  // All graph_op's get a device.
  opBuilder.addAttribute(
      {context.getIdentifier(TF_DEVICE_ATTR),
       SymbolicValue::getString(getDeviceString(targetDevice), allocator)});

  // Finally build a new graphop instruction with the simplified operands.
  return opBuilder.build(B, context, loc, resultType);
}

GraphOperationInst *
tf::createTensorToInt1Inst(SILValue value, SILBuilder &builder,
                           SILLocation location,
                           GraphFunctionDeviceInfo &deviceInfo) {
  ASTContext &context = builder.getASTContext();
  GraphOperationBuilder opBuilder("tf_tensor_to_i1");
  opBuilder.addArgument(value);
  deviceInfo.handleDevicePlacement(
      "tf_tensor_to_i1",
      /*opDevice*/ getDeviceString(DeviceType::ALL),
      builder.getModule().getASTContext(), &opBuilder);
  GraphOperationInst *condValue = opBuilder.build(
      builder, context, location,
      {SILType::getBuiltinIntegerType(1, context)});
  assert(condValue->getNumResults() == 1);
  return condValue;
}

/// Return true when this function must be entirely lowered to a TF graph
/// function, with no host-side logic remaining (i.e., no sends/recvs, and no
/// start/stop tensor computation on the host side). In other words, this
/// function uses the tensorflow calling convention.
///
/// The only way to call/use such a function is from a TF graph node (e.g. by
/// referencing the function in a function-typed op attribute).
bool tf::isAcceleratorOnly(const SILFunction &hostFn) {
  return hostFn.getRepresentation() ==
         SILFunctionType::Representation::TensorFlow; // @convention(tensorflow)
}

//===----------------------------------------------------------------------===//
// TensorFunctionClassifier Implementation
//===----------------------------------------------------------------------===//

// This function contains a (somewhat ad-hoc) set of heuristics and rules in
// deciding what to partition (aka "transform").
//
// In general we first handle cases that do not need partition (cases that
// "return false"), such as functions that are not defined in this module, or
// functions that can and will be inlined.
//
// We then handle cases that should need partition, such as functions that are
// public or marked @inline(never).
//
// For the remaining functions, we use the following heuristics to decide
// whether to partition them: If they take or return tensorflow values, inlining
// them is believed to be profittable for GPE, so we do not partition
// them. Otherwise we partition them.
bool TensorFunctionClassifier::shouldBePartitioned(SILFunction *fn,
                                                   bool forceTFFunctions) {
  // Part 1: Functions that need no partition.

  // Ignore transparent functions.
  if (fn->isTransparent())
    return false;

  // Don't transform functions that are marked @_inlineable or inline(always)
  // unless they are a thunk.  Thunks are only generated for inherently
  // interesting declarations.
  if (!fn->isThunk()) {
    if (fn->getInlineStrategy() == AlwaysInline)
      return false;

    auto hasInlinableAttrs = [&](Decl *decl) -> bool {
      return decl->getAttrs().hasAttribute<InlinableAttr>();
    };
    if (auto dc = fn->getDeclContext()) {
      if (auto fnDecl = dc->getInnermostDeclarationDeclContext()) {
        if (hasInlinableAttrs(fnDecl))
          return false;

        // If this is an accessor for a computed property, check the property
        // for @_inlineable as well.
        if (auto *fd = dyn_cast<AccessorDecl>(fnDecl))
          if (hasInlinableAttrs(fd->getStorage()))
            return false;
      }
    }
  }

  // If this is a function defined in some other module, don't transform it.
  // We might still try inlining it into some call-sites defined in this module,
  // but we never partition externally defined functions.
  if (isAvailableExternally(fn->getLinkage()))
    return false;

  // Part 2: Functions that must be partitioned.

  // Graph functions always get partitioned because they can be used as
  // attributes.
  if (isAcceleratorOnly(*fn))
    return true;

  // If the function is marked public, but it isn't marked inlinable, then it is
  // a public entrypoint that cannot be deabstracted through, so we must
  // transform it.
  //
  // TODO: It will probably be a common error to forget to add the inlinable
  // attributes, we should either infer the attribute or produce better QoI that
  // suggests adding it when an error occurs.
  if (fn->getLinkage() == SILLinkage::Public)
    return true;

  // If the function is explicitly marked @noinline, then it should be
  // partitioned, even if it otherwise looks like it shouldn't be.
  if (fn->getInlineStrategy() == NoInline)
    return true;

  // Something is creating public thunks around 'shared' implementations, which
  // prevents the above check from working.  Check for public functions.
  // FIXME: This should go away when we get deabstraction.
  if (fn->getLinkage() == SILLinkage::Shared)
    if (auto *dc = fn->getDeclContext())
      if (auto *fd = dyn_cast<FuncDecl>(dc))
        if (fd->getFormalAccess() >= AccessLevel::Public)
          return true;

  // Part 3: Use function signature to decide whether to partition.

  // Otherwise, the function is either (public and inlinable) or it is internal
  // to the current module.  In both cases, we check to see if the function
  // takes tensorflow values as arguments or results.  If so, it will be inlined
  // away by deabstraction, and we don't need to touch it.
  if (containsTensorFlowValue(fn->getLoweredFunctionType())) {
    if (forceTFFunctions) {
      // Return true if it is referenced somewhere.
      // TODO: There might be a better check other than fn->getRefCount() > 0.
      // See the clean up code in TFDeabstraction::inlineCalls() function.
      return fn->getRefCount() > 0;
    } else {
      return false;
    }
  }

  // If it contains no tensor inputs or results, then we are willing to
  // transform it!
  return true;
}

/// Return true if the specified function type has TensorHandle's in its
/// argument or result list, even if they are abstracted by structs or
/// tuples.
bool TensorFunctionClassifier::containsTensorFlowValue(
    CanSILFunctionType fnType) {
  for (auto &result : fnType->getResults())
    if (containsTensorFlowValue(result.getType(),
                                /*checkHigherOrderFunctions*/ true))
      return true;

  for (auto &param : fnType->getParameters())
    if (containsTensorFlowValue(param.getType(),
                                /*checkHigherOrderFunctions*/ true))
      return true;

  return false;
}
