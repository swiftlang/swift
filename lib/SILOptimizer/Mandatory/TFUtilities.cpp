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
typedef SILTensorOpInfo::OperandClass OperandClass;

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

/// If the specified decl has a single stored field, return it.  Otherwise
/// return null.
VarDecl *tf::getFieldIfContainsSingleField(NominalTypeDecl *decl) {
  // Check to see if there is a single stored field.
  auto fieldIt = decl->getStoredProperties().begin();
  if (fieldIt == decl->getStoredProperties().end())
    return nullptr;
  auto result = *fieldIt++;
  if (fieldIt != decl->getStoredProperties().end())
    return nullptr;
  return result;
}

bool tf::isTensorHandle(SILType ty) {
  return isTensorHandle(ty.getASTType());
}

bool tf::isOpaqueHandle(SILType ty) {
  return isOpaqueHandle(ty.getASTType());
}

/// Determine whether the specified type is one of our well-known types, and
/// if so, which one it is.
TFValueKind tf::classifyTensorFlowValue(SILType ty) {
  return classifyTensorFlowValue(ty.getASTType());
}

/// Return true if the specified type is TensorHandle<T>, ResourceHandle, or
/// VariantHandle.
bool tf::isTensorFlowValue(SILType ty) {
  return (bool)isTensorFlowValue(ty.getASTType());
}

static bool is64(Type ty) {
  return ty->getASTContext().LangOpts.Target.isArch64Bit();
}

/// This function maps a Swift type (either a language type like Float or an
/// LLVM Builtin type like Builtin.f32) into the TensorFlow TF_DataType value.
///
/// This returns 0 (which is an invalid tensorflow type ID) on error.
///
unsigned tf::convertSwiftTypeToTF(Type ty) {
#ifdef SWIFT_ENABLE_TENSORFLOW
  // Handle wrappers like Float, which come up in TensorHandle<Float>
  if (auto *s = ty->getAs<StructType>()) {
    // Make sure the type is defined inside the Swift module.
    auto context = s->getDecl()->getDeclContext()->getParentModule();
    if (!context || context->getName().str() != "Swift")
      return 0;

    return llvm::StringSwitch<unsigned>(s->getDecl()->getNameStr())
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
        .Case("BFloat16", TF_BFLOAT16)
        .Case("Float", TF_FLOAT)
        .Case("Double", TF_DOUBLE)
        .Case("Int", is64(s) ? TF_INT64 : TF_INT32)
        .Case("UInt", is64(s) ? TF_UINT64 : TF_UINT32)
        .Case("String", TF_STRING)
        .Default(0);
  }

  // BuiltinIntegerType doesn't carry sign information, which TensorFlow needs,
  // so we can't rely on getting type information from the builtin types
  // themselves.  For now we'll just use signed types.
  if (auto *BII = ty->getAs<BuiltinIntegerType>()) {
    if (BII->getWidth().isPointerWidth())
      return is64(ty) ? TF_INT64 : TF_INT32;

    switch (BII->getFixedWidth()) {
    case 1:
      return TF_BOOL;
    case 8:
      return TF_INT8;
    case 16:
      return TF_INT16;
    case 32:
      return TF_INT32;
    case 64:
      return TF_INT64;
    }
  }

  if (auto *BIF = ty->getAs<BuiltinFloatType>()) {
    switch (BIF->getFPKind()) {
    case BuiltinFloatType::IEEE16:
      return TF_HALF;
    case BuiltinFloatType::IEEE32:
      return TF_FLOAT;
    case BuiltinFloatType::IEEE64:
      return TF_DOUBLE;
    case BuiltinFloatType::IEEE80:
    case BuiltinFloatType::IEEE128:
    case BuiltinFloatType::PPC128:
      return 0;
    }
  }

  if (auto *BRPT = ty->getAs<BuiltinRawPointerType>()) {
    return TF_STRING;
  }
#endif
  return 0;
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

/// Analyze the specified SIL instruction and return a SILTensorOpInfo result if
/// the instruction is a valid tensor operation.  This is the way that
/// SILTensorOpInfo's are created.
Optional<SILTensorOpInfo> SILTensorOpInfo::decode(SILInstruction *inst) {
  // Tensor operations are builtin instructions and apply instructions.
  if (auto *builtin = dyn_cast<BuiltinInst>(inst)) {
    SILTensorOpInfo toiInfo(builtin);
    if (toiInfo.decodeBuiltin())
      return toiInfo;
  }
  return None;
}

typedef std::pair<StringRef, OperandClass> AttributeEntry;

/// Given a builtin name that refer to a tensorflow op function, this returns
/// the op name and operand clases and returns an empty string.  If the string
/// provided is invalid, this returns an error message to present.
static std::string
decodeTensorOpName(StringRef name, StringRef &opName,
                   SmallVectorImpl<AttributeEntry> &operandClasses) {
  // Decode the base name for the op.
  auto pos = name.find(",");
  opName = name.substr(0, pos);
  if (pos == StringRef::npos)
    return "";
  name = name.substr(pos);

  // Parse out operand information.
  while (!name.empty()) {
    assert(name[0] == ',');
    name = name.drop_front(1);

    pos = name.find(",");
    if (pos == StringRef::npos)
      pos = name.size();

    // Parse out the attribute name.  If it contains a $, then parse out the
    // OperandClass as well.
    auto attrName = name.substr(0, pos);

    // Figure out what the suffix is (if any) and reject invalid suffixes if
    // present.
    auto dollarLoc = attrName.find('$');

    auto opClass = OperandClass::Normal;
    if (dollarLoc != StringRef::npos) {
      auto suffix = attrName.drop_front(dollarLoc + 1);
      if (auto res = SILTensorOpInfo::getOperandClass(suffix))
        opClass = res.getValue();
      else {
        return "invalid attribute modifier '" + attrName.str() + "'";
      }
    }

    // Slice the suffix off the attribute name and add the decoded version.
    operandClasses.push_back({attrName.substr(0, dollarLoc), opClass});
    name = name.substr(pos);
  }

  return "";
}

/// The vast majority of interesting tensor operations are builtin instructions,
/// which come from the user-exposed #tfop() syntax.
bool SILTensorOpInfo::decodeBuiltin() {
  builtinName = inst->getName().str();

  // If the builtin doesn't start with our magic prefix, then it isn't an op.
  if (!builtinName.startswith("__tfop_"))
    return false;

  // This helper emits a diagnostic if the #tfop descriptor is malformed in a
  // way that prevents it from ever working.  Errors that are a result of a
  // client's misuse of the op is checked by checkAttributeConstants, because
  // the location information is far more important to get right there.
  auto diagInvalid = [&](std::string problem) {
    diagnose(inst->getModule().getASTContext(), inst->getLoc().getSourceLoc(),
             diag::tfop_invalid_tfop, problem);
  };

  // Ok, it is, decode and validate it.
  auto errStr = decodeTensorOpName(builtinName.substr(strlen("__tfop_")),
                                   opName, operandClasses);
  if (!errStr.empty()) {
    diagInvalid(errStr);
    return false;
  }

  // Validate that this instruction is ok.
  if (inst->getNumOperands() != operandClasses.size()) {
    diagInvalid("op has " + llvm::utostr(operandClasses.size()) +
                " operand classes, but " +
                llvm::utostr(inst->getNumOperands()) +
                " inputs and attributes");
    return false;
  }

  return true;
}

/// Return the string suffix for the specified attribute modifier.
const char *SILTensorOpInfo::getOperandClassSuffix(OperandClass opClass) {
  switch (opClass) {
  case OperandClass::Input:
    return "$in";
  case OperandClass::InputElt:
    return "$inelt";
  case OperandClass::Normal:
    return "";
  case OperandClass::DType:
    return "$dtype";
  case OperandClass::Tensor:
    return "$tensor";
  case OperandClass::Shape:
    return "$shape";
  case OperandClass::Array:
    return "$array";
  case OperandClass::ArrayElement:
    return "$elt";
  case OperandClass::ShapeArray:
    return "$shapearray";
  }
}

/// Return the operand class of the specified string form like "tensor"
llvm::Optional<OperandClass>
SILTensorOpInfo::getOperandClass(StringRef suffix) {
  return llvm::StringSwitch<llvm::Optional<OperandClass>>(suffix)
      .Case("in", OperandClass::Input)
      .Case("inelt", OperandClass::InputElt)
      .Case("", OperandClass::Normal)
      .Case("tensor", OperandClass::Tensor)
      .Case("shape", OperandClass::Shape)
      .Case("dtype", OperandClass::DType)
      .Case("array", OperandClass::Array)
      .Case("elt", OperandClass::ArrayElement)
      .Case("shapearray", OperandClass::ShapeArray)
      .Default(None);
}

//===----------------------------------------------------------------------===//
// GraphOperationDecoder implementation
//===----------------------------------------------------------------------===//

/// Return the device attribute associated with `inst`, which is required to
/// exist.
StringRef GraphOperationInfo::getDeviceString() const {
  auto attr = inst->getAttributeNamed(DEVICE_ATTR);
  assertWithDump(attr.hasValue(), "Tensor op instruction has no device string");
  return attr.getValue().getStringValue();
}

void GraphOperationInfo::assertWithDump(bool cond,
                                        const char *assertMsg) const {
#ifndef NDEBUG
  if (cond)
    return;
  inst->dump();
  llvm_unreachable(assertMsg);
#endif // NDEBUG
}

/// Decode the name of a graph_op into its TensorFlow op name and a list of
/// information about the operands.
StringRef
GraphOperationInfo::decodeName(SmallVectorImpl<InputMarker> &inputInfo) {
  auto name = inst->getName().str();
  auto pos = name.find(',');
  auto opName = name.substr(0, pos);

  while (pos != StringRef::npos) {
    name = name.drop_front(pos + 1);
    pos = name.find(',');
    auto letter = name.substr(0, pos);
    assertWithDump(letter.size() == 1, "malformed graph_op instruction");
    InputMarker kind;
    switch (letter[0]) {
    case 's':
      kind = InputMarker::IM_Scalar;
      break;
    case 'i':
      kind = InputMarker::IM_Normal;
      break;
    case 'L':
      kind = InputMarker::IM_InputList;
      break;
    case 'e':
      kind = InputMarker::IM_InputListElt;
      break;
    default:
      assertWithDump(false, "malformed graph_op instruction");
    }
    inputInfo.push_back(kind);
  }

  return opName;
}

/// Given an attribute name like foo$dtype, decode the name and the class.  If
/// there is no modifier specified, this defaults to OperandClass::Normal.
std::pair<StringRef, SILTensorOpInfo::OperandClass>
GraphOperationInfo::decodeAttributeName(Identifier name) {
  auto nameStr = name.str();
  // Figure out what the suffix is (if any).
  auto dollarLoc = nameStr.find('$');

  auto opClass = OperandClass::Normal;
  if (dollarLoc != StringRef::npos) {
    auto suffix = nameStr.drop_front(dollarLoc + 1);
    opClass = SILTensorOpInfo::getOperandClass(suffix).getValue();
  }

  // Slice the suffix off the attribute name and add the decoded version.
  return {nameStr.substr(0, dollarLoc), opClass};
}

int64_t GraphOperationInfo::getIntAttr(unsigned attrIdx,
                                       StringRef attrName) const {
  auto attr = inst->getAttribute(attrIdx);
  auto attrInfo = GraphOperationInfo::decodeAttributeName(attr.name);
  assert(attrInfo.first == attrName);
  auto attrValue = attr.value;
  return attrValue.getIntegerValue().getLimitedValue();
}

std::string GraphOperationInfo::getStringAttr(unsigned attrIdx,
                                              StringRef attrName) const {
  auto attr = inst->getAttribute(attrIdx);
  auto attrInfo = GraphOperationInfo::decodeAttributeName(attr.name);
  assert(attrInfo.first == attrName);
  auto attrValue = attr.value;
  return attrValue.getStringValue().str();
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
  if (auto *inst = dyn_cast<SILInstruction>((SILNode *)value))
    return getUserSourceLocation(inst);
  return getUserSourceLocation(value.getDebugLocation());
}

/// Get the user's source location for the specified instruction.  Because it
/// is an instruction, we can apply various heuristics to improve the
/// precision of the returned location information.
SILLocation tf::getUserSourceLocation(SILInstruction *inst) {
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

  SmallVector<GraphOperationAttribute, 8> attributes;

  // Add a dtype attribute for the array element.
  attributes.push_back(
      {context.getIdentifier("dtype"),
       SymbolicValue::getMetatype(elementType->getCanonicalType())});

  // Add an attribute for the value$tensor attribute.
  auto tensorSuffix = SILTensorOpInfo::getOperandClassSuffix(
      SILTensorOpInfo::OperandClass::Tensor);
  attributes.push_back(
      {context.getIdentifier(std::string("value") + tensorSuffix), scalars});

  // Add the value$shape attribute if we have an array value.
  if (scalars.getKind() == SymbolicValue::Array) {
    auto shapeId = SILTensorOpInfo::OperandClass::Shape;
    auto shapeSuffix = SILTensorOpInfo::getOperandClassSuffix(shapeId);
    attributes.push_back(
        {context.getIdentifier(std::string("value") + shapeSuffix), shape});
  }

  // All graph_op's get a device.
  attributes.push_back(
      {context.getIdentifier(DEVICE_ATTR),
       SymbolicValue::getString(getDeviceString(targetDevice), allocator)});

  // Finally build a new graphop instruction with the simplified operands.
  return B.createGraphOperation(loc, context.getIdentifier("Const"),
                                /*operands*/ {}, attributes, resultType);
}

GraphOperationInst *
tf::createTensorToInt1Inst(SILValue value, SILBuilder &builder,
                           SILLocation location,
                           GraphFunctionDeviceInfo &deviceInfo) {
  ASTContext &context = builder.getASTContext();
  SmallVector<GraphOperationAttribute, 1> attributes;
  deviceInfo.handleDevicePlacement(
      "tf_tensor_to_i1",
      /*opDevice*/ getDeviceString(DeviceType::ALL),
      builder.getModule().getASTContext(), attributes);
  GraphOperationInst *condValue = builder.createGraphOperation(
      location, context.getIdentifier("tf_tensor_to_i1"),
      /*operands*/ {value}, attributes,
      {SILType::getBuiltinIntegerType(1, context)});
  assert(condValue->getNumResults() == 1);
  return condValue;
}

//===----------------------------------------------------------------------===//
// TensorFunctionClassifier Implementation
//===----------------------------------------------------------------------===//

/// Return true if the specified function is the top-level context that
/// tensor partitioning should be applied to.  This returns false (for
/// example) for inlined functions that take and return tensors, since we
/// know that they are either unreachable or will be inlined into any
/// clients that use them.
bool TensorFunctionClassifier::shouldBePartitioned(SILFunction *fn) {
  // Ignore transparent functions.
  if (fn->isTransparent())
    return false;

  auto hasInlinableAttrs = [&](Decl *decl) -> bool {
    if (decl->getAttrs().hasAttribute<InlinableAttr>())
      return true;
    return false;
  };

  // Don't transform functions that are marked @_inlineable or inline(always)
  // unless they are a thunk.  Thunks are only generated for inherently
  // interesting declarations.
  if (!fn->isThunk()) {
    if (fn->getInlineStrategy() == AlwaysInline)
      return false;

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

  // If this is a function that was inlined from some other module but only
  // exists so we can see into it, don't transform it.  It won't be a canonical
  // declaration for anything anyway.
  if (isAvailableExternally(fn->getLinkage()))
    return false;

  // Something is creating public thunks around 'shared' implementations, which
  // prevents the above check from working.  Check for public functions.
  // FIXME: This should go away when we get deabstraction.
  if (fn->getLinkage() == SILLinkage::Shared)
    if (auto *dc = fn->getDeclContext())
      if (auto *fd = dyn_cast<FuncDecl>(dc))
        if (fd->getFormalAccess() >= AccessLevel::Public)
          return true;

  // Otherwise, the function is either public and inlininable or it is internal
  // to the current module.  In both cases, we check to see if the function
  // takes TensorHandle values as arguments or results.  If so, then we know
  // that it will be inlined away by deabstraction, and we don't need to touch
  // it.
  if (containsTensorFlowValue(fn->getLoweredFunctionType()))
    return false;

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

bool tf::isShapeArrayPseudoAttr(StringRef attrName, SymbolicValue attrValue) {
  if (attrName != SHAPE_ARRAY_ATTR)
    return false;
  CanType eltType;
  (void)attrValue.getArrayValue(eltType);
  return eltType->getString() == "TensorShape";
}
