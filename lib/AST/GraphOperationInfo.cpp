//===--- GraphOperationInfo.cpp - GraphOperationInst Parse Logic ----------===//
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

#include "swift/SIL/GraphOperationInfo.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILInstruction.h"

using llvm::SmallVectorImpl;
using llvm::StringRef;
using namespace swift;
using namespace tf;

GraphOperationInfo::GraphOperationInfo(GraphOperationInst *inst) : inst(inst) {
  PrettyStackTraceSILNode X("decoding graph_op name", inst);

  ArrayRef<Operand> remainingOperands = inst->getAllOperands();
  StringRef remainingMangled = inst->getName().str();
  auto nextMarkerPos = remainingMangled.find(',');
  OperationName = remainingMangled.substr(0, nextMarkerPos);

  while (nextMarkerPos != StringRef::npos) {
    remainingMangled = remainingMangled.drop_front(nextMarkerPos);
    nextMarkerPos = remainingMangled.find(',', 1);

    StringRef thisMarker = remainingMangled.substr(0, nextMarkerPos);
    StringRef thisMarkerName = thisMarker.drop_front(2);
    assert(thisMarker.size() >= 2 && "marker too short");
    switch (thisMarker[1]) {
    case 'i':
      // Push a SAK_Single.
      StructuredArguments.emplace_back(SAK_Single, thisMarkerName,
                                      remainingOperands.front().get());
      remainingOperands = remainingOperands.drop_front(1);
      break;
    case 'L':
      // Push a SAK_List with ArgumentList of size 0 pointing at the right place
      // in the inst's arguments.
      StructuredArguments.emplace_back(SAK_List, thisMarkerName,
                                      remainingOperands.take_front(0));
      break;
    case 'e':
      // Extend the ArgumentList of the curent SAK_List by 1 to include the next
      // of the inst's arguments.
      assert(StructuredArguments.size() > 0 && "list element not in list");
      assert(StructuredArguments.back().Kind == SAK_List &&
             "list element not in list");
      assert(thisMarkerName.empty() && "list element should not have name");
      StructuredArguments.back().ArgumentList = ArrayRef<Operand>(
          StructuredArguments.back().ArgumentList.data(),
          StructuredArguments.back().ArgumentList.size() + 1);
      remainingOperands = remainingOperands.drop_front(1);
      break;
    default:
      llvm_unreachable("unknown marker kind");
    }
  }
}

int64_t GraphOperationInfo::getIntAttr(unsigned attrIdx,
                                       StringRef attrName) const {
  auto attr = inst->getAttribute(attrIdx);
  auto attrInfo = GraphOperationInfo::decodeArgumentName(attr.name.str());
  assert(attrInfo && "attribute has malformed name");
  assert(attrInfo->first == attrName);
  auto attrValue = attr.value;
  return attrValue.getIntegerValue().getLimitedValue();
}

std::string GraphOperationInfo::getStringAttr(unsigned attrIdx,
                                              StringRef attrName) const {
  auto attr = inst->getAttribute(attrIdx);
  auto attrInfo = GraphOperationInfo::decodeArgumentName(attr.name.str());
  assert(attrInfo && "attribute has malformed name");
  assert(attrInfo->first == attrName);
  auto attrValue = attr.value;
  return attrValue.getStringValue().str();
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

/// Return the string suffix for the specified ArgumentLowering.
const char *
GraphOperationInfo::getArgumentLoweringSuffix(ArgumentLowering lowering) {
  switch (lowering) {
  case ArgumentLowering::Input:
    return "";
  case ArgumentLowering::NormalAttribute:
    return "";
  case ArgumentLowering::TensorAttribute:
    return "$tensor";
  case ArgumentLowering::ShapeAttribute:
    return "$shape";
  case ArgumentLowering::TFDataTypeAttribute:
    return "$dtype";
  case ArgumentLowering::Out:
    return "$out";
  }
}

/// Given an argument name like foo$tensor, decode the name and the
/// ArgumentLowering.  If the name is empty, this defaults to
/// ArgumentLowering::Input.  If the name is non-empty but there is no
/// modifier specified, then this defaults to
/// ArgumentLowering::NormalAttribute.  If the modifier is invalid, returns
/// None (e.g  "value$bla").
/// TODO(SR-9250): Most callers should not have to deal with the Optional.
llvm::Optional<std::pair<StringRef, GraphOperationInfo::ArgumentLowering>>
GraphOperationInfo::decodeArgumentName(StringRef Name) {
  if (Name.empty())
    return {{Name, ArgumentLowering::Input}};

  auto dollarLoc = Name.find('$');
  auto lowering = ArgumentLowering::NormalAttribute;
  if (dollarLoc != StringRef::npos) {
    auto suffix = Name.drop_front(dollarLoc + 1);
    auto loweringOpt =
        llvm::StringSwitch<llvm::Optional<ArgumentLowering>>(suffix)
          .Case("", ArgumentLowering::NormalAttribute)
          .Case("tensor", ArgumentLowering::TensorAttribute)
          .Case("shape", ArgumentLowering::ShapeAttribute)
          .Case("dtype", ArgumentLowering::TFDataTypeAttribute)
          .Case("out", ArgumentLowering::Out)
          .Default(None);
    if (!loweringOpt)
      return None;
    lowering = *loweringOpt;
  }
  return {{Name.substr(0, dollarLoc), lowering}};
}

/// Returns this argument's name, without suffix, and the ArgumentLowering.
std::pair<StringRef, GraphOperationInfo::ArgumentLowering>
GraphOperationInfo::StructuredArgument::getArgumentNameAndLowering() const {
  auto decoded = decodeArgumentName(Name);
  assert(decoded && "argument has malformed name");
  return *decoded;
}

/// Return true if the specified type is TensorHandle<T>, ResourceHandle, or
/// VariantHandle.
bool tf::isTensorFlowValue(SILType ty) {
  return (bool)isTensorFlowValue(ty.getASTType());
}

/// Determine whether the specified type is one of our well-known types, and
/// if so, which one it is.
TFValueKind tf::classifyTensorFlowValue(SILType ty) {
  return classifyTensorFlowValue(ty.getASTType());
}

bool tf::isShapeArrayPseudoAttr(StringRef attrName, SymbolicValue attrValue) {
  if (attrName != TF_SHAPE_ARRAY_ATTR)
    return false;
  CanType eltType;
  (void)attrValue.getArrayValue(eltType);
  return eltType->getString() == "TensorShape";
}

int tf::decodeShapeAttr(const ASTContext &ctx, SymbolicValue attr,
                        SmallVectorImpl<int64_t> &result) {
  // Handle "nil as Optional<TensorShape>" unknown rank case.
  if (attr.getKind() == SymbolicValue::Kind::Enum &&
      attr.getEnumValue() == ctx.getOptionalNoneDecl()) {
    return -1;
  }

  // Extract value from Optional<TensorShape>.
  if (attr.getKind() == SymbolicValue::Kind::EnumWithPayload) {
    attr = attr.getEnumPayloadValue();
  }

  attr = attr.lookThroughSingleElementAggregates();

  CanType eltType;
  auto arrayValue = attr.getArrayValue(eltType);
  for (auto elt : arrayValue) {
    elt = elt.lookThroughSingleElementAggregates();
    result.push_back(elt.getIntegerValue().sextOrTrunc(64).getLimitedValue());
  }
  return arrayValue.size();
}

/// Decode the shape array in `attrValue` into `dims`, `numDims` and `dimPtrs`.
void tf::decodeShapeArray(const ASTContext &ctx, SymbolicValue attrValue,
                          SmallVectorImpl<int64_t> &dims,
                          SmallVectorImpl<int> &numDims,
                          SmallVectorImpl<int64_t *> &dimPtrs) {
  CanType eltType;
  auto shapeArray = attrValue.getArrayValue(eltType);
  assert(eltType->getString() == "TensorShape" ||
         eltType->getString() == "Optional<TensorShape>");
  auto numShapes = shapeArray.size();
  for (unsigned shapeIdx = 0; shapeIdx != numShapes; ++shapeIdx) {
    auto shape = shapeArray[shapeIdx];
    numDims.push_back(decodeShapeAttr(ctx, shape, dims));
  }

  // Now that we've build the array of dimensions, convert it to the array
  // of pointers that TensorFlow needs.  This is safe now that the vector
  // has finished its resizing.
  auto dimPtr = dims.data();
  for (unsigned shapeIdx = 0; shapeIdx != numShapes; ++shapeIdx) {
    dimPtrs.push_back(dimPtr);

    // Make sure to handle the "unknown rank" case (numDims[shapeIdx] == -1) by
    // without incrementing the pointer.
    if (numDims[shapeIdx] >= 0)
      dimPtr += numDims[shapeIdx];
  }
}

/// Return the TF_DataType value represented by `value`. `value` must be a
/// valid tensorflow type ID.
unsigned tf::getTFDataType(SymbolicValue value) {
  value = value.lookThroughSingleElementAggregates();
  assert(value.getKind() == SymbolicValue::Integer);
  assert(value.getIntegerValue().isIntN(32));
  unsigned tfType = value.getIntegerValue().getLimitedValue();
  assert(tfType > 0 && "0 is invalid TF_DataType");
  return tfType;
}

/// Return a constant integer representing the TF_DataType value for the given
/// Swift type. `type` must be a valid TensorFlow type.
SymbolicValue tf::convertSwiftTypeToConstantTFDataType(Type type) {
  unsigned tfType = convertSwiftTypeToTF(type);
  assert(tfType != 0);
  return SymbolicValue::getInteger(tfType, 32);
}

/// Return the graph function name for a SIL function that is being used as a
/// function attribute. This transformation may modify the name to make it
/// conform to graph function name rules.
std::string tf::getGraphFuncNameForFuncAttr(StringRef silFnName) {
  if (silFnName.startswith("$"))
    silFnName = silFnName.substr(1);
  return std::string(silFnName) + ".tf_only";
}
