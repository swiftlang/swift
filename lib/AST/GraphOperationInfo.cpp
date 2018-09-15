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
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILInstruction.h"

using llvm::SmallVectorImpl;
using llvm::StringRef;
using namespace swift;
using namespace tf;
typedef GraphOperationInfo::OperandClass OperandClass;

/// Return the string suffix for the specified attribute modifier.
const char *GraphOperationInfo::getOperandClassSuffix(OperandClass opClass) {
  switch (opClass) {
  case OperandClass::Input:
    return "$in";
  case OperandClass::Normal:
    return "";
  case OperandClass::Tensor:
    return "$tensor";
  case OperandClass::Shape:
    return "$shape";
  case OperandClass::UnknownShapeList:
    return "$unknownShapeList";
  case OperandClass::Array:
    return "$array";
  case OperandClass::Out:
    return "$out";
  }
}

/// Return the operand class of the specified string form like "tensor"
llvm::Optional<OperandClass>
GraphOperationInfo::getOperandClass(StringRef suffix) {
  return llvm::StringSwitch<llvm::Optional<OperandClass>>(suffix)
      .Case("in", OperandClass::Input)
      .Case("", OperandClass::Normal)
      .Case("tensor", OperandClass::Tensor)
      .Case("shape", OperandClass::Shape)
      .Case("unknownShapeList", OperandClass::UnknownShapeList)
      .Case("array", OperandClass::Array)
      .Case("out", OperandClass::Out)
      .Default(None);
}

/// Return the device attribute associated with `inst`, which is required to
/// exist.
// StringRef GraphOperationInfo::getDeviceString() const {
//   auto attr = inst->getAttributeNamed(DEVICE_ATTR);
//   assertWithDump(attr.hasValue(), "Tensor op instruction has no device
//   string"); return attr.getValue().getStringValue();
// }

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

/// Given an attribute name like foo$tensor, decode the name and the class.  If
/// there is no modifier specified, this defaults to OperandClass::Normal.
std::pair<StringRef, GraphOperationInfo::OperandClass>
GraphOperationInfo::decodeAttributeName(Identifier name) {
  auto nameStr = name.str();
  // Figure out what the suffix is (if any).
  auto dollarLoc = nameStr.find('$');

  auto opClass = OperandClass::Normal;
  if (dollarLoc != StringRef::npos) {
    auto suffix = nameStr.drop_front(dollarLoc + 1);
    if (auto res = getOperandClass(suffix))
      opClass = res.getValue();
    else {
      std::string msg = "invalid attribute modifier '" + name.str().str() + "'";
      llvm_unreachable(msg.c_str());
    }
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
