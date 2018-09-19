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
StringRef GraphOperationInfo::decodeName(
    SmallVectorImpl<StructuredOperand> &structuredOperands) const {
  PrettyStackTraceSILNode X("decoding graph_op name", inst);

  ArrayRef<Operand> remainingOperands = inst->getAllOperands();
  StringRef remainingMangled = inst->getName().str();
  auto nextMarkerPos = remainingMangled.find(',');
  StringRef opName = remainingMangled.substr(0, nextMarkerPos);

  while (nextMarkerPos != StringRef::npos) {
    remainingMangled = remainingMangled.drop_front(nextMarkerPos);
    nextMarkerPos = remainingMangled.find(',', 1);

    StringRef thisMarker = remainingMangled.substr(0, nextMarkerPos);
    StringRef thisMarkerName = thisMarker.drop_front(2);
    assert(thisMarker.size() >= 2 && "marker too short");
    switch (thisMarker[1]) {
    case 's':
      // Push a SOK_Scalar.
      assert(thisMarkerName.empty() && "SOK_Scalar should not have name");
      structuredOperands.push_back({SOK_Scalar, StringRef(),
                                    remainingOperands.front().get()});
      remainingOperands = remainingOperands.drop_front(1);
      break;
    case 'i':
      // Push a SOK_Single.
      structuredOperands.push_back({SOK_Single, thisMarkerName,
                                    remainingOperands.front().get()});
      remainingOperands = remainingOperands.drop_front(1);
      break;
    case 'L':
      // Push a SOK_List with OperandList of size 0 pointing at the right place
      // in the inst's operands.
      structuredOperands.push_back({SOK_List, thisMarkerName,
                                    remainingOperands.take_front(0)});
      break;
    case 'e':
      // Extend the OperandList of the curent SOK_List by 1 to include the next
      // of the inst's operands.
      assert(structuredOperands.size() > 0 && "list element not in list");
      assert(structuredOperands.back().Kind == SOK_List &&
             "list element not in list");
      assert(thisMarkerName.empty() && "list element should not have name");
      structuredOperands.back().OperandList = ArrayRef<Operand>(
          structuredOperands.back().OperandList.data(),
          structuredOperands.back().OperandList.size() + 1);
      remainingOperands = remainingOperands.drop_front(1);
      break;
    default:
      llvm_unreachable("unknown marker kind");
    }
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
