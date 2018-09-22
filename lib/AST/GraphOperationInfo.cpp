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

/// Return the string suffix for the specified OperandLowering.
const char *
GraphOperationInfo::getOperandLoweringSuffix(OperandLowering lowering) {
  switch (lowering) {
  case OperandLowering::Input:
    return "";
  case OperandLowering::NormalAttribute:
    return "";
  case OperandLowering::TensorAttribute:
    return "$tensor";
  case OperandLowering::ShapeAttribute:
    return "$shape";
  case OperandLowering::UnknownShapeListAttribute:
    return "$unknownShapeList";
  case OperandLowering::TypeListAttribute:
    return "$typeList";
  case OperandLowering::Out:
    return "$out";
  }
}

/// Given an operand name like foo$tensor, decode the name and the
/// OperandLowering.  If the name is empty, this defaults to
/// OperandLowering::Input.  If the name is non-empty but there is no
/// modifier specified, then this defaults to
/// OperandLowering::NormalAttribute.
std::pair<StringRef, GraphOperationInfo::OperandLowering>
GraphOperationInfo::decodeOperandName(StringRef Name) {
  if (Name.empty())
    return {Name, OperandLowering::Input};

  auto dollarLoc = Name.find('$');
  auto lowering = OperandLowering::NormalAttribute;
  if (dollarLoc != StringRef::npos) {
    auto suffix = Name.drop_front(dollarLoc + 1);
    auto loweringOpt =
        llvm::StringSwitch<llvm::Optional<OperandLowering>>(suffix)
          .Case("", OperandLowering::NormalAttribute)
          .Case("tensor", OperandLowering::TensorAttribute)
          .Case("shape", OperandLowering::ShapeAttribute)
          .Case("unknownShapeList", OperandLowering::UnknownShapeListAttribute)
          .Case("typeList", OperandLowering::TypeListAttribute)
          .Case("out", OperandLowering::Out)
          .Default(None);
    assert(loweringOpt && "invalid attribute modifier");
    lowering = *loweringOpt;
  }
  return {Name.substr(0, dollarLoc), lowering};
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

StringRef GraphOperationInfo::getName() const {
  auto mangled = inst->getName().str();
  return mangled.substr(0, mangled.find(','));
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
    case 'i':
      // Push a SOK_Single.
      structuredOperands.emplace_back(SOK_Single, thisMarkerName,
                                      remainingOperands.front().get());
      remainingOperands = remainingOperands.drop_front(1);
      break;
    case 'L':
      // Push a SOK_List with OperandList of size 0 pointing at the right place
      // in the inst's operands.
      structuredOperands.emplace_back(SOK_List, thisMarkerName,
                                      remainingOperands.take_front(0));
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

/// Returns the result of GraphOperationInfo::decodeOperandName on this
/// operand.
std::pair<StringRef, GraphOperationInfo::OperandLowering>
GraphOperationInfo::StructuredOperand::decodeName() const {
  return decodeOperandName(Name);
}

int64_t GraphOperationInfo::getIntAttr(unsigned attrIdx,
                                       StringRef attrName) const {
  auto attr = inst->getAttribute(attrIdx);
  auto attrInfo = GraphOperationInfo::decodeOperandName(attr.name.str());
  assert(attrInfo.first == attrName);
  auto attrValue = attr.value;
  return attrValue.getIntegerValue().getLimitedValue();
}

std::string GraphOperationInfo::getStringAttr(unsigned attrIdx,
                                              StringRef attrName) const {
  auto attr = inst->getAttribute(attrIdx);
  auto attrInfo = GraphOperationInfo::decodeOperandName(attr.name.str());
  assert(attrInfo.first == attrName);
  auto attrValue = attr.value;
  return attrValue.getStringValue().str();
}
