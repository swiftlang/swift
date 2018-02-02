//===--- TensorFlow.cpp - AST Level TensorFlow Support Logic --------------===//
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
// This file implements the AST level TensorFlow support logic that is used
// across the Swift compiler.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/TensorFlow.h"
using namespace swift;
using namespace tf;


/// This decodes the specified string as an operand & result constraint string
/// which looks something like "tt:t".  It fills in the operand/result
/// descriptor strings and decoded forms.
auto TensorOpInfo::
decodeDescriptorString(StringRef operandAndResult) -> ParseErrorInfo {
  if (operandAndResult.empty()) {
    return {
      operandAndResult.data(),
      "empty descriptor is invalid"
    };
  }

  auto colonLoc = operandAndResult.find(':');
  if (colonLoc == StringRef::npos) {
    return {
      operandAndResult.data(),
      "constraint string must have a ':' to indicate results"
    };
  }

  operandDescriptorStr = operandAndResult.take_front(colonLoc);
  resultDescriptorStr = operandAndResult.drop_front(colonLoc+1);

  auto decode = [&](StringRef str,
                    SmallVectorImpl<OpDescriptor> &result) -> ParseErrorInfo {
    result.clear();
    for (auto c : str) {
      OpDescriptor kind;
      switch (c) {
        case 't': kind = OpDescriptor::Tensor; break;
        case 's': kind = OpDescriptor::Scalar; break;
        default:
          return {
            str.data()+str.find(c),
            "unknown #tfop constraint character '" + std::string(1, c) + "'"
          };
      }
      result.push_back(kind);
    }

    return ParseErrorInfo::getSuccess();
  };

  // Decode the operands and the results.
  auto errInfo = decode(operandDescriptorStr, operandDescriptors);
  if (!errInfo.isSuccess())
    return errInfo;

  errInfo = decode(resultDescriptorStr, resultDescriptors);
  return errInfo;
}

