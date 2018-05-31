//===--- AutoDiff.cpp - Runtime support for autodiff ------------*- c++ -*-===//
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
// SWIFT_ENABLE_TENSORFLOW
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/AutoDiff.h"

using namespace swift;

SWIFT_RUNTIME_EXPORT
OpaqueValue *swift_autodiffCreateTape(Metadata *type) {
  auto tape = new AutoDiffTape(type);
  return reinterpret_cast<OpaqueValue *>(tape);
}

SWIFT_RUNTIME_EXPORT
void swift_autodiffDestroyTape(OpaqueValue *tape) {
  auto tapePtr = reinterpret_cast<AutoDiffTape *>(tape);
  delete tapePtr;
}

SWIFT_RUNTIME_EXPORT
void swift_autodiffPushToTape(OpaqueValue *tape, OpaqueValue *value) {
  auto tapePtr = reinterpret_cast<AutoDiffTape *>(tape);
  tapePtr->elements.push_back(value);
}

SWIFT_RUNTIME_EXPORT
OpaqueValue *swift_autodiffPopFromTape(OpaqueValue *tape) {
  auto tapePtr = reinterpret_cast<AutoDiffTape *>(tape);
  auto value = tapePtr->elements.back();
  tapePtr->elements.pop_back();
  return value;
}
