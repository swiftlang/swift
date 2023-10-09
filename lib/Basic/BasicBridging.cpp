//===--- BasicBridging.cpp - Utilities for swift bridging -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/BridgingUtils.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                BridgedStringRef
//===----------------------------------------------------------------------===//

void BridgedStringRef::write(BridgedOStream os) const {
  static_cast<raw_ostream *>(os.streamAddr)->write(data, length);
}

//===----------------------------------------------------------------------===//
//                                BridgedOwnedString
//===----------------------------------------------------------------------===//

BridgedOwnedString::BridgedOwnedString(const std::string &stringToCopy)
  : data(nullptr), length(stringToCopy.size()) {
  if (length != 0) {
    data = new char[length];
    std::memcpy(data, stringToCopy.data(), length);
  }
}

void BridgedOwnedString::destroy() const {
  if (data)
    delete [] data;
}

