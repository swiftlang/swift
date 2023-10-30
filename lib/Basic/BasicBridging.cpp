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

#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, bridging functions are not inlined and therefore
// inluded in the cpp file.
#include "swift/Basic/BasicBridgingImpl.h"
#endif

using namespace swift;

//===----------------------------------------------------------------------===//
//                                BridgedStringRef
//===----------------------------------------------------------------------===//

void BridgedStringRef::write(BridgedOStream os) const {
  static_cast<raw_ostream *>(os.streamAddr)->write(Data, Length);
}

//===----------------------------------------------------------------------===//
//                                BridgedOwnedString
//===----------------------------------------------------------------------===//

BridgedOwnedString::BridgedOwnedString(const std::string &stringToCopy)
    : Data(nullptr), Length(stringToCopy.size()) {
  if (Length != 0) {
    Data = new char[Length];
    std::memcpy(Data, stringToCopy.data(), Length);
  }
}

void BridgedOwnedString::destroy() const {
  if (Data)
    delete[] Data;
}

