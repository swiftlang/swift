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

#include "swift/Basic/Assertions.h"
#include "swift/Basic/BasicBridging.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, bridging functions are not inlined and therefore
// inluded in the cpp file.
#include "swift/Basic/BasicBridgingImpl.h"
#endif

using namespace swift;

void assertFail(const char * _Nonnull msg, const char * _Nonnull file,
                SwiftUInt line, const char * _Nonnull function) {
  ASSERT_failure(msg, file, line, function);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedOStream
//===----------------------------------------------------------------------===//

void BridgedOStream::write(BridgedStringRef string) const {
  *os << string.unbridged();
}

void BridgedOStream::newLine() const {
  os->write('\n');
}

void BridgedOStream::flush() const {
  os->flush();
}

BridgedOStream Bridged_dbgs() {
  return BridgedOStream(&llvm::dbgs());
}

//===----------------------------------------------------------------------===//
// MARK: BridgedStringRef
//===----------------------------------------------------------------------===//

void BridgedStringRef::write(BridgedOStream os) const {
  os.unbridged()->write(Data, Length);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedOwnedString
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

//===----------------------------------------------------------------------===//
// MARK: Data
//===----------------------------------------------------------------------===//

void BridgedData_free(BridgedData data) {
  if (data.BaseAddress == nullptr)
    return;
  free(const_cast<char *>(data.BaseAddress));
}

//===----------------------------------------------------------------------===//
// MARK: BridgedCharSourceRangeVector
//===----------------------------------------------------------------------===//

BridgedCharSourceRangeVector::BridgedCharSourceRangeVector()
    : vector(new std::vector<CharSourceRange>()) {}

void BridgedCharSourceRangeVector::push_back(BridgedCharSourceRange range) {
  static_cast<std::vector<CharSourceRange> *>(vector)->push_back(
      range.unbridged());
}
