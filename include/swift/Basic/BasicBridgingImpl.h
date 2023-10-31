//===--- BasicBridgingImpl.h - header for the swift BasicBridging module --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_BASICBRIDGINGIMPL_H
#define SWIFT_BASIC_BASICBRIDGINGIMPL_H

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

//===----------------------------------------------------------------------===//
// MARK: BridgedArrayRef
//===----------------------------------------------------------------------===//

const void *_Nullable BridgedArrayRef_data(BridgedArrayRef arr) {
  return arr.Data;
}

SwiftInt BridgedArrayRef_count(BridgedArrayRef arr) {
  return static_cast<SwiftInt>(arr.Length);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedData
//===----------------------------------------------------------------------===//

const char *_Nullable BridgedData_baseAddress(BridgedData data) {
  return data.BaseAddress;
}

SwiftInt BridgedData_count(BridgedData data) {
  return static_cast<SwiftInt>(data.Length);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedStringRef
//===----------------------------------------------------------------------===//

const uint8_t *_Nullable BridgedStringRef_data(BridgedStringRef str) {
  return (const uint8_t *)str.unbridged().data();
}

SwiftInt BridgedStringRef_count(BridgedStringRef str) {
  return (SwiftInt)str.unbridged().size();
}

bool BridgedStringRef_empty(BridgedStringRef str) {
  return str.unbridged().empty();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedOwnedString
//===----------------------------------------------------------------------===//

const uint8_t *_Nullable BridgedOwnedString_data(BridgedOwnedString str) {
  auto *data = str.unbridgedRef().data();
  return (const uint8_t *)(data ? data : "");
}

SwiftInt BridgedOwnedString_count(BridgedOwnedString str) {
  return (SwiftInt)str.unbridgedRef().size();
}

bool BridgedOwnedString_empty(BridgedOwnedString str) {
  return str.unbridgedRef().empty();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedSourceLoc
//===----------------------------------------------------------------------===//

bool BridgedSourceLoc_isValid(BridgedSourceLoc loc) {
  return loc.getOpaquePointerValue() != nullptr;
}

BridgedSourceLoc BridgedSourceLoc::advancedBy(size_t n) const {
  return BridgedSourceLoc(unbridged().getAdvancedLoc(n));
}

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_BASIC_BASICBRIDGINGIMPL_H
