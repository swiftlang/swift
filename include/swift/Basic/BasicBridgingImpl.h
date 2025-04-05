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

#include "swift/Basic/Assertions.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/StringRef.h"

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

bool BridgedArrayRef_isEmpty(BridgedArrayRef arr) {
  return arr.Length == 0;
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

BridgedStringRef::BridgedStringRef(llvm::StringRef sref)
    : Data(sref.data()), Length(sref.size()) {}

llvm::StringRef BridgedStringRef::unbridged() const {
  return llvm::StringRef(Data, Length);
}

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

llvm::StringRef BridgedOwnedString::unbridgedRef() const { return llvm::StringRef(Data, Length); }

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

BridgedSourceLoc::BridgedSourceLoc(swift::SourceLoc loc)
  : Raw(loc.getOpaquePointerValue()) {}

swift::SourceLoc BridgedSourceLoc::unbridged() const {
  return swift::SourceLoc(
      llvm::SMLoc::getFromPointer(static_cast<const char *>(Raw)));
}

bool BridgedSourceLoc_isValid(BridgedSourceLoc loc) {
  return loc.getOpaquePointerValue() != nullptr;
}

BridgedSourceLoc BridgedSourceLoc::advancedBy(size_t n) const {
  return BridgedSourceLoc(unbridged().getAdvancedLoc(n));
}

//===----------------------------------------------------------------------===//
// MARK: BridgedSourceRange
//===----------------------------------------------------------------------===//

BridgedSourceRange::BridgedSourceRange(swift::SourceRange range)
    : Start(range.Start), End(range.End) {}

swift::SourceRange BridgedSourceRange::unbridged() const {
  return swift::SourceRange(Start.unbridged(), End.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: BridgedCharSourceRange
//===----------------------------------------------------------------------===//

BridgedCharSourceRange::BridgedCharSourceRange(swift::CharSourceRange range)
    : Start(range.getStart()), ByteLength(range.getByteLength()) {}

swift::CharSourceRange BridgedCharSourceRange::unbridged() const {
  return swift::CharSourceRange(Start.unbridged(), ByteLength);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedSwiftVersion
//===----------------------------------------------------------------------===//

BridgedSwiftVersion::BridgedSwiftVersion(SwiftInt major, SwiftInt minor)
    : Major(major), Minor(minor) {
  ASSERT(major >= 0 && minor >= 0);
  ASSERT(major == Major && minor == Minor);
}

extern "C" void
swift_ASTGen_bridgedSwiftClosureCall_1(const void *_Nonnull closure,
                                       const void *_Nullable arg1);

void BridgedSwiftClosure::operator()(const void *_Nullable arg1) {
#if SWIFT_BUILD_SWIFT_SYNTAX
  swift_ASTGen_bridgedSwiftClosureCall_1(closure, arg1);
#else
  llvm_unreachable("Must not be used in C++-only build");
#endif
}

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_BASIC_BASICBRIDGINGIMPL_H
