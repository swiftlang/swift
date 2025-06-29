//===--- BasicBridgingImpl.h - header for the swift BasicBridging module --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 - 2025 Apple Inc. and the Swift project authors
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

void ASSERT_inBridgingHeader(bool condition) { ASSERT(condition); }

//===----------------------------------------------------------------------===//
// MARK: BridgedStringRef
//===----------------------------------------------------------------------===//

BridgedStringRef::BridgedStringRef(llvm::StringRef sref)
    : Data(sref.data()), Length(sref.size()) {}

llvm::StringRef BridgedStringRef::unbridged() const {
  return llvm::StringRef(Data, Length);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedOwnedString
//===----------------------------------------------------------------------===//

llvm::StringRef BridgedOwnedString::unbridgedRef() const { return llvm::StringRef(Data, Length); }

//===----------------------------------------------------------------------===//
// MARK: BridgedSourceRange
//===----------------------------------------------------------------------===//

BridgedSourceRange::BridgedSourceRange(swift::SourceRange range)
    : Start(range.Start), End(range.End) {}

swift::SourceRange BridgedSourceRange::unbridged() const {
  return swift::SourceRange(Start, End);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedCharSourceRange
//===----------------------------------------------------------------------===//

BridgedCharSourceRange::BridgedCharSourceRange(swift::CharSourceRange range)
    : Start(range.getStart()), ByteLength(range.getByteLength()) {}

swift::CharSourceRange BridgedCharSourceRange::unbridged() const {
  return swift::CharSourceRange(Start, ByteLength);
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
