//===--- BasicBridging.h - header for the swift BasicBridging module ------===//
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

#ifndef SWIFT_BASIC_BASICBRIDGING_H
#define SWIFT_BASIC_BASICBRIDGING_H

#include "swift/Basic/BridgedSwiftObject.h"
#include "swift/Basic/SourceLoc.h"
#include <stddef.h>

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

typedef intptr_t SwiftInt;
typedef uintptr_t SwiftUInt;

typedef struct {
  const unsigned char * _Nullable data;
  size_t length;
} BridgedStringRef;

typedef struct {
  const void * _Nullable data;
  size_t numElements;
} BridgedArrayRef;

typedef struct {
  void * _Nonnull streamAddr;
} BridgedOStream;

void OStream_write(BridgedOStream os, BridgedStringRef str);

void freeBridgedStringRef(BridgedStringRef str);

//===----------------------------------------------------------------------===//
// Source location
//===----------------------------------------------------------------------===//

typedef struct {
  swift::SourceLoc start;
  SwiftInt byteLength;
} BridgedCharSourceRange;

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
