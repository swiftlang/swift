//===--- Reflection.h - Types for access to reflection metadata. ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_REFLECTION_H
#define SWIFT_STDLIB_SHIMS_REFLECTION_H

#include "SwiftStdbool.h"
#include "SwiftStdint.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*NameFreeFunc)(const char*);

typedef struct _FieldReflectionMetadata {
  const char* name;
  NameFreeFunc freeFunc;
  __swift_bool isStrong;
  __swift_bool isVar;
} _FieldReflectionMetadata;

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_STDLIB_SHIMS_REFLECTION_H
