//===--- _SwiftConcurrency.h - Swift Concurrency Support --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines types and support functions for the Swift concurrency model.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CONCURRENCY_H
#define SWIFT_CONCURRENCY_H

#ifdef __cplusplus
namespace swift {
extern "C" {
#endif

typedef struct _SwiftContext {
  struct _SwiftContext *parentContext;
} _SwiftContext;

#ifdef __cplusplus
} // extern "C"
} // namespace swift
#endif

#endif // SWIFT_CONCURRENCY_H
