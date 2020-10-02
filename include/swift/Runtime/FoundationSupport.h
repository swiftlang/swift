//===--- FoundationSupport.cpp - Support functions for Foundation ---------===//
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
// Helper functions for the Foundation framework.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_FOUNDATION_SUPPORT_H
#define SWIFT_RUNTIME_FOUNDATION_SUPPORT_H

#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP
#include <objc/runtime.h>

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

/// Returns a boolean indicating whether the Objective-C name of a class type is
/// stable across executions, i.e., if the class name is safe to serialize. (The
/// names of private and local types are unstable.)
SWIFT_RUNTIME_STDLIB_SPI
bool _swift_isObjCTypeNameSerializable(Class theClass);

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif // SWIFT_OBJC_INTEROP
#endif // SWIFT_RUNTIME_FOUNDATION_SUPPORT_H
