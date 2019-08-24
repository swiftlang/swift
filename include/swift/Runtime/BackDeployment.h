//===--- BackDeployment.h - Support for running on older OS versions. -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_BACKDEPLOYMENT_H
#define SWIFT_STDLIB_BACKDEPLOYMENT_H

#if defined(__APPLE__) && defined(__MACH__)

#include "swift/Runtime/Config.h"
#include "../../../stdlib/public/SwiftShims/Visibility.h"

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

#if SWIFT_CLASS_IS_SWIFT_MASK_GLOBAL_VARIABLE
# ifndef __cplusplus
// This file gets included from some C/ObjC files and
// SWIFT_RUNTIME_STDLIB_SPI doesn't imply extern in C.
extern
# endif
SWIFT_RUNTIME_STDLIB_SPI unsigned long long _swift_classIsSwiftMask;
#endif

/// Returns true if the current OS version, at runtime, is a back-deployment
/// version.
SWIFT_RUNTIME_STDLIB_INTERNAL
int _swift_isBackDeploying();

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif // defined(__APPLE__) && defined(__MACH__)

#endif // SWIFT_STDLIB_BACKDEPLOYMENT_H
