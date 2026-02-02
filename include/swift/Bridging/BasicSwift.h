//===--- BasicSwift.h -------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BRIDGING_BASICSWIFT_H
#define SWIFT_BRIDGING_BASICSWIFT_H

#include "swift/Basic/BasicBridging.h"

#ifdef __cplusplus
extern "C" {
#endif

/// Create a new static build configuration for the given language options.
void * _Nonnull swift_Basic_createStaticBuildConfiguration(BridgedLangOptions cLangOpts);

/// Free the given static build configuration.
void swift_Basic_freeStaticBuildConfiguration(void * _Nonnull staticBuildConfiguration);

#ifdef __cplusplus
}
#endif

#endif // SWIFT_BRIDGING_BASICSWIFT_H
