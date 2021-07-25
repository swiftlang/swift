//===--- CompatibilityConcurrency.cpp -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Allow this library to get force-loaded by autolinking
__attribute__((weak, visibility("hidden"))) extern "C" char
    _swift_FORCE_LOAD_$_swiftCompatibilityConcurrency = 0;
