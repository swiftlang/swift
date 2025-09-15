//===--- CompatibilityOverrideIncludePath.h -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file This file should be included instead of including
/// COMPATIBILITY_OVERRIDE_INCLUDE_PATH directly to ensure that syntax
/// highlighting in certain errors is not broken. It is assumed that
/// CompatibilityOverride.h is already included.
///
//===----------------------------------------------------------------------===//

#ifndef COMPATIBILITY_OVERRIDE_H
#error "Must define COMPATIBILITY_OVERRIDE_H before including this file"
#endif

#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
