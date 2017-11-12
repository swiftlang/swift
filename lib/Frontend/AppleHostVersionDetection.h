//===--- AppleHostVersionDetection.h - NSProcessInfo interface --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_APPLEHOSTVERSIONDETECTION_H
#define SWIFT_FRONTEND_APPLEHOSTVERSIONDETECTION_H

#include "clang/Basic/VersionTuple.h"

namespace swift {

/// Returns a string in a form suitable for llvm::Triple's OS component
/// representing the current host OS.
///
/// Returns an empty version if the host OS version cannot be detected.
///
/// Note that this will load additional code into the process to detect the
/// OS version properly.
clang::VersionTuple inferAppleHostOSVersion();

} // end namespace swift

#endif
