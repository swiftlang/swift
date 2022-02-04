//===--- LibSwiftScan.h - C API for Swift Dependency Scanning ---*- C ---*-===//
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
//
// This C API is primarily intended to serve as:
// - Swift Driver's dependency scanning facility
//   (https://github.com/apple/swift-driver).
// - An object-file scanning facility for extracting of Swift type information
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_C_SWIFT_SCAN_H
#define SWIFT_C_SWIFT_SCAN_H

/// The version constants for the SwiftDependencyScan C API.
/// SWIFTSCAN_VERSION_MINOR should increase when there are API additions.
/// SWIFTSCAN_VERSION_MAJOR is intended for "major" source/ABI breaking changes.
#define SWIFTSCAN_VERSION_MAJOR 0
#define SWIFTSCAN_VERSION_MINOR 3

#include "DependencyScan.h"
#include "BinaryScan.h"

#endif // SWIFT_C_SWIFT_SCAN_H
