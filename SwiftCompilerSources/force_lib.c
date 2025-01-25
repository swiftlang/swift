//===--- force_lib.c ------------------------------------------------------===//
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

/// Dummy source file to force CMake generated SwiftInTheCompiler.xcodeproj
/// to successfully build static libraries containing only object files used
/// during "bootstrap" process to link Swift sources into the compiler.
