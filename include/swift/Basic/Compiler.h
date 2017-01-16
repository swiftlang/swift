//===--- Compiler.h - Compiler specific definitions -------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_COMPILER_H
#define SWIFT_BASIC_COMPILER_H

#if defined(_MSC_VER) && !defined(__clang__)
#define COMPILER_IS_MSVC 1
#else
#define COMPILER_IS_MSVC 0
#endif

#endif // SWIFT_BASIC_COMPILER_H
