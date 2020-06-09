//===--- EnvironmentVariables.h - Debug variables. --------------*- C++ -*-===//
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
// Debug behavior conditionally enabled using environment variables.
//
//===----------------------------------------------------------------------===//

#include "../Basic/Lazy.h"

namespace swift {
namespace runtime {
namespace environment {

void initialize(void *);

extern OnceToken_t initializeToken;

// Declare backing variables.
#define VARIABLE(name, type, defaultValue, help) extern type name ## _variable;
#include "../../../stdlib/public/runtime/EnvironmentVariables.def"

// Define getter functions.
#define VARIABLE(name, type, defaultValue, help)        \
  inline type name() {                                  \
    SWIFT_ONCE_F(initializeToken, initialize, nullptr); \
    return name ## _variable;                           \
  }
#include "../../../stdlib/public/runtime/EnvironmentVariables.def"

} // end namespace environment
} // end namespace runtime
} // end namespace Swift
