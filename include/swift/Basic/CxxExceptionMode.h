//===--- CxxExceptionMode.h -------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_CXX_EXCEPTION_MODE_H
#define SWIFT_BASIC_CXX_EXCEPTION_MODE_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include <stdint.h>

namespace swift {

/// The exception policy used when importing C++ declarations.
enum class CxxExceptionMode : uint8_t {
  Annotated = 0,
  Strict = 1,
};

inline llvm::StringRef getCxxExceptionModeName(CxxExceptionMode mode) {
  switch (mode) {
  case CxxExceptionMode::Annotated:
    return "annotated";
  case CxxExceptionMode::Strict:
    return "strict";
  }
  llvm_unreachable("unhandled C++ exception mode");
}

} // namespace swift

#endif // SWIFT_BASIC_CXX_EXCEPTION_MODE_H
