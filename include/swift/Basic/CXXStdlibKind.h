//===--- CXXStdlibKind.h ----------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_CXX_STDLIB_KIND_H
#define SWIFT_BASIC_CXX_STDLIB_KIND_H

#include "llvm/Support/ErrorHandling.h"
#include <stdint.h>
#include <string>

namespace swift {

enum class CXXStdlibKind : uint8_t {
  Unknown = 0,

  /// libc++ is the default C++ stdlib on Darwin platforms. It is also supported
  /// on Linux when explicitly requested via `-Xcc -stdlib=libc++` flag.
  Libcxx = 1,

  /// libstdc++ is the default C++ stdlib on most Linux distributions.
  Libstdcxx = 2,

  /// msvcprt is used when targeting Windows.
  Msvcprt = 3,
};

inline std::string to_string(CXXStdlibKind kind) {
  switch (kind) {
  case CXXStdlibKind::Unknown:
    return "unknown C++ stdlib";
  case CXXStdlibKind::Libcxx:
    return "libc++";
  case CXXStdlibKind::Libstdcxx:
    return "libstdc++";
  case CXXStdlibKind::Msvcprt:
    return "msvcprt";
  }
  llvm_unreachable("unhandled CXXStdlibKind");
}

} // namespace swift

#endif // SWIFT_BASIC_CXX_STDLIB_KIND_H
