//===--- Version.cpp - Swift Version Number -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines several version-related utility functions for Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Version.h"

#include "swift/Basic/LLVM.h"
#include "llvm/Support/raw_ostream.h"

#define TOSTR2(X) #X
#define TOSTR(X) TOSTR2(X)

#ifdef SWIFT_VERSION_PATCHLEVEL
/// Helper macro for SWIFT_VERSION_STRING.
#define SWIFT_MAKE_VERSION_STRING(X, Y, Z) TOSTR(X) "." TOSTR(Y) "." TOSTR(Z)

/// A string that describes the Swift version number, e.g., "1.0".
#define SWIFT_VERSION_STRING                                                   \
  SWIFT_MAKE_VERSION_STRING(SWIFT_VERSION_MAJOR, SWIFT_VERSION_MINOR,          \
                            SWIFT_VERSION_PATCHLEVEL)
#else
/// Helper macro for SWIFT_VERSION_STRING.
#define SWIFT_MAKE_VERSION_STRING(X, Y) TOSTR(X) "." TOSTR(Y)

/// A string that describes the Swift version number, e.g., "1.0".
#define SWIFT_VERSION_STRING                                                   \
  SWIFT_MAKE_VERSION_STRING(SWIFT_VERSION_MAJOR, SWIFT_VERSION_MINOR)
#endif

namespace swift {
namespace version {

std::pair<unsigned, unsigned> getSwiftNumericVersion() {
  return { SWIFT_VERSION_MAJOR, SWIFT_VERSION_MINOR };
}

std::string getSwiftFullVersion() {
  std::string buf;
  llvm::raw_string_ostream OS(buf);
#ifdef SWIFT_VENDOR
  OS << SWIFT_VENDOR " ";
#endif
  OS << "Swift version " SWIFT_VERSION_STRING;
#ifdef SWIFT_REPOSITORY_STRING
  OS << " (" SWIFT_REPOSITORY_STRING ")";
#endif
  return OS.str();
}

} // end namespace version
} // end namespace swift

