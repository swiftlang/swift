//===--- Util.h - Common Driver Utilities -----------------------*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_UTIL_H
#define SWIFT_DRIVER_UTIL_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

namespace driver {
  class Action;

  /// Type used for list of Actions.
  typedef SmallVector<Action *, 3> ActionList;

  enum class LinkKind {
    None,
    Executable,
    DynamicLibrary
  };

  /// Searches for an executable relative to the compiler itself.
  ///
  /// This first looks next to the compiler binary, then checks to see if the
  /// compiler is in an Xcode toolchain and looks in the bin directory outside
  /// the toolchain.
  ///
  /// \returns The path to the executable being searched for, or an empty string
  /// if it cannot be found.
  std::string findRelativeExecutable(StringRef compilerPath,
                                     StringRef executableName);

} // end namespace driver
} // end namespace swift

#endif
