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

#include "llvm/ADT/SmallVector.h"

namespace llvm {
  class Triple;
}

namespace swift {

  /// Returns the platform name for a given target triple.
  ///
  /// For example, the iOS simulator has the name "iphonesimulator", while real
  /// iOS uses "iphoneos". OS X is "macosx". (These names are intended to be
  /// compatible with Xcode's SDKs.)
  ///
  /// If the triple does not correspond to a known platform, the empty string is
  /// returned.
  StringRef getPlatformNameForTriple(const llvm::Triple &triple);

namespace driver {
  class Action;

  /// Type used for list of Actions.
  typedef llvm::SmallVector<Action *, 3> ActionList;

} // end namespace driver
} // end namespace swift

#endif
