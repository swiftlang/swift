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

namespace swift {
namespace driver {
  class Action;

  /// Type used for list of Actions.
  typedef llvm::SmallVector<Action *, 3> ActionList;

} // end namespace driver
} // end namespace swift

#endif
