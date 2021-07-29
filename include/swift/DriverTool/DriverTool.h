//===--- DriverTool.h - Driver control ----------------------*- C++ -*-===//
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
//
// This file provides a high-level API for interacting with the basic
// driver operation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DRIVERTOOL_H
#define SWIFT_DRIVERTOOL_H

#include "swift/Basic/LLVM.h"

namespace swift {
  int mainEntry(int argc_, const char **argv_);
} // namespace swift

#endif
