//===--- TargetOptions.h ----------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_TARGETOPTIONS_H
#define SWIFT_BASIC_TARGETOPTIONS_H

namespace swift {

/// Options for controlling the target.
class TargetOptions {
public:
  /// The name of the target triple to compile for.
  std::string Triple;
};

} // end namespace swift

#endif
