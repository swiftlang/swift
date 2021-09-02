//===--- SwiftTLSContext.h ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_SWIFTTLSCONTEXT_H
#define SWIFT_RUNTIME_SWIFTTLSCONTEXT_H

#include "ExclusivityPrivate.h"

namespace swift {
namespace runtime {

class SwiftTLSContext {
public:
  /// The set of tracked accesses.
  AccessSet accessSet;

  // The "implicit" boolean parameter which is passed to a dynamically
  // replaceable function.
  // If true, the original function should be executed instead of the
  // replacement function.
  bool CallOriginalOfReplacedFunction = false;

  static SwiftTLSContext &get();
};

} // namespace runtime
} // namespace swift

#endif
