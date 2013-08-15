//===--- Local.h - Functions that perform local SIL transformations. -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//

#ifndef SWIFT_SILPASSES_UTILS_LOCAL_H
#define SWIFT_SILPASSES_UTILS_LOCAL_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

  /// \brief If the given instruction is dead, delete it along with its dead
  /// operands.
  ///
  /// \return Returns true if any instructions were deleted.
  bool recursivelyDeleteTriviallyDeadInstructions(SILInstruction *I);

} // end namespace swift

#endif
