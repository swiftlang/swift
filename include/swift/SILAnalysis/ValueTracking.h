//===-- ValueTracking.h - SIL Value Tracking Analysis ----------*- C++ -*--===//
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

#ifndef SWIFT_SILANALYSIS_VALUETRACKING_H
#define SWIFT_SILANALYSIS_VALUETRACKING_H

namespace swift {

class SILValue;

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
SILValue getUnderlyingObject(SILValue V);

/// Return true if the pointer is to a function-local object that never escapes
/// from the function.
bool isNonEscapingLocalObject(SILValue V);

} // end namespace swift

#endif // SWIFT_SILANALYSIS_VALUETRACKING_H
