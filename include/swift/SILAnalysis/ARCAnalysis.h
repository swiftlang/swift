//===--------------- ARCAnalysis.h - SIL ARC Analysis ----*- C++ -*--------===//
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

#ifndef SWIFT_SILANALYSIS_ARCANALYSIS_H
#define SWIFT_SILANALYSIS_ARCANALYSIS_H

namespace swift {

class SILValue;
class SILInstruction;
class AliasAnalysis;

} // end namespace swift

namespace swift {
namespace arc {

/// \returns True if the user \p User decrement the ref count of pointer \p Ptr.
bool canDecrementRefCount(SILInstruction *User, SILValue Ptr,AliasAnalysis *AA);

/// Can Inst use Target in a manner that requires Target to be alive
/// before Inst?
bool cannotUseValue(SILInstruction *Inst, SILValue Target,
                    AliasAnalysis *AA);

} // end namespace arc
} // end namespace swift

#endif
