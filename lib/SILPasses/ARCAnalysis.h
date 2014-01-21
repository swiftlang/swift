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

namespace swift {

class SILValue;
class SILInstruction;

} // end namespace swift

namespace swift {
namespace arc {

/// Can Inst decrement the ref count of Target?
bool cannotDecrementRefCount(SILInstruction *Inst, SILValue Target);

/// Can Inst use Target in a manner that requires Target to be alive
/// before Inst?
bool cannotUseValue(SILInstruction *Inst, SILValue Target);

} // end namespace arc
} // end namespace swift
