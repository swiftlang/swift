//===--- FlowSensitiveVerification.h --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_VERIFIER_FLOWSENSITIVEVERIFICATION_H
#define SWIFT_SIL_VERIFIER_FLOWSENSITIVEVERIFICATION_H

namespace swift {

class SILFunction;

namespace silverifier {

/// Verify the various control-flow-sensitive rules of SIL:
///
/// - stack allocations and deallocations must obey a stack discipline
/// - accesses must be uniquely ended
/// - async continuations must be awaited before getting the continuation again,
/// suspending
///  the task, or exiting the function
/// - flow-sensitive states must be equivalent on all paths into a block
void verifyFlowSensitiveRules(SILFunction *fn);

} // namespace silverifier
} // namespace swift

#endif
