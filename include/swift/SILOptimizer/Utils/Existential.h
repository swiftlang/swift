//===--- Existential.h - Existential related Analyses. -------*- C++ //-*-===//
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

#ifndef SWIFT_SILOPTIMIZER_UTILS_EXISTENTIAL_H
#define SWIFT_SILOPTIMIZER_UTILS_EXISTENTIAL_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

/// Find init_existential from global_addr, if any.
SILValue findInitExistentialFromGlobalAddrAndCopyAddr(GlobalAddrInst *GAI,
                                                  CopyAddrInst *CAI);
SILValue findInitExistentialFromGlobalAddrAndApply(GlobalAddrInst *GAI,
                                                  ApplySite AI, int ArgIdx);

/// Returns the address of an object with which the stack location \p ASI is
/// initialized. This is either a init_existential_addr or the destination of a
/// copy_addr. Returns a null value if the address does not dominate the
/// alloc_stack user \p ASIUser.
/// If the value is copied from another stack location, \p isCopied is set to
/// true.
SILValue getAddressOfStackInit(AllocStackInst *ASI, SILInstruction *ASIUser,
                               bool &isCopied);

/// Find the init_existential, which could be used to determine a concrete
/// type of the \p Self.
/// If the value is copied from another stack location, \p isCopied is set to
/// true.
SILInstruction *findInitExistential(FullApplySite AI, SILValue Self,
                                    ArchetypeType *&OpenedArchetype,
                                    SILValue &OpenedArchetypeDef,
                                    bool &isCopied);
} // end namespace swift

#endif
