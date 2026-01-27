//===--- Borrow.h - Swift Language Borrow Logic ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_BORROW_H
#define SWIFT_RUNTIME_BORROW_H

#include "swift/ABI/Metadata.h"

#include <cstdint>

namespace swift {

enum BorrowRepresentation {
  Inline,
  Pointer
};

BorrowRepresentation swift_getBorrowRepresentation(const Metadata *referent);

size_t swift_getBorrowSize(const Metadata *referent);
size_t swift_getBorrowStride(const Metadata *referent);
size_t swift_getBorrowExtraInhabitants(const Metadata *referent);
size_t swift_getBorrowAlignment(const Metadata *referent);

SWIFT_RUNTIME_EXPORT
void swift_initBorrow(const Metadata *referent, OpaqueValue *destBorrow,
                      OpaqueValue *srcReferent);

SWIFT_RUNTIME_EXPORT
const OpaqueValue *swift_dereferenceBorrow(const Metadata *referent,
                                           const OpaqueValue *borrow);

} // end namespace swift

#endif // #ifndef SWIFT_RUNTIME_BORROW_H
