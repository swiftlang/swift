//===--- Borrow.cpp - Swift Language Borrow Logic -------------------------===//
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

#include "swift/Runtime/Borrow.h"
#include "swift/Runtime/Metadata.h"

namespace swift {

BorrowRepresentation swift_getBorrowRepresentation(const Metadata *referent) {
  if (!referent->getValueWitnesses()->isBitwiseBorrowable() ||
      referent->getValueWitnesses()->isAddressableForDependencies()) {
    return BorrowRepresentation::Pointer;
  }

  return BorrowRepresentation::Inline;
}

size_t swift_getBorrowSize(const Metadata *referent) {
  auto rep = swift_getBorrowRepresentation(referent);

  switch (rep) {
  case BorrowRepresentation::Inline:
    return referent->vw_size();

  case BorrowRepresentation::Pointer:
    return $sBpWV.getSize();
  }
}

size_t swift_getBorrowStride(const Metadata *referent) {
  auto rep = swift_getBorrowRepresentation(referent);

  switch (rep) {
  case BorrowRepresentation::Inline:
    return referent->vw_stride();

  case BorrowRepresentation::Pointer:
    return $sBpWV.getStride();
  }
}

size_t swift_getBorrowExtraInhabitants(const Metadata *referent) {
  auto rep = swift_getBorrowRepresentation(referent);

  switch (rep) {
  case BorrowRepresentation::Inline:
    return referent->vw_getNumExtraInhabitants();

  case BorrowRepresentation::Pointer:
    return $sBpWV.getNumExtraInhabitants();
  }
}

size_t swift_getBorrowAlignment(const Metadata *referent) {
  auto rep = swift_getBorrowRepresentation(referent);

  switch (rep) {
  case BorrowRepresentation::Inline:
    return referent->vw_alignment();

  case BorrowRepresentation::Pointer:
    return $sBpWV.getAlignment();
  }
}

SWIFT_RUNTIME_EXPORT
void swift_initBorrow(const Metadata *referent, OpaqueValue *destBorrow,
                      OpaqueValue *srcReferent) {
  auto rep = swift_getBorrowRepresentation(referent);

  switch (rep) {
  case BorrowRepresentation::Inline:
    memcpy(destBorrow, srcReferent, referent->vw_size());
    return;

  case BorrowRepresentation::Pointer:
    *reinterpret_cast<OpaqueValue **>(destBorrow) = srcReferent;
    return;
  }
}

SWIFT_RUNTIME_EXPORT
const OpaqueValue *swift_dereferenceBorrow(const Metadata *referent,
                                           const OpaqueValue *borrow) {
  auto rep = swift_getBorrowRepresentation(referent);

  switch (rep) {
  case BorrowRepresentation::Inline:
    return borrow;

  case BorrowRepresentation::Pointer:
    return *reinterpret_cast<const OpaqueValue * const *>(borrow);
  }
}

} // end namespace swift
