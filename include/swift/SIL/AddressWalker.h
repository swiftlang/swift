//===--- AddressWalker.h --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This header defines a walker for SIL addresses that is guaranteed by the
/// language to be able to traverse the SIL from an address def to all of its
/// transitive uses. This is validated by the SIL optimizer.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_ADDRESSWALKER_H
#define SWIFT_SIL_ADDRESSWALKER_H

#include "swift/SIL/AddressUseKind.h"
#include "swift/SIL/SILValue.h"

namespace swift {

/// A state structure for findTransitiveUsesForAddress. Intended to be only used
/// a single time. Please always use a new one for every call to
/// findTransitiveUsesForAddress.
///
/// Validated by the SIL verifier as always being able to visit all addresses
/// derived from alloc_stack, ref_element_addr, project_box, ref_tail_addr and
/// all other address roots.
class TransitiveAddressWalker {
  /// Whether we could tell if this address use didn't escape, did have a
  /// pointer escape, or unknown if we failed to understand something.
  AddressUseKind result = AddressUseKind::NonEscaping;

  unsigned didInvalidate = false;

public:
  virtual ~TransitiveAddressWalker() {}

protected:
  /// Customization point for visiting uses. Returns true if we should continue
  /// visiting.
  ///
  /// NOTE: Do not call this directly from within
  /// findTransitiveUsesForAddress. Please call callVisitUse. This is intended
  /// just for subclasses to override.
  virtual bool visitUse(Operand *use) { return true; }

  virtual void onError(Operand *use) {}

  void meet(AddressUseKind other) {
    assert(!didInvalidate);
    result = swift::meet(result, other);
  }

private:
  /// Shim that actually calls visitUse and changes early exit.
  void callVisitUse(Operand *use) {
    assert(!didInvalidate);
    if (!visitUse(use))
      result = AddressUseKind::Unknown;
  }

public:
  AddressUseKind walk(SILValue address) &&;
};

/// The algorithm that is used to determine what the verifier will consider to
/// be transitive uses of the given address. Used to implement \see
/// findTransitiveUses.
///
/// Returns \p AccessUseKind::Unknown on error.
AddressUseKind findTransitiveUsesForAddress(SILValue address,
                                            TransitiveAddressWalker &visitor);

} // namespace swift

#endif
