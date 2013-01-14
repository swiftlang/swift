//===--- Initialization.h - Buffer initialization. --------------*- C++ -*-===//
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
//
// A storage structure for representing a buffer or collection of buffers to
// be initialized.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_INITIALIZATION_H
#define SWIFT_LOWERING_INITIALIZATION_H

#include "SILGen.h"
#include "swift/Basic/DiverseList.h"

namespace swift {
namespace Lowering {
  class SILGenFunction;

/// Initialization - Abstract base class for initialization buffers. An
/// initialization represents an uninitialized buffer or a tuple of
/// uninitialized buffers that must be initialized with the result of an
/// expression, such as in a var declaration or return statement. An
/// initialization may also have partial cleanups that should be disabled and
/// replaced when the buffer is initialized; for instance, a freshly allocated
/// box with an uninitialized value must be deallocated with `dealloc_ref`, but
/// once the box is fully initialized, both the box and the contained value can
/// be cleaned up together with `release`.
class Initialization {
public:
  Initialization() {}
  virtual ~Initialization() {}
  
  Initialization(Initialization const &) = delete;
  Initialization &operator=(Initialization const &) = delete;

  /// If this initialization represents a single contiguous buffer, return the
  /// Value of that buffer's address. If not, returns an invalid Value.
  virtual Value getAddressOrNull() = 0;
  
  /// Binds an address value to this initialization. Used for [byref] arguments,
  /// but invalid anywhere else.
  virtual void bindAddress(Value address, SILGenFunction &gen) {
    llvm_unreachable("unexpected address value in initialization!");
  }
  
  /// Returns true if this initialization represents a single contiguous buffer.
  bool hasAddress() { return getAddressOrNull().isValid(); }

  /// Returns the address of the single contiguous buffer represented by this
  /// initialization. Once the address has been stored to,
  /// finishInitialization must be called.
  Value getAddress() {
    Value address = getAddressOrNull();
    assert(address && "initialization does not represent a single buffer");
    return address;
  }

  /// If this initialization represents an aggregation of sub-initializations,
  /// return the sub-initializations. Once all the sub-initializations have been
  /// initialized and finalized with finishInitialization, finishInitialization
  /// must then be called on this aggregate initialization.
  virtual ArrayRef<Initialization*> getSubInitializations() = 0;
  
  /// Perform post-initialization bookkeeping for this initialization.
  virtual void finishInitialization(SILGenFunction &gen) {}

  /// Perform a zero initialization of this buffer or buffers, then
  /// invoke finishInitialization() if necessary.
  virtual void zeroInitialize(SILGenFunction &gen) = 0;
  
private:
  virtual void _anchor();
};

/// Abstract base class for single-buffer initializations.
class SingleInitializationBase : public Initialization {
public:
  ArrayRef<Initialization*> getSubInitializations() override {
    return {};
  }
  
  void zeroInitialize(SILGenFunction &gen) override {
    Value address = getAddress();
    if (address.getType().isAddressOnly())
      gen.B.createZeroAddr(SILLocation(), address);
    else {
      Value zero = gen.B.createZeroValue(SILLocation(),
                                         address.getType().getObjectType());
      gen.B.createStore(SILLocation(), zero, address);
    }
    finishInitialization(gen);
  }
};


} // end namespace Lowering
} // end namespace swift

#endif
