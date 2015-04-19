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

#include "RValue.h"
#include <memory>

namespace swift {
namespace Lowering {
  class ManagedValue;
  class SILGenFunction;

class Initialization;
using InitializationPtr = std::unique_ptr<Initialization>;
  
/// Initialization - Abstract base class for initialization buffers. An
/// initialization represents an uninitialized buffer or a tuple of
/// uninitialized buffers that must be initialized with the result of an
/// expression, such as in a var declaration or return statement. An
/// initialization may also have partial cleanups that should be disabled and
/// replaced when the buffer is initialized; for instance, a freshly allocated
/// box with an uninitialized value must be deallocated with `dealloc_ref`, but
/// once the box is fully initialized, both the box and the contained value can
/// be cleaned up together with `release`.
///
/// FIXME: provide a reset() operation to support multiple
/// initialization paths.
class Initialization {
public:
  Initialization() {}
  virtual ~Initialization() {}

  /// Return true if this initialization is a simple address in memory.
  virtual bool isSingleBuffer() const {
    return false;
  }

   /// If this initialization represents a single contiguous buffer, return the
  /// SILValue of that buffer's address. If not, returns an invalid SILValue.
  virtual SILValue getAddressOrNull() const = 0;
  
  /// Returns the address of the single contiguous buffer represented by this
  /// initialization. Once the address has been stored to,
  /// finishInitialization must be called.
  SILValue getAddress() const {
    SILValue address = getAddressOrNull();
    assert(address && "initialization does not represent a single buffer");
    return address;
  }
  
  
  /// If this initialization has an address we can directly emit into, return
  /// it.  Otherwise, return a null SILValue.
  virtual SILValue getAddressForInPlaceInitialization() const {
    return SILValue();
  }
  
  /// Return true if we can get the addresses of elements with the
  /// 'getSubInitializationsForTuple' method.  Subclasses can override this to
  /// enable this behavior.
  virtual bool canSplitIntoSubelementAddresses() const {
    return false;
  }

  /// If this initialization represents an aggregation of sub-initializations,
  /// return the sub-initializations. If it represents a single
  /// initialization of tuple type, explode it into initializations for each
  /// individual tuple element. In either case, once all the sub-initializations
  /// have been initialized and finalized with finishInitialization,
  /// finishInitialization must then be called on this aggregate initialization.
  ///
  /// \param buf - If new Initializations need to be created, their ownership
  /// is given to this vector.
  /// \param Loc The location with which the single initialization should be
  ///        associated.
  virtual ArrayRef<InitializationPtr>
  getSubInitializationsForTuple(SILGenFunction &gen, CanType type,
                                SmallVectorImpl<InitializationPtr> &buf,
                                SILLocation Loc) {
    assert(0&&"Must implement if canSplitIntoSubelementAddresses returns true");
    abort();
  }
  
  /// Perform post-initialization bookkeeping for this initialization.
  virtual void finishInitialization(SILGenFunction &gen) {}

  
  /// When emitting an exploded RValue into an initialization, this method is
  /// called once per scalar value in the explosion.
  ///
  /// If this is an *copy* of the rvalue into this initialization then isInit is
  /// false.  If it is an *initialization* of the memory in the initialization,
  /// then isInit is true.
  virtual void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                                   SILLocation loc, SILGenFunction &gen) = 0;

private:
  Initialization(const Initialization &) = delete;
  Initialization(Initialization &&) = delete;
  
  virtual void _anchor();
};

/// Abstract base class for single-buffer initializations.  These are
/// initializations that have an addressable memory object to be stored into.
class SingleBufferInitialization : public Initialization {
public:
  SingleBufferInitialization() {}
  
  bool isSingleBuffer() const override {
    return true;
  }

  // SingleBufferInitializations always have an address.
  SILValue getAddressForInPlaceInitialization() const override {
    return getAddress();
  }
  
  bool canSplitIntoSubelementAddresses() const override {
    return true;
  }
  
  ArrayRef<InitializationPtr>
  getSubInitializationsForTuple(SILGenFunction &gen, CanType type,
                                SmallVectorImpl<InitializationPtr> &buf,
                                SILLocation Loc) override;


  void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                           SILLocation loc, SILGenFunction &gen) override {
    copyOrInitValueIntoSingleBuffer(explodedElement, isInit, getAddress(),
                                    loc, gen);
  }
  
  /// Emit the exploded element into a buffer at the specified address.
  static void copyOrInitValueIntoSingleBuffer(ManagedValue explodedElement,
                                              bool isInit,
                                              SILValue BufferAddress,
                                              SILLocation loc,
                                              SILGenFunction &gen);
};
  
/// This is an initialization for a specific address in memory.
class KnownAddressInitialization : public SingleBufferInitialization {
  /// The physical address of the global.
  SILValue address;
  
  virtual void anchor() const;
public:
  KnownAddressInitialization(SILValue address) : address(address) {}
  
  SILValue getAddressOrNull() const override {
    return address;
  }
};

/// Abstract base class for single-buffer initializations.
class TemporaryInitialization : public SingleBufferInitialization {
  SILValue Addr;
  CleanupHandle Cleanup;
public:
  TemporaryInitialization(SILValue addr, CleanupHandle cleanup)
    : Addr(addr), Cleanup(cleanup) {}

  void finishInitialization(SILGenFunction &gen) override;

  SILValue getAddressOrNull() const override {
    return Addr;
  }

  SILValue getAddress() const {
    return Addr;
  }

  /// Returns the cleanup corresponding to the value of the temporary.
  CleanupHandle getInitializedCleanup() const { return Cleanup; }

  ManagedValue getManagedAddress() const  {
    return ManagedValue(getAddress(), getInitializedCleanup());
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
