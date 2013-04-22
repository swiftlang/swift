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

#include "swift/SIL/SILValue.h"
#include "SILGen.h"
#include "swift/Basic/DiverseList.h"
#include <memory>

namespace swift {
namespace Lowering {
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
class Initialization {
public:
  enum class Kind {
    /// This Initialization is for a _ binding or other ignored value; the
    /// corresponding result can be discarded.
    Ignored,
    /// This Initialization is for a [byref] argument binding, which is
    /// initialized using bindAddress().
    AddressBinding,
    /// This Initialization is for a single buffer with a physical address,
    /// which can be accessed with getAddress() and stored to.
    SingleBuffer,
    /// This Initialization is for a tuple of sub-initializations, which can
    /// be accessed with getSubInitializations().
    Tuple
  };
  
  /// The Kind of initialization.
  const Kind kind;
  /// The Swift type of the value to be initialized.
  const Type type;
  
  Initialization(Kind kind, Type type) : kind(kind), type(type) {}
  virtual ~Initialization() {}

  /// If this initialization represents a single contiguous buffer, return the
  /// SILValue of that buffer's address. If not, returns an invalid SILValue.
  virtual SILValue getAddressOrNull() = 0;
  
  /// Binds an address value to this initialization. Used for [byref] arguments,
  /// but invalid anywhere else.
  virtual void bindAddress(SILValue address, SILGenFunction &gen) {
    llvm_unreachable("unexpected address value in initialization!");
  }
  
  /// Returns true if this initialization represents a single contiguous buffer.
  bool hasAddress() { return getAddressOrNull().isValid(); }

  /// Returns the address of the single contiguous buffer represented by this
  /// initialization. Once the address has been stored to,
  /// finishInitialization must be called.
  SILValue getAddress() {
    SILValue address = getAddressOrNull();
    assert(address && "initialization does not represent a single buffer");
    return address;
  }

  /// If this initialization represents an aggregation of sub-initializations,
  /// return the sub-initializations. Once all the sub-initializations have been
  /// initialized and finalized with finishInitialization, finishInitialization
  /// must then be called on this aggregate initialization.
  virtual ArrayRef<InitializationPtr> getSubInitializations() = 0;
  
  /// If this initialization represents an aggregation of sub-initializations,
  /// return the sub-initializations. If it represents a single
  /// initialization of tuple type, explode it into initializations for each
  /// individual tuple element. In either case, once all the sub-initializations
  /// have been initialized and finalized with finishInitialization,
  /// finishInitialization must then be called on this aggregate initialization.
  ///
  /// \param buf - If new Initializations need to be created, their ownership
  /// is given to this vector.
  ArrayRef<InitializationPtr> getSubInitializations(
                                      SILGenFunction &gen,
                                      SmallVectorImpl<InitializationPtr> &buf);
  
  /// Perform post-initialization bookkeeping for this initialization.
  virtual void finishInitialization(SILGenFunction &gen) {}

  /// Perform a default initialization of this buffer or buffers, then
  /// invoke finishInitialization().
  virtual void defaultInitialize(SILGenFunction &gen) = 0;
  
private:
  Initialization(const Initialization &) = delete;
  Initialization(Initialization &&) = delete;
  
  virtual void _anchor();
};

/// Abstract base class for single-buffer initializations.
class SingleInitializationBase : public Initialization {
public:
  SingleInitializationBase(Type type)
    : Initialization(Initialization::Kind::SingleBuffer, type)
  {}
  
  ArrayRef<InitializationPtr> getSubInitializations() override {
    return {};
  }
  
  void defaultInitialize(SILGenFunction &gen) override {
    SILValue address = getAddress();
    gen.B.createInitializeVar(SILLocation(), address,
                              /*CanDefaultConstruct*/ true);
    finishInitialization(gen);
  }
};


} // end namespace Lowering
} // end namespace swift

#endif
