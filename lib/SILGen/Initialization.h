//===--- Initialization.h - Buffer initialization. --------------*- C++ -*-===//
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
//
// A storage structure for representing a buffer or collection of buffers to
// be initialized.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_INITIALIZATION_H
#define SWIFT_LOWERING_INITIALIZATION_H

#include "ManagedValue.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <memory>

namespace swift {
namespace Lowering {

class SILGenFunction;
class Initialization;
using InitializationPtr = std::unique_ptr<Initialization>;
class TemporaryInitialization;
using TemporaryInitializationPtr = std::unique_ptr<TemporaryInitialization>;
  
/// An abstract class for consuming a value.  This is used for initializing
/// variables, although that is not the only way it is used.
///
/// Implementations of this interface deal with the details of managing
/// cleanups for the received value, as well as potentially managing partial
/// cleanups of components of the value during the operation.
///
/// For example, during the initialization of a boxed local variable, it
/// is invalid to release the box, because that will attempt to destroy
/// the uninitialized value.  Instead, a cleanup to deallocate the box
/// (with `dealloc_ref`) must be active; once initialization is complete,
/// that cleanup (and any separate cleanup on the boxed value) must be
/// deactivated, and a cleanup to release the box can be enabled instead.
///
/// This interface supports four ways to receive the initializing value:
///
///   - If canPerformInPlaceInitialization() return true,
///     getAddressForInPlaceInitialization may be called.
///     It is not legal to call getAddressForInPlaceInitialization
///     multiple times.
///
///   - If canSplitIntoTupleElements() returns true, getTupleElements may
///     be called.  It is not legal to call getTupleElements multiple times.
///     Once getTupleElements has been called, the returned initializations
///     must be completely initialized (including calling
///     finishInitialization) before finishInitialization is called on the
///     outer initialization.
///
///   - copyOrInitValueInto may be called.
///
/// In all of these cases, finishInitialization must be called after
/// initialization is complete.
///
/// Alternatively, some "initializers" may call finishUninitialized if there
/// was no immediate initializer.  This is generally not possibly when the
/// Initialization is used merely as the destination for expression emission;
/// an Initialization subclass only need implement this when the subclass
/// might be used for an irrefutable pattern lacking an initializer.
///
/// FIXME: provide a reset() operation to support multiple
/// initialization paths.
class Initialization {
public:
  Initialization() {}
  virtual ~Initialization() {}

  /// Return true if this initialization supports in-place initialization.
  ///
  /// This method must return consistently both before and after
  /// initialization begins.
  virtual bool canPerformInPlaceInitialization() const {
    return false;
  }

  /// A hack that should be removed relating to the initialization of
  /// global values.
  virtual bool isInPlaceInitializationOfGlobal() const {
    llvm_unreachable("didn't implement isInPlaceInitializationOfGlobal");
  }
  
  /// Begin an in-place initialization, given that
  /// canPerformInPlaceInitialization() returned true.
  virtual SILValue
  getAddressForInPlaceInitialization(SILGenFunction &SGF, SILLocation loc) {
    llvm_unreachable("Must implement if getAddressForInPlaceInitialization "
                     "returns true");
  }
  
  /// Return true if we can get the addresses of elements with the
  /// 'splitIntoTupleElements' method.  Subclasses can override this to
  /// enable this behavior.
  virtual bool canSplitIntoTupleElements() const {
    return false;
  }

  /// Break this initialization (which expects a value of tuple type)
  /// into component sub-initializations for the elements.  This
  /// only destructures a single level of tuple.
  ///
  /// Once this method is called, the caller must ensure the complete
  /// initialization of the result initializations, including calling
  /// finishInitialization on them.  It is still necessary to call
  /// finishInitialization on the tuple initialization after this is done.
  ///
  /// \param buf - If new Initializations need to be created, their ownership
  ///   is given to this vector.  The caller should not otherwise interact
  ///   with these initializations.
  /// \param loc The location for any instructions required to split the
  ///   initialization.
  virtual MutableArrayRef<InitializationPtr>
  splitIntoTupleElements(SILGenFunction &SGF, SILLocation loc, CanType type,
                         SmallVectorImpl<InitializationPtr> &buf) {
    llvm_unreachable("Must implement if canSplitIntoTupleElements "
                     "returns true");
  }
  
  /// Initialize this with the given value.  This should be an operation
  /// of last resort: it is generally better to split tuples or evaluate
  /// in-place when the initialization supports that.
  ///
  /// If this is an *copy* of the rvalue into this initialization then isInit is
  /// false.  If it is an *initialization* of the memory in the initialization,
  /// then isInit is true.
  virtual void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                                   ManagedValue explodedElement,
                                   bool isInit) = 0;

  /// Perform post-initialization bookkeeping for this initialization.
  virtual void finishInitialization(SILGenFunction &SGF) {}

  /// Perform post-initialization bookkeeping for this initialization,
  /// given that it wasn't actually initialized.
  virtual void finishUninitialized(SILGenFunction &SGF) {
    llvm_unreachable("Initialization subclass does not support being left "
                     "uninitialized");
  }

private:
  Initialization(const Initialization &) = delete;
  Initialization(Initialization &&) = delete;
  
  virtual void _anchor();
};

/// Abstract base class for single-buffer initializations.  These are
/// initializations that have an addressable memory object to be stored into.
class SingleBufferInitialization : public Initialization {
  llvm::TinyPtrVector<CleanupHandle::AsPointer> SplitCleanups;
public:
  SingleBufferInitialization() {}
  
  bool canPerformInPlaceInitialization() const override {
    return true;
  }

  // SingleBufferInitializations must always implement this method.
  SILValue getAddressForInPlaceInitialization(SILGenFunction &SGF,
                                              SILLocation loc) override = 0;

  bool isInPlaceInitializationOfGlobal() const override = 0;
  
  bool canSplitIntoTupleElements() const override {
    return true;
  }
  
  MutableArrayRef<InitializationPtr>
  splitIntoTupleElements(SILGenFunction &SGF, SILLocation loc, CanType type,
                         SmallVectorImpl<InitializationPtr> &buf) override;

  void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                           ManagedValue value, bool isInit) override {
    auto address = getAddressForInPlaceInitialization(SGF, loc);
    copyOrInitValueIntoSingleBuffer(SGF, loc, value, isInit, address);
  }

  /// Overriders must call this.
  void finishInitialization(SILGenFunction &SGF) override;
  
  /// Emit the exploded element into a buffer at the specified address.
  static void copyOrInitValueIntoSingleBuffer(SILGenFunction &SGF,
                                              SILLocation loc,
                                              ManagedValue value,
                                              bool isInit,
                                              SILValue bufferAddress);

  static MutableArrayRef<InitializationPtr>
  splitSingleBufferIntoTupleElements(SILGenFunction &SGF, SILLocation loc,
                                     CanType type, SILValue bufferAddress,
                                     SmallVectorImpl<InitializationPtr> &buf,
                       TinyPtrVector<CleanupHandle::AsPointer> &splitCleanups);
};
  
/// This is an initialization for a specific address in memory.
class KnownAddressInitialization : public SingleBufferInitialization {
  /// The physical address of the global.
  SILValue address;
  
public:
  KnownAddressInitialization(SILValue address) : address(address) {}
  
  SILValue getAddress() const {
    return address;
  }

  SILValue getAddressForInPlaceInitialization(SILGenFunction &SGF,
                                              SILLocation loc) override {
    return address;
  }

  bool isInPlaceInitializationOfGlobal() const override;

  void finishUninitialized(SILGenFunction &SGF) override {}
};

/// Abstract base class for single-buffer initializations.
class TemporaryInitialization : public SingleBufferInitialization {
  SILValue Addr;
  CleanupHandle Cleanup;
public:
  TemporaryInitialization(SILValue addr, CleanupHandle cleanup)
    : Addr(addr), Cleanup(cleanup) {}

  void finishInitialization(SILGenFunction &SGF) override;

  void finishUninitialized(SILGenFunction &SGF) override {
    TemporaryInitialization::finishInitialization(SGF);
  }

  SILValue getAddressForInPlaceInitialization(SILGenFunction &SGF,
                                              SILLocation loc) override {
    return Addr;
  }

  SILValue getAddress() const {
    return Addr;
  }

  bool isInPlaceInitializationOfGlobal() const override;

  /// Returns the cleanup corresponding to the value of the temporary.
  CleanupHandle getInitializedCleanup() const { return Cleanup; }

  ManagedValue getManagedAddress() const  {
    return ManagedValue(getAddress(), getInitializedCleanup());
  }
};

/// An initialization which accumulates several other initializations
/// into a tuple.
class TupleInitialization : public Initialization {
public:
  /// The sub-Initializations aggregated by this tuple initialization.
  /// The TupleInitialization object takes ownership of Initializations pushed
  /// here.
  SmallVector<InitializationPtr, 4> SubInitializations;
    
  TupleInitialization() {}

  bool canSplitIntoTupleElements() const override {
    return true;
  }
    
  MutableArrayRef<InitializationPtr>
  splitIntoTupleElements(SILGenFunction &SGF, SILLocation loc, CanType type,
                         SmallVectorImpl<InitializationPtr> &buf) override {
    return SubInitializations;
  }

  void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                           ManagedValue valueMV, bool isInit) override;

  // We don't need to do anything in finishInitialization.  There are two
  // ways to initialize a TupleInitialization:
  //   - splitting the initialization, in which case the initializer is
  //     responsible for finishing the sub-initializations itself, or
  //   - calling copyOrInitValueInto, which immediately finishes all
  //     of the sub-initializations.

  void finishUninitialized(SILGenFunction &SGF) override;
};

} // end namespace Lowering
} // end namespace swift

#endif
