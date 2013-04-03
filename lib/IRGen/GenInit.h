//===--- GenInit.h - Swift IR generation for initialization -----*- C++ -*-===//
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
//  This file provides the private interface to the initialization code.
//
//  Most of the complexity here is in the dance of cleanups.  A variable
//  may have up to two cleanups associated with it:
//
//    - A deallocation cleanup, which deletes a heap-allocated variable
//      without invoking its destructor.  This is required when the
//      initializer can throw without the variable being properly
//      initialized.  This is a full-expression cleanup pushed
//      immediately within the heap-allocation but contained within the
//      initializer.  It is deactivated at the instant of initialization for
//      the initializer.
//    - A release/destroy cleanup.  For a heap-allocated variable, this
//      releases the owner, possibly invoking the destructor;  for a
//      stack-allocated variable, this simply destroys the object.
//      It has the lifetime of the actual variable.  It is pushed,
//      inactive, outside the initializer full-expression.

//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENINIT_H
#define SWIFT_IRGEN_GENINIT_H

#include "llvm/ADT/DenseMap.h"
#include "IRGenFunction.h"
#include "swift/SIL/SILValue.h"

namespace swift {
  class ValueDecl;

namespace irgen {

/// An opaque means of identifying an object being initialized.
class InitializedObject {
  void *Opaque;
  InitializedObject(void *opaque) : Opaque(opaque) {}
  friend class Initialization;

public:
  static InitializedObject invalid() { return InitializedObject(nullptr); }
  bool isValid() const { return Opaque != nullptr; }
};

/// An Initialization object manages the cleanups and lifetimes of a
/// variable initialization.
///
/// To use this, you need to:
///   - create an abstract Object for the object you'd like to initialize,
///     using one of the getObject* methods;
///   - register that Object with the Initialization using one of the
///     registerObject* methods;
///   - mark when that object has been allocated using markAllocated; and
///   - mark when that object has been initialization using
///     markInitialized.
///
/// This class also provides several convenience methods for
/// performing one or more stages of this process.
class Initialization {
  struct ValueRecord {
    CleanupsDepth DeallocCleanup;
    CleanupsDepth DestroyCleanup;
  };

  llvm::DenseMap<void *, ValueRecord> Records;

public:
  /// Create an object reference for the given local declaration.
  InitializedObject getObjectForDecl(ValueDecl *value) {
    return InitializedObject(value);
  }
  
  /// Create an object reference for the given SIL value.
  InitializedObject getObjectForValue(SILValue v) {
    return InitializedObject(v.getOpaqueValue());
  }

  /// Create an object reference for the given temporary.
  InitializedObject getObjectForTemporary() {
    // For now, we only support one of these.
    return InitializedObject(reinterpret_cast<void*>(1));
  }

  /// Create a variable.  The abstract object for the variable is the
  /// one returned by getObjectForDecl(var).
  /// 
  /// Precondition: the abstract object has been registered
  ///   with this Initialization, but is not marked as allocated
  ///   or initialized.
  /// Postcondition: the abstract object will have been marked as
  ///   allocated, but not marked as initialized.
  OwnedAddress emitVariable(IRGenFunction &IGF, VarDecl *var,
                            const TypeInfo &type);

  /// Create a local variable.
  ///
  /// Same pre/postconditions as emitVariable.
  OwnedAddress emitLocalAllocation(IRGenFunction &IGF, InitializedObject object,
                                   OnHeap_t onHeap, const TypeInfo &type,
                                   const Twine &name);

  /// Create a global variable.
  ///
  /// Same pre/postconditions as emitVariable.
  OwnedAddress emitGlobalVariable(IRGenFunction &IGF, VarDecl *var,
                                  const TypeInfo &type);

  /// Emit an initializer into the given address.
  ///
  /// Precondition: the object has been marked as allocated at the
  ///   given address.
  /// Postcondition: the object will have been marked as initialized.
  void emitInit(IRGenFunction &IGF, InitializedObject object, Address address,
                Expr *init, const TypeInfo &type);
  void emitZeroInit(IRGenFunction &IGF, InitializedObject object,
                    Address address, const TypeInfo &type);

  /// Add an object that is going to be initialized; use the
  /// appropriate destroy cleanup for the given type.
  CleanupsDepth registerObject(IRGenFunction &IGF, InitializedObject object,
                               OnHeap_t onHeap, const TypeInfo &objectTI);

  /// Add an object that is going to be initialized, but which does not
  /// need a destroy cleanup.
  void registerObjectWithoutDestroy(InitializedObject object);

  /// Mark that an object has been allocated, and associate a dealloc
  /// cleanup with it.  This should be called even if allocation is
  /// trivial and there isn't a dealloc cleanup.
  void markAllocated(IRGenFunction &IGF, InitializedObject object,
                     OwnedAddress address,
                     CleanupsDepth dealloc);

  /// Mark that the value has reached its instant of initialization.
  void markInitialized(IRGenFunction &IGF, InitializedObject object);

private:
  /// Add an object that is going to be initialized.
  void registerObject(InitializedObject object, CleanupsDepth destroy);
};

} // end namespace irgen
} // end namespace swift

#endif
