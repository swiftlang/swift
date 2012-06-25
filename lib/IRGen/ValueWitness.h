//===--- ValueWitness.h - Enumeration of value witnesses --------*- C++ -*-===//
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
// This file defines the list of witnesses required to attest that a
// type is a value type.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_VALUEWITNESS_H
#define SWIFT_IRGEN_VALUEWITNESS_H

namespace swift {
namespace irgen {

/// The members required to attest that a type is a value type.
///
/// Logically, there are three basic data operations we must support
/// on arbitrary types:
///   - initializing an object by copying another
///   - changing an object to be a copy of another
///   - destroying an object
///
/// As an optimization to permit efficient transfers of data, the
/// "copy" operations each have an analogous "take" operation which
/// implicitly destroys the source object.
///
/// Therefore there are five basic data operations:
///   initWithCopy(T*, T*)
///   initWithTake(T*, T*)
///   assignWithCopy(T*, T*)
///   assignWithTake(T*, T*)
///   destroy(T*)
///
/// As a further optimization, for every T*, there is a related
/// operation which replaces that T* with a B*, combinatorially.  This
/// makes 18 operations, except that some of these operations are
/// fairly unlikely and so do not merit optimized entries, due to
/// the common code patterns of the two use cases:
///   - Existential code usually doesn't work directly with T*s
///     because pointers into existential objects are not generally
///     reliable.
///   - Generic code works with T*s a fair amount, but it usually
///     doesn't have to deal with B*s after initialization
///     because initialization returns a reliable pointer.
/// This leads us to the following conclusions:
//    - Operations to copy a B* to a T* are very unlikely
///     to be used (-4 operations).
///   - Assignments involving two B*s are only likely in
///     existential code, where we won't have the right
///     typing guarantees to use them (-2 operations).
/// Furthermore, take-initializing a buffer from a buffer is just a
/// memcpy of the buffer (-1), and take-assigning a buffer from a
/// buffer is just a destroy and a memcpy (-1).
///
/// This leaves us with 12 data operations, to which we add the
/// meta-operation 'sizeAndAlign' for a total of 13.
enum class ValueWitness : unsigned {
  // destroyBuffer comes first because I expect it to be the most
  // common operation (both by code size and occurrence), since it's
  // the optimal way to destroy an individual local/temporary.
  //
  // Several other candidates that are likely to see use in
  // existential code are then grouped together for cache-locality
  // reasons.
  
  ///   void (*destroyBuffer)(B *buffer, W *self);
  ///
  /// Given a valid buffer which owns a valid object of this type,
  /// destroy it.  This can be decomposed as
  ///   self->destroy(self->projectBuffer(buffer), self);
  ///   self->deallocateBuffer(buffer), self);
  DestroyBuffer,

  ///   T *(*initializeBufferWithCopyOfBuffer)(B *dest, B *src, W *self);
  /// Given an invalid buffer, initialize it as a copy of the
  /// object in the source buffer.  This can be decomposed as:
  ///   initalizeBufferWithCopy(dest, self->projectBuffer(src), self)
  InitializeBufferWithCopyOfBuffer,
  
  ///   T *(*projectBuffer)(B *buffer, W *self);
  ///
  /// Given an initialized fixed-size buffer, find its allocated
  /// storage.
  ProjectBuffer,

  ///   void (*deallocateBuffer)(B *buffer, W *self);
  ///
  /// Given a buffer owning storage for an uninitialized object of this
  /// type, deallocate the storage, putting the buffer in an invalid
  /// state.
  DeallocateBuffer, // likely along exception edges of initializers

  ///   void (*destroy)(T *object, witness_t *self);
  ///
  /// Given a valid object of this type, destroy it, leaving it as an
  /// invalid object.  This is useful when generically destroying
  /// an object which has been allocated in-line, such as an array,
  /// struct, or tuple element.
  Destroy,

  ///   T *(*initializeBufferWithCopy)(B *dest, T *src, W *self);
  /// Given an invalid buffer, initialize it as a copy of the
  /// source object.  This can be decomposed as:
  ///   initializeWithCopy(self->allocateBuffer(dest, self), src, self)
  InitializeBufferWithCopy,

  ///   T *(*initializeWithCopy)(T *dest, T *src, W *self);
  ///
  /// Given an invalid object of this type, initialize it as a copy of
  /// the source object.  Returns the dest object.
  InitializeWithCopy,

  ///   T *(*assignWithCopy)(T *dest, T *src, W *self);
  ///
  /// Given a valid object of this type, change it to be a copy of the
  /// source object.  Returns the dest object.
  AssignWithCopy,

  ///   T *(*initializeBufferWithTake)(B *dest, T *src, W *self);
  ///
  /// Given an invalid buffer, initialize it by taking the value
  /// of the source object.  The source object becomes invalid.
  /// Returns the dest object.  
  InitializeBufferWithTake,

  ///   T *(*initializeWithTake)(T *dest, T *src, W *self);
  ///
  /// Given an invalid object of this type, initialize it by taking
  /// the value of the source object.  The source object becomes
  /// invalid.  Returns the dest object.
  InitializeWithTake,

  ///   T *(*assignWithTake)(T *dest, T *src, W *self);
  ///
  /// Given a valid object of this type, change it to be a copy of the
  /// source object.  The source object becomes invalid.  Returns the
  /// dest object.
  AssignWithTake,

  ///   T *(*allocateBuffer)(B *buffer, W *self);
  /// 
  /// Given a buffer in an invalid state, make it the owner of storage
  /// for an uninitialized object of this type.  Return the address of
  /// that object.
  AllocateBuffer,

  ///   typedef struct { size_t Size; size_t Align } layout_t;
  ///   layout_t (*sizeAndAlignment)(W *self);
  ///
  /// Returns the required storage size and alignment of an object of
  /// this type.
  SizeAndAlignment
};
 
enum {
  NumValueWitnesses = unsigned(ValueWitness::SizeAndAlignment) + 1
};

} // end namespace irgen
} // end namespace swift

#endif
