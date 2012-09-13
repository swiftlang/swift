//===--- KnownMetadata.cpp - Swift Language ABI Known Metadata Objects ----===//
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
// Definitions of some builtin metadata objects.
//
//===----------------------------------------------------------------------===//

#include "Metadata.h"
#include "Alloc.h"

using namespace swift;

/// A function which helpfully does nothing.
static void doNothing(void *ptr, void *self) {}

/// A projectBuffer implementation which just reinterprets the buffer.
static OpaqueValue *projectBuffer(ValueBuffer *dest,
                                  ValueWitnessTable *self) {
  return reinterpret_cast<OpaqueValue*>(dest);
}

/// A function which does a naive copy.
template <class T> static T *copy(T *dest, T *src, ValueWitnessTable *self) {
  *dest = *src;
  return dest;
}

#define POD_VALUE_WITNESS_TABLE(TYPE, SIZE) { \
  (value_witness_types::destroyBuffer*) &doNothing,                     \
  (value_witness_types::initializeBufferWithCopyOfBuffer*) &copy<TYPE>, \
  (value_witness_types::projectBuffer*) &projectBuffer,                 \
  (value_witness_types::deallocateBuffer*) &doNothing,                  \
  (value_witness_types::destroy*) &doNothing,                           \
  (value_witness_types::initializeBufferWithCopy*) &copy<TYPE>,         \
  (value_witness_types::initializeWithCopy*) &copy<TYPE>,               \
  (value_witness_types::assignWithCopy*) &copy<TYPE>,                   \
  (value_witness_types::initializeBufferWithTake*) &copy<TYPE>,         \
  (value_witness_types::initializeWithTake*) &copy<TYPE>,               \
  (value_witness_types::assignWithTake*) &copy<TYPE>,                   \
  (value_witness_types::allocateBuffer*) &projectBuffer,                \
  (value_witness_types::size) (SIZE),                                   \
  (value_witness_types::alignment) (SIZE),                              \
  (value_witness_types::stride) (SIZE)                                  \
}

extern "C" ValueWitnessTable _TWVVSs4POD1 = POD_VALUE_WITNESS_TABLE(uint8_t, 1);
extern "C" ValueWitnessTable _TWVVSs4POD2 = POD_VALUE_WITNESS_TABLE(uint16_t, 2);
extern "C" ValueWitnessTable _TWVVSs4POD4 = POD_VALUE_WITNESS_TABLE(uint32_t, 4);
extern "C" ValueWitnessTable _TWVVSs4POD8 = POD_VALUE_WITNESS_TABLE(uint64_t, 8);

/// A function to initialize a buffer/variable by retaining the given
/// pointer and then assigning it.
static HeapObject **initWithRetain(HeapObject **dest,
                                   HeapObject **src,
                                   ValueWitnessTable *self) {
  *dest = swift_retain(*src);
  return dest;
}

/// A function to destroy a buffer/variable by releasing the value in it.
static void destroyWithRelease(HeapObject **var,
                               ValueWitnessTable *self) {
  swift_release(*var);
}

/// A function to assign to a variable by copying from an existing one.
static HeapObject **assignWithRetain(HeapObject **dest,
                                     HeapObject **src,
                                     ValueWitnessTable *self) {
  HeapObject *newValue = swift_retain(*src);
  swift_release(*dest);
  *dest = newValue;
  return dest;
}

/// A function to assign to a variable by taking from an existing one.
static HeapObject **assignWithoutRetain(HeapObject **dest,
                                        HeapObject **src,
                                        ValueWitnessTable *self) {
  HeapObject *newValue = *src;
  swift_release(*dest);
  *dest = newValue;
  return dest;
}

/// The basic value-witness table for Swift object pointers.
extern "C" ValueWitnessTable _TWVBo = {
  (value_witness_types::destroyBuffer*) &destroyWithRelease,
  (value_witness_types::initializeBufferWithCopyOfBuffer*) &initWithRetain,
  (value_witness_types::projectBuffer*) &projectBuffer,
  (value_witness_types::deallocateBuffer*) &doNothing,
  (value_witness_types::destroy*) &destroyWithRelease,
  (value_witness_types::initializeBufferWithCopy*) &initWithRetain,
  (value_witness_types::initializeWithCopy*) &initWithRetain,
  (value_witness_types::assignWithCopy*) &assignWithRetain,
  (value_witness_types::initializeBufferWithTake*) &copy<uintptr_t>,
  (value_witness_types::initializeWithTake*) &copy<uintptr_t>,
  (value_witness_types::assignWithTake*) &assignWithoutRetain,
  (value_witness_types::allocateBuffer*) &projectBuffer,
  (value_witness_types::size) sizeof(void*),
  (value_witness_types::alignment) sizeof(void*),
  (value_witness_types::stride) sizeof(void*)
};
