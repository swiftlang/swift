//===--- RuntimeInternals.h - Runtime Internal Structures -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Runtime data structures that Reflection inspects externally.
//
// FIXME: Ideally the original definitions would be templatized on a Runtime
// parameter and we could use the original definitions in both the runtime and
// in Reflection.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_RUNTIME_INTERNALS_H
#define SWIFT_REFLECTION_RUNTIME_INTERNALS_H

namespace swift {

namespace reflection {

template <typename Runtime>
struct ConformanceNode {
  typename Runtime::StoredPointer Left, Right;
  typename Runtime::StoredPointer Type;
  typename Runtime::StoredPointer Proto;
  typename Runtime::StoredPointer Description;
  typename Runtime::StoredSize FailureGeneration;
};

template <typename Runtime>
struct MetadataAllocation {
  uint16_t Tag;
  typename Runtime::StoredPointer Ptr;
  unsigned Size;
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_RUNTIME_INTERNALS_H
