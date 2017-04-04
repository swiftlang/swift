//===--- KeyPath.h - ABI constants for key path objects ----------*- C++ *-===//
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
//  Constants used in the layout of key path objects.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_ABI_KEYPATH_H__
#define __SWIFT_ABI_KEYPATH_H__

// We include the basic constants in a shim header so that it can be shared with
// the Swift implementation in the standard library.

#include <cstdint>
#include <cassert>
#include "../../../stdlib/public/SwiftShims/KeyPath.h"

namespace swift {

/// Header layout for a key path's data buffer header.
class KeyPathBufferHeader {
  uint32_t Data;
  
  constexpr KeyPathBufferHeader(unsigned Data) : Data(Data) {}
  
  static constexpr uint32_t validateSize(uint32_t size) {
    return assert(size <= _SwiftKeyPathBufferHeader_SizeMask
                  && "size too big!"),
           size;
  }
public:
  constexpr KeyPathBufferHeader(unsigned size,
                                bool trivialOrInstantiableInPlace,
                                bool hasReferencePrefix)
    : Data((validateSize(size) & _SwiftKeyPathBufferHeader_SizeMask)
           | (trivialOrInstantiableInPlace ? _SwiftKeyPathBufferHeader_TrivialFlag : 0)
           | (hasReferencePrefix ? _SwiftKeyPathBufferHeader_HasReferencePrefixFlag : 0)) 
  {
  }
  
  constexpr KeyPathBufferHeader withSize(unsigned size) const {
    return (Data & ~_SwiftKeyPathBufferHeader_SizeMask) | validateSize(size);
  }
  
  constexpr KeyPathBufferHeader withIsTrivial(bool isTrivial) const {
    return (Data & ~_SwiftKeyPathBufferHeader_TrivialFlag)
      | (isTrivial ? _SwiftKeyPathBufferHeader_TrivialFlag : 0);
  }
  constexpr KeyPathBufferHeader withIsInstantiableInPlace(bool isTrivial) const {
    return (Data & ~_SwiftKeyPathBufferHeader_TrivialFlag)
      | (isTrivial ? _SwiftKeyPathBufferHeader_TrivialFlag : 0);
  }

  constexpr KeyPathBufferHeader withHasReferencePrefix(bool hasPrefix) const {
    return (Data & ~_SwiftKeyPathBufferHeader_HasReferencePrefixFlag)
      | (hasPrefix ? _SwiftKeyPathBufferHeader_HasReferencePrefixFlag : 0);
  }

  constexpr uint32_t getData() const {
    return Data;
  }
};

/// Header layout for a key path component's header.
class KeyPathComponentHeader {
  uint32_t Data;
  
  constexpr KeyPathComponentHeader(unsigned Data) : Data(Data) {}

  static constexpr uint32_t validateInlineOffset(uint32_t offset) {
    return assert(offsetCanBeInline(offset)
                  && "offset too big!"),
           offset;
  }

public:
  static constexpr bool offsetCanBeInline(unsigned offset) {
    return offset <= _SwiftKeyPathComponentHeader_MaximumOffsetPayload;
  }

  constexpr static KeyPathComponentHeader
  forStructComponentWithInlineOffset(unsigned offset) {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_StructTag
      << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | validateInlineOffset(offset));
  }
  
  constexpr static KeyPathComponentHeader
  forStructComponentWithOutOfLineOffset() {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_StructTag
      << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_OutOfLineOffsetPayload);
  }

  constexpr static KeyPathComponentHeader
  forStructComponentWithUnresolvedOffset() {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_StructTag
      << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_UnresolvedOffsetPayload);
  }
  
  constexpr static KeyPathComponentHeader
  forClassComponentWithInlineOffset(unsigned offset) {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_ClassTag
      << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | validateInlineOffset(offset));
  }

  constexpr static KeyPathComponentHeader
  forClassComponentWithOutOfLineOffset() {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_ClassTag
      << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_OutOfLineOffsetPayload);
  }
  
  constexpr static KeyPathComponentHeader
  forClassComponentWithUnresolvedOffset() {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_StructTag
      << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_UnresolvedOffsetPayload);
  }
  
  constexpr static KeyPathComponentHeader
  forOptionalChain() {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_OptionalTag
      << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_OptionalChainPayload);
  }
  constexpr static KeyPathComponentHeader
  forOptionalWrap() {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_OptionalTag
      << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_OptionalWrapPayload);
  }
  constexpr static KeyPathComponentHeader
  forOptionalForce() {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_OptionalTag
      << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_OptionalForcePayload);
  }
  
  constexpr uint32_t getData() const { return Data; }
};

}

#endif
