//===--- KeyPath.h - ABI constants for key path objects ---------*- C++ -*-===//
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

  static constexpr uint32_t isLetBit(bool isLet) {
    return isLet ? 0 : _SwiftKeyPathComponentHeader_StoredMutableFlag;
  }

public:
  static constexpr bool offsetCanBeInline(unsigned offset) {
    return offset <= _SwiftKeyPathComponentHeader_MaximumOffsetPayload;
  }

  constexpr static KeyPathComponentHeader
  forStructComponentWithInlineOffset(bool isLet,
                                     unsigned offset) {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_StructTag
       << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | validateInlineOffset(offset)
      | isLetBit(isLet));
  }
  
  constexpr static KeyPathComponentHeader
  forStructComponentWithOutOfLineOffset(bool isLet) {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_StructTag
       << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_OutOfLineOffsetPayload
      | isLetBit(isLet));
  }

  constexpr static KeyPathComponentHeader
  forStructComponentWithUnresolvedFieldOffset(bool isLet) {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_StructTag
       << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_UnresolvedFieldOffsetPayload
      | isLetBit(isLet));
  }
  
  constexpr static KeyPathComponentHeader
  forClassComponentWithInlineOffset(bool isLet,
                                    unsigned offset) {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_ClassTag
       << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | validateInlineOffset(offset)
      | isLetBit(isLet));
  }

  constexpr static KeyPathComponentHeader
  forClassComponentWithOutOfLineOffset(bool isLet) {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_ClassTag
       << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_OutOfLineOffsetPayload
      | isLetBit(isLet));
  }
  
  constexpr static KeyPathComponentHeader
  forClassComponentWithUnresolvedFieldOffset(bool isLet) {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_ClassTag
       << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_UnresolvedFieldOffsetPayload
      | isLetBit(isLet));
  }
  
  constexpr static KeyPathComponentHeader
  forClassComponentWithUnresolvedIndirectOffset(bool isLet) {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_ClassTag
       << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | _SwiftKeyPathComponentHeader_UnresolvedIndirectOffsetPayload
      | isLetBit(isLet));
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
  
  enum ComputedPropertyKind {
    GetOnly,
    SettableNonmutating,
    SettableMutating,
  };
  
  enum ComputedPropertyIDKind {
    Pointer,
    StoredPropertyIndex,
    VTableOffset,
  };
  
  constexpr static uint32_t
  getResolutionStrategy(ComputedPropertyIDKind idKind) {
    return idKind == Pointer ? _SwiftKeyPathComponentHeader_ComputedIDUnresolvedIndirectPointer
         : (assert("no resolution strategy implemented" && false), 0);
  }
  
  constexpr static KeyPathComponentHeader
  forComputedProperty(ComputedPropertyKind kind,
                      ComputedPropertyIDKind idKind,
                      bool hasArguments,
                      bool resolvedID) {
    return KeyPathComponentHeader(
      (_SwiftKeyPathComponentHeader_ComputedTag
        << _SwiftKeyPathComponentHeader_DiscriminatorShift)
      | (kind != GetOnly
           ? _SwiftKeyPathComponentHeader_ComputedSettableFlag : 0)
      | (kind == SettableMutating
           ? _SwiftKeyPathComponentHeader_ComputedMutatingFlag : 0)
      | (idKind == StoredPropertyIndex
           ? _SwiftKeyPathComponentHeader_ComputedIDByStoredPropertyFlag : 0)
      | (idKind == VTableOffset
           ? _SwiftKeyPathComponentHeader_ComputedIDByVTableOffsetFlag : 0)
      | (hasArguments ? _SwiftKeyPathComponentHeader_ComputedHasArgumentsFlag : 0)
      | (resolvedID ? _SwiftKeyPathComponentHeader_ComputedIDResolved
                    : getResolutionStrategy(idKind)));
  }
  
  constexpr static KeyPathComponentHeader
  forExternalComponent(unsigned numSubstitutions) {
    return assert(numSubstitutions <
        (1u << _SwiftKeyPathComponentHeader_DiscriminatorShift) - 1u
        && "too many substitutions"),
      KeyPathComponentHeader(
        (_SwiftKeyPathComponentHeader_ExternalTag
          << _SwiftKeyPathComponentHeader_DiscriminatorShift)
        | numSubstitutions);
  }
  
  constexpr uint32_t getData() const { return Data; }
};

}

#endif
