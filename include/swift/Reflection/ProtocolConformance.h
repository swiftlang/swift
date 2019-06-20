//===--- ProtocolConformance.h --------------------------------*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_PROTOCOLCONFORMANCE_H
#define SWIFT_REFLECTION_PROTOCOLCONFORMANCE_H

#if !defined(__APPLE__) || !defined(__MACH__)
// TODO: Implement this.
#error "Only supported on Darwin now. Do not include this otherwise!"
#endif

#include "swift/ABI/Metadata.h"
#include "swift/Basic/LLVM.h"
#include "swift/Reflection/ReflectionInfo.h"

namespace swift {
namespace reflection {

template <typename Runtime>
class ProtocolDescriptorIterator
    : public std::iterator<std::forward_iterator_tag,
                           TargetProtocolDescriptor<Runtime>> {
  // The protocol descriptor section is filled with relative direct
  // pointers to protocol descriptors.
  using PointerType = RelativeDirectPointer<TargetProtocolDescriptor<Runtime>>;

public:
  using ProtocolDescriptor = TargetProtocolDescriptor<Runtime>;

  const void *Cur;
  const void *const End;

  ProtocolDescriptorIterator(const ReflectionInfo &reflectionInfo)
      : Cur(reflectionInfo.Protocol.Metadata.getStartAddress()),
        End(reflectionInfo.Protocol.Metadata.getEndAddress()) {}

  const ProtocolDescriptor &operator*() const { return *curAsPointer(); }

  const ProtocolDescriptor *operator->() const { return curAsPointer(); }

  bool hasNext() const { return Cur < End; }

  ProtocolDescriptorIterator &operator++() {
    assert(hasNext());
    const void *Next =
        reinterpret_cast<const char *>(Cur) + sizeof(PointerType);
    Cur = Next;
    return *this;
  }

  bool operator==(ProtocolDescriptorIterator const &other) const {
    return Cur == other.Cur && End == other.End;
  }

  bool operator!=(ProtocolDescriptorIterator const &other) const {
    return !(*this == other);
  }

  const ProtocolDescriptor *curAsPointer() const {
    return static_cast<const PointerType *>(Cur)->get();
  }
};

} // namespace reflection
} // namespace swift

#endif
