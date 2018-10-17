//===--- Class.h - Compiler/runtime class-metadata values -------*- C++ -*-===//
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
// This header provides target-independent information about class
// metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_CLASS_H
#define SWIFT_ABI_CLASS_H

#include <stdint.h>

namespace swift {

/// Objective-C class flags, stored in the ro-data.
enum class ObjCClassFlags : uint32_t {
  /// This class is a metaclass.
  Meta                 = 0x00001,

  /// This class is a root class.
  Root                 = 0x00002,

  /// This class provides a non-trivial .cxx_construct or .cxx_destruct
  /// implementation.
  HasCXXStructors      = 0x00004,

  /// This class has hidden visibility.
  Hidden               = 0x00010,

  /// This class has the exception attribute.
  Exception            = 0x00020,

  /// This class provides a metadata update callback trailing the ro-data.
  /// Note that we're re-using the obsolete flag above.
  HasMetadataUpdateCallback = 0x00040,

  /// (Obsolete) ARC-specific: this class has a .release_ivars method.
  HasIvarReleaser      = 0x00040,

  /// This class implementation was compiled under ARC.
  CompiledByARC        = 0x00080,

  /// This class provides a non-trivial .cxx_destruct method, but
  /// its .cxx_construct is trivial.  For backwards compatibility,
  /// when setting this flag, HasCXXStructors must be set as well.
  HasCXXDestructorOnly = 0x00100
};
inline ObjCClassFlags &operator|=(ObjCClassFlags &lhs, ObjCClassFlags rhs) {
  lhs = ObjCClassFlags(uint32_t(lhs) | uint32_t(rhs));
  return lhs;
}
inline ObjCClassFlags operator|(ObjCClassFlags lhs, ObjCClassFlags rhs) {
  return (lhs |= rhs);
}

}

#endif /* SWIFT_ABI_CLASS_H */
