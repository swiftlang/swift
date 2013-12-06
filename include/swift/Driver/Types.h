//===--- Types.h - Input & Temporary Driver Types ---------------*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_TYPES_H
#define SWIFT_DRIVER_TYPES_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
namespace driver {
namespace types {
  enum ID {
    TY_INVALID,
#define TYPE(NAME, ID, TEMP_SUFFIX, FLAGS) TY_##ID,
#include "swift/Driver/Types.def"
#undef TYPE
    TY_LAST
  };

  /// Return the name of the type for \p Id.
  StringRef getTypeName(ID Id);

  /// Return the suffix to use when creating a temp file of this type,
  /// or null if unspecified.
  StringRef getTypeTempSuffix(ID Id);

  /// Lookup the type to use for the file extension \p Ext.
  ID lookupTypeForExtension(StringRef Ext);

} // end namespace types
} // end namespace driver
} // end namespace swift

#endif
