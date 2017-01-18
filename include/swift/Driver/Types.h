//===--- Types.h - Input & Temporary Driver Types ---------------*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_TYPES_H
#define SWIFT_DRIVER_TYPES_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/StringRef.h"
#include <functional>

namespace swift {
namespace driver {
namespace types {
  enum ID : uint8_t {
#define TYPE(NAME, ID, TEMP_SUFFIX, FLAGS) TY_##ID,
#include "swift/Driver/Types.def"
#undef TYPE
    TY_INVALID
  };

  /// Return the name of the type for \p Id.
  StringRef getTypeName(ID Id);

  /// Return the suffix to use when creating a temp file of this type,
  /// or null if unspecified.
  StringRef getTypeTempSuffix(ID Id);

  /// Lookup the type to use for the file extension \p Ext.
  ID lookupTypeForExtension(StringRef Ext);

  /// Lookup the type to use for the name \p Name.
  ID lookupTypeForName(StringRef Name);

  /// Returns true if the type represents textual data.
  bool isTextual(ID Id);

  /// Returns true if the type is produced in the compiler after the LLVM 
  /// passes.
  ///
  /// For those types the compiler produces multiple output files in multi-
  /// threaded compilation.
  bool isAfterLLVM(ID Id);

  /// Returns true if the type is a file that contributes to the Swift module
  /// being compiled.
  ///
  /// These need to be passed to the Swift frontend 
  bool isPartOfSwiftCompilation(ID Id);

  template <typename Fn>
  void forAllTypes(const Fn &fn);
} // end namespace types
} // end namespace driver
} // end namespace swift

namespace llvm {
  template<>
  struct DenseMapInfo<swift::driver::types::ID> {
    using ID = swift::driver::types::ID;
    static inline ID getEmptyKey() {
      return ID::TY_INVALID;
    }
    static inline ID getTombstoneKey() {
      return static_cast<ID>(ID::TY_INVALID + 1);
    }
    static unsigned getHashValue(ID Val) {
      return (unsigned)Val * 37U;
    }
    static bool isEqual(ID LHS, ID RHS) {
      return LHS == RHS;
    }
  };
}

template <typename Fn>
void swift::driver::types::forAllTypes(const Fn &fn) {
  static_assert(std::is_constructible<std::function<void(types::ID)>,Fn>::value,
                "must have the signature 'void(types::ID)'");
  for (uint8_t i = 0; i < static_cast<uint8_t>(TY_INVALID); ++i)
    fn(static_cast<ID>(i));
}

#endif
