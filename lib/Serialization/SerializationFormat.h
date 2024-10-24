//===--- SerializationFormat.h - Serialization helpers ------ ---*- C++ -*-===//
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
///
/// \file
/// Various helper functions to deal with common serialization functionality.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_SERIALIZATIONFORMAT_H
#define SWIFT_SERIALIZATION_SERIALIZATIONFORMAT_H

#include "llvm/ADT/bit.h"
#include "llvm/Support/Endian.h"

namespace swift {

template <typename value_type, typename CharT>
[[nodiscard]] inline value_type readNext(const CharT *&memory) {
  return llvm::support::endian::readNext<value_type, llvm::endianness::little, llvm::support::unaligned>(memory);
}

} // end namespace swift

#endif
