//===--- Punycode.h - UTF-8 to Punycode transcoding -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// These functions implement a variant of the Punycode algorithm from RFC3492,
// originally designed for encoding international domain names, for the purpose
// of encoding Swift identifiers into mangled symbol names. This version differs
// from RFC3492 in the following respects:
// - '_' is used as the encoding delimiter instead of '-'.
// - Encoding digits are represented using [a-zA-J] instead of [a-z0-9], because
//   symbol names are case-sensitive, and Swift mangled identifiers cannot begin
//   with a digit.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_PUNYCODE_H
#define SWIFT_BASIC_PUNYCODE_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallVector.h"
#include <vector>
#include <cstdint>

namespace swift {
namespace Punycode {

/// Encodes a sequence of code points into Punycode.
///
/// Returns false if input contains surrogate code points.
bool encodePunycode(const std::vector<uint32_t> &InputCodePoints,
                    std::string &OutPunycode);

/// Decodes a Punycode string into a sequence of Unicode scalars.
///
/// Returns false if decoding failed.
bool decodePunycode(StringRef InputPunycode,
                    std::vector<uint32_t> &OutCodePoints);

bool encodePunycodeUTF8(StringRef InputUTF8, std::string &OutPunycode);

bool decodePunycodeUTF8(StringRef InputPunycode, std::string &OutUTF8);

} // end namespace Punycode
} // end namespace swift

#endif // SWIFT_BASIC_PUNYCODE_H

