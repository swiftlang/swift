//===--- Punycode.h - UTF-8 to Punycode transcoding -------------*- C++ -*-===//
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
// These functions implement a variant of the Punycode algorithm from RFC3492,
// originally designed for encoding international domain names, for the purpose
// of encoding Swift identifiers into mangled symbol names. This version differs
// from RFC3492 in the following respects:
// - '_' is used as the encoding delimiter instead of '-'.
// - Encoding digits are represented using [a-zA-J] instead of [a-z0-9], because
//   symbol names are case-sensitive, and Swift mangled identifiers cannot begin
//   with a digit.
// - Optionally, non-symbol ASCII characters (characters except [$_a-zA-Z0-9])
//   are mapped to the code range 0xD800 - 0xD880 and are also encoded like
//   non-ASCII unicode characters.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_PUNYCODE_H
#define SWIFT_DEMANGLING_PUNYCODE_H

#include "llvm/ADT/StringRef.h"
#include "swift/Demangling/NamespaceMacros.h"
#include <vector>
#include <cstdint>

namespace swift {
namespace Punycode {
SWIFT_BEGIN_INLINE_NAMESPACE

using llvm::StringRef;

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

/// Encodes an UTF8 string into Punycode.
///
/// If \p mapNonSymbolChars is true, non-symbol ASCII characters (characters
/// except [$_a-zA-Z0-9]) are also encoded like non-ASCII unicode characters.
///
/// If \p repairInvalidUTF8 is true, invalid UTF-8 sequences will be replaced
/// with one or more Unicode replacement characters.
///
/// Returns false if \p InputUTF8 contains surrogate code points and
/// repairInvalidUTF8 is false.
bool encodePunycodeUTF8(StringRef InputUTF8, std::string &OutPunycode,
                        bool mapNonSymbolChars = false,
                        bool repairInvalidUTF8 = false);

bool decodePunycodeUTF8(StringRef InputPunycode, std::string &OutUTF8);

SWIFT_END_INLINE_NAMESPACE
} // end namespace Punycode
} // end namespace swift

#endif // SWIFT_DEMANGLING_PUNYCODE_H

