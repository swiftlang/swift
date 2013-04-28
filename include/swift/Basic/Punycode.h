//===--- Punycode.h - UTF-8 to Punycode transcoding -------------*- C++ -*-===//
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
//
// These functions implement a variant of the Punycode algorithm from RFC3492,
// originally designed for encoding international domain names, for the purpose
// encoding Swift identifiers into mangled symbol names. This version differs
// from RFC3492 in the following respects:
// - '_' is used as the encoding delimiter instead of the '-'.
// - Encoding digits are represented using [a-zA-J] instead of [a-z0-9], because
//   symbol names are case-sensitive, and Swift mangled identifiers cannot begin
//   with a digit.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_BASIC_PUNYCODE_H__
#define __SWIFT_BASIC_PUNYCODE_H__

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
namespace Punycode {

/// Encodes a UTF-8-encoded Unicode string into Punycode.
void encodePunycode(llvm::StringRef inputUTF8,
                    llvm::SmallVectorImpl<char> &outPunycode);

// FIXME: To be written
///// Decodes a Punycode string into a UTF-8-encoded Unicode string.
//void decodePunycode(llvm::StringRef inputPunycode,
//                    llvm::SmallVectorImpl<char> &outUTF8);

} // end namespace Punycode
} // end namespace swift

#endif