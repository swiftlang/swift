//===--- XMLUtils.h - Various XML utility routines ------------------------===//
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

#ifndef LLVM_REST_XML_UTILS_H
#define LLVM_REST_XML_UTILS_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {
namespace rest {

// FIXME: copied from Clang's
// CommentASTToXMLConverter::appendToResultWithXMLEscaping
static inline void appendWithXMLEscaping(raw_ostream &OS, StringRef S) {
  for (const char C : S) {
    switch (C) {
    case '&':
      OS << "&amp;";
      break;
    case '<':
      OS << "&lt;";
      break;
    case '>':
      OS << "&gt;";
      break;
    case '"':
      OS << "&quot;";
      break;
    case '\'':
      OS << "&apos;";
      break;
    default:
      OS << C;
      break;
    }
  }
}

} // namespace rest
} // namespace llvm

#endif // LLVM_REST_XML_UTILS_H

