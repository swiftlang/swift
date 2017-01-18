//===--- XMLUtils.h - Various XML utility routines --------------*- C++ -*-===//
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

#ifndef SWIFT_MARKUP_XML_UTILS_H
#define SWIFT_MARKUP_XML_UTILS_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
namespace markup {

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

// FIXME: copied from Clang's
// CommentASTToXMLConverter::appendToResultWithCDATAEscaping
static inline void appendWithCDATAEscaping(raw_ostream &OS, StringRef S) {
  if (S.empty())
    return;

  OS << "<![CDATA[";
  while (!S.empty()) {
    size_t Pos = S.find("]]>");
    if (Pos == 0) {
      OS << "]]]]><![CDATA[>";
      S = S.drop_front(3);
      continue;
    }
    if (Pos == StringRef::npos)
      Pos = S.size();

    OS << S.substr(0, Pos);

    S = S.drop_front(Pos);
  }
  OS << "]]>";
}

} // namespace markup
} // namespace swift

#endif // SWIFT_MARKUP_XML_UTILS_H

