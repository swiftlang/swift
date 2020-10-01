//===--- QuotedString.cpp - Printing a string as a quoted string ----------===//
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

#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/QuotedString.h"

using namespace swift;

void swift::printAsQuotedString(llvm::raw_ostream &out, llvm::StringRef text) {
  out << '"';
  for (auto C : text) {
    switch (C) {
    case '\\': out << "\\\\"; break;
    case '\t': out << "\\t"; break;
    case '\n': out << "\\n"; break;
    case '\r': out << "\\r"; break;
    case '"': out << "\\\""; break;
    case '\'': out << '\''; break; // no need to escape these
    case '\0': out << "\\0"; break;
    default:
      auto c = (unsigned char)C;
      // Other ASCII control characters should get escaped.
      if (c < 0x20 || c == 0x7F) {
        static const char hexdigit[] = {
          '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
          'A', 'B', 'C', 'D', 'E', 'F'
        };
        out << "\\u{" << hexdigit[c >> 4] << hexdigit[c & 0xF] << '}';
      } else {
        out << (char)c;
      }
      break;
    }
  }
  out << '"';
}
