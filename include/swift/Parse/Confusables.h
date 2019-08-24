//===--- Confusables.h - Swift Confusable Character Diagnostics -*- C++ -*-===//
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

#ifndef SWIFT_CONFUSABLES_H
#define SWIFT_CONFUSABLES_H

#include <stdint.h>

namespace swift {
namespace confusable {
  /// Given a UTF-8 codepoint, determines whether it appears on the Unicode
  /// specification table of confusable characters and maps to punctuation,
  /// and either returns either the expected ASCII character or 0.
  char tryConvertConfusableCharacterToASCII(uint32_t codepoint);
}
}

#endif
