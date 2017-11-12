//===--- ColorUtils.h - -----------------------------------------*- C++ -*-===//
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
// This file defines an 'OSColor' class for helping printing colorful outputs
// to the terminal.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_COLORUTILS_H
#define SWIFT_BASIC_COLORUTILS_H

#include "llvm/Support/raw_ostream.h"

namespace swift {

/// RAII class for setting a color for a raw_ostream and resetting when it goes
/// out-of-scope.
class OSColor {
  llvm::raw_ostream &OS;
  bool HasColors;
public:
  OSColor(llvm::raw_ostream &OS, llvm::raw_ostream::Colors Color) : OS(OS) {
    HasColors = OS.has_colors();
    if (HasColors)
      OS.changeColor(Color);
  }
  ~OSColor() {
    if (HasColors)
      OS.resetColor();
  }

  OSColor &operator<<(char C) { OS << C; return *this; }
  OSColor &operator<<(llvm::StringRef Str) { OS << Str; return *this; }
};

} // namespace swift

#endif // SWIFT_BASIC_COLORUTILS_H
