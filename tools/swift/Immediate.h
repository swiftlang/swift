//===-- Immediate.h - Entry point for swift immediate mode ----------------===//
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
// This is the entry point to the swift immediate mode, which takes a
// TranslationUnit, and runs it immediately using the JIT.
//
//===----------------------------------------------------------------------===//

#include <string>

namespace swift {
  class TranslationUnit;

  void RunImmediately(TranslationUnit *TU);
}
