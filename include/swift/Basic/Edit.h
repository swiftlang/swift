//===--- Edit.h - Misc edit utilities ---------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
namespace swift {
  class SourceManager;
  class CharSourceRange;

  void writeEdit(SourceManager &SM, CharSourceRange Range, StringRef Text,
                 llvm::raw_ostream &OS);
}
