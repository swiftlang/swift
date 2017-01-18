//===--- Edit.h - Misc edit utilities ---------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_EDIT_H
#define SWIFT_BASIC_EDIT_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {
  class SourceManager;
  class CharSourceRange;

  struct SingleEdit {
    SourceManager &SM;
    CharSourceRange Range;
    std::string Text;
  };

  void writeEditsInJson(ArrayRef<SingleEdit> Edits, llvm::raw_ostream &OS);
}

#endif // SWIFT_BASIC_EDIT_H
