//===--- CaptureInfo.h - Data Structure for Capture Lists -------*- C++ -*-===//
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

#ifndef SWIFT_AST_CAPTURE_INFO_H
#define SWIFT_AST_CAPTURE_INFO_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include <vector>

namespace llvm {
class raw_ostream;
}

namespace swift {
class ValueDecl;

/// \brief Stores information about captured variables.
class CaptureInfo {
  ArrayRef<ValueDecl *> Captures;

public:
  bool empty() { return Captures.empty(); }

  ArrayRef<ValueDecl *> getCaptures() const { return Captures; }
  void setCaptures(ArrayRef<ValueDecl *> C) { Captures = C; }

  /// \brief Return a filtered list of the captures for this function,
  /// filtering out global variables.  This function returns the list that
  /// actually needs to be closed over.
  std::vector<ValueDecl *> getLocalCaptures() const;

  /// \returns true if getLocalCaptures() will return a non-empty list.
  bool hasLocalCaptures() const;

  void dump() const;
  void print(raw_ostream &OS) const;
};

} // namespace swift

#endif // LLVM_SWIFT_AST_CAPTURE_INFO_H

