//===- SourceLoc.h - Source Locations and Ranges ----------------*- C++ -*-===//
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
//  This file defines types used to reason about source locations and ranges.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SOURCELOC_H
#define SWIFT_SOURCELOC_H

#include "swift/Basic/LLVM.h"
#include "llvm/Support/SMLoc.h"

namespace llvm {
  class SourceMgr;
}
namespace swift {

/// SourceLoc in swift is just an SMLoc.  We define it as a different type
/// (instead of as a typedef) just to remove the "getFromPointer" methods and
/// enforce purity in the Swift codebase.
class SourceLoc {
public:
  llvm::SMLoc Value;
  
  SourceLoc() {}
  explicit SourceLoc(llvm::SMLoc Value) : Value(Value) {}
  
  bool isValid() const { return Value.isValid(); }
  bool isInvalid() const { return !Value.isValid(); }
  
  bool operator==(const SourceLoc &RHS) const { return RHS.Value == Value; }
  bool operator!=(const SourceLoc &RHS) const { return RHS.Value != Value; }
  
  /// getAdvanced - Return a source location advanced a specified number of
  /// characters.
  SourceLoc getAdvancedLoc(int NumCharacters) const {
    assert(isValid() && "Can't advance an invalid location");
    return SourceLoc(llvm::SMLoc::getFromPointer(Value.getPointer() +
                                                 NumCharacters));
  }

  /// print - Print out the SourceLoc.  If this location is in the same buffer
  /// as specified by LastBuffer, then we don't print the filename.  If not, we
  /// do print the filename, and then update LastBuffer with the BufferID
  /// printed.
  void print(raw_ostream &OS, const llvm::SourceMgr &SM,
             int &LastBuffer, int &LastLine) const;

  void print(raw_ostream &OS, const llvm::SourceMgr &SM) const {
    int TmpBuf = -1, TmpLine = -1;
    print(OS, SM, TmpBuf, TmpLine);
  }
  void dump(const llvm::SourceMgr &SM) const;
};

/// SourceRange in swift is a pair of locations.  However, note that the end
/// location is the start of the last token in the range, not the last character
/// in the range.  This is unlike SMRange, so we use a distinct type to make
/// sure that proper conversions happen where important.
class SourceRange {
public:
  SourceLoc Start, End;

  SourceRange() {}
  SourceRange(SourceLoc Loc) : Start(Loc), End(Loc) { }
  SourceRange(SourceLoc Start, SourceLoc End) : Start(Start), End(End) {
    assert(Start.isValid() == End.isValid() &&
           "Start and end should either both be valid or both be invalid!");
  }
  
  bool isValid() const { return Start.isValid(); }
  bool isInvalid() const { return Start.isInvalid(); }

  /// print - Print out the SourceRange. If the locations are in the same buffer
  /// as specified by LastBuffer, then we don't print the filename.  If not, we
  /// do print the filename, and then update LastBuffer with the BufferID
  /// printed.
  void print(raw_ostream &OS, const llvm::SourceMgr &SM,
             int &LastBuffer, int &LastLine, bool PrintText) const;

  void print(raw_ostream &OS, const llvm::SourceMgr &SM) const {
    int TmpBuf = -1, TmpLine = -1;
    print(OS, SM, TmpBuf, TmpLine, /*PrintText=*/true);
  }
  void dump(const llvm::SourceMgr &SM) const;
};

} // end namespace swift

#endif
