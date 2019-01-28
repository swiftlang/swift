//===--- SourceLoc.h - Source Locations and Ranges --------------*- C++ -*-===//
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
//  This file defines types used to reason about source locations and ranges.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_SOURCELOC_H
#define SWIFT_BASIC_SOURCELOC_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace swift {
  class SourceManager;

/// SourceLoc in swift is an SMLoc and an integer. The integer is used to
/// represent the locations of synthesized AST nodes as being slotted in
/// between actual user-written code locations; in SourceLocs representing
/// actual written source code, it is always zero.
class SourceLoc {
  friend class SourceManager;
  friend class SourceRange;
  friend class CharSourceRange;
  friend class DiagnosticConsumer;

  llvm::SMLoc Value;
  uintptr_t SyntheticLocation = 0;
  
public:
  SourceLoc() {}
  explicit SourceLoc(llvm::SMLoc Value, uintptr_t SyntheticLocation = 0)
    : Value(Value), SyntheticLocation(SyntheticLocation) { }
  
  bool isValid() const { return Value.isValid(); }
  bool isInvalid() const { return !isValid(); }
  
  bool isPhysical() const { return SyntheticLocation == 0; }
  bool isSynthetic() const { return !isPhysical(); }
  
  bool operator==(const SourceLoc &RHS) const {
    return RHS.Value == Value && RHS.SyntheticLocation == SyntheticLocation;
  }
  bool operator!=(const SourceLoc &RHS) const { return !operator==(RHS); }
  
  bool operator<(const SourceLoc &RHS) const {
    if (Value == RHS.Value)
      return SyntheticLocation < RHS.SyntheticLocation;
    else
      return Value.getPointer() < RHS.Value.getPointer();
  }
  
  /// Return a source location advanced a specified number of bytes.
  SourceLoc getAdvancedLoc(int ByteOffset) const {
    assert(isValid() && "Can't advance an invalid location");
    return SourceLoc(
        llvm::SMLoc::getFromPointer(Value.getPointer() + ByteOffset),
                     0);
  }

  SourceLoc getAdvancedLocOrInvalid(int ByteOffset) const {
    if (isValid())
      return getAdvancedLoc(ByteOffset);
    return SourceLoc();
  }

  /// Returns a synthetic \c SourceLoc between \c this and \c Other.
  /// 
  /// \c Other must be attached to the same physical location as \c this,
  /// and must come after \c this.
  SourceLoc getSyntheticLocBefore(SourceLoc Other) {
    assert(Value == Other.Value);
    assert(SyntheticLocation < Other.SyntheticLocation);
    
    uintptr_t maxAdvance = Other.SyntheticLocation - SyntheticLocation;
    
    // We consume only 1/16 of the remaining space to maximum because, in
    // typical use, you're a lot more likely to generate SourceLocs after this
    // one than before it. For the same reason, we also limit the maximum
    // we will advance in one step to 2^24; 16 million synthetic SourceLocs
    // between two nodes ought to be enough for anybody.
    //
    // FIXME: This is quick and dirty; allocating and subdividing synthetic
    // offsets requires much more thought and perhaps a bit of a literature.
    // search.
    uintptr_t advance = maxAdvance / 16;
    if (advance > 1 << 24)
      advance = 1 << 24;
    
    uintptr_t newSynLoc = SyntheticLocation + advance;
    assert(newSynLoc > SyntheticLocation);
    
    return SourceLoc(Value, newSynLoc);
  }

  const void *getOpaquePointerValue() const { return Value.getPointer(); }
  
  uintptr_t getSyntheticLocation() const {
    return SyntheticLocation;
  }

  /// Print out the SourceLoc.  If this location is in the same buffer
  /// as specified by \c LastBufferID, then we don't print the filename.  If
  /// not, we do print the filename, and then update \c LastBufferID with the
  /// BufferID printed.
  void print(raw_ostream &OS, const SourceManager &SM,
             unsigned &LastBufferID) const;

  void printLineAndColumn(raw_ostream &OS, const SourceManager &SM,
                          unsigned BufferID = 0) const;

  void print(raw_ostream &OS, const SourceManager &SM) const {
    unsigned Tmp = ~0U;
    print(OS, SM, Tmp);
  }

  void dump(const SourceManager &SM) const;
};

/// SourceRange in swift is a pair of locations.  However, note that the end
/// location is the start of the last token in the range, not the last character
/// in the range.  This is unlike SMRange, so we use a distinct type to make
/// sure that proper conversions happen where important.
class SourceRange {
public:
  SourceLoc Start, End;

  SourceRange() {}
  SourceRange(SourceLoc Loc) : Start(Loc), End(Loc) {}
  SourceRange(SourceLoc Start, SourceLoc End) : Start(Start), End(End) {
    assert(Start.isValid() == End.isValid() &&
           "Start and end should either both be valid or both be invalid!");
  }
  
  bool isValid() const { return Start.isValid(); }
  bool isInvalid() const { return !isValid(); }

  /// Extend this SourceRange to the smallest continuous SourceRange that
  /// includes both this range and the other one.
  void widen(SourceRange Other);

  bool operator==(const SourceRange &other) const {
    return Start == other.Start && End == other.End;
  }
  bool operator!=(const SourceRange &other) const { return !operator==(other); }

  /// Print out the SourceRange.  If the locations are in the same buffer
  /// as specified by LastBufferID, then we don't print the filename.  If not,
  /// we do print the filename, and then update LastBufferID with the BufferID
  /// printed.
  void print(raw_ostream &OS, const SourceManager &SM,
             unsigned &LastBufferID, bool PrintText = true) const;

  void print(raw_ostream &OS, const SourceManager &SM,
             bool PrintText = true) const {
    unsigned Tmp = ~0U;
    print(OS, SM, Tmp, PrintText);
  }

  void dump(const SourceManager &SM) const;
};

/// A half-open character-based source range.
class CharSourceRange {
  SourceLoc Start;
  unsigned ByteLength;

public:
  /// Constructs an invalid range.
  CharSourceRange() = default;

  CharSourceRange(SourceLoc Start, unsigned ByteLength)
    : Start(Start), ByteLength(ByteLength) {}

  /// Constructs a character range which starts and ends at the
  /// specified character locations.
  CharSourceRange(const SourceManager &SM, SourceLoc Start, SourceLoc End);

  /// Use Lexer::getCharSourceRangeFromSourceRange() instead.
  CharSourceRange(const SourceManager &SM, SourceRange Range) = delete;

  bool isValid() const { return Start.isValid(); }
  bool isInvalid() const { return !isValid(); }

  bool operator==(const CharSourceRange &other) const {
    return Start == other.Start && ByteLength == other.ByteLength;
  }
  bool operator!=(const CharSourceRange &other) const {
    return !operator==(other);
  }

  SourceLoc getStart() const { return Start; }
  SourceLoc getEnd() const { return Start.getAdvancedLocOrInvalid(ByteLength); }

  /// Returns true if the given source location is contained in the range.
  bool contains(SourceLoc loc) const {
    // Recall that !(b < a) is equivalent to (a <= b)
    return !(loc < getStart()) && (loc < getEnd()); 
  }

  bool contains(CharSourceRange Other) const {
    return contains(Other.getStart()) && !(getEnd() < Other.getEnd());
  }

  /// expands *this to cover Other
  void widen(CharSourceRange Other) {
    auto Diff = Other.getEnd().Value.getPointer() - getEnd().Value.getPointer();
    if (Diff > 0) {
      ByteLength += Diff;
    }
    const auto MyStartPtr = getStart().Value.getPointer();
    Diff = MyStartPtr - Other.getStart().Value.getPointer();
    if (Diff > 0) {
      ByteLength += Diff;
      Start = SourceLoc(llvm::SMLoc::getFromPointer(MyStartPtr - Diff));
    }
  }

  bool overlaps(CharSourceRange Other) const {
    if (getByteLength() == 0 || Other.getByteLength() == 0) return false;
    return contains(Other.getStart()) || Other.contains(getStart());
  }

  StringRef str() const {
    return StringRef(Start.Value.getPointer(), ByteLength);
  }

  /// Return the length of this valid range in bytes.  Can be zero.
  unsigned getByteLength() const {
    assert(isValid() && "length does not make sense for an invalid range");
    return ByteLength;
  }
  
  /// Print out the CharSourceRange.  If the locations are in the same buffer
  /// as specified by LastBufferID, then we don't print the filename.  If not,
  /// we do print the filename, and then update LastBufferID with the BufferID
  /// printed.
  void print(raw_ostream &OS, const SourceManager &SM,
             unsigned &LastBufferID, bool PrintText = true) const;

  void print(raw_ostream &OS, const SourceManager &SM,
             bool PrintText = true) const {
    unsigned Tmp = ~0U;
    print(OS, SM, Tmp, PrintText);
  }
  
  void dump(const SourceManager &SM) const;
};

} // end namespace swift

namespace llvm {
template <typename T> struct DenseMapInfo;

template <> struct DenseMapInfo<swift::SourceLoc> {
  static swift::SourceLoc getEmptyKey() {
    return swift::SourceLoc(
        SMLoc::getFromPointer(DenseMapInfo<const char *>::getEmptyKey()));
  }

  static swift::SourceLoc getTombstoneKey() {
    // Make this different from empty key. See for context:
    // http://lists.llvm.org/pipermail/llvm-dev/2015-July/088744.html
    return swift::SourceLoc(
        SMLoc::getFromPointer(DenseMapInfo<const char *>::getTombstoneKey()));
  }

  static unsigned getHashValue(const swift::SourceLoc &Val) {
    return hash_combine( 
      DenseMapInfo<const void *>::getHashValue(Val.getOpaquePointerValue()),
      DenseMapInfo<uintptr_t>::getHashValue(Val.getSyntheticLocation())
    );
  }

  static bool isEqual(const swift::SourceLoc &LHS,
                      const swift::SourceLoc &RHS) {
    return LHS == RHS;
  }
};

template <> struct DenseMapInfo<swift::SourceRange> {
  static swift::SourceRange getEmptyKey() {
    return swift::SourceRange(swift::SourceLoc(
        SMLoc::getFromPointer(DenseMapInfo<const char *>::getEmptyKey())));
  }

  static swift::SourceRange getTombstoneKey() {
    // Make this different from empty key. See for context:
    // http://lists.llvm.org/pipermail/llvm-dev/2015-July/088744.html
    return swift::SourceRange(swift::SourceLoc(
        SMLoc::getFromPointer(DenseMapInfo<const char *>::getTombstoneKey())));
  }

  static unsigned getHashValue(const swift::SourceRange &Val) {
    return hash_combine(DenseMapInfo<const void *>::getHashValue(
                            Val.Start.getOpaquePointerValue()),
                        DenseMapInfo<const void *>::getHashValue(
                            Val.End.getOpaquePointerValue()));
  }

  static bool isEqual(const swift::SourceRange &LHS,
                      const swift::SourceRange &RHS) {
    return LHS == RHS;
  }
};
} // namespace llvm

#endif // SWIFT_BASIC_SOURCELOC_H
