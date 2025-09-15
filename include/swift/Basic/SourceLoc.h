//===--- SourceLoc.h - Source Locations and Ranges --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
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

/// `SourceLoc.h` is imported into Swift. Be *very* careful with what you
/// include here and keep these includes minimal!
/// If you don't need to import a header into Swift, include it in the `#ifdef`
/// block below instead.
///
/// See include guidelines and caveats in `BasicBridging.h`.
#include "swift/Basic/SwiftBridging.h"
#include <assert.h>
#include <stdint.h>

// Not imported into Swift in pure bridging mode.
#ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <functional>

#endif // #ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

namespace swift {
class SourceManager;

/// `SourceLoc` just wraps a `const char *`.  We define it as a different type
/// (instead of as a typedef) from `llvm::SMLoc` to enforce purity in the
/// Swift codebase.
class SourceLoc {
  friend class SourceManager;
  friend class SourceRange;
  friend class CharSourceRange;
  friend class DiagnosticConsumer;

  const char *_Nullable Pointer = nullptr;

public:
  SourceLoc() {}
#ifdef COMPILED_WITH_SWIFT
  SWIFT_NAME("init(raw:)")
  SourceLoc(const void *_Nullable Pointer) : Pointer((const char *)Pointer) {}
#endif

  SWIFT_UNAVAILABLE("Use 'init(raw:)' instead")
  static SourceLoc getFromPointer(const char *_Nullable Pointer) {
    SourceLoc Loc;
    Loc.Pointer = Pointer;
    return Loc;
  }

  SWIFT_UNAVAILABLE("Use 'raw' instead")
  const char *_Nullable getPointer() const { return Pointer; }
  SWIFT_UNAVAILABLE("Use 'raw' instead")
  const void *_Nullable getOpaquePointerValue() const { return Pointer; }
#ifdef COMPILED_WITH_SWIFT
  SWIFT_COMPUTED_PROPERTY
  const void *_Nullable getRaw() const { return getOpaquePointerValue(); }
#endif

  bool isValid() const { return Pointer != nullptr; }
  bool isInvalid() const { return !isValid(); }

  /// Return a source location advanced a specified number of bytes.
  SWIFT_NAME("advanced(by:)")
  SourceLoc getAdvancedLoc(int ByteOffset) const {
    assert(isValid() && "Can't advance an invalid location");
    return SourceLoc::getFromPointer(Pointer + ByteOffset);
  }

  SWIFT_UNAVAILABLE("Unavailable in Swift")
  SourceLoc getAdvancedLocOrInvalid(int ByteOffset) const {
    if (isValid())
      return getAdvancedLoc(ByteOffset);
    return SourceLoc();
  }

// Not imported into Swift in pure bridging mode.
#ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

  /// An explicit bool operator so one can check if a SourceLoc is valid in an
  /// if statement:
  ///
  /// if (auto x = getSourceLoc()) { ... }
  explicit operator bool() const { return isValid(); }

  operator llvm::SMLoc() const { return llvm::SMLoc::getFromPointer(Pointer); }

  bool operator==(const SourceLoc &RHS) const { return RHS.Pointer == Pointer; }
  bool operator!=(const SourceLoc &RHS) const { return !operator==(RHS); }

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

  SWIFT_DEBUG_DUMPER(dump(const SourceManager &SM));

  friend size_t hash_value(SourceLoc loc) {
    return reinterpret_cast<uintptr_t>(loc.getOpaquePointerValue());
  }

  friend void simple_display(raw_ostream &OS, const SourceLoc &loc) {
    // Nothing meaningful to print.
  }

#endif // #ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE
};

/// SourceRange in swift is a pair of locations.  However, note that the end
/// location is the start of the last token in the range, not the last character
/// in the range.  This is unlike SMRange, so we use a distinct type to make
/// sure that proper conversions happen where important.
class SourceRange {
public:
  SWIFT_NAME("start")
  SourceLoc Start;

  SWIFT_NAME("end")
  SourceLoc End;

  SourceRange() {}

  SWIFT_NAME("init(start:)")
  SourceRange(SourceLoc Loc) : Start(Loc), End(Loc) {}

  SWIFT_NAME("init(start:end:)")
  SourceRange(SourceLoc Start, SourceLoc End) : Start(Start), End(End) {
    assert(Start.isValid() == End.isValid() &&
           "Start and end should either both be valid or both be invalid!");
  }
  
  bool isValid() const { return Start.isValid(); }
  bool isInvalid() const { return !isValid(); }

// Not imported into Swift in pure bridging mode.
#ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

  /// An explicit bool operator so one can check if a SourceRange is valid in an
  /// if statement:
  ///
  /// if (auto x = getSourceRange()) { ... }
  explicit operator bool() const { return isValid(); }

  /// Combine the given source ranges into the smallest contiguous SourceRange
  /// that includes them all, ignoring any invalid ranges present.
  static SourceRange combine(ArrayRef<SourceRange> ranges);

  /// Combine the given source ranges into the smallest contiguous SourceRange
  /// that includes them all, ignoring any invalid ranges present.
  template <typename ...T>
  static SourceRange combine(T... ranges) {
    return SourceRange::combine({ranges...});
  }

  /// Extend this SourceRange to the smallest continuous SourceRange that
  /// includes both this range and the other one.
  void widen(SourceRange Other);

  /// Checks whether this range contains the given location. Note that the given
  /// location should correspond to the start of a token, since locations inside
  /// the last token may be considered outside the range by this function.
  bool contains(SourceLoc Loc) const;

  /// Checks whether this range overlaps with the given range.
  bool overlaps(SourceRange Other) const;

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

  SWIFT_DEBUG_DUMPER(dump(const SourceManager &SM));

  friend size_t hash_value(SourceRange range) {
    return llvm::hash_combine(range.Start, range.End);
  }

  friend void simple_display(raw_ostream &OS, const SourceRange &loc) {
    // Nothing meaningful to print.
  }

#endif // #ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE
};

/// A half-open character-based source range.
class CharSourceRange {
  SourceLoc Start;
  unsigned ByteLength;

public:
  /// Constructs an invalid range.
  CharSourceRange() = default;

  SWIFT_NAME("init(start:byteLength:)")
  CharSourceRange(SourceLoc Start, unsigned ByteLength)
    : Start(Start), ByteLength(ByteLength) {}

// Not imported into Swift in pure bridging mode.
#ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

  /// Constructs a character range which starts and ends at the
  /// specified character locations.
  CharSourceRange(const SourceManager &SM, SourceLoc Start, SourceLoc End);

  /// Use Lexer::getCharSourceRangeFromSourceRange() instead.
  CharSourceRange(const SourceManager &SM, SourceRange Range) = delete;

#endif // #ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

  bool isValid() const { return Start.isValid(); }
  bool isInvalid() const { return !isValid(); }

  SWIFT_COMPUTED_PROPERTY
  SourceLoc getStart() const { return Start; }
  SWIFT_COMPUTED_PROPERTY
  SourceLoc getEnd() const { return Start.getAdvancedLocOrInvalid(ByteLength); }

  /// Return the length of this valid range in bytes.  Can be zero.
  SWIFT_COMPUTED_PROPERTY
  unsigned getByteLength() const {
    assert(isValid() && "length does not make sense for an invalid range");
    return ByteLength;
  }

// Not imported into Swift in pure bridging mode.
#ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

  /// Returns true if the given source location is contained in the range.
  bool contains(SourceLoc loc) const {
    auto less = std::less<const char *>();
    auto less_equal = std::less_equal<const char *>();
    return less_equal(getStart().Pointer, loc.Pointer) &&
           less(loc.Pointer, getEnd().Pointer);
  }

  bool contains(CharSourceRange Other) const {
    auto less_equal = std::less_equal<const char *>();
    return contains(Other.getStart()) &&
           less_equal(Other.getEnd().Pointer, getEnd().Pointer);
  }

  /// expands *this to cover Other
  void widen(CharSourceRange Other) {
    auto Diff = Other.getEnd().Pointer - getEnd().Pointer;
    if (Diff > 0) {
      ByteLength += Diff;
    }
    const auto MyStartPtr = getStart().Pointer;
    Diff = MyStartPtr - Other.getStart().Pointer;
    if (Diff > 0) {
      ByteLength += Diff;
      Start = SourceLoc::getFromPointer(MyStartPtr - Diff);
    }
  }

  bool overlaps(CharSourceRange Other) const {
    if (getByteLength() == 0 || Other.getByteLength() == 0) return false;
    return contains(Other.getStart()) || Other.contains(getStart());
  }

  StringRef str() const { return StringRef(Start.Pointer, ByteLength); }

  bool operator==(const CharSourceRange &other) const {
    return Start == other.Start && ByteLength == other.ByteLength;
  }
  bool operator!=(const CharSourceRange &other) const {
    return !operator==(other);
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

  SWIFT_DEBUG_DUMPER(dump(const SourceManager &SM));

#endif // #ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE
};

} // end namespace swift

// Not imported into Swift in pure bridging mode.
#ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

namespace llvm {
template <typename T, typename Enable> struct DenseMapInfo;

template <> struct DenseMapInfo<swift::SourceLoc> {
  static swift::SourceLoc getEmptyKey() {
    return swift::SourceLoc::getFromPointer(
        DenseMapInfo<const char *>::getEmptyKey());
  }

  static swift::SourceLoc getTombstoneKey() {
    // Make this different from empty key. See for context:
    // http://lists.llvm.org/pipermail/llvm-dev/2015-July/088744.html
    return swift::SourceLoc::getFromPointer(
        DenseMapInfo<const char *>::getTombstoneKey());
  }

  static unsigned getHashValue(const swift::SourceLoc &Val) {
    return DenseMapInfo<const void *>::getHashValue(
        Val.getOpaquePointerValue());
  }

  static bool isEqual(const swift::SourceLoc &LHS,
                      const swift::SourceLoc &RHS) {
    return LHS == RHS;
  }
};

template <> struct DenseMapInfo<swift::SourceRange> {
  static swift::SourceRange getEmptyKey() {
    return swift::SourceRange(swift::SourceLoc::getFromPointer(
        DenseMapInfo<const char *>::getEmptyKey()));
  }

  static swift::SourceRange getTombstoneKey() {
    // Make this different from empty key. See for context:
    // http://lists.llvm.org/pipermail/llvm-dev/2015-July/088744.html
    return swift::SourceRange(swift::SourceLoc::getFromPointer(
        DenseMapInfo<const char *>::getTombstoneKey()));
  }

  static unsigned getHashValue(const swift::SourceRange &Val) {
    return hash_combine(Val.Start.getOpaquePointerValue(),
                        Val.End.getOpaquePointerValue());
  }

  static bool isEqual(const swift::SourceRange &LHS,
                      const swift::SourceRange &RHS) {
    return LHS == RHS;
  }
};
} // namespace llvm

#endif // #ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_BASIC_SOURCELOC_H
