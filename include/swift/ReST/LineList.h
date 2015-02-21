//===--- LineList.h - Data structures for ReST parsing --------------------===//
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

#ifndef LLVM_REST_LINELIST_H
#define LLVM_REST_LINELIST_H

#include "swift/Basic/LLVM.h"
#include "swift/ReST/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {
namespace rest {
class ReSTContext;
class LineListRefIndex;

/// A 0-based column number.
struct ColumnNum {
private:
  explicit ColumnNum(unsigned Value) : Value(Value) {}

public:
  unsigned Value;

  static ColumnNum make(unsigned Value) {
    return ColumnNum(Value);
  }

  ColumnNum() : Value(0) {}
  ColumnNum(const ColumnNum &) = default;

  ColumnNum &operator=(const ColumnNum &) = default;

  ColumnNum &operator++() { Value++; return *this; }
  ColumnNum operator++(int) { return ColumnNum(Value++); }

  bool operator==(ColumnNum RHS) const { return Value == RHS.Value; }
  bool operator!=(ColumnNum RHS) const { return !(*this == RHS); }

  bool operator<(ColumnNum RHS) const  { return Value <  RHS.Value; }
  bool operator<=(ColumnNum RHS) const { return Value <= RHS.Value; }
  bool operator>(ColumnNum RHS) const  { return Value >  RHS.Value; }
  bool operator>=(ColumnNum RHS) const { return Value >= RHS.Value; }

  ColumnNum operator+(ColumnNum RHS) const {
    return ColumnNum(Value + RHS.Value);
  }

  ColumnNum &operator+=(unsigned RHS) {
    *this = *this + ColumnNum::make(RHS);
    return *this;
  }

  ColumnNum nextTabStop() const {
    return ColumnNum(((Value + 8) / 8) * 8);
  }
};

namespace detail {

enum class LineKind {
  Unknown,
  Blank,
  BulletListAsterisk,
  BulletListPlus,
  BulletListHyphenMinus,
  BulletListBullet,
  BulletListTriangularBullet,
  BulletListHyphenBullet,
  EnumeratedListArabic,
  EnumeratedListUppercaseAlphabet,
  EnumeratedListLowercaseAlphabet,
  EnumeratedListUppercaseRoman,
  EnumeratedListLowercaseRoman,
  EnumeratedListUppercaseAmbiguous,
  EnumeratedListLowercaseAmbiguous,
  EnumeratedListAuto,
  FieldList,
};

static inline bool isBullet(LineKind K) {
  switch (K) {
  case LineKind::Unknown:
  case LineKind::Blank:
    return false;
  case LineKind::BulletListAsterisk:
  case LineKind::BulletListPlus:
  case LineKind::BulletListHyphenMinus:
  case LineKind::BulletListBullet:
  case LineKind::BulletListTriangularBullet:
  case LineKind::BulletListHyphenBullet:
    return true;
  case LineKind::EnumeratedListArabic:
  case LineKind::EnumeratedListUppercaseAlphabet:
  case LineKind::EnumeratedListLowercaseAlphabet:
  case LineKind::EnumeratedListUppercaseRoman:
  case LineKind::EnumeratedListLowercaseRoman:
  case LineKind::EnumeratedListUppercaseAmbiguous:
  case LineKind::EnumeratedListLowercaseAmbiguous:
  case LineKind::EnumeratedListAuto:
  case LineKind::FieldList:
    return false;
  }
  llvm_unreachable("bad LineKind");
}

static inline bool isEnumerated(LineKind K) {
  switch (K) {
  case LineKind::Unknown:
  case LineKind::Blank:
  case LineKind::BulletListAsterisk:
  case LineKind::BulletListPlus:
  case LineKind::BulletListHyphenMinus:
  case LineKind::BulletListBullet:
  case LineKind::BulletListTriangularBullet:
  case LineKind::BulletListHyphenBullet:
    return false;
  case LineKind::EnumeratedListArabic:
  case LineKind::EnumeratedListUppercaseAlphabet:
  case LineKind::EnumeratedListLowercaseAlphabet:
  case LineKind::EnumeratedListUppercaseRoman:
  case LineKind::EnumeratedListLowercaseRoman:
  case LineKind::EnumeratedListUppercaseAmbiguous:
  case LineKind::EnumeratedListLowercaseAmbiguous:
  case LineKind::EnumeratedListAuto:
    return true;
  case LineKind::FieldList:
    return false;
  }
  llvm_unreachable("bad LineKind");
}

enum class EnumeratorStyleKind {
  DotAfter,
  SurroundedByParens,
  ParenAfter,
};

struct LineClassification {
private:
  struct EnumeratedExtraData {
    unsigned EnumeratorStyle : 2;
    unsigned HasTextAfterEnumerator : 1;
    unsigned EnumeratorAndWhitespaceBytes : 10;
  };

  struct FieldListExtraData {
    unsigned FieldNameBytes : 10;
    unsigned FieldMarkerAndWhitespaceBytes : 10;
  };

  union {
    unsigned ExtraData;
    EnumeratedExtraData EnumeratedData;
    FieldListExtraData FieldListData;
  };

public:
  LineKind Kind;

  LineClassification() : Kind(LineKind::Unknown) {}
  LineClassification(const LineClassification &) = default;

  LineClassification &operator=(const LineClassification &) = default;

  unsigned getBulletAndWhitespaceBytes() const {
    assert(isBullet(Kind));
    return ExtraData;
  }

  EnumeratorStyleKind getEnumeratorStyle() const {
    return static_cast<EnumeratorStyleKind>(EnumeratedData.EnumeratorStyle);
  }

  bool hasTextAfterEnumerator() const {
    assert(isEnumerated(Kind));
    return EnumeratedData.HasTextAfterEnumerator;
  }

  unsigned getEnumeratorAndWhitespaceBytes() const {
    assert(isEnumerated(Kind));
    return EnumeratedData.EnumeratorAndWhitespaceBytes;
  }

  ColumnNum getEnumeratorAndWhitespaceCols() const {
    // Reimplement this function if we allow Unicode characters in enumerators
    // or in whitespace.
    return ColumnNum::make(getEnumeratorAndWhitespaceBytes());
  }

  unsigned getFieldNameBytes() const {
    assert(Kind == LineKind::FieldList);
    return FieldListData.FieldNameBytes;
  }

  unsigned getFieldMarkerAndWhitespaceBytes() const {
    assert(Kind == LineKind::FieldList);
    return FieldListData.FieldMarkerAndWhitespaceBytes;
  }

  static LineClassification makeUnknown() { return LineClassification(); }

  static LineClassification makeBlank() {
    LineClassification Result;
    Result.Kind = LineKind::Blank;
    return Result;
  }

  static LineClassification makeBullet(LineKind Kind,
                                       unsigned BulletAndWhitespaceBytes) {
    assert(isBullet(Kind));
    LineClassification Result;
    Result.Kind = Kind;
    Result.ExtraData = BulletAndWhitespaceBytes;
    return Result;
  }

  static LineClassification
  makeEnumerated(LineKind Kind, EnumeratorStyleKind EnumeratorStyle,
                 bool HasTextAfterEnumerator,
                 unsigned EnumeratorAndWhitespaceBytes) {
    assert(isEnumerated(Kind));
    LineClassification Result;
    Result.Kind = Kind;
    Result.EnumeratedData.EnumeratorStyle =
        static_cast<unsigned>(EnumeratorStyle);
    Result.EnumeratedData.HasTextAfterEnumerator = HasTextAfterEnumerator;
    Result.EnumeratedData.EnumeratorAndWhitespaceBytes =
        EnumeratorAndWhitespaceBytes;
    return Result;
  }

  static LineClassification
  makeFieldList(unsigned FieldNameBytes,
                unsigned FieldMarkerAndWhitespaceBytes) {
    LineClassification Result;
    Result.Kind = LineKind::FieldList;
    Result.FieldListData.FieldNameBytes = FieldNameBytes;
    Result.FieldListData.FieldMarkerAndWhitespaceBytes =
        FieldMarkerAndWhitespaceBytes;
    return Result;
  }
};

} // namespace detail

struct LinePart {
  StringRef Text;
  SourceRange Range;
};

struct Line {
  StringRef Text;
  SourceRange Range;
  ColumnNum FirstTextCol;
  unsigned FirstTextByte : 10;
  mutable unsigned ClassificationComputed : 1;

private:
  mutable detail::LineClassification Classification;

  void computeClassification() const;

public:
  const detail::LineClassification &getClassification() const {
    if (ClassificationComputed)
      return Classification;
    computeClassification();
    ClassificationComputed = true;
    return Classification;
  }

  Line() : FirstTextByte(0), ClassificationComputed(0) {}
};

class LineListRef;

class LineList {
  MutableArrayRef<Line> Lines;

public:
  LineList(MutableArrayRef<Line> Lines) : Lines(Lines) {}

  LineList(const LineList &) = default;
  LineList &operator=(const LineList &) = default;

  Line &operator[](unsigned i) {
    assert(i < Lines.size());
    return Lines[i];
  }

  const Line &operator[](unsigned i) const {
    assert(i < Lines.size());
    return Lines[i];
  }

  unsigned size() const { return Lines.size(); }

  LineListRef subList(unsigned StartIndex, unsigned Size) const;

  bool isPreviousLineBlank(unsigned i) const {
    // [ReST/Syntax Details/Whitespace/Blank Lines]
    // Quote:
    //     The first line of a document is treated as if it is preceded by a
    //     blank line, [...]
    if (i == 0)
      return true;

    assert(i < size());
    return (*this)[i - 1].getClassification().Kind == detail::LineKind::Blank;
  }

  bool isNextLineBlank(unsigned i) const {
    // [ReST/Syntax Details/Whitespace/Blank Lines]
    // Quote:
    //     [...] the last line of a document is treated as if it is followed by
    //     a blank line.
    if (i == size() - 1)
      return true;

    assert(i < size());
    return (*this)[i + 1].getClassification().Kind == detail::LineKind::Blank;
  }
};

/// A lightweight view into a \c LineList.
class LineListRef {
  friend class LineList;

  LineList LL;
  unsigned StartIdx;
  unsigned Size;
  Optional<Line> FirstLine;

public:
  LineListRef(LineList LL) : LL(LL), StartIdx(0), Size(LL.size()) {}

  LineListRef(const LineListRef &) = default;
  LineListRef &operator=(const LineListRef &) = default;

  Line &operator[](unsigned i) {
    if (i == 0 && isFirstLineTruncated())
      return FirstLine.getValue();
    return LL[StartIdx + i];
  }

  const Line &operator[](unsigned i) const {
    if (i == 0 && isFirstLineTruncated())
      return FirstLine.getValue();
    return LL[StartIdx + i];
  }

  unsigned size() const { return Size; }

  bool empty() const { return size() == 0; }

  LineListRef subList(unsigned StartIdx, unsigned Size) const {
    assert(StartIdx <= this->Size);
    assert(this->StartIdx + StartIdx + Size <= this->StartIdx + this->Size);
    auto Result = LL.subList(this->StartIdx + StartIdx, Size);
    if (StartIdx == 0 && FirstLine.hasValue())
      Result.FirstLine = FirstLine;
    return Result;
  }

  LineListRef dropFrontLines(unsigned NumLines) {
    return subList(NumLines, size() - NumLines);
  }

  bool isPreviousLineBlank(unsigned i) const {
    return LL.isPreviousLineBlank(StartIdx + i);
  }

  bool isNextLineBlank(unsigned i) const {
    return LL.isNextLineBlank(StartIdx + i);
  }

  void fromFirstLineDropFront(unsigned Bytes);

  LinePart getLinePart(unsigned LineIndex, unsigned StartByte,
                       unsigned NumBytes) const {
    LinePart Result;
    const Line &FirstLine = (*this)[LineIndex];
    assert(NumBytes <= FirstLine.Text.size() - StartByte);
    Result.Text = FirstLine.Text.substr(StartByte, NumBytes);
    SourceLoc StartLoc = FirstLine.Range.Start.getAdvancedLoc(StartByte);
    Result.Range = SourceRange(StartLoc, StartLoc.getAdvancedLoc(NumBytes));
    return Result;
  }

  LinePart getLinePart(LineListRefIndex Begin, LineListRefIndex End) const;

  bool isFirstLineTruncated() const { return FirstLine.hasValue(); }

  LineListRefIndex begin(unsigned LineIndex) const;
  LineListRefIndex end(unsigned LineIndex) const;
};

class LineListRefIndex {
  friend class LineListRef;

  struct IterationState {
    unsigned CurrentLine;
    unsigned CurrentLineByte;
  };

  LineListRef LL;
  IterationState State;

  static bool isEndImpl(const LineListRef &LL, IterationState State) {
    return State.CurrentLineByte == LL[State.CurrentLine].Text.size();
  }

  static std::pair<unsigned, unsigned>
  decodeUnicodeScalar(const LineListRef &LL, IterationState State) {
    assert(!isEndImpl(LL, State));
    const Line &L = LL[State.CurrentLine];
    unsigned Byte = L.Text[State.CurrentLineByte];
    if (Byte <= 0x7f) {
      // Fast path for ASCII encoded in UTF-8.
      return std::make_pair(Byte, 1);
    }
    return decodeUnicodeScalarNonASCII(
        L.Text.drop_front(State.CurrentLineByte));
  }

  static std::pair<unsigned, unsigned> decodeUnicodeScalarNonASCII(StringRef S);

  static std::pair<Optional<unsigned>, IterationState>
  getPossiblyEscapedUnicodeScalarSlow(const LineListRef &LL,
                                      IterationState State);

  static IterationState advanceState(const LineListRef &LL,
                                     IterationState State, unsigned Bytes) {
    assert(!isEndImpl(LL, State));
    auto Result = State;
    // Advance position within the line.
    Result.CurrentLineByte += Bytes;
    assert(Result.CurrentLineByte <= LL[Result.CurrentLine].Text.size() &&
           "Can only advance by one Unicode scalar");
    return Result;
  }

public:
  LineListRefIndex(const LineListRef &LL, unsigned LineIndex)
      : LL(LL), State{LineIndex, LL[LineIndex].FirstTextByte} {}

  LineListRefIndex(const LineListRefIndex &) = default;
  LineListRefIndex &operator=(const LineListRefIndex &) = default;

  bool isStart() const {
    return State.CurrentLine == 0 &&
           State.CurrentLineByte == LL[0].FirstTextByte;
  }

  bool isEnd() const { return isEndImpl(LL, State); }

  unsigned getUnicodeScalar() const {
    return decodeUnicodeScalar(LL, State).first;
  }

  Optional<unsigned> getPossiblyEscapedUnicodeScalar() const {
    if (isEnd())
      return None;

    auto ScalarAndLength = decodeUnicodeScalar(LL, State);
    if (ScalarAndLength.first != static_cast<unsigned>('\\')) {
      // Not an escape sequence.
      return ScalarAndLength.first;
    }
    // We found an escape sequence.
    return getPossiblyEscapedUnicodeScalarSlow(LL, State).first;
  }

  unsigned consumeUnicodeScalar() {
    assert(!isEnd());
    auto ScalarAndLength = decodeUnicodeScalar(LL, State);
    State = advanceState(LL, State, ScalarAndLength.second);
    return ScalarAndLength.first;
  }

  Optional<unsigned> consumePossiblyEscapedUnicodeScalar() {
    if (isEnd())
      return None;

    auto ScalarAndLength = decodeUnicodeScalar(LL, State);
    auto NextState = advanceState(LL, State, ScalarAndLength.second);
    if (ScalarAndLength.first != static_cast<unsigned>('\\')) {
      // Not an escape sequence.
      State = NextState;
      return ScalarAndLength.first;
    }
    // We found an escape sequence.
    auto ScalarAndNewState = getPossiblyEscapedUnicodeScalarSlow(LL, NextState);
    State = ScalarAndNewState.second;
    return ScalarAndNewState.first;
  }

  friend bool operator==(const LineListRefIndex &LHS,
                         const LineListRefIndex &RHS) {
    return LHS.State.CurrentLine == RHS.State.CurrentLine &&
           LHS.State.CurrentLineByte == RHS.State.CurrentLineByte;
  }

  friend bool operator!=(const LineListRefIndex &LHS,
                         const LineListRefIndex &RHS) {
    return !(LHS == RHS);
  }
};

class LineListBuilder {
  std::vector<Line> Lines;
  ReSTContext &Context;
public:
  LineListBuilder(ReSTContext &Context) : Context(Context) {}
  
  void addLine(StringRef Text, SourceRange Range);

  LineList takeLineList();
};

inline LineListRef LineList::subList(unsigned StartIndex, unsigned Size) const {
  assert(StartIndex <= Lines.size());
  assert(StartIndex + Size <= Lines.size());
  LineListRef Result(*this);
  Result.StartIdx += StartIndex;
  Result.Size = Size;
  return Result;
}

inline LinePart LineListRef::getLinePart(LineListRefIndex Begin,
                                         LineListRefIndex End) const {
  assert(Begin.State.CurrentLine == End.State.CurrentLine);
  unsigned NumBytes = End.State.CurrentLineByte - Begin.State.CurrentLineByte;
  LinePart Result;
  const Line &TheLine = (*this)[Begin.State.CurrentLine];
  Result.Text = TheLine.Text.substr(Begin.State.CurrentLineByte, NumBytes);
  SourceLoc StartLoc = TheLine.Range.Start.getAdvancedLoc(NumBytes);
  Result.Range = SourceRange(StartLoc, StartLoc.getAdvancedLoc(NumBytes));
  return Result;
}

inline LineListRefIndex LineListRef::begin(unsigned LineIndex) const {
  return LineListRefIndex(LL, LineIndex);
}

inline LineListRefIndex LineListRef::end(unsigned LineIndex) const {
  auto I = LineListRefIndex(LL, LineIndex);
  I.State.CurrentLineByte = (*this)[LineIndex].Text.size();
  return I;
}

} // namespace rest
} // namespace llvm

#endif // LLVM_REST_LINELIST_H

