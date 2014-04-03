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

namespace llvm {
namespace rest {
class ReSTContext;

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

static bool isBullet(LineKind K) {
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
}

static bool isEnumerated(LineKind K) {
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

struct Line {
  StringRef Text;
  SourceRange Range;
  ColumnNum FirstTextCol;
  unsigned FirstTextByte : 10;
  unsigned ClassificationComputed : 1;

private:
  mutable detail::LineClassification Classification;

  void computeClassification() const;

public:
  const detail::LineClassification &getClassification() const {
    if (ClassificationComputed)
      return Classification;
    computeClassification();
    return Classification;
  }

  Line() : FirstTextByte(0), ClassificationComputed(0) {}
};

struct LinePart {
  StringRef Text;
  SourceRange Range;
};

class LineListRef;

class LineList {
  MutableArrayRef<Line> Lines;

public:
  LineList(MutableArrayRef<Line> Lines) : Lines(Lines) {}

  Line &operator[](unsigned i) {
    assert(i < Lines.size());
    return Lines[i];
  }

  const Line &operator[](unsigned i) const {
    assert(i < Lines.size());
    return Lines[i];
  }

  unsigned size() const { return Lines.size(); }

  LineListRef subList(unsigned StartIndex, unsigned Size);

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

  LineList &LL;
  unsigned StartIdx;
  unsigned Size;
  Optional<Line> FirstLine;

public:
  LineListRef(LineList &LL) : LL(LL), StartIdx(0), Size(LL.size()) {}
  LineListRef(const LineListRef &LL) = default;

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
    const Line &FirstLine = (*this)[0];
    Result.Text = FirstLine.Text.substr(StartByte, NumBytes);
    SourceLoc StartLoc = FirstLine.Range.Start.getAdvancedLoc(StartByte);
    Result.Range = SourceRange(StartLoc, StartLoc.getAdvancedLoc(NumBytes));
    return Result;
  }

  bool isFirstLineTruncated() const { return FirstLine.hasValue(); }
};

inline LineListRef LineList::subList(unsigned StartIndex, unsigned Size) {
  assert(StartIndex <= Lines.size());
  assert(StartIndex + Size <= Lines.size());
  LineListRef Result(*this);
  Result.StartIdx += StartIndex;
  Result.Size = Size;
  return Result;
}

class LineListBuilder {
  std::vector<Line> Lines;

public:
  void addLine(StringRef Text, SourceRange Range);

  LineList takeLineList(ReSTContext &Context);
};

} // namespace rest
} // namespace llvm

#endif // LLVM_REST_LINELIST_H

