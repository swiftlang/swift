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

#include "swift/ReST/SourceLoc.h"

namespace llvm {
namespace rest {

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
  union {
    unsigned ExtraData;
    struct {
      unsigned EnumeratorStyle : 7;
      unsigned HasText : 1;
      unsigned EnumeratorAndWhitespaceBytes : 10;
    };
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

  unsigned getEnumeratorAndWhitespaceBytes() const {
    assert(isEnumerated(Kind));
    return EnumeratorAndWhitespaceBytes;
  }

  ColumnNum getEnumeratorAndWhitespaceCols() const {
    // Reimplement this function if we allow Unicode characters in enumerators
    // or in whitespace.
    return ColumnNum::make(getEnumeratorAndWhitespaceBytes());
  }

  unsigned getFieldNameSecondColonByte() const {
    assert(Kind == LineKind::FieldList);
    return ExtraData;
  }

  static LineClassification makeUnknown() {
    return LineClassification();
  }

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

  static LineClassification makeEnumerated(LineKind Kind,
                                           EnumeratorStyleKind EnumeratorStyle,
                                           bool HasText,
                                           unsigned EnumeratorAndWhitespaceBytes) {
    assert(isEnumerated(Kind));
    LineClassification Result;
    Result.Kind = Kind;
    Result.EnumeratorStyle = static_cast<unsigned>(EnumeratorStyle);
    Result.HasText = HasText;
    Result.EnumeratorAndWhitespaceBytes = EnumeratorAndWhitespaceBytes;
    return Result;
  }

  static LineClassification makeFieldList(unsigned FieldNameSecondColonByte) {
    LineClassification Result;
    Result.Kind = LineKind::FieldList;
    Result.ExtraData = FieldNameSecondColonByte;
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

class LineListRef;

class LineList {
  std::vector<Line> Lines;

public:
  Line &operator[](unsigned i) {
    assert(i < Lines.size());
    return Lines[i];
  }

  const Line &operator[](unsigned i) const {
    assert(i < Lines.size());
    return Lines[i];
  }

  unsigned size() const {
    return Lines.size();
  }

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

  void addLine(StringRef Text, SourceRange Range);
};

/// A lightweight view into a \c LineList.
class LineListRef {
  friend class LineList;

  LineList &LL;
  unsigned StartIdx;
  unsigned Size;

public:
  LineListRef(LineList &LL) : LL(LL), StartIdx(0), Size(LL.size()) {}

  Line &operator[](unsigned i) {
    return LL[StartIdx + i];
  }

  const Line &operator[](unsigned i) const {
    return LL[StartIdx + i];
  }

  unsigned size() const {
    return Size;
  }

  LineListRef subList(unsigned StartIdx, unsigned Size) const {
    assert(this->StartIdx + StartIdx + Size <= this->StartIdx + this->Size);
    return LL.subList(this->StartIdx + StartIdx, Size);
  }

  bool isPreviousLineBlank(unsigned i) const {
    return LL.isPreviousLineBlank(StartIdx + i);
  }

  bool isNextLineBlank(unsigned i) const {
    return LL.isNextLineBlank(StartIdx + i);
  }
};

inline LineListRef LineList::subList(unsigned StartIndex, unsigned Size) {
  assert(StartIndex <= Lines.size());
  assert(StartIndex + Size <= Lines.size());
  LineListRef Result(*this);
  Result.StartIdx += StartIndex;
  Result.Size = Size;
  return Result;
}

} // namespace rest
} // namespace llvm

#endif // LLVM_REST_LINELIST_H

