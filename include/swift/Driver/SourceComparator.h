//===------------------------------ SourceComparator.h -----------*- C++-*-===//
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

#ifndef SWIFT_DRIVER_SOURCECOMPARATOR_H
#define SWIFT_DRIVER_SOURCECOMPARATOR_H

#include "swift/AST/IncrementalRanges.h"
#include "llvm/Support/MemoryBuffer.h"
#include <string>
#include <unordered_map>
#include <vector>

namespace swift {
namespace incremental_ranges {

class SourceComparator {

public:
  //==============================================================================
  // MARK: Packaging indices for both sides
  //==============================================================================
  enum class Side : size_t { L = 0, R = 1 };

  template <typename T> struct LR {
    T contents[2];
    T &operator[](const Side side) { return contents[size_t(side)]; }
    const T &operator[](const Side side) const {
      return contents[size_t(side)];
    }
    T &lhs() { return contents[size_t(Side::L)]; }
    T &rhs() { return contents[size_t(Side::R)]; }

    const T &lhs() const { return contents[size_t(Side::L)]; }
    const T &rhs() const { return contents[size_t(Side::R)]; }
    LR(T lhs, T rhs) {
      contents[0] = lhs;
      contents[1] = rhs;
    }
    bool operator==(const LR &other) {
      return lhs() == other.lhs() && rhs() == other.rhs();
    }
  };
  template <typename Derived> struct Indices : public LR<size_t> {
    Indices(size_t lhs, size_t rhs) : LR(lhs, rhs) {}
    Derived operator+(size_t x) const { return Derived(lhs() + x, rhs() + x); }
    Derived operator-(size_t x) const { return Derived(lhs() - x, rhs() - x); }
    size_t min() const { return std::min(lhs(), rhs()); }
    bool operator<(const Derived &other) const {
      return lhs() < other.lhs() && rhs() < other.rhs();
    }
    bool operator<=(const Derived &other) const {
      return lhs() <= other.lhs() && rhs() <= other.rhs();
    }
  };
  struct CharIndices : public Indices<CharIndices> {
    CharIndices(size_t lhs, size_t rhs) : Indices(lhs, rhs) {}
    CharIndices &operator++() {
      ++lhs(), ++rhs();
      return *this;
    }
    CharIndices &operator--() {
      --lhs(), --rhs();
      return *this;
    }
    bool bothNonZero() const { return lhs() != 0 && rhs() != 0; }
  };
  struct LineIndices : public Indices<LineIndices> {
    LineIndices() : Indices(~0, ~0) {}
    LineIndices(size_t lhs, size_t rhs) : Indices(lhs, rhs) {}
    LineIndices &operator++() {
      ++lhs(), ++rhs();
      return *this;
    }
    LineIndices &operator--() {
      --lhs(), --rhs();
      return *this;
    }
  };
  struct StringRefs : LR<StringRef> {
    StringRefs(StringRef lhs, StringRef rhs) : LR(lhs, rhs) {}
    CharIndices size() const { return CharIndices(lhs().size(), rhs().size()); }
    bool matchAt(const CharIndices &indices) const {
      return lhs()[indices.lhs()] == rhs()[indices.rhs()];
    }
  };

  struct OptStringRefs : LR<Optional<StringRef>> {
    OptStringRefs(Optional<StringRef> lhs, Optional<StringRef> rhs)
        : LR(lhs, rhs) {}
  };

  struct LineSpan {
    /// [start, end)
    LineIndices start, end;

    LineSpan(LineIndices start, LineIndices end) : start(start), end(end) {}
    bool operator==(const LineSpan &rhs) {
      return start == rhs.start && end == rhs.end;
    }
    bool bothNonEmpty() const {
      return start.lhs() < end.lhs() && start.rhs() < end.rhs();
    }
    bool eitherNonEmpty() const {
      return start.lhs() < end.lhs() || start.rhs() < end.rhs();
    }
    bool bothEmpty() const { return !eitherNonEmpty(); }
    LineIndices size() const {
      return LineIndices(end.lhs() - start.lhs(), end.rhs() - start.rhs());
    }
  };

  struct Lines : LR<std::vector<StringRef>> {
    Lines(std::vector<StringRef> lhs, std::vector<StringRef> rhs)
        : LR(lhs, rhs) {}
    bool matchAt(LineIndices where) const {
      return lhs()[where.lhs()] == rhs()[where.rhs()];
    }
    StringRefs operator[](const LineIndices &indices) const {
      return StringRefs(lhs()[indices.lhs()], rhs()[indices.rhs()]);
    }
    const std::vector<StringRef> &side(Side s) const {
      return s == Side::L ? lhs() : rhs();
    }
    LineIndices size() const { return LineIndices(lhs().size(), rhs().size()); }

    CharIndices sizeOfLastLines() const {
      return CharIndices(lhs().empty() ? 0 : lhs().back().size(),
                         rhs().empty() ? 0 : rhs().back().size());
    }
  };

  struct LRSerializableRange : LR<SerializableSourceRange> {
    LRSerializableRange(const SerializableSourceRange &lhs,
                        const SerializableSourceRange &rhs)
        : LR(lhs, rhs) {}
  };
  struct LRRanges : LR<Ranges> {
    LRRanges() : LR({}, {}) {}
    void push_back(const LRSerializableRange &range) {
      lhs().push_back(range.lhs());
      rhs().push_back(range.rhs());
    }
  };

private:
  /// The inputs, separated into lines
  Lines linesToCompare;

  /// The trimmed input regions to compare
  LineSpan regionsToCompare;

  std::vector<Optional<size_t>> matches;

  struct PRLink {
    LineIndices lines;
    PRLink *next;
  };

  std::vector<PRLink> linkVec;
  size_t nextLink = 0;

  //==============================================================================
  // MARK: SortedSequence
  //==============================================================================
private:
  class SortedSequence {
    typedef size_t Elem;
    std::vector<Elem> contents;

  public:
    /// Find the place at which x would normally be inserted into the sequence,
    /// and replace that element with x, returning the index.
    /// If x is already in the sequence, do nothing and return None.
    /// Append to the end if necessary.
    /// Preserve sort order.
    Optional<size_t> replaceNextLargerWith(const Elem x);
    bool empty() const { return contents.empty(); }

    Elem back() const { return contents.back(); }

    size_t size() const { return contents.size(); }

  private:
    /// return index of first greater element
    size_t locationOfFirstGreaterOrEqualElement(const Elem x) const;
  };

  //==============================================================================
  // MARK: initialization
  //==============================================================================

public:
  SourceComparator(StringRef s1, StringRef s2);

private:
  static std::vector<StringRef> splitIntoLines(StringRef s);

  //==============================================================================
  // MARK: Comparing
  //==============================================================================
public:
  void compare();

private:
  void trimStart();
  void trimEnd();

  /// Return a map from a line in rhs to indices of all identical lines in
  /// rhs
  std::unordered_map<std::string, std::vector<size_t>>
  buildEquivalenceClasses();

  PRLink *newPRLink(LineIndices, PRLink *next);

  std::pair<std::vector<PRLink *>, SortedSequence> buildDAGOfSubsequences(
      std::unordered_map<std::string, std::vector<size_t>> rhsMap);

  void scanMatchedLines(
      std::pair<std::vector<PRLink *>, SortedSequence> linksAndThres);

  //==============================================================================
  // MARK: summarizing
  //==============================================================================
public:
  std::vector<LineSpan> getMatchingRegions() const;
  std::vector<LineSpan> getMismatchingRegions() const;

  void forEachMatch(function_ref<void(const LineSpan)>) const;
  void forEachMismatch(function_ref<void(const LineSpan)>) const;

  /// A region is zero-origin, semi-open. An SerializableRange is 1-origin,
  /// closed.
  LRSerializableRange convertAMismatch(LineSpan mismatch) const;

  LRRanges convertAllMismatches() const;

private:
  bool areEitherInRange(LineIndices lines) const;

  LineIndices getFirstMatch() const;
  LineIndices getFirstMismatch() const;

  LineIndices getNextMismatchAfter(const LineIndices nextMatch) const;

  LineIndices getNextMatchAfter(const LineIndices nextMismatch) const;

  //==============================================================================
  // MARK: converting mismatches
  //==============================================================================
  CharIndices computeFirstMismatchInLine(LineSpan mismatchLines) const;
  CharIndices
  computeFirstMismatchInLinesOnBothSides(LineSpan mismatchLines) const;
  CharIndices computeFirstMismatchInLineOnOneSide(LineSpan mismatchLines,
                                                  Side) const;

  CharIndices computeEndMismatchInLine(LineSpan mismatchLines,
                                       CharIndices firstMismatchInLine) const;
  CharIndices computeEndMismatchInLineOnBothSides(
      LineSpan mismatchLines, const CharIndices firstMismatchInLine) const;
  CharIndices
  computeEndMismatchInLineOnOneSide(LineSpan mismatchLines,
                                    const CharIndices firstMismatchInLine,
                                    const Side) const;

  size_t computeEndLinesToGoWithChars(LineSpan mismatchLines,
                                      CharIndices endMismatchInLine,
                                      const Side) const;
  //==============================================================================
  // MARK: printing
  //==============================================================================
public:
  void dump() const { print(llvm::errs()); }
  void print(raw_ostream &) const;

  void reportMismatch(const LineSpan mismatch, raw_ostream &) const;

  //==============================================================================
  // MARK: testing
  //==============================================================================
public:
  static bool test();
};

} // namespace incremental_ranges
} // namespace swift

#endif /* SWIFT_DRIVER_SOURCECOMPARATOR_H */
