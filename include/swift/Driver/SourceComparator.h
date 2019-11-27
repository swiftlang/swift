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

// Declares the code needed to essentially "diff" the previously-compiled and
// possibly-about-to-be-compiled versions of a source file.
// Unlike Unix diff, it can go down to the granularity of characters within a
// line.

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

  /// It is useful to have structures that hold both the left- and
  /// right-hand-sides of a "diff" input Describes a side:
  enum class Side : size_t { L = 0, R = 1 };

  /// A general class to hold something that exists on both sides:
  template <typename T> struct LR {
    T contents[2];
    /// Get the contents on a particular side
    T &operator[](const Side side) { return contents[size_t(side)]; }
    const T &operator[](const Side side) const {
      return contents[size_t(side)];
    }

    /// Get a side by name:
    // TODO: Could get ride of these and just use the subscripting above
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

  /// A structure holding size_t's on both sides.
  /// Zero-origin
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
  /// Character indices into a line.
  /// Zero-origin
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

  /// Indices into a collection of lines.
  /// Zero-origin
  struct LineIndices : public Indices<LineIndices> {
    LineIndices() : Indices(~0, ~0) {}
    LineIndices(size_t lhs, size_t rhs) : Indices(lhs, rhs) {}
    LineIndices &operator=(const LineIndices &) = default;
    LineIndices &operator++() {
      ++lhs(), ++rhs();
      return *this;
    }
    LineIndices &operator--() {
      --lhs(), --rhs();
      return *this;
    }
  };

  /// Strings on both sides.
  struct StringRefs : LR<StringRef> {
    StringRefs(StringRef lhs, StringRef rhs) : LR(lhs, rhs) {}
    CharIndices size() const { return CharIndices(lhs().size(), rhs().size()); }
    /// Do the strings match at the provided character indices?
    bool matchAt(const CharIndices &indices) const {
      return lhs()[indices.lhs()] == rhs()[indices.rhs()];
    }
  };

  /// Optional strings on both sides
  struct OptStringRefs : LR<Optional<StringRef>> {
    OptStringRefs(Optional<StringRef> lhs, Optional<StringRef> rhs)
        : LR(lhs, rhs) {}
  };

  /// A half-open span of lines on both sides, from start up to but not
  /// including end.
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

  /// A pair of collections of lines, one on each side
  struct Lines : LR<std::vector<StringRef>> {
    Lines(std::vector<StringRef> lhs, std::vector<StringRef> rhs)
        : LR(lhs, rhs) {}

    bool matchAt(LineIndices where) const {
      return lhs()[where.lhs()] == rhs()[where.rhs()];
    }
    StringRefs operator[](const LineIndices &indices) const {
      return StringRefs(lhs()[indices.lhs()], rhs()[indices.rhs()]);
    }
    /// Cannot use subscripting to get a particular side here because used it
    /// above for something else. So use a named function.
    const std::vector<StringRef> &side(Side s) const {
      return s == Side::L ? lhs() : rhs();
    }
    LineIndices size() const { return LineIndices(lhs().size(), rhs().size()); }

    CharIndices sizeOfLastLines() const {
      return CharIndices(lhs().empty() ? 0 : lhs().back().size(),
                         rhs().empty() ? 0 : rhs().back().size());
    }
  };

  /// A pair of SerializableSourceRanges, one per side.
  /// (SerializableSourceRanges are 1-origin, and semi-open.)
  struct LRSerializableRange : LR<SerializableSourceRange> {
    LRSerializableRange(const SerializableSourceRange &lhs,
                        const SerializableSourceRange &rhs)
        : LR(lhs, rhs) {}
  };
  /// A pair of vectors of SerializableSourceRanges, one per side.
  struct LRRanges : LR<Ranges> {
    LRRanges() : LRRanges({}, {}) {}
    LRRanges(Ranges &&lhs, Ranges &&rhs) : LR(std::move(lhs), std::move(rhs)) {}
    static LRRanges wholeFile() {
      return {SerializableSourceRange::RangesForWholeFile(),
              SerializableSourceRange::RangesForWholeFile()};
    }
    void push_back(const LRSerializableRange &range) {
      lhs().push_back(range.lhs());
      rhs().push_back(range.rhs());
    }
    bool empty() const {
      assert(lhs().empty() == rhs().empty() && "diff should produce at least "
                                               "empty ranges for an addition "
                                               "to the other side");
      return lhs().empty();
    }
  };

private:
  /// The inputs, separated into lines
  Lines linesToCompare;

  /// The trimmed input regions to compare, after cutting away identical
  /// beginnings and endings.
  LineSpan regionsToCompare;

  /// If line i on the left matches line j on the right, match[i] == j.
  /// If line i on the left has no match, match[i] == None.
  std::vector<Optional<size_t>> matches;

  /// Chains matching lines together (I think)
  struct PRLink {
    LineIndices lines;
    Optional<size_t> next;
    PRLink(LineIndices lines, const Optional<size_t> next)
        : lines(lines), next(next) {}
    PRLink() : PRLink(LineIndices(), None) {}
  };


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
    /// Return index of first greater element or the end index if there is none.
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
  /// Run the actual diff algorithm
  void compare();

private:
  void trimStart();
  void trimEnd();

  /// Return a map from a line in rhs to indices of all identical lines in
  /// rhs
  std::unordered_map<std::string, std::vector<size_t>>
  buildEquivalenceClasses();

  std::pair<std::vector<PRLink>, SortedSequence> buildDAGOfSubsequences(
      std::unordered_map<std::string, std::vector<size_t>> rhsMap);

  void scanMatchedLines(
      std::pair<std::vector<PRLink>, SortedSequence> &&linksAndThres);

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
  /// This function resolves changed regions to within lines. It's a bit tricky.
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
