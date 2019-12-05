//===------------------------- SourceComparator.cpp ------------------------==//
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

#include "swift/Driver/SourceComparator.h"

#include <tuple>
#include <unordered_map>

using namespace swift;
using namespace incremental_ranges;

//==============================================================================
// MARK: SourceComparator
//==============================================================================

//==============================================================================
// MARK: initialization
//==============================================================================

SourceComparator::SourceComparator(StringRef s1, StringRef s2)
    : linesToCompare(splitIntoLines(s1), splitIntoLines(s2)),
      regionsToCompare(LineSpan({0, 0}, linesToCompare.size())),
      matches(linesToCompare.lhs().size(), None) {}

std::vector<StringRef> SourceComparator::splitIntoLines(const StringRef s) {
  std::vector<StringRef> result;
  for (auto toSplit = s; !toSplit.empty();) {
    StringRef line;
    std::tie(line, toSplit) = toSplit.split('\n');
    result.push_back(line);
  }
  return result;
}

//==============================================================================
// MARK: Comparing
//==============================================================================

void SourceComparator::compare() {
  // Trim ends of common elements
  trimStart(), trimEnd();
  // Do the comparison
  scanMatchedLines(buildDAGOfSubsequences(buildEquivalenceClasses()));
}

void SourceComparator::trimStart() {
  for (; regionsToCompare.bothNonEmpty() &&
         linesToCompare.matchAt(regionsToCompare.start);
       ++regionsToCompare.start)
    matches[regionsToCompare.start.lhs()] = regionsToCompare.start.rhs();
}
void SourceComparator::trimEnd() {
  for (; regionsToCompare.bothNonEmpty() &&
         linesToCompare.matchAt(--regionsToCompare.end);)
    matches[regionsToCompare.end.lhs()] = regionsToCompare.end.rhs();
}

std::unordered_map<std::string, std::vector<size_t>>
SourceComparator::buildEquivalenceClasses() {
  // Map each element of rhs to the sequence of indices it occupies.
  std::unordered_map<std::string, std::vector<size_t>> rhsMap;
  for (auto index = regionsToCompare.start.rhs();
       index < regionsToCompare.end.rhs(); ++index)
    rhsMap[linesToCompare.rhs()[index]].push_back(index);
  return rhsMap;
}

std::pair<std::vector<SourceComparator::PRLink>,
          SourceComparator::SortedSequence>
SourceComparator::buildDAGOfSubsequences(
    std::unordered_map<std::string, std::vector<size_t>> rhsMap) {
  SortedSequence thresh;
  const size_t linksSize = regionsToCompare.size().min();
  std::vector<PRLink> links;
  links.resize(linksSize);

  for (auto i = regionsToCompare.start.lhs(); i < regionsToCompare.end.lhs();
       ++i) {
    // What lines in rhs does the ith line in lhs match?
    auto iter = rhsMap.find(linesToCompare.lhs()[i]);
    if (iter == rhsMap.end())
      continue; // no match in rhs
    auto &res = iter->second;
    size_t lastJ = ~0;
    // For each match in rhs in reverse order
    for (auto j : llvm::reverse(res)) {
      assert(j < lastJ);
      lastJ = j;
      // replace the index of the match with the index of the earlier match
      // or add it if last match
      // (thresh gets populated for each lhs match with rhs indices)
      if (auto optK = thresh.replaceNextLargerWith(j)) {
        // There was a later match at thresh[k]
        auto k = optK.getValue();
        // prev match (of what?) was at links[k-1]?
        // chain to that match
        Optional<size_t> newNext = k == 0 ? None : Optional<size_t>(k - 1);
        links.emplace(links.begin() + k, LineIndices(i, j), newNext);
      }
    }
  }
  return {std::move(links), thresh};
}

void SourceComparator::scanMatchedLines(
    std::pair<std::vector<PRLink>, SortedSequence> &&linksAndThres) {
  const auto &links = linksAndThres.first;
  const auto &thresh = linksAndThres.second;

  if (thresh.empty())
    return;
  // For every match put rhs index in matches[col2 index]
  for (const auto *p = &links[thresh.size() - 1]; p;
       p = p->next ? &links[p->next.getValue()] : nullptr)
    matches[p->lines.lhs()] = p->lines.rhs();
}

//==============================================================================
// MARK: summarizing
//==============================================================================

std::vector<SourceComparator::LineSpan>
SourceComparator::getMatchingRegions() const {
  std::vector<LineSpan> matches;
  forEachMatch([&](const LineSpan r) { matches.push_back(r); });
  return matches;
}

std::vector<SourceComparator::LineSpan>
SourceComparator::getMismatchingRegions() const {
  std::vector<LineSpan> mismatches;
  forEachMismatch([&](const LineSpan r) { mismatches.push_back(r); });
  return mismatches;
}

void SourceComparator::forEachMatch(
    function_ref<void(const LineSpan)> consumeMatch) const {
  for (LineIndices nextMatch = getFirstMatch(),
                   nextMismatch = getNextMismatchAfter(nextMatch);
       areEitherInRange(nextMatch); nextMatch = getNextMatchAfter(nextMismatch),
                   nextMismatch = getNextMismatchAfter(nextMatch)) {
    consumeMatch(LineSpan{nextMatch, nextMismatch});
  }
}

void SourceComparator::forEachMismatch(
    function_ref<void(const LineSpan)> consumeMismatch) const {
  for (LineIndices nextMismatch = getFirstMismatch(),
                   nextMatch = getNextMatchAfter(nextMismatch);
       areEitherInRange(nextMismatch);
       nextMismatch = getNextMismatchAfter(nextMatch),
                   nextMatch = getNextMatchAfter(nextMismatch)) {
    consumeMismatch(LineSpan{nextMismatch, nextMatch});
  }
}

bool SourceComparator::areEitherInRange(const LineIndices lines) const {
  return lines.lhs() < linesToCompare.lhs().size() ||
         lines.rhs() < linesToCompare.rhs().size();
}

SourceComparator::LineIndices SourceComparator::getFirstMatch() const {
  for (size_t i = 0; i < linesToCompare.lhs().size(); ++i) {
    if (auto m = matches[i])
      return {i, m.getValue()};
  }
  return linesToCompare.size();
}

SourceComparator::LineIndices SourceComparator::getFirstMismatch() const {
  size_t i = 0;
  for (; i < linesToCompare.lhs().size() && matches[i].getValueOr(~0) == i;
       ++i) {
  }
  return {i, i};
}

SourceComparator::LineIndices
SourceComparator::getNextMismatchAfter(const LineIndices nextMatch) const {
  LineIndices possibleNextMatch = nextMatch + 1;
  for (; possibleNextMatch.lhs() < matches.size() &&
         possibleNextMatch.rhs() ==
             matches[possibleNextMatch.lhs()].getValueOr(~0);
       ++possibleNextMatch) {
  }
  return possibleNextMatch;
}

SourceComparator::LineIndices
SourceComparator::getNextMatchAfter(const LineIndices nextMismatch) const {
  // matches[nextMismatch.lhs] could be present if the gap was in 2
  // so start right at nextMismatch
  for (auto maybeMatch1 = nextMismatch.lhs(); maybeMatch1 < matches.size();
       ++maybeMatch1) {
    if (auto rhsMatch = matches[maybeMatch1])
      return {maybeMatch1, rhsMatch.getValue()};
  }
  return linesToCompare.size();
}

SourceComparator::LRSerializableRange
SourceComparator::convertAMismatch(const LineSpan mismatchLines) const {
  CharIndices firstMismatchInLine = computeFirstMismatchInLine(mismatchLines);
  CharIndices endMismatchInLine =
      computeEndMismatchInLine(mismatchLines, firstMismatchInLine);

  const LineIndices endLinesForCoords = {
      computeEndLinesToGoWithChars(mismatchLines, endMismatchInLine, Side::L),
      computeEndLinesToGoWithChars(mismatchLines, endMismatchInLine, Side::R),
  };

  // 1-origin for diff format
  // Since we convert from a semi-closed line range to a semi-closed
  // line, char range, we don't add 1 to the end lines.
  const auto lhsMismatchStart = SerializableSourceLocation{
      mismatchLines.start.lhs() + 1, firstMismatchInLine.lhs() + 1};
  const auto rhsMismatchStart = SerializableSourceLocation{
      mismatchLines.start.rhs() + 1, firstMismatchInLine.rhs() + 1};
  const auto lhsMismatchEnd = SerializableSourceLocation{
      endLinesForCoords.lhs() + 1, endMismatchInLine.lhs() + 1};
  const auto rhsMismatchEnd = SerializableSourceLocation{
      endLinesForCoords.rhs() + 1, endMismatchInLine.rhs() + 1};

  if (lhsMismatchEnd < lhsMismatchStart)
    llvm::errs() << "LEFT: ";
  if (rhsMismatchEnd < rhsMismatchStart)
    llvm::errs() << "RIGHT: ";

  const auto lhsMismatchRange =
      SerializableSourceRange(lhsMismatchStart, lhsMismatchEnd);
  const auto rhsMismatchRange =
      SerializableSourceRange(rhsMismatchStart, rhsMismatchEnd);

  return {lhsMismatchRange, rhsMismatchRange};
}

SourceComparator::CharIndices SourceComparator::computeFirstMismatchInLine(
    const LineSpan mismatchLines) const {
  return mismatchLines.bothNonEmpty()
             ? computeFirstMismatchInLinesOnBothSides(mismatchLines)
             : computeFirstMismatchInLineOnOneSide(
                   mismatchLines,
                   mismatchLines.start.lhs() < mismatchLines.end.lhs()
                       ? Side::L
                       : Side::R);
}

SourceComparator::CharIndices
SourceComparator::computeFirstMismatchInLinesOnBothSides(
    const LineSpan mismatchLines) const {
  CharIndices firstMismatchInLine = {0, 0}; // next char after last char
  if (mismatchLines.start < linesToCompare.size()) {
    const auto firstLines = linesToCompare[mismatchLines.start];
    const auto ends = firstLines.size();
    for (;
         firstMismatchInLine < ends && firstLines.matchAt(firstMismatchInLine);
         ++firstMismatchInLine) {
    }
  }
  return firstMismatchInLine;
}

SourceComparator::CharIndices
SourceComparator::computeFirstMismatchInLineOnOneSide(
    const LineSpan mismatchLines, const Side nonemptySide) const {
  return {0, 0};
}

SourceComparator::CharIndices SourceComparator::computeEndMismatchInLine(
    const LineSpan mismatchLines, const CharIndices firstMismatchInLine) const {
  return mismatchLines.bothNonEmpty()
             ? computeEndMismatchInLineOnBothSides(mismatchLines,
                                                   firstMismatchInLine)
             : computeEndMismatchInLineOnOneSide(
                   mismatchLines, firstMismatchInLine,
                   mismatchLines.start.lhs() < mismatchLines.end.lhs()
                       ? Side::L
                       : Side::R);
}

SourceComparator::CharIndices
SourceComparator::computeEndMismatchInLineOnBothSides(
    LineSpan mismatchLines, const CharIndices firstMismatchInLine) const {
  CharIndices endMismatchInLine(0, 0);
  const auto lastLines = linesToCompare[mismatchLines.end - 1];
  for (endMismatchInLine = lastLines.size();
       firstMismatchInLine < endMismatchInLine &&
       lastLines.matchAt(endMismatchInLine - 1);
       --endMismatchInLine) {
  }
  return endMismatchInLine;
}

SourceComparator::CharIndices
SourceComparator::computeEndMismatchInLineOnOneSide(
    LineSpan mismatchLines, const CharIndices firstMismatchInLine,
    const Side side) const {
  const size_t lineSize =
      linesToCompare.side(side)[mismatchLines.end[side] - 1].size();
  return side == Side::L ? CharIndices(lineSize, 0) : CharIndices(0, lineSize);
}

size_t SourceComparator::computeEndLinesToGoWithChars(
    const LineSpan mismatchLines, const CharIndices endMismatchInLine,
    const Side side) const {
  const bool noMismatch = mismatchLines.start[side] == mismatchLines.end[side];
  if (noMismatch)
    return mismatchLines.start[side];
  const bool mismatchEndsWithinALine = endMismatchInLine[side] != 0;
  // If mismatch ends within a line, then the line coordinate should be the
  // line *before* the line after the mismatch.
  return mismatchEndsWithinALine ? mismatchLines.end[side] - 1
                                 : mismatchLines.end[side];
}

SourceComparator::LRRanges SourceComparator::convertAllMismatches() const {
  LRRanges mismatches;

  forEachMismatch([&](const SourceComparator::LineSpan mismatch) {
    mismatches.push_back(convertAMismatch(mismatch));
  });
  return mismatches;
}

//==============================================================================
// MARK: printing
//==============================================================================

void SourceComparator::print(raw_ostream &out) const {
  forEachMismatch(
      [&](const LineSpan &mismatch) { reportMismatch(mismatch, out); });
}

void SourceComparator::reportMismatch(const LineSpan mismatch,
                                      raw_ostream &out) const {

  auto printRange = [&](size_t rangeStart, size_t rangeEnd) {
    // 1-origin for llvm compat
    ++rangeStart, ++rangeEnd;
    const auto rangeLast = rangeEnd - 1;
    out << std::min(rangeStart, rangeLast);
    if (rangeStart < rangeLast)
      out << ", " << rangeLast;
  };

  if (mismatch.bothEmpty())
    return;

  const auto lhsStart = mismatch.start.lhs();
  const auto lhsEnd = mismatch.end.lhs();
  const auto rhsStart = mismatch.start.rhs();
  const auto rhsEnd = mismatch.end.rhs();

  printRange(lhsStart, lhsEnd);
  {
    const auto added = "a", deleted = "d", changed = "c";
    out << (lhsStart >= lhsEnd ? added
                               : rhsStart >= rhsEnd ? deleted : changed);
  }
  printRange(rhsStart, rhsEnd);
  out << "\n";

  for (auto i = lhsStart; i < lhsEnd; ++i)
    out << "< " << linesToCompare.lhs()[i] << "\n";
  if (lhsStart < lhsEnd && rhsStart < rhsEnd)
    out << "---\n";
  for (auto i = rhsStart; i < rhsEnd; ++i)
    out << "> " << linesToCompare.rhs()[i] << "\n";
}

//==============================================================================
// MARK: SortedSequence
//==============================================================================

Optional<size_t>
SourceComparator::SortedSequence::replaceNextLargerWith(const Elem x) {
  auto loc = locationOfFirstGreaterOrEqualElement(x);
  if (loc >= contents.size()) {
    contents.push_back(x);
    return loc;
  }
  if (contents[loc] == x)
    return None;
  contents[loc] = x;
  return loc;
}

size_t SourceComparator::SortedSequence::locationOfFirstGreaterOrEqualElement(
    const Elem x) const {
  return std::lower_bound(contents.begin(), contents.end(), x) -
         contents.begin();
}

//==============================================================================
// MARK: testing
//==============================================================================

bool SourceComparator::test() {
  auto doCompare = [&](const char *lhs,
                       const char *rhs) -> LRSerializableRange {
    SourceComparator sc(lhs, rhs);
    sc.compare();
    sc.dump();
    auto mismatches = sc.convertAllMismatches();
    assert(mismatches.lhs().size() == 1);
    assert(mismatches.rhs().size() == 1);
    return LRSerializableRange(mismatches.lhs()[0], mismatches.rhs()[0]);
  };
  {
    auto a = "}\nfunc nine() {}\nfunc ten() {}";
    auto b = "}\nfunc ten() {}";
    auto mismatches = doCompare(a, b);
    assert(mismatches.lhs() == SerializableSourceRange({2, 1}, {2, 15}));
    assert(mismatches.rhs() == SerializableSourceRange({2, 1}, {2, 1}));
  }
  {
    auto a = "abcde";
    auto b = "abde";
    auto mismatches = doCompare(a, b);
    assert(mismatches.lhs() == SerializableSourceRange({1, 3}, {1, 4}));
    assert(mismatches.rhs() == SerializableSourceRange({1, 3}, {1, 3}));
  }
  {
    auto a = "line0\n";
    auto b = "line0\nline1\n";
    auto mismatches = doCompare(a, b);
    assert(mismatches.lhs() == SerializableSourceRange({2, 1}, {2, 1}));
    assert(mismatches.rhs() == SerializableSourceRange({2, 1}, {2, 6}));
  }
  {
    auto a = "var snort = 333\n";
    auto b = "var fred = 123456\n";
    auto mismatches = doCompare(a, b);
    assert(mismatches.lhs() == SerializableSourceRange({1, 5}, {1, 16}));
    assert(mismatches.rhs() == SerializableSourceRange({1, 5}, {1, 18}));
  }
  {
    auto a = "line1\nline2a\nline3\nline4a\nline5\nline6\n";
    auto b = "line1\nline2b\nline3\nline4b\nline6\n";

    SourceComparator sc(a, b);
    sc.compare();

    std::vector<Optional<size_t>> matchesVec{0, None, 2, None, None, 4};
    assert(sc.matches == matchesVec);

    auto matches = sc.getMatchingRegions();
    assert(matches.size() == 3);
    assert(matches[0] == LineSpan({0, 0}, {1, 1}));
    assert(matches[1] == LineSpan({2, 2}, {3, 3}));
    assert(matches[2] == LineSpan({5, 4}, {6, 5}));

    auto mismatches = sc.getMismatchingRegions();
    assert(mismatches.size() == 2);
    assert(mismatches[0] == LineSpan({1, 1}, {2, 2}));
    assert(mismatches[1] == LineSpan({3, 3}, {5, 4}));
    sc.dump();
    sc.convertAllMismatches();
  }
  return true;
}
