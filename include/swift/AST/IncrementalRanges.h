//===------------- IncrementalRanges.h -----------------------------*- C++
//-*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_INCREMENTALRANGES_H
#define SWIFT_AST_INCREMENTALRANGES_H

// Summary: TBD

#include "swift/Basic/LLVM.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/Support/YAMLTraits.h"
#include <vector>

namespace swift {
class PersistentParserState;
class SourceManager;
class DiagnosticEngine;
class SourceFile;
} // namespace swift

//==============================================================================
// MARK: Range types
//==============================================================================

namespace swift {
namespace incremental_ranges {

struct SerializableSourceRange;

typedef std::vector<SerializableSourceRange> Ranges;
typedef std::map<std::string, Ranges> RangesByFilename;
} // namespace incremental_ranges
} // namespace swift

//==============================================================================
// MARK: SerializableSourceLocation
//==============================================================================
namespace swift {
namespace incremental_ranges {

/// 1-origin
struct SerializableSourceLocation {
  uint64_t line = 0;
  uint64_t column = 0;

  SerializableSourceLocation(const SourceLoc loc, const SourceManager &SM);
  SerializableSourceLocation(uint64_t line, uint64_t column)
      : line(line), column(column) {}
  SerializableSourceLocation() = default;
  static const SerializableSourceLocation endOfAnyFile;

  bool operator< (const SerializableSourceLocation &x) const {
    return line < x.line ? true
    : line > x.line ? false
    : column < x.column;
  }
  bool operator==(const SerializableSourceLocation &x) const {
    return line == x.line && column == x.column;
  }
  bool operator<=(const SerializableSourceLocation &x) const {
    return *this < x || *this == x;
  }
  void print(raw_ostream &out) const;
  void dump() const;
};

} // namespace incremental_ranges
} // namespace swift

template <>
struct llvm::yaml::MappingTraits<
    swift::incremental_ranges::SerializableSourceLocation> {
  static const bool flow = true;
  static void
  mapping(llvm::yaml::IO &io,
          swift::incremental_ranges::SerializableSourceLocation &loc) {
    io.mapRequired("line", loc.line), io.mapRequired("column", loc.column);
  }
};
//==============================================================================
// MARK: SerializableSourceRange
//==============================================================================

namespace swift {
namespace incremental_ranges {
/// half-open, to facilitate representing empty ranges
struct SerializableSourceRange {
  SerializableSourceLocation start, end;

  SerializableSourceRange(const CharSourceRange r, const SourceManager &SM);
  SerializableSourceRange(SerializableSourceLocation start,
                          SerializableSourceLocation end);
  SerializableSourceRange() = default;

  static const SerializableSourceRange wholeFile;
  static Ranges RangesForWholeFile();

  bool isEmpty() const { return start == end; }

  bool overlaps(const SerializableSourceRange &x) const {
    return start < x.end && x.start < end;
  }
  bool operator==(const SerializableSourceRange &x) const {
    return start == x.start && end == x.end;
  }
  bool isImproperSubsetOf(const SerializableSourceRange &) const;
  bool properlyPreceeds(const SerializableSourceRange &) const;
  static bool isProperlySorted(ArrayRef<SerializableSourceRange>);

  bool
  isImproperSubsetOfAny(ArrayRef<SerializableSourceRange> supersetRanges) const;
  bool isImproperSubsetOfAnySlowlyAndSimply(
      ArrayRef<SerializableSourceRange> supersetRanges) const;

  /// Optimized for fewer ranges in the subset
  /// Return first outlier found in subset not in superset
  static Optional<SerializableSourceRange>
  findOutlierIfAny(ArrayRef<SerializableSourceRange> subset,
                   ArrayRef<SerializableSourceRange> superset);

  static Ranges findAllOutliers(ArrayRef<SerializableSourceRange> subset,
                                ArrayRef<SerializableSourceRange> superset);

  std::string printString() const;
  void print(raw_ostream &out) const;
  void dump() const;
};

} // namespace incremental_ranges
} // namespace swift


template <>
struct llvm::yaml::MappingTraits<
    swift::incremental_ranges::SerializableSourceRange> {
  static const bool flow = true;
  static void mapping(llvm::yaml::IO &io,
                      swift::incremental_ranges::SerializableSourceRange &sr) {
    io.mapRequired("start", sr.start), io.mapRequired("end", sr.end);
  }
};

//==============================================================================
// MARK: SwiftRangesFileContents
//==============================================================================

namespace swift {
namespace incremental_ranges {

struct SwiftRangesFileContents {
  RangesByFilename unparsedRangesByNonPrimary;
  Ranges noninlinableFunctionBodies;

  SwiftRangesFileContents() : SwiftRangesFileContents({}, {}) {}

  SwiftRangesFileContents(RangesByFilename &&unparsedRangesByNonPrimary,
                          Ranges &&noninlinableFunctionBodies)
      : unparsedRangesByNonPrimary(std::move(unparsedRangesByNonPrimary)),
        noninlinableFunctionBodies(std::move(noninlinableFunctionBodies)) {}

  static Optional<SwiftRangesFileContents>
  load(const StringRef primaryPath, const llvm::MemoryBuffer &swiftRangesBuffer,
       const bool showIncrementalBuildDecisions, DiagnosticEngine &diags);

  void dump(StringRef primary) const;

  static constexpr const char *header = "### Swift source ranges file v0 ###\n";
};
} // namespace incremental_ranges
} // namespace swift

template <>
struct llvm::yaml::MappingTraits<
    swift::incremental_ranges::SwiftRangesFileContents> {
  static void
  mapping(llvm::yaml::IO &io,
          swift::incremental_ranges::SwiftRangesFileContents &srfc) {
    io.mapRequired("unparsedRangesByNonPrimary",
                   srfc.unparsedRangesByNonPrimary);
    io.mapRequired("noninlinableFunctionBodies",
                   srfc.noninlinableFunctionBodies);
  }
};

LLVM_YAML_IS_SEQUENCE_VECTOR(swift::incremental_ranges::SerializableSourceRange)
LLVM_YAML_IS_STRING_MAP(swift::incremental_ranges::Ranges)
LLVM_YAML_IS_STRING_MAP(swift::incremental_ranges::RangesByFilename)

//==============================================================================
// MARK: SwiftRangesEmitter
//==============================================================================
namespace swift {
namespace incremental_ranges {

class SwiftRangesEmitter {
  const StringRef outputPath;
  SourceFile *const primaryFile;
  const PersistentParserState &persistentState;
  const SourceManager &sourceMgr;
  DiagnosticEngine &diags;

public:
  SwiftRangesEmitter(StringRef outputPath, SourceFile *primaryFile,
                     const PersistentParserState &persistentState,
                     const SourceManager &sourceMgr, DiagnosticEngine &diags)
      : outputPath(outputPath), primaryFile(primaryFile),
        persistentState(persistentState), sourceMgr(sourceMgr), diags(diags) {}

  /// True for error
  bool emit() const;

public:
  void emitRanges(llvm::raw_ostream &out) const;

private:
  RangesByFilename collectSerializedUnparsedRangesByNonPrimary() const;

  Ranges collectSortedSerializedNoninlinableFunctionBodies() const;
  std::vector<CharSourceRange> collectNoninlinableFunctionBodies() const;

  std::map<std::string, std::vector<CharSourceRange>>
  collectUnparsedRanges() const;
  std::vector<CharSourceRange>
  sortRanges(std::vector<CharSourceRange> ranges) const;

  /// Assuming \p ranges is sorted, coalesce overlapping ranges in place and
  /// return end of the resultant vector.
  std::vector<CharSourceRange>
      coalesceSortedRanges(std::vector<CharSourceRange>) const;

  std::vector<SerializableSourceRange>
  serializeRanges(std::vector<CharSourceRange> ranges) const;

  bool isImmediatelyBeforeOrOverlapping(CharSourceRange prev,
                                        CharSourceRange next) const;
};
} // namespace incremental_ranges
} // namespace swift

//==============================================================================
// MARK: CompiledSourceEmitter
//==============================================================================
namespace swift {
namespace incremental_ranges {

class CompiledSourceEmitter {
  const StringRef outputPath;
  const SourceFile *const primaryFile;
  const SourceManager &sourceMgr;
  DiagnosticEngine &diags;

public:
  CompiledSourceEmitter(StringRef outputPath, const SourceFile *primaryFile,
                        const SourceManager &sourceMgr, DiagnosticEngine &diags)
      : outputPath(outputPath), primaryFile(primaryFile), sourceMgr(sourceMgr),
        diags(diags) {}

  /// True for error
  bool emit();
};

} // namespace incremental_ranges
} // namespace swift

#endif // SWIFT_AST_INCREMENTALRANGES_H
