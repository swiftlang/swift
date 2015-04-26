//===--- SourceLoc.h - ReST source location and source manager classes ----===//
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


#ifndef LLVM_REST_SOURCELOC_H
#define LLVM_REST_SOURCELOC_H

#include "llvm/ADT/StringRef.h"
#include <algorithm>
#include <cassert>
#include <utility>
#include <vector>

namespace llvm {
namespace markup {

class SourceLoc {
  friend class SourceManagerBase;

  template<typename ExternalSourceLocTy>
  friend class SourceManager;

  unsigned Value;

  static const unsigned InvalidValue = 0;

public:
  SourceLoc() : Value(InvalidValue) {}
  SourceLoc(const SourceLoc &) = default;

  bool isValid() const { return !isInvalid(); }
  bool isInvalid() const { return Value == InvalidValue; }

  bool operator==(SourceLoc RHS) const { return Value == RHS.Value; }
  bool operator!=(SourceLoc RHS) const { return !(*this == RHS); }

  /// Return a source location advanced a specified number of bytes.
  SourceLoc getAdvancedLoc(int ByteOffset) const {
    assert(isValid() && "Can't advance an invalid location");
    SourceLoc Result = *this;
    Result.Value += ByteOffset;
    return Result;
  }
};

class SourceRange {
public:
  /// The source range is a half-open byte range [Start; End).
  SourceLoc Start, End;

  SourceRange() {}
  SourceRange(SourceLoc Loc) : Start(Loc), End(Loc) { }
  SourceRange(SourceLoc Start, SourceLoc End) : Start(Start), End(End) {
    assert(Start.isValid() == End.isValid() &&
           "Start and end should either both be valid or both be invalid!");
  }

  bool isValid() const { return Start.isValid(); }
  bool isInvalid() const { return Start.isInvalid(); }
};

class SourceManagerBase {
protected:
  SourceLoc NextUnassignedLoc;

  /// All source pieces, in order of increasing source location.
  std::vector<SourceRange> RegisteredRanges;

public:
  SourceManagerBase() : NextUnassignedLoc() {
    NextUnassignedLoc.Value = 1;
  }
  SourceManagerBase(const SourceManagerBase &) = delete;

  void operator=(const SourceManagerBase &) = delete;

  bool isBeforeInBuffer(SourceLoc LHS, SourceLoc RHS) const {
    // When we support multiple buffers, assert that locations come from the
    // same buffer.
    return LHS.Value < RHS.Value;
  }

  /// Returns true if range \c R contains the location \c Loc.
  bool containsLoc(SourceRange R, SourceLoc Loc) const {
    return Loc == R.Start ||
           (isBeforeInBuffer(R.Start, Loc) && isBeforeInBuffer(Loc, R.End));
  }
};

template <typename ExternalSourceLocTy>
class SourceManager : public SourceManagerBase {
  std::vector<ExternalSourceLocTy> ExternalLocs;

public:
  SourceManager() = default;
  SourceManager(const SourceManager &) = delete;

  void operator=(const SourceManager &) = delete;

  SourceRange registerLine(StringRef Line, ExternalSourceLocTy ExternalLoc);

  /// Returns the external source range and a byte offset inside it.
  std::pair<ExternalSourceLocTy, unsigned>
  toExternalSourceLoc(SourceLoc Loc) const;
};

template <typename ExternalSourceLocTy>
SourceRange SourceManager<ExternalSourceLocTy>::registerLine(
    StringRef Line, ExternalSourceLocTy ExternalLoc) {
  if (Line.size() > 4095)
    return SourceRange();

  SourceLoc Start = NextUnassignedLoc;
  SourceLoc End = Start.getAdvancedLoc(Line.size());
  RegisteredRanges.push_back(SourceRange(Start, End));
  ExternalLocs.push_back(ExternalLoc);
  NextUnassignedLoc = End.getAdvancedLoc(2);
#ifndef NDEBUG
  // To make debugging easier, make each line start at offset that is equal to
  // 1 mod 1000.
  NextUnassignedLoc.Value = ((NextUnassignedLoc.Value + 999) / 1000) * 1000 + 1;
#endif
  return SourceRange(Start, End);
}

template <typename ExternalSourceLocTy>
std::pair<ExternalSourceLocTy, unsigned>
SourceManager<ExternalSourceLocTy>::toExternalSourceLoc(SourceLoc Loc) const {
  auto I = std::lower_bound(RegisteredRanges.begin(), RegisteredRanges.end(),
                            Loc, [this](const SourceRange &LHS, SourceLoc Loc) {
    return this->isBeforeInBuffer(LHS.Start, Loc);
  });
  assert(I != RegisteredRanges.end() && "unknown source location");
  const auto &InternalRange = *I;
  assert(containsLoc(InternalRange, Loc) && "unknown source location");
  const auto &ExternalLoc = ExternalLocs[I - RegisteredRanges.begin()];
  return { ExternalLoc, Loc.Value - InternalRange.Start.Value };
}

} // namespace markup
} // namespace llvm

#endif // LLVM_REST_SOURCELOC_H

