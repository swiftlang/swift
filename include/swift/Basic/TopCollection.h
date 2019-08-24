//===--- TopCollection.h - A size-limiting top-N collection -----*- C++ -*-===//
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
// This file defines the TopCollection class, a data structure which,
// given a size limit, keeps the best-scoring (i.e. lowest) N values
// added to it.
//
// The current implementation of this is only suited for small values
// of maxSize.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_TOPCOLLECTION_H
#define SWIFT_BASIC_TOPCOLLECTION_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

template <class ScoreType, class T, unsigned InlineCapacity = 16>
class TopCollection {
public:
  struct Entry {
    ScoreType Score;
    T Value;
  };

private:
  SmallVector<Entry, InlineCapacity> Data;

  unsigned MaxSize;
  unsigned EndOfAccepted = 0;
public:
  explicit TopCollection(unsigned maxSize) : MaxSize(maxSize) {
    assert(maxSize > 0 && "creating collection with a maximum size of 0?");
    Data.reserve(maxSize);
  }

  // The invariants work fine with these.
  TopCollection(const TopCollection &other) = default;
  TopCollection &operator=(const TopCollection &other) = default;

  // We need to clear EndOfAccepted to preserve the invariants here.
  TopCollection(TopCollection &&other)
    : Data(std::move(other.Data)),
      MaxSize(other.MaxSize),
      EndOfAccepted(other.EndOfAccepted) {
    other.EndOfAccepted = 0;
  }
  TopCollection &operator=(TopCollection &&other) {
    Data = std::move(other.Data);
    MaxSize = other.MaxSize;
    EndOfAccepted = other.EndOfAccepted;
    other.EndOfAccepted = 0;
    return *this;
  }

  ~TopCollection() {}

  bool empty() const {
    return EndOfAccepted == 0;
  }

  size_t size() const {
    return EndOfAccepted;
  }

  using iterator = const Entry *;
  iterator begin() const { return Data.begin(); }
  iterator end() const { return Data.begin() + EndOfAccepted; }

  /// Return a score beyond which scores are uninteresting.  Inserting
  /// a value with this score will never change the collection.
  ScoreType getMinUninterestingScore(ScoreType defaultBound) const {
    assert(EndOfAccepted <= MaxSize);
    assert(EndOfAccepted <= Data.size());

    // If we've accepted as many values as we can, then all scores up (and
    // including) that value are interesting.
    if (EndOfAccepted == MaxSize)
      return Data[EndOfAccepted - 1].Score + 1;

    // Otherwise, if there are values in the collection that we've rejected,
    // any score up to that is still interesting.
    if (EndOfAccepted != Data.size())
      return Data[EndOfAccepted].Score;

    // Otherwise, use the default bound.
    return defaultBound;
  }

  /// Try to add a scored value to the collection.
  ///
  /// \return true if the insertion was successful
  bool insert(ScoreType score, T &&value) {
    assert(EndOfAccepted <= MaxSize);
    assert(EndOfAccepted <= Data.size());

    // Find the index of the last entry whose score is larger than 'score'.
    auto i = EndOfAccepted;
    while (i != 0 && score < Data[i - 1].Score)
      --i;

    assert(0 <= i && i <= EndOfAccepted);
    assert(i == 0 || score >= Data[i - 1].Score);

    // If we skipped anything, it's definitely safe to insert.
    if (i != EndOfAccepted) {
      // fall through

    // If there's a tie with the previous thing, we might have to declare
    // an existing tier off-bounds.
    } else if (i != 0 && score == Data[i - 1].Score) {
      // Only if there isn't space to expand the tier.
      if (i == MaxSize) {
        for (--i; i != 0; --i) {
          if (Data[i - 1].Score != Data[i].Score)
            break;
        }
        EndOfAccepted = i;
        return false;
      }

    // Otherwise, there's a non-trivial prefix that the new element has a
    // strictly higher score than.  We can still insert if there's space.
    } else {
      // Don't insert if there's no room.
      if (i == MaxSize)
        return false;

      // Don't insert if we're at least as high as things we've previously
      // rejected.
      if (i != Data.size() && score >= Data[i].Score)
        return false;
    }

    // We don't care about any of the actual values after EndOfAccepted
    // *except* that we need to remember the minimum value following
    // EndOfAccepted if that's less than MaxSize so that we continue to
    // drop values with that score.
    //
    // Note that all of the values between EndOfAccepted and MaxSize
    // should have the same score, because otherwise there's a tier we
    // shouldn't have marked dead.

    // Just overwrite the next element instead of inserting if possible.
    if (i == EndOfAccepted && i != Data.size()) {
      Data[i].Score = score;
      Data[i].Value = std::move(value);
    } else {
      if (Data.size() == MaxSize) {
        Data.pop_back();
        if (EndOfAccepted == MaxSize)
          EndOfAccepted--;
      }
      Data.insert(Data.begin() + i, { score, std::move(value) });
    }

    EndOfAccepted++;
    assert(EndOfAccepted <= Data.size());
    assert(EndOfAccepted <= MaxSize);
    return true;
  }

  /// Drop any values which a score more than the given value from the
  /// minimum score.
  template <class Range>
  void filterMaxScoreRange(Range difference) {
    if (EndOfAccepted < 2) return;
    for (unsigned i = 1; i != EndOfAccepted; ++i) {
      if (Data[i].Score > Data[0].Score + difference) {
        EndOfAccepted = i;
        return;
      }
    }

  }
};

} // end namespace swift

#endif // SWIFT_BASIC_CLUSTEREDBITVECTOR_H
