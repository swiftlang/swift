//===--- AutoDiff.h - Swift Automatic Differentiation ---------------------===//
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
//  SWIFT_ENABLE_TENSORFLOW
//  This file defines AST support for automatic differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AUTODIFF_H
#define SWIFT_AST_AUTODIFF_H

#include "ASTContext.h"
#include "llvm/ADT/SmallBitVector.h"
#include "swift/Basic/Range.h"

namespace swift {

class ParsedAutoDiffParameter {
public:
  enum class Kind { Named, Ordered, Self };

private:
  SourceLoc Loc;
  Kind Kind;
  union Value {
    struct { Identifier Name; }; // Named
    struct { unsigned Index; }; // Ordered
    struct {};                  // Self
    Value(Identifier name) : Name(name) {}
    Value(unsigned index) : Index(index) {}
    Value() {}
  } V;

public:
  ParsedAutoDiffParameter(SourceLoc loc, enum Kind kind, Value value)
    : Loc(loc), Kind(kind), V(value) {}

  ParsedAutoDiffParameter(SourceLoc loc, enum Kind kind, unsigned index)
  : Loc(loc), Kind(kind), V(index) {}

  static ParsedAutoDiffParameter getNamedParameter(SourceLoc loc,
                                                   Identifier name) {
    return { loc, Kind::Named, name };
  }

  static ParsedAutoDiffParameter getOrderedParameter(SourceLoc loc,
                                                     unsigned index) {
    return { loc, Kind::Ordered, index };
  }

  static ParsedAutoDiffParameter getSelfParameter(SourceLoc loc) {
    return { loc, Kind::Self, {} };
  }

  Identifier getName() const {
    assert(Kind == Kind::Named);
    return V.Name;
  }

  unsigned getIndex() const {
    return V.Index;
  }

  enum Kind getKind() const {
    return Kind;
  }

  SourceLoc getLoc() const {
    return Loc;
  }

  bool isEqual(const ParsedAutoDiffParameter &other) const {
    if (getKind() != other.getKind())
      return false;
    if (getKind() == Kind::Named)
      return getName() == other.getName();
    return getKind() == Kind::Self;
  }
};

/// An efficient index subset data structure, uniqued in `ASTContext`.
/// Stores a bit vector representing set indices and a total capacity.
class AutoDiffIndexSubset : public llvm::FoldingSetNode {
public:
  typedef uint64_t BitWord;

  static constexpr unsigned bitWordSize = sizeof(BitWord);
  static constexpr unsigned numBitsPerBitWord = bitWordSize * 8;

  static std::pair<unsigned, unsigned>
  getBitWordIndexAndOffset(unsigned index) {
    auto bitWordIndex = index / numBitsPerBitWord;
    auto bitWordOffset = index % numBitsPerBitWord;
    return {bitWordIndex, bitWordOffset};
  }

  static unsigned getNumBitWordsNeededForCapacity(unsigned capacity) {
    if (capacity == 0) return 0;
    return capacity / numBitsPerBitWord + 1;
  }

private:
  /// The total capacity of the index subset, which is `1` less than the largest
  /// index.
  unsigned capacity;
  /// The number of bit words in the index subset.
  unsigned numBitWords;

  BitWord *getBitWordsData() {
    return reinterpret_cast<BitWord *>(this + 1);
  }

  const BitWord *getBitWordsData() const {
    return reinterpret_cast<const BitWord *>(this + 1);
  }

  ArrayRef<BitWord> getBitWords() const {
    return {getBitWordsData(), getNumBitWords()};
  }

  BitWord getBitWord(unsigned i) const {
    return getBitWordsData()[i];
  }

  BitWord &getBitWord(unsigned i) {
    return getBitWordsData()[i];
  }

  MutableArrayRef<BitWord> getMutableBitWords() {
    return {const_cast<BitWord *>(getBitWordsData()), getNumBitWords()};
  }

  explicit AutoDiffIndexSubset(const SmallBitVector &indices)
      : capacity((unsigned)indices.size()),
        numBitWords(getNumBitWordsNeededForCapacity(capacity)) {
    std::uninitialized_fill_n(getBitWordsData(), numBitWords, 0);
    for (auto i : indices.set_bits()) {
      unsigned bitWordIndex, offset;
      std::tie(bitWordIndex, offset) = getBitWordIndexAndOffset(i);
      getBitWord(bitWordIndex) |= (1 << offset);
    }
  }

public:
  AutoDiffIndexSubset() = delete;
  AutoDiffIndexSubset(const AutoDiffIndexSubset &) = delete;
  AutoDiffIndexSubset &operator=(const AutoDiffIndexSubset &) = delete;

  // Defined in ASTContext.cpp.
  static AutoDiffIndexSubset *get(ASTContext &ctx,
                                  const SmallBitVector &indices);

  static AutoDiffIndexSubset *get(ASTContext &ctx, unsigned capacity,
                                  ArrayRef<unsigned> indices) {
    SmallBitVector indicesBitVec(capacity, false);
    for (auto index : indices)
      indicesBitVec.set(index);
    return AutoDiffIndexSubset::get(ctx, indicesBitVec);
  }

  static AutoDiffIndexSubset *getDefault(ASTContext &ctx, unsigned capacity,
                                         bool includeAll = false) {
    return get(ctx, SmallBitVector(capacity, includeAll));
  }

  static AutoDiffIndexSubset *getFromRange(ASTContext &ctx, unsigned capacity,
                                           unsigned start, unsigned end) {
    assert(start < capacity);
    assert(end <= capacity);
    SmallBitVector bitVec(capacity);
    bitVec.set(start, end);
    return get(ctx, bitVec);
  }

  /// Creates an index subset corresponding to the given string generated by
  /// `getString()`. If the string is invalid, returns nullptr.
  static AutoDiffIndexSubset *getFromString(ASTContext &ctx, StringRef string);

  /// Returns the number of bit words used to store the index subset.
  // Note: Use `getCapacity()` to get the total index subset capacity.
  // This is public only for unit testing
  // (in unittests/AST/SILAutoDiffIndices.cpp).
  unsigned getNumBitWords() const {
    return numBitWords;
  }

  /// Returns the capacity of the index subset.
  unsigned getCapacity() const {
    return capacity;
  }

  /// Returns a textual string description of these indices.
  ///
  /// It has the format `[SU]+`, where the total number of characters is equal
  /// to the capacity, and where "S" means that the corresponding index is
  /// contained and "U" means that the corresponding index is not.
  std::string getString() const;

  class iterator;

  iterator begin() const {
    return iterator(this);
  }

  iterator end() const {
    return iterator(this, (int)capacity);
  }

  iterator_range<iterator> getIndices() const {
    return make_range(begin(), end());
  }

  unsigned getNumIndices() const {
    return (unsigned)std::distance(begin(), end());
  }

  SmallBitVector getBitVector() const {
    SmallBitVector indicesBitVec(capacity, false);
    for (auto index : getIndices())
      indicesBitVec.set(index);
    return indicesBitVec;
  }

  bool contains(unsigned index) const {
    unsigned bitWordIndex, offset;
    std::tie(bitWordIndex, offset) = getBitWordIndexAndOffset(index);
    return getBitWord(bitWordIndex) & (1 << offset);
  }

  bool isEmpty() const {
    return llvm::all_of(getBitWords(), [](BitWord bw) { return !(bool)bw; });
  }

  bool equals(AutoDiffIndexSubset *other) const {
    return capacity == other->getCapacity() &&
        getBitWords().equals(other->getBitWords());
  }

  bool isSubsetOf(AutoDiffIndexSubset *other) const;
  bool isSupersetOf(AutoDiffIndexSubset *other) const;

  AutoDiffIndexSubset *adding(unsigned index, ASTContext &ctx) const;
  AutoDiffIndexSubset *extendingCapacity(ASTContext &ctx,
                                         unsigned newCapacity) const;

  void Profile(llvm::FoldingSetNodeID &id) const {
    id.AddInteger(capacity);
    for (auto index : getIndices())
      id.AddInteger(index);
  }

  void print(llvm::raw_ostream &s = llvm::outs()) const {
    s << '{';
    interleave(range(capacity), [this, &s](unsigned i) { s << contains(i); },
               [&s] { s << ", "; });
    s << '}';
  }

  void dump(llvm::raw_ostream &s = llvm::errs()) const {
    s << "(autodiff_index_subset capacity=" << capacity << " indices=(";
    interleave(getIndices(), [&s](unsigned i) { s << i; },
               [&s] { s << ", "; });
    s << "))";
  }

  int findNext(int startIndex) const;
  int findFirst() const { return findNext(-1); }
  int findPrevious(int endIndex) const;
  int findLast() const { return findPrevious(capacity); }

  class iterator {
  public:
    typedef unsigned value_type;
    typedef unsigned difference_type;
    typedef unsigned * pointer;
    typedef unsigned & reference;
    typedef std::forward_iterator_tag iterator_category;

  private:
    const AutoDiffIndexSubset *parent;
    int current = 0;

    void advance() {
      assert(current != -1 && "Trying to advance past end.");
      current = parent->findNext(current);
    }

  public:
    iterator(const AutoDiffIndexSubset *parent, int current)
        : parent(parent), current(current) {}
    explicit iterator(const AutoDiffIndexSubset *parent)
        : iterator(parent, parent->findFirst()) {}
    iterator(const iterator &) = default;

    iterator operator++(int) {
      auto prev = *this;
      advance();
      return prev;
    }

    iterator &operator++() {
      advance();
      return *this;
    }

    unsigned operator*() const { return current; }

    bool operator==(const iterator &other) const {
      assert(parent == other.parent &&
             "Comparing iterators from different AutoDiffIndexSubsets");
      return current == other.current;
    }

    bool operator!=(const iterator &other) const {
      assert(parent == other.parent &&
             "Comparing iterators from different AutoDiffIndexSubsets");
      return current != other.current;
    }
  };
};

} // end namespace swift

#endif // SWIFT_AST_AUTODIFF_H
