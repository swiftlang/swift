//===--- RemanglerBase.h - String to Node-Tree Demangling -------*- C++ -*-===//
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
// This file contains shared code between the old and new remanglers.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_BASEREMANGLER_H
#define SWIFT_DEMANGLING_BASEREMANGLER_H

#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/NamespaceMacros.h"
#include "llvm/ADT/PointerIntPair.h"
#include <unordered_map>

using namespace swift::Demangle;
using llvm::StringRef;

namespace swift {
namespace Demangle {
SWIFT_BEGIN_INLINE_NAMESPACE

#define RETURN_IF_ERROR(x)                                                     \
  do {                                                                         \
    ManglingError err = (x);                                                   \
    if (!err.isSuccess())                                                      \
      return err;                                                              \
  } while (0)

// An entry in the remangler's substitution map.
class SubstitutionEntry {
  llvm::PointerIntPair<Node *, 1, bool> NodeAndTreatAsIdentifier;
  size_t StoredHash = 0;

  Node *getNode() const { return NodeAndTreatAsIdentifier.getPointer(); }

  bool getTreatAsIdentifier() const {
    return NodeAndTreatAsIdentifier.getInt();
  }

public:
  void setNode(Node *node, bool treatAsIdentifier, size_t hash) {
    NodeAndTreatAsIdentifier.setPointer(node);
    NodeAndTreatAsIdentifier.setInt(treatAsIdentifier);
    StoredHash = hash;
  }

  struct Hasher {
    size_t operator()(const SubstitutionEntry &entry) const {
      return entry.StoredHash;
    }
  };

  bool isEmpty() const { return !getNode(); }

  bool matches(Node *node, bool treatAsIdentifier) const {
    return node == getNode() && treatAsIdentifier == getTreatAsIdentifier();
  }

  size_t hash() const { return StoredHash; }

private:
  friend bool operator==(const SubstitutionEntry &lhs,
                         const SubstitutionEntry &rhs) {
    if (lhs.StoredHash != rhs.StoredHash)
      return false;
    if (lhs.getTreatAsIdentifier() != rhs.getTreatAsIdentifier())
      return false;
    if (lhs.getTreatAsIdentifier()) {
      return identifierEquals(lhs.getNode(), rhs.getNode());
    }
    return lhs.deepEquals(lhs.getNode(), rhs.getNode());
  }

  static bool identifierEquals(Node *lhs, Node *rhs);

  bool deepEquals(Node *lhs, Node *rhs) const;
};

/// The output string for the Remangler.
///
/// It's allocating the string with the provided Factory.
class RemanglerBuffer {
  CharVector Stream;
  NodeFactory &Factory;

public:
  RemanglerBuffer(NodeFactory &Factory) : Factory(Factory) {
    Stream.init(Factory, 32);
  }

  void reset(size_t toPos) { Stream.resetSize(toPos); }

  StringRef strRef() const { return Stream.str(); }

  RemanglerBuffer &operator<<(char c) & {
    Stream.push_back(c, Factory);
    return *this;
  }

  RemanglerBuffer &operator<<(llvm::StringRef Value) & {
    Stream.append(Value, Factory);
    return *this;
  }

  RemanglerBuffer &operator<<(int n) & {
    Stream.append(n, Factory);
    return *this;
  }

  RemanglerBuffer &operator<<(unsigned n) & {
    Stream.append((unsigned long long)n, Factory);
    return *this;
  }

  RemanglerBuffer &operator<<(unsigned long n) & {
    Stream.append((unsigned long long)n, Factory);
    return *this;
  }

  RemanglerBuffer &operator<<(unsigned long long n) & {
    Stream.append(n, Factory);
    return *this;
  }
};

/// The base class for the old and new remangler.
class RemanglerBase {
protected:
  // Used to allocate temporary nodes and the output string (in Buffer).
  NodeFactory &Factory;

  // Recursively calculating the node hashes can be expensive if the node tree
  // is deep, so we keep a hash table mapping (Node *, treatAsIdentifier) pairs
  // to hashes.
  static const size_t HashHashCapacity = 512; // Must be a power of 2
  static const size_t HashHashMaxProbes = 8;
  SubstitutionEntry HashHash[HashHashCapacity] = {};

  // An efficient hash-map implementation in the spirit of llvm's SmallPtrSet:
  // The first 16 substitutions are stored in an inline-allocated array to avoid
  // malloc calls in the common case.
  // Lookup is still reasonable fast because there are max 16 elements in the
  // array.
  static const size_t InlineSubstCapacity = 16;
  SubstitutionEntry InlineSubstitutions[InlineSubstCapacity];
  size_t NumInlineSubsts = 0;

  // The "overflow" for InlineSubstitutions. Only if InlineSubstitutions is
  // full, new substitutions are stored in OverflowSubstitutions.
  std::unordered_map<SubstitutionEntry, unsigned, SubstitutionEntry::Hasher>
    OverflowSubstitutions;

  RemanglerBuffer Buffer;

protected:
  RemanglerBase(NodeFactory &Factory)
    : Factory(Factory), Buffer(Factory) { }

  /// Compute the hash for a node.
  size_t hashForNode(Node *node, bool treatAsIdentifier = false);

  /// Construct a SubstitutionEntry for a given node.
  /// This will look in the HashHash to see if we already know the hash,
  /// to avoid having to walk the entire subtree.
  SubstitutionEntry entryForNode(Node *node, bool treatAsIdentifier = false);

  /// Find a substitution and return its index.
  /// Returns -1 if no substitution is found.
  int findSubstitution(const SubstitutionEntry &entry);

  /// Adds a substitution.
  void addSubstitution(const SubstitutionEntry &entry);

  /// Resets the output string buffer to \p toPos.
  void resetBuffer(size_t toPos) { Buffer.reset(toPos); }

public:

  /// Append a custom string to the output buffer.
  void append(StringRef str) { Buffer << str; }

  StringRef getBufferStr() const { return Buffer.strRef(); }

  std::string str() {
    return getBufferStr().str();
  }
};

SWIFT_END_INLINE_NAMESPACE
} // end namespace Demangle
} // end namespace swift

#endif // SWIFT_DEMANGLING_BASEREMANGLER_H
