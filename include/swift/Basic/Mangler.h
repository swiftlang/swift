//===--- Mangler.h - Base class for Swift name mangling ---------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_MANGLER_H
#define SWIFT_BASIC_MANGLER_H

#include "swift/Basic/ManglingUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/raw_ostream.h"

using llvm::StringRef;
using llvm::ArrayRef;

namespace swift {
namespace NewMangling {

/// Select an old or new mangled string, based on useNewMangling().
///
/// Also performs test to check if the demangling of both string yield the same
/// demangling tree.
/// TODO: remove this function when the old mangling is removed.
std::string selectMangling(const std::string &Old, const std::string &New,
                           bool compareTrees = true);

void printManglingStats();

/// The basic Swift symbol mangler.
///
/// This class serves as an abstract base class for specific manglers. It
/// provides some basic utilities, like handling of substitutions, mangling of
/// identifiers, etc.
class Mangler {
protected:
  template <typename Mangler>
  friend void mangleIdentifier(Mangler &M, StringRef ident);

  /// The storage for the mangled symbol.
  llvm::SmallVector<char, 128> Storage;

  /// The output stream for the mangled symbol.
  llvm::raw_svector_ostream Buffer;

  /// A temporary storage needed by the ::mangleIdentifier() template function.
  llvm::SmallVector<WordReplacement, 8> SubstWordsInIdent;

  /// Substitutions, except identifier substitutions.
  llvm::DenseMap<const void *, unsigned> Substitutions;

  /// Identifier substitutions.
  llvm::StringMap<unsigned> StringSubstitutions;

  /// The position in the Buffer where the last substitution was written.
  int lastSubstIdx = -2;

  /// Word substitutions in mangled identifiers.
  llvm::SmallVector<SubstitutionWord, 26> Words;

  /// If enabled, non-ASCII names are encoded in modified Punycode.
  bool UsePunycode;

  /// A helpful little wrapper for an integer value that should be mangled
  /// in a particular, compressed value.
  class Index {
    unsigned N;
  public:
    explicit Index(unsigned n) : N(n) {}
    friend llvm::raw_ostream &operator<<(llvm::raw_ostream &out, Index n) {
      if (n.N != 0) out << (n.N - 1);
      return (out << '_');
    }
  };

  /// Returns the buffer as a StringRef, needed by mangleIdentifier().
  StringRef getBufferStr() const {
    return StringRef(Storage.data(), Storage.size());
  }

protected:

  Mangler(bool usePunycode) : Buffer(Storage), UsePunycode(usePunycode) { }

  /// Adds the mangling prefix.
  void beginMangling();

  /// Finish the mangling of the symbol and return the mangled name.
  std::string finalize();

  /// Finish the mangling of the symbol and write the mangled name into
  /// \p stream.
  void finalize(llvm::raw_ostream &stream);

  /// Appends a mangled identifier string.
  void appendIdentifier(StringRef ident);

  void addSubstitution(const void *ptr) {
    Substitutions[ptr] = Substitutions.size() + StringSubstitutions.size();
  }
  void addSubstitution(StringRef Str) {
    StringSubstitutions[Str] = Substitutions.size() + StringSubstitutions.size();
  }

  bool tryMangleSubstitution(const void *ptr);
  
  void mangleSubstitution(unsigned Index);

#ifndef NDEBUG
  void recordOpStatImpl(StringRef op, size_t OldPos);
#endif

  void recordOpStat(StringRef op, size_t OldPos) {
#ifndef NDEBUG
    recordOpStatImpl(op, OldPos);
#endif
  }

  void appendOperator(StringRef op) {
    size_t OldPos = Storage.size();
    Buffer << op;
    recordOpStat(op, OldPos);
  }
  void appendOperator(StringRef op, int natural) {
    size_t OldPos = Storage.size();
    Buffer << op << natural << '_';
    recordOpStat(op, OldPos);
  }
  void appendOperator(StringRef op, Index index) {
    size_t OldPos = Storage.size();
    Buffer << op << index;
    recordOpStat(op, OldPos);
  }
  void appendOperator(StringRef op, Index index1, Index index2) {
    size_t OldPos = Storage.size();
    Buffer << op << index1 << index2;
    recordOpStat(op, OldPos);
  }
  void appendOperator(StringRef op, StringRef arg) {
    size_t OldPos = Storage.size();
    Buffer << op << arg;
    recordOpStat(op, OldPos);
  }
  void appendListSeparator() {
    appendOperator("_");
  }
  void appendListSeparator(bool &isFirstListItem) {
    if (isFirstListItem) {
      appendListSeparator();
      isFirstListItem = false;
    }
  }
  void appendOperatorParam(StringRef op) {
    Buffer << op;
  }
  void appendOperatorParam(StringRef op, int natural) {
    Buffer << op << natural << '_';
  }
  void appendOperatorParam(StringRef op, Index index) {
    Buffer << op << index;
  }
  void appendOperatorParam(StringRef op, Index index1, Index index2) {
    Buffer << op << index1 << index2;
  }
  void appendOperatorParam(StringRef op, StringRef arg) {
    Buffer << op << arg;
  }
};

} // end namespace Mangle
} // end namespace swift

#endif // SWIFT_BASIC_MANGLER_H
