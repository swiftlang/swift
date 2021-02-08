//===--- Decl.h - Swift Language Declaration ASTs ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines some data types for rethrows and reasync effects.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_EFFECTS_H
#define SWIFT_EFFECTS_H

#include "swift/AST/Type.h"

#include <utility>

namespace llvm {
class raw_ostream;
}

namespace swift {

class ValueDecl;

class ProtocolRethrowsRequirementList {
public:
  typedef std::pair<Type, ValueDecl *> Entry;

private:
  ArrayRef<Entry> entries;

public:
  ProtocolRethrowsRequirementList(ArrayRef<Entry> entries) : entries(entries) {}
  ProtocolRethrowsRequirementList() : entries() {}

  typedef const Entry *const_iterator;
  typedef const_iterator iterator;

  const_iterator begin() const { return entries.begin(); }
  const_iterator end() const { return entries.end(); }

  size_t size() const { return entries.size(); }

  void print(raw_ostream &OS) const;

  SWIFT_DEBUG_DUMP;
};

void simple_display(llvm::raw_ostream &out, const ProtocolRethrowsRequirementList reqs);

enum class FunctionRethrowingKind : uint8_t {
  /// The function is not throwing
  None,

  /// The function rethrows by closure
  ByClosure, 

  /// The function rethrows by conformance
  ByConformance, 

  /// The function throws
  Throws, 

  /// The function throwing determinate is invalid
  Invalid
};

void simple_display(llvm::raw_ostream &out, FunctionRethrowingKind value);

} // end namespace swift

#endif