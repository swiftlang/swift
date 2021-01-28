//===--- AccessNotes.h - Access Notes ---------------------------*- C++ -*-===//
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
//  Implements access notes, which allow certain modifiers or attributes to be
//  added to the declarations in a module.
//
//===----------------------------------------------------------------------===//

#ifndef ACCESSNOTES_H
#define ACCESSNOTES_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/NullablePtr.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include <vector>

namespace swift {
class ASTContext;
class ValueDecl;

class AccessNoteDeclName {
public:
  std::vector<Identifier> parentNames;
  DeclName name;

  AccessNoteDeclName(ASTContext &ctx, StringRef str);
  AccessNoteDeclName();

  bool matches(ValueDecl *VD) const;
  bool empty() const;

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP;
};

class AccessNote {
public:
  AccessNoteDeclName name;

  Optional<bool> ObjC;
  Optional<bool> Dynamic;
  Optional<ObjCSelector> ObjCName;

  void dump(llvm::raw_ostream &os, int indent = 0) const;
  SWIFT_DEBUG_DUMP;
};

class AccessNotes {
public:
  std::string reason;
  std::vector<AccessNote> notes;

  static llvm::Expected<AccessNotes> load(ASTContext &ctx,
                                          llvm::MemoryBuffer *buffer);

  NullablePtr<const AccessNote> lookup(ValueDecl *VD) const;

  void dump(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP;
};

}

#endif
