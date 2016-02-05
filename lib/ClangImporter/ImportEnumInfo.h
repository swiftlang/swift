//===--- ImportEnumInfo.h - Importable Clang enums information --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides ImportEnumInfo, which describes a Clang enum ready to be
// imported
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANG_IMPORT_ENUM_H
#define SWIFT_CLANG_IMPORT_ENUM_H

#include "swift/AST/Decl.h"
#include "clang/AST/Attr.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/DenseMap.h"

namespace clang {
class EnumDecl;
class Preprocessor;
class MacroInfo;
}

namespace swift {
namespace importer {

/// Describes how a particular C enumeration type will be imported
/// into Swift. All of the possibilities have the same storage
/// representation, but can be used in different ways.
enum class EnumKind {
  /// The enumeration type should map to an enum, which means that
  /// all of the cases are independent.
  Enum,
  /// The enumeration type should map to an option set, which means
  /// that
  /// the constants represent combinations of independent flags.
  Options,
  /// The enumeration type should map to a distinct type, but we don't
  /// know the intended semantics of the enum constants, so conservatively
  /// map them to independent constants.
  Unknown,
  /// The enumeration constants should simply map to the appropriate
  /// integer values.
  Constants,
};

class EnumInfo {
  using AttributePtrUnion = clang::NSErrorDomainAttr *;

  /// The kind
  EnumKind kind = EnumKind::Unknown;

  /// The enum's common constant name prefix, which will be stripped from
  /// constants
  StringRef constantNamePrefix = StringRef();

  /// The identifying attribute for specially imported enums
  ///
  /// Currently, only NS_ERROR_ENUM creates one for its error domain, but others
  /// should in the future.
  AttributePtrUnion attribute = nullptr;

public:
  EnumInfo() = default;

  EnumInfo(const clang::EnumDecl *decl, clang::Preprocessor &pp) {
    classifyEnum(decl, pp);
    determineConstantNamePrefix(decl);
  }

  EnumKind getKind() const { return kind; }

  StringRef getConstantNamePrefix() const { return constantNamePrefix; }

  /// Whether this maps to an enum who also provides an error domain
  bool isErrorEnum() const {
    return getKind() == EnumKind::Enum && attribute;
  }

  /// For this error enum, extract the name of the error domain constant
  clang::IdentifierInfo *getErrorDomain() const {
    assert(isErrorEnum() && "not error enum");
    return attribute->getErrorDomain();
  }

private:
  void determineConstantNamePrefix(const clang::EnumDecl *);
  void classifyEnum(const clang::EnumDecl *, clang::Preprocessor &);
};
}
}

#endif // SWIFT_CLANG_IMPORT_ENUM_H
