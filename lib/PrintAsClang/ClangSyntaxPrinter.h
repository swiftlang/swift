//===--- ClangSyntaxPrinter.h - Printer for C and C++ code ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRINTASCLANG_CLANGSYNTAXPRINTER_H
#define SWIFT_PRINTASCLANG_CLANGSYNTAXPRINTER_H

#include "swift/Basic/LLVM.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

namespace cxx_synthesis {

/// Return the name of the implementation namespace that is used to hide
/// declarations from the namespace that corresponds to the imported Swift
/// module in C++.
StringRef getCxxImplNamespaceName();

} // end namespace cxx_synthesis

class ClangSyntaxPrinter {
public:
  ClangSyntaxPrinter(raw_ostream &os) : os(os) {}

  /// Print a given identifier. If the identifer conflicts with a keyword, add a
  /// trailing underscore.
  void printIdentifier(StringRef name);

  /// Print a C++ namespace declaration with the give name and body.
  void
  printNamespace(llvm::function_ref<void(raw_ostream &OS)> namePrinter,
                 llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

  void
  printNamespace(StringRef name,
                 llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

  /// Where nullability information should be printed.
  enum class NullabilityPrintKind {
    Before,
    After,
    ContextSensitive,
  };

  void printNullability(
      Optional<OptionalTypeKind> kind,
      NullabilityPrintKind printKind = NullabilityPrintKind::After) const;

  /// Returns true if \p name matches a keyword in any Clang language mode.
  static bool isClangKeyword(StringRef name);
  static bool isClangKeyword(Identifier name);

protected:
  raw_ostream &os;
};

} // end namespace swift

#endif
