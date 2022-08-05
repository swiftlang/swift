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

class ModuleDecl;

namespace cxx_synthesis {

/// Return the name of the implementation namespace that is used to hide
/// declarations from the namespace that corresponds to the imported Swift
/// module in C++.
StringRef getCxxImplNamespaceName();

/// Return the name of the C++ class inside of `swift::_impl`
/// namespace that holds an opaque value, like a resilient struct.
StringRef getCxxOpaqueStorageClassName();

} // end namespace cxx_synthesis

class ClangSyntaxPrinter {
public:
  ClangSyntaxPrinter(raw_ostream &os) : os(os) {}

  /// Print a given identifier. If the identifer conflicts with a keyword, add a
  /// trailing underscore.
  void printIdentifier(StringRef name);

  /// Print the base name of the given declaration.
  void printBaseName(const ValueDecl *decl);

  /// Print the C-style prefix for the given module name, that's used for
  /// C type names inside the module.
  void printModuleNameCPrefix(const ModuleDecl &mod);

  /// Print the optional namespace qualifiers for the given module reference if
  /// it's not the same as the current context.
  void
  printModuleNamespaceQualifiersIfNeeded(const ModuleDecl *referencedModule,
                                         const ModuleDecl *currentContext);

  /// Print a C++ namespace declaration with the give name and body.
  void
  printNamespace(llvm::function_ref<void(raw_ostream &OS)> namePrinter,
                 llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

  void
  printNamespace(StringRef name,
                 llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

  /// Print an extern C block with given body.
  void
  printExternC(llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

  /// Print the `swift::_impl::` namespace qualifier.
  void printSwiftImplQualifier() const;

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

  /// Print the call expression to the Swift type metadata access function.
  void printSwiftTypeMetadataAccessFunctionCall(StringRef name);

  /// Print the set of statements to access the value witness table pointer
  /// ('vwTable') from the given type metadata variable.
  void printValueWitnessTableAccessSequenceFromTypeMetadata(
      StringRef metadataVariable, StringRef vwTableVariable, int indent);

protected:
  raw_ostream &os;
};

} // end namespace swift

#endif
