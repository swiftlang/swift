//===--- PrintClangValueType.h - Printer for C/C++ value types --*- C++ -*-===//
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

#ifndef SWIFT_PRINTASCLANG_PRINTCLANGVALUETYPE_H
#define SWIFT_PRINTASCLANG_PRINTCLANGVALUETYPE_H

#include "OutputLanguageMode.h"
#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class ModuleDecl;
class NominalTypeDecl;
class PrimitiveTypeMapping;
class StructDecl;
class SwiftToClangInteropContext;

/// Responsible for printing a Swift struct or enum decl or in C or C++ mode, to
/// be included in a Swift module's generated clang header.
class ClangValueTypePrinter {
public:
  ClangValueTypePrinter(raw_ostream &os, raw_ostream &cPrologueOS,
                        PrimitiveTypeMapping &typeMapping,
                        SwiftToClangInteropContext &interopContext)
      : os(os), cPrologueOS(cPrologueOS), typeMapping(typeMapping),
        interopContext(interopContext) {}

  /// Print the C++ class definition that
  /// corresponds to the given structure or enum declaration.
  void printValueTypeDecl(const NominalTypeDecl *typeDecl,
                          llvm::function_ref<void(void)> bodyPrinter);

  /// Print the pararameter type that referes to a Swift struct type in C/C++.
  void printValueTypeParameterType(const NominalTypeDecl *type,
                                   OutputLanguageMode outputLang,
                                   const ModuleDecl *moduleContext,
                                   bool isInOutParam);

  /// Print the use of a C++ struct/enum parameter value as it's passed to the
  /// underlying C function that represents the native Swift function.
  void
  printParameterCxxToCUseScaffold(bool isIndirect, const NominalTypeDecl *type,
                                  const ModuleDecl *moduleContext,
                                  llvm::function_ref<void()> cxxParamPrinter,
                                  bool isInOut, bool isSelf);

  /// Print the return type that refers to a Swift struct type in C/C++.
  void printValueTypeReturnType(const NominalTypeDecl *typeDecl,
                                OutputLanguageMode outputLang,
                                const ModuleDecl *moduleContext);

  /// Print the supporting code  that's required to indirectly return a C++
  /// class that represents a Swift value type as it's being indirectly passed
  /// from the C function that represents the native Swift function.
  void printValueTypeIndirectReturnScaffold(
      const NominalTypeDecl *typeDecl, const ModuleDecl *moduleContext,
      llvm::function_ref<void(StringRef)> bodyPrinter);

  /// Print the supporting code  that's required to directly return a C++ class
  /// that represents a Swift value type as it's being returned from the C
  /// function that represents the native Swift function.
  void
  printValueTypeDirectReturnScaffold(const NominalTypeDecl *typeDecl,
                                     const ModuleDecl *moduleContext,
                                     llvm::function_ref<void()> bodyPrinter);

private:
  /// Prints out the C stub name used to pass/return value directly for the
  /// given value type.
  ///
  /// If the C stub isn't declared yet in the emitted header, that declaration
  /// will be emitted by this function.
  void printCStubTypeName(const NominalTypeDecl *type);

  raw_ostream &os;
  raw_ostream &cPrologueOS;
  PrimitiveTypeMapping &typeMapping;
  SwiftToClangInteropContext &interopContext;
};

} // end namespace swift

#endif
