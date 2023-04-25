//===--- PrintClangClassType.h - Print class types in C/C++ -----*- C++ -*-===//
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

#ifndef SWIFT_PRINTASCLANG_PRINTCLANGCLASSTYPE_H
#define SWIFT_PRINTASCLANG_PRINTCLANGCLASSTYPE_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class ClassDecl;
class ModuleDecl;
class DeclAndTypePrinter;

/// Responsible for printing a Swift class decl or in C or C++ mode, to
/// be included in a Swift module's generated clang header.
class ClangClassTypePrinter {
public:
  ClangClassTypePrinter(raw_ostream &os) : os(os) {}

  /// Print the C++ class definition that corresponds to the given Swift class.
  void printClassTypeDecl(const ClassDecl *typeDecl,
                          llvm::function_ref<void(void)> bodyPrinter,
                          DeclAndTypePrinter &declAndTypePrinter);

  static void
  printClassTypeReturnScaffold(raw_ostream &os, const ClassDecl *type,
                               const ModuleDecl *moduleContext,
                               llvm::function_ref<void(void)> bodyPrinter);

  static void printParameterCxxtoCUseScaffold(
      raw_ostream &os, const ClassDecl *type, const ModuleDecl *moduleContext,
      llvm::function_ref<void(void)> bodyPrinter, bool isInOut);

private:
  raw_ostream &os;
};

} // end namespace swift

#endif
