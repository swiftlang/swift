//===--- PrintClangClassType.cpp - Print class types in C/C++ ---*- C++ -*-===//
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

#include "PrintClangClassType.h"
#include "ClangSyntaxPrinter.h"
#include "PrintClangValueType.h"
#include "swift/AST/Decl.h"

using namespace swift;

void ClangClassTypePrinter::printClassTypeDecl(
    const ClassDecl *typeDecl, llvm::function_ref<void(void)> bodyPrinter) {
  auto printCxxImplClassName = ClangValueTypePrinter::printCxxImplClassName;

  ClangSyntaxPrinter printer(os);

  // Print out a forward declaration of the "hidden" _impl class.
  printer.printNamespace(cxx_synthesis::getCxxImplNamespaceName(),
                         [&](raw_ostream &os) {
                           os << "class ";
                           printCxxImplClassName(os, typeDecl);
                           os << ";\n";
                         });

  StringRef baseClassName = "RefCountedClass";
  StringRef baseClassQualifiedName = "swift::_impl::RefCountedClass";

  os << "class ";
  printer.printBaseName(typeDecl);
  // FIXME: Add support for inherintance.
  os << " final : public " << baseClassQualifiedName;
  os << " {\n";
  os << "public:\n";

  os << "  using " << baseClassName << "::" << baseClassName << ";\n";
  os << "  using " << baseClassName << "::operator=;\n";

  os << "private:\n";
  os << "  inline ";
  printer.printBaseName(typeDecl);
  os << "(void * _Nonnull ptr) noexcept : " << baseClassName << "(ptr) {}\n";
  os << "\n  friend class " << cxx_synthesis::getCxxImplNamespaceName() << "::";
  printCxxImplClassName(os, typeDecl);
  os << ";\n";
  os << "};\n\n";

  // Print out the "hidden" _impl class.
  printer.printNamespace(
      cxx_synthesis::getCxxImplNamespaceName(), [&](raw_ostream &os) {
        os << "class ";
        printCxxImplClassName(os, typeDecl);
        os << " {\n";
        os << "public:\n";
        os << "static inline ";
        printer.printBaseName(typeDecl);
        os << " makeRetained(void * _Nonnull ptr) noexcept { return ";
        printer.printBaseName(typeDecl);
        os << "(ptr); }\n";
        os << "};\n";
      });
}

void ClangClassTypePrinter::printClassTypeReturnScaffold(
    raw_ostream &os, const ClassDecl *type, const ModuleDecl *moduleContext,
    llvm::function_ref<void(void)> bodyPrinter) {
  os << "  return ";
  ClangSyntaxPrinter(os).printModuleNamespaceQualifiersIfNeeded(
      type->getModuleContext(), moduleContext);
  os << cxx_synthesis::getCxxImplNamespaceName() << "::";
  ClangValueTypePrinter::printCxxImplClassName(os, type);
  os << "::makeRetained(";
  bodyPrinter();
  os << ");\n";
}

void ClangClassTypePrinter::printParameterCxxtoCUseScaffold(
    raw_ostream &os, const ClassDecl *type, const ModuleDecl *moduleContext,
    llvm::function_ref<void(void)> bodyPrinter, bool isInOut) {
  if (isInOut)
    os << '&';
  os << "::swift::" << cxx_synthesis::getCxxImplNamespaceName()
     << "::_impl_RefCountedClass"
     << "::getOpaquePointer";
  if (isInOut)
    os << "Ref";
  os << '(';
  bodyPrinter();
  os << ')';
}
