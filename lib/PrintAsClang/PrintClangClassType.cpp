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
#include "DeclAndTypePrinter.h"
#include "PrintClangValueType.h"
#include "swift/AST/Decl.h"
#include "swift/IRGen/Linking.h"

using namespace swift;

void ClangClassTypePrinter::printClassTypeDecl(
    const ClassDecl *typeDecl, llvm::function_ref<void(void)> bodyPrinter,
    DeclAndTypePrinter &declAndTypePrinter) {
  auto printCxxImplClassName = ClangValueTypePrinter::printCxxImplClassName;

  ClangSyntaxPrinter printer(typeDecl->getASTContext(), os);

  auto typeMetadataFunc = irgen::LinkEntity::forTypeMetadataAccessFunction(
      typeDecl->getDeclaredType()->getCanonicalType());
  std::string typeMetadataFuncName = typeMetadataFunc.mangleAsString(typeDecl->getASTContext());

  printer.printParentNamespaceForNestedTypes(typeDecl, [&](raw_ostream &os) {
    // Print out a forward declaration of the "hidden" _impl class.
    printer.printNamespace(cxx_synthesis::getCxxImplNamespaceName(),
                           [&](raw_ostream &os) {
                             os << "class";
                             declAndTypePrinter.printAvailability(os, typeDecl);
                             os << ' ';
                             printCxxImplClassName(os, typeDecl);
                             os << ";\n";
                             // Print out special functions, like functions that
                             // access type metadata.
                             printer.printCTypeMetadataTypeFunction(
                                 typeDecl, typeMetadataFuncName, {});
                           });

    std::string baseClassName;
    std::string baseClassQualifiedName;

    if (auto *parentClass = typeDecl->getSuperclassDecl()) {
      llvm::raw_string_ostream baseNameOS(baseClassName);
      ClangSyntaxPrinter(typeDecl->getASTContext(), baseNameOS).printBaseName(parentClass);
      llvm::raw_string_ostream baseQualNameOS(baseClassQualifiedName);
      ClangSyntaxPrinter(typeDecl->getASTContext(), baseQualNameOS)
          .printModuleNamespaceQualifiersIfNeeded(
              parentClass->getModuleContext(), typeDecl->getModuleContext());
      baseQualNameOS << baseNameOS.str();
    } else {
      baseClassName = "RefCountedClass";
      baseClassQualifiedName = "swift::_impl::RefCountedClass";
    }

    os << "class";
    declAndTypePrinter.printAvailability(os, typeDecl);
    ClangSyntaxPrinter(typeDecl->getASTContext(), os).printSymbolUSRAttribute(typeDecl);
    os << ' ';
    printer.printBaseName(typeDecl);
    if (typeDecl->isFinal())
      os << " final";
    os << " : public " << baseClassQualifiedName;
    os << " {\n";
    os << "public:\n";

    os << "  using " << baseClassName << "::" << baseClassName << ";\n";
    os << "  using " << baseClassName << "::operator=;\n";
    bodyPrinter();
    os << "protected:\n";
    os << "  ";
    printer.printInlineForThunk();
    printer.printBaseName(typeDecl);
    os << "(void * _Nonnull ptr) noexcept : " << baseClassName << "(ptr) {}\n";
    os << "private:\n";
    os << "  friend class " << cxx_synthesis::getCxxImplNamespaceName() << "::";
    printCxxImplClassName(os, typeDecl);
    os << ";\n";

    printer.printSwiftMangledNameForDebugger(typeDecl);

    os << "};\n\n";

    // Print out the "hidden" _impl class.
    printer.printNamespace(
        cxx_synthesis::getCxxImplNamespaceName(), [&](raw_ostream &os) {
          os << "class";
          declAndTypePrinter.printAvailability(os, typeDecl);
          os << ' ';
          printCxxImplClassName(os, typeDecl);
          os << " {\n";
          os << "public:\n";
          os << "static ";
          printer.printInlineForThunk();
          printer.printBaseName(typeDecl);
          os << " makeRetained(void * _Nonnull ptr) noexcept { return ";
          printer.printBaseName(typeDecl);
          os << "(ptr); }\n";
          os << "};\n";
        });
  });

  ClangValueTypePrinter::printTypeGenericTraits(
      os, typeDecl, typeMetadataFuncName, /*genericRequirements=*/{},
      typeDecl->getModuleContext(), declAndTypePrinter);
}

void ClangClassTypePrinter::printClassTypeReturnScaffold(
    raw_ostream &os, const ClassDecl *type, const ModuleDecl *moduleContext,
    llvm::function_ref<void(void)> bodyPrinter) {
  ClangSyntaxPrinter printer(type->getASTContext(), os);
  os << "  return ";
  printer.printModuleNamespaceQualifiersIfNeeded(type->getModuleContext(),
                                                 moduleContext);
  if (!printer.printNestedTypeNamespaceQualifiers(type))
    os << "::";
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
