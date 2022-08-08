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
#include "swift/IRGen/Linking.h"

using namespace swift;

void ClangClassTypePrinter::printClassTypeDecl(
    const ClassDecl *typeDecl, llvm::function_ref<void(void)> bodyPrinter) {
  auto printCxxImplClassName = ClangValueTypePrinter::printCxxImplClassName;

  ClangSyntaxPrinter printer(os);

  auto typeMetadataFunc = irgen::LinkEntity::forTypeMetadataAccessFunction(
      typeDecl->getDeclaredType()->getCanonicalType());
  std::string typeMetadataFuncName = typeMetadataFunc.mangleAsString();

  // Print out a forward declaration of the "hidden" _impl class.
  printer.printNamespace(cxx_synthesis::getCxxImplNamespaceName(),
                         [&](raw_ostream &os) {
                           os << "class ";
                           printCxxImplClassName(os, typeDecl);
                           os << ";\n";
                           // Print out special functions, like functions that
                           // access type metadata.
                           printer.printCTypeMetadataTypeFunction(
                               typeDecl, typeMetadataFuncName);
                         });

  std::string baseClassName;
  std::string baseClassQualifiedName;

  if (auto *parentClass = typeDecl->getSuperclassDecl()) {
    llvm::raw_string_ostream baseNameOS(baseClassName);
    ClangSyntaxPrinter(baseNameOS).printBaseName(parentClass);
    llvm::raw_string_ostream baseQualNameOS(baseClassQualifiedName);
    ClangSyntaxPrinter(baseQualNameOS)
        .printModuleNamespaceQualifiersIfNeeded(parentClass->getModuleContext(),
                                                typeDecl->getModuleContext());
    if (!baseQualNameOS.str().empty())
      baseQualNameOS << "::";
    baseQualNameOS << baseNameOS.str();
  } else {
    baseClassName = "RefCountedClass";
    baseClassQualifiedName = "swift::_impl::RefCountedClass";
  }

  os << "class ";
  printer.printBaseName(typeDecl);
  if (typeDecl->isFinal())
    os << " final";
  os << " : public " << baseClassQualifiedName;
  os << " {\n";
  os << "public:\n";

  os << "  using " << baseClassName << "::" << baseClassName << ";\n";
  os << "  using " << baseClassName << "::operator=;\n";

  os << "protected:\n";
  os << "  inline ";
  printer.printBaseName(typeDecl);
  os << "(void * _Nonnull ptr) noexcept : " << baseClassName << "(ptr) {}\n";
  os << "private:\n";
  os << "  friend class " << cxx_synthesis::getCxxImplNamespaceName() << "::";
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

  // FIXME: avoid popping out of the module's namespace here.
  os << "} // end namespace \n\n";
  os << "namespace swift {\n";

  os << "#pragma clang diagnostic push\n";
  os << "#pragma clang diagnostic ignored \"-Wc++17-extensions\"\n";
  os << "template<>\n";
  os << "static inline const constexpr bool isUsableInGenericContext<";
  printer.printBaseName(typeDecl->getModuleContext());
  os << "::";
  printer.printBaseName(typeDecl);
  os << "> = true;\n";
  os << "#pragma clang diagnostic pop\n";

  os << "template<>\n";
  os << "inline void * _Nonnull getTypeMetadata<";
  printer.printBaseName(typeDecl->getModuleContext());
  os << "::";
  printer.printBaseName(typeDecl);
  os << ">() {\n";
  os << "  return ";
  printer.printBaseName(typeDecl->getModuleContext());
  os << "::" << cxx_synthesis::getCxxImplNamespaceName()
     << "::" << typeMetadataFuncName << "(0)._0;\n";
  os << "}\n";
  os << "namespace " << cxx_synthesis::getCxxImplNamespaceName() << "{\n";
  os << "template<>\n";
  os << "struct implClassFor<";
  printer.printBaseName(typeDecl->getModuleContext());
  os << "::";
  printer.printBaseName(typeDecl);
  os << "> { using type = ";
  printer.printBaseName(typeDecl->getModuleContext());
  os << "::" << cxx_synthesis::getCxxImplNamespaceName() << "::";
  printCxxImplClassName(os, typeDecl);
  os << "; };\n";
  os << "} // namespace\n";
  os << "} // namespace swift\n";
  os << "\nnamespace ";
  printer.printBaseName(typeDecl->getModuleContext());
  os << " {\n";
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
