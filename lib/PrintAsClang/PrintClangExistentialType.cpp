//===--- PrintClangExistentialType.cpp - Print existential types -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "PrintClangExistentialType.h"
#include "ClangSyntaxPrinter.h"
#include "DeclAndTypePrinter.h"
#include "PrintClangValueType.h"
#include "swift/AST/Decl.h"

using namespace swift;

void ClangExistentialTypePrinter::printMarkerProtocolDecl(
    const ProtocolDecl *PD, DeclAndTypePrinter &declAndTypePrinter) {
  auto printCxxImplClassName = ClangValueTypePrinter::printCxxImplClassName;
  ClangSyntaxPrinter printer(PD->getASTContext(), os);

  printer.printParentNamespaceForNestedTypes(PD, [&](raw_ostream &os) {
    // Forward declaration of the _impl helper class.
    printer.printNamespace(cxx_synthesis::getCxxImplNamespaceName(),
                           [&](raw_ostream &os) {
                             os << "class";
                             declAndTypePrinter.printAvailability(os, PD);
                             os << ' ';
                             printCxxImplClassName(os, PD);
                             os << ";\n";
                           });

    // Marker protocol: empty subclass of swift::Any (zero witness tables).
    os << "class";
    declAndTypePrinter.printAvailability(os, PD);
    ClangSyntaxPrinter(PD->getASTContext(), os).printSymbolUSRAttribute(PD);
    os << ' ';
    printer.printBaseName(PD);
    os << " final : public swift::Any";
    os << " {\npublic:\n";

    os << "private:\n";
    os << "  ";
    printer.printInlineForThunk();
    printer.printBaseName(PD);
    os << "() noexcept : Any() {}\n";
    os << "  friend class " << cxx_synthesis::getCxxImplNamespaceName() << "::";
    printCxxImplClassName(os, PD);
    os << ";\n";

    printer.printSwiftMangledNameForDebugger(PD);

    os << "};\n\n";

    // The _impl helper class.
    printer.printNamespace(
        cxx_synthesis::getCxxImplNamespaceName(), [&](raw_ostream &os) {
          os << "class";
          declAndTypePrinter.printAvailability(os, PD);
          os << ' ';
          printCxxImplClassName(os, PD);
          os << " {\npublic:\n";
          os << "};\n";
        });
  });
}

void ClangExistentialTypePrinter::printExistentialTypeDecl(
    const ProtocolDecl *PD, DeclAndTypePrinter &declAndTypePrinter) {
  if (PD->isMarkerProtocol()) {
    printMarkerProtocolDecl(PD, declAndTypePrinter);
    return;
  }

  auto printCxxImplClassName = ClangValueTypePrinter::printCxxImplClassName;
  ClangSyntaxPrinter printer(PD->getASTContext(), os);

  printer.printParentNamespaceForNestedTypes(PD, [&](raw_ostream &os) {
    // Forward declaration of the _impl helper class.
    printer.printNamespace(cxx_synthesis::getCxxImplNamespaceName(),
                           [&](raw_ostream &os) {
                             os << "class";
                             declAndTypePrinter.printAvailability(os, PD);
                             os << ' ';
                             printCxxImplClassName(os, PD);
                             os << ";\n";
                           });

    // Existential wrapper class: subclass of SwiftExistentialType.
    os << "class";
    declAndTypePrinter.printAvailability(os, PD);
    ClangSyntaxPrinter(PD->getASTContext(), os).printSymbolUSRAttribute(PD);
    os << ' ';
    printer.printBaseName(PD);
    os << " final : public swift::_impl::SwiftExistentialType";
    os << " {\npublic:\n";

    os << "private:\n";
    os << "  ";
    printer.printInlineForThunk();
    printer.printBaseName(PD);
    os << "() noexcept : SwiftExistentialType(uninit_t{}) {}\n";
    os << "#pragma clang diagnostic push\n";
    os << "#pragma clang diagnostic ignored \"-Wunused-private-field\"\n";
    os << "  const void *_Nonnull _witnessTable;\n";
    os << "#pragma clang diagnostic pop\n";
    os << "  friend class " << cxx_synthesis::getCxxImplNamespaceName() << "::";
    printCxxImplClassName(os, PD);
    os << ";\n";

    printer.printSwiftMangledNameForDebugger(PD);

    os << "};\n\n";

    // The _impl helper class.
    printer.printNamespace(
        cxx_synthesis::getCxxImplNamespaceName(), [&](raw_ostream &os) {
          os << "class";
          declAndTypePrinter.printAvailability(os, PD);
          os << ' ';
          printCxxImplClassName(os, PD);
          os << " {\npublic:\n";
          os << "};\n";
        });
  });
}
