//===--- PrintClangValueType.cpp - Printer for C/C++ value types *- C++ -*-===//
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

#include "PrintClangValueType.h"
#include "ClangSyntaxPrinter.h"
#include "OutputLanguageMode.h"
#include "SwiftToClangInteropContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/IRGen/IRABIDetailsProvider.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;

void ClangValueTypePrinter::printStructDecl(const StructDecl *SD) {
  auto typeSizeAlign =
      interopContext.getIrABIDetails().getTypeSizeAlignment(SD);
  if (!typeSizeAlign) {
    // FIXME: handle non-fixed layout structs.
    return;
  }
  if (typeSizeAlign->size == 0) {
    // FIXME: How to represent 0 sized structs?
    return;
  }

  os << "class ";
  ClangSyntaxPrinter(os).printIdentifier(SD->getName().str());
  os << " final {\n";
  // FIXME: Print the other members of the struct.
  os << "private:\n";
  os << "  alignas(" << typeSizeAlign->alignment << ") ";
  os << "char _storage[" << typeSizeAlign->size << "];\n";
  os << "};\n";
}
