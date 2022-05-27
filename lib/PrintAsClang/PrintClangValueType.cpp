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
#include "DeclAndTypePrinter.h"
#include "OutputLanguageMode.h"
#include "PrimitiveTypeMapping.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;

void ClangValueTypePrinter::printStructDecl(const StructDecl *SD) {
  os << "class ";
  ClangSyntaxPrinter(os).printIdentifier(SD->getName().str());
  os << " final {\n";
  // FIXME: Print the members of the struct.
  os << "};\n";
}
