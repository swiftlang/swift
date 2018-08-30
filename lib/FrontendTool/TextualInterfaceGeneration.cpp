//===--- TextualInterfaceGeneration.cpp - swiftinterface files ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TextualInterfaceGeneration.h"

#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/Serialization/InlinableText.h"

using namespace swift;

bool swift::emitModuleInterface(raw_ostream &out, ModuleDecl *M) {
  PrintOptions printOptions = PrintOptions::printTextualInterfaceFile();
  printOptions.DefaultArgument = [&](const ParamDecl *pd) -> std::string {
    SmallString<128> scratch;
    return extractDefaultArgumentText(pd, scratch);
  };
  SmallVector<Decl *, 16> topLevelDecls;
  M->getTopLevelDecls(topLevelDecls);
  for (const Decl *D : topLevelDecls) {
    if (!D->shouldPrintInContext(printOptions))
      continue;
    D->print(out, printOptions);
    out << "\n";
  }
  return false;
}
