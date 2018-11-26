//===--- ModuleDiagsConsumer.cpp - Print module differ diagnostics --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the ModuleDifferDiagsConsumer class, which displays
//  diagnostics from the module differ as text to an output.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsModuleDiffer.h"
#include "ModuleDiagsConsumer.h"

using namespace swift;

namespace {
// Reproduce the DiagIDs, as we want both the size and access to the raw ids
// themselves.
enum LocalDiagID : uint32_t {
#define DIAG(KIND, ID, Options, Text, Signature) ID,
#include "swift/AST/DiagnosticsAll.def"
  NumDiags
};

static StringRef getCategoryName(uint32_t ID) {
  switch(ID) {
  case LocalDiagID::removed_decl:
    return "/* Removed Decls */";
  case LocalDiagID::moved_decl:
    return "/* Moved Decls */";
  case LocalDiagID::renamed_decl:
    return "/* Renamed Decls */";
  case LocalDiagID::decl_attr_change:
  case LocalDiagID::decl_new_attr:
    return "/* Decl Attribute changes */";
  case LocalDiagID::decl_type_change:
    return "/* Type Changes */";
  case LocalDiagID::raw_type_change:
    return "/* RawRepresentable Changes */";
  case LocalDiagID::generic_sig_change:
    return "/* Generic Signature Changes */";
  default:
    return StringRef();
  }
}
}

swift::ide::api::
ModuleDifferDiagsConsumer::ModuleDifferDiagsConsumer(bool DiagnoseModuleDiff):
    PrintingDiagnosticConsumer(llvm::errs()),
    DiagnoseModuleDiff(DiagnoseModuleDiff) {
#define DIAG(KIND, ID, Options, Text, Signature)                              \
  auto ID = getCategoryName(LocalDiagID::ID);                                 \
  assert(!ID.empty());                                                        \
  AllDiags[ID] = std::set<std::string>();
#include "swift/AST/DiagnosticsModuleDiffer.def"
}

void swift::ide::api::
ModuleDifferDiagsConsumer::handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind,
                        StringRef FormatString,
                        ArrayRef<DiagnosticArgument> FormatArgs,
                        const DiagnosticInfo &Info) {
  auto Category = getCategoryName((uint32_t)Info.ID);
  if (Category.empty()) {
    PrintingDiagnosticConsumer::handleDiagnostic(SM, Loc, Kind, FormatString,
      FormatArgs, Info);
    return;
  }
  if (!DiagnoseModuleDiff)
    return;
  llvm::SmallString<256> Text;
  {
    llvm::raw_svector_ostream Out(Text);
    DiagnosticEngine::formatDiagnosticText(Out, FormatString, FormatArgs);
  }
  AllDiags[Category].insert(Text.str().str());
}

swift::ide::api::ModuleDifferDiagsConsumer::~ModuleDifferDiagsConsumer() {
  for (auto &Pair: AllDiags) {
    llvm::outs() << "\n";
    llvm::outs() << Pair.first << "\n";
    for (auto &Item: Pair.second) {
      llvm::outs() << Item << "\n";
    }
  }
}
