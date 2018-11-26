//===--- ModuleDiagsConsumer.h - Print module differ diagnostics --*- C++ -*-===//
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
//  This file defines the ModuleDifferDiagsConsumer class, which displays
//  diagnostics from the module differ as text to an output.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_MODULE_DIFFER_DIAGS_CONSUMER_H__
#define __SWIFT_MODULE_DIFFER_DIAGS_CONSUMER_H__

#include "llvm/ADT/MapVector.h"
#include "swift/Basic/LLVM.h"
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"

#include "llvm/Support/raw_ostream.h"
#include <set>

namespace swift {
namespace ide {
namespace api {

/// \brief Diagnostic consumer that displays diagnostics to standard output.
class ModuleDifferDiagsConsumer: public PrintingDiagnosticConsumer {
  bool DiagnoseModuleDiff;
  llvm::MapVector<StringRef, std::set<std::string>> AllDiags;
public:
  ModuleDifferDiagsConsumer(bool DiagnoseModuleDiff);
  ~ModuleDifferDiagsConsumer();
  void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind,
                        StringRef FormatString,
                        ArrayRef<DiagnosticArgument> FormatArgs,
                        const DiagnosticInfo &Info) override;
};
}
}
}

#endif
