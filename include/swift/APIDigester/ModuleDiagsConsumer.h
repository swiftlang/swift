//===--- ModuleDiagsConsumer.h - Print module differ diagnostics --*- C++ -*-===//
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
//
//  This file defines the ModuleDifferDiagsConsumer class, which displays
//  diagnostics from the module differ as text to an output.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_MODULE_DIFFER_DIAGS_CONSUMER_H__
#define __SWIFT_MODULE_DIFFER_DIAGS_CONSUMER_H__

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSet.h"
#include <set>

namespace swift {
namespace ide {
namespace api {

/// Diagnostic consumer that displays diagnostics to standard output.
class ModuleDifferDiagsConsumer: public PrintingDiagnosticConsumer {
  llvm::raw_ostream &OS;
  bool DiagnoseModuleDiff;
  llvm::MapVector<StringRef, std::set<std::string>> AllDiags;
public:
  ModuleDifferDiagsConsumer(bool DiagnoseModuleDiff,
                            llvm::raw_ostream &OS = llvm::errs());
  ~ModuleDifferDiagsConsumer();
  void handleDiagnostic(SourceManager &SM, const DiagnosticInfo &Info) override;
};

class FilteringDiagnosticConsumer: public DiagnosticConsumer {
  bool HasError = false;
  std::vector<std::unique_ptr<DiagnosticConsumer>> subConsumers;
  std::unique_ptr<llvm::StringSet<>> allowedBreakages;
  bool DowngradeToWarning;
  bool shouldProceed(const DiagnosticInfo &Info);
public:
  FilteringDiagnosticConsumer(std::vector<std::unique_ptr<DiagnosticConsumer>> subConsumers,
                              std::unique_ptr<llvm::StringSet<>> allowedBreakages,
                              bool DowngradeToWarning):
    subConsumers(std::move(subConsumers)),
    allowedBreakages(std::move(allowedBreakages)),
    DowngradeToWarning(DowngradeToWarning) {}
  ~FilteringDiagnosticConsumer() = default;

  void flush() override;
  void informDriverOfIncompleteBatchModeCompilation() override;
  bool finishProcessing() override;
  bool hasError() const { return HasError; }

  void handleDiagnostic(SourceManager &SM,
                        const DiagnosticInfo &Info) override;
};
}
}
}

#endif
