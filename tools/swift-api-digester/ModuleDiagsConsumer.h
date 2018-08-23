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

#include "swift/Basic/LLVM.h"
#include "swift/AST/DiagnosticConsumer.h"

#include "llvm/Support/raw_ostream.h"

namespace swift {
namespace ide {
namespace api {

/// \brief Diagnostic consumer that displays diagnostics to standard output.
class ModuleDifferDiagsConsumer: public PrintingDiagnosticConsumer {
public:
  ModuleDifferDiagsConsumer(llvm::raw_ostream &OS):
    PrintingDiagnosticConsumer(OS) {}

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
