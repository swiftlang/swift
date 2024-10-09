//===--- swift_indent_main.cpp - Swift code formatting tool ---------------===//
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
//  Formats Swift files or file ranges according to a set of parameters.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/Indenting.h"
#include "swift/Option/Options.h"
#include "swift/Subsystems.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

#include <string>
#include <vector>

using namespace swift;
using namespace swift::ide;
using namespace llvm::opt;

class FormatterDocument {
private:
  SourceManager SM;
  unsigned BufferID;
  CompilerInvocation CompInv;
  std::unique_ptr<ParserUnit> Parser;
  class FormatterDiagConsumer : public swift::DiagnosticConsumer {
    void handleDiagnostic(SourceManager &SM,
                          const swift::DiagnosticInfo &Info) override {
      llvm::errs() << "Parse error: ";
      DiagnosticEngine::formatDiagnosticText(llvm::errs(), Info.FormatString,
                                             Info.FormatArgs);
      llvm::errs() << "\n";
    }
  } DiagConsumer;

public:
  FormatterDocument(std::unique_ptr<llvm::MemoryBuffer> Buffer) {
    // Formatting logic requires tokens on source file.
    CompInv.getLangOptions().CollectParsedToken = true;
    updateCode(std::move(Buffer));
  }

  void updateCode(std::unique_ptr<llvm::MemoryBuffer> Buffer) {
    BufferID = SM.addNewSourceBuffer(std::move(Buffer));
    Parser.reset(new ParserUnit(
        SM, SourceFileKind::Main, BufferID, CompInv.getLangOptions(),
        CompInv.getTypeCheckerOptions(), CompInv.getSILOptions(),
        CompInv.getModuleName()));
    Parser->getDiagnosticEngine().addConsumer(DiagConsumer);
    Parser->parse();
  }

  std::pair<LineRange, std::string> reformat(LineRange Range,
                                             CodeFormatOptions Options) {
    return ::reformat(Range, Options, SM, Parser->getSourceFile());
  }

  const llvm::MemoryBuffer &memBuffer() const {
    return *SM.getLLVMSourceMgr().getMemoryBuffer(BufferID);
  }
};
