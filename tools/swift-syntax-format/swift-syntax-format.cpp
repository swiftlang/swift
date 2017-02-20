//===--- swift-syntax-test.cpp - Reflection Syntax testing application ----===//
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
// This is a host-side tool to format Swift code.
//
//===----------------------------------------------------------------------===//


#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Syntax/Format.h"
#include "swift/Syntax/LegacyASTTransformer.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace swift::syntax;
using llvm::ArrayRef;
using llvm::StringRef;

int doFormat(ArrayRef<StringRef> InputFiles) {

  for (auto InputFilename : InputFiles) {
    CompilerInvocation Invocation;
    Invocation.addInputFilename(InputFilename);
    Invocation.setModuleName("Format");
    CompilerInstance Instance;

    auto &SourceMgr = Instance.getSourceMgr();
    auto &Diags = Instance.getDiags();

    PrintingDiagnosticConsumer DiagPrinter;
    Instance.addDiagnosticConsumer(&DiagPrinter);
    if (Instance.setup(Invocation)) {
      return EXIT_FAILURE;
    }

    // First, parse the file normally and get the regular old AST.
    Instance.performParseOnly();

    auto BufferID = Instance.getInputBufferIDs().back();
    auto &SF = Instance.getMainModule()
      ->getMainSourceFile(SourceFileKind::Main);

    auto Buffer = llvm::MemoryBuffer::getFile(InputFilename);
    if (!Buffer) {
      Diags.diagnose(SourceLoc(), diag::cannot_open_file,
                     InputFilename, Buffer.getError().message());
      return EXIT_FAILURE;
    }

    auto Tokens = tokenizeWithTrivia(Invocation.getLangOptions(),
                                     SourceMgr, BufferID);

    SmallVector<Decl *, 256> FileDecls;
    SF.getTopLevelDecls(FileDecls);
    sema::Semantics Sema;
    for (auto *Decl : FileDecls) {
      if (Decl->escapedFromIfConfig()) {
        continue;
      }
      auto NewNode = transformAST(ASTNode(Decl), Sema, SourceMgr,
                                  BufferID, Tokens);
      if (NewNode.hasValue()) {
        auto Reformatted = format(NewNode.getValue());
        Reformatted.print(llvm::outs());
      }
    }
  }
  return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "A tool to format Swift code\n");

  return doFormat({"/Users/david/test.swift"});
}
