//===--- SwiftSignatureHelp.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SwiftASTManager.h"
#include "SwiftEditorDiagConsumer.h"
#include "SwiftLangSupport.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/SignatureHelp.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDETool/IDEInspectionInstance.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Comment.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Type.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

struct SignatureInfo {
  size_t LabelBegin;
  size_t LabelLength;
  StringRef DocComment;
  std::optional<unsigned> ActiveParam;
  SmallVector<SourceKit::SignatureHelpResult::Parameter, 1> Params;
  
  SignatureInfo() {}
};

class SignaturePrinter : public StreamPrinter {
  SignatureInfo &Info;

public:
  SignaturePrinter(SignatureInfo &Info, llvm::raw_ostream &OS)
      : StreamPrinter(OS), Info(Info) { }

  void printStructurePre(PrintStructureKind Kind, const Decl *D) override {
    if (Kind == PrintStructureKind::FunctionParameter) {
      Info.Params.emplace_back();
      Info.Params.back().LabelBegin = OS.tell() - Info.LabelBegin;
    }
  }

  void printStructurePost(PrintStructureKind Kind, const Decl *D) override {
    if (Kind == PrintStructureKind::FunctionParameter) {
      Info.Params.back().LabelLength =
          OS.tell() - Info.Params.back().LabelBegin - Info.LabelBegin;
    }
  }
};

static void getSignatureInfo(const Signature &Sig, SignatureInfo &Info,
                             SmallVectorImpl<char> &Scratch) {
  llvm::raw_svector_ostream OS(Scratch);

  Info.LabelBegin = OS.tell();

  DeclBaseName BaseName;
  if (auto *D = Sig.FuncD) {
    BaseName = D->getBaseName();
  } else {
    assert(Sig.IsSubscript && "Only implicit subscripts are expected not to "
                              "have a declaration");
    BaseName = DeclBaseName::createSubscript();
  }

  SignaturePrinter Printer(Info, OS);

  if (BaseName.isSpecial()) {
    Printer.printText(BaseName.userFacingName());
  } else {
    Printer.printName(BaseName.getIdentifier(),
                      Sig.BaseType ? PrintNameContext::TypeMember
                                   : PrintNameContext::Normal);
  }

  PrintOptions Opts = PrintOptions::printQuickHelpDeclaration();
  // TODO(a7medev): () to Void (optional)
  // TODO(a7medev): Omit return type if Void
  // TODO(a7medev): Initializers should have original type name and shouldn't have a return value
  Sig.FuncTy->print(Printer, Opts);

  Info.LabelLength = OS.tell() - Info.LabelBegin;
  Info.ActiveParam = Sig.ParamIdx;

  // Documentation.
  if (auto *D = Sig.FuncD) {
    unsigned DocCommentBegin = OS.tell();
    // TODO(a7medev): Separate parameter documentation.
    ide::getDocumentationCommentAsXML(D, OS);
    unsigned DocCommentLength = OS.tell() - DocCommentBegin;

    StringRef DocComment(Scratch.begin() + DocCommentBegin, DocCommentLength);
    Info.DocComment = DocComment;
  }
}

static void
deliverResults(SourceKit::SignatureHelpConsumer &SKConsumer,
               CancellableResult<SignatureHelpResults> Result) {
  switch (Result.getKind()) {
  case CancellableResultKind::Success: {
    SKConsumer.setReusingASTContext(Result->DidReuseAST);

    if (!Result->Result) {
      // If we have no results, don't call SKConsumer.handleResult which causes
      // empty results to be delivered.
      break;
    }

    SmallString<512> Scratch;
    SmallVector<SignatureInfo, 4> Infos;

    for (auto &Sig : Result->Result->Signatures) {
      Infos.emplace_back();
      auto &Info = Infos.back();

      getSignatureInfo(Sig, Info, Scratch);
    }

    SourceKit::SignatureHelpResult SKResult;
    SmallVector<SourceKit::SignatureHelpResult::Signature, 8> SKSignatures;

    for (auto &Info : Infos) {
      StringRef Label(Scratch.begin() + Info.LabelBegin, Info.LabelLength);
      SKSignatures.push_back({Label, Info.DocComment, Info.ActiveParam, Info.Params});
    }

    SKResult.Signatures = SKSignatures;

    // TODO(a7medev): Select active signature and param.
    SKResult.ActiveSignature = 0;
    SKResult.ActiveParam = 0;
    
    SKConsumer.handleResult(SKResult);
    break;
  }
  case CancellableResultKind::Failure:
    SKConsumer.failed(Result.getError());
    break;
  case CancellableResultKind::Cancelled:
    SKConsumer.cancelled();
    break;
  }
}

void SwiftLangSupport::getSignatureHelp(
    llvm::MemoryBuffer *UnresolvedInputFile, unsigned Offset,
    ArrayRef<const char *> Args, SourceKitCancellationToken CancellationToken,
    SignatureHelpConsumer &SKConsumer, std::optional<VFSOptions> vfsOptions) {
  std::string error;

  // FIXME: the use of None as primary file is to match the fact we do not read
  // the document contents using the editor documents infrastructure.
  auto fileSystem =
      getFileSystem(vfsOptions, /*primaryFile=*/std::nullopt, error);
  if (!fileSystem) {
    return SKConsumer.failed(error);
  }

  performWithParamsToCompletionLikeOperation(
      UnresolvedInputFile, Offset, /*InsertCodeCompletionToken=*/true, Args,
      fileSystem, CancellationToken,
      [&](CancellableResult<CompletionLikeOperationParams> ParmsResult) {
        ParmsResult.mapAsync<SignatureHelpResults>(
            [&](auto &Params, auto DeliverTransformed) {
              getIDEInspectionInstance()->signatureHelp(
                  Params.Invocation, Args, fileSystem, Params.completionBuffer,
                  Offset, Params.DiagC, Params.CancellationFlag,
                  DeliverTransformed);
            },
            [&](auto Result) { deliverResults(SKConsumer, Result); });
      });
}
