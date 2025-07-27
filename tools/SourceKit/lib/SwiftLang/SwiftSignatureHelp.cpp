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

  /// Structures are printed as a tree, we maintain the the current depth in that tree to
  /// determine whether the parameter we're adding is the function's parameters or a
  /// a parameter for a subtype being printed (e.g. parameter for a closure parameter).
  unsigned StructureDepth = 0;

  // A direct function parameter lies at depth 2:
  // FunctionType -> FunctionParameter
  static constexpr unsigned DirectParamDepth = 2;

public:
  SignaturePrinter(SignatureInfo &Info, llvm::raw_ostream &OS)
      : StreamPrinter(OS), Info(Info) { }

  void printStructurePre(PrintStructureKind Kind, const Decl *D) override {
    StructureDepth++;

    if (Kind == PrintStructureKind::FunctionParameter &&
        StructureDepth == DirectParamDepth) {
      Info.Params.emplace_back();
      Info.Params.back().LabelBegin = OS.tell() - Info.LabelBegin;
    }
  }

  void printStructurePost(PrintStructureKind Kind, const Decl *D) override {
    if (Kind == PrintStructureKind::FunctionParameter &&
        StructureDepth == DirectParamDepth) {
      Info.Params.back().LabelLength =
          OS.tell() - Info.Params.back().LabelBegin - Info.LabelBegin;
    }

    StructureDepth--;
  }
};

static void getSignatureInfo(const Signature &Sig, SignatureInfo &Info,
                             SmallVectorImpl<char> &Scratch) {
  auto *FD = Sig.FuncD;
  auto *AFT = Sig.FuncTy;

  llvm::raw_svector_ostream OS(Scratch);

  Info.LabelBegin = OS.tell();

  DeclBaseName BaseName;

  if (FD) {
    BaseName = FD->getBaseName();
  } else {
    assert(Sig.IsSubscript && "Only implicit subscripts are expected not to "
                              "have a declaration");
    BaseName = DeclBaseName::createSubscript();
  }

  PrintOptions PO = PrintOptions::printQuickHelpDeclaration();
  SignaturePrinter Printer(Info, OS);

  if (BaseName.isSpecial()) {
    OS << BaseName.userFacingName();
  } else {
    OS << "func ";

    Printer.printName(BaseName.getIdentifier(),
                      Sig.BaseType ? PrintNameContext::TypeMember
                                   : PrintNameContext::Normal);
  }

  NonRecursivePrintOptions NRPO =
      NonRecursivePrintOption::SkipFunctionTypeVoidResult;

  if (isa_and_nonnull<ConstructorDecl>(FD))
    NRPO |= NonRecursivePrintOption::SkipFunctionTypeResult;

  AFT->print(Printer, PO, NRPO);

  Info.LabelLength = OS.tell() - Info.LabelBegin;
  Info.ActiveParam = Sig.ParamIdx;

  // Documentation.
  if (FD) {
    unsigned DocCommentBegin = OS.tell();
    // TODO(a7medev): Separate parameter documentation.
    ide::getDocumentationCommentAsXML(FD, OS);
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
