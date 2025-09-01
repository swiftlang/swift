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

    SmallString<512> SS;
    llvm::raw_svector_ostream OS(SS);

    struct SignatureInfo {
      size_t LabelBegin;
      size_t LabelLength;
      StringRef DocComment;
      std::optional<unsigned> ActiveParam;
      SmallVector<SourceKit::SignatureHelpResult::Parameter, 0> Params;
      
      SignatureInfo() {}
    };
    
    SmallVector<SignatureInfo, 8> Signatures;

    for (auto signature : Result->Result->Signatures) {
      Signatures.emplace_back();
      auto &signatureElem = Signatures.back();

      // Label.
      signatureElem.LabelBegin = SS.size();
      // TODO(a7medev): Add parameter documentation.
      auto &Params = signatureElem.Params;
      // TODO(a7medev): Replace `printMemberDeclDescription` with logic similar to code completion.
      //                for benefits like: handling subscripts, implicit subscripts (e.g. [keyPath:]), generics, etc.
      SwiftLangSupport::printMemberDeclDescription(
          signature.FuncD, signature.BaseType, /*usePlaceholder=*/false, OS);
      signatureElem.LabelLength = SS.size() - signatureElem.LabelBegin;
      signatureElem.ActiveParam = signature.ParamIdx;

      // Documentation.
      unsigned DocCommentBegin = SS.size();
      ide::getDocumentationCommentAsXML(signature.FuncD, OS);
      unsigned DocCommentLength = SS.size() - DocCommentBegin;
      
      StringRef DocComment(SS.begin() + DocCommentBegin, DocCommentLength);
      signatureElem.DocComment = DocComment;
    }

    SourceKit::SignatureHelpResult SKResult;
    SmallVector<SourceKit::SignatureHelpResult::Signature, 8> SKSignatures;

    for (auto &info : Signatures) {
      StringRef Label(SS.begin() + info.LabelBegin, info.LabelLength);
      SKSignatures.push_back({Label, info.DocComment, info.ActiveParam, info.Params});
    }

    SKResult.Signatures = SKSignatures;

    // TODO(a7medev): Select active signature.
    SKResult.ActiveSignature = 0;
    
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
