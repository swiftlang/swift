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
#include "SwiftLangSupport.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/IDE/CodeCompletionResultPrinter.h"
#include "swift/IDE/CodeCompletionStringBuilder.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/SignatureHelp.h"
#include "swift/IDETool/IDEInspectionInstance.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

struct SignatureInfo {
  size_t LabelBegin;
  size_t LabelLength;
  StringRef DocComment;
  std::optional<unsigned> ActiveParam;
  SmallVector<SourceKit::SignatureHelpResult::Parameter, 1> Params;

  SignatureInfo() {}
};

/// \returns Array of parameters of \p VD accounting for implicitly curried
/// instance methods.
static ArrayRef<const ParamDecl *>
getParameterArray(const ValueDecl *VD, bool IsImplicitlyCurried,
                  const ParamDecl *&Scratch) {
  if (!VD)
    return {};

  if (IsImplicitlyCurried) {
    auto *FD = dyn_cast<AbstractFunctionDecl>(VD);
    assert(FD && FD->hasImplicitSelfDecl());

    Scratch = FD->getImplicitSelfDecl();
    return ArrayRef(&Scratch, 1);
  }

  if (auto *ParamList = VD->getParameterList())
    return ParamList->getArray();

  return {};
}

static CodeCompletionString *
createSignatureLabel(llvm::BumpPtrAllocator &Allocator, ValueDecl *FD,
                     AnyFunctionType *AFT, const DeclContext *DC,
                     GenericSignature GenericSig, bool IsSubscript,
                     bool IsMember, bool IsImplicitlyCurried) {
  CodeCompletionStringBuilder StringBuilder(
      Allocator, DC,
      /*AnnotateResults=*/false,
      /*UnderscoreEmptyArgumentLabel=*/!IsSubscript,
      /*FullParameterFlags=*/true);

  DeclBaseName BaseName;

  if (FD) {
    BaseName = FD->getBaseName();
  } else if (IsSubscript) {
    BaseName = DeclBaseName::createSubscript();
  }

  if (!BaseName.empty())
    StringBuilder.addValueBaseName(BaseName, IsMember);

  StringBuilder.addLeftParen();

  const ParamDecl *ParamScratch;
  StringBuilder.addCallArgumentPatterns(
      AFT->getParams(),
      getParameterArray(FD, IsImplicitlyCurried, ParamScratch), GenericSig,
      /*includeDefaultArgs=*/true, /*includeDefaultValues=*/true);

  StringBuilder.addRightParen();

  StringBuilder.addEffectsSpecifiers(
      AFT, dyn_cast_or_null<AbstractFunctionDecl>(FD));

  if (FD && FD->isImplicitlyUnwrappedOptional())
    StringBuilder.addTypeAnnotationForImplicitlyUnwrappedOptional(
        AFT->getResult(), GenericSig);
  else
    StringBuilder.addTypeAnnotation(AFT->getResult(), GenericSig);

  return StringBuilder.createCompletionString();
}

static void getSignatureInfo(const DeclContext *DC, const Signature &Sig,
                             SignatureInfo &Info,
                             SmallVectorImpl<char> &Scratch) {
  auto *FD = Sig.FuncD;
  auto *AFT = Sig.FuncTy;

  bool IsConstructor = false;
  GenericSignature genericSig;
  if (FD) {
    IsConstructor = isa<ConstructorDecl>(FD);

    if (auto *FDC = dyn_cast<DeclContext>(FD))
      genericSig = FDC->getGenericSignatureOfContext();
  }

  llvm::BumpPtrAllocator Allocator;
  auto *SignatureLabel = createSignatureLabel(
      Allocator, FD, AFT, DC, genericSig, Sig.IsSubscript,
      /*IsMember=*/bool(Sig.BaseType), Sig.IsImplicitlyCurried);

  llvm::raw_svector_ostream OS(Scratch);
  Info.LabelBegin = OS.tell();

  bool SkipResult = AFT->getResult()->isVoid() || IsConstructor;

  auto Chunks = SignatureLabel->getChunks();
  auto C = Chunks.begin();
  while (C != Chunks.end()) {
    if (C->is(ChunkKind::TypeAnnotation) && SkipResult) {
      ++C;
      continue;
    }

    if (C->is(ChunkKind::TypeAnnotation))
      OS << " -> ";

    if (C->is(ChunkKind::CallArgumentBegin)) {
      unsigned NestingLevel = C->getNestingLevel();
      ++C;

      auto &P = Info.Params.emplace_back();
      P.LabelBegin = OS.tell() - Info.LabelBegin;

      do {
        if (!C->is(ChunkKind::CallArgumentClosureType) && C->hasText())
          OS << C->getText();

        ++C;
      } while (C != Chunks.end() && !C->endsPreviousNestedGroup(NestingLevel));

      P.LabelLength = OS.tell() - P.LabelBegin - Info.LabelBegin;
    }

    if (C->hasText())
      OS << C->getText();

    ++C;
  }

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

static void deliverResults(SourceKit::SignatureHelpConsumer &SKConsumer,
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

      getSignatureInfo(Result->Result->DC, Sig, Info, Scratch);
    }

    SourceKit::SignatureHelpResult SKResult;
    SmallVector<SourceKit::SignatureHelpResult::Signature, 8> SKSignatures;

    for (auto &Info : Infos) {
      StringRef Label(Scratch.begin() + Info.LabelBegin, Info.LabelLength);
      SKSignatures.push_back(
          {Label, Info.DocComment, Info.ActiveParam, Info.Params});
    }

    SKResult.Signatures = SKSignatures;

    // TODO(a7medev): Select active signature and param.
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
