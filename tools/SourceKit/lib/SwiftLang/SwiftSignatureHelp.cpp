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

namespace {
struct SignatureInfo {
  StringRef Text;
  StringRef DocComment;
  std::optional<unsigned> ActiveParam;
  SmallVector<SourceKit::SignatureHelpResponse::Parameter, 1> Params;

  SignatureInfo() {}
};
} // namespace

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

static CodeCompletionString *createSignatureString(
    llvm::BumpPtrAllocator &Allocator, ValueDecl *FD, AnyFunctionType *AFT,
    const DeclContext *DC, GenericSignature GenericSig, bool IsSubscript,
    bool IsMember, bool IsImplicitlyCurried, bool IsSecondApply) {
  CodeCompletionStringBuilder StringBuilder(
      Allocator, /*AnnotateResults=*/false,
      /*UnderscoreEmptyArgumentLabel=*/!IsSubscript,
      /*FullParameterFlags=*/true);

  DeclBaseName BaseName;

  if (!IsSecondApply && FD) {
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
      getParameterArray(FD, IsImplicitlyCurried, ParamScratch), DC, GenericSig,
      DefaultArgumentOutputMode::All, /*includeDefaultValues=*/true);

  StringBuilder.addRightParen();

  if (!IsImplicitlyCurried) {
    // For a second apply, we don't pass the declaration to avoid adding
    // incorrect rethrows and reasync which are only usable in a single apply.
    StringBuilder.addEffectsSpecifiers(
        AFT,
        /*AFD=*/IsSecondApply ? nullptr
                              : dyn_cast_or_null<AbstractFunctionDecl>(FD));
  }

  if (FD && FD->isImplicitlyUnwrappedOptional()) {
    StringBuilder.addTypeAnnotationForImplicitlyUnwrappedOptional(
        AFT->getResult(), DC, GenericSig);
  } else {
    StringBuilder.addTypeAnnotation(AFT->getResult(), DC, GenericSig);
  }

  return StringBuilder.createCompletionString();
}

static StringRef copyAndClearString(llvm::BumpPtrAllocator &Allocator,
                                    SmallVectorImpl<char> &Str) {
  auto Ref = StringRef(Str.data(), Str.size()).copy(Allocator);
  Str.clear();
  return Ref;
}

static void getSignatureInfo(const DeclContext *DC, const Signature &Sig,
                             SignatureInfo &Info,
                             llvm::BumpPtrAllocator &Allocator) {
  auto *FD = Sig.FuncD;
  auto *AFT = Sig.FuncTy;

  bool IsConstructor = false;
  GenericSignature genericSig;
  if (FD) {
    IsConstructor = isa<ConstructorDecl>(FD);

    if (auto *GC = FD->getAsGenericContext())
      genericSig = GC->getGenericSignature();
  }

  auto *SignatureString =
      createSignatureString(Allocator, FD, AFT, DC, genericSig, Sig.IsSubscript,
                            /*IsMember=*/bool(Sig.BaseType),
                            Sig.IsImplicitlyCurried, Sig.IsSecondApply);

  llvm::SmallString<512> SS;
  llvm::raw_svector_ostream OS(SS);

  bool SkipResult = AFT->getResult()->isVoid() || IsConstructor;

  auto Chunks = SignatureString->getChunks();
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
      P.Offset = SS.size();

      do {
        if (!C->is(ChunkKind::CallArgumentClosureType) && C->hasText())
          OS << C->getText();

        ++C;
      } while (C != Chunks.end() && !C->endsPreviousNestedGroup(NestingLevel));

      P.Length = SS.size() - P.Offset;
      continue;
    }

    if (C->hasText())
      OS << C->getText();

    ++C;
  }

  Info.Text = copyAndClearString(Allocator, SS);
  Info.ActiveParam = Sig.ParamIdx;

  // Parameter names.
  const ParamDecl *ParamScratch;
  auto ParamDecls =
      getParameterArray(FD, Sig.IsImplicitlyCurried, ParamScratch);

  if (!ParamDecls.empty()) {
    for (unsigned i = 0; i < Info.Params.size(); ++i) {
      Info.Params[i].Name = ParamDecls[i]->getParameterName().str();
    }
  }

  // Documentation.
  if (FD) {
    ide::getRawDocumentationComment(FD, OS);

    Info.DocComment = copyAndClearString(Allocator, SS);
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

    llvm::BumpPtrAllocator Allocator;
    SmallVector<SignatureInfo, 4> Infos;

    for (auto &Sig : Result->Result->Signatures) {
      Infos.emplace_back();
      auto &Info = Infos.back();

      getSignatureInfo(Result->Result->DC, Sig, Info, Allocator);
    }

    SourceKit::SignatureHelpResponse SKResult;
    SmallVector<SourceKit::SignatureHelpResponse::Signature, 8> SKSignatures;

    for (auto &Info : Infos) {
      SKSignatures.push_back(
          {Info.Text, Info.DocComment, Info.ActiveParam, Info.Params});
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
    StringRef PrimaryFilePath, unsigned Offset, ArrayRef<const char *> Args,
    SourceKitCancellationToken CancellationToken,
    SignatureHelpConsumer &SKConsumer, std::optional<VFSOptions> vfsOptions) {
  std::string error;

  auto fileSystem = getFileSystem(vfsOptions, PrimaryFilePath, error);
  if (!fileSystem) {
    return SKConsumer.failed(error);
  }

  std::string InputFileError;
  std::unique_ptr<llvm::MemoryBuffer> InputBuffer =
      getASTManager()->getMemoryBuffer(PrimaryFilePath, fileSystem,
                                       InputFileError);
  if (!InputBuffer) {
    return SKConsumer.failed(InputFileError);
  }

  performWithParamsToCompletionLikeOperation(
      InputBuffer.get(), Offset, /*InsertCodeCompletionToken=*/true, Args,
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
