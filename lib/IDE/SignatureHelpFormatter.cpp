//===--- SignatureHelpFormatter.cpp --- -------------------------*- C++ -*-===//
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

#include "swift/IDE/SignatureHelpFormatter.h"
#include "CodeCompletionStringBuilder.h"
#include "swift/AST/ParameterList.h"
#include "swift/IDE/CommentConversion.h"

using namespace swift;
using namespace swift::ide;

using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

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

static StringRef copyAndClearString(llvm::BumpPtrAllocator &Allocator,
                                    SmallVectorImpl<char> &Str) {
  auto Ref = StringRef(Str.data(), Str.size()).copy(Allocator);
  Str.clear();
  return Ref;
}

CodeCompletionString *
SignatureHelpFormatter::createSignatureString(const ide::Signature &Signature,
                                              const DeclContext *DC) {
  ValueDecl *FD = Signature.FuncD;
  AnyFunctionType *AFT = Signature.FuncTy;

  GenericSignature GenericSig;
  if (FD) {
    if (auto *GC = FD->getAsGenericContext())
      GenericSig = GC->getGenericSignature();
  }

  CodeCompletionStringBuilder StringBuilder(
      Allocator, /*AnnotateResults=*/false,
      /*UnderscoreEmptyArgumentLabel=*/!Signature.IsSubscript,
      /*FullParameterFlags=*/true);

  DeclBaseName BaseName;

  if (!Signature.IsSecondApply && FD) {
    BaseName = FD->getBaseName();
  } else if (Signature.IsSubscript) {
    BaseName = DeclBaseName::createSubscript();
  }

  if (!BaseName.empty())
    StringBuilder.addValueBaseName(BaseName,
                                   /*IsMember=*/bool(Signature.BaseType));

  StringBuilder.addLeftParen();

  const ParamDecl *ParamScratch;
  StringBuilder.addCallArgumentPatterns(
      AFT->getParams(),
      getParameterArray(FD, Signature.IsImplicitlyCurried, ParamScratch), DC,
      GenericSig, DefaultArgumentOutputMode::All,
      /*includeDefaultValues=*/true);

  StringBuilder.addRightParen();

  if (!Signature.IsImplicitlyCurried) {
    if (Signature.IsSecondApply) {
      // For a second apply, we don't pass the declaration to avoid adding
      // incorrect rethrows and reasync which are only usable in a single apply.
      StringBuilder.addEffectsSpecifiers(AFT, /*AFD=*/nullptr);
    } else {
      StringBuilder.addEffectsSpecifiers(
          AFT, dyn_cast_or_null<AbstractFunctionDecl>(FD));
    }
  }

  if (FD && FD->isImplicitlyUnwrappedOptional()) {
    StringBuilder.addTypeAnnotationForImplicitlyUnwrappedOptional(
        AFT->getResult(), DC, GenericSig);
  } else {
    StringBuilder.addTypeAnnotation(AFT->getResult(), DC, GenericSig);
  }

  return StringBuilder.createCompletionString();
}

FormattedSignatureHelp::Signature
SignatureHelpFormatter::formatSignature(const DeclContext *DC,
                                        const ide::Signature &Signature) {
  auto *FD = Signature.FuncD;
  auto *AFT = Signature.FuncTy;

  bool IsConstructor = isa_and_nonnull<ConstructorDecl>(FD);

  auto *SignatureString = createSignatureString(Signature, DC);

  llvm::SmallString<512> SS;
  llvm::raw_svector_ostream OS(SS);

  bool SkipResult = AFT->getResult()->isVoid() || IsConstructor;

  SmallVector<FormattedSignatureHelp::Parameter, 8> FormattedParams;

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

      auto &P = FormattedParams.emplace_back();
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

  StringRef SignatureText = copyAndClearString(Allocator, SS);

  // Parameter names.
  const ParamDecl *ParamScratch;
  auto ParamDecls =
      getParameterArray(FD, Signature.IsImplicitlyCurried, ParamScratch);

  if (!ParamDecls.empty()) {
    for (unsigned i = 0; i < FormattedParams.size(); ++i) {
      FormattedParams[i].Name = ParamDecls[i]->getParameterName().str();
    }
  }

  // Documentation.
  StringRef DocComment;
  if (FD) {
    ide::getRawDocumentationComment(FD, OS);
    DocComment = copyAndClearString(Allocator, SS);
  }

  return FormattedSignatureHelp::Signature(
      SignatureText, DocComment, Signature.ParamIdx,
      ArrayRef(FormattedParams).copy(Allocator));
}

FormattedSignatureHelp
SignatureHelpFormatter::format(SignatureHelpResult Result) {
  SmallVector<FormattedSignatureHelp::Signature, 8> FormattedSignatures;
  FormattedSignatures.reserve(Result.Signatures.size());

  for (auto &Signature : Result.Signatures) {
    FormattedSignatures.push_back(formatSignature(Result.DC, Signature));
  }

  // FIXME: Ideally we should select an active signature based on the context.
  unsigned ActiveSignature = 0;

  return FormattedSignatureHelp(ArrayRef(FormattedSignatures).copy(Allocator),
                                ActiveSignature);
}
