//===--- SwiftConformingMethodList.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
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
#include "swift/IDE/ConformingMethodList.h"
#include "swift/IDE/CompletionInstance.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Comment.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Type.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

static void
translateConformingMethodListOptions(OptionsDictionary &from,
                                     ConformingMethodList::Options &to) {
  // ConformingMethodList doesn't receive any options at this point.
}

/// The results returned from \c swiftConformingMethodListImpl through the
/// callback.
struct ConformingMethodListImplResult {
  /// The actual results. If \c nullptr, no results were found.
  const ide::ConformingMethodListResult *Result;
  /// Whether an AST was reused for the completion.
  bool DidReuseAST;
};

static void swiftConformingMethodListImpl(
    SwiftLangSupport &Lang, llvm::MemoryBuffer *UnresolvedInputFile,
    unsigned Offset, ArrayRef<const char *> Args,
    ArrayRef<const char *> ExpectedTypeNames,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
    llvm::function_ref<void(CancellableResult<ConformingMethodListImplResult>)>
        Callback) {
  assert(Callback && "Must provide callback to receive results");

  using ResultType = CancellableResult<ConformingMethodListImplResult>;

  struct ConsumerToCallbackAdapter
      : public swift::ide::ConformingMethodListConsumer {
    bool ReusingASTContext;
    llvm::function_ref<void(ResultType)> Callback;
    bool HandleResultWasCalled = false;

    ConsumerToCallbackAdapter(bool ReusingASTContext,
                              llvm::function_ref<void(ResultType)> Callback)
        : ReusingASTContext(ReusingASTContext), Callback(Callback) {}

    void handleResult(const ide::ConformingMethodListResult &result) override {
      HandleResultWasCalled = true;
      Callback(ResultType::success({&result, ReusingASTContext}));
    }
  };

  Lang.performCompletionLikeOperation(
      UnresolvedInputFile, Offset, Args, FileSystem,
      [&](CancellableResult<CompletionInstanceResult> Result) {
        switch (Result.getKind()) {
        case CancellableResultKind::Success: {
          ConsumerToCallbackAdapter Consumer(Result->DidReuseAST, Callback);

          // Create a factory for code completion callbacks that will feed the
          // Consumer.
          std::unique_ptr<CodeCompletionCallbacksFactory> callbacksFactory(
              ide::makeConformingMethodListCallbacksFactory(ExpectedTypeNames,
                                                            Consumer));

          if (!Result->DidFindCodeCompletionToken) {
            Callback(ResultType::success(
                {/*Results=*/nullptr, Result->DidReuseAST}));
            return;
          }

          performCodeCompletionSecondPass(*Result->CI.getCodeCompletionFile(),
                                          *callbacksFactory);
          if (!Consumer.HandleResultWasCalled) {
            // If we didn't receive a handleResult call from the second pass,
            // we didn't receive any results. To make sure Callback gets called
            // exactly once, call it manually with no results here.
            Callback(ResultType::success(
                {/*Results=*/nullptr, Result->DidReuseAST}));
          }
          break;
        }
        case CancellableResultKind::Failure:
          Callback(ResultType::failure(Result.getError()));
          break;
        case CancellableResultKind::Cancelled:
          Callback(ResultType::cancelled());
          break;
        }
      });
}

void SwiftLangSupport::getConformingMethodList(
    llvm::MemoryBuffer *UnresolvedInputFile, unsigned Offset,
    OptionsDictionary *optionsDict, ArrayRef<const char *> Args,
    ArrayRef<const char *> ExpectedTypeNames,
    SourceKit::ConformingMethodListConsumer &SKConsumer,
    Optional<VFSOptions> vfsOptions) {
  std::string error;

  // FIXME: the use of None as primary file is to match the fact we do not read
  // the document contents using the editor documents infrastructure.
  auto fileSystem = getFileSystem(vfsOptions, /*primaryFile=*/None, error);
  if (!fileSystem)
    return SKConsumer.failed(error);

  ConformingMethodList::Options options;
  if (optionsDict) {
    translateConformingMethodListOptions(*optionsDict, options);
  }

  class Consumer : public ide::ConformingMethodListConsumer {
    SourceKit::ConformingMethodListConsumer &SKConsumer;

  public:
    Consumer(SourceKit::ConformingMethodListConsumer &SKConsumer)
        : SKConsumer(SKConsumer) {}

    /// Convert an IDE result to a SK result and send it to \c SKConsumer .
    void handleResult(const ide::ConformingMethodListResult &Result) override {
      SmallString<512> SS;
      llvm::raw_svector_ostream OS(SS);

      unsigned TypeNameBegin = SS.size();
      Result.ExprType.print(OS);
      unsigned TypeNameLength = SS.size() - TypeNameBegin;

      unsigned TypeUSRBegin = SS.size();
      SwiftLangSupport::printTypeUSR(Result.ExprType, OS);
      unsigned TypeUSRLength = SS.size() - TypeUSRBegin;

      struct MemberInfo {
        size_t DeclNameBegin = 0;
        size_t DeclNameLength = 0;
        size_t TypeNameBegin = 0;
        size_t TypeNameLength = 0;
        size_t TypeUSRBegin = 0;
        size_t TypeUSRLength = 0;
        size_t DescriptionBegin = 0;
        size_t DescriptionLength = 0;
        size_t SourceTextBegin = 0;
        size_t SourceTextLength = 0;
        StringRef BriefComment;

        MemberInfo() {}
      };
      SmallVector<MemberInfo, 8> Members;

      for (auto member : Result.Members) {
        Members.emplace_back();
        auto &memberElem = Members.back();

        auto funcTy = cast<FuncDecl>(member)->getMethodInterfaceType();
        funcTy = Result.ExprType->getTypeOfMember(Result.DC->getParentModule(),
                                                  member, funcTy);
        auto resultTy = funcTy->castTo<FunctionType>()->getResult();

        // Name.
        memberElem.DeclNameBegin = SS.size();
        member->getName().print(OS);
        memberElem.DeclNameLength = SS.size() - memberElem.DeclNameBegin;

        // Type name.
        memberElem.TypeNameBegin = SS.size();
        resultTy.print(OS);
        memberElem.TypeNameLength = SS.size() - memberElem.TypeNameBegin;

        // Type USR.
        memberElem.TypeUSRBegin = SS.size();
        SwiftLangSupport::printTypeUSR(resultTy, OS);
        memberElem.TypeUSRLength = SS.size() - memberElem.TypeUSRBegin;

        // Description.
        memberElem.DescriptionBegin = SS.size();
        SwiftLangSupport::printMemberDeclDescription(
            member, Result.ExprType, /*usePlaceholder=*/false, OS);
        memberElem.DescriptionLength =
            SS.size() - memberElem.DescriptionBegin;

        // Sourcetext.
        memberElem.SourceTextBegin = SS.size();
        SwiftLangSupport::printMemberDeclDescription(
            member, Result.ExprType, /*usePlaceholder=*/true, OS);
        memberElem.SourceTextLength =
            SS.size() - memberElem.SourceTextBegin;

        // DocBrief.
        auto MaybeClangNode = member->getClangNode();
        if (MaybeClangNode) {
          if (auto *D = MaybeClangNode.getAsDecl()) {
            const auto &ClangContext = D->getASTContext();
            if (const clang::RawComment *RC =
                    ClangContext.getRawCommentForAnyRedecl(D))
              memberElem.BriefComment = RC->getBriefText(ClangContext);
          }
        } else {
          memberElem.BriefComment = member->getBriefComment();
        }
      }

      SourceKit::ConformingMethodListResult SKResult;
      SmallVector<SourceKit::ConformingMethodListResult::Member, 8>
          SKMembers;

      for (auto info : Members) {
        StringRef Name(SS.begin() + info.DeclNameBegin, info.DeclNameLength);
        StringRef TypeName(SS.begin() + info.TypeNameBegin,
                           info.TypeNameLength);
        StringRef TypeUSR(SS.begin() + info.TypeUSRBegin, info.TypeUSRLength);
        StringRef Description(SS.begin() + info.DescriptionBegin,
                              info.DescriptionLength);
        StringRef SourceText(SS.begin() + info.SourceTextBegin,
                             info.SourceTextLength);
        SKMembers.push_back({Name, TypeName, TypeUSR, Description, SourceText,
                               info.BriefComment});
      }

      SKResult.TypeName = StringRef(SS.begin() + TypeNameBegin, TypeNameLength);
      SKResult.TypeUSR = StringRef(SS.begin() + TypeUSRBegin, TypeUSRLength);
      SKResult.Members = SKMembers;

      SKConsumer.handleResult(SKResult);
    }

    void setReusingASTContext(bool flag) {
      SKConsumer.setReusingASTContext(flag);
    }
  } Consumer(SKConsumer);

  swiftConformingMethodListImpl(
      *this, UnresolvedInputFile, Offset, Args, ExpectedTypeNames, fileSystem,
      [&](CancellableResult<ConformingMethodListImplResult> Result) {
        switch (Result.getKind()) {
        case CancellableResultKind::Success: {
          if (Result->Result) {
            Consumer.handleResult(*Result->Result);
          }
          // If we didn't receive any result, don't call handleResult on
          // Consumer to deliver empty results.
          Consumer.setReusingASTContext(Result->DidReuseAST);
          break;
        }
        case CancellableResultKind::Failure:
          SKConsumer.failed(Result.getError());
          break;
        case CancellableResultKind::Cancelled:
          SKConsumer.cancelled();
          break;
        }
      });
}
