//===--- SwiftTypeContextInfo.cpp -----------------------------------------===//
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
#include "SwiftLangSupport.h"
#include "SwiftEditorDiagConsumer.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CompletionInstance.h"
#include "swift/IDE/TypeContextInfo.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Comment.h"
#include "clang/AST/Decl.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

static void translateTypeContextInfoOptions(OptionsDictionary &from,
                                            TypeContextInfo::Options &to) {
  // TypeContextInfo doesn't receive any options at this point.
}

/// The results returned from \c swiftTypeContextInfoImpl via \c Callback.
struct SwiftTypeContextInfoImplResult {
  /// The actual results. If empty, no results were found.
  ArrayRef<ide::TypeContextInfoItem> Results;
  /// Whether an AST was reused to produce the results.
  bool DidReuseAST;
};

static void swiftTypeContextInfoImpl(
    SwiftLangSupport &Lang, llvm::MemoryBuffer *UnresolvedInputFile,
    unsigned Offset, ArrayRef<const char *> Args,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
    llvm::function_ref<void(CancellableResult<SwiftTypeContextInfoImplResult>)>
        Callback) {
  assert(Callback && "Must provide callback to receive results");

  using ResultType = CancellableResult<SwiftTypeContextInfoImplResult>;

  struct ConsumerToCallbackAdapter : public ide::TypeContextInfoConsumer {
    bool ReusingASTContext;
    llvm::function_ref<void(ResultType)> Callback;
    bool HandleResultsCalled = false;

    ConsumerToCallbackAdapter(bool ReusingASTContext,
                              llvm::function_ref<void(ResultType)> Callback)
        : ReusingASTContext(ReusingASTContext), Callback(Callback) {}

    void handleResults(ArrayRef<ide::TypeContextInfoItem> Results) override {
      HandleResultsCalled = true;
      Callback(ResultType::success({Results, ReusingASTContext}));
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
              ide::makeTypeContextInfoCallbacksFactory(Consumer));

          if (!Result->DidFindCodeCompletionToken) {
            Callback(
                ResultType::success({/*Results=*/{}, Result->DidReuseAST}));
          }

          performCodeCompletionSecondPass(*Result->CI.getCodeCompletionFile(),
                                          *callbacksFactory);
          if (!Consumer.HandleResultsCalled) {
            // If we didn't receive a handleResult call from the second pass,
            // we didn't receive any results. To make sure Callback gets called
            // exactly once, call it manually with no results here.
            Callback(
                ResultType::success({/*Results=*/{}, Result->DidReuseAST}));
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

void SwiftLangSupport::getExpressionContextInfo(
    llvm::MemoryBuffer *UnresolvedInputFile, unsigned Offset,
    OptionsDictionary *optionsDict, ArrayRef<const char *> Args,
    SourceKit::TypeContextInfoConsumer &SKConsumer,
    Optional<VFSOptions> vfsOptions) {
  std::string error;

  TypeContextInfo::Options options;
  if (optionsDict) {
    translateTypeContextInfoOptions(*optionsDict, options);
  }

  // FIXME: the use of None as primary file is to match the fact we do not read
  // the document contents using the editor documents infrastructure.
  auto fileSystem = getFileSystem(vfsOptions, /*primaryFile=*/None, error);
  if (!fileSystem)
    return SKConsumer.failed(error);

  class Consumer : public ide::TypeContextInfoConsumer {
    SourceKit::TypeContextInfoConsumer &SKConsumer;

    /// Convert an IDE result to a SK result and send it to \c SKConsumer.
    void handleSingleResult(const ide::TypeContextInfoItem &Item) {
      SmallString<512> SS;
      llvm::raw_svector_ostream OS(SS);

      unsigned TypeNameBegin = SS.size();
      Item.ExpectedTy.print(OS);
      unsigned TypeNameLength = SS.size() - TypeNameBegin;

      unsigned TypeUSRBegin = SS.size();
      SwiftLangSupport::printTypeUSR(Item.ExpectedTy, OS);
      unsigned TypeUSRLength = SS.size() - TypeUSRBegin;

      struct MemberInfo {
        size_t DeclNameBegin = 0;
        size_t DeclNameLength = 0;
        size_t DescriptionBegin = 0;
        size_t DescriptionLength = 0;
        size_t SourceTextBegin = 0;
        size_t SourceTextLength = 0;
        StringRef BriefComment;

        MemberInfo() {}
      };
      SmallVector<MemberInfo, 8> ImplicitMembers;
      for (auto member : Item.ImplicitMembers) {
        ImplicitMembers.emplace_back();
        auto &memberElem = ImplicitMembers.back();

        // Name.
        memberElem.DeclNameBegin = SS.size();
        member->getName().print(OS);
        memberElem.DeclNameLength = SS.size() - memberElem.DeclNameBegin;

        // Description.
        memberElem.DescriptionBegin = SS.size();
        SwiftLangSupport::printMemberDeclDescription(
            member, Item.ExpectedTy, /*usePlaceholder=*/false, OS);
        memberElem.DescriptionLength = SS.size() - memberElem.DescriptionBegin;

        // Sourcetext.
        memberElem.SourceTextBegin = SS.size();
        SwiftLangSupport::printMemberDeclDescription(
            member, Item.ExpectedTy, /*usePlaceholder=*/true, OS);
        memberElem.SourceTextLength = SS.size() - memberElem.SourceTextBegin;

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

      SourceKit::TypeContextInfoItem Info;
      SmallVector<SourceKit::TypeContextInfoItem::Member, 8> SKImplicitMembers;

      for (auto &info : ImplicitMembers) {
        StringRef Name(SS.begin() + info.DeclNameBegin, info.DeclNameLength);
        StringRef Description(SS.begin() + info.DescriptionBegin,
                              info.DescriptionLength);
        StringRef SourceText(SS.begin() + info.SourceTextBegin,
                             info.SourceTextLength);
        SKImplicitMembers.push_back(
            {Name, Description, SourceText, info.BriefComment});
      }

      Info.TypeName = StringRef(SS.begin() + TypeNameBegin, TypeNameLength);
      Info.TypeUSR = StringRef(SS.begin() + TypeUSRBegin, TypeUSRLength);
      Info.ImplicitMembers = SKImplicitMembers;

      SKConsumer.handleResult(Info);
    }

  public:
    Consumer(SourceKit::TypeContextInfoConsumer &SKConsumer)
        : SKConsumer(SKConsumer){};

    void handleResults(ArrayRef<ide::TypeContextInfoItem> Results) override {
      for (auto &Item : Results)
        handleSingleResult(Item);
    }

    void setReusingASTContext(bool flag) {
      SKConsumer.setReusingASTContext(flag);
    }
  } Consumer(SKConsumer);

  swiftTypeContextInfoImpl(
      *this, UnresolvedInputFile, Offset, Args, fileSystem,
      [&](CancellableResult<SwiftTypeContextInfoImplResult> Result) {
        switch (Result.getKind()) {
        case CancellableResultKind::Success: {
          Consumer.handleResults(Result->Results);
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
