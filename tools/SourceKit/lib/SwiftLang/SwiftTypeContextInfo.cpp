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
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/TypeContextInfo.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Comment.h"
#include "clang/AST/Decl.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

/// Copy a memory buffer inserting '0' at the position of \c origBuf.
// TODO: Share with code completion.
static std::unique_ptr<llvm::MemoryBuffer>
makeCodeCompletionMemoryBuffer(const llvm::MemoryBuffer *origBuf,
                               unsigned &Offset,
                               const std::string bufferIdentifier) {

  auto origBuffSize = origBuf->getBufferSize();
  if (Offset > origBuffSize)
    Offset = origBuffSize;

  auto newBuffer = llvm::WritableMemoryBuffer::getNewUninitMemBuffer(
      origBuffSize + 1, bufferIdentifier);
  auto *pos = origBuf->getBufferStart() + Offset;
  auto *newPos =
      std::copy(origBuf->getBufferStart(), pos, newBuffer->getBufferStart());
  *newPos = '\0';
  std::copy(pos, origBuf->getBufferEnd(), newPos + 1);

  return std::unique_ptr<llvm::MemoryBuffer>(newBuffer.release());
}

static bool swiftTypeContextInfoImpl(SwiftLangSupport &Lang,
                                     llvm::MemoryBuffer *UnresolvedInputFile,
                                     unsigned Offset,
                                     ArrayRef<const char *> Args,
                                     ide::TypeContextInfoConsumer &Consumer,
                                     std::string &Error) {
  auto bufferIdentifier =
      Lang.resolvePathSymlinks(UnresolvedInputFile->getBufferIdentifier());

  auto newBuffer = makeCodeCompletionMemoryBuffer(UnresolvedInputFile, Offset,
                                                  bufferIdentifier);

  CompilerInstance CI;
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  CompilerInvocation Invocation;
  bool Failed = Lang.getASTManager()->initCompilerInvocation(
      Invocation, Args, CI.getDiags(), bufferIdentifier, Error);
  if (Failed)
    return false;
  if (!Invocation.getFrontendOptions().InputsAndOutputs.hasInputs()) {
    Error = "no input filenames specified";
    return false;
  }

  Invocation.setCodeCompletionPoint(newBuffer.get(), Offset);

  // Create a factory for code completion callbacks that will feed the
  // Consumer.
  std::unique_ptr<CodeCompletionCallbacksFactory> callbacksFactory(
      ide::makeTypeContextInfoCallbacksFactory(Consumer));

  Invocation.setCodeCompletionFactory(callbacksFactory.get());

  if (CI.setup(Invocation)) {
    // FIXME: error?
    return true;
  }
  CI.performSema();

  return true;
}

/// Print 'description' or 'sourcetext' the given \c VD to \c OS. If
/// \c usePlaceholder is true, call argument positions are substituted with
/// type editor placeholders which is suitable for 'sourcetext'.
static void printDeclDescription(llvm::raw_ostream &OS, Type baseTy,
                                 ValueDecl *VD, bool usePlaceholder);

void SwiftLangSupport::getExpressionContextInfo(
    llvm::MemoryBuffer *UnresolvedInputFile, unsigned Offset,
    ArrayRef<const char *> Args,
    SourceKit::TypeContextInfoConsumer &SKConsumer) {
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

      SmallVector<SourceKit::TypeContextInfoItem::Member, 8> ImplicitMembers;
      for (auto member : Item.ImplicitMembers) {

        // Name.
        unsigned DeclNameBegin = SS.size();
        member->getFullName().print(OS);
        unsigned DeclNameLength = SS.size() - DeclNameBegin;
        StringRef DeclNameStr(SS.begin() + DeclNameBegin, DeclNameLength);

        // Description.
        unsigned DescriptionBegin = SS.size();
        printDeclDescription(OS, Item.ExpectedTy, member,
                             /*usePlaceholder=*/false);
        unsigned DescriptionLength = SS.size() - DescriptionBegin;
        StringRef Description(SS.begin() + DescriptionBegin, DescriptionLength);

        // Sourcetext.
        unsigned SourceTextBegin = SS.size();
        printDeclDescription(OS, Item.ExpectedTy, member,
                             /*usePlaceholder=*/true);
        unsigned SourceTextLength = SS.size() - SourceTextBegin;
        StringRef SourceText(SS.begin() + SourceTextBegin, SourceTextLength);

        // DocBrief.
        StringRef BriefComment;
        auto MaybeClangNode = member->getClangNode();
        if (MaybeClangNode) {
          if (auto *D = MaybeClangNode.getAsDecl()) {
            const auto &ClangContext = D->getASTContext();
            if (const clang::RawComment *RC =
                    ClangContext.getRawCommentForAnyRedecl(D))
              BriefComment = RC->getBriefText(ClangContext);
          }
        } else {
          BriefComment = member->getBriefComment();
        }

        ImplicitMembers.push_back(
            {DeclNameStr, Description, SourceText, BriefComment});
      }

      SourceKit::TypeContextInfoItem Info;
      Info.TypeName = StringRef(SS.begin() + TypeNameBegin, TypeNameLength);
      Info.TypeUSR = StringRef(SS.begin() + TypeUSRBegin, TypeUSRLength);
      Info.ImplicitMembers = ImplicitMembers;

      SKConsumer.handleResult(Info);
    }

  public:
    Consumer(SourceKit::TypeContextInfoConsumer &SKConsumer)
        : SKConsumer(SKConsumer){};

    void handleResults(ArrayRef<ide::TypeContextInfoItem> Results) {
      for (auto &Item : Results)
        handleSingleResult(Item);
    }
  } Consumer(SKConsumer);

  std::string Error;
  if (!swiftTypeContextInfoImpl(*this, UnresolvedInputFile, Offset, Args,
                                Consumer, Error)) {
    SKConsumer.failed(Error);
  }
}

static void printDeclDescription(llvm::raw_ostream &OS, Type baseTy,
                                 ValueDecl *VD, bool usePlaceholder) {

  // Base name.
  OS << VD->getBaseName().userFacingName();

  // Parameters.
  auto *M = VD->getModuleContext();
  auto substMap = baseTy->getMemberSubstitutionMap(M, VD);
  auto printSingleParam = [&](ParamDecl *param) {
    auto paramTy = param->getInterfaceType();

    // Label.
    if (!param->getArgumentName().empty())
      OS << param->getArgumentName() << ": ";

    // InOut.
    if (param->isInOut()) {
      OS << "&";
      paramTy = paramTy->getInOutObjectType();
    }

    // Type.
    if (usePlaceholder)
      OS << "<#T##";

    if (auto substitutedTy = paramTy.subst(substMap))
      paramTy = substitutedTy;

    if (paramTy->hasError() && param->getTypeLoc().hasLocation()) {
      // Fallback to 'TypeRepr' printing.
      param->getTypeLoc().getTypeRepr()->print(OS);
    } else {
      paramTy.print(OS);
    }

    if (usePlaceholder)
      OS << "#>";
  };
  auto printParams = [&](ParameterList *params) {
    OS << '(';
    bool isFirst = true;
    for (auto param : params->getArray()) {
      if (isFirst)
        isFirst = false;
      else
        OS << ", ";
      printSingleParam(param);
    }
    OS << ')';
  };
  if (auto EED = dyn_cast<EnumElementDecl>(VD)) {
    if (auto params = EED->getParameterList())
      printParams(params);
  } else if (auto *FD = dyn_cast<FuncDecl>(VD)) {
    if (auto params = FD->getParameters())
      printParams(params);
  } else if (isa<VarDecl>(VD)) {
    // Var decl doesn't have parameters.
  } else {
    llvm_unreachable("Invalid Decl type for context info implicit member");
  }
}
