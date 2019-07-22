//===--- SwiftCompletion.cpp ----------------------------------------------===//
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

#include "CodeCompletionOrganizer.h"
#include "SwiftASTManager.h"
#include "SwiftLangSupport.h"
#include "SwiftEditorDiagConsumer.h"
#include "SourceKit/Support/FileSystemProvider.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/UIdent.h"

#include "swift/AST/ASTPrinter.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CodeCompletionCache.h"

#include "llvm/Support/Compiler.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;
using CodeCompletion::SwiftCompletionInfo;
using CodeCompletion::Completion;
using CodeCompletion::CodeCompletionView;
using CodeCompletion::CodeCompletionViewRef;
using CodeCompletion::NameToPopularityMap;

static_assert(swift::ide::CodeCompletionResult::MaxNumBytesToErase == 127,
              "custom array implementation for code completion results "
              "has this limit hardcoded");

namespace {
struct SwiftToSourceKitCompletionAdapter {
  static bool handleResult(SourceKit::CodeCompletionConsumer &consumer,
                           CodeCompletionResult *result) {
    llvm::SmallString<64> name;
    {
      llvm::raw_svector_ostream OSS(name);
      CodeCompletion::CompletionBuilder::getFilterName(
          result->getCompletionString(), OSS);
    }

    llvm::SmallString<64> description;
    {
      llvm::raw_svector_ostream OSS(description);
      // FIXME: The leading punctuation (e.g. "?." in an optional completion)
      // should really be part of a structured result description and clients
      // can
      // decide whether to display it or not. For now just include it in the
      // description only in the new code path.
      CodeCompletion::CompletionBuilder::getDescription(
          result, OSS, /*leadingPunctuation=*/false);
    }

    Completion extended(*result, name, description);
    return handleResult(consumer, &extended, /*leadingPunctuation=*/false,
                        /*legacyLiteralToKeyword=*/true);
  }

  static bool handleResult(SourceKit::CodeCompletionConsumer &consumer,
                           Completion *result, bool leadingPunctuation,
                           bool legacyLiteralToKeyword);

  static void getResultSourceText(const CodeCompletionString *CCStr,
                                  raw_ostream &OS);
  static void getResultTypeName(const CodeCompletionString *CCStr,
                                raw_ostream &OS);
  static void getResultAssociatedUSRs(ArrayRef<StringRef> AssocUSRs,
                                      raw_ostream &OS);
};

struct SwiftCodeCompletionConsumer
    : public ide::SimpleCachingCodeCompletionConsumer {
  using HandlerFunc = std::function<void(
      MutableArrayRef<CodeCompletionResult *>, SwiftCompletionInfo &)>;
  HandlerFunc handleResultsImpl;
  SwiftCompletionInfo swiftContext;

  SwiftCodeCompletionConsumer(HandlerFunc handleResultsImpl)
      : handleResultsImpl(handleResultsImpl) {}

  void setContext(swift::ASTContext *context,
                  swift::CompilerInvocation *invocation,
                  swift::ide::CodeCompletionContext *completionContext) {
    swiftContext.swiftASTContext = context;
    swiftContext.invocation = invocation;
    swiftContext.completionContext = completionContext;
  }
  void clearContext() { swiftContext = SwiftCompletionInfo(); }

  void handleResults(MutableArrayRef<CodeCompletionResult *> Results) override {
    assert(swiftContext.swiftASTContext);
    CodeCompletionContext::sortCompletionResults(Results);
    handleResultsImpl(Results, swiftContext);
  }
};
} // anonymous namespace

/// Returns completion context kind \c UIdent to report to the client.
/// For now, only returns "unresolved member" kind because others are unstable.
/// Returns invalid \c UIents other cases.
static UIdent getUIDForCodeCompletionKindToReport(CompletionKind kind) {
  switch (kind) {
  case CompletionKind::UnresolvedMember:
    return UIdent("source.lang.swift.completion.unresolvedmember");
  default:
    return UIdent();
  }
}

static bool swiftCodeCompleteImpl(
    SwiftLangSupport &Lang, llvm::MemoryBuffer *UnresolvedInputFile,
    unsigned Offset, SwiftCodeCompletionConsumer &SwiftConsumer,
    ArrayRef<const char *> Args,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
    std::string &Error) {
  assert(FileSystem);

  // Resolve symlinks for the input file; we resolve them for the input files
  // in the arguments as well.
  // FIXME: We need the Swift equivalent of Clang's FileEntry.
  auto InputFile = llvm::MemoryBuffer::getMemBuffer(
      UnresolvedInputFile->getBuffer(),
      Lang.resolvePathSymlinks(UnresolvedInputFile->getBufferIdentifier()));

  auto origBuffSize = InputFile->getBufferSize();
  unsigned CodeCompletionOffset = Offset;
  if (CodeCompletionOffset > origBuffSize) {
    CodeCompletionOffset = origBuffSize;
  }

  CompilerInstance CI;
  // Display diagnostics to stderr.
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  EditorDiagConsumer TraceDiags;
  trace::TracedOperation TracedOp(trace::OperationKind::CodeCompletion);
  if (TracedOp.enabled()) {
    CI.addDiagnosticConsumer(&TraceDiags);
    trace::SwiftInvocation SwiftArgs;
    trace::initTraceInfo(SwiftArgs, InputFile->getBufferIdentifier(), Args);
    TracedOp.setDiagnosticProvider(
        [&TraceDiags](SmallVectorImpl<DiagnosticEntryInfo> &diags) {
          TraceDiags.getAllDiagnostics(diags);
        });
    TracedOp.start(SwiftArgs,
                   {std::make_pair("OriginalOffset", std::to_string(Offset)),
                    std::make_pair("Offset",
                      std::to_string(CodeCompletionOffset))});
  }

  CompilerInvocation Invocation;
  bool Failed = Lang.getASTManager()->initCompilerInvocation(
      Invocation, Args, CI.getDiags(), InputFile->getBufferIdentifier(),
      FileSystem, Error);
  if (Failed) {
    return false;
  }
  if (!Invocation.getFrontendOptions().InputsAndOutputs.hasInputs()) {
    Error = "no input filenames specified";
    return false;
  }

  const char *Position = InputFile->getBufferStart() + CodeCompletionOffset;
  std::unique_ptr<llvm::WritableMemoryBuffer> NewBuffer =
      llvm::WritableMemoryBuffer::getNewUninitMemBuffer(
                                              InputFile->getBufferSize() + 1,
                                              InputFile->getBufferIdentifier());
  char *NewBuf = NewBuffer->getBufferStart();
  char *NewPos = std::copy(InputFile->getBufferStart(), Position, NewBuf);
  *NewPos = '\0';
  std::copy(Position, InputFile->getBufferEnd(), NewPos+1);

  Invocation.setCodeCompletionPoint(NewBuffer.get(), CodeCompletionOffset);

  auto swiftCache = Lang.getCodeCompletionCache(); // Pin the cache.
  ide::CodeCompletionContext CompletionContext(swiftCache->getCache());

  // Create a factory for code completion callbacks that will feed the
  // Consumer.
  std::unique_ptr<CodeCompletionCallbacksFactory> CompletionCallbacksFactory(
      ide::makeCodeCompletionCallbacksFactory(CompletionContext,
                                              SwiftConsumer));

  Invocation.setCodeCompletionFactory(CompletionCallbacksFactory.get());

  // FIXME: We need to be passing the buffers from the open documents.
  // It is not a huge problem in practice because Xcode auto-saves constantly.

  if (FileSystem != llvm::vfs::getRealFileSystem()) {
    CI.getSourceMgr().setFileSystem(FileSystem);
  }

  if (CI.setup(Invocation)) {
    // FIXME: error?
    return true;
  }

  CloseClangModuleFiles scopedCloseFiles(
      *CI.getASTContext().getClangModuleLoader());
  SwiftConsumer.setContext(&CI.getASTContext(), &Invocation,
                           &CompletionContext);
  registerIDETypeCheckRequestFunctions(CI.getASTContext().evaluator);
  CI.performSema();
  SwiftConsumer.clearContext();

  return true;
}

void SwiftLangSupport::codeComplete(
    llvm::MemoryBuffer *UnresolvedInputFile, unsigned Offset,
    SourceKit::CodeCompletionConsumer &SKConsumer, ArrayRef<const char *> Args,
    Optional<VFSOptions> vfsOptions) {

  std::string error;
  // FIXME: the use of None as primary file is to match the fact we do not read
  // the document contents using the editor documents infrastructure.
  auto fileSystem = getFileSystem(vfsOptions, /*primaryFile=*/None, error);
  if (!fileSystem)
    return SKConsumer.failed(error);

  SwiftCodeCompletionConsumer SwiftConsumer([&](
      MutableArrayRef<CodeCompletionResult *> Results,
      SwiftCompletionInfo &info) {

    auto kind = getUIDForCodeCompletionKindToReport(
        info.completionContext->CodeCompletionKind);
    if (kind.isValid())
      SKConsumer.setCompletionKind(kind);

    bool hasRequiredType = info.completionContext->typeContextKind == TypeContextKind::Required;
    CodeCompletionContext::sortCompletionResults(Results);
    // FIXME: this adhoc filtering should be configurable like it is in the
    // codeCompleteOpen path.
    for (auto *Result : Results) {
      if (Result->getKind() == CodeCompletionResult::Literal) {
        switch (Result->getLiteralKind()) {
        case CodeCompletionLiteralKind::NilLiteral:
        case CodeCompletionLiteralKind::BooleanLiteral:
          break;
        case CodeCompletionLiteralKind::ImageLiteral:
        case CodeCompletionLiteralKind::ColorLiteral:
          if (hasRequiredType &&
              Result->getExpectedTypeRelation() <
                  CodeCompletionResult::Convertible)
            continue;
          break;
        default:
          continue;
        }
      }
      if (!SwiftToSourceKitCompletionAdapter::handleResult(SKConsumer, Result))
        break;
    }
  });

  std::string Error;
  if (!swiftCodeCompleteImpl(*this, UnresolvedInputFile, Offset, SwiftConsumer,
                             Args, fileSystem, Error)) {
    SKConsumer.failed(Error);
  }
}

static void getResultStructure(
    CodeCompletion::SwiftResult *result, bool leadingPunctuation,
    CodeCompletionInfo::DescriptionStructure &structure,
    std::vector<CodeCompletionInfo::ParameterStructure> &parameters) {
  auto *CCStr = result->getCompletionString();
  auto FirstTextChunk = CCStr->getFirstTextChunkIndex(leadingPunctuation);

  if (!FirstTextChunk.hasValue())
    return;

  bool isOperator = result->isOperator();

  auto chunks = CCStr->getChunks();
  using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

  unsigned i = *FirstTextChunk;
  unsigned textSize = 0;

  // The result name.
  for (; i < chunks.size(); ++i) {
    auto C = chunks[i];
    if (C.is(ChunkKind::TypeAnnotation) ||
        C.is(ChunkKind::CallParameterClosureType) ||
        C.is(ChunkKind::Whitespace))
      continue;

    if (C.is(ChunkKind::LeftParen) || C.is(ChunkKind::LeftBracket) ||
        C.is(ChunkKind::BraceStmtWithCursor) ||
        C.is(ChunkKind::CallParameterBegin))
      break;

    if (C.is(ChunkKind::Equal))
      isOperator = true;

    if (C.hasText())
      textSize += C.getText().size();
  }

  structure.baseName.begin = 0;
  structure.baseName.end = textSize;

  // The parameters.
  for (; i < chunks.size(); ++i) {
    auto C = chunks[i];
    if (C.is(ChunkKind::TypeAnnotation) ||
        C.is(ChunkKind::CallParameterClosureType) ||
        C.is(ChunkKind::Whitespace))
      continue;

    if (C.is(ChunkKind::BraceStmtWithCursor))
      break;

    if (C.is(ChunkKind::ThrowsKeyword) ||
        C.is(ChunkKind::RethrowsKeyword)) {
      structure.throwsRange.begin = textSize;
      structure.throwsRange.end = textSize + C.getText().size();
    }

    if (C.is(ChunkKind::CallParameterBegin)) {
      CodeCompletionInfo::ParameterStructure param;

      ++i;
      bool inName = false;
      bool inAfterColon = false;
      for (; i < chunks.size(); ++i) {
        if (chunks[i].endsPreviousNestedGroup(C.getNestingLevel()))
          break;
        if (chunks[i].is(ChunkKind::CallParameterClosureType))
          continue;
        if (isOperator && chunks[i].is(ChunkKind::CallParameterType))
          continue;

        // Parameter name
        if (chunks[i].is(ChunkKind::CallParameterName) ||
            chunks[i].is(ChunkKind::CallParameterInternalName)) {
          param.name.begin = textSize;
          param.isLocalName =
              chunks[i].is(ChunkKind::CallParameterInternalName);
          inName = true;
        }

        // Parameter type
        if (chunks[i].is(ChunkKind::CallParameterType)) {
          unsigned start = textSize;
          unsigned prev = i - 1; // if i == 0, prev = ~0u.

          // Combine & for inout into the type name.
          if (prev != ~0u && chunks[prev].is(ChunkKind::Ampersand)) {
            start -= chunks[prev].getText().size();
            prev -= 1;
          }

          // Combine the whitespace after ':' into the type name.
          if (prev != ~0u && chunks[prev].is(ChunkKind::CallParameterColon))
            start -= 1;

          param.afterColon.begin = start;
          inAfterColon = true;
          if (inName) {
            param.name.end = start;
            inName = false;
          }
        }

        if (chunks[i].hasText())
          textSize += chunks[i].getText().size();
      }

      // If we had a name with no type following it, finish it now.
      if (inName)
        param.name.end = textSize;

      // Finish the type name.
      if (inAfterColon)
        param.afterColon.end = textSize;
      if (!param.range().empty())
        parameters.push_back(std::move(param));

      if (i < chunks.size() && chunks[i].hasText())
        textSize += chunks[i].getText().size();
    }

    if (C.hasText())
      textSize += C.getText().size();
  }

  if (!parameters.empty()) {
    structure.parameterRange.begin = parameters.front().range().begin;
    structure.parameterRange.end = parameters.back().range().end;
  }
}

static UIdent KindLiteralArray("source.lang.swift.literal.array");
static UIdent KindLiteralBoolean("source.lang.swift.literal.boolean");
static UIdent KindLiteralColor("source.lang.swift.literal.color");
static UIdent KindLiteralImage("source.lang.swift.literal.image");
static UIdent KindLiteralDictionary("source.lang.swift.literal.dictionary");
static UIdent KindLiteralInteger("source.lang.swift.literal.integer");
static UIdent KindLiteralNil("source.lang.swift.literal.nil");
static UIdent KindLiteralString("source.lang.swift.literal.string");
static UIdent KindLiteralTuple("source.lang.swift.literal.tuple");

static UIdent
getUIDForCodeCompletionLiteralKind(CodeCompletionLiteralKind kind) {
  switch (kind) {
  case CodeCompletionLiteralKind::ArrayLiteral:
    return KindLiteralArray;
  case CodeCompletionLiteralKind::BooleanLiteral:
    return KindLiteralBoolean;
  case CodeCompletionLiteralKind::ColorLiteral:
    return KindLiteralColor;
  case CodeCompletionLiteralKind::ImageLiteral:
    return KindLiteralImage;
  case CodeCompletionLiteralKind::DictionaryLiteral:
    return KindLiteralDictionary;
  case CodeCompletionLiteralKind::IntegerLiteral:
    return KindLiteralInteger;
  case CodeCompletionLiteralKind::NilLiteral:
    return KindLiteralNil;
  case CodeCompletionLiteralKind::StringLiteral:
    return KindLiteralString;
  case CodeCompletionLiteralKind::Tuple:
    return KindLiteralTuple;
  }
}

static UIdent KeywordUID("source.lang.swift.keyword");
static UIdent KeywordLetUID("source.lang.swift.keyword.let");
static UIdent KeywordVarUID("source.lang.swift.keyword.var");
static UIdent KeywordIfUID("source.lang.swift.keyword.if");
static UIdent KeywordForUID("source.lang.swift.keyword.for");
static UIdent KeywordWhileUID("source.lang.swift.keyword.while");
static UIdent KeywordFuncUID("source.lang.swift.keyword.func");

bool SwiftToSourceKitCompletionAdapter::handleResult(
    SourceKit::CodeCompletionConsumer &Consumer, Completion *Result,
    bool leadingPunctuation, bool legacyLiteralToKeyword) {

  static UIdent KeywordUID("source.lang.swift.keyword");
  static UIdent PatternUID("source.lang.swift.pattern");

  CodeCompletionInfo Info;
  if (Result->hasCustomKind()) {
    Info.CustomKind = Result->getCustomKind();
  } else if (Result->getKind() == CodeCompletionResult::Keyword) {
    Info.Kind = KeywordUID;
  } else if (Result->getKind() == CodeCompletionResult::Pattern) {
    Info.Kind = PatternUID;
  } else if (Result->getKind() == CodeCompletionResult::BuiltinOperator) {
    Info.Kind = PatternUID; // FIXME: add a UID for operators
  } else if (Result->getKind() == CodeCompletionResult::Declaration) {
    Info.Kind = SwiftLangSupport::getUIDForCodeCompletionDeclKind(
        Result->getAssociatedDeclKind());
  } else if (Result->getKind() == CodeCompletionResult::Literal) {
    auto literalKind = Result->getLiteralKind();
    if (legacyLiteralToKeyword &&
        (literalKind == CodeCompletionLiteralKind::BooleanLiteral ||
         literalKind == CodeCompletionLiteralKind::NilLiteral)) {
      // Fallback to keywords as appropriate.
      Info.Kind = KeywordUID;
    } else {
      Info.Kind = getUIDForCodeCompletionLiteralKind(literalKind);
    }
  }

  if (Info.Kind.isInvalid() && !Info.CustomKind)
    return true;

  llvm::SmallString<512> SS;

  unsigned DescBegin = SS.size();
  {
    llvm::raw_svector_ostream ccOS(SS);
    CodeCompletion::CompletionBuilder::getDescription(
        Result, ccOS, leadingPunctuation);
  }
  unsigned DescEnd = SS.size();

  if (DescBegin == DescEnd) {
    LOG_FUNC_SECTION_WARN {
      llvm::SmallString<64> LogMessage;
      llvm::raw_svector_ostream LogMessageOs(LogMessage);

      LogMessageOs << "Code completion result with empty description "
                      "was ignored: \n";
      Result->print(LogMessageOs);

      *Log << LogMessage;
    }
    return true;
  }

  unsigned TextBegin = SS.size();
  {
    llvm::raw_svector_ostream ccOS(SS);
    getResultSourceText(Result->getCompletionString(), ccOS);
  }
  unsigned TextEnd = SS.size();

  unsigned TypeBegin = SS.size();
  {
    llvm::raw_svector_ostream ccOS(SS);
    getResultTypeName(Result->getCompletionString(), ccOS);
  }
  unsigned TypeEnd = SS.size();

  unsigned USRsBegin = SS.size();
  {
    llvm::raw_svector_ostream ccOS(SS);
    getResultAssociatedUSRs(Result->getAssociatedUSRs(), ccOS);
  }
  unsigned USRsEnd = SS.size();

  Info.Name = Result->getName();
  Info.Description = StringRef(SS.begin() + DescBegin, DescEnd - DescBegin);
  Info.SourceText = StringRef(SS.begin() + TextBegin, TextEnd - TextBegin);
  Info.TypeName = StringRef(SS.begin() + TypeBegin, TypeEnd - TypeBegin);
  Info.AssocUSRs = StringRef(SS.begin() + USRsBegin, USRsEnd - USRsBegin);

  static UIdent CCCtxNone("source.codecompletion.context.none");
  static UIdent CCCtxExpressionSpecific(
      "source.codecompletion.context.exprspecific");
  static UIdent CCCtxLocal("source.codecompletion.context.local");
  static UIdent CCCtxCurrentNominal("source.codecompletion.context.thisclass");
  static UIdent CCCtxSuper("source.codecompletion.context.superclass");
  static UIdent CCCtxOutsideNominal("source.codecompletion.context.otherclass");
  static UIdent CCCtxCurrentModule("source.codecompletion.context.thismodule");
  static UIdent CCCtxOtherModule("source.codecompletion.context.othermodule");

  switch (Result->getSemanticContext()) {
  case SemanticContextKind::None:
    Info.SemanticContext = CCCtxNone; break;
  case SemanticContextKind::ExpressionSpecific:
    Info.SemanticContext = CCCtxExpressionSpecific; break;
  case SemanticContextKind::Local:
    Info.SemanticContext = CCCtxLocal; break;
  case SemanticContextKind::CurrentNominal:
    Info.SemanticContext = CCCtxCurrentNominal; break;
  case SemanticContextKind::Super:
    Info.SemanticContext = CCCtxSuper; break;
  case SemanticContextKind::OutsideNominal:
    Info.SemanticContext = CCCtxOutsideNominal; break;
  case SemanticContextKind::CurrentModule:
    Info.SemanticContext = CCCtxCurrentModule; break;
  case SemanticContextKind::OtherModule:
    Info.SemanticContext = CCCtxOtherModule; break;
  }

  Info.ModuleName = Result->getModuleName();
  Info.DocBrief = Result->getBriefDocComment();
  Info.NotRecommended = Result->isNotRecommended();

  Info.NumBytesToErase = Result->getNumBytesToErase();

  // Extended result values.
  Info.ModuleImportDepth = Result->getModuleImportDepth();

  // Description structure.
  std::vector<CodeCompletionInfo::ParameterStructure> parameters;
  CodeCompletionInfo::DescriptionStructure structure;
  getResultStructure(Result, leadingPunctuation, structure, parameters);
  Info.descriptionStructure = structure;
  if (!parameters.empty())
    Info.parametersStructure = parameters;

  return Consumer.handleResult(Info);
}

static CodeCompletionLiteralKind
getCodeCompletionLiteralKindForUID(UIdent uid) {
  if (uid == KindLiteralArray) {
    return CodeCompletionLiteralKind::ArrayLiteral;
  } else if (uid == KindLiteralBoolean) {
    return CodeCompletionLiteralKind::BooleanLiteral;
  } else if (uid == KindLiteralColor) {
    return CodeCompletionLiteralKind::ColorLiteral;
  } else if (uid == KindLiteralImage) {
    return CodeCompletionLiteralKind::ImageLiteral;
  } else if (uid == KindLiteralDictionary) {
    return CodeCompletionLiteralKind::DictionaryLiteral;
  } else if (uid == KindLiteralInteger) {
    return CodeCompletionLiteralKind::IntegerLiteral;
  } else if (uid == KindLiteralNil) {
    return CodeCompletionLiteralKind::NilLiteral;
  } else if (uid == KindLiteralString) {
    return CodeCompletionLiteralKind::StringLiteral;
  } else if (uid == KindLiteralTuple) {
    return CodeCompletionLiteralKind::Tuple;
  } else {
    llvm_unreachable("unexpected literal kind");
  }
}

static CodeCompletionKeywordKind
getCodeCompletionKeywordKindForUID(UIdent uid) {
#define SWIFT_KEYWORD(kw) \
  static UIdent Keyword##kw##UID("source.lang.swift.keyword." #kw); \
  if (uid == Keyword##kw##UID) { \
    return CodeCompletionKeywordKind::kw_##kw; \
  }
#include "swift/Syntax/TokenKinds.def"

  // FIXME: should warn about unexpected keyword kind.
  return CodeCompletionKeywordKind::None;
}

using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

/// Provide the text for the call parameter, including constructing a typed
/// editor placeholder for it.
static void constructTextForCallParam(
    ArrayRef<CodeCompletionString::Chunk> ParamGroup, raw_ostream &OS) {
  assert(ParamGroup.front().is(ChunkKind::CallParameterBegin));

  for (; !ParamGroup.empty(); ParamGroup = ParamGroup.slice(1)) {
    auto &C = ParamGroup.front();
    if (C.is(ChunkKind::CallParameterInternalName) ||
        C.is(ChunkKind::CallParameterType)) {
      break;
    }
    if (!C.isAnnotation() && C.hasText()) {
      OS << C.getText();
    }
  }

  SmallString<32> DisplayString;
  SmallString<32> TypeString;
  SmallString<32> ExpansionTypeString;

  for (auto &C : ParamGroup) {
    if (C.isAnnotation() || !C.hasText())
      continue;
    if (C.is(ChunkKind::CallParameterClosureType)) {
      assert(ExpansionTypeString.empty());
      ExpansionTypeString = C.getText();
      continue;
    }
    if (C.is(ChunkKind::CallParameterType)) {
      assert(TypeString.empty());
      TypeString = C.getText();
    }
    DisplayString += C.getText();
  }

  StringRef Display = DisplayString.str();
  StringRef Type = TypeString.str();
  StringRef ExpansionType = ExpansionTypeString.str();
  if (ExpansionType.empty())
    ExpansionType = Type;

  OS << "<#T##" << Display;
  if (Display == Type && Display == ExpansionType) {
    // Short version, display and type are the same.
  } else {
    OS << "##" << Type;
    if (ExpansionType != Type)
      OS << "##" << ExpansionType;
  }
  OS << "#>";
}

void SwiftToSourceKitCompletionAdapter::getResultSourceText(
    const CodeCompletionString *CCStr, raw_ostream &OS) {
  auto Chunks = CCStr->getChunks();
  for (size_t i = 0; i < Chunks.size(); ++i) {
    auto &C = Chunks[i];
    if (C.is(ChunkKind::BraceStmtWithCursor)) {
      OS << " {\n" << getCodePlaceholder() << "\n}";
      continue;
    }
    if (C.is(ChunkKind::CallParameterBegin)) {
      size_t Start = i++;
      for (; i < Chunks.size(); ++i) {
        if (Chunks[i].endsPreviousNestedGroup(C.getNestingLevel()))
          break;
      }
      constructTextForCallParam(Chunks.slice(Start, i-Start), OS);
      --i;
      continue;
    }
    if (!C.isAnnotation() && C.hasText()) {
      OS << C.getText();
    }
  }
}

void SwiftToSourceKitCompletionAdapter::getResultTypeName(
    const CodeCompletionString *CCStr, raw_ostream &OS) {
  for (auto C : CCStr->getChunks()) {
    if (C.getKind() == CodeCompletionString::Chunk::ChunkKind::TypeAnnotation) {
      OS << C.getText();
    }
  }
}

void SwiftToSourceKitCompletionAdapter::getResultAssociatedUSRs(
    ArrayRef<StringRef> AssocUSRs, raw_ostream &OS) {
  bool First = true;
  for (auto USR : AssocUSRs) {
    if (!First)
      OS << " ";
    First = false;
    OS << USR;
  }
}

//===----------------------------------------------------------------------===//
// CodeCompletion::SessionCache
//===----------------------------------------------------------------------===//
void CodeCompletion::SessionCache::setSortedCompletions(
    std::vector<Completion *> &&completions) {
  llvm::sys::ScopedLock L(mtx);
  sortedCompletions = std::move(completions);
}
ArrayRef<Completion *> CodeCompletion::SessionCache::getSortedCompletions() {
  llvm::sys::ScopedLock L(mtx);
  return sortedCompletions;
}
llvm::MemoryBuffer *CodeCompletion::SessionCache::getBuffer() {
  llvm::sys::ScopedLock L(mtx);
  return buffer.get();
}
ArrayRef<std::string> CodeCompletion::SessionCache::getCompilerArgs() {
  llvm::sys::ScopedLock L(mtx);
  return args;
}
llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> CodeCompletion::SessionCache::getFileSystem() {
  llvm::sys::ScopedLock L(mtx);
  return fileSystem;
}
CompletionKind CodeCompletion::SessionCache::getCompletionKind() {
  llvm::sys::ScopedLock L(mtx);
  return completionKind;
}
TypeContextKind CodeCompletion::SessionCache::getCompletionTypeContextKind() {
  llvm::sys::ScopedLock L(mtx);
  return typeContextKind;
}
bool CodeCompletion::SessionCache::getCompletionMayUseImplicitMemberExpr() {
  llvm::sys::ScopedLock L(mtx);
  return completionMayUseImplicitMemberExpr;
}
const CodeCompletion::FilterRules &
CodeCompletion::SessionCache::getFilterRules() {
  llvm::sys::ScopedLock L(mtx);
  return filterRules;
}

//===----------------------------------------------------------------------===//
// CodeCompletion::SessionCacheMap
//===----------------------------------------------------------------------===//

unsigned CodeCompletion::SessionCacheMap::getBufferID(StringRef name) const {
  auto pair = nameToBufferMap.insert(std::make_pair(name, nextBufferID));
  if (pair.second)
    ++nextBufferID;
  return pair.first->getValue();
}

CodeCompletion::SessionCacheRef
CodeCompletion::SessionCacheMap::get(StringRef name, unsigned offset) const {
  llvm::sys::ScopedLock L(mtx);
  auto key = std::make_pair(getBufferID(name), offset);
  auto I = sessions.find(key);
  if (I == sessions.end())
    return nullptr;
  return I->second;
}
bool CodeCompletion::SessionCacheMap::set(StringRef name, unsigned offset,
                                          CodeCompletion::SessionCacheRef s) {
  llvm::sys::ScopedLock L(mtx);
  auto key = std::make_pair(getBufferID(name), offset);
  return sessions.insert(std::make_pair(key, s)).second;
}
bool CodeCompletion::SessionCacheMap::remove(StringRef name, unsigned offset) {
  llvm::sys::ScopedLock L(mtx);
  auto key = std::make_pair(getBufferID(name), offset);
  return sessions.erase(key);
}

//===----------------------------------------------------------------------===//
// (New) Code completion interface
//===----------------------------------------------------------------------===//

namespace {
class SwiftGroupedCodeCompletionConsumer : public CodeCompletionView::Walker {
  GroupedCodeCompletionConsumer &consumer;

public:
  SwiftGroupedCodeCompletionConsumer(GroupedCodeCompletionConsumer &consumer)
      : consumer(consumer) {}
  bool handleResult(Completion *result) override {
    return SwiftToSourceKitCompletionAdapter::handleResult(
        consumer, result, /*leadingPunctuation=*/true,
        /*legacyLiteralToKeyword=*/false);
  }
  void startGroup(StringRef name) override {
    static UIdent GroupUID("source.lang.swift.codecomplete.group");
    consumer.startGroup(GroupUID, name);
  }
  void endGroup() override { consumer.endGroup(); }
};

} // end anonymous namespace

static void translateCodeCompletionOptions(OptionsDictionary &from,
                                           CodeCompletion::Options &to,
                                           StringRef &filterText,
                                           unsigned &resultOffset,
                                           unsigned &maxResults) {
  static UIdent KeySortByName("key.codecomplete.sort.byname");
  static UIdent KeyUseImportDepth("key.codecomplete.sort.useimportdepth");
  static UIdent KeyGroupOverloads("key.codecomplete.group.overloads");
  static UIdent KeyGroupStems("key.codecomplete.group.stems");
  static UIdent KeyFilterText("key.codecomplete.filtertext");
  static UIdent KeyRequestLimit("key.codecomplete.requestlimit");
  static UIdent KeyRequestStart("key.codecomplete.requeststart");
  static UIdent KeyHideUnderscores("key.codecomplete.hideunderscores");
  static UIdent KeyHideLowPriority("key.codecomplete.hidelowpriority");
  static UIdent KeyHideByName("key.codecomplete.hidebyname");
  static UIdent KeyIncludeExactMatch("key.codecomplete.includeexactmatch");
  static UIdent KeyAddInnerResults("key.codecomplete.addinnerresults");
  static UIdent KeyAddInnerOperators("key.codecomplete.addinneroperators");
  static UIdent KeyAddInitsToTopLevel("key.codecomplete.addinitstotoplevel");
  static UIdent KeyCallPatternHeuristics("key.codecomplete.callpatternheuristics");
  static UIdent KeyFuzzyMatching("key.codecomplete.fuzzymatching");
  static UIdent KeyTopNonLiteral("key.codecomplete.showtopnonliteralresults");
  static UIdent KeyContextWeight("key.codecomplete.sort.contextweight");
  static UIdent KeyFuzzyWeight("key.codecomplete.sort.fuzzyweight");
  static UIdent KeyPopularityBonus("key.codecomplete.sort.popularitybonus");
  from.valueForOption(KeySortByName, to.sortByName);
  from.valueForOption(KeyUseImportDepth, to.useImportDepth);
  from.valueForOption(KeyGroupOverloads, to.groupOverloads);
  from.valueForOption(KeyGroupStems, to.groupStems);
  from.valueForOption(KeyFilterText, filterText);
  from.valueForOption(KeyRequestLimit, maxResults);
  from.valueForOption(KeyRequestStart, resultOffset);
  unsigned howMuchHiding = 1;
  from.valueForOption(KeyHideUnderscores, howMuchHiding);
  to.hideUnderscores = howMuchHiding;
  to.reallyHideAllUnderscores = howMuchHiding > 1;
  from.valueForOption(KeyHideLowPriority, to.hideLowPriority);
  from.valueForOption(KeyIncludeExactMatch, to.includeExactMatch);
  from.valueForOption(KeyAddInnerResults, to.addInnerResults);
  from.valueForOption(KeyAddInnerOperators, to.addInnerOperators);
  from.valueForOption(KeyAddInitsToTopLevel, to.addInitsToTopLevel);
  from.valueForOption(KeyCallPatternHeuristics, to.callPatternHeuristics);
  from.valueForOption(KeyFuzzyMatching, to.fuzzyMatching);
  from.valueForOption(KeyContextWeight, to.semanticContextWeight);
  from.valueForOption(KeyFuzzyWeight, to.fuzzyMatchWeight);
  from.valueForOption(KeyPopularityBonus, to.popularityBonus);
  from.valueForOption(KeyHideByName, to.hideByNameStyle);
  from.valueForOption(KeyTopNonLiteral, to.showTopNonLiteralResults);
}

/// Canonicalize a name that is in the format of a reference to a function into
/// the name format used internally for filtering.
///
/// Returns true if the name is invalid.
static bool canonicalizeFilterName(const char *origName,
                                   SmallVectorImpl<char> &Result) {
  assert(origName);
  const char *p = origName;
  char curr = '\0';
  char prev;

  // FIXME: disallow unnamed parameters without underscores `foo(::)`.
  while (true) {
    prev = curr;
    curr = *p++;
    switch (curr) {
    case '\0':
      return false; // Done.
    case '_':
      // Remove the _ underscore for an unnamed parameter.
      if (prev == ':' || prev == '(') {
        char next = *p;
        if (next == ':' || next == ')')
          continue;
      }
      LLVM_FALLTHROUGH;
    default:
      Result.push_back(curr);
      continue;
    }
  }
}

static void translateFilterRules(ArrayRef<FilterRule> rawFilterRules,
                                 CodeCompletion::FilterRules &filterRules) {
  for (auto &rule : rawFilterRules) {
    switch (rule.kind) {
    case FilterRule::Everything:
      filterRules.hideAll = rule.hide;
      break;
    case FilterRule::Identifier:
      for (auto name : rule.names) {
        SmallString<128> canonName;
        // Note: name is null-terminated.
        if (canonicalizeFilterName(name.data(), canonName))
          continue;
        filterRules.hideByFilterName[canonName] = rule.hide;
      }
      break;
    case FilterRule::Description:
      for (auto name : rule.names) {
        filterRules.hideByDescription[name] = rule.hide;
      }
      break;
    case FilterRule::Module:
      for (auto name : rule.names) {
        filterRules.hideModule[name] = rule.hide;
      }
      break;
    case FilterRule::Keyword:
      if (rule.uids.empty())
        filterRules.hideAllKeywords = rule.hide;
      for (auto uid : rule.uids) {
        auto kind = getCodeCompletionKeywordKindForUID(uid);
        filterRules.hideKeyword[kind] = rule.hide;
      }
      break;
    case FilterRule::Literal:
      if (rule.uids.empty())
        filterRules.hideAllValueLiterals = rule.hide;
      for (auto uid : rule.uids) {
        auto kind = getCodeCompletionLiteralKindForUID(uid);
        filterRules.hideValueLiteral[kind] = rule.hide;
      }
      break;
    case FilterRule::CustomCompletion:
      if (rule.uids.empty())
        filterRules.hideCustomCompletions = rule.hide;
      // FIXME: hide individual custom completions
      break;
    }
  }
}

static bool checkInnerResult(CodeCompletionResult *result, bool &hasDot,
                             bool &hasQDot, bool &hasInit) {
  auto chunks = result->getCompletionString()->getChunks();
  if (!chunks.empty() &&
      chunks[0].is(CodeCompletionString::Chunk::ChunkKind::Dot)) {
    hasDot = true;
    return true;
  } else if (chunks.size() >= 2 &&
             chunks[0].is(
                 CodeCompletionString::Chunk::ChunkKind::QuestionMark) &&
             chunks[1].is(CodeCompletionString::Chunk::ChunkKind::Dot)) {
    hasQDot = true;
    return true;
  } else if (result->getKind() ==
                 CodeCompletion::SwiftResult::ResultKind::Declaration &&
             result->getAssociatedDeclKind() ==
                 CodeCompletionDeclKind::Constructor) {
    hasInit = true;
    return true;
  } else {
    return false;
  }
}

template <typename Result>
static std::vector<Result *>
filterInnerResults(ArrayRef<Result *> results, bool includeInner,
                   bool includeInnerOperators,
                   bool &hasDot, bool &hasQDot, bool &hasInit,
                   const CodeCompletion::FilterRules &rules) {
  std::vector<Result *> topResults;
  for (auto *result : results) {
    if (!includeInnerOperators && result->isOperator())
      continue;

    llvm::SmallString<64> filterName;
    {
      llvm::raw_svector_ostream OSS(filterName);
      CodeCompletion::CompletionBuilder::getFilterName(
          result->getCompletionString(), OSS);
    }
    llvm::SmallString<64> description;
    {
      llvm::raw_svector_ostream OSS(description);
      CodeCompletion::CompletionBuilder::getDescription(
          result, OSS, /*leadingPunctuation=*/false);
    }
    if (rules.hideCompletion(result, filterName, description))
      continue;

    bool inner = checkInnerResult(result, hasDot, hasQDot, hasInit);

    if (!inner ||
        (includeInner &&
         result->getSemanticContext() <= SemanticContextKind::CurrentNominal))
      topResults.push_back(result);
  }
  return topResults;
}

static void transformAndForwardResults(
    GroupedCodeCompletionConsumer &consumer, SwiftLangSupport &lang,
    CodeCompletion::SessionCacheRef session,
    const NameToPopularityMap *nameToPopularity,
    CodeCompletion::Options options, unsigned offset, StringRef filterText,
    unsigned resultOffset, unsigned maxResults) {

  CodeCompletion::CompletionSink innerSink;
  Completion *exactMatch = nullptr;
  auto buildInnerResult = [&](ArrayRef<CodeCompletionString::Chunk> chunks) {
    auto *completionString =
        CodeCompletionString::create(innerSink.allocator, chunks);
    CodeCompletion::SwiftResult paren(
        CodeCompletion::SwiftResult::ResultKind::BuiltinOperator,
        SemanticContextKind::ExpressionSpecific,
        exactMatch ? exactMatch->getNumBytesToErase() : 0, completionString);

    SwiftCompletionInfo info;
    std::vector<Completion *> extended =
        extendCompletions(&paren, innerSink, info, nameToPopularity, options,
                          exactMatch, SemanticContextKind::ExpressionSpecific);
    assert(extended.size() == 1);
    return extended.front();
  };
  auto buildParen = [&]() {
    return buildInnerResult(CodeCompletionString::Chunk::createWithText(
        CodeCompletionString::Chunk::ChunkKind::LeftParen, 0, "("));
  };
  auto buildDot = [&]() {
    return buildInnerResult(CodeCompletionString::Chunk::createWithText(
        CodeCompletionString::Chunk::ChunkKind::Dot, 0, "."));
  };
  auto buildQDot = [&]() {
    CodeCompletionString::Chunk chunks[2] = {
        CodeCompletionString::Chunk::createWithText(
            CodeCompletionString::Chunk::ChunkKind::QuestionMark, 0, "?"),
        CodeCompletionString::Chunk::createWithText(
            CodeCompletionString::Chunk::ChunkKind::Dot, 0, ".")};
    return buildInnerResult(chunks);
  };

  CodeCompletion::CodeCompletionOrganizer organizer(
      options, session->getCompletionKind(),
      session->getCompletionTypeContextKind());

  auto &rules = session->getFilterRules();

  bool hasEarlyInnerResults =
      session->getCompletionKind() == CompletionKind::PostfixExpr;

  if (!hasEarlyInnerResults) {
    organizer.addCompletionsWithFilter(session->getSortedCompletions(),
                                       filterText, rules, exactMatch);
    // Add leading dot?
    if (options.addInnerOperators && !rules.hideFilterName(".") &&
        session->getCompletionMayUseImplicitMemberExpr()) {
      organizer.addCompletionsWithFilter(
          buildDot(), filterText, CodeCompletion::FilterRules(), exactMatch);
    }
  }

  if (hasEarlyInnerResults &&
      (options.addInnerResults || options.addInnerOperators)) {
    bool hasInit = false;
    bool hasDot = false;
    bool hasQDot = false;
    auto completions = session->getSortedCompletions();
    auto innerResults =
        filterInnerResults(completions, options.addInnerResults,
                           options.addInnerOperators, hasDot, hasQDot, hasInit,
                           rules);
    if (options.addInnerOperators) {
      if (hasInit && !rules.hideFilterName("("))
        innerResults.insert(innerResults.begin(), buildParen());
      if (hasDot && !rules.hideFilterName("."))
        innerResults.insert(innerResults.begin(), buildDot());
      if (hasQDot && !rules.hideFilterName("?."))
        innerResults.insert(innerResults.begin(), buildQDot());
    }

    organizer.addCompletionsWithFilter(innerResults, filterText,
                                       CodeCompletion::FilterRules(), exactMatch);
  }

  organizer.groupAndSort(options);

  if ((options.addInnerResults || options.addInnerOperators) &&
      exactMatch && exactMatch->getKind() == Completion::Declaration) {
    std::vector<Completion *> innerResults;
    bool hasDot = false;
    bool hasQDot = false;
    bool hasInit = false;
    SwiftCodeCompletionConsumer swiftConsumer([&](
        MutableArrayRef<CodeCompletionResult *> results,
        SwiftCompletionInfo &info) {
      auto *context = info.completionContext;
      if (!context || context->CodeCompletionKind != CompletionKind::PostfixExpr)
        return;
      auto topResults = filterInnerResults(results, options.addInnerResults,
                                           options.addInnerOperators, hasDot,
                                           hasQDot, hasInit, rules);
      // FIXME: Overriding the default to context "None" is a hack so that they
      // won't overwhelm other results that also match the filter text.
      innerResults = extendCompletions(
          topResults, innerSink, info, nameToPopularity, options, exactMatch,
          SemanticContextKind::None, SemanticContextKind::None);
    });

    auto *inputBuf = session->getBuffer();
    std::string str = inputBuf->getBuffer().slice(0, offset);
    {
      llvm::raw_string_ostream OSS(str);
      SwiftToSourceKitCompletionAdapter::getResultSourceText(
          exactMatch->getCompletionString(), OSS);
    }

    auto buffer =
        llvm::MemoryBuffer::getMemBuffer(str, inputBuf->getBufferIdentifier());
    auto args = session->getCompilerArgs();
    std::vector<const char *> cargs;
    for (auto &arg : args)
      cargs.push_back(arg.c_str());
    std::string error;
    if (!swiftCodeCompleteImpl(lang, buffer.get(), str.size(), swiftConsumer,
                               cargs, session->getFileSystem(), error)) {
      consumer.failed(error);
      return;
    }

    if (options.addInnerOperators) {
      if (hasInit && !rules.hideFilterName("("))
        innerResults.insert(innerResults.begin(), buildParen());
      if (hasDot && !rules.hideFilterName("."))
        innerResults.insert(innerResults.begin(), buildDot());
      if (hasQDot && !rules.hideFilterName("?."))
        innerResults.insert(innerResults.begin(), buildQDot());
    }

    // Add the inner results (and don't filter them).
    exactMatch = nullptr; // No longer needed.
    organizer.addCompletionsWithFilter(innerResults, filterText,
                                       CodeCompletion::FilterRules(), exactMatch);

    CodeCompletion::Options noGroupOpts = options;
    noGroupOpts.groupStems = false;
    noGroupOpts.groupOverloads = false;
    organizer.groupAndSort(noGroupOpts);
  }

  // Build the final results view.
  auto view = organizer.takeResultsView();
  CodeCompletion::LimitedResultView limitedResults(*view, resultOffset,
                                                   maxResults);

  // Forward results to the SourceKit consumer.
  SwiftGroupedCodeCompletionConsumer groupedConsumer(consumer);
  limitedResults.walk(groupedConsumer);
  consumer.setNextRequestStart(limitedResults.getNextOffset());
}

void SwiftLangSupport::codeCompleteOpen(
    StringRef name, llvm::MemoryBuffer *inputBuf, unsigned offset,
    OptionsDictionary *options, ArrayRef<FilterRule> rawFilterRules,
    GroupedCodeCompletionConsumer &consumer, ArrayRef<const char *> args, Optional<VFSOptions> vfsOptions) {
  StringRef filterText;
  unsigned resultOffset = 0;
  unsigned maxResults = 0;
  CodeCompletion::Options CCOpts;
  if (options)
    translateCodeCompletionOptions(*options, CCOpts, filterText, resultOffset,
                                   maxResults);

  std::string error;
  // FIXME: the use of None as primary file is to match the fact we do not read
  // the document contents using the editor documents infrastructure.
  auto fileSystem = getFileSystem(vfsOptions, /*primaryFile=*/None, error);
  if (!fileSystem)
    return consumer.failed(error);

  CodeCompletion::FilterRules filterRules;
  translateFilterRules(rawFilterRules, filterRules);

  // Set up the code completion consumer to pass results to organizer.
  CodeCompletion::CompletionSink sink;
  std::vector<Completion *> completions;

  NameToPopularityMap *nameToPopularity = nullptr;
  // This reference must outlive the uses of nameToPopularity.
  auto popularAPIRef = PopularAPI;
  if (popularAPIRef) {
    nameToPopularity = &popularAPIRef->nameToFactor;
  }

  CompletionKind completionKind = CompletionKind::None;
  TypeContextKind typeContextKind = TypeContextKind::None;
  bool mayUseImplicitMemberExpr = false;

  SwiftCodeCompletionConsumer swiftConsumer(
      [&](MutableArrayRef<CodeCompletionResult *> results,
          SwiftCompletionInfo &info) {
        auto &completionCtx = *info.completionContext;
        completionKind = completionCtx.CodeCompletionKind;
        typeContextKind = completionCtx.typeContextKind;
        mayUseImplicitMemberExpr = completionCtx.MayUseImplicitMemberExpr;
        completions =
            extendCompletions(results, sink, info, nameToPopularity, CCOpts);
      });

  // Add any codecomplete.open specific flags.
  std::vector<const char *> extendedArgs(args.begin(), args.end());
  if (CCOpts.addInitsToTopLevel) {
    extendedArgs.push_back("-Xfrontend");
    extendedArgs.push_back("-code-complete-inits-in-postfix-expr");
  }
  if (CCOpts.callPatternHeuristics) {
    extendedArgs.push_back("-Xfrontend");
    extendedArgs.push_back("-code-complete-call-pattern-heuristics");
  }

  // Invoke completion.
  if (!swiftCodeCompleteImpl(*this, inputBuf, offset, swiftConsumer,
                             extendedArgs, fileSystem, error)) {
    consumer.failed(error);
    return;
  }

  // Add any relevant custom completions.
  if (auto custom = CustomCompletions)
    addCustomCompletions(sink, completions, custom->customCompletions,
                         completionKind);

  // Pre-sort the completions.
  CodeCompletion::CodeCompletionOrganizer::preSortCompletions(completions);

  // Store in the session.
  using CodeCompletion::SessionCache;
  using CodeCompletion::SessionCacheRef;
  auto bufferCopy = llvm::MemoryBuffer::getMemBufferCopy(
      inputBuf->getBuffer(), inputBuf->getBufferIdentifier());
  std::vector<std::string> argsCopy(extendedArgs.begin(), extendedArgs.end());
  SessionCacheRef session{new SessionCache(
      std::move(sink), std::move(bufferCopy), std::move(argsCopy), fileSystem,
      completionKind, typeContextKind, mayUseImplicitMemberExpr,
      std::move(filterRules))};
  session->setSortedCompletions(std::move(completions));

  if (!CCSessions.set(name, offset, session)) {
    std::string err;
    llvm::raw_string_ostream OS(err);
    OS << "codecomplete.open: code completion session for '" << name << "', "
       << offset << " already exists";
    consumer.failed(OS.str());
  }

  transformAndForwardResults(consumer, *this, session, nameToPopularity, CCOpts,
                             offset, filterText, resultOffset, maxResults);
}

void SwiftLangSupport::codeCompleteClose(
    StringRef name, unsigned offset, GroupedCodeCompletionConsumer &consumer) {
  if (!CCSessions.remove(name, offset)) {
    std::string err;
    llvm::raw_string_ostream OS(err);
    OS << "codecomplete.close: no code completion session for '" << name
       << "', " << offset;
    consumer.failed(OS.str());
  }
}

void SwiftLangSupport::codeCompleteUpdate(
    StringRef name, unsigned offset, OptionsDictionary *options,
    GroupedCodeCompletionConsumer &consumer) {
  auto session = CCSessions.get(name, offset);
  if (!session) {
    std::string err;
    llvm::raw_string_ostream OS(err);
    OS << "codecomplete.update: no code completion session for '" << name
       << "', " << offset;
    consumer.failed(OS.str());
    return;
  }

  StringRef filterText;
  unsigned resultOffset = 0;
  unsigned maxResults = 0;

  // FIXME: consider whether CCOpts has changed since the 'open'.
  CodeCompletion::Options CCOpts;
  if (options)
    translateCodeCompletionOptions(*options, CCOpts, filterText, resultOffset,
                                   maxResults);

  NameToPopularityMap *nameToPopularity = nullptr;
  // This reference must outlive the uses of nameToPopularity.
  auto popularAPIRef = PopularAPI;
  if (popularAPIRef) {
    nameToPopularity = &popularAPIRef->nameToFactor;
  }

  transformAndForwardResults(consumer, *this, session, nameToPopularity, CCOpts,
                             offset, filterText, resultOffset, maxResults);
}

swift::ide::CodeCompletionCache &SwiftCompletionCache::getCache() {
  return *inMemory;
}
SwiftCompletionCache::~SwiftCompletionCache() {}

void SwiftLangSupport::codeCompleteCacheOnDisk(StringRef path) {
  ThreadSafeRefCntPtr<SwiftCompletionCache> newCache(new SwiftCompletionCache);
  newCache->onDisk = llvm::make_unique<ide::OnDiskCodeCompletionCache>(path);
  newCache->inMemory =
      llvm::make_unique<ide::CodeCompletionCache>(newCache->onDisk.get());

  CCCache = newCache; // replace the old cache.
}

void SwiftLangSupport::codeCompleteSetPopularAPI(
    ArrayRef<const char *> popularAPI, ArrayRef<const char *> unpopularAPI) {
  using Factor = CodeCompletion::PopularityFactor;
  ThreadSafeRefCntPtr<SwiftPopularAPI> newPopularAPI(new SwiftPopularAPI);
  auto &nameToFactor = newPopularAPI->nameToFactor;
  for (unsigned i = 0, n = popularAPI.size(); i < n; ++i) {
    SmallString<64> name;
    if (canonicalizeFilterName(popularAPI[i], name))
      continue;
    nameToFactor[name] = Factor(double(n - i) / n);
  }
  for (unsigned i = 0, n = unpopularAPI.size(); i < n; ++i) {
    SmallString<64> name;
    if (canonicalizeFilterName(unpopularAPI[i], name))
      continue;
    nameToFactor[name] = Factor(-double(n - i) / n);
  }

  PopularAPI = newPopularAPI; // replace the old popular API.
}

void SwiftLangSupport::codeCompleteSetCustom(
    ArrayRef<CustomCompletionInfo> completions) {
  ThreadSafeRefCntPtr<SwiftCustomCompletions> newCustomCompletions(
      new SwiftCustomCompletions);
  newCustomCompletions->customCompletions.assign(completions.begin(),
                                                 completions.end());
  CustomCompletions = newCustomCompletions; // Replace the existing completions.
}
