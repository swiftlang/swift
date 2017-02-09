//===--- Requests.cpp -----------------------------------------------------===//
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

#include "DictionaryKeys.h"
#include "sourcekitd/CodeCompletionResultsArray.h"
#include "sourcekitd/DocStructureArray.h"
#include "sourcekitd/DocSupportAnnotationArray.h"
#include "sourcekitd/TokenAnnotationsArray.h"

#include "SourceKit/Core/Context.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Core/NotificationCenter.h"
#include "SourceKit/Support/Concurrency.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/UIdent.h"
#include "SourceKit/SwiftLang/Factory.h"

#include "swift/Basic/DemangleWrappers.h"
#include "swift/Basic/ManglingMacros.h"
#include "swift/Basic/Mangler.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include <mutex>

// FIXME: Portability.
#include <dispatch/dispatch.h>

using namespace sourcekitd;
using namespace SourceKit;

namespace {
class LazySKDUID {
  const char *Name;
  mutable std::atomic<sourcekitd_uid_t> UID { nullptr };
public:
  LazySKDUID(const char *Name) : Name(Name) { }

  sourcekitd_uid_t get() const {
    if (!UID)
      UID = SKDUIDFromUIdent(UIdent(Name));
    return UID;
  }

  operator sourcekitd_uid_t() const {
    return get();
  }
};
} // anonymous namespace

static LazySKDUID RequestProtocolVersion("source.request.protocol_version");

static LazySKDUID RequestCrashWithExit("source.request.crash_exit");

static LazySKDUID RequestDemangle("source.request.demangle");
static LazySKDUID RequestMangleSimpleClass("source.request.mangle_simple_class");

static LazySKDUID RequestIndex("source.request.indexsource");
static LazySKDUID RequestDocInfo("source.request.docinfo");
static LazySKDUID RequestCodeComplete("source.request.codecomplete");
static LazySKDUID RequestCodeCompleteOpen("source.request.codecomplete.open");
static LazySKDUID RequestCodeCompleteClose("source.request.codecomplete.close");
static LazySKDUID
    RequestCodeCompleteUpdate("source.request.codecomplete.update");
static LazySKDUID
    RequestCodeCompleteCacheOnDisk("source.request.codecomplete.cache.ondisk");
static LazySKDUID RequestCodeCompleteSetPopularAPI(
    "source.request.codecomplete.setpopularapi");
static LazySKDUID
    RequestCodeCompleteSetCustom("source.request.codecomplete.setcustom");
static LazySKDUID RequestCursorInfo("source.request.cursorinfo");
static LazySKDUID RequestRangeInfo("source.request.rangeinfo");
static LazySKDUID RequestRelatedIdents("source.request.relatedidents");
static LazySKDUID RequestEditorOpen("source.request.editor.open");
static LazySKDUID RequestEditorOpenInterface(
    "source.request.editor.open.interface");
static LazySKDUID RequestEditorOpenHeaderInterface(
    "source.request.editor.open.interface.header");
static LazySKDUID RequestEditorOpenSwiftSourceInterface(
    "source.request.editor.open.interface.swiftsource");
static LazySKDUID RequestEditorOpenSwiftTypeInterface(
    "source.request.editor.open.interface.swifttype");
static LazySKDUID RequestEditorExtractTextFromComment(
    "source.request.editor.extract.comment");
static LazySKDUID RequestEditorClose("source.request.editor.close");
static LazySKDUID RequestEditorReplaceText("source.request.editor.replacetext");
static LazySKDUID RequestEditorFormatText("source.request.editor.formattext");
static LazySKDUID RequestEditorExpandPlaceholder(
    "source.request.editor.expand_placeholder");
static LazySKDUID RequestEditorFindUSR("source.request.editor.find_usr");
static LazySKDUID RequestEditorFindInterfaceDoc(
    "source.request.editor.find_interface_doc");
static LazySKDUID RequestBuildSettingsRegister(
    "source.request.buildsettings.register");
static LazySKDUID RequestModuleGroups(
    "source.request.module.groups");
static LazySKDUID RequestNameTranslation("source.request.name.translation");

static LazySKDUID KindExpr("source.lang.swift.expr");
static LazySKDUID KindStmt("source.lang.swift.stmt");
static LazySKDUID KindType("source.lang.swift.type");

static LazySKDUID KindEverything("source.codecompletion.everything");
static LazySKDUID KindModule("source.codecompletion.module");
static LazySKDUID KindKeyword("source.codecompletion.keyword");
static LazySKDUID KindLiteral("source.codecompletion.literal");
static LazySKDUID KindCustom("source.codecompletion.custom");
static LazySKDUID KindIdentifier("source.codecompletion.identifier");

static UIdent DiagKindNote("source.diagnostic.severity.note");
static UIdent DiagKindWarning("source.diagnostic.severity.warning");
static UIdent DiagKindError("source.diagnostic.severity.error");


static UIdent KindNameObjc("source.lang.name.kind.objc");
static UIdent KindNameSwift("source.lang.name.kind.swift");

static LazySKDUID SwiftNameKind("source.lang.name.kind.swift");
static LazySKDUID ObjcNameKind("source.lang.name.kind.objc");

static void onDocumentUpdateNotification(StringRef DocumentName) {
  static UIdent DocumentUpdateNotificationUID(
      "source.notification.editor.documentupdate");

  ResponseBuilder RespBuilder;
  auto Dict = RespBuilder.getDictionary();
  Dict.set(KeyNotification, DocumentUpdateNotificationUID);
  Dict.set(KeyName, DocumentName);

  sourcekitd::postNotification(RespBuilder.createResponse());
}

static SourceKit::Context *GlobalCtx = nullptr;

void sourcekitd::initialize() {
  GlobalCtx = new SourceKit::Context(sourcekitd::getRuntimeLibPath(),
                                     SourceKit::createSwiftLangSupport);
  GlobalCtx->getNotificationCenter().addDocumentUpdateNotificationReceiver(
    onDocumentUpdateNotification);
}
void sourcekitd::shutdown() {
  delete GlobalCtx;
  GlobalCtx = nullptr;
}

static SourceKit::Context &getGlobalContext() {
  assert(GlobalCtx);
  return *GlobalCtx;
}

static sourcekitd_response_t demangleNames(ArrayRef<const char *> MangledNames,
                                           bool Simplified);

static sourcekitd_response_t
mangleSimpleClassNames(ArrayRef<std::pair<StringRef, StringRef>> ModuleClassPairs);

static sourcekitd_response_t indexSource(StringRef Filename,
                                         ArrayRef<const char *> Args,
                                         StringRef KnownHash);

static sourcekitd_response_t reportDocInfo(llvm::MemoryBuffer *InputBuf,
                                           StringRef ModuleName,
                                           ArrayRef<const char *> Args);

static void reportCursorInfo(const CursorInfo &Info, ResponseReceiver Rec);

static void reportRangeInfo(const RangeInfo &Info, ResponseReceiver Rec);

static void reportNameInfo(const NameTranslatingInfo &Info, ResponseReceiver Rec);

static void findRelatedIdents(StringRef Filename,
                              int64_t Offset,
                              ArrayRef<const char *> Args,
                              ResponseReceiver Rec);

static sourcekitd_response_t codeComplete(llvm::MemoryBuffer *InputBuf,
                                          int64_t Offset,
                                          ArrayRef<const char *> Args);

static sourcekitd_response_t codeCompleteOpen(StringRef name,
                                              llvm::MemoryBuffer *InputBuf,
                                              int64_t Offset,
                                              Optional<RequestDict> optionsDict,
                                              ArrayRef<const char *> Args);

static sourcekitd_response_t
codeCompleteUpdate(StringRef name, int64_t Offset,
                   Optional<RequestDict> optionsDict);

static sourcekitd_response_t codeCompleteClose(StringRef name, int64_t Offset);

static sourcekitd_response_t
editorOpen(StringRef Name, llvm::MemoryBuffer *Buf, bool EnableSyntaxMap,
           bool EnableStructure, bool EnableDiagnostics, bool SyntacticOnly,
           ArrayRef<const char *> Args);

static sourcekitd_response_t
editorOpenInterface(StringRef Name, StringRef ModuleName,
                    Optional<StringRef> Group, ArrayRef<const char *> Args,
                    bool SynthesizedExtensions,
                    Optional<StringRef> InterestedUSR);

static sourcekitd_response_t
editorOpenHeaderInterface(StringRef Name, StringRef HeaderName,
                          ArrayRef<const char *> Args,
                          bool SynthesizedExtensions);

static void
editorOpenSwiftSourceInterface(StringRef Name, StringRef SourceName,
                               ArrayRef<const char *> Args,
                               ResponseReceiver Rec);

static void
editorOpenSwiftTypeInterface(StringRef TypeUsr, ArrayRef<const char *> Args,
                             ResponseReceiver Rec);

static sourcekitd_response_t editorExtractTextFromComment(StringRef Source);

static sourcekitd_response_t
editorClose(StringRef Name, bool RemoveCache);

static sourcekitd_response_t
editorReplaceText(StringRef Name, llvm::MemoryBuffer *Buf, unsigned Offset,
                  unsigned Length, bool EnableSyntaxMap, bool EnableStructure,
                  bool EnableDiagnostics, bool SyntacticOnly);

static void
editorApplyFormatOptions(StringRef Name, RequestDict &FmtOptions);

static sourcekitd_response_t
editorFormatText(StringRef Name, unsigned Line, unsigned Length);

static sourcekitd_response_t
editorExpandPlaceholder(StringRef Name, unsigned Offset, unsigned Length);

static sourcekitd_response_t
editorFindUSR(StringRef DocumentName, StringRef USR);

static sourcekitd_response_t
editorFindInterfaceDoc(StringRef ModuleName, ArrayRef<const char *> Args);

static sourcekitd_response_t
editorFindModuleGroups(StringRef ModuleName, ArrayRef<const char *> Args);

static bool isSemanticEditorDisabled();

static void fillDictionaryForDiagnosticInfo(
    ResponseBuilder::Dictionary Elem, const DiagnosticEntryInfoBase &Info);

static void handleRequestImpl(sourcekitd_object_t Req,
                              ResponseReceiver Receiver);

void sourcekitd::handleRequest(sourcekitd_object_t Req,
                               ResponseReceiver Receiver) {
  LOG_SECTION("handleRequest-before", InfoHighPrio) {
    sourcekitd::printRequestObject(Req, Log->getOS());
  }

  handleRequestImpl(Req, [Receiver](sourcekitd_response_t Resp) {
    LOG_SECTION("handleRequest-after", InfoHighPrio) {
      // Responses are big, print them out with info medium priority.
      if (Logger::isLoggingEnabledForLevel(Logger::Level::InfoMediumPrio))
        sourcekitd::printResponse(Resp, Log->getOS());
    }

    Receiver(Resp);
  });
}

static std::unique_ptr<llvm::MemoryBuffer>
getInputBufForRequest(Optional<StringRef> SourceFile,
                      Optional<StringRef> SourceText,
                      llvm::SmallString<64> &ErrBuf) {

  std::unique_ptr<llvm::MemoryBuffer> InputBuf;

  if (SourceText.hasValue()) {
    StringRef BufName;
    if (SourceFile.hasValue())
      BufName = *SourceFile;
    else
      BufName = "<input>";
    InputBuf = llvm::MemoryBuffer::getMemBuffer(*SourceText, BufName);

  } else if (SourceFile.hasValue()) {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      llvm::MemoryBuffer::getFile(*SourceFile);
    if (FileBufOrErr) {
      InputBuf = std::move(FileBufOrErr.get());
    } else {
      llvm::raw_svector_ostream OSErr(ErrBuf);
      OSErr << "error opening input file '" << *SourceFile << "' ("
            << FileBufOrErr.getError().message() << ')';
      return nullptr;
    }
  } else {
    InputBuf = llvm::MemoryBuffer::getNewMemBuffer(0, "<input>");
  }

  return InputBuf;
}


static void
handleSemanticRequest(RequestDict Req,
                      ResponseReceiver Receiver,
                      sourcekitd_uid_t ReqUID,
                      Optional<StringRef> SourceFile,
                      Optional<StringRef> SourceText,
                      ArrayRef<const char *> Args);

void handleRequestImpl(sourcekitd_object_t ReqObj, ResponseReceiver Rec) {
  RequestDict Req(ReqObj);
  sourcekitd_uid_t ReqUID = Req.getUID(KeyRequest);
  if (!ReqUID)
    return Rec(createErrorRequestInvalid("missing 'key.request' with UID value"));

  if (ReqUID == RequestProtocolVersion) {
    ResponseBuilder RB;
    auto dict = RB.getDictionary();
    dict.set(KeyVersionMajor, ProtocolMajorVersion);
    dict.set(KeyVersionMinor, ProtocolMinorVersion);
    return Rec(RB.createResponse());
  }

  if (ReqUID == RequestCrashWithExit) {
    // 'exit' has the same effect as crashing but without the crash log.
    ::exit(1);
  }

  if (ReqUID == RequestDemangle) {
    SmallVector<const char *, 8> MangledNames;
    bool Failed = Req.getStringArray(KeyNames, MangledNames, /*isOptional=*/true);
    if (Failed) {
      return Rec(createErrorRequestInvalid(
                                        "'key.names' not an array of strings"));
    }
    int64_t Simplified = false;
    Req.getInt64(KeySimplified, Simplified, /*isOptional=*/true);
    return Rec(demangleNames(MangledNames, Simplified));
  }

  if (ReqUID == RequestMangleSimpleClass) {
    SmallVector<std::pair<StringRef, StringRef>, 16> ModuleClassPairs;
    sourcekitd_response_t err = nullptr;
    bool failed = Req.dictionaryArrayApply(KeyNames, [&](RequestDict dict) {
      Optional<StringRef> ModuleName = dict.getString(KeyModuleName);
      if (!ModuleName.hasValue()) {
        err = createErrorRequestInvalid("missing 'key.modulename'");
        return true;
      }
      Optional<StringRef> ClassName = dict.getString(KeyName);
      if (!ClassName.hasValue()) {
        err = createErrorRequestInvalid("missing 'key.name'");
        return true;
      }
      ModuleClassPairs.push_back(std::make_pair(*ModuleName, *ClassName));
      return false;
    });

    if (failed) {
      if (!err)
        err = createErrorRequestInvalid("missing 'key.names'");
      return Rec(err);
    }

    return Rec(mangleSimpleClassNames(ModuleClassPairs));
  }

  // Just accept 'source.request.buildsettings.register' for now, don't do
  // anything else.
  // FIXME: Heavy WIP here.
  if (ReqUID == RequestBuildSettingsRegister) {
    return Rec(ResponseBuilder().createResponse());
  }

  Optional<StringRef> SourceFile = Req.getString(KeySourceFile);
  Optional<StringRef> SourceText = Req.getString(KeySourceText);

  llvm::SmallString<64> ErrBuf;

  SmallVector<const char *, 8> Args;
  bool Failed = Req.getStringArray(KeyCompilerArgs, Args, /*isOptional=*/true);
  if (Failed) {
    return Rec(createErrorRequestInvalid(
        "'key.compilerargs' not an array of strings"));
  }

  if (ReqUID == RequestDocInfo) {
    std::unique_ptr<llvm::MemoryBuffer>
      InputBuf = getInputBufForRequest(SourceFile, SourceText, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    StringRef ModuleName;
    Optional<StringRef> ModuleNameOpt = Req.getString(KeyModuleName);
    if (ModuleNameOpt.hasValue()) ModuleName = *ModuleNameOpt;
    return Rec(reportDocInfo(InputBuf.get(), ModuleName, Args));
  }

  if (ReqUID == RequestEditorOpen) {
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    std::unique_ptr<llvm::MemoryBuffer>
    InputBuf = getInputBufForRequest(SourceFile, SourceText, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    int64_t EnableSyntaxMap = true;
    Req.getInt64(KeyEnableSyntaxMap, EnableSyntaxMap, /*isOptional=*/true);
    int64_t EnableStructure = true;
    Req.getInt64(KeyEnableStructure, EnableStructure, /*isOptional=*/true);
    int64_t EnableDiagnostics = true;
    Req.getInt64(KeyEnableDiagnostics, EnableDiagnostics, /*isOptional=*/true);
    int64_t SyntacticOnly = false;
    Req.getInt64(KeySyntacticOnly, SyntacticOnly, /*isOptional=*/true);
    return Rec(editorOpen(*Name, InputBuf.get(), EnableSyntaxMap, EnableStructure,
                          EnableDiagnostics, SyntacticOnly, Args));
  }
  if (ReqUID == RequestEditorClose) {
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));

    // Whether we remove the cached AST from libcache, by default, false.
    int64_t RemoveCache = false;
    Req.getInt64(KeyRemoveCache, RemoveCache, /*isOptional=*/true);
    return Rec(editorClose(*Name, RemoveCache));
  }
  if (ReqUID == RequestEditorReplaceText) {
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    std::unique_ptr<llvm::MemoryBuffer>
    InputBuf = getInputBufForRequest(SourceFile, SourceText, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    int64_t Offset = 0;
    Req.getInt64(KeyOffset, Offset, /*isOptional=*/true);
    int64_t Length = 0;
    Req.getInt64(KeyLength, Length, /*isOptional=*/true);
    int64_t EnableSyntaxMap = true;
    Req.getInt64(KeyEnableSyntaxMap, EnableSyntaxMap, /*isOptional=*/true);
    int64_t EnableStructure = true;
    Req.getInt64(KeyEnableStructure, EnableStructure, /*isOptional=*/true);
    int64_t EnableDiagnostics = true;
    Req.getInt64(KeyEnableDiagnostics, EnableDiagnostics, /*isOptional=*/true);
    int64_t SyntacticOnly = false;
    Req.getInt64(KeySyntacticOnly, SyntacticOnly, /*isOptional=*/true);
    return Rec(editorReplaceText(*Name, InputBuf.get(), Offset, Length,
                                 EnableSyntaxMap, EnableStructure,
                                 EnableDiagnostics, SyntacticOnly));
  }
  if (ReqUID == RequestEditorFormatText) {
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    Optional<RequestDict> FmtOptions = Req.getDictionary(KeyFormatOptions);
    if (FmtOptions.hasValue())
      editorApplyFormatOptions(*Name, *FmtOptions);
    int64_t Line = 0;
    Req.getInt64(KeyLine, Line, /*isOptional=*/false);
    int64_t Length = 0;
    Req.getInt64(KeyLength, Length, /*isOptional=*/true);
    return Rec(editorFormatText(*Name, Line, Length));
  }
  if (ReqUID == RequestEditorExpandPlaceholder) {
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    int64_t Offset = 0;
    Req.getInt64(KeyOffset, Offset, /*isOptional=*/false);
    int64_t Length = 0;
    Req.getInt64(KeyLength, Length, /*isOptional=*/false);
    return Rec(editorExpandPlaceholder(*Name, Offset, Length));
  }

  if (ReqUID == RequestEditorOpenInterface) {
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    Optional<StringRef> ModuleName = Req.getString(KeyModuleName);
    if (!ModuleName.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.modulename'"));
    Optional<StringRef> GroupName = Req.getString(KeyGroupName);
    int64_t SynthesizedExtension = false;
    Req.getInt64(KeySynthesizedExtension, SynthesizedExtension,
                 /*isOptional=*/true);
    Optional<StringRef> InterestedUSR = Req.getString(KeyInterestedUSR);
    return Rec(editorOpenInterface(*Name, *ModuleName, GroupName, Args,
                                   SynthesizedExtension, InterestedUSR));
  }

  if (ReqUID == RequestEditorOpenHeaderInterface) {
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    Optional<StringRef> HeaderName = Req.getString(KeyFilePath);
    if (!HeaderName.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.filepath'"));
    int64_t SynthesizedExtension = false;
    Req.getInt64(KeySynthesizedExtension, SynthesizedExtension,
                 /*isOptional=*/true);
    return Rec(editorOpenHeaderInterface(*Name, *HeaderName, Args,
                                         SynthesizedExtension));
  }

  if (ReqUID == RequestEditorOpenSwiftSourceInterface) {
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    Optional<StringRef> FileName = Req.getString(KeySourceFile);
    if (!FileName.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.sourcefile'"));
    return editorOpenSwiftSourceInterface(*Name, *FileName, Args, Rec);
  }

  if (ReqUID == RequestEditorOpenSwiftTypeInterface) {
    Optional<StringRef> Usr = Req.getString(KeyUSR);
    if (!Usr.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.usr'"));
    return editorOpenSwiftTypeInterface(*Usr, Args, Rec);
  }

  if (ReqUID == RequestEditorExtractTextFromComment) {
    Optional<StringRef> Source = Req.getString(KeySourceText);
    if (!Source.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.sourcetext'"));
    return Rec(editorExtractTextFromComment(Source.getValue()));
  }

  if (ReqUID == RequestEditorFindUSR) {
    Optional<StringRef> Name = Req.getString(KeySourceFile);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.sourcefile'"));
    Optional<StringRef> USR = Req.getString(KeyUSR);
    if (!USR.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.usr'"));
    return Rec(editorFindUSR(*Name, *USR));
  }

  if (ReqUID == RequestEditorFindInterfaceDoc) {
    Optional<StringRef> ModuleName = Req.getString(KeyModuleName);
    if (!ModuleName.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.modulename'"));
    return Rec(editorFindInterfaceDoc(*ModuleName, Args));
  }

  if (ReqUID == RequestModuleGroups) {
    Optional<StringRef> ModuleName = Req.getString(KeyModuleName);
    if (!ModuleName.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.modulename'"));
    return Rec(editorFindModuleGroups(*ModuleName, Args));
  }

  if (ReqUID == RequestCodeCompleteClose) {
    // Unlike opening code completion, this is not a semantic request.
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("missing 'key.offset'"));
    return Rec(codeCompleteClose(*Name, Offset));
  }

  if (ReqUID == RequestCodeCompleteCacheOnDisk) {
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
    Lang.codeCompleteCacheOnDisk(*Name);
    ResponseBuilder b;
    return Rec(b.createResponse());
  }

  if (ReqUID == RequestCodeCompleteSetPopularAPI) {
    llvm::SmallVector<const char *, 0> popular;
    llvm::SmallVector<const char *, 0> unpopular;
    Req.getStringArray(KeyPopular, popular, /*isOptional=*/false);
    Req.getStringArray(KeyUnpopular, unpopular, /*isOptional=*/false);
    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
    Lang.codeCompleteSetPopularAPI(popular, unpopular);
    ResponseBuilder b;
    return Rec(b.createResponse());
  }

  if (ReqUID == RequestCodeCompleteSetCustom) {
    SmallVector<CustomCompletionInfo, 16> customCompletions;
    sourcekitd_response_t err = nullptr;
    bool failed = Req.dictionaryArrayApply(KeyResults, [&](RequestDict dict) {
      CustomCompletionInfo CCInfo;
      Optional<StringRef> Name = dict.getString(KeyName);
      if (!Name.hasValue()) {
        err = createErrorRequestInvalid("missing 'key.name'");
        return true;
      }
      CCInfo.Name = *Name;

      sourcekitd_uid_t Kind = dict.getUID(KeyKind);
      if (!Kind) {
        err = createErrorRequestInvalid("missing 'key.kind'");
        return true;
      }
      CCInfo.Kind = Kind;

      SmallVector<sourcekitd_uid_t, 3> contexts;
      if (dict.getUIDArray(KeyContext, contexts, false)) {
        err = createErrorRequestInvalid("missing 'key.context'");
        return true;
      }

      for (auto context : contexts) {
        if (context == KindExpr) {
          CCInfo.Contexts |= CustomCompletionInfo::Expr;
        } else if (context == KindStmt) {
          CCInfo.Contexts |= CustomCompletionInfo::Stmt;
        } else if (context == KindType) {
          CCInfo.Contexts |= CustomCompletionInfo::Type;
        } else {
          err = createErrorRequestInvalid("invalid value for 'key.context'");
          return true;
        }
      }

      customCompletions.push_back(std::move(CCInfo));
      return false;
    });

    if (failed) {
      if (!err)
        err = createErrorRequestInvalid("missing 'key.results'");
      return Rec(err);
    }

    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
    Lang.codeCompleteSetCustom(customCompletions);
    return Rec(ResponseBuilder().createResponse());
  }

  if (!SourceFile.hasValue() && !SourceText.hasValue() &&
      ReqUID != RequestCodeCompleteUpdate)
    return Rec(createErrorRequestInvalid(
        "missing 'key.sourcefile' or 'key.sourcetext'"));

  // Requests that need semantic typechecking.

  // Typechecking arrays can blow up the stack currently.
  // Run them under a malloc'ed stack.

  static WorkQueue SemaQueue{ WorkQueue::Dequeuing::Concurrent,
                              "sourcekit.request.semantic" };
  sourcekitd_request_retain(ReqObj);
  SemaQueue.dispatch(
    [ReqObj, Rec, ReqUID, SourceFile, SourceText, Args] {
      RequestDict Req(ReqObj);
      handleSemanticRequest(Req, Rec, ReqUID, SourceFile, SourceText, Args);
      sourcekitd_request_release(ReqObj);
    },
    /*isStackDeep=*/true);
}

static void
handleSemanticRequest(RequestDict Req,
                      ResponseReceiver Rec,
                      sourcekitd_uid_t ReqUID,
                      Optional<StringRef> SourceFile,
                      Optional<StringRef> SourceText,
                      ArrayRef<const char *> Args) {

  llvm::SmallString<64> ErrBuf;

  if (ReqUID == RequestCodeComplete) {
    std::unique_ptr<llvm::MemoryBuffer>
    InputBuf = getInputBufForRequest(SourceFile, SourceText, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("missing 'key.offset'"));
    return Rec(codeComplete(InputBuf.get(), Offset, Args));
  }

  if (ReqUID == RequestCodeCompleteOpen) {
    std::unique_ptr<llvm::MemoryBuffer> InputBuf =
        getInputBufForRequest(SourceFile, SourceText, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("missing 'key.offset'"));
    Optional<RequestDict> options = Req.getDictionary(KeyCodeCompleteOptions);
    return Rec(codeCompleteOpen(*Name, InputBuf.get(), Offset, options, Args));
  }

  if (ReqUID == RequestCodeCompleteUpdate) {
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("missing 'key.offset'"));
    Optional<RequestDict> options = Req.getDictionary(KeyCodeCompleteOptions);
    return Rec(codeCompleteUpdate(*Name, Offset, options));
  }

  if (!SourceFile.hasValue())
    return Rec(createErrorRequestInvalid("missing 'key.sourcefile'"));

  if (ReqUID == RequestIndex) {
    StringRef Hash;
    Optional<StringRef> HashOpt = Req.getString(KeyHash);
    if (HashOpt.hasValue()) Hash = *HashOpt;
    return Rec(indexSource(*SourceFile, Args, Hash));
  }

  if (isSemanticEditorDisabled())
      return Rec(createErrorRequestFailed("semantic editor is disabled"));

  if (ReqUID == RequestCursorInfo) {
    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();

    int64_t Offset;
    if (!Req.getInt64(KeyOffset, Offset, /*isOptional=*/false)) {
      int64_t Length = 0;
      Req.getInt64(KeyLength, Length, /*isOptional=*/true);
      int64_t Actionables = false;
      Req.getInt64(KeyActionable, Actionables, /*isOptional=*/true);
      return Lang.getCursorInfo(
          *SourceFile, Offset, Length, Actionables, Args,
          [Rec](const CursorInfo &Info) { reportCursorInfo(Info, Rec); });
    }
    if (auto USR = Req.getString(KeyUSR)) {
      return Lang.getCursorInfoFromUSR(
          *SourceFile, *USR, Args,
          [Rec](const CursorInfo &Info) { reportCursorInfo(Info, Rec); });
    }

    return Rec(createErrorRequestInvalid(
        "either 'key.offset' or 'key.usr' is required"));
  }

  if (ReqUID == RequestRangeInfo) {
    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
    int64_t Offset;
    int64_t Length;
    if (!Req.getInt64(KeyOffset, Offset, /*isOptional=*/false)) {
      if (!Req.getInt64(KeyLength, Length, /*isOptional=*/false)) {
        return Lang.getRangeInfo(*SourceFile, Offset, Length, Args,
          [Rec](const RangeInfo &Info) { reportRangeInfo(Info, Rec); });
      }
    }

    return Rec(createErrorRequestInvalid(
      "'key.offset' or 'key.length' is required"));
  }

  if (ReqUID == RequestNameTranslation) {
    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false)) {
      return Rec(createErrorRequestInvalid("'key.offset' is required"));
    }
    NameTranslatingInfo Input;
    auto NK = Req.getUID(KeyNameKind);
    if (!NK) {
      return Rec(createErrorRequestInvalid("'key.namekind' is required"));
    }
    if (NK == SwiftNameKind)
      Input.NameKind = KindNameSwift;
    else if (NK == ObjcNameKind)
      Input.NameKind = KindNameObjc;
    else
      return Rec(createErrorRequestInvalid("'key.namekind' is unrecognizable"));
    if (auto Base = Req.getString(KeyBaseName)) {
      Input.BaseName = Base.getValue();
    }
    llvm::SmallVector<const char*, 4> ArgParts;
    llvm::SmallVector<const char*, 4> Selectors;
    Req.getStringArray(KeyArgNames, ArgParts, true);
    Req.getStringArray(KeySelectorPieces, Selectors, true);
    if (!ArgParts.empty() && !Selectors.empty()) {
      return Rec(createErrorRequestInvalid("cannot specify 'key.selectorpieces' "
                                           "and 'key.argnames' at the same time"));
    }
    std::vector<StringRef> Pieces(ArgParts.size() + Selectors.size());
    std::transform(ArgParts.begin(), ArgParts.end(), Pieces.begin(),
                   [](const char *C) { return StringRef(C); });
    std::transform(Selectors.begin(), Selectors.end(), Pieces.begin(),
                   [](const char *C) { return StringRef(C); });
    Input.ArgNames = llvm::makeArrayRef(Pieces);
    return Lang.getNameInfo(*SourceFile, Offset, Input, Args,
      [Rec](const NameTranslatingInfo &Info) { reportNameInfo(Info, Rec); });
  }

  if (ReqUID == RequestRelatedIdents) {
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("missing 'key.offset'"));
    return findRelatedIdents(*SourceFile, Offset, Args, Rec);
  }

  {
    llvm::raw_svector_ostream OSErr(ErrBuf);
    OSErr << "unknown request: " << UIdentFromSKDUID(ReqUID).getName();
  }
  return Rec(createErrorRequestInvalid(ErrBuf.c_str()));
}

//===----------------------------------------------------------------------===//
// Index
//===----------------------------------------------------------------------===//

namespace {
class SKIndexingConsumer : public IndexingConsumer {
  struct Entity {
    UIdent Kind;
    ResponseBuilder::Dictionary Data;
    ResponseBuilder::Array Entities;
    ResponseBuilder::Array Related;
  };
  SmallVector<Entity, 6> EntitiesStack;

  struct Dependency {
    UIdent Kind;
    ResponseBuilder::Dictionary Data;
    ResponseBuilder::Array Dependencies;
  };
  SmallVector<Dependency, 6> DependenciesStack;

  ResponseBuilder::Dictionary TopDict;
  bool Cancelled = false;

public:
  std::string ErrorDescription;

  explicit SKIndexingConsumer(ResponseBuilder &RespBuilder) {
    TopDict = RespBuilder.getDictionary();

    // First in stack is the top-level "key.entities" container.
    EntitiesStack.push_back(
        { UIdent(),
          TopDict,
          ResponseBuilder::Array(),
          ResponseBuilder::Array() });

    DependenciesStack.push_back({UIdent(), TopDict, ResponseBuilder::Array() });
  }
  ~SKIndexingConsumer() {
    assert(Cancelled ||
           (EntitiesStack.size() == 1 && DependenciesStack.size() == 1));
    (void) Cancelled;
  }

  void failed(StringRef ErrDescription) override;

  bool recordHash(StringRef Hash, bool isKnown) override;

  bool startDependency(UIdent Kind,
                       StringRef Name,
                       StringRef Path,
                       bool IsSystem,
                       StringRef Hash) override;

  bool finishDependency(UIdent Kind) override;

  bool startSourceEntity(const EntityInfo &Info) override;

  bool recordRelatedEntity(const EntityInfo &Info) override;

  bool finishSourceEntity(UIdent Kind) override;
};
}

static sourcekitd_response_t indexSource(StringRef Filename,
                                         ArrayRef<const char *> Args,
                                         StringRef KnownHash) {
  ResponseBuilder RespBuilder;
  SKIndexingConsumer IdxConsumer(RespBuilder);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.indexSource(Filename, IdxConsumer, Args, KnownHash);

  if (!IdxConsumer.ErrorDescription.empty())
    return createErrorRequestFailed(IdxConsumer.ErrorDescription.c_str());

  return RespBuilder.createResponse();
}

void SKIndexingConsumer::failed(StringRef ErrDescription) {
  ErrorDescription = ErrDescription;
}

bool SKIndexingConsumer::recordHash(StringRef Hash, bool isKnown) {
  assert(!Hash.empty());
  TopDict.set(KeyHash, Hash);
  if (!isKnown) {
    // If the hash is known key.entities should be missing otherwise it should
    // exist, even as an empty array, so create it here.
    assert(EntitiesStack.size() == 1);
    Entity &Top = EntitiesStack.back();
    ResponseBuilder::Array &Arr = Top.Entities;
    assert(Arr.isNull());
    Arr = Top.Data.setArray(KeyEntities);
  }
  return true;
}

bool SKIndexingConsumer::startDependency(UIdent Kind,
                                         StringRef Name,
                                         StringRef Path,
                                         bool IsSystem,
                                         StringRef Hash) {
  Dependency &Parent = DependenciesStack.back();
  ResponseBuilder::Array &Arr = Parent.Dependencies;
  if (Arr.isNull())
    Arr = Parent.Data.setArray(KeyDependencies);

  auto Elem = Arr.appendDictionary();
  Elem.set(KeyKind, Kind);
  Elem.set(KeyName, Name);
  Elem.set(KeyFilePath, Path);
  if (IsSystem)
    Elem.setBool(KeyIsSystem, IsSystem);
  if (!Hash.empty())
    Elem.set(KeyHash, Hash);

  DependenciesStack.push_back({ Kind, Elem, ResponseBuilder::Array() });
  return true;
}

bool SKIndexingConsumer::finishDependency(UIdent Kind) {
  assert(DependenciesStack.back().Kind == Kind);
  DependenciesStack.pop_back();
  return true;
}

bool SKIndexingConsumer::startSourceEntity(const EntityInfo &Info) {
  Entity &Parent = EntitiesStack.back();
  ResponseBuilder::Array &Arr = Parent.Entities;
  if (Arr.isNull())
    Arr = Parent.Data.setArray(KeyEntities);

  auto Elem = Arr.appendDictionary();
  Elem.set(KeyKind, Info.Kind);
  if (!Info.Name.empty())
    Elem.set(KeyName, Info.Name);
  if (!Info.USR.empty())
    Elem.set(KeyUSR, Info.USR);
  if (Info.Line != 0) {
    assert(Info.Column != 0);
    Elem.set(KeyLine, Info.Line);
    Elem.set(KeyColumn, Info.Column);
  }
  if (!Info.Group.empty())
    Elem.set(KeyGroupName, Info.Group);

  if (!Info.ReceiverUSR.empty())
    Elem.set(KeyReceiverUSR, Info.ReceiverUSR);
  if (Info.IsDynamic)
    Elem.setBool(KeyIsDynamic, true);
  if (Info.IsTestCandidate)
    Elem.setBool(KeyIsTestCandidate, true);

  if (!Info.Attrs.empty()) {
    auto AttrArray = Elem.setArray(KeyAttributes);
    for (auto Attr : Info.Attrs) {
      auto AttrDict = AttrArray.appendDictionary();
      AttrDict.set(KeyAttribute, Attr);
    }
  }

  EntitiesStack.push_back({ Info.Kind, Elem, ResponseBuilder::Array(),
                            ResponseBuilder::Array()});
  return true;
}

bool SKIndexingConsumer::recordRelatedEntity(const EntityInfo &Info) {
  assert(EntitiesStack.size() > 1 && "Related entity at top-level ?");
  Entity &Parent = EntitiesStack.back();
  ResponseBuilder::Array &Arr = Parent.Related;
  if (Arr.isNull())
    Arr = Parent.Data.setArray(KeyRelated);

  auto Elem = Arr.appendDictionary();
  Elem.set(KeyKind, Info.Kind);
  if (!Info.Name.empty())
    Elem.set(KeyName, Info.Name);
  if (!Info.USR.empty())
    Elem.set(KeyUSR, Info.USR);
  if (Info.Line != 0) {
    assert(Info.Column != 0);
    Elem.set(KeyLine, Info.Line);
    Elem.set(KeyColumn, Info.Column);
  }

  return true;
}

bool SKIndexingConsumer::finishSourceEntity(UIdent Kind) {
  Entity &CurrEnt = EntitiesStack.back();
  assert(CurrEnt.Kind == Kind);
  (void) CurrEnt;
  EntitiesStack.pop_back();
  return true;
}

//===----------------------------------------------------------------------===//
// ReportDocInfo
//===----------------------------------------------------------------------===//

namespace {

class SKDocConsumer : public DocInfoConsumer {
  ResponseBuilder &RespBuilder;

  struct Entity {
    UIdent Kind;
    ResponseBuilder::Dictionary Data;
    ResponseBuilder::Array Entities;
    ResponseBuilder::Array Inherits;
    ResponseBuilder::Array Conforms;
    ResponseBuilder::Array Attrs;
  };
  SmallVector<Entity, 6> EntitiesStack;

  ResponseBuilder::Dictionary TopDict;
  ResponseBuilder::Array Diags;

  DocSupportAnnotationArrayBuilder AnnotationsBuilder;

  bool Cancelled = false;

  void addDocEntityInfoToDict(const DocEntityInfo &Info,
                              ResponseBuilder::Dictionary Dict);
public:
  std::string ErrorDescription;

  explicit SKDocConsumer(ResponseBuilder &RespBuilder)
      : RespBuilder(RespBuilder) {
    TopDict = RespBuilder.getDictionary();

    // First in stack is the top-level "key.entities" container.
    EntitiesStack.push_back(
        { UIdent(),
          TopDict,
          ResponseBuilder::Array(),
          ResponseBuilder::Array(),
          ResponseBuilder::Array(),
          ResponseBuilder::Array() });
  }
  ~SKDocConsumer() {
    assert(Cancelled || EntitiesStack.size() == 1);
    (void) Cancelled;
  }

  sourcekitd_response_t createResponse() {
    TopDict.setCustomBuffer(KeyAnnotations,
        CustomBufferKind::DocSupportAnnotationArray,
        AnnotationsBuilder.createBuffer());
    return RespBuilder.createResponse();
  }

  void failed(StringRef ErrDescription) override;

  bool handleSourceText(StringRef Text) override;

  bool handleAnnotation(const DocEntityInfo &Info) override;

  bool startSourceEntity(const DocEntityInfo &Info) override;

  bool handleInheritsEntity(const DocEntityInfo &Info) override;
  bool handleConformsToEntity(const DocEntityInfo &Info) override;
  bool handleExtendsEntity(const DocEntityInfo &Info) override;

  bool handleAvailableAttribute(const AvailableAttrInfo &Info) override;

  bool finishSourceEntity(UIdent Kind) override;

  bool handleDiagnostic(const DiagnosticEntryInfo &Info) override;
};
}

static bool isSwiftPrefixed(StringRef MangledName) {
  if (MangledName.size() < 2)
    return false;
  return MangledName[0] == '_' &&
         (MangledName[1] == 'T' || MangledName[1] == MANGLING_PREFIX_STR[1]);
}

static sourcekitd_response_t demangleNames(ArrayRef<const char *> MangledNames,
                                           bool Simplified) {
  swift::Demangle::DemangleOptions DemangleOptions;
  if (Simplified) {
    DemangleOptions =
      swift::Demangle::DemangleOptions::SimplifiedUIDemangleOptions();
  }

  auto getDemangledName = [&](StringRef MangledName) -> std::string {
    if (!isSwiftPrefixed(MangledName))
      return std::string(); // Not a mangled name

    std::string Result = swift::demangle_wrappers::demangleSymbolAsString(
        MangledName, DemangleOptions);

    if (Result == MangledName)
      return std::string(); // Not a mangled name

    return Result;
  };

  ResponseBuilder RespBuilder;
  auto Arr = RespBuilder.getDictionary().setArray(KeyResults);
  for (auto MangledName : MangledNames) {
    std::string Result = getDemangledName(MangledName);
    auto Entry = Arr.appendDictionary();
    Entry.set(KeyName, Result.c_str());
  }

  return RespBuilder.createResponse();
}

static std::string mangleSimpleClass(StringRef moduleName,
                                     StringRef className) {
  using namespace swift::Demangle;

  auto moduleNode = NodeFactory::create(Node::Kind::Module, moduleName);
  auto IdNode = NodeFactory::create(Node::Kind::Identifier, className);
  auto classNode = NodeFactory::create(Node::Kind::Class);
  auto typeNode = NodeFactory::create(Node::Kind::Type);
  auto typeManglingNode = NodeFactory::create(Node::Kind::TypeMangling);
  auto globalNode = NodeFactory::create(Node::Kind::Global);

  classNode->addChildren(moduleNode, IdNode);
  typeNode->addChild(classNode);
  typeManglingNode->addChild(typeNode);
  globalNode->addChild(typeManglingNode);
  return mangleNode(globalNode, swift::useNewMangling(globalNode));
}

static sourcekitd_response_t
mangleSimpleClassNames(ArrayRef<std::pair<StringRef, StringRef>> ModuleClassPairs) {
  ResponseBuilder RespBuilder;
  auto Arr = RespBuilder.getDictionary().setArray(KeyResults);
  for (auto &pair : ModuleClassPairs) {
    std::string Result = mangleSimpleClass(pair.first, pair.second);
    auto Entry = Arr.appendDictionary();
    Entry.set(KeyName, Result.c_str());
  }

  return RespBuilder.createResponse();
}

static sourcekitd_response_t reportDocInfo(llvm::MemoryBuffer *InputBuf,
                                           StringRef ModuleName,
                                           ArrayRef<const char *> Args) {
  ResponseBuilder RespBuilder;
  SKDocConsumer DocConsumer(RespBuilder);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.getDocInfo(InputBuf, ModuleName, Args, DocConsumer);

  if (!DocConsumer.ErrorDescription.empty())
    return createErrorRequestFailed(DocConsumer.ErrorDescription.c_str());

  return DocConsumer.createResponse();
}

void SKDocConsumer::addDocEntityInfoToDict(const DocEntityInfo &Info,
                                           ResponseBuilder::Dictionary Elem) {
  Elem.set(KeyKind, Info.Kind);
  if (!Info.Name.empty())
    Elem.set(KeyName, Info.Name);
  if (!Info.Argument.empty())
    Elem.set(KeyKeyword, Info.Argument);
  if (!Info.USR.empty())
    Elem.set(KeyUSR, Info.USR);
  if (!Info.OriginalUSR.empty())
    Elem.set(KeyOriginalUSR, Info.OriginalUSR);
  if (!Info.ProvideImplementationOfUSR.empty())
    Elem.set(KeyDefaultImplementationOf, Info.ProvideImplementationOfUSR);
  if (Info.Length > 0) {
    Elem.set(KeyOffset, Info.Offset);
    Elem.set(KeyLength, Info.Length);
  }
  if (Info.IsUnavailable)
    Elem.set(KeyIsUnavailable, Info.IsUnavailable);
  if (Info.IsDeprecated)
    Elem.set(KeyIsDeprecated, Info.IsDeprecated);
  if (Info.IsOptional)
    Elem.set(KeyIsOptional, Info.IsOptional);
  if (!Info.DocComment.empty())
    Elem.set(KeyDocFullAsXML, Info.DocComment);
  if (!Info.FullyAnnotatedDecl.empty())
    Elem.set(KeyFullyAnnotatedDecl, Info.FullyAnnotatedDecl);

  if (!Info.GenericParams.empty()) {
    auto GPArray = Elem.setArray(KeyGenericParams);
    for (auto &GP : Info.GenericParams) {
      auto GPElem = GPArray.appendDictionary();
      GPElem.set(KeyName, GP.Name);
      if (!GP.Inherits.empty())
        GPElem.set(KeyInherits, GP.Inherits);
    }
  }
  // Note that due to protocol extensions, GenericRequirements may be non-empty
  // while GenericParams is empty.
  if (!Info.GenericRequirements.empty()) {
    auto ReqArray = Elem.setArray(KeyGenericRequirements);
    for (auto &Req : Info.GenericRequirements) {
      auto ReqElem = ReqArray.appendDictionary();
      ReqElem.set(KeyDescription, Req);
    }
  }
}

void SKDocConsumer::failed(StringRef ErrDescription) {
  ErrorDescription = ErrDescription;
}

bool SKDocConsumer::handleSourceText(StringRef Text) {
  TopDict.set(KeySourceText, Text);
  return true;
}

bool SKDocConsumer::handleAnnotation(const DocEntityInfo &Info) {
  AnnotationsBuilder.add(Info);
  return true;
}

bool SKDocConsumer::startSourceEntity(const DocEntityInfo &Info) {
  Entity &Parent = EntitiesStack.back();
  ResponseBuilder::Array &Arr = Parent.Entities;
  if (Arr.isNull())
    Arr = Parent.Data.setArray(KeyEntities);

  auto Elem = Arr.appendDictionary();
  addDocEntityInfoToDict(Info, Elem);

  EntitiesStack.push_back({ Info.Kind, Elem, ResponseBuilder::Array(),
                            ResponseBuilder::Array(),
                            ResponseBuilder::Array(),
                            ResponseBuilder::Array()});
  return true;
}

bool SKDocConsumer::handleInheritsEntity(const DocEntityInfo &Info) {
  assert(EntitiesStack.size() > 1 && "Related entity at top-level ?");
  Entity &Parent = EntitiesStack.back();
  ResponseBuilder::Array &Arr = Parent.Inherits;
  if (Arr.isNull())
    Arr = Parent.Data.setArray(KeyInherits);

  addDocEntityInfoToDict(Info, Arr.appendDictionary());
  return true;
}

bool SKDocConsumer::handleConformsToEntity(const DocEntityInfo &Info) {
  assert(EntitiesStack.size() > 1 && "Related entity at top-level ?");
  Entity &Parent = EntitiesStack.back();
  ResponseBuilder::Array &Arr = Parent.Conforms;
  if (Arr.isNull())
    Arr = Parent.Data.setArray(KeyConforms);

  addDocEntityInfoToDict(Info, Arr.appendDictionary());
  return true;
}

bool SKDocConsumer::handleExtendsEntity(const DocEntityInfo &Info) {
  assert(EntitiesStack.size() > 1 && "Related entity at top-level ?");
  Entity &Parent = EntitiesStack.back();
  addDocEntityInfoToDict(Info, Parent.Data.setDictionary(KeyExtends));
  return true;
}

bool SKDocConsumer::handleAvailableAttribute(const AvailableAttrInfo &Info) {
  Entity &Parent = EntitiesStack.back();
  ResponseBuilder::Array &Arr = Parent.Attrs;
  if (Arr.isNull())
    Arr = Parent.Data.setArray(KeyAttributes);

  auto Elem = Arr.appendDictionary();
  Elem.set(KeyKind, Info.AttrKind);
  if (Info.IsUnavailable)
    Elem.set(KeyIsUnavailable, Info.IsUnavailable);
  if (Info.IsDeprecated)
    Elem.set(KeyIsDeprecated, Info.IsDeprecated);
  if (Info.Platform.isValid())
    Elem.set(KeyPlatform, Info.Platform);
  if (!Info.Message.empty())
    Elem.set(KeyMessage, Info.Message);
  if (Info.Introduced.hasValue())
    Elem.set(KeyIntroduced, Info.Introduced.getValue().getAsString());
  if (Info.Deprecated.hasValue())
    Elem.set(KeyDeprecated, Info.Deprecated.getValue().getAsString());
  if (Info.Obsoleted.hasValue())
    Elem.set(KeyObsoleted, Info.Obsoleted.getValue().getAsString());

  return true;
}

bool SKDocConsumer::finishSourceEntity(UIdent Kind) {
  Entity &CurrEnt = EntitiesStack.back();
  assert(CurrEnt.Kind == Kind);
  (void) CurrEnt;
  EntitiesStack.pop_back();
  return true;
}

bool SKDocConsumer::handleDiagnostic(const DiagnosticEntryInfo &Info) {
  ResponseBuilder::Array &Arr = Diags;
  if (Arr.isNull())
    Arr = TopDict.setArray(KeyDiagnostics);

  auto Elem = Arr.appendDictionary();
  UIdent SeverityUID;
  switch (Info.Severity) {
  case DiagnosticSeverityKind::Warning:
    SeverityUID = DiagKindWarning;
    break;
  case DiagnosticSeverityKind::Error:
    SeverityUID = DiagKindError;
    break;
  }

  Elem.set(KeySeverity, SeverityUID);
  fillDictionaryForDiagnosticInfo(Elem, Info);

  if (!Info.Notes.empty()) {
    auto NotesArr = Elem.setArray(KeyDiagnostics);
    for (auto &NoteDiag : Info.Notes) {
      auto NoteElem = NotesArr.appendDictionary();
      NoteElem.set(KeySeverity, DiagKindNote);
      fillDictionaryForDiagnosticInfo(NoteElem, NoteDiag);
    }
  }

  return true;
}

//===----------------------------------------------------------------------===//
// ReportCursorInfo
//===----------------------------------------------------------------------===//

static void reportCursorInfo(const CursorInfo &Info, ResponseReceiver Rec) {

  if (Info.IsCancelled)
    return Rec(createErrorRequestCancelled());

  ResponseBuilder RespBuilder;
  if (Info.Kind.isInvalid())
    return Rec(RespBuilder.createResponse());

  auto Elem = RespBuilder.getDictionary();
  Elem.set(KeyKind, Info.Kind);
  Elem.set(KeyName, Info.Name);
  if (!Info.USR.empty())
    Elem.set(KeyUSR, Info.USR);
  if (!Info.TypeName.empty())
    Elem.set(KeyTypeName, Info.TypeName);
  if (!Info.DocComment.empty())
    Elem.set(KeyDocFullAsXML, Info.DocComment);
  if (!Info.AnnotatedDeclaration.empty())
    Elem.set(KeyAnnotatedDecl, Info.AnnotatedDeclaration);
  if (!Info.FullyAnnotatedDeclaration.empty())
    Elem.set(KeyFullyAnnotatedDecl, Info.FullyAnnotatedDeclaration);
  if (!Info.ModuleName.empty())
    Elem.set(KeyModuleName, Info.ModuleName);
  if (!Info.GroupName.empty())
    Elem.set(KeyGroupName, Info.GroupName);
  if (!Info.ModuleInterfaceName.empty())
    Elem.set(KeyModuleInterfaceName, Info.ModuleInterfaceName);
  if (Info.DeclarationLoc.hasValue()) {
    Elem.set(KeyOffset, Info.DeclarationLoc.getValue().first);
    Elem.set(KeyLength, Info.DeclarationLoc.getValue().second);
    if (!Info.Filename.empty())
      Elem.set(KeyFilePath, Info.Filename);
  }
  if (!Info.OverrideUSRs.empty()) {
    auto Overrides = Elem.setArray(KeyOverrides);
    for (auto USR : Info.OverrideUSRs) {
      auto Override = Overrides.appendDictionary();
      Override.set(KeyUSR, USR);
    }
  }
  if (!Info.ModuleGroupArray.empty()) {
    auto Groups = Elem.setArray(KeyModuleGroups);
    for (auto Name : Info.ModuleGroupArray) {
      auto Entry = Groups.appendDictionary();
      Entry.set(KeyGroupName, Name);
    }
  }
  if (!Info.AvailableActions.empty()) {
    auto Actions = Elem.setArray(KeyActionable);
    for (auto Name : Info.AvailableActions) {
      auto Entry = Actions.appendDictionary();
      Entry.set(KeyActionName, Name);
    }
  }
  if (!Info.AnnotatedRelatedDeclarations.empty()) {
    auto RelDecls = Elem.setArray(KeyRelatedDecls);
    for (auto AnnotDecl : Info.AnnotatedRelatedDeclarations) {
      auto RelDecl = RelDecls.appendDictionary();
      RelDecl.set(KeyAnnotatedDecl, AnnotDecl);
    }
  }
  if (Info.IsSystem)
    Elem.setBool(KeyIsSystem, true);
  if (!Info.TypeInterface.empty())
    Elem.set(KeyTypeInterface, Info.TypeInterface);
  if (!Info.TypeUSR.empty())
    Elem.set(KeyTypeUsr, Info.TypeUSR);
  if (!Info.ContainerTypeUSR.empty())
    Elem.set(KeyContainerTypeUsr, Info.ContainerTypeUSR);

  return Rec(RespBuilder.createResponse());
}

//===----------------------------------------------------------------------===//
// ReportRangeInfo
//===----------------------------------------------------------------------===//

static void reportRangeInfo(const RangeInfo &Info, ResponseReceiver Rec) {
  if (Info.IsCancelled)
    return Rec(createErrorRequestCancelled());
  ResponseBuilder RespBuilder;
  auto Elem = RespBuilder.getDictionary();
  Elem.set(KeyKind, Info.RangeKind);
  Elem.set(KeyTypeName, Info.ExprType);
  Elem.set(KeyRangeContent, Info.RangeContent);
  Rec(RespBuilder.createResponse());
}

//===----------------------------------------------------------------------===//
// ReportNameInfo
//===----------------------------------------------------------------------===//

static void reportNameInfo(const NameTranslatingInfo &Info, ResponseReceiver Rec) {
  if (Info.IsCancelled)
    return Rec(createErrorRequestCancelled());

  ResponseBuilder RespBuilder;
  if (Info.NameKind.isInvalid())
    return Rec(RespBuilder.createResponse());
  if (Info.BaseName.empty() && Info.ArgNames.empty())
    return Rec(RespBuilder.createResponse());

  auto Elem = RespBuilder.getDictionary();
  Elem.set(KeyNameKind, Info.NameKind);

  if (!Info.BaseName.empty()) {
    Elem.set(KeyBaseName, Info.BaseName);
  }
  if (!Info.ArgNames.empty()) {
    auto Arr = Elem.setArray(Info.NameKind == KindNameSwift ? KeyArgNames :
                             KeySelectorPieces);
    for (auto N : Info.ArgNames) {
      auto NameEle = Arr.appendDictionary();
      NameEle.set(KeyName, N);
    }
  }
  Rec(RespBuilder.createResponse());
}

//===----------------------------------------------------------------------===//
// FindRelatedIdents
//===----------------------------------------------------------------------===//

static void findRelatedIdents(StringRef Filename,
                              int64_t Offset,
                              ArrayRef<const char *> Args,
                              ResponseReceiver Rec) {
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.findRelatedIdentifiersInFile(Filename, Offset, Args,
                                    [Rec](const RelatedIdentsInfo &Info) {
    if (Info.IsCancelled)
      return Rec(createErrorRequestCancelled());

    ResponseBuilder RespBuilder;
    auto Arr = RespBuilder.getDictionary().setArray(KeyResults);
    for (auto R : Info.Ranges) {
      auto Elem = Arr.appendDictionary();
      Elem.set(KeyOffset, R.first);
      Elem.set(KeyLength, R.second);
    }

    Rec(RespBuilder.createResponse());
  });
}

//===----------------------------------------------------------------------===//
// CodeComplete
//===----------------------------------------------------------------------===//

namespace {
class SKCodeCompletionConsumer : public CodeCompletionConsumer {
  ResponseBuilder &RespBuilder;
  CodeCompletionResultsArrayBuilder ResultsBuilder;

  std::string ErrorDescription;

public:
  explicit SKCodeCompletionConsumer(ResponseBuilder &RespBuilder)
    : RespBuilder(RespBuilder) {
  }

  sourcekitd_response_t createResponse() {
    if (!ErrorDescription.empty())
      return createErrorRequestFailed(ErrorDescription.c_str());

    RespBuilder.getDictionary().setCustomBuffer(KeyResults,
        CustomBufferKind::CodeCompletionResultsArray,
        ResultsBuilder.createBuffer());
    return RespBuilder.createResponse();
  }


  void failed(StringRef ErrDescription) override;

  bool handleResult(const CodeCompletionInfo &Info) override;
};
}

static sourcekitd_response_t codeComplete(llvm::MemoryBuffer *InputBuf,
                                          int64_t Offset,
                                          ArrayRef<const char *> Args) {
  ResponseBuilder RespBuilder;
  SKCodeCompletionConsumer CCC(RespBuilder);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.codeComplete(InputBuf, Offset, CCC, Args);
  return CCC.createResponse();
}

void SKCodeCompletionConsumer::failed(StringRef ErrDescription) {
  ErrorDescription = ErrDescription;
}

bool SKCodeCompletionConsumer::handleResult(const CodeCompletionInfo &R) {
  Optional<StringRef> ModuleNameOpt;
  if (!R.ModuleName.empty())
    ModuleNameOpt = R.ModuleName;
  Optional<StringRef> DocBriefOpt;
  if (!R.DocBrief.empty())
    DocBriefOpt = R.DocBrief;
  Optional<StringRef> AssocUSRsOpt;
  if (!R.AssocUSRs.empty())
    AssocUSRsOpt = R.AssocUSRs;

  assert(!R.ModuleImportDepth && "not implemented on CompactArray path");

  ResultsBuilder.add(R.Kind,
                     R.Name,
                     R.Description,
                     R.SourceText,
                     R.TypeName,
                     ModuleNameOpt,
                     DocBriefOpt,
                     AssocUSRsOpt,
                     R.SemanticContext,
                     R.NotRecommended,
                     R.NumBytesToErase);
  return true;
}


//===----------------------------------------------------------------------===//
// (New) CodeComplete
//===----------------------------------------------------------------------===//

namespace {
class SKGroupedCodeCompletionConsumer : public GroupedCodeCompletionConsumer {
  ResponseBuilder &RespBuilder;
  ResponseBuilder::Dictionary Response;
  SmallVector<ResponseBuilder::Array, 3> GroupContentsStack;
  std::string ErrorDescription;

public:
  explicit SKGroupedCodeCompletionConsumer(ResponseBuilder &RespBuilder)
      : RespBuilder(RespBuilder) {}

  sourcekitd_response_t createResponse() {
    if (!ErrorDescription.empty())
      return createErrorRequestFailed(ErrorDescription.c_str());
    assert(GroupContentsStack.empty() && "mismatched start/endGroup");
    return RespBuilder.createResponse();
  }

  void failed(StringRef ErrDescription) override;
  bool handleResult(const CodeCompletionInfo &Info) override;
  void startGroup(UIdent kind, StringRef name) override;
  void endGroup() override;
  void setNextRequestStart(unsigned offset) override;
};

class SKOptionsDictionary : public OptionsDictionary {
  RequestDict &Options;

public:
  explicit SKOptionsDictionary(RequestDict &Options) : Options(Options) {}

  bool valueForOption(UIdent Key, unsigned &Val) override {
    int64_t result;
    if (Options.getInt64(Key, result, false))
      return false;
    Val = static_cast<unsigned>(result);
    return true;
  }

  bool valueForOption(UIdent Key, bool &Val) override {
    int64_t result;
    if (Options.getInt64(Key, result, false))
      return false;
    Val = result ? true : false;
    return true;
  }

  bool valueForOption(UIdent Key, StringRef &Val) override {
    Optional<StringRef> value = Options.getString(Key);
    if (!value)
      return false;
    Val = *value;
    return true;
  }
};
} // end anonymous namespace

static sourcekitd_response_t codeCompleteOpen(StringRef Name,
                                              llvm::MemoryBuffer *InputBuf,
                                              int64_t Offset,
                                              Optional<RequestDict> optionsDict,
                                              ArrayRef<const char *> Args) {
  ResponseBuilder RespBuilder;
  SKGroupedCodeCompletionConsumer CCC(RespBuilder);
  std::unique_ptr<SKOptionsDictionary> options;
  std::vector<FilterRule> filterRules;
  if (optionsDict) {
    options = llvm::make_unique<SKOptionsDictionary>(*optionsDict);
    bool failed = false;
    optionsDict->dictionaryArrayApply(KeyFilterRules, [&](RequestDict dict) {
      FilterRule rule;
      auto kind = dict.getUID(KeyKind);
      if (kind == KindEverything) {
        rule.kind = FilterRule::Everything;
      } else if (kind == KindModule) {
        rule.kind = FilterRule::Module;
      } else if (kind == KindKeyword) {
        rule.kind = FilterRule::Keyword;
      } else if (kind == KindLiteral) {
        rule.kind = FilterRule::Literal;
      } else if (kind == KindCustom) {
        rule.kind = FilterRule::CustomCompletion;
      } else if (kind == KindIdentifier) {
        rule.kind = FilterRule::Identifier;
      } else {
        // Warning: unknown
      }

      int64_t hide;
      if (dict.getInt64(KeyHide, hide, false)) {
        failed = true;
        CCC.failed("filter rule missing required key 'key.hide'");
        return true;
      }

      rule.hide = hide;

      switch (rule.kind) {
      case FilterRule::Everything:
        break;
      case FilterRule::Module:
      case FilterRule::Identifier: {
        SmallVector<const char *, 8> names;
        if (dict.getStringArray(KeyNames, names, false)) {
          failed = true;
          CCC.failed("filter rule missing required key 'key.names'");
          return true;
        }
        rule.names.assign(names.begin(), names.end());
        break;
      }
      case FilterRule::Keyword:
      case FilterRule::Literal:
      case FilterRule::CustomCompletion: {
        SmallVector<sourcekitd_uid_t, 8> uids;
        dict.getUIDArray(KeyUIDs, uids, true);
        for (auto uid : uids)
          rule.uids.push_back(UIdentFromSKDUID(uid));
        break;
      }
      }

      filterRules.push_back(std::move(rule));
      return false; // continue
    });

    if (failed)
      return CCC.createResponse();
  }
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.codeCompleteOpen(Name, InputBuf, Offset, options.get(), filterRules, CCC,
                        Args);
  return CCC.createResponse();
}

static sourcekitd_response_t codeCompleteClose(StringRef Name, int64_t Offset) {
  ResponseBuilder RespBuilder;
  SKGroupedCodeCompletionConsumer CCC(RespBuilder);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.codeCompleteClose(Name, Offset, CCC);
  return CCC.createResponse();
}

static sourcekitd_response_t
codeCompleteUpdate(StringRef name, int64_t offset,
                   Optional<RequestDict> optionsDict) {
  ResponseBuilder RespBuilder;
  SKGroupedCodeCompletionConsumer CCC(RespBuilder);
  std::unique_ptr<SKOptionsDictionary> options;
  if (optionsDict)
    options = llvm::make_unique<SKOptionsDictionary>(*optionsDict);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.codeCompleteUpdate(name, offset, options.get(), CCC);
  return CCC.createResponse();
}

void SKGroupedCodeCompletionConsumer::failed(StringRef ErrDescription) {
  ErrorDescription = ErrDescription;
}

bool SKGroupedCodeCompletionConsumer::handleResult(const CodeCompletionInfo &R) {
  assert(!GroupContentsStack.empty() && "missing root group");

  auto result = GroupContentsStack.back().appendDictionary();
  if (R.CustomKind)
    result.set(KeyKind, sourcekitd_uid_t(R.CustomKind));
  else
    result.set(KeyKind, R.Kind);
  result.set(KeyName, R.Name);
  result.set(KeyDescription, R.Description);
  result.set(KeySourceText, R.SourceText);
  result.set(KeyTypeName, R.TypeName);
  result.set(KeyContext, R.SemanticContext);
  if (!R.ModuleName.empty())
    result.set(KeyModuleName, R.ModuleName);
  if (!R.DocBrief.empty())
    result.set(KeyDocBrief, R.DocBrief);
  if (!R.AssocUSRs.empty())
    result.set(KeyAssociatedUSRs, R.AssocUSRs);
  if (R.ModuleImportDepth)
    result.set(KeyModuleImportDepth, *R.ModuleImportDepth);
  if (R.NotRecommended)
    result.set(KeyNotRecommended, R.NotRecommended);
  result.set(KeyNumBytesToErase, R.NumBytesToErase);

  if (R.descriptionStructure) {
    auto addRange = [](ResponseBuilder::Dictionary dict, UIdent offset,
                       UIdent length, CodeCompletionInfo::IndexRange range) {
      if (!range.empty()) {
        dict.set(offset, range.begin);
        dict.set(length, range.length());
      }
    };

    auto structure = result.setDictionary(KeySubStructure);
    addRange(structure, KeyNameOffset, KeyNameLength,
             R.descriptionStructure->baseName);
    addRange(structure, KeyBodyOffset, KeyBodyLength,
             R.descriptionStructure->parameterRange);
    addRange(structure, KeyThrowOffset, KeyThrowLength,
             R.descriptionStructure->throwsRange);

    if (R.parametersStructure) {
      auto params = structure.setArray(KeySubStructure);
      for (auto &P : *R.parametersStructure) {
        auto param = params.appendDictionary();
        addRange(param, KeyNameOffset, KeyNameLength, P.name);
        addRange(param, KeyBodyOffset, KeyBodyLength, P.afterColon);
        if (P.isLocalName)
          param.set(KeyIsLocal, true);
      }
    }
  }

  return true;
}

void SKGroupedCodeCompletionConsumer::startGroup(UIdent kind, StringRef name) {
  ResponseBuilder::Dictionary group;
  if (GroupContentsStack.empty()) {
    group = RespBuilder.getDictionary();
    Response = group;
  } else {
    group = GroupContentsStack.back().appendDictionary();
  }
  group.set(KeyKind, kind);
  group.set(KeyName, name);
  auto contents = group.setArray(KeyResults);
  GroupContentsStack.push_back(contents);
}
void SKGroupedCodeCompletionConsumer::endGroup() {
  assert(!GroupContentsStack.empty());
  GroupContentsStack.pop_back();
}
void SKGroupedCodeCompletionConsumer::setNextRequestStart(unsigned offset) {
  assert(!Response.isNull());
  Response.set(KeyNextRequestStart, offset);
}

//===----------------------------------------------------------------------===//
// Editor
//===----------------------------------------------------------------------===//

namespace {
class SKEditorConsumer : public EditorConsumer {
  ResponseReceiver RespReceiver;
  ResponseBuilder RespBuilder;

  ResponseBuilder::Dictionary Dict;
  DocStructureArrayBuilder DocStructure;
  TokenAnnotationsArrayBuilder SyntaxMap;
  TokenAnnotationsArrayBuilder SemanticAnnotations;

  ResponseBuilder::Array Diags;
  sourcekitd_response_t Error = nullptr;

  bool EnableSyntaxMap;
  bool EnableStructure;
  bool EnableDiagnostics;
  bool SyntacticOnly;

public:
  SKEditorConsumer(bool EnableSyntaxMap, bool EnableStructure,
                   bool EnableDiagnostics, bool SyntacticOnly)
      : EnableSyntaxMap(EnableSyntaxMap), EnableStructure(EnableStructure),
        EnableDiagnostics(EnableDiagnostics), SyntacticOnly(SyntacticOnly) {

    Dict = RespBuilder.getDictionary();
  }

  SKEditorConsumer(ResponseReceiver RespReceiver, bool EnableSyntaxMap,
                   bool EnableStructure, bool EnableDiagnostics,
                   bool SyntacticOnly)
  : SKEditorConsumer(EnableSyntaxMap, EnableStructure,
                     EnableDiagnostics, SyntacticOnly) {
    this->RespReceiver = RespReceiver;
  }

  sourcekitd_response_t createResponse();

  bool needsSemanticInfo() override {
    return !SyntacticOnly && !isSemanticEditorDisabled();
  }

  void handleRequestError(const char *Description) override;

  bool handleSyntaxMap(unsigned Offset, unsigned Length, UIdent Kind) override;

  bool handleSemanticAnnotation(unsigned Offset, unsigned Length, UIdent Kind,
                                bool isSystem) override;

  bool beginDocumentSubStructure(unsigned Offset, unsigned Length, UIdent Kind,
                                 UIdent AccessLevel,
                                 UIdent SetterAccessLevel,
                                 unsigned NameOffset,
                                 unsigned NameLength,
                                 unsigned BodyOffset,
                                 unsigned BodyLength,
                                 StringRef DisplayName,
                                 StringRef TypeName,
                                 StringRef RuntimeName,
                                 StringRef SelectorName,
                                 ArrayRef<StringRef> InheritedTypes,
                                 ArrayRef<UIdent> Attrs) override;

  bool endDocumentSubStructure() override;

  bool handleDocumentSubStructureElement(UIdent Kind,
                                         unsigned Offset,
                                         unsigned Length) override;

  bool recordAffectedRange(unsigned Offset, unsigned Length) override;

  bool recordAffectedLineRange(unsigned Line, unsigned Length) override;

  bool recordFormattedText(StringRef Text) override;

  bool setDiagnosticStage(UIdent DiagStage) override;
  bool handleDiagnostic(const DiagnosticEntryInfo &Info,
                        UIdent DiagStage) override;

  bool handleSourceText(StringRef Text) override;

  virtual void finished() override {
    if (RespReceiver)
      RespReceiver(createResponse());
  }
};

}

static sourcekitd_response_t
editorOpen(StringRef Name, llvm::MemoryBuffer *Buf, bool EnableSyntaxMap,
           bool EnableStructure, bool EnableDiagnostics, bool SyntacticOnly,
           ArrayRef<const char *> Args) {
  SKEditorConsumer EditC(EnableSyntaxMap, EnableStructure,
                         EnableDiagnostics, SyntacticOnly);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorOpen(Name, Buf, EnableSyntaxMap, EditC, Args);
  return EditC.createResponse();
}

static sourcekitd_response_t
editorOpenInterface(StringRef Name, StringRef ModuleName,
                    Optional<StringRef> Group, ArrayRef<const char *> Args,
                    bool SynthesizedExtensions,
                    Optional<StringRef> InterestedUSR) {
  SKEditorConsumer EditC(/*EnableSyntaxMap=*/true,
                         /*EnableStructure=*/true,
                         /*EnableDiagnostics=*/false,
                         /*SyntacticOnly=*/false);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorOpenInterface(EditC, Name, ModuleName, Group, Args,
                           SynthesizedExtensions, InterestedUSR);
  return EditC.createResponse();
}


/// Getting the interface from a swift source file differs from getting interfaces
/// from headers or modules for its performing asynchronously.
static void
editorOpenSwiftSourceInterface(StringRef Name, StringRef HeaderName,
                               ArrayRef<const char *> Args,
                               ResponseReceiver Rec) {
  auto EditC = std::make_shared<SKEditorConsumer>(Rec,
                                                  /*EnableSyntaxMap=*/true,
                                                  /*EnableStructure=*/true,
                                                  /*EnableDiagnostics=*/false,
                                                  /*SyntacticOnly=*/false);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorOpenSwiftSourceInterface(Name, HeaderName, Args, EditC);
}

static void
editorOpenSwiftTypeInterface(StringRef TypeUsr, ArrayRef<const char *> Args,
                             ResponseReceiver Rec) {
  auto EditC = std::make_shared<SKEditorConsumer>(Rec,
                                                  /*EnableSyntaxMap=*/true,
                                                  /*EnableStructure=*/true,
                                                  /*EnableDiagnostics=*/false,
                                                  /*SyntacticOnly=*/false);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorOpenTypeInterface(*EditC, Args, TypeUsr);
}

static sourcekitd_response_t editorExtractTextFromComment(StringRef Source) {
  SKEditorConsumer EditC(/*EnableSyntaxMap=*/false,
                         /*EnableStructure=*/false,
                         /*EnableDiagnostics=*/false,
                         /*SyntacticOnly=*/true);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorExtractTextFromComment(Source, EditC);
  return EditC.createResponse();
}

static sourcekitd_response_t
editorOpenHeaderInterface(StringRef Name, StringRef HeaderName,
                          ArrayRef<const char *> Args,
                          bool SynthesizedExtensions) {
  SKEditorConsumer EditC(/*EnableSyntaxMap=*/true,
                         /*EnableStructure=*/true,
                         /*EnableDiagnostics=*/false,
                         /*SyntacticOnly=*/false);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorOpenHeaderInterface(EditC, Name, HeaderName, Args,
                                 SynthesizedExtensions);
  return EditC.createResponse();
}

static sourcekitd_response_t
editorClose(StringRef Name, bool RemoveCache) {
  ResponseBuilder RespBuilder;
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorClose(Name, RemoveCache);
  return RespBuilder.createResponse();
}

static sourcekitd_response_t
editorReplaceText(StringRef Name, llvm::MemoryBuffer *Buf, unsigned Offset,
                  unsigned Length, bool EnableSyntaxMap, bool EnableStructure,
                  bool EnableDiagnostics, bool SyntacticOnly) {
  SKEditorConsumer EditC(EnableSyntaxMap, EnableStructure,
                         EnableDiagnostics, SyntacticOnly);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorReplaceText(Name, Buf, Offset, Length, EditC);
  return EditC.createResponse();
}

static void
editorApplyFormatOptions(StringRef Name, RequestDict &FmtOptions) {
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  SKOptionsDictionary SKFmtOptions(FmtOptions);
  Lang.editorApplyFormatOptions(Name, SKFmtOptions);
}

static sourcekitd_response_t
editorFormatText(StringRef Name, unsigned Line, unsigned Length) {
  SKEditorConsumer EditC(false, false, false,
                         /*SyntacticOnly=*/true);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorFormatText(Name, Line, Length, EditC);
  return EditC.createResponse();
}

static sourcekitd_response_t
editorExpandPlaceholder(StringRef Name, unsigned Offset, unsigned Length) {
  SKEditorConsumer EditC(false, false, false,
                         /*SyntacticOnly=*/true);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorExpandPlaceholder(Name, Offset, Length, EditC);
  return EditC.createResponse();
}

sourcekitd_response_t SKEditorConsumer::createResponse() {
  if (Error)
    return Error;

  if (EnableSyntaxMap) {
    Dict.setCustomBuffer(KeySyntaxMap,
        CustomBufferKind::TokenAnnotationsArray,
        SyntaxMap.createBuffer());
  }
  if (!SemanticAnnotations.empty()) {
    Dict.setCustomBuffer(KeyAnnotations,
        CustomBufferKind::TokenAnnotationsArray,
        SemanticAnnotations.createBuffer());
  }
  if (EnableStructure) {
    Dict.setCustomBuffer(KeySubStructure, CustomBufferKind::DocStructureArray,
                         DocStructure.createBuffer());
  }

  return RespBuilder.createResponse();
}

void SKEditorConsumer::handleRequestError(const char *Description) {
  if (!Error) {
    Error = createErrorRequestFailed(Description);
  }
  if (RespReceiver) {
    RespReceiver(Error);
    RespReceiver = ResponseReceiver();
  }
}

bool SKEditorConsumer::handleSyntaxMap(unsigned Offset, unsigned Length,
                                       UIdent Kind) {
  if (!EnableSyntaxMap)
    return true;

  SyntaxMap.add(Kind, Offset, Length, /*IsSystem=*/false);
  return true;
}

bool SKEditorConsumer::handleSemanticAnnotation(unsigned Offset,
                                                unsigned Length,
                                                UIdent Kind, bool isSystem) {
  assert(Kind.isValid());
  SemanticAnnotations.add(Kind, Offset, Length, isSystem);
  return true;
}

bool
SKEditorConsumer::beginDocumentSubStructure(unsigned Offset,
                                            unsigned Length, UIdent Kind,
                                            UIdent AccessLevel,
                                            UIdent SetterAccessLevel,
                                            unsigned NameOffset,
                                            unsigned NameLength,
                                            unsigned BodyOffset,
                                            unsigned BodyLength,
                                            StringRef DisplayName,
                                            StringRef TypeName,
                                            StringRef RuntimeName,
                                            StringRef SelectorName,
                                            ArrayRef<StringRef> InheritedTypes,
                                            ArrayRef<UIdent> Attrs) {
  if (EnableStructure) {
    DocStructure.beginSubStructure(
        Offset, Length, Kind, AccessLevel, SetterAccessLevel, NameOffset,
        NameLength, BodyOffset, BodyLength, DisplayName, TypeName, RuntimeName,
        SelectorName, InheritedTypes, Attrs);
  }
  return true;
}

bool SKEditorConsumer::endDocumentSubStructure() {
  if (EnableStructure)
    DocStructure.endSubStructure();
  return true;
}

bool SKEditorConsumer::handleDocumentSubStructureElement(UIdent Kind,
                                                         unsigned Offset,
                                                         unsigned Length) {
  if (EnableStructure)
    DocStructure.addElement(Kind, Offset, Length);
  return true;
}

bool SKEditorConsumer::recordAffectedRange(unsigned Offset, unsigned Length) {
  Dict.set(KeyOffset, Offset);
  Dict.set(KeyLength, Length);

  return true;
}

bool SKEditorConsumer::recordAffectedLineRange(unsigned Line, unsigned Length) {
  Dict.set(KeyLine, Line);
  Dict.set(KeyLength, Length);

  return true;
}

bool SKEditorConsumer::recordFormattedText(StringRef Text) {
  Dict.set(KeySourceText, Text);

  return true;
}

static void fillDictionaryForDiagnosticInfo(
    ResponseBuilder::Dictionary Elem, const DiagnosticEntryInfoBase &Info) {

  Elem.set(KeyDescription, Info.Description);
  if (Info.Line != 0) {
    Elem.set(KeyLine, Info.Line);
    Elem.set(KeyColumn, Info.Column);
  } else {
    Elem.set(KeyOffset, Info.Offset);
  }
  if (!Info.Filename.empty())
    Elem.set(KeyFilePath, Info.Filename);

  if (!Info.Ranges.empty()) {
    auto RangesArr = Elem.setArray(KeyRanges);
    for (auto R : Info.Ranges) {
      auto RangeElem = RangesArr.appendDictionary();
      RangeElem.set(KeyOffset, R.first);
      RangeElem.set(KeyLength, R.second);
    }
  }

  if (!Info.Fixits.empty()) {
    auto FixitsArr = Elem.setArray(KeyFixits);
    for (auto F : Info.Fixits) {
      auto FixitElem = FixitsArr.appendDictionary();
      FixitElem.set(KeyOffset, F.Offset);
      FixitElem.set(KeyLength, F.Length);
      FixitElem.set(KeySourceText, F.Text);
    }
  }
}

bool SKEditorConsumer::setDiagnosticStage(UIdent DiagStage) {
  Dict.set(KeyDiagnosticStage, DiagStage);
  return true;
}

bool SKEditorConsumer::handleDiagnostic(const DiagnosticEntryInfo &Info,
                                        UIdent DiagStage) {
  if (!EnableDiagnostics)
    return true;

  ResponseBuilder::Array &Arr = Diags;
  if (Arr.isNull())
    Arr = Dict.setArray(KeyDiagnostics);

  auto Elem = Arr.appendDictionary();
  UIdent SeverityUID;
  switch (Info.Severity) {
  case DiagnosticSeverityKind::Warning:
    SeverityUID = DiagKindWarning;
    break;
  case DiagnosticSeverityKind::Error:
    SeverityUID = DiagKindError;
    break;
  }

  Elem.set(KeySeverity, SeverityUID);
  Elem.set(KeyDiagnosticStage, DiagStage);
  fillDictionaryForDiagnosticInfo(Elem, Info);

  if (!Info.Notes.empty()) {
    auto NotesArr = Elem.setArray(KeyDiagnostics);
    for (auto &NoteDiag : Info.Notes) {
      auto NoteElem = NotesArr.appendDictionary();
      NoteElem.set(KeySeverity, DiagKindNote);
      fillDictionaryForDiagnosticInfo(NoteElem, NoteDiag);
    }
  }

  return true;
}

bool SKEditorConsumer::handleSourceText(StringRef Text) {
  Dict.set(KeySourceText, Text);
  return true;
}

static sourcekitd_response_t
editorFindUSR(StringRef DocumentName, StringRef USR) {
  ResponseBuilder RespBuilder;
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  llvm::Optional<std::pair<unsigned, unsigned>>
      Range = Lang.findUSRRange(DocumentName, USR);
  if (!Range) {
    // If cannot find the synthesized USR, find the actual USR instead.
    Range = Lang.findUSRRange(DocumentName,
                              USR.split(LangSupport::SynthesizedUSRSeparator).
                                first);
  }
  if (Range.hasValue()) {
    RespBuilder.getDictionary().set(KeyOffset, Range->first);
    RespBuilder.getDictionary().set(KeyLength, Range->second);
  }
  return RespBuilder.createResponse();
}

static sourcekitd_response_t
editorFindInterfaceDoc(StringRef ModuleName, ArrayRef<const char *> Args) {
  ResponseBuilder RespBuilder;
  sourcekitd_response_t Resp;
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.findInterfaceDocument(ModuleName, Args,
    [&](const InterfaceDocInfo &Info) {
      if (!Info.Error.empty()) {
        SmallString<128> Err(Info.Error);
        Resp = createErrorRequestFailed(Err.c_str());
        return;
      }

      auto Elem = RespBuilder.getDictionary();
      if (!Info.ModuleInterfaceName.empty())
        Elem.set(KeyModuleInterfaceName, Info.ModuleInterfaceName);
      if (!Info.CompilerArgs.empty())
        Elem.set(KeyCompilerArgs, Info.CompilerArgs);
      Resp = RespBuilder.createResponse();
    });

  return Resp;
}

static sourcekitd_response_t
editorFindModuleGroups(StringRef ModuleName, ArrayRef<const char *> Args) {
  ResponseBuilder RespBuilder;
  sourcekitd_response_t Resp;
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.findModuleGroups(ModuleName, Args,
                        [&](ArrayRef<StringRef> Groups, StringRef Error) {
    if (!Error.empty()) {
      Resp = createErrorRequestFailed(Error.str().c_str());
      return;
    }
    auto Dict = RespBuilder.getDictionary();
    auto Arr = Dict.setArray(KeyModuleGroups);
    for (auto G : Groups) {
      auto Entry = Arr.appendDictionary();
      Entry.set(KeyGroupName, G);
    }
    Resp = RespBuilder.createResponse();
  });
  return Resp;
}

static bool isSemanticEditorDisabled() {
  enum class SemaInfoToggle : char {
    None, Disable, Enable
  };
  static SemaInfoToggle Toggle = SemaInfoToggle::None;

  if (Toggle == SemaInfoToggle::None) {
    static std::once_flag flag;
    std::call_once(flag, []() {
      Toggle = SemaInfoToggle::Enable;

      const char *EnvOpt = ::getenv("SOURCEKIT_DELAY_SEMA_EDITOR");
      if (!EnvOpt) {
        return;
      }

      unsigned Seconds;
      if (StringRef(EnvOpt).getAsInteger(10, Seconds))
        return;

      // A crash occurred previously. Disable semantic info in the editor for
      // the given amount, to avoid repeated crashers.
      LOG_WARN_FUNC("delaying semantic editor for " << Seconds << " seconds");
      Toggle = SemaInfoToggle::Disable;
      dispatch_time_t When = dispatch_time(DISPATCH_TIME_NOW,
                                           NSEC_PER_SEC * Seconds);
      dispatch_after(When, dispatch_get_main_queue(), ^{
        Toggle = SemaInfoToggle::Enable;
      });
    });
  }

  assert(Toggle != SemaInfoToggle::None);
  return Toggle == SemaInfoToggle::Disable;
}
