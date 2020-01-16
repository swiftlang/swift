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
#include "sourcekitd/ExpressionTypeArray.h"

#include "SourceKit/Core/Context.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Core/NotificationCenter.h"
#include "SourceKit/Support/Concurrency.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/Statistic.h"
#include "SourceKit/Support/Tracing.h"
#include "SourceKit/Support/UIdent.h"
#include "SourceKit/SwiftLang/Factory.h"

#include "swift/Basic/ExponentialGrowthAppendingBinaryByteStream.h"
#include "swift/Basic/Mangler.h"
#include "swift/Basic/Version.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/Syntax/Serialization/SyntaxSerialization.h"
#include "swift/Syntax/SyntaxNodes.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/BinaryByteStream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/NativeFormatting.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/VirtualFileSystem.h"
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
    sourcekitd_uid_t UIDValue = UID;
    if (!UIDValue)
      UID = SKDUIDFromUIdent(UIdent(Name));
    return UID;
  }

  operator sourcekitd_uid_t() const {
    return get();
  }

  StringRef str() const {
    return StringRef(Name);
  }
};

struct SKEditorConsumerOptions {
  bool EnableSyntaxMap = false;
  bool EnableStructure = false;
  bool EnableDiagnostics = false;
  SyntaxTreeTransferMode SyntaxTransferMode = SyntaxTreeTransferMode::Off;
  SyntaxTreeSerializationFormat SyntaxSerializationFormat =
      SyntaxTreeSerializationFormat::JSON;
  bool SyntacticOnly = false;
};

} // anonymous namespace

#define REQUEST(NAME, CONTENT) static LazySKDUID Request##NAME(CONTENT);
#define KIND(NAME, CONTENT) static LazySKDUID Kind##NAME(CONTENT);
#include "SourceKit/Core/ProtocolUIDs.def"

#define REFACTORING(KIND, NAME, ID) static LazySKDUID Kind##Refactoring##KIND("source.refactoring.kind."#ID);
#include "swift/IDE/RefactoringKinds.def"

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
  llvm::EnablePrettyStackTrace();
  GlobalCtx = new SourceKit::Context(sourcekitd::getRuntimeLibPath(),
                                     SourceKit::createSwiftLangSupport);
  GlobalCtx->getNotificationCenter()->addDocumentUpdateNotificationReceiver(
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
                                         ArrayRef<const char *> Args);

static sourcekitd_response_t reportDocInfo(llvm::MemoryBuffer *InputBuf,
                                           StringRef ModuleName,
                                           ArrayRef<const char *> Args);

static void reportCursorInfo(const RequestResult<CursorInfoData> &Result, ResponseReceiver Rec);

static void reportExpressionTypeInfo(const RequestResult<ExpressionTypesInFile> &Result,
                                     ResponseReceiver Rec);

static void reportRangeInfo(const RequestResult<RangeInfo> &Result, ResponseReceiver Rec);

static void reportNameInfo(const RequestResult<NameTranslatingInfo> &Result, ResponseReceiver Rec);

static void findRelatedIdents(StringRef Filename,
                              int64_t Offset,
                              bool CancelOnSubsequentRequest,
                              ArrayRef<const char *> Args,
                              ResponseReceiver Rec);

static sourcekitd_response_t
codeComplete(llvm::MemoryBuffer *InputBuf, int64_t Offset,
             Optional<RequestDict> optionsDict,
             ArrayRef<const char *> Args, Optional<VFSOptions> vfsOptions);

static sourcekitd_response_t codeCompleteOpen(StringRef name,
                                              llvm::MemoryBuffer *InputBuf,
                                              int64_t Offset,
                                              Optional<RequestDict> optionsDict,
                                              ArrayRef<const char *> Args,
                                              Optional<VFSOptions> vfsOptions);

static sourcekitd_response_t
codeCompleteUpdate(StringRef name, int64_t Offset,
                   Optional<RequestDict> optionsDict);

static sourcekitd_response_t codeCompleteClose(StringRef name, int64_t Offset);

static sourcekitd_response_t typeContextInfo(llvm::MemoryBuffer *InputBuf,
                                             int64_t Offset,
                                             ArrayRef<const char *> Args,
                                             Optional<VFSOptions> vfsOptions);

static sourcekitd_response_t
conformingMethodList(llvm::MemoryBuffer *InputBuf, int64_t Offset,
                     ArrayRef<const char *> Args,
                     ArrayRef<const char *> ExpectedTypes,
                     Optional<VFSOptions> vfsOptions);

static sourcekitd_response_t
editorOpen(StringRef Name, llvm::MemoryBuffer *Buf,
           SKEditorConsumerOptions Opts, ArrayRef<const char *> Args,
           Optional<VFSOptions> vfsOptions);

static sourcekitd_response_t
editorOpenInterface(StringRef Name, StringRef ModuleName,
                    Optional<StringRef> Group, ArrayRef<const char *> Args,
                    bool SynthesizedExtensions,
                    Optional<StringRef> InterestedUSR);

static sourcekitd_response_t
editorOpenHeaderInterface(StringRef Name, StringRef HeaderName,
                          ArrayRef<const char *> Args,
                          bool UsingSwiftArgs,
                          bool SynthesizedExtensions,
                          StringRef swiftVersion);

static void
editorOpenSwiftSourceInterface(StringRef Name, StringRef SourceName,
                               ArrayRef<const char *> Args,
                               ResponseReceiver Rec);

static void
editorOpenSwiftTypeInterface(StringRef TypeUsr, ArrayRef<const char *> Args,
                             ResponseReceiver Rec);

static sourcekitd_response_t editorExtractTextFromComment(StringRef Source);

static sourcekitd_response_t editorConvertMarkupToXML(StringRef Source);

static sourcekitd_response_t
editorClose(StringRef Name, bool RemoveCache);

static sourcekitd_response_t
editorReplaceText(StringRef Name, llvm::MemoryBuffer *Buf, unsigned Offset,
                  unsigned Length, SKEditorConsumerOptions Opts);

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

static bool
buildRenameLocationsFromDict(RequestDict &Req, bool UseNewName,
                             std::vector<RenameLocations> &RenameLocations,
                             llvm::SmallString<64> &Error);

static sourcekitd_response_t
createCategorizedEditsResponse(
    const RequestResult<ArrayRef<CategorizedEdits>> &Result);

static sourcekitd_response_t
syntacticRename(llvm::MemoryBuffer *InputBuf,
                ArrayRef<RenameLocations> RenameLocations,
                ArrayRef<const char*> Args);

static sourcekitd_response_t
createCategorizedRenameRangesResponse(
    const RequestResult<ArrayRef<CategorizedRenameRanges>> &Result);

static sourcekitd_response_t
findRenameRanges(llvm::MemoryBuffer *InputBuf,
                 ArrayRef<RenameLocations> RenameLocations,
                 ArrayRef<const char *> Args);

static bool isSemanticEditorDisabled();

static void fillDictionaryForDiagnosticInfo(
    ResponseBuilder::Dictionary Elem, const DiagnosticEntryInfo &Info);

static void enableCompileNotifications(bool value);

static SyntaxTreeTransferMode syntaxTransferModeFromUID(sourcekitd_uid_t UID) {
  if (UID == nullptr) {
    // Default is no syntax tree
    return SyntaxTreeTransferMode::Off;
  } else if (UID == KindSyntaxTreeOff) {
    return SyntaxTreeTransferMode::Off;
  } else if (UID == KindSyntaxTreeIncremental) {
    return SyntaxTreeTransferMode::Incremental;
  } else if (UID == KindSyntaxTreeFull) {
    return SyntaxTreeTransferMode::Full;
  } else {
    llvm_unreachable("Unexpected syntax tree transfer mode");
  }
}

static llvm::Optional<SyntaxTreeSerializationFormat>
syntaxSerializationFormatFromUID(sourcekitd_uid_t UID) {
  if (UID == nullptr) {
    // Default is JSON
    return SyntaxTreeSerializationFormat::JSON;
  } else if (UID == KindSyntaxTreeSerializationJSON) {
    return SyntaxTreeSerializationFormat::JSON;
  } else if (UID == KindSyntaxTreeSerializationByteTree) {
    return SyntaxTreeSerializationFormat::ByteTree;
  } else {
    return llvm::None;
  }
}

namespace {
class SKOptionsDictionary : public OptionsDictionary {
  RequestDict Options;

public:
  explicit SKOptionsDictionary(RequestDict Options) : Options(Options) {}

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

  bool forEach(UIdent key, llvm::function_ref<bool(OptionsDictionary &)> applier) override {
    return Options.dictionaryArrayApply(key, [=](RequestDict dict) {
      SKOptionsDictionary skDict(dict);
      return applier(skDict);
    });
  }
};
} // anonymous namespace

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

static std::unique_ptr<llvm::MemoryBuffer> getInputBufForRequest(
    Optional<StringRef> SourceFile, Optional<StringRef> SourceText,
    const Optional<VFSOptions> &vfsOptions, llvm::SmallString<64> &ErrBuf) {
  std::unique_ptr<llvm::MemoryBuffer> InputBuf;

  if (SourceText.hasValue()) {
    StringRef BufName;
    if (SourceFile.hasValue())
      BufName = *SourceFile;
    else
      BufName = "<input>";
    InputBuf = llvm::MemoryBuffer::getMemBuffer(*SourceText, BufName);

  } else if (vfsOptions.hasValue() && SourceFile.hasValue()) {
    ErrBuf = "using 'key.sourcefile' to read source text from the filesystem "
             "is not supported when using 'key.vfs.name'";
    return nullptr;

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
    InputBuf = llvm::WritableMemoryBuffer::getNewMemBuffer(0, "<input>");
  }

  return InputBuf;
}

/// Read optional VFSOptions from a request dictionary. The request dictionary
/// *must* outlive the resulting VFSOptions.
/// \returns true on failure and sets \p error.
static Optional<VFSOptions> getVFSOptions(RequestDict &Req) {
  auto name = Req.getString(KeyVFSName);
  if (!name)
    return None;

  std::unique_ptr<OptionsDictionary> options;
  if (auto dict = Req.getDictionary(KeyVFSOptions)) {
    options = llvm::make_unique<SKOptionsDictionary>(*dict);
  }

  return VFSOptions{name->str(), std::move(options)};
}

static void handleSemanticRequest(
    RequestDict Req, ResponseReceiver Receiver, sourcekitd_uid_t ReqUID,
    Optional<StringRef> SourceFile, Optional<StringRef> SourceText,
    ArrayRef<const char *> Args, Optional<VFSOptions> vfsOptions);

void handleRequestImpl(sourcekitd_object_t ReqObj, ResponseReceiver Rec) {
  // NOTE: if we had a connection context, these stats should move into it.
  static Statistic numRequests(UIdentFromSKDUID(KindStatNumRequests), "# of requests (total)");
  static Statistic numSemaRequests(UIdentFromSKDUID(KindStatNumSemaRequests), "# of semantic requests");
  ++numRequests;

  RequestDict Req(ReqObj);
  sourcekitd_uid_t ReqUID = Req.getUID(KeyRequest);
  if (!ReqUID)
    return Rec(createErrorRequestInvalid("missing 'key.request' with UID value"));

  if (ReqUID == RequestGlobalConfiguration) {
    auto Config = getGlobalContext().getGlobalConfiguration();
    ResponseBuilder RB;
    auto dict = RB.getDictionary();

    Optional<bool> OptimizeForIDE;
    int64_t EditorMode = true;
    if (!Req.getInt64(KeyOptimizeForIDE, EditorMode, true)) {
      OptimizeForIDE = EditorMode;
    }

    GlobalConfig::Settings UpdatedConfig = Config->update(OptimizeForIDE);
    dict.set(KeyOptimizeForIDE, UpdatedConfig.OptimizeForIDE);
    return Rec(RB.createResponse());
  }
  if (ReqUID == RequestProtocolVersion) {
    ResponseBuilder RB;
    auto dict = RB.getDictionary();
    dict.set(KeyVersionMajor, ProtocolMajorVersion);
    dict.set(KeyVersionMinor, static_cast<int64_t>(ProtocolMinorVersion));
    return Rec(RB.createResponse());
  }

  if (ReqUID == RequestCompilerVersion) {
    ResponseBuilder RB;
    auto dict = RB.getDictionary();
    auto thisVersion = swift::version::Version::getCurrentLanguageVersion();
    dict.set(KeyVersionMajor, static_cast<int64_t>(thisVersion[0]));
    dict.set(KeyVersionMinor, static_cast<int64_t>(thisVersion[1]));
    if (thisVersion.size() > 2)
      dict.set(KeyVersionPatch, static_cast<int64_t>(thisVersion[2]));
    else
      dict.set(KeyVersionPatch, static_cast<int64_t>(0));
    return Rec(RB.createResponse());
  }

  if (ReqUID == RequestCrashWithExit) {
    // 'exit' has the same effect as crashing but without the crash log.
    ::exit(1);
  }

  if (ReqUID == RequestTestNotification) {
    static UIdent TestNotification("source.notification.test");
    ResponseBuilder RespBuilder;
    auto Dict = RespBuilder.getDictionary();
    Dict.set(KeyNotification, TestNotification);
    sourcekitd::postNotification(RespBuilder.createResponse());
    return Rec(ResponseBuilder().createResponse());
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

  if (ReqUID == RequestEnableCompileNotifications) {
    int64_t value = true;
    if (Req.getInt64(KeyValue, value, /*isOptional=*/false)) {
      return Rec(createErrorRequestInvalid("missing 'key.value'"));
    }
    enableCompileNotifications(value);
    return Rec(ResponseBuilder().createResponse());
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

  Optional<VFSOptions> vfsOptions;

  if (Optional<StringRef> VFSName = Req.getString(KeyVFSName)) {
    if (ReqUID != RequestEditorOpen && ReqUID != RequestCodeComplete &&
        ReqUID != RequestCodeCompleteOpen && ReqUID != RequestCursorInfo) {
      return Rec(createErrorRequestInvalid(
          "This request does not support custom filesystems"));
    }

    vfsOptions = getVFSOptions(Req);
  }

  SmallVector<const char *, 8> Args;
  bool Failed = Req.getStringArray(KeyCompilerArgs, Args, /*isOptional=*/true);
  if (Failed) {
    return Rec(createErrorRequestInvalid(
        "'key.compilerargs' not an array of strings"));
  }

  if (ReqUID == RequestDocInfo) {
    std::unique_ptr<llvm::MemoryBuffer> InputBuf = getInputBufForRequest(
        SourceFile, SourceText, vfsOptions, ErrBuf);
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
    std::unique_ptr<llvm::MemoryBuffer> InputBuf =
        getInputBufForRequest(SourceFile, SourceText, vfsOptions, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    int64_t EnableSyntaxMap = true;
    Req.getInt64(KeyEnableSyntaxMap, EnableSyntaxMap, /*isOptional=*/true);
    int64_t EnableStructure = true;
    Req.getInt64(KeyEnableStructure, EnableStructure, /*isOptional=*/true);
    int64_t EnableDiagnostics = true;
    Req.getInt64(KeyEnableDiagnostics, EnableDiagnostics, /*isOptional=*/true);
    auto TransferModeUID = Req.getUID(KeySyntaxTreeTransferMode);
    auto SerializationFormatUID = Req.getUID(KeySyntaxTreeSerializationFormat);
    int64_t SyntacticOnly = false;
    Req.getInt64(KeySyntacticOnly, SyntacticOnly, /*isOptional=*/true);

    SKEditorConsumerOptions Opts;
    Opts.EnableSyntaxMap = EnableSyntaxMap;
    Opts.EnableStructure = EnableStructure;
    Opts.EnableDiagnostics = EnableDiagnostics;
    Opts.SyntaxTransferMode = syntaxTransferModeFromUID(TransferModeUID);
    auto SyntaxSerializationFormat =
        syntaxSerializationFormatFromUID(SerializationFormatUID);
    if (!SyntaxSerializationFormat)
      return Rec(createErrorRequestFailed("Invalid serialization format"));
    Opts.SyntaxSerializationFormat = SyntaxSerializationFormat.getValue();
    Opts.SyntacticOnly = SyntacticOnly;
    return Rec(editorOpen(*Name, InputBuf.get(), Opts, Args, std::move(vfsOptions)));
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
    std::unique_ptr<llvm::MemoryBuffer> InputBuf = getInputBufForRequest(
        SourceFile, SourceText, vfsOptions, ErrBuf);
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
    auto TransferModeUID = Req.getUID(KeySyntaxTreeTransferMode);
    auto SerializationFormatUID = Req.getUID(KeySyntaxTreeSerializationFormat);

    SKEditorConsumerOptions Opts;
    Opts.EnableSyntaxMap = EnableSyntaxMap;
    Opts.EnableStructure = EnableStructure;
    Opts.EnableDiagnostics = EnableDiagnostics;
    Opts.SyntaxTransferMode = syntaxTransferModeFromUID(TransferModeUID);
    auto SyntaxSerializationFormat =
        syntaxSerializationFormatFromUID(SerializationFormatUID);
    if (!SyntaxSerializationFormat)
      return Rec(createErrorRequestFailed("Invalid serialization format"));
    Opts.SyntaxSerializationFormat = SyntaxSerializationFormat.getValue();
    Opts.SyntacticOnly = SyntacticOnly;

    return Rec(editorReplaceText(*Name, InputBuf.get(), Offset, Length, Opts));
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
    Optional<int64_t> UsingSwiftArgs = Req.getOptionalInt64(KeyUsingSwiftArgs);
    std::string swiftVer;
    Optional<StringRef> swiftVerValStr = Req.getString(KeySwiftVersion);
    if (swiftVerValStr.hasValue()) {
      swiftVer = swiftVerValStr.getValue();
    } else {
      Optional<int64_t> swiftVerVal = Req.getOptionalInt64(KeySwiftVersion);
      if (swiftVerVal.hasValue())
        swiftVer = std::to_string(*swiftVerVal);
    }
    return Rec(editorOpenHeaderInterface(*Name, *HeaderName, Args,
                                         UsingSwiftArgs.getValueOr(false),
                                         SynthesizedExtension, swiftVer));
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

  if (ReqUID == RequestMarkupToXML) {
    Optional<StringRef> Source = Req.getString(KeySourceText);
    if (!Source.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.sourcetext'"));
    return Rec(editorConvertMarkupToXML(Source.getValue()));
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

  if (ReqUID == RequestSyntacticRename) {
    std::unique_ptr<llvm::MemoryBuffer> InputBuf = getInputBufForRequest(
        SourceFile, SourceText, vfsOptions, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));

    std::vector<RenameLocations> RenameLocations;
    if (buildRenameLocationsFromDict(Req, true, RenameLocations, ErrBuf))
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    return Rec(syntacticRename(InputBuf.get(), RenameLocations, Args));
  }

  if (ReqUID == RequestFindRenameRanges) {
    std::unique_ptr<llvm::MemoryBuffer> InputBuf = getInputBufForRequest(
        SourceFile, SourceText, vfsOptions, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));

    std::vector<RenameLocations> RenameLocations;
    if (buildRenameLocationsFromDict(Req, false, RenameLocations, ErrBuf))
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    return Rec(findRenameRanges(InputBuf.get(), RenameLocations, Args));
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
        } else if (context == KindForEachSequence) {
          CCInfo.Contexts |= CustomCompletionInfo::ForEachSequence;
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

  if (ReqUID == RequestStatistics) {
    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
    Lang.getStatistics([Rec](ArrayRef<Statistic *> stats) {
      ResponseBuilder builder;
      auto results = builder.getDictionary().setArray(KeyResults);
      auto addStat = [&results](Statistic *stat) {
        auto dict = results.appendDictionary();
        dict.set(KeyKind, stat->name);
        dict.set(KeyDescription, stat->description);
        dict.set(KeyValue, stat->value);
      };

      addStat(&numRequests);
      addStat(&numSemaRequests);
      std::for_each(stats.begin(), stats.end(), addStat);

      Rec(builder.createResponse());
    });
    return;
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
  ++numSemaRequests;
  SemaQueue.dispatch(
      [ReqObj, Rec, ReqUID, SourceFile, SourceText, Args] {
        RequestDict Req(ReqObj);
        auto vfsOptions = getVFSOptions(Req);
        handleSemanticRequest(Req, Rec, ReqUID, SourceFile, SourceText, Args,
                              std::move(vfsOptions));
        sourcekitd_request_release(ReqObj);
      },
      /*isStackDeep=*/true);
}

static void handleSemanticRequest(
    RequestDict Req, ResponseReceiver Rec, sourcekitd_uid_t ReqUID,
    Optional<StringRef> SourceFile, Optional<StringRef> SourceText,
    ArrayRef<const char *> Args, Optional<VFSOptions> vfsOptions) {
  llvm::SmallString<64> ErrBuf;

  if (isSemanticEditorDisabled())
      return Rec(createErrorRequestFailed("semantic editor is disabled"));

  if (ReqUID == RequestCodeComplete) {
    std::unique_ptr<llvm::MemoryBuffer> InputBuf =
        getInputBufForRequest(SourceFile, SourceText, vfsOptions, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("missing 'key.offset'"));
    Optional<RequestDict> options = Req.getDictionary(KeyCodeCompleteOptions);
    return Rec(codeComplete(InputBuf.get(), Offset, options, Args,
                            std::move(vfsOptions)));
  }

  if (ReqUID == RequestCodeCompleteOpen) {
    std::unique_ptr<llvm::MemoryBuffer> InputBuf = getInputBufForRequest(
        SourceFile, SourceText, vfsOptions, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    Optional<StringRef> Name = Req.getString(KeyName);
    if (!Name.hasValue())
      return Rec(createErrorRequestInvalid("missing 'key.name'"));
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("missing 'key.offset'"));
    Optional<RequestDict> options = Req.getDictionary(KeyCodeCompleteOptions);
    return Rec(codeCompleteOpen(*Name, InputBuf.get(), Offset, options, Args,
                                std::move(vfsOptions)));
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

  if (ReqUID == RequestTypeContextInfo) {
    std::unique_ptr<llvm::MemoryBuffer> InputBuf = getInputBufForRequest(
        SourceFile, SourceText, vfsOptions, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("missing 'key.offset'"));
    return Rec(typeContextInfo(InputBuf.get(), Offset, Args,
                               std::move(vfsOptions)));
  }

  if (ReqUID == RequestConformingMethodList) {
    std::unique_ptr<llvm::MemoryBuffer> InputBuf = getInputBufForRequest(
        SourceFile, SourceText, vfsOptions, ErrBuf);
    if (!InputBuf)
      return Rec(createErrorRequestFailed(ErrBuf.c_str()));
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("missing 'key.offset'"));
    SmallVector<const char *, 8> ExpectedTypeNames;
    if (Req.getStringArray(KeyExpectedTypes, ExpectedTypeNames, true))
      return Rec(createErrorRequestInvalid("invalid 'key.expectedtypes'"));
    return Rec(
        conformingMethodList(InputBuf.get(), Offset, Args, ExpectedTypeNames,
                             std::move(vfsOptions)));
  }

  if (!SourceFile.hasValue())
    return Rec(createErrorRequestInvalid("missing 'key.sourcefile'"));

  if (ReqUID == RequestIndex) {
    return Rec(indexSource(*SourceFile, Args));
  }

  if (ReqUID == RequestCursorInfo) {
    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();

    // For backwards compatibility, the default is 1.
    int64_t CancelOnSubsequentRequest = 1;
    Req.getInt64(KeyCancelOnSubsequentRequest, CancelOnSubsequentRequest,
                 /*isOptional=*/true);

    int64_t Offset;
    if (!Req.getInt64(KeyOffset, Offset, /*isOptional=*/false)) {
      int64_t Length = 0;
      Req.getInt64(KeyLength, Length, /*isOptional=*/true);
      int64_t Actionables = false;
      Req.getInt64(KeyRetrieveRefactorActions, Actionables, /*isOptional=*/true);
      return Lang.getCursorInfo(
          *SourceFile, Offset, Length, Actionables, CancelOnSubsequentRequest,
          Args, std::move(vfsOptions),
          [Rec](const RequestResult<CursorInfoData> &Result) {
            reportCursorInfo(Result, Rec);
          });
    }
    if (auto USR = Req.getString(KeyUSR)) {
      return Lang.getCursorInfoFromUSR(
          *SourceFile, *USR, CancelOnSubsequentRequest, Args, std::move(vfsOptions),
          [Rec](const RequestResult<CursorInfoData> &Result) {
            reportCursorInfo(Result, Rec);
          });
    }

    return Rec(createErrorRequestInvalid(
        "either 'key.offset' or 'key.usr' is required"));
  }

  if (ReqUID == RequestRangeInfo) {
    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
    int64_t Offset;
    int64_t Length;
    // For backwards compatibility, the default is 1.
    int64_t CancelOnSubsequentRequest = 1;
    Req.getInt64(KeyCancelOnSubsequentRequest, CancelOnSubsequentRequest,
                 /*isOptional=*/true);
    if (!Req.getInt64(KeyOffset, Offset, /*isOptional=*/false)) {
      if (!Req.getInt64(KeyLength, Length, /*isOptional=*/false)) {
        return Lang.getRangeInfo(*SourceFile, Offset, Length,
                                 CancelOnSubsequentRequest, Args,
          [Rec](const RequestResult<RangeInfo> &Result) {
            reportRangeInfo(Result, Rec);
          });
      }
    }

    return Rec(createErrorRequestInvalid(
      "'key.offset' or 'key.length' is required"));
  }

  if (ReqUID == RequestSemanticRefactoring) {
    int64_t Line = 0;
    int64_t Column = 0;
    int64_t Length = 0;
    auto KA = Req.getUID(KeyActionUID);
    if (!KA) {
      return Rec(createErrorRequestInvalid("'key.actionuid' is required"));
    }
    SemanticRefactoringInfo Info;
    Info.Kind = SemanticRefactoringKind::None;

#define SEMANTIC_REFACTORING(KIND, NAME, ID)                                   \
  if (KA == KindRefactoring##KIND) Info.Kind = SemanticRefactoringKind::KIND;
#include "swift/IDE/RefactoringKinds.def"

    if (Info.Kind == SemanticRefactoringKind::None)
      return Rec(createErrorRequestInvalid("'key.actionuid' isn't recognized"));

    if (!Req.getInt64(KeyLine, Line, /*isOptional=*/false)) {
      if (!Req.getInt64(KeyColumn, Column, /*isOptional=*/false)) {
        Req.getInt64(KeyLength, Length, /*isOptional=*/true);
        if (auto N = Req.getString(KeyName))
          Info.PreferredName = *N;
        LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
        Info.Line = Line;
        Info.Column = Column;
        Info.Length = Length;
        return Lang.semanticRefactoring(*SourceFile, Info, Args,
          [Rec](const RequestResult<ArrayRef<CategorizedEdits>> &Result) {
            Rec(createCategorizedEditsResponse(Result));
        });
      }
    }
    return Rec(createErrorRequestInvalid("'key.line' or 'key.column' are required"));
  }

  if (ReqUID == RequestCollectExpressionType) {
    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();

    SmallVector<const char *, 8> ExpectedProtocols;
    if (Req.getStringArray(KeyExpectedTypes, ExpectedProtocols, true))
      return Rec(createErrorRequestInvalid("invalid 'key.interested_protocols'"));
    int64_t CanonicalTy = false;
    Req.getInt64(KeyCanonicalizeType, CanonicalTy, /*isOptional=*/true);
    return Lang.collectExpressionTypes(*SourceFile, Args, ExpectedProtocols,
                                       CanonicalTy,
      [Rec](const RequestResult<ExpressionTypesInFile> &Result) {
        reportExpressionTypeInfo(Result, Rec);
      });
  }

  if (ReqUID == RequestFindLocalRenameRanges) {
    int64_t Line = 0, Column = 0, Length = 0;
    if (Req.getInt64(KeyLine, Line, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("'key.line' is required"));
    if (Req.getInt64(KeyColumn, Column, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("'key.column' is required"));
    Req.getInt64(KeyLength, Length, /*isOptional=*/true);

    LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
    return Lang.findLocalRenameRanges(
        *SourceFile, Line, Column, Length, Args,
        [Rec](const RequestResult<ArrayRef<CategorizedRenameRanges>> &Result) {
          Rec(createCategorizedRenameRangesResponse(Result));
        });
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

    static UIdent UIDKindNameSwift(KindNameSwift.str());
    static UIdent UIDKindNameObjc(KindNameObjc.str());

    if (NK == KindNameSwift.get())
      Input.NameKind = UIDKindNameSwift;
    else if (NK == KindNameObjc.get())
      Input.NameKind = UIDKindNameObjc;
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
    std::transform(ArgParts.begin(), ArgParts.end(),
                   std::back_inserter(Input.ArgNames),
                   [](const char *C) { return StringRef(C); });
    std::transform(Selectors.begin(), Selectors.end(),
                   std::back_inserter(Input.ArgNames),
                   [](const char *C) { return StringRef(C); });
    return Lang.getNameInfo(*SourceFile, Offset, Input, Args,
      [Rec](const RequestResult<NameTranslatingInfo> &Result) {
        reportNameInfo(Result, Rec);
      });
  }

  if (ReqUID == RequestRelatedIdents) {
    int64_t Offset;
    if (Req.getInt64(KeyOffset, Offset, /*isOptional=*/false))
      return Rec(createErrorRequestInvalid("missing 'key.offset'"));

    // For backwards compatibility, the default is 1.
    int64_t CancelOnSubsequentRequest = 1;
    Req.getInt64(KeyCancelOnSubsequentRequest, CancelOnSubsequentRequest,
                 /*isOptional=*/true);

    return findRelatedIdents(*SourceFile, Offset, CancelOnSubsequentRequest,
                             Args, Rec);
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
  ~SKIndexingConsumer() override {
    assert(Cancelled ||
           (EntitiesStack.size() == 1 && DependenciesStack.size() == 1));
    (void) Cancelled;
  }

  void failed(StringRef ErrDescription) override;

  bool startDependency(UIdent Kind,
                       StringRef Name,
                       StringRef Path,
                       bool IsSystem) override;

  bool finishDependency(UIdent Kind) override;

  bool startSourceEntity(const EntityInfo &Info) override;

  bool recordRelatedEntity(const EntityInfo &Info) override;

  bool finishSourceEntity(UIdent Kind) override;
};
} // end anonymous namespace

static sourcekitd_response_t indexSource(StringRef Filename,
                                         ArrayRef<const char *> Args) {
  ResponseBuilder RespBuilder;
  SKIndexingConsumer IdxConsumer(RespBuilder);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.indexSource(Filename, IdxConsumer, Args);

  if (!IdxConsumer.ErrorDescription.empty())
    return createErrorRequestFailed(IdxConsumer.ErrorDescription.c_str());

  return RespBuilder.createResponse();
}

void SKIndexingConsumer::failed(StringRef ErrDescription) {
  ErrorDescription = ErrDescription;
}

bool SKIndexingConsumer::startDependency(UIdent Kind,
                                         StringRef Name,
                                         StringRef Path,
                                         bool IsSystem) {
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
  if (Info.IsImplicit)
    Elem.setBool(KeyIsImplicit, true);
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
  ~SKDocConsumer() override {
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
} // end anonymous namespace

static sourcekitd_response_t demangleNames(ArrayRef<const char *> MangledNames,
                                           bool Simplified) {
  swift::Demangle::DemangleOptions DemangleOptions;
  if (Simplified) {
    DemangleOptions =
      swift::Demangle::DemangleOptions::SimplifiedUIDemangleOptions();
  }

  auto getDemangledName = [&](StringRef MangledName) -> std::string {
    if (!swift::Demangle::isSwiftSymbol(MangledName))
      return std::string(); // Not a mangled name

    std::string Result = swift::Demangle::demangleSymbolAsString(
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
  Demangler Dem;
  auto moduleNode = Dem.createNode(Node::Kind::Module, moduleName);
  auto IdNode = Dem.createNode(Node::Kind::Identifier, className);
  auto classNode = Dem.createNode(Node::Kind::Class);
  auto typeNode = Dem.createNode(Node::Kind::Type);
  auto typeManglingNode = Dem.createNode(Node::Kind::TypeMangling);
  auto globalNode = Dem.createNode(Node::Kind::Global);

  classNode->addChild(moduleNode, Dem);
  classNode->addChild(IdNode, Dem);
  typeNode->addChild(classNode, Dem);
  typeManglingNode->addChild(typeNode, Dem);
  globalNode->addChild(typeManglingNode, Dem);
  return mangleNode(globalNode);
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
  if (!Info.SubModuleName.empty())
    Elem.set(KeyModuleName, Info.SubModuleName);
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
  if (!Info.FullyAnnotatedGenericSig.empty())
    Elem.set(KeyFullyAnnotatedGenericSignature, Info.FullyAnnotatedGenericSig);
  if (!Info.LocalizationKey.empty())
    Elem.set(KeyLocalizationKey, Info.LocalizationKey);

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
  fillDictionaryForDiagnosticInfo(Elem, Info);
  return true;
}

//===----------------------------------------------------------------------===//
// ReportCursorInfo
//===----------------------------------------------------------------------===//

static void reportCursorInfo(const RequestResult<CursorInfoData> &Result,
                             ResponseReceiver Rec) {
  if (Result.isCancelled())
    return Rec(createErrorRequestCancelled());
  if (Result.isError())
    return Rec(createErrorRequestFailed(Result.getError()));

  const CursorInfoData &Info = Result.value();

  ResponseBuilder RespBuilder;
  if (!Info.InternalDiagnostic.empty()) {
    auto Elem = RespBuilder.getDictionary();
    Elem.set(KeyInternalDiagnostic, Info.InternalDiagnostic);
    return Rec(RespBuilder.createResponse());
  }
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
  if (!Info.LocalizationKey.empty())
    Elem.set(KeyLocalizationKey, Info.LocalizationKey);
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
    auto Actions = Elem.setArray(KeyRefactorActions);
    for (auto Info : Info.AvailableActions) {
      auto Entry = Actions.appendDictionary();
      Entry.set(KeyActionUID, Info.Kind);
      Entry.set(KeyActionName, Info.KindName);
      if (!Info.UnavailableReason.empty())
        Entry.set(KeyActionUnavailableReason, Info.UnavailableReason);
    }
  }
  if (Info.ParentNameOffset) {
    Elem.set(KeyParentLoc, Info.ParentNameOffset.getValue());
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

static void reportRangeInfo(const RequestResult<RangeInfo> &Result,
                            ResponseReceiver Rec) {
  if (Result.isCancelled())
    return Rec(createErrorRequestCancelled());
  if (Result.isError())
    return Rec(createErrorRequestFailed(Result.getError()));

  const RangeInfo &Info = Result.value();

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

static void reportNameInfo(const RequestResult<NameTranslatingInfo> &Result,
                           ResponseReceiver Rec) {
  if (Result.isCancelled())
    return Rec(createErrorRequestCancelled());
  if (Result.isError())
    return Rec(createErrorRequestFailed(Result.getError()));

  const NameTranslatingInfo &Info = Result.value();

  ResponseBuilder RespBuilder;
  if (!Info.InternalDiagnostic.empty()) {
    auto Elem = RespBuilder.getDictionary();
    Elem.set(KeyInternalDiagnostic, Info.InternalDiagnostic);
    return Rec(RespBuilder.createResponse());
  }
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
    static UIdent UIDKindNameSwift(KindNameSwift.str());
    auto Arr = Elem.setArray(Info.NameKind == UIDKindNameSwift ?
                             KeyArgNames : KeySelectorPieces);
    for (auto N : Info.ArgNames) {
      auto NameEle = Arr.appendDictionary();
      NameEle.set(KeyName, N);
    }
  }
  if (Info.IsZeroArgSelector) {
    Elem.set(KeyIsZeroArgSelector, Info.IsZeroArgSelector);
  }
  Rec(RespBuilder.createResponse());
}

//===----------------------------------------------------------------------===//
// ReportExpressionTypeInfo
//===----------------------------------------------------------------------===//
static void reportExpressionTypeInfo(const RequestResult<ExpressionTypesInFile> &Result,
                                     ResponseReceiver Rec) {
  if (Result.isCancelled())
    return Rec(createErrorRequestCancelled());
  if (Result.isError())
    return Rec(createErrorRequestFailed(Result.getError()));

  const ExpressionTypesInFile &Info = Result.value();

  ResponseBuilder Builder;
  auto Dict = Builder.getDictionary();
  ExpressionTypeArrayBuilder ArrBuilder(Info.TypeBuffer);
  for (auto &R: Info.Results) {
    ArrBuilder.add(R);
  }
  Dict.setCustomBuffer(KeyExpressionTypeList, CustomBufferKind::ExpressionTypeArray,
                       ArrBuilder.createBuffer());
  Rec(Builder.createResponse());
}

//===----------------------------------------------------------------------===//
// FindRelatedIdents
//===----------------------------------------------------------------------===//

static void findRelatedIdents(StringRef Filename,
                              int64_t Offset,
                              bool CancelOnSubsequentRequest,
                              ArrayRef<const char *> Args,
                              ResponseReceiver Rec) {
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.findRelatedIdentifiersInFile(Filename, Offset, CancelOnSubsequentRequest,
                                    Args,
                                    [Rec](const RequestResult<RelatedIdentsInfo> &Result) {
  if (Result.isCancelled())
    return Rec(createErrorRequestCancelled());
  if (Result.isError())
    return Rec(createErrorRequestFailed(Result.getError()));

  const RelatedIdentsInfo &Info = Result.value();

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

  void setCompletionKind(UIdent kind) override;
  bool handleResult(const CodeCompletionInfo &Info) override;
};
} // end anonymous namespace

static sourcekitd_response_t
codeComplete(llvm::MemoryBuffer *InputBuf, int64_t Offset,
             Optional<RequestDict> optionsDict,
             ArrayRef<const char *> Args,
             Optional<VFSOptions> vfsOptions) {
  ResponseBuilder RespBuilder;
  SKCodeCompletionConsumer CCC(RespBuilder);

  std::unique_ptr<SKOptionsDictionary> options;
  if (optionsDict)
    options = std::make_unique<SKOptionsDictionary>(*optionsDict);

  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.codeComplete(InputBuf, Offset, options.get(), CCC, Args,
                    std::move(vfsOptions));
  return CCC.createResponse();
}

void SKCodeCompletionConsumer::failed(StringRef ErrDescription) {
  ErrorDescription = ErrDescription;
}

void SKCodeCompletionConsumer::setCompletionKind(UIdent kind) {
  assert(kind.isValid());
  RespBuilder.getDictionary().set(KeyKind, kind);
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
} // end anonymous namespace

static sourcekitd_response_t codeCompleteOpen(StringRef Name,
                                              llvm::MemoryBuffer *InputBuf,
                                              int64_t Offset,
                                              Optional<RequestDict> optionsDict,
                                              ArrayRef<const char *> Args,
                                              Optional<VFSOptions> vfsOptions) {
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
      if (kind == KindCodeCompletionEverything) {
        rule.kind = FilterRule::Everything;
      } else if (kind == KindCodeCompletionModule) {
        rule.kind = FilterRule::Module;
      } else if (kind == KindCodeCompletionKeyword) {
        rule.kind = FilterRule::Keyword;
      } else if (kind == KindCodeCompletionLiteral) {
        rule.kind = FilterRule::Literal;
      } else if (kind == KindCodeCompletionCustom) {
        rule.kind = FilterRule::CustomCompletion;
      } else if (kind == KindCodeCompletionIdentifier) {
        rule.kind = FilterRule::Identifier;
      } else if (kind == KindCodeCompletionDescription) {
        rule.kind = FilterRule::Description;
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
      case FilterRule::Description: {
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
                        Args, std::move(vfsOptions));
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
// Type Context Info
//===----------------------------------------------------------------------===//

static sourcekitd_response_t typeContextInfo(llvm::MemoryBuffer *InputBuf,
                                             int64_t Offset,
                                             ArrayRef<const char *> Args,
                                             Optional<VFSOptions> vfsOptions) {
  ResponseBuilder RespBuilder;

  class Consumer : public TypeContextInfoConsumer {
    ResponseBuilder::Array SKResults;
    Optional<std::string> ErrorDescription;

  public:
    Consumer(ResponseBuilder Builder)
        : SKResults(Builder.getDictionary().setArray(KeyResults)) {}

    void handleResult(const TypeContextInfoItem &Item) override {
      auto SKElem = SKResults.appendDictionary();
      SKElem.set(KeyTypeName, Item.TypeName);
      SKElem.set(KeyTypeUsr, Item.TypeUSR);
      auto members = SKElem.setArray(KeyImplicitMembers);
      for (auto member : Item.ImplicitMembers) {
        auto memberElem = members.appendDictionary();
        memberElem.set(KeyName, member.Name);
        memberElem.set(KeyDescription, member.Description);
        memberElem.set(KeySourceText, member.SourceText);
        if (!member.DocBrief.empty())
          memberElem.set(KeyDocBrief, member.DocBrief);
      }
    }

    void failed(StringRef ErrDescription) override {
      ErrorDescription = ErrDescription;
    }

    bool isError() const { return ErrorDescription.hasValue(); }
    const char *getErrorDescription() const {
      return ErrorDescription->c_str();
    }
  } Consumer(RespBuilder);

  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.getExpressionContextInfo(InputBuf, Offset, Args, Consumer,
                                std::move(vfsOptions));

  if (Consumer.isError())
    return createErrorRequestFailed(Consumer.getErrorDescription());
  return RespBuilder.createResponse();
}

//===----------------------------------------------------------------------===//
// Conforming Method List
//===----------------------------------------------------------------------===//

static sourcekitd_response_t
conformingMethodList(llvm::MemoryBuffer *InputBuf, int64_t Offset,
                     ArrayRef<const char *> Args,
                     ArrayRef<const char *> ExpectedTypes,
                     Optional<VFSOptions> vfsOptions) {
  ResponseBuilder RespBuilder;

  class Consumer : public ConformingMethodListConsumer {
    ResponseBuilder::Dictionary SKResult;
    Optional<std::string> ErrorDescription;

  public:
    Consumer(ResponseBuilder Builder) : SKResult(Builder.getDictionary()) {}

    void handleResult(const ConformingMethodListResult &Result) override {
      SKResult.set(KeyTypeName, Result.TypeName);
      SKResult.set(KeyTypeUsr, Result.TypeUSR);
      auto members = SKResult.setArray(KeyMembers);
      for (auto member : Result.Members) {
        auto memberElem = members.appendDictionary();
        memberElem.set(KeyName, member.Name);
        memberElem.set(KeyTypeName, member.TypeName);
        memberElem.set(KeyTypeUsr, member.TypeUSR);
        memberElem.set(KeyDescription, member.Description);
        memberElem.set(KeySourceText, member.SourceText);
        if (!member.DocBrief.empty())
          memberElem.set(KeyDocBrief, member.DocBrief);
      }
    }

    void failed(StringRef ErrDescription) override {
      ErrorDescription = ErrDescription;
    }

    bool isError() const { return ErrorDescription.hasValue(); }
    const char *getErrorDescription() const {
      return ErrorDescription->c_str();
    }
  } Consumer(RespBuilder);

  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.getConformingMethodList(InputBuf, Offset, Args, ExpectedTypes, Consumer,
                               std::move(vfsOptions));

  if (Consumer.isError())
    return createErrorRequestFailed(Consumer.getErrorDescription());
  return RespBuilder.createResponse();
}

//===----------------------------------------------------------------------===//
// Editor
//===----------------------------------------------------------------------===//

namespace {
class SKEditorConsumer : public EditorConsumer {
  ResponseReceiver RespReceiver;
  ResponseBuilder RespBuilder;

public:
  ResponseBuilder::Dictionary Dict;
  DocStructureArrayBuilder DocStructure;
  TokenAnnotationsArrayBuilder SyntaxMap;
  TokenAnnotationsArrayBuilder SemanticAnnotations;

  ResponseBuilder::Array Diags;
  sourcekitd_response_t Error = nullptr;

  SKEditorConsumerOptions Opts;

public:
  SKEditorConsumer(SKEditorConsumerOptions Opts) : Opts(Opts) {
    Dict = RespBuilder.getDictionary();
  }

  SKEditorConsumer(ResponseReceiver RespReceiver, SKEditorConsumerOptions Opts)
      : SKEditorConsumer(Opts) {
    this->RespReceiver = RespReceiver;
  }

  sourcekitd_response_t createResponse();

  bool needsSemanticInfo() override {
    return !Opts.SyntacticOnly && !isSemanticEditorDisabled();
  }

  void handleRequestError(const char *Description) override;

  bool syntaxMapEnabled() override { return Opts.EnableSyntaxMap; }

  void handleSyntaxMap(unsigned Offset, unsigned Length, UIdent Kind) override;

  void handleSemanticAnnotation(unsigned Offset, unsigned Length, UIdent Kind,
                                bool isSystem) override;

  bool documentStructureEnabled() override { return Opts.EnableStructure; }

  void beginDocumentSubStructure(unsigned Offset, unsigned Length, UIdent Kind,
                                 UIdent AccessLevel,
                                 UIdent SetterAccessLevel,
                                 unsigned NameOffset,
                                 unsigned NameLength,
                                 unsigned BodyOffset,
                                 unsigned BodyLength,
                                 unsigned DocOffset,
                                 unsigned DocLength,
                                 StringRef DisplayName,
                                 StringRef TypeName,
                                 StringRef RuntimeName,
                                 StringRef SelectorName,
                                 ArrayRef<StringRef> InheritedTypes,
                                 ArrayRef<std::tuple<UIdent, unsigned, unsigned>> Attrs) override;

  void endDocumentSubStructure() override;

  void handleDocumentSubStructureElement(UIdent Kind, unsigned Offset,
                                         unsigned Length) override;

  void recordAffectedRange(unsigned Offset, unsigned Length) override;

  void recordAffectedLineRange(unsigned Line, unsigned Length) override;

  void recordFormattedText(StringRef Text) override;

  void setDiagnosticStage(UIdent DiagStage) override;
  void handleDiagnostic(const DiagnosticEntryInfo &Info,
                        UIdent DiagStage) override;

  void handleSourceText(StringRef Text) override;

  void handleSyntaxTree(const swift::syntax::SourceFileSyntax &SyntaxTree,
                        std::unordered_set<unsigned> &ReusedNodeIds) override;

  SyntaxTreeTransferMode syntaxTreeTransferMode() override {
    return Opts.SyntaxTransferMode;
  }

  void finished() override {
    if (RespReceiver)
      RespReceiver(createResponse());
  }
};

} // end anonymous namespace

static sourcekitd_response_t
editorOpen(StringRef Name, llvm::MemoryBuffer *Buf,
           SKEditorConsumerOptions Opts, ArrayRef<const char *> Args,
           Optional<VFSOptions> vfsOptions) {
  SKEditorConsumer EditC(Opts);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorOpen(Name, Buf, EditC, Args, std::move(vfsOptions));
  return EditC.createResponse();
}

static sourcekitd_response_t
editorOpenInterface(StringRef Name, StringRef ModuleName,
                    Optional<StringRef> Group, ArrayRef<const char *> Args,
                    bool SynthesizedExtensions,
                    Optional<StringRef> InterestedUSR) {
  SKEditorConsumerOptions Opts;
  Opts.EnableSyntaxMap = true;
  Opts.EnableStructure = true;
  SKEditorConsumer EditC(Opts);
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
  SKEditorConsumerOptions Opts;
  Opts.EnableSyntaxMap = true;
  Opts.EnableStructure = true;
  auto EditC = std::make_shared<SKEditorConsumer>(Rec, Opts);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorOpenSwiftSourceInterface(Name, HeaderName, Args, EditC);
}

static void
editorOpenSwiftTypeInterface(StringRef TypeUsr, ArrayRef<const char *> Args,
                             ResponseReceiver Rec) {
  SKEditorConsumerOptions Opts;
  Opts.EnableSyntaxMap = true;
  Opts.EnableStructure = true;
  auto EditC = std::make_shared<SKEditorConsumer>(Rec, Opts);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorOpenTypeInterface(*EditC, Args, TypeUsr);
}

static sourcekitd_response_t editorExtractTextFromComment(StringRef Source) {
  SKEditorConsumerOptions Opts;
  Opts.SyntacticOnly = true;
  SKEditorConsumer EditC(Opts);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorExtractTextFromComment(Source, EditC);
  return EditC.createResponse();
}

static sourcekitd_response_t editorConvertMarkupToXML(StringRef Source) {
  SKEditorConsumerOptions Opts;
  Opts.SyntacticOnly = true;
  SKEditorConsumer EditC(Opts);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorConvertMarkupToXML(Source, EditC);
  return EditC.createResponse();
}

static sourcekitd_response_t
editorOpenHeaderInterface(StringRef Name, StringRef HeaderName,
                          ArrayRef<const char *> Args,
                          bool UsingSwiftArgs,
                          bool SynthesizedExtensions,
                          StringRef swiftVersion) {
  SKEditorConsumerOptions Opts;
  Opts.EnableSyntaxMap = true;
  Opts.EnableStructure = true;
  SKEditorConsumer EditC(Opts);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorOpenHeaderInterface(EditC, Name, HeaderName, Args, UsingSwiftArgs,
                                 SynthesizedExtensions, swiftVersion);
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
                  unsigned Length, SKEditorConsumerOptions Opts) {
  SKEditorConsumer EditC(Opts);
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
  SKEditorConsumerOptions Opts;
  Opts.SyntacticOnly = true;
  SKEditorConsumer EditC(Opts);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorFormatText(Name, Line, Length, EditC);
  return EditC.createResponse();
}

static sourcekitd_response_t
editorExpandPlaceholder(StringRef Name, unsigned Offset, unsigned Length) {
  SKEditorConsumerOptions Opts;
  Opts.SyntacticOnly = true;
  SKEditorConsumer EditC(Opts);
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  Lang.editorExpandPlaceholder(Name, Offset, Length, EditC);
  return EditC.createResponse();
}

sourcekitd_response_t SKEditorConsumer::createResponse() {
  if (Error)
    return Error;

  if (Opts.EnableSyntaxMap) {
    Dict.setCustomBuffer(KeySyntaxMap,
        CustomBufferKind::TokenAnnotationsArray,
        SyntaxMap.createBuffer());
  }
  if (!SemanticAnnotations.empty()) {
    Dict.setCustomBuffer(KeyAnnotations,
        CustomBufferKind::TokenAnnotationsArray,
        SemanticAnnotations.createBuffer());
  }
  if (Opts.EnableStructure) {
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

void SKEditorConsumer::handleSyntaxMap(unsigned Offset, unsigned Length,
                                       UIdent Kind) {
  if (!Opts.EnableSyntaxMap)
    return;

  SyntaxMap.add(Kind, Offset, Length, /*IsSystem=*/false);
}

void SKEditorConsumer::handleSemanticAnnotation(unsigned Offset,
                                                unsigned Length, UIdent Kind,
                                                bool isSystem) {
  assert(Kind.isValid());
  SemanticAnnotations.add(Kind, Offset, Length, isSystem);
}

void
SKEditorConsumer::beginDocumentSubStructure(unsigned Offset,
                                            unsigned Length, UIdent Kind,
                                            UIdent AccessLevel,
                                            UIdent SetterAccessLevel,
                                            unsigned NameOffset,
                                            unsigned NameLength,
                                            unsigned BodyOffset,
                                            unsigned BodyLength,
                                            unsigned DocOffset,
                                            unsigned DocLength,
                                            StringRef DisplayName,
                                            StringRef TypeName,
                                            StringRef RuntimeName,
                                            StringRef SelectorName,
                                            ArrayRef<StringRef> InheritedTypes,
                                            ArrayRef<std::tuple<UIdent, unsigned, unsigned>> Attrs) {
  if (Opts.EnableStructure) {
    DocStructure.beginSubStructure(
        Offset, Length, Kind, AccessLevel, SetterAccessLevel, NameOffset,
        NameLength, BodyOffset, BodyLength, DocOffset, DocLength, DisplayName,
        TypeName, RuntimeName, SelectorName, InheritedTypes, Attrs);
  }
}

void SKEditorConsumer::endDocumentSubStructure() {
  if (Opts.EnableStructure)
    DocStructure.endSubStructure();
}

void SKEditorConsumer::handleDocumentSubStructureElement(UIdent Kind,
                                                         unsigned Offset,
                                                         unsigned Length) {
  if (Opts.EnableStructure)
    DocStructure.addElement(Kind, Offset, Length);
}

void SKEditorConsumer::recordAffectedRange(unsigned Offset, unsigned Length) {
  Dict.set(KeyOffset, Offset);
  Dict.set(KeyLength, Length);
}

void SKEditorConsumer::recordAffectedLineRange(unsigned Line, unsigned Length) {
  Dict.set(KeyLine, Line);
  Dict.set(KeyLength, Length);
}

void SKEditorConsumer::recordFormattedText(StringRef Text) {
  Dict.set(KeySourceText, Text);
}

static void fillDictionaryForDiagnosticInfoBase(
    ResponseBuilder::Dictionary Elem, const DiagnosticEntryInfoBase &Info);

static void fillDictionaryForDiagnosticInfo(
    ResponseBuilder::Dictionary Elem, const DiagnosticEntryInfo &Info) {

  UIdent SeverityUID;
  static UIdent UIDKindDiagWarning(KindDiagWarning.str());
  static UIdent UIDKindDiagError(KindDiagError.str());
  switch (Info.Severity) {
  case DiagnosticSeverityKind::Warning:
    SeverityUID = UIDKindDiagWarning;
    break;
  case DiagnosticSeverityKind::Error:
    SeverityUID = UIDKindDiagError;
    break;
  }

  Elem.set(KeySeverity, SeverityUID);
  fillDictionaryForDiagnosticInfoBase(Elem, Info);

  if (!Info.Notes.empty()) {
    auto NotesArr = Elem.setArray(KeyDiagnostics);
    for (auto &NoteDiag : Info.Notes) {
      auto NoteElem = NotesArr.appendDictionary();
      NoteElem.set(KeySeverity, KindDiagNote);
      fillDictionaryForDiagnosticInfoBase(NoteElem, NoteDiag);
    }
  }
}

static void fillDictionaryForDiagnosticInfoBase(
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

void SKEditorConsumer::setDiagnosticStage(UIdent DiagStage) {
  Dict.set(KeyDiagnosticStage, DiagStage);
}

void SKEditorConsumer::handleDiagnostic(const DiagnosticEntryInfo &Info,
                                        UIdent DiagStage) {
  if (!Opts.EnableDiagnostics)
    return;

  ResponseBuilder::Array &Arr = Diags;
  if (Arr.isNull())
    Arr = Dict.setArray(KeyDiagnostics);

  auto Elem = Arr.appendDictionary();
  Elem.set(KeyDiagnosticStage, DiagStage);
  fillDictionaryForDiagnosticInfo(Elem, Info);
}

void SKEditorConsumer::handleSourceText(StringRef Text) {
  Dict.set(KeySourceText, Text);
}

void serializeSyntaxTreeAsByteTree(
    const swift::syntax::SourceFileSyntax &SyntaxTree,
    std::unordered_set<unsigned> &ReusedNodeIds,
    ResponseBuilder::Dictionary &Dict) {
  auto StartClock = clock();
  // Serialize the syntax tree as a ByteTree
  auto Stream = swift::ExponentialGrowthAppendingBinaryByteStream();
  Stream.reserve(32 * 1024);
  std::map<void *, void *> UserInfo;
  UserInfo[swift::byteTree::UserInfoKeyReusedNodeIds] = &ReusedNodeIds;
  swift::byteTree::ByteTreeWriter::write(Stream,
                                         swift::byteTree::SYNTAX_TREE_VERSION,
                                         *SyntaxTree.getRaw(), UserInfo);

  std::unique_ptr<llvm::WritableMemoryBuffer> Buf =
      llvm::WritableMemoryBuffer::getNewUninitMemBuffer(Stream.data().size());
  memcpy(Buf->getBufferStart(), Stream.data().data(), Stream.data().size());

  Dict.setCustomBuffer(KeySerializedSyntaxTree, CustomBufferKind::RawData,
                       std::move(Buf));

  auto EndClock = clock();
  LOG_SECTION("incrParse Performance", InfoLowPrio) {
    Log->getOS() << "Serialized " << Stream.data().size()
                 << " bytes as ByteTree in ";
    auto Seconds = (double)(EndClock - StartClock) * 1000 / CLOCKS_PER_SEC;
    llvm::write_double(Log->getOS(), Seconds, llvm::FloatStyle::Fixed, 2);
    Log->getOS() << "ms";
  }
}

void serializeSyntaxTreeAsJson(
    const swift::syntax::SourceFileSyntax &SyntaxTree,
    std::unordered_set<unsigned> ReusedNodeIds,
    ResponseBuilder::Dictionary &Dict) {
  auto StartClock = clock();
  // 4096 is a heuristic buffer size that appears to usually be able to fit an
  // incremental syntax tree
  size_t ReserveBufferSize = 4096;
  std::string SyntaxTreeString;
  SyntaxTreeString.reserve(ReserveBufferSize);
  {
    llvm::raw_string_ostream SyntaxTreeStream(SyntaxTreeString);
    SyntaxTreeStream.SetBufferSize(ReserveBufferSize);
    swift::json::Output::UserInfoMap JsonUserInfo;
    JsonUserInfo[swift::json::OmitNodesUserInfoKey] = &ReusedNodeIds;
    swift::json::Output SyntaxTreeOutput(SyntaxTreeStream, JsonUserInfo,
                                         /*PrettyPrint=*/false);
    SyntaxTreeOutput << *SyntaxTree.getRaw();
  }
  Dict.set(KeySerializedSyntaxTree, SyntaxTreeString);

  auto EndClock = clock();
  LOG_SECTION("incrParse Performance", InfoLowPrio) {
    Log->getOS() << "Serialized " << SyntaxTreeString.size()
                 << " bytes as JSON in ";
    auto Seconds = (double)(EndClock - StartClock) * 1000 / CLOCKS_PER_SEC;
    llvm::write_double(Log->getOS(), Seconds, llvm::FloatStyle::Fixed, 2);
    Log->getOS() << "ms";
  }
}

void SKEditorConsumer::handleSyntaxTree(
    const swift::syntax::SourceFileSyntax &SyntaxTree,
    std::unordered_set<unsigned> &ReusedNodeIds) {

  std::unordered_set<unsigned> OmitNodes;
  switch (Opts.SyntaxTransferMode) {
  case SourceKit::SyntaxTreeTransferMode::Off:
    // Don't serialize the tree at all
    return;
  case SourceKit::SyntaxTreeTransferMode::Full:
    // Serialize the tree without omitting any nodes
    OmitNodes = {};
    break;
  case SourceKit::SyntaxTreeTransferMode::Incremental:
    // Serialize the tree and omit all nodes that have been reused
    OmitNodes = ReusedNodeIds;
    break;
  }

  switch (Opts.SyntaxSerializationFormat) {
  case SourceKit::SyntaxTreeSerializationFormat::JSON:
    serializeSyntaxTreeAsJson(SyntaxTree, OmitNodes, Dict);
    break;
  case SourceKit::SyntaxTreeSerializationFormat::ByteTree:
    serializeSyntaxTreeAsByteTree(SyntaxTree, OmitNodes, Dict);
    break;
  }
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
    [&](const RequestResult<InterfaceDocInfo> &Result) {
      if (Result.isCancelled()) {
        Resp = createErrorRequestCancelled();
        return;
      }
      if (Result.isError()) {
        Resp = createErrorRequestFailed(Result.getError());
        return;
      }

      const InterfaceDocInfo &Info = Result.value();

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
                        [&](const RequestResult<ArrayRef<StringRef>> &Result) {
    if (Result.isCancelled()) {
      Resp = createErrorRequestCancelled();
      return;
    }
    if (Result.isError()) {
      Resp = createErrorRequestFailed(Result.getError());
      return;
    }

    ArrayRef<StringRef> Groups = Result.value();

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

static bool
buildRenameLocationsFromDict(RequestDict &Req, bool UseNewName,
                             std::vector<RenameLocations> &RenameLocations,
                             llvm::SmallString<64> &Error) {
  bool Failed = Req.dictionaryArrayApply(KeyRenameLocations,
                                         [&](RequestDict RenameLocation) {
    int64_t IsFunctionLike = false;
    if (RenameLocation.getInt64(KeyIsFunctionLike, IsFunctionLike, false)) {
      Error = "missing key.is_function_like";
      return true;
    }

    int64_t IsNonProtocolType = false;
    if (RenameLocation.getInt64(KeyIsNonProtocolType, IsNonProtocolType, false)) {
      Error = "missing key.is_non_protocol_type";
      return true;
    }

    Optional<StringRef> OldName = RenameLocation.getString(KeyName);
    if (!OldName.hasValue()) {
      Error = "missing key.name";
      return true;
    }

    Optional<StringRef> NewName;
    if (UseNewName) {
      NewName = RenameLocation.getString(KeyNewName);
      if (!NewName.hasValue()) {
        Error = "missing key.newname";
        return true;
      }
    }

    RenameLocations.push_back({*OldName,
                               UseNewName ? *NewName : "",
                               static_cast<bool>(IsFunctionLike),
                               static_cast<bool>(IsNonProtocolType),
                               {}});
    auto &LineCols = RenameLocations.back().LineColumnLocs;
    bool Failed = RenameLocation.dictionaryArrayApply(KeyLocations,
                                                      [&](RequestDict LineAndCol) {
      int64_t Line = 0;
      int64_t Column = 0;

      if (LineAndCol.getInt64(KeyLine, Line, false)) {
        Error = "missing key.line";
        return true;
      }
      if (LineAndCol.getInt64(KeyColumn, Column, false)) {
        Error = "missing key.column";
        return true;
      }

      sourcekitd_uid_t NameType = LineAndCol.getUID(KeyNameType);
      if (!NameType) {
        Error = "missing key.nametype";
        return true;
      }
      RenameType RenameType = RenameType::Unknown;
      if (NameType == KindDefinition) {
        RenameType = RenameType::Definition;
      } else if (NameType == KindReference) {
        RenameType = RenameType::Reference;
      } else if (NameType == KindCall) {
        RenameType = RenameType::Call;
      } else if (NameType != KindUnknown) {
        Error = "invalid value for 'key.nametype'";
        return true;
      }
      LineCols.push_back({static_cast<unsigned>(Line),
        static_cast<unsigned>(Column), RenameType});
      return false;
    });
    if (Failed && Error.empty()) {
      Error = "invalid key.locations";
    }
    return Failed;
  });
  if (Failed && Error.empty()) {
    Error = "invalid key.renamelocations";
  }
  return Failed;
}

static sourcekitd_response_t
createCategorizedEditsResponse(const RequestResult<ArrayRef<CategorizedEdits>> &Result) {
  if (Result.isCancelled())
    return createErrorRequestCancelled();
  if (Result.isError())
    return createErrorRequestFailed(Result.getError());

  const ArrayRef<CategorizedEdits> &AllEdits = Result.value();

  ResponseBuilder RespBuilder;
  auto Dict = RespBuilder.getDictionary();
  auto Arr = Dict.setArray(KeyCategorizedEdits);
  for (auto &TheEdit : AllEdits) {
    auto Entry = Arr.appendDictionary();
    Entry.set(KeyCategory, TheEdit.Category);
    auto Edits = Entry.setArray(KeyEdits);
    for(auto E: TheEdit.Edits) {
      auto Edit = Edits.appendDictionary();
      Edit.set(KeyLine, E.StartLine);
      Edit.set(KeyColumn, E.StartColumn);
      Edit.set(KeyEndLine, E.EndLine);
      Edit.set(KeyEndColumn, E.EndColumn);
      Edit.set(KeyText, E.NewText);
      if (!E.RegionsWithNote.empty()) {
        auto Notes = Edit.setArray(KeyRangesWorthNote);
        for (auto R : E.RegionsWithNote) {
          auto N = Notes.appendDictionary();
          N.set(KeyKind, R.Kind);
          N.set(KeyLine, R.StartLine);
          N.set(KeyColumn, R.StartColumn);
          N.set(KeyEndLine, R.EndLine);
          N.set(KeyEndColumn, R.EndColumn);
          if (R.ArgIndex)
            N.set(KeyArgIndex, *R.ArgIndex);
        }
      }
    }
  }
  return RespBuilder.createResponse();
}

static sourcekitd_response_t
syntacticRename(llvm::MemoryBuffer *InputBuf,
                ArrayRef<RenameLocations> RenameLocations,
                ArrayRef<const char*> Args) {
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  sourcekitd_response_t Result;
  Lang.syntacticRename(InputBuf, RenameLocations, Args,
    [&](const RequestResult<ArrayRef<CategorizedEdits>> &ReqResult) {
      Result = createCategorizedEditsResponse(ReqResult);
  });
  return Result;
}

static sourcekitd_response_t
createCategorizedRenameRangesResponse(const RequestResult<ArrayRef<CategorizedRenameRanges>> &Result) {
  if (Result.isCancelled())
    return createErrorRequestCancelled();
  if (Result.isError())
    return createErrorRequestFailed(Result.getError());

  const ArrayRef<CategorizedRenameRanges> &Ranges = Result.value();

  ResponseBuilder RespBuilder;
  auto Dict = RespBuilder.getDictionary();
  auto Arr = Dict.setArray(KeyCategorizedRanges);
  for (const auto &CategorizedRange : Ranges) {
    auto Entry = Arr.appendDictionary();
    Entry.set(KeyCategory, CategorizedRange.Category);
    auto Ranges = Entry.setArray(KeyRanges);
    for (const auto &R : CategorizedRange.Ranges) {
      auto Range = Ranges.appendDictionary();
      Range.set(KeyLine, R.StartLine);
      Range.set(KeyColumn, R.StartColumn);
      Range.set(KeyEndLine, R.EndLine);
      Range.set(KeyEndColumn, R.EndColumn);
      Range.set(KeyKind, R.Kind);
      if (R.ArgIndex) {
        Range.set(KeyArgIndex, *R.ArgIndex);
      }
    }
  }
  return RespBuilder.createResponse();
}

static sourcekitd_response_t
findRenameRanges(llvm::MemoryBuffer *InputBuf,
                 ArrayRef<RenameLocations> RenameLocations,
                 ArrayRef<const char *> Args) {
  LangSupport &Lang = getGlobalContext().getSwiftLangSupport();
  sourcekitd_response_t Result;
  Lang.findRenameRanges(
      InputBuf, RenameLocations, Args,
      [&](const RequestResult<ArrayRef<CategorizedRenameRanges>> &ReqResult) {
        Result = createCategorizedRenameRangesResponse(ReqResult);
      });
  return Result;
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

namespace {
class CompileTrackingConsumer final : public trace::TraceConsumer {
public:
  void operationStarted(uint64_t OpId, trace::OperationKind OpKind,
                        const trace::SwiftInvocation &Inv,
                        const trace::StringPairs &OpArgs) override;
  void operationFinished(uint64_t OpId, trace::OperationKind OpKind,
                         ArrayRef<DiagnosticEntryInfo> Diagnostics) override;
  swift::OptionSet<trace::OperationKind> desiredOperations() override {
    return swift::OptionSet<trace::OperationKind>() |
           trace::OperationKind::PerformSema |
           trace::OperationKind::CodeCompletion;
  }
};
} // end anonymous namespace

void CompileTrackingConsumer::operationStarted(
    uint64_t OpId, trace::OperationKind OpKind,
    const trace::SwiftInvocation &Inv, const trace::StringPairs &OpArgs) {
  if (!desiredOperations().contains(OpKind))
    return;

  static UIdent CompileWillStartUID("source.notification.compile-will-start");
  ResponseBuilder RespBuilder;
  auto Dict = RespBuilder.getDictionary();
  Dict.set(KeyNotification, CompileWillStartUID);
  Dict.set(KeyCompileID, std::to_string(OpId));
  Dict.set(KeyFilePath, Inv.Args.PrimaryFile);
  // FIXME: OperationKind
  Dict.set(KeyCompilerArgsString, Inv.Args.Arguments);
  sourcekitd::postNotification(RespBuilder.createResponse());
}

void CompileTrackingConsumer::operationFinished(
    uint64_t OpId, trace::OperationKind OpKind,
    ArrayRef<DiagnosticEntryInfo> Diagnostics) {
  if (!desiredOperations().contains(OpKind))
    return;

  static UIdent CompileDidFinishUID("source.notification.compile-did-finish");
  ResponseBuilder RespBuilder;
  auto Dict = RespBuilder.getDictionary();
  Dict.set(KeyNotification, CompileDidFinishUID);
  Dict.set(KeyCompileID, std::to_string(OpId));
  auto DiagArray = Dict.setArray(KeyDiagnostics);
  for (const auto &DiagInfo : Diagnostics) {
    fillDictionaryForDiagnosticInfo(DiagArray.appendDictionary(), DiagInfo);
  }

  sourcekitd::postNotification(RespBuilder.createResponse());
}

static void enableCompileNotifications(bool value) {
  static std::atomic<bool> status{false};
  if (status.exchange(value) == value) {
    return; // Unchanged.
  }

  static CompileTrackingConsumer compileConsumer;
  if (value) {
    trace::registerConsumer(&compileConsumer);
  } else {
    trace::unregisterConsumer(&compileConsumer);
  }
}
