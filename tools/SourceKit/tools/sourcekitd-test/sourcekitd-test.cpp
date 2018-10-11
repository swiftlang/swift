//===--- sourcekitd-test.cpp ----------------------------------------------===//
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

#include "sourcekitd/sourcekitd.h"

#include "TestOptions.h"
#include "SourceKit/Support/Concurrency.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"
#include "swift/Demangling/ManglingMacros.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormatVariadic.h"
#include <fstream>
#if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h>
#include <sys/param.h>
#elif defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#endif

// FIXME: Platform compatibility.
#include <dispatch/dispatch.h>

using namespace llvm;

using namespace sourcekitd_test;

#if defined(_WIN32)
namespace {
int STDOUT_FILENO = _fileno(stdout);
const constexpr size_t MAXPATHLEN = MAX_PATH + 1;
char *realpath(const char *path, char *resolved_path) {
  DWORD dwLength = GetFullPathNameA(path, 0, nullptr, nullptr);
  if (dwLength == 0)
    return nullptr;
  if ((resolved_path = static_cast<char *>(malloc(dwLength + 1))))
    GetFullPathNameA(path, dwLength, resolved_path, nullptr);
  return resolved_path;
}
}
#endif

static int handleTestInvocation(ArrayRef<const char *> Args, TestOptions &InitOpts);
static bool handleResponse(sourcekitd_response_t Resp, const TestOptions &Opts,
                           const std::string &SourceFile,
                           std::unique_ptr<llvm::MemoryBuffer> SourceBuf,
                           TestOptions *InitOpts);
static void printCursorInfo(sourcekitd_variant_t Info, StringRef Filename,
                            llvm::raw_ostream &OS);
static void printNameTranslationInfo(sourcekitd_variant_t Info, llvm::raw_ostream &OS);
static void printRangeInfo(sourcekitd_variant_t Info, StringRef Filename,
                           llvm::raw_ostream &OS);
static void printDocInfo(sourcekitd_variant_t Info, StringRef Filename);
static void printInterfaceGen(sourcekitd_variant_t Info, bool CheckASCII);
static void printSemanticInfo();
static void printRelatedIdents(sourcekitd_variant_t Info, StringRef Filename,
                               llvm::raw_ostream &OS);
static void printFoundInterface(sourcekitd_variant_t Info,
                                llvm::raw_ostream &OS);
static void printFoundUSR(sourcekitd_variant_t Info,
                          llvm::MemoryBuffer *SourceBuf,
                          llvm::raw_ostream &OS);
static void printNormalizedDocComment(sourcekitd_variant_t Info);
static void expandPlaceholders(llvm::MemoryBuffer *SourceBuf,
                               llvm::raw_ostream &OS);

static void printModuleGroupNames(sourcekitd_variant_t Info,
                                  llvm::raw_ostream &OS);
static void printSyntacticRenameEdits(sourcekitd_variant_t Info,
                                      llvm::raw_ostream &OS);
static void printRenameRanges(sourcekitd_variant_t Info, llvm::raw_ostream &OS);
static void prepareDemangleRequest(sourcekitd_object_t Req,
                                   const TestOptions &Opts);
static void printDemangleResults(sourcekitd_variant_t Info, raw_ostream &OS);
static void prepareMangleRequest(sourcekitd_object_t Req,
                                 const TestOptions &Opts);
static void printMangleResults(sourcekitd_variant_t Info, raw_ostream &OS);
static void printStatistics(sourcekitd_variant_t Info, raw_ostream &OS);

static unsigned resolveFromLineCol(unsigned Line, unsigned Col,
                                   StringRef Filename);
static unsigned resolveFromLineCol(unsigned Line, unsigned Col,
                                   llvm::MemoryBuffer *InputBuf);
static std::pair<unsigned, unsigned> resolveToLineCol(unsigned Offset,
                                                      StringRef Filename);
static std::pair<unsigned, unsigned> resolveToLineCol(unsigned Offset,
                                                  llvm::MemoryBuffer *InputBuf);
static std::pair<unsigned, unsigned> resolveToLineColFromBuf(unsigned Offset,
                                                      const char *Buf);
static llvm::MemoryBuffer *getBufferForFilename(StringRef Filename);

static void notification_receiver(sourcekitd_response_t resp);

static SourceKitRequest ActiveRequest = SourceKitRequest::None;

#define KEY(NAME, CONTENT) static sourcekitd_uid_t Key##NAME;
#define REQUEST(NAME, CONTENT) static sourcekitd_uid_t Request##NAME;
#define KIND(NAME, CONTENT) static sourcekitd_uid_t Kind##NAME;
#include "SourceKit/Core/ProtocolUIDs.def"

#define REFACTORING(KIND, NAME, ID) static sourcekitd_uid_t Kind##Refactoring##KIND;
#include "swift/IDE/RefactoringKinds.def"

static sourcekitd_uid_t SemaDiagnosticStage;

static sourcekitd_uid_t NoteDocUpdate;
static SourceKit::Semaphore semaSemaphore(0);
static sourcekitd_response_t semaResponse;
static const char *semaName;

static sourcekitd_uid_t NoteTest;
static SourceKit::Semaphore noteSyncSemaphore(0);

namespace {
struct AsyncResponseInfo {
  SourceKit::Semaphore semaphore{0};
  sourcekitd_response_t response = nullptr;
  TestOptions options;
  std::string sourceFilename;
  std::unique_ptr<llvm::MemoryBuffer> sourceBuffer;
};
} // end anonymous namespace

static std::vector<AsyncResponseInfo> asyncResponses;

struct NotificationBuffer {
  std::vector<sourcekitd_response_t> notes;
  /// Add a notification to the buffer, taking ownership of it. Must be called
  /// from the main queue.
  void add(sourcekitd_response_t note) {
    notes.push_back(note);
  }
  /// Call the given handler for all notifications currently buffered.
  void handleNotifications(llvm::function_ref<void(sourcekitd_response_t)> f) {
    // Notifications are handled on the main queue.
    dispatch_sync(dispatch_get_main_queue(), ^{
      for (auto note : notes) {
        f(note);
        sourcekitd_response_dispose(note);
      }
      notes.clear();
    });
  }
};
static NotificationBuffer notificationBuffer;

static void syncNotificationsWithService() {
  // Send TestNotification request, then wait for the notification. This ensures
  // that all notifications previously posted on the service side have been
  // passed to our notification handler.
  sourcekitd_object_t req = sourcekitd_request_dictionary_create(nullptr, nullptr, 0);
  sourcekitd_request_dictionary_set_uid(req, KeyRequest, RequestTestNotification);
  auto resp = sourcekitd_send_request_sync(req);
  if (sourcekitd_response_is_error(resp)) {
    sourcekitd_response_description_dump(resp);
    exit(1);
  }
  sourcekitd_response_dispose(resp);
  sourcekitd_request_release(req);
  if (noteSyncSemaphore.wait(60 * 1000)) {
    llvm::report_fatal_error("Test notification not received");
  }
}

static void printBufferedNotifications(bool syncWithService = true) {
  if (syncWithService) {
    syncNotificationsWithService();
  }
  notificationBuffer.handleNotifications([](sourcekitd_response_t note) {
    sourcekitd_response_description_dump_filedesc(note, STDOUT_FILENO);
  });
}

static int skt_main(int argc, const char **argv);

int main(int argc, const char **argv) {
  dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0), ^{
    int ret = skt_main(argc, argv);
    exit(ret);
  });

  dispatch_main();
}

static int skt_main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  sourcekitd_initialize();

  sourcekitd_set_notification_handler(^(sourcekitd_response_t resp) {
    notification_receiver(resp);
  });

#define KEY(NAME, CONTENT) Key##NAME = sourcekitd_uid_get_from_cstr(CONTENT);
#include "SourceKit/Core/ProtocolUIDs.def"

  SemaDiagnosticStage = sourcekitd_uid_get_from_cstr("source.diagnostic.stage.swift.sema");

  NoteDocUpdate = sourcekitd_uid_get_from_cstr("source.notification.editor.documentupdate");
  NoteTest = sourcekitd_uid_get_from_cstr("source.notification.test");

#define REQUEST(NAME, CONTENT) Request##NAME = sourcekitd_uid_get_from_cstr(CONTENT);
#define KIND(NAME, CONTENT) Kind##NAME = sourcekitd_uid_get_from_cstr(CONTENT);
#include "SourceKit/Core/ProtocolUIDs.def"

#define REFACTORING(KIND, NAME, ID) Kind##Refactoring##KIND = sourcekitd_uid_get_from_cstr("source.refactoring.kind."#ID);
#include "swift/IDE/RefactoringKinds.def"

  // A test invocation may initialize the options to be used for subsequent
  // invocations.
  TestOptions InitOpts;
  auto Args = llvm::makeArrayRef(argv+1, argc-1);
  while (1) {
    unsigned i = 0;
    for (auto Arg: Args) {
      if (StringRef(Arg) == "==")
        break;
      ++i;
    }
    if (i == Args.size())
      break;
    if (int ret = handleTestInvocation(Args.slice(0, i), InitOpts)) {
      sourcekitd_shutdown();
      return ret;
    }
    Args = Args.slice(i+1);
  }

  if (int ret = handleTestInvocation(Args, InitOpts)) {
    sourcekitd_shutdown();
    return ret;
  }

  for (auto &info : asyncResponses) {
    if (info.semaphore.wait(60 * 1000)) {
      llvm::report_fatal_error("async request timed out");
    }

    if (handleResponse(info.response, info.options, info.sourceFilename,
                       std::move(info.sourceBuffer), nullptr)) {
      sourcekitd_shutdown();
      return 1;
    }
  }
  printBufferedNotifications();

  sourcekitd_shutdown();
  return 0;
}

static inline const char *getInterfaceGenDocumentName() {
  // Absolute "path" so that handleTestInvocation doesn't try to make it
  // absolute.
  return "/<interface-gen>";
}

static int printAnnotations();
static int printDiags();

static void getSemanticInfo(sourcekitd_variant_t Info, StringRef Filename);

static void addCodeCompleteOptions(sourcekitd_object_t Req, TestOptions &Opts) {
  if (!Opts.RequestOptions.empty()) {
    sourcekitd_object_t CCOpts =
        sourcekitd_request_dictionary_create(nullptr, nullptr, 0);
    for (auto &Opt : Opts.RequestOptions) {
      auto KeyValue = StringRef(Opt).split('=');
      std::string KeyStr("key.codecomplete.");
      KeyStr.append(KeyValue.first);
      sourcekitd_uid_t Key = sourcekitd_uid_get_from_cstr(KeyStr.c_str());

      // FIXME: more robust way to determine the option type.
      if (KeyValue.first == "filtertext") {
        sourcekitd_request_dictionary_set_stringbuf(
            CCOpts, Key, KeyValue.second.data(), KeyValue.second.size());
      } else {
        int64_t Value = 0;
        KeyValue.second.getAsInteger(0, Value);
        sourcekitd_request_dictionary_set_int64(CCOpts, Key, Value);
      }
    }
    sourcekitd_request_dictionary_set_value(Req, KeyCodeCompleteOptions,
                                            CCOpts);
    sourcekitd_request_release(CCOpts);
  }
}

static bool readPopularAPIList(StringRef filename,
                               std::vector<std::string> &result) {
  std::ifstream in(filename);
  if (!in.is_open()) {
    llvm::errs() << "error opening '" << filename << "'\n";
    return true;
  }

  std::string line;
  while (std::getline(in, line)) {
    result.emplace_back();
    std::swap(result.back(), line);
  }
  return false;
}

namespace {
class PrintingTimer {
  std::string desc;
  llvm::sys::TimePoint<> start;
  llvm::raw_ostream &OS;
public:
  PrintingTimer(std::string desc, llvm::raw_ostream &OS = llvm::errs())
      : desc(std::move(desc)), start(std::chrono::system_clock::now()), OS(OS) {
  }
  ~PrintingTimer() {
    std::chrono::duration<float, std::milli> delta(
        std::chrono::system_clock::now() - start);
    OS << desc << ": " << llvm::formatv("{0:ms+f3}", delta) << "\n";
  }
};
}

/// Wrapper for sourcekitd_send_request_sync that handles printing options.
static sourcekitd_response_t sendRequestSync(sourcekitd_object_t req,
                                             const TestOptions &opts) {
  if (opts.PrintRequest)
    sourcekitd_request_description_dump(req);
  Optional<PrintingTimer> timer;
  if (opts.timeRequest)
    timer.emplace("request time");
  return sourcekitd_send_request_sync(req);
}

static int handleJsonRequestPath(StringRef QueryPath, const TestOptions &Opts) {
  auto Buffer = getBufferForFilename(QueryPath)->getBuffer();
  char *Err = nullptr;
  auto Req = sourcekitd_request_create_from_yaml(Buffer.data(), &Err);
  if (!Req) {
    assert(Err);
    llvm::errs() << Err;
    free(Err);
    return 1;
  }

  sourcekitd_response_t Resp = sendRequestSync(Req, Opts);
  auto Error = sourcekitd_response_is_error(Resp);
  if (Opts.PrintResponse) {
    sourcekitd_response_description_dump_filedesc(Resp, STDOUT_FILENO);
  }
  return Error ? 1 : 0;
}

static int handleTestInvocation(TestOptions Opts, TestOptions &InitOpts);

static int handleTestInvocation(ArrayRef<const char *> Args,
                                TestOptions &InitOpts) {

  unsigned Optargc = 0;
  for (auto Arg: Args) {
    if (StringRef(Arg) == "--")
      break;
    ++Optargc;
  }

  TestOptions Opts = InitOpts;
  if (Opts.parseArgs(Args.slice(0, Optargc)))
    return 1;

  if (Optargc < Args.size())
    Opts.CompilerArgs = Args.slice(Optargc+1);

  assert(Opts.repeatRequest >= 1);
  for (unsigned i = 0; i < Opts.repeatRequest; ++i) {
    if (int ret = handleTestInvocation(Opts, InitOpts)) {
      printBufferedNotifications(/*syncWithService=*/true);
      return ret;
    }
    // We will sync with the service before exiting; don't do so here.
    printBufferedNotifications(/*syncWithService=*/false);
  }
  return 0;
}

static int handleTestInvocation(TestOptions Opts, TestOptions &InitOpts) {

  if (!Opts.JsonRequestPath.empty())
    return handleJsonRequestPath(Opts.JsonRequestPath, Opts);

  if (Opts.Request == SourceKitRequest::DemangleNames ||
      Opts.Request == SourceKitRequest::MangleSimpleClasses)
    Opts.SourceFile.clear();

  std::string SourceFile = Opts.SourceFile;
  if (!SourceFile.empty()) {
    llvm::SmallString<64> AbsSourceFile;
    AbsSourceFile += SourceFile;
    llvm::sys::fs::make_absolute(AbsSourceFile);
    SourceFile = AbsSourceFile.str();
  }
  std::string SemaName = !Opts.Name.empty() ? Opts.Name : SourceFile;

  if (!Opts.TextInputFile.empty()) {
    auto Buf = getBufferForFilename(Opts.TextInputFile);
    Opts.SourceText = Buf->getBuffer();
  }

  std::unique_ptr<llvm::MemoryBuffer> SourceBuf;
  if (Opts.SourceText.hasValue()) {
    SourceBuf = llvm::MemoryBuffer::getMemBuffer(*Opts.SourceText, Opts.SourceFile);
  } else if (!SourceFile.empty()) {
    SourceBuf = llvm::MemoryBuffer::getMemBuffer(
          getBufferForFilename(SourceFile)->getBuffer(), SourceFile);
  }

  // FIXME: we should detect if offset is required but not set.
  unsigned ByteOffset = Opts.Offset;
  if (Opts.Line != 0) {
    ByteOffset = resolveFromLineCol(Opts.Line, Opts.Col, SourceBuf.get());
  }

  if (Opts.EndLine != 0) {
    Opts.Length = resolveFromLineCol(Opts.EndLine, Opts.EndCol, SourceBuf.get()) -
      ByteOffset;
  }

  sourcekitd_object_t Req = sourcekitd_request_dictionary_create(nullptr,
                                                                 nullptr, 0);
  ActiveRequest = Opts.Request;
  switch (Opts.Request) {
  case SourceKitRequest::None:
    llvm::errs() << "request is not set\n";
    // FIXME: This non-zero return value is not propagated as an exit code.
    //        In other words, despite returning 1 here, the program still exits
    //        with a zero (successful) exit code.
    return 1;

  case SourceKitRequest::ProtocolVersion:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestProtocolVersion);
    break;

  case SourceKitRequest::DemangleNames:
    prepareDemangleRequest(Req, Opts);
    break;

  case SourceKitRequest::MangleSimpleClasses:
    prepareMangleRequest(Req, Opts);
    break;

  case SourceKitRequest::EnableCompileNotifications: {
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                          RequestEnableCompileNotifications);
    int64_t value = 1;
    for (auto &Opt : Opts.RequestOptions) {
      auto KeyValue = StringRef(Opt).split('=');
      if (KeyValue.first == "value") {
        KeyValue.second.getAsInteger(0, value);
      } else {
        llvm::errs() << "unknown parameter '" << KeyValue.first
                     << "' in -req-opts";
        return 1;
      }
    }
    sourcekitd_request_dictionary_set_int64(Req, KeyValue, value);
    break;
  }

  case SourceKitRequest::Index:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestIndex);
    break;

  case SourceKitRequest::CodeComplete:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestCodeComplete);
    sourcekitd_request_dictionary_set_int64(Req, KeyOffset, ByteOffset);
    break;

  case SourceKitRequest::CodeCompleteOpen:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                          RequestCodeCompleteOpen);
    sourcekitd_request_dictionary_set_int64(Req, KeyOffset, ByteOffset);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    addCodeCompleteOptions(Req, Opts);
    break;

  case SourceKitRequest::CodeCompleteClose:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                          RequestCodeCompleteClose);
    sourcekitd_request_dictionary_set_int64(Req, KeyOffset, ByteOffset);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    break;

  case SourceKitRequest::CodeCompleteUpdate:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                          RequestCodeCompleteUpdate);
    sourcekitd_request_dictionary_set_int64(Req, KeyOffset, ByteOffset);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    addCodeCompleteOptions(Req, Opts);
    break;

  case SourceKitRequest::CodeCompleteCacheOnDisk:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                          RequestCodeCompleteCacheOnDisk);
    sourcekitd_request_dictionary_set_string(Req, KeyName,
                                             Opts.CachePath.c_str());
    break;

  case SourceKitRequest::CodeCompleteSetPopularAPI: {
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                          RequestCodeCompleteSetPopularAPI);

    auto addPopularList = [&Req](StringRef filename, sourcekitd_uid_t key) {
      std::vector<std::string> names;
      if (readPopularAPIList(filename, names))
        return true;

      sourcekitd_object_t popular = sourcekitd_request_array_create(nullptr, 0);
      for (auto name : names)
        sourcekitd_request_array_set_string(popular, SOURCEKITD_ARRAY_APPEND,
                                            name.c_str());
      sourcekitd_request_dictionary_set_value(Req, key, popular);
      return false;
    };

    for (auto &Opt : Opts.RequestOptions) {
      auto KeyValue = StringRef(Opt).split('=');
      auto key = llvm::StringSwitch<sourcekitd_uid_t>(KeyValue.first)
        .Case("popular", KeyPopular)
        .Case("unpopular", KeyUnpopular)
        .Default(nullptr);
      if (!key) {
        llvm::errs() << "invalid key '" << KeyValue.first << "' in -req-opts\n";
        return 1;
      }

      if (addPopularList(KeyValue.second, key))
        return 1;
    }

    break;
  }

  case SourceKitRequest::CrashWithExit:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestCrashWithExit);
    break;

  case SourceKitRequest::CursorInfo:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestCursorInfo);
    if (Opts.CollectActionables) {
      sourcekitd_request_dictionary_set_int64(Req, KeyRetrieveRefactorActions, 1);
    }
    if (Opts.Length) {
      sourcekitd_request_dictionary_set_int64(Req, KeyLength, Opts.Length);
    }
    if (!Opts.USR.empty()) {
      sourcekitd_request_dictionary_set_string(Req, KeyUSR, Opts.USR.c_str());
    } else {
      sourcekitd_request_dictionary_set_int64(Req, KeyOffset, ByteOffset);
    }
    break;
  case SourceKitRequest::RangeInfo: {
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestRangeInfo);
    sourcekitd_request_dictionary_set_int64(Req, KeyOffset, ByteOffset);
    auto Length = Opts.Length;
    if (Opts.Length == 0 && Opts.EndLine > 0) {
      auto EndOff = resolveFromLineCol(Opts.EndLine, Opts.EndCol, SourceFile);
      Length = EndOff - ByteOffset;
    }
    sourcekitd_request_dictionary_set_int64(Req, KeyLength, Length);
    break;
  }

#define SEMANTIC_REFACTORING(KIND, NAME, ID) case SourceKitRequest::KIND:                 \
    {                                                                                     \
      sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestSemanticRefactoring); \
      sourcekitd_request_dictionary_set_uid(Req, KeyActionUID, KindRefactoring##KIND);    \
      sourcekitd_request_dictionary_set_string(Req, KeyName, Opts.Name.c_str());          \
      sourcekitd_request_dictionary_set_int64(Req, KeyLine, Opts.Line);                   \
      sourcekitd_request_dictionary_set_int64(Req, KeyColumn, Opts.Col);                  \
      sourcekitd_request_dictionary_set_int64(Req, KeyLength, Opts.Length);               \
      break;                                                                              \
    }
#include "swift/IDE/RefactoringKinds.def"

  case SourceKitRequest::MarkupToXML: {
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestMarkupToXML);
    break;
  }
  case SourceKitRequest::NameTranslation: {
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestNameTranslation);
    sourcekitd_request_dictionary_set_int64(Req, KeyOffset, ByteOffset);
    StringRef BaseName;
    llvm::SmallVector<StringRef, 4> ArgPieces;
    sourcekitd_uid_t ArgName;
    if (!Opts.SwiftName.empty()) {
      sourcekitd_request_dictionary_set_uid(Req, KeyNameKind, KindNameSwift);
      ArgName = KeyArgNames;
      StringRef Text(Opts.SwiftName);
      auto ArgStart = Text.find_first_of('(');
      if (ArgStart == StringRef::npos) {
        BaseName = Text;
      } else {
        BaseName = Text.substr(0, ArgStart);
        auto ArgEnd = Text.find_last_of(')');
        if (ArgEnd == StringRef::npos) {
          llvm::errs() << "Swift name is malformed.\n";
          return 1;
        }
        StringRef AllArgs = Text.substr(ArgStart + 1, ArgEnd - ArgStart - 1);
        AllArgs.split(ArgPieces, ':');
        if (!ArgPieces.empty()) {
          if (!ArgPieces.back().empty()) {
            llvm::errs() << "Swift name is malformed.\n";
            return 1;
          }
          ArgPieces.pop_back();
        }
      }
    } else if (!Opts.ObjCName.empty()) {
      sourcekitd_request_dictionary_set_uid(Req, KeyNameKind, KindNameObjc);
      BaseName = Opts.ObjCName;
      ArgName = KeySelectorPieces;
    } else if (!Opts.ObjCSelector.empty()) {
      sourcekitd_request_dictionary_set_uid(Req, KeyNameKind, KindNameObjc);
      StringRef Name(Opts.ObjCSelector);
      Name.split(ArgPieces, ':');
      if (ArgPieces.back().empty())
        ArgPieces.pop_back();
      ArgName = KeySelectorPieces;
    } else {
      llvm::errs() << "must specify either -swift-name or -objc-name or -objc-selector\n";
      return 1;
    }
    if (!BaseName.empty()) {
      std::string S = BaseName;
      sourcekitd_request_dictionary_set_string(Req, KeyBaseName, S.c_str());
    }
    if (!ArgPieces.empty()) {
      sourcekitd_object_t Arr = sourcekitd_request_array_create(nullptr, 0);
      for (StringRef A : ArgPieces) {
        std::string S = A;
        sourcekitd_request_array_set_string(Arr, SOURCEKITD_ARRAY_APPEND,
                                            S.c_str());
      }
      sourcekitd_request_dictionary_set_value(Req, ArgName, Arr);
    }
    break;
  }

  case SourceKitRequest::RelatedIdents:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestRelatedIdents);
    sourcekitd_request_dictionary_set_int64(Req, KeyOffset, ByteOffset);
    break;

  case SourceKitRequest::SyntaxMap:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestEditorOpen);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    sourcekitd_request_dictionary_set_int64(Req, KeyEnableSyntaxMap, true);
    sourcekitd_request_dictionary_set_int64(Req, KeyEnableStructure, false);
    sourcekitd_request_dictionary_set_uid(Req, KeySyntaxTreeTransferMode,
                                          KindSyntaxTreeOff);
    sourcekitd_request_dictionary_set_int64(Req, KeySyntacticOnly, !Opts.UsedSema);
    break;

  case SourceKitRequest::Structure:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestEditorOpen);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    sourcekitd_request_dictionary_set_int64(Req, KeyEnableSyntaxMap, false);
    sourcekitd_request_dictionary_set_int64(Req, KeyEnableStructure, true);
    sourcekitd_request_dictionary_set_uid(Req, KeySyntaxTreeTransferMode,
                                          KindSyntaxTreeOff);
    sourcekitd_request_dictionary_set_int64(Req, KeySyntacticOnly, !Opts.UsedSema);
    break;

  case SourceKitRequest::Format:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestEditorOpen);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    sourcekitd_request_dictionary_set_int64(Req, KeyEnableSyntaxMap, false);
    sourcekitd_request_dictionary_set_int64(Req, KeyEnableStructure, false);
    sourcekitd_request_dictionary_set_uid(Req, KeySyntaxTreeTransferMode,
                                          KindSyntaxTreeOff);
    sourcekitd_request_dictionary_set_int64(Req, KeySyntacticOnly, !Opts.UsedSema);
    break;

  case SourceKitRequest::ExpandPlaceholder:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestEditorOpen);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    sourcekitd_request_dictionary_set_int64(Req, KeyEnableSyntaxMap, false);
    sourcekitd_request_dictionary_set_int64(Req, KeyEnableStructure, false);
    sourcekitd_request_dictionary_set_int64(Req, KeySyntacticOnly, !Opts.UsedSema);
    break;

  case SourceKitRequest::SyntaxTree:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestEditorOpen);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    sourcekitd_request_dictionary_set_int64(Req, KeyEnableSyntaxMap, false);
    sourcekitd_request_dictionary_set_int64(Req, KeyEnableStructure, false);
    sourcekitd_request_dictionary_set_uid(Req, KeySyntaxTreeTransferMode,
                                          KindSyntaxTreeFull);
    sourcekitd_request_dictionary_set_int64(Req, KeySyntacticOnly, true);
    break;

  case SourceKitRequest::DocInfo:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestDocInfo);
    break;

  case SourceKitRequest::SemanticInfo:
    InitOpts.UsedSema = true;
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestEditorOpen);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    break;

  case SourceKitRequest::Open:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestEditorOpen);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    break;

  case SourceKitRequest::Close:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestEditorClose);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    break;

  case SourceKitRequest::Edit:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                          RequestEditorReplaceText);
    sourcekitd_request_dictionary_set_string(Req, KeyName, SemaName.c_str());
    sourcekitd_request_dictionary_set_int64(Req, KeyOffset, ByteOffset);
    sourcekitd_request_dictionary_set_int64(Req, KeyLength, Opts.Length);
    sourcekitd_request_dictionary_set_string(Req, KeySourceText,
                                       Opts.ReplaceText.getValue().c_str());
    break;

  case SourceKitRequest::PrintAnnotations:
    return printAnnotations();
  case SourceKitRequest::PrintDiags:
    return printDiags();
  case SourceKitRequest::ExtractComment:
    if (Opts.SourceFile.empty()) {
      llvm::errs() << "Missing '<source-file>' \n";
      return 1;
    }
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                          RequestEditorExtractTextFromComment);
    break;

  case SourceKitRequest::InterfaceGen:
  case SourceKitRequest::InterfaceGenOpen:
    sourcekitd_request_dictionary_set_int64(Req, KeySynthesizedExtension,
                                            Opts.SynthesizedExtensions);
    if (Opts.ModuleName.empty() && Opts.HeaderPath.empty() &&
        Opts.SourceFile.empty() && Opts.USR.empty()) {
      llvm::errs() << "Missing '-module <module name>' or '-header <path>'" <<
        "or '<source-file>' or '-usr <USR>' \n";
      return 1;
    }
    if (!Opts.ModuleName.empty()) {
      sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                            RequestEditorOpenInterface);
    } else if (!Opts.USR.empty()) {
      sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                            RequestEditorOpenSwiftTypeInterface);
    } else if (!Opts.SourceFile.empty()) {
      sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                            RequestEditorOpenSwiftSourceInterface);
    } else {
      if (Opts.UsingSwiftArgs)
          sourcekitd_request_dictionary_set_int64(Req, KeyUsingSwiftArgs, true);
      sourcekitd_request_dictionary_set_uid(Req, KeyRequest,
                                            RequestEditorOpenHeaderInterface);
    }

    sourcekitd_request_dictionary_set_string(Req, KeyName, getInterfaceGenDocumentName());
    if (!Opts.ModuleGroupName.empty())
      sourcekitd_request_dictionary_set_string(Req, KeyGroupName,
                                               Opts.ModuleGroupName.c_str());
    if (!Opts.InterestedUSR.empty())
      sourcekitd_request_dictionary_set_string(Req, KeyInterestedUSR,
                                               Opts.InterestedUSR.c_str());
    if (!Opts.USR.empty())
      sourcekitd_request_dictionary_set_string(Req, KeyUSR, Opts.USR.c_str());
    break;

  case SourceKitRequest::FindInterfaceDoc:
    if (Opts.ModuleName.empty()) {
      llvm::errs() << "Missing '-module <module name>'\n";
      return 1;
    }
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestEditorFindInterfaceDoc);
    break;

  case SourceKitRequest::FindUSR:
    if (Opts.USR.empty()) {
      llvm::errs() << "Missing '-usr <USR string>'\n";
      return 1;
    }
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestEditorFindUSR);
    sourcekitd_request_dictionary_set_string(Req, KeyUSR, Opts.USR.c_str());
    break;

  case SourceKitRequest::ModuleGroups:
    if (Opts.ModuleName.empty()) {
      llvm::errs() << "Missing '-module <module name>'\n";
      return 1;
    }
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestModuleGroups);
    break;

  case SourceKitRequest::FindLocalRenameRanges:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestFindLocalRenameRanges);
    sourcekitd_request_dictionary_set_int64(Req, KeyLine, Opts.Line);
    sourcekitd_request_dictionary_set_int64(Req, KeyColumn, Opts.Col);
    sourcekitd_request_dictionary_set_int64(Req, KeyLength, Opts.Length);
    break;

  case SourceKitRequest::SyntacticRename:
  case SourceKitRequest::FindRenameRanges: {
    if (Opts.Request == SourceKitRequest::SyntacticRename) {
      sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestSyntacticRename);
    } else {
      sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestFindRenameRanges);
    }
    if (Opts.RenameSpecPath.empty()) {
      llvm::errs() << "Missing '-rename-spec <file path>'\n";
      return 1;
    }
    auto Buffer = getBufferForFilename(Opts.RenameSpecPath)->getBuffer();
    char *Err = nullptr;
    auto RenameSpec = sourcekitd_request_create_from_yaml(Buffer.data(), &Err);
    if (!RenameSpec) {
      assert(Err);
      llvm::errs() << Err;
      free(Err);
      return 1;
    }
    sourcekitd_request_dictionary_set_value(Req, KeyRenameLocations, RenameSpec);
    break;
  }
  case SourceKitRequest::Statistics:
    sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestStatistics);
    break;
  }

  if (!SourceFile.empty()) {
    if (Opts.PassAsSourceText) {
      auto Buf = getBufferForFilename(SourceFile);
      sourcekitd_request_dictionary_set_string(Req, KeySourceText,
                                               Buf->getBufferStart());
    }
    sourcekitd_request_dictionary_set_string(Req, KeySourceFile,
                                             SourceFile.c_str());
  }

  if (Opts.SourceText) {
    sourcekitd_request_dictionary_set_string(Req, KeySourceText,
                                             Opts.SourceText->c_str());
  }

  if (!Opts.CompilerArgs.empty()) {
    sourcekitd_object_t Args = sourcekitd_request_array_create(nullptr, 0);
    for (auto Arg : Opts.CompilerArgs)
      sourcekitd_request_array_set_string(Args, SOURCEKITD_ARRAY_APPEND, Arg);
    sourcekitd_request_dictionary_set_value(Req, KeyCompilerArgs, Args);
    sourcekitd_request_release(Args);
  }

  if (!Opts.ModuleName.empty()) {
    sourcekitd_request_dictionary_set_string(Req, KeyModuleName,
                                             Opts.ModuleName.c_str());
  }
  if (!Opts.HeaderPath.empty()) {
    sourcekitd_request_dictionary_set_string(Req, KeyFilePath,
                                             Opts.HeaderPath.c_str());
  }
  if (Opts.CancelOnSubsequentRequest.hasValue()) {
    sourcekitd_request_dictionary_set_int64(Req, KeyCancelOnSubsequentRequest,
                                            *Opts.CancelOnSubsequentRequest);
  }

  if (!Opts.SwiftVersion.empty()) {
    if (Opts.PassVersionAsString) {
      sourcekitd_request_dictionary_set_string(Req, KeySwiftVersion,
                                               Opts.SwiftVersion.c_str());
    } else {
      unsigned ver;
      if (StringRef(Opts.SwiftVersion).getAsInteger(10, ver)) {
        llvm::errs() << "error: expected integer for 'swift-version'\n";
        return true;
      }
      sourcekitd_request_dictionary_set_int64(Req, KeySwiftVersion, ver);
    }
  }


  if (!Opts.isAsyncRequest) {
    sourcekitd_response_t Resp = sendRequestSync(Req, Opts);
    sourcekitd_request_release(Req);
    return handleResponse(Resp, Opts, SemaName, std::move(SourceBuf),
                          &InitOpts)
               ? 1
               : 0;
  } else {
#if SOURCEKITD_HAS_BLOCKS
    AsyncResponseInfo info;
    info.options = Opts;
    info.sourceFilename = std::move(SemaName);
    info.sourceBuffer = std::move(SourceBuf);
    unsigned respIndex = asyncResponses.size();
    asyncResponses.push_back(std::move(info));

    if (Opts.PrintRequest)
      sourcekitd_request_description_dump(Req);

    sourcekitd_send_request(Req, nullptr, ^(sourcekitd_response_t resp) {
      auto &info = asyncResponses[respIndex];
      info.response = resp;
      info.semaphore.signal(); // Ready to be handled!
    });

#else
    llvm::report_fatal_error(
        "-async not supported when sourcekitd is built without blocks support");
#endif

    sourcekitd_request_release(Req);
    return 0;
  }
}

static bool handleResponse(sourcekitd_response_t Resp, const TestOptions &Opts,
                           const std::string &SourceFile,
                           std::unique_ptr<llvm::MemoryBuffer> SourceBuf,
                           TestOptions *InitOpts) {
  bool KeepResponseAlive = false;
  bool IsError = sourcekitd_response_is_error(Resp);
  if (IsError) {
    sourcekitd_response_description_dump(Resp);

  } else if (!Opts.PrintResponse) {
    // Nothing.
  } else if (Opts.PrintResponseAsJSON) {
    sourcekitd_variant_t Info = sourcekitd_response_get_value(Resp);
    char *json = sourcekitd_variant_json_description_copy(Info);
    llvm::outs() << json << '\n';
    free(json);

  } else if (Opts.PrintRawResponse) {
    sourcekitd_response_description_dump_filedesc(Resp, STDOUT_FILENO);

  } else {
    sourcekitd_variant_t Info = sourcekitd_response_get_value(Resp);
    switch (Opts.Request) {
    case SourceKitRequest::None:
      llvm_unreachable("request should be set");
    case SourceKitRequest::PrintAnnotations:
    case SourceKitRequest::PrintDiags:
      llvm_unreachable("print-annotations/print-diags is handled elsewhere");

    case SourceKitRequest::EnableCompileNotifications:
      // Ignore the response.  If it was an error it is handled above.
      break;

    case SourceKitRequest::Open:
      getSemanticInfo(Info, SourceFile);
      KeepResponseAlive = true;
      break;

    case SourceKitRequest::Edit:
      if (Opts.Length == 0 && Opts.ReplaceText->empty()) {
        // Length=0, replace="" is a nop and will not trigger sema.
        sourcekitd_response_description_dump_filedesc(Resp, STDOUT_FILENO);
      } else {
        getSemanticInfo(Info, SourceFile);
        KeepResponseAlive = true;
      }
      break;

    case SourceKitRequest::DemangleNames:
      printDemangleResults(sourcekitd_response_get_value(Resp), outs());
      break;

    case SourceKitRequest::MangleSimpleClasses:
      printMangleResults(sourcekitd_response_get_value(Resp), outs());
      break;

    case SourceKitRequest::ProtocolVersion:
    case SourceKitRequest::Close:
    case SourceKitRequest::Index:
    case SourceKitRequest::CodeComplete:
    case SourceKitRequest::CodeCompleteOpen:
    case SourceKitRequest::CodeCompleteClose:
    case SourceKitRequest::CodeCompleteUpdate:
    case SourceKitRequest::CodeCompleteCacheOnDisk:
    case SourceKitRequest::CodeCompleteSetPopularAPI:
    case SourceKitRequest::CrashWithExit:
      sourcekitd_response_description_dump_filedesc(Resp, STDOUT_FILENO);
      break;

    case SourceKitRequest::RelatedIdents:
      printRelatedIdents(Info, SourceFile, llvm::outs());
      break;

    case SourceKitRequest::CursorInfo:
      printCursorInfo(Info, SourceFile, llvm::outs());
      break;

    case SourceKitRequest::NameTranslation:
      printNameTranslationInfo(Info, llvm::outs());
      break;

    case SourceKitRequest::RangeInfo:
      printRangeInfo(Info, SourceFile, llvm::outs());
      break;

    case SourceKitRequest::DocInfo:
      printDocInfo(Info, SourceFile);
      break;

    case SourceKitRequest::SemanticInfo:
      getSemanticInfo(Info, SourceFile);
      printSemanticInfo();
      break;

    case SourceKitRequest::InterfaceGen:
      printInterfaceGen(Info, Opts.CheckInterfaceIsASCII);
      break;

    case SourceKitRequest::ExtractComment:
    case SourceKitRequest::MarkupToXML:
      printNormalizedDocComment(Info);
      break;

    case SourceKitRequest::InterfaceGenOpen:
      // Just initialize the options for the subsequent request.
      assert(!Opts.isAsyncRequest && InitOpts &&
             "async interface-gen-open is not supported");
      InitOpts->SourceFile = getInterfaceGenDocumentName();
      InitOpts->SourceText =
          sourcekitd_variant_dictionary_get_string(Info, KeySourceText);
      break;

    case SourceKitRequest::FindInterfaceDoc:
      printFoundInterface(Info, llvm::outs());
      break;

    case SourceKitRequest::FindUSR:
      printFoundUSR(Info, SourceBuf.get(), llvm::outs());
      break;

    case SourceKitRequest::SyntaxTree: {
      // Print only the serialized syntax tree.
      llvm::outs() << sourcekitd_variant_dictionary_get_string(
        sourcekitd_response_get_value(Resp), KeySerializedSyntaxTree);
      llvm::outs() << '\n';
      break;
    }
    case SourceKitRequest::SyntaxMap:
    case SourceKitRequest::Structure:
      sourcekitd_response_description_dump_filedesc(Resp, STDOUT_FILENO);
      if (Opts.ReplaceText.hasValue()) {
        unsigned Offset = resolveFromLineCol(Opts.Line, Opts.Col, SourceFile);
        unsigned Length = Opts.Length;
        sourcekitd_object_t EdReq = sourcekitd_request_dictionary_create(nullptr,
                                                                    nullptr, 0);
        sourcekitd_request_dictionary_set_uid(EdReq, KeyRequest,
                                              RequestEditorReplaceText);
        sourcekitd_request_dictionary_set_string(EdReq, KeyName,
                                                 SourceFile.c_str());
        sourcekitd_request_dictionary_set_int64(EdReq, KeyOffset, Offset);
        sourcekitd_request_dictionary_set_int64(EdReq, KeyLength, Length);
        sourcekitd_request_dictionary_set_string(EdReq, KeySourceText,
                                           Opts.ReplaceText.getValue().c_str());
        bool EnableSyntaxMax = Opts.Request == SourceKitRequest::SyntaxMap;
        bool EnableSubStructure = Opts.Request == SourceKitRequest::Structure;
        sourcekitd_request_dictionary_set_int64(EdReq, KeyEnableSyntaxMap,
                                                EnableSyntaxMax);
        sourcekitd_request_dictionary_set_int64(EdReq, KeyEnableStructure,
                                                EnableSubStructure);
        sourcekitd_request_dictionary_set_int64(EdReq, KeySyntacticOnly,
                                                !Opts.UsedSema);

        sourcekitd_response_t EdResp = sendRequestSync(EdReq, Opts);
        sourcekitd_response_description_dump_filedesc(EdResp, STDOUT_FILENO);
        sourcekitd_response_dispose(EdResp);
        sourcekitd_request_release(EdReq);
      }
      break;

    case SourceKitRequest::Format:
      {
        sourcekitd_object_t Fmt = sourcekitd_request_dictionary_create(nullptr,
                                                                    nullptr, 0);
        sourcekitd_request_dictionary_set_uid(Fmt, KeyRequest,
                                              RequestEditorFormatText);
        sourcekitd_request_dictionary_set_string(Fmt, KeyName,
                                                 SourceFile.c_str());
        sourcekitd_request_dictionary_set_string(Fmt, KeySourceText, "");
        sourcekitd_request_dictionary_set_int64(Fmt, KeyLine, Opts.Line);
        sourcekitd_request_dictionary_set_int64(Fmt, KeyLength, Opts.Length);

        if (!Opts.RequestOptions.empty()) {
          sourcekitd_object_t FO = sourcekitd_request_dictionary_create(nullptr,
                                                                    nullptr, 0);
          for (auto &FmtOpt : Opts.RequestOptions) {
            auto KeyValue = StringRef(FmtOpt).split('=');
            std::string KeyStr("key.editor.format.");
            KeyStr.append(KeyValue.first);
            sourcekitd_uid_t Key = sourcekitd_uid_get_from_cstr(KeyStr.c_str());
            int64_t Value = 0;
            KeyValue.second.getAsInteger(0, Value);
            sourcekitd_request_dictionary_set_int64(FO, Key, Value);
          }
          sourcekitd_request_dictionary_set_value(Fmt, KeyFormatOptions, FO);
          sourcekitd_request_release(FO);
        }

        sourcekitd_response_t FmtResp = sendRequestSync(Fmt, Opts);
        sourcekitd_response_description_dump_filedesc(FmtResp, STDOUT_FILENO);
        sourcekitd_response_dispose(FmtResp);
        sourcekitd_request_release(Fmt);
      }
      break;

      case SourceKitRequest::ExpandPlaceholder:
        expandPlaceholders(SourceBuf.get(), llvm::outs());
        break;
      case SourceKitRequest::ModuleGroups:
        printModuleGroupNames(Info, llvm::outs());
        break;
#define SEMANTIC_REFACTORING(KIND, NAME, ID) case SourceKitRequest::KIND:
#include "swift/IDE/RefactoringKinds.def"
      case SourceKitRequest::SyntacticRename:
        printSyntacticRenameEdits(Info, llvm::outs());
        break;
      case SourceKitRequest::FindRenameRanges:
      case SourceKitRequest::FindLocalRenameRanges:
        printRenameRanges(Info, llvm::outs());
        break;
      case SourceKitRequest::Statistics:
        printStatistics(Info, llvm::outs());
    }
  }

  if (!KeepResponseAlive)
    sourcekitd_response_dispose(Resp);
  return IsError;
}

sourcekitd_variant_t LatestSemaAnnotations = {{0,0,0}};
sourcekitd_variant_t LatestSemaDiags = {{0,0,0}};

static void getSemanticInfoImpl(sourcekitd_variant_t Info) {
  LatestSemaAnnotations =
    sourcekitd_variant_dictionary_get_value(Info, KeyAnnotations);
  LatestSemaDiags =
    sourcekitd_variant_dictionary_get_value(Info, KeyDiagnostics);
}

static void getSemanticInfo(sourcekitd_variant_t Info, StringRef Filename) {
  getSemanticInfoImpl(Info);

  // Wait for the notification that semantic info is available.
  // But only for 1 min.
  bool expired = semaSemaphore.wait(60 * 1000);
  if (expired) {
    llvm::report_fatal_error("Never got notification for semantic info");
  }

  if (Filename != semaName){
    llvm::report_fatal_error(
      llvm::Twine("Got notification for different doc name: ") + semaName);
  }
  getSemanticInfoImpl(sourcekitd_response_get_value(semaResponse));
}

static int printAnnotations() {
  sourcekitd_variant_description_dump_filedesc(LatestSemaAnnotations,
                                               STDOUT_FILENO);
  return 0;
}

static int printDiags() {
  sourcekitd_variant_description_dump_filedesc(LatestSemaDiags, STDOUT_FILENO);
  return 0;
}

static void printSemanticInfo() {
  printAnnotations();
  if (sourcekitd_variant_get_type(LatestSemaDiags) != SOURCEKITD_VARIANT_TYPE_NULL)
    printDiags();
}

static void notification_receiver(sourcekitd_response_t resp) {
  if (sourcekitd_response_is_error(resp)) {
    sourcekitd_response_description_dump(resp);
    exit(1);
  }

  sourcekitd_variant_t payload = sourcekitd_response_get_value(resp);
  sourcekitd_uid_t note =
      sourcekitd_variant_dictionary_get_uid(payload, KeyNotification);

  if (note == NoteDocUpdate) {
    semaName = sourcekitd_variant_dictionary_get_string(payload, KeyName);

    sourcekitd_object_t edReq = sourcekitd_request_dictionary_create(nullptr,
                                                                nullptr, 0);
    sourcekitd_request_dictionary_set_uid(edReq, KeyRequest,
                                          RequestEditorReplaceText);
    sourcekitd_request_dictionary_set_string(edReq, KeyName, semaName);
    sourcekitd_request_dictionary_set_string(edReq, KeySourceText, "");
    semaResponse = sourcekitd_send_request_sync(edReq);
    sourcekitd_request_release(edReq);
    semaSemaphore.signal();
  } else if (note == NoteTest) {
    noteSyncSemaphore.signal();
  } else {
    notificationBuffer.add(resp);
  }
}

static void printNameTranslationInfo(sourcekitd_variant_t Info,
                                     llvm::raw_ostream &OS) {
  sourcekitd_uid_t KindUID = sourcekitd_variant_dictionary_get_uid(Info,
                                                                   KeyNameKind);
  if (KindUID == nullptr) {
    OS << "<empty name translation info>\n";
    return;
  }
  const char *Kind = sourcekitd_uid_get_string_ptr(KindUID);
  const char *BaseName = sourcekitd_variant_dictionary_get_string(Info,
                                                                  KeyBaseName);
  std::vector<const char *> Selectors;
  sourcekitd_variant_t SelectorsObj =
    sourcekitd_variant_dictionary_get_value(Info, KeySelectorPieces);
  for (unsigned i = 0, e = sourcekitd_variant_array_get_count(SelectorsObj);
         i != e; ++i) {
    sourcekitd_variant_t Entry =
      sourcekitd_variant_array_get_value(SelectorsObj, i);
    Selectors.push_back(sourcekitd_variant_dictionary_get_string(Entry, KeyName));
  }

  bool IsZeroArgSelector = false;
  auto IsZeroArgObj = sourcekitd_variant_dictionary_get_value(Info, KeyIsZeroArgSelector);
  if (sourcekitd_variant_get_type(IsZeroArgObj) != SOURCEKITD_VARIANT_TYPE_NULL) {
    IsZeroArgSelector = sourcekitd_variant_int64_get_value(IsZeroArgObj);
  }

  std::vector<const char *> Args;
  sourcekitd_variant_t ArgsObj =
    sourcekitd_variant_dictionary_get_value(Info, KeyArgNames);
  for (unsigned i = 0, e = sourcekitd_variant_array_get_count(ArgsObj);
       i != e; ++i) {
    sourcekitd_variant_t Entry =
      sourcekitd_variant_array_get_value(ArgsObj, i);
    Args.push_back(sourcekitd_variant_dictionary_get_string(Entry, KeyName));
  }

  OS << Kind << "\n";
  OS << StringRef(BaseName);
  if (!Args.empty()) {
    OS << "(";
    for (auto A : Args) {
      StringRef Text(A);
      if (Text.empty())
        OS << "_" << ":";
      else
        OS << Text << ":";
    }
    OS << ")";
  }
  for (auto S : Selectors) {
    OS << S;
    if (!IsZeroArgSelector) {
      OS << ":";
    }
  }
  OS << '\n';
}

static void printCursorInfo(sourcekitd_variant_t Info, StringRef FilenameIn,
                            llvm::raw_ostream &OS) {
  sourcekitd_uid_t KindUID = sourcekitd_variant_dictionary_get_uid(Info,
                                      sourcekitd_uid_get_from_cstr("key.kind"));
  if (KindUID == nullptr) {
    OS << "<empty cursor info>\n";
    return;
  }

  std::string Filename = FilenameIn;
  char full_path[MAXPATHLEN];
  if (const char *path = realpath(Filename.c_str(), full_path))
    Filename = path;

  const char *Kind = sourcekitd_uid_get_string_ptr(KindUID);
  const char *USR = sourcekitd_variant_dictionary_get_string(Info, KeyUSR);
  const char *Name = sourcekitd_variant_dictionary_get_string(Info, KeyName);
  const char *Typename = sourcekitd_variant_dictionary_get_string(Info,
                                                                  KeyTypeName);
  const char *TypeUsr = sourcekitd_variant_dictionary_get_string(Info,
                                                                 KeyTypeUsr);
  const char *ContainerTypeUsr = sourcekitd_variant_dictionary_get_string(Info,
                                                          KeyContainerTypeUsr);
  const char *ModuleName = sourcekitd_variant_dictionary_get_string(Info,
                                                              KeyModuleName);
  const char *GroupName = sourcekitd_variant_dictionary_get_string(Info,
                                                                   KeyGroupName);

  const char *LocalizationKey =
    sourcekitd_variant_dictionary_get_string(Info, KeyLocalizationKey);
  const char *ModuleInterfaceName =
      sourcekitd_variant_dictionary_get_string(Info, KeyModuleInterfaceName);
  const char *TypeInterface =
      sourcekitd_variant_dictionary_get_string(Info, KeyTypeInterface);
  bool IsSystem = sourcekitd_variant_dictionary_get_bool(Info, KeyIsSystem);
  const char *AnnotDecl = sourcekitd_variant_dictionary_get_string(Info,
                                                              KeyAnnotatedDecl);
  const char *FullAnnotDecl =
      sourcekitd_variant_dictionary_get_string(Info, KeyFullyAnnotatedDecl);
  const char *DocFullAsXML =
      sourcekitd_variant_dictionary_get_string(Info, KeyDocFullAsXML);
  sourcekitd_variant_t OffsetObj =
      sourcekitd_variant_dictionary_get_value(Info, KeyOffset);
  llvm::Optional<int64_t> Offset;
  unsigned Length = 0;
  if (sourcekitd_variant_get_type(OffsetObj) != SOURCEKITD_VARIANT_TYPE_NULL) {
    Offset = sourcekitd_variant_int64_get_value(OffsetObj);
    Length = sourcekitd_variant_dictionary_get_int64(Info, KeyLength);
  }
  const char *FilePath = sourcekitd_variant_dictionary_get_string(Info, KeyFilePath);

  std::vector<const char *> OverrideUSRs;
  sourcekitd_variant_t OverridesObj =
      sourcekitd_variant_dictionary_get_value(Info, KeyOverrides);
  for (unsigned i = 0, e = sourcekitd_variant_array_get_count(OverridesObj);
         i != e; ++i) {
    sourcekitd_variant_t Entry =
      sourcekitd_variant_array_get_value(OverridesObj, i);
    OverrideUSRs.push_back(sourcekitd_variant_dictionary_get_string(Entry, KeyUSR));
  }

  std::vector<const char *> GroupNames;
  sourcekitd_variant_t GroupObj =
    sourcekitd_variant_dictionary_get_value(Info, KeyModuleGroups);
  for (unsigned i = 0, e = sourcekitd_variant_array_get_count(GroupObj);
       i != e; ++i) {
    sourcekitd_variant_t Entry =
    sourcekitd_variant_array_get_value(GroupObj, i);
    GroupNames.push_back(sourcekitd_variant_dictionary_get_string(Entry, KeyGroupName));
  }

  std::vector<const char *> RelatedDecls;
  sourcekitd_variant_t RelatedDeclsObj =
  sourcekitd_variant_dictionary_get_value(Info, KeyRelatedDecls);
  for (unsigned i = 0, e = sourcekitd_variant_array_get_count(RelatedDeclsObj);
       i != e; ++i) {
    sourcekitd_variant_t Entry =
    sourcekitd_variant_array_get_value(RelatedDeclsObj, i);
    RelatedDecls.push_back(sourcekitd_variant_dictionary_get_string(Entry,
                                                             KeyAnnotatedDecl));
  }

  struct ActionInfo {
    const char* KindUID;
    const char* KindName;
    const char* UnavailReason;
  };
  std::vector<ActionInfo> AvailableActions;
  sourcekitd_variant_t ActionsObj =
  sourcekitd_variant_dictionary_get_value(Info, KeyRefactorActions);
  for (unsigned i = 0, e = sourcekitd_variant_array_get_count(ActionsObj);
       i != e; ++i) {
    sourcekitd_variant_t Entry =
    sourcekitd_variant_array_get_value(ActionsObj, i);
    AvailableActions.push_back({
      sourcekitd_uid_get_string_ptr(sourcekitd_variant_dictionary_get_uid(Entry,
                                                                KeyActionUID)),
      sourcekitd_variant_dictionary_get_string(Entry, KeyActionName),
      sourcekitd_variant_dictionary_get_string(Entry, KeyActionUnavailableReason)
    });
  }

  uint64_t ParentOffset =
    sourcekitd_variant_dictionary_get_int64(Info, KeyParentLoc);

  OS << Kind << " (";
  if (Offset.hasValue()) {
    if (Filename != FilePath)
      OS << FilePath << ":";
    auto LineCol = resolveToLineCol(Offset.getValue(), FilePath);
    OS << LineCol.first << ':' << LineCol.second;
    auto EndLineCol = resolveToLineCol(Offset.getValue()+Length, FilePath);
    OS << '-' << EndLineCol.first << ':' << EndLineCol.second;
  }
  OS << ")\n";
  OS << Name << '\n';
  if (USR)
    OS << USR << '\n';
  if (Typename)
    OS << Typename << '\n';
  if (TypeUsr)
    OS << TypeUsr << '\n';
  if (ContainerTypeUsr)
    OS << "<Container>" << ContainerTypeUsr << "</Container>" << '\n';
  if (ModuleName)
    OS << ModuleName << '\n';
  if (GroupName)
    OS << "<Group>" << GroupName << "</Group>" << '\n';
  if (ModuleInterfaceName)
    OS << ModuleInterfaceName << '\n';
  if (IsSystem)
    OS << "SYSTEM\n";
  if (AnnotDecl)
    OS << AnnotDecl << '\n';
  if (FullAnnotDecl)
    OS << FullAnnotDecl << '\n';
  if (DocFullAsXML)
    OS << DocFullAsXML << '\n';
  if (LocalizationKey)
    OS << "<LocalizationKey>" << LocalizationKey;
  OS << "</LocalizationKey>" << '\n';
  OS << "OVERRIDES BEGIN\n";
  for (auto OverUSR : OverrideUSRs)
    OS << OverUSR << '\n';
  OS << "OVERRIDES END\n";
  OS << "RELATED BEGIN\n";
  for (auto RelDecl : RelatedDecls)
    OS << RelDecl << '\n';
  OS << "RELATED END\n";
  OS << "TYPE INTERFACE BEGIN\n";
  if (TypeInterface)
    OS << TypeInterface << '\n';
  OS << "TYPE INTERFACE END\n";
  OS << "MODULE GROUPS BEGIN\n";
  for (auto Group : GroupNames)
    OS << Group << '\n';
  OS << "MODULE GROUPS END\n";
  OS << "ACTIONS BEGIN\n";
  for (auto Action : AvailableActions) {
    OS << Action.KindUID << '\n';
    OS << Action.KindName << '\n';
    if (Action.UnavailReason) {
      OS << Action.UnavailReason << '\n';
    }
  }
  OS << "ACTIONS END\n";
  if (ParentOffset) {
    OS << "PARENT OFFSET: " << ParentOffset << "\n";
  }
}

static void printRangeInfo(sourcekitd_variant_t Info, StringRef FilenameIn,
                            llvm::raw_ostream &OS) {
  sourcekitd_uid_t KindUID = sourcekitd_variant_dictionary_get_uid(Info,
                                      sourcekitd_uid_get_from_cstr("key.kind"));
  if (KindUID == nullptr) {
    OS << "<empty range info>\n";
    return;
  }

  std::string Filename = FilenameIn;
  char full_path[MAXPATHLEN];
  if (const char *path = realpath(Filename.c_str(), full_path))
    Filename = path;

  sourcekitd_variant_t OffsetObj =
    sourcekitd_variant_dictionary_get_value(Info, KeyOffset);
  llvm::Optional<int64_t> Offset;
  if (sourcekitd_variant_get_type(OffsetObj) != SOURCEKITD_VARIANT_TYPE_NULL) {
    Offset = sourcekitd_variant_int64_get_value(OffsetObj);
  }
  const char *Kind = sourcekitd_uid_get_string_ptr(KindUID);
  const char *Typename = sourcekitd_variant_dictionary_get_string(Info,
                                                                  KeyTypeName);

  const char *RangeContent = sourcekitd_variant_dictionary_get_string(Info,
                                                              KeyRangeContent);
  OS << "<kind>" << Kind << "</kind>\n";
  OS << "<content>" <<RangeContent << "</content>\n";
  if (Typename)
    OS << "<type>" <<Typename << "</type>\n";
}
static void printFoundInterface(sourcekitd_variant_t Info,
                                llvm::raw_ostream &OS) {
  const char *Name = sourcekitd_variant_dictionary_get_string(Info,
                                                        KeyModuleInterfaceName);
  OS << "DOC: (";
  if (Name)
    OS << Name;
  OS << ")\n";

  sourcekitd_variant_t ArgObj =
      sourcekitd_variant_dictionary_get_value(Info, KeyCompilerArgs);

  OS << "ARGS: [";
  if (sourcekitd_variant_get_type(ArgObj) != SOURCEKITD_VARIANT_TYPE_NULL) {
    for (unsigned i = 0, e = sourcekitd_variant_array_get_count(ArgObj);
           i != e; ++i) {
      OS << sourcekitd_variant_array_get_string(ArgObj, i);
      OS << ' ';
    }
  }
  OS << "]\n";
}

static void printFoundUSR(sourcekitd_variant_t Info,
                          llvm::MemoryBuffer *SourceBuf,
                          llvm::raw_ostream &OS) {
  sourcekitd_variant_t OffsetObj =
      sourcekitd_variant_dictionary_get_value(Info, KeyOffset);
  llvm::Optional<int64_t> Offset;
  if (sourcekitd_variant_get_type(OffsetObj) != SOURCEKITD_VARIANT_TYPE_NULL)
    Offset = sourcekitd_variant_int64_get_value(OffsetObj);

  if (!Offset.hasValue()) {
    OS << "USR NOT FOUND\n";
    return;
  }

  int64_t Length = sourcekitd_variant_dictionary_get_int64(Info, KeyLength);

  auto LineCol1 = resolveToLineCol(Offset.getValue(), SourceBuf);
  auto LineCol2 = resolveToLineCol(Offset.getValue() + Length, SourceBuf);
  OS << '(' << LineCol1.first << ':' << LineCol1.second << '-'
            << LineCol2.first << ':' << LineCol2.second << ")\n";
}

static void printNormalizedDocComment(sourcekitd_variant_t Info) {
  sourcekitd_variant_t Source =
    sourcekitd_variant_dictionary_get_value(Info, KeySourceText);
  sourcekitd_variant_description_dump_filedesc(Source, STDOUT_FILENO);
}

static void printDocInfo(sourcekitd_variant_t Info, StringRef Filename) {
  const char *text =
      sourcekitd_variant_dictionary_get_string(Info, KeySourceText);
  llvm::raw_fd_ostream OS(STDOUT_FILENO, /*shouldClose=*/false);
  if (text) {
    OS << text << '\n';
    OS.flush();
  }

  sourcekitd_variant_t annotations =
      sourcekitd_variant_dictionary_get_value(Info, KeyAnnotations);
  sourcekitd_variant_t entities =
      sourcekitd_variant_dictionary_get_value(Info,
                                  sourcekitd_uid_get_from_cstr("key.entities"));
  sourcekitd_variant_t diags =
      sourcekitd_variant_dictionary_get_value(Info, KeyDiagnostics);

  sourcekitd_variant_description_dump_filedesc(annotations, STDOUT_FILENO);
  sourcekitd_variant_description_dump_filedesc(entities, STDOUT_FILENO);

  if (sourcekitd_variant_get_type(diags) != SOURCEKITD_VARIANT_TYPE_NULL)
    sourcekitd_variant_description_dump_filedesc(diags, STDOUT_FILENO);
}

static void checkTextIsASCII(const char *Text) {
  for (const char *p = Text; *p; ++p) {
    if (*p & 0x80) {
      auto LineCol = resolveToLineColFromBuf(p - Text, Text);

      llvm::errs() << "!!Interface text is non-ascii!!\n"
                   << "@ " << LineCol.first << ":" << LineCol.second << "\n";
      exit(1);
    }
  }
}

static void printModuleGroupNames(sourcekitd_variant_t Info,
                                  llvm::raw_ostream &OS) {
  sourcekitd_variant_t Groups =
    sourcekitd_variant_dictionary_get_value(Info, KeyModuleGroups);
  OS << "<GROUPS>\n";
  for (unsigned i = 0, e = sourcekitd_variant_array_get_count(Groups);
       i != e; ++i) {
    sourcekitd_variant_t Entry =
    sourcekitd_variant_array_get_value(Groups, i);
    OS << sourcekitd_variant_dictionary_get_string(Entry, KeyGroupName) << "\n";
  }
  OS << "<\\GROUPS>\n";
}

static StringRef getLineColRange(StringRef Text, int64_t StartLine,
                                 int64_t StartCol, int64_t EndLine,
                                 int64_t EndCol) {
  unsigned Line = 1;
  size_t Length = 0;

  while (Line < StartLine) {
    Text = Text.split('\n').second;
    ++Line;
  }
  Text = Text.drop_front(StartCol - 1);
  if (StartLine == EndLine)
    return Text.substr(0, EndCol - StartCol);

  while(Line < EndLine) {
    Length = Text.find('\n', Length) + 1;
    ++Line;
  }
  Length += EndCol - 1;

  return Text.substr(0, Length);
}

static void printSyntacticRenameEdits(sourcekitd_variant_t Info,
                                      llvm::raw_ostream &OS) {
  sourcekitd_variant_t CategorizedEdits =
    sourcekitd_variant_dictionary_get_value(Info, KeyCategorizedEdits);
  for (unsigned i = 0, e = sourcekitd_variant_array_get_count(CategorizedEdits);
       i != e; ++i) {
    sourcekitd_variant_t
    Categorized = sourcekitd_variant_array_get_value(CategorizedEdits, i);
    sourcekitd_uid_t
    Category = sourcekitd_variant_dictionary_get_uid(Categorized, KeyCategory);
    OS << sourcekitd_uid_get_string_ptr(Category) << ":\n";

    sourcekitd_variant_t Edits =
    sourcekitd_variant_dictionary_get_value(Categorized, KeyEdits);
    for(unsigned j = 0, je = sourcekitd_variant_array_get_count(Edits);
        j != je; ++j) {
      OS << "  "; // indent
      sourcekitd_variant_t Edit = sourcekitd_variant_array_get_value(Edits, j);
      int64_t Line = sourcekitd_variant_dictionary_get_int64(Edit, KeyLine);
      int64_t Column = sourcekitd_variant_dictionary_get_int64(Edit, KeyColumn);
      int64_t EndLine = sourcekitd_variant_dictionary_get_int64(Edit, KeyEndLine);
      int64_t EndColumn = sourcekitd_variant_dictionary_get_int64(Edit, KeyEndColumn);
      OS << Line << ':' << Column << '-' << EndLine << ':' << EndColumn << " \"";
      StringRef Text(sourcekitd_variant_dictionary_get_string(Edit, KeyText));
      OS << Text << "\"\n";
      sourcekitd_variant_t NoteRanges =
        sourcekitd_variant_dictionary_get_value(Edit, KeyRangesWorthNote);
      if (unsigned e = sourcekitd_variant_array_get_count(NoteRanges)) {
        for (unsigned i = 0; i != e; ++i) {
          OS << "  <note>";
          sourcekitd_variant_t Note =
            sourcekitd_variant_array_get_value(NoteRanges, i);
          int64_t Line = sourcekitd_variant_dictionary_get_int64(Note, KeyLine);
          int64_t Column = sourcekitd_variant_dictionary_get_int64(Note, KeyColumn);
          int64_t EndLine = sourcekitd_variant_dictionary_get_int64(Note, KeyEndLine);
          int64_t EndColumn = sourcekitd_variant_dictionary_get_int64(Note, KeyEndColumn);
          auto index = sourcekitd_variant_dictionary_get_value(Note, KeyArgIndex);
          sourcekitd_uid_t Kind = sourcekitd_variant_dictionary_get_uid(Note,
                                                              KeyKind);
          StringRef NoteText = getLineColRange(Text, Line, Column, EndLine, EndColumn);
          OS << sourcekitd_uid_get_string_ptr(Kind) << " ";
          OS << Line << ":" << Column << "-" << EndLine << ":" << EndColumn;
          if (sourcekitd_variant_get_type(index) != SOURCEKITD_VARIANT_TYPE_NULL) {
            OS << " arg-index=" << sourcekitd_variant_int64_get_value(index);
          }
          OS << " \"" << NoteText << "\"";
          OS << "</note>\n";
        }
      }
    }
  }
}

static void printRenameRanges(sourcekitd_variant_t Info,
                              llvm::raw_ostream &OS) {
  sourcekitd_variant_t CategorizedRanges =
      sourcekitd_variant_dictionary_get_value(Info, KeyCategorizedRanges);
  sourcekitd_variant_array_apply(CategorizedRanges, ^bool(
                                     size_t i,
                                     sourcekitd_variant_t Categorized) {
    sourcekitd_uid_t Category =
        sourcekitd_variant_dictionary_get_uid(Categorized, KeyCategory);
    OS << sourcekitd_uid_get_string_ptr(Category) << ":\n";

    sourcekitd_variant_t Ranges =
        sourcekitd_variant_dictionary_get_value(Categorized, KeyRanges);
    sourcekitd_variant_array_apply(Ranges, ^bool(size_t j,
                                                 sourcekitd_variant_t Range) {

      OS << "  "; // indent
      int64_t Line = sourcekitd_variant_dictionary_get_int64(Range, KeyLine);
      int64_t Column =
          sourcekitd_variant_dictionary_get_int64(Range, KeyColumn);
      int64_t EndLine =
          sourcekitd_variant_dictionary_get_int64(Range, KeyEndLine);
      int64_t EndColumn =
          sourcekitd_variant_dictionary_get_int64(Range, KeyEndColumn);
      OS << Line << ':' << Column << '-' << EndLine << ':' << EndColumn << " ";
      auto Kind = sourcekitd_variant_dictionary_get_uid(Range, KeyKind);
      OS << sourcekitd_uid_get_string_ptr(Kind);
      auto index = sourcekitd_variant_dictionary_get_value(Range, KeyArgIndex);
      if (sourcekitd_variant_get_type(index) != SOURCEKITD_VARIANT_TYPE_NULL) {
        OS << " arg-index=" << sourcekitd_variant_int64_get_value(index);
      }
      OS << "\n";
      return true;
    });
    return true;
  });
}

static void printInterfaceGen(sourcekitd_variant_t Info, bool CheckASCII) {
  const char *text =
      sourcekitd_variant_dictionary_get_string(Info, KeySourceText);

  if (text) {
    llvm::raw_fd_ostream OS(STDOUT_FILENO, /*shouldClose=*/false);
    OS << text << '\n';
  }

  if (CheckASCII) {
    checkTextIsASCII(text);
  }

  sourcekitd_variant_t syntaxmap =
      sourcekitd_variant_dictionary_get_value(Info, KeySyntaxMap);
  sourcekitd_variant_description_dump_filedesc(syntaxmap, STDOUT_FILENO);
  sourcekitd_variant_t annotations =
      sourcekitd_variant_dictionary_get_value(Info, KeyAnnotations);
  sourcekitd_variant_description_dump_filedesc(annotations, STDOUT_FILENO);
  sourcekitd_variant_t structure =
      sourcekitd_variant_dictionary_get_value(Info, KeySubStructure);
  sourcekitd_variant_description_dump_filedesc(structure, STDOUT_FILENO);
}

static void printRelatedIdents(sourcekitd_variant_t Info, StringRef Filename,
                               llvm::raw_ostream &OS) {
  OS << "START RANGES\n";
  sourcekitd_variant_t Res =
      sourcekitd_variant_dictionary_get_value(Info, KeyResults);
  for (unsigned i=0, e = sourcekitd_variant_array_get_count(Res); i != e; ++i) {
    sourcekitd_variant_t Range = sourcekitd_variant_array_get_value(Res, i);
    int64_t Offset = sourcekitd_variant_dictionary_get_int64(Range, KeyOffset);
    int64_t Length = sourcekitd_variant_dictionary_get_int64(Range, KeyLength);
    auto LineCol = resolveToLineCol(Offset, Filename);
    OS << LineCol.first << ':' << LineCol.second << " - " << Length << '\n';
  }
  OS << "END RANGES\n";
}

static void prepareDemangleRequest(sourcekitd_object_t Req,
                                   const TestOptions &Opts) {
  sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestDemangle);
  if (Opts.SimplifiedDemangling)
    sourcekitd_request_dictionary_set_int64(Req, KeySimplified, 1);
  sourcekitd_object_t arr = sourcekitd_request_array_create(nullptr, 0);
  sourcekitd_request_dictionary_set_value(Req, KeyNames, arr);

  auto addName = [&](StringRef MangledName) {
    sourcekitd_request_array_set_stringbuf(arr, SOURCEKITD_ARRAY_APPEND,
                                        MangledName.data(), MangledName.size());
  };

  if (Opts.Inputs.empty()) {
    auto input = llvm::MemoryBuffer::getSTDIN();
    if (!input) {
      llvm::errs() << input.getError().message() << '\n';
      ::exit(1);
    }
    llvm::StringRef inputContents = input.get()->getBuffer();

    // This doesn't handle Unicode symbols, but maybe that's okay.
    // Also accept the future mangling prefix.
    llvm::Regex maybeSymbol("(_T|_?\\$[Ss])[_a-zA-Z0-9$.]+");
    llvm::SmallVector<llvm::StringRef, 1> matches;
    while (maybeSymbol.match(inputContents, &matches)) {
      addName(matches.front());
      auto offset = matches.front().data() - inputContents.data();
      inputContents = inputContents.substr(offset + matches.front().size());
    }

  } else {
    for (llvm::StringRef name : Opts.Inputs) {
      addName(name);
    }
  }

  sourcekitd_request_release(arr);
}

static void prepareMangleRequest(sourcekitd_object_t Req,
                                 const TestOptions &Opts) {
  sourcekitd_request_dictionary_set_uid(Req, KeyRequest, RequestMangleSimpleClass);
  sourcekitd_object_t arr = sourcekitd_request_array_create(nullptr, 0);
  sourcekitd_request_dictionary_set_value(Req, KeyNames, arr);

  auto addPair = [&](StringRef ModuleName, StringRef ClassName) {
    sourcekitd_object_t pair =
      sourcekitd_request_dictionary_create(nullptr, nullptr, 0);
    sourcekitd_request_dictionary_set_stringbuf(pair, KeyModuleName,
                                          ModuleName.data(), ModuleName.size());
    sourcekitd_request_dictionary_set_stringbuf(pair, KeyName,
                                            ClassName.data(), ClassName.size());
    sourcekitd_request_array_set_value(arr, SOURCEKITD_ARRAY_APPEND, pair);
    sourcekitd_request_release(pair);
  };

  for (StringRef pair : Opts.Inputs) {
    auto Idx = pair.find('.');
    if (Idx == StringRef::npos) {
      errs() << "expected pairs with format '<module>.<class name>'\n";
      ::exit(1);
    }
    StringRef moduleName = pair.substr(0, Idx);
    StringRef className = pair.substr(Idx+1);
    addPair(moduleName, className);
  }

  sourcekitd_request_release(arr);
}

static void printDemangleResults(sourcekitd_variant_t Info, raw_ostream &OS) {
  OS << "START DEMANGLE\n";
  sourcekitd_variant_t results =
    sourcekitd_variant_dictionary_get_value(Info, KeyResults);
  sourcekitd_variant_array_apply(results, ^bool(size_t index, sourcekitd_variant_t value) {
    StringRef name = sourcekitd_variant_dictionary_get_string(value, KeyName);
    if (name.empty())
      OS << "<empty>";
    else
      OS << name;
    OS << '\n';
    return true;
  });
  OS << "END DEMANGLE\n";
}

static void printMangleResults(sourcekitd_variant_t Info, raw_ostream &OS) {
  OS << "START MANGLE\n";
  sourcekitd_variant_t results =
    sourcekitd_variant_dictionary_get_value(Info, KeyResults);
  sourcekitd_variant_array_apply(results, ^bool(size_t index, sourcekitd_variant_t value) {
    StringRef name = sourcekitd_variant_dictionary_get_string(value, KeyName);
    if (name.empty())
      OS << "<empty>";
    else
      OS << name;
    OS << '\n';
    return true;
  });
  OS << "END MANGLE\n";
}

static void printStatistics(sourcekitd_variant_t Info, raw_ostream &OS) {
  sourcekitd_variant_t results =
    sourcekitd_variant_dictionary_get_value(Info, KeyResults);
  sourcekitd_variant_array_apply(results, ^bool(size_t index, sourcekitd_variant_t value) {
    auto uid = sourcekitd_variant_dictionary_get_uid(value, KeyKind);
    auto desc = sourcekitd_variant_dictionary_get_string(value, KeyDescription);
    auto statValue = sourcekitd_variant_dictionary_get_int64(value, KeyValue);
    OS << statValue << "\t" << desc << "\t- " << sourcekitd_uid_get_string_ptr(uid) << "\n";
    return true;
  });
}

static void initializeRewriteBuffer(StringRef Input,
                                    clang::RewriteBuffer &RewriteBuf) {
  RewriteBuf.Initialize(Input);
  StringRef CheckStr = "CHECK";
  size_t Pos = 0;
  while (true) {
    Pos = Input.find(CheckStr, Pos);
    if (Pos == StringRef::npos)
      break;
    Pos = Input.substr(0, Pos).rfind("//");
    assert(Pos != StringRef::npos);
    size_t EndLine = Input.find('\n', Pos);
    assert(EndLine != StringRef::npos);
    ++EndLine;
    RewriteBuf.RemoveText(Pos, EndLine-Pos);
    Pos = EndLine;
  }
}

static std::vector<std::pair<unsigned, unsigned>>
getPlaceholderRanges(StringRef Source) {
  const char *StartPtr = Source.data();
  std::vector<std::pair<unsigned, unsigned>> Ranges;
  while (true) {
    size_t Pos = Source.find("<#");
    if (Pos == StringRef::npos)
      break;
    unsigned OffsetStart = Source.data() + Pos - StartPtr;
    Source = Source.substr(Pos+2);
    Pos = Source.find("#>");
    if (Pos == StringRef::npos)
      break;
    unsigned OffsetEnd = Source.data() + Pos + 2 - StartPtr;
    Source = Source.substr(Pos+2);
    Ranges.emplace_back(OffsetStart, OffsetEnd-OffsetStart);
  }
  return Ranges;
}

static void expandPlaceholders(llvm::MemoryBuffer *SourceBuf,
                               llvm::raw_ostream &OS) {
  clang::RewriteBuffer RewriteBuf;
  initializeRewriteBuffer(SourceBuf->getBuffer(), RewriteBuf);
  auto Ranges = getPlaceholderRanges(SourceBuf->getBuffer());
  for (auto Range : Ranges) {
    unsigned Offset = Range.first;
    unsigned Length = Range.second;
    sourcekitd_object_t Exp = sourcekitd_request_dictionary_create(nullptr,
                                                                   nullptr, 0);
    sourcekitd_request_dictionary_set_uid(Exp, KeyRequest,
                                          RequestEditorExpandPlaceholder);
    auto SourceBufID = SourceBuf->getBufferIdentifier();
    sourcekitd_request_dictionary_set_stringbuf(Exp, KeyName,
                                                SourceBufID.data(),
                                                SourceBufID.size());
    sourcekitd_request_dictionary_set_string(Exp, KeySourceText, "");
    sourcekitd_request_dictionary_set_int64(Exp, KeyOffset, Offset);
    sourcekitd_request_dictionary_set_int64(Exp, KeyLength, Length);

    sourcekitd_response_t Resp = sourcekitd_send_request_sync(Exp);
    if (sourcekitd_response_is_error(Resp)) {
      sourcekitd_response_description_dump(Resp);
      exit(1);
    }
    sourcekitd_request_release(Exp);
    sourcekitd_variant_t Info = sourcekitd_response_get_value(Resp);
    const char *Text = sourcekitd_variant_dictionary_get_string(Info, KeySourceText);
    if (!Text) {
      sourcekitd_response_dispose(Resp);
      continue;
    }
    unsigned EditOffset = sourcekitd_variant_dictionary_get_int64(Info, KeyOffset);
    unsigned EditLength = sourcekitd_variant_dictionary_get_int64(Info, KeyLength);
    RewriteBuf.ReplaceText(EditOffset, EditLength, Text);
    sourcekitd_response_dispose(Resp);
  }

  RewriteBuf.write(OS);
}

static std::pair<unsigned, unsigned>
resolveToLineCol(unsigned Offset, StringRef Filename) {
  return resolveToLineCol(Offset, getBufferForFilename(Filename));
}

static std::pair<unsigned, unsigned>
resolveToLineCol(unsigned Offset, llvm::MemoryBuffer *InputBuf) {
  if (Offset >= InputBuf->getBufferSize()) {
    llvm::errs() << "offset " << Offset << " for filename '"
        << InputBuf->getBufferIdentifier() << "' is too large\n";
    exit(1);
  }
  return resolveToLineColFromBuf(Offset, InputBuf->getBufferStart());
}

static std::pair<unsigned, unsigned>
resolveToLineColFromBuf(unsigned Offset, const char *Ptr) {
  const char *End = Ptr+Offset;

  unsigned Line = 1;
  const char *LineStart = Ptr;
  for (; Ptr < End; ++Ptr) {
    if (*Ptr == '\n') {
      ++Line;
      LineStart = Ptr+1;
    }
  }
  unsigned Col = Ptr-LineStart + 1;

  return { Line, Col };
}

static unsigned resolveFromLineCol(unsigned Line, unsigned Col,
                                   StringRef Filename) {
  return resolveFromLineCol(Line, Col, getBufferForFilename(Filename));
}

static unsigned resolveFromLineCol(unsigned Line, unsigned Col,
                                   llvm::MemoryBuffer *InputBuf) {
  if (Line == 0 || Col == 0) {
    llvm::errs() << "wrong pos format, line/col should start from 1\n";
    exit(1);
  }

  const char *Ptr = InputBuf->getBufferStart();
  const char *End = InputBuf->getBufferEnd();
  const char *LineStart = Ptr;
  --Line;
  for (; Line && (Ptr < End); ++Ptr) {
    if (*Ptr == '\n') {
      --Line;
      LineStart = Ptr+1;
    }
  }
  if (Line != 0) {
    llvm::errs() << "wrong pos format, line too large\n";
    exit(1);
  }
  Ptr = LineStart;
  for (; Ptr < End; ++Ptr) {
    --Col;
    if (Col == 0)
      return Ptr - InputBuf->getBufferStart();
    if (*Ptr == '\n')
      break;
  }

  llvm::errs() << "wrong pos format, column too large\n";
  exit(1);
}

static llvm::StringMap<llvm::MemoryBuffer*> Buffers;

static llvm::MemoryBuffer *getBufferForFilename(StringRef Filename) {
  auto It = Buffers.find(Filename);
  if (It != Buffers.end())
    return It->second;

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
    llvm::MemoryBuffer::getFile(Filename);
  if (!FileBufOrErr) {
    llvm::errs() << "error opening input file '" << Filename << "' ("
                 << FileBufOrErr.getError().message() << ")\n";
    exit(1);
  }

  return Buffers[Filename] = FileBufOrErr.get().release();
}
