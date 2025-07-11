//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "CodeCompletionSwiftInterop.h"
#include "SourceKit/Core/Context.h"
#include "sourcekitd/sourcekitdInProc-Internal.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Driver/FrontendUtil.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/IDE/CodeCompletionCache.h"
#include "swift/IDE/CodeCompletionResultPrinter.h"
#include "swift/IDE/FuzzyStringMatcher.h"
#include "swift/IDE/Utils.h"
#include "swift/IDETool/CompilerInvocation.h"
#include "swift/IDETool/IDEInspectionInstance.h"
#include "llvm/Support/Signals.h"
#include <mutex>

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <dlfcn.h>
#endif

using namespace swift;
using namespace swift::ide;
using namespace sourcekitdInProc;

static std::string getRuntimeResourcesPath();

using FileSystemRef = llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem>;

namespace {
struct CompletionResult;

class SharedStringInMemoryFS : public llvm::vfs::InMemoryFileSystem {
  SmallVector<std::shared_ptr<std::string>, 8> strings;

public:
  SharedStringInMemoryFS(
      const llvm::StringMap<std::shared_ptr<std::string>> &files) {
    strings.reserve(files.size());
    for (auto &pair : files) {
      strings.push_back(pair.getValue());
      auto buffer =
          llvm::MemoryBuffer::getMemBuffer(*pair.getValue(), pair.getKey());
      if (!addFile(pair.getKey(), /*ModTime=*/0, std::move(buffer))) {
        // FIXME: report error!
      }
    }
  }
};

class Connection {
  IDEInspectionInstance *ideInspectionInstance;
  /// If the connection was not passed an `IDEInspectionInstance` it creates
  /// its own. This unique_ptr scopes the lifetime of 'ideInspectionInstance'
  /// to the lifetime of 'Connection'.
  std::unique_ptr<IDEInspectionInstance> ownedIDEInspectionInstance;
  llvm::StringMap<std::shared_ptr<std::string>> modifiedFiles;
  std::shared_ptr<CodeCompletionCache> completionCache;
  std::string swiftExecutablePath;
  std::string runtimeResourcePath;
  std::shared_ptr<SourceKit::RequestTracker> requestTracker;

public:
  std::unique_ptr<CompletionResult> currentResponse;
  const time_t sessionTimestamp;

  Connection(IDEInspectionInstance *ideInspectionInstance)
      : ideInspectionInstance(ideInspectionInstance),
        completionCache(std::make_shared<CodeCompletionCache>()),
        swiftExecutablePath(getSwiftExecutablePath()),
        runtimeResourcePath(getRuntimeResourcesPath()),
        requestTracker(new SourceKit::RequestTracker()),
        sessionTimestamp(llvm::sys::toTimeT(std::chrono::system_clock::now())) {
    if (ideInspectionInstance == nullptr) {
      this->ownedIDEInspectionInstance.reset(new IDEInspectionInstance());
      this->ideInspectionInstance = this->ownedIDEInspectionInstance.get();
    }
  }

  void setFileContents(StringRef path, const char *contents) {
    if (contents) {
      modifiedFiles[path] = std::make_shared<std::string>(contents);
    } else {
      modifiedFiles.erase(path);
    }
  }

  std::shared_ptr<CodeCompletionCache> getCompletionCache() const {
    return completionCache;
  }

  FileSystemRef createFileSystem() {
    auto *overlayFS =
        new llvm::vfs::OverlayFileSystem(llvm::vfs::getRealFileSystem());
    overlayFS->pushOverlay(new SharedStringInMemoryFS(modifiedFiles));
    return overlayFS;
  }

  void cancelRequest(SourceKit::SourceKitCancellationToken cancellationToken) {
    requestTracker->cancel(cancellationToken);
  }

  void codeComplete(
      StringRef path, unsigned offset, ArrayRef<const char *> args,
      FileSystemRef fileSystem, ide::CodeCompletionContext &completionContext,
      SourceKit::SourceKitCancellationToken cancellationToken,
      llvm::function_ref<void(CancellableResult<CodeCompleteResult>)> callback);

  void markCachedCompilerInstanceShouldBeInvalidated() {
    ideInspectionInstance->markCachedCompilerInstanceShouldBeInvalidated();
  }
};

} // anonymous namespace

swiftide_connection_t swiftide_connection_create(void) {
  return swiftide_connection_create_with_inspection_instance(nullptr);
}

swiftide_connection_t swiftide_connection_create_with_inspection_instance(
    void *opaqueIDESwiftInspectionInstance) {
  static std::once_flag once;
  std::call_once(
      once, [] { llvm::sys::PrintStackTraceOnErrorSignal("IDESwiftInterop"); });
  IDEInspectionInstance *inspectInstance =
      static_cast<IDEInspectionInstance *>(opaqueIDESwiftInspectionInstance);

  return static_cast<swiftide_connection_t>(new Connection(inspectInstance));
}

void swiftide_connection_dispose(swiftide_connection_t conn) {
  assert(conn);
  delete static_cast<Connection *>(conn);
}

void swiftide_connection_mark_cached_compiler_instance_should_be_invalidated(
    swiftide_connection_t _conn, swiftide_cache_invalidation_options_t _opts) {
  auto *conn = static_cast<Connection *>(_conn);
  // '_opts' is not used at this point.
  assert(conn);
  conn->markCachedCompilerInstanceShouldBeInvalidated();
}

void swiftide_set_file_contents(swiftide_connection_t _conn, const char *path,
                                const char *contents) {
  auto *conn = static_cast<Connection *>(_conn);
  assert(conn);
  conn->setFileContents(path, contents);
}

namespace {
struct CompletionResult {
  Connection &conn;
  CodeCompletionContext context;
  ImportDepth importDepth;
  std::string error;
  bool isCancelled = false;
  SmallString<0> scratch;
  CodeCompletionResultSink resultSink;
  /// The compiler instance that produced the results. Used to lazily compute
  /// diagnostics for results.
  std::shared_ptr<CompilerInstance> compilerInstance;

  CompletionResult(Connection &conn)
      : conn(conn), context(*conn.getCompletionCache()) {
    // Pre-allocate a whole page. Empirically, this is enough to cover the vast
    // majority of cases.
    scratch.reserve(4096);
  }

  bool hasError() const { return !error.empty(); }

  ArrayRef<CodeCompletionResult *> getCompletions() {
    return resultSink.Results;
  }
};

struct SwiftInteropCodeCompletionConsumer : public ide::CodeCompletionConsumer {
  CompletionResult &result;

  SwiftInteropCodeCompletionConsumer(CompletionResult &result)
      : result(result) {}

  void handleResults(CodeCompletionContext &context) override {
    assert(&context == &(result.context));
  }
};

struct CompletionRequest {
  llvm::BumpPtrAllocator allocator;
  StringRef path;
  unsigned offset;
  std::vector<const char *> compilerArguments;
  bool annotateResult = false;
  bool includeObjectLiterals = true;
  bool addInitsToTopLevel = false;
  bool addCallWithNoDefaultArgs = true;
  bool verifyUSRToDecl = false;

  CompletionRequest(const char *path, unsigned offset,
                    ArrayRef<const char *> args) {
    this->path = StringRef(path).copy(allocator);
    this->offset = offset;
    compilerArguments.reserve(args.size());
    for (const char *arg : args) {
      compilerArguments.push_back(copyCString(arg, allocator));
    }
  }
};

} // namespace

void swiftide_cancel_request(swiftide_connection_t _conn,
                             swiftide_request_handle_t handle) {
  assert(_conn);
  auto *conn = static_cast<Connection *>(_conn);
  conn->cancelRequest(handle);
}

swiftide_completion_request_t
swiftide_completion_request_create(const char *path, uint32_t offset,
                                   char *const *const compiler_args,
                                   uint32_t num_compiler_args) {

  return new CompletionRequest(
      path, offset, llvm::ArrayRef(compiler_args, num_compiler_args));
}

void swiftide_completion_request_dispose(swiftide_completion_request_t _req) {
  delete static_cast<CompletionRequest *>(_req);
}

void swiftide_completion_request_set_annotate_result(
    swiftide_completion_request_t _req, bool annotate) {
  auto &req = *static_cast<CompletionRequest *>(_req);
  req.annotateResult = annotate;
}

void swiftide_completion_request_set_include_objectliterals(
    swiftide_completion_request_t _req, bool flag) {
  auto &req = *static_cast<CompletionRequest *>(_req);
  req.includeObjectLiterals = flag;
}

void swiftide_completion_request_set_add_inits_to_top_level(
    swiftide_completion_request_t _req, bool flag) {
  auto &req = *static_cast<CompletionRequest *>(_req);
  req.addInitsToTopLevel = flag;
}

void swiftide_completion_request_set_add_call_with_no_default_args(
    swiftide_completion_request_t _req, bool flag) {
  auto &req = *static_cast<CompletionRequest *>(_req);
  req.addCallWithNoDefaultArgs = flag;
}

void swiftide_completion_request_set_verify_usr_to_decl(
    swiftide_completion_request_t _req, bool flag) {
  auto &req = *static_cast<CompletionRequest *>(_req);
  req.verifyUSRToDecl = flag;
}

swiftide_completion_response_t
swiftide_complete_cancellable(swiftide_connection_t _conn,
                              swiftide_completion_request_t _req,
                              swiftide_request_handle_t handle) {
  assert(_conn && _req);
  auto *conn = static_cast<Connection *>(_conn);
  auto &req = *static_cast<CompletionRequest *>(_req);

  if (conn->currentResponse) {
    llvm::report_fatal_error(
        "must dispose of previous response before completing again");
  }

  conn->currentResponse = std::make_unique<CompletionResult>(*conn);
  auto result = conn->currentResponse.get();

  SwiftInteropCodeCompletionConsumer consumer(*result);

  auto fileSystem = conn->createFileSystem();

  result->context.setAnnotateResult(req.annotateResult);
  result->context.setIncludeObjectLiterals(req.includeObjectLiterals);
  result->context.setAddInitsToTopLevel(req.addInitsToTopLevel);
  result->context.setAddCallWithNoDefaultArgs(req.addCallWithNoDefaultArgs);
  result->context.setVerifyUSRToDecl(req.verifyUSRToDecl);

  conn->codeComplete(
      req.path, req.offset, req.compilerArguments, fileSystem, result->context,
      handle, [&result](CancellableResult<CodeCompleteResult> completeResult) {
        switch (completeResult.getKind()) {
        case CancellableResultKind::Success:
          result->importDepth = completeResult->ImportDep;
          result->resultSink = completeResult->ResultSink;
          result->compilerInstance = completeResult->Info.compilerInstance;
          break;
        case CancellableResultKind::Failure:
          result->error = completeResult.getError();
          break;
        case CancellableResultKind::Cancelled:
          result->isCancelled = true;
          break;
        }
      });

  return static_cast<swiftide_completion_response_t>(result);
}

swiftide_completion_response_t
swiftide_complete(swiftide_connection_t _conn,
                  swiftide_completion_request_t _req) {
  // handle = nullptr indicates that the request can't be cancelled.
  return swiftide_complete_cancellable(_conn, _req, /*handle=*/nullptr);
}

void Connection::codeComplete(
    StringRef path, unsigned offset, ArrayRef<const char *> args,
    FileSystemRef fileSystem, ide::CodeCompletionContext &completionContext,
    SourceKit::SourceKitCancellationToken cancellationToken,
    llvm::function_ref<void(CancellableResult<CodeCompleteResult>)> callback) {
  using ResultType = CancellableResult<CodeCompleteResult>;
  // Resolve symlinks for the input file; we resolve them for the input files
  // in the arguments as well.
  // FIXME: We need the Swift equivalent of Clang's FileEntry.
  llvm::SmallString<128> bufferIdentifier;
  if (auto err = fileSystem->getRealPath(path, bufferIdentifier))
    bufferIdentifier = path;

  auto inputFile = fileSystem->openFileForRead(path);
  if (auto err = inputFile.getError()) {
    std::string error;
    llvm::raw_string_ostream OS(error);
    OS << "failed to open '" << path << "': " << err.message();
    callback(ResultType::failure(error));
    return;
  }

  auto inputBuffer = inputFile->get()->getBuffer(bufferIdentifier);
  if (auto err = inputBuffer.getError()) {
    std::string error;
    llvm::raw_string_ostream OS(error);
    OS << "failed to read '" << path << "': " << err.message();
    callback(ResultType::failure(error));
    return;
  }

  // Create a buffer for code completion. This contains '\0' at 'Offset'
  // position of 'UnresolvedInputFile' buffer.
  auto newBuffer = ide::makeCodeCompletionMemoryBuffer(
      inputBuffer.get().get(), offset, bufferIdentifier);

  CompilerInvocation invocation;
  SourceManager SM;
  DiagnosticEngine diags(SM);
  ForwardingDiagnosticConsumer ciDiags(diags);
  PrintingDiagnosticConsumer printDiags;
  diags.addConsumer(printDiags);

  std::string compilerInvocationError;
  bool creatingInvocationFailed = initCompilerInvocation(
      invocation, args, FrontendOptions::ActionType::Typecheck, diags, path,
      fileSystem, swiftExecutablePath, runtimeResourcePath, sessionTimestamp,
      compilerInvocationError);
  if (creatingInvocationFailed) {
    callback(ResultType::failure(compilerInvocationError));
    return;
  } else if (!invocation.getFrontendOptions().InputsAndOutputs.hasInputs()) {
    callback(ResultType::failure("no input filenames specified"));
    return;
  }
  auto cancellationFlag = std::make_shared<std::atomic<bool>>(false);
  requestTracker->setCancellationHandler(
      cancellationToken, [cancellationFlag]() {
        cancellationFlag->store(true, std::memory_order_relaxed);
      });

  ideInspectionInstance->codeComplete(
      invocation, args, fileSystem, newBuffer.get(), offset, &ciDiags,
      completionContext, cancellationFlag, callback);
}

void swiftide_completion_result_dispose(swiftide_completion_response_t result) {
  auto *response = static_cast<CompletionResult *>(result);
  auto &conn = response->conn;
  assert(conn.currentResponse.get() == response);
  conn.currentResponse = nullptr;
}

bool swiftide_completion_result_is_error(
    swiftide_completion_response_t _result) {
  auto &result = *static_cast<CompletionResult *>(_result);
  return result.hasError();
}

const char *swiftide_completion_result_get_error_description(
    swiftide_completion_response_t _result) {
  auto &result = *static_cast<CompletionResult *>(_result);
  return result.error.c_str();
}

bool swiftide_completion_result_is_cancelled(
    swiftide_completion_response_t _result) {
  auto result = static_cast<CompletionResult *>(_result);
  return result->isCancelled;
}

/// Copies a string representation of the completion result. This string should
/// be disposed of with \c free when done.
const char *swiftide_completion_result_description_copy(
    swiftide_completion_response_t _result) {
  auto &result = *static_cast<CompletionResult *>(_result);
  std::string desc;
  do {
    llvm::raw_string_ostream OS(desc);
    if (result.hasError()) {
      OS << "error: " << result.error;
      break;
    }

    /// FXIME: this code copied from PrintingCodeCompletionConsumer
    OS << "Begin completions, " << result.getCompletions().size() << " items\n";
    for (auto *item : result.getCompletions()) {
      item->printPrefix(OS);
      if (result.context.getAnnotateResult()) {
        printCodeCompletionResultDescriptionAnnotated(
            *item, OS, /*leadingPunctuation=*/false);
        OS << "; typename=";
        printCodeCompletionResultTypeNameAnnotated(*item, OS);
      } else {
        item->getCompletionString()->print(OS);
      }

      OS << "; name=" << item->getFilterName();
      OS << "\n";
    }
    OS << "End completions\n";
  } while (0);
  return strdup(desc.c_str());
}

void swiftide_completion_result_get_completions(
    swiftide_completion_response_t _result,
    void (^completions_handler)(const swiftide_completion_item_t *completions,
                                const char **filter_names,
                                uint64_t num_completions)) {
  auto &result = *static_cast<CompletionResult *>(_result);
  if (result.hasError() || result.getCompletions().empty()) {
    completions_handler(nullptr, nullptr, 0);
    return;
  }

  std::vector<const char *> filterNames;
  filterNames.reserve(result.getCompletions().size());
  for (auto *item : result.getCompletions()) {
    filterNames.push_back(item->getFilterName().data());
  }

  assert(filterNames.size() == result.getCompletions().size());

  completions_handler(
      (const swiftide_completion_item_t *)result.getCompletions().data(),
      filterNames.data(), result.getCompletions().size());
}

swiftide_completion_item_t swiftide_completion_result_get_completion_at_index(
    swiftide_completion_response_t _response, uint64_t index) {
  auto &response = *static_cast<CompletionResult *>(_response);
  if (response.hasError() || response.getCompletions().size() < index) {
    return nullptr;
  }
  return response.getCompletions()[index];
}

swiftide_completion_kind_t
swiftide_completion_result_get_kind(swiftide_completion_response_t _response) {
  auto &response = *static_cast<CompletionResult *>(_response);
  switch (response.context.CodeCompletionKind) {
  case CompletionKind::None:
    return SWIFTIDE_COMPLETION_KIND_NONE;
  case CompletionKind::Import:
    return SWIFTIDE_COMPLETION_KIND_IMPORT;
  case CompletionKind::Using:
    return SWIFTIDE_COMPLETION_KIND_USING;
  case CompletionKind::UnresolvedMember:
    return SWIFTIDE_COMPLETION_KIND_UNRESOLVEDMEMBER;
  case CompletionKind::DotExpr:
    return SWIFTIDE_COMPLETION_KIND_DOTEXPR;
  case CompletionKind::StmtOrExpr:
    return SWIFTIDE_COMPLETION_KIND_STMTOREXPR;
  case CompletionKind::PostfixExprBeginning:
    return SWIFTIDE_COMPLETION_KIND_POSTFIXEXPRBEGINNING;
  case CompletionKind::PostfixExpr:
    return SWIFTIDE_COMPLETION_KIND_POSTFIXEXPR;
  case CompletionKind::KeyPathExprObjC:
    return SWIFTIDE_COMPLETION_KIND_KEYPATHEXPROBJC;
  case CompletionKind::KeyPathExprSwift:
    return SWIFTIDE_COMPLETION_KIND_KEYPATHEXPRSWIFT;
  case CompletionKind::TypePossibleFunctionParamBeginning:
    return SWIFTIDE_COMPLETION_KIND_TYPEPOSSIBLEFUNCTIONPARAMBEGINNING;
  case CompletionKind::TypeDeclResultBeginning:
    return SWIFTIDE_COMPLETION_KIND_TYPEDECLRESULTBEGINNING;
  case CompletionKind::TypeBeginning:
    return SWIFTIDE_COMPLETION_KIND_TYPEBEGINNING;
  case CompletionKind::TypeSimpleOrComposition:
    return SWIFTIDE_COMPLETION_KIND_TYPESIMPLEORCOMPOSITION;
  case CompletionKind::TypeSimpleBeginning:
    return SWIFTIDE_COMPLETION_KIND_TYPESIMPLEBEGINNING;
  case CompletionKind::TypeSimpleWithDot:
    // TODO: check if this is still correct
    return SWIFTIDE_COMPLETION_KIND_TYPEIDENTIFIERWITHDOT;
  case CompletionKind::TypeSimpleWithoutDot:
    // TODO: check if this is still correct
    return SWIFTIDE_COMPLETION_KIND_TYPEIDENTIFIERWITHOUTDOT;
  case CompletionKind::CaseStmtKeyword:
    return SWIFTIDE_COMPLETION_KIND_CASESTMTKEYWORD;
  case CompletionKind::CaseStmtBeginning:
    return SWIFTIDE_COMPLETION_KIND_CASESTMTBEGINNING;
  case CompletionKind::NominalMemberBeginning:
    return SWIFTIDE_COMPLETION_KIND_NOMINALMEMBERBEGINNING;
  case CompletionKind::AccessorBeginning:
    return SWIFTIDE_COMPLETION_KIND_ACCESSORBEGINNING;
  case CompletionKind::AttributeBegin:
    return SWIFTIDE_COMPLETION_KIND_ATTRIBUTEBEGIN;
  case CompletionKind::AttributeDeclParen:
    return SWIFTIDE_COMPLETION_KIND_ATTRIBUTEDECLPAREN;
  case CompletionKind::EffectsSpecifier:
    return SWIFTIDE_COMPLETION_KIND_EFFECTSSPECIFIER;
  case CompletionKind::PoundAvailablePlatform:
    return SWIFTIDE_COMPLETION_KIND_POUNDAVAILABLEPLATFORM;
  case CompletionKind::CallArg:
    return SWIFTIDE_COMPLETION_KIND_CALLARG;
  case CompletionKind::ReturnStmtExpr:
    return SWIFTIDE_COMPLETION_KIND_RETURNSTMTEXPR;
  case CompletionKind::YieldStmtExpr:
    return SWIFTIDE_COMPLETION_KIND_YIELDSTMTEXPR;
  case CompletionKind::ForEachSequence:
    return SWIFTIDE_COMPLETION_KIND_FOREACHSEQUENCE;
  case CompletionKind::ForEachInKw:
    return SWIFTIDE_COMPLETION_KIND_FOREACHKWIN;
  case CompletionKind::AfterPoundExpr:
    return SWIFTIDE_COMPLETION_KIND_AFTERPOUNDEXPR;
  case CompletionKind::AfterPoundDirective:
    return SWIFTIDE_COMPLETION_KIND_AFTERPOUNDDIRECTIVE;
  case CompletionKind::PlatformConditon:
    return SWIFTIDE_COMPLETION_KIND_PLATFORMCONDITON;
  case CompletionKind::AfterIfStmtElse:
    return SWIFTIDE_COMPLETION_KIND_AFTERIFSTMTELSE;
  case CompletionKind::GenericRequirement:
    return SWIFTIDE_COMPLETION_KIND_GENERICREQUIREMENT;
  case CompletionKind::PrecedenceGroup:
    return SWIFTIDE_COMPLETION_KIND_PRECEDENCEGROUP;
  case CompletionKind::StmtLabel:
    return SWIFTIDE_COMPLETION_KIND_STMTLABEL;
  case CompletionKind::ForEachPatternBeginning:
    return SWIFTIDE_COMPLETION_KIND_FOREACHPATTERNBEGINNING;
  case CompletionKind::TypeAttrBeginning:
    return SWIFTIDE_COMPLETION_KIND_TYPEATTRBEGINNING;
  case CompletionKind::TypeAttrInheritanceBeginning:
    return SWIFTIDE_COMPLETION_KIND_TYPEATTRINHERITANCEBEGINNING;
  case CompletionKind::OptionalBinding:
    return SWIFTIDE_COMPLETION_KIND_OPTIONALBINDING;
  case CompletionKind::TypeSimpleInverted:
    return SWIFTIDE_COMPLETION_KIND_TYPESIMPLEINVERTED;
  case CompletionKind::ThenStmtExpr:
    return SWIFTIDE_COMPLETION_KIND_THENSTMTEXPR;
  }
}

void swiftide_completion_result_foreach_baseexpr_typename(
    swiftide_completion_response_t _response, bool (^handler)(const char *)) {
  auto &response = *static_cast<CompletionResult *>(_response);
  for (const auto typeName : response.context.LookedupNominalTypeNames) {
    if (/*shouldStop=*/handler(typeName.data())) {
      return;
    }
  }
}

bool swiftide_completion_result_is_reusing_astcontext(
    swiftide_completion_response_t _response) {
  auto &response = *static_cast<CompletionResult *>(_response);
  return response.context.ReusingASTContext;
}

const char *
swiftide_completion_item_description_copy(swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  llvm::SmallString<256> buffer;
  {
    llvm::raw_svector_ostream OS(buffer);
    item.printPrefix(OS);
    item.getCompletionString()->print(OS);
  }
  return strdup(buffer.c_str());
}

void swiftide_completion_item_get_label(
    swiftide_completion_response_t _response, swiftide_completion_item_t _item,
    bool annotate, void (^handler)(const char *)) {
  auto &response = *static_cast<CompletionResult *>(_response);
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  response.scratch.clear();
  {
    llvm::raw_svector_ostream OS(response.scratch);
    if (annotate) {
      printCodeCompletionResultDescriptionAnnotated(item, OS, false);
    } else {
      printCodeCompletionResultDescription(item, OS, false);
    }
  }
  handler(response.scratch.c_str());
}

void swiftide_completion_item_get_source_text(
    swiftide_completion_response_t _response, swiftide_completion_item_t _item,
    void (^handler)(const char *)) {
  auto &response = *static_cast<CompletionResult *>(_response);
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  response.scratch.clear();
  {
    llvm::raw_svector_ostream OS(response.scratch);
    printCodeCompletionResultSourceText(item, OS);
  }
  handler(response.scratch.c_str());
}

void swiftide_completion_item_get_type_name(
    swiftide_completion_response_t _response, swiftide_completion_item_t _item,
    bool annotate, void (^handler)(const char *)) {
  auto &response = *static_cast<CompletionResult *>(_response);
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  response.scratch.clear();
  {
    llvm::raw_svector_ostream OS(response.scratch);
    if (annotate) {
      printCodeCompletionResultTypeNameAnnotated(item, OS);
    } else {
      printCodeCompletionResultTypeName(item, OS);
    }
  }
  handler(response.scratch.empty() ? nullptr : response.scratch.c_str());
}

void swiftide_completion_item_get_doc_brief(
    swiftide_completion_response_t _response, swiftide_completion_item_t _item,
    void (^handler)(const char *)) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  if (item.getBriefDocComment().empty()) {
    return handler(nullptr);
  }
  handler(item.getBriefDocComment().data());
}

void swiftide_completion_item_get_associated_usrs(
    swiftide_completion_response_t _response, swiftide_completion_item_t _item,
    void (^handler)(const char **, uint64_t)) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  llvm::SmallVector<const char *, 4> usrs;
  for (auto usr : item.getAssociatedUSRs()) {
    usrs.push_back(usr.data());
  }
  handler(usrs.data(), usrs.size());
}

uint32_t swiftide_completion_item_get_kind(swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  // FIXME: keep this in sync with ide::CodeCompletionResult
  return static_cast<swiftide_completion_item_kind_t>(item.getKind());
}

uint32_t
swiftide_completion_item_get_associated_kind(swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  // FIXME: keep this in sync with ide::CodeCompletionResult
  switch (item.getKind()) {
  case CodeCompletionResultKind::Declaration:
    switch (item.getAssociatedDeclKind()) {
#define CASE(KIND, VAL)                                                        \
  case swift::ide::CodeCompletionDeclKind::KIND:                               \
    return SWIFTIDE_COMPLETION_ITEM_DECL_KIND_##VAL;

      CASE(Module, MODULE)
      CASE(Class, CLASS)
      CASE(Actor, ACTOR)
      CASE(Struct, STRUCT)
      CASE(Enum, ENUM)
      CASE(EnumElement, ENUMELEMENT)
      CASE(Protocol, PROTOCOL)
      CASE(AssociatedType, ASSOCIATEDTYPE)
      CASE(TypeAlias, TYPEALIAS)
      CASE(GenericTypeParam, GENERICTYPEPARAM)
      CASE(Constructor, CONSTRUCTOR)
      CASE(Destructor, DESTRUCTOR)
      CASE(Subscript, SUBSCRIPT)
      CASE(StaticMethod, STATICMETHOD)
      CASE(InstanceMethod, INSTANCEMETHOD)
      CASE(PrefixOperatorFunction, PREFIXOPERATORFUNCTION)
      CASE(PostfixOperatorFunction, POSTFIXOPERATORFUNCTION)
      CASE(InfixOperatorFunction, INFIXOPERATORFUNCTION)
      CASE(FreeFunction, FREEFUNCTION)
      CASE(StaticVar, STATICVAR)
      CASE(InstanceVar, INSTANCEVAR)
      CASE(LocalVar, LOCALVAR)
      CASE(GlobalVar, GLOBALVAR)
      CASE(PrecedenceGroup, PRECEDENCEGROUP)
      CASE(Macro, MACRO)
#undef CASE
    }
    llvm_unreachable("unhandled enum value");
  case CodeCompletionResultKind::Literal:
    return static_cast<uint32_t>(item.getLiteralKind());
  case CodeCompletionResultKind::Keyword:
    return static_cast<uint32_t>(item.getKeywordKind());
  default:
    return 0;
  }
}

uint32_t swiftide_completion_item_get_semantic_context(
    swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  switch (item.getSemanticContext()) {
  case swift::ide::SemanticContextKind::None:
    return SWIFTIDE_COMPLETION_SEMANTIC_CONTEXT_NONE;
  case swift::ide::SemanticContextKind::Local:
    return SWIFTIDE_COMPLETION_SEMANTIC_CONTEXT_LOCAL;
  case swift::ide::SemanticContextKind::CurrentNominal:
    return SWIFTIDE_COMPLETION_SEMANTIC_CONTEXT_CURRENTNOMINAL;
  case swift::ide::SemanticContextKind::Super:
    return SWIFTIDE_COMPLETION_SEMANTIC_CONTEXT_SUPER;
  case swift::ide::SemanticContextKind::OutsideNominal:
    return SWIFTIDE_COMPLETION_SEMANTIC_CONTEXT_OUTSIDENOMINAL;
  case swift::ide::SemanticContextKind::CurrentModule:
    return SWIFTIDE_COMPLETION_SEMANTIC_CONTEXT_CURRENTMODULE;
  case swift::ide::SemanticContextKind::OtherModule:
    return SWIFTIDE_COMPLETION_SEMANTIC_CONTEXT_OTHERMODULE;
  }
}

uint32_t swiftide_completion_item_get_flair(swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  uint32_t result = 0;
  auto flair = item.getFlair();
  if (flair.contains(CodeCompletionFlairBit::ExpressionSpecific))
    result |= SWIFTIDE_COMPLETION_FLAIR_EXPRESSIONSPECIFIC;
  if (flair.contains(CodeCompletionFlairBit::SuperChain))
    result |= SWIFTIDE_COMPLETION_FLAIR_SUPERCHAIN;
  if (flair.contains(CodeCompletionFlairBit::ArgumentLabels))
    result |= SWIFTIDE_COMPLETION_FLAIR_ARGUMENTLABELS;
  if (flair.contains(CodeCompletionFlairBit::CommonKeywordAtCurrentPosition))
    result |= SWIFTIDE_COMPLETION_FLAIR_COMMONKEYWORDATCURRENTPOSITION;
  if (flair.contains(CodeCompletionFlairBit::RareKeywordAtCurrentPosition))
    result |= SWIFTIDE_COMPLETION_FLAIR_RAREKEYWORDATCURRENTPOSITION;
  if (flair.contains(CodeCompletionFlairBit::RareTypeAtCurrentPosition))
    result |= SWIFTIDE_COMPLETION_FLAIR_RARETYPEATCURRENTPOSITION;
  if (flair.contains(
          CodeCompletionFlairBit::ExpressionAtNonScriptOrMainFileScope))
    result |= SWIFTIDE_COMPLETION_FLAIR_EXPRESSIONATNONSCRIPTORMAINFILESCOPE;
  return result;
}

bool swiftide_completion_item_is_not_recommended(
    swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  return item.isNotRecommended();
}

uint32_t swiftide_completion_item_not_recommended_reason(
    swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  switch (item.getNotRecommendedReason()) {
  case NotRecommendedReason::None:
    return SWIFTIDE_COMPLETION_NOT_RECOMMENDED_NONE;
  case NotRecommendedReason::RedundantImport:
    return SWIFTIDE_COMPLETION_NOT_RECOMMENDED_REDUNDANT_IMPORT;
  case NotRecommendedReason::RedundantImportIndirect:
    return SWIFTIDE_COMPLETION_NOT_RECOMMENDED_REDUNDANT_IMPORT_INDIRECT;
  case NotRecommendedReason::Deprecated:
    return SWIFTIDE_COMPLETION_NOT_RECOMMENDED_DEPRECATED;
  case NotRecommendedReason::SoftDeprecated:
    return SWIFTIDE_COMPLETION_NOT_RECOMMENDED_SOFTDEPRECATED;
  case NotRecommendedReason::VariableUsedInOwnDefinition:
    return SWIFTIDE_COMPLETION_NOT_RECOMMENDED_VARIABLE_USED_IN_OWN_DEFINITION;
  case NotRecommendedReason::NonAsyncAlternativeUsedInAsyncContext:
    return SWIFTIDE_COMPLETION_NOT_RECOMMENDED_NON_ASYNC_ALTERNATIVE_USED_IN_ASYNC_CONTEXT;
  }
}

bool swiftide_completion_item_has_diagnostic(swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  return (item.getNotRecommendedReason() != NotRecommendedReason::None);
}

void swiftide_completion_item_get_diagnostic(
    swiftide_completion_response_t _response, swiftide_completion_item_t _item,
    void (^handler)(swiftide_completion_diagnostic_severity_t, const char *)) {
  auto &response = *static_cast<CompletionResult *>(_response);
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  swiftide_completion_diagnostic_severity_t severity;
  llvm::SmallString<64> scratch;
  auto severityAndMessage = item.getDiagnosticSeverityAndMessage(
      scratch, response.compilerInstance->getASTContext());
  switch (severityAndMessage.first) {
  case CodeCompletionDiagnosticSeverity::None:
    handler(SWIFTIDE_COMPLETION_DIAGNOSTIC_SEVERITY_NONE, nullptr);
    return;
  case CodeCompletionDiagnosticSeverity::Error:
    severity = SWIFTIDE_COMPLETION_DIAGNOSTIC_SEVERITY_ERROR;
    break;
  case CodeCompletionDiagnosticSeverity::Warning:
    severity = SWIFTIDE_COMPLETION_DIAGNOSTIC_SEVERITY_WARNING;
    break;
  case CodeCompletionDiagnosticSeverity::Remark:
    severity = SWIFTIDE_COMPLETION_DIAGNOSTIC_SEVERITY_REMARK;
    break;
  case CodeCompletionDiagnosticSeverity::Note:
    severity = SWIFTIDE_COMPLETION_DIAGNOSTIC_SEVERITY_NOTE;
    break;
  }

  handler(severity, severityAndMessage.second.data());
}

bool swiftide_completion_item_is_system(swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  return item.isSystem();
}

void swiftide_completion_item_get_module_name(
    swiftide_completion_response_t _response, swiftide_completion_item_t _item,
    void (^handler)(const char *)) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  handler(item.getContextFreeResult().getModuleName().data());
}

uint32_t swiftide_completion_item_get_num_bytes_to_erase(
    swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  return item.getNumBytesToErase();
}

uint32_t
swiftide_completion_item_get_type_relation(swiftide_completion_item_t _item) {
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  // FIXME: keep this in sync with ide::CodeCompletionResult
  return static_cast<swiftide_completion_type_relation_t>(
      item.getExpectedTypeRelation());
}

uint32_t
swiftide_completion_item_import_depth(swiftide_completion_response_t _response,
                                      swiftide_completion_item_t _item) {
  auto &response = *static_cast<CompletionResult *>(_response);
  auto &item = *static_cast<CodeCompletionResult *>(_item);
  if (item.getSemanticContext() == SemanticContextKind::OtherModule) {
    if (auto depth = response.importDepth.lookup(item.getModuleName())) {
      return *depth;
    } else {
      return ~0u;
    }
  } else {
    return 0;
  }
}

swiftide_fuzzy_match_pattern_t
swiftide_fuzzy_match_pattern_create(const char *pattern) {
  return static_cast<swiftide_fuzzy_match_pattern_t>(
      new FuzzyStringMatcher(pattern));
}

bool swiftide_fuzzy_match_pattern_matches_candidate(
    swiftide_fuzzy_match_pattern_t _pattern, const char *_candidate,
    double *outScore) {
  auto &matcher = *static_cast<FuzzyStringMatcher *>(_pattern);
  StringRef candidate = _candidate;
  if (matcher.matchesCandidate(candidate)) {
    if (outScore)
      *outScore = matcher.scoreCandidate(candidate);
    return true;
  }
  return false;
}

void swiftide_fuzzy_match_pattern_dispose(
    swiftide_fuzzy_match_pattern_t _pattern) {
  delete static_cast<FuzzyStringMatcher *>(_pattern);
}

static std::string getRuntimeResourcesPath() {
  auto libPath = getRuntimeLibPath();
  llvm::SmallString<128> libPathTmp(libPath);
  llvm::sys::path::append(libPathTmp, "swift");
  return libPathTmp.str().str();
}
