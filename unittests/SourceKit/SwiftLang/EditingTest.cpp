//===----------------------------------------------------------------------===//
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

#include "SourceKit/Core/Context.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Core/NotificationCenter.h"
#include "SourceKit/Support/Concurrency.h"
#include "SourceKit/SwiftLang/Factory.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"
#include "gtest/gtest.h"

#include <chrono>
#include <condition_variable>
#include <mutex>
#include <thread>

using namespace SourceKit;
using namespace llvm;

static StringRef getRuntimeLibPath() {
  return sys::path::parent_path(SWIFTLIB_DIR);
}

namespace {

class DiagConsumer : public EditorConsumer {
public:
  UIdent DiagStage;
  std::vector<DiagnosticEntryInfo> Diags;

private:
  void handleRequestError(const char *Description) override {
    llvm_unreachable("unexpected error");
  }

  bool syntaxMapEnabled() override { return true; }

  void handleSyntaxMap(unsigned Offset, unsigned Length, UIdent Kind) override {
  }

  void handleSemanticAnnotation(unsigned Offset, unsigned Length, UIdent Kind,
                                bool isSystem) override {}

  bool documentStructureEnabled() override { return false; }

  void beginDocumentSubStructure(unsigned Offset, unsigned Length,
                                 UIdent Kind, UIdent AccessLevel,
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
                                 ArrayRef<std::tuple<UIdent, unsigned, unsigned>> Attrs) override {
  }

  void endDocumentSubStructure() override {}

  void handleDocumentSubStructureElement(UIdent Kind, unsigned Offset,
                                         unsigned Length) override {}

  void recordAffectedRange(unsigned Offset, unsigned Length) override {}

  void recordAffectedLineRange(unsigned Line, unsigned Length) override {}

  void recordFormattedText(StringRef Text) override {}

  void setDiagnosticStage(UIdent diagStage) override { DiagStage = diagStage; }
  void handleDiagnostic(const DiagnosticEntryInfo &Info,
                        UIdent DiagStage) override {
    Diags.push_back(Info);
  }

  void handleSourceText(StringRef Text) override {}
  void handleSyntaxTree(const swift::syntax::SourceFileSyntax &SyntaxTree,
                        std::unordered_set<unsigned> &ReusedNodeIds) override {}

  SyntaxTreeTransferMode syntaxTreeTransferMode() override {
    return SyntaxTreeTransferMode::Off;
  }

};

struct DocUpdateMutexState {
  std::mutex Mtx;
  std::condition_variable CV;
  bool HasUpdate = false;
};

class EditTest : public ::testing::Test {
  SourceKit::Context *Ctx;
  std::shared_ptr<DocUpdateMutexState> DocUpdState;

public:
  EditTest() {
    // This is avoiding destroying \p SourceKit::Context because another
    // thread may be active trying to use it to post notifications.
    // FIXME: Use shared_ptr ownership to avoid such issues.
    Ctx = new SourceKit::Context(getRuntimeLibPath(),
                                 SourceKit::createSwiftLangSupport,
                                 /*dispatchOnMain=*/false);
    auto localDocUpdState = std::make_shared<DocUpdateMutexState>();
    Ctx->getNotificationCenter()->addDocumentUpdateNotificationReceiver(
        [localDocUpdState](StringRef docName) {
          std::unique_lock<std::mutex> lk(localDocUpdState->Mtx);
          localDocUpdState->HasUpdate = true;
          localDocUpdState->CV.notify_one();
        });
    DocUpdState = localDocUpdState;
  }

  LangSupport &getLang() { return Ctx->getSwiftLangSupport(); }

  void SetUp() override {
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmPrinters();
    llvm::InitializeAllAsmParsers();
  }

  void addNotificationReceiver(DocumentUpdateNotificationReceiver Receiver) {
    Ctx->getNotificationCenter()->addDocumentUpdateNotificationReceiver(Receiver);
  }

  bool waitForDocUpdate(bool reset = false) {
    std::chrono::seconds secondsToWait(20);
    std::unique_lock<std::mutex> lk(DocUpdState->Mtx);
    auto when = std::chrono::system_clock::now() + secondsToWait;
    auto result = !DocUpdState->CV.wait_until(
        lk, when, [&]() { return DocUpdState->HasUpdate; });
    if (reset)
      DocUpdState->HasUpdate = false;
    return result;
  }

  void open(const char *DocName, StringRef Text, ArrayRef<const char *> CArgs,
            EditorConsumer &Consumer) {
    auto Args = makeArgs(DocName, CArgs);
    auto Buf = MemoryBuffer::getMemBufferCopy(Text, DocName);
    getLang().editorOpen(DocName, Buf.get(), Consumer, Args, None);
  }

  void close(const char *DocName) {
    getLang().editorClose(DocName, /*removeCache=*/false);
  }

  void replaceText(StringRef DocName, unsigned Offset, unsigned Length,
                   StringRef Text, EditorConsumer &Consumer) {
    auto Buf = MemoryBuffer::getMemBufferCopy(Text, DocName);
    getLang().editorReplaceText(DocName, Buf.get(), Offset, Length, Consumer);
  }

  unsigned findOffset(StringRef Val, StringRef Text) {
    auto pos = Text.find(Val);
    assert(pos != StringRef::npos);
    return pos;
  }

  void reset(DiagConsumer &Consumer) {
    Consumer.Diags.clear();
    Consumer.DiagStage = UIdent();
    std::unique_lock<std::mutex> lk(DocUpdState->Mtx);
    DocUpdState->HasUpdate = false;
  }

  void doubleOpenWithDelay(std::chrono::microseconds delay, bool close);

private:
  std::vector<const char *> makeArgs(const char *DocName,
                                     ArrayRef<const char *> CArgs) {
    std::vector<const char *> Args = CArgs;
    Args.push_back(DocName);
    return Args;
  }
};

} // anonymous namespace

static UIdent SemaDiagStage("source.diagnostic.stage.swift.sema");
static UIdent ParseDiagStage("source.diagnostic.stage.swift.parse");

TEST_F(EditTest, DiagsAfterEdit) {
  const char *DocName = "/test.swift";
  const char *Contents =
    "func foo {}\n"
    "let v = 0\n";
  const char *Args[] = { "-parse-as-library" };

  DiagConsumer Consumer;
  open(DocName, Contents, Args, Consumer);
  ASSERT_EQ(1u, Consumer.Diags.size());
  EXPECT_STREQ("expected '(' in argument list of function declaration", Consumer.Diags[0].Description.c_str());

  reset(Consumer);
  replaceText(DocName, findOffset("func foo", Contents)+strlen("func foo"), 0, "()", Consumer);
  EXPECT_TRUE(Consumer.Diags.empty());

  bool expired = waitForDocUpdate();
  ASSERT_FALSE(expired);

  reset(Consumer);
  replaceText(DocName, 0, 0, StringRef(), Consumer);
  EXPECT_TRUE(Consumer.Diags.empty());

  // If typecheck completed for the edit we'll get 'sema stage', otherwise
  // we'll get 'parser stage' and there will be another doc-update.
  if (Consumer.DiagStage == ParseDiagStage) {
    bool expired = waitForDocUpdate();
    ASSERT_FALSE(expired);

    reset(Consumer);
    replaceText(DocName, 0, 0, StringRef(), Consumer);
    EXPECT_TRUE(Consumer.Diags.empty());
  }
  EXPECT_EQ(SemaDiagStage, Consumer.DiagStage);
}

void EditTest::doubleOpenWithDelay(std::chrono::microseconds delay,
                                   bool closeDoc) {
  const char *DocName = "/test.swift";
  const char *Contents =
    "func foo() { _ = unknown_name }\n";
  const char *Args[] = { "-parse-as-library" };

  DiagConsumer Consumer;
  open(DocName, Contents, Args, Consumer);
  ASSERT_LE(Consumer.Diags.size(), 1u);
  if (Consumer.Diags.size() > 0) {
    EXPECT_EQ(SemaDiagStage, Consumer.DiagStage);
    Consumer.Diags.clear();
    Consumer.DiagStage = UIdent();
  }

  // Open again without closing; this reinitializes the semantic info on the doc
  if (delay > std::chrono::microseconds(0))
    std::this_thread::sleep_for(delay);
  if (closeDoc)
    close(DocName);
  reset(Consumer);

  open(DocName, Contents, Args, Consumer);
  ASSERT_LE(Consumer.Diags.size(), 1u);
  if (Consumer.Diags.size() > 0) {
    EXPECT_EQ(SemaDiagStage, Consumer.DiagStage);
    Consumer.Diags.clear();
    Consumer.DiagStage = UIdent();
  }

  // Wait for the document update from the second time we open the document. We
  // may or may not get a notification from the first time it was opened, but
  // only the second time will there be any semantic information available to
  // be queried, since the semantic info from the first open is unreachable.
  for (int i = 0; i < 2; ++i) {
    bool expired = waitForDocUpdate(/*reset=*/true);
    ASSERT_FALSE(expired) << "no " << (i ? "second " : "") << "notification";
    replaceText(DocName, 0, 0, StringRef(), Consumer);
    if (!Consumer.Diags.empty())
      break;
    ASSERT_EQ(0, i) << "no diagnostics after second notification";
  }

  ASSERT_EQ(1u, Consumer.Diags.size());
  EXPECT_STREQ("use of unresolved identifier 'unknown_name'", Consumer.Diags[0].Description.c_str());

  close(DocName);
}

// This test is failing occassionally in CI: rdar://45644449
TEST_F(EditTest, DISABLED_DiagsAfterCloseAndReopen) {
  // Attempt to open the same file twice in a row. This tests (subject to
  // timing) cases where:
  // * the 2nd open happens before the first AST starts building
  // * the 2nd open happens after the first AST starts building
  // * the 2nd open happens after the AST finishes

  // The middle case in particular verifies the ASTManager is only calling the
  // correct ASTConsumers.

  doubleOpenWithDelay(std::chrono::microseconds(0), true);
  doubleOpenWithDelay(std::chrono::milliseconds(1), true);
  doubleOpenWithDelay(std::chrono::milliseconds(10), true);
  doubleOpenWithDelay(std::chrono::milliseconds(100), true);
}

TEST_F(EditTest, DiagsAfterReopen) {
  // See description of DiagsAfterCloseAndReopen, but in this case we don't
  // close the original document, causing it to reinitialize instead of create
  // a fresh document.

  doubleOpenWithDelay(std::chrono::microseconds(0), false);
  doubleOpenWithDelay(std::chrono::milliseconds(1), false);
  doubleOpenWithDelay(std::chrono::milliseconds(10), false);
  doubleOpenWithDelay(std::chrono::milliseconds(100), false);
}
