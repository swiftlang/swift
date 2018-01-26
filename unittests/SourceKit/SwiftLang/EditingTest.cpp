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
#include <mutex>
#include <condition_variable>

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

  bool handleSyntaxMap(unsigned Offset, unsigned Length, UIdent Kind) override {
    return false;
  }

  bool handleSemanticAnnotation(unsigned Offset, unsigned Length,
                                UIdent Kind, bool isSystem) override {
    return false;
  }

  bool beginDocumentSubStructure(unsigned Offset, unsigned Length,
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
    return false;
  }

  bool endDocumentSubStructure() override { return false; }

  bool handleDocumentSubStructureElement(UIdent Kind,
                                         unsigned Offset,
                                         unsigned Length) override {
    return false;
  }

  bool recordAffectedRange(unsigned Offset, unsigned Length) override {
    return false;
  }
  
  bool recordAffectedLineRange(unsigned Line, unsigned Length) override {
    return false;
  }

  bool recordFormattedText(StringRef Text) override { return false; }

  bool setDiagnosticStage(UIdent diagStage) override {
    DiagStage = diagStage;
    return true;
  }
  bool handleDiagnostic(const DiagnosticEntryInfo &Info,
                        UIdent DiagStage) override {
    Diags.push_back(Info);
    return true;
  }

  bool handleSourceText(StringRef Text) override { return false; }
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
    Ctx->getNotificationCenter().addDocumentUpdateNotificationReceiver(
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
    Ctx->getNotificationCenter().addDocumentUpdateNotificationReceiver(Receiver);
  }

  bool waitForDocUpdate() {
    std::chrono::seconds secondsToWait(10);
    std::unique_lock<std::mutex> lk(DocUpdState->Mtx);
    auto when = std::chrono::system_clock::now() + secondsToWait;
    return !DocUpdState->CV.wait_until(
        lk, when, [&]() { return DocUpdState->HasUpdate; });
  }

  void open(const char *DocName, StringRef Text, ArrayRef<const char *> CArgs,
            EditorConsumer &Consumer) {
    auto Args = makeArgs(DocName, CArgs);
    auto Buf = MemoryBuffer::getMemBufferCopy(Text, DocName);
    getLang().editorOpen(DocName, Buf.get(), /*EnableSyntaxMap=*/false, Consumer,
                         Args);
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
