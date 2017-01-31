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
#include "gtest/gtest.h"

using namespace SourceKit;
using namespace llvm;

static StringRef getRuntimeLibPath() {
  return sys::path::parent_path(SWIFTLIB_DIR);
}

namespace {

class NullEditorConsumer : public EditorConsumer {
  bool needsSemanticInfo() override { return false; }

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
                                 StringRef DisplayName,
                                 StringRef TypeName,
                                 StringRef RuntimeName,
                                 StringRef SelectorName,
                                 ArrayRef<StringRef> InheritedTypes,
                                 ArrayRef<UIdent> Attrs) override {
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

  bool setDiagnosticStage(UIdent DiagStage) override { return false; }
  bool handleDiagnostic(const DiagnosticEntryInfo &Info,
                        UIdent DiagStage) override {
    return false;
  }

  bool handleSourceText(StringRef Text) override { return false; }
};

struct TestCursorInfo {
  std::string Name;
  std::string Typename;
  std::string Filename;
  Optional<std::pair<unsigned, unsigned>> DeclarationLoc;
};

class CursorInfoTest : public ::testing::Test {
  SourceKit::Context Ctx{ getRuntimeLibPath(), SourceKit::createSwiftLangSupport };
  std::atomic<int> NumTasks;

public:
  LangSupport &getLang() { return Ctx.getSwiftLangSupport(); }

  void SetUp() {
    NumTasks = 0;
  }

  void addNotificationReceiver(DocumentUpdateNotificationReceiver Receiver) {
    Ctx.getNotificationCenter().addDocumentUpdateNotificationReceiver(Receiver);
  }

  void open(StringRef DocName, StringRef Text) {
    NullEditorConsumer Consumer;
    auto Buf = MemoryBuffer::getMemBufferCopy(Text, DocName);
    getLang().editorOpen(DocName, Buf.get(), /*EnableSyntaxMap=*/false, Consumer,
                         /*Args=*/{});
  }

  void replaceText(StringRef DocName, unsigned Offset, unsigned Length,
                   StringRef Text) {
    NullEditorConsumer Consumer;
    auto Buf = MemoryBuffer::getMemBufferCopy(Text, DocName);
    getLang().editorReplaceText(DocName, Buf.get(), Offset, Length, Consumer);
  }

  TestCursorInfo getCursor(const char *DocName, unsigned Offset,
                           ArrayRef<const char *> CArgs) {
    auto Args = makeArgs(DocName, CArgs);
    Semaphore sema(0);

    TestCursorInfo TestInfo;
    getLang().getCursorInfo(DocName, Offset, 0, false, Args,
      [&](const CursorInfo &Info) {
        TestInfo.Name = Info.Name;
        TestInfo.Typename = Info.TypeName;
        TestInfo.Filename = Info.Filename;
        TestInfo.DeclarationLoc = Info.DeclarationLoc;
        sema.signal();
      });

    bool expired = sema.wait(60 * 1000);
    if (expired)
      llvm::report_fatal_error("check took too long");
    return TestInfo;
  }

  unsigned findOffset(StringRef Val, StringRef Text) {
    auto pos = Text.find(Val);
    assert(pos != StringRef::npos);
    return pos;
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

TEST_F(CursorInfoTest, FileNotExist) {
  const char *DocName = "/test.swift";
  const char *Contents =
    "let foo = 0\n";
  const char *Args[] = { "/<not-existent-file>" };

  open(DocName, Contents);
  auto FooOffs = findOffset("foo =", Contents);
  auto Info = getCursor(DocName, FooOffs, Args);
  EXPECT_STREQ("foo", Info.Name.c_str());
  EXPECT_STREQ("Int", Info.Typename.c_str());
}

static const char *ExpensiveInit =
    "[0:0,0:0,0:0,0:0,0:0,0:0,0:0]";

TEST_F(CursorInfoTest, EditAfter) {
  const char *DocName = "/test.swift";
  const char *Contents =
    "let value = foo\n"
    "let foo = 0\n";
  const char *Args[] = { "-parse-as-library" };

  open(DocName, Contents);
  auto FooRefOffs = findOffset("foo", Contents);
  auto FooOffs = findOffset("foo =", Contents);
  auto Info = getCursor(DocName, FooRefOffs, Args);
  EXPECT_STREQ("foo", Info.Name.c_str());
  EXPECT_STREQ("Int", Info.Typename.c_str());
  EXPECT_STREQ(DocName, Info.Filename.c_str());
  ASSERT_TRUE(Info.DeclarationLoc.hasValue());
  EXPECT_EQ(FooOffs, Info.DeclarationLoc->first);
  EXPECT_EQ(strlen("foo"), Info.DeclarationLoc->second);

  StringRef TextToReplace = "0";
  replaceText(DocName, findOffset(TextToReplace, Contents), TextToReplace.size(),
              ExpensiveInit);
  // Insert a space in front of 'foo' decl.
  replaceText(DocName, FooOffs, 0, " ");
  ++FooOffs;

  // Should not wait for the new AST, it should give the previous answer.
  Info = getCursor(DocName, FooRefOffs, Args);
  EXPECT_STREQ("foo", Info.Name.c_str());
  EXPECT_STREQ("Int", Info.Typename.c_str());
  EXPECT_STREQ(DocName, Info.Filename.c_str());
  ASSERT_TRUE(Info.DeclarationLoc.hasValue());
  EXPECT_EQ(FooOffs, Info.DeclarationLoc->first);
  EXPECT_EQ(strlen("foo"), Info.DeclarationLoc->second);
}

TEST_F(CursorInfoTest, EditBefore) {
  const char *DocName = "/test.swift";
  const char *Contents =
    "let foo = 0\n"
    "let value = foo;\n";
  const char *Args[] = { "-parse-as-library" };

  open(DocName, Contents);
  auto FooRefOffs = findOffset("foo;", Contents);
  auto FooOffs = findOffset("foo =", Contents);
  auto Info = getCursor(DocName, FooRefOffs, Args);
  EXPECT_STREQ("foo", Info.Name.c_str());
  EXPECT_STREQ("Int", Info.Typename.c_str());
  EXPECT_STREQ(DocName, Info.Filename.c_str());
  ASSERT_TRUE(Info.DeclarationLoc.hasValue());
  EXPECT_EQ(FooOffs, Info.DeclarationLoc->first);
  EXPECT_EQ(strlen("foo"), Info.DeclarationLoc->second);

  StringRef TextToReplace = "0";
  replaceText(DocName, findOffset(TextToReplace, Contents), TextToReplace.size(),
              ExpensiveInit);
  FooRefOffs += StringRef(ExpensiveInit).size() - TextToReplace.size();
  // Insert a space in front of 'foo' decl.
  replaceText(DocName, FooOffs, 0, " ");
  ++FooOffs;
  ++FooRefOffs;

  // Should not wait for the new AST, it should give the previous answer.
  Info = getCursor(DocName, FooRefOffs, Args);
  EXPECT_STREQ("foo", Info.Name.c_str());
  EXPECT_STREQ("Int", Info.Typename.c_str());
  EXPECT_STREQ(DocName, Info.Filename.c_str());
  ASSERT_TRUE(Info.DeclarationLoc.hasValue());
  EXPECT_EQ(FooOffs, Info.DeclarationLoc->first);
  EXPECT_EQ(strlen("foo"), Info.DeclarationLoc->second);
}

TEST_F(CursorInfoTest, CursorInfoMustWaitDueDeclLoc) {
  const char *DocName = "/test.swift";
  const char *Contents =
    "let value = foo\n"
    "let foo = 0\n";
  const char *Args[] = { "-parse-as-library" };

  open(DocName, Contents);
  auto FooRefOffs = findOffset("foo", Contents);
  auto FooOffs = findOffset("foo =", Contents);
  auto Info = getCursor(DocName, FooRefOffs, Args);
  EXPECT_STREQ("foo", Info.Name.c_str());
  EXPECT_STREQ("Int", Info.Typename.c_str());

  StringRef TextToReplace = "0";
  replaceText(DocName, findOffset(TextToReplace, Contents), TextToReplace.size(),
              ExpensiveInit);
  // Edit over the 'foo' decl.
  replaceText(DocName, FooOffs, strlen("foo"), "foo");

  // Should wait for the new AST, because the declaration location for the 'foo'
  // reference has been edited out.
  Info = getCursor(DocName, FooRefOffs, Args);
  EXPECT_STREQ("foo", Info.Name.c_str());
  EXPECT_STREQ("[Int : Int]", Info.Typename.c_str());
  ASSERT_TRUE(Info.DeclarationLoc.hasValue());
  EXPECT_EQ(FooOffs, Info.DeclarationLoc->first);
  EXPECT_EQ(strlen("foo"), Info.DeclarationLoc->second);
}

TEST_F(CursorInfoTest, CursorInfoMustWaitDueOffset) {
  const char *DocName = "/test.swift";
  const char *Contents =
    "let value = foo\n"
    "let foo = 0\n";
  const char *Args[] = { "-parse-as-library" };

  open(DocName, Contents);
  auto FooRefOffs = findOffset("foo", Contents);
  auto FooOffs = findOffset("foo =", Contents);
  auto Info = getCursor(DocName, FooRefOffs, Args);
  EXPECT_STREQ("foo", Info.Name.c_str());
  EXPECT_STREQ("Int", Info.Typename.c_str());

  StringRef TextToReplace = "0";
  replaceText(DocName, findOffset(TextToReplace, Contents), TextToReplace.size(),
              ExpensiveInit);
  // Edit over the 'foo' reference.
  replaceText(DocName, FooRefOffs, strlen("foo"), "foo");

  // Should wait for the new AST, because the cursor location has been edited
  // out.
  Info = getCursor(DocName, FooRefOffs, Args);
  EXPECT_STREQ("foo", Info.Name.c_str());
  EXPECT_STREQ("[Int : Int]", Info.Typename.c_str());
  ASSERT_TRUE(Info.DeclarationLoc.hasValue());
  EXPECT_EQ(FooOffs, Info.DeclarationLoc->first);
  EXPECT_EQ(strlen("foo"), Info.DeclarationLoc->second);
}

TEST_F(CursorInfoTest, CursorInfoMustWaitDueToken) {
  const char *DocName = "/test.swift";
  const char *Contents =
    "let value = foo\n"
    "let foo = 0\n";
  const char *Args[] = { "-parse-as-library" };

  open(DocName, Contents);
  auto FooRefOffs = findOffset("foo", Contents);
  auto FooOffs = findOffset("foo =", Contents);
  auto Info = getCursor(DocName, FooRefOffs, Args);
  EXPECT_STREQ("foo", Info.Name.c_str());
  EXPECT_STREQ("Int", Info.Typename.c_str());

  StringRef TextToReplace = "0";
  replaceText(DocName, findOffset(TextToReplace, Contents), TextToReplace.size(),
              ExpensiveInit);
  // Change 'foo' to 'fog' by replacing the last character.
  replaceText(DocName, FooRefOffs+2, 1, "g");
  replaceText(DocName, FooOffs+2, 1, "g");

  // Should wait for the new AST, because the cursor location points to a
  // different token.
  Info = getCursor(DocName, FooRefOffs, Args);
  EXPECT_STREQ("fog", Info.Name.c_str());
  EXPECT_STREQ("[Int : Int]", Info.Typename.c_str());
  ASSERT_TRUE(Info.DeclarationLoc.hasValue());
  EXPECT_EQ(FooOffs, Info.DeclarationLoc->first);
  EXPECT_EQ(strlen("fog"), Info.DeclarationLoc->second);
}
