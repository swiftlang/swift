#include "libInMemoryFrontend/InMemoryFrontend.h"
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/Driver/FrontendUtil.h"
#include "swift/Frontend/Frontend.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"

using namespace swift;

class StreamDiagConsumer : public DiagnosticConsumer {
  llvm::raw_ostream &OS;

public:
  StreamDiagConsumer(llvm::raw_ostream &OS) : OS(OS) {}

  void handleDiagnostic(SourceManager &SM,
                        const DiagnosticInfo &Info) override {
    switch (Info.Kind) {
    case DiagnosticKind::Error:
      OS << "error: ";
      break;
    case DiagnosticKind::Warning:
      OS << "warning: ";
      break;
    case DiagnosticKind::Note:
      OS << "note: ";
      break;
    case DiagnosticKind::Remark:
      OS << "remark: ";
      break;
    }
    DiagnosticEngine::formatDiagnosticText(OS, Info.FormatString,
                                           Info.FormatArgs);
  }
};

static StringRef getRuntimeLibPath() {
  return llvm::sys::path::parent_path(SWIFTLIB_DIR);
}

class InMemoryFrontendTest : public ::testing::Test {
protected:
  InMemoryFrontendTest()
      : MemFS(new llvm::vfs::InMemoryFileSystem()),
        FS(new llvm::vfs::OverlayFileSystem(llvm::vfs::getRealFileSystem())),
        ErrOS(ErrStr), DiagConsumer(ErrOS) {
    FS->pushOverlay(MemFS);

    CI.addDiagnosticConsumer(&DiagConsumer);
    CI.getSourceMgr().setFileSystem(FS);
  }

  bool ParseArgsAndSetupInstance(llvm::ArrayRef<const char *> OrigArgs) {
    SmallVector<const char *, 16> Args;
    Args.push_back("-resource-dir");
    Args.push_back(getRuntimeLibPath().data());
    Args.append(OrigArgs.begin(), OrigArgs.end());

    // Without this configuration option, the clang tries to emit object files
    // for the modules that it compiles. To do this, it looks up the current
    // triple in the llvm TargetRegistry. We have not initialized the
    // TargetRegistry, so it fails.
    Invocation.getClangImporterOptions().DetailedPreprocessingRecord = true;

    bool ParseResult = driver::getSingleFrontendInvocationFromDriverArguments(
        Args, CI.getDiags(), [&](ArrayRef<const char *> FrontendArgs) {
          return Invocation.parseArgs(FrontendArgs, CI.getDiags());
        });
    if (ParseResult)
      return true;

    return CI.setup(Invocation);
  }

  llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> MemFS;
  llvm::IntrusiveRefCntPtr<llvm::vfs::OverlayFileSystem> FS;

  SmallString<32> ErrStr;
  llvm::raw_svector_ostream ErrOS;
  StreamDiagConsumer DiagConsumer;

  CompilerInstance CI;
  CompilerInvocation Invocation;
};

TEST_F(InMemoryFrontendTest, SemaError) {
  MemFS->addFile("/file1.swift", /*ModificationTime=*/0,
                 llvm::MemoryBuffer::getMemBuffer("let x: String = \"hello\"",
                                                  "/file1.swift"));
  MemFS->addFile(
      "/file2.swift", /*ModificationTime=*/0,
      llvm::MemoryBuffer::getMemBuffer("let y: Int = x", "/file2.swift"));

  const char *Args[] = {"/file1.swift", "/file2.swift"};
  bool SetupResult = ParseArgsAndSetupInstance(Args);
  ASSERT_FALSE(SetupResult) << ErrStr;

  std::unique_ptr<llvm::MemoryBuffer> ModBuf;
  std::unique_ptr<llvm::MemoryBuffer> ModDocBuf;
  bool CompileResult =
      inmemoryfrontend::compileSwiftModule(CI, &ModBuf, &ModDocBuf);
  EXPECT_TRUE(CompileResult);
  EXPECT_EQ("error: cannot convert value of type 'String' to specified type "
            "'Int'",
            ErrStr);
}

TEST_F(InMemoryFrontendTest, Success) {
  MemFS->addFile("/file1.swift", /*ModificationTime=*/0,
                 llvm::MemoryBuffer::getMemBuffer("let x: String = \"hello\"",
                                                  "/file1.swift"));
  MemFS->addFile(
      "/file2.swift", /*ModificationTime=*/0,
      llvm::MemoryBuffer::getMemBuffer("let y: String = x", "/file2.swift"));

  const char *Args[] = {"/file1.swift", "/file2.swift"};
  bool SetupResult = ParseArgsAndSetupInstance(Args);
  ASSERT_FALSE(SetupResult) << ErrStr;

  std::unique_ptr<llvm::MemoryBuffer> ModBuf;
  std::unique_ptr<llvm::MemoryBuffer> ModDocBuf;
  bool CompileResult =
      inmemoryfrontend::compileSwiftModule(CI, &ModBuf, &ModDocBuf);
  ASSERT_FALSE(CompileResult) << ErrStr;
  ASSERT_TRUE(ModBuf);
  ASSERT_TRUE(ModDocBuf);

  EXPECT_EQ(serialization::Status::Valid,
            serialization::validateSerializedAST(ModBuf->getBuffer()).status);
}
