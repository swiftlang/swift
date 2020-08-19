#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterOptions.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/FileSystemOptions.h"
#include "clang/Basic/FileSystemStatCache.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"

using namespace swift;

static std::string createFilename(StringRef base, StringRef name) {
  SmallString<256> path = base;
  llvm::sys::path::append(path, name);
  return llvm::Twine(path).str();
}

static bool emitFileWithContents(StringRef path, StringRef contents,
                                 std::string *pathOut = nullptr) {
  int FD;
  if (llvm::sys::fs::openFileForWrite(path, FD))
    return true;
  if (pathOut)
    *pathOut = path.str();
  llvm::raw_fd_ostream file(FD, /*shouldClose=*/true);
  file << contents;
  return false;
}

static bool emitFileWithContents(StringRef base, StringRef name,
                                 StringRef contents,
                                 std::string *pathOut = nullptr) {
  return emitFileWithContents(createFilename(base, name), contents, pathOut);
}

TEST(ClangImporterTest, emitPCHInMemory) {
  // Create a temporary cache on disk and clean it up at the end.
  ClangImporterOptions options;
  SmallString<256> temp;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory(
      "ClangImporterTest.emitPCHInMemory", temp));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(temp); };

  // Create a cache subdirectory for the modules and PCH.
  std::string cache = createFilename(temp, "cache");
  ASSERT_FALSE(llvm::sys::fs::create_directory(cache));
  options.ModuleCachePath = cache;
  options.PrecompiledHeaderOutputDir = cache;

  // Create the includes.
  std::string include = createFilename(temp, "include");
  ASSERT_FALSE(llvm::sys::fs::create_directory(include));
  options.ExtraArgs.emplace_back((llvm::Twine("-I") + include).str());
  ASSERT_FALSE(emitFileWithContents(include, "module.modulemap",
                                    "module A {\n"
                                    "  header \"A.h\"\n"
                                    "}\n"));
  ASSERT_FALSE(emitFileWithContents(include, "A.h", "int foo(void);\n"));

  // Create a bridging header.
  ASSERT_FALSE(emitFileWithContents(temp, "bridging.h", "#import <A.h>\n",
                                    &options.BridgingHeader));

  // Set up the importer and emit a bridging PCH.
  swift::LangOptions langOpts;
  langOpts.Target = llvm::Triple("x86_64", "apple", "darwin");
  swift::TypeCheckerOptions typeckOpts;
  INITIALIZE_LLVM();
  swift::SearchPathOptions searchPathOpts;
  swift::SourceManager sourceMgr;
  swift::DiagnosticEngine diags(sourceMgr);
  std::unique_ptr<ASTContext> context(
      ASTContext::get(langOpts, typeckOpts, searchPathOpts, sourceMgr, diags));
  auto importer = ClangImporter::create(*context, options);

  std::string PCH = createFilename(cache, "bridging.h.pch");
  ASSERT_FALSE(importer->canReadPCH(PCH));
  ASSERT_FALSE(importer->emitBridgingPCH(options.BridgingHeader, PCH));
  ASSERT_TRUE(importer->canReadPCH(PCH));

  // Overwrite the PCH with garbage.  We should still be able to read it from
  // the in-memory cache.
  ASSERT_FALSE(emitFileWithContents(PCH, "garbage"));
  ASSERT_TRUE(importer->canReadPCH(PCH));
}

class ForgetfulStatCache : public clang::FileSystemStatCache {
private:
  std::unique_ptr<clang::MemorizeStatCalls> RealStatCache;

public:
  ForgetfulStatCache() {
    RealStatCache = std::make_unique<clang::MemorizeStatCalls>();
  }
  ~ForgetfulStatCache() = default;

  std::error_code getStat(StringRef Path, llvm::vfs::Status &Status,
                          bool isFile,
                          std::unique_ptr<llvm::vfs::File> *F,
                          llvm::vfs::FileSystem &FS) override {
    if (llvm::sys::path::extension(Path) == ".pcm") {
      const auto UID = llvm::sys::fs::UniqueID(1, std::numeric_limits<uint64_t>::max());
      Status = llvm::vfs::Status(Path, UID,
                                 /*MTime*/{}, /*User*/0, /*Group*/0,
                                 /*Size*/0,
                                 llvm::sys::fs::file_type::regular_file,
                                 llvm::sys::fs::perms::all_all);
      return std::error_code();
    }

    return clang::FileSystemStatCache::get(Path, Status, isFile, F,
                                           RealStatCache.get(), FS);
  }
};

TEST(ClangImporterTest, missingSubmodule) {
  // Create a temporary cache on disk and clean it up at the end.
  ClangImporterOptions options;
  SmallString<256> temp;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory(
      "ClangImporterTest.missingSubmodule", temp));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(temp); };

  // Create a cache subdirectory for the modules and PCH.
  std::string cache = createFilename(temp, "cache");
  ASSERT_FALSE(llvm::sys::fs::create_directory(cache));
  options.ModuleCachePath = cache;
  options.PrecompiledHeaderOutputDir = cache;

  // Create the includes.
  std::string include1 = createFilename(temp, "include1");
  ASSERT_FALSE(llvm::sys::fs::create_directory(include1));

  std::string include2 = createFilename(temp, "include2");
  ASSERT_FALSE(llvm::sys::fs::create_directory(include2));

  options.ExtraArgs.emplace_back((llvm::Twine("-I") + include1).str());
  options.ExtraArgs.emplace_back((llvm::Twine("-I") + include2).str());
  options.ExtraArgs.emplace_back("-DEXTRA_C_DEFINE=2");

  ASSERT_FALSE(emitFileWithContents(include1, "module.modulemap",
                                    "module CLib1 {\n"
                                    "  umbrella header \"" + include1 + "/Clib1.h\"\n"
                                    "  export * \n"
                                    "}\n"
                                    "module CLib2 {\n"
                                    "  umbrella header \"" + include2 + "/Clib2.h\"\n"
                                    "  export * \n"
                                    "}\n"));
  ASSERT_FALSE(emitFileWithContents(include1, "CLib1.h",
                                    "#if !defined(EXTRA_C_DEFINE) || EXTRA_C_DEFINE != 2\n"
                                    "#error \"unexpected compiler flags\"\n"
                                    "#endif\n"
                                    "void foo(void);\n"));
  ASSERT_FALSE(emitFileWithContents(include2, "CLib2.h",
                                    "#if !defined(EXTRA_C_DEFINE) || EXTRA_C_DEFINE != 2\n"
                                    "#error \"unexpected compiler flags\"\n"
                                    "#endif\n"
                                    "void foo(void);\n"));

  // Set up the importer.
  swift::LangOptions langOpts;
  langOpts.Target = llvm::Triple("x86_64", "apple", "darwin");
  swift::TypeCheckerOptions typeckOpts;
  INITIALIZE_LLVM();
  swift::SearchPathOptions searchPathOpts;
  swift::SourceManager sourceMgr;
  swift::DiagnosticEngine diags(sourceMgr);
  std::unique_ptr<ASTContext> context(
      ASTContext::get(langOpts, typeckOpts, searchPathOpts, sourceMgr, diags));
  auto importer = ClangImporter::create(*context, options);

  // Install a stats cache that conveniently "forgets" that PCMs have different
  // underlying FileEntry values.
  importer->getClangInstance()
    .getFileManager()
    .setStatCache(std::make_unique<ForgetfulStatCache>());

  context->addModuleLoader(std::move(importer));

  auto CLib1 = context->getIdentifier("CLib1");
  auto CLib2 = context->getIdentifier("CLib2");
  // The first load succeeds and primes the ModuleManager with PCM data.
  (void)context->getModule({ Located<Identifier>{ CLib1, {} } });
  // The second load fails because we collide in the ModuleManager.
  ASSERT_TRUE(context->getModule({ Located<Identifier>{ CLib2, {} } }) == nullptr);
}
