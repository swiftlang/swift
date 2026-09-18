//===---------------------- ModuleDeps.cpp --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ScanFixture.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Platform.h"
#include "swift/DependencyScan/DependencyScanImpl.h"
#include "clang/Frontend/SerializedDiagnosticReader.h"
#include "clang/Frontend/SerializedDiagnostics.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/Triple.h"
#include "gtest/gtest.h"
#include <atomic>
#include <string>
#include <thread>
#include <vector>

using namespace swift;
using namespace swift::unittest;
using namespace swift::dependencies;

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

TEST_F(ScanTest, TestModuleDeps) {
  SmallString<256> tempDir;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory("ScanTest.TestModuleDeps", tempDir));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(tempDir); };

  // Create test input file
  std::string TestPathStr = createFilename(tempDir, "foo.swift");
  ASSERT_FALSE(emitFileWithContents(tempDir, "foo.swift", "import A\n"));

  // Create includes
  std::string IncludeDirPath = createFilename(tempDir, "include");
  ASSERT_FALSE(llvm::sys::fs::create_directory(IncludeDirPath));
  std::string CHeadersDirPath = createFilename(IncludeDirPath, "CHeaders");
  ASSERT_FALSE(llvm::sys::fs::create_directory(CHeadersDirPath));
  std::string SwiftDirPath = createFilename(IncludeDirPath, "Swift");
  ASSERT_FALSE(llvm::sys::fs::create_directory(SwiftDirPath));

  // Create imported module Swift interface files
  ASSERT_FALSE(emitFileWithContents(SwiftDirPath, "A.swiftinterface",
                                    "// swift-interface-format-version: 1.0\n\
// swift-module-flags: -module-name A\n\
import Swift\n\
@_exported import A\n\
public func overlayFuncA() { }\n"));
  ASSERT_FALSE(emitFileWithContents(SwiftDirPath, "E.swiftinterface",
                                    "// swift-interface-format-version: 1.0\n\
// swift-module-flags: -module-name E\n\
import Swift\n\
public func funcE()\n"));
  ASSERT_FALSE(emitFileWithContents(SwiftDirPath, "F.swiftinterface",
                                    "// swift-interface-format-version: 1.0\n\
// swift-module-flags: -module-name\n\
import Swift\n\
@_exported import F\n\
public func funcF() { }"));
  ASSERT_FALSE(emitFileWithContents(SwiftDirPath, "G.swiftinterface",
                                    "// swift-interface-format-version: 1.0\n\
// swift-module-flags: -module-name G -swift-version 5 -target x86_64-apple-macosx10.9\n\
#if swift(>=5.0)\n\
@_exported import G\n\
import Swift\n\
public func overlayFuncG() { }\n\
let stringG : String = \"Build\"\n\
#endif"));

  // Create imported module C modulemap/headers
  ASSERT_FALSE(
      emitFileWithContents(CHeadersDirPath, "A.h", "void funcA(void);"));
  ASSERT_FALSE(emitFileWithContents(CHeadersDirPath, "B.h", "#include \"A.h\"\
void funcB(void);"));
  ASSERT_FALSE(emitFileWithContents(CHeadersDirPath, "C.h", "#include \"B.h\"\n\
void funcC(void);\
const char* stringC() { return \"Module\"; }"));
  ASSERT_FALSE(
      emitFileWithContents(CHeadersDirPath, "D.h", "void funcD(void);"));
  ASSERT_FALSE(
      emitFileWithContents(CHeadersDirPath, "F.h", "void funcF(void);"));
  ASSERT_FALSE(emitFileWithContents(
      CHeadersDirPath, "G.h",
      "#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 110000\n\
#include \"X.h\"\n\
#endif\n\
void funcG(void);"));
  ASSERT_FALSE(
      emitFileWithContents(CHeadersDirPath, "X.h", "void funcX(void);"));
  ASSERT_FALSE(emitFileWithContents(CHeadersDirPath, "Bridging.h",
                                    "#include \"BridgingOther.h\"\n\
int bridging_other(void);"));
  ASSERT_FALSE(emitFileWithContents(CHeadersDirPath, "BridgingOther.h",
                                    "#include \"F.h\"\n\
int bridging_other(void);"));

  ASSERT_FALSE(
      emitFileWithContents(CHeadersDirPath, "module.modulemap", "module A {\n\
header \"A.h\"\n\
export *\n\
}\n\
module B {\n\
header \"B.h\"\n\
export *\n\
}\n\
module C {\n\
header \"C.h\"\n\
export *\n\
}\n\
module D {\n\
header \"D.h\"\n\
export *\n\
}\n\
module F {\n\
header \"F.h\"\n\
export *\n\
}\n\
module G {\n\
header \"G.h\"\n\
export *\n\
}\n\
module X {\n\
header \"X.h\"\n\
export *\n\
}"));

  // Paths to shims and stdlib
  llvm::SmallString<128> ShimsLibDir = StdLibDir;
  llvm::sys::path::append(ShimsLibDir, "shims");
  auto Target = llvm::Triple(llvm::sys::getDefaultTargetTriple());
  llvm::sys::path::append(StdLibDir, getPlatformNameForTriple(Target));

  std::vector<std::string> CommandStrArr = {
    TestPathStr,
    std::string("-I ") + SwiftDirPath,
    std::string("-I ") + CHeadersDirPath,
    std::string("-I ") + StdLibDir.str().str(),
    std::string("-I ") + ShimsLibDir.str().str(),
  };

  // On Windows we need to add an extra escape for path separator characters because otherwise
  // the command line tokenizer will treat them as escape characters.
  for (size_t i = 0; i < CommandStrArr.size(); ++i) {
    std::replace(CommandStrArr[i].begin(), CommandStrArr[i].end(), '\\', '/');
  }

  std::vector<const char*> Command;
  for (auto &command : CommandStrArr) {
    Command.push_back(command.c_str());
  }
  auto DependenciesOrErr = ScannerTool.getDependencies(Command, {});
  ASSERT_FALSE(DependenciesOrErr.getError());
  auto Dependencies = DependenciesOrErr.get();
  // TODO: Output/verify dependency graph correctness
  // llvm::dbgs() << "Deps: " << Dependencies << "\n";

  swiftscan_dependency_graph_dispose(Dependencies);
}

TEST_F(ScanTest, TestModuleDepsHash) {
  SmallString<256> tempDir;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory("ScanTest.TestModuleDepsHash", tempDir));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(tempDir); };

  // Create test input file
  std::string TestPathStr = createFilename(tempDir, "foo.swift");
  ASSERT_FALSE(emitFileWithContents(tempDir, "foo.swift", "import A\n"));

  // Create includes
  std::string IncludeDirPath = createFilename(tempDir, "include");
  ASSERT_FALSE(llvm::sys::fs::create_directory(IncludeDirPath));
  std::string SwiftDirPath = createFilename(IncludeDirPath, "Swift");
  ASSERT_FALSE(llvm::sys::fs::create_directory(SwiftDirPath));

  // Create imported module Swift interface files
  ASSERT_FALSE(emitFileWithContents(SwiftDirPath, "A.swiftinterface",
                                    "// swift-interface-format-version: 1.0\n\
// swift-module-flags: -module-name A\n\
import Swift\n\
public func overlayFuncA() { }\n"));

  // Paths to shims and stdlib
  llvm::SmallString<128> ShimsLibDir = StdLibDir;
  llvm::sys::path::append(ShimsLibDir, "shims");
  auto Target = llvm::Triple(llvm::sys::getDefaultTargetTriple());
  llvm::sys::path::append(StdLibDir, getPlatformNameForTriple(Target));

  std::vector<std::string> BaseCommandStrArr = {
    TestPathStr,
    std::string("-I ") + SwiftDirPath,
    std::string("-I ") + StdLibDir.str().str(),
    std::string("-I ") + ShimsLibDir.str().str(),
  };

  std::vector<std::string> CommandStrArrA = BaseCommandStrArr;
  CommandStrArrA.push_back("-module-name");
  CommandStrArrA.push_back("A");
  std::vector<std::string> CommandStrArrB = BaseCommandStrArr;
  CommandStrArrB.push_back("-module-name");
  CommandStrArrB.push_back("B");

  // On Windows we need to add an extra escape for path separator characters because otherwise
  // the command line tokenizer will treat them as escape characters.
  for (size_t i = 0; i < CommandStrArrA.size(); ++i) {
    std::replace(CommandStrArrA[i].begin(), CommandStrArrA[i].end(), '\\', '/');
  }
  std::vector<const char*> CommandA;
  for (auto &command : CommandStrArrA) {
    CommandA.push_back(command.c_str());
  }

  for (size_t i = 0; i < CommandStrArrB.size(); ++i) {
    std::replace(CommandStrArrB[i].begin(), CommandStrArrB[i].end(), '\\', '/');
  }
  std::vector<const char*> CommandB;
  for (auto &command : CommandStrArrB) {
    CommandB.push_back(command.c_str());
  }

  std::vector<DepScanInMemoryDiagnosticCollector::ScannerDiagnosticInfo>
      InitializationDiagnostics;
  auto queryAContext = ScannerTool.createScanQueryContext(CommandA, {},
                                                          InitializationDiagnostics);
  auto queryBContext = ScannerTool.createScanQueryContext(CommandB, {},
                                                          InitializationDiagnostics);
  // Ensure that scans that only differ in module name have distinct scanning context hashes
  ASSERT_NE(queryAContext->ScanInstance.get()->getInvocation().getModuleScanningHash(),
            queryBContext->ScanInstance.get()->getInvocation().getModuleScanningHash());
}

namespace {
class DiagnosticChecker : public clang::serialized_diags::SerializedDiagnosticReader {
public:
  std::vector<std::string> errorMessages;
  std::vector<std::string> warningMessages;

protected:
  std::error_code visitDiagnosticRecord(
      unsigned Severity, const clang::serialized_diags::Location &Location,
      unsigned Category, unsigned Flag, StringRef Message) override {
      switch (static_cast<clang::serialized_diags::Level>(Severity)) {
        case clang::serialized_diags::Warning:
          warningMessages.push_back(Message.str());
          break;
        case clang::serialized_diags::Error:
          errorMessages.push_back(Message.str());
          break;
        default:
          break;
      }
      return std::error_code();
  }
};
}


TEST_F(ScanTest, TestSerializedDiagnosticOutput) {
  SmallString<256> tempDir;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory(
      "ScanTest.TestSerializedDiagnosticOutput", tempDir));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(tempDir); };

  // Create test input file
  std::string TestPathStr = createFilename(tempDir, "foo.swift");
  ASSERT_FALSE(emitFileWithContents(tempDir, "foo.swift", "import A\n\
#warning(\"This is a warning\")\n"));

  // Create include directory
  std::string SwiftDirPath = createFilename(tempDir, "Swift");
  ASSERT_FALSE(llvm::sys::fs::create_directory(SwiftDirPath));

  // Create output directory
  std::string OutputDirPath = createFilename(tempDir, "Output");
  ASSERT_FALSE(llvm::sys::fs::create_directory(OutputDirPath));
  std::string SerializedDiagnosticsOutputPath =
      createFilename(OutputDirPath, "scan-diags.dia");

  // Create imported module Swift interface files
  ASSERT_FALSE(emitFileWithContents(SwiftDirPath, "A.swiftinterface",
                                    "// swift-interface-format-version: 1.0\n\
// swift-module-flags: -module-name A\n\
import Swift\n\
#error(\"This is an error\")\n\
public func funcA() { }\n"));

  // Paths to shims and stdlib
  llvm::SmallString<128> ShimsLibDir = StdLibDir;
  llvm::sys::path::append(ShimsLibDir, "shims");
  auto Target = llvm::Triple(llvm::sys::getDefaultTargetTriple());
  llvm::sys::path::append(StdLibDir, getPlatformNameForTriple(Target));

  // Generate command line
  std::vector<std::string> CommandStr = {
      TestPathStr,
      "-I", SwiftDirPath,
      "-I", StdLibDir.str().str(),
      "-I", ShimsLibDir.str().str(),
      "-serialize-diagnostics-path", SerializedDiagnosticsOutputPath,
      "-module-name", "testSerializedDiagnosticOutput"};
  // On Windows we need to add an extra escape for path separator characters
  // because otherwise the command line tokenizer will treat them as escape
  // characters.
  for (size_t i = 0; i < CommandStr.size(); ++i) {
    std::replace(CommandStr[i].begin(), CommandStr[i].end(), '\\', '/');
  }
  std::vector<const char *> Command;
  for (auto &command : CommandStr)
    Command.push_back(command.c_str());

  {
    auto ScanningService = std::make_unique<SwiftDependencyScanningService>();
    std::vector<DepScanInMemoryDiagnosticCollector::ScannerDiagnosticInfo>
        InitializationDiagnostics;
    auto QueryContext = ScannerTool.createScanQueryContext(Command, {},
                                                           InitializationDiagnostics);
    ASSERT_FALSE(QueryContext.getError());

    ModuleDependenciesCache ScanCache(
        QueryContext->ScanInstance.get()->getMainModule()->getNameStr().str(),
        QueryContext->ScanInstance.get()
            ->getInvocation()
            .getModuleScanningHash());
    auto DependenciesOrErr =
        performModuleScan(*ScanningService, ScanCache, *QueryContext);

    ASSERT_FALSE(DependenciesOrErr.getError());
  }
  ASSERT_TRUE(llvm::sys::fs::exists(SerializedDiagnosticsOutputPath));

  auto DiagnosticsReader = DiagnosticChecker();
  auto ReadError = DiagnosticsReader.readDiagnostics(SerializedDiagnosticsOutputPath);
  ASSERT_FALSE(ReadError);
  ASSERT_EQ(DiagnosticsReader.errorMessages.size(), static_cast<size_t>(1));
  ASSERT_EQ(DiagnosticsReader.warningMessages.size(), static_cast<size_t>(1));
  EXPECT_EQ(DiagnosticsReader.errorMessages.front(), "This is an error");
  EXPECT_EQ(DiagnosticsReader.warningMessages.front(), "This is a warning");
}

TEST_F(ScanTest, TestEscapedCommandLine) {
  llvm::ErrorOr<swiftscan_string_ref_t> information =
      getTargetInfo({
                      "-sdk",
#if defined(_WIN32)
                      "    C:\\Program Files\\Swift\\Platforms\\Windows.platform\\Developer\\SDKs\\Windows.sdk\\usr\\include",
#else
                      "C:\\\\Program\\ Files\\\\Swift\\\\Platforms\\\\Windows.platform\\\\Developer\\\\SDKs\\\\Windows.sdk\\\\usr\\\\include",
#endif
                    },
                    "swiftc");
  ASSERT_TRUE(information);
  llvm::StringRef Result{static_cast<const char *>(information->data),
                         information->length};
  ASSERT_NE(Result, llvm::StringRef{});
  llvm::Expected<llvm::json::Value> V = llvm::json::parse(Result);
  ASSERT_TRUE(static_cast<bool>(V));
  ASSERT_EQ(V->getAsObject()->getObject("paths")->getString("sdkPath"),
            "C:\\Program Files\\Swift\\Platforms\\Windows.platform\\Developer\\SDKs\\Windows.sdk\\usr\\include");
}

// Disabled due to rdar://165014838
TEST_F(ScanTest, DISABLED_TestModuleCycle) {
  SmallString<256> tempDir;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory("ScanTest.TestModuleCycle", tempDir));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(tempDir); };

  // Create test input file
  std::string TestPathStr = createFilename(tempDir, "foo.swift");
  ASSERT_FALSE(emitFileWithContents(tempDir, "foo.swift", "import A\n"));

  // Create includes
  std::string IncludeDirPath = createFilename(tempDir, "include");
  ASSERT_FALSE(llvm::sys::fs::create_directory(IncludeDirPath));
  std::string SwiftDirPath = createFilename(IncludeDirPath, "Swift");
  ASSERT_FALSE(llvm::sys::fs::create_directory(SwiftDirPath));

  // Create imported module Swift interface files
  ASSERT_FALSE(emitFileWithContents(SwiftDirPath, "A.swiftinterface",
                                    "// swift-interface-format-version: 1.0\n\
// swift-module-flags: -module-name A\n\
import Swift\n\
import B\n\
public func funcA() { }\n"));
  ASSERT_FALSE(emitFileWithContents(SwiftDirPath, "B.swiftinterface",
                                    "// swift-interface-format-version: 1.0\n\
// swift-module-flags: -module-name B\n\
import Swift\n\
import A\n\
public func funcB() { }\n"));

  // Paths to shims and stdlib
  llvm::SmallString<128> ShimsLibDir = StdLibDir;
  llvm::sys::path::append(ShimsLibDir, "shims");
  auto Target = llvm::Triple(llvm::sys::getDefaultTargetTriple());
  llvm::sys::path::append(StdLibDir, getPlatformNameForTriple(Target));

  std::vector<std::string> BaseCommandStrArr = {
    TestPathStr,
    std::string("-I ") + SwiftDirPath,
    std::string("-I ") + StdLibDir.str().str(),
    std::string("-I ") + ShimsLibDir.str().str(),
  };

  std::vector<std::string> CommandStr = BaseCommandStrArr;
  CommandStr.push_back("-module-name");
  CommandStr.push_back("test");

  // On Windows we need to add an extra escape for path separator characters because otherwise
  // the command line tokenizer will treat them as escape characters.
  for (size_t i = 0; i < CommandStr.size(); ++i) {
    std::replace(CommandStr[i].begin(), CommandStr[i].end(), '\\', '/');
  }
  std::vector<const char*> Command;
  for (auto &command : CommandStr)
    Command.push_back(command.c_str());

  auto DependenciesOrErr = ScannerTool.getDependencies(Command, {});

  // Ensure a hollow output with diagnostic info is produced
  ASSERT_FALSE(DependenciesOrErr.getError());
  auto Dependencies = DependenciesOrErr.get();
  auto Diagnostics = Dependencies->diagnostics;
  ASSERT_TRUE(Diagnostics->count == 1);
  auto Diagnostic = Diagnostics->diagnostics[0];
  ASSERT_TRUE(Diagnostic->severity == SWIFTSCAN_DIAGNOSTIC_SEVERITY_ERROR);
  auto Message = std::string((const char*)Diagnostic->message.data,
                             Diagnostic->message.length);
  ASSERT_TRUE(Message == "module dependency cycle: 'A.swiftinterface -> B.swiftinterface -> A.swiftinterface'\n");

  // Ensure hollow output is hollow
  ASSERT_TRUE(Dependencies->dependencies->count == 1);
  ASSERT_TRUE(Dependencies->dependencies->modules[0]->source_files->count == 0);
  ASSERT_TRUE(Dependencies->dependencies->modules[0]->direct_dependencies->count == 0);
  ASSERT_TRUE(Dependencies->dependencies->modules[0]->link_libraries->count == 0);
  swiftscan_dependency_graph_dispose(Dependencies);
}

TEST_F(ScanTest, TestStressConcurrentDiagnostics) {
  SmallString<256> tempDir;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory("ScanTest.TestStressConcurrentDiagnostics", tempDir));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(tempDir); };

  // Create includes
  std::string IncludeDirPath = createFilename(tempDir, "include");
  ASSERT_FALSE(llvm::sys::fs::create_directory(IncludeDirPath));
  std::string CHeadersDirPath = createFilename(IncludeDirPath, "CHeaders");
  ASSERT_FALSE(llvm::sys::fs::create_directory(CHeadersDirPath));

  // Create test input file
  std::string TestPathStr = createFilename(tempDir, "foo.swift");

  // Create imported module C modulemap/headers
  std::string modulemapContent = "";
  std::string testFileContent = "";
  for (int i = 0; i < 100; ++i) {
    std::string headerName = "A_" + std::to_string(i) + ".h";
    std::string headerContent = "void funcA_" + std::to_string(i) + "(void);";
    ASSERT_FALSE(
        emitFileWithContents(CHeadersDirPath, headerName, headerContent));

    std::string moduleMapEntry = "module A_" + std::to_string(i) + "{\n";
    moduleMapEntry.append("header \"A_" + std::to_string(i) + ".h\"\n");
    moduleMapEntry.append("export *\n");
    moduleMapEntry.append("}\n");
    modulemapContent.append(moduleMapEntry);
    testFileContent.append("import A_" + std::to_string(i) + "\n");
  }

  ASSERT_FALSE(emitFileWithContents(tempDir, "foo.swift", testFileContent));
  ASSERT_FALSE(
      emitFileWithContents(CHeadersDirPath, "module.modulemap", modulemapContent));

  // Paths to shims and stdlib
  llvm::SmallString<128> ShimsLibDir = StdLibDir;
  llvm::sys::path::append(ShimsLibDir, "shims");
  auto Target = llvm::Triple(llvm::sys::getDefaultTargetTriple());
  llvm::sys::path::append(StdLibDir, getPlatformNameForTriple(Target));

  std::vector<std::string> BaseCommandStrArr = {
    TestPathStr,
    std::string("-I ") + CHeadersDirPath,
    std::string("-I ") + StdLibDir.str().str(),
    std::string("-I ") + ShimsLibDir.str().str(),
    // Pass in a flag which will cause every instantiation of
    // the clang scanner to fail with "unknown argument"
    "-Xcc",
    "-foobar"
  };

  std::vector<std::string> CommandStr = BaseCommandStrArr;
  CommandStr.push_back("-module-name");
  CommandStr.push_back("testConcurrentDiags");

  // On Windows we need to add an extra escape for path separator characters because otherwise
  // the command line tokenizer will treat them as escape characters.
  for (size_t i = 0; i < CommandStr.size(); ++i) {
    std::replace(CommandStr[i].begin(), CommandStr[i].end(), '\\', '/');
  }
  std::vector<const char*> Command;
  for (auto &command : CommandStr)
    Command.push_back(command.c_str());

  auto DependenciesOrErr = ScannerTool.getDependencies(Command, {});

  // Ensure a hollow output with diagnostic info is produced
  ASSERT_FALSE(DependenciesOrErr.getError());
  auto Dependencies = DependenciesOrErr.get();
  auto Diagnostics = Dependencies->diagnostics;
  ASSERT_TRUE(Diagnostics->count >= 1);
  swiftscan_dependency_graph_dispose(Dependencies);
}

// Set up a workspace importing enough C modules that each scan spends a while
// in the Clang dependency scanner, and build the scanner command line for it.
static void makeConcurrentQueryCommand(StringRef tempDir, StringRef stdLibDir,
                                       StringRef moduleName,
                                       ArrayRef<std::string> extraArgs,
                                       std::vector<std::string> &commandOut) {
  // Create includes
  std::string IncludeDirPath = createFilename(tempDir, "include");
  ASSERT_FALSE(llvm::sys::fs::create_directory(IncludeDirPath));
  std::string CHeadersDirPath = createFilename(IncludeDirPath, "CHeaders");
  ASSERT_FALSE(llvm::sys::fs::create_directory(CHeadersDirPath));

  // Create test input file
  std::string TestPathStr = createFilename(tempDir, "foo.swift");

  // Create enough imported C modules for each query to spend a while in the
  // Clang dependency scanner, so that the queries actually overlap.
  std::string modulemapContent = "";
  std::string testFileContent = "";
  for (int i = 0; i < 50; ++i) {
    std::string headerName = "A_" + std::to_string(i) + ".h";
    std::string headerContent = "void funcA_" + std::to_string(i) + "(void);";
    ASSERT_FALSE(
        emitFileWithContents(CHeadersDirPath, headerName, headerContent));

    std::string moduleMapEntry = "module A_" + std::to_string(i) + " {\n";
    moduleMapEntry.append("header \"A_" + std::to_string(i) + ".h\"\n");
    moduleMapEntry.append("export *\n");
    moduleMapEntry.append("}\n");
    modulemapContent.append(moduleMapEntry);
    testFileContent.append("import A_" + std::to_string(i) + "\n");
  }

  ASSERT_FALSE(emitFileWithContents(tempDir, "foo.swift", testFileContent));
  ASSERT_FALSE(emitFileWithContents(CHeadersDirPath, "module.modulemap",
                                    modulemapContent));

  // Paths to shims and stdlib
  llvm::SmallString<128> ShimsLibDir = stdLibDir;
  llvm::sys::path::append(ShimsLibDir, "shims");
  llvm::SmallString<128> PlatformStdLibDir = stdLibDir;
  auto Target = llvm::Triple(llvm::sys::getDefaultTargetTriple());
  llvm::sys::path::append(PlatformStdLibDir, getPlatformNameForTriple(Target));

  // Note: '-I' and its value are kept as separate arguments. Combining them
  // into "-I <path>" leaves a leading space in the path, which breaks search
  // path resolution once a working directory is in play.
  commandOut = {
      TestPathStr,
      "-I",
      CHeadersDirPath,
      "-I",
      PlatformStdLibDir.str().str(),
      "-I",
      ShimsLibDir.str().str(),
      "-module-name",
      moduleName.str(),
  };
  commandOut.insert(commandOut.end(), extraArgs.begin(), extraArgs.end());

  // On Windows we need to add an extra escape for path separator characters
  // because otherwise the command line tokenizer will treat them as escape
  // characters.
  for (auto &arg : commandOut)
    std::replace(arg.begin(), arg.end(), '\\', '/');
}

// Issue several identical full-scan queries concurrently through one tool and
// check that they agree. Disagreement means a query resolved dependencies
// through another query's file system.
static void runConcurrentQueries(DependencyScanningTool &tool,
                                 ArrayRef<std::string> commandStr,
                                 StringRef workingDir = {}) {
  std::vector<const char *> Command;
  for (auto &command : commandStr)
    Command.push_back(command.c_str());

  constexpr unsigned NumQueries = 4;
  std::atomic<unsigned> NumReady(0);
  std::atomic<unsigned> NumFailed(0);
  std::atomic<size_t> ModuleCounts[NumQueries] = {};
  std::vector<std::thread> Queries;
  Queries.reserve(NumQueries);
  for (unsigned i = 0; i < NumQueries; ++i) {
    Queries.emplace_back([&, i]() {
      // Start all of the queries at roughly the same time to maximize the
      // overlap between them.
      ++NumReady;
      while (NumReady.load() < NumQueries)
        std::this_thread::yield();

      auto DependenciesOrErr = tool.getDependencies(Command, workingDir);
      if (DependenciesOrErr.getError()) {
        ++NumFailed;
        return;
      }
      auto Dependencies = DependenciesOrErr.get();
      ModuleCounts[i].store(Dependencies->dependencies->count);
      swiftscan_dependency_graph_dispose(Dependencies);
    });
  }

  for (auto &Query : Queries)
    Query.join();

  ASSERT_EQ(NumFailed.load(), 0u);

  // The queries are identical, so they must agree.
  for (unsigned i = 1; i < NumQueries; ++i)
    ASSERT_EQ(ModuleCounts[i].load(), ModuleCounts[0].load());
}

// Ensure that full-scan queries issued concurrently against a single
// `DependencyScanningTool` do not interfere with one another. Each query gets
// its own `ModuleDependencyScanner`, and anything that scanner shares with the
// tool must therefore tolerate being used by several scans at once.
TEST_F(ScanTest, TestConcurrentQueries) {
  SmallString<256> tempDir;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory(
      "ScanTest.TestConcurrentQueries", tempDir));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(tempDir); };

  std::vector<std::string> CommandStr;
  makeConcurrentQueryCommand(tempDir, StdLibDir, "testConcurrentQueries",
                             /*extraArgs=*/{}, CommandStr);
  ASSERT_FALSE(CommandStr.empty());

  runConcurrentQueries(ScannerTool, CommandStr);
}

// As above, but with compiler caching enabled, which installs a different
// `DependencyScanningServiceOptions::MakeVFS` callback. That callback must also
// build a separate file system on every call: the Clang dependency scanner calls
// `setCurrentWorkingDirectory` on whatever file system it is handed, so workers
// sharing one corrupt each other's working directory (rdar://184810704). Run
// under ASan/TSan to catch the heap corruption rather than just the disagreeing
// results.
TEST_F(ScanTest, TestConcurrentCachingQueries) {
  SmallString<256> tempDir;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory(
      "ScanTest.TestConcurrentCachingQueries", tempDir));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(tempDir); };

  // Scan with a deliberately long working directory. `RealFileSystem` stores its
  // working directory in a `SmallString<128>`, so a shared file system only
  // corrupts the heap once the path exceeds that inline capacity -- with a short
  // path the racing writes stay inside the object and the bug hides.
  std::string LongWorkingDir =
      createFilename(createFilename(tempDir, std::string(80, 'w')),
                     std::string(80, 'd'));
  ASSERT_FALSE(llvm::sys::fs::create_directories(LongWorkingDir));
  ASSERT_GT(LongWorkingDir.size(), 128u);

  std::vector<std::string> ExtraArgs = {
      "-cache-compile-job",
      "-cas-path",
      createFilename(tempDir, "cas"),
      "-module-cache-path",
      createFilename(tempDir, "clang-module-cache"),
  };

  std::vector<std::string> CommandStr;
  makeConcurrentQueryCommand(tempDir, StdLibDir, "testConcurrentCachingQueries",
                             ExtraArgs, CommandStr);
  ASSERT_FALSE(CommandStr.empty());

  // Confirm that caching really is enabled for this command line, so that this
  // test cannot silently degrade into a duplicate of TestConcurrentQueries. With
  // caching on, Clang module dependencies carry an include-tree CASID.
  std::vector<const char *> Command;
  for (auto &command : CommandStr)
    Command.push_back(command.c_str());
  auto DependenciesOrErr = ScannerTool.getDependencies(Command, LongWorkingDir);
  ASSERT_FALSE(DependenciesOrErr.getError());
  auto Dependencies = DependenciesOrErr.get();
  unsigned NumClangModulesWithIncludeTree = 0;
  for (size_t i = 0; i < Dependencies->dependencies->count; ++i) {
    auto *Details = Dependencies->dependencies->modules[i]->details;
    if (Details->kind == SWIFTSCAN_DEPENDENCY_INFO_CLANG &&
        Details->clang_details.clang_include_tree.length)
      ++NumClangModulesWithIncludeTree;
  }
  swiftscan_dependency_graph_dispose(Dependencies);
  ASSERT_GT(NumClangModulesWithIncludeTree, 0u);

  runConcurrentQueries(ScannerTool, CommandStr, LongWorkingDir);
}

