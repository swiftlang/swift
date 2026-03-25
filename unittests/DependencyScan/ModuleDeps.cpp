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
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TargetParser/Host.h"
#include "gtest/gtest.h"
#include <string>

using namespace swift::unittest;
using llvm::SmallString;
using llvm::StringRef;

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
  llvm::sys::path::append(StdLibDir, SWIFTLIB_PLATFORM_SUBDIR);

  std::vector<std::string> CommandStrArr = {
      TestPathStr,
      "-I",
      SwiftDirPath,
      "-I",
      CHeadersDirPath,
      "-I",
      StdLibDir.str().str(),
      "-I",
      ShimsLibDir.str().str(),
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
  auto Graph = performScan(Command);
  ASSERT_TRUE(Graph);
  // TODO: Output/verify dependency graph correctness
  // llvm::dbgs() << "Deps: " << Graph << "\n";

  swiftscan_dependency_graph_dispose(Graph);
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
  llvm::sys::path::append(StdLibDir, SWIFTLIB_PLATFORM_SUBDIR);

  std::vector<std::string> BaseCommandStrArr = {
      TestPathStr,
      "-I",
      SwiftDirPath,
      "-I",
      StdLibDir.str().str(),
      "-I",
      ShimsLibDir.str().str(),
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

  // Perform two scans that only differ in module name
  auto DepsA = performScan(CommandA);
  ASSERT_TRUE(DepsA);
  auto DepsB = performScan(CommandB);
  ASSERT_TRUE(DepsB);

  // Walk the dependency modules and collect context hashes from Swift textual
  // dependencies. Scans with different module names should produce different
  // context hashes.
  auto *ModulesA = swiftscan_dependency_graph_get_dependencies(DepsA);
  auto *ModulesB = swiftscan_dependency_graph_get_dependencies(DepsB);
  ASSERT_TRUE(ModulesA && ModulesA->count > 0);
  ASSERT_TRUE(ModulesB && ModulesB->count > 0);

  // Get the context hash from the main module of each scan
  auto MainHashA = swiftscan_dependency_graph_get_main_module_name(DepsA);
  auto MainHashB = swiftscan_dependency_graph_get_main_module_name(DepsB);
  // The main module names should differ
  ASSERT_NE(
      std::string(static_cast<const char *>(MainHashA.data), MainHashA.length),
      std::string(static_cast<const char *>(MainHashB.data), MainHashB.length));

  swiftscan_dependency_graph_dispose(DepsA);
  swiftscan_dependency_graph_dispose(DepsB);
}

TEST_F(ScanTest, TestDiagnosticOutput) {
  SmallString<256> tempDir;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory(
      "ScanTest.TestDiagnosticOutput", tempDir));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(tempDir); };

  // Create test input file with a warning
  std::string TestPathStr = createFilename(tempDir, "foo.swift");
  ASSERT_FALSE(emitFileWithContents(tempDir, "foo.swift", "import A\n\
#warning(\"This is a warning\")\n"));

  // Create include directory
  std::string SwiftDirPath = createFilename(tempDir, "Swift");
  ASSERT_FALSE(llvm::sys::fs::create_directory(SwiftDirPath));

  // Create imported module Swift interface file with an error
  ASSERT_FALSE(emitFileWithContents(SwiftDirPath, "A.swiftinterface",
                                    "// swift-interface-format-version: 1.0\n\
// swift-module-flags: -module-name A\n\
import Swift\n\
#error(\"This is an error\")\n\
public func funcA() { }\n"));

  // Paths to shims and stdlib
  llvm::SmallString<128> ShimsLibDir = StdLibDir;
  llvm::sys::path::append(ShimsLibDir, "shims");
  llvm::sys::path::append(StdLibDir, SWIFTLIB_PLATFORM_SUBDIR);

  // Generate command line
  std::vector<std::string> CommandStr = {TestPathStr,
                                         "-I",
                                         SwiftDirPath,
                                         "-I",
                                         StdLibDir.str().str(),
                                         "-I",
                                         ShimsLibDir.str().str(),
                                         "-module-name",
                                         "testDiagnosticOutput"};
  for (size_t i = 0; i < CommandStr.size(); ++i) {
    std::replace(CommandStr[i].begin(), CommandStr[i].end(), '\\', '/');
  }
  std::vector<const char *> Command;
  for (auto &command : CommandStr)
    Command.push_back(command.c_str());

  auto Graph = performScan(Command);
  ASSERT_TRUE(Graph);

  // Check diagnostics via C API
  auto *Diagnostics = swiftscan_dependency_graph_get_diagnostics(Graph);
  ASSERT_TRUE(Diagnostics);

  // Collect errors and warnings
  std::vector<std::string> errorMessages;
  std::vector<std::string> warningMessages;
  for (size_t i = 0; i < Diagnostics->count; ++i) {
    auto severity =
        swiftscan_diagnostic_get_severity(Diagnostics->diagnostics[i]);
    auto message =
        swiftscan_diagnostic_get_message(Diagnostics->diagnostics[i]);
    std::string msg(static_cast<const char *>(message.data), message.length);
    if (severity == SWIFTSCAN_DIAGNOSTIC_SEVERITY_ERROR)
      errorMessages.push_back(msg);
    else if (severity == SWIFTSCAN_DIAGNOSTIC_SEVERITY_WARNING)
      warningMessages.push_back(msg);
  }

  ASSERT_EQ(errorMessages.size(), static_cast<size_t>(1));
  ASSERT_EQ(warningMessages.size(), static_cast<size_t>(1));
  EXPECT_NE(errorMessages.front().find("This is an error"), std::string::npos);
  EXPECT_NE(warningMessages.front().find("This is a warning"),
            std::string::npos);

  swiftscan_dependency_graph_dispose(Graph);
}

TEST_F(ScanTest, TestEscapedCommandLine) {
  std::vector<const char *> args = {
      "-sdk",
#if defined(_WIN32)
      "    C:\\Program "
      "Files\\Swift\\Platforms\\Windows.platform\\Developer\\SDKs\\Windows."
      "sdk\\usr\\include",
#else
      "C:\\\\Program\\ "
      "Files\\\\Swift\\\\Platforms\\\\Windows."
      "platform\\\\Developer\\\\SDKs\\\\Windows.sdk\\\\usr\\\\include",
#endif
  };
  auto invocation = swiftscan_scan_invocation_create();
  swiftscan_scan_invocation_set_working_directory(invocation, "");
  swiftscan_scan_invocation_set_argv(invocation, static_cast<int>(args.size()),
                                     (const char **)args.data());
  auto information =
      swiftscan_compiler_target_info_query_v2(invocation, "swiftc");
  swiftscan_scan_invocation_dispose(invocation);
  std::string Result(static_cast<const char *>(information.data),
                     information.length);
  ASSERT_FALSE(Result.empty());
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
  llvm::sys::path::append(StdLibDir, SWIFTLIB_PLATFORM_SUBDIR);

  std::vector<std::string> BaseCommandStrArr = {
      TestPathStr,
      "-I",
      SwiftDirPath,
      "-I",
      StdLibDir.str().str(),
      "-I",
      ShimsLibDir.str().str(),
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

  auto Graph = performScan(Command);

  // Ensure a hollow output with diagnostic info is produced
  ASSERT_TRUE(Graph);
  auto *Diagnostics = swiftscan_dependency_graph_get_diagnostics(Graph);
  ASSERT_TRUE(Diagnostics->count == 1);
  auto Diagnostic = Diagnostics->diagnostics[0];
  ASSERT_TRUE(swiftscan_diagnostic_get_severity(Diagnostic) ==
              SWIFTSCAN_DIAGNOSTIC_SEVERITY_ERROR);
  auto msg_ref = swiftscan_diagnostic_get_message(Diagnostic);
  auto Message =
      std::string(static_cast<const char *>(msg_ref.data), msg_ref.length);
  ASSERT_TRUE(Message == "module dependency cycle: 'A.swiftinterface -> B.swiftinterface -> A.swiftinterface'\n");

  // Ensure hollow output is hollow
  auto Dependencies = swiftscan_dependency_graph_get_dependencies(Graph);
  ASSERT_EQ(Dependencies->count, 1);
  auto SourceFiles =
      swiftscan_module_info_get_source_files(Dependencies->modules[0]);
  ASSERT_EQ(SourceFiles->count, 0);
  auto DirectDependencies =
      swiftscan_module_info_get_direct_dependencies(Dependencies->modules[0]);
  ASSERT_EQ(DirectDependencies->count, 0);
  auto LinkLibraries =
      swiftscan_module_info_get_link_libraries(Dependencies->modules[0]);
  ASSERT_EQ(LinkLibraries->count, 0);
  swiftscan_dependency_graph_dispose(Graph);
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
  llvm::sys::path::append(StdLibDir, SWIFTLIB_PLATFORM_SUBDIR);

  std::vector<std::string> BaseCommandStrArr = {
      TestPathStr, "-I", CHeadersDirPath, "-I", StdLibDir.str().str(), "-I",
      ShimsLibDir.str().str(),
      // Pass in a flag which will cause every instantiation of
      // the clang scanner to fail with "unknown argument"
      "-Xcc", "-foobar"};

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

  auto Graph = performScan(Command);

  // Ensure a hollow output with diagnostic info is produced
  ASSERT_TRUE(Graph);
  auto *Diagnostics = swiftscan_dependency_graph_get_diagnostics(Graph);
  ASSERT_TRUE(Diagnostics->count >= 1);
  swiftscan_dependency_graph_dispose(Graph);
}
