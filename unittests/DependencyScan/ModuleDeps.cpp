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
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/Triple.h"
#include "gtest/gtest.h"
#include <string>

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
  auto DependenciesOrErr = ScannerTool.getDependencies(Command, {}, {});
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

  auto ScanDiagnosticConsumer = std::make_shared<DependencyScanDiagnosticCollector>();
  auto instanceA = ScannerTool.initCompilerInstanceForScan(CommandA, {}, ScanDiagnosticConsumer);
  auto instanceB = ScannerTool.initCompilerInstanceForScan(CommandB, {}, ScanDiagnosticConsumer);
  // Ensure that scans that only differ in module name have distinct scanning context hashes
  ASSERT_NE(instanceA->ScanInstance.get()->getInvocation().getModuleScanningHash(),
            instanceB->ScanInstance.get()->getInvocation().getModuleScanningHash());
}

TEST_F(ScanTest, TestModuleCycle) {
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

  auto ScanDiagnosticConsumer = std::make_shared<DependencyScanDiagnosticCollector>();

  auto DependenciesOrErr = ScannerTool.getDependencies(Command, {}, {});

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
