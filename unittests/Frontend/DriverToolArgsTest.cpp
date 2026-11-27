//===--- DriverToolArgsTest.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Option/Options.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::options;

TEST(DriverToolArgs, UnwrapsXFrontendAndInterleavedToolFlags) {
  std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
  auto FS = llvm::makeIntrusiveRefCnt<llvm::vfs::InMemoryFileSystem>();
  const llvm::opt::OptSpecifier Excluded[] = {OPT_help, OPT_module_name};

  // Interleaved multi-token -Xfrontend option with direct driver tool flags.
  {
    const char *RawArgs[] = {
        "-Xfrontend", "-target-sdk-version", "-module-name", "Foo",
        "-Xfrontend", "15.0",                "-output-dir",  "/tmp/out",
    };
    SourceManager SM;
    DiagnosticEngine Diags(SM);
    llvm::opt::InputArgList ParsedArgs(nullptr, nullptr);
    ASSERT_FALSE(CompilerInvocation::parseDriverToolArgs(
        *Table, RawArgs, ParsedArgs, SwiftSymbolGraphExtractOption, Diags));
    EXPECT_EQ(ParsedArgs.getLastArgValue(OPT_module_name), "Foo");
    EXPECT_EQ(ParsedArgs.getLastArgValue(OPT_output_dir), "/tmp/out");

    CompilerInvocation Invocation;
    Invocation.setModuleName("swift_symbolgraph_extract");
    ASSERT_FALSE(Invocation.parseDriverToolFrontendArgs(
        ParsedArgs, Excluded, Diags, /*workingDirectory=*/{}, {}, FS));
    EXPECT_FALSE(Diags.hadAnyError());
    ASSERT_TRUE(Invocation.getLangOptions().SDKVersion.has_value());
    EXPECT_EQ(Invocation.getLangOptions().SDKVersion->getAsString(), "15.0");
  }

  // -Xfrontend arguments are appended after direct frontend arguments
  // (matching swift-driver), while preserving relative order among -Xfrontend
  // flags.
  {
    const char *RawArgs[] = {
        "-target-sdk-version", "14.0", "-Xfrontend", "-target-sdk-version",
        "-Xfrontend",          "15.0", "-Xfrontend", "-target-sdk-version",
        "-Xfrontend",          "16.0",
    };
    SourceManager SM;
    DiagnosticEngine Diags(SM);
    llvm::opt::InputArgList ParsedArgs(nullptr, nullptr);
    ASSERT_FALSE(CompilerInvocation::parseDriverToolArgs(
        *Table, RawArgs, ParsedArgs, SwiftSymbolGraphExtractOption, Diags));

    CompilerInvocation Invocation;
    Invocation.setModuleName("swift_symbolgraph_extract");
    ASSERT_FALSE(Invocation.parseDriverToolFrontendArgs(
        ParsedArgs, Excluded, Diags, /*workingDirectory=*/{}, {}, FS));
    EXPECT_FALSE(Diags.hadAnyError());
    ASSERT_TRUE(Invocation.getLangOptions().SDKVersion.has_value());
    EXPECT_EQ(Invocation.getLangOptions().SDKVersion->getAsString(), "16.0");
  }
}

TEST(DriverToolArgs, RejectsInvalidXFrontendArgs) {
  std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
  auto FS = llvm::makeIntrusiveRefCnt<llvm::vfs::InMemoryFileSystem>();
  const llvm::opt::OptSpecifier Excluded[] = {OPT_help, OPT_module_name};

  const SmallVector<SmallVector<const char *, 4>, 8> InvalidCases = {
      // Driver-tool-only option without FrontendOption.
      {"-Xfrontend", "-pretty-print"},
      // Verbose driver flag without FrontendOption.
      {"-Xfrontend", "-v"},
      // Nested -Xfrontend.
      {"-Xfrontend", "-Xfrontend", "-Xfrontend", "-warnings-as-errors"},
      // Missing argument value inside -Xfrontend.
      {"-Xfrontend", "-target-sdk-version"},
      // Positional input passed via -Xfrontend.
      {"-Xfrontend", "foo.swift"},
      // Unknown flag passed via -Xfrontend.
      {"-Xfrontend", "-unknown-frontend-option-xyz"},
  };

  for (const auto &Case : InvalidCases) {
    SourceManager SM;
    DiagnosticEngine Diags(SM);
    llvm::opt::InputArgList ParsedArgs(nullptr, nullptr);
    ASSERT_FALSE(CompilerInvocation::parseDriverToolArgs(
        *Table, Case, ParsedArgs, SwiftSymbolGraphExtractOption, Diags));

    CompilerInvocation Invocation;
    Invocation.setModuleName("swift_symbolgraph_extract");
    EXPECT_TRUE(Invocation.parseDriverToolFrontendArgs(
        ParsedArgs, Excluded, Diags, /*workingDirectory=*/{}, {}, FS))
        << "Expected rejection for " << Case.back();
    EXPECT_TRUE(Diags.hadAnyError())
        << "Expected diagnostic for " << Case.back();
  }
}

TEST(DriverToolArgs, ParsesFrontendAndCompileOptionsWithExcludedToolFlags) {
  std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
  auto FS = llvm::makeIntrusiveRefCnt<llvm::vfs::InMemoryFileSystem>();

  // Includes tool-only flags (-output-dir, -pretty-print, -v), excluded
  // frontend flags (-module-name Swift), compilation-environment flags, and
  // compile-only frontend flags (-warn-long-function-bodies, -O, -g).
  const char *RawArgs[] = {
      "-module-name",
      "Swift",
      "-output-dir",
      "/tmp/out",
      "-pretty-print",
      "-v",
      "-warnings-as-errors",
      "-strict-memory-safety",
      "-use-static-resource-dir",
      "-import-bridging-header",
      "/tmp/bridge.h",
      "-serialized-path-obfuscate",
      "/src=/dst",
      "-Xfrontend",
      "-target-sdk-version",
      "-Xfrontend",
      "15.0",
      "-Xfrontend",
      "-warn-long-function-bodies",
      "-Xfrontend",
      "123",
      "-O",
      "-g",
  };

  SourceManager SM;
  DiagnosticEngine Diags(SM);
  llvm::opt::InputArgList ParsedArgs(nullptr, nullptr);
  ASSERT_FALSE(CompilerInvocation::parseDriverToolArgs(
      *Table, RawArgs, ParsedArgs, SwiftSymbolGraphExtractOption, Diags));

  CompilerInvocation Invocation;
  Invocation.setModuleName("swift_symbolgraph_extract");
  const llvm::opt::OptSpecifier Excluded[] = {OPT_help, OPT_module_name};
  ASSERT_FALSE(Invocation.parseDriverToolFrontendArgs(
      ParsedArgs, Excluded, Diags, /*workingDirectory=*/{},
      "/usr/bin/swift-symbolgraph-extract", FS));
  EXPECT_FALSE(Diags.hadAnyError());

  // Pre-set synthetic module name is preserved even when -module-name Swift
  // was passed on the command line without -parse-stdlib.
  EXPECT_EQ(Invocation.getFrontendOptions().ModuleName,
            "swift_symbolgraph_extract");
  EXPECT_EQ(Invocation.getFrontendOptions().RequestedAction,
            FrontendOptions::ActionType::NoneAction);
  EXPECT_FALSE(
      Invocation.getDiagnosticOptions().WarningGroupControlRules.empty());
  EXPECT_TRUE(Invocation.getLangOptions().hasFeature(
      Feature::StrictMemorySafety));
  ASSERT_TRUE(Invocation.getLangOptions().SDKVersion.has_value());
  EXPECT_EQ(Invocation.getLangOptions().SDKVersion->getAsString(), "15.0");
  EXPECT_EQ(Invocation.getTypeCheckerOptions().WarnLongFunctionBodies, 123u);
  EXPECT_FALSE(Invocation.getFrontendOptions().UseSharedResourceFolder);
  EXPECT_TRUE(Invocation.getFrontendOptions().ModuleHasBridgingHeader);
  EXPECT_EQ(Invocation.getFrontendOptions().ImplicitObjCHeaderPath,
            "/tmp/bridge.h");
  EXPECT_EQ(Invocation.getClangImporterOptions().BridgingHeader,
            "/tmp/bridge.h");
  EXPECT_EQ(
      Invocation.getFrontendOptions().serializedPathObfuscator.obfuscate(
          "/src/foo.swift"),
      "/dst/foo.swift");
}

TEST(DriverToolArgs, InitializesEnvironmentWhenRemainingFrontendArgsEmpty) {
  std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
  auto FS = llvm::makeIntrusiveRefCnt<llvm::vfs::InMemoryFileSystem>();

  // Every argument is consumed by the tool, leaving zero frontend arguments.
  const char *RawArgs[] = {
      "-module-name", "Swift", "-o", "/tmp/Swift.swiftinterface", "-v",
  };

  SourceManager SM;
  DiagnosticEngine Diags(SM);
  llvm::opt::InputArgList ParsedArgs(nullptr, nullptr);
  ASSERT_FALSE(CompilerInvocation::parseDriverToolArgs(
      *Table, RawArgs, ParsedArgs, SwiftSynthesizeInterfaceOption, Diags));

  CompilerInvocation Invocation;
  Invocation.setModuleName("swift_synthesize_interface");
  const llvm::opt::OptSpecifier Excluded[] = {OPT_help, OPT_module_name, OPT_o};
  ASSERT_FALSE(Invocation.parseDriverToolFrontendArgs(
      ParsedArgs, Excluded, Diags, /*workingDirectory=*/{},
      "/toolchain/usr/bin/swift-synthesize-interface", FS));
  EXPECT_FALSE(Diags.hadAnyError());

  EXPECT_EQ(Invocation.getFrontendOptions().ModuleName,
            "swift_synthesize_interface");
  EXPECT_EQ(Invocation.getSearchPathOptions().RuntimeResourcePath,
            "/toolchain/usr/lib/swift");
  EXPECT_FALSE(
      Invocation.getSearchPathOptions().getRuntimeLibraryImportPaths().empty());
  EXPECT_FALSE(
      Invocation.getFrontendOptions().PrebuiltModuleCachePath.empty());
}

TEST(DriverToolArgs, RejectsDisallowedModesInputsAndOutputs) {
  std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
  auto FS = llvm::makeIntrusiveRefCnt<llvm::vfs::InMemoryFileSystem>();
  const llvm::opt::OptSpecifier Excluded[] = {OPT_help, OPT_module_name};

  const SmallVector<SmallVector<const char *, 4>, 8> DisallowedCases = {
      {"-emit-ir"},
      {"-Xfrontend", "-emit-ir"},
      {"-emit-ir", "foo.swift"},
      {"-typecheck"},
      {"-version"},
      {"-Xfrontend", "-version"},
      {"-emit-supported-arguments"},
      {"-emit-module"},
      {"-emit-module-path", "/tmp/Foo.swiftmodule"},
      {"-emit-dependencies-path", "/tmp/Foo.d"},
      {"foo.swift"},
      {"-primary-file", "foo.swift"},
      {"-filelist", "/tmp/files.txt"},
      {"-o", "/tmp/out"},
  };

  for (const auto &Case : DisallowedCases) {
    SourceManager SM;
    DiagnosticEngine Diags(SM);
    llvm::opt::InputArgList ParsedArgs(nullptr, nullptr);
    ASSERT_FALSE(CompilerInvocation::parseDriverToolArgs(
        *Table, Case, ParsedArgs, SwiftSymbolGraphExtractOption, Diags));

    CompilerInvocation Invocation;
    Invocation.setModuleName("swift_symbolgraph_extract");
    EXPECT_TRUE(Invocation.parseDriverToolFrontendArgs(
        ParsedArgs, Excluded, Diags, /*workingDirectory=*/{}, {}, FS))
        << "Expected rejection for " << Case.front();
    EXPECT_TRUE(Diags.hadAnyError())
        << "Expected diagnostic for " << Case.front();
  }
}
