//===--- Options.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ArgumentParser
import SwiftXcodeGen

enum LogLevelOption: String, CaseIterable {
  case debug, info, note, warning, error
}
extension LogLevelOption: ExpressibleByArgument {
  init?(argument: String) {
    self.init(rawValue: argument)
  }
}
extension Logger.LogLevel {
  init(_ option: LogLevelOption) {
    self =
      switch option {
      case .debug: .debug
      case .info: .info
      case .note: .note
      case .warning: .warning
      case .error: .error
      }
  }
}

extension ArgumentHelp {
  static func hidden(_ abstract: String) -> Self {
    .init(abstract, visibility: .hidden)
  }
}

struct LLVMProjectOptions: ParsableArguments {
  @Flag(
    name: .customLong("clang"),
    inversion: .prefixedNo,
    help: "Generate an xcodeproj for Clang"
  )
  var addClang: Bool = false

  @Flag(
    name: .customLong("clang-tools-extra"),
    inversion: .prefixedNo,
    help: """
      When generating a project for Clang, whether to include clang-tools-extra
      """
  )
  var addClangToolsExtra: Bool = true

  // FIXME: Semantic functionality is currently not supported, unhide when
  // fixed.
  @Flag(
    name: .customLong("compiler-rt"),
    inversion: .prefixedNo,
    help: .hidden(
      """
      When generating a project for LLVM, whether to include compiler-rt.
      """
    )
  )
  var addCompilerRT: Bool = false

  @Flag(
    name: .customLong("lldb"),
    inversion: .prefixedNo,
    help: "Generate an xcodeproj for LLDB"
  )
  var addLLDB: Bool = false

  @Flag(
    name: .customLong("llvm"),
    inversion: .prefixedNo,
    help: "Generate an xcodeproj for LLVM"
  )
  var addLLVM: Bool = false
}

struct SwiftTargetOptions: ParsableArguments {
  @Flag(
    name: .customLong("swift-targets"),
    inversion: .prefixedNo,
    help: """
      Generate targets for Swift files, e.g ASTGen, SwiftCompilerSources. Note
      this by default excludes the standard library, see '--stdlib-swift'.
      """
  )
  var addSwiftTargets: Bool = true

  @Flag(
    name: .customLong("swift-dependencies"),
    inversion: .prefixedNo,
    help: """
      When generating Swift targets, add dependencies (e.g swift-syntax) to the
      generated project. This makes build times slower, but improves syntax
      highlighting for targets that depend on them.
      """
  )
  var addSwiftDependencies: Bool = true
}

struct RunnableTargetOptions: ParsableArguments {
  @Option(
    name: .customLong("runnable-build-dir"),
    help: """
      If specified, runnable targets will use this build directory. Useful for
      configurations where a separate debug build directory is used.
      """
  )
  var runnableBuildDir: AnyPath?

  @Flag(
    name: .customLong("runnable-targets"),
    inversion: .prefixedNo,
    help: """
      Whether to add runnable targets for e.g swift-frontend. This is useful
      for debugging in Xcode.
      """
  )
  var addRunnableTargets: Bool = true

  @Flag(
    name: .customLong("build-runnable-targets"),
    inversion: .prefixedNo,
    help: """
      If runnable targets are enabled, whether to add a build action for them.
      If false, they will be added as freestanding schemes.
      """
  )
  var addBuildForRunnableTargets: Bool = true
}

struct ProjectOptions: ParsableArguments {
  // Hidden as mostly only useful for testing purposes.
  @Flag(
    name: .customLong("clang-targets"),
    inversion: .prefixedNo,
    help: .hidden
  )
  var addClangTargets: Bool = true

  @Flag(
    name: .customLong("compiler-libs"),
    inversion: .prefixedNo,
    help: "Generate targets for compiler libraries"
  )
  var addCompilerLibs: Bool = true

  @Flag(
    name: .customLong("compiler-tools"),
    inversion: .prefixedNo,
    help: "Generate targets for compiler tools"
  )
  var addCompilerTools: Bool = true

  @Flag(
    name: .customLong("docs"),
    inversion: .prefixedNo,
    help: "Add doc groups to the generated projects"
  )
  var addDocs: Bool = true

  @Flag(
    name: [.customLong("stdlib"), .customLong("stdlib-cxx")],
    inversion: .prefixedNo,
    help: "Generate a target for C/C++ files in the standard library"
  )
  var addStdlibCxx: Bool = true

  @Flag(
    name: .customLong("stdlib-swift"),
    inversion: .prefixedNo,
    help: """
      Generate targets for Swift files in the standard library. This requires
      using Xcode with a main development Swift snapshot, and as such is
      disabled by default. 

      A development snapshot is necessary to avoid spurious build/live issues
      due to the fact that the stdlib is built using the just-built Swift
      compiler, which may support features not yet supported by the Swift
      compiler in Xcode's toolchain.
      """
  )
  var addStdlibSwift: Bool = false

  @Flag(
    name: .customLong("test-folders"),
    inversion: .prefixedNo,
    help: "Add folder references for test files"
  )
  var addTestFolders: Bool = true

  @Flag(
    name: .customLong("unittests"),
    inversion: .prefixedNo,
    help: "Generate a target for the unittests"
  )
  var addUnitTests: Bool = true

  @Flag(
    name: .customLong("infer-args"),
    inversion: .prefixedNo,
    help: """
      Whether to infer build arguments for files that don't have any, based
      on the build arguments of surrounding files. This is mainly useful for
      files that aren't built in the default config, but are still useful to
      edit (e.g sourcekitdAPI-InProc.cpp).
      """
  )
  var inferArgs: Bool = true

  @Flag(
    name: .customLong("prefer-folder-refs"),
    inversion: .prefixedNo,
    help: """
      Whether to prefer folder references for groups containing non-source
      files
      """
  )
  var preferFolderRefs: Bool = true

  @Flag(
    name: .customLong("buildable-folders"),
    inversion: .prefixedNo,
    help: """
      Requires Xcode 16: Enables the use of "buildable folders", allowing
      folder references to be used for compatible targets. This allows new
      source files to be added to a target without needing to regenerate the
      project.
      """
  )
  var useBuildableFolders: Bool = true

  @Option(
    name: .customLong("runtimes-build-dir"),
    help: """
      Experimental: The path to a build directory for the new 'Runtimes/'
      stdlib CMake build. This creates a separate 'SwiftRuntimes' project, along
      with a 'Swift+Runtimes' workspace.

      Note: This requires passing '-DCMAKE_EXPORT_COMPILE_COMMANDS=YES' to
      CMake.
      """
  )
  var runtimesBuildDir: AnyPath?

  @Option(help: .hidden)
  var blueFolders: String = ""
}

struct MiscOptions: ParsableArguments {
  @Option(
    help: """
      The project root directory, which is the parent directory of the Swift repo.
      By default this is inferred from the build directory path.
      """
  )
  var projectRootDir: AnyPath?

  @Option(
    help: """
      The output directory to write the Xcode project to. Defaults to the project
      root directory.
      """
  )
  var outputDir: AnyPath?

  @Option(help: "The log level verbosity (default: info)")
  var logLevel: LogLevelOption?

  @Flag(
    name: .long,
    inversion: .prefixedNo,
    help: "Parallelize generation of projects"
  )
  var parallel: Bool = true

  @Flag(
    name: .shortAndLong,
    help: "Quiet output; equivalent to --log-level warning"
  )
  var quiet: Bool = false
}
