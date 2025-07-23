//===--- SwiftXcodegen.swift ----------------------------------------------===//
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
import Foundation
import SwiftXcodeGen

@main
@dynamicMemberLookup
struct SwiftXcodegen: AsyncParsableCommand, Sendable {
  // MARK: Options

  @OptionGroup(title: "LLVM Projects")
  var llvmProjectOpts: LLVMProjectOptions

  subscript<T>(dynamicMember kp: KeyPath<LLVMProjectOptions, T>) -> T {
    llvmProjectOpts[keyPath: kp]
  }

  @OptionGroup(title: "Swift targets")
  var swiftTargetOpts: SwiftTargetOptions

  subscript<T>(dynamicMember kp: KeyPath<SwiftTargetOptions, T>) -> T {
    swiftTargetOpts[keyPath: kp]
  }

  @OptionGroup(title: "Runnable targets")
  var runnableTargetOptions: RunnableTargetOptions

  subscript<T>(dynamicMember kp: KeyPath<RunnableTargetOptions, T>) -> T {
    runnableTargetOptions[keyPath: kp]
  }

  @OptionGroup(title: "Project configuration")
  var projectOpts: ProjectOptions

  subscript<T>(dynamicMember kp: KeyPath<ProjectOptions, T>) -> T {
    projectOpts[keyPath: kp]
  }

  @OptionGroup(title: "Misc")
  var miscOptions: MiscOptions

  subscript<T>(dynamicMember kp: KeyPath<MiscOptions, T>) -> T {
    miscOptions[keyPath: kp]
  }

  @Argument(help: "The path to the Ninja build directory to generate for")
  var buildDir: AnyPath

  // MARK: Command

  private func newProjectSpec(
    _ name: String,
    for buildDir: RepoBuildDir,
    runnableBuildDir: RepoBuildDir? = nil,
    mainRepoDir: RelativePath? = nil
  ) -> ProjectSpec {
    ProjectSpec(
      name,
      for: buildDir,
      runnableBuildDir: runnableBuildDir ?? buildDir,
      addClangTargets: self.addClangTargets,
      addSwiftTargets: self.addSwiftTargets,
      addSwiftDependencies: self.addSwiftDependencies,
      addRunnableTargets: false,
      addBuildForRunnableTargets: self.addBuildForRunnableTargets,
      inferArgs: self.inferArgs,
      preferFolderRefs: self.preferFolderRefs,
      useBuildableFolders: self.useBuildableFolders,
      mainRepoDir: mainRepoDir
    )
  }

  @discardableResult
  func writeSwiftXcodeProject(
    for ninja: NinjaBuildDir,
    into outputDir: AbsolutePath
  ) throws -> GeneratedProject {
    let buildDir = try ninja.buildDir(for: .swift)

    // Check to see if we have a separate runnable build dir.
    let runnableBuildDirPath =
      self.runnableBuildDir?.absoluteInWorkingDir.realPath
    let runnableBuildDir = try runnableBuildDirPath.map {
      try NinjaBuildDir(at: $0, projectRootDir: ninja.projectRootDir)
        .buildDir(for: .swift)
    }

    var spec = newProjectSpec(
      "Swift",
      for: buildDir,
      runnableBuildDir: runnableBuildDir
    )
    if self.addDocs {
      spec.addTopLevelDocs()
      spec.addDocsGroup(at: "docs")
      spec.addDocsGroup(at: "userdocs")
    }

    spec.addHeaders(in: "include")

    if self.addCompilerLibs {
      spec.addClangTargets(below: "lib", addingPrefix: "swift")

      spec.addClangTarget(at: "SwiftCompilerSources")
      spec.addSwiftTargets(below: "lib")
      spec.addSwiftTargets(below: "SwiftCompilerSources")
    }

    if self.addCompilerTools {
      spec.addClangTargets(below: "tools")
      spec.addSwiftTargets(below: "tools")
    }

    if self.addStdlibCxx || self.addStdlibSwift {
      // These are headers copied from LLVM, avoid including them in the project
      // to avoid confusion.
      spec.addExcludedPath("stdlib/include/llvm")
    }
    if self.addStdlibCxx {
      // This doesn't build with Clang 15, it does build with ToT Clang though.
      spec.addUnbuildableFile(
        "stdlib/tools/swift-reflection-test/swift-reflection-test.c"
      )

      // Add a single target for all the C/C++ files in the stdlib. We may have
      // unbuildable files, which will be added to the Unbuildables target.
      spec.addClangTarget(at: "stdlib", mayHaveUnbuildableFiles: true)
    }
    if self.addStdlibSwift {
      // Add any Swift targets in the stdlib.
      spec.addSwiftTargets(below: "stdlib")
    }

    if self.addUnitTests {
      // Create a single 'unittests' target.
      spec.addClangTarget(at: "unittests")
    }
    if self.addTestFolders {
      spec.addReference(to: "test")
      spec.addReference(to: "validation-test")
    }

    for blueFolder in self.blueFolders.components(separatedBy: ",")
    where !blueFolder.isEmpty {
      spec.addReference(to: RelativePath(blueFolder))
    }

    // Only enable runnable targets for Swift for now.
    if self.addRunnableTargets {
      spec.addRunnableTargets = true

      // If we don't have debug info, warn.
      // https://github.com/swiftlang/swift-format/issues/1037
      // swift-format-ignore
      if let config = try spec.runnableBuildDir.buildConfiguration,
         !config.hasDebugInfo
      {
        log.warning(
          """
          Specified build directory '\(spec.runnableBuildDir.path)' does not \
          have debug info; runnable targets will not be debuggable with LLDB. \
          Either build with debug info enabled, or specify a separate debug \
          build directory with '--runnable-build-dir'. Runnable targets may be \
          disabled by passing '--no-runnable-targets'.
          """
        )
      }
    }
    return try spec.generateAndWrite(into: outputDir)
  }

  func writeSwiftRuntimesXcodeProject(
    for ninja: NinjaBuildDir,
    into outputDir: AbsolutePath
  ) throws -> GeneratedProject {
    let buildDir = try ninja.buildDir(for: .swiftRuntimes)
    var spec = newProjectSpec("SwiftRuntimes", for: buildDir)

    spec.addClangTarget(at: "core", mayHaveUnbuildableFiles: true)
    spec.addSwiftTargets(below: "core")

    if self.addDocs {
      spec.addTopLevelDocs()
    }
    return try spec.generateAndWrite(into: outputDir)
  }

  @discardableResult
  func writeClangXcodeProject(
    for ninja: NinjaBuildDir,
    into outputDir: AbsolutePath
  ) throws -> GeneratedProject {
    var spec = newProjectSpec(
      "Clang",
      for: try ninja.buildDir(for: .llvm),
      mainRepoDir: "clang"
    )
    if self.addDocs {
      spec.addTopLevelDocs()
      spec.addDocsGroup(at: "docs")
    }
    spec.addHeaders(in: "include")

    if self.addCompilerLibs {
      spec.addClangTargets(below: "lib", addingPrefix: "clang")
    }
    if self.addCompilerTools {
      spec.addClangTargets(below: "tools")

      if self.addClangToolsExtra {
        spec.addClangTargets(
          below: "../clang-tools-extra",
          addingPrefix: "extra-",
          mayHaveUnbuildableFiles: true,
          excluding: ["test"]
        )
        if self.addTestFolders {
          spec.addReference(to: "../clang-tools-extra/test")
        } else {
          // Avoid adding any headers present in the test folder.
          spec.addExcludedPath("../clang-tools-extra/test")
        }
      }
    }
    if self.addUnitTests {
      spec.addClangTarget(at: "unittests")
    }
    if self.addTestFolders {
      spec.addReference(to: "test")
    }
    return try spec.generateAndWrite(into: outputDir)
  }

  @discardableResult
  func writeLLDBXcodeProject(
    for ninja: NinjaBuildDir,
    into outputDir: AbsolutePath
  ) throws -> GeneratedProject {
    var spec = newProjectSpec("LLDB", for: try ninja.buildDir(for: .lldb))
    if self.addDocs {
      spec.addTopLevelDocs()
      spec.addDocsGroup(at: "docs")
    }
    spec.addHeaders(in: "include")

    if self.addCompilerLibs {
      spec.addClangTargets(below: "source", addingPrefix: "lldb")
    }
    if self.addCompilerTools {
      spec.addClangTargets(below: "tools")
    }
    if self.addUnitTests {
      spec.addClangTarget(at: "unittests")
    }
    if self.addTestFolders {
      spec.addReference(to: "test")
    }
    return try spec.generateAndWrite(into: outputDir)
  }

  @discardableResult
  func writeLLVMXcodeProject(
    for ninja: NinjaBuildDir,
    into outputDir: AbsolutePath
  ) throws -> GeneratedProject {
    var spec = newProjectSpec(
      "LLVM",
      for: try ninja.buildDir(for: .llvm),
      mainRepoDir: "llvm"
    )
    if self.addDocs {
      spec.addTopLevelDocs()
      spec.addDocsGroup(at: "docs")
    }
    spec.addHeaders(in: "include")

    if self.addCompilerLibs {
      spec.addClangTargets(below: "lib", addingPrefix: "llvm")
    }
    if self.addCompilerTools {
      spec.addClangTargets(below: "tools")
    }
    if self.addTestFolders {
      spec.addReference(to: "test")
    }
    // FIXME: Looks like compiler-rt has its own build directory
    // llvm-macosx-arm64/tools/clang/runtime/compiler-rt-bins/build.ninja
    if self.addCompilerRT {
      spec.addClangTargets(
        below: "../compiler-rt",
        addingPrefix: "extra-"
      )
      if self.addTestFolders {
        spec.addReference(to: "../compiler-rt/test")
      } else {
        // Avoid adding any headers present in the test folder.
        spec.addExcludedPath("../compiler-rt/test")
      }
    }
    return try spec.generateAndWrite(into: outputDir)
  }

  func getWorkspace(for proj: GeneratedProject) throws -> WorkspaceGenerator {
    var generator = WorkspaceGenerator()
    generator.addProject(proj)
    return generator
  }

  func runTask<R>(
    _ body: @escaping @Sendable () throws -> R
  ) async throws -> Task<R, Error> {
    let task = Task(operation: body)
    if !self.parallel {
      _ = try await task.value
    }
    return task
  }

  func showCaveatsIfNeeded() {
    guard log.logLevel <= .note else { return }

    var notes: [String] = []
    if projectOpts.useBuildableFolders {
      notes.append(
        """
        - Buildable folders are enabled by default, which requires Xcode 16. You
          can pass '--no-buildable-folders' to disable this. See the '--help'
          entry for more info.
        """
      )
    }

    if !projectOpts.addStdlibSwift {
      notes.append(
        """
        - Swift standard library targets are disabled by default since they require
          using a development snapshot of Swift with Xcode. You can pass '--stdlib-swift'
          to enable. See the '--help' entry for more info.
        """
      )
    }
    guard !notes.isEmpty else { return }
    log.note("Caveats:")
    for note in notes {
      for line in note.components(separatedBy: .newlines) {
        log.note(line)
      }
    }
  }

  func generate() async throws {
    let buildDirPath = buildDir.absoluteInWorkingDir.realPath
    log.info("Generating project for '\(buildDirPath)'...")

    let projectRootDir = self.projectRootDir?.absoluteInWorkingDir
    let buildDir = try NinjaBuildDir(at: buildDirPath, projectRootDir: projectRootDir)
    let outputDir = miscOptions.outputDir?.absoluteInWorkingDir ?? buildDir.projectRootDir

    let swiftProj = try await runTask {
      try writeSwiftXcodeProject(for: buildDir, into: outputDir)
    }
    let runtimesProj = try await runTask { () -> GeneratedProject? in
      guard let runtimesBuildDir = self.runtimesBuildDir?.absoluteInWorkingDir else {
        return nil
      }
      let buildDir = try NinjaBuildDir(
        at: runtimesBuildDir,
        projectRootDir: projectRootDir
      )
      return try writeSwiftRuntimesXcodeProject(for: buildDir, into: outputDir)
    }
    let llvmProj = try await runTask {
      self.addLLVM ? try writeLLVMXcodeProject(for: buildDir, into: outputDir) : nil
    }
    let clangProj = try await runTask {
      self.addClang ? try writeClangXcodeProject(for: buildDir, into: outputDir) : nil
    }
    let lldbProj = try await runTask {
      self.addLLDB ? try writeLLDBXcodeProject(for: buildDir, into: outputDir) : nil
    }

    var swiftWorkspace = try await getWorkspace(for: swiftProj.value)

    if let runtimesProj = try await runtimesProj.value {
      swiftWorkspace.addProject(runtimesProj)
      try swiftWorkspace.write("Swift+Runtimes", into: outputDir)
    }

    if let llvmProj = try await llvmProj.value {
      var swiftLLVMWorkspace = swiftWorkspace
      swiftLLVMWorkspace.addProject(llvmProj)
      try swiftLLVMWorkspace.write("Swift+LLVM", into: outputDir)
    }

    // https://github.com/swiftlang/swift-format/issues/1037
    // swift-format-ignore
    if let clangProj = try await clangProj.value,
       let llvmProj = try await llvmProj.value
    {
      var clangLLVMWorkspace = WorkspaceGenerator()
      clangLLVMWorkspace.addProject(clangProj)
      clangLLVMWorkspace.addProject(llvmProj)
      try clangLLVMWorkspace.write("Clang+LLVM", into: outputDir)

      var allWorkspace = swiftWorkspace
      allWorkspace.addProject(clangProj)
      allWorkspace.addProject(llvmProj)
      try allWorkspace.write("Swift+Clang+LLVM", into: outputDir)
    }

    if let lldbProj = try await lldbProj.value {
      var swiftLLDBWorkspace = swiftWorkspace
      swiftLLDBWorkspace.addProject(lldbProj)
      try swiftLLDBWorkspace.write("Swift+LLDB", into: outputDir)

      if let llvmProj = try await llvmProj.value {
        var lldbLLVMWorkspace = WorkspaceGenerator()
        lldbLLVMWorkspace.addProject(lldbProj)
        lldbLLVMWorkspace.addProject(llvmProj)
        try lldbLLVMWorkspace.write("LLDB+LLVM", into: outputDir)
      }
    }
  }

  func printingTimeTaken<T>(_ fn: () async throws -> T) async rethrows -> T {
    let start = Date()
    let result = try await fn()

    // Note we don't print the time taken when we fail.
    let delta = Date().timeIntervalSince(start)
    log.info("Successfully generated in \(Int((delta * 1000).rounded()))ms")

    return result
  }

  func run() async {
    // Set the log level
    log.logLevel = .init(self.logLevel ?? (self.quiet ? .warning : .info))
    do {
      try await printingTimeTaken {
        try await generate()
      }
      showCaveatsIfNeeded()
    } catch {
      log.error("\(error)")
    }
    if log.hadError {
      Darwin.exit(1)
    }
  }
}
