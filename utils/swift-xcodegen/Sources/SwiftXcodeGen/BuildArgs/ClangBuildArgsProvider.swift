//===--- ClangBuildArgsProvider.swift -------------------------------------===//
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

import Foundation

struct ClangBuildArgsProvider {
  private var args = CommandArgTree()
  private var outputs: [RelativePath: AbsolutePath] = [:]

  init(for buildDir: RepoBuildDir) throws {
    let buildDirPath = buildDir.path
    let repoPath = buildDir.repoPath

    // TODO: Should we get Clang build args from the build.ninja? We're already
    // parsing that to get the Swift targets, seems unfortunate to have 2
    // sources of truth.
    let fileName = buildDirPath.appending("compile_commands.json")
    guard fileName.exists else {
      throw XcodeGenError.pathNotFound(fileName)
    }
    log.debug("[*] Reading Clang build args from '\(fileName)'")
    let parsed = try JSONDecoder().decode(
      CompileCommands.self, from: try fileName.read()
    )
    // Gather the candidates for each file to get build arguments for. We may
    // have multiple outputs, in which case, pick the first one that exists.
    var commandsToAdd: [RelativePath:
                          (output: AbsolutePath?, args: [Command.Argument])] = [:]
    for command in parsed {
      guard command.command.executable.knownCommand == .clang,
            let relFilePath = command.file.removingPrefix(repoPath)
      else {
        continue
      }
      let output = command.output.map { command.directory.appending($0) }
      if let existing = commandsToAdd[relFilePath],
         let existingOutput = existing.output,
          output == nil || existingOutput.exists || !output!.exists {
        continue
      }
      commandsToAdd[relFilePath] = (output, command.command.args)
    }
    for (path, (output, commandArgs)) in commandsToAdd {
      // Only include arguments that have known flags.
      args.insert(commandArgs.filter({ $0.flag != nil }), for: path)
      outputs[path] = output
    }
  }

  /// Retrieve the arguments at a given path, including those in the parent.
  func getArgs(for path: RelativePath) -> BuildArgs {
    // Sort the arguments to get a deterministic ordering.
    // FIXME: We ought to get the command from the arg tree.
    .init(for: .clang, args: args.getArgs(for: path).sorted())
  }

  /// Retrieve the arguments at a given path, excluding those already covered
  /// by a parent.
  func getUniqueArgs(
    for path: RelativePath, parent: RelativePath, infer: Bool = false
  ) -> BuildArgs {
    var fileArgs: Set<Command.Argument> = []
    if hasBuildArgs(for: path) {
      fileArgs = args.getUniqueArgs(for: path, parent: parent)
    } else if infer {
      // If we can infer arguments, walk up to the nearest parent with args.
      if let component = path.stackedComponents
        .reversed().dropFirst().first(where: hasBuildArgs) {
        fileArgs = args.getUniqueArgs(for: component, parent: parent)
      }
    }
    // Sort the arguments to get a deterministic ordering.
    // FIXME: We ought to get the command from the arg tree.
    return .init(for: .clang, args: fileArgs.sorted())
  }

  /// Whether the given path has any unique args not covered by `parent`.
  func hasUniqueArgs(for path: RelativePath, parent: RelativePath) -> Bool {
    args.hasUniqueArgs(for: path, parent: parent)
  }

  /// Whether the given file has build arguments.
  func hasBuildArgs(for path: RelativePath) -> Bool {
    !args.getArgs(for: path).isEmpty
  }

  func isObjectFilePresent(for path: RelativePath) -> Bool {
    outputs[path]?.exists == true
  }
}
