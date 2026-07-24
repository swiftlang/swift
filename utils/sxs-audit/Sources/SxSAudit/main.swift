// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

internal import ArgumentParser

@main
internal struct SxSAudit: ParsableCommand {
  internal static var configuration: CommandConfiguration {
    CommandConfiguration(commandName: "sxs-audit",
                         abstract: "Audit the private runtime closure " +
                           "required by Windows PE packages.")
  }

  @Option(help: ArgumentHelp("Directory containing the PE files and private " +
                             "assembly directories."))
  internal var root: String

  @Option(name: .customLong("runtime-root"),
          help: "Directory containing the complete flat private runtime.")
  internal var runtime: String

  @Option(name: .customLong("exclude-runtime-dll"), parsing: .singleValue,
          help: ArgumentHelp("Add a DLL name or trailing-* prefix to the " +
                             "default exclusions."))
  internal var exclusions = Array<String>()

  @Option(name: .customLong("file"), parsing: .singleValue,
          help: "Assign a provided PE file with PACKAGE=FILE.")
  internal var files = Array<String>()

  @Option(name: .customLong("feature"), parsing: .singleValue,
          help: "Assign an authored runtime feature with PACKAGE=FEATURE.")
  internal var features = Array<String>()

  @Option(name: .customLong("authored"), parsing: .singleValue,
          help: "Add an authored runtime member with PACKAGE=ASSEMBLY.")
  internal var authored = Array<String>()

  internal mutating func run() throws {
    let inputs = try inputs(root: root, runtime: runtime,
                            exclusions: exclusions)
    let packages = try packages(files: files, features: features,
                                authored: authored)
    let result = try plan(root: inputs.root, runtime: inputs.runtime,
                          packages: packages,
                          exclusions: inputs.exclusions)
    print(report(result), terminator: "")
  }
}
