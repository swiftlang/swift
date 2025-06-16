//===--- SwiftTargets.swift -----------------------------------------------===//
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

struct SwiftTargets {
  private var targets: [SwiftTarget] = []

  private var outputAliases: [String: [String]] = [:]
  private var dependenciesByTargetName: [String: Set<String>] = [:]
  private var targetsByName: [String: SwiftTarget] = [:]
  private var targetsByOutput: [String: SwiftTarget] = [:]

  // Track some state for debugging
  private var debugLogUnknownFlags: Set<String> = []

  init(for buildDir: RepoBuildDir) throws {
    log.debug("[*] Reading Swift targets from build.ninja")
    for rule in try buildDir.ninjaFile.buildEdges {
      try tryAddTarget(for: rule, buildDir: buildDir)
    }
    targets.sort(by: { $0.name < $1.name })

    log.debug("-------- SWIFT TARGET DEPS --------")
    for target in targets {
      var deps: Set<SwiftTarget> = []
      for dep in dependenciesByTargetName[target.name] ?? [] {
        for output in allOutputs(for: dep) {
          guard let depTarget = targetsByOutput[output] else { continue }
          deps.insert(depTarget)
        }
      }
      target.dependencies = deps.sorted(by: \.name)
      log.debug("| '\(target.name)' has deps: \(target.dependencies)")
    }
    log.debug("-----------------------------------")
    if !debugLogUnknownFlags.isEmpty {
      log.debug("---------- UNKNOWN FLAGS ----------")
      for flag in debugLogUnknownFlags.sorted() {
        log.debug("| \(flag)")
      }
      log.debug("-----------------------------------")
    }
  }

  private func allOutputs(for output: String) -> Set<String> {
    // TODO: Should we attempt to do canonicalization instead?
    var stack: [String] = [output]
    var results: Set<String> = []
    while let last = stack.popLast() {
      guard results.insert(last).inserted else { continue }
      for alias in outputAliases[last] ?? [] {
        stack.append(alias)
      }
    }
    return results
  }

  private mutating func computeBuildArgs(
    for edge: NinjaBuildFile.BuildEdge,
    in ninja: NinjaBuildFile
  ) throws -> BuildArgs? {
    let commandLine = try ninja.commandLine(for: edge)
    let command = try CommandParser.parseKnownCommandOnly(commandLine)
    guard let command, command.executable.knownCommand == .swiftc else {
      return nil
    }

    var buildArgs = BuildArgs(for: .swiftc, args: command.args)

    // Only include known flags for now.
    buildArgs = buildArgs.filter { arg in
      if arg.flag != nil {
        return true
      }
      if log.logLevel <= .debug {
        // Note the unknown flags.
        guard let value = arg.value, value.hasPrefix("-") else { return false }
        debugLogUnknownFlags.insert(value)
      }
      return false
    }

    return buildArgs
  }

  /// Check to see if this is a forced-XXX-dep.swift file, which is only used
  /// to hack around CMake dependencies, and can be dropped.
  private func isForcedDepSwiftFile(_ path: AbsolutePath) -> Bool {
    path.fileName.scanningUTF8 { scanner in
      guard scanner.tryEat(utf8: "forced-") else {
        return false
      }
      while scanner.tryEat() {
        if scanner.tryEat(utf8: "-dep.swift"), !scanner.hasInput {
          return true
        }
      }
      return false
    }
  }

  func getSources(
    from edge: NinjaBuildFile.BuildEdge, buildDir: RepoBuildDir
  ) throws -> SwiftTarget.Sources {
    let files: [AnyPath] = edge.inputs.map(AnyPath.init)

    // Split the files into repo sources and external sources. Repo sources
    // are those under the repo path, external sources are outside that path,
    // and are either for dependencies such as swift-syntax, or are generated
    // from e.g de-gyb'ing.
    var sources = SwiftTarget.Sources()

    for input in files where input.language == .swift {
      switch input {
      case .relative(let r):
        // A relative path is for a file in the build directory, it's external.
        let abs = buildDir.path.appending(r)
        guard abs.exists else { continue }
        sources.externalSources.append(abs.realPath)

      case .absolute(let a):
        guard a.exists else { continue }
        // Symlinks shouldn't really be a concern here, but we need to realpath
        // in order to canonicalize the casing.
        let a = a.realPath
        guard let rel = a.removingPrefix(buildDir.repoPath) else {
          sources.externalSources.append(a)
          continue
        }
        sources.repoSources.append(rel)
      }
    }
    // Avoid adding forced dependency files.
    sources.externalSources = sources.externalSources
      .filter { !isForcedDepSwiftFile($0) }
    return sources
  }

  private mutating func tryAddTarget(
    for edge: NinjaBuildFile.BuildEdge,
    buildDir: RepoBuildDir
  ) throws {
    // Phonies are only used to track aliases.
    if edge.isPhony {
      for output in edge.outputs {
        outputAliases[output, default: []] += edge.inputs
      }
      return
    }

    // Ignore build rules that don't have object file or swiftmodule outputs.
    let forBuild = edge.outputs.contains(
      where: { $0.hasExtension(.o) }
    )
    let forModule = edge.outputs.contains(
      where: { $0.hasExtension(.swiftmodule) }
    )
    guard forBuild || forModule else {
      return
    }
    let primaryOutput = edge.outputs.first!
    let sources = try getSources(from: edge, buildDir: buildDir)
    let repoSources = sources.repoSources
    let externalSources = sources.externalSources

    // Is this for a build (producing a '.o'), we need to have at least one
    // repo source. Module dependencies can use external sources.
    guard !repoSources.isEmpty || (forModule && !externalSources.isEmpty) else {
      return
    }

    guard let buildArgs = try computeBuildArgs(for: edge, in: buildDir.ninjaFile) else { return }

    // Pick up the module name from the arguments, or use an explicitly
    // specified module name if we have one. The latter might be invalid so
    // may not be part of the build args (e.g 'swift-plugin-server'), but is
    // fine for generation.
    let moduleName = buildArgs.lastValue(for: .moduleName) ?? edge.bindings[.swiftModuleName]
    guard let moduleName else {
      log.debug("! Skipping Swift target with output \(primaryOutput); no module name")
      return
    }
    let moduleLinkName = buildArgs.lastValue(for: .moduleLinkName) ?? edge.bindings[.swiftLibraryName]
    let name = moduleLinkName ?? moduleName

    // Add the dependencies. We track dependencies for any input files, along
    // with any recorded swiftmodule dependencies.
    dependenciesByTargetName.withValue(for: name, default: []) { deps in
      deps.formUnion(
        edge.inputs.filter {
          $0.hasExtension(.swiftmodule) || $0.hasExtension(.o)
        }
      )
      deps.formUnion(
        edge.dependencies.filter { $0.hasExtension(.swiftmodule) }
      )
    }

    var buildRule: SwiftTarget.BuildRule?
    var emitModuleRule: SwiftTarget.EmitModuleRule?
    if forBuild && !repoSources.isEmpty {
      // We've already ensured that `repoSources` is non-empty.
      buildRule = .init(
        parentPath: repoSources.commonAncestor!, sources: sources,
        buildArgs: buildArgs
      )
    }
    if forModule {
      emitModuleRule = .init(sources: sources, buildArgs: buildArgs)
    }
    let target = targetsByName[name] ?? {
      log.debug("+ Discovered Swift target '\(name)' with output '\(primaryOutput)'")
      let target = SwiftTarget(name: name, moduleName: moduleName)
      targetsByName[name] = target
      targets.append(target)
      return target
    }()
    for output in edge.outputs {
      targetsByOutput[output] = target
    }
    if buildRule == nil || target.buildRule == nil {
      if let buildRule {
        target.buildRule = buildRule
      }
    } else {
      log.debug("""
        ! Skipping '\(name)' build rule for \
        '\(primaryOutput)'; already added
        """)
    }
    if emitModuleRule == nil || target.emitModuleRule == nil {
      if let emitModuleRule {
        target.emitModuleRule = emitModuleRule
      }
    } else {
      log.debug("""
        ! Skipping '\(name)' emit module rule for \
        '\(primaryOutput)'; already added
        """)
    }
  }

  func getTargets(below path: RelativePath) -> [SwiftTarget] {
    targets.filter { target in
      guard let parent = target.buildRule?.parentPath, parent.starts(with: path) 
      else {
        return false
      }
      return true
    }
  }
}
