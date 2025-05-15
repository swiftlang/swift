//===--- ProjectGenerator.swift -------------------------------------------===//
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

import Darwin

extension Xcode.Reference {
  fileprivate var displayName: String {
    name ?? RelativePath(path).fileName
  }
}

fileprivate final class ProjectGenerator {
  let spec: ProjectSpec

  private var project = Xcode.Project()
  private let allTarget: Xcode.Target

  enum CachedGroup {
    /// Covered by a parent folder reference.
    case covered
    /// Present in the project.
    case present(Xcode.Group)
  }
  private var groups: [RelativePath: CachedGroup] = [:]
  private var files: [RelativePath: Xcode.FileReference] = [:]
  private var targets: [String: Xcode.Target] = [:]
  private var unbuildableSources: [RelativePath] = []
  private var runnableBuildTargets: [RunnableTarget: Xcode.Target] = [:]
  private var buildableFolders: [RelativePath: Xcode.BuildableFolder] = [:]

  /// The group in which external files are stored.
  private var externalsGroup: Xcode.Group {
    if let _externalsGroup {
      return _externalsGroup
    }
    let group = project.mainGroup.addGroup(
      path: "", pathBase: .groupDir, name: "external"
    )
    _externalsGroup = group
    return group
  }
  private var _externalsGroup: Xcode.Group?

  private lazy var includeSubstitutionTarget = {
    project.addTarget(name: "swift-include-substitutions")
  }()
  private var includeSubstitutions: Set<BuildArgs.PathSubstitution> = []

  private lazy var unbuildablesTarget: Xcode.Target = {
    generateBaseTarget(
      "Unbuildables", at: ".", canUseBuildableFolder: false,
      productType: .staticArchive, includeInAllTarget: false
    )!
  }()

  /// The main repo dir relative to the project.
  private lazy var mainRepoDirInProject: RelativePath? =
    spec.mainRepoDir.map { repoRelativePath.appending($0) }

  private var generated: Bool = false

  var name: String {
    spec.name
  }
  var buildDir: RepoBuildDir {
    spec.buildDir
  }
  var addSwiftDependencies: Bool {
    spec.addSwiftDependencies
  }

  var repoPath: AbsolutePath {
    buildDir.repoPath
  }

  var repoRelativePath: RelativePath {
    buildDir.repoRelativePath
  }

  var projectRootDir: AbsolutePath {
    buildDir.projectRootDir
  }

  var pathName: RelativePath {
    "\(name).xcodeproj"
  }

  var runnableTargets: RunnableTargets {
    get throws {
      try spec.runnableBuildDir.runnableTargets
    }
  }

  init(for spec: ProjectSpec) {
    self.spec = spec

    // Create an 'ALL' meta-target that depends on everything.
    self.allTarget = project.addTarget(name: "ALL")

    // Setup the project root.
    self.project.mainGroup.path = projectRootDir.rawPath
    self.project.mainGroup.pathBase = .absolute
    self.project.buildSettings.common.PROJECT_DIR = projectRootDir.rawPath
  }

  /// Computes both the parent group along with the relative child path
  /// for a file path relative to the project root.
  private func parentGroup(
    for path: RelativePath
  ) -> (parentGroup: Xcode.Group, childPath: RelativePath)? {
    guard let parent = path.parentDir else {
      // We've already handled paths under the repo, so this must be for
      // paths outside the repo.
      return (externalsGroup, path)
    }
    // We avoid adding a parent for paths above the repo, e.g we want a
    // top-level 'lib', not 'swift/lib'.
    if parent == repoRelativePath || parent == mainRepoDirInProject {
      return (project.mainGroup, path)
    }
    guard let parentGroup = group(for: parent) else { return nil }
    return (parentGroup, RelativePath(path.fileName))
  }

  /// Returns the group for a given path, or `nil` if the path is covered
  /// by a parent folder reference.
  private func group(for path: RelativePath) -> Xcode.Group? {
    if let result = groups[path] {
      switch result {
      case .covered:
        return nil
      case .present(let g):
        return g
      }
    }
    guard
      files[path] == nil, let (parentGroup, childPath) = parentGroup(for: path)
    else {
      groups[path] = .covered
      return nil
    }
    let group = parentGroup.addGroup(
      path: childPath.rawPath, pathBase: .groupDir, name: path.fileName
    )
    groups[path] = .present(group)
    return group
  }

  private func checkNotExcluded(
    _ path: RelativePath?, for description: String? = nil
  ) -> Bool {
    guard let path else { return true }

    // Not very efficient, but excludedPaths should be small in practice.
    guard let excluded = spec.excludedPaths.first(
      where: { path.starts(with: $0.path) }
    ) else {
      return true
    }
    if let description, let reason = excluded.reason {
      log.note("""
        Skipping \(description) at \
        '\(repoRelativePath.appending(path))'; \(reason)
        """)
    }
    return false
  }

  @discardableResult
  private func getOrCreateProjectRef(
    _ ref: ProjectSpec.PathReference
  ) -> Xcode.FileReference? {
    let path = ref.path
    if let file = files[path] {
      return file
    }
    assert(
      projectRootDir.appending(path).exists, "File '\(path)' does not exist"
    )
    // If this is a folder reference, make sure we haven't already created a
    // group there.
    if ref.kind == .folder {
      guard groups[path] == nil else {
        return nil
      }
    }
    guard let (parentGroup, childPath) = parentGroup(for: path) else {
      return nil
    }
    let file = parentGroup.addFileReference(
      path: childPath.rawPath, isDirectory: ref.kind == .folder,
      pathBase: .groupDir, name: path.fileName
    )
    files[path] = file
    return file
  }

  @discardableResult
  private func getOrCreateRepoRef(
    _ ref: ProjectSpec.PathReference
  ) -> Xcode.FileReference? {
    let path = ref.path
    guard checkNotExcluded(path) else { return nil }
    return getOrCreateProjectRef(ref.withPath(repoRelativePath.appending(path)))
  }

  @discardableResult
  func getOrCreateRepoBuildableFolder(
    at path: RelativePath
  ) -> Xcode.BuildableFolder? {
    guard let ref = getOrCreateRepoRef(.folder(path)) else { return nil }
    let folder = ref.getOrCreateBuildableFolder(at: path)
    buildableFolders[path] = folder

    // Exclude any sources we don't want to handle.
    do {
      let excluded = try buildDir.getAllRepoSubpaths(of: path)
        .filter(\.isExcludedSource)
      folder.setTargets([], for: excluded)
    } catch {
      log.error("\(error)")
    }

    return folder
  }

  private func getParentBuildableFolder(
    _ path: RelativePath
  ) -> Xcode.BuildableFolder? {
    // First check the mapping directly.
    if let buildableFolder = buildableFolders[path] {
      return buildableFolder
    }
    // Then check the parent.
    if let parent = path.parentDir,
        let buildableFolder = getParentBuildableFolder(parent) {
      buildableFolders[path] = buildableFolder
      return buildableFolder
    }
    return nil
  }

  func getAllRepoSubpaths(of parent: RelativePath) throws -> [RelativePath] {
    try buildDir.getAllRepoSubpaths(of: parent)
  }

  func generateBaseTarget(
    _ name: String, at parentPath: RelativePath?, canUseBuildableFolder: Bool,
    productType: Xcode.Target.ProductType?, includeInAllTarget: Bool
  ) -> Xcode.Target? {
    let name = {
      // If we have a same-named target, disambiguate.
      if targets[name] == nil {
        return name
      }
      var i = 2
      var newName: String { "\(name)\(i)" }
      while targets[newName] != nil {
        i += 1
      }
      return newName
    }()
    var buildableFolder: Xcode.BuildableFolder?
    // Note that special targets like "Unbuildables" have an empty parent path.
    if let parentPath, !parentPath.isEmpty {
      // If we've been asked to use buildable folders, see if we can create
      // a folder reference at the parent path. Otherwise, create a group at
      // the parent path. If we can't create either a folder or group, this is
      // nested in a folder reference and there's nothing we can do.
      if spec.useBuildableFolders && canUseBuildableFolder {
        buildableFolder = getOrCreateRepoBuildableFolder(at: parentPath)
      }
      guard buildableFolder != nil ||
              group(for: repoRelativePath.appending(parentPath)) != nil else {
        // If this isn't a child of an explicitly added reference, something
        // has probably gone wrong.
        if !spec.referencesToAdd.contains(
          where: { parentPath.starts(with: $0.path) }
        ) {
          log.warning("""
            Target '\(name)' at '\(repoRelativePath.appending(parentPath))' is \
            nested in a folder reference; skipping. This is likely an xcodegen bug.
            """)
        }
        return nil
      }
    }
    let target = project.addTarget(productType: productType, name: name)
    targets[name] = target
    if includeInAllTarget {
      allTarget.addDependency(on: target)
    }
    if let buildableFolder {
      target.addBuildableFolder(buildableFolder)
    }
    target.buildSettings.common.ONLY_ACTIVE_ARCH = "YES"
    target.buildSettings.common.USE_HEADERMAP = "NO"
    // The product name needs to be unique across every project we generate
    // (to allow the combined workspaces to work), so add in the project name.
    target.buildSettings.common.PRODUCT_NAME = "\(self.name)_\(name)"

    // Don't optimize or generate debug info, that will only slow down
    // compilation; we don't actually care about the binary.
    target.buildSettings.common.GCC_OPTIMIZATION_LEVEL = "0"
    target.buildSettings.common.GCC_GENERATE_DEBUGGING_SYMBOLS = "NO"
    target.buildSettings.common.GCC_WARN_64_TO_32_BIT_CONVERSION = "NO"

    return target
  }

  func replacingToolchainPath(_ str: String) -> String? {
    // Replace a toolchain path with the toolchain being used by Xcode.
    // TODO: Can we do better than a scan here? Could we get the old
    // toolchain path from the build args?
    str.scanningUTF8 { scanner in
      repeat {
        if scanner.tryEat(utf8: ".xctoolchain") {
          return "${TOOLCHAIN_DIR}\(String(utf8: scanner.remaining))"
        }
      } while scanner.tryEat()
      return nil
    }
  }

  func replacingProjectDir(_ str: String) -> String {
    // Replace paths within the project directory with PROJECT_DIR.
    str.replacing(projectRootDir.rawPath, with: "${PROJECT_DIR}")
  }

  func applyBaseSubstitutions(to buildArgs: inout BuildArgs) {
    buildArgs.transformValues(includeSubOptions: true) { value in
      if let replacement = replacingToolchainPath(value) {
        return replacement
      }
      return replacingProjectDir(value)
    }
  }

  /// Checks whether a target can be represented using a buildable folder.
  func canUseBuildableFolder(
    at parentPath: RelativePath, sources: [RelativePath]
  ) throws -> Bool {
    // To use a buildable folder, all child sources need to be accounted for
    // in the target. Ignore special targets like "Unbuildables" which have an
    // empty parent path.
    // TODO: We ought to be able to add stray sources as exclusions.
    guard spec.useBuildableFolders, !parentPath.isEmpty else { return false }
    let sources = Set(sources)
    return try getAllRepoSubpaths(of: parentPath)
      .allSatisfy { !$0.isSourceLike || sources.contains($0) }
  }

  /// Checks whether a given Clang target can be represented using a buildable
  /// folder.
  func canUseBuildableFolder(for clangTarget: ClangTarget) throws -> Bool {
    try canUseBuildableFolder(
      at: clangTarget.parentPath,
      sources: clangTarget.sources + clangTarget.unbuildableSources
    )
  }

  func canUseBuildableFolder(
    for buildRule: SwiftTarget.BuildRule
  ) throws -> Bool {
    guard let parentPath = buildRule.parentPath else { return false }
    return try canUseBuildableFolder(
      at: parentPath, sources: buildRule.sources.repoSources
    )
  }

  func addSourcesPhaseToClangTarget(
    _ target: Xcode.Target, sources: [RelativePath], targetPath: RelativePath
  ) throws {
    let sourcesToBuild = target.addSourcesBuildPhase()
    for source in sources {
      var fileArgs = try buildDir.clangArgs.getUniqueArgs(
        for: source, parent: targetPath, infer: spec.inferArgs
      )
      if !fileArgs.isEmpty {
        applyBaseSubstitutions(to: &fileArgs)
      }
      // If we're using a buildable folder, the extra arguments are added to it
      // directly.
      if let buildableFolder = getParentBuildableFolder(source) {
        if !fileArgs.isEmpty {
          buildableFolder.setExtraCompilerArgs(
            fileArgs.printedArgs, for: source, in: target
          )
        }
        continue
      }
      // Otherwise we add as a file reference and add the arguments to the
      // target.
      guard let sourceRef = getOrCreateRepoRef(.file(source)) else {
        continue
      }
      let buildFile = sourcesToBuild.addBuildFile(fileRef: sourceRef)

      // Add any per-file settings.
      buildFile.settings.COMPILER_FLAGS = fileArgs.printed
    }
  }

  func generateClangTarget(_ targetInfo: ClangTarget) throws {
    let targetPath = targetInfo.parentPath
    guard checkNotExcluded(targetPath, for: "Clang target") else {
      return
    }

    // Need to defer the addition of headers and unbuildable sources since the
    // target may want to use a buildable folder.
    defer {
      // If we're using a buildable folder, the headers are automatically
      // included.
      if let buildableFolder = getParentBuildableFolder(targetPath) {
        buildableFolder.setTargets(
          [unbuildablesTarget], for: targetInfo.unbuildableSources
        )
      } else {
        for header in targetInfo.headers {
          getOrCreateRepoRef(.file(header))
        }
      }
      // Add the unbuildable sources regardless of buildable folder since
      // we still need the compiler arguments to be set.
      unbuildableSources += targetInfo.unbuildableSources
    }

    // If we have no sources, we're done.
    if targetInfo.sources.isEmpty {
      // Inform the user if the target was completely empty.
      if targetInfo.headers.isEmpty && targetInfo.unbuildableSources.isEmpty {
        log.note("""
        Skipping '\(repoRelativePath)/\(targetPath)'; has no sources with \
        build args
        """)
      }
      // Still create a buildable folder if we can. It won't have an associated
      // target, but unbuildable sources may still be added as exceptions.
      if try canUseBuildableFolder(for: targetInfo) {
        getOrCreateRepoBuildableFolder(at: targetPath)
      }
      return
    }
    let target = generateBaseTarget(
      targetInfo.name, at: targetPath,
      canUseBuildableFolder: try canUseBuildableFolder(for: targetInfo),
      productType: .staticArchive, includeInAllTarget: true
    )
    guard let target else { return }

    var libBuildArgs = try buildDir.clangArgs.getArgs(for: targetPath)
    applyBaseSubstitutions(to: &libBuildArgs)

    target.buildSettings.common.HEADER_SEARCH_PATHS = 
      libBuildArgs.takePrintedValues(for: .I)

    target.buildSettings.common.CLANG_CXX_LANGUAGE_STANDARD =
      libBuildArgs.takeLastValue(for: .std)

    target.buildSettings.common.OTHER_CPLUSPLUSFLAGS = libBuildArgs.printedArgs

    try addSourcesPhaseToClangTarget(
      target, sources: targetInfo.sources, targetPath: targetPath
    )
  }

  /// Record path substitutions for a given target.
  func recordPathSubstitutions(
    for target: Xcode.Target, _ substitutions: [BuildArgs.PathSubstitution]
  ) {
    guard !substitutions.isEmpty else { return }
    includeSubstitutions.formUnion(substitutions)
    target.addDependency(on: includeSubstitutionTarget)
  }

  /// Add the script phase to populate the substituted includes if needed.
  func addSubstitutionPhaseIfNeeded() {
    guard !includeSubstitutions.isEmpty else { return }

    let subs = includeSubstitutions.sorted(by: \.oldPath.rawPath).map { sub in
      (oldPath: replacingProjectDir(sub.oldPath.rawPath),
       newPath: sub.newPath.rawPath)
    }

    let rsyncs = subs.map { sub in
      let oldPath = sub.oldPath.escaped
      let newPath = sub.newPath.escaped
      return """
      mkdir -p \(newPath)
      rsync -aqm --delete --exclude='*.swift*' --exclude '*.o' --exclude '*.d' \
      --exclude '*.dylib' --exclude '*.a' --exclude '*.cmake' --exclude '*.json' \
      \(oldPath)/ \(newPath)/
      """
    }.joined(separator: "\n")

    let command = """
      set -e
      if [ -z "${SYMROOT}" ]; then
        echo 'SYMROOT not defined'
        exit 1
      fi
      \(rsyncs)
      """

    includeSubstitutionTarget.addShellScriptBuildPhase(
      script: command,
      inputs: subs.map(\.oldPath),
      outputs: subs.map(\.newPath),
      alwaysRun: false
    )
  }

  func applySubstitutions(
    to buildArgs: inout BuildArgs, target: Xcode.Target, targetInfo: SwiftTarget
  ) {
    // First force -Onone. Running optimizations only slows down build times, we
    // don't actually care about the compiled binary.
    buildArgs.append(.flag(.Onone))

    // Exclude the experimental skipping function bodies flags, we specify
    // -experimental-skip-all-function bodies for modules, and if we promote
    // an emit module rule to a build rule, these would cause issues.
    buildArgs.exclude(
      .experimentalSkipNonInlinableFunctionBodies,
      .experimentalSkipNonInlinableFunctionBodiesWithoutTypes
    )
    if buildArgs.hasSubOptions(for: .swiftFrontend) {
      buildArgs[subOptions: .swiftFrontend].exclude(
        .experimentalSkipAllFunctionBodies,
        .experimentalSkipNonInlinableFunctionBodies,
        .experimentalSkipNonInlinableFunctionBodiesWithoutTypes
      )
    }

    // Then inject includes for the dependencies.
    for dep in targetInfo.dependencies {
      // TODO: The escaping here is easy to miss, maybe we should invest in
      // a custom interpolation type to make it clearer.
      buildArgs.append("-I \(getModuleDir(for: dep).escaped)")
    }

    // Replace references to the sdk with $SDKROOT.
    if let sdk = buildArgs.takeLastValue(for: .sdk) {
      buildArgs.transformValues(includeSubOptions: true) { value in
        value.replacing(sdk, with: "${SDKROOT}")
      }
    }
    buildArgs = buildArgs.map { arg in
      // -enable-experimental-cxx-interop was removed as a driver option in 5.9,
      // to maintain the broadest compatibility with different toolchains, use
      // the frontend option.
      guard arg.flag == .enableExperimentalCxxInterop else { return arg }
      return .option(
        .Xfrontend, spacing: .spaced, value: "\(.enableExperimentalCxxInterop)"
      )
    }
    // Replace includes that point into the build folder since they can
    // reference swiftmodules that expect a mismatched compiler. We'll
    // instead point them to a directory that has the swiftmodules removed,
    // and the modules will be picked up from the DerivedData products.
    let subs = buildArgs.substitutePaths(
      for: .I, includeSubOptions: true) { include -> RelativePath? in
      // NOTE: If llvm/clang ever start having swift targets, this will need
      // changing to encompass the parent. For now, avoid copying the extra
      // files.
      guard let suffix = include.removingPrefix(buildDir.path) else {
        return nil
      }
      return includeSubstDirectory.appending(suffix)
    }
    recordPathSubstitutions(for: target, subs)
    applyBaseSubstitutions(to: &buildArgs)
  }

  func getModuleDir(for target: SwiftTarget) -> RelativePath {
    "${SYMROOT}/Modules/\(target.name)"
  }

  var includeSubstDirectory: RelativePath {
    "${SYMROOT}/swift-includes"
  }

  @discardableResult
  func generateSwiftTarget(
    _ targetInfo: SwiftTarget, emitModuleRule: SwiftTarget.EmitModuleRule,
    includeInAllTarget: Bool = true
  ) throws -> Xcode.Target? {
    if addSwiftDependencies {
      // Produce a BuildRule and generate it.
      let buildRule = SwiftTarget.BuildRule(
        parentPath: nil, sources: emitModuleRule.sources,
        buildArgs: emitModuleRule.buildArgs
      )
      return try generateSwiftTarget(
        targetInfo, buildRule: buildRule, includeInAllTarget: includeInAllTarget
      )
    }
    let target = generateBaseTarget(
      targetInfo.name, at: nil, canUseBuildableFolder: false, productType: nil,
      includeInAllTarget: includeInAllTarget
    )
    guard let target else { return nil }

    var buildArgs = emitModuleRule.buildArgs
    for secondary in emitModuleRule.sources.externalSources {
      buildArgs.append(.value(secondary.rawPath))
    }
    applySubstitutions(to: &buildArgs, target: target, targetInfo: targetInfo)

    let targetDir = getModuleDir(for: targetInfo)
    let destModule = targetDir.appending("\(targetInfo.moduleName).swiftmodule")

    target.addShellScriptBuildPhase(
      script: """
        mkdir -p \(targetDir.escaped)
        run() {
          echo "$ $@"
          exec "$@"
        }
        run xcrun swiftc -sdk "${SDKROOT}" \
        -emit-module -emit-module-path \(destModule.escaped) \
        -Xfrontend -experimental-skip-all-function-bodies \
        \(buildArgs.printed)
        """,
      inputs: [],
      outputs: [destModule.rawPath],
      alwaysRun: true
    )
    return target
  }

  @discardableResult
  func generateSwiftTarget(
    _ targetInfo: SwiftTarget, buildRule: SwiftTarget.BuildRule,
    includeInAllTarget: Bool = true
  ) throws -> Xcode.Target? {
    guard checkNotExcluded(buildRule.parentPath, for: "Swift target") else {
      return nil
    }
    // Create the target.
    let target = generateBaseTarget(
      targetInfo.name, at: buildRule.parentPath,
      canUseBuildableFolder: try canUseBuildableFolder(for: buildRule),
      productType: .staticArchive,
      includeInAllTarget: includeInAllTarget
    )
    guard let target else { return nil }

    // Explicit modules currently fails to build with:
    // Invalid argument '-std=c++17' not allowed with 'Objective-C'
    target.buildSettings.common.SWIFT_ENABLE_EXPLICIT_MODULES = "NO"

    let buildSettings = target.buildSettings
    var buildArgs = buildRule.buildArgs

    applySubstitutions(to: &buildArgs, target: target, targetInfo: targetInfo)

    // Follow the same logic as swift-driver and set the module name to 'main'
    // if we don't have one.
    let moduleName = buildArgs.takePrintedLastValue(for: .moduleName)
    buildSettings.common.PRODUCT_MODULE_NAME = moduleName ?? "main"

    // Emit a module if we need to.
    // TODO: This currently just uses the build rule command args, should we
    // diff/merge the args? Or do it separately if they differ?
    if targetInfo.emitModuleRule != nil {
      buildSettings.common.DEFINES_MODULE = "YES"
    }

    // Disable the Obj-C bridging header; we don't currently use this, and
    // even if we did, we'd probably want to use the one in the Ninja build
    // folder.
    // This also works around a compiler crash
    // (https://github.com/swiftlang/swift/issues/78190).
    buildSettings.common.SWIFT_OBJC_INTERFACE_HEADER_NAME = ""

    if let last = buildArgs.takeFlagGroup(.O, .Onone) {
      buildSettings.common.SWIFT_OPTIMIZATION_LEVEL = last.printed
    }

    // Respect '-wmo' if passed.
    // TODO: Should we try force batch mode where we can? Unfortunately the
    // stdlib currently doesn't build with batch mode, so we'd need to special
    // case it.
    if buildArgs.takeFlags(.wmo, .wholeModuleOptimization) {
      buildSettings.common.SWIFT_COMPILATION_MODE = "wholemodule"
    }

    let swiftVersion = buildArgs.takeLastValue(for: .swiftVersion)
    buildSettings.common.SWIFT_VERSION = swiftVersion ?? "5.0"

    if let targetStr = buildArgs.takeLastValue(for: .target),
       let ver = targetStr.firstMatch(of: #/macosx?(\d+(?:\.\d+)?)/#) {
      buildSettings.common.MACOSX_DEPLOYMENT_TARGET = String(ver.1)
    }

    // Each target gets their own product dir. Add the search paths for
    // dependencies individually, so that we don't accidentally pull in a
    // module we don't need (e.g swiftCore for targets that don't want the
    // just-built stdlib).
    let productDir = getModuleDir(for: targetInfo).rawPath
    buildSettings.common.TARGET_BUILD_DIR = productDir
    buildSettings.common.BUILT_PRODUCTS_DIR = productDir

    buildSettings.common.SWIFT_INCLUDE_PATHS =
      buildArgs.takePrintedValues(for: .I)

    buildSettings.common.OTHER_SWIFT_FLAGS = buildArgs.printedArgs

    // Add compile sources phase.
    let sourcesToBuild = target.addSourcesBuildPhase()
    for source in buildRule.sources.repoSources {
      guard let sourceRef = getOrCreateRepoRef(.file(source)) else {
        continue
      }
      sourcesToBuild.addBuildFile(fileRef: sourceRef)
    }
    for absSource in buildRule.sources.externalSources {
      guard let source = absSource.removingPrefix(projectRootDir) else {
        log.warning("""
          Source file '\(absSource)' is outside the project directory; ignoring
          """)
        continue
      }
      guard let sourceRef = getOrCreateProjectRef(.file(source)) else {
        continue
      }
      sourcesToBuild.addBuildFile(fileRef: sourceRef)
    }
    // Finally add any .swift.gyb files.
    if let parentPath = buildRule.parentPath {
      for gyb in try getAllRepoSubpaths(of: parentPath) where gyb.isSwiftGyb {
        getOrCreateRepoRef(.file(gyb))
      }
    }
    return target
  }

  @discardableResult
  func generateSwiftTarget(
    _ target: SwiftTarget, includeInAllTarget: Bool = true
  ) throws -> Xcode.Target? {
    if let buildRule = target.buildRule {
      return try generateSwiftTarget(target, buildRule: buildRule)
    }
    if let emitModuleRule = target.emitModuleRule {
      return try generateSwiftTarget(target, emitModuleRule: emitModuleRule)
    }
    return nil
  }

  func sortGroupChildren(_ group: Xcode.Group) {
    group.subitems.sort { lhs, rhs in
      // The 'externals' group is special, sort it first.
      if (lhs === _externalsGroup) != (rhs === _externalsGroup) {
        return lhs === _externalsGroup
      }
      // Sort directories first.
      if lhs.isDirectoryLike != rhs.isDirectoryLike {
        return lhs.isDirectoryLike
      }
      // Then alphabetically.
      return lhs.displayName.lowercased() < rhs.displayName.lowercased()
    }
    for case let sub as Xcode.Group in group.subitems {
      sortGroupChildren(sub)
    }
  }

  func generateIfNeeded() throws {
    guard !generated else { return }
    generated = true

    // First add file/folder references.
    for ref in spec.referencesToAdd {
      getOrCreateRepoRef(ref)
    }

    // Gather the Swift targets to generate, including any dependencies.
    var swiftTargets: Set<SwiftTarget> = []
    for targetSource in spec.swiftTargetSources {
      for target in try buildDir.getSwiftTargets(for: targetSource) {
        swiftTargets.insert(target)
        swiftTargets.formUnion(target.dependencies)
      }
    }
    let sortedTargets = swiftTargets.sorted(by: \.name)
    if !sortedTargets.isEmpty {
      log.debug("---- SWIFT TARGETS TO GENERATE ----")
      log.debug("\(sortedTargets.map(\.name).joined(separator: ", "))")
      log.debug("-----------------------------------")
    }
    // Generate the Swift targets.
    var generatedSwiftTargets: [SwiftTarget: Xcode.Target] = [:]
    for target in sortedTargets {
      generatedSwiftTargets[target] = try generateSwiftTarget(target)
    }
    // Wire up the dependencies.
    for (targetInfo, target) in generatedSwiftTargets {
      for dep in targetInfo.dependencies {
        guard let depTarget = generatedSwiftTargets[dep] else { continue }
        target.addDependency(on: depTarget)
      }
    }

    // Add substitutions phase if any Swift targets need it.
    addSubstitutionPhaseIfNeeded()

    // Generate the Clang targets.
    for targetSource in spec.clangTargetSources.sorted(by: \.name) {
      let target = try buildDir.getClangTarget(
        for: targetSource, knownUnbuildables: spec.knownUnbuildables
      )
      guard let target else { continue }
      try generateClangTarget(target)
    }

    // Add any unbuildable sources to the special 'Unbuildables' target.
    if !unbuildableSources.isEmpty {
      try addSourcesPhaseToClangTarget(
        unbuildablesTarget, sources: unbuildableSources, targetPath: "."
      )
    }

    // Add targets for runnable targets if needed.
    if spec.addRunnableTargets && spec.addBuildForRunnableTargets {
      // We need to preserve PATH to find Ninja, which could e.g be in a
      // homebrew prefix, which isn't in the launchd environment (and therefore
      // Xcode doesn't have it).
      let path = getenv("PATH").map { String(cString: $0) }

      for runnable in try runnableTargets {
        // TODO: Can/should we use the external build tool target kind?
        let target = project.addTarget(name: "ninja-build-\(runnable.name)")
        var script = ""
        if let path {
          script += """
            export PATH="\(path)"

            """
        }
        script += """
          ninja -C \(spec.runnableBuildDir.path.escaped) -- \
          \(runnable.ninjaTargetName.escaped)
          """
        target.addShellScriptBuildPhase(
          script: script, inputs: [], outputs: [], alwaysRun: true
        )
        runnableBuildTargets[runnable] = target
      }
    }

    // Sort the groups.
    sortGroupChildren(project.mainGroup)
  }

  func generateAndWrite(
    into outputDir: AbsolutePath
  ) throws -> GeneratedProject {
    try generateIfNeeded()

    let projDir = outputDir.appending(pathName)

    let projDataPath = projDir.appending("project.pbxproj")
    try projDataPath.write(project.generatePlist().serialize())
    log.info("Generated '\(projDataPath)'")

    // Add the ALL meta-target as a scheme (we use a suffix to disambiguate it
    // from the ALL workspace scheme we generate).
    let allBuildTargets = [Scheme.BuildTarget(allTarget, in: pathName)]
    var schemes = SchemeGenerator(in: projDir)
    schemes.add(Scheme(
      "ALL-\(name)", replaceExisting: true, buildTargets: allBuildTargets
    ))

    // Add schemes for runnable targets.
    if spec.addRunnableTargets {
      for runnable in try runnableTargets {
        // Avoid replacing an existing scheme if it exists.
        // FIXME: Really we ought to be reading in the existing scheme, and
        // updating any values that need changing.
        var scheme = Scheme(runnable.name, replaceExisting: false)
        if let target = runnableBuildTargets[runnable] {
          scheme.buildAction.targets.append(.init(target, in: pathName))
        }
        // FIXME: Because we can't update an existing scheme, use a symlink to
        // refer to the run destination, allowing us to change it if needed.
        let link = projDir.appending(
          ".swift-xcodegen/runnable/\(runnable.name)"
        )
        try link.symlink(to: runnable.path)

        scheme.runAction = .init(path: link)
        schemes.add(scheme)
      }
    }
    try schemes.write()
    return GeneratedProject(at: projDir, allBuildTargets: allBuildTargets)
  }
}

extension ProjectSpec {
  public func generateAndWrite(
    into outputDir: AbsolutePath
  ) throws -> GeneratedProject {
    let generator = ProjectGenerator(for: self)
    return try generator.generateAndWrite(into: outputDir)
  }
}
