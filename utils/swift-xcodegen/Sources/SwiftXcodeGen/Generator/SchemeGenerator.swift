//===--- SchemeGenerator.swift --------------------------------------------===//
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

struct Scheme {
  var name: String
  var buildAction: BuildAction
  var runAction: RunAction?
  var replaceExisting: Bool

  init(_ name: String, replaceExisting: Bool, buildTargets: [BuildTarget]) {
    self.name = name
    self.replaceExisting = replaceExisting
    self.buildAction = .init(targets: buildTargets)
  }

  init(_ name: String, replaceExisting: Bool, buildTargets: BuildTarget...) {
    self.init(
      name,
      replaceExisting: replaceExisting,
      buildTargets: buildTargets
    )
  }

  mutating func addBuildTarget(
    _ target: Xcode.Target,
    in path: RelativePath
  ) {
    buildAction.targets.append(.init(target, in: path))
  }
}

extension Scheme {
  struct BuildAction {
    var targets: [BuildTarget]
  }

  struct BuildTarget {
    var name: String
    var container: RelativePath

    init(_ target: Xcode.Target, in path: RelativePath) {
      self.name = target.name
      self.container = path
    }

    init(_ name: String, in path: RelativePath) {
      self.name = name
      self.container = path
    }
  }

  struct RunAction {
    var path: AbsolutePath
  }
}

struct SchemeGenerator {
  let containerDir: AbsolutePath
  let disableAutoCreation: Bool

  var schemes: [Scheme] = []

  init(in containerDir: AbsolutePath, disableAutoCreation: Bool = true) {
    self.containerDir = containerDir
    self.disableAutoCreation = disableAutoCreation
  }
}

extension SchemeGenerator {
  mutating func add(_ scheme: Scheme) {
    schemes.append(scheme)
  }
}

extension SchemeGenerator {
  private func disableAutoCreationIfNeeded() throws {
    guard disableAutoCreation else { return }

    var relPath: RelativePath = "xcshareddata/WorkspaceSettings.xcsettings"
    if containerDir.hasExtension(.xcodeproj) {
      relPath = "project.xcworkspace/\(relPath)"
    }
    let settingsPath = containerDir.appending(relPath)

    let workspaceSettings = """
      <?xml version="1.0" encoding="UTF-8"?>
      <plist version="1.0">
      <dict>
          <key>IDEWorkspaceSharedSettings_AutocreateContextsIfNeeded</key>
          <false/>
      </dict>
      </plist>
      """

    try settingsPath.write(workspaceSettings)
    log.info("Generated '\(settingsPath)'")
  }

  private func writeScheme(_ scheme: Scheme) throws {
    let path = containerDir.appending(
      "xcshareddata/xcschemes/\(scheme.name).xcscheme"
    )

    // Don't overwrite if we haven't been asked to.
    if !scheme.replaceExisting && path.exists {
      return
    }

    var plist = """
      <?xml version="1.0" encoding="UTF-8"?>
      <Scheme LastUpgradeVersion = "9999" version = "1.3">
        <BuildAction parallelizeBuildables = "YES" buildImplicitDependencies = "YES">
          <BuildActionEntries>

      """
    for buildTarget in scheme.buildAction.targets {
      plist += """
        <BuildActionEntry buildForTesting = "YES" buildForRunning = "YES" buildForProfiling = "YES" buildForArchiving = "YES" buildForAnalyzing = "YES">
          <BuildableReference
            BuildableIdentifier = "primary"
            BuildableName = "\(buildTarget.name)"
            BlueprintName = "\(buildTarget.name)"
            ReferencedContainer = "container:\(buildTarget.container)">
          </BuildableReference>
        </BuildActionEntry>

        """
    }
    plist += """
          </BuildActionEntries>
        </BuildAction>

      """

    if let runAction = scheme.runAction {
      plist += """
        <LaunchAction>
          <PathRunnable
             FilePath = "\(runAction.path.escaped(addQuotesIfNeeded: false))">
          </PathRunnable>
        </LaunchAction>

        """
    }

    plist += """
      </Scheme>
      """

    try path.write(plist)
    log.info("Generated '\(path)'")
  }

  private func writeSchemes() throws {
    for scheme in schemes {
      try writeScheme(scheme)
    }
  }

  func write() throws {
    try disableAutoCreationIfNeeded()
    try writeSchemes()
  }
}
