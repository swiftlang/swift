//===--- WorkspaceGenerator.swift -----------------------------------------===//
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

public struct WorkspaceGenerator {
  var elements: [Element] = []
  public init() {}
}

public extension WorkspaceGenerator {
  enum Element {
    case xcodeProj(GeneratedProject)
    case group(RelativePath, targets: [String])
  }

  mutating func addProject(_ proj: GeneratedProject) {
    elements.append(.xcodeProj(proj))
  }
  mutating func addGroup(at path: RelativePath, targets: [String]) {
    elements.append(.group(path, targets: targets))
  }

  func write(_ name: String, into dir: AbsolutePath) throws {
    var contents = """
      <?xml version="1.0" encoding="UTF-8"?>
      <Workspace version = "1.0">

      """
    for element in elements {
      contents += "<FileRef location = "
      switch element {
      case .xcodeProj(let proj):
        // FIXME: This is assuming the workspace will be siblings with the
        // project.
        contents += "\"container:\(proj.path.fileName)\""
      case .group(let path, _):
        contents += "\"group:\(path)\""
      }
      contents += "></FileRef>\n"
    }
    contents += "</Workspace>"

    let workspaceDir = dir.appending("\(name).xcworkspace")

    // Skip generating if there's only a single container and it doesn't already
    // exist.
    guard elements.count > 1 || workspaceDir.exists else { return }

    let dataPath = workspaceDir.appending("contents.xcworkspacedata")
    try dataPath.write(contents)
    log.info("Generated '\(dataPath)'")

    var schemes = SchemeGenerator(in: workspaceDir)
    let buildTargets =
      elements
      .sorted(by: {
        // Sort project schemes first.
        switch ($0, $1) {
        case (.xcodeProj, .group):
          return true
        default:
          return false
        }
      })
      .flatMap { elt in
        switch elt {
        case .xcodeProj(let proj):
          return proj.allBuildTargets
        case .group(let path, let targets):
          return targets.map { target in
            Scheme.BuildTarget(target, in: path)
          }
        }
      }
    schemes.add(
      Scheme(
        "ALL",
        replaceExisting: true,
        buildTargets: buildTargets
      )
    )
    try schemes.write()
  }
}
