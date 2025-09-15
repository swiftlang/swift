//===--- SwiftTarget.swift ------------------------------------------------===//
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

final class SwiftTarget {
  let name: String
  let moduleName: String

  var buildRule: BuildRule?
  var emitModuleRule: EmitModuleRule?

  var dependencies: [SwiftTarget] = []

  init(name: String, moduleName: String) {
    self.name = name
    self.moduleName = moduleName
  }
}

extension SwiftTarget: Hashable {
  static func == (lhs: SwiftTarget, rhs: SwiftTarget) -> Bool {
    ObjectIdentifier(lhs) == ObjectIdentifier(rhs)
  }
  func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }
}

extension SwiftTarget: CustomDebugStringConvertible {
  var debugDescription: String {
    name
  }
}

extension SwiftTarget {
  struct Sources {
    var repoSources: [RelativePath] = []
    var externalSources: [AbsolutePath] = []
  }
  struct BuildRule {
    var parentPath: RelativePath?
    var sources: Sources
    var buildArgs: BuildArgs
  }
  struct EmitModuleRule {
    var sources: Sources
    var buildArgs: BuildArgs
  }
}

extension SwiftTarget {
  var buildArgs: BuildArgs {
    buildRule?.buildArgs ?? emitModuleRule?.buildArgs ?? .init(for: .swiftc)
  }
}

extension RepoBuildDir {
  func getSwiftTargets(for source: SwiftTargetSource) throws -> [SwiftTarget] {
    try swiftTargets.getTargets(below: source.path)
  }
}
