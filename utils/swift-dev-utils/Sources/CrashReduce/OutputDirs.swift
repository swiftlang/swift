//===--- OutputDirs.swift -------------------------------------------------===//
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

public struct OutputDirs {
  public var main: AbsolutePath
  public var ideCrashers: AbsolutePath
  public var cxxCrashers: AbsolutePath

  public init(
    main: AbsolutePath,
    ideCrashers: AbsolutePath,
    cxxCrashers: AbsolutePath,
  ) {
    self.main = main
    self.ideCrashers = ideCrashers
    self.cxxCrashers = cxxCrashers
  }

  var allPaths: [AbsolutePath] {
    Set([main, ideCrashers, cxxCrashers]).sorted(by: \.rawPath)
  }

  func outputDir(for reproducer: Reproducer) -> AbsolutePath {
    if reproducer.kind == .complete {
      return ideCrashers
    }
    if reproducer.options.extraArgs.contains(where: {
      $0.value?.starts(with: "-cxx-interoperability-mode") ?? false
    }) {
      return cxxCrashers
    }
    return main
  }
}
