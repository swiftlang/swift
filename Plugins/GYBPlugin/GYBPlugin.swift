//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2014-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Plugin that expands *.swift.gyb files using the Gyb tool that is part of
// the Swift repository.
//
//===----------------------------------------------------------------------===//
import Foundation
import PackagePlugin

@main
struct GYBPlugin: BuildToolPlugin {
  func createBuildCommands(context: PluginContext, target: Target) throws -> [Command] {
    guard let sourceModule = target.sourceModule else { return [] }

    let gybFiles = sourceModule.sourceFiles(withSuffix: ".gyb").map {
      $0.url.path
    }

    if gybFiles.isEmpty {
      return []
    }

    // TODO: Find 'python' in a more robust manner, or switch to a Swift-only
    // implementation of gyb.
    let python = FileManager.default.fileExists(atPath: "/usr/local/bin/python3")
      ? URL(filePath: "/usr/local/bin/python3")!
      : URL(filePath: "/usr/bin/python3")!
    let gybPath = URL(filePath: "utils/gyb")!.path
    
    // Set pointer size passed to GYB.
    // TODO: Can we get this from the target description somehow?
    let pointerSize: Int
    if let pointerSizeEnv = ProcessInfo.processInfo.environment["SWIFT_POINTER_SIZE"] {
      pointerSize = Int(pointerSizeEnv)!
    } else {
      pointerSize = 8
    }

    return gybFiles.map { gybFilePath in
      let swiftFileURL = URL(filePath: context.pluginWorkDirectoryURL.path)
        .appending(path: "\(pointerSize)")
        .appending(path: URL(filePath: gybFilePath).lastPathComponent)
        .deletingPathExtension()
      return .buildCommand(
        displayName: "GYB \(gybFilePath) in module \(sourceModule.name) for ptr size = \(pointerSize) to \(swiftFileURL.path)",
        executable: python,
        arguments: [
          gybPath, "-DCMAKE_SIZEOF_VOID_P=\(pointerSize)",
          "-o", swiftFileURL.path,
          gybFilePath,
        ],
        inputFiles: [ URL(filePath: gybFilePath) ],
        outputFiles: [ swiftFileURL ]
      )
    }
  }
}
