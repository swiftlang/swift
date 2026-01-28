import Foundation
import PackagePlugin

@main
struct GYBPlugin: BuildToolPlugin {
  func createBuildCommands(context: PluginContext, target: Target) throws -> [Command] {
    let sourceDirURL = target.directoryURL
    guard let sourceModule = target.sourceModule else { return [] }

    let cmakeFile = sourceDirURL.appending(path: "CMakeLists.txt")
    let gybFiles = try [SourceFile](sourcesFromCMakeFile: cmakeFile.path)
      .filter { $0.gyb && $0.kind == .embedded }
      .map { $0.filename }

    if gybFiles.isEmpty {
      return []
    }

    // TODO: Find python and gyb for real
    let python = URL(filePath: "/usr/local/bin/python")!
    let gybPath = URL(filePath: "../..//utils/gyb")!.path
    
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
        .appending(path: gybFilePath)
        .deletingPathExtension()
      return .buildCommand(
        displayName: "GYB \(gybFilePath) in module \(sourceModule.name) for ptr size = \(pointerSize) to \(swiftFileURL.path)",
        executable: python,
        arguments: [
          gybPath, "-DCMAKE_SIZEOF_VOID_P=\(pointerSize)",
          "-o", swiftFileURL.path,
          sourceDirURL.appending(path: gybFilePath).path,
        ],
        inputFiles: [ cmakeFile, sourceDirURL.appending(path: gybFilePath) ],
        outputFiles: [ swiftFileURL ]
      )
    }
  }
}

enum FileKind: String {
case normal = "NORMAL"
case embedded = "EMBEDDED"
}

struct SourceFile {
  let filename: String
  let kind: FileKind
  let gyb: Bool
}

extension [SourceFile] {
  /// Parse the given CMakeLists.txt file to find all of the source files in it.
  init(sourcesFromCMakeFile cmakeFilename: String) throws {
    let allLines = try String(contentsOfFile: cmakeFilename, encoding: .utf8)
      .split(separator: "\n")
    let sourceFileRegex = #/(?<kind>(NORMAL|EMBEDDED))(\s+)(?<filename>[^ ]*\.swift)(?<gyb>\.gyb)?/#
    self = allLines.compactMap { line in
      line.firstMatch(of: sourceFileRegex).map { matched in
        SourceFile(
          filename: String(matched.filename + (matched.gyb ?? "")),
          kind: FileKind(rawValue: String(matched.kind))!,
          gyb: matched.gyb != nil
        )
      }
    }
  }
}
