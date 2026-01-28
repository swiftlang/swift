// swift-tools-version: 6.3
import CompilerPluginSupport
import Foundation
import PackageDescription

let coreSources = try! [SourceFile](sourcesFromCMakeFile: Context.packageDirectory + "/core/CMakeLists.txt")
let coreEmbeddedSources = coreSources
    .filter { !$0.gyb && $0.kind == .embedded }
    .map { $0.filename }
let coreExcludedSources = coreSources
    .filter { $0.kind == .normal }
    .map { $0.filename }

// Common Swift settings for the Standard Library and its various supporting
// libraries. This was extracted from CMakeLists.txt and likely contains some
// stale settings.
let basicSwiftSettings: [SwiftSetting] = try! [
  .enableExperimentalFeature("NoncopyableGenerics2"),
  .enableExperimentalFeature("SuppressedAssociatedTypes"),
  .enableExperimentalFeature("SE427NoInferenceOnExtension"),
  .enableExperimentalFeature("NonescapableTypes"),
  .enableExperimentalFeature("LifetimeDependence"),
  .enableExperimentalFeature("InoutLifetimeDependence"),
  .enableExperimentalFeature("LifetimeDependenceMutableAccessors"),
  .enableExperimentalFeature("Embedded"),
  .swiftLanguageMode(.v5),
  .strictMemorySafety(),
  .unsafeFlags([
      "-Xfrontend", "-emit-empty-object-file",
      "-nostdimport", "-nostdlibimport",
    ])
] + availabilityMacros.map { macro in
  .unsafeFlags([
      "-Xfrontend", "-define-availability", "-Xfrontend", "\(macro)"
  ])
}

let package = Package(
  name: "SwiftStandardLibrary",
  platforms: [.macOS(.v15), .iOS(.v13), .tvOS(.v13), .watchOS(.v6), .macCatalyst(.v13)],
  products: [
    .library(
      name: "Swift",
      targets: ["Swift"]
    ),

    .library(
      name: "SwiftUnicodeDataTables",
      targets: ["SwiftUnicodeDataTables"]
    ),

    .library(
      name: "_Builtin_float",
      targets: ["_Builtin_float"]
    ),

    .library(
      name: "_Volatile",
      targets: ["_Volatile"]
    ),

    .library(
      name: "Synchronization",
      targets: ["Synchronization"]
    ),
  ],
  targets: [
    // Swift standard library
    .target(
      name: "Swift",
      path: "core",
      exclude: coreExcludedSources + [
        // Not relevant to the SwiftPM build.
        "GroupInfo.json",
        "PreviousModuleInstallName.json",

        // This is hidden for some platforms in the CMake.
        // TODO: Figure out whether we can get this enabled everywhere,
        // using #ifs in the sources if necessary.
        "ObjectIdentifier+DebugDescription.swift",
        
        // TODO: This file is unused and should probably be removed.
        "EitherSequence.swift",
      ],
      sources: coreEmbeddedSources + [
        // TODO: These are Embedded-only, which isn't a notion that is modeled
        // in CMakeLists.txt
        "EmbeddedRuntime.swift",
        "EmbeddedStubs.swift",
        "EmbeddedPrint.swift",

        // This is behind a CMake flag (SWIFT_STDLIB_ENABLE_VECTOR_TYPES).
        // Assume it should always enabled for now.
        "SIMDVector.swift",
      ],
      swiftSettings: basicSwiftSettings + [
        .enableExperimentalFeature("Extern"),
        .enableExperimentalFeature("AddressableTypes"),
        .enableExperimentalFeature("AddressableParameters"),
        .unsafeFlags(["-parse-stdlib"]),
      ],
      plugins: ["GYBPlugin"],
    ),

    // Unicode tables for String and Character operations that require them.
    // TODO: This might be more cleanly expressed as a trait.
    .target(
      name: "SwiftUnicodeDataTables",
      path: "stubs/Unicode",
      sources: [
        "UnicodeData.cpp",
        "UnicodeGrapheme.cpp",
        "UnicodeNormalization.cpp",
        "UnicodeScalarProps.cpp",
        "UnicodeWord.cpp",
      ],
      publicHeadersPath: "",
      cSettings: [
        .headerSearchPath("../../SwiftShims"),
      ],
    ),

    .target(
      name: "_Builtin_float",
      dependencies: ["Swift"],
      path: "ClangOverlays",
      exclude: [
        "linker-support/magic-symbols-for-install-name.c",
      ],
      swiftSettings: basicSwiftSettings,
      plugins: ["GYBPlugin"],
    ),

    .target(
      name: "_Volatile",
      dependencies: ["Swift"],
      path: "Volatile",
      swiftSettings: basicSwiftSettings + [
        .unsafeFlags(["-parse-stdlib"]),
      ],
    ),

    .target(
      name: "Synchronization",
      dependencies: ["Swift"],
      path: "Synchronization",
      exclude: [
        // TODO: This exclusion list is macOS-specific.
        "Mutex/FreeBSDImpl.swift",
        "Mutex/LinuxImpl.swift",
        "Mutex/MutexUnavailable.swift",
        "Mutex/OpenBSDImpl.swift",
        "Mutex/SpinLoopHint.swift",
        "Mutex/WasmImpl.swift",
        "Mutex/WindowsImpl.swift",
      ],
      swiftSettings: basicSwiftSettings + [
        .enableExperimentalFeature("BuiltinModule"),
        .enableExperimentalFeature("RawLayout"),
        .enableExperimentalFeature("StaticExclusiveOnly"),
      ],
      plugins: ["GYBPlugin"],
    ),

    // Plugin for expanding .swift.gyb files into .swift files.
    .plugin(
      name: "GYBPlugin",
      capability: .buildTool(),
      path: "GYBPlugin",
    ),
  ]
)

// utils/availability-macros.def processing logic, used to figure out
// availability macro definitions.
var availabilityMacros: [String] {
  get throws {
    let allLines = try String(contentsOfFile: Context.packageDirectory + "/../../utils/availability-macros.def", encoding: .utf8)
      .split(separator: "\n")
     let foundMacros = allLines.compactMap { (line) -> String? in
      if line.contains("#") { return nil }

      guard let colonIndex = line.firstIndex(of: ":") else {
        return nil
      }

      // Under embedded swift, all stdlib APIs should be available always.
      // Replace all availability macros with very very old OS version.      
      return String(line[..<colonIndex]) + ":macOS 10.9, iOS 7.0, watchOS 2.0, tvOS 9.0, visionOS 1.0"
    }

    // Create "StdlibDeploymentTarget" counterparts for "SwiftStdlib"
    // availability macros.
    return foundMacros + foundMacros.compactMap { macro in
      guard macro.contains("SwiftStdlib") else {
        return nil
      }

      let afterPrefix = macro.index(macro.startIndex, offsetBy: "SwiftStdlib".count)
      return "StdlibDeploymentTarget" + String(macro[afterPrefix...])
    }
  }
}

// CMakeLists.txt processing logic, used to automatically keep the source files
// for the standard library in sync.
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

