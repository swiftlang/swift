// swift-tools-version: 6.3

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
// This package manifest is used only for Embedded Swift. Clients need to
// provide a number of specialized build settings to make it work at all.
//
//===----------------------------------------------------------------------===//

import CompilerPluginSupport
import Foundation
import PackageDescription

// Common Swift settings for the Standard Library and its various supporting
// libraries. This was extracted from CMakeLists.txt and likely contains some
// stale settings.
let basicSwiftSettings: [SwiftSetting] = try! [
  .enableExperimentalFeature("NoncopyableGenerics2"),
  .enableExperimentalFeature("SE427NoInferenceOnExtension"),
  .enableExperimentalFeature("NonescapableTypes"),
  .enableExperimentalFeature("Lifetimes"),
  .enableExperimentalFeature("LifetimeDependence"),
  .enableExperimentalFeature("InoutLifetimeDependence"),
  .enableExperimentalFeature("LifetimeDependenceMutableAccessors"),
  .enableExperimentalFeature("BorrowAndMutateAccessors"),
  .enableExperimentalFeature("Reparenting"),
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
  platforms: [
    .macOS(.v15),
    .iOS(.v13),
    .tvOS(.v13),
    .watchOS(.v6),
    .macCatalyst(.v13)
  ],
  products: [
    .library(
      name: "Swift",
      targets: ["Swift"]
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
  traits: [
    /// The UnicodeDataTables trait is used to link in the library containing
    /// the Unicode data tables.
    .trait(name: "UnicodeDataTables"),
  ],
  targets: [
    // Swift standard library
    .target(
      name: "Swift",
      dependencies: [
        .target(
          name: "SwiftUnicodeDataTables",
          condition: .when(traits: ["UnicodeDataTables"])
        ),
      ],
      path: "stdlib/public/core",
      exclude: [
        // Not relevant to the SwiftPM build.
        "GroupInfo.json",
        "PreviousModuleInstallName.json",

        // This is hidden for some platforms in the CMake.
        // TODO: Figure out whether we can get this enabled everywhere,
        // using #ifs in the sources if necessary.
        "ObjectIdentifier+DebugDescription.swift",
        
        // TODO: This file is unused and should probably be removed.
        "EitherSequence.swift",

        // Ignore the CMake build system.
        "CMakeLists.txt",

        // Standard library files that aren't included in the Embedded
        // build. These are marked as "NORMAL" in CMakeLists.txt
        "BridgeObjectiveC.swift",
        "BridgingBuffer.swift",
        "CocoaArray.swift",
        "Codable.swift",
        "CommandLine.swift",
        "DebuggerSupport.swift",
        "Dump.swift",
        "Hashing.swift",
        "LegacyABI.swift",
        "Macros.swift",
        "Mirrors.swift",
        "PlaygroundDisplay.swift",
        "Prespecialize.swift",
        "Print.swift",
        "REPL.swift",
        "ReflectionMirror.swift",
        "RuntimeFunctionCounters.swift",
        "SetAnyHashableExtensions.swift",
        "SetBridging.swift",
        "Shims.swift",
        "ThreadLocalStorage.swift",
        "VarArgs.swift",
        "AtomicInt.swift.gyb",
      ],
      swiftSettings: basicSwiftSettings + [
        .enableExperimentalFeature("Extern"),
        .enableExperimentalFeature("AddressableTypes"),
        .enableExperimentalFeature("AddressableParameters"),
        .unsafeFlags(["-parse-stdlib"]),
      ],
      plugins: ["GYBPlugin"],
    ),

    // Unicode tables for operations that require them. Clients are expected
    // to use this by defining the UnicodeDataTables trait.
    .target(
      name: "SwiftUnicodeDataTables",
      path: "stdlib/public/stubs/Unicode",
      sources: [
        "UnicodeData.cpp",
        "UnicodeGrapheme.cpp",
        "UnicodeNormalization.cpp",
        "UnicodeScalarProps.cpp",
        "UnicodeWord.cpp",
      ],
      publicHeadersPath: "",
      cSettings: [
        .headerSearchPath("../../../../stdlib/public/SwiftShims"),
        .define("SWIFT_STDLIB_ENABLE_UNICODE_DATA"),
      ],
    ),

    .target(
      name: "_Builtin_float",
      dependencies: ["Swift"],
      path: "stdlib/public/ClangOverlays",
      exclude: [
        // Ignore the CMake build system.
        "CMakeLists.txt",

        "linker-support/magic-symbols-for-install-name.c",
      ],
      swiftSettings: basicSwiftSettings,
      plugins: ["GYBPlugin"],
    ),

    .target(
      name: "_Volatile",
      dependencies: ["Swift"],
      path: "stdlib/public/Volatile",
      exclude: [
        "CMakeLists.txt",
      ],
      swiftSettings: basicSwiftSettings + [
        .unsafeFlags(["-parse-stdlib"]),
      ],
    ),

    .target(
      name: "Synchronization",
      dependencies: ["Swift"],
      path: "stdlib/public/Synchronization",
      exclude: [
        // Ignore the CMake build system.
        "CMakeLists.txt",

        // TODO: This exclusion list means that Mutex support is completely
        // disabled.
        "Mutex/Mutex.swift",
        "Mutex/DarwinImpl.swift",
        "Mutex/FreeBSDImpl.swift",
        "Mutex/LinuxImpl.swift",
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
    ),
  ]
)

// utils/availability-macros.def processing logic, used to figure out
// availability macro definitions.
var availabilityMacros: [String] {
  get throws {
    let allLines = try String(contentsOfFile: Context.packageDirectory + "/utils/availability-macros.def", encoding: .utf8)
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
