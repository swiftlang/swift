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

let availabilitySettings: [SwiftSetting] = try!
  availabilityMacros.map { macro in
    .unsafeFlags([
        "-Xfrontend", "-define-availability", "-Xfrontend", "\(macro)"
    ])
  }

// Common Swift settings for the Standard Library and its various supporting
// libraries. This was extracted from CMakeLists.txt and likely contains some
// stale settings.
let basicSwiftSettings: [SwiftSetting] = [
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
] + availabilitySettings

let package = Package(
  name: "swift-standard-library",
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

    .library(
      name: "_Concurrency",
      targets: ["_Concurrency"]
    ),
  ],
  traits: [
    /// The UnicodeDataTables trait is used to link in the library containing
    /// the Unicode data tables.
    .trait(name: "UnicodeDataTables"),
  ],
  targets: [
    .target(
      name: "SwiftShims",
      path: "stdlib/public/SwiftShims/swift/shims",
      publicHeadersPath: "."
    ),

    // Swift standard library
    .target(
      name: "Swift",
      dependencies: [
        "SwiftShims",
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

    .target(
      name: "_ConcurrencyCpp",
      path: "stdlib/public/Concurrency",
      sources: [
        "Actor.cpp",
        "AsyncLet.cpp",
        "Clock.cpp",
        "ConcurrencyHooks.cpp",
        "GlobalExecutor.cpp",
        "EmbeddedSupport.cpp",
        "Error.cpp",
        "ExecutorBridge.cpp",
        "ExecutorChecks.cpp",
        "Task.cpp",
        "TaskAlloc.cpp",
        "TaskStatus.cpp",
        "TaskGroup.cpp",
        "TaskLocal.cpp",
        "ThreadingError.cpp",
        "TracingSignpost.cpp",
        "AsyncStream.cpp",
      ],
      publicHeadersPath: ".",
      cSettings: [
        .headerSearchPath("../../../include/"),
        .headerSearchPath("../../../stdlib/public/SwiftShims"),
        .headerSearchPath("../../../stdlib/public/DummyHeaders"),
        .headerSearchPath("../../../stdlib/include"),
        .define("SWIFT_TARGET_LIBRARY_NAME", to: "swift_Concurrency"),
        .define("SWIFT_CONCURRENCY_EMBEDDED", to: "1"),
        .define("SWIFT_RUNTIME_EMBEDDED", to: "1"),
      ],
      plugins: ["GYBPlugin"],
    ),

    .target(
      name: "ConcurrencyShims",
      path: "stdlib/public/Concurrency/InternalShims",
      publicHeadersPath: "."
    ),

    .target(
      name: "_Concurrency",
      dependencies: ["ConcurrencyShims", "Swift", "Synchronization", "_ConcurrencyCpp"],
      path: "stdlib/public/Concurrency",
      sources: [
        "Actor.swift",
        "AsyncLet.swift",
        "CheckedContinuation.swift",
        "Errors.swift",
        "Executor.swift",
        "ExecutorBridge.swift",
        "ExecutorAssertions.swift",
        "AsyncCompactMapSequence.swift",
        "AsyncDropFirstSequence.swift",
        "AsyncDropWhileSequence.swift",
        "AsyncFilterSequence.swift",
        "AsyncFlatMapSequence.swift",
        "AsyncIteratorProtocol.swift",
        "AsyncMapSequence.swift",
        "AsyncPrefixSequence.swift",
        "AsyncPrefixWhileSequence.swift",
        "AsyncSequence.swift",
        "AsyncThrowingCompactMapSequence.swift",
        "AsyncThrowingDropWhileSequence.swift",
        "AsyncThrowingFilterSequence.swift",
        "AsyncThrowingFlatMapSequence.swift",
        "AsyncThrowingMapSequence.swift",
        "AsyncThrowingPrefixWhileSequence.swift",
        "PartialAsyncTask.swift",
        "GlobalActor.swift",
        "GlobalConcurrentExecutor.swift",
        "MainActor.swift",
        "PriorityQueue.swift",
        "SourceCompatibilityShims.swift",
        "Task.swift",
        "Task+PriorityEscalation.swift",
        "Task+TaskExecutor.swift",
        "TaskCancellation.swift",
        "TaskGroup.swift",
        "DiscardingTaskGroup.swift",
        "TaskLocal.swift",
        "TaskSleep.swift",
        "AsyncStreamBuffer.swift",
        "AsyncStream.swift",
        "AsyncThrowingStream.swift",
        "Deque/_DequeBuffer.swift",
        "Deque/_DequeBufferHeader.swift",
        "Deque/_DequeSlot.swift",
        "Deque/_UnsafeWrappedBuffer.swift",
        "Deque/Compatibility.swift",
        "Deque/Deque+Storage.swift",
        "Deque/Deque+UnsafeHandle.swift",
        "Deque/Deque.swift",
        "Deque/Deque+Codable.swift",
        "Deque/Deque+Collection.swift",
        "Deque/Deque+CustomDebugStringConvertible.swift",
        "Deque/Deque+CustomReflectable.swift",
        "Deque/Deque+CustomStringConvertible.swift",
        "Deque/Deque+Equatable.swift",
        "Deque/Deque+ExpressibleByArrayLiteral.swift",
        "Deque/Deque+Extras.swift",
        "Deque/Deque+Hashable.swift",
        "Deque/Deque+Testing.swift",
        "Deque/UnsafeMutableBufferPointer+Utilities.swift",
        "Clock.swift",
        "ContinuousClock.swift",
        "SuspendingClock.swift",
        "TaskSleepDuration.swift",
        "UnimplementedExecutor.swift",
        "CooperativeExecutor.swift",
        "PlatformExecutorCooperative.swift",
        "PlatformExecutorDarwin.swift",
        "PlatformExecutorLinux.swift",
        "PlatformExecutorWindows.swift",
        "PlatformExecutorOpenBSD.swift",
        "PlatformExecutorFreeBSD.swift",
      ],
      swiftSettings: basicSwiftSettings + [
        .enableExperimentalFeature("BuiltinModule"),
        .unsafeFlags(["-parse-stdlib"]),
        .define("SWIFT_CONCURRENCY_EMBEDDED"),
        .unsafeFlags(["-runtime-compatibility-version", "none"]),
      ],
      plugins: ["GYBPlugin"],
    ),

    // Plugin for expanding .swift.gyb files into .swift files.
    .plugin(
      name: "GYBPlugin",
      capability: .buildTool(),
    ),
  ],
  cxxLanguageStandard: .gnucxx17
)

// utils/availability-macros.def processing logic, used to figure out
// availability macro definitions.
var availabilityMacros: [String] {
  get throws {
    // Due to https://github.com/swiftlang/swift-package-manager/issues/5610,
    // we're unable to use packageDirectory, so hardcode the string.
    #if false
    let allLines = try String(contentsOfFile: Context.packageDirectory + "utils/availability-macros.def", encoding: .utf8)
      .split(separator: "\n")
    #else
    let allLines = """
      SwiftStdlib 9999:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999
      SwiftStdlib 5.0:macOS 10.14.4, iOS 12.2, watchOS 5.2, tvOS 12.2
      SwiftStdlib 5.1:macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0
      SwiftStdlib 5.2:macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4
      SwiftStdlib 5.3:macOS 11.0, iOS 14.0, watchOS 7.0, tvOS 14.0
      SwiftStdlib 5.4:macOS 11.3, iOS 14.5, watchOS 7.4, tvOS 14.5
      SwiftStdlib 5.5:macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0
      SwiftStdlib 5.6:macOS 12.3, iOS 15.4, watchOS 8.5, tvOS 15.4
      SwiftStdlib 5.7:macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0
      SwiftStdlib 5.8:macOS 13.3, iOS 16.4, watchOS 9.4, tvOS 16.4
      SwiftStdlib 5.9:macOS 14.0, iOS 17.0, watchOS 10.0, tvOS 17.0
      SwiftStdlib 5.10:macOS 14.4, iOS 17.4, watchOS 10.4, tvOS 17.4, visionOS 1.1
      SwiftStdlib 6.0:macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0
      SwiftStdlib 6.1:macOS 15.4, iOS 18.4, watchOS 11.4, tvOS 18.4, visionOS 2.4
      SwiftStdlib 6.2:macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0
      SwiftStdlib 6.3:macOS 26.4, iOS 26.4, watchOS 26.4, tvOS 26.4, visionOS 26.4
      SwiftStdlib 6.4:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999
      SwiftCompatibilitySpan 5.0:macOS 10.14.4, iOS 12.2, watchOS 5.2, tvOS 12.2, visionOS 1.0
      SwiftCompatibilitySpan 6.2:macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionBacktracing 6.2: macOS 26.0
      """.split(separator: "\n")
    #endif
    let foundMacros = allLines.compactMap { (line) -> String? in
      if line.contains("#") { return nil }

      guard let colonIndex = line.firstIndex(of: ":") else {
        return nil
      }

      // Under embedded swift, all stdlib APIs should be available always.
      // Replace all availability macros with very very old OS version.      
//      return String(line[..<colonIndex]) + ":macOS 10.14, iOS 12.0, watchOS 7.0, tvOS 12.0, visionOS 1.0"
      return String(line[..<colonIndex]) + ":*"
    }

    // Create "StdlibDeploymentTarget" counterparts for "SwiftStdlib"
    // availability macros.
    return foundMacros + foundMacros.compactMap { macro in
      if !macro.contains("SwiftStdlib") {
        return nil
      }

      return macro.replacingOccurrences(of: "SwiftStdlib", with: "StdlibDeploymentTarget")
    }
  }
}
