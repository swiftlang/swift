//===------- SwiftcInvocation.swift - Utilities for invoking swiftc -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This file provides the logic for invoking swiftc to parse Swift files.
//===----------------------------------------------------------------------===//

import Foundation

#if os(macOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

/// The result of process execution, containing the exit status code,
/// stdout, and stderr
struct ProcessResult {
  /// The process exit code. A non-zero exit code usually indicates failure.
  let exitCode: Int

  /// The contents of the process's stdout as Data. 
  let stdoutData: Data

  /// The contents of the process's stderr as Data.
  let stderrData: Data

  /// The contents of the process's stdout, assuming the data was UTF-8 encoded.
  var stdout: String {
    return String(data: stdoutData, encoding: .utf8)!
  }

  /// The contents of the process's stderr, assuming the data was UTF-8 encoded.
  var stderr: String {
    return String(data: stderrData, encoding: .utf8)!
  }

  /// Whether or not this process had a non-zero exit code.
  var wasSuccessful: Bool {
    return exitCode == 0
  }
}

/// Runs the provided executable with the provided arguments and returns the
/// contents of stdout and stderr as Data.
/// - Parameters:
///   - executable: The full file URL to the executable you're running.
///   - arguments: A list of strings to pass to the process as arguments.
/// - Returns: A ProcessResult containing stdout, stderr, and the exit code.
func run(_ executable: URL, arguments: [String] = []) -> ProcessResult {
  // Use an autoreleasepool to prevent memory- and file-descriptor leaks.
  return autoreleasepool {
    () -> ProcessResult in
    
    let stdoutPipe = Pipe()
    var stdoutData = Data()
    stdoutPipe.fileHandleForReading.readabilityHandler = { file in
      stdoutData.append(file.availableData)
    }
    
    let stderrPipe = Pipe()
    var stderrData = Data()
    stderrPipe.fileHandleForReading.readabilityHandler = { file in
      stderrData.append(file.availableData)
    }
    
    let process = Process()
    
    process.terminationHandler = { process in
      stdoutPipe.fileHandleForReading.readabilityHandler = nil
      stderrPipe.fileHandleForReading.readabilityHandler = nil
    }
    
    process.launchPath = executable.path
    process.arguments = arguments
    process.standardOutput = stdoutPipe
    process.standardError = stderrPipe
    process.launch()
    process.waitUntilExit()
    return ProcessResult(exitCode: Int(process.terminationStatus),
                         stdoutData: stdoutData,
                         stderrData: stderrData)
  }
}

/// Finds the dylib or executable which the provided address falls in.
/// - Parameter dsohandle: A pointer to a symbol in the object file you're
///                        looking for. If not provided, defaults to the
///                        caller's `#dsohandle`, which will give you the
///                        object file the caller resides in.
/// - Returns: A File URL pointing to the object where the provided address
///            resides. This may be a dylib, shared object, static library,
///            or executable. If unable to find the appropriate object, returns
///            `nil`.
func findFirstObjectFile(for dsohandle: UnsafeRawPointer = #dsohandle) -> URL? {
  var info = dl_info()
  if dladdr(dsohandle, &info) == 0 {
    return nil
  }
  let path = String(cString: info.dli_fname)
  return URL(fileURLWithPath: path)
}

enum InvocationError: Error, CustomStringConvertible {
  case couldNotFindSwiftc
  case couldNotFindSDK

  var description: String {
    switch self {
    case .couldNotFindSwiftc:
      return "could not locate swift compiler binary"
    case .couldNotFindSDK:
      return "could not locate macOS SDK"
    }
  }
}

struct SwiftcRunner {
  /// Gets the `swiftc` binary packaged alongside this library.
  /// - Returns: The path to `swiftc` relative to the path of this library
  ///            file, or `nil` if it could not be found.
  /// - Note: This makes assumptions about your Swift installation directory
  ///         structure. Importantly, it assumes that the directory tree is
  ///         shaped like this:
  ///         ```
  ///         install_root/
  ///           - bin/
  ///             - swiftc
  ///           - lib/
  ///             - swift/
  ///               - ${target}/
  ///                 - libswiftSwiftSyntax.[dylib|so]
  ///         ```
  static func locateSwiftc() -> URL? {
    guard let libraryPath = findFirstObjectFile() else { return nil }
    let swiftcURL = libraryPath.deletingLastPathComponent()
                               .deletingLastPathComponent()
                               .deletingLastPathComponent()
                               .deletingLastPathComponent()
                               .appendingPathComponent("bin")
                               .appendingPathComponent("swiftc")
    guard FileManager.default.fileExists(atPath: swiftcURL.path) else {
      return nil
    }
    return swiftcURL
  }

#if os(macOS)
  /// The location of the macOS SDK, or `nil` if it could not be found.
  static let macOSSDK: String? = {
    let url = URL(fileURLWithPath: "/usr/bin/env")
    let result = run(url, arguments: ["xcrun", "--show-sdk-path"])
    guard result.wasSuccessful else { return nil }
    let toolPath = result.stdout.trimmingCharacters(in: .whitespacesAndNewlines)
    if toolPath.isEmpty { return nil }
    return toolPath
  }()
#endif

  /// Internal static cache of the Swiftc path.
  static let _swiftcURL: URL? = SwiftcRunner.locateSwiftc()

  /// The URL where the `swiftc` binary lies.
  let swiftcURL: URL

  /// The source file being parsed.
  let sourceFile: URL

  /// Creates a SwiftcRunner that will parse and emit the syntax
  /// tree for a provided source file.
  /// - Parameter sourceFile: The URL to the source file you're trying
  ///                         to parse.
  init(sourceFile: URL) throws {
    guard let url = SwiftcRunner._swiftcURL else {
      throw InvocationError.couldNotFindSwiftc
    }
    self.swiftcURL = url
    self.sourceFile = sourceFile
  }

  /// Invokes swiftc with the provided arguments.
  func invoke() throws -> ProcessResult {
    var arguments = ["-frontend", "-emit-syntax"]
    arguments.append(sourceFile.path)
#if os(macOS)
    guard let sdk = SwiftcRunner.macOSSDK else {
      throw InvocationError.couldNotFindSDK
    }
    arguments.append("-sdk")
    arguments.append(sdk)
#endif
    return run(swiftcURL, arguments: arguments)
  }
}
