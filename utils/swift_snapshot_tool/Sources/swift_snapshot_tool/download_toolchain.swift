//===--- download_toolchain.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation

private let SWIFT_BASE_URL = "https://swift.org/builds"

private func fileExists(_ path: String) -> Bool {
  FileManager.default.fileExists(atPath: path)
}

enum DownloadFileError: Error {
  case failedToGetURL
}

private func downloadFile(url: URL, localPath: URL, verboseDownload: Bool = true) async throws {
  let config = URLSessionConfiguration.`default`
  let session = URLSession(configuration: config)
  return try await withCheckedThrowingContinuation { continuation in
    let task = session.downloadTask(with: url) {
      urlOrNil, responseOrNil, errorOrNil in
      // check for and handle errors:
      // * errorOrNil should be nil
      // * responseOrNil should be an HTTPURLResponse with statusCode in 200..<299

      guard let fileURL = urlOrNil else {
        continuation.resume(throwing: DownloadFileError.failedToGetURL)
        return
      }

      do {
        // Remove it if it is already there. Swallow the error if it is not
        // there.
        try FileManager.default.removeItem(at: localPath)
      } catch {}

      do {
        // Then move it... not swalling any errors.
        try FileManager.default.moveItem(at: fileURL, to: localPath)
      } catch let e {
        continuation.resume(throwing: e)
        return
      }

      continuation.resume()
    }

    task.resume()
    while task.state != URLSessionTask.State.completed {
      sleep(1)
      if verboseDownload {
        let percent = String(
          format: "%.2f",
          Float(task.countOfBytesReceived) / Float(task.countOfBytesExpectedToReceive) * 100)
        log("\(percent)%. Bytes \(task.countOfBytesReceived)/\(task.countOfBytesExpectedToReceive)")
      }
    }
  }
}

private func shell(_ command: String, environment: [String: String] = [:],
                   mustSucceed: Bool = true,verbose: Bool = false,
                   extraArgs: [String] = []) -> (
  stdout: String, stderr: String, exitCode: Int
) {
  let task = Process()
  let stdout = Pipe()
  let stderr = Pipe()

  task.standardOutput = stdout
  task.standardError = stderr
  var newCommand = command
  if extraArgs.count != 0 {
    newCommand = newCommand.appending(" ").appending(extraArgs.joined(separator: " "))
  }
  task.arguments = ["-c", newCommand]
  if !environment.isEmpty {
    if let e = task.environment {
      log("Task Env: \(e)")
      log("Passed in Env: \(environment)")
      task.environment = e.merging(
        environment,
        uniquingKeysWith: {
          (current, _) in current
        })
    } else {
      task.environment = environment
    }
  }
  if verbose {
    log("Command: \(command)\n")
  }
  task.launchPath = "/bin/zsh"
  task.standardInput = nil
  task.launch()
  task.waitUntilExit()

  let stderrData = String(
    decoding: stderr.fileHandleForReading.readDataToEndOfFile(), as: UTF8.self)
  let stdoutData = String(
    decoding: stdout.fileHandleForReading.readDataToEndOfFile(), as: UTF8.self)
  if verbose {
    log("StdErr:\n\(stderrData)\n")
    log("StdOut:\n\(stdoutData)\n")
  }
  if mustSucceed && task.terminationStatus != 0 {
    log("Command Failed!")
    fatalError()
  }
  return (stdout: stdoutData, stderr: stderrData, Int(task.terminationStatus))
}

func downloadToolchainAndRunTest(
  platform: Platform, tag: Tag, branch: Branch,
  workspace: String, script: String,
  extraArgs: [String],
  verbose: Bool = false
) async throws -> Int {
  let fileType = platform.fileType
  let toolchainType = platform.toolchainType

  let realBranch = branch != .development ? "swift-\(branch)-branch" : branch.rawValue
  let toolchainDir = "\(workspace)/\(tag.name)-\(platform)"
  let downloadPath = URL(fileURLWithPath: "\(workspace)/\(tag.name)-\(platform).\(fileType)")
  if !fileExists(toolchainDir) {
    let downloadURL = URL(
      string:
        "\(SWIFT_BASE_URL)/\(realBranch)/\(toolchainType)/\(tag.name)/\(tag.name)-\(platform).\(fileType)"
    )!
    log("[INFO] Starting Download: \(downloadURL) -> \(downloadPath)")
    try await downloadFile(url: downloadURL, localPath: downloadPath)
    log("[INFO] Finished Download: \(downloadURL) -> \(downloadPath)")
  } else {
    log("[INFO] File exists! No need to download! Path: \(downloadPath)")
  }

  switch platform {
  case .osx:
    if !fileExists(toolchainDir) {
      log("[INFO] Installing: \(downloadPath)")
      _ = shell("pkgutil --expand \(downloadPath.path) \(toolchainDir)")
      let payloadPath = "\(toolchainDir)/\(tag.name)-osx-package.pkg/Payload"
      _ = shell("tar -xf \(payloadPath) -C \(toolchainDir)")
    } else {
      log("[INFO] No need to install: \(downloadPath)")
    }

    let swiftcPath = "\(toolchainDir)/usr/bin/swiftc"
    let swiftFrontendPath = "\(toolchainDir)/usr/bin/swift-frontend"
    // Just for now just support macosx.
    let platform = "macosx"
    let swiftLibraryPath = "\(toolchainDir)/usr/lib/swift/\(platform)"
    log(shell("\(swiftcPath) --version").stdout)
    let exitCode = shell(
      "\(script)", environment: ["SWIFTC": swiftcPath, "SWIFT_FRONTEND": swiftFrontendPath,
        "SWIFT_LIBRARY_PATH": swiftLibraryPath],
      mustSucceed: false,
      verbose: verbose,
      extraArgs: extraArgs
    ).exitCode
    log("[INFO] Exit code: \(exitCode). Tag: \(tag.name). Script: \(script)")
    return exitCode
  case .ubuntu1404, .ubuntu1604, .ubuntu1804:
    fatalError("Unsupported platform: \(platform)")
  }
}
