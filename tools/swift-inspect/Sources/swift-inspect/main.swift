//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ArgumentParser
import SwiftRemoteMirror
import Foundation


internal struct UniversalOptions: ParsableArguments {
  @Argument(help: "The pid or partial name of the target process")
  var nameOrPid: String?

#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  @Flag(help: ArgumentHelp(
      "Fork a corpse of the target process",
      discussion: "Creates a low-level copy of the target process, allowing " +
                  "the target to immediately resume execution before " +
                  "swift-inspect has completed its work."))
#endif
  var forkCorpse: Bool = false

#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  @Flag(help: "Run on all processes")
#endif
  var all: Bool = false

  mutating func validate() throws {
    if nameOrPid != nil && all || nameOrPid == nil && !all {
      #if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
        throw ValidationError("Please specify partial process name, pid or --all")
      #else
        throw ValidationError("Please specify partial process name or pid")
      #endif
    }
    if all {
      // Fork corpse is enabled if all is specified
      forkCorpse = true
    }
  }
}

internal struct BacktraceOptions: ParsableArguments {
  @Flag(help: "Show the backtrace for each allocation")
  var backtrace: Bool = false

  @Flag(help: "Show a long-form backtrace for each allocation")
  var backtraceLong: Bool = false

  var style: BacktraceStyle? {
    if backtraceLong { return .long }
    if backtrace { return .oneline }
    return nil
  }
}

internal struct MetadataOptions: ParsableArguments {
  @Flag(help: "Output JSON")
  var json: Bool = false

  #if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  @Flag(help: "Print out a summary from all process output")
  #endif
  var summary: Bool = false

  @Option(help: "Output to a file")
  var outputFile: String? = nil
}

internal func inspect(options: UniversalOptions,
                      _ body: (any RemoteProcess) throws -> Void) throws {
  if let nameOrPid = options.nameOrPid {
    guard let processId = process(matching: nameOrPid) else {
      print("No process found matching \(nameOrPid)", to: &Std.err)
      return
    }
    guard let process = getRemoteProcess(processId: processId,
                                         options: options) else {
      print("Failed to create inspector for process id \(processId)", to: &Std.err)
      return
    }
    try body(process)
  }
  else {
#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
    if let processIdentifiers = getAllProcesses(options: options) {
      let totalCount = processIdentifiers.count
      var successfulCount = 0
      for (index, processIdentifier) in processIdentifiers.enumerated() {
        let progress = "[\(successfulCount)/\(index + 1)/\(totalCount)]"
        if let remoteProcess = getRemoteProcess(processId: processIdentifier, options: options) {
          do {
            print(progress, "\(remoteProcess.processName)(\(remoteProcess.processIdentifier))",
              terminator: "", to: &Std.err)
            try body(remoteProcess)
            successfulCount += 1
          } catch {
            print(" - \(error)", terminator: "", to: &Std.err)
          }
          remoteProcess.release() // break retain cycle
        } else {
          print(progress, " - failed to create inspector for process id \(processIdentifier)",
            terminator: "\n", to: &Std.err)
        }
        print("\u{01B}[0K", terminator: "\r", to: &Std.err)
      }
      print("", to: &Std.err)
    } else {
      print("Failed to get list of processes", to: &Std.err)
    }
#endif
  }
}

@main
internal struct SwiftInspect: ParsableCommand {
  // DumpArrays and DumpConcurrency cannot be reliably be ported outside of
  // Darwin due to the need to iterate the heap.
#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  static let subcommands: [ParsableCommand.Type] = [
    DumpConformanceCache.self,
    DumpRawMetadata.self,
    DumpGenericMetadata.self,
    DumpCacheNodes.self,
    DumpArrays.self,
    DumpConcurrency.self,
  ]
#elseif os(Windows) || os(Android)
  static let subcommands: [ParsableCommand.Type] = [
    DumpConformanceCache.self,
    DumpRawMetadata.self,
    DumpGenericMetadata.self,
    DumpCacheNodes.self,
    DumpArrays.self,
  ]
#else
  static let subcommands: [ParsableCommand.Type] = [
    DumpConformanceCache.self,
    DumpRawMetadata.self,
    DumpGenericMetadata.self,
    DumpCacheNodes.self,
  ]
#endif

  static let configuration = CommandConfiguration(
    abstract: "Swift runtime debug tool",
    subcommands: subcommands)
}
