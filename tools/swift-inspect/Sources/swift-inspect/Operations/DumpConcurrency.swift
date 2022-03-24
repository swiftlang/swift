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

#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)

import ArgumentParser
import SwiftRemoteMirror
import libswiftInspect

public struct DumpConcurrency: ParsableCommand {
  public static let configuration = CommandConfiguration(
    abstract: "Print information about the target's concurrency runtime.")

  @OptionGroup()
  var options: UniversalOptions

  public init() {}

  public func run() throws {
    try inspect(options: options) { process in
      let dumper = ConcurrencyDumper(context: process.context,
                                     process: process as! DarwinRemoteProcess,
                                     printer: { Swift.print($0) })
      dumper.dumpTasks()
      dumper.dumpActors()
      dumper.dumpThreads()
    }
  }
}

#endif
