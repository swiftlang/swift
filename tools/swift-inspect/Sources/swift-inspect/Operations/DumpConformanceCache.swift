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

internal struct DumpConformanceCache: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print the contents of the target's protocol conformance cache.")

  @OptionGroup()
  var options: UniversalOptions

  func run() throws {
    try inspect(options: options) { process in
      try process.context.iterateConformanceCache { type, proto in
        let type: String = process.context.name(type: type) ?? "<unknown>"
        let conformance: String = process.context.name(protocol: proto) ?? "<unknown>"
        print("Conformance: \(type): \(conformance)")
      }
    }
  }
}
