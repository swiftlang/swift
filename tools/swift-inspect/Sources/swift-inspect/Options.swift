//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ArgumentParser
import libswiftInspect

internal struct UniversalOptions: ParsableArguments {
  @Argument(help: "The pid or partial name of the target process")
  var nameOrPid: String

#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  @Flag(help: ArgumentHelp(
      "Fork a corpse of the target process",
      discussion: "Creates a low-level copy of the target process, allowing " +
                  "the target to immediately resume execution before " +
                  "swift-inspect has completed its work."))
  var forkCorpse: Bool = false
#endif
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
