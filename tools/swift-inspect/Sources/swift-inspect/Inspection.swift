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

import libswiftInspect

internal func inspect(options: UniversalOptions,
                      _ body: (any RemoteProcess) throws -> Void) throws {
  var inspectOptions = InspectOptions()

  inspectOptions.nameOrPid = options.nameOrPid
#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)
  inspectOptions.forkCorpse = options.forkCorpse
#endif

  try inspect(options: inspectOptions, body)
}
