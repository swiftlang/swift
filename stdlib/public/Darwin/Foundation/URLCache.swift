//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

extension URLCache {
  @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
  public convenience init(memoryCapacity: Int, diskCapacity: Int, directory: URL? = nil) {
    self.init(__memoryCapacity: memoryCapacity, diskCapacity: diskCapacity, directoryURL: directory)
  }
}
