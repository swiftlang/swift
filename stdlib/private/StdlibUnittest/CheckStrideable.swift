//===----------------------------------------------------------------------===//
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

public func checkStrideable<S : Strideable>(
  instances: [S],
  distances: [S.Stride],
  distanceOracle: (Int, Int) -> S.Stride,

  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  for i in instances.indices {
    let first = instances[i]
    for j in instances.indices {
      let second = instances[j]
      expectEqual(distanceOracle(i, j), first.distance(to: second))
      expectEqual(second, first.advanced(by: distanceOracle(i, j)))
    }
  }
}

