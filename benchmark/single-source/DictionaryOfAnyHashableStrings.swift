//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

// This benchmark tests the performance of Dictionary<AnyHashable, Any> with
// small ASCII String keys. Untyped NSDictionary values get imported as this
// type, so it occurs relatively often in practice.

public var DictionaryOfAnyHashableStrings = [
  BenchmarkInfo(
    name: "DictionaryOfAnyHashableStrings_insert",
    runFunction: run_DictionaryOfAnyHashableStrings_insert,
    tags: [.abstraction, .runtime, .cpubench],
    setUpFunction: {
      keys = buildKeys(500)
    }
  ),
  BenchmarkInfo(
    name: "DictionaryOfAnyHashableStrings_lookup",
    runFunction: run_DictionaryOfAnyHashableStrings_lookup,
    tags: [.abstraction, .runtime, .cpubench],
    setUpFunction: {
      keys = buildKeys(500)
      workload = buildWorkload()
    }
  ),
]

var keys: [String] = []
var workload: [AnyHashable: Any] = [:]

func buildKeys(_ size: Int) -> [String] {
  var result: [String] = []
  let keyPrefixes = ["font", "bgcolor", "fgcolor", "blink", "marquee"]
  for key in keyPrefixes {
    for i in 0 ..< size {
      result.append(key + "\(i)")
    }
  }
  return result
}

func buildWorkload() -> [AnyHashable: Any] {
  precondition(keys.count > 0)
  var result: [AnyHashable: Any] = [:]
  var i = 0
  for key in keys {
    result[key] = i
    i += 1
  }
  return result
}


@inline(never)
public func run_DictionaryOfAnyHashableStrings_insert(_ n: Int) {
  precondition(keys.count > 0)
  for _ in 0 ... n {
    blackHole(buildWorkload())
  }
}

@inline(never)
public func run_DictionaryOfAnyHashableStrings_lookup(_ n: Int) {
  precondition(workload.count > 0)
  precondition(keys.count > 0)
  for _ in 0 ... n {
    for i in 0 ..< keys.count {
      let key = keys[i]
      CheckResults((workload[key] as! Int) == i)
    }
  }
}
