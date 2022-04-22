//===--- NSStringConversion.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// <rdar://problem/19003201>
#if canImport(Darwin)

import TestsUtils
import Foundation

fileprivate var test:NSString = ""
fileprivate var mutableTest = ""

public let benchmarks = [
  BenchmarkInfo(name: "NSStringConversion",
                runFunction: run_NSStringConversion,
                tags: [.validation, .api, .String, .bridging]),
  BenchmarkInfo(name: "NSStringConversion.UTF8",
                runFunction: run_NSStringConversion_nonASCII,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "tëst", encoding: String.Encoding.utf8.rawValue)! }),
  BenchmarkInfo(name: "NSStringConversion.Mutable",
                runFunction: run_NSMutableStringConversion,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSMutableString(cString: "test", encoding: String.Encoding.ascii.rawValue)! }),
  BenchmarkInfo(name: "NSStringConversion.Medium",
                runFunction: run_NSStringConversion_medium,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "aaaaaaaaaaaaaaa", encoding: String.Encoding.ascii.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.Long",
                runFunction: run_NSStringConversion_long,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "The quick brown fox jumps over the lazy dog", encoding: String.Encoding.ascii.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.LongUTF8",
                runFunction: run_NSStringConversion_longNonASCII,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "Thë qüick bröwn föx jumps over the lazy dög", encoding: String.Encoding.utf8.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.Rebridge",
                runFunction: run_NSStringConversion_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "test", encoding: String.Encoding.ascii.rawValue)! }),
  BenchmarkInfo(name: "NSStringConversion.Rebridge.UTF8",
                runFunction: run_NSStringConversion_nonASCII_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "tëst", encoding: String.Encoding.utf8.rawValue)! }),
  BenchmarkInfo(name: "NSStringConversion.Rebridge.Mutable",
                runFunction: run_NSMutableStringConversion_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSMutableString(cString: "test", encoding: String.Encoding.ascii.rawValue)! }),
  BenchmarkInfo(name: "NSStringConversion.Rebridge.Medium",
                runFunction: run_NSStringConversion_medium_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "aaaaaaaaaaaaaaa", encoding: String.Encoding.ascii.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.Rebridge.Long",
                runFunction: run_NSStringConversion_long_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "The quick brown fox jumps over the lazy dog", encoding: String.Encoding.ascii.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.Rebridge.LongUTF8",
                runFunction: run_NSStringConversion_longNonASCII_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "Thë qüick bröwn föx jumps over the lazy dög", encoding: String.Encoding.utf8.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.MutableCopy.UTF8",
                runFunction: run_NSStringConversion_nonASCIIMutable,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { mutableTest = "tëst" }),
  BenchmarkInfo(name: "NSStringConversion.MutableCopy.Medium",
                runFunction: run_NSStringConversion_mediumMutable,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { mutableTest = "aaaaaaaaaaaaaaa" } ),
  BenchmarkInfo(name: "NSStringConversion.MutableCopy.Long",
                runFunction: run_NSStringConversion_longMutable,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { mutableTest = "The quick brown fox jumps over the lazy dog" } ),
  BenchmarkInfo(name: "NSStringConversion.MutableCopy.LongUTF8",
                runFunction: run_NSStringConversion_longNonASCIIMutable,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { mutableTest = "Thë qüick bröwn föx jumps over the lazy dög" } ),
  BenchmarkInfo(name: "NSStringConversion.MutableCopy.Rebridge",
                runFunction: run_NSStringConversion_rebridgeMutable,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { mutableTest = "test" }),
  BenchmarkInfo(name: "NSStringConversion.MutableCopy.Rebridge.UTF8",
                runFunction: run_NSStringConversion_nonASCII_rebridgeMutable,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { mutableTest = "tëst" }),
  BenchmarkInfo(name: "NSStringConversion.MutableCopy.Rebridge.Medium",
                runFunction: run_NSStringConversion_medium_rebridgeMutable,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { mutableTest = "aaaaaaaaaaaaaaa" } ),
  BenchmarkInfo(name: "NSStringConversion.MutableCopy.Rebridge.Long",
                runFunction: run_NSStringConversion_long_rebridgeMutable,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { mutableTest = "The quick brown fox jumps over the lazy dog" } ),
  BenchmarkInfo(name: "NSStringConversion.MutableCopy.Rebridge.LongUTF8",
                runFunction: run_NSStringConversion_longNonASCII_rebridgeMutable,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { mutableTest = "Thë qüick bröwn föx jumps over the lazy dög" } ),
  BenchmarkInfo(name: "NSStringConversion.InlineBuffer.UTF8",
                runFunction: run_NSStringConversion_inlineBuffer,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: {
                  mutableTest = Array(repeating: "제10회 유니코드 국제 회의가 1997년 3월 10일부터 12일까지 독일의 마인즈에서 열립니다. 지금 등록하십시오. 이 회의에서는 업계 전반의 전문가들이 함께 모여 다음과 같은 분야를 다룹니다. - 인터넷과 유니코드, 국제화와 지역화, 운영 체제와 응용 프로그램에서 유니코드의 구현, 글꼴, 문자 배열, 다국어 컴퓨팅.세계를 향한 대화, 유니코드로 하십시오. 제10회 유니코드 국제 회의가 1997년 3월 10일부터 12일까지 독일의 마인즈에서 열립니다. 지금 등록하십시오. 이 회의에서는 업계 전반의 전문가들이 함께 모여 다음과 같은 분야를 다룹니다. 세계를 향한 대화, 유니코드로 하십시오.", count: 256).joined(separator: "")
                }
               ),
  BenchmarkInfo(name: "NSStringConversion.InlineBuffer.ASCII",
                runFunction: run_NSStringConversion_inlineBuffer,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: {
                  mutableTest = Array(repeating: "The 10th Unicode International Conference will be held from March 10 to 12, 1997 in Mainz, Germany. Register now. The conference brings together experts from across the industry, covering areas such as: - Internet and Unicode, internationalization and localization, implementation of Unicode in operating systems and applications, fonts, character arrays, multilingual computing. Dialogue to the world, do Unicode. The 10th Unicode International Conference will be held from March 10 to 12, 1997 in Mainz, Germany. Register now. The conference brings together experts from across the industry, covering areas such as: Dialogue to the world, do it in Unicode.", count: 256).joined(separator: "")
                }
               )
]

public func run_NSStringConversion(_ n: Int) {
let test:NSString = NSString(cString: "test", encoding: String.Encoding.ascii.rawValue)!
  for _ in 1...n * 10000 {
    //Doesn't test accessing the String contents to avoid changing historical benchmark numbers
    blackHole(identity(test) as String)
  }
}

fileprivate func innerLoop(_ str: NSString, _ n: Int, _ scale: Int = 5000) {
  for _ in 1...n * scale {
    for char in (identity(str) as String).utf8 {
      blackHole(char)
    }
  }
}

public func run_NSStringConversion_nonASCII(_ n: Int) {
  innerLoop(test, n, 2500)
}

public func run_NSMutableStringConversion(_ n: Int) {
  innerLoop(test, n)
}

public func run_NSStringConversion_medium(_ n: Int) {
  innerLoop(test, n, 1000)
}

public func run_NSStringConversion_long(_ n: Int) {
  innerLoop(test, n, 1000)
}

public func run_NSStringConversion_longNonASCII(_ n: Int) {
  innerLoop(test, n, 300)
}

fileprivate func innerMutableLoop(_ str: String, _ n: Int, _ scale: Int = 5000) {
  for _ in 1...n * scale {
    let copy = (str as NSString).mutableCopy() as! NSMutableString
    for char in (identity(copy) as String).utf8 {
      blackHole(char)
    }
  }
}

public func run_NSStringConversion_nonASCIIMutable(_ n: Int) {
  innerMutableLoop(mutableTest, n, 500)
}

public func run_NSStringConversion_mediumMutable(_ n: Int) {
  innerMutableLoop(mutableTest, n, 500)
}

public func run_NSStringConversion_longMutable(_ n: Int) {
  innerMutableLoop(mutableTest, n, 250)
}

public func run_NSStringConversion_longNonASCIIMutable(_ n: Int) {
  innerMutableLoop(mutableTest, n, 150)
}

fileprivate func innerRebridge(_ str: NSString, _ n: Int, _ scale: Int = 5000) {
  for _ in 1...n * scale {
    let bridged = identity(str) as String
    blackHole(bridged)
    blackHole(bridged as NSString)
  }
}

public func run_NSStringConversion_rebridge(_ n: Int) {
  innerRebridge(test, n, 2500)
}

public func run_NSStringConversion_nonASCII_rebridge(_ n: Int) {
  innerRebridge(test, n, 2500)
}

public func run_NSMutableStringConversion_rebridge(_ n: Int) {
  innerRebridge(test, n)
}

public func run_NSStringConversion_medium_rebridge(_ n: Int) {
  innerRebridge(test, n, 1000)
}

public func run_NSStringConversion_long_rebridge(_ n: Int) {
  innerRebridge(test, n, 1000)
}

public func run_NSStringConversion_longNonASCII_rebridge(_ n: Int) {
  innerRebridge(test, n, 300)
}

fileprivate func innerMutableRebridge(_ str: String, _ n: Int, _ scale: Int = 5000) {
  for _ in 1...n * scale {
    let copy = (str as NSString).mutableCopy() as! NSMutableString
    let bridged = identity(copy) as String
    blackHole(bridged)
    blackHole(bridged as NSString)
  }
}

public func run_NSStringConversion_rebridgeMutable(_ n: Int) {
  innerMutableRebridge(mutableTest, n, 1000)
}

public func run_NSStringConversion_nonASCII_rebridgeMutable(_ n: Int) {
  innerMutableRebridge(mutableTest, n, 500)
}

public func run_NSStringConversion_medium_rebridgeMutable(_ n: Int) {
  innerMutableRebridge(mutableTest, n, 500)
}

public func run_NSStringConversion_long_rebridgeMutable(_ n: Int) {
  innerMutableRebridge(mutableTest, n, 500)
}

public func run_NSStringConversion_longNonASCII_rebridgeMutable(_ n: Int) {
  innerMutableRebridge(mutableTest, n, 300)
}

public func run_NSStringConversion_inlineBuffer(_ n: Int) {
  withUnsafeTemporaryAllocation(
    of: CFStringInlineBuffer.self,
    capacity: 1
  ) { bufferPtr in
    let buffer = bufferPtr.baseAddress!
    for _ in 0 ..< n {
      var result = UInt64(0)
      let bridged = mutableTest as NSString as CFString
      let len = CFStringGetLength(bridged)
      CFStringInitInlineBuffer(bridged, buffer, CFRangeMake(0, len))
      for i in 0 ..< CFStringGetLength(bridged) {
        let char = CFStringGetCharacterFromInlineBuffer(buffer, i)
        result += UInt64(char)
      }
      blackHole(result)
    }
  }
}

#endif
