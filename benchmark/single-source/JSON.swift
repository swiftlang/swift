//===--- JSON.swift -------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
import Foundation

public let JSON = [
  BenchmarkInfo(name: "JSONPerfEncode", runFunction: run_JSONPerfEncode, tags: [.validation, .bridging], setUpFunction: setup_coder),
  BenchmarkInfo(name: "JSONPerfDecode", runFunction: run_JSONPerfDecode, tags: [.validation, .bridging], setUpFunction: setup_coder),
]


struct Tove: Codable {
  var slithy: Bool = true
  var gyreInRadians: Double = 0.3
}

struct Borogove : Codable {
  var mimsyness: Int = Int.max
}

struct Wabe : Codable {
  var toves: [Tove]
  var borogoves: [Borogove]?
  static var canonical: Wabe {
    return Wabe(toves: [Tove(), Tove(), Tove(), Tove(),
                        Tove(slithy: false, gyreInRadians: 1.8)],
                borogoves: [Borogove(mimsyness: 18), Borogove(mimsyness: 74),
                            Borogove(), Borogove()])
  }
}

struct Beast : Codable {
  var name: String
}


struct Jabberwocky : Codable {
  var time = "brillig"
  var wabe = Wabe.canonical
  var beware: [Beast] = [ Beast(name: "Jabberwock"), Beast(name: "Bandersnatch"), Beast(name: "Jubjub Bird")]
  var swordType = "vörpal"
}

struct JSONPerfTester {
  var poems = Array(repeating: Jabberwocky(), count: 6)
  var encoder: JSONEncoder = JSONEncoder()
  var decoder: JSONDecoder = JSONDecoder()
  var data: Data! = nil
  
  init() {
    data = try! encoder.encode(Array(poems.prefix(3)))
    //pre-warm everything to try to reduce variance
    let _ = try! encoder.encode(poems)
    let _ = try! decoder.decode(Array<Jabberwocky>.self,
                                       from: data)
  }
}

var tester: JSONPerfTester! = nil

public func setup_coder() {
  tester = JSONPerfTester()
}

@inline(never)
public func run_JSONPerfEncode(_ N: Int) {
  for _ in 0 ..< N {
    let _ = try! tester.encoder.encode(tester.poems)
  }
}

@inline(never)
public func run_JSONPerfDecode(_ N: Int) {
  for _ in 0 ..< N {
    let _ = try! tester.decoder.decode(Array<Jabberwocky>.self,
                                       from: tester.data)
  }
}
