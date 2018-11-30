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

#if _runtime(_ObjC)
public let Codable = [
  BenchmarkInfo(name: "JSONPerfEncode", runFunction: run_JSONPerfEncode, tags: [.validation, .bridging], setUpFunction: setup_json),
  BenchmarkInfo(name: "JSONPerfDecode", runFunction: run_JSONPerfDecode, tags: [.validation, .bridging], setUpFunction: setup_json),
  BenchmarkInfo(name: "PlistPerfEncode", runFunction: run_PlistPerfEncode, tags: [.validation, .bridging], setUpFunction: setup_plist),
  BenchmarkInfo(name: "PlistPerfDecode", runFunction: run_PlistPerfDecode, tags: [.validation, .bridging], setUpFunction: setup_plist),
]
#else
public let Codable = [
  BenchmarkInfo(name: "JSONPerfEncode", runFunction: run_JSONPerfEncode, tags: [.validation, .bridging], setUpFunction: setup_json),
  BenchmarkInfo(name: "JSONPerfDecode", runFunction: run_JSONPerfDecode, tags: [.validation, .bridging], setUpFunction: setup_json),
]
#endif



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

protocol GenericEncoder {
    func encode<T : Encodable>(_ value: T) throws -> Data
}

protocol GenericDecoder {
    func decode<T : Decodable>(_ type: T.Type, from data: Data) throws -> T
}

extension JSONEncoder : GenericEncoder {}
extension JSONDecoder : GenericDecoder {}
#if _runtime(_ObjC)
extension PropertyListEncoder : GenericEncoder {}
extension PropertyListDecoder : GenericDecoder {}
#endif

struct CodablePerfTester<Enc: GenericEncoder, Dec: GenericDecoder>  {
  var poems = Array(repeating: Jabberwocky(), count: 6)
  var encoder: Enc
  var decoder: Dec
  var data: Data! = nil
  
  init(encoder e: Enc, decoder d: Dec) {
    encoder = e
    decoder = d
    data = try! encoder.encode(Array(poems.prefix(3)))
    //pre-warm everything to try to reduce variance
    let _ = try! encoder.encode(poems)
    let _ = try! decoder.decode(Array<Jabberwocky>.self,
                                       from: data)
  }
  
  func encode() {
    let _ = try! encoder.encode(poems)
  }
  
  func decode() {
    let _ = try! decoder.decode(Array<Jabberwocky>.self, from: data)
  }
}

var JSONTester: CodablePerfTester<JSONEncoder, JSONDecoder>! = nil

public func setup_json() {
  JSONTester = CodablePerfTester(encoder: JSONEncoder(), decoder: JSONDecoder())
}

@inline(never)
public func run_JSONPerfEncode(_ N: Int) {
  for _ in 0 ..< N {
    JSONTester.encode()
  }
}

@inline(never)
public func run_JSONPerfDecode(_ N: Int) {
  for _ in 0 ..< N {
    JSONTester.decode()
  }
}

#if _runtime(_ObjC)

var plistTester: CodablePerfTester<PropertyListEncoder, PropertyListDecoder>! = nil

public func setup_plist() {
  plistTester = CodablePerfTester(encoder: PropertyListEncoder(), decoder: PropertyListDecoder())
}

@inline(never)
public func run_PlistPerfEncode(_ N: Int) {
  for _ in 0 ..< N {
    plistTester.encode()
  }
}

@inline(never)
public func run_PlistPerfDecode(_ N: Int) {
  for _ in 0 ..< N {
    plistTester.decode()
  }
}

#endif
