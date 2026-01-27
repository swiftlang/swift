//===--- CrashInfo.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// The crash information extracted from one or more crash logs.
struct CrashInfo {
  let primary: CrashLog
  private(set) var bySig: [Signature: CrashLog] = [:]
  private(set) var signatures: KnownSignatures

  var primarySig: Signature {
    primary.signature
  }

  init(_ crashLog: CrashLog) {
    self.primary = crashLog
    self.signatures = KnownSignatures(crashLog.signature)
    self.bySig = [crashLog.signature: crashLog]
  }

  mutating func add(_ crashLog: CrashLog) {
    guard bySig[crashLog.signature] == nil else { return }
    bySig[crashLog.signature] = crashLog
    signatures.add(crashLog.signature)
  }

  mutating func merge(_ other: Self) {
    for crash in other.bySig.values {
      add(crash)
    }
  }
}

/// A set of known signatures for a given crasher.
struct KnownSignatures: Hashable {
  let primary: Signature
  var sigs: Set<Signature> = []

  init(_ sig: Signature) {
    self.primary = sig
    add(sig)
  }

  var secondaries: Set<Signature> {
    sigs.subtracting([primary])
  }

  mutating func add(_ sig: Signature) {
    sigs.insert(sig)
  }

  func contains(_ sig: Signature) -> Bool {
    sigs.contains(sig)
  }

  func isSuperset(of other: KnownSignatures) -> Bool {
    sigs.isSuperset(of: other.sigs)
  }
}

extension KnownSignatures: Codable {
  init(from decoder: any Decoder) throws {
    var container = try decoder.unkeyedContainer()
    self = Self(try container.decode(Signature.self))
    while !container.isAtEnd {
      add(try container.decode(Signature.self))
    }
  }

  func encode(to encoder: any Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(primary)
    for sig in self.sigs.sorted() {
      try container.encode(sig)
    }
  }
}

extension KnownSignatures: CustomStringConvertible {
  var description: String {
    sigs.sorted().map { $0.shortDescription }.joined(separator: ", ")
  }
}
