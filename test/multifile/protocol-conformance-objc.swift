// RUN: %target-swift-frontend -sdk %sdk -emit-ir -primary-file %s %S/Inputs/protocol-conformance-objc-other.swift -module-name test

// REQUIRES: objc_interop

import Foundation

@objc protocol Horse {
  var hindEnd: Int { get set }
}

extension Pony : Horse {}

