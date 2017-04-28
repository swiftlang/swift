// REQUIRES: objc_interop
// RUN: rm -rf %t.mod && mkdir -p %t.mod
// RUN: %swift -emit-module -o %t.mod/cities.swiftmodule %S/Inputs/cities.swift -module-name Cities -parse-as-library
// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -update-code -disable-migrator-fixits -primary-file %s  -I %t.mod -api-diff-data-file %S/API.json -emit-migrated-file-path %t/wrap_optional.swift.result -o %t/wrap_optional.swift.remap
// RUN: diff -u %S/wrap_optional.swift.expected %t/wrap_optional.swift.result

import Cities

class MyCities : Cities {
  override init(x: Int) { super.init(x: x) }
  override init!(y: Int) { super.init(y: y) }
  override func mooloolaba(x: Cities, y: Cities?) {}
  override func toowoomba(x: [Cities], y: [Cities]?) {}
  override func mareeba(x: [String : Cities?], y: [String : Cities]?) {}
}

class MySubCities : MyCities {}

class MySubSubCities : MySubCities {
  override func yandina(x: [[String : Cities]]!) {}
  override func buderim() -> Cities? { return Cities(x: 1) }
  override func noosa() -> [[String : Cities]?] { return [] }
}

typealias IntPair = (Int, Int)

extension ExtraCities {
  func coolum(x: [String : [Int : [(((String))?)]]]) {}
  func currimundi(x: (Int, IntPair)!) {}
}

class MyExtraCities : ExtraCities {
  func blibli(x: (String?, String) -> String!) {}
  func currimundi(x: (Int, (Int, Int))!) {}
}
