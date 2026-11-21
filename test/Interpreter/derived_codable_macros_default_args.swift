// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out -module-name main -Xfrontend -enable-experimental-feature -Xfrontend DeriveConformancesViaMacros -Xfrontend -load-plugin-library -Xfrontend %swift-plugin-dir/%target-library-name(SwiftMacros)
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_DeriveConformancesViaMacros

import Foundation

enum Unlabeled: Codable, Equatable {
  case foo(_ a: Int = 1, _ b: Int)
  enum CodingKeys: CodingKey { case foo }
  enum FooCodingKeys: CodingKey { case _1 }
}

enum Labeled: Codable, Equatable {
  case foo(a: Int = 1, b: Int)
  enum CodingKeys: CodingKey { case foo }
  enum FooCodingKeys: CodingKey { case b }
}

func roundTrip<T: Codable>(_ value: T) throws {
  let data = try JSONEncoder().encode(value)
  print(String(decoding: data, as: UTF8.self))
  print(try JSONDecoder().decode(T.self, from: data))
}

// CHECK: {"foo":{"_1":3}}
// CHECK-NEXT: foo(1, 3)
try roundTrip(Unlabeled.foo(2, 3))

// CHECK-NEXT: {"foo":{"b":3}}
// CHECK-NEXT: foo(a: 1, b: 3)
try roundTrip(Labeled.foo(a: 2, b: 3))
