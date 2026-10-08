// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -strict-memory-safety -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-UNSAFE

// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-SAFE

// REQUIRES: swift_feature_DeriveConformancesViaMacros

struct SimpleStruct: Codable {
  var x: Int
}

// CHECK: init(from decoder: any Swift::Decoder) throws {
// CHECK:   let container = try decoder.container(keyedBy: Self.CodingKeys.self)
// CHECK-UNSAFE:   unsafe self.x = try container.decode(Int.self, forKey: .x)
// CHECK-SAFE:     self.x = try container.decode(Int.self, forKey: .x)
// CHECK: }

// CHECK: func encode(to encoder: any Swift::Encoder) throws {
// CHECK:   var container = encoder.container(keyedBy: Self.CodingKeys.self)
// CHECK-UNSAFE:   try unsafe container.encode(self.x, forKey: .x)
// CHECK-SAFE:     try container.encode(self.x, forKey: .x)
// CHECK: }

enum SimpleEnum: Codable {
  case a(Int)
}

// CHECK: init(from decoder: any Swift::Decoder) throws {
// CHECK:   switch onlyKey {
// CHECK:   case .a:
// CHECK:     let nestedContainer = try container.nestedContainer(keyedBy: Self.ACodingKeys.self, forKey: .a)
// CHECK-UNSAFE:     unsafe self = .a(try nestedContainer.decode(Int.self, forKey: ._0))
// CHECK-SAFE:       self = .a(try nestedContainer.decode(Int.self, forKey: ._0))
// CHECK:   }
// CHECK: }

// CHECK: func encode(to encoder: any Swift::Encoder) throws {
// CHECK:   var container = encoder.container(keyedBy: Self.CodingKeys.self)
// CHECK-UNSAFE: switch unsafe self {
// CHECK-SAFE:    switch self {
// CHECK:   case .a(let a0):
// CHECK:     var nestedContainer = container.nestedContainer(keyedBy: Self.ACodingKeys.self, forKey: .a)
// CHECK-UNSAFE:     try unsafe nestedContainer.encode(a0, forKey: ._0)
// CHECK-SAFE:       try nestedContainer.encode(a0, forKey: ._0)
// CHECK:   }
// CHECK: }
