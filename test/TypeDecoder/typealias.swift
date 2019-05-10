// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/typealias -emit-module

// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/typealias -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

// RUN: sed -ne '/\/\/ *DEMANGLE-DECL: /s/\/\/ *DEMANGLE-DECL: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/typealias -decl-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-DECL

typealias Alias = Int

struct Outer {
  typealias Alias = Int

  struct Inner {
    typealias Alias = Int
  }
}

struct GenericOuter<T> {
  typealias Alias = Int

  struct Inner {
    typealias Alias = Int
  }
}

protocol Proto {
  typealias Alias = Int
}

extension Proto {
  typealias OtherAlias = Int
}

extension GenericOuter where T : Proto {
  typealias ConditionalAlias = Int
}

struct Conforms : Proto {}

func blackHole(_: Any...) {}

do {
  let x1: Alias = 0
  let x2: Outer.Alias = 0
  let x3: Outer.Inner.Alias = 0

  blackHole(x1, x2, x3)
}

do {
  let x1: GenericOuter<Int>.Alias = 0
  let x2: GenericOuter<Int>.Inner.Alias = 0

  blackHole(x1, x2)
}

do {
  // Note that the first two are not sugared because of representational issues.
  let x1: Proto.Alias = 0
  let x2: Proto.OtherAlias = 0

  let x3: Conforms.Alias = 0
  let x4: Conforms.OtherAlias = 0

  blackHole(x1, x2, x3, x4)
}

func generic<T : Proto>(_: T) {
  let x1: T.Alias = 0
  let x2: T.OtherAlias = 0

  blackHole(x1, x2)
}

do {
  let x1: GenericOuter<Conforms>.ConditionalAlias = 0

  blackHole(x1)
}

// DEMANGLE-TYPE: $s9typealias5AliasaD
// DEMANGLE-TYPE: $s9typealias5OuterV5AliasaD
// DEMANGLE-TYPE: $s9typealias5OuterV5InnerV5AliasaD

// CHECK-TYPE: Alias
// CHECK-TYPE: Outer.Alias
// CHECK-TYPE: Outer.Inner.Alias

// DEMANGLE-TYPE: $s9typealias12GenericOuterV5AliasaySi_GD
// DEMANGLE-TYPE: $s9typealias12GenericOuterV5InnerV5AliasaySi__GD

// CHECK-TYPE: GenericOuter<Int>.Alias
// CHECK-TYPE: GenericOuter<Int>.Inner.Alias

// DEMANGLE-TYPE: $s9typealias5ProtoP5AliasayAA8ConformsV_GD
// DEMANGLE-TYPE: $s9typealias5ProtoPAAE10OtherAliasayAA8ConformsV_GD

// CHECK-TYPE: Conforms.Alias
// CHECK-TYPE: Conforms.OtherAlias

// DEMANGLE-TYPE: $s9typealias5ProtoP5Aliasayx_GD
// DEMANGLE-TYPE: $s9typealias5ProtoPAAE10OtherAliasayx_GD

// CHECK-TYPE: τ_0_0.Alias
// CHECK-TYPE: τ_0_0.OtherAlias

// DEMANGLE-TYPE: $s9typealias12GenericOuterVA2A5ProtoRzlE16ConditionalAliasayAA8ConformsV_GD

// CHECK-TYPE: GenericOuter<Conforms>.ConditionalAlias


// DEMANGLE-DECL: $s9typealias5Aliasa
// DEMANGLE-DECL: $s9typealias5OuterV5Aliasa
// DEMANGLE-DECL: $s9typealias5OuterV5InnerV5Aliasa
// DEMANGLE-DECL: $s9typealias12GenericOuterV5Aliasa
// DEMANGLE-DECL: $s9typealias12GenericOuterV5InnerV5Aliasa
// DEMANGLE-DECL: $s9typealias5ProtoP5Aliasa
// DEMANGLE-DECL: $s9typealias5ProtoPAAE10OtherAliasa
// DEMANGLE-DECL: $s9typealias12GenericOuterVA2A5ProtoRzlE16ConditionalAliasa

// CHECK-DECL: typealias.(file).Alias
// CHECK-DECL: typealias.(file).Outer.Alias
// CHECK-DECL: typealias.(file).Outer.Inner.Alias
// CHECK-DECL: typealias.(file).GenericOuter.Alias
// CHECK-DECL: typealias.(file).GenericOuter.Inner.Alias
// CHECK-DECL: typealias.(file).Proto.Alias
// CHECK-DECL: typealias.(file).Proto extension.OtherAlias
// CHECK-DECL: typealias.(file).GenericOuter extension.ConditionalAlias