// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/generic_typealias -emit-module

// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/generic_typealias -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

// RUN: sed -ne '/\/\/ *DEMANGLE-DECL: /s/\/\/ *DEMANGLE-DECL: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/generic_typealias -decl-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-DECL

typealias Alias<T> = Int

struct Outer {
  typealias Alias<T> = Int

  struct Inner {
    typealias Alias<T> = Int
  }
}

struct GenericOuter<T> {
  typealias Alias<T> = Int
  typealias AliasWhere = Int where T == GenericOuter<Never>

  struct Inner {
    typealias Alias<T> = Int
    typealias AliasWhere = Int where T: Equatable
  }
}

protocol Proto {
  typealias Alias<T> = Int
}

extension Proto {
  typealias OtherAlias<T> = Int
  typealias OtherAliasWhere = Int where Self == Conforms
}

extension GenericOuter where T : Proto {
  typealias ConditionalAlias<T> = Int
}

struct Conforms : Proto {}

func blackHole(_: Any...) {}

do {
  let x1: Alias<String> = 0
  let x2: Outer.Alias<String> = 0
  let x3: Outer.Inner.Alias<String> = 0

  blackHole(x1, x2, x3)
}

do {
  let x1: GenericOuter<Int>.Alias<String> = 0
  let x2: GenericOuter<Int>.Inner.Alias<String> = 0

  blackHole(x1, x2)
}

do {
  // FIXME: https://bugs.swift.org/browse/SR-9762
  // let x1: Proto.Alias<String> = 0
  // let x2: Proto.OtherAlias<String> = 0

  let x1: Conforms.Alias<String> = 0
  let x2: Conforms.OtherAlias<String> = 0

  blackHole(x1, x2)
}

func generic<T : Proto>(_: T) {
  let x1: T.Alias<String> = 0
  let x2: T.OtherAlias<String> = 0

  blackHole(x1, x2)
}

do {
  let x1: GenericOuter<Conforms>.ConditionalAlias<String> = 0

  blackHole(x1)
}

// DEMANGLE-TYPE: $s17generic_typealias5AliasaySSGD
// DEMANGLE-TYPE: $s17generic_typealias5OuterV5Aliasay_SSGD
// DEMANGLE-TYPE: $s17generic_typealias5OuterV5InnerV5Aliasay__SSGD

// CHECK-TYPE: Alias<String>
// CHECK-TYPE: Outer.Alias<String>
// CHECK-TYPE: Outer.Inner.Alias<String>

// DEMANGLE-TYPE: $s17generic_typealias12GenericOuterV5AliasaySi_SSGD
// DEMANGLE-TYPE: $s17generic_typealias12GenericOuterV10AliasWhereayACys5NeverOG_GD
// DEMANGLE-TYPE: $s17generic_typealias12GenericOuterV5InnerV5AliasaySi__SSGD
// DEMANGLE-TYPE: $s17generic_typealias12GenericOuterV5InnerV10AliasWhereaySi__GD

// CHECK-TYPE: GenericOuter<Int>.Alias<String>
// CHECK-TYPE: GenericOuter<GenericOuter<Never>>.AliasWhere
// CHECK-TYPE: GenericOuter<Int>.Inner.Alias<String>
// CHECK-TYPE: GenericOuter<Int>.Inner.AliasWhere

// DEMANGLE-TYPE: $s17generic_typealias5ProtoP5AliasayAA8ConformsV_SSGD
// DEMANGLE-TYPE: $s17generic_typealias5ProtoPAAE10OtherAliasayAA8ConformsV_SSGD
// DEMANGLE-TYPE: $s17generic_typealias5ProtoPAAE15OtherAliasWhereayAA8ConformsV_GD

// CHECK-TYPE: Conforms.Alias<String>
// CHECK-TYPE: Conforms.OtherAlias<String>
// CHECK-TYPE: Conforms.OtherAliasWhere

// DEMANGLE-TYPE: $s17generic_typealias5ProtoP5Aliasayx_SSGD
// DEMANGLE-TYPE: $s17generic_typealias5ProtoPAAE10OtherAliasayx_SSGD

// CHECK-TYPE: τ_0_0.Alias<String>
// CHECK-TYPE: τ_0_0.OtherAlias<String>

// DEMANGLE-TYPE: $s17generic_typealias12GenericOuterVA2A5ProtoRzlE16ConditionalAliasayAA8ConformsV_SSGD

// CHECK-TYPE: GenericOuter<Conforms>.ConditionalAlias<String>


// DEMANGLE-DECL: $s17generic_typealias5Aliasa
// DEMANGLE-DECL: $s17generic_typealias5OuterV5Aliasa
// DEMANGLE-DECL: $s17generic_typealias5OuterV5InnerV5Aliasa
// DEMANGLE-DECL: $s17generic_typealias12GenericOuterV5Aliasa
// DEMANGLE-DECL: $s17generic_typealias12GenericOuterV10AliasWherea
// DEMANGLE-DECL: $s17generic_typealias12GenericOuterV5InnerV5Aliasa
// DEMANGLE-DECL: $s17generic_typealias12GenericOuterV5InnerV10AliasWherea
// DEMANGLE-DECL: $s17generic_typealias5ProtoP5Aliasa
// DEMANGLE-DECL: $s17generic_typealias5ProtoPAAE10OtherAliasa
// DEMANGLE-DECL: $s17generic_typealias5ProtoPAAE15OtherAliasWherea
// DEMANGLE-DECL: $s17generic_typealias12GenericOuterVA2A5ProtoRzlE16ConditionalAliasa

// CHECK-DECL: generic_typealias.(file).Alias
// CHECK-DECL: generic_typealias.(file).Outer.Alias
// CHECK-DECL: generic_typealias.(file).Outer.Inner.Alias
// CHECK-DECL: generic_typealias.(file).GenericOuter.Alias
// CHECK-DECL: generic_typealias.(file).GenericOuter.AliasWhere
// CHECK-DECL: generic_typealias.(file).GenericOuter.Inner.Alias
// CHECK-DECL: generic_typealias.(file).GenericOuter.Inner.AliasWhere
// CHECK-DECL: generic_typealias.(file).Proto.Alias
// CHECK-DECL: generic_typealias.(file).Proto extension.OtherAlias
// CHECK-DECL: generic_typealias.(file).Proto extension.OtherAliasWhere
// CHECK-DECL: generic_typealias.(file).GenericOuter extension.ConditionalAlias
