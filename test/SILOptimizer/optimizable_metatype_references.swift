// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 %s -module-name MyModule -emit-sil -o - | %FileCheck --check-prefix=SIL %s
// RUN: %target-build-swift -swift-version 5 %s -o %t/main -module-name MyModule
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

protocol MyProtocol {}

struct MyStruct: Hashable, Equatable, MyProtocol {
  static let static1: any Any.Type = MyStruct.self
  static let static2: any Equatable.Type = MyStruct.self
  static let static3: any (Hashable & Equatable).Type = MyStruct.self
  static let static4 = MyStruct.self
  static let static5: any MyProtocol.Type = MyStruct.self
}

print("OK!")
// CHECK: OK

// No constant folding for static1 (empty next line)
// SIL: sil_global hidden [let] @$s8MyModule0A6StructV7static1ypXpvpZ : $@thick any Any.Type
// SIL-EMPTY:

// No constant folding for static2 (empty next line)
// SIL: sil_global hidden [let] @$s8MyModule0A6StructV7static2SQ_pXpvpZ : $@thick any Equatable.Type
// SIL-EMPTY:

// No constant folding for static3 (empty next line)
// SIL: sil_global hidden [let] @$s8MyModule0A6StructV7static3SH_pXpvpZ : $@thick any Hashable.Type
// SIL-EMPTY:

// Constant folding for static4 kicks in
// SIL:      sil_global hidden [let] @$s8MyModule0A6StructV7static4ACmvpZ : $@thin MyStruct.Type = {
// SIL-NEXT: %initval = metatype $@thin MyStruct.Type
// SIL-NEXT: }

// Constant folding for static5 kicks in
// SIL: sil_global hidden [let] @$s8MyModule0A6StructV7static5AA0A8Protocol_pXpvpZ : $@thick any MyProtocol.Type = {
// SIL: %0 = metatype $@thick MyStruct.Type
// SIL: %initval = init_existential_metatype %0, $@thick any MyProtocol.Type
// SIL: }
