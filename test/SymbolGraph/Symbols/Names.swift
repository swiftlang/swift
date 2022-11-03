// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/ExternalNames.swift -module-name ExternalNames -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name Names -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name Names -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/Names.symbols.json
// RUN: %FileCheck %s --input-file %t/Names.symbols.json --check-prefix=FUNC
// RUN: %FileCheck %s --input-file %t/Names.symbols.json --check-prefix=INNERTYPE
// RUN: %FileCheck %s --input-file %t/Names.symbols.json --check-prefix=INNERENUM
// RUN: %FileCheck %s --input-file %t/Names.symbols.json --check-prefix=INNERCASE

// RUN: %FileCheck %s --input-file %t/Names@ExternalNames.symbols.json --check-prefix=EXT_INNERTYPE
// RUN: %FileCheck %s --input-file %t/Names@ExternalNames.symbols.json --check-prefix=EXT_INNERENUM

// RUN: %FileCheck %s --input-file %t/Names.symbols.json --check-prefix=INNERTYPEALIAS
// RUN: %FileCheck %s --input-file %t/Names.symbols.json --check-prefix=INNERTYPEALIAS_INNERINNERTYPE

// RUN: %FileCheck %s --input-file %t/Names@ExternalNames.symbols.json --check-prefix=EXT_INNERTYPEALIAS
// RUN: %FileCheck %s --input-file %t/Names@ExternalNames.symbols.json --check-prefix=EXT_INNERTYPEALIAS_INNERINNERTYPE

public struct MyStruct {
  public struct InnerStruct {}

  public typealias InnerTypeAlias = InnerStruct

  public func foo() {}

  public enum InnerEnum {
      case InnerCase
  }
}

public extension MyStruct.InnerTypeAlias {
  struct InnerInnerStruct {}
}

import ExternalNames

public extension ExternalStruct.InnerStruct {
  func foo() {}
}

public extension ExternalStruct.InnerTypeAlias {
  func bar() {}
}

public extension ExternalStruct.InnerEnum {
  func foo() {}
}

public extension ExternalStruct.InnerTypeAlias {
  struct InnerInnerStruct {}
}

// CHECK-LABEL: "precise": "s:5Names8MyStructV"
// CHECK: names
// CHECK-NEXT: "title": "MyStruct"

// FUNC-LABEL: "precise": "s:5Names8MyStructV3fooyyF",
// FUNC: names
// FUNC-NEXT: "title": "foo()"

// INNERTYPE-LABEL: "precise": "s:5Names8MyStructV05InnerC0V"
// INNERTYPE: names
// INNERTYPE-NEXT: "title": "MyStruct.InnerStruct"

// INNERENUM-LABEL: "precise": "s:5Names8MyStructV9InnerEnumO",
// INNERENUM: names
// INNERENUM-NEXT: "title": "MyStruct.InnerEnum"

// INNERCASE-LABEL: "precise": "s:5Names8MyStructV9InnerEnumO0D4CaseyA2EmF",
// INNERCASE: names
// INNERCASE-NEXT: "title": "MyStruct.InnerEnum.InnerCase",

// EXT_INNERTYPE-LABEL: "precise": "s:e:s:13ExternalNames0A6StructV05InnerC0V0B0E3fooyyF"
// EXT_INNERTYPE: names
// EXT_INNERTYPE-NEXT: "title": "ExternalStruct.InnerStruct"

// EXT_INNERENUM-LABEL: "precise": "s:e:s:13ExternalNames0A6StructV9InnerEnumO0B0E3fooyyF",
// EXT_INNERENUM: names
// EXT_INNERENUM-NEXT: "title": "ExternalStruct.InnerEnum"


// The typealias symbol itself should use the name of the typealias

// INNERTYPEALIAS-LABEL: "precise": "s:5Names8MyStructV14InnerTypeAliasa"
// INNERTYPEALIAS: names
// INNERTYPEALIAS-NEXT: "title": "MyStruct.InnerTypeAlias"

// Types declared in extensions to typealiases should always use the name of their
// original (aliased) parent type

// INNERTYPEALIAS_INNERINNERTYPE-LABEL: "precise": "s:5Names8MyStructV05InnerC0V0ddC0V"
// INNERTYPEALIAS_INNERINNERTYPE: names
// INNERTYPEALIAS_INNERINNERTYPE-NEXT: "title": "MyStruct.InnerStruct.InnerInnerStruct"

// Extension symbols which extend a typealias should use the name of the original
// (aliased) type

// EXT_INNERTYPEALIAS-LABEL: "precise": "s:e:s:13ExternalNames0A6StructV05InnerC0V0B0E3baryyF"
// EXT_INNERTYPEALIAS: names
// EXT_INNERTYPEALIAS-NEXT: "title": "ExternalStruct.InnerStruct"

// Symbols declared in an extension to a typealias should also use the name of the original
// (aliased) type

// EXT_INNERTYPEALIAS_INNERINNERTYPE-LABEL: "precise": "s:13ExternalNames0A6StructV05InnerC0V0B0E0ddC0V"
// EXT_INNERTYPEALIAS_INNERINNERTYPE: names
// EXT_INNERTYPEALIAS_INNERINNERTYPE-NEXT: "title": "ExternalStruct.InnerStruct.InnerInnerStruct"
