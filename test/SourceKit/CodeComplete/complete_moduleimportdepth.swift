import ImportsImportsFoo
import FooHelper.FooHelperExplicit
func test() {
  let x = 1
  #^A^#
}

// REQUIRES: objc_interop
// RUN: %complete-test -hide-none -group=none -tok=A %s -raw -- -I %S/Inputs -F %S/../Inputs/libIDE-mock-sdk > %t
// RUN: %FileCheck %s < %t

// Swift == 1
// CHECK-LABEL:  key.name: "abs(:)",
// CHECK-NEXT:   key.description: "abs(x: Comparable & SignedNumeric)",
// CHECK-NEXT:   key.typename: "Comparable & SignedNumeric",
// CHECK-NEXT:   key.doc.brief: "Returns the absolute value of the given number.",
// CHECK-NEXT:   key.context: source.codecompletion.context.othermodule,
// CHECK-NEXT:   key.moduleimportdepth: 1,
// CHECK-NEXT:   key.num_bytes_to_erase: 0,
// CHECK-NOT:    key.modulename
// CHECK:        key.modulename: "Swift"
// CHECK-NEXT:   key.sourcetext: "abs(<#T##x: Comparable & SignedNumeric##Comparable & SignedNumeric#>)"
// CHECK-NEXT: },

// FooHelper.FooHelperExplicit == 1
// CHECK-LABEL:  key.name: "fooHelperExplicitFrameworkFunc1(:)",
// CHECK-NEXT:   key.description: "fooHelperExplicitFrameworkFunc1(a: Int32)",
// CHECK-NEXT:   key.typename: "Int32",
// CHECK-NEXT:   key.context: source.codecompletion.context.othermodule,
// CHECK-NEXT:   key.moduleimportdepth: 1,
// CHECK-NEXT:   key.num_bytes_to_erase: 0,
// CHECK-NOT:    key.modulename
// CHECK:        key.modulename: "FooHelper.FooHelperExplicit"
// CHECK-NEXT:   key.sourcetext: "fooHelperExplicitFrameworkFunc1(<#T##a: Int32##Int32#>)"
// CHECK-NEXT: },

// ImportsImportsFoo == 1
// CHECK-LABEL:  key.name: "importsImportsFoo()",
// CHECK-NEXT:   key.description: "importsImportsFoo()",
// CHECK-NEXT:   key.typename: "Void",
// CHECK-NEXT:   key.context: source.codecompletion.context.othermodule,
// CHECK-NEXT:   key.moduleimportdepth: 1,
// CHECK-NEXT:   key.num_bytes_to_erase: 0,
// CHECK-NOT:    key.modulename
// CHECK:        key.modulename: "ImportsImportsFoo"
// CHECK-NEXT:   key.sourcetext: "importsImportsFoo()"
// CHECK-NEXT: },

// Bar == 2
// CHECK-LABEL:  key.name: "BarForwardDeclaredClass",
// CHECK-NEXT:   key.description: "BarForwardDeclaredClass",
// CHECK-NEXT:   key.typename: "BarForwardDeclaredClass",
// CHECK-NEXT:   key.context: source.codecompletion.context.othermodule,
// CHECK-NEXT:   key.moduleimportdepth: 2,
// CHECK-NEXT:   key.num_bytes_to_erase: 0,
// CHECK-NOT:    key.modulename
// CHECK:        key.modulename: "Bar"
// CHECK-NEXT:   key.sourcetext: "BarForwardDeclaredClass"
// CHECK-NEXT: },

// ImportsFoo == 2
// CHECK-LABEL:  key.name: "importsFoo()",
// CHECK-NEXT:   key.description: "importsFoo()",
// CHECK-NEXT:   key.typename: "Void",
// CHECK-NEXT:   key.context: source.codecompletion.context.othermodule,
// CHECK-NEXT:   key.moduleimportdepth: 2,
// CHECK-NEXT:   key.num_bytes_to_erase: 0,
// CHECK-NOT:    key.modulename
// CHECK:        key.modulename: "ImportsFoo"
// CHECK-NEXT:   key.sourcetext: "importsFoo()"
// CHECK-NEXT: },

// Foo == FooSub == 3
// CHECK-LABEL:  key.name: "FooClassBase",
// CHECK-NEXT:   key.description: "FooClassBase",
// CHECK-NEXT:   key.typename: "FooClassBase",
// CHECK-NEXT:   key.context: source.codecompletion.context.othermodule,
// CHECK-NEXT:   key.moduleimportdepth: 3,
// CHECK-NEXT:   key.num_bytes_to_erase: 0,
// CHECK-NOT:    key.modulename
// CHECK:        key.modulename: "Foo"
// CHECK-NEXT:   key.sourcetext: "FooClassBase"
// CHECK-NEXT: },

// CHECK-LABEL:  key.name: "FooSubEnum1",
// CHECK-NEXT:   key.description: "FooSubEnum1",
// CHECK-NEXT:   key.typename: "FooSubEnum1",
// CHECK-NEXT:   key.context: source.codecompletion.context.othermodule,
// CHECK-NEXT:   key.moduleimportdepth: 3,
// CHECK-NEXT:   key.num_bytes_to_erase: 0,
// CHECK-NOT:    key.modulename
// CHECK:        key.modulename: "Foo.FooSub"
// CHECK-NEXT:   key.sourcetext: "FooSubEnum1"
// CHECK-NEXT: },

// FooHelper == 4
// FIXME: rdar://problem/20230030
// We're picking up the implicit import of FooHelper used to attach FooHelperExplicit to.
// xCHECK-LABEL:  key.name: "FooHelperUnnamedEnumeratorA2",
// xCHECK-NEXT:   key.description: "FooHelperUnnamedEnumeratorA2",
// xCHECK-NEXT:   key.typename: "Int",
// xCHECK-NEXT:   key.context: source.codecompletion.context.othermodule,
// xCHECK-NEXT:   key.moduleimportdepth: 4,
// xCHECK-NEXT:   key.num_bytes_to_erase: 0,
// xCHECK-NOT:    key.modulename
// xCHECK:        key.modulename: "FooHelper"
// xCHECK-NEXT:   key.sourcetext: "FooHelperUnnamedEnumeratorA2"
// xCHECK-NEXT: },
