import errors

func foo() {
  invalidGlobalMissingInit = ""

  let bar: InvalidStruct
  bar.memberB
  invalidPartialFunc()
  bar.

  let baz: ;

}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -experimental-allow-module-with-compiler-errors -primary-file %S/Inputs/errors-a.swift %S/Inputs/errors-b.swift -module-name errors -o %t/errors.a.swiftmodule
// RUN: %target-swift-frontend -emit-module -experimental-allow-module-with-compiler-errors %S/Inputs/errors-a.swift -primary-file %S/Inputs/errors-b.swift -module-name errors -o %t/errors.b.swiftmodule
// RUN: %target-swift-frontend -merge-modules -emit-module -experimental-allow-module-with-compiler-errors %t/errors.a.swiftmodule %t/errors.b.swiftmodule -module-name errors -o %t/errors.swiftmodule

// Read the module back in to make sure it can be deserialized
// RUN: %target-swift-ide-test -print-module -source-filename dummy -module-to-print errors -I %t | %FileCheck %s
// CHECK: typealias InvalidAlias = <<error type>>
// CHECK: class InvalidClass : <<error type>>, InvalidProtocol
// CHECK: var classMemberA: <<error type>>
// CHECK: init(param1: <<error type>>, param2: <<error type>>)
// CHECK: convenience init()
// CHECK: convenience init(param: <<error type>>)
// CHECK: class InvalidClassSub1 : InvalidClass
// CHECK: var classMemberB: <<error type>>
// CHECK: init(param1: <<error type>>, param2: <<error type>>)
// CHECK: convenience init()
// CHECK: class InvalidClassSub2 : InvalidClass
// CHECK: var classMemberC: <<error type>>
// CHECK: convenience init()
// CHECK: enum InvalidEnum
// CHECK: case enumeratorA
// CHECK: case enumeratorB
// CHECK: case enumeratorC
// CHECK: struct InvalidGenericStruct<T, U>
// CHECK: var genericMemberA: <<error type>>
// CHECK: protocol InvalidProtocol
// CHECK: associatedtype Item
// CHECK: mutating func add(_: <<error type>>)
// CHECK: func get() -> Self.Item
// CHECK: func set(item: Self.Item)
// CHECK: struct InvalidStruct : <<error type>>, InvalidProtocol
// CHECK: typealias Item = <<error type>>
// CHECK: let memberA: Int
// CHECK: let memberB: <<error type>>
// CHECK: var memberC: <<error type>> { get }
// CHECK: lazy var memberD: <<error type>> { mutating get }
// CHECK: var memberE: <<error type>>
// CHECK: mutating func set(item: <<error type>>)
// CHECK: func invalidFuncBody()
// CHECK: func invalidFuncSignature()
// CHECK: func invalidFuncThrows() throws
// CHECK: func invalidFuncType() -> <<error type>>
// CHECK: func invalidGenericFuncBody<T>(param: T) -> T
// CHECK: func invalidGenericFuncType<T>(param: T) -> <<error type>>
// CHECK: invalidGlobalClosureBody:
// CHECK: invalidGlobalClosureType:
// CHECK: invalidGlobalKeypath:
// CHECK: invalidGlobalMissingInit: String
// CHECK: func invalidPartialFunc()
// CHECK: func typeUsesFunc

// Check cursor info for the various symbols
// RUN: %sourcekitd-test -req=cursor -pos=4:3 %s -- -I %t -target %target-triple %s | %FileCheck %s -check-prefix=CHECK-GLOBAL
// CHECK-GLOBAL: source.lang.swift.ref.var.global
// CHECK-GLOBAL: invalidGlobalMissingInit

// RUN: %sourcekitd-test -req=cursor -pos=6:12 %s -- -I %t -target %target-triple %s | %FileCheck %s -check-prefix=CHECK-STRUCT
// CHECK-STRUCT: source.lang.swift.ref.struct
// CHECK-STRUCT: InvalidStruct

// : %sourcekitd-test -req=cursor -pos=7:7 %s -- -I %t -target %target-triple %s | %FileCheck %s -check-prefix=CHECK-MEMBER
// CHECK-MEMBER: source.lang.swift.ref.var.instance
// CHECK-MEMBER: memberB

// RUN: %sourcekitd-test -req=cursor -pos=8:3 %s -- -I %t -target %target-triple %s | %FileCheck %s -check-prefix=CHECK-FUNC
// CHECK-FUNC: source.lang.swift.ref.function.free
// CHECK-FUNC: invalidPartialFunc

// Check completions
// RUN: %sourcekitd-test -req=complete -pos=9:7 %s -- -I %t -target %target-triple %s | %FileCheck %s -check-prefix=CHECK-MEMBER-COMPLETE
// CHECK-MEMBER-COMPLETE: key.name: "add(
// CHECK-MEMBER-COMPLETE: key.name: "get(
// CHECK-MEMBER-COMPLETE: key.name: "memberA"
// CHECK-MEMBER-COMPLETE: key.name: "memberB"
// CHECK-MEMBER-COMPLETE: key.name: "memberC"
// CHECK-MEMBER-COMPLETE: key.name: "memberD"
// CHECK-MEMBER-COMPLETE: key.name: "memberE"
// CHECK-MEMBER-COMPLETE: key.name: "set(

// RUN: %sourcekitd-test -req=complete -pos=11:11 %s -- -I %t -target %target-triple %s | %FileCheck %s -check-prefix=CHECK-TYPE-COMPLETE
// CHECK-TYPE-COMPLETE: key.name: "InvalidAlias"
// CHECK-TYPE-COMPLETE: key.name: "InvalidClass"
// CHECK-TYPE-COMPLETE: key.name: "InvalidClassSub1"
// CHECK-TYPE-COMPLETE: key.name: "InvalidClassSub2"
// CHECK-TYPE-COMPLETE: key.name: "InvalidEnum"
// CHECK-TYPE-COMPLETE: key.name: "InvalidGenericStruct"
// CHECK-TYPE-COMPLETE: key.name: "InvalidProtocol"
// CHECK-TYPE-COMPLETE: key.name: "InvalidStruct"

// RUN: %sourcekitd-test -req=complete -pos=12:1 %s -- -I %t -target %target-triple %s | %FileCheck %s -check-prefix=CHECK-GLOBAL-COMPLETE
// CHECK-GLOBAL-COMPLETE: key.name: "InvalidAlias"
// CHECK-GLOBAL-COMPLETE: key.name: "InvalidClass"
// CHECK-GLOBAL-COMPLETE: key.name: "InvalidClassSub1"
// CHECK-GLOBAL-COMPLETE: key.name: "InvalidClassSub2"
// CHECK-GLOBAL-COMPLETE: key.name: "InvalidEnum"
// CHECK-GLOBAL-COMPLETE: key.name: "invalidFuncBody(
// CHECK-GLOBAL-COMPLETE: key.name: "invalidFuncSignature(
// CHECK-GLOBAL-COMPLETE: key.name: "invalidFuncThrows(
// CHECK-GLOBAL-COMPLETE: key.name: "invalidFuncType(
// CHECK-GLOBAL-COMPLETE: key.name: "invalidGenericFuncBody(
// CHECK-GLOBAL-COMPLETE: key.name: "invalidGenericFuncType(
// CHECK-GLOBAL-COMPLETE: key.name: "InvalidGenericStruct"
// CHECK-GLOBAL-COMPLETE: key.name: "invalidGlobalClosureBody"
// CHECK-GLOBAL-COMPLETE: key.name: "invalidGlobalClosureType
// CHECK-GLOBAL-COMPLETE: key.name: "invalidGlobalKeypath
// CHECK-GLOBAL-COMPLETE: key.name: "invalidGlobalMissingInit
// CHECK-GLOBAL-COMPLETE: key.name: "invalidPartialFunc(
// CHECK-GLOBAL-COMPLETE: key.name: "InvalidProtocol"
// CHECK-GLOBAL-COMPLETE: key.name: "InvalidStruct"
// CHECK-GLOBAL-COMPLETE: key.name: "typeUsesFunc(
