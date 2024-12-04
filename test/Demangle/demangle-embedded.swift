; This is not really a Swift source file: -*- Text -*-

; RUN: echo '$e4main8MyStructV3fooyyFAA1XV_Tg5' | swift-demangle | %FileCheck %s
; CHECK: generic specialization <main.X> of main.MyStruct.foo() -> ()
