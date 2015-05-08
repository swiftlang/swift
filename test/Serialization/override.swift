// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_class.swift -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o %t -I %t %S/Inputs/def_override.swift
// RUN: llvm-bcanalyzer %t/def_override.swiftmodule | FileCheck %s
// RUN: %target-swift-frontend -parse -I %t %s -verify

// CHECK-NOT: UnknownCode

import def_override

let methods = OverrideFunc()
methods.reset()

let baseMethods: StillEmpty = methods
baseMethods.reset()


let props = OverrideComputedProperty()
props.value = props.value + 1
print(props.readOnly)

let baseProps: ComputedProperty = props
baseProps.value = baseProps.value + 1
print(baseProps.readOnly)


let newSetter = OverrideAddsSetter()
newSetter.readOnly = newSetter.value


let simpleSubscript1 = OverrideSimpleSubscript()
print(simpleSubscript1[4])

let newSetterSubscript = OverrideAddsSubscriptSetter()
newSetterSubscript[4] = newSetterSubscript[5]


let simpleSubscript2 = OverrideComplexSubscript()
simpleSubscript2[4, true] = 5
print(simpleSubscript2[4, true])
