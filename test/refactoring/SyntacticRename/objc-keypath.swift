@objcMembers class /*Outer:def*/Outer {
  let /*outerProp:def*/outerProp = 10

  @objcMembers class /*Inner:def*/Inner {
    let /*prop:def*/prop = 20
    let /*tuple:def*/tuple = (1, 4)
    let /*namedTuple:def*/namedTuple = (x: 1, y: 3)
    let /*array:def*/array = [1, 2, 3]
    let /*dict:def*/dict = ["foo": Outer()]
  }
}

// Valid
_ = #keyPath(/*Outer*/Outer . /*Inner*/Inner . /*prop*/prop)
_ = #keyPath(/*Outer*/Outer . /*Inner*/Inner . /*array*/array)
_ = #keyPath(/*Outer*/Outer . /*Inner*/Inner . /*dict*/dict . someKey)
_ = #keyPath(/*Outer*/Outer . /*Inner*/Inner . /*dict*/dict . someKey . /*outerProp*/outerProp)

// Invalid but resolved
_ = #keyPath(/*Outer*/Outer . /*Inner*/Inner . /*tuple*/tuple)
_ = #keyPath(/*Outer*/Outer . /*Inner*/Inner . /*namedTuple*/namedTuple)
_ = #keyPath(/*Outer*/Outer . /*Inner*/Inner . /*array*/array[0] . hashValue)

// FIXME: Invalid and not resolved
_ = #keyPath(/*Outer:unknown*/Outer . /*Inner:unknown*/Inner . /*dict:unknown*/dict . someKey . undefined)

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="Outer" -old-name "Outer" >> %t/outer.swift
// RUN: diff -u %S/Outputs/objc-keypath/outer.swift.expected %t/outer.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="Inner" -old-name "Inner" >> %t/inner.swift
// RUN: diff -u %S/Outputs/objc-keypath/inner.swift.expected %t/inner.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="outerProp" -old-name "outerProp" >> %t/outerprop.swift
// RUN: diff -u %S/Outputs/objc-keypath/outerprop.swift.expected %t/outerprop.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="prop" -old-name "prop" >> %t/prop.swift
// RUN: diff -u %S/Outputs/objc-keypath/prop.swift.expected %t/prop.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="tuple" -old-name "tuple" >> %t/tuple.swift
// RUN: diff -u %S/Outputs/objc-keypath/tuple.swift.expected %t/tuple.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="namedTuple" -old-name "namedTuple" >> %t/namedtuple.swift
// RUN: diff -u %S/Outputs/objc-keypath/namedtuple.swift.expected %t/namedtuple.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="array" -old-name "array" >> %t/array.swift
// RUN: diff -u %S/Outputs/objc-keypath/array.swift.expected %t/array.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="dict" -old-name "dict" >> %t/dict.swift
// RUN: diff -u %S/Outputs/objc-keypath/dict.swift.expected %t/dict.swift
