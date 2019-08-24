protocol P1 {
  func foo()
  func foo1()
}
class C1 : P1 {
  func foo1() {}
}
class C2 : P1 {
  func foo() {}
}
class C3 : P1 {}

protocol P2 {
	associatedtype T1
	associatedtype T2
	func foo1()
}
class C4 : P2 {}
class C5 : P2 {
  typealias T1 = Int
  func foo1() {}
}
class C6 : P2 {
  typealias T1 = Int
  typealias T2 = Int
}
class C7 : P2 {
  typealias T2 = Int
  func foo1() {}
}
class C8 : P2 {
  typealias T1 = Int
  typealias T2 = Int
  func foo1() {}
}

class C9 {}
extension C9 : P1 {}
extension C9 : P2 {}
class C10 {}
extension C10 : P1 {
  func foo() {}
  func foo1() {}
}
extension C10 : P2 {
  typealias T1 = Int
  typealias T2 = Int
  func foo1() {}
}
class C11 {}
extension C11 : P1 {
  func foo() {}
}
extension C11 : P2 {
  typealias T1 = Int
  typealias T2 = Int
}
class C12 {}
extension C12 : P1 {
  func foo1() {}
}
extension C12 : P2 {
  typealias T1 = Int
  func foo1() {}
}
class C13 {}
extension C13 : P1 {
  func foo() {}
  func foo1() {}
}
extension C13 : P2 {
  typealias T1 = Int
  func foo1() {}
}
class C14 {}
extension C14 : P1 {
  func foo() {}
}
extension C14 : P2 {
  typealias T1 = Int
  typealias T2 = Int
  func foo1() {}
}
protocol P3 {
  func foo3()
  func foo4()
}
extension C14: P3 {
  func foo3()
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -fill-stub -source-filename %s -pos=5:8 >> %t.result/P5-8.swift
// RUN: diff -u %S/Outputs/basic/P5-8.swift.expected %t.result/P5-8.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=8:8 >> %t.result/P8-8.swift
// RUN: diff -u %S/Outputs/basic/P8-8.swift.expected %t.result/P8-8.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=11:8 >> %t.result/P11-8.swift
// RUN: diff -u %S/Outputs/basic/P11-8.swift.expected %t.result/P11-8.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=18:8 >> %t.result/P18-8.swift
// RUN: diff -u %S/Outputs/basic/P18-8.swift.expected %t.result/P18-8.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=19:8 >> %t.result/P19-8.swift
// RUN: diff -u %S/Outputs/basic/P19-8.swift.expected %t.result/P19-8.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=23:8 >> %t.result/P23-8.swift
// RUN: diff -u %S/Outputs/basic/P23-8.swift.expected %t.result/P23-8.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=27:8 >> %t.result/P27-8.swift
// RUN: diff -u %S/Outputs/basic/P27-8.swift.expected %t.result/P27-8.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=38:12 >> %t.result/P38-12.swift
// RUN: diff -u %S/Outputs/basic/P38-12.swift.expected %t.result/P38-12.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=39:12 >> %t.result/P39-12.swift
// RUN: diff -u %S/Outputs/basic/P39-12.swift.expected %t.result/P39-12.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=51:12 >> %t.result/P51-12.swift
// RUN: diff -u %S/Outputs/basic/P51-12.swift.expected %t.result/P51-12.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=54:12 >> %t.result/P54-12.swift
// RUN: diff -u %S/Outputs/basic/P54-12.swift.expected %t.result/P54-12.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=59:12 >> %t.result/P59-12.swift
// RUN: diff -u %S/Outputs/basic/P59-12.swift.expected %t.result/P59-12.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=62:12 >> %t.result/P62-12.swift
// RUN: diff -u %S/Outputs/basic/P62-12.swift.expected %t.result/P62-12.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=71:12 >> %t.result/P71-12.swift
// RUN: diff -u %S/Outputs/basic/P71-12.swift.expected %t.result/P71-12.swift
// RUN: %refactor -fill-stub -source-filename %s -pos=88:12 >> %t.result/P88-12.swift
// RUN: diff -u %S/Outputs/basic/P88-12.swift.expected %t.result/P88-12.swift
