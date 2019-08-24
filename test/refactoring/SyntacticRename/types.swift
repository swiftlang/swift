class /*/*class-Foo:unknown*/Foo in comment*/ /*class-Foo:def*/Foo {}
protocol /*protocol-Proto:def*/Proto {
	associatedtype /*associated-Item:def*/Item
}
class /*class-Bar:def*/Bar : /*class-Foo*/Foo, /*protocol-Proto*/Proto {
	typealias /*associated-Item:def*/Item = Int
}

let x: /*class-Bar*/Bar = /*class-Bar*/Bar()

func foo(a: /*class-Foo*/Foo, b: /*class-Bar*/Bar, c: inout /*class-Bar*/Bar) -> /*class-Foo*/Foo {
	return /*class-Bar*/Bar()
}

typealias /*alias-FuncType:def*/FuncType = (/*class-Foo*/Foo, /*class-Bar*/Bar) -> /*class-Bar*/Bar
var example: /*alias-FuncType*/FuncType = {a, b in x}

func bar<X:/*protocol-Proto*/Proto, Y>(a: X, b: Y) {}

class /*class-Animal:def*/Animal</*generic-T:def*/T, /*generic-U:def*/U> where /*generic-U*/U:/*protocol-Proto*/Proto, /*generic-U*/U . /*associated-Item*/Item == Int {}

var anAnimal: /*class-Animal*/Animal<Int, /*class-Bar*/Bar> = /*class-Animal*/Animal()
extension /*class-Animal:def*/Animal {
	func boo(a: /*generic-T*/T, b: /*generic-U*/U) {}
}

enum /*enum-Barcode:def*/Barcode {
	case upc(Int, Int, Int, Int)
	case /*case-qrCode:def*/qrCode(code: String)
	case /*case-other:def*/other(Int)
	case /*case-another:def*/another
}
var barCode: /*enum-Barcode*/Barcode = /*enum-Barcode*/Barcode.upc(1, 1, 1, 1)
barCode = . /*case-qrCode:call*/qrCode(code: "ABCDEFG")
barCode = /*enum-Barcode*/Barcode . /*case-qrCode:call*/qrCode(code: "J")
barCode = . /*case-other:call*/other(2)
barCode = . /*case-another*/another

switch barCode {
case .upc(let a, let b, let c, let d):
	print(a)
case . /*case-qrCode*/qrCode(let s):
	print(s)
case . /*case-another*/another:
	print(1)
case . /*case-other*/other(let x):
	print(x)
}

enum /*enum-WithValue:def*/WithValue: Int {
	case /*case-one:def*/one = 1
}
var _ = /*enum-WithValue*/WithValue . /*case-one*/one

// RUN: %empty-directory(%t.result)
// RUN: %refactor -syntactic-rename -source-filename %s -pos="class-Foo" -is-non-protocol-type -old-name "Foo" -new-name "MoreFoo" >> %t.result/types_class-Foo.swift
// RUN: diff -u %S/Outputs/types/class-Foo.swift.expected %t.result/types_class-Foo.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="protocol-Proto" -old-name "Proto" -new-name "NextProto" >> %t.result/types_protocol-Proto.swift
// RUN: diff -u %S/Outputs/types/protocol-Proto.swift.expected %t.result/types_protocol-Proto.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="class-Bar" -is-non-protocol-type -old-name "Bar" -new-name "MoreBar" >> %t.result/types_class-Bar.swift
// RUN: diff -u %S/Outputs/types/class-Bar.swift.expected %t.result/types_class-Bar.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="associated-Item" -old-name "Item" -new-name "Element" >> %t.result/types_associated-Item.swift
// RUN: diff -u %S/Outputs/types/associated-Item.swift.expected %t.result/types_associated-Item.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="alias-FuncType" -old-name "FuncType" -new-name "FuncType2" >> %t.result/types_alias-FuncType.swift
// RUN: diff -u %S/Outputs/types/alias-FuncType.swift.expected %t.result/types_alias-FuncType.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="class-Animal" -is-non-protocol-type -old-name "Animal" -new-name "Gonzo" >> %t.result/types_class-Animal.swift
// RUN: diff -u %S/Outputs/types/class-Animal.swift.expected %t.result/types_class-Animal.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="generic-T" -is-non-protocol-type -old-name "T" -new-name "Tee" >> %t.result/types_generic-T.swift
// RUN: diff -u %S/Outputs/types/generic-T.swift.expected %t.result/types_generic-T.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="generic-U" -is-non-protocol-type -old-name "U" -new-name "Ewe" >> %t.result/types_generic-U.swift
// RUN: diff -u %S/Outputs/types/generic-U.swift.expected %t.result/types_generic-U.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="enum-Barcode" -is-non-protocol-type -old-name "Barcode" -new-name "BetterBarcode" >> %t.result/types_enum-Barcode.swift
// RUN: diff -u %S/Outputs/types/enum-Barcode.swift.expected %t.result/types_enum-Barcode.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="case-qrCode" -is-function-like -old-name "qrCode(code:)" -new-name "QRCode(_:)" >> %t.result/types_case-qrCode.swift
// RUN: diff -u %S/Outputs/types/case-qrCode.swift.expected %t.result/types_case-qrCode.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="case-other" -is-function-like -old-name "other(_:)" -new-name "Other(x:)" >> %t.result/types_case-other.swift
// RUN: diff -u %S/Outputs/types/case-other.swift.expected %t.result/types_case-other.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="case-another" -old-name "another" -new-name "Another" >> %t.result/types_case-another.swift
// RUN: diff -u %S/Outputs/types/case-another.swift.expected %t.result/types_case-another.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="enum-WithValue" -is-non-protocol-type -old-name "WithValue" -new-name "NewName" >> %t.result/types_enum-WithValue.swift
// RUN: diff -u %S/Outputs/types/enum-WithValue.swift.expected %t.result/types_enum-WithValue.swift
// RUN: %refactor -syntactic-rename -source-filename %s -pos="case-one" -old-name "one" -new-name "two" >> %t.result/types_case-one.swift
// RUN: diff -u %S/Outputs/types/case-one.swift.expected %t.result/types_case-one.swift
