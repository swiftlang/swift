// RUN: %target-swift-frontend -parse -verify -primary-file %s %S/Inputs/enum_multi_file_helper.swift

var raw1: Int = Foo.A.rawValue
var raw2: Bar.RawValue = 0
var cooked1: Foo? = Foo(rawValue: raw1)
var cooked2: Bar? = Bar(rawValue: 22)
var cooked3: Baz? = Baz(rawValue: 0)
var cooked4: Garply? = Garply(rawValue: "A")

func consume<T: RawRepresentable>(obj: T) {}
func test() {
  consume(cooked1!)
  consume(cooked2!)
  consume(cooked3!)
  consume(cooked4!)
}
