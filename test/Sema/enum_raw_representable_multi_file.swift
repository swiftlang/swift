// RUN: %swift -parse -verify -primary-file %s %S/Inputs/enum_multi_file_helper.swift

var raw1: Int = Foo.A.toRaw()
var raw2: Bar.RawType = 0
var cooked1: Foo? = Foo.fromRaw(raw1)
var cooked2: Bar? = Bar.fromRaw(22)
var cooked3: Baz? = Baz.fromRaw(0)
var cooked4: Garply? = Garply.fromRaw("A")

func consume<T: RawRepresentable>(obj: T) {}
func test() {
  consume(cooked1!)
  consume(cooked2!)
  consume(cooked3!)
  consume(cooked4!)
}
