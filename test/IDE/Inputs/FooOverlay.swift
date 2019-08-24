@_exported import Foo // not scoped, top-level module
@_exported import Foo.FooSub // not scoped, submodule
@_exported import func Foo.FooSub.fooSubFunc1 // scoped, from submodule
@_exported import struct Foo.FooStruct1 // scoped, from top-level module

func fooSubOverlayFunc1(x: Int32) -> Int32 {
  return fooSubFunc1(x)
}
