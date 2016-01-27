protocol MyProtocol {
  associatedtype Inner
  var inner: Inner { get }
}

class MyClass {
  let i: Int
  let ms: MyStruct
  let me: MyEnum
  init(i: Int, ms: MyStruct, me: MyEnum) {
    self.i = i
    self.ms = ms
    self.me = me
  }
}

struct MyStruct {
  let i: Int
  let mc: MyClass
  let me: MyEnum
}

enum MyEnum {
  case C(MyClass)
  indirect case S(MyStruct)
  indirect case E(MyEnum)
  case I(Int)
}

class MyGenericClass<T : MyProtocol> {
  let t: T
  let i: T.Inner
  let mgs: MyGenericStruct<T>
  let mge: MyGenericEnum<T>

  init(t: T, i: T.Inner, mgs: MyGenericStruct<T>, mge: MyGenericEnum<T>) {
    self.t = t
    self.i = i
    self.mgs = mgs
    self.mge = mge
  }
}

struct MyGenericStruct<T : MyProtocol> {
  let t: T
  let i: T.Inner
  let mgc: MyGenericClass<T>
  let mge: MyGenericEnum<T>
}

enum MyGenericEnum<T : MyProtocol> {
  case GC(MyGenericClass<T>)
  indirect case GS(MyGenericStruct<T>)
  indirect case GE(MyGenericEnum<T>)
  case I(Int)
}

