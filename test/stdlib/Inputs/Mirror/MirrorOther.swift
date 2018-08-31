struct OtherStruct {
  let a: OtherOuter.Inner
  let b: OtherOuterGeneric<Int>.Inner<String>
}

struct OtherOuter {}
struct OtherOuterGeneric<T> {}
