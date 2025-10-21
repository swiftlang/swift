// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

// This test ensures that nested type inherited from the base type are
// interchangeable with the base types themselves.

import NestedTypes

func parent(
  bc: Base.EnumClass,
  be: Base.Enum,
  bs: Base.Struct,
  bp: Base.Parent,
  bpc: Base.Parent.Child,
  bu: Base.Union,
) {
  let dc: Derived.EnumClass = bc
  let de: Derived.Enum = be
  let ds: Derived.Struct = bs
  let dp: Derived.Parent = bp
  let dpc: Derived.Parent.Child = bpc
  let du: Derived.Union = bu

  let _: Base.EnumClass = dc
  let _: Base.Enum = de
  let _: Base.Struct = ds
  let _: Base.Parent = dp
  let _: Base.Parent.Child = dpc
  let _: Base.Union = du
}

func grandparent(
  bc: Base.EnumClass,
  be: Base.Enum,
  bs: Base.Struct,
  bp: Base.Parent,
  bpc: Base.Parent.Child,
  bu: Base.Union,
) {
  let dc: Derived2.EnumClass = bc
  let de: Derived2.Enum = be
  let ds: Derived2.Struct = bs
  let dp: Derived2.Parent = bp
  let dpc: Derived2.Parent.Child = bpc
  let du: Derived2.Union = bu

  let _: Derived.EnumClass = dc
  let _: Derived.Enum = de
  let _: Derived.Struct = ds
  let _: Derived.Parent = dp
  let _: Derived.Parent.Child = dpc
  let _: Derived.Union = du
}

func siblings(
  dc: Derived.EnumClass,
  de: Derived.Enum,
  ds: Derived.Struct,
  dp: Derived.Parent,
  dpc: Derived.Parent.Child,
  du: Derived.Union,
) {
  let _: Derived1.EnumClass = dc
  let _: Derived1.Enum = de
  let _: Derived1.Struct = ds
  let _: Derived1.Parent = dp
  let _: Derived1.Parent.Child = dpc
  let _: Derived1.Union = du
}

// Instances created from derived class can be type-annotated with base class
// and vice versa
func assigner() {
  let _: Base.EnumClass = Derived.EnumClass.ecb
  let _: Base.Enum = Derived.ea // expected-error {{type 'Derived' has no member 'ea'}}
                                // ^FIXME: nested enums are broken, so inherited nested enums are broken too
  let _: Base.Struct = Derived.Struct(sa: 4, sb: 2)
  let _: Base.Parent = Derived.Parent()
  let _: Base.Parent.Child = Derived.Parent.Child(pca: 42)
  let _: Base.Union = Derived.Union(ua: 42)

  let _: Derived.EnumClass = Base.EnumClass.ecc
  let _: Derived.Enum = Base.ea // expected-error {{type 'Base' has no member 'ea'}}
                                // ^FIXME: nested enums are broken, so inherited nested enums are broken too
  let _: Derived.Struct = Base.Struct(sa: 4, sb: 2)
  let _: Derived.Parent = Base.Parent()
  let _: Derived.Parent.Child = Base.Parent.Child(pca: 42)
  let _: Derived.Union = Base.Union(ua: 42)
}

// Extensions on base type should be "seen" in derived types too, and vice versa
extension Base.Parent {
  static func getChild1() -> Child {
    return Child(pca: 111)
  }
}

extension Derived {
  func haveChild1() {
    let _: Parent.Child = Parent.getChild1()
  }
}

extension Derived.Parent {
  static func getChild2() -> Child {
    return Child(pca: 111)
  }
}

extension Base {
  func haveChild2() {
    let _: Parent.Child = Parent.getChild2()
  }
}
