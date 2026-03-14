// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

// This test ensures that nested type aliases inherited from the base type are 
// interchangeable with the base types themselves.

import TypeAliases

extension Derived {
  func takesBaseTypes(bs: Base.Struct, bt: Base.T, bu: Base.U) {
    let _: Struct = bs
    let _: U = bs

    let _: T = bt
    let _: Int32 = bt
    
    let _: U = bu
    let _: Struct = bu
  }

  func takesDerivedTypes(ds: Struct, dt: T, du: U) {
    let _: Base.Struct = ds
    let _: Base.U = ds

    let _: Base.T = dt
    let _: Int32 = dt
    
    let _: Base.U = du
    let _: Base.Struct = du
  }
}
func takesBaseTypes(bs: Base.Struct, bt: Base.T, bu: Base.U) {
  let _: Derived.Struct = bs
  let _: Derived.U = bs

  let _: Derived.T = bt
  let _: Int32 = bt
  
  let _: Derived.U = bu
  let _: Derived.Struct = bu
}

func takesDerivedTypes(ds: Derived.Struct, dt: Derived.T, du: Derived.U) {
  let _: Base.Struct = ds
  let _: Base.U = ds

  let _: Base.T = dt
  let _: Int32 = dt
  
  let _: Base.U = du
  let _: Base.Struct = du
}
