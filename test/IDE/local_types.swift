// RUN: %swiftc_driver -sdk %sdk %s %S/Inputs/local_types_input.swift -emit-module -emit-module-path %T -module-name LocalTypes
// RUN: %swift-ide-test -print-local-type-decl -module-to-print LocalTypes -I %T -source-filename %s

// Single nesting

public func singleNestingFunc() {
  // CHECK: _TtMV4TestL33_1EA75C0391F28784323014CC86FC6367_1S
  // CHECK-NEXT: struct S {
  // CHECK-NEXT:   let single: Int
  // CHECK-NEXT:   func foo()
  // CHECK-NEXT:   init(single: Int)
  // CHECK-NEXT: }
  struct S {
    let single: Int
    func foo() {}
  }
  // CHECK: _TtMC4TestL33_1EA75C0391F28784323014CC86FC6367_1C
  // CHECK-NEXT: class C {
  // CHECK-NEXT:   final let single
  // CHECK-NEXT:   func foo()
  // CHECK-NEXT:   init()
  // CHECK-NEXT:   @objc deinit 
  // CHECK-NEXT: }
  class C {
    let single = 2
    func foo() {}
  }
  // CHECK: _TtMO4TestL33_1EA75C0391F28784323014CC86FC6367_1E
  // CHECK-NEXT: enum E {
  // CHECK-NEXT:   case Single(Int)
  // CHECK-NEXT:   func foo()
  // CHECK-NEXT: }
  enum E {
    case Single(Int)
    func foo() {}
  }
}

public func singleNestingFunc1() {
  // Identically-named nominal types in this function
  // should not collide with those in globalfunc() - they should be
  // uniqued with a file-level counter.
  // CHECK: _TtMV4TestL33_1EA75C0391F28784323014CC86FC63670_1S
  // CHECK: struct S {
  // CHECK:   let singlesingle: Int
  // CHECK:   func foo()
  // CHECK:   init(singlesingle: Int)
  // CHECK: }
  struct S {
    let singlesingle: Int
    func foo() {}
  }

  // CHECK: _TtMC4TestL33_1EA75C0391F28784323014CC86FC63670_1C
  // CHECK-NEXT: class C {
  // CHECK-NEXT:   final let singlesingle
  // CHECK-NEXT:   func foo()
  // CHECK-NEXT:   init()
  // CHECK-NEXT:   @objc deinit 
  // CHECK-NEXT: }
  class C {
    let singlesingle = 2
    func foo() {}
  }
  // CHECK: _TtMO4TestL33_1EA75C0391F28784323014CC86FC63670_1E
  // CHECK-NEXT: enum E {
  // CHECK-NEXT:   case SingleSingle(Int)
  // CHECK-NEXT:   func foo()
  // CHECK-NEXT: }
  enum E {
    case SingleSingle(Int)
    func foo() {}
  }
}

public let singleNestingClosure: () -> () = {
  // CHECK: _TtMV4TestL33_1EA75C0391F28784323014CC86FC63671_1S
  // CHECK-NEXT: struct S {
  // CHECK-NEXT:   let singleclosure: Int
  // CHECK-NEXT:   func foo()
  // CHECK-NEXT:   init(singleclosure: Int)
  // CHECK-NEXT: }
  struct S {
    let singleclosure: Int
    func foo() {}
  }
  // CHECK: _TtMC4TestL33_1EA75C0391F28784323014CC86FC63671_1C
  // CHECK-NEXT: class C {
  // CHECK-NEXT:   final let singleclosure
  // CHECK-NEXT:   func foo()
  // CHECK-NEXT:   init()
  // CHECK-NEXT:   @objc deinit 
  // CHECK-NEXT: }
  class C {
    let singleclosure = 2
    func foo() {}
  }
  // CHECK: _TtMO4TestL33_1EA75C0391F28784323014CC86FC63671_1E
  // CHECK-NEXT: enum E {
  // CHECK-NEXT:   case SingleClosure(Int)
  // CHECK-NEXT:   func foo()
  // CHECK-NEXT: }
  enum E {
    case SingleClosure(Int)
    func foo() {}
  }
}

// Double nesting

public func doubleNestingFunc() {
  func inner() {
    // CHECK: _TtMV4TestL33_1EA75C0391F28784323014CC86FC63672_1S
    // CHECK-NEXT: struct S {
    // CHECK-NEXT:   let double: Int
    // CHECK-NEXT:   func foo()
    // CHECK-NEXT:   init(double: Int)
    // CHECK-NEXT: }
    struct S {
      let double: Int
      func foo() {}
    }
    // CHECK: _TtMC4TestL33_1EA75C0391F28784323014CC86FC63672_1C
    // CHECK-NEXT: class C {
    // CHECK-NEXT:   final let double
    // CHECK-NEXT:   func foo()
    // CHECK-NEXT:   init()
    // CHECK-NEXT:   @objc deinit 
    // CHECK-NEXT: }
    class C {
      let double = 2
      func foo() {}
    }
    // CHECK: _TtMO4TestL33_1EA75C0391F28784323014CC86FC63672_1E
    // CHECK-NEXT: enum E {
    // CHECK-NEXT:   case Double(Int)
    // CHECK-NEXT:   func foo()
    // CHECK-NEXT: }
    enum E {
      case Double(Int)
      func foo() {}
    }
  }
  inner()
}

public let doubleNestingClosure: () -> () = {
  let inner: () -> () = {
    // CHECK: _TtMV4TestL33_1EA75C0391F28784323014CC86FC63673_1S
    // CHECK-NEXT: struct S {
    // CHECK-NEXT:   let doubleclosure: Int
    // CHECK-NEXT:   func foo()
    // CHECK-NEXT:   init(doubleclosure: Int)
    // CHECK-NEXT: }
    struct S {
      let doubleclosure: Int
      func foo() {}
    }
    // CHECK: _TtMC4TestL33_1EA75C0391F28784323014CC86FC63673_1C
    // CHECK-NEXT: class C {
    // CHECK-NEXT:   final let doubleclosure
    // CHECK-NEXT:   func foo()
    // CHECK-NEXT:   init()
    // CHECK-NEXT:   @objc deinit 
    // CHECK-NEXT: }
    class C {
      let doubleclosure = 2
      func foo() {}
    }
    // CHECK: _TtMO4TestL33_1EA75C0391F28784323014CC86FC63673_1E
    // CHECK-NEXT: enum E {
    // CHECK-NEXT:   case DoubleClosure(Int)
    // CHECK-NEXT:   func foo()
    // CHECK-NEXT: }
    enum E {
      case DoubleClosure(Int)
      func foo() {}
    }
  }
  inner()
}

// With transparent

@transparent func WithTransparent() {
  // CHECK: _TtMV4TestL33_1EA75C0391F28784323014CC86FC6367_13InTransparent
  // CHECK-NEXT: struct InTransparent {
  // CHECK-NEXT:   let transparent: String
  // CHECK-NEXT:   init(transparent: String)
  // CHECK-NEXT: }
  struct InTransparent {
    let transparent: String
  }
  println(InTransparent(transparent: "Hello from transparent!").transparent)
}

func IncludesTransparent() {
  WithTransparent()
}

// From local_types_input.swift
// Tests that module merging maintains unique local discriminators

// CHECK: _TtMV4TestL33_36209A1A664DD512BB8AC5C1CA36E461_1S
// CHECK-NEXT: struct S {
// CHECK-NEXT:   let i: Int
// CHECK-NEXT:   func foo()
// CHECK-NEXT:   init(i: Int)
// CHECK-NEXT: }

// CHECK: _TtMC4TestL33_36209A1A664DD512BB8AC5C1CA36E461_1C
// CHECK-NEXT: class C {
// CHECK-NEXT:   final let i
// CHECK-NEXT:   func foo()
// CHECK-NEXT:   init()
// CHECK-NEXT:   @objc deinit 
// CHECK-NEXT: }

// CHECK: _TtMO4TestL33_36209A1A664DD512BB8AC5C1CA36E461_1E
// CHECK-NEXT: enum E {
// CHECK-NEXT:   case I(Int)
// CHECK-NEXT:   func foo()
// CHECK-NEXT: }
