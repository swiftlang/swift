// RUN: %target-typecheck-verify-swift

let f1: (Int) -> Int = { $0 }
let f2: @convention(swift) (Int) -> Int = { $0 }
let f2a: @convention(swift, cType: "int *(int)") (Int32) -> Int32 = { $0 } // expected-error{{convention 'swift' does not support the 'cType' argument label, did you mean '@convention(c, cType: "int *(int)")' or '@convention(block, cType: "int *(int)")' instead?}}
let f3: @convention(block) (Int) -> Int = { $0 }
let f4: @convention(c) (Int) -> Int = { $0 }
let f4a: @convention(c, cType: "int (int)") (Int32) -> Int32 = { $0 } // expected-error{{unable to parse 'int (int)'; it should be a C function pointer type or a block pointer type}}
let f4b: @convention(c, cType: "void *") (Int32) -> Int32 = { $0 } // expected-error{{unable to parse 'void *'; it should be a C function pointer type or a block pointer type}}
let f4c: @convention(c, cType: "int (*)(int)") (Int32) -> Int32 = { $0 }

let f5: @convention(INTERCAL) (Int) -> Int = { $0 } // expected-error{{convention 'INTERCAL' not supported}}

// https://github.com/apple/swift/issues/53417

do {
  func block(_ f: @convention(block) @autoclosure () -> Int) -> Void {} // expected-error {{'@convention(block)' attribute is not allowed on '@autoclosure' types}}
  block(1)

  func c(_ f: @convention(c) @autoclosure () -> Int) -> Void {} // expected-error{{'@convention(c)' attribute is not allowed on '@autoclosure' types}}
  c(1)

  func swift(_ f: @convention(swift) @autoclosure () -> Int) -> Void {} // OK
  swift(1)

  func thin(_ f: @convention(thin) @autoclosure () -> Int) -> Void {} // OK
  thin(1)

  func block2(_ f: @autoclosure @convention(block) () -> Int) -> Void {} // expected-error {{'@convention(block)' attribute is not allowed on '@autoclosure' types}}
  block2(1)

  func c2(_ f: @autoclosure @convention(c) () -> Int) -> Void {} // expected-error {{'@convention(c)' attribute is not allowed on '@autoclosure' types}}
  c2(1)

  func swift2(_ f: @autoclosure @convention(swift) () -> Int) -> Void {} // OK
  swift2(1)

  func thin2(_ f: @autoclosure @convention(thin) () -> Int) -> Void {} // OK
  thin2(1)
}
