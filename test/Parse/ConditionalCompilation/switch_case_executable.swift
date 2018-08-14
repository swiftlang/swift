// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/test1 -module-name main -D PATTERN_1 
// RUN: %target-build-swift %s -o %t/test2 -module-name main -D PATTERN_2
// RUN: %target-codesign %t/test1
// RUN: %target-codesign %t/test2
// RUN: %target-run %t/test1 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK1 %s
// RUN: %target-run %t/test2 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK2 %s

// REQUIRES: executable_test

//------------------------------------------------------------------------------
print("START: switch toplevel")
// CHECK-LABEL: START: switch toplevel

let val = 1
switch val {
  case 100:
    break
#if PATTERN_1
  case 1:
    print("output1")
#elseif PATTERN_2
  case 1:
    print("output2")
#endif
  default:
    print("never")
}

// CHECK1-NEXT: output1
// CHECK2-NEXT: output2

print("END: switch toplevel")
// CHECK-NEXT: END: switch toplevel


//------------------------------------------------------------------------------
print("START: switch func")
// CHECK-LABEL: START: switch func

enum MyEnum {
  case A, B
#if PATTERN_1
  case str(String)
#elseif PATTERN_2
  case int(Int)
#endif
}

func test1(_ val: MyEnum) {
  switch val {
  case .A, .B:
    print("never")
#if PATTERN_1
  case let .str(v):
    print("output3 - " + v)
#elseif PATTERN_2
  case let .int(v):
    print("output4 - \(v + 12)")
#endif
  }
}

#if PATTERN_1
test1(.str("foo bar"))
#elseif PATTERN_2
test1(.int(42))
#endif
// CHECK1-NEXT: output3 - foo bar
// CHECK2-NEXT: output4 - 54

print("END: switch func")
// CHECK-NEXT: END: switch func

//------------------------------------------------------------------------------
print("START: func local")
// CHECK-LABEL: func local

func test2(_ val: Int) -> () -> Void {
  let ret: () -> Void
  switch val {
#if PATTERN_1
  case let v:
    struct Foo : CustomStringConvertible {
      let val: Int
      var description: String { return "Foo(\(val))" }
    }
    func fn() {
      print("output5 - \(Foo(val:v))")
    }
    ret = fn
#elseif PATTERN_2
  case let v:
    struct Bar : CustomStringConvertible {
      let val: Int
      var description: String { return "Bar(\(val))" }
    }
    ret = { print("output6 - \(Bar(val: v))") }
#endif
  }
  return ret
}

test2(42)()
// CHECK1-NEXT: output5 - Foo(42)
// CHECK2-NEXT: output6 - Bar(42)

print("END: func local")
// CHECK-NEXT: END: func local

//------------------------------------------------------------------------------
print("START: nested directives")
// CHECK-LABEL: START: nested directives

#if PATTERN_1 || PATTERN_2
func test3() {
#if PATTERN_1 || PATTERN_2
  class Nested {
#if PATTERN_1 || PATTERN_2
    func foo(_ x: Int) {
      switch x {
#if true
#if PATTERN_1
      case 0..<42:
        print("output7 - 0..<42 \(x)")
#elseif PATTERN_2
      case 0..<42:
        print("output8 - 0..<42 \(x)")
#else
      case 0..<42:
        print("NEVER")
#endif
      default:
        print("output9 - default \(x)")
#endif
      }
    }
#endif
  }
  Nested().foo(12)
#endif
  Nested().foo(53)
}
#endif
test3()
// CHECK1-NEXT: output7 - 0..<42 12
// CHECK1-NEXT: output9 - default 53
// CHECK2-NEXT: output8 - 0..<42 12
// CHECK2-NEXT: output9 - default 53

print("END: nested directives")
// CHECK-NEXT: END: nested directives
