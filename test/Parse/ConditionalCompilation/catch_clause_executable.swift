// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/test1 -module-name main -D PATTERN_1 
// RUN: %target-build-swift %s -o %t/test2 -module-name main -D PATTERN_2
// RUN: %target-codesign %t/test1
// RUN: %target-codesign %t/test2
// RUN: %target-run %t/test1 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK1 %s
// RUN: %target-run %t/test2 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK2 %s

// REQUIRES: executable_test

//------------------------------------------------------------------------------
print("START: catch toplevel")
// CHECK-LABEL: START: catch toplevel

enum E: Error {
  case a, b, c
}

do { throw E.b }
catch E.a {}
#if PATTERN_1
catch E.b { print("output1") }
#elseif PATTERN_2
catch E.b { print("output2") }
#endif
catch { print("never") }

// CHECK1-NEXT: output1
// CHECK2-NEXT: output2

print("END: catch toplevel")
// CHECK-NEXT: END: catch toplevel


//------------------------------------------------------------------------------
print("START: catch func")
// CHECK-LABEL: START: catch func

enum MyEnum: Error {
  case A, B
#if PATTERN_1
  case str(String)
#elseif PATTERN_2
  case int(Int)
#endif
}

func test1(_ val: MyEnum) {
  do { throw val }
  catch MyEnum.A, MyEnum.B { print("never") }
  #if PATTERN_1
  catch let MyEnum.str(v) { print("output3 - " + v) }
  #elseif PATTERN_2
  catch let MyEnum.int(v) { print("output4 - \(v + 12)") }
  #endif
  catch { print("never") }
}

#if PATTERN_1
test1(.str("foo bar"))
#elseif PATTERN_2
test1(.int(42))
#endif
// CHECK1-NEXT: output3 - foo bar
// CHECK2-NEXT: output4 - 54

print("END: catch func")
// CHECK-NEXT: END: catch func

//------------------------------------------------------------------------------
print("START: func local")
// CHECK-LABEL: func local

extension Int: Error {}
func test2(_ val: Int) -> () -> Void {
  let ret: () -> Void
  do {throw val}
#if PATTERN_1
  catch let v as Int {
    struct Foo : CustomStringConvertible {
      let val: Int
      var description: String { return "Foo(\(val))" }
    }
    func fn() {
      print("output5 - \(Foo(val:v))")
    }
    ret = fn
  }
#elseif PATTERN_2
  catch let v as Int {
    struct Bar : CustomStringConvertible {
      let val: Int
      var description: String { return "Bar(\(val))" }
    }
    ret = { print("output6 - \(Bar(val: v))") }
  }
#endif
  catch { ret = { print("never") } }
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
      do { throw x }
#if true
#if PATTERN_1
      catch let x as Int where (0..<42).contains(x) {
        print("output7 - 0..<42 \(x)")
      }
#elseif PATTERN_2
      catch let x as Int where (0..<42).contains(x) {
        print("output8 - 0..<42 \(x)")
      }
#else
      catch let x as Int where (0..<42).contains(x) {
        print("NEVER")
      }
#endif
      catch {
        print("output9 - default \(x)")
      }
#endif
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
