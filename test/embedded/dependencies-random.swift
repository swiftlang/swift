// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern %s -c -o %t/a.o

// RUN: grep DEP\: %s | sed 's#// DEP\: ##' | sort > %t/allowed-dependencies.txt

// Linux/ELF and Wasm don't use the "_" prefix in symbol mangling.
// RUN: if [ %target-os == "linux-gnu" ] || [[ %target-os =~ "wasi" ]]; then sed -E -i -e 's/^_(.*)$/\1/' %t/allowed-dependencies.txt; fi

// Wasm has additional dependencies
// RUN: if [[ %target-os =~ "wasi" ]]; then sed -i '' -e '3 i\'$'\n''__stack_pointer' -e '1 i\'$'\n''__indirect_function_table' -e '1 i\'$'\n''__memory_base' %t/allowed-dependencies.txt; fi

// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.o | sort | tee %t/actual-dependencies.txt

// Fail if there is any entry in actual-dependencies.txt that's not in allowed-dependencies.txt
// RUN: test -z "`comm -13 %t/allowed-dependencies.txt %t/actual-dependencies.txt`"

// DEP: ___stack_chk_fail
// DEP: ___stack_chk_guard
// DEP: _arc4random_buf
// DEP: _free
// DEP: _memmove
// DEP: _memset
// DEP: _putchar
// DEP: _posix_memalign

// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-clang -x c -c %S/Inputs/linux-rng-support.c -o %t/linux-rng-support.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/a.o %t/print.o %t/linux-rng-support.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// UNSUPPORTED: OS=linux-gnu && CPU=aarch64

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

@_extern(c, "putchar")
@discardableResult
func putchar(_: CInt) -> CInt

public func print(_ s: StaticString, terminator: StaticString = "\n") {
  var p = s.utf8Start
  while p.pointee != 0 {
    putchar(CInt(p.pointee))
    p += 1
  }
  p = terminator.utf8Start
  while p.pointee != 0 {
    putchar(CInt(p.pointee))
    p += 1
  }
}

class MyClass {
  func foo() { print("MyClass.foo") }
}

class MySubClass: MyClass {
  override func foo() { print("MySubClass.foo") }
}

@main
struct Main {
  static var objects: [MyClass] = []
  static func main() {
    print("Hello Embedded Swift!")
    // CHECK: Hello Embedded Swift!
    objects.append(MyClass())
    objects.append(MySubClass())
    for o in objects {
      o.foo()
    }
    // CHECK: MyClass.foo
    // CHECK: MySubClass.foo
    print(Bool.random() ? "you won" : "you lost")
    // CHECK: you {{won|lost}}
  }
}
