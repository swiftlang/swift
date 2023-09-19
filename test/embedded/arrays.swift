// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend %t/main.swift -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-clang -g -x c %t/runtime.c -c -o %t/runtime.o
// RUN: %target-clang %t/main.o %t/runtime.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// BEGIN main.swift

@_silgen_name("putchar")
func putchar(_: UInt8)

public func print(_ s: StaticString, terminator: StaticString = "\n") {
  var p = s.utf8Start
  while p.pointee != 0 {
    putchar(p.pointee)
    p += 1
  }
  p = terminator.utf8Start
  while p.pointee != 0 {
    putchar(p.pointee)
    p += 1
  }
}

@_silgen_name("vprintf")
func vprintf(_: UnsafePointer<UInt8>, _: UnsafeRawPointer)

var global: Int = 0
public func print(_ n: Int) {
    let f: StaticString = "%d"
    global = n
    vprintf(f.utf8Start, &global)
}

public func print(_ array: [Int]) {
  print("[", terminator: "")
  for e in array {
    print(e)
    print(", ", terminator: "")
  }
  print("]")
}

func test() {
  var a = [1, 2, 3]
  a.append(8)
  a.append(contentsOf: [5, 4])
  let b = a.sorted()
  var c = b
  c = c.reversed()
  print(c) // CHECK: [8, 5, 4, 3, 2, 1, ]
}

test()

// BEGIN runtime.c

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

size_t _swiftEmptyArrayStorage[] = { /*isa*/0, /*refcount*/0, /*count*/0, /*flags*/1 };

void *swift_allocObject(void *metadata, size_t requiredSize, size_t requiredAlignmentMask) {
  void *r = NULL;
  posix_memalign(&r, requiredAlignmentMask + 1, requiredSize);
  return r;
}

void swift_deallocClassInstance() { }
void swift_initStackObject() { }
bool swift_isUniquelyReferenced_nonNull_native(void *) { return true; }
void swift_release() { }
void swift_retain() { }
void swift_setDeallocating() { }

void swift_beginAccess() { }
void swift_endAccess() { }
