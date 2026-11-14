// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-clang -x c -c %t/check.c -o %t/check.o

// RUN: %target-swift-frontend -O %t/main.swift -c -o %t/main-O.o -enable-experimental-feature Embedded -enable-experimental-feature TypedAllocation -enable-experimental-feature TailAllocatedAttribute -enable-experimental-feature BuiltinModule -enable-experimental-feature Extern
// RUN: %target-clang %target-clang-resource-dir-opt %t/main-O.o %t/check.o %embedded-stdlib -o %t/a-O.out
// RUN: %target-run %t/a-O.out | %FileCheck %s --check-prefixes=CHECK,OPT --implicit-check-not=MISMATCH

// RUN: %target-swift-frontend -Onone %t/main.swift -c -o %t/main-Onone.o -enable-experimental-feature Embedded -enable-experimental-feature TypedAllocation -enable-experimental-feature TailAllocatedAttribute -enable-experimental-feature BuiltinModule -enable-experimental-feature Extern
// RUN: %target-clang %target-clang-resource-dir-opt %t/main-Onone.o %t/check.o %embedded-stdlib -o %t/a-Onone.out
// RUN: %target-run %t/a-Onone.out | %FileCheck %s --implicit-check-not=MISMATCH

// REQUIRES: OS=macosx
// REQUIRES: SWIFT_STDLIB_ARCH=arm64
// REQUIRES: executable_test
// REQUIRES: embedded_stdlib
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_TypedAllocation
// REQUIRES: swift_feature_TailAllocatedAttribute
// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_Extern
// REQUIRES: swift_embedded_platform
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// UNSUPPORTED: DARWIN_SIMULATOR=tvos
// UNSUPPORTED: DARWIN_SIMULATOR=watchos
// UNSUPPORTED: DARWIN_SIMULATOR=xros

// Every free of a tail-allocated object passes its allocation's size,
// alignment and type ID (checked by check.c below): the annotated stdlib
// classes, a ManagedBuffer-shaped class (the stdlib here is built without typed
// allocation, so its ManagedBuffer has no count) and a class with two tails.
// Globals and `use` keep objects on the heap.

// CHECK: scenario array: allocs={{[1-9][0-9]*}} frees={{[1-9][0-9]*}} mismatches=0
// CHECK: scenario string: allocs={{[1-9][0-9]*}} frees={{[1-9][0-9]*}} mismatches=0
// CHECK: scenario set: allocs={{[1-9][0-9]*}} frees={{[1-9][0-9]*}} mismatches=0
// CHECK: scenario dictionary: allocs={{[1-9][0-9]*}} frees={{[1-9][0-9]*}} mismatches=0
// CHECK: scenario keypath-class-holder: allocs={{[1-9][0-9]*}} frees={{[1-9][0-9]*}} mismatches=0
// CHECK: scenario inherited-attribute: allocs={{[1-9][0-9]*}} frees={{[1-9][0-9]*}} mismatches=0
// CHECK: scenario two-tails: allocs={{[1-9][0-9]*}} frees={{[1-9][0-9]*}} mismatches=0
// A dead array is still optimized away.
// OPT: scenario dead-array: allocs=0
// CHECK: total mismatches=0

//--- main.swift
import Builtin

@_extern(c, "test_begin") func testBegin(_ name: UnsafePointer<UInt8>)
@_extern(c, "test_end") func testEnd()
@_extern(c, "test_report") func testReport()

@inline(never) func run(_ name: StaticString, _ body: () -> Void) {
  testBegin(name.utf8Start)
  body()
  testEnd()
}

@inline(never) func makeCount() -> Int { 37 }

@_optimize(none) func use<T>(_ value: T) {}

final class Obj {
  var value: Int
  init(_ value: Int) { self.value = value }
}

// A writable key path through a class makes the key path machinery allocate a
// ClassHolder.
struct Outer {
  var obj: Obj
  var n: Int
}

@inline(never) func write(_ o: inout Outer, _ kp: WritableKeyPath<Outer, Int>) {
  o[keyPath: kp] = makeCount()
}

// ManagedBuffer's shape: an open generic base class with a stored count, set
// by a @_transparent `create` (in an extension, as @_transparent isn't allowed
// in a class body), and a subclass that inherits the attribute.
@_tailAllocated(counts: _tailCount, Element)
open class CountedBuffer<Element> {
  @usableFromInline internal final var _tailCount: Int
  public init(_doNotCallMe: ()) { fatalError() }
}

extension CountedBuffer {
  @_transparent
  public final class func create(count: Int) -> CountedBuffer<Element> {
    let p = Builtin.allocWithTailElems_1(self, count._builtinWordValue, Element.self)
    p._tailCount = count
    return p
  }
}

// Bytes, so the size isn't a multiple of 8.
final class ByteBuffer: CountedBuffer<UInt8> {}

// Two tails: bytes, then Int64s after alignment padding.
@_tailAllocated(counts: tailCounts, UInt8, Int64)
final class TwoTails {
  final var tailCounts: (Int, Int)
  init(_doNotCallMe: ()) { fatalError() }

  static func create(bytes: Int, words: Int) -> TwoTails {
    let p = Builtin.allocWithTailElems_2(
      TwoTails.self, bytes._builtinWordValue, UInt8.self,
      words._builtinWordValue, Int64.self)
    p.tailCounts = (bytes, words)
    return p
  }
}

var arraySink: [Int] = []
var stringSink = ""
var setSink: Set<Int> = []
var dictionarySink: [Int: Obj] = [:]
var outerSink = Outer(obj: Obj(0), n: 0)
var byteSink: CountedBuffer<UInt8>? = nil
var twoTailsSink: TwoTails? = nil

run("array") {
  // Growing also frees the outgrown buffers.
  for i in 0..<makeCount() * 4 { arraySink.append(i) }
  use(arraySink)
  arraySink = []
}
run("string") {
  for _ in 0..<makeCount() { stringSink += "abcd" }
  use(stringSink)
  stringSink = ""
}
run("set") {
  for i in 0..<makeCount() { setSink.insert(i) }
  use(setSink)
  setSink = []
}
run("dictionary") {
  for i in 0..<makeCount() { dictionarySink[i] = Obj(i) }
  use(dictionarySink)
  dictionarySink = [:]
}
run("keypath-class-holder") {
  write(&outerSink, \Outer.obj.value)
  use(outerSink)
}
run("inherited-attribute") {
  byteSink = ByteBuffer.create(count: makeCount())
  use(byteSink)
  byteSink = nil
}
run("two-tails") {
  twoTailsSink = TwoTails.create(bytes: makeCount(), words: makeCount())
  use(twoTailsSink)
  twoTailsSink = nil
}
run("dead-array") {
  // An unused array literal is removed by dead object elimination today
  // (unlike Array(repeating:count:)); it must stay removed.
  _ = [makeCount(), makeCount(), makeCount()]
}
testReport()

//--- check.c
// An Embedded Swift platform layer that checks that every deallocation passes
// what its allocation passed (size, alignment, type ID, typed or not) and
// prints a MISMATCH line otherwise. Tests group allocations into scenarios with
// test_begin/test_end and call test_report at the end. %embedded-stdlib
// provides no platform functions, so the others are implemented here too.

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  void *ptr;
  size_t size, alignment;
  uint64_t typeId;
  int typed;
} Allocation;

enum { MaxAllocations = 4096 };
static Allocation live[MaxAllocations];
static size_t liveCount;
static const char *scenario = "setup";
static unsigned long allocs, frees, mismatches, totalMismatches;

void test_begin(const char *name) {
  scenario = name;
  allocs = frees = mismatches = 0;
}

void test_end(void) {
  printf("scenario %s: allocs=%lu frees=%lu mismatches=%lu\n", scenario, allocs,
         frees, mismatches);
  scenario = "setup";
}

void test_report(void) { printf("total mismatches=%lu\n", totalMismatches); }

static void mismatch(Allocation a, Allocation f) {
  printf("MISMATCH scenario %s: allocated size=0x%zx align=%zu id=0x%llx "
         "typed=%d, freed size=0x%zx align=%zu id=0x%llx typed=%d\n",
         scenario, a.size, a.alignment, (unsigned long long)a.typeId, a.typed,
         f.size, f.alignment, (unsigned long long)f.typeId, f.typed);
  ++mismatches;
  ++totalMismatches;
}

static void *allocate(size_t size, size_t alignment, uint64_t typeId,
                      int typed) {
  void *p = NULL;
  if (posix_memalign(&p, alignment < 16 ? 16 : alignment, size ? size : 1))
    return NULL;
  if (liveCount == MaxAllocations)
    abort();
  live[liveCount++] = (Allocation){p, size, alignment, typeId, typed};
  ++allocs;
  return p;
}

static void deallocate(void *ptr, size_t size, size_t alignment,
                       uint64_t typeId, int typed) {
  for (size_t i = 0; i != liveCount; ++i) {
    Allocation a = live[i], f = {ptr, size, alignment, typeId, typed};
    if (a.ptr != ptr)
      continue;
    if (a.size != size || a.alignment != alignment || a.typeId != typeId ||
        a.typed != typed)
      mismatch(a, f);
    live[i] = live[--liveCount];
    ++frees;
    free(ptr);
    return;
  }
  mismatch((Allocation){0}, (Allocation){ptr, size, alignment, typeId, typed});
}

void *_swift_allocate(size_t alignment, size_t size, uint64_t flags) {
  return allocate(size, alignment, 0, 0);
}
void _swift_deallocate(void *ptr, size_t alignment, size_t size,
                       uint64_t flags) {
  deallocate(ptr, size, alignment, 0, 0);
}
void *_swift_typedAllocate(size_t size, size_t alignment, uint64_t flags,
                           uint64_t typeId) {
  return allocate(size, alignment, typeId, 1);
}
void _swift_typedDeallocate(void *ptr, size_t size, size_t alignment,
                            uint64_t flags, uint64_t typeId) {
  deallocate(ptr, size, alignment, typeId, 1);
}

__attribute__((weak)) void _swift_lockStandardOutput(void) {}
__attribute__((weak)) void _swift_unlockStandardOutput(void) {}
size_t _swift_writeToStandardOutput(const unsigned char *chars, size_t count) {
  return fwrite(chars, 1, count, stdout);
}
void _swift_reportError(const unsigned char *message, size_t count,
                        uint64_t flags) {
  fprintf(stderr, "%.*s\n", (int)count, (const char *)message);
}
void _swift_reportErrorAt(const unsigned char *message, size_t count,
                          const unsigned char *file, size_t fileCount,
                          size_t line, uint64_t flags) {
  _swift_reportError(message, count, flags);
}
void _swift_generateRandom(void *buffer, size_t n) { memset(buffer, 0x5a, n); }
void _swift_generateRandomHashSeed(void *buffer, size_t n) {
  memset(buffer, 0x5a, n);
}
void _swift_exit(int code) { exit(code); }
