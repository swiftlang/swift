// RUN: %empty-directory(%t)
// RUN: %target-build-swift \
// RUN:     %s \
// RUN:     -target %target-future-triple \
// RUN:     -Onone \
// RUN:     -parse-as-library \
// RUN:     -module-name main \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -enable-experimental-feature CoroutineAccessorsAllocateInCallee \
// RUN:     -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_CoroutineAccessorsAllocateInCallee

struct MaybePtrBox<T> {
  private var ptr: UnsafeMutablePointer<T>

  static func sentinel() -> UnsafeMutablePointer<T> {
    .init(bitPattern: 0xdeadbeef)!
  }
  
  init() {
    ptr = .init(MaybePtrBox.sentinel())
  }
  func isValid() -> Bool {
    ptr != MaybePtrBox.sentinel()
  }
  mutating func set(_ t: T?) {
    switch (isValid(), t) {
    case (true, .some(let t)):
      ptr.pointee = t
    case (true, .none):
      ptr.deallocate()
      ptr = MaybePtrBox.sentinel()
    case (false, .some(let t)):
      ptr = .allocate(capacity: 1)
      ptr.initialize(to: t)
    case (false, .none):
      break
    }
  }
  var value : T? {
    // Analogous to the implementation of Dictionary's subscript.
    read {
      let val: T?
      if isValid() {
        val = ptr.pointee
      } else {
        val = .none
      }
      yield val
    }
    modify {
      var val: T?
      if isValid() {
        val = ptr.pointee
      } else {
        val = .none
      }
      yield &val
      set(val)
    }
  }
}

protocol AsyncMutatable {
  mutating func mutate() async
}

struct Stringg : AsyncMutatable {
  var value: String
  mutating func mutate() async {
    value += value
  }
}

@main
struct M {
  static func mutate<T : AsyncMutatable>(_ t: inout T?) async {
    var b = MaybePtrBox<T>()
    b.set(t)
    await b.value?.mutate()
    t = b.value
  }
  static func main() async {
    var v1 = Optional<Stringg>.none
    // CHECK: nil 
    print(v1)
    // CHECK: nil 
    await mutate(&v1)
    print(v1)
    var v2 = Optional.some(Stringg(value: "hi"))
    // CHECK: "hi"
    print(v2)
    await mutate(&v2)
    // CHECK: "hihi"
    print(v2)
  }
}
