// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -parse-as-library -Xfrontend -enable-copy-propagation) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// =============================================================================
// = Declarations                                                           {{ =
// =============================================================================

class C {
  init(_ d: D) {
    self.d = d
  }
  weak var d: D?
  func foo(_ string: String) {
    d?.cWillFoo(self, string)
  }
}
class D {
  func cWillFoo(_ c: C, _ string: String) {
    print(#function, string)
  }
}

class DataWrapper {

    var pointer: UnsafeMutableRawBufferPointer

    init(count: Int) {
        pointer = UnsafeMutableRawBufferPointer.allocate(byteCount: count * MemoryLayout<Int>.stride, alignment: MemoryLayout<Int>.alignment)
    }

    var bytes: UnsafeMutableRawBufferPointer { return pointer }

    deinit {
        pointer.deallocate()
    }
}

var fileHandleMap: [Int32 : String] = [:]

func openTheFile(_ path: String) -> Int32 {
  let fd: Int32 = 42
  precondition(fileHandleMap[fd] == nil)
  fileHandleMap[fd] = path
  return fd
}

func closeTheFile(_ fd: Int32) {
  fileHandleMap.removeValue(forKey: fd)
}

func writeToTheFile(_ fd: Int32) {
  precondition(fileHandleMap[fd] != nil)
}

class FileHandleWrapper {

    var handle: Int32? = nil

    func open(path: String) {
        let fd = openTheFile(path)
        if fd >= 0 {
            handle = fd
        }
    }

    func close() {
        if let fd = handle {
            closeTheFile(fd)
            handle = nil
        }
    }

    deinit {
        if let fd = handle {
            closeTheFile(fd)
        }
    }
}


// =============================================================================
// = Declarations                                                           }} =
// =============================================================================

// =============================================================================
// = Tests                                                                  {{ =
// =============================================================================

func test_localLet_keepsObjectAliveBeyondCallToClassWithWeakReference() {
  let d = D()
  let c = C(d)
  // CHECK: cWillFoo{{.*}} test_localLet_keepsObjectAliveBeyondCallToClassWithWeakReference
  c.foo(#function)
}

func test_localVar_keepsObjectAliveBeyondCallToClassWithWeakReference() {
  var d = D()
  let c = C(d)
  // Reenable with rdar://86271875
  // CHECK: cWillFoo{{.*}} test_localVar_keepsObjectAliveBeyondCallToClassWithWeakReference
  c.foo(#function)
}

func test_localLet_keepsObjectAliveBeyondCallToClassWithPointer_doit(_ input: [UInt8]) {
    let data = DataWrapper(count: input.count)
    data.bytes.copyBytes(from: input)
}
func test_localLet_keepsObjectAliveBeyondCallToClassWithPointer() {
  test_localLet_keepsObjectAliveBeyondCallToClassWithPointer_doit([1,2,3,4,50])
}

func test_localVar_keepsObjectAliveBeyondCallToClassWithPointer_doit(_ input: [UInt8]) {
    let data = DataWrapper(count: input.count)
    data.bytes.copyBytes(from: input)
}
func test_localVar_keepsObjectAliveBeyondCallToClassWithPointer() {
  test_localVar_keepsObjectAliveBeyondCallToClassWithPointer_doit([1,2,3,4,50])
}

func test_localLet_keepsObjectAliveBeyondCallToSynchronizationPointFunction_doit(_ path: String) {
    var file = FileHandleWrapper()
    file.open(path: path)
    // Retrieving 'fd' is the last use of 'file'
    guard let fd = file.handle else { return }
    // 'fd' has now been closed. The subsequent write will fail.
    writeToTheFile(fd)
}
func test_localLet_keepsObjectAliveBeyondCallToSynchronizationPointFunction() {
  test_localLet_keepsObjectAliveBeyondCallToSynchronizationPointFunction_doit("blue")
}

func test_localVar_keepsObjectAliveBeyondCallToSynchronizationPointFunction_doit(_ path: String) {
    var file = FileHandleWrapper()
    file.open(path: path)
    // Retrieving 'fd' is the last use of 'file'
    guard let fd = file.handle else { return }
    // 'fd' has now been closed. The subsequent write will fail.
    writeToTheFile(fd)
}
func test_localVar_keepsObjectAliveBeyondCallToSynchronizationPointFunction() {
  test_localVar_keepsObjectAliveBeyondCallToSynchronizationPointFunction_doit("blue")
}

func do_foo(_ work: () -> ()) {
  work()
}
class Fooer {
  __consuming func foo() {
    weak var weakSelf = self
    do_foo {
      weakSelf?.foo1()
      weakSelf?.foo2()
    }
  }
  func foo1() {
    // CHECK: Fooer foo1
    print(type(of: self), #function)
  }
  func foo2() {
    // CHECK: Fooer foo2
    print(type(of: self), #function)
  }
}

func test_self_keepsObjectAliveBeyond_callTo_functionTakingClosureCapturingWeakVar() {
  Fooer().foo()
}

func do_foo_async(_ work: @escaping () -> ()) -> Task<Void, Never> {
  Task {
    work()
  }
}
class FooerAsync {
  var strongSelf: FooerAsync?
  __consuming func foo() -> Task<Void, Never> {
    weak var weakSelf = self
    strongSelf = self
    return do_foo_async {
      // At this point, strongSelf is keeping the object alive.
      weakSelf?.foo1()
      // By this point, strongSelf has been nil'd.  However, self in the 
      // enclosing foo() may still be keeping the object alive, depending on how
      // the closure was scheduled.
      weakSelf?.foo2()
    }
  }
  func foo1() {
    // CHECK: FooerAsync foo1
    print(type(of: self), #function)
    strongSelf = nil
  }
  func foo2() {
    print(type(of: self), #function)
  }
}

func test_repeatedLoadWeakSelf() -> Task<Void, Never> {
  FooerAsync().foo()
}

// =============================================================================
// = Tests                                                                  }} =
// =============================================================================

@main struct Main {
  static func main() async {
    test_localLet_keepsObjectAliveBeyondCallToClassWithWeakReference()
    test_localVar_keepsObjectAliveBeyondCallToClassWithWeakReference()
    test_localLet_keepsObjectAliveBeyondCallToClassWithPointer()
    test_localVar_keepsObjectAliveBeyondCallToClassWithPointer()
    test_localLet_keepsObjectAliveBeyondCallToSynchronizationPointFunction()
    test_localVar_keepsObjectAliveBeyondCallToSynchronizationPointFunction()

    test_self_keepsObjectAliveBeyond_callTo_functionTakingClosureCapturingWeakVar()
    await test_repeatedLoadWeakSelf().value
  }
}
