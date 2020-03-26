// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/test_runtime_function_counters
// RUN: %target-codesign %t/test_runtime_function_counters
// RUN: %target-run %t/test_runtime_function_counters 2>&1 | %FileCheck %s
// REQUIRES: runtime_function_counters
// REQUIRES: executable_test
// REQUIRES: rdar48995133

/// Test functionality related to the runtime function counters.

class C {
  var next: C? = nil
  func test(_ c: C) {
  }
}

struct MyStruct {
  var ref1: AnyObject? = C()
  var ref2: AnyObject = C()
  var str: String = ""
}

public final class List<T> {
  var value: T
  var next: List<T>?

  init(_ value: T) {
    self.value = value
    self.next = nil
  }

  init(_ value: T, _ tail: List<T>) {
    self.value = value
    self.next = tail
  }
}

public func length<T>(_ l: List<T>) -> Int {
  var ll: List<T>? = l
  var len = 0
  while ll != nil {
    len = len + 1
    ll = ll?.next
  }
  return len
}

/// CHECK-LABEL: TEST: Collect references inside objects

// Constant strings don't really have a reference, but BridgeObject
// still counts as one.
//
// FIXME(TODO: JIRA): On 32-bit, we use AnyObject? instead of BridgeObject. If
// we get back onto a real reference (or if 64-bit gets off of a real
// reference), then drop adjust the optionality of the following check.
//
/// CHECK: Constant string: [{{([0-9a-fA-Fx]+)?}}]

/// An array has one reference
/// CHECK: Array<Int>: [{{[0-9a-fA-Fx]+}}]

/// MyStruct has two references plus a String with a third
//
// FIXME(TODO: JIRA): On 32-bit, we use AnyObject? instead of BridgeObject. If
// we get back onto a real reference (or if 64-bit gets off of a real
// reference), then drop adjust the optionality of the following check.
//
/// CHECK: MyStruct: [{{[0-9a-fA-Fx]+}}, {{[0-9a-fA-Fx]+}}{{(, [0-9a-fA-Fx]+)?}}]

/// Dictionary has one reference
/// CHECK: Dictionary<Int, Int>: [{{[0-9a-fA-Fx]+}}]
/// Set has one reference
/// CHECK: Set<Int>: [{{[0-9a-fA-Fx]+}}]
/// Test collection of references inside different types of objects.
@inline(never)
func testCollectReferencesInsideObject() {
  print("TEST: Collect references inside objects")
  let s = "MyString"
  let aint = [1,2,3,4]
  let dint = [1:1, 2:2]
  let sint: Set<Int> = [1,2,3,4]

  print("Constant string: \(_collectReferencesInsideObject(s))")
  print("Array<Int>: \(_collectReferencesInsideObject(aint))")
  print("MyStruct: \(_collectReferencesInsideObject(MyStruct()))")
  print("Dictionary<Int, Int>: \(_collectReferencesInsideObject(dint))")
  print("Set<Int>: \(_collectReferencesInsideObject(sint))")

  var mystring = "MyString"
  mystring.append("End")
  testString(mystring)
  testDict(dint)
  testObjectCycle()
}


/// CHECK-LABEL: TEST: APIs from _RuntimeFunctionCounters
/// CHECK: Number of runtime function pointers:
/// Test some APIs from _RuntimeFunctionCounters
func testRuntimeCounters() {
  print("TEST: APIs from _RuntimeFunctionCounters")
  let numRuntimeFunctionPointer =
    Int(_RuntimeFunctionCounters.getNumRuntimeFunctionCounters())

  print("Number of runtime function pointers: \(numRuntimeFunctionPointer)")

  let names = _RuntimeFunctionCounters.getRuntimeFunctionNames()
  let offsets = _RuntimeFunctionCounters.getRuntimeFunctionCountersOffsets()

  for i in 0..<numRuntimeFunctionPointer {
    print("Runtime function \(i) : \(names[i]) at offset: \(offsets[i])")
  }

  var d: [Int : Int] = [:]
  let globalCounters1 = _GlobalRuntimeFunctionCountersState()

  for i in 0..<50 {
    let k = i
    let v = i*i
    d[k] = v
  }

  let globalCounters2 = _GlobalRuntimeFunctionCountersState()

  globalCounters1.dumpDiff(globalCounters2, skipUnchanged: true)
}

/// Test finding references inside a String object.
@inline(never)
func testString(_ s: String) {
  print("TEST: Collect references for strings")
  let refs = _collectReferencesInsideObject(s)
  print("References are: \(refs)")
  let objectCounters1 = _ObjectRuntimeFunctionCountersState(refs[0])
  let _ = [String](repeating: s, count: 4)
  let objectCounters2 = _ObjectRuntimeFunctionCountersState(refs[0])
  objectCounters1.dumpDiff(objectCounters2, skipUnchanged: true)
}

/// Test finding references inside a Dictionary object.
@inline(never)
func testDict(_ _dint: [Int : Int]) {
  print("TEST: Collect references for dictionaries")
  var dint = _dint
  dint[3] = 3
  let refs = _collectReferencesInsideObject(dint)
  print("References are: \(refs)")
  let objectCounters1 = _ObjectRuntimeFunctionCountersState(refs[0])
  dint[222] = 222
  dint[2222] = 2222
  let objectCounters2 = _ObjectRuntimeFunctionCountersState(refs[0])
  objectCounters1.dumpDiff(objectCounters2, skipUnchanged: true)
}

/// Test finding references inside an object graph with a cycle.
/// It should not result in a stack overflow.
@inline(never)
func testObjectCycle() {
  print("TEST: Collect references on object graph with cycles")
  print("testObjectCycle")
  let c1 = C()
  let c2 = C()
  c1.next = c1
  c2.next = c1
  let refs = _collectReferencesInsideObject(c1)
  print("References are: \(refs)")
  let objectCounters1 = _ObjectRuntimeFunctionCountersState(refs[0])
  c1.next = nil
  c2.next = nil
  let objectCounters2 = _ObjectRuntimeFunctionCountersState(refs[0])
  objectCounters1.dumpDiff(objectCounters2, skipUnchanged: true)
}

/// Test runtime function counters for a List object.
@inline(never)
func testLists() {
  print("TEST: Runtime function counters for Lists")
  print("testLists")
  let globalCounters1 = _GlobalRuntimeFunctionCountersState()
  var l: List<Int>? = List(1, List(2, List(3, List(4, List(5)))))
  let refs = _collectReferencesInsideObject(l!)
  let globalCounters11 = _GlobalRuntimeFunctionCountersState()
  let _ = _collectReferencesInsideObject(l!)
  let globalCounters111 = _GlobalRuntimeFunctionCountersState()

  print("Global counters diff for 11")
  globalCounters1.dumpDiff(globalCounters11, skipUnchanged: true)
  print("Global counters diff for 111")
  globalCounters1.dumpDiff(globalCounters111, skipUnchanged: true)

  let len = length(l!)
  let globalCounters2 = _GlobalRuntimeFunctionCountersState()
  print("Length of the list is \(len)")
  print("Global counters diff after constructing a list and computing its length")
  globalCounters1.dumpDiff(globalCounters2, skipUnchanged: true)
  let objectCounters1 = _ObjectRuntimeFunctionCountersState(refs[0])
  l = nil
  let objectCounters2 = _ObjectRuntimeFunctionCountersState(refs[0])
  print("List head counters after list becomes unreferenced")
  objectCounters1.dumpDiff(objectCounters2, skipUnchanged: true)
}

/// Test the _measureRuntimeFunctionCountersDiffs API.
@inline(never)
func testMeasureRuntimeFunctionCountersDiffs() {
  print("TEST: Measure runtime function counters diff")
  let l: List<Int>? = List(1, List(2, List(3, List(4, List(5)))))
  let refs = _collectReferencesInsideObject(l!)
  var len = 0
  let (globalCounters, objectsCountersDiffs) =
    _measureRuntimeFunctionCountersDiffs(objects: [refs[0]]) {
    len = length(l!)
  }
  print("List length is: \(len)")
  print("Global counters changes")
  globalCounters.dump(skipUnchanged: true)
  print("Objects counters changes")
  for (i, objectCounters) in objectsCountersDiffs.enumerated() {
    print("Object counters diff for \(refs[i])")
    objectCounters.dump(skipUnchanged: true)
  }
}

/// This is a handler that is invoked on each runtime functions counters update.
@inline(never)
func updatesHandler(object: UnsafeRawPointer, functionId: Int64) {
  let savedMode = _RuntimeFunctionCounters.disableRuntimeFunctionCountersUpdates()
  print("Start handler")
  let functionName = _RuntimeFunctionCounters.runtimeFunctionNames[Int(functionId)]
  print("Function \(functionName) was invoked on object \(object)")
  print("End handler")
  _RuntimeFunctionCounters.enableRuntimeFunctionCountersUpdates(mode: savedMode)
}

/// Check that it is possible to set your own runtime functions counters
/// updates handler and this handler is invoked at runtime.
/// CHECK-LABEL: TEST: Provide runtime function counters update handler
/// CHECK: Start handler
/// Check that allocations and deallocations are intercepted too.
/// CHECK: swift_allocObject
/// CHECK: swift_deallocObject
/// CHECK: End handler
/// Test that you can provide custom handlers for runtime functions counters
/// updates.
var globalC: C? = nil
@inline(never)
func testFunctionRuntimeCountersUpdateHandler() {
  print("TEST: Provide runtime function counters update handler")
  let l: List<Int>? = List(1, List(2, List(3, List(4, List(5)))))
  let oldHandler =
    _RuntimeFunctionCounters.setGlobalRuntimeFunctionCountersUpdateHandler(
      handler: updatesHandler)
  globalC = C()
  globalC = nil
  let len = length(l!)
  _ = _RuntimeFunctionCounters.setGlobalRuntimeFunctionCountersUpdateHandler(
    handler: oldHandler)
  print("Restored old handler")
  print(len)
}

/// Enable runtime function counters stats collection.
_RuntimeFunctionCounters.enableRuntimeFunctionCountersUpdates()

/// Test collection of references inside different types of objects.
testCollectReferencesInsideObject()

/// Test some APIs from _RuntimeFunctionCounters.
testRuntimeCounters()

/// Test dumping of counters for all objects.
_RuntimeFunctionCounters.dumpObjectsRuntimeFunctionPointers()

/// Test runtime function counters for a List object.
testLists()

/// Test the _measureRuntimeFunctionCountersDiffs API.
testMeasureRuntimeFunctionCountersDiffs()

/// Test that you can provide custom handlers for runtime functions counters updates.
testFunctionRuntimeCountersUpdateHandler()
