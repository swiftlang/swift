// RUN: %empty-directory(%t)
// RUN: cd %t
// RUN: %target-build-swift %S/Inputs/tsan-uninstrumented.swift -module-name TSanUninstrumented -emit-module -emit-module-path %t/TSanUninstrumented.swiftmodule -parse-as-library
// RUN: %target-build-swift %S/Inputs/tsan-uninstrumented.swift -c -module-name TSanUninstrumented -parse-as-library -o %t/TSanUninstrumented.o
// RUN: %target-swiftc_driver %s %t/TSanUninstrumented.o -I%t -L%t -g -sanitize=thread %import-libdispatch -o %t/tsan-binary
// RUN: not env %env-TSAN_OPTIONS=abort_on_error=0 %target-run %t/tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: stress_test
// REQUIRES: tsan_runtime

// Test ThreadSanitizer execution end-to-end when calling
// an uninstrumented module with inout parameters

#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#else
#error("Unsupported platform")
#endif
import TSanUninstrumented

// Globals to allow closures passed to pthread_create() to be thin.
var gInThread1: () -> () = { }
var gInThread2: () -> () = { }

// Spawn two threads, run the two passed in closures simultaneously, and
// join them.
func testRace(name: String, thread inThread1: @escaping () -> (), thread inThread2: @escaping () -> ()) {
#if canImport(Darwin)
  var thread1: pthread_t?
  var thread2: pthread_t?
#else
  var thread1: pthread_t = 0
  var thread2: pthread_t = 0
  var t : pthread_t = 0
#endif
  fputs("Running \(name)\n", stderr)

  // Store these in globals so the closure passed to pthread_create
  // can be turned into a C function pointer.
  gInThread1 = inThread1
  gInThread2 = inThread2
  pthread_create(&thread1, nil, { _ in
    gInThread1()
    return nil
  }, nil)

  pthread_create(&thread2, nil, { _ in
    gInThread2()
    return nil
  }, nil)

#if canImport(Darwin)
  _ = pthread_join(thread1!, nil)
  _ = pthread_join(thread2!, nil)
#else
  _ = pthread_join(thread1, nil)
  _ = pthread_join(thread2, nil)
#endif

  // TSan reports go to stderr
  fputs("Done \(name)\n", stderr)
}


public class InstrumentedClass {
  public init() { }

  public var storedProperty1: Int = 7
  public var storedProperty2: Int = 22

  public var storedStructProperty: UninstrumentedStruct = UninstrumentedStruct()

  private var _backingStoredProperty: Int = 7
  public var computedPropertyBackedByStoredProperty: Int {
    get {
      return _backingStoredProperty
    }

    set(newVal) {
      _backingStoredProperty = newVal;
    }
  }
}


// Tests for accesses to globals
// We use different globals for each test to avoid suppressions due
// to TSan's issue uniquing logic.

var globalForGlobalStructMutatingMethod = UninstrumentedStruct()
testRace(name: "GlobalStructMutatingMethod",
        thread: { _ = globalForGlobalStructMutatingMethod.read() },
        thread: { globalForGlobalStructMutatingMethod.mutate() } )
// CHECK-LABEL: Running GlobalStructMutatingMethod
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is global

var globalForGlobalStructDifferentStoredPropertiesInout = UninstrumentedStruct()
testRace(name: "GlobalStructDifferentStoredPropertiesInout",
        thread: { uninstrumentedTakesInout(&globalForGlobalStructDifferentStoredPropertiesInout.storedProperty1) },
        thread: { uninstrumentedTakesInout(&globalForGlobalStructDifferentStoredPropertiesInout.storedProperty2) } )
// CHECK-LABEL: Running GlobalStructDifferentStoredPropertiesInout
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is global

var globalForGlobalStructSameStoredPropertyInout = UninstrumentedStruct()
testRace(name: "GlobalStructSameStoredPropertyInout",
        thread: { uninstrumentedTakesInout(&globalForGlobalStructSameStoredPropertyInout.storedProperty1) },
        thread: { uninstrumentedTakesInout(&globalForGlobalStructSameStoredPropertyInout.storedProperty1) } )
// CHECK-LABEL: Running GlobalStructSameStoredPropertyInout
// CHECK: ThreadSanitizer: Swift access race


var globalForGlobalStructSubscriptDifferentIndexesInout = UninstrumentedStruct()
testRace(name: "GlobalStructSubscriptDifferentIndexesInout",
        thread: { uninstrumentedTakesInout(&globalForGlobalStructSubscriptDifferentIndexesInout[0]) },
        thread: { uninstrumentedTakesInout(&globalForGlobalStructSubscriptDifferentIndexesInout[1]) } )
// CHECK-LABEL: Running GlobalStructSubscriptDifferentIndexes
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is global


var globalForGlobalStructSubscriptDifferentIndexesGetSet = UninstrumentedStruct()
testRace(name: "GlobalStructSubscriptDifferentIndexesGetSet",
        thread: { _ = globalForGlobalStructSubscriptDifferentIndexesGetSet[0] },
        thread: { globalForGlobalStructSubscriptDifferentIndexesGetSet[1] = 12 } )
// CHECK-LABEL: Running GlobalStructSubscriptDifferentIndexesGetSet
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is global

var globalForGlobalClassGeneralMethods = UninstrumentedClass()
testRace(name: "GlobalClassGeneralMethods",
        thread: { _ = globalForGlobalClassGeneralMethods.read() },
        thread: { globalForGlobalClassGeneralMethods.mutate() } )
// CHECK-LABEL: Running GlobalClassGeneralMethods
// CHECK-NOT: ThreadSanitizer: {{.*}} race

var globalForGlobalClassDifferentStoredPropertiesInout = UninstrumentedClass()
testRace(name: "GlobalClassDifferentStoredPropertiesInout",
        thread: { uninstrumentedTakesInout(&globalForGlobalClassDifferentStoredPropertiesInout.storedProperty1) },
        thread: { uninstrumentedTakesInout(&globalForGlobalClassDifferentStoredPropertiesInout.storedProperty2) } )
// CHECK-LABEL: Running GlobalClassDifferentStoredPropertiesInout
// CHECK-NOT: ThreadSanitizer: {{.*}} race

var globalForGlobalClassSubscriptDifferentIndexesInout = UninstrumentedClass()
testRace(name: "GlobalClassSubscriptDifferentIndexesInout",
        thread: { uninstrumentedTakesInout(&globalForGlobalClassSubscriptDifferentIndexesInout[0]) },
        thread: { uninstrumentedTakesInout(&globalForGlobalClassSubscriptDifferentIndexesInout[1]) } )
// CHECK-LABEL: Running GlobalClassSubscriptDifferentIndexesInout
// CHECK-NOT: ThreadSanitizer: {{.*}} race


var globalForGlobalClassSameStoredPropertyInout = UninstrumentedClass()
testRace(name: "GlobalClassSameStoredPropertyInout",
        thread: { uninstrumentedTakesInout(&globalForGlobalClassSameStoredPropertyInout.storedProperty1) },
        thread: { uninstrumentedTakesInout(&globalForGlobalClassSameStoredPropertyInout.storedProperty1) } )
// CHECK-LABEL: Running GlobalClassSameStoredPropertyInout
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is heap block

// These access a global declared in the TSanUninstrumented module
testRace(name: "InoutAccessToStoredGlobalInUninstrumentedModule",
        thread: { uninstrumentedTakesInout(&storedGlobalInUninstrumentedModule1) },
        thread: { uninstrumentedTakesInout(&storedGlobalInUninstrumentedModule1) } )
// CHECK-LABEL: Running InoutAccessToStoredGlobalInUninstrumentedModule
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is global

// These access a global declared in the TSanUninstrumented module.
testRace(name: "ReadAndWriteToStoredGlobalInUninstrumentedModule",
        thread: { storedGlobalInUninstrumentedModule2 = 7 },
        thread: { uninstrumentedBlackHole(storedGlobalInUninstrumentedModule2) } )
// CHECK-LABEL: Running ReadAndWriteToStoredGlobalInUninstrumentedModule
// CHECK: ThreadSanitizer: data race
// CHECK: Location is global

// These access a computed global declared in the TSanUninstrumented module
testRace(name: "InoutAccessToComputedGlobalInUninstrumentedModule",
        thread: { uninstrumentedTakesInout(&computedGlobalInUninstrumentedModule1) },
        thread: { uninstrumentedTakesInout(&computedGlobalInUninstrumentedModule1) } )
// CHECK-LABEL: Running InoutAccessToComputedGlobalInUninstrumentedModule
// CHECK-NOT: ThreadSanitizer: {{.*}} race

// These access a computed global declared in the TSanUninstrumented module
testRace(name: "ReadAndWriteToComputedGlobalInUninstrumentedModule",
        thread: { computedGlobalInUninstrumentedModule2 = 7 },
        thread: { _ = computedGlobalInUninstrumentedModule2 } )
// CHECK-LABEL: Running ReadAndWriteToComputedGlobalInUninstrumentedModule
// CHECK-NOT: ThreadSanitizer: {{.*}} race



// Tests for accesses to stored class properties

var globalForGlobalUninstrumentedClassStoredPropertyMutatingMethod = UninstrumentedClass()
testRace(name: "GlobalUninstrumentedClassStoredPropertyMutatingMethod",
        thread: { _ = globalForGlobalUninstrumentedClassStoredPropertyMutatingMethod.storedStructProperty.read() },
        thread: { globalForGlobalUninstrumentedClassStoredPropertyMutatingMethod.storedStructProperty.mutate() } )
// CHECK-LABEL: Running GlobalUninstrumentedClassStoredPropertyMutatingMethod
// CHECK-NOT: ThreadSanitizer: {{.*}} race

// Note: TSan doesn't see a race above because it doesn't see any load on the
// read side because the getter for the class property is not instrumented.


var globalForGlobalUninstrumentedClassStoredPropertyInout = UninstrumentedClass()
testRace(name: "GlobalUninstrumentedClassStoredPropertyInout",
        thread: { uninstrumentedTakesInout(&globalForGlobalUninstrumentedClassStoredPropertyInout.storedStructProperty.storedProperty1) },
        thread: { uninstrumentedTakesInout(&globalForGlobalUninstrumentedClassStoredPropertyInout.storedStructProperty.storedProperty2) } )
// CHECK-LABEL: Running GlobalUninstrumentedClassStoredPropertyInout
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is heap block

// Note: TSan sees the race above because the inout instrumentation adds an
// ''access'' at the call site to the address returned from materializeForSet


var globalForGlobalUninstrumentedClassComputedPropertyInout = UninstrumentedClass()
testRace(name: "GlobalUninstrumentedClassComputedPropertyInout",
        thread: { uninstrumentedTakesInout(&globalForGlobalUninstrumentedClassComputedPropertyInout.computedStructProperty.storedProperty1) },
        thread: { uninstrumentedTakesInout(&globalForGlobalUninstrumentedClassComputedPropertyInout.computedStructProperty.storedProperty1) } )
// CHECK-LABEL: Running GlobalUninstrumentedClassComputedPropertyInout
// CHECK-NOT: ThreadSanitizer: {{.*}} race

// In the above the write in instrumented code is to the value buffer allocated
// at the call site so there is no data race if the getter and setters themselves
// are synchronized/don't access shared storage. Even with synchronized accessors,
// there is still the possibility of a race condition with lost updates with
// some interleavings of the calls to the getters and setters -- but no data race.

var globalForGlobalInstrumentedClassStoredPropertyMutatingMethod = InstrumentedClass()
testRace(name: "GlobalInstrumentedClassStoredPropertyMutatingMethod",
        thread: { _ = globalForGlobalInstrumentedClassStoredPropertyMutatingMethod.storedStructProperty.read() },
        thread: { globalForGlobalInstrumentedClassStoredPropertyMutatingMethod.storedStructProperty.mutate() } )
// CHECK-LABEL: Running GlobalInstrumentedClassStoredPropertyMutatingMethod
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is heap block
//
// TSan does see this above race because the getter and materializeForSet is instrumented

var globalForGlobalInstrumentedComputedBackedProperty = InstrumentedClass()
testRace(name: "GlobalInstrumentedComputedBackedProperty",
        thread: { _ = globalForGlobalInstrumentedComputedBackedProperty.computedPropertyBackedByStoredProperty },
        thread: { globalForGlobalInstrumentedComputedBackedProperty.computedPropertyBackedByStoredProperty = 77 } )
// CHECK-LABEL: Running GlobalInstrumentedComputedBackedProperty
// CHECK: ThreadSanitizer: data race
// CHECK: Location is heap block
//
// TSan does see this above race because the getter and setter are instrumented
// and write to a shared heap location.

func runLocalTests() {
  runCapturedLocalStructMutatingMethod()
  runCapturedLocalStructDifferentStoredPropertiesInout()
  runCapturedLocalClassGeneralMethods()
  runCapturedLocalDifferentStoredPropertiesInout()
  runCapturedLocalSameStoredPropertyInout()
}

func runCapturedLocalStructMutatingMethod() {
  var l = UninstrumentedStruct()
  testRace(name: "CapturedLocalStructMutatingMethod",
         thread: { _ = l.read() },
         thread: { l.mutate() } )
}
// CHECK-LABEL: Running CapturedLocalStructMutatingMethod
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is heap block


func runCapturedLocalStructDifferentStoredPropertiesInout() {
  var l = UninstrumentedStruct()
  testRace(name: "CapturedLocalStructDifferentStoredPropertiesInout",
         thread: { uninstrumentedTakesInout(&l.storedProperty1) },
         thread: { uninstrumentedTakesInout(&l.storedProperty2) } )
}
// CHECK-LABEL: Running CapturedLocalStructDifferentStoredPropertiesInout
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is heap block


func runCapturedLocalClassGeneralMethods() {
  let l = UninstrumentedClass()
  testRace(name: "CapturedLocalClassGeneralMethods",
          thread: { _ = l.read() },
          thread: { l.mutate() } )
}
// CHECK-LABEL: Running CapturedLocalClassGeneralMethods
// CHECK-NOT: ThreadSanitizer: {{.*}} race


func runCapturedLocalDifferentStoredPropertiesInout() {
  let l = UninstrumentedClass()
  testRace(name: "CapturedLocalClassDifferentStoredPropertiesInout",
          thread: { uninstrumentedTakesInout(&l.storedProperty1) },
          thread: { uninstrumentedTakesInout(&l.storedProperty2) } )
}
// CHECK-LABEL: Running CapturedLocalClassDifferentStoredPropertiesInout
// CHECK-NOT: ThreadSanitizer: {{.*}} race

func runCapturedLocalSameStoredPropertyInout() {
  let l = UninstrumentedClass()
  testRace(name: "CapturedLocalClassSameStoredPropertyInout",
          thread: { uninstrumentedTakesInout(&l.storedProperty1) },
          thread: { uninstrumentedTakesInout(&l.storedProperty1) } )
}
// CHECK-LABEL: Running CapturedLocalClassSameStoredPropertyInout
// CHECK: ThreadSanitizer: Swift access race
// CHECK: Location is heap block

runLocalTests()
