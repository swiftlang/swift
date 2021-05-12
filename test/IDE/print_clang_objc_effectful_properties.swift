// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -print-implicit-attrs -source-filename %s -module-to-print=EffectfulProperties -function-definitions=false > %t/EffectfulProperties.printed.txt
// RUN: %FileCheck -input-file %t/EffectfulProperties.printed.txt %s

// REQUIRES: objc_interop

// CHECK-LABEL: class EffProps : NSObject {
// CHECK:  func getDogWithCompletion(_ completionHandler: @escaping (NSObject) -> Void)
// CHECK:  var doggo: NSObject { get async }

// CHECK:       func obtainCat(_ completionHandler: @escaping (NSObject?, Error?) -> Void)
// CHECK-NEXT:  var catto: NSObject? { get async throws }

// CHECK:       func checkAvailability(completionHandler: @escaping (Bool) -> Void)
// CHECK-NEXT:  var available: Bool { get async }
// CHECK-NEXT:  @available(swift, obsoleted: 3, renamed: "checkAvailability(completionHandler:)")
// CHECK-NEXT:  func checkAvailabilityWithCompletionHandler(_ completionHandler: @escaping (Bool) -> Void)

// CHECK:       func returnNothing(completion completionHandler: @escaping () -> Void)
// CHECK-NEXT:  var touch: Void { get async }
// CHECK-NEXT:  @available(swift, obsoleted: 3, renamed: "returnNothing(completion:)")
// CHECK-NEXT:  func returnNothingWithCompletion(_ completionHandler: @escaping () -> Void)

// CHECK:       func nullableHandler(_ completion: ((String) -> Void)? = nil)
// CHECK-NEXT:  var fromNullableHandler: String { get async }

// CHECK:  func getMainDog(_ completion: @escaping (String) -> Void)
// CHECK-NEXT:  var mainDogProp: String { get async }

// CHECK:  @completionHandlerAsync("regularMainDog()", completionHandlerIndex: 0)
// CHECK-NEXT:  func regularMainDog(_ completion: @escaping (String) -> Void)
// CHECK-NEXT:  @discardableResult
// CHECK-NEXT:  func regularMainDog() async -> String
// CHECK: }

// CHECK-LABEL: class NotEffProps : NSObject {
// CHECK-NOT: var
// CHECK: func EffPropGetDogWithCompletion(_ s: OpaquePointer, _ myBlock: @escaping (NSObject) -> Void) -> Double
