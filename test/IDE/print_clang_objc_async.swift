// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -print-implicit-attrs -source-filename %s -module-to-print=ObjCConcurrency -function-definitions=false -enable-experimental-concurrency > %t/ObjCConcurrency.printed.txt
// RUN: %FileCheck -input-file %t/ObjCConcurrency.printed.txt %s

// REQUIRES: objc_interop
// REQUIRES: concurrency
import _Concurrency

// CHECK-LABEL: class SlowServer : NSObject, ServiceProvider {

// CHECK: @available(*, renamed: "doSomethingSlow(_:)")
// CHECK-NEXT: func doSomethingSlow(_ operation: String, completionHandler handler: @escaping @Sendable (Int) -> Void)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func doSomethingSlow(_ operation: String) async -> Int

// CHECK: @available(*, renamed: "doSomethingDangerous(_:)")
// CHECK-NEXT: func doSomethingDangerous(_ operation: String, completionHandler handler: (@Sendable (String?, Error?) -> Void)? = nil)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func doSomethingDangerous(_ operation: String) async throws -> String

// CHECK: @available(*, renamed: "doSomethingReckless(_:)")
// CHECK-NEXT: func doSomethingReckless(_ operation: String, completionHandler handler: ((String?, Error?) -> Void)? = nil)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func doSomethingReckless(_ operation: String) async throws -> String

// CHECK: @available(*, renamed: "checkAvailability()")
// CHECK-NEXT: func checkAvailability(completionHandler: @escaping @Sendable (Bool) -> Void)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func checkAvailability() async -> Bool

// CHECK: @available(*, renamed: "anotherExample()")
// CHECK-NEXT: func anotherExample(completionBlock block: @escaping @Sendable (String) -> Void)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func anotherExample() async -> String

// CHECK: @available(*, renamed: "finalExample()")
// CHECK-NEXT: func finalExampleWithReply(to block: @escaping @Sendable (String) -> Void)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func finalExample() async -> String

// CHECK: @available(*, renamed: "replyingOperation(_:)")
// CHECK-NEXT: func replyingOperation(_ operation: String, replyTo block: @escaping @Sendable (String) -> Void)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func replyingOperation(_ operation: String) async -> String

// CHECK: @available(*, renamed: "findAnswer()")
// CHECK-NEXT: func findAnswer(completionHandler handler: @escaping @Sendable (String?, Error?) -> Void)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func findAnswer() async throws -> String

// CHECK: @available(*, renamed: "findAnswerFailingly()")
// CHECK-NEXT: func findAnswerFailingly(completionHandler handler: @escaping @Sendable (String?, Error?) -> Void) throws
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func findAnswerFailingly() async throws -> String

// CHECK: @available(*, renamed: "findQAndA()")
// CHECK-NEXT: func findQAndA(completionHandler handler: @escaping @Sendable (String?, String?, Error?) -> Void)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func findQAndA() async throws -> (String?, String)

// CHECK: @available(*, renamed: "findQuestionableAnswers()")
// CHECK-NEXT: func findQuestionableAnswers(completionHandler handler: @escaping CompletionHandler)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func findQuestionableAnswers() async throws -> (String, String?)

// CHECK: @available(*, renamed: "findAnswerableQuestions()")
// CHECK-NEXT: func findAnswerableQuestions(completionHandler handler: @escaping @Sendable (String?, String?, Error?) -> Void)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func findAnswerableQuestions() async throws -> (String, String?)

// CHECK: @available(*, renamed: "findUnanswerableQuestions()")
// CHECK-NEXT: func findUnanswerableQuestions(completionHandler handler: @escaping NonsendableCompletionHandler)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func findUnanswerableQuestions() async throws -> (String, String?)

// CHECK: @available(*, renamed: "doSomethingFun(_:)")
// CHECK-NEXT: func doSomethingFun(_ operation: String, then completionHandler: @escaping @Sendable () -> Void)
// CHECK-NEXT: func doSomethingFun(_ operation: String) async

// CHECK: @available(*, renamed: "doSomethingConflicted(_:)")
// CHECK-NEXT: func doSomethingConflicted(_ operation: String, completionHandler handler: @escaping @Sendable (Int) -> Void)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func doSomethingConflicted(_ operation: String) async -> Int
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func doSomethingConflicted(_ operation: String) -> Int

// CHECK: func dance(_ step: String) async -> String
// CHECK: func __leap(_ height: Int) async -> String

// CHECK: @available(*, renamed: "runOnMainThread()")
// CHECK-NEXT: func runOnMainThread(completionHandler completion: (@MainActor (String) -> Void)? = nil)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func runOnMainThread() async -> String

// CHECK: @available(*, renamed: "asyncImportSame(_:)")
// CHECK-NEXT: func asyncImportSame(_ operation: String, completionHandler handler: @escaping @Sendable (Int) -> Void)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func asyncImportSame(_ operation: String) async -> Int
// CHECK-NEXT: func asyncImportSame(_ operation: String, replyTo handler: @escaping (Int) -> Void)
// CHECK-NOT: func asyncImportSame(_ operation: String) async -> Int

// CHECK: {{^[}]$}}

// CHECK-LABEL: protocol RefrigeratorDelegate
// CHECK-NEXT: func someoneDidOpenRefrigerator(_ fridge: Any)
// CHECK-NEXT: func refrigerator(_ fridge: Any, didGetFilledWithItems items: [Any])
// CHECK-NEXT: {{^}} @objc func refrigerator(_ fridge: Any, didGetFilledWithIntegers items: UnsafeMutablePointer<Int>, count: Int)
// CHECK-NEXT: {{^}} @objc func refrigerator(_ fridge: Any, willAddItem item: Any)
// CHECK-NEXT: @discardableResult
// CHECK-NEXT: {{^}} @objc func refrigerator(_ fridge: Any, didRemoveItem item: Any) -> Bool
// CHECK-NEXT: {{^[}]$}}

// CHECK-LABEL: protocol ProtocolWithSwiftAttributes {
// CHECK-NEXT: nonisolated func independentMethod()
// CHECK-NEXT: nonisolated func nonisolatedMethod()
// CHECK-NEXT: {{^}} @MainActor @objc func mainActorMethod()
// CHECK-NEXT: {{^}} @MainActor @objc func uiActorMethod()
// CHECK-NEXT: {{^}} @objc optional func missingAtAttributeMethod()
// CHECK-NEXT: {{^[}]$}}

// CHECK: {{^}}nonisolated var MAGIC_NUMBER: Int32 { get }

// CHECK: func doSomethingConcurrently(_ block: @Sendable () -> Void)

// CHECK: @MainActor @objc protocol TripleMainActor {

// CHECK-LABEL: class NXSender :
// CHECK: func sendAny(_ obj: Sendable) -> Sendable
// CHECK: func sendOptionalAny(_ obj: Sendable?) -> Sendable?
// CHECK: func sendSendable(_ sendable: SendableClass & Sendable) -> SendableClass & Sendable
// CHECK: func sendSendableSubclasses(_ sendableSubclass: NonSendableClass & Sendable) -> NonSendableClass & Sendable
// CHECK: func sendProto(_ obj: LabellyProtocol & Sendable) -> LabellyProtocol & Sendable
// CHECK: func sendProtos(_ obj: LabellyProtocol & ObjCClub & Sendable) -> LabellyProtocol & ObjCClub & Sendable
// CHECK: func sendAnyArray(_ array: [Sendable]) -> [Sendable]
// CHECK: func sendGeneric(_ generic: GenericObject<SendableClass> & Sendable) -> GenericObject<SendableClass> & Sendable
// CHECK: func sendPtr(_ val: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer
// CHECK: func sendStringArray(_ obj: [String]) -> [String]
// CHECK: func sendAnyTypedef(_ obj: Sendable) -> Sendable
// CHECK: func sendAnyTypedefs(_ objs: [Sendable]) -> [Sendable]
// CHECK: func sendBlockTypedef(_ block: @escaping @Sendable (Any) -> Void) -> @Sendable (Any) -> Void
// CHECK: func sendBlockTypedefs(_ blocks: [@Sendable @convention(block) (Any) -> Void]) -> [@Sendable @convention(block) (Any) -> Void]
// CHECK: func sendUnbound(_ array: [Sendable]) -> [Sendable]
// CHECK: var sendableProp: Sendable
// CHECK: }

// CHECK: func NXSendFunc(_ arg: Sendable) -> Sendable
// CHECK: var NXSendGlobal: Sendable

// CHECK-LABEL: struct StructWithSendableContents
// FIXME: `Unmanaged` should support `AnyObject & Sendable`.
// CHECK: var sendableField: Unmanaged<AnyObject>
// FIXME: Should be imported as Unmanaged!
// CHECK: var sendableIndirectField: Sendable & AnyObject
// CHECK: var sendableComputed: Sendable { get }
