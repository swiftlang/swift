// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

class C {
  init() {}
}
struct Weak<T: AnyObject> {
  weak var rawValue: T? = nil
}
typealias Tuple = (instanceIdentifier: C, object: Weak<C>)
let object = C()
let tuple: Tuple = (instanceIdentifier: .init(), object: .init(rawValue: object))
let closureResult = [tuple].map { $0.object.rawValue }.first
let keypathResult = [tuple].map(\.object.rawValue).first
print("Closure result: \(String(describing: closureResult))")
// CHECK: Closure result: Optional(Optional(main.C))
print("Keypath result: \(String(describing: keypathResult))")
// CHECK: Keypath result: Optional(Optional(main.C))
withExtendedLifetime([object]) { }
