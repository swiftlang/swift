// RUN: %target-parse-verify-swift

protocol P {
	func method() -> Int
}
func address_only_bind<T: P>(_ x: T?) -> Int {
  let y = x!.method()
  return y
}
