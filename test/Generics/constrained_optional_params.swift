// RUN: %target-parse-verify-swift

protocol P {
	func method() -> Int
}
func address_only_bind<T: P>(x: T?) -> Int {
  var y = x!.method()
  return y
}
