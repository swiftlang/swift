// RUN: %swift -parse %s -verify

protocol P {
	func method() -> Int
}
func address_only_bind<T: P>(x: T?) -> Int {
  var y = x!.method()
  return y
}
