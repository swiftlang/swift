import RoundTrip

class Class {}

let c = Class()

weak var weakVar: Class? = c
unowned let unownedVar: Class = c
unowned(unsafe) let unmanagedVar: Class = c

public func test() {
  roundTripType(type(of: weakVar))
  roundTripType(type(of: unownedVar))
  roundTripType(type(of: unmanagedVar))
}
