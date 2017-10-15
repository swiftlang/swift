
// Swift 3 sees the ObjC class NSRuncibleSpoon as the class, and uses methods
// with type signatures involving NSRuncibleSpoon to conform to protocols
// across the language boundary. Swift 4 sees the type as bridged to
// a RuncibleSpoon value type, but still needs to be able to use conformances
// declared by Swift 3.

// Swift 4

import SomeObjCModule
import SomeSwift3Module

public func testMixAndMatch(bridged: RuncibleSpoon, unbridged: NSRuncibleSpoon) {
  let objcInstanceViaClass
    = SomeObjCClass(someSwiftInitRequirement: bridged)
  let objcClassAsProtocol: SomeSwift3Protocol.Type = SomeObjCClass.self
  let objcInstanceViaProtocol
    = objcClassAsProtocol.init(someSwiftInitRequirement: unbridged)

  var bridgedSink: RuncibleSpoon
  var unbridgedSink: NSRuncibleSpoon

  let swiftPropertyViaClass = objcInstanceViaClass.someSwiftPropertyRequirement
  bridgedSink = swiftPropertyViaClass
  let swiftPropertyViaProtocol = objcInstanceViaProtocol.someSwiftPropertyRequirement
  unbridgedSink = swiftPropertyViaProtocol

  objcInstanceViaClass.someSwiftMethodRequirement(bridged)
  objcInstanceViaProtocol.someSwiftMethodRequirement(unbridged)

  let swiftInstanceViaClass
    = SomeSwift3Class(someObjCInitRequirement: unbridged)
  let swiftClassAsProtocol: SomeObjCProtocol.Type = SomeSwift3Class.self
  let swiftInstanceViaProtocol
    = swiftClassAsProtocol.init(someObjCInitRequirement: bridged)
  
  let objcPropertyViaClass = swiftInstanceViaClass.someObjCPropertyRequirement
  unbridgedSink = objcPropertyViaClass
  let objcPropertyViaProtocol = swiftInstanceViaProtocol.someObjCPropertyRequirement
  bridgedSink = objcPropertyViaProtocol

  swiftInstanceViaClass.someObjCMethodRequirement(unbridged)
  swiftInstanceViaProtocol.someObjCMethodRequirement(bridged)

  _ = bridgedSink
  _ = unbridgedSink
}

public protocol SomeSwift4Protocol {
  init(someSwiftInitRequirement: RuncibleSpoon)
  func someSwiftMethodRequirement(_: RuncibleSpoon)
  var someSwiftPropertyRequirement: RuncibleSpoon { get }
}

extension SomeObjCClass: SomeSwift4Protocol {}

public class SomeSwift4Class: NSObject {
  public required init(someObjCInitRequirement x: RuncibleSpoon) {
    someObjCPropertyRequirement = x
  }
  public func someObjCMethodRequirement(_: RuncibleSpoon) {}
  public var someObjCPropertyRequirement: RuncibleSpoon
}

extension SomeSwift4Class: SomeObjCProtocol {}
