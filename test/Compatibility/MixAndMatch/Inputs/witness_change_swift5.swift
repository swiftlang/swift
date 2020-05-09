
// Swift 4 sees the ObjC class NSRuncibleSpoon as the class, and uses methods
// with type signatures involving NSRuncibleSpoon to conform to protocols
// across the language boundary. Swift 5 sees the type as bridged to
// a RuncibleSpoon value type, but still needs to be able to use conformances
// declared by Swift 4.

// Swift 5

import SomeObjCModule
import SomeSwift4Module

public func testMixAndMatch(bridged: RuncibleSpoon, unbridged: NSRuncibleSpoon) {
  let objcInstanceViaClass
    = SomeObjCClass(someSwiftInitRequirement: bridged)
  let objcClassAsProtocol: SomeSwift4Protocol.Type = SomeObjCClass.self
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
    = SomeSwift4Class(someObjCInitRequirement: unbridged)
  let swiftClassAsProtocol: SomeObjCProtocol.Type = SomeSwift4Class.self
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

public protocol SomeSwift5Protocol {
  init(someSwiftInitRequirement: RuncibleSpoon)
  func someSwiftMethodRequirement(_: RuncibleSpoon)
  var someSwiftPropertyRequirement: RuncibleSpoon { get }
}

extension SomeObjCClass: SomeSwift5Protocol {}

public class SomeSwift5Class: NSObject {
  public required init(someObjCInitRequirement x: RuncibleSpoon) {
    someObjCPropertyRequirement = x
  }
  public func someObjCMethodRequirement(_: RuncibleSpoon) {}
  public var someObjCPropertyRequirement: RuncibleSpoon
}

extension SomeSwift5Class: SomeObjCProtocol {}
