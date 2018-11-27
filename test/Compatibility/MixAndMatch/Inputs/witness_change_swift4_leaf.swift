
// Swift 4 sees the ObjC class NSRuncibleSpoon as the class, and uses methods
// with type signatures involving NSRuncibleSpoon to conform to protocols
// across the language boundary. Swift 5 sees the type as bridged to
// a RuncibleSpoon value type, but still needs to be able to use conformances
// declared by Swift 4.

// Swift 4, importing Swift 4 and Swift 5 code

import SomeObjCModule
import SomeSwift4Module
import SomeSwift5Module

func testMatchAndMix(bridged: RuncibleSpoon, unbridged: NSRuncibleSpoon) {
  let objcInstanceViaClass
    = SomeObjCClass(someSwiftInitRequirement: unbridged)

  let objcClassAsS4Protocol: SomeSwift4Protocol.Type = SomeObjCClass.self
  let objcInstanceViaS4Protocol
    = objcClassAsS4Protocol.init(someSwiftInitRequirement: unbridged)

  let objcClassAsS5Protocol: SomeSwift5Protocol.Type = SomeObjCClass.self
  let objcInstanceViaS5Protocol
    = objcClassAsS5Protocol.init(someSwiftInitRequirement: bridged)

  var bridgedSink: RuncibleSpoon
  var unbridgedSink: NSRuncibleSpoon

  let swiftPropertyViaClass = objcInstanceViaClass.someSwiftPropertyRequirement
  unbridgedSink = swiftPropertyViaClass
  let swiftPropertyViaS4Protocol = objcInstanceViaS4Protocol.someSwiftPropertyRequirement
  unbridgedSink = swiftPropertyViaS4Protocol
  let swiftPropertyViaS5Protocol = objcInstanceViaS5Protocol.someSwiftPropertyRequirement
  bridgedSink = swiftPropertyViaS5Protocol

  objcInstanceViaClass.someSwiftMethodRequirement(unbridged)
  objcInstanceViaS4Protocol.someSwiftMethodRequirement(unbridged)
  objcInstanceViaS5Protocol.someSwiftMethodRequirement(bridged)

  let swift4InstanceViaClass
    = SomeSwift4Class(someObjCInitRequirement: unbridged)
  let swift4ClassAsProtocol: SomeObjCProtocol.Type = SomeSwift4Class.self
  let swift4InstanceViaProtocol
    = swift4ClassAsProtocol.init(someObjCInitRequirement: unbridged)
  
  let objcPropertyViaClassS4 = swift4InstanceViaClass.someObjCPropertyRequirement
  unbridgedSink = objcPropertyViaClassS4
  let objcPropertyViaProtocolS4 = swift4InstanceViaProtocol.someObjCPropertyRequirement
  unbridgedSink = objcPropertyViaProtocolS4

  swift4InstanceViaClass.someObjCMethodRequirement(unbridged)
  swift4InstanceViaProtocol.someObjCMethodRequirement(unbridged)

  let swift5InstanceViaClass
    = SomeSwift5Class(someObjCInitRequirement: bridged)
  let swift5ClassAsProtocol: SomeObjCProtocol.Type = SomeSwift5Class.self
  let swift5InstanceViaProtocol
    = swift5ClassAsProtocol.init(someObjCInitRequirement: unbridged)
  
  let objcPropertyViaClassS5 = swift5InstanceViaClass.someObjCPropertyRequirement
  bridgedSink = objcPropertyViaClassS5
  let objcPropertyViaProtocolS5 = swift5InstanceViaProtocol.someObjCPropertyRequirement
  unbridgedSink = objcPropertyViaProtocolS5

  swift5InstanceViaClass.someObjCMethodRequirement(bridged)
  swift5InstanceViaProtocol.someObjCMethodRequirement(unbridged)

  _ = bridgedSink
  _ = unbridgedSink
}

