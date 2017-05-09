
// Swift 3 sees the ObjC class NSRuncibleSpoon as the class, and uses methods
// with type signatures involving NSRuncibleSpoon to conform to protocols
// across the language boundary. Swift 4 sees the type as bridged to
// a RuncibleSpoon value type, but still needs to be able to use conformances
// declared by Swift 3.

// Swift 3, importing Swift 3 and Swift 4 code

import SomeObjCModule
import SomeSwift3Module
import SomeSwift4Module

func testMatchAndMix(bridged: RuncibleSpoon, unbridged: NSRuncibleSpoon) {
  let objcInstanceViaClass
    = SomeObjCClass(someSwiftInitRequirement: unbridged)

  let objcClassAsS3Protocol: SomeSwift3Protocol.Type = SomeObjCClass.self
  let objcInstanceViaS3Protocol
    = objcClassAsS3Protocol.init(someSwiftInitRequirement: unbridged)

  let objcClassAsS4Protocol: SomeSwift4Protocol.Type = SomeObjCClass.self
  let objcInstanceViaS4Protocol
    = objcClassAsS4Protocol.init(someSwiftInitRequirement: bridged)

  var bridgedSink: RuncibleSpoon
  var unbridgedSink: NSRuncibleSpoon

  let swiftPropertyViaClass = objcInstanceViaClass.someSwiftPropertyRequirement
  unbridgedSink = swiftPropertyViaClass
  let swiftPropertyViaS3Protocol = objcInstanceViaS3Protocol.someSwiftPropertyRequirement
  unbridgedSink = swiftPropertyViaS3Protocol
  let swiftPropertyViaS4Protocol = objcInstanceViaS4Protocol.someSwiftPropertyRequirement
  bridgedSink = swiftPropertyViaS4Protocol

  objcInstanceViaClass.someSwiftMethodRequirement(unbridged)
  objcInstanceViaS3Protocol.someSwiftMethodRequirement(unbridged)
  objcInstanceViaS4Protocol.someSwiftMethodRequirement(bridged)

  let swift3InstanceViaClass
    = SomeSwift3Class(someObjCInitRequirement: unbridged)
  let swift3ClassAsProtocol: SomeObjCProtocol.Type = SomeSwift3Class.self
  let swift3InstanceViaProtocol
    = swift3ClassAsProtocol.init(someObjCInitRequirement: unbridged)
  
  let objcPropertyViaClassS3 = swift3InstanceViaClass.someObjCPropertyRequirement
  unbridgedSink = objcPropertyViaClassS3
  let objcPropertyViaProtocolS3 = swift3InstanceViaProtocol.someObjCPropertyRequirement
  unbridgedSink = objcPropertyViaProtocolS3

  swift3InstanceViaClass.someObjCMethodRequirement(unbridged)
  swift3InstanceViaProtocol.someObjCMethodRequirement(unbridged)

  let swift4InstanceViaClass
    = SomeSwift4Class(someObjCInitRequirement: bridged)
  let swift4ClassAsProtocol: SomeObjCProtocol.Type = SomeSwift4Class.self
  let swift4InstanceViaProtocol
    = swift4ClassAsProtocol.init(someObjCInitRequirement: unbridged)
  
  let objcPropertyViaClassS4 = swift4InstanceViaClass.someObjCPropertyRequirement
  bridgedSink = objcPropertyViaClassS4
  let objcPropertyViaProtocolS4 = swift4InstanceViaProtocol.someObjCPropertyRequirement
  unbridgedSink = objcPropertyViaProtocolS4

  swift4InstanceViaClass.someObjCMethodRequirement(bridged)
  swift4InstanceViaProtocol.someObjCMethodRequirement(unbridged)

  _ = bridgedSink
  _ = unbridgedSink
}

