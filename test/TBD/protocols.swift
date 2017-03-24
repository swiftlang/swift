// RUN: not %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s 2>&1 | %FileCheck %s

// FIXME: TBDGen is incorrect:
// CHECK: symbol '_T04test07PrivatebB6StructV13privateVarGetSifg' (test.PrivatePrivatePrivateStruct.privateVarGet.getter : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructV16privateVarGetSetSifm' (test.PublicPrivatePrivateStruct.privateVarGetSet.materializeForSet : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructV16privateVarGetSetSifs' (test.PublicPrivatePrivateStruct.privateVarGetSet.setter : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test06PublicbB6StructV15publicVarGetSetSivfi' (test.PublicPublicPublicStruct.(publicVarGetSet : Swift.Int).(variable initialization expression)) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructV13privateMethodyyF' (test.PublicPrivatePrivateStruct.privateMethod () -> ()) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVAA0B0A2aDP13privateVarGetSifgTW' (protocol witness for test.Private.privateVarGet.getter : Swift.Int in conformance test.PrivatePrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructVAA0C0A2aDP16privateVarGetSetSifmTW' (protocol witness for test.Private.privateVarGetSet.materializeForSet : Swift.Int in conformance test.PublicPrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVAA0B0AAWP' (protocol witness table for test.PrivatePrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructVAA0C0A2aDP13privateMethodyyFTW' (protocol witness for test.Private.privateMethod () -> () in conformance test.PublicPrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVAA0B0AAWa' (protocol witness table accessor for test.PrivatePrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVAA0B0A2aDP16privateVarGetSetSifmTW' (protocol witness for test.Private.privateVarGetSet.materializeForSet : Swift.Int in conformance test.PrivatePrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructV12publicMethodyyF' (test.PrivatePublicPrivateStruct.publicMethod () -> ()) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVMa' (type metadata accessor for test.PrivatePrivatePrivateStruct) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVAA0B0A2aDP13privateMethodyyFTW' (protocol witness for test.Private.privateMethod () -> () in conformance test.PrivatePrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test06PublicbB6StructVAA0B0A2aDP12publicMethodyyFTW' (protocol witness for test.Public.publicMethod () -> () in conformance test.PublicPublicPublicStruct : test.Public in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVMa' (type metadata accessor for test.PrivatePublicPrivateStruct) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVN' (type metadata for test.PrivatePublicPrivateStruct) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVMn' (nominal type descriptor for test.PrivatePrivatePrivateStruct) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVMn' (nominal type descriptor for test.PrivatePublicPrivateStruct) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructV16privateVarGetSetSifm' (test.PrivatePrivatePrivateStruct.privateVarGetSet.materializeForSet : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructV16privateVarGetSetSifg' (test.PrivatePrivatePrivateStruct.privateVarGetSet.getter : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructV16privateVarGetSetSifs' (test.PrivatePrivatePrivateStruct.privateVarGetSet.setter : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateB6StructVAA0C0AAWP' (protocol witness table for test.PublicPrivatePublicStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateB6StructVAA0C0A2aDP13privateMethodyyFTW' (protocol witness for test.Private.privateMethod () -> () in conformance test.PublicPrivatePublicStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateB6StructVAA0C0AAWa' (protocol witness table accessor for test.PublicPrivatePublicStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVAA0C0A2aDP15publicVarGetSetSifmTW' (protocol witness for test.Public.publicVarGetSet.materializeForSet : Swift.Int in conformance test.PrivatePublicPrivateStruct : test.Public in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVWV' (value witness table for test.PrivatePrivatePrivateStruct) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVN' (type metadata for test.PrivatePrivatePrivateStruct) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructVAA0C0A2aDP13privateVarGetSifgTW' (protocol witness for test.Private.privateVarGet.getter : Swift.Int in conformance test.PublicPrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVAA0C0A2aDP12publicVarGetSifgTW' (protocol witness for test.Public.publicVarGet.getter : Swift.Int in conformance test.PrivatePublicPrivateStruct : test.Public in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVWV' (value witness table for test.PrivatePublicPrivateStruct) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVAA0C0AAWP' (protocol witness table for test.PrivatePublicPrivateStruct : test.Public in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructVAA0C0AAWP' (protocol witness table for test.PublicPrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructV13privateVarGetSifg' (test.PublicPrivatePrivateStruct.privateVarGet.getter : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateB6StructV16privateVarGetSetSivfi' (test.PublicPrivatePublicStruct.(privateVarGetSet : Swift.Int).(variable initialization expression)) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructVAA0C0A2aDP16privateVarGetSetSifgTW' (protocol witness for test.Private.privateVarGetSet.getter : Swift.Int in conformance test.PublicPrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVAA0C0AAWa' (protocol witness table accessor for test.PrivatePublicPrivateStruct : test.Public in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructVAA0C0AAWa' (protocol witness table accessor for test.PublicPrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructVAA0C0A2aDP16privateVarGetSetSifsTW' (protocol witness for test.Private.privateVarGetSet.setter : Swift.Int in conformance test.PublicPrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateB6StructV13privateVarGetSivfi' (test.PublicPrivatePublicStruct.(privateVarGet : Swift.Int).(variable initialization expression)) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVAA0B0A2aDP16privateVarGetSetSifgTW' (protocol witness for test.Private.privateVarGetSet.getter : Swift.Int in conformance test.PrivatePrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test7PrivateMp' (protocol descriptor for test.Private) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructVAA0B0A2aDP16privateVarGetSetSifsTW' (protocol witness for test.Private.privateVarGetSet.setter : Swift.Int in conformance test.PrivatePrivatePrivateStruct : test.Private in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructV15publicVarGetSetSifg' (test.PrivatePublicPrivateStruct.publicVarGetSet.getter : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructV15publicVarGetSetSifm' (test.PrivatePublicPrivateStruct.publicVarGetSet.materializeForSet : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructV15publicVarGetSetSifs' (test.PrivatePublicPrivateStruct.publicVarGetSet.setter : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test06PublicbB6StructVAA0B0AAWP' (protocol witness table for test.PublicPublicPublicStruct : test.Public in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test07PrivatebB6StructV13privateMethodyyF' (test.PrivatePrivatePrivateStruct.privateMethod () -> ()) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVAA0C0A2aDP12publicMethodyyFTW' (protocol witness for test.Public.publicMethod () -> () in conformance test.PrivatePublicPrivateStruct : test.Public in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructV12publicVarGetSifg' (test.PrivatePublicPrivateStruct.publicVarGet.getter : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test06PublicbB6StructVAA0B0AAWa' (protocol witness table accessor for test.PublicPublicPublicStruct : test.Public in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVAA0C0A2aDP15publicVarGetSetSifgTW' (protocol witness for test.Public.publicVarGetSet.getter : Swift.Int in conformance test.PrivatePublicPrivateStruct : test.Public in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PublicPrivateC6StructV16privateVarGetSetSifg' (test.PublicPrivatePrivateStruct.privateVarGetSet.getter : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test06PublicbB6StructV12publicVarGetSivfi' (test.PublicPublicPublicStruct.(publicVarGet : Swift.Int).(variable initialization expression)) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test013PrivatePublicB6StructVAA0C0A2aDP15publicVarGetSetSifsTW' (protocol witness for test.Public.publicVarGetSet.setter : Swift.Int in conformance test.PrivatePublicPrivateStruct : test.Public in test) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test06PublicbB6StructV15publicVarGetSetSifs' (test.PublicPublicPublicStruct.publicVarGetSet.setter : Swift.Int) is in TBD file, but not in generated IR
// CHECK: symbol '_T04test013PublicPrivateB6StructV16privateVarGetSetSifm' (test.PublicPrivatePublicStruct.privateVarGetSet.materializeForSet : Swift.Int) is in TBD file, but not in generated IR

public protocol Public {
    func publicMethod()
    associatedtype PublicAT
    var publicVarGet: Int { get }
    var publicVarGetSet: Int { get set }
}
protocol Private {
    func privateMethod()
    associatedtype PrivateAT
    var privateVarGet: Int { get }
    var privateVarGetSet: Int { get set }
}

// Naming scheme: type access, protocol access, witness access, type kind

public struct PublicPublicPublicStruct: Public {
    public func publicMethod() {}
    public typealias PublicAT = Int
    public let publicVarGet: Int = 0
    public var publicVarGetSet: Int = 0
}

public struct PublicPrivatePublicStruct: Private {
    public func privateMethod() {}
    public typealias PrivateAT = Int
    public let privateVarGet: Int = 0
    public var privateVarGetSet: Int = 0
}

public struct PublicPrivatePrivateStruct: Private {
    func privateMethod() {}
    typealias PrivateAT = Int
    let privateVarGet: Int = 0
    var privateVarGetSet: Int = 0
}

struct PrivatePublicPrivateStruct: Public {
    func publicMethod() {}
    typealias PublicAT = Int
    let publicVarGet: Int = 0
    var publicVarGetSet: Int = 0
}

struct PrivatePrivatePrivateStruct: Private {
    func privateMethod() {}
    typealias PrivateAT = Int
    let privateVarGet: Int = 0
    var privateVarGetSet: Int = 0
}
