// RUN: not %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s 2>&1 | %FileCheck %s

// FIXME: TBDGen is incorrect:
// CHECK: symbol '_T04test9publicLetSiv' (test.publicLet : Swift.Int) is in generated IR file, but not in TBD file
// CHECK: symbol '_T04test9publicLetSifau' (test.publicLet.unsafeMutableAddressor : Swift.Int) is in generated IR file, but not in TBD file

public let publicLet: Int = 0

public var publicVarGet: Int { get { return 0 } }
var privateVarGet: Int { get { return 0 } }
public var publicVarGetSet: Int {
    get { return 0 }
    set {}
}
var privateVarGetSet: Int {
    get { return 0 }
    set {}
}
