// RUN: %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s

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
