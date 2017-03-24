// RUN: %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s

public let publicLet: Int = 0
let internalLet: Int = 0

public var publicVar: Int = 0
var internalVar: Int = 0

public var publicVarGet: Int { get { return 0 } }
var internalVarGet: Int { get { return 0 } }
public var publicVarGetSet: Int {
    get { return 0 }
    set {}
}
var internalVarGetSet: Int {
    get { return 0 }
    set {}
}
