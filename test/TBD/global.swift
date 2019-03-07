// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -enable-resilience %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -enable-resilience -enable-testing %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -enable-resilience %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s -enable-testing -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -enable-resilience -enable-testing %s -O

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/typecheck.tbd
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/emit-ir.tbd
// RUN: diff -u %t/typecheck.tbd %t/emit-ir.tbd

public let publicLet: Int = 0
internal let internalLet: Int = 0
private let privateLet: Int = 0

public var publicVar: Int = 0
internal var internalVar: Int = 0
private var privateVar: Int = 0

public var publicVarGet: Int { get { return 0 } }
internal var internalVarGet: Int { get { return 0 } }
private var privateVarGet: Int { get { return 0 } }

public var publicVarGetSet: Int {
    get { return 0 }
    set {}
}
internal var internalVarGetSet: Int {
    get { return 0 }
    set {}
}
private var privateVarGetSet: Int {
    get { return 0 }
    set {}
}
