// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -enable-testing -O

public func publicNoArgs() {}
public func publicSomeArgs(_: Int, x: Int) {}
public func publicWithDefault(_: Int = 0) {}

internal func internalNoArgs() {}
internal func internalSomeArgs(_: Int, x: Int) {}
internal func internalWithDefault(_: Int = 0) {}

private func privateNoArgs() {}
private func privateSomeArgs(_: Int, x: Int) {}
private func privateWithDefault(_: Int = 0) {}

@_cdecl("c_publicNoArgs") public func publicNoArgsCDecl() {}
@_cdecl("c_publicSomeArgs") public func publicSomeArgsCDecl(_: Int, x: Int) {}
@_cdecl("c_publicWithDefault") public func publicWithDefaultCDecl(_: Int = 0) {}

@_cdecl("c_internalNoArgs") internal func internalNoArgsCDecl() {}
@_cdecl("c_internalSomeArgs") internal func internalSomeArgsCDecl(_: Int, x: Int) {}
@_cdecl("c_internalWithDefault") internal func internalWithDefaultCDecl(_: Int = 0) {}
