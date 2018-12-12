// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -enable-testing -O

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/typecheck.tbd
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/emit-ir.tbd
// RUN: diff -u %t/typecheck.tbd %t/emit-ir.tbd

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

@_silgen_name("silgen_publicNoArgs") public func publicNoArgsSilgenNameDecl()
@_silgen_name("silgen_publicSomeArgs") public func publicSomeArgsSilgenNameDecl(_: Int, x: Int)

@_silgen_name("silgen_internalNoArgs") internal func internalNoArgsSilgenNameDecl()
@_silgen_name("silgen_internalSomeArgs") internal func internalSomeArgsSilgenNameDecl(_: Int, x: Int)
