// RUN: %target-swift-frontend -emit-ir -o- -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 3 %s
// RUN: %target-swift-frontend -emit-ir -o- -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s

public func publicNoArgs() {}
public func publicSomeArgs(_: Int, x: Int) {}
public func publicWithDefault(_: Int = 0) {}

internal func internalNoArgs() {}
internal func internalSomeArgs(_: Int, x: Int) {}
internal func internalWithDefault(_: Int = 0) {}

private func privateNoArgs() {}
private func privateSomeArgs(_: Int, x: Int) {}
private func privateWithDefault(_: Int = 0) {}
