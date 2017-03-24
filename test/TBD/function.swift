// RUN: %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s

public func publicNoArgs() {}
public func publicSomeArgs(_: Int, x: Int) {}

func internalNoArgs() {}
func internalSomeArgs(_: Int, x: Int) {}
