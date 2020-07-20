// RUN: %target-swiftc_driver -O -Rpass-missed=sil-opt-remark-gen -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil %s -o /dev/null -Xfrontend -verify

public class Klass {}

public var global = Klass()

@inline(never)
public func getGlobal() -> Klass {
    return global // expected-remark @:5 {{Unable to remove retain}}
}

public func useGlobal() {
    let x = getGlobal()
    // Make sure that the retain msg is at the beginning of the print and the
    // releases are the end of the print.
    print(x) // expected-remark @:5 {{Unable to remove retain}}
    // expected-remark @-1:12 {{Unable to remove release}}
    // expected-remark @-2:12 {{Unable to remove release}}
}
