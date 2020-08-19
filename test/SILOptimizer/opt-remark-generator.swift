// RUN: %target-swiftc_driver -O -Rpass-missed=sil-opt-remark-gen -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil %s -o /dev/null -Xfrontend -verify
// REQUIRES: optimized_stdlib

// XFAIL: OS=linux-androideabi && CPU=armv7

public class Klass {}

public var global = Klass()

@inline(never)
public func getGlobal() -> Klass {
    return global // expected-remark @:5 {{retain of type 'Klass'}}
                  // expected-note @-5:12 {{of 'global'}}
}

public func useGlobal() {
    let x = getGlobal()
    // Make sure that the retain msg is at the beginning of the print and the
    // releases are the end of the print.
    print(x) // expected-remark @:5 {{retain of type 'Klass'}}
             // expected-note @-4:9 {{of 'x'}}
             // expected-remark @-2:12 {{release of type}}
             // expected-remark @-3:12 {{release of type 'Klass'}}
             // expected-note @-7:9 {{of 'x'}}
}

public enum TrivialState {
case first
case second
case third
}

struct StructWithOwner {
    var owner = Klass()
    var state = TrivialState.first
}

func printStructWithOwner(x : StructWithOwner) {
    print(x) // expected-remark {{retain of type 'Klass'}}
             // expected-note @-2:27 {{of 'x.owner'}}
             // We should be able to infer the arg here.
             // expected-remark @-3:12 {{release of type}}
}

func printStructWithOwnerOwner(x : StructWithOwner) {
    print(x.owner) // expected-remark {{retain of type 'Klass'}}
                   // expected-note @-2:32 {{of 'x.owner'}}
                   // We should be able to infer the arg here.
                   // expected-remark @-3:18 {{release of type}}
}

func returnStructWithOwnerOwner(x: StructWithOwner) -> Klass {
    return x.owner // expected-remark {{retain of type 'Klass'}}
                   // expected-note @-2:33 {{of 'x.owner'}}
}

func callingAnInitializerStructWithOwner(x: Klass) -> StructWithOwner {
    return StructWithOwner(owner: x) // expected-remark {{retain of type 'Klass'}}
                                     // expected-note @-2:42 {{of 'x'}}
}

struct KlassPair {
    var lhs: Klass
    var rhs: Klass
}

func printKlassPair(x : KlassPair) {
    // We pattern match columns to ensure we get retain on the p and release on
    // the end ')'
    print(x) // expected-remark @:5 {{retain of type 'Klass'}}
             // expected-note @-4:21 {{of 'x.lhs'}}
             // expected-remark @-2:5 {{retain of type 'Klass'}}
             // expected-note @-6:21 {{of 'x.rhs'}}
             // This is a release for Array<Any> for print.
             // expected-remark @-5:12 {{release of type}}
}

func printKlassPairLHS(x : KlassPair) {
    // We print the remarks at the 'p' and at the ending ')'.
    print(x.lhs) // expected-remark @:5 {{retain of type 'Klass'}}
                 // expected-note @-3:24 {{of 'x.lhs'}}
                 // Release for Array<Any> needed for print.
                 // expected-remark @-3:16 {{release of type}}
}

func returnKlassPairLHS(x: KlassPair) -> Klass {
    return x.lhs // expected-remark @:14 {{retain of type 'Klass'}}
                 // expected-note @-2:25 {{of 'x.lhs'}}
}

func callingAnInitializerKlassPair(x: Klass, y: Klass) -> KlassPair {
    return KlassPair(lhs: x, rhs: y) // expected-remark {{retain of type 'Klass'}}
                                     // expected-note @-2:36 {{of 'x'}}
                                     // expected-remark @-2:5 {{retain of type 'Klass'}}
                                     // expected-note @-4:46 {{of 'y'}}
}

func printKlassTuplePair(x : (Klass, Klass)) {
    // We pattern match columns to ensure we get retain on the p and release on
    // the end ')'
    print(x) // expected-remark @:5 {{retain of type 'Klass'}}
             // expected-note @-4:26 {{of 'x'}}
             // expected-remark @-2:5 {{retain of type 'Klass'}}
             // expected-note @-6:26 {{of 'x'}}
             // Release on temp array for print(...).
             // expected-remark @-5:12 {{release of type}}
}

func printKlassTupleLHS(x : (Klass, Klass)) {
    // We print the remarks at the 'p' and at the ending ')'.
    print(x.0) // expected-remark @:5 {{retain of type 'Klass'}}
               // expected-note @-3:25 {{of 'x'}}
               // Release on Array<Any> for print.
               // expected-remark @-3:14 {{release of type}}
}

func returnKlassTupleLHS(x: (Klass, Klass)) -> Klass {
    return x.0 // expected-remark @:12 {{retain of type 'Klass'}}
               // expected-note @-2:26 {{of 'x'}}
}

func callingAnInitializerKlassTuplePair(x: Klass, y: Klass) -> (Klass, Klass) {
    return (x, y) // expected-remark {{retain of type 'Klass'}}
                  // expected-note @-2:41 {{of 'x'}}
                  // expected-remark @-2:5 {{retain of type 'Klass'}}
                  // expected-note @-4:51 {{of 'y'}}
}

public class SubKlass : Klass {
    @inline(never)
    final func doSomething() {}
}

func lookThroughCast(x: SubKlass) -> Klass {
    return x as Klass // expected-remark {{retain of type 'SubKlass'}}
                      // expected-note @-2:22 {{of 'x'}}
}

func lookThroughRefCast(x: Klass) -> SubKlass {
    return x as! SubKlass // expected-remark {{retain of type 'Klass'}}
                          // expected-note @-2:25 {{of 'x'}}
}

func lookThroughEnum(x: Klass?) -> Klass {
    return x! // expected-remark {{retain of type 'Klass'}}
              // expected-note @-2:22 {{of 'x.some'}}
}

func castAsQuestion(x: Klass) -> SubKlass? {
    x as? SubKlass // expected-remark {{retain of type 'Klass'}}
                   // expected-note @-2:21 {{of 'x'}}
}

func castAsQuestionDiamond(x: Klass) -> SubKlass? {
    guard let y = x as? SubKlass else {
        return nil
    }

    y.doSomething()
    return y // expected-remark {{retain of type 'Klass'}}
             // expected-note @-7:28 {{of 'x'}}
}

func castAsQuestionDiamondGEP(x: KlassPair) -> SubKlass? {
    guard let y = x.lhs as? SubKlass else {
        return nil
    }

    y.doSomething()
    // We eliminate the rhs retain/release.
    return y // expected-remark {{retain of type 'Klass'}}
             // expected-note @-8:31 {{of 'x.lhs'}}
}

// We don't handle this test case as well.
func castAsQuestionDiamondGEP2(x: KlassPair) {
    switch (x.lhs as? SubKlass, x.rhs as? SubKlass) { // expected-remark @:19 {{retain of type 'Klass'}}
                                                      // expected-note @-2 {{of 'x.lhs'}}
                                                      // expected-remark @-2:39 {{retain of type 'Klass'}}
                                                      // expected-note @-4 {{of 'x.rhs'}}
    case let (.some(x1), .some(x2)):
        print(x1, x2) // expected-remark {{retain of type 'Optional<SubKlass>'}}
                      // expected-remark @-1 {{retain of type 'Optional<SubKlass>'}}
                      // expected-remark @-2 {{release of type}}
                      // expected-remark @-3 {{release of type 'Optional<SubKlass>'}}
                      // expected-remark @-4 {{release of type 'Optional<SubKlass>'}}
    case let (.some(x1), nil):
        print(x1) // expected-remark {{retain of type 'SubKlass'}}
                  // expected-remark @-1 {{release of type}}
                  // expected-remark @-2 {{release of type 'Optional<SubKlass>'}}
    case let (nil, .some(x2)):
        print(x2) // expected-remark {{retain of type 'SubKlass'}}
                  // expected-remark @-1 {{release of type}}
                  // expected-remark @-2 {{release of type 'Optional<SubKlass>'}}
    case (nil, nil):
        break
    }
}

func inoutKlassPairArgument(x: inout KlassPair) -> Klass {
    return x.lhs // expected-remark {{retain of type 'Klass'}}
                 // expected-note @-2 {{of 'x.lhs'}}
}

func inoutKlassTuplePairArgument(x: inout (Klass, Klass)) -> Klass {
    return x.0 // expected-remark {{retain of type 'Klass'}}
               // expected-note @-2 {{of 'x.0'}}
}

func inoutKlassOptionalArgument(x: inout Klass?) -> Klass {
    return x! // expected-remark {{retain of type 'Klass'}}
              // expected-note @-2 {{of 'x.some'}}
}

func inoutKlassBangCastArgument(x: inout Klass) -> SubKlass {
    return x as! SubKlass // expected-remark {{retain of type 'Klass'}}
                          // expected-note @-2 {{of 'x'}}
}

func inoutKlassQuestionCastArgument(x: inout Klass) -> SubKlass? {
    return x as? SubKlass // expected-remark {{retain of type 'Klass'}}
                          // expected-note @-2 {{of 'x'}}
}

func inoutKlassBangCastArgument2(x: inout Klass?) -> SubKlass {
    return x as! SubKlass // expected-remark {{retain of type 'Klass'}}
                          // expected-note @-2 {{of 'x.some'}}
}

func inoutKlassQuestionCastArgument2(x: inout Klass?) -> SubKlass? {
    return x as? SubKlass // expected-remark {{retain of type 'Klass'}}
                          // expected-note @-2 {{of 'x.some'}}
}
