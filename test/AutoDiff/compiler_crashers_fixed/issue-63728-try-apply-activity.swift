// RUN: not %target-swift-frontend -emit-sil -verify %s

// The testcase from https://github.com/apple/swift/issues/63728 is not valid
// (the function is not differentiable), however, it should not cause verifier errors
// Here the root case is lack of activity analysis for `try_apply` terminators

import _Differentiation

func a() throws {
    let keyPaths = (readable: [String: KeyPath<T, Double>](), writable: [String: WritableKeyPath<T, Double>]())
    @differentiable(reverse)
    func f(p: PAndT) -> Double {
        var mutableP = p
        let s = p.p.e
        var sArray: [[Double]] = []
        sArray.append((s["a"]!.asArray()).map {$0.value})
        mutableP.s = w(mutableP.s, at: keyPaths.writable["a"]!, with: sArray[0][0])
        return mutableP.s[keyPath: keyPaths.writable["a"]!]
    }
}

public struct S<I: SProtocol, D> {
    public func asArray() -> [(index: I, value: D)] {
        return [(index: I, value: D)]()
    }
}
struct T: Differentiable {}
struct P: Differentiable {
    public var e: F<Double, Double>
}

struct PAndT: Differentiable{
    @differentiable(reverse) public var p: P
    @differentiable(reverse) public var s: T
}

public struct F<I: SProtocol, D>{
    public func asArray() -> [(index: I, value: D)] {return [(index: I, value: D)]()}
    public subscript(_ name: String) -> S<I, D>? {get { return self.sGet(name) }}
    func sGet(_ name: String) -> S<I, D>? { fatalError("") }
}

public protocol ZProtocol: Differentiable {var z: () -> TangentVector { get }}
public protocol SProtocol: Hashable {}

//extension S: Differentiable where D: ZProtocol, D.TangentVector == D {}

extension F: Differentiable where D: ZProtocol, D.TangentVector == D {}
public extension ZProtocol {var z: () -> TangentVector {{ Self.TangentVector.zero }}}

extension F: Equatable where I: Equatable, D: Equatable {}
//extension S: Equatable where I: Equatable, D: Equatable {}
extension Double: SProtocol, ZProtocol {}

@differentiable(reverse where O: Differentiable, M: ZProtocol)
func w<O, M>(_ o: O, at m: WritableKeyPath<O, M>, with v: M) -> O {return o}

@derivative(of: w)
func vjpw<O, M>(_ o: O, at m: WritableKeyPath<O, M>, with v: M) -> (value: O, pullback: (O.TangentVector) -> (O.TangentVector, M.TangentVector)) where O: Differentiable, M: ZProtocol{fatalError("")}
