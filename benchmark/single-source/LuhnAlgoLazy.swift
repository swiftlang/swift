// LuhnAlgoLazy benchmark
//
// Description: Performs a Luhn checksum lazily
// Source: https://gist.github.com/airspeedswift/e584239d7658b317f59a

import TestsUtils

public var LuhnAlgoLazy = BenchmarkInfo(
  name: "LuhnAlgoLazy",
  runFunction: run_LuhnAlgoLazy,
  tags: [.algorithm]
)

@inline(never)
public func run_LuhnAlgoLazy(_ N: Int) {
    let resultRef = true
    var result = false

    for _ in 1...100*N {
        result = lazychecksum(ccnum)

        if result != resultRef {
            break
        }
    }

    CheckResults(result == resultRef)
}

// Another version of the Luhn algorithm, similar to the one found here:
//   https://gist.github.com/airspeedswift/b349c256e90da746b852
//
// This time, trying to keep two versions, one eager one lazy,
// as similar as possible. Only adding "lazy" to the start of
// the expression to switch between the two.
//
// Much of the same code as the previous version at the top,
// Skip down to line 110 for the different par

// mapSome is my Swift version of Haskell's mapMaybe, which
// is a map that takes a transform function that returns an
// optional, and returns a collection of only those values
// that weren't nil

// first we need a lazy view that holds the original
// sequence and the transform function
struct MapSomeSequenceView<Base: Sequence, T> {
    fileprivate let _base: Base
    fileprivate let _transform: (Base.Element) -> T?
}

// extend it to implement Sequence
extension MapSomeSequenceView: Sequence {
    typealias Iterator = AnyIterator<T>

    func makeIterator() -> Iterator {
        var g = _base.makeIterator()
        // AnyIterator is a helper that takes a
        // closure and calls it to generate each
        // element
        return AnyIterator {
            while let element = g.next() {
                if let some = self._transform(element) {
                    return some
                }
            }
            return nil
        }
    }
}

// now extend a lazy collection to return that view
// from a call to mapSome.  In pracice, when doing this,
// you should do it for all the lazy wrappers
// (i.e. random-access, forward and sequence)
extension LazyCollectionProtocol {
    // I might be missing a trick with this super-ugly return type, is there a
    // better way?
    func mapSome<U>(
        _ transform: @escaping (Elements.Element) -> U?
    ) -> LazySequence<MapSomeSequenceView<Elements, U>> {
        return MapSomeSequenceView(_base: elements, _transform: transform).lazy
    }
}

// curried function - call with 1 argument to get a function
// that tells you if i is a multiple of a given number
// e.g.
//  let isEven = isMultipleOf(2)
//  isEven(4) // true
func isMultipleOf<T: FixedWidthInteger>(_ of: T)->(T)->Bool {
    return { $0 % of == 0 }
}

// extend LazySequence to map only every nth element, with all
// other elements untransformed.
extension LazySequenceProtocol {
  func mapEveryN(
    _ n: Int,
    _ transform: @escaping (Element) -> Element
  ) -> LazyMapSequence<EnumeratedSequence<Self>, Element> {
    let isNth = isMultipleOf(n)
    func transform2(
      _ pair: EnumeratedSequence<Self>.Element
    ) -> Element {
      return isNth(pair.0 + 1) ? transform(pair.1) : pair.1
    }
    return self.enumerated().lazy.map(transform2)
  }
}

infix operator |> : PipeRightPrecedence
precedencegroup PipeRightPrecedence {
    associativity: left
}

func |><T,U>(t: T, f: (T)->U) -> U {
    return f(t)
}

infix operator • : DotPrecedence
precedencegroup DotPrecedence {
    associativity: left
}

func • <T, U, V> (g: @escaping (U) -> V, f: @escaping (T) -> U) -> (T) -> V {
    return { x in g(f(x)) }
}

// function to free a method from the shackles
// of it's owner
func freeMemberFunc<T,U>(_ f: @escaping (T)->()->U)->(T)->U {
    return { (t: T)->U in f(t)() }
}

extension String {
  func toInt() -> Int? { return Int(self) }
}

// stringToInt can now be pipelined or composed
let stringToInt = freeMemberFunc(String.toInt)
// if only Character also had a toInt method
let charToString = { (c: Character) -> String in String(c) }
let charToInt = stringToInt • charToString

func sum<S: Sequence>(_ nums: S)->S.Element where S.Element: FixedWidthInteger {
    return nums.reduce(0,+)
}

func reverse<C: LazyCollectionProtocol>(
    _ source: C
) -> LazyCollection<ReversedCollection<C.Elements>> {
  return source.elements.reversed().lazy
}

func map<S: LazySequenceProtocol, U>(
  _ source: S, _ transform: @escaping (S.Elements.Element)->U
) -> LazyMapSequence<S.Elements,U> {
  return source.map(transform)
}

func mapSome<C: LazyCollectionProtocol, U>(
    _ source: C,
    _ transform: @escaping (C.Elements.Element)->U?
) -> LazySequence<MapSomeSequenceView<C.Elements, U>> {
    return source.mapSome(transform)
}

func mapEveryN<S: LazySequenceProtocol>(
    _ source: S, _ n: Int,
    _ transform: @escaping (S.Element)->S.Element
) -> LazyMapSequence<EnumeratedSequence<S>, S.Element> {
    return source.mapEveryN(n, transform)
}

// Non-lazy version of mapSome:
func mapSome<S: Sequence, C: RangeReplaceableCollection>(
    _ source: S,
    _ transform: @escaping (S.Element)->C.Element?
) -> C {
    var result = C()
    for x in source {
        if let y = transform(x) {
            result.append(y)
        }
    }
    return result
}

// Specialized default version of mapSome that returns an array, to avoid
// forcing the user having to specify:
func mapSome<S: Sequence,U>(
    _ source: S,
    _ transform: @escaping (S.Element
)->U?)->[U] {
    // just calls the more generalized version
    return mapSome(source, transform)
}

// Non-lazy version of mapEveryN:
func mapEveryN<S: Sequence>(
    _ source: S, _ n: Int,
    _ transform: @escaping (S.Element) -> S.Element
) -> [S.Element] {
    let isNth = isMultipleOf(n)
    return source.enumerated().map {
        (pair: (index: Int, elem: S.Element)) in
        isNth(pair.index+1)
            ? transform(pair.elem)
            : pair.elem
    }
}

let double = { $0*2 }

let combineDoubleDigits = {
    (10...18).contains($0) ? $0-9 : $0
}

// first, the lazy version of checksum calcuation
let lazychecksum = { (ccnum: String) -> Bool in
    ccnum.lazy
    |> reverse
    |> { mapSome($0, charToInt) }
    |> { mapEveryN($0, 2, double) }
    |> { map($0, combineDoubleDigits) }
    |> sum
    |> isMultipleOf(10)
}

let ccnum = "4012 8888 8888 1881"
