// RUN: %target-swift-frontend %s -emit-sil -O -swift-version 6 -target %target-swift-5.9-abi-triple -o /dev/null

// REQUIRES: swift_in_compiler

// rdar://(typed-throws-pack-spec) — bug-3-instance-pack-expand-release.
//
// PackSpecialization didn't account for the typed-throws error result when
// computing the new function signature. After GenericSpecializer converted
// `@error_indirect E` to `@error E` (E = Never), PackSpecialization
// produced a function whose signature claimed multiple direct results
// (`(@owned Result, @owned Never, @error Never)`) but whose body returned
// only the Result. Downstream passes (CSE, SILCombine, depending on
// function shape) tripped the `cast<Ty>()` assertion in `Casting.h:578`.
//
// Skipping pack-specialization for callees with an error result avoids the
// crash. The throw-aware version of the pass is left as future work.

public struct Product<each Element> {
    public let values: (repeat each Element)

    @inlinable
    public init(_ values: repeat each Element) {
        self.values = (repeat each values)
    }
}

extension Product {
    @inlinable
    public consuming func map<each NewElement, E: Swift.Error>(
        _ transforms: repeat (each Element) throws(E) -> each NewElement
    ) throws(E) -> Product<repeat each NewElement> {
        Product<repeat each NewElement>(
            repeat try (each transforms)(each values)
        )
    }
}

@inlinable
public func _instantiate() -> Product<Int, String, Bool>? {
    let triple = Product(1, "hi", true)
    return try? triple.map(
        { (x: Int) -> Int in x + 1 },
        { (s: String) -> String in s.uppercased() },
        { (b: Bool) -> Bool in !b }
    )
}
