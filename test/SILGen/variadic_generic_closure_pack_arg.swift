// RUN: %target-swift-emit-silgen -target %target-swift-5.9-abi-triple %s

// Test that passing a pack expansion tuple to a closure compiles without
// crashing. This tests the fix for a bug where SILGenProlog.cpp would only
// check the substituted type for pack expansions when deciding whether to
// create a temporary initialization, but the original abstraction pattern
// could still have pack expansions even when the substituted type doesn't.

func processValues<each T>(_ values: repeat each T, test: ((repeat each T)) throws -> Void) rethrows {
    try test((repeat each values))
}

func useIt() {
    // When specialized with concrete types like (Int, Int, Int), the substituted
    // tuple type doesn't contain pack expansions, but the abstraction pattern does.
    processValues(1, 2, 3) { values in
        print(values)
    }
}
