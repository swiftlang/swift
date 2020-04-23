// RUN: %target-swift-frontend %s -typecheck -verify

// Now that Never is a bottom type, the following conversions should all be allowed.
let int: Int = fatalError()

func explicitReturn() -> Int {
    return fatalError()
}

func explicitMultiElement() -> Int {
    print("no way")
    return fatalError()
}

func implicitReturn() -> Int {
    fatalError()
}

