// RUN: %target-swift-frontend %s -typecheck -verify

let int: Int = fatalError()

func explicitReturn() -> Int {
    return fatalError()
}

func explicitMultiElement() -> Int {
    print("yes way")
    return fatalError()
}

func implicitReturn() -> Int {
    fatalError()
}

