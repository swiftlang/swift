// RUN: %target-swift-frontend %s -typecheck -verify

let int: Int = fatalError() // expected-error {{cannot convert value of type 'Never' to specified type 'Int'}}

func explicitReturn() -> Int {
    return fatalError() // expected-error {{cannot convert return expression of type 'Never' to return type 'Int'}}
}

func explicitMultiElement() -> Int {
    print("no way")
    return fatalError() // expected-error {{cannot convert return expression of type 'Never' to return type 'Int'}}
}

func implicitReturn() -> Int {
    fatalError()
}

