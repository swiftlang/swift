// RUN: %target-typecheck-verify-swift

// rdar://problem/38461036 , https://bugs.swift.org/browse/SR-7192 and highlights the real problem in https://bugs.swift.org/browse/SR-6941

protocol SameType {}
protocol Conforms {}

struct Works: Hashable, Conforms {}
struct Fails: Hashable {}

extension Array: SameType where Element == Works {}
// expected-note@-1 2 {{requirement from conditional conformance of '[Fails]' to 'SameType'}}
extension Dictionary: SameType where Value == Works {}
// expected-note@-1 2 {{requirement from conditional conformance of '[Int : Fails]' to 'SameType'}}
extension Array: Conforms where Element: Conforms {}
// expected-note@-1 5 {{requirement from conditional conformance of '[Fails]' to 'Conforms'}}
extension Dictionary: Conforms where Value: Conforms {}
// expected-note@-1 3 {{requirement from conditional conformance of '[Int : Fails]' to 'Conforms'}}
// expected-note@-2 2 {{requirement from conditional conformance of '[Int : Conforms]' to 'Conforms'}}

let works = Works()
let fails = Fails()

func arraySameType() {
    let arrayWorks = [works]
    let arrayFails = [fails]

    let _: SameType = [works]
    let _: SameType = [fails]
    // expected-error@-1 {{cannot convert value of type 'Fails' to expected element type 'Works'}}

    let _: SameType = arrayWorks
    let _: SameType = arrayFails
    // expected-error@-1 {{protocol 'SameType' requires the types 'Fails' and 'Works' be equivalent}}

    let _: SameType = [works] as [Works]
    let _: SameType = [fails] as [Fails]
    // expected-error@-1 {{protocol 'SameType' requires the types 'Fails' and 'Works' be equivalent}}

    let _: SameType = [works] as SameType
    let _: SameType = [fails] as SameType
    // expected-error@-1 {{'[Fails]' is not convertible to 'SameType'}}

    let _: SameType = arrayWorks as SameType
    let _: SameType = arrayFails as SameType
    // expected-error@-1 {{'[Fails]' is not convertible to 'SameType'}}
}

func dictionarySameType() {
    let dictWorks: [Int : Works] = [0 : works]
    let dictFails: [Int : Fails] = [0 : fails]

    let _: SameType = [0 : works]
    let _: SameType = [0 : fails]
    // expected-error@-1 {{cannot convert value of type 'Fails' to expected dictionary value type 'Works'}}

    let _: SameType = dictWorks
    let _: SameType = dictFails
    // expected-error@-1 {{protocol 'SameType' requires the types 'Fails' and 'Works' be equivalent}}

    let _: SameType = [0 : works] as [Int : Works]
    let _: SameType = [0 : fails] as [Int : Fails]
    // expected-error@-1 {{protocol 'SameType' requires the types 'Fails' and 'Works' be equivalent}}

    let _: SameType = [0 : works] as SameType
    let _: SameType = [0 : fails] as SameType
    // expected-error@-1 {{'[Int : Fails]' is not convertible to 'SameType'}}

    let _: SameType = dictWorks as SameType
    let _: SameType = dictFails as SameType
    // expected-error@-1 {{'[Int : Fails]' is not convertible to 'SameType'}}
}

func arrayConforms() {
    let arrayWorks = [works]
    let arrayFails = [fails]

    let _: Conforms = [works]
    let _: Conforms = [fails]
    // expected-error@-1 {{protocol 'Conforms' requires that 'Fails' conform to 'Conforms'}}

    let _: Conforms = arrayWorks
    let _: Conforms = arrayFails
    // expected-error@-1 {{protocol 'Conforms' requires that 'Fails' conform to 'Conforms'}}

    let _: Conforms = [works] as [Works]
    let _: Conforms = [fails] as [Fails]
    // expected-error@-1 {{protocol 'Conforms' requires that 'Fails' conform to 'Conforms'}}

    let _: Conforms = [works] as Conforms
    let _: Conforms = [fails] as Conforms
    // expected-error@-1 {{'[Fails]' is not convertible to 'Conforms'}}

    let _: Conforms = arrayWorks as Conforms
    let _: Conforms = arrayFails as Conforms
    // expected-error@-1 {{'[Fails]' is not convertible to 'Conforms'}}
}

func dictionaryConforms() {
    let dictWorks: [Int : Works] = [0 : works]
    let dictFails: [Int : Fails] = [0 : fails]

    let _: Conforms = [0 : works]
    let _: Conforms = [0 : fails]
    // expected-error@-1 {{protocol 'Conforms' requires that 'Fails' conform to 'Conforms'}}

    let _: Conforms = dictWorks
    let _: Conforms = dictFails
    // expected-error@-1 {{protocol 'Conforms' requires that 'Fails' conform to 'Conforms'}}

    let _: Conforms = [0 : works] as [Int : Works]
    let _: Conforms = [0 : fails] as [Int : Fails]
    // expected-error@-1 {{protocol 'Conforms' requires that 'Fails' conform to 'Conforms'}}

    let _: Conforms = [0 : works] as Conforms
    let _: Conforms = [0 : fails] as Conforms
    // expected-error@-1 {{'[Int : Fails]' is not convertible to 'Conforms'}}

    let _: Conforms = dictWorks as Conforms
    let _: Conforms = dictFails as Conforms
    // expected-error@-1 {{'[Int : Fails]' is not convertible to 'Conforms'}}
}

func combined() {
    let _: Conforms = [[0: [1 : [works]]]]
    let _: Conforms = [[0: [1 : [fails]]]]
    // expected-error@-1 {{protocol 'Conforms' requires that 'Fails' conform to 'Conforms'}}

    // Needs self conforming protocols:
    let _: Conforms = [[0: [1 : [works]] as Conforms]]
    // expected-error@-1 {{value of protocol type 'Conforms' cannot conform to 'Conforms'; only struct/enum/class types can conform to protocols}}

    let _: Conforms = [[0: [1 : [fails]] as Conforms]]
    // expected-error@-1 {{protocol 'Conforms' requires that 'Fails' conform to 'Conforms'}}
    // expected-error@-2 {{value of protocol type 'Conforms' cannot conform to 'Conforms'; only struct/enum/class types can conform to protocols}}
}
