// RUN: %target-typecheck-verify-swift

protocol Protocol1 {
  func foo(arg1: Int, arg2: String) -> String // expected-note{{protocol requires function 'foo(arg1:arg2:)' with type '(Int, String) -> String'; add a stub for conformance}} {{14:27-27=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    func generic<T>(t: T) {\n        <#code#>\n    \}\n\n    required init(arg: Int) {\n        <#code#>\n    \}\n\n    var baz: Int\n\n    var baz2: Int\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  func bar() throws -> String // expected-note{{protocol requires function 'bar()' with type '() throws -> String'; add a stub for conformance}} {{14:27-27=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    func generic<T>(t: T) {\n        <#code#>\n    \}\n\n    required init(arg: Int) {\n        <#code#>\n    \}\n\n    var baz: Int\n\n    var baz2: Int\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  func generic<T>(t: T) // expected-note{{protocol requires function 'generic(t:)' with type '<T> (t: T) -> ()'; add a stub for conformance}} {{14:27-27=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    func generic<T>(t: T) {\n        <#code#>\n    \}\n\n    required init(arg: Int) {\n        <#code#>\n    \}\n\n    var baz: Int\n\n    var baz2: Int\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  init(arg: Int) // expected-note{{protocol requires initializer 'init(arg:)' with type '(arg: Int)'; add a stub for conformance}} {{14:27-27=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    func generic<T>(t: T) {\n        <#code#>\n    \}\n\n    required init(arg: Int) {\n        <#code#>\n    \}\n\n    var baz: Int\n\n    var baz2: Int\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  var baz: Int { get } // expected-note{{protocol requires property 'baz' with type 'Int'; add a stub for conformance}} {{14:27-27=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    func generic<T>(t: T) {\n        <#code#>\n    \}\n\n    required init(arg: Int) {\n        <#code#>\n    \}\n\n    var baz: Int\n\n    var baz2: Int\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  var baz2: Int { get set } // expected-note{{protocol requires property 'baz2' with type 'Int'; add a stub for conformance}} {{14:27-27=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    func generic<T>(t: T) {\n        <#code#>\n    \}\n\n    required init(arg: Int) {\n        <#code#>\n    \}\n\n    var baz: Int\n\n    var baz2: Int\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  subscript(arg: Int) -> String { get } //expected-note{{rotocol requires subscript with type '(Int) -> String'; add a stub for conformance}} {{14:27-27=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    func generic<T>(t: T) {\n        <#code#>\n    \}\n\n    required init(arg: Int) {\n        <#code#>\n    \}\n\n    var baz: Int\n\n    var baz2: Int\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  subscript(arg1: Int, arg2: Int) -> String { get set } //expected-note{{protocol requires subscript with type '(Int, Int) -> String'; add a stub for conformance}} {{14:27-27=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    func generic<T>(t: T) {\n        <#code#>\n    \}\n\n    required init(arg: Int) {\n        <#code#>\n    \}\n\n    var baz: Int\n\n    var baz2: Int\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
}

class Adopter: Protocol1 { // expected-error{{type 'Adopter' does not conform to protocol 'Protocol1'}}
}



protocol Protocol2 {
  func foo(arg1: Int, arg2: String) -> String // expected-note{{protocol requires function 'foo(arg1:arg2:)' with type '(Int, String) -> String'; add a stub for conformance}} {{31:32-32=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    var baz: Int {\n        <#code#>\n    \}\n\n    var baz2: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  func bar() throws -> String // expected-note{{protocol requires function 'bar()' with type '() throws -> String'; add a stub for conformance}} {{31:32-32=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    var baz: Int {\n        <#code#>\n    \}\n\n    var baz2: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  init(arg: Int) // expected-note{{protocol requires initializer 'init(arg:)' with type '(arg: Int)'}} {{none}}
  var baz: Int { get } // expected-note{{protocol requires property 'baz' with type 'Int'; add a stub for conformance}} {{31:32-32=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    var baz: Int {\n        <#code#>\n    \}\n\n    var baz2: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  var baz2: Int { get set } // expected-note{{protocol requires property 'baz2' with type 'Int'; add a stub for conformance}} {{31:32-32=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    var baz: Int {\n        <#code#>\n    \}\n\n    var baz2: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  subscript(arg: Int) -> String { get } //expected-note{{rotocol requires subscript with type '(Int) -> String'; add a stub for conformance}} {{31:32-32=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    var baz: Int {\n        <#code#>\n    \}\n\n    var baz2: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  subscript(arg1: Int, arg2: Int) -> String { get set } //expected-note{{protocol requires subscript with type '(Int, Int) -> String'; add a stub for conformance}} {{31:32-32=\n    func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n\n    func bar() throws -> String {\n        <#code#>\n    \}\n\n    var baz: Int {\n        <#code#>\n    \}\n\n    var baz2: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n\n    subscript(arg: Int) -> String {\n        <#code#>\n    \}\n\n    subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
}

class Adopter2 {}

extension Adopter2: Protocol2 { // expected-error{{ype 'Adopter2' does not conform to protocol 'Protocol2'}}
}



protocol ProtocolWithAssocType {
  associatedtype AssocType //expected-note{{protocol requires nested type 'AssocType'}} {{+2:41-41=\n    typealias AssocType = <#type#>\n}}
}
struct Adopter3: ProtocolWithAssocType { //expected-error{{type 'Adopter3' does not conform to protocol 'ProtocolWithAssocType'}}
}



protocol ProtocolWithAssocType2 {
  associatedtype AssocType //expected-note{{protocol requires nested type 'AssocType'; add nested type 'AssocType' for conformance}} {{+4:45-45=\n    typealias AssocType = <#type#>\n}}
}
struct Adopter4 {
}
extension Adopter4: ProtocolWithAssocType2 { //expected-error{{type 'Adopter4' does not conform to protocol 'ProtocolWithAssocType2'}}
}



protocol ProtocolWithSelfRequirement {
  func foo() -> Self // expected-note{{protocol requires function 'foo()' with type '() -> Adopter5'; add a stub for conformance}} {{+3:47-47=\n    func foo() -> Adopter5 {\n        <#code#>\n    \}\n\n    func foo(lhs: Adopter5, rhs: Adopter5) -> Adopter5 {\n        <#code#>\n    \}\n}}
  func foo(lhs: Self, rhs: Self) -> Self //expected-note{{protocol requires function 'foo(lhs:rhs:)' with type '(Adopter5, Adopter5) -> Adopter5'; add a stub for conformance}} {{+2:47-47=\n    func foo() -> Adopter5 {\n        <#code#>\n    \}\n\n    func foo(lhs: Adopter5, rhs: Adopter5) -> Adopter5 {\n        <#code#>\n    \}\n}}
}
struct Adopter5: ProtocolWithSelfRequirement { //expected-error{{type 'Adopter5' does not conform to protocol 'ProtocolWithSelfRequirement'}}
}



protocol ProtocolWithSelfRequirement2 {
  func foo() -> Self // expected-note{{protocol requires function 'foo()' with type '() -> Adopter6'; add a stub for conformance}} {{+4:51-51=\n    func foo() -> Adopter6 {\n        <#code#>\n    \}\n\n    func foo(lhs: Adopter6, rhs: Adopter6) -> Adopter6 {\n        <#code#>\n    \}\n}}
  func foo(lhs: Self, rhs: Self) -> Self //expected-note{{protocol requires function 'foo(lhs:rhs:)' with type '(Adopter6, Adopter6) -> Adopter6'; add a stub for conformance}} {{+3:51-51=\n    func foo() -> Adopter6 {\n        <#code#>\n    \}\n\n    func foo(lhs: Adopter6, rhs: Adopter6) -> Adopter6 {\n        <#code#>\n    \}\n}}
}
struct Adopter6 {}
extension Adopter6: ProtocolWithSelfRequirement2 { //expected-error{{type 'Adopter6' does not conform to protocol 'ProtocolWithSelfRequirement2'}}
}


protocol ProtocolWithSelfRequirement3 {
  func foo() -> Self // expected-note{{protocol requires function 'foo()' with type '() -> Self'; add a stub for conformance}} {{+3:47-47=\n    func foo() -> Self {\n        <#code#>\n    \}\n\n    func foo(lhs: Adopter7, rhs: Adopter7) -> Self {\n        <#code#>\n    \}\n}}
  func foo(lhs: Self, rhs: Self) -> Self //expected-note{{protocol requires function 'foo(lhs:rhs:)' with type '(Adopter7, Adopter7) -> Self'; add a stub for conformance}} {{+2:47-47=\n    func foo() -> Self {\n        <#code#>\n    \}\n\n    func foo(lhs: Adopter7, rhs: Adopter7) -> Self {\n        <#code#>\n    \}\n}}
}
class Adopter7: ProtocolWithSelfRequirement3 { //expected-error{{type 'Adopter7' does not conform to protocol 'ProtocolWithSelfRequirement3'}}
}


public protocol ProtocolWithPublicAccess1 {
  func foo() // expected-note{{protocol requires function 'foo()' with type '() -> ()'; add a stub for conformance}} {{+5:71-71=\n    func foo() {\n        <#code#>\n    \}\n\n    typealias AssocType = <#type#>\n}}
}
public protocol ProtocolWithPublicAccess2 {
  associatedtype AssocType //expected-note{{protocol requires nested type 'AssocType'}} {{+2:71-71=\n    func foo() {\n        <#code#>\n    \}\n\n    typealias AssocType = <#type#>\n}}
}
class Adopter8: ProtocolWithPublicAccess1, ProtocolWithPublicAccess2 {
  // expected-error@-1{{type 'Adopter8' does not conform to protocol 'ProtocolWithPublicAccess1'}}
  // expected-error@-2{{type 'Adopter8' does not conform to protocol 'ProtocolWithPublicAccess2'}}
}

public protocol ProtocolWithPublicAccess3 {
  func foo() // expected-note{{protocol requires function 'foo()' with type '() -> ()'; add a stub for conformance}} {{+5:78-78=\n    public func foo() {\n        <#code#>\n    \}\n\n    public typealias AssocType = <#type#>\n}}
}
public protocol ProtocolWithPublicAccess4 {
  associatedtype AssocType //expected-note{{protocol requires nested type 'AssocType'}} {{+2:78-78=\n    public func foo() {\n        <#code#>\n    \}\n\n    public typealias AssocType = <#type#>\n}}
}
public class Adopter9: ProtocolWithPublicAccess3, ProtocolWithPublicAccess4 {
  // expected-error@-1{{type 'Adopter9' does not conform to protocol 'ProtocolWithPublicAccess3'}}
  // expected-error@-2{{type 'Adopter9' does not conform to protocol 'ProtocolWithPublicAccess4'}}
}

private protocol ProtocolWithPrivateAccess1 {
  func foo() // expected-note{{protocol requires function 'foo()' with type '() -> ()'; add a stub for conformance}} {{+5:74-74=\n    func foo() {\n        <#code#>\n    \}\n\n    typealias AssocType = <#type#>\n}}
}
private protocol ProtocolWithPrivateAccess2 {
  associatedtype AssocType //expected-note{{protocol requires nested type 'AssocType'}} {{+2:74-74=\n    func foo() {\n        <#code#>\n    \}\n\n    typealias AssocType = <#type#>\n}}
}
class Adopter10: ProtocolWithPrivateAccess1, ProtocolWithPrivateAccess2 {
  // expected-error@-1{{type 'Adopter10' does not conform to protocol 'ProtocolWithPrivateAccess1'}}
  // expected-error@-2{{type 'Adopter10' does not conform to protocol 'ProtocolWithPrivateAccess2'}}
}

private protocol ProtocolWithPrivateAccess3 {
  func foo() // expected-note{{protocol requires function 'foo()' with type '() -> ()'; add a stub for conformance}} {{+5:81-81=\n    func foo() {\n        <#code#>\n    \}\n\n    typealias AssocType = <#type#>\n}}
}
private protocol ProtocolWithPrivateAccess4 {
  associatedtype AssocType //expected-note{{protocol requires nested type 'AssocType'}} {{+2:81-81=\n    func foo() {\n        <#code#>\n    \}\n\n    typealias AssocType = <#type#>\n}}
}
public class Adopter11: ProtocolWithPrivateAccess3, ProtocolWithPrivateAccess4 {
  // expected-error@-1{{type 'Adopter11' does not conform to protocol 'ProtocolWithPrivateAccess3'}}
  // expected-error@-2{{type 'Adopter11' does not conform to protocol 'ProtocolWithPrivateAccess4'}}
}

protocol ProtocolRequiresInit1 {
  init(arg: Int) // expected-note{{protocol requires initializer 'init(arg:)' with type '(arg: Int)'; add a stub for conformance}} {{+2:48-48=\n    init(arg: Int) {\n        <#code#>\n    \}\n}}
}
final class Adopter12 : ProtocolRequiresInit1 {} //expected-error {{type 'Adopter12' does not conform to protocol 'ProtocolRequiresInit1'}}

protocol ProtocolRequiresInit2 {
  init(arg: Int) // expected-note{{protocol requires initializer 'init(arg:)' with type '(arg: Int)'; add a stub for conformance}} {{+3:46-46=\n    convenience init(arg: Int) {\n        <#code#>\n    \}\n}}
}
final class Adopter13 {}
extension Adopter13 : ProtocolRequiresInit2 {} //expected-error {{type 'Adopter13' does not conform to protocol 'ProtocolRequiresInit2'}}

protocol ProtocolRequiresInit3 {
  init(arg: Int) // expected-note{{protocol requires initializer 'init(arg:)' with type '(arg: Int)'; add a stub for conformance}} {{+3:46-46=\n    init(arg: Int) {\n        <#code#>\n    \}\n}}
}
struct Adopter14 {}
extension Adopter14 : ProtocolRequiresInit3 {} //expected-error {{type 'Adopter14' does not conform to protocol 'ProtocolRequiresInit3'}}

protocol ProtocolChain1 {
  func foo1() // expected-note {{protocol requires function 'foo1()' with type '() -> ()'; add a stub for conformance}}{{154:35-35=\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n\n    func bar3() {\n        <#code#>\n    \}\n\n    var bar4: Int\n\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func foo3() {\n        <#code#>\n    \}\n\n    var foo4: Int\n}}
  func foo2() // expected-note {{protocol requires function 'foo2()' with type '() -> ()'; add a stub for conformance}}{{154:35-35=\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n\n    func bar3() {\n        <#code#>\n    \}\n\n    var bar4: Int\n\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func foo3() {\n        <#code#>\n    \}\n\n    var foo4: Int\n}}
  func foo3() // expected-note {{protocol requires function 'foo3()' with type '() -> ()'; add a stub for conformance}}{{154:35-35=\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n\n    func bar3() {\n        <#code#>\n    \}\n\n    var bar4: Int\n\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func foo3() {\n        <#code#>\n    \}\n\n    var foo4: Int\n}}
  var foo4 : Int {get set } // expected-note {{protocol requires property 'foo4' with type 'Int'; add a stub for conformance}}{{154:35-35=\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n\n    func bar3() {\n        <#code#>\n    \}\n\n    var bar4: Int\n\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func foo3() {\n        <#code#>\n    \}\n\n    var foo4: Int\n}}
}
protocol ProtocolChain2 : ProtocolChain1 {
  func bar1() // expected-note {{protocol requires function 'bar1()' with type '() -> ()'; add a stub for conformance}}{{154:35-35=\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n\n    func bar3() {\n        <#code#>\n    \}\n\n    var bar4: Int\n\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func foo3() {\n        <#code#>\n    \}\n\n    var foo4: Int\n}}
  func bar2() // expected-note {{protocol requires function 'bar2()' with type '() -> ()'; add a stub for conformance}}{{154:35-35=\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n\n    func bar3() {\n        <#code#>\n    \}\n\n    var bar4: Int\n\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func foo3() {\n        <#code#>\n    \}\n\n    var foo4: Int\n}}
  func bar3() // expected-note {{protocol requires function 'bar3()' with type '() -> ()'; add a stub for conformance}}{{154:35-35=\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n\n    func bar3() {\n        <#code#>\n    \}\n\n    var bar4: Int\n\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func foo3() {\n        <#code#>\n    \}\n\n    var foo4: Int\n}}
  var bar4 : Int {get set } // expected-note {{protocol requires property 'bar4' with type 'Int'; add a stub for conformance}}{{154:35-35=\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n\n    func bar3() {\n        <#code#>\n    \}\n\n    var bar4: Int\n\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func foo3() {\n        <#code#>\n    \}\n\n    var foo4: Int\n}}
}

class Adopter15 : ProtocolChain2 {} //expected-error {{type 'Adopter15' does not conform to protocol 'ProtocolChain2'}} expected-error {{type 'Adopter15' does not conform to protocol 'ProtocolChain1'}}

protocol ProtocolParallel1 {
  associatedtype Foo1 // expected-note {{protocol requires nested type 'Foo1'; add nested type 'Foo1' for conformance}}{{171:57-57=\n    typealias Foo1 = <#type#>\n\n    typealias Foo2 = <#type#>\n\n    typealias Foo3 = <#type#>\n\n    func Foo4() {\n        <#code#>\n    \}\n\n    typealias Bar1 = <#type#>\n\n    typealias Bar2 = <#type#>\n\n    typealias Bar3 = <#type#>\n}}
  associatedtype Foo2 // expected-note {{protocol requires nested type 'Foo2'; add nested type 'Foo2' for conformance}}{{171:57-57=\n    typealias Foo1 = <#type#>\n\n    typealias Foo2 = <#type#>\n\n    typealias Foo3 = <#type#>\n\n    func Foo4() {\n        <#code#>\n    \}\n\n    typealias Bar1 = <#type#>\n\n    typealias Bar2 = <#type#>\n\n    typealias Bar3 = <#type#>\n}}
  associatedtype Foo3 // expected-note {{protocol requires nested type 'Foo3'; add nested type 'Foo3' for conformance}}{{171:57-57=\n    typealias Foo1 = <#type#>\n\n    typealias Foo2 = <#type#>\n\n    typealias Foo3 = <#type#>\n\n    func Foo4() {\n        <#code#>\n    \}\n\n    typealias Bar1 = <#type#>\n\n    typealias Bar2 = <#type#>\n\n    typealias Bar3 = <#type#>\n}}
  // FIXME: Why do we add stubs for all missing requirements when the note implies a single one?
  func Foo4() // expected-note {{protocol requires function 'Foo4()' with type '() -> ()'; add a stub for conformance}}{{171:57-57=\n    typealias Foo1 = <#type#>\n\n    typealias Foo2 = <#type#>\n\n    typealias Foo3 = <#type#>\n\n    func Foo4() {\n        <#code#>\n    \}\n\n    typealias Bar1 = <#type#>\n\n    typealias Bar2 = <#type#>\n\n    typealias Bar3 = <#type#>\n}}
}

protocol ProtocolParallel2 {
  associatedtype Bar1 // expected-note {{protocol requires nested type 'Bar1'; add nested type 'Bar1' for conformance}}{{171:57-57=\n    typealias Foo1 = <#type#>\n\n    typealias Foo2 = <#type#>\n\n    typealias Foo3 = <#type#>\n\n    func Foo4() {\n        <#code#>\n    \}\n\n    typealias Bar1 = <#type#>\n\n    typealias Bar2 = <#type#>\n\n    typealias Bar3 = <#type#>\n}}
  associatedtype Bar2 // expected-note {{protocol requires nested type 'Bar2'; add nested type 'Bar2' for conformance}}{{171:57-57=\n    typealias Foo1 = <#type#>\n\n    typealias Foo2 = <#type#>\n\n    typealias Foo3 = <#type#>\n\n    func Foo4() {\n        <#code#>\n    \}\n\n    typealias Bar1 = <#type#>\n\n    typealias Bar2 = <#type#>\n\n    typealias Bar3 = <#type#>\n}}
  associatedtype Bar3 // expected-note {{protocol requires nested type 'Bar3'; add nested type 'Bar3' for conformance}}{{171:57-57=\n    typealias Foo1 = <#type#>\n\n    typealias Foo2 = <#type#>\n\n    typealias Foo3 = <#type#>\n\n    func Foo4() {\n        <#code#>\n    \}\n\n    typealias Bar1 = <#type#>\n\n    typealias Bar2 = <#type#>\n\n    typealias Bar3 = <#type#>\n}}
  func Bar4() // expected-note {{protocol requires function 'Bar4()' with type '() -> ()'; add a stub for conformance}}{{171:57-57=\n    func Bar4() {\n        <#code#>\n    \}\n}}
}

class Adopter16 : ProtocolParallel1, ProtocolParallel2 {} // expected-error {{type 'Adopter16' does not conform to protocol 'ProtocolParallel1'}} expected-error {{type 'Adopter16' does not conform to protocol 'ProtocolParallel2'}}

protocol ProtocolParallel3 {
  func foo1() // expected-note{{protocol requires function 'foo1()' with type '() -> ()'; add a stub for conformance}}{{+7:56-56=\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n}}
  func foo2() // expected-note{{protocol requires function 'foo2()' with type '() -> ()'; add a stub for conformance}}{{+6:56-56=\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n}}
}
protocol ProtocolParallel4 {
  func bar1()
  func bar2()
}
class Adopter17: ProtocolParallel3, ProtocolParallel4 { // expected-error {{type 'Adopter17' does not conform to protocol 'ProtocolParallel3'}}
  func bar1() {}
  func bar2() {}
}

protocol ProtocolHasSubscriptFunction {
  func `subscript`() // expected-note{{protocol requires function 'subscript()' with type '() -> ()'; add a stub for conformance}} {{+2:74-74=\n    func `subscript`() {\n        <#code#>\n    \}\n}}
}
class ProtocolHasSubscriptFunctionAdopter: ProtocolHasSubscriptFunction { // expected-error{{type 'ProtocolHasSubscriptFunctionAdopter' does not conform to protocol 'ProtocolHasSubscriptFunction'}}

}

protocol ProtocolHasConsumingRequirement {
  __consuming func foo() // expected-note {{protocol requires function 'foo()' with type '() -> ()'; add a stub for conformance}} {{+2:81-81=\n    func foo() {\n        <#code#>\n    \}\n}}
}
struct ProtocolHasConsumingRequirementAdopter: ProtocolHasConsumingRequirement { // expected-error {{type 'ProtocolHasConsumingRequirementAdopter' does not conform to protocol 'ProtocolHasConsumingRequirement'}}

}
