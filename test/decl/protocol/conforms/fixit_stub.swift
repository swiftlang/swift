// RUN: %target-parse-verify-swift

protocol Protocol1 {
  func foo(arg1: Int, arg2: String) -> String // expected-note{{protocol requires function 'foo(arg1:arg2:)' with type '(Int, String) -> String'; do you want to add a stub?}} {{27-27=\n    internal func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n}}
  func bar() throws -> String // expected-note{{protocol requires function 'bar()' with type '() throws -> String'; do you want to add a stub?}} {{27-27=\n    internal func bar() throws -> String {\n        <#code#>\n    \}\n}}
  init(arg: Int) // expected-note{{protocol requires initializer 'init(arg:)' with type '(arg: Int)'; do you want to add a stub?}} {{27-27=\n    internal init(arg: Int) {\n        <#code#>\n    \}\n}}
  var baz: Int { get } // expected-note{{protocol requires property 'baz' with type 'Int'; do you want to add a stub?}} {{27-27=\n    internal var baz: Int\n}}
  var baz2: Int { get set } // expected-note{{protocol requires property 'baz2' with type 'Int'; do you want to add a stub?}} {{27-27=\n    internal var baz2: Int\n}}
  subscript(arg: Int) -> String { get } //expected-note{{rotocol requires subscript with type '(Int) -> String'; do you want to add a stub?}} {{27-27=\n    internal subscript(arg: Int) -> String {\n        <#code#>\n    \}\n}}
  subscript(arg1: Int, arg2: Int) -> String { get set } //expected-note{{protocol requires subscript with type '(Int, Int) -> String'; do you want to add a stub?}} {{27-27=\n    internal subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
}

class Adopter: Protocol1 { // expected-error{{type 'Adopter' does not conform to protocol 'Protocol1'}} expected-note{{candidate has non-matching type '()'}}
}



protocol Protocol2 {
  func foo(arg1: Int, arg2: String) -> String // expected-note{{protocol requires function 'foo(arg1:arg2:)' with type '(Int, String) -> String'; do you want to add a stub?}} {{32-32=\n    internal func foo(arg1: Int, arg2: String) -> String {\n        <#code#>\n    \}\n}}
  func bar() throws -> String // expected-note{{protocol requires function 'bar()' with type '() throws -> String'; do you want to add a stub?}} {{32-32=\n    internal func bar() throws -> String {\n        <#code#>\n    \}\n}}
  init(arg: Int) // expected-note{{protocol requires initializer 'init(arg:)' with type '(arg: Int)'; do you want to add a stub?}} {{32-32=\n    internal init(arg: Int) {\n        <#code#>\n    \}\n}}
  var baz: Int { get } // expected-note{{protocol requires property 'baz' with type 'Int'; do you want to add a stub?}} {{32-32=\n    internal var baz: Int {\n        <#code#>\n    \}\n}}
  var baz2: Int { get set } // expected-note{{protocol requires property 'baz2' with type 'Int'; do you want to add a stub?}} {{32-32=\n    internal var baz2: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
  subscript(arg: Int) -> String { get } //expected-note{{rotocol requires subscript with type '(Int) -> String'; do you want to add a stub?}} {{32-32=\n    internal subscript(arg: Int) -> String {\n        <#code#>\n    \}\n}}
  subscript(arg1: Int, arg2: Int) -> String { get set } //expected-note{{protocol requires subscript with type '(Int, Int) -> String'; do you want to add a stub?}} {{32-32=\n    internal subscript(arg1: Int, arg2: Int) -> String {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
}

class Adopter2 {} //  expected-note{{candidate has non-matching type '()'}}

extension Adopter2: Protocol2 { // expected-error{{ype 'Adopter2' does not conform to protocol 'Protocol2'}}
}



protocol ProtocolWithAssocType {
  associatedtype AssocType //expected-note{{protocol requires nested type 'AssocType'}} {{41-41=\n    typealias AssocType = <#type#>\n}}
}

struct Adopter3: ProtocolWithAssocType { //expected-error{{type 'Adopter3' does not conform to protocol 'ProtocolWithAssocType'}}
}



protocol ProtocolWithAssocType2 {
  associatedtype AssocType //expected-note{{protocol requires nested type 'AssocType'; do you want to add it?}} {{45-45=\n    typealias AssocType = <#type#>\n}}
}

struct Adopter4 {
}

extension Adopter4: ProtocolWithAssocType2 { //expected-error{{type 'Adopter4' does not conform to protocol 'ProtocolWithAssocType2'}}
}



protocol ProtocolWithSelfRequirement {
  func foo() -> Self // expected-note{{protocol requires function 'foo()' with type '() -> Self'; do you want to add a stub?}} {{47-47=\n    internal func foo() -> Adopter5 {\n        <#code#>\n    \}\n}}
  func foo(lhs: Self, rhs: Self) -> Self //expected-note{{protocol requires function 'foo(lhs:rhs:)' with type '(Adopter5, Adopter5) -> Self'; do you want to add a stub?}} {{47-47=\n    internal func foo(lhs: Adopter5, rhs: Adopter5) -> Adopter5 {\n        <#code#>\n    \}\n}}
}

struct Adopter5: ProtocolWithSelfRequirement { //expected-error{{type 'Adopter5' does not conform to protocol 'ProtocolWithSelfRequirement'}}
}



protocol ProtocolWithSelfRequirement2 {
  func foo() -> Self // expected-note{{protocol requires function 'foo()' with type '() -> Self'; do you want to add a stub?}} {{51-51=\n    internal func foo() -> Adopter6 {\n        <#code#>\n    \}\n}}
  func foo(lhs: Self, rhs: Self) -> Self //expected-note{{protocol requires function 'foo(lhs:rhs:)' with type '(Adopter6, Adopter6) -> Self'; do you want to add a stub?}} {{51-51=\n    internal func foo(lhs: Adopter6, rhs: Adopter6) -> Adopter6 {\n        <#code#>\n    \}\n}}
}

struct Adopter6 {}

extension Adopter6: ProtocolWithSelfRequirement2 { //expected-error{{type 'Adopter6' does not conform to protocol 'ProtocolWithSelfRequirement2'}}
}


protocol ProtocolWithSelfRequirement3 {
  func foo() -> Self // expected-note{{protocol requires function 'foo()' with type '() -> Self'; do you want to add a stub?}} {{47-47=\n    internal func foo() -> Self {\n        <#code#>\n    \}\n}}
  func foo(lhs: Self, rhs: Self) -> Self //expected-note{{protocol requires function 'foo(lhs:rhs:)' with type '(Adopter7, Adopter7) -> Self'; do you want to add a stub?}} {{47-47=\n    internal func foo(lhs: Adopter7, rhs: Adopter7) -> Self {\n        <#code#>\n    \}\n}}
}

class Adopter7: ProtocolWithSelfRequirement3 { //expected-error{{type 'Adopter7' does not conform to protocol 'ProtocolWithSelfRequirement3'}}
}
