// RUN: %target-typecheck-verify-swift

var a: Int = nil
// expected-error@-1 {{nil cannot initialize specified type 'Int'}} 
// expected-note@-2 {{add '?' to form the optional type 'Int?'}} {{11-11=?}}

var b: () -> Void = nil
// expected-error@-1 {{nil cannot initialize specified type '() -> Void'}} 
// expected-note@-2 {{add '?' to form the optional type '(() -> Void)?'}} {{8-8=(}} {{18-18=)?}}

var c, d: Int = nil
// expected-error@-1 {{type annotation missing in pattern}}
// expected-error@-2 {{nil cannot initialize specified type 'Int'}} 
// expected-note@-3 {{add '?' to form the optional type 'Int?'}} {{14-14=?}}

var (e, f): (Int, Int) = nil
// expected-error@-1 {{nil cannot initialize specified type '(Int, Int)'}} 

var g: Int = nil, h: Int = nil
// expected-error@-1 {{nil cannot initialize specified type 'Int'}} 
// expected-note@-2 {{add '?' to form the optional type 'Int?'}} {{11-11=?}}
// expected-error@-3 {{nil cannot initialize specified type 'Int'}} 
// expected-note@-4 {{add '?' to form the optional type 'Int?'}} {{25-25=?}}

var _: Int = nil
// expected-error@-1 {{nil cannot initialize specified type 'Int'}} 
// expected-note@-2 {{add '?' to form the optional type 'Int?'}} {{11-11=?}}

// 'nil' can initialize the specified type, if its generic parameters are bound
var _: Array? = nil // expected-error {{generic parameter 'Element' could not be inferred}}
