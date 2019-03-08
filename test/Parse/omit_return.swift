// RUN: %target-swift-frontend %s -typecheck -verify

// MARK: - Helpers



@discardableResult
func logAndReturn<T : CustomStringConvertible>(_ t: T) -> String {
    let log = "\(t)"
    print(log)
    return log
}

@discardableResult
func failableLogAndReturn<T : CustomStringConvertible>(_ t: T) throws -> String {
    let log = "\(t)"
    print(log)
    return log
}


typealias MyOwnVoid = ()

func failableIdentity<T>(_ t: T) throws -> T { t }

enum MyOwnNever {}

func myOwnFatalError() -> MyOwnNever { fatalError() }


// MARK: - Notable Free Functions



func identity<T>(_ t: T) -> T { t }



// MARK: - Free Functions



func ff_nop() {
}

func ff_missing() -> String {
}

func ff_implicit() -> String {
    "hello"
}

func ff_explicit() -> String {
    return "hello"
}

func ff_explicitClosure() -> () -> Void {
    return { print("howdy") }
}

func ff_implicitClosure() -> () -> Void {
    { print("howdy") }
}

func ff_implicitWrong() -> String {
    17 // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
}

func ff_explicitWrong() -> String {
    return 17 // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
}

func ff_implicitMulti() -> String {
    print("uh oh")
    "shucks howdy" // expected-warning {{string literal is unused}}
}

func ff_explicitMulti() -> String {
    print("okay")
    return "all right"
}

func ff_effectfulUsed() -> String {
    logAndReturn("okay")
}

// Unused Returns

func ff_effectfulIgnored() {
    logAndReturn("okay")
}

func ff_effectfulIgnoredExplicitReturnTypeVoid() -> Void {
    logAndReturn("okay")
}

func ff_effectfulIgnoredExplicitReturnTypeSwiftVoid() -> Swift.Void {
    logAndReturn("okay")
}

func ff_effectfulIgnoredExplicitReturnTypeMyVoidTypealias() -> MyOwnVoid {
    logAndReturn("okay")
}

func ff_effectfulIgnoredExplicitReturnTypeEmptyTuple() -> () {
    logAndReturn("okay")
}

// Stubs

func ff_stubImplicitReturn() {
    fatalError()
}

func ff_stubExplicitReturnTypeVoid() -> Void {
    fatalError()
}

func ff_stubExplicitReturnTypeSwiftVoid() -> Swift.Void {
    fatalError()
}

func ff_stubExplicitReturnTypeMyVoidTypealias() -> MyOwnVoid {
    fatalError()
}

func ff_stubExplicitReturnTypeEmptyTuple() -> () {
    fatalError()
}

func ff_stubImplicitReturnNever() -> Never {
    fatalError()
}

func ff_stubExplicitReturnNever() -> Never {
    return fatalError()
}

func ff_stubExplicitReturnNeverAsMyOwnNever() -> MyOwnNever {
    return fatalError() // expected-error {{cannot convert return expression of type 'Never' to return type 'MyOwnNever'}}
}

func ff_stubExplicitReturnMyOwnNeverAsNever() -> Never {
    return myOwnFatalError() // expected-error {{cannot convert return expression of type 'MyOwnNever' to return type 'Never'}}
}

func ff_stubImplicitReturnNeverAsMyOwnNever() -> MyOwnNever {
    fatalError()
}

func ff_stubImplicitReturnMyOwnNeverAsNever() -> Never {
    myOwnFatalError()
}

func ff_stubReturnString() -> String {
    fatalError()
}

func ff_stubReturnGeneric<T>() -> T {
    fatalError()
}

// Trying

func ff_tryExplicit() throws -> String {
    return try failableIdentity("shucks")
}

func ff_tryImplicit() throws -> String {
    try failableIdentity("howdy")
}

func ff_tryExplicitMissingThrows() -> String {
    return try failableIdentity("shucks") // expected-error {{errors thrown from here are not handled}}
}

func ff_tryImplicitMissingThrows() -> String {
    try failableIdentity("howdy") // expected-error {{errors thrown from here are not handled}}
}

// Forced Trying

func ff_forceTryExplicit() -> String {
    return try! failableIdentity("howdy")
}

func ff_forceTryImplicit() -> String {
    try! failableIdentity("shucks")
}

func ff_forceTryExplicitAddingThrows() throws -> String {
    return try! failableIdentity("howdy")
}

func ff_forceTryImplicitAddingThrows() throws -> String {
    try! failableIdentity("shucks")
}

// Optional Trying

func ff_optionalTryExplicit() -> String? {
    return try? failableIdentity("howdy")
}

func ff_optionalTryImplicit() -> String? {
    try? failableIdentity("shucks")
}

func ff_optionalTryExplicitAddingThrows() throws -> String? {
    return try? failableIdentity("shucks")
}

func ff_optionalTryImplicitAddingThrows() throws -> String? {
    try? failableIdentity("howdy")
}



// MARK: - Free Properties : Implicit Get



var fv_nop: () {
} // expected-error {{computed property must have accessors specified}}

var fv_missing: String {
} // expected-error {{computed property must have accessors specified}}

var fv_implicit: String {
    "hello"
}

var fv_explicit: String {
    return "hello"
}

var fv_explicitClosure: () -> Void {
    return { print("howdy") }
}

var fv_implicitClosure: () -> Void {
    { print("howdy") }
}

var fv_implicitWrong: String {
    17 // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
}

var fv_explicitWrong: String {
    return 17 // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
}

var fv_implicitMulti: String {
    print("uh oh")
    "shucks howdy" // expected-warning {{string literal is unused}}
}

var fv_explicitMulti: String {
    print("okay")
    return "all right"
}

var fv_effectfulUsed: String {
    logAndReturn("okay")
}

// Unused returns

var fv_effectfulIgnored: () {
    logAndReturn("okay")
}

var fv_effectfulIgnoredVoid: Void {
    logAndReturn("okay")
}

var fv_effectfulIgnoredSwiftVoid: Swift.Void {
    logAndReturn("okay")
}

// Stubs

var fv_stubEmptyTuple: () {
    fatalError()
}

var fv_stubVoid: Void {
    fatalError()
}

var fv_stubSwiftVoid: Swift.Void {
    fatalError()
}

var fv_stubMyVoidTypealias: MyOwnVoid {
    fatalError()
}

var fv_stubImplicitReturnNever: Never {
    fatalError()
}

var fv_stubExplicitReturnNever: Never {
    return fatalError()
}

var fv_stubExplicitReturnNeverAsMyOwnNever: MyOwnNever {
    return fatalError() // expected-error {{cannot convert return expression of type 'Never' to return type 'MyOwnNever'}}
}

var fv_stubExplicitReturnMyOwnNeverAsNever: Never {
    return myOwnFatalError() // expected-error {{cannot convert return expression of type 'MyOwnNever' to return type 'Never'}}
}

var fv_stubImplicitReturnNeverAsMyOwnNever: MyOwnNever {
    fatalError()
}

var fv_stubImplicitReturnMyOwnNeverAsNever: Never {
    myOwnFatalError()
}

var fv_stubString: String {
    fatalError()
}

// Forced Trying

var fv_forceTryUnusedExplicit: () {
    return try! failableLogAndReturn("oh") //expected-error {{unexpected non-void return value in void function}}
}

var fv_forceTryUnusedImplicit: () {
    try! failableLogAndReturn("uh") 
}

var fv_forceTryExplicit: String {
    return try! failableIdentity("shucks")
}

var fv_forceTryImplicit: String {
    try! failableIdentity("howdy")
}

// Optional Trying

var fv_optionalTryUnusedExplicit: () {
    return try? failableLogAndReturn("uh")  //expected-error {{unexpected non-void return value in void function}}
}

var fv_optionalTryUnusedImplicit: () {
    try? failableLogAndReturn("oh") //expected-warning {{result of 'try?' is unused}}
}

var fv_optionalTryExplicit: String? {
    return try? failableIdentity("shucks")
}

var fv_optionalTryImplicit: String? {
    try? failableIdentity("howdy")
}



// MARK: - Free Properties : Get



var fvg_nop: () {
    get {
    }
}

var fvg_missing: String {
    get {
    }
}

var fvg_implicit: String {
    get {
        "hello"
    }
}

var fvg_explicit: String {
    get {
        return "hello"
    }
}

var fvg_explicitClosure: () -> Void {
    get {
        return { print("howdy") }
    }
}

var fvg_implicitClosure: () -> Void {
    get {
        { print("howdy") }
    }
}

var fvg_implicitWrong: String {
    get {
        17 // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
    }
}

var fvg_explicitWrong: String {
    get {
        return 17 // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
    }
}

var fvg_implicitMulti: String {
    get {
        print("uh oh")
        "shucks howdy" // expected-warning {{string literal is unused}}
    }
}

var fvg_explicitMulti: String {
    get {
        print("okay")
        return "all right"
    }
}

var fvg_effectfulUsed: String {
    get {
        logAndReturn("okay")
    }
}

// Unused returns

var fvg_effectfulIgnored: () {
    get {
        logAndReturn("okay")
    }
}

var fvg_effectfulIgnoredVoid: Void {
    get {
        logAndReturn("okay")
    }
}

var fvg_effectfulIgnoredSwiftVoid: Swift.Void {
    get {
        logAndReturn("okay")
    }
}

// Stubs

var fvg_stubEmptyTuple: () {
    get {
        fatalError()
    }
}

var fvg_stubVoid: Void {
    get {
        fatalError()
    }
}

var fvg_stubSwiftVoid: Swift.Void {
    get {
        fatalError()
    }
}

var fvg_stubMyVoidTypealias: MyOwnVoid {
    get {
        fatalError()
    }
}

var fvg_stubImplicitReturnNever: Never {
    get {
        fatalError()
    }
}

var fvg_stubExplicitReturnNever: Never {
    get {
        return fatalError()
    }
}

var fvg_stubExplicitReturnNeverAsMyOwnNever: MyOwnNever {
    get {
        return fatalError() // expected-error {{cannot convert return expression of type 'Never' to return type 'MyOwnNever'}}
    }
}

var fvg_stubExplicitReturnMyOwnNeverAsNever: Never {
    get {
        return myOwnFatalError() // expected-error {{cannot convert return expression of type 'MyOwnNever' to return type 'Never'}}
    }
}

var fvg_stubImplicitReturnNeverAsMyOwnNever: MyOwnNever {
    get {
        fatalError()
    }
}

var fvg_stubImplicitReturnMyOwnNeverAsNever: Never {
    get {
        myOwnFatalError()
    }
}

var fvg_stubString: String {
    get {
        fatalError()
    }
}

// Forced Trying

var fvg_forceTryExplicit: String {
    get {
        return try! failableIdentity("shucks")
    }
}

var fvg_forceTryImplicit: String {
    get {
        try! failableIdentity("howdy")
    }
}

// Optional Trying

var fvg_optionalTryExplicit: String? {
    get {
        return try? failableIdentity("shucks")
    }
}

var fvg_optionalTryImplicit: String? {
    get {
        try? failableIdentity("howdy")
    }
}



// MARK: - Free Properties : Set



var fvs_nop: () {
    get {}
    set {}
}

var fvs_implicit: String {
    get { "ok" }
    set {
        "hello" // expected-warning {{string literal is unused}} 
    }
}

var fvs_explicit: String {
    get { "ok" }
    set {
        return "hello" // expected-error {{unexpected non-void return value in void function}}
    }
}

var fvs_explicitClosure: () -> Void {
    get { return { print("howdy") } }
    set {
        return { print("howdy") } // expected-error {{unexpected non-void return value in void function}}
    }
}

var fvs_implicitClosure: () -> Void {
    get { { print("howdy") } }
    set {
        { print("howdy") } // expected-error {{closure expression is unused}} expected-note {{did you mean to use a 'do' statement?}}
    }
}

var fvs_implicitWrong: String {
    get { "ok" }
    set {
        17 // expected-warning {{integer literal is unused}}
    }
}

var fvs_explicitWrong: String {
    get { "ok" }
    set {
        return 17 // expected-error {{unexpected non-void return value in void function}}
    }
}

var fvs_implicitMulti: String {
    get { "ok" }
    set {
        print("uh oh")
        "shucks howdy" // expected-warning {{string literal is unused}}
    }
}

var fvs_explicitMulti: String {
    get { "ok" }
    set {
        print("okay")
        return "all right" // expected-error {{unexpected non-void return value in void function}}
    }
}

var fvs_effectfulUsed: String {
    get { "ok" }
    set {
        logAndReturn("okay")
    }
}

// Stubs

var fvs_stub: () {
    get { () }
    set {
        fatalError()
    }
}

var fvs_stubMyOwnFatalError: () {
    get { () }
    set {
        myOwnFatalError()
    }
}

// Forced Trying

var fvs_forceTryExplicit: String {
    get { "ok" }
    set {
        return try! failableIdentity("shucks") // expected-error {{unexpected non-void return value in void function}}
    }
}

var fvs_forceTryImplicit: String {
    get { "ok" }
    set {
        try! failableIdentity("howdy") // expected-warning {{result of call to 'failableIdentity' is unused}}
    }
}

// Optional Trying

var fvs_optionalTryExplicit: String? {
    get { "ok" }
    set {
        return try? failableIdentity("shucks") // expected-error {{unexpected non-void return value in void function}}
    }
}

var fvs_optionalTryImplicit: String? {
    get { "ok" }
    set {
        try? failableIdentity("howdy") // expected-warning {{result of 'try?' is unused}}
    }
}



// MARK: - Free Properties : Read






// MARK: - Free Properties : Modify






// MARK: - Subscripts : Implicit Readonly



enum S_nop {
    subscript() -> () {
    } // expected-error {{subscript must have accessors specified}}
}

enum S_missing {
    subscript() -> String {
    } // expected-error {{subscript must have accessors specified}}
}

enum S_implicit {
    subscript() -> String {
        "hello"
    }
}

enum S_explicit {
    subscript() -> String {
        return "hello"
    }
}

enum S_explicitClosure {
    subscript() -> () -> Void {
        return { print("howdy") } 
    }
}

enum S_implicitClosure {
    subscript() -> () -> Void {
        { print("howdy") } 
    }
}

enum S_implicitWrong {
    subscript() -> String {
        17 // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
    }
}

enum S_explicitWrong {
    subscript() -> String {
        return 17 // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
    }
}

enum S_implicitMulti {
    subscript() -> String {
        print("uh oh")
        "shucks howdy" // expected-warning {{string literal is unused}}
    }
}

enum S_explicitMulti {
    subscript() -> String {
        print("okay")
        return "all right"
    }
}

enum S_effectfulUsed {
    subscript() -> String {
        logAndReturn("okay")
    }
}

// Unused returns

enum S_effectfulIgnored {
    subscript() -> () {
        logAndReturn("okay")
    }
}

enum S_effectfulIgnoredVoid {
    subscript() -> Void {
        logAndReturn("okay")
    }
}

enum S_effectfulIgnoredSwiftVoid {
    subscript() -> Swift.Void {
        logAndReturn("okay")
    }
}

// Stubs

enum S_stubEmptyTuple {
    subscript() -> () {
        fatalError()
    }
}

enum S_stubVoid {
    subscript() -> Void {
        fatalError()
    }
}

enum S_stubSwiftVoid {
    subscript() -> Swift.Void {
        fatalError()
    }
}

enum S_stubMyVoidTypealias {
    subscript() -> MyOwnVoid {
        fatalError()
    }
}

enum S_stubImplicitReturnNever {
    subscript() -> Never {
        fatalError()
    }
}

enum S_stubExplicitReturnNever {
    subscript() -> Never {
        return fatalError()
    }
}

enum S_stubExplicitReturnNeverAsMyOwnNever {
    subscript() -> MyOwnNever {
        return fatalError() // expected-error {{cannot convert return expression of type 'Never' to return type 'MyOwnNever'}}
    }
}

enum S_stubExplicitReturnMyOwnNeverAsNever {
    subscript() -> Never {
        return myOwnFatalError() // expected-error {{cannot convert return expression of type 'MyOwnNever' to return type 'Never'}}
    }
}

enum S_stubImplicitReturnNeverAsMyOwnNever {
    subscript() -> MyOwnNever {
        fatalError()
    }
}

enum S_stubImplicitReturnMyOwnNeverAsNever {
    subscript() -> Never {
        myOwnFatalError()
    }
}

enum S_stubString {
    subscript() -> String {
        fatalError()
    }
}

enum S_stubGeneric {
    subscript<T>() -> T {
        fatalError()
    }
}

// Forced Trying

enum S_forceTryExplicit {
    subscript() -> String {
        return try! failableIdentity("shucks")
    }
}

enum S_forceTryImplicit {
    subscript() -> String {
        try! failableIdentity("howdy")
    }
}

// Optional Trying

enum S_optionalTryExplicit {
    subscript() -> String? {
        return try? failableIdentity("shucks")
    }
}

enum S_optionalTryImplicit {
    subscript() -> String? {
        try? failableIdentity("howdy")
    }
}



// MARK: - Subscripts : Explicit Readonly



enum SRO_nop {
    subscript() -> () {
        get {
        }
    }
}

enum SRO_missing {
    subscript() -> String {
        get {
        }
    }
}

enum SRO_implicit {
    subscript() -> String {
        get {
            "hello"
        }
    }
}

enum SRO_explicit {
    subscript() -> String {
        get {
            return "hello"
        }
    }
}

enum SRO_explicitClosure {
    subscript() -> () -> Void {
        get {
            return { print("howdy") } 
        }
    }
}

enum SRO_implicitClosure {
    subscript() -> () -> Void {
        get {
            { print("howdy") } 
        }
    }
}

enum SRO_implicitWrong {
    subscript() -> String {
        get {
            17 // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
        }
    }
}

enum SRO_explicitWrong {
    subscript() -> String {
        get {
            return 17 // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
        }
    }
}

enum SRO_implicitMulti {
    subscript() -> String {
        get {
            print("uh oh")
            "shucks howdy" // expected-warning {{string literal is unused}}
        }
    }
}

enum SRO_explicitMulti {
    subscript() -> String {
        get {
            print("okay")
            return "all right"
        }
    }
}

enum SRO_effectfulUsed {
    subscript() -> String {
        get {
            logAndReturn("okay")
        }
    }
}

// Unused returns

enum SRO_effectfulIgnored {
    subscript() -> () {
        get {
            logAndReturn("okay")
        }
    }
}

enum SRO_effectfulIgnoredVoid {
    subscript() -> Void {
        get {
            logAndReturn("okay")
        }
    }
}

enum SRO_effectfulIgnoredSwiftVoid {
    subscript() -> Swift.Void {
        get {
            logAndReturn("okay")
        }
    }
}

// Stubs

enum SRO_stubEmptyTuple {
    subscript() -> () {
        get {
            fatalError()
        }
    }
}

enum SRO_stubVoid {
    subscript() -> Void {
        get {
            fatalError()
        }
    }
}

enum SRO_stubSwiftVoid {
    subscript() -> Swift.Void {
        get {
            fatalError()
        }
    }
}

enum SRO_stubMyVoidTypealias {
    subscript() -> MyOwnVoid {
        get {
            fatalError()
        }
    }
}

enum SRO_stubImplicitReturnNever {
    subscript() -> Never {
        get {
            fatalError()
        }
    }
}

enum SRO_stubExplicitReturnNever {
    subscript() -> Never {
        get {
            return fatalError()
        }
    }
}

enum SRO_stubExplicitReturnNeverAsMyOwnNever {
    subscript() -> MyOwnNever {
        get {
            return fatalError() // expected-error {{cannot convert return expression of type 'Never' to return type 'MyOwnNever'}}
        }
    }
}

enum SRO_stubExplicitReturnMyOwnNeverAsNever {
    subscript() -> Never {
        get {
            return myOwnFatalError() // expected-error {{cannot convert return expression of type 'MyOwnNever' to return type 'Never'}}
        }
    }
}

enum SRO_stubImplicitReturnNeverAsMyOwnNever {
    subscript() -> MyOwnNever {
        get {
            fatalError()
        }
    }
}

enum SRO_stubImplicitReturnMyOwnNeverAsNever {
    subscript() -> Never {
        get {
            myOwnFatalError()
        }
    }
}

enum SRO_stubString {
    subscript() -> String {
        get {
            fatalError()
        }
    }
}

enum SRO_stubGeneric {
    subscript<T>() -> T {
        get {
            fatalError()
        }
    }
}

// Forced Trying

enum SRO_forceTryExplicit {
    subscript() -> String {
        get {
            return try! failableIdentity("shucks")
        }
    }
}

enum SRO_forceTryImplicit {
    subscript() -> String {
        get {
            try! failableIdentity("howdy")
        }
    }
}

// Optional Trying

enum SRO_optionalTryExplicit {
    subscript() -> String? {
        get {
            return try? failableIdentity("shucks")
        }
    }
}

enum SRO_optionalTryImplicit {
    subscript() -> String? {
        get {
            try? failableIdentity("howdy")
        }
    }
}



// MARK: - Subscripts : Set






// MARK: - Subscripts : Read/Modify






// MARK: - Constructors



struct C_nop {
    init() {
    }
}

struct C_missing {
    var i: Int
    init?() {
    }
}

struct C_implicitNil {
    init?() {
        nil
    }
}

struct C_explicitNil {
    init?() {
        return nil
    }
}

struct C_forcedMissing {
    var i: Int
    init!() {
    }
}

struct C_forcedImplicitNil {
    init!() {
        nil
    }
}

struct C_forcedExplicitNil {
    init?() {
        return nil
    }
}

struct C_implicit {
    init() {
        "hello" // expected-warning {{string literal is unused}}
    }
}

struct C_explicit {
    init() {
        return "hello" // expected-error {{'nil' is the only return value permitted in an initializer}}
    }
}



// MARK: - Destructors



class D_nop {
    deinit {
    }
}

class D_implicit {
    deinit {
        "bye now" // expected-warning {{string literal is unused}}
    }
}

class D_explicit {
    deinit {
        return "bye now" // expected-error {{unexpected non-void return value in void function}}
    }
}

class D_implicitMulti {
    deinit {
        print("okay")
        "see ya" // expected-warning {{string literal is unused}}
    }
}

class D_explicitMulti {
    deinit {
        print("okay")
        return "see ya" // expected-error {{unexpected non-void return value in void function}}
    }
}

// Unused returns

class D_effectfulIgnored {
    deinit {
        logAndReturn("bye now")
    }
}

// Stubs

class D_stub {
    deinit {
        fatalError()
    }
}

class D_stubMyOwnDeinit {
    deinit {
        myOwnFatalError()
    }
}

// Forced Trying

class D_forceTryUnusedExplicit {
    deinit {
        return try! failableLogAndReturn("uh") // expected-error {{unexpected non-void return value in void function}}
    }
}

class D_forceTryUnusedImplicit {
    deinit {
        try! failableLogAndReturn("oh")
    }
}

// Optional Trying

class D_optionalTryUnusedExplicit {
    deinit {
        return try? failableLogAndReturn("uh") // expected-error {{unexpected non-void return value in void function}}
    }
}

class D_optionalTryUnusedImplicit {
    deinit {
        try? failableLogAndReturn("oh") // expected-warning {{result of 'try?' is unused}}
    }
}
