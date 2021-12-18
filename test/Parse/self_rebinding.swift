// RUN: %target-typecheck-verify-swift

class Writer {
    private var articleWritten = 47

    func stop() {
        let rest: () -> Void = { [weak self] in
            let articleWritten = self?.articleWritten ?? 0
            guard let `self` = self else {
                return
            }

            self.articleWritten = articleWritten
        }

        fatalError("I'm running out of time")
        rest()
    }

    func nonStop() {
        let write: () -> Void = { [weak self] in
            self?.articleWritten += 1

            if let self = self {
                self.articleWritten += 1
            }

            if let `self` = self {
                self.articleWritten += 1
            }

            guard let self = self else {
                return
            }

            self.articleWritten += 1
        }

        write()
    }
}

struct T {
    var mutable: Int = 0
    func f() {
        // expected-error @+2 {{keyword 'self' cannot be used as an identifier here}}
        // expected-note @+1 {{if this name is unavoidable, use backticks to escape it}}
        let self = self
    }
}

class MyCls {
    func something() {}

    func test() {
        // expected-warning @+1 {{initialization of immutable value 'self' was never used}}
        let `self` = Writer() // Even if `self` is shadowed,
        something() // this should still refer `MyCls.something`.
    }
}

// MARK: fix method called 'self' can be confused with regular 'self' https://bugs.swift.org/browse/SR-4559

func funcThatReturnsSomething(_ any: Any) -> Any {
    any
}

struct TypeWithSelfMethod {
    
    let property = self // expected-warning {{'self' refers to the method 'TypeWithSelfMethod.self', which may be unexpected}} expected-note{{use 'TypeWithSelfMethod.self' to silence this warning}} {{20-20=TypeWithSelfMethod.}}
    
    // Existing warning expected, not confusable
    let property2 = self() // expected-error {{cannot use instance member 'self' within property initializer; property initializers run before 'self' is available}}
    
    let propertyFromClosure: () = {
        print(self) // expected-warning {{'self' refers to the method 'TypeWithSelfMethod.self', which may be unexpected}} expected-note{{use 'TypeWithSelfMethod.self' to silence this warning}} {{15-15=TypeWithSelfMethod.}}
    }()
    
    let propertyFromFunc = funcThatReturnsSomething(self) // expected-warning {{'self' refers to the method 'TypeWithSelfMethod.self', which may be unexpected}} expected-note{{use 'TypeWithSelfMethod.self' to silence this warning}} {{53-53=TypeWithSelfMethod.}}
    
    let propertyFromFunc2 = funcThatReturnsSomething(TypeWithSelfMethod.self) // OK
    
    func `self`() {
        
    }
}

/// Test fix_unqualified_access_member_named_self doesn't appear for computed var called `self`
/// it can't currently be referenced as a static member -- unlike a method with the same name
struct TypeWithSelfComputedVar {
    
    let property = self // expected-error {{cannot use instance member 'self' within property initializer; property initializers run before 'self' is available}}
    
    let propertyFromClosure: () = {
        print(self) // expected-error {{cannot use instance member 'self' within property initializer; property initializers run before 'self' is available}}
    }()
    
    let propertyFromFunc = funcThatReturnsSomething(self) // expected-error {{cannot use instance member 'self' within property initializer; property initializers run before 'self' is available}}
    
    var `self`: () {
        ()
    }
}

/// Test fix_unqualified_access_member_named_self doesn't appear appear for property called `self`
/// it can't currently be referenced as a static member -- unlike a method with the same name
struct TypeWithSelfProperty {
    
    let property = self // expected-error {{cannot use instance member 'self' within property initializer; property initializers run before 'self' is available}}
    
    let propertyFromClosure: () = {
        print(self) // expected-error {{cannot use instance member 'self' within property initializer; property initializers run before 'self' is available}}
    }()
    
    let propertyFromFunc = funcThatReturnsSomething(self) // expected-error {{cannot use instance member 'self' within property initializer; property initializers run before 'self' is available}}
    
    let `self`: () = ()
}
