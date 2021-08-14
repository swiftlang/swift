// RUN: %target-typecheck-verify-swift -enable-objc-interop -import-objc-header %S/../Inputs/objc_direct.h

// REQUIRES: objc_interop

class BarSubclass: Bar {
    override func directMethod() -> String! {
        // expected-error@-1 {{instance method overrides a 'final' instance method}}
        // expected-error@-2 {{overriding non-open instance method outside of its defining module}}
        ""
    }
    override func directMethod2() -> String! {
        // expected-error@-1 {{instance method overrides a 'final' instance method}}
        // expected-error@-2 {{overriding non-open instance method outside of its defining module}}
        ""
    }
    override static func directClassMethod() -> String! {
        // expected-error@-1 {{static method overrides a 'final' class method}}
        // expected-error@-2 {{overriding non-open static method outside of its defining module}}
        ""
    }
    override static func directClassMethod2() -> String! {
        // expected-error@-1 {{static method overrides a 'final' class method}}
        // expected-error@-2 {{overriding non-open static method outside of its defining module}}
        ""
    }
    override func directProtocolMethod() -> String! {
        // expected-error@-1 {{instance method overrides a 'final' instance method}}
        // expected-error@-2 {{overriding non-open instance method outside of its defining module}}
        ""
    }
    override var directProperty: Int32 {
        // expected-error@-1 {{property overrides a 'final' property}}
        // expected-error@-2 {{overriding non-open property outside of its defining module}}
        get {0}
        set(value) {}
    }
    override var directProperty2: Int32 {
        // expected-error@-1 {{property overrides a 'final' property}}
        // expected-error@-2 {{overriding non-open property outside of its defining module}}
        get {0}
        set(value) {}
    }
    override subscript(index: Int32) -> Int32 {
        // expected-error@-1 {{subscript overrides a 'final' subscript}}
        // expected-error@-2 {{overriding non-open subscript outside of its defining module}}
        get {0}
        set(value) {}
    }
}
