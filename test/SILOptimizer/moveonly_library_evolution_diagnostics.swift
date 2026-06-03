// RUN: %target-swift-frontend -O -sil-verify-all -verify -emit-sil -enable-library-evolution %s

// rdar://173376043 (Improve diagnostics for Compiler crash: ~Copyable protocol extension with public settable property + library evolution)
public protocol NonCopyableWithVar: ~Copyable {}
extension NonCopyableWithVar where Self: ~Copyable {
    public var value: Int32 {
        get { 0 } // expected-error{{copy of noncopyable typed value. This is a compiler bug. Please file a bug with a small example of the bug}}
        set {}
    }
}

