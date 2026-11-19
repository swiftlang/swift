// RUN: %target-swift-frontend -O -sil-verify-all -verify -emit-sil -enable-library-evolution %s

// rdar://173376043 (Improve diagnostics for Compiler crash: ~Copyable protocol extension with public settable property + library evolution)
// The key path accessors of the property require that 'Self' be copyable, so
// they do not copy a non-copyable 'Self'.
public protocol NonCopyableWithVar: ~Copyable {}
extension NonCopyableWithVar where Self: ~Copyable {
    public var value: Int32 {
        get { 0 }
        set {}
    }
}

