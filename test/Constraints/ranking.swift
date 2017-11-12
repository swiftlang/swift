// RUN: %target-typecheck-verify-swift

func f0<T>(_ x: T) {}

// FIXME: Lookup breaks if these come after f1!
class A { 
  init() {} 
};
class B : A { 
  override init() { super.init() } 
}

func f1(_ a: A) -> A { return a }
func f1(_ b: B) -> B { return b }

f0(f1(B()))
