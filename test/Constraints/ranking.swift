// RUN: %target-parse-verify-swift

func f0<T>(x: T) {}

// FIXME: Lookup breaks if these come after f1!
class A { 
  init() {} 
};
class B : A { 
  override init() { super.init() } 
}

func f1(a: A) -> A { return a }
func f1(b: B) -> B { return b }

f0(f1(B()))
