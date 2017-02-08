// RUN: %target-typecheck-verify-swift

class A {
  init() {}
}

class B {
  init() {}
    
  convenience init(x: ()) {
    class C: A {
      override init() { // No error
        super.init()
      }
    }
      
    class D: A {
      convenience init(x: ()) {
        class DI : A {
          override init() { // No error
            super.init()
          }
        }

        self.init()
      }

      override init() { // No error
        class DI : A {
          override init() { // No error
            super.init()
          }
        }
        super.init()
      }
    }

    struct E {
      init() {} // No error
    }
 
    self.init()
  }
}

