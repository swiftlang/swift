// RUN: %target-typecheck-verify-swift -swift-version 6
// REQUIRES: asserts

func doStuff(_ fn : @escaping () -> Int) {}
func doVoidStuff(_ fn : @escaping () -> ()) {}
func doVoidStuffNonEscaping(_ fn: () -> ()) {}

class ExplicitSelfRequiredTest {
  var x = 42
  func method() -> Int {
    doVoidStuff({ doStuff({ x+1 })}) // expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{28-28= [self] in}} expected-note{{reference 'self.' explicitly}} {{29-29=self.}}
    return 42
  }
  
  func weakSelfError() {
    doVoidStuff({ [weak self] in x += 1 }) // expected-error {{explicit use of 'self' is required when 'self' is optional, to make control flow explicit}} expected-note {{reference 'self?.' explicitly}}
    doVoidStuffNonEscaping({ [weak self] in x += 1 }) // expected-error {{explicit use of 'self' is required when 'self' is optional, to make control flow explicit}} expected-note {{reference 'self?.' explicitly}}
    doStuff({ [weak self] in x+1 }) // expected-error {{explicit use of 'self' is required when 'self' is optional, to make control flow explicit}} expected-note {{reference 'self?.' explicitly}}
    doVoidStuff({ [weak self] in _ = method() }) // expected-error {{explicit use of 'self' is required when 'self' is optional, to make control flow explicit}} expected-note{{reference 'self?.' explicitly}}
    doVoidStuffNonEscaping({ [weak self] in _ = method() }) // expected-error {{explicit use of 'self' is required when 'self' is optional, to make control flow explicit}} expected-note{{reference 'self?.' explicitly}}
    doStuff({ [weak self] in method() }) // expected-error {{explicit use of 'self' is required when 'self' is optional, to make control flow explicit}} expected-note{{reference 'self?.' explicitly}}
  }
}

// https://github.com/apple/swift/issues/56501
class C_56501 {
  func operation() {}

  func test1() {
    doVoidStuff { [self] in
      operation()
    }
  }

  func test2() {
    doVoidStuff { [self] in
      doVoidStuff {
        // expected-error@+3 {{call to method 'operation' in closure requires explicit use of 'self'}}
        // expected-note@-2 {{capture 'self' explicitly to enable implicit 'self' in this closure}}
        // expected-note@+1 {{reference 'self.' explicitly}}
        operation()
      }
    }
  }

  func test3() {
    doVoidStuff { [self] in
      doVoidStuff { [self] in
        operation()
      }
    }
  }

  func test4() {
    doVoidStuff { [self] in
      doVoidStuff {
        self.operation()
      }
    }
  }

  func test5() {
    doVoidStuff { [self] in
      doVoidStuffNonEscaping {
        operation()
      }
    }
  }

  func test6() {
    doVoidStuff { [self] in
      doVoidStuff { [self] in
        doVoidStuff {
          // expected-error@+3 {{call to method 'operation' in closure requires explicit use of 'self'}}
          // expected-note@-2 {{capture 'self' explicitly to enable implicit 'self' in this closure}}
          // expected-note@+1 {{reference 'self.' explicitly}}
          operation()
        }
      }
    }
  }
}

public class TestImplicitSelfForWeakSelfCapture {
  static let staticOptional: TestImplicitSelfForWeakSelfCapture? = .init()
  func method() { }
  
  private init() {
    doVoidStuff { [weak self] in
      method() // expected-error {{explicit use of 'self' is required when 'self' is optional, to make control flow explicit}} expected-note {{reference 'self?.' explicitly}}
      guard let self = self else { return }
      method()
    }

    doVoidStuff { [weak self] in
      guard let self else { return }
      method()
    }
    
    doVoidStuff { [weak self] in
      if let self = self {
        method()
      }

      if let self {
        method()
      }
    }
    
    doVoidStuff { [weak self] in
      guard let self = self else { return }
      doVoidStuff { // expected-note {{capture 'self' explicitly to enable implicit 'self' in this closure}}
        method() // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note {{reference 'self.' explicitly}}
      }
    }
    
    doVoidStuff { [weak self] in
      guard let self = self ?? TestImplicitSelfForWeakSelfCapture.staticOptional else { return }
      method() // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}}
    }
    
    doVoidStuffNonEscaping { [weak self] in
      method() // expected-error {{explicit use of 'self' is required when 'self' is optional, to make control flow explicit}} expected-note {{reference 'self?.' explicitly}}
      guard let self = self else { return }
      method()
    }
    
    doVoidStuffNonEscaping { [weak self] in
      if let self = self {
        method()
      }
    }
    
    doVoidStuff { [weak self] in
      let `self`: TestImplicitSelfForWeakSelfCapture? = self ?? TestImplicitSelfForWeakSelfCapture.staticOptional
      guard let self = self else { return }
      method() // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}}
    }
    
    doVoidStuffNonEscaping { [weak self] in
      let `self`: TestImplicitSelfForWeakSelfCapture? = self ?? TestImplicitSelfForWeakSelfCapture.staticOptional
      guard let self = self else { return }
      method() // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}}
    }
    
    doVoidStuffNonEscaping { [weak self] in
      guard let self = self else { return }
      doVoidStuff { // expected-note {{capture 'self' explicitly to enable implicit 'self' in this closure}}
        method() // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note {{reference 'self.' explicitly}}
      }
    }
    
    doVoidStuffNonEscaping { [weak self] in
      guard let self = self ?? TestImplicitSelfForWeakSelfCapture.staticOptional else { return }
      method() // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}}
    }
    
    doVoidStuff { [weak self] in
      func innerFunction1() {
          method() // expected-error {{explicit use of 'self' is required when 'self' is optional, to make control flow explicit}} expected-note {{reference 'self?.' explicitly}}
          self?.method()
      }
      
      guard let self else { return }
      
      func innerFunction2() {
          method()
          self.method()
      }
    }
    
    doVoidStuffNonEscaping { [weak self] in
      func innerFunction1() {
          method() // expected-error {{explicit use of 'self' is required when 'self' is optional, to make control flow explicit}} expected-note {{reference 'self?.' explicitly}}
          self?.method()
      }
      
      guard let self else { return }
      
      func innerFunction2() {
          method()
          self.method()
      }
    }
  }
}

class NoImplicitSelfInInnerClass {
  func method() { }
  
  private init() { // expected-note {{'self' declared here}} expected-note {{'self' declared here}}
    doVoidStuff {
      class InnerType { // expected-note {{type declared here}}
          func functionInsideInnerType() {
            method() // expected-error {{class declaration cannot close over value 'self' defined in outer scope}}
            self.method() // expected-error {{value of type 'InnerType' has no member 'method'}}
          }
      }
    }
    
    doVoidStuff { [weak self] in
      guard let self else { return }
      method()
      
      class InnerType { // expected-note {{type declared here}}
          func functionInsideInnerType() {
            method() // expected-error {{class declaration cannot close over value 'self' defined in outer scope}}
            self.method() // expected-error {{value of type 'InnerType' has no member 'method'}}
          }
      }
    }
  }
  
  func foo(condition: Bool) {
    doVoidStuff { [weak self] in
      guard condition, let self else { return }
      method()
    }
    
    doVoidStuff { [weak self] in
      guard let self, condition else { return }
      method()
    }
    
    doVoidStuffNonEscaping { [weak self] in
      guard condition, let self else { return }
      method()
    }
    
    doVoidStuffNonEscaping { [weak self] in
      guard let self, condition else { return }
      method()
    }
  }

  func foo(optionalCondition: Bool?) {
    doVoidStuff { [weak self] in
      guard let optionalCondition, optionalCondition, let self else { return }
      method()
    }
    
    doVoidStuff { [weak self] in
      guard let self, let optionalCondition, optionalCondition else { return }
      method()
    }
    
    doVoidStuff { [weak self] in
      guard let optionalCondition, let self, optionalCondition else { return }
      method()
    }
    
    doVoidStuffNonEscaping { [weak self] in
      guard let optionalCondition, optionalCondition, let self else { return }
      method()
    }
    
    doVoidStuffNonEscaping { [weak self] in
      guard let self, let optionalCondition, optionalCondition else { return }
      method()
    }
    
    doVoidStuffNonEscaping { [weak self] in
      guard let optionalCondition, let self, optionalCondition else { return }
      method()
    }
  }
  
  func foo() {
    doVoidStuff { [weak self] in
      guard #available(SwiftStdlib 5.8, *), let self else { return }
      method()
    }
    
    doVoidStuff { [weak self] in
      guard let self, #available(SwiftStdlib 5.8, *) else { return }
      method()
    }
    
    doVoidStuffNonEscaping { [weak self] in
      guard #available(SwiftStdlib 5.8, *), let self else { return }
      method()
    }
    
    doVoidStuffNonEscaping { [weak self] in
      guard let self, #available(SwiftStdlib 5.8, *) else { return }
      method()
    }
  }
}

public class TestRebindingSelfIsDisallowed {
  let count: Void = ()
  
  private init() {
    doVoidStuff {
      let `self` = "self shouldn't become a string"
      let _: Int = count // expected-error{{cannot convert value of type 'Void' to specified type 'Int'}}
    }
    
    doVoidStuffNonEscaping {
      let `self` = "self shouldn't become a string"
      let _: Int = count // expected-error{{cannot convert value of type 'Void' to specified type 'Int'}}
    }
    
    doVoidStuff { [weak self] in
      let `self` = "self shouldn't become a string"
      let _: Int = count // expected-error{{reference to property 'count' in closure requires explicit use of 'self' to make capture semantics explicit}}
    }
    
    doVoidStuffNonEscaping { [weak self] in
      let `self` = "self shouldn't become a string"
      let _: Int = count // expected-error{{reference to property 'count' in closure requires explicit use of 'self' to make capture semantics explicit}}
    }
  }
  
  func method() {
    let `self` = "self shouldn't become a string"
    let _: Int = count // expected-error{{cannot convert value of type 'Void' to specified type 'Int'}}
  }
}
