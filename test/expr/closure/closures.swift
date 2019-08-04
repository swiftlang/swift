// RUN: %target-typecheck-verify-swift

func doStuff(_ fn : @escaping () -> Int) {}
func doVoidStuff(_ fn : @escaping () -> ()) {}

// <rdar://problem/16193162> Require specifying self for locations in code where strong reference cycles are likely
class ExplicitSelfRequiredTest {
  var x = 42
  func method() -> Int {
    // explicit closure requires an explicit "self." base or an explicit capture.
    doVoidStuff({ self.x += 1 })
    doVoidStuff({ [self] in x += 1 })
    doVoidStuff({ [self = self] in x += 1 })
    doStuff({ [self] in x+1 })
    doStuff({ [self = self] in x+1 })
    doStuff({ self.x+1 })
    doStuff({ x+1 })    // expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{14-14= [self] in}} expected-note{{reference 'self.' explicitly}} {{15-15=self.}}
    doVoidStuff({ doStuff({ x+1 })}) // expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{28-28= [self] in}} expected-note{{reference 'self.' explicitly}} {{29-29=self.}}
    doVoidStuff({ x += 1 })    // expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{18-18= [self] in}} expected-note{{reference 'self.' explicitly}} {{19-19=self.}}
    doVoidStuff({ [y = self] in x += 1 }) // expected-warning {{capture 'y' was never used}} expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{20-20=self, }} expected-note{{reference 'self.' explicitly}} {{33-33=self.}}
    doStuff({ [y = self] in x+1 }) // expected-warning {{capture 'y' was never used}} expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{16-16=self, }} expected-note{{reference 'self.' explicitly}} {{29-29=self.}}
    doVoidStuff({ [weak self] in x += 1 }) // expected-note {{weak capture of 'self' here does not enable implicit 'self'}} expected-warning {{variable 'self' was written to, but never read}} expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}}
    doStuff({ [weak self] in x+1 }) // expected-note {{weak capture of 'self' here does not enable implicit 'self'}} expected-warning {{variable 'self' was written to, but never read}} expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}}
    doVoidStuff({ [self = self.x] in x += 1 }) // expected-note {{variable other than 'self' captured here under the name 'self' does not enable implicit 'self'}} expected-warning {{capture 'self' was never used}} expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}}
    doStuff({ [self = self.x] in x+1 }) // expected-note {{variable other than 'self' captured here under the name 'self' does not enable implicit 'self'}} expected-warning {{capture 'self' was never used}} expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}}

    // Methods follow the same rules as properties, uses of 'self' without capturing must be marked with "self."
    doStuff { method() }  // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{14-14= [self] in}} expected-note{{reference 'self.' explicitly}} {{15-15=self.}}
    doVoidStuff { _ = method() }  // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{18-18= [self] in}} expected-note{{reference 'self.' explicitly}} {{23-23=self.}}
    doVoidStuff { () -> () in _ = method() }  // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{18-18= [self]}} expected-note{{reference 'self.' explicitly}} {{35-35=self.}}
    doVoidStuff { [y = self] in _ = method() } // expected-warning {{capture 'y' was never used}} expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{20-20=self, }} expected-note{{reference 'self.' explicitly}} {{37-37=self.}}
    doStuff({ [y = self] in method() }) // expected-warning {{capture 'y' was never used}} expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{16-16=self, }} expected-note{{reference 'self.' explicitly}} {{29-29=self.}}
    doVoidStuff({ [weak self] in _ = method() }) // expected-note {{weak capture of 'self' here does not enable implicit 'self'}} expected-warning {{variable 'self' was written to, but never read}} expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}}
    doStuff({ [weak self] in method() }) // expected-note {{weak capture of 'self' here does not enable implicit 'self'}} expected-warning {{variable 'self' was written to, but never read}} expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}}
    doVoidStuff({ [self = self.x] in _ = method() }) // expected-note {{variable other than 'self' captured here under the name 'self' does not enable implicit 'self'}} expected-warning {{capture 'self' was never used}} expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}}
    doStuff({ [self = self.x] in method() }) // expected-note {{variable other than 'self' captured here under the name 'self' does not enable implicit 'self'}} expected-warning {{capture 'self' was never used}} expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}}
    doVoidStuff { _ = self.method() }
    doVoidStuff { [self] in _ = method() }
    doVoidStuff { [self = self] in _ = method() }
    doStuff { self.method() }
    doStuff { [self] in method() }
    doStuff({ [self = self] in method() })
    
    // When there's no space between the opening brace and the first expression, insert it
    doStuff {method() }  // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{14-14= [self] in }} expected-note{{reference 'self.' explicitly}} {{14-14=self.}}
    doVoidStuff {_ = method() }  // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{18-18= [self] in }} expected-note{{reference 'self.' explicitly}} {{22-22=self.}}
    doVoidStuff {() -> () in _ = method() }  // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{18-18= [self] }} expected-note{{reference 'self.' explicitly}} {{34-34=self.}}
    // With an empty capture list, insertion should should be suggested without a comma
    doStuff { [] in method() } // expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{16-16=self}} expected-note{{reference 'self.' explicitly}} {{21-21=self.}}
    
    // If we already have a capture list, self should be added to the list
    let y = 1
    doStuff { [y] in method() } // expected-warning {{capture 'y' was never used}} expected-error {{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{16-16=self, }} expected-note{{reference 'self.' explicitly}} {{22-22=self.}}

    // <rdar://problem/18877391> "self." shouldn't be required in the initializer expression in a capture list
    // This should not produce an error, "x" isn't being captured by the closure.
    doStuff({ [myX = x] in myX })

    // This should produce an error, since x is used within the inner closure.
    doStuff({ [myX = {x}] in 4 })    // expected-error {{reference to property 'x' in closure requires explicit use of 'self' to make capture semantics explicit}} expected-note{{capture 'self' explicitly to enable implicit 'self' in this closure}} {{23-23= [self] in }} expected-note{{reference 'self.' explicitly}} {{23-23=self.}}
    // expected-warning @-1 {{capture 'myX' was never used}}

    return 42
  }
}
