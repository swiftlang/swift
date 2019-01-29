// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/convenience_init_peer_delegation.m -o %t/convenience_init_peer_delegation.objc.o -fmodules -fobjc-arc
// RUN: %target-build-swift -c -o %t/convenience_init_peer_delegation.swift.o -import-objc-header %S/Inputs/convenience_init_peer_delegation.h %s
// RUN: %target-swiftc_driver %t/convenience_init_peer_delegation.objc.o %t/convenience_init_peer_delegation.swift.o -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

extension Base {
  convenience init(swiftToDesignated: ()) {
    print("\(#function) \(type(of: self))")
    self.init()
  }

  convenience init(swiftToConvenience: ()) {
    print("\(#function) \(type(of: self))")
    self.init(conveniently: ())
  }

  convenience init(swiftToConvenienceFactory: ()) {
    print("\(#function) \(type(of: self))")
    self.init(convenientFactory: false)
  }

  convenience init(swiftToNormalFactory: ()) {
    // FIXME: This shouldn't be allowed, since the factory won't actually use
    // the dynamic Self type.
    print("\(#function) \(type(of: self))")
    self.init(normalFactory: false)
  }

  @objc convenience init(objcToDesignated: ()) {
    print("\(#function) \(type(of: self))")
    self.init()
  }
  @objc convenience init(objcToConvenience: ()) {
    print("\(#function) \(type(of: self))")
    self.init(conveniently: ())
  }
  @objc convenience init(objcToConvenienceFactory: ()) {
    print("\(#function) \(type(of: self))")
    self.init(convenientFactory: false)
  }
  @objc convenience init(objcToNormalFactory: ()) {
    // FIXME: This shouldn't be allowed, since the factory won't actually use
    // the dynamic Self type.
    print("\(#function) \(type(of: self))")
    self.init(normalFactory: false)
  }
}

/// Checks that `op` performs `base` allocations of Base and `sub` allocations
/// of Sub.
func check(base: Int = 0, sub: Int = 0,
           file: StaticString = #file, line: UInt = #line,
           op: () -> Void) {
  baseCounter = 0
  subCounter = 0
  op()
  precondition(baseCounter == base,
               "expected \(base) Base instances, got \(baseCounter)",
               file: file, line: line)
  precondition(subCounter == sub,
               "expected \(sub) Sub instances, got \(subCounter)",
               file: file, line: line)
}

// CHECK: START
print("START")

// Check that this whole setup works.
// CHECK-NEXT: init Base
check(base: 1) { _ = Base() }
// CHECK-NEXT: init Sub
check(sub: 1) { _ = Sub() }

// CHECK-NEXT: init(swiftToDesignated:) Sub
// CHECK-NEXT: init Sub
check(sub: 1) { _ = Sub(swiftToDesignated: ()) }
// CHECK-NEXT: init(swiftToConvenience:) Sub
// CHECK-NEXT: -[Base initConveniently]
// CHECK-NEXT: init Sub
check(sub: 1) { _ = Sub(swiftToConvenience: ()) }
// CHECK-NEXT: init(swiftToConvenienceFactory:) Sub
// CHECK-NEXT: +[Base baseWithConvenientFactory:]
// CHECK-NEXT: init Sub
check(sub: 1) { _ = Sub(swiftToConvenienceFactory: ()) }

// FIXME: This shouldn't be allowed in the first place; see the definition 
// above.
// CHECK-NEXT: init(swiftToNormalFactory:) Base
// CHECK-NEXT: +[Base baseWithNormalFactory:]
// CHECK-NEXT: init Base
check(base: 1) { _ = Base(swiftToNormalFactory: ()) }
// CHECK-NEXT: init(swiftToNormalFactory:) Sub
// CHECK-NEXT: +[Base baseWithNormalFactory:]
// CHECK-NEXT: init Base
check(base: 1) { _ = Sub(swiftToNormalFactory: ()) }

// CHECK-NEXT: init(objcToDesignated:) Sub
// CHECK-NEXT: init Sub
check(sub: 1) { _ = Sub(objcToDesignated: ()) }
// CHECK-NEXT: init(objcToConvenience:) Sub
// CHECK-NEXT: -[Base initConveniently]
// CHECK-NEXT: init Sub
check(sub: 1) { _ = Sub(objcToConvenience: ()) }
// CHECK-NEXT: init(objcToConvenienceFactory:) Sub
// CHECK-NEXT: +[Base baseWithConvenientFactory:]
// CHECK-NEXT: init Sub
check(sub: 2) { _ = Sub(objcToConvenienceFactory: ()) }

// FIXME: This shouldn't be allowed in the first place; see the definition 
// above.
// CHECK-NEXT: init(objcToNormalFactory:) Base
// CHECK-NEXT: +[Base baseWithNormalFactory:]
// CHECK-NEXT: init Base
check(base: 2) { _ = Base(objcToNormalFactory: ()) }
// CHECK-NEXT: init(objcToNormalFactory:) Sub
// CHECK-NEXT: +[Base baseWithNormalFactory:]
// CHECK-NEXT: init Base
check(base: 1, sub: 1) { _ = Sub(objcToNormalFactory: ()) }

// CHECK-NEXT: END
print("END")

