# Embedded Swift language restrictions (EmbeddedRestrictions)

Embedded Swift is a subset of the Swift language that that introduces some restrictions on the use of language features to eliminate the need for the Swift runtime. These restrictions are captured by the `EmbeddedRestrictions` diagnostic group.

The Embedded Swift compilation model can produce extremely small binaries without external dependencies, suitable for restricted environments including embedded (microcontrollers) and baremetal setups (no operating system at all), and low-level environments (firmware, kernels, device drivers, low-level components of userspace OS runtimes). While the vast majority of Swift language features are available in Embedded Swift, there are some language features that require the full Swift standard library and runtime, which are not available in Embedded Swift.

Diagnostics in the `EmbeddedRestrictions` group describe those language features that cannot be used in Embedded Swift. These include:

* `weak` and `unowned` references, because Embedded Swift uses a simplified reference-counting model that cannot support them. For example:

      class Node {
        weak var parent: Node?    // error: attribute 'weak' cannot be used in Embedded Swift
      }

* Dynamic casts to a type involving a protocol are not supported, because Embedded Swift does not include runtime metadata about protocol conformances. For example:

      protocol P: AnyObject { }
      func casting(object: AnyObject) {
        if let p = object as? P { // error: cannot perform a dynamic cast to a type involving protocol 'P' in Embedded Swift
          // ...
        }
      }

* Overriding and `open` generic methods in a class, which are prohibited because they cannot be specialized for every possible call site. For example:

      open class MyGenericClass<T> {
        open func f<U>(value: U) { }  // warning: generic instance method 'f(value:)' in a class cannot be 'open' in Embedded Swift

        func g() { } // okay, not generic relative to the class itself

        class func h() where T: P { }
      }

      class MyGenericSubclass<T>: MyGenericClass<T> {
        override class func h() where T: P { } // warning: generic class method 'h' in a class cannot override another method in Embedded Swift
      }

* Generic methods used on values of protocol type, which are prohibited because they cannot be specialized for every possible call site. For example:

      protocol P: AnyObject {
        func doNothing()
        func doSomething<T>(on value: T)
      }

      func testGenerics<Value: P>(value: value, i: Int) {
        value.doNothing()        // okay
        value.doSomething(on: i) // okay, always specialized
      }

      func testValuesOfProtocolType(value: any P, i: Int) {
        value.doNothing()        // okay
        value.doSomething(on: i) // warning: cannot use generic instance method 'doSomething(on:)' on a value of type 'any P' in Embedded Swift
      }

* Passing a value of protocol type to a generic function that opens the existential, which requires unspecialized generics because the function cannot be specialized for a dynamically-provided type. For example:

      func acceptAny<T>(_ value: T) { }

      func testPassingValueOfProtocolType(value: any P) {
        acceptAny(value)              // warning: cannot open existential type 'any P' when passing it as an argument to global function 'acceptAny' in Embedded Swift
        acceptAny(value as any P)     // okay, passes the existential itself
      }

## See Also

- [A Vision for Embedded Swift](https://github.com/swiftlang/swift-evolution/blob/main/visions/embedded-swift.md)
