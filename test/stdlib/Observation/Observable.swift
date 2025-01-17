// REQUIRES: swift_swift_parser, executable_test

// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library -enable-experimental-feature Macros -Xfrontend -plugin-path -Xfrontend %swift-plugin-dir)

// Run this test via the swift-plugin-server
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library -enable-experimental-feature Macros -Xfrontend -external-plugin-path -Xfrontend %swift-plugin-dir#%swift-plugin-server)

// REQUIRES: observation
// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: swift_feature_Macros
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import Observation

@usableFromInline
@inline(never)
func _blackHole<T>(_ value: T) { }

@Observable
class ContainsNothing { }

@Observable
class ContainsWeak {
  weak var obj: AnyObject? = nil
}

@Observable
public class PublicContainsWeak {
  public weak var obj: AnyObject? = nil
}

@Observable
class ContainsUnowned {
  unowned var obj: AnyObject? = nil
}

@Observable
class ContainsIUO {
  var obj: Int! = nil
}

class NonObservable {

}

@Observable
class InheritsFromNonObservable: NonObservable {

}

protocol NonObservableProtocol {

}

@Observable
class ConformsToNonObservableProtocol: NonObservableProtocol {

}

struct NonObservableContainer {
  @Observable
  class ObservableContents {
    var field: Int = 3
  }
}

@Observable
final class SendableClass: Sendable {
  var field: Int = 3
}

@Observable
class CodableClass: Codable {
  var field: Int = 3
}

@Observable
final class HashableClass {
  var field: Int = 3
}

extension HashableClass: Hashable {
  static func == (lhs: HashableClass, rhs: HashableClass) -> Bool {
    lhs.field == rhs.field
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(field)
  }
}

@Observable
class ImplementsAccessAndMutation {
  var field = 3
  let accessCalled: (PartialKeyPath<ImplementsAccessAndMutation>) -> Void
  let withMutationCalled: (PartialKeyPath<ImplementsAccessAndMutation>) -> Void

  init(accessCalled: @escaping (PartialKeyPath<ImplementsAccessAndMutation>) -> Void, withMutationCalled: @escaping (PartialKeyPath<ImplementsAccessAndMutation>) -> Void) {
    self.accessCalled = accessCalled
    self.withMutationCalled = withMutationCalled
  }

  internal func access<Member>(
      keyPath: KeyPath<ImplementsAccessAndMutation , Member>
  ) {
    accessCalled(keyPath)
    _$observationRegistrar.access(self, keyPath: keyPath)
  }

  internal func withMutation<Member, T>(
    keyPath: KeyPath<ImplementsAccessAndMutation , Member>,
    _ mutation: () throws -> T
  ) rethrows -> T {
    withMutationCalled(keyPath)
    return try _$observationRegistrar.withMutation(of: self, keyPath: keyPath, mutation)
  }
}

@Observable
class HasIgnoredProperty {
  var field = 3
  @ObservationIgnored var ignored = 4
}

@Observable
class Entity {
  var age: Int = 0
}

@Observable
class Person : Entity {
  var firstName = ""
  var lastName = ""

  var friends = [Person]()

  var fullName: String { firstName + " " + lastName }
}

@Observable
class MiddleNamePerson: Person {
  var middleName = ""

  override var fullName: String { firstName + " " + middleName + " " + lastName }
}

@Observable
class IsolatedClass {
  @MainActor var test = "hello"
}

@MainActor
@Observable
class IsolatedInstance {
  var test = "hello"
}

@Observable
class IgnoredComputed {
  @ObservationIgnored
  var message: String { "hello" }
}

@Observable
class ClassHasExistingConformance: Observable { }

protocol Intermediary: Observable { }

@Observable
class HasIntermediaryConformance: Intermediary { }

class CapturedState<State>: @unchecked Sendable {
  var state: State

  init(state: State) {
    self.state = state
  }
}

@Observable
class RecursiveInner {
  var value = "prefix"
}

@Observable
class RecursiveOuter {
  var inner = RecursiveInner()
  var value = "prefix"
  @ObservationIgnored var innerEventCount = 0
  @ObservationIgnored var outerEventCount = 0

  func recursiveTrackingCalls() {
    withObservationTracking({
      let _ = value
      _ = withObservationTracking({
        inner.value
      }, onChange: {
        self.innerEventCount += 1
      })
    }, onChange: {
      self.outerEventCount += 1
    })
  }
}

@Observable
#if FOO
@available(SwiftStdlib 5.9, *)
#elseif BAR
@available(SwiftStdlib 5.9, *)
#else
#endif
class GuardedAvailability {
}

@Observable class TestASTScopeLCA {
  // Make sure ASTScope unqualified lookup can find local variables
  // inside initial values with closures when accessor macros are
  // involved.
  var state : Bool = {
    let value = true
    return value
  }()
}

@Observable class Parent {
  class Nested {}
}

extension Parent.Nested {}

struct CowContainer {
  final class Contents { }
  
  var contents = Contents()
  
  mutating func mutate() {
    if !isKnownUniquelyReferenced(&contents) {
      contents = Contents()
    }
  }
  
  var id: ObjectIdentifier {
    ObjectIdentifier(contents)
  }
}


@Observable
final class CowTest {
  var container = CowContainer()
}

@main
struct Validator {
  @MainActor
  static func main() {

    
    let suite = TestSuite("Observable")

    suite.test("only instantiate") {
      let test = MiddleNamePerson()
    }
    
    suite.test("unobserved value changes") {
      let test = MiddleNamePerson()
      for i in 0..<100 {
        test.firstName = "\(i)"
      }
    }
    
    suite.test("tracking changes") {
      let changed = CapturedState(state: false)
      
      let test = MiddleNamePerson()
      withObservationTracking {
        _blackHole(test.firstName)
      } onChange: {
        changed.state = true
      }
      
      test.firstName = "c"
      expectEqual(changed.state, true)
      changed.state = false
      test.firstName = "c"
      expectEqual(changed.state, false)
    }

    suite.test("conformance") {
      func testConformance<O: Observable>(_ o: O) -> Bool {
        return true
      }
      
      func testConformance<O>(_ o: O) -> Bool {
        return false
      }
      
      let test = Person()
      expectEqual(testConformance(test), true)
    }
    
    suite.test("tracking nonchanged") {
      let changed = CapturedState(state: false)
      
      let test = MiddleNamePerson()
      withObservationTracking {
        _blackHole(test.lastName)
      } onChange: {
        changed.state = true
      }
      
      test.firstName = "c"
      expectEqual(changed.state, false)
    }
    
    suite.test("tracking computed") {
      let changed = CapturedState(state: false)
      
      let test = MiddleNamePerson()
      withObservationTracking {
        _blackHole(test.fullName)
      } onChange: {
        changed.state = true
      }
      
      test.middleName = "c"
      expectEqual(changed.state, true)
      changed.state = false
      test.middleName = "c"
      expectEqual(changed.state, false)
    }
    
    suite.test("graph changes") {
      let changed = CapturedState(state: false)
      
      let test = MiddleNamePerson()
      let friend = MiddleNamePerson()
      test.friends.append(friend)
      withObservationTracking {
        _blackHole(test.friends.first?.fullName)
      } onChange: {
        changed.state = true
      }

      test.middleName = "c"
      expectEqual(changed.state, false)
      friend.middleName = "c"
      expectEqual(changed.state, true)
    }
    
    suite.test("nesting") {
      let changedOuter = CapturedState(state: false)
      let changedInner = CapturedState(state: false)
      
      let test = MiddleNamePerson()
      withObservationTracking {
        withObservationTracking {
          _blackHole(test.firstName)
        } onChange: {
          changedInner.state = true
        }
      } onChange: {
        changedOuter.state = true
      }
      
      test.firstName = "c"
      expectEqual(changedInner.state, true)
      expectEqual(changedOuter.state, true)
      changedOuter.state = false
      test.firstName = "c"
      expectEqual(changedOuter.state, false)
    }
    
    suite.test("access and mutation") {
      let accessKeyPath = CapturedState<PartialKeyPath<ImplementsAccessAndMutation>?>(state: nil)
      let mutationKeyPath = CapturedState<PartialKeyPath<ImplementsAccessAndMutation>?>(state: nil)
      let test = ImplementsAccessAndMutation { keyPath in
        accessKeyPath.state = keyPath
      } withMutationCalled: { keyPath in
        mutationKeyPath.state = keyPath
      }
      
      expectEqual(accessKeyPath.state, nil)
      _blackHole(test.field)
      expectEqual(accessKeyPath.state, \.field)
      expectEqual(mutationKeyPath.state, nil)
      accessKeyPath.state = nil
      test.field = 123
      expectEqual(accessKeyPath.state, nil)
      expectEqual(mutationKeyPath.state, \.field)
    }
    
    suite.test("ignores no change") {
      let changed = CapturedState(state: false)
      
      let test = HasIgnoredProperty()
      withObservationTracking {
        _blackHole(test.ignored)
      } onChange: {
        changed.state = true
      }
      
      test.ignored = 122112
      expectEqual(changed.state, false)
      changed.state = false
      test.field = 3429
      expectEqual(changed.state, false)
    }
    
    suite.test("ignores change") {
      let changed = CapturedState(state: false)
      
      let test = HasIgnoredProperty()
      withObservationTracking {
        _blackHole(test.ignored)
        _blackHole(test.field)
      } onChange: {
        changed.state = true
      }
      
      test.ignored = 122112
      expectEqual(changed.state, false)
      changed.state = false
      test.field = 3429
      expectEqual(changed.state, true)
    }

    suite.test("isolated class") { @MainActor in
      let changed = CapturedState(state: false)
      
      let test = IsolatedClass()
      withObservationTracking {
        _blackHole(test.test)
      } onChange: {
        changed.state = true
      }
      
      test.test = "c"
      expectEqual(changed.state, true)
      changed.state = false
      test.test = "c"
      expectEqual(changed.state, false)
    }

    suite.test("recursive tracking inner then outer") {
      let obj = RecursiveOuter()
      obj.recursiveTrackingCalls()
      obj.inner.value = "test"
      expectEqual(obj.innerEventCount, 1)
      expectEqual(obj.outerEventCount, 1)
      obj.recursiveTrackingCalls()
      obj.value = "test"
      expectEqual(obj.innerEventCount, 1)
      expectEqual(obj.outerEventCount, 2)
    }

    suite.test("recursive tracking outer then inner") {
      let obj = RecursiveOuter()
      obj.recursiveTrackingCalls()
      obj.value = "test"
      expectEqual(obj.innerEventCount, 0)
      expectEqual(obj.outerEventCount, 1)
      obj.recursiveTrackingCalls()
      obj.inner.value = "test"
      expectEqual(obj.innerEventCount, 2)
      expectEqual(obj.outerEventCount, 2)
    }
    
    suite.test("validate copy on write semantics") {
      let subject = CowTest()
      let startId = subject.container.id
      expectEqual(subject.container.id, startId)
      subject.container.mutate()
      expectEqual(subject.container.id, startId)
    }

    runAllTests()
  }
}


