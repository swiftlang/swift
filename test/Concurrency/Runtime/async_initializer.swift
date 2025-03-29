// RUN: %target-run-simple-swift(-parse-as-library  -target %target-swift-5.1-abi-triple) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: freestanding

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.1, *)
actor NameGenerator {
  private var counter = 0
  private var prefix : String
  init(_ title: String) { self.prefix = title }
  func getName() -> String {
    counter += 1
    return "\(prefix) \(counter)"
   }
}

@available(SwiftStdlib 5.1, *)
protocol Person {
  init() async
  var name : String { get set }
}

@available(SwiftStdlib 5.1, *)
class EarthPerson : Person {
  private static let oracle = NameGenerator("Earthling")

  var name : String

  required init() async {
    self.name = await EarthPerson.oracle.getName()
  }

  init(name: String) async {
    self.name = await (detach { name }).get()
  }
}

@available(SwiftStdlib 5.1, *)
class NorthAmericaPerson : EarthPerson {
  private static let oracle = NameGenerator("NorthAmerican")
  required init() async {
    await super.init()
    self.name = await NorthAmericaPerson.oracle.getName()
  }

  override init(name: String) async {
    await super.init(name: name)
  }
}

@available(SwiftStdlib 5.1, *)
class PrecariousClass {
  init?(nilIt : Int) async {
    let _ : Optional<Int> = await (detach { nil }).get()
    return nil
  }

  init(throwIt : Double) async throws {
    if await (detach { 0 }).get() != 1 {
      throw Something.bogus
    }
  }

  init?(nilOrThrowIt shouldThrow: Bool) async throws {
    let flag = await (detach { shouldThrow }).get()
    if flag {
      throw Something.bogus
    }
    return nil
  }

  init!(crashOrThrowIt shouldThrow: Bool) async throws {
    let flag = await (detach { shouldThrow }).get()
    if flag {
      throw Something.bogus
    }
    return nil
  }
}

enum Something : Error {
  case bogus
}

@available(SwiftStdlib 5.1, *)
struct PrecariousStruct {
  init?(nilIt : Int) async {
    let _ : Optional<Int> = await (detach { nil }).get()
    return nil
  }

  init(throwIt : Double) async throws {
    if await (detach { 0 }).get() != 1 {
      throw Something.bogus
    }
  }
}



// CHECK: Earthling 1
// CHECK-NEXT: Alice
// CHECK-NEXT: Earthling 2
// CHECK-NEXT: Bob
// CHECK-NEXT: Earthling 3
// CHECK-NEXT: Alex
// CHECK-NEXT: NorthAmerican 1
// CHECK-NEXT: NorthAmerican 2
// CHECK-NEXT: Earthling 6

// CHECK-NEXT: class was nil
// CHECK-NEXT: class threw
// CHECK-NEXT: nilOrThrowIt init was nil
// CHECK-NEXT: nilOrThrowIt init threw
// CHECK-NEXT: crashOrThrowIt init threw
// CHECK-NEXT: struct was nil
// CHECK-NEXT: struct threw
// CHECK: done

@available(SwiftStdlib 5.1, *)
@main struct RunIt {
  static func main() async {
    let people : [Person] = [
      await EarthPerson(),
      await NorthAmericaPerson(name: "Alice"),
      await EarthPerson(),
      await NorthAmericaPerson(name: "Bob"),
      await EarthPerson(),
      await NorthAmericaPerson(name: "Alex"),
      await NorthAmericaPerson(),
      await NorthAmericaPerson(),
      await EarthPerson()
    ]

    for p in people {
     print("\(p.name)")
    }

    // ----

    if await PrecariousClass(nilIt: 0) == nil {
      print("class was nil")
    }

    do { let _ = try await PrecariousClass(throwIt: 0.0) } catch {
      print("class threw")
    }

    if try! await PrecariousClass(nilOrThrowIt: false) == nil {
      print("nilOrThrowIt init was nil")
    }

    do { let _ = try await PrecariousClass(nilOrThrowIt: true) } catch {
      print("nilOrThrowIt init threw")
    }

    do { let _ = try await PrecariousClass(crashOrThrowIt: true) } catch {
      print("crashOrThrowIt init threw")
    }

    if await PrecariousStruct(nilIt: 0) == nil {
      print("struct was nil")
    }

    do { let _ = try await PrecariousStruct(throwIt: 0.0) } catch {
      print("struct threw")
    }

    print("done")
  }
}
