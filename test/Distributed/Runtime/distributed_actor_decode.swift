// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor DA: CustomStringConvertible {
  nonisolated var description: String {
    "DA(\(self.id))"
  }
}

// ==== Fake Transport ---------------------------------------------------------

@available(SwiftStdlib 5.5, *)
struct ActorAddress: ActorIdentity {
  let address: String
  init(parse address : String) {
    self.address = address
  }

  // Explicit implementations to make our TestEncoder/Decoder simpler
  init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    self.address = try container.decode(String.self)
    print("decode ActorAddress -> \(self)")
  }

  func encode(to encoder: Encoder) throws {
    print("encode \(self)")
    var container = encoder.singleValueContainer()
    try container.encode(self.address)
  }
}

@available(SwiftStdlib 5.5, *)
struct FakeTransport: ActorTransport {
  func decodeIdentity(from decoder: Decoder) throws -> AnyActorIdentity {
    print("FakeTransport.decodeIdentity from:\(decoder)")
    let address = try ActorAddress(from: decoder)
    return AnyActorIdentity(address)
  }

  func resolve<Act>(_ identity: AnyActorIdentity, as actorType: Act.Type) throws -> Act?
      where Act: DistributedActor {
    print("resolve type:\(actorType), address:\(identity)")
    return nil
  }

  func assignIdentity<Act>(_ actorType: Act.Type) -> AnyActorIdentity
      where Act: DistributedActor {
    let address = ActorAddress(parse: "xxx")
    print("assign type:\(actorType), address:\(address)")
    return .init(address)
  }

  public func actorReady<Act>(_ actor: Act) where Act: DistributedActor {
    print("ready actor:\(actor), address:\(actor.id)")
  }

  func resignIdentity(_ identity: AnyActorIdentity) {
    print("resign address:\(identity)")
  }
}

// ==== Test Coding ------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
class TestEncoder: Encoder {
  var codingPath: [CodingKey]
  var userInfo: [CodingUserInfoKey: Any]

  var data: String? = nil

  init(transport: ActorTransport) {
    self.codingPath = []
    self.userInfo = [.actorTransportKey: transport]
  }

  func container<Key>(keyedBy type: Key.Type) -> KeyedEncodingContainer<Key> {
    fatalError("Not implemented: \(#function)")
  }

  func unkeyedContainer() -> UnkeyedEncodingContainer {
    fatalError("Not implemented: \(#function)")
  }

  func singleValueContainer() -> SingleValueEncodingContainer {
    TestSingleValueEncodingContainer(parent: self)
  }

  class TestSingleValueEncodingContainer: SingleValueEncodingContainer {
    let parent: TestEncoder
    init(parent: TestEncoder) {
      self.parent = parent
    }

    var codingPath: [CodingKey] = []

    func encodeNil() throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: Bool) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: String) throws {

    }
    func encode(_ value: Double) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: Float) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: Int) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: Int8) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: Int16) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: Int32) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: Int64) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: UInt) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: UInt8) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: UInt16) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: UInt32) throws { fatalError("Not implemented: \(#function)") }
    func encode(_ value: UInt64) throws { fatalError("Not implemented: \(#function)") }
    func encode<T: Encodable>(_ value: T) throws {
      print("encode: \(value)")
      if let identity = value as? AnyActorIdentity {
        self.parent.data =
            (identity.underlying as! ActorAddress).address
      }
    }
  }

  func encode<Act: DistributedActor>(_ actor: Act) throws -> String {
    try actor.encode(to: self)
    return self.data!
  }
}

@available(SwiftStdlib 5.5, *)
class TestDecoder: Decoder {
  let encoder: TestEncoder
  let data: String

  init(encoder: TestEncoder, transport: ActorTransport, data: String) {
    self.encoder = encoder
    self.userInfo = [.actorTransportKey: transport]
    self.data = data
  }

  var codingPath: [CodingKey] = []
  var userInfo: [CodingUserInfoKey : Any]

  func container<Key>(keyedBy type: Key.Type) throws -> KeyedDecodingContainer<Key> where Key : CodingKey {
    fatalError("Not implemented: \(#function)")
  }
  func unkeyedContainer() throws -> UnkeyedDecodingContainer {
    fatalError("Not implemented: \(#function)")
  }
  func singleValueContainer() throws -> SingleValueDecodingContainer {
    TestSingleValueDecodingContainer(parent: self)
  }

  class TestSingleValueDecodingContainer: SingleValueDecodingContainer {
    let parent: TestDecoder
    init(parent: TestDecoder) {
      self.parent = parent
    }

    var codingPath: [CodingKey] = []
    func decodeNil() -> Bool { fatalError("Not implemented: \(#function)") }
    func decode(_ type: Bool.Type) throws -> Bool { fatalError("Not implemented: \(#function)") }
    func decode(_ type: String.Type) throws -> String {
      print("decode String -> \(self.parent.data)")
      return self.parent.data
    }
    func decode(_ type: Double.Type) throws -> Double { fatalError("Not implemented: \(#function)") }
    func decode(_ type: Float.Type) throws -> Float { fatalError("Not implemented: \(#function)") }
    func decode(_ type: Int.Type) throws -> Int { fatalError("Not implemented: \(#function)") }
    func decode(_ type: Int8.Type) throws -> Int8 { fatalError("Not implemented: \(#function)") }
    func decode(_ type: Int16.Type) throws -> Int16 { fatalError("Not implemented: \(#function)") }
    func decode(_ type: Int32.Type) throws -> Int32 { fatalError("Not implemented: \(#function)") }
    func decode(_ type: Int64.Type) throws -> Int64 { fatalError("Not implemented: \(#function)") }
    func decode(_ type: UInt.Type) throws -> UInt { fatalError("Not implemented: \(#function)") }
    func decode(_ type: UInt8.Type) throws -> UInt8 { fatalError("Not implemented: \(#function)") }
    func decode(_ type: UInt16.Type) throws -> UInt16 { fatalError("Not implemented: \(#function)") }
    func decode(_ type: UInt32.Type) throws -> UInt32 { fatalError("Not implemented: \(#function)") }
    func decode(_ type: UInt64.Type) throws -> UInt64 { fatalError("Not implemented: \(#function)") }
    func decode<T>(_ type: T.Type) throws -> T where T : Decodable { fatalError("Not implemented: \(#function)") }
  }
}

// ==== Execute ----------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func test() {
  let transport = FakeTransport()

  // CHECK: assign type:DA, address:ActorAddress(address: "xxx")
  let da = DA(transport: transport)

  // CHECK: encode: AnyActorIdentity(ActorAddress(address: "xxx"))
  // CHECK: FakeTransport.decodeIdentity from:main.TestDecoder
  let encoder = TestEncoder(transport: transport)
  let data = try! encoder.encode(da)

  // CHECK: decode String -> xxx
  // CHECK: decode ActorAddress -> ActorAddress(address: "xxx")
  let da2 = try! DA(from: TestDecoder(encoder: encoder, transport: transport, data: data))

  // CHECK: decoded da2: DA(AnyActorIdentity(ActorAddress(address: "xxx")))
  print("decoded da2: \(da2)")
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    test()
  }
}
