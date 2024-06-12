import Foundation

@main struct Plugin {
  static func main() {
    var spec = Spec(raw: mockPlugin)
    readabilityHandler = { spec.handleNext() }
  }
}

struct Spec {
  struct Item: Codable {
    let expect: JSONValue
    let response: JSONValue
  }

  private let items: [Item]
  private var handled: Set<Int> = []

  init(raw: String) {
    items = try! decoder.decode([Item].self, from: Data(raw.utf8))
  }

  mutating func handleNext() {
    let actual: JSONValue = try! waitForNextMessage()
    guard let index = items.firstIndex(where: { match(expect: $0.expect, val: actual) }) else {
      fatalError("couldn't find matching item for request: \(actual)")
    }
    guard handled.insert(index).inserted else {
      fatalError("request is already handled")
    }
    let response = substitute(value: items[index].response, req: actual)
    try! outputMessage(response)
  }

  private func match(expect: JSONValue, val: JSONValue) -> Bool {
    switch (expect, val) {
    case (.object(let lhs), .object(let rhs)):
      return lhs.allSatisfy { key, value in
        rhs[key].map { match(expect: value, val: $0) } ?? false
      }
    case (.array(let lhs), .array(let rhs)):
      return zip(lhs, rhs).allSatisfy { match(expect: $0, val: $1) }
    default:
      return expect == val
    }
  }

  private func substitute(value: JSONValue, req: JSONValue) -> JSONValue {
    var path: Substring
    switch value {
    case .object(let obj):
      return .object(obj.mapValues { substitute(value: $0, req: req) })
    case .array(let arr):
      return .array(arr.map { substitute(value: $0, req: req) })
    case .string(let str) where str.starts(with: "=req"):
      path = str.dropFirst(4)
      break
    default:
      return value
    }
    var value = req
    while !path.isEmpty {
      // '.' <alphanum> -> object key.
      if path.starts(with: ".") {
        guard case .object(let obj) = value else {
          return .string("<substition error: not an object>")
        }
        path = path.dropFirst()
        let key = path.prefix(while: { $0.isNumber || $0.isLetter || $0 == "_" })
        path = path.dropFirst(key.count)
        guard let next = obj[String(key)] else {
          return .string("<substition error: key not found>")
        }
        value = next
        continue
      }
      // '[' <digit>+ ']' -> array index.
      if path.starts(with: "[") {
        guard case .array(let arr) = value else {
          return .string("<substition error: not an array>")
        }
        path = path.dropFirst()
        let index = path.prefix(while: { $0.isNumber })
        path = path.dropFirst(index.count)
        guard path.starts(with: "]") else {
          return .string("<substition error: missing ']' after digits")
        }
        path = path.dropFirst()
        guard let idx = Int(index), idx < arr.count else {
          return .string("<substition error: index out of bounds>")
        }
        value = arr[idx]
        continue
      }
      return .string("<substition error: malformed path>")
    }
    return value
  }
}

var readabilityHandler: (() -> Void)?

@_expose(wasm, "swift_wasm_macro_v1_pump")
@_cdecl("swift_wasm_macro_v1_pump")
func wasmPump() {
  guard let readabilityHandler else { fatalError("main not called") }
  readabilityHandler()
}

let decoder = JSONDecoder()
let encoder = JSONEncoder()

private func outputMessage<TX: Encodable>(_ message: TX) throws {
  let encoded = try encoder.encode(message)
  let count = encoded.count
  var header = UInt64(count).littleEndian
  withUnsafeBytes(of: &header) { _write(contentsOf: $0) }

  // Write the JSON payload.
  encoded.withUnsafeBytes { _write(contentsOf: $0) }
}

private func waitForNextMessage<RX: Decodable>() throws -> RX {
  // Read the header (a 64-bit length field in little endian byte order).
  var header: UInt64 = 0
  withUnsafeMutableBytes(of: &header) { _read(into: $0) }

  // Read the JSON payload.
  let count = Int(UInt64(littleEndian: header))
  let data = UnsafeMutableRawBufferPointer.allocate(byteCount: count, alignment: 1)
  defer { data.deallocate() }
  _read(into: data)

  // Decode and return the message.
  return try decoder.decode(RX.self, from: Data(data))
}

private func _read(into buffer: UnsafeMutableRawBufferPointer) {
  guard var ptr = buffer.baseAddress else { return }
  let endPtr = ptr.advanced(by: buffer.count)
  while ptr != endPtr {
    switch read(fileno(stdin), ptr, numericCast(endPtr - ptr)) {
    case -1: fatalError("Error: \(errno)")
    case 0: fatalError("End of input")
    case let n: ptr += Int(n)
    }
  }
}

private func _write(contentsOf buffer: UnsafeRawBufferPointer) {
  guard var ptr = buffer.baseAddress else { return }
  let endPtr = ptr.advanced(by: buffer.count)
  while ptr != endPtr {
    switch write(fileno(stdout), ptr, numericCast(endPtr - ptr)) {
    case -1: fatalError("Error: \(errno)")
    case 0: fatalError("write returned 0")
    case let n: ptr += Int(n)
    }
  }
}

enum JSONValue: Hashable {
  case object([String: JSONValue])
  case array([JSONValue])
  case string(String)
  case number(Double)
  case bool(Bool)
  case null
}

extension JSONValue: Codable {
  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()

    if let decoded = try? container.decode(String.self) {
      self = .string(decoded)
    } else if let decoded = try? container.decode(Double.self) {
      self = .number(decoded)
    } else if let decoded = try? container.decode(Bool.self) {
      self = .bool(decoded)
    } else if let decoded = try? container.decode([JSONValue].self) {
      self = .array(decoded)
    } else if let decoded = try? container.decode([String: JSONValue].self) {
      self = .object(decoded)
    } else if container.decodeNil() {
      self = .null
    } else {
      throw DecodingError.dataCorruptedError(in: container, debugDescription: "Could not decode JSONValue")
    }
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()

    switch self {
    case .string(let string):
      try container.encode(string)
    case .bool(let bool):
      try container.encode(bool)
    case .number(let double):
      try container.encode(double)
    case .array(let array):
      try container.encode(array)
    case .object(let dictionary):
      try container.encode(dictionary)
    case .null:
      try container.encodeNil()
    }
  }
}
