// A simple program that takes the JSON output of libswiftGenericMetadataBuilder
// and converts it into C code that can be bulit as a library.
import Foundation

enum ParseError: Error {
  case oddLengthHexString
  case invalidHex(Substring)
}

struct Ptrauth: Decodable {
  var key: Int8
  var addr: Bool
  var diversity: UInt
}

struct Fixup: Decodable {
  var target: String
  var addend: Int64
  var authPtr: Ptrauth?
}

enum AtomContent: Decodable {
  case bytes([UInt8])
  case pointer(Fixup)

  static func getString(decoder: Decoder) -> String? {
    do {
      return try decoder.singleValueContainer().decode(String.self)
    } catch {
      return nil
    }
  }

  init(from decoder: Decoder) throws {
    if let string = Self.getString(decoder: decoder) {
      var bytes: [UInt8] = []
      var toParse = Substring(string)
      while !toParse.isEmpty {
        let prefix = toParse.prefix(2)
        if prefix.count == 1 {
          throw ParseError.oddLengthHexString
        }
        guard let byte = UInt8(prefix, radix: 16) else {
          throw ParseError.invalidHex(prefix)
        }
        bytes.append(byte)
        toParse = toParse.dropFirst(2)
      }
      self = .bytes(bytes)
    } else {
      let fixup = try Fixup(from: decoder)
      self = .pointer(fixup)
    }
  }
}

struct Atom: Decodable {
  var name: String
  var contents: [AtomContent]
}

struct Document: Decodable {
  var atoms: [Atom]
}

func deUnderscore(_ str: String) -> Substring {
  guard str.hasPrefix("_") else {
    fatalError("symbol/atom name '\(str)' does not have an underscore prefix")
  }
  return str.dropFirst()
}

func printHeader() {
  print("#include <stdint.h>")
}

func printExternDeclarations(_ document: Document) {
  var atomNames: Set<String> = []
  var externNames: Set<String> = []
  for atom in document.atoms {
    atomNames.insert(atom.name)
  }
  for atom in document.atoms {
    for content in atom.contents {
      switch content {
        case .bytes(_):
          break
        case .pointer(let fixup):
          if !atomNames.contains(fixup.target) {
            externNames.insert(fixup.target)
          }
      }
    }
  }
  for name in externNames.sorted() {
    print("extern char \(deUnderscore(name));")
  }
}

func printDeclarations(_ document: Document) {
  printExternDeclarations(document)
  for atom in document.atoms {
    print("struct __attribute__((packed)) __attribute__((aligned(sizeof(void *)))) \(deUnderscore(atom.name))_t {")
    for (i, content) in atom.contents.enumerated() {
      let name = "content\(i)"
      switch content {
        case .bytes(let bytes):
          print("  uint8_t \(name)[\(bytes.count)];")
        case .pointer(let fixup):
          let ptrauthQualifier: String
          if let ptrauth = fixup.authPtr {
            ptrauthQualifier = """
              __ptrauth(\(ptrauth.key),
                        \(ptrauth.addr ? 1 : 0),
                        \(ptrauth.diversity))
            """
          } else {
            ptrauthQualifier = ""
          }
          print("  const char *\(ptrauthQualifier) \(name);")
      }
    }
    print("};")
    print("struct \(deUnderscore(atom.name))_t \(deUnderscore(atom.name));")
    print("")
  }
}

func printDefinitions(_ document: Document) {
  for atom in document.atoms {
    print("struct \(deUnderscore(atom.name))_t \(deUnderscore(atom.name)) = {")
    for content in atom.contents {
      switch content {
        case .bytes(let bytes):
          print("  {")
          for byte in bytes {
            print("    0x\(String(byte, radix: 16)),")
          }
          print("  },")
        case .pointer(let fixup):
          print("  (char *)&\(deUnderscore(fixup.target)) + \(fixup.addend),")
      }
    }
    print("};")
    print("")
  }
}

func printC(_ document: Document) {
  printHeader()
  printDeclarations(document)
  printDefinitions(document)
}

for arg in CommandLine.arguments.dropFirst() {
  let data = try! Data(contentsOf: URL(fileURLWithPath: arg))
  let decoder = JSONDecoder()
  let decoded = try! decoder.decode(Document.self, from: data)
  printC(decoded)
}
