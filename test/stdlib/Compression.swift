// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Compression
import Foundation

// Read from Data
class DataSource {

  private var _buf : Data
  private var _bufSize : Int // total byte count
  private var _pos : Int  // next byte to read

  public init(_ d: Data) {
    _buf = d
    _bufSize = d.count
    _pos = 0
  }

  public func readData(ofLength len: Int) -> Data? {
    let n = min(len,_bufSize - _pos)
    if (n == 0) { return nil } // EOF
    let d = _buf.subdata(in: _pos ..< _pos + n)
    _pos += n
    return d
  }

}

@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
func ofiltercompress_ifilterdecompress(
  _ contents: Data, using algo: Algorithm
) throws -> Bool {

  var payload = Data()
  var decompressed = Data()

  // Output filter compress
  let f = DataSource(contents)
  let ofilter = try OutputFilter(.compress, using: algo) { (x: Data?) -> () in
      if let x = x { payload.append(x) }
  }
  while (true) {
    let i = f.readData(ofLength: 900)
    try ofilter.write(i) // will finalize when i is empty
    if i == nil { break }
  }

  // Input filter decompress
  let g = DataSource(payload)
  let ifilter = try InputFilter(.decompress, using: algo) { (x: Int) -> Data? in
    return g.readData(ofLength: x)
  }
  while let i = try ifilter.readData(ofLength: 400) {
    decompressed.append(i)
  }

  print("ofilter | ifilter \(algo): \(contents.count) -> \(payload.count) -> \(decompressed.count)")
  return contents == decompressed
}

@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
func ifiltercompress_ofilterdecompress(
  _ contents: Data, using algo: Algorithm
) throws -> Bool {

  var payload = Data()
  var decompressed = Data()

  // Input filter compress
  let f = DataSource(contents)
  let ifilter = try InputFilter(.compress, using: algo) {(x: Int) -> Data? in
    return f.readData(ofLength:x)
  }
  while let i = try ifilter.readData(ofLength: 400) {
    payload.append(i)
  }

  // Output filter decompress
  let g = DataSource(payload)
  let ofilter = try OutputFilter(.decompress, using: algo) {(x: Data?) -> () in
    if let x = x {
      decompressed.append(x)
    }
  }
  while (true) {
    let i = g.readData(ofLength: 900)
    try ofilter.write(i) // will finalize when i is empty
    if i == nil { break }
  }

  print("ifilter | ofilter \(algo): \(contents.count) -> \(payload.count) -> \(decompressed.count)")

  return contents == decompressed
}

func randomString(withBlockLength n: Int) -> String {
  var strings = [String]()
  for _ in 0 ..< n {
    var s = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
      // Change some random chars
      for _ in 1...10 {
        let pos = Int.random(in: 0 ..< s.count)
        let idx = s.index(s.startIndex, offsetBy: pos)
        s = String(s[..<idx] + "#" + s[idx...])
      }
    strings.append(s)
  }
  return strings.joined(separator: "")
}

let tests = TestSuite("Compression")

if #available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *) {

  do {
    for blockLength in [0, 1, 2, 5, 10, 100] {
      let testString = randomString(withBlockLength: blockLength)
      let contents = testString.data(using: .utf8)!

      for algo in Algorithm.allCases {
        tests.test("OutputFilter/Compress/InputFilter/Decompress/\(algo)/\(blockLength)") {
          expectDoesNotThrow({
            expectTrue(
              try ofiltercompress_ifilterdecompress(contents, using: algo),
              "Failing input: \(testString)"
            )
          })
        }

        tests.test("InputFilter/Compress/OutputFilter/Decompress/\(algo)/\(blockLength)") {
          expectDoesNotThrow({
            expectTrue(
              try ifiltercompress_ofilterdecompress(contents, using: algo),
              "Failing input: \(testString)"
            )
          })
        }
      }

    } // blockLength
  }

}

runAllTests()
