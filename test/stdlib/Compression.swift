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

@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
func ofiltercompress_ifilterdecompress(_ contents: Data, using algo: Algorithm) throws -> Bool {

  var payload = Data()
  var decompressed = Data()

  // Output filter compress
  let f = DataSource(contents)
  let ofilter = try Compression.OutputFilter(.compress, using: algo) { (x:Data?) -> () in if x != nil { payload.append(x!) } }
  while (true) {
    let i = f.readData(ofLength: 900)
    try ofilter.write(i) // will finalize when i is empty
    if i == nil { break }
  }

  // Input filter decompress
  let g = DataSource(payload)
  let ifilter = try InputFilter(.decompress, using: algo, readingFrom: { (x:Int) -> Data? in return g.readData(ofLength:x) } )
  while (true) {
    let i = try ifilter.readData(ofLength: 400)
    if i == nil { break }
    decompressed.append(i!)
  }

  print("ofilter | ifilter \(algo): \(contents.count) -> \(payload.count) -> \(decompressed.count)")
  let pass = (contents == decompressed)
  if !pass { print("FAIL") }

  return pass
}

@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
func ifiltercompress_ofilterdecompress(_ contents: Data, using algo: Algorithm) throws -> Bool {

  var payload = Data()
  var decompressed = Data()

  // Input filter compress
  let f = DataSource(contents)
  let ifilter = try InputFilter(.compress, using: algo, readingFrom: { (x:Int) -> Data? in return f.readData(ofLength:x) } )
  while (true) {
    let i = try ifilter.readData(ofLength: 400)
    if i == nil { break }
    payload.append(i!)
  }

  // Output filter decompress
  let g = DataSource(payload)
  let ofilter = try OutputFilter(.decompress, using: algo) { (x:Data?) -> () in if x != nil { decompressed.append(x!) } }
  while (true) {
    let i = g.readData(ofLength: 900)
    try ofilter.write(i) // will finalize when i is empty
    if i == nil { break }
  }

  print("ifilter | ofilter \(algo): \(contents.count) -> \(payload.count) -> \(decompressed.count)")
  let pass = (contents == decompressed)
  if !pass { print("FAIL") }

  return pass
}

@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
func test_all() throws -> Int { // return number of failed cases

  var nfailed = 0
  let allAlgos: [Algorithm] = [ .lz4, .zlib, .lzfse, .lzma ]

  for len_blocks in [0,1,2,5,10,100] {

    var testString = "";
    for _ in 0 ..< len_blocks {
      var s = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
      // Change some random chars
      for _ in 1...10 {
        let pos = Int.random(in: 0 ..< s.count)
        s = s.prefix(pos) + "#" + s.dropFirst(pos + 1)
      }
      testString += s
    }
    let contents = testString.data(using: .utf8)!

    for algo in allAlgos {

      if !(try ofiltercompress_ifilterdecompress(contents, using: algo)) { nfailed += 1 }
      if !(try ifiltercompress_ofilterdecompress(contents, using: algo)) { nfailed += 1 }

    }

  } // len_blocks

  if nfailed > 0 { print("FAIL \(nfailed) tests") } else { print("PASS") }
  return nfailed
}

let tests = TestSuite("Compression")

tests.test("Compression filters") {
  var nfailed = 0
  if #available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
    do { nfailed = try test_all() } catch { print("exception caught"); nfailed = 1 }
  }
  expectEqual(nfailed,0)
}

runAllTests()
