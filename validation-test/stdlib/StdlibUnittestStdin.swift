// RUN: %target-run-simple-swift

// XFAIL: linux

import StdlibUnittest
import Darwin

func simple_getline() -> [UInt8]? {
  var result = [UInt8]()
  while true {
    let c = getchar()
    result.append(UInt8(c))
    if c == EOF {
      if result.count == 0 {
        return nil
      }
      return result
    }
    if c == CInt(UnicodeScalar("\n").value) {
      return result
    }
  }
}

var StdinTestSuite = TestSuite("Stdin")

StdinTestSuite.test("Empty")
  .stdin("")
  .code {
}

StdinTestSuite.test("EmptyLine")
  .stdin("\n")
  .code {
  expectOptionalEqual([ 0x0a ], simple_getline())
}

StdinTestSuite.test("Whitespace")
  .stdin(" \n")
  .code {
  expectOptionalEqual([ 0x20, 0x0a ], simple_getline())
}

StdinTestSuite.test("NonEmptyLine")
  .stdin("abc\n")
  .code {
  expectOptionalEqual([ 0x61, 0x62, 0x63, 0x0a ], simple_getline())
}

StdinTestSuite.test("TwoLines")
  .stdin("abc\ndefghi\n")
  .code {
  expectOptionalEqual([ 0x61, 0x62, 0x63, 0x0a ], simple_getline())
  expectOptionalEqual(
    [ 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x0a ], simple_getline())
}

StdinTestSuite.test("EOF/1")
  .stdin("abc\n", eof: true)
  .code {
  expectOptionalEqual([ 0x61, 0x62, 0x63, 0x0a ], simple_getline())
}

StdinTestSuite.test("EOF/2")
  .stdin("abc\n", eof: true)
  .code {
  expectOptionalEqual([ 0x61, 0x62, 0x63, 0x0a ], simple_getline())
}

runAllTests()

