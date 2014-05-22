// Test for the NSString API as exposed by String

// RUN: %target-run-simple-swift %s | FileCheck %s

import Foundation

func testFindFileAndURL(path: String) {
  var err: NSError?
  var content = String.stringWithContentsOfFile(path, error: &err)
  
  println("error: " + (err ? err.description : "<no error>"))
  println("content: " + (content ? content!._lines[0] : "<no content>"))

  var url = NSURL.URLWithString("file://" + path)

  err = nil
  content = String.stringWithContentsOfURL(url, error: &err)
  
  println("error: " + (err ? err.description : "<no error>"))
  println("content: " + (content ? content!._lines[0] : "<no content>"))
  
}

func testClassMethods() {
  var encodings: NSStringEncoding[] = String.availableStringEncodings()
  // CHECK: available encodings found
  // CHECK-NOT: 0 available encodings
  println("\(encodings.count) available encodings found")

  // CHECK-NEXT: defaultCStringEncoding is available
  var defaultCStringEncoding = String.defaultCStringEncoding()
  for e in encodings {
    if e == defaultCStringEncoding {
      println("defaultCStringEncoding is available")
    }
  }

  // CHECK-NEXT: It is called
  println(
    "It is called \"" +
    String.localizedNameOfStringEncoding(defaultCStringEncoding) + "\"")

  var path = String.pathWithComponents(["flugelhorn", "baritone", "bass"])
  // CHECK-NEXT: <flugelhorn/baritone/bass>
  println("<\(path)>")

  // CHECK-NEXT: <sox>
  var chars: unichar[] = [
    unichar("s".value), unichar("o".value), unichar("x".value) ]

  var sox: String = String(utf16CodeUnits: chars, count: chars.count)
  println("<\(sox)>")

  var pathToThisSource = Process.arguments[1]
  var nonExistentPath = pathToThisSource + "-NoNeXiStEnT"
  
  // CHECK-NEXT: error: Error Domain=NSCocoaErrorDomain
  // CHECK-NEXT: content: <no content>
  // CHECK-NEXT: error: Error Domain=NSCocoaErrorDomain
  // CHECK-NEXT: content: <no content>
  testFindFileAndURL(nonExistentPath)

  // CHECK-NEXT: error: <no error>
  // CHECK-NEXT: content: // Test for the NSString API as exposed by String
  // CHECK-NEXT: error: <no error>
  // CHECK-NEXT: content: // Test for the NSString API as exposed by String
  testFindFileAndURL(pathToThisSource)

  // CHECK-NEXT: foo, a basmati bar!
  println(
    String.stringWithCString(
      "foo, a basmati bar!", encoding: String.defaultCStringEncoding()))

  var emptyString = ""
  
  // CHECK-NEXT: {{.*}} has 0 completions and the longest is <None Found>
  var outputName = "None Found"
  var count = nonExistentPath.completePathIntoString(
    &outputName, caseSensitive: false)
  
  println(
    "<\(nonExistentPath)> has \(count) "
    + "completions and the longest is <\(outputName)>")

  // CHECK-NEXT: <[[THISPATH:.*]]> has 1 completions and the longest is <[[THISPATH]]>
  count = pathToThisSource.completePathIntoString(
    &outputName, caseSensitive: false)
  
  println(
    "<\(pathToThisSource)> has \(count) "
    + "completions and the longest is <\(outputName)>")

  var world: NSString = "world"
  // CHECK-NEXT: Hello, world!%42
  println(String(format: "Hello, %@!%%%ld", world, 42))
}


testClassMethods()

// Make sure NSStringEncoding and its values are type-compatible.
var enc : NSStringEncoding
enc = NSWindowsCP1250StringEncoding
enc = NSUTF32LittleEndianStringEncoding
enc = NSUTF32BigEndianStringEncoding
enc = NSASCIIStringEncoding
enc = NSUTF8StringEncoding

// CHECK: done!
println("done!")
