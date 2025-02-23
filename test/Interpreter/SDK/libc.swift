/* magic */
// Do not edit the line above.

// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift %s %t | %FileCheck %s

// REQUIRES: executable_test

// TODO: rdar://problem/33388782
// REQUIRES: CPU=x86_64

#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#elseif os(Windows)
  import CRT

  let S_IRUSR: Int32 = ucrt._S_IREAD
  let S_IWUSR: Int32 = 0
  let S_IXUSR: Int32 = 0

  let S_IRGRP: Int32 = 0o0040
  let S_IROTH: Int32 = 0o0004
#else
#error("Unsupported platform")
#endif

let sourcePath = CommandLine.arguments[1]
let tempPath = CommandLine.arguments[2] + "/libc.txt"

// CHECK: Hello world
fputs("Hello world", stdout)

// CHECK: 4294967295
print("\(UINT32_MAX)")

// CHECK: the magic word is ///* magic *///
let sourceFile = open(sourcePath, O_RDONLY)
assert(sourceFile >= 0)
var bytes = UnsafeMutablePointer<CChar>.allocate(capacity: 12)
var readed = read(sourceFile, bytes, 11)
close(sourceFile)
assert(readed == 11)
bytes[11] = CChar(0)
print("the magic word is //\(String(cString: bytes))//")

// CHECK: O_CREAT|O_EXCL returned errno *17*
let errFile =
  open(sourcePath, O_RDONLY | O_CREAT | O_EXCL)
if errFile != -1 {
  print("O_CREAT|O_EXCL failed to return an error")
} else {
  let e = errno
  print("O_CREAT|O_EXCL returned errno *\(e)*")
}

// CHECK-NOT: error
// CHECK: created mode *{{33216|33060}}* *{{33216|33060}}*
let tempFile =
  open(tempPath, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR | S_IXUSR)
if tempFile == -1 {
  let e = errno
  print("error: open(tempPath \(tempPath)) returned -1, errno \(e)")
  abort()
}
let written = write(tempFile, bytes, 11)
if (written != 11) {
  print("error: write(tempFile) returned \(written), errno \(errno)")
  abort()
}

var err: Int32
var statbuf1 = stat()
err = fstat(tempFile, &statbuf1)
if err != 0 {
  let e = errno
  print("error: fstat returned \(err), errno \(e)")
  abort()
}

close(tempFile)

var statbuf2 = stat()
err = stat(tempPath, &statbuf2)
if err != 0 {
  let e = errno
  print("error: stat returned \(err), errno \(e)")
  abort()
}

print("created mode *\(statbuf1.st_mode)* *\(statbuf2.st_mode)*")

#if os(Windows)
assert(statbuf1.st_mode == S_IFREG | S_IRUSR | S_IRGRP | S_IROTH)
#else
assert(statbuf1.st_mode == S_IFREG | S_IRUSR | S_IWUSR | S_IXUSR)
#endif
assert(statbuf1.st_mode == statbuf2.st_mode)

