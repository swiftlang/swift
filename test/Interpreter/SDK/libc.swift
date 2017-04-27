/* magic */
// Do not edit the line above.

// RUN: rm -rf %t  &&  mkdir -p %t
// RUN: %target-run-simple-swift %s %t | %FileCheck %s

// REQUIRES: executable_test

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
  import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android)
  import Glibc
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
  print("O_CREAT|O_EXCL returned errno *\(errno)*") 
}

// CHECK-NOT: error
// CHECK: created mode *33216* *33216*
let tempFile = 
  open(tempPath, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR | S_IXUSR)
let written = write(tempFile, bytes, 11)
assert(written == 11)

var err: Int32
var statbuf1 = stat()
err = fstat(tempFile, &statbuf1)
if err != 0 { 
  print("error: fstat returned \(err), errno \(errno)")
  abort()
}

close(tempFile)

var statbuf2 = stat()
err = stat(tempPath, &statbuf2)
if err != 0 { 
  print("error: stat returned \(err), errno \(errno)")
  abort()
}

print("created mode *\(statbuf1.st_mode)* *\(statbuf2.st_mode)*")

assert(statbuf1.st_mode == S_IFREG | S_IRUSR | S_IWUSR | S_IXUSR)
assert(statbuf1.st_mode == statbuf2.st_mode)

