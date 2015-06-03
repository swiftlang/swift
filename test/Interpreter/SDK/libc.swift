/* magic */
// Do not edit the line above.

// RUN: rm -rf %t  &&  mkdir -p %t
// RUN: %target-run-simple-swift %s %t | FileCheck %s
// REQUIRES: executable_test

// XFAIL: linux

import Darwin

let sourcePath = Process.arguments[1]
let tempPath = Process.arguments[2] + "/libc.txt"

// CHECK: Hello world
fputs("Hello world", stdout)

// CHECK: 4294967295
print("\(UINT32_MAX)")

// CHECK: the magic word is ///* magic *///
let sourceFile = open(sourcePath, O_RDONLY)
assert(sourceFile >= 0)
var bytes = UnsafeMutablePointer<CChar>.alloc(12)
var readed = read(sourceFile, bytes, 11)
close(sourceFile)
assert(readed == 11)
bytes[11] = CChar(0)
print("the magic word is //\(String.fromCString(bytes)!)//")

// CHECK: O_CREAT|O_EXCL returned errno *17*
let errFile = 
  open(sourcePath, O_RDONLY | O_CREAT | O_EXCL)
if errFile != -1 { 
  print("O_CREAT|O_EXCL failed to return an error") 
} else { 
  print("O_CREAT|O_EXCL returned errno *\(errno)*") 
}

// CHECK: created mode *33216*
let tempFile = 
  open(tempPath, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR | S_IXUSR)
let written = write(tempFile, bytes, 11)
assert(written == 11)
close(tempFile)
var statbuf = stat()
let err = stat(tempPath, &statbuf)
if err != 0 { 
  print("stat returned \(err), errno \(errno)") 
} else { 
  print("created mode *\(statbuf.st_mode)*") 
  assert(statbuf.st_mode == S_IFREG | S_IRUSR | S_IWUSR | S_IXUSR)
}

