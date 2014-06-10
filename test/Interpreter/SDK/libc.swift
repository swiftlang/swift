/* magic */
// Do not edit the line above.

// RUN: rm -rf %t  &&  mkdir -p %t
// RUN: %target-run-simple-swift %s %t | FileCheck %s

import Darwin

let sourcePath = Process.arguments[1]
let tempPath = Process.arguments[2] + "/libc.txt"

// CHECK: Hello world
fputs("Hello world", stdout)

// CHECK: 4294967295
println("\(UINT32_MAX)")

// CHECK: the magic word is /* magic */
let sourceFile = 
  sourcePath.withCString { (cstr: CString) -> CInt in
    return open(cstr, O_RDONLY)
  }
assert(sourceFile >= 0)
var bytes = UnsafePointer<CChar>.alloc(11)
var readed = read(sourceFile, bytes, 11)
close(sourceFile)
assert(readed == 11)
var magic = String.fromCString(bytes)
println("the magic word is \(magic) //")

// CHECK: O_CREAT|O_EXCL returned errno *17*
let errFile = 
  sourcePath.withCString { (cstr: CString) -> CInt in
    return open(cstr, O_RDONLY | O_CREAT | O_EXCL)
  }
if errFile != -1 { 
  println("O_CREAT|O_EXCL failed to return an error") 
} else { 
  println("O_CREAT|O_EXCL returned errno *\(errno)*") 
}

// CHECK: created mode *33216*
let tempFile = 
  tempPath.withCString { (cstr: CString) -> CInt in
    return open(cstr, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR | S_IXUSR)
  }
let written = write(tempFile, bytes, 11)
assert(written == 11)
close(tempFile)
var statbuf = stat()
let err = tempPath.withCString { (cstr: CString) in
  stat(cstr, &statbuf)
}
if err != 0 { 
  println("stat returned \(err), errno \(errno)") 
} else { 
  println("created mode *\(statbuf.st_mode)*") 
  assert(statbuf.st_mode == S_IFREG | S_IRUSR | S_IWUSR | S_IXUSR)
}

