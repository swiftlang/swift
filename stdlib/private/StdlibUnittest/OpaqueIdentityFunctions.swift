//===--- OpaqueIdentityFunctions.swift ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@asmname("swift_stdlib_getPointer")
func _stdlib_getPointer(x: COpaquePointer) -> COpaquePointer

public func _opaqueIdentity<T>(x: T) -> T {
  var ptr = UnsafeMutablePointer<T>.alloc(1)
  ptr.initialize(x)
  let result =
    UnsafeMutablePointer<T>(_stdlib_getPointer(COpaquePointer(ptr))).memory
  ptr.destroy()
  ptr.dealloc(1)
  return result
}

func _blackHolePtr<T>(x: UnsafePointer<T>) {
  _stdlib_getPointer(COpaquePointer(x))
}

public func _blackHole<T>(var x: T) {
  _blackHolePtr(&x)
}

@inline(never)
public func getBool(x: Bool) -> Bool { return _opaqueIdentity(x) }

@inline(never)
public func getInt8(x: Int8) -> Int8 { return _opaqueIdentity(x) }

@inline(never)
public func getInt16(x: Int16) -> Int16 { return _opaqueIdentity(x) }

@inline(never)
public func getInt32(x: Int32) -> Int32 { return _opaqueIdentity(x) }

@inline(never)
public func getInt64(x: Int64) -> Int64 { return _opaqueIdentity(x) }

@inline(never)
public func getInt(x: Int) -> Int { return _opaqueIdentity(x) }

@inline(never)
public func getWord(x: Word) -> Word { return _opaqueIdentity(x) }

@inline(never)
public func getUInt8(x: UInt8) -> UInt8 { return _opaqueIdentity(x) }

@inline(never)
public func getUInt16(x: UInt16) -> UInt16 { return _opaqueIdentity(x) }

@inline(never)
public func getUInt32(x: UInt32) -> UInt32 { return _opaqueIdentity(x) }

@inline(never)
public func getUInt64(x: UInt64) -> UInt64 { return _opaqueIdentity(x) }

@inline(never)
public func getUInt(x: UInt) -> UInt { return _opaqueIdentity(x) }

@inline(never)
public func getUWord(x: UWord) -> UWord { return _opaqueIdentity(x) }

@inline(never)
public func getFloat32(x: Float32) -> Float32 { return _opaqueIdentity(x) }

@inline(never)
public func getFloat64(x: Float64) -> Float64 { return _opaqueIdentity(x) }

#if arch(i386) || arch(x86_64)
@inline(never)
public func getFloat80(x: Float80) -> Float80 { return _opaqueIdentity(x) }
#endif

public func getPointer(x: COpaquePointer) -> COpaquePointer {
  return _opaqueIdentity(x)
}

