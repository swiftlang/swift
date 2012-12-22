// RUN: %swift %s -verify -parse-as-library

//===----------------------------------------------------------------------===//
// Swift Standard Prolog Library.
//===----------------------------------------------------------------------===//

import Builtin

//===----------------------------------------------------------------------===//
// Standardized aliases
//===----------------------------------------------------------------------===//
typealias Void = ()
typealias Int  = Int64
typealias UInt = UInt64

// FIXME/TBD: Consider adding "int", "double", etc as aliases for Int/Double.
// They violate the naming convention but lower the barrier to entry.

//===----------------------------------------------------------------------===//
// Default types for unconstrained number literals
//===----------------------------------------------------------------------===//
typealias IntegerLiteralType = Int
typealias FloatLiteralType = Double
typealias CharacterLiteralType = Char

//===----------------------------------------------------------------------===//
// Objective-C interactions
//===----------------------------------------------------------------------===//

// This violates the naming convention but looks really wrong as Id.
struct id {
  var value : Builtin.ObjCPointer
}

//===----------------------------------------------------------------------===//
// Extern C functions
//===----------------------------------------------------------------------===//

// FIXME: Once we have an FFI interface, make these have proper function bodies

// The C "abort" function
func [asmname="abort"] abort()

func [asmname="putchar"]
c_putchar(val : Int32)
func [asmname="print_int"]
c_print_int(p : Builtin.RawPointer, buf_len : Int, x : Int128, Radix : Int,
            uppercase : Bool) -> UInt64
func [asmname="print_uint"]
c_print_uint(p : Builtin.RawPointer, buf_len : Int, x : UInt128, Radix : Int,
             uppercase : Bool) -> UInt64
func [asmname="print_double"]
c_print_double(p : Builtin.RawPointer, x : Double) -> UInt64

// Some math stuff.
func [asmname="sqrtf"] sqrt(a : Float) -> Float
func [asmname="sqrt"] sqrt(a : Double) -> Double
func [asmname="sinf"] sin(a : Float) -> Float
func [asmname="sin"] sin(a : Double) -> Double
func [asmname="cosf"] cos(a : Float) -> Float
func [asmname="cos"] cos(a : Double) -> Double
func [asmname="atan2f"] atan2(y : Float, x : Float) -> Float
func [asmname="atan2"] atan2(y : Double, x : Double) -> Double


func [asmname="mach_absolute_time"] mach_absolute_time() -> UInt64

func [asmname="swift_replOutputIsUTF8"] _isUTF8() -> Bool

// Some file stuff

func [asmname="swift_file_open"]
c_file_open(filename : Builtin.RawPointer) -> Int32

func [asmname="swift_file_close"]
c_file_close(fd : Int32) -> Int32

func [asmname="swift_file_read"]
c_file_read(fd : Int32, buf : Builtin.RawPointer, sz : Int) -> Int

func [asmname="swift_file_size"]
c_file_size(filename : Builtin.RawPointer) -> Int

func [asmname="swift_fd_size"]
c_fd_size(fd : Int32) -> Int

func [asmname="opendir"]
posix_opendir_hack(dir : Builtin.RawPointer) -> Builtin.RawPointer

func [asmname="posix_readdir_hack"]
posix_readdir_hack(handle : Builtin.RawPointer) -> (Builtin.RawPointer, Int)

func [asmname="closedir"]
posix_closedir_hack(handle : Builtin.RawPointer)

func [asmname="posix_isDirectory_hack"]
posix_isDirectory_hack(handle : Builtin.RawPointer) -> Int

func [asmname="getchar"]
getchar() -> Int32

func [asmname="open"]
posix_open(filename : Builtin.RawPointer, mode : Int32, perm : Int) -> Int32

func [asmname="close"]
posix_close(fd : Int32) -> Int32

func [asmname="lseek"]
posix_seek(fd : Int32, offset : Int, whence : Int32) -> Int

func [asmname="write"]
posix_write(fd : Int32, buf : Builtin.RawPointer, sz : Int) -> Int

func [asmname="read"]
posix_read(fd : Int32, buf : Builtin.RawPointer, sz : Int) -> Int

func [asmname="dup"]
posix_dup(fd : Int32) -> Int32
