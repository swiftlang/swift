// RUN: %target-swift-emit-silgen -import-objc-header %S/Inputs/readbytes.h %s -o /dev/null -verify
//
// REQUIRES: objc_interop
//
// Diagnose invalid conversion from an inout Data argument to a raw pointer.

import Foundation

func readBytes(_ pointer: UnsafeRawPointer) {}
func writeBytes(_ pointer: UnsafeMutableRawPointer) {}

// SILGen diagnostics prohibits these implicit casts:
func test_errors(darg: Data) {
    var data: Data = darg
    readBytes(&data)   // expected-warning {{forming 'UnsafeRawPointer' to a variable of type 'Data'; this is likely incorrect because 'Data' may contain an object reference.}}
    writeBytes(&data)  // expected-warning {{forming 'UnsafeMutableRawPointer' to a variable of type 'Data'; this is likely incorrect because 'Data' may contain an object reference.}}
    read_char(&data)   // expected-warning {{forming 'UnsafePointer<Int8>' to a variable of type 'Data'; this is likely incorrect because 'Data' may contain an object reference.}}
    write_char(&data)  // expected-warning {{forming 'UnsafeMutablePointer<Int8>' to a variable of type 'Data'; this is likely incorrect because 'Data' may contain an object reference.}}
    read_uchar(&data)  // expected-warning {{forming 'UnsafePointer<UInt8>' to a variable of type 'Data'; this is likely incorrect because 'Data' may contain an object reference.}}
    write_uchar(&data) // expected-warning {{forming 'UnsafeMutablePointer<UInt8>' to a variable of type 'Data'; this is likely incorrect because 'Data' may contain an object reference.}}
}

// Test the explicit cast workaround for all of the above errors
func test_explicit(darg: Data) {
    var data: Data = darg
    data.withUnsafeBytes() {
        readBytes($0.baseAddress!)
        read_char($0.baseAddress!)
        read_uchar($0.baseAddress!)
    }
    data.withUnsafeMutableBytes() {
        writeBytes($0.baseAddress!)
        write_char($0.baseAddress!)
        write_uchar($0.baseAddress!)
    }
}
