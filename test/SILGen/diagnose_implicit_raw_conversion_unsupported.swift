// RUN: %target-swift-emit-silgen -import-objc-header %S/Inputs/readbytes.h %s -o /dev/null -verify
//
// Diagnose invalid conversion from an inout argument to a raw pointer.
//
// These cases are caught early during Sema, so they are never seen by SILGen's implicit conversion diagnostics.

func readBytes(_ pointer: UnsafeRawPointer) {}
func writeBytes(_ pointer: UnsafeMutableRawPointer) {}
func readInt8(_ pointer: UnsafePointer<Int8>) {}
func writeInt8(_ pointer: UnsafeMutablePointer<Int8>) {}
func readUInt8(_ pointer: UnsafePointer<UInt8>) {}
func writeUInt8(_ pointer: UnsafeMutablePointer<UInt8>) {}

// These implicit casts never worked and will continue to be unsupported.
func test_unsupported<T>(arg: T) {
    var t: T = arg 
    readInt8(&t) // expected-error {{cannot convert value of type 'UnsafePointer<T>' to expected argument type 'UnsafePointer<Int8>'}}
                 // expected-note@-1 {{arguments to generic parameter 'Pointee' ('T' and 'Int8') are expected to be equal}}
    writeInt8(&t) // expected-error {{cannot convert value of type 'UnsafeMutablePointer<T>' to expected argument type 'UnsafeMutablePointer<Int8>'}}
                  // expected-note@-1 {{arguments to generic parameter 'Pointee' ('T' and 'Int8') are expected to be equal}}
    readUInt8(&t) // expected-error {{cannot convert value of type 'UnsafePointer<T>' to expected argument type 'UnsafePointer<UInt8>'}}
                  // expected-note@-1 {{arguments to generic parameter 'Pointee' ('T' and 'UInt8') are expected to be equal}}
    writeUInt8(&t) // expected-error {{cannot convert value of type 'UnsafeMutablePointer<T>' to expected argument type 'UnsafeMutablePointer<UInt8>'}} 
                   // expected-note@-1 {{arguments to generic parameter 'Pointee' ('T' and 'UInt8') are expected to be equal}}

    let constArray: [UInt8] = [0]
    readInt8(constArray) // expected-error {{cannot convert value of type 'UnsafePointer<UInt8>' to expected argument type 'UnsafePointer<Int8>'}}
                         // expected-note@-1 {{arguments to generic parameter 'Pointee' ('UInt8' and 'Int8') are expected to be equal}}

    // Mutating a const array obviously does not work, no need to show these
    // in the proposal
    writeBytes(constArray) // expected-error {{cannot convert value of type '[UInt8]' to expected argument type 'UnsafeMutableRawPointer'}}
    writeInt8(constArray)  // expected-error {{cannot convert value of type '[UInt8]' to expected argument type 'UnsafeMutablePointer<Int8>'}}
    writeUInt8(constArray) // expected-error {{cannot convert value of type '[UInt8]' to expected argument type 'UnsafeMutablePointer<UInt8>'}}

    var byteArray: [UInt8] = [0]
    readInt8(&byteArray) // expected-error {{cannot convert value of type 'UnsafePointer<UInt8>' to expected argument type 'UnsafePointer<Int8>'}}
                          // expected-note@-1 {{arguments to generic parameter 'Pointee' ('UInt8' and 'Int8') are expected to be equal}}
    writeInt8(&byteArray) // expected-error {{cannot convert value of type 'UnsafeMutablePointer<UInt8>' to expected argument type 'UnsafeMutablePointer<Int8>'}}
                          // expected-note@-1 {{arguments to generic parameter 'Pointee' ('UInt8' and 'Int8') are expected to be equal}}
}
