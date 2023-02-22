// RUN: %target-swift-emit-silgen -import-objc-header %S/Inputs/readbytes.h %s -o /dev/null -verify
//
// Diagnose invalid conversion from an inout argument to a raw pointer.

func readBytes(_ pointer: UnsafeRawPointer) {}
func writeBytes(_ pointer: UnsafeMutableRawPointer) {}
func readInt8(_ pointer: UnsafePointer<Int8>) {}
func writeInt8(_ pointer: UnsafeMutablePointer<Int8>) {}
func readUInt8(_ pointer: UnsafePointer<UInt8>) {}
func writeUInt8(_ pointer: UnsafeMutablePointer<UInt8>) {}

struct Aggregate {
    var pointer: UnsafeRawPointer?
    var value: Int32
}

class C {
    init() {}
}

struct NonTrivial {
    var c: C
}

// SILGen diagnostics prohibits these implicit casts:
func test_errors<T>(arg: T, sarg: String) {
    var nonTrivial = NonTrivial(c: C())
    readBytes(&nonTrivial)   // expected-warning {{forming 'UnsafeRawPointer' to a variable of type 'NonTrivial'; this is likely incorrect because 'NonTrivial' may contain an object reference.}}
    writeBytes(&nonTrivial)  // expected-warning {{forming 'UnsafeMutableRawPointer' to a variable of type 'NonTrivial'; this is likely incorrect because 'NonTrivial' may contain an object reference.}}

    var t: T = arg
    readBytes(&t)   // expected-warning {{forming 'UnsafeRawPointer' to a variable of type 'T'; this is likely incorrect because 'T' may contain an object reference.}}
    writeBytes(&t)  // expected-warning {{forming 'UnsafeMutableRawPointer' to a variable of type 'T'; this is likely incorrect because 'T' may contain an object reference.}}
    read_char(&t)   // expected-warning {{forming 'UnsafePointer<Int8>' to a variable of type 'T'; this is likely incorrect because 'T' may contain an object reference.}}
    write_char(&t)  // expected-warning {{forming 'UnsafeMutablePointer<Int8>' to a variable of type 'T'; this is likely incorrect because 'T' may contain an object reference.}}
    read_uchar(&t)  // expected-warning {{forming 'UnsafePointer<UInt8>' to a variable of type 'T'; this is likely incorrect because 'T' may contain an object reference.}}
    write_uchar(&t) // expected-warning {{forming 'UnsafeMutablePointer<UInt8>' to a variable of type 'T'; this is likely incorrect because 'T' may contain an object reference.}}

    let constArray: [T] = [arg]
    readBytes(constArray) // expected-warning {{forming 'UnsafeRawPointer' to a variable of type '[T]'; this is likely incorrect because 'T' may contain an object reference.}}

    var array: [T] = [arg]
    readBytes(&array)     // expected-warning {{forming 'UnsafeRawPointer' to a variable of type '[T]'; this is likely incorrect because 'T' may contain an object reference.}}
    writeBytes(&array)  // expected-warning {{forming 'UnsafeMutableRawPointer' to a variable of type '[T]'; this is likely incorrect because 'T' may contain an object reference.}}

    read_char(&array)   // expected-warning {{forming 'UnsafePointer<Int8>' to a variable of type 'Array<T>'; this is likely incorrect because 'T' may contain an object reference.}}
    write_char(&array)  // expected-warning {{forming 'UnsafeMutablePointer<Int8>' to a variable of type 'Array<T>'; this is likely incorrect because 'T' may contain an object reference.}}
    read_uchar(&array)  // expected-warning {{forming 'UnsafePointer<UInt8>' to a variable of type 'Array<T>'; this is likely incorrect because 'T' may contain an object reference.}}
    write_uchar(&array) // expected-warning {{forming 'UnsafeMutablePointer<UInt8>' to a variable of type 'Array<T>'; this is likely incorrect because 'T' may contain an object reference.}}

    var string: String = sarg
    readBytes(&string)   // expected-warning {{forming 'UnsafeRawPointer' to an inout variable of type String exposes the internal representation rather than the string contents.}}
    writeBytes(&string)  // expected-warning {{forming 'UnsafeMutableRawPointer' to an inout variable of type String exposes the internal representation rather than the string contents.}}
    read_char(&string)   // expected-warning {{forming 'UnsafePointer<Int8>' to an inout variable of type String exposes the internal representation rather than the string contents.}}
    write_char(&string)  // expected-warning {{forming 'UnsafeMutablePointer<Int8>' to an inout variable of type String exposes the internal representation rather than the string contents.}}
    read_uchar(&string)  // expected-warning {{forming 'UnsafePointer<UInt8>' to an inout variable of type String exposes the internal representation rather than the string contents.}}
    write_uchar(&string) // expected-warning {{forming 'UnsafeMutablePointer<UInt8>' to an inout variable of type String exposes the internal representation rather than the string contents.}}
}

// Test the explicit cast workaround for all of the above errors
func test_explicit<T>(arg: T, sarg: String) {
    var nonTrivial = NonTrivial(c: C())
    withUnsafeBytes(of: &nonTrivial) {
        readBytes($0.baseAddress!)
    }
    withUnsafeMutableBytes(of: &nonTrivial) {
        writeBytes($0.baseAddress!)
    }

    var t: T = arg
    withUnsafeBytes(of: &t) {
        readBytes($0.baseAddress!)
        read_char($0.baseAddress!)
        read_uchar($0.baseAddress!)
    }
    withUnsafeMutableBytes(of: &t) {
        writeBytes($0.baseAddress!)
        write_char($0.baseAddress!)
        write_uchar($0.baseAddress!)
    }
    let constArray: [T] = [arg]
    constArray.withUnsafeBytes() {
        readBytes($0.baseAddress!)
    }
    var array: [T] = [arg]
    array.withUnsafeBytes() {
        readBytes($0.baseAddress!)
        read_char($0.baseAddress!)
        read_uchar($0.baseAddress!)
    }
    array.withUnsafeMutableBytes() {
        writeBytes($0.baseAddress!)
        write_char($0.baseAddress!)
        write_uchar($0.baseAddress!)
    }

    let string: String = sarg
    readBytes(string)
    read_char(string)
    read_uchar(string)
}

// SILGen diagnostics accepts these implicit casts:
func test_accepted<I: FixedWidthInteger>(intArg: I, sarg: String, simdArg: SIMD4<Float>) {
    var aggregate = Aggregate(pointer: UnsafeRawPointer(bitPattern: 0), value: 0)
    readBytes(&aggregate)
    writeBytes(&aggregate)
    read_char(&aggregate)
    write_char(&aggregate)
    read_uchar(&aggregate)
    write_uchar(&aggregate)

    var int: I = intArg 
    readBytes(&int)
    writeBytes(&int)
    read_char(&int)
    write_char(&int)
    read_uchar(&int)
    write_uchar(&int)

    let constArray: [I] = [intArg]
    readBytes(constArray)
    // read_char(constArray) - this case never worked because of a bug in SE-0324
    // read_uchar(constArray) - this case never worked because of a bug in SE-0324

    var intArray: [I] = [intArg]
    readBytes(&intArray)
    writeBytes(&intArray)
    read_char(&intArray)
    write_char(&intArray)
    read_uchar(&intArray)
    write_uchar(&intArray)

    var byteArray: [UInt8] = [0]
    readBytes(&byteArray)
    writeBytes(&byteArray)
    readUInt8(&byteArray)
    writeUInt8(&byteArray)
    write_char(&byteArray)
    read_uchar(byteArray)
    write_uchar(&byteArray)

    let string: String = sarg
    readBytes(string)
    readUInt8(string)
    read_char(string)
    read_uchar(string)

    var simd: SIMD4<Float> = simdArg
    readBytes(&simd)
    writeBytes(&simd)
    read_char(&simd)
    write_char(&simd)
    read_uchar(&simd)
    write_uchar(&simd)
}
