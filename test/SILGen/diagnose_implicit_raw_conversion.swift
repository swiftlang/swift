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
func test_errors<T>(arg: T, sarg: String, anyBCArg: BitwiseCopyable) {
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
    read_char(constArray) // expected-warning {{forming 'UnsafePointer<CChar>' (aka 'UnsafePointer<Int8>') to a variable of type '[T]'; this is likely incorrect because 'T' may contain an object reference.}}
    read_uchar(constArray) // expected-warning {{forming 'UnsafePointer<UInt8>' to a variable of type '[T]'; this is likely incorrect because 'T' may contain an object reference.}}

    var array: [T] = [arg]
    readBytes(&array)     // expected-warning {{forming 'UnsafeRawPointer' to a variable of type '[T]'; this is likely incorrect because 'T' may contain an object reference.}}
    writeBytes(&array)  // expected-warning {{forming 'UnsafeMutableRawPointer' to a variable of type '[T]'; this is likely incorrect because 'T' may contain an object reference.}}

    read_char(&array)   // expected-warning {{forming 'UnsafePointer<Int8>' to a variable of type 'Array<T>'; this is likely incorrect because 'T' may contain an object reference.}}
    write_char(&array)  // expected-warning {{forming 'UnsafeMutablePointer<Int8>' to a variable of type 'Array<T>'; this is likely incorrect because 'T' may contain an object reference.}}
    read_uchar(&array)  // expected-warning {{forming 'UnsafePointer<UInt8>' to a variable of type 'Array<T>'; this is likely incorrect because 'T' may contain an object reference.}}
    write_uchar(&array) // expected-warning {{forming 'UnsafeMutablePointer<UInt8>' to a variable of type 'Array<T>'; this is likely incorrect because 'T' may contain an object reference.}}

    var anyBC: BitwiseCopyable = anyBCArg
    readBytes(&anyBC)   // expected-warning {{forming 'UnsafeRawPointer' to a variable of type 'any BitwiseCopyable'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    writeBytes(&anyBC)  // expected-warning {{forming 'UnsafeMutableRawPointer' to a variable of type 'any BitwiseCopyable'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    read_char(&anyBC)   // expected-warning {{forming 'UnsafePointer<Int8>' to a variable of type 'any BitwiseCopyable'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    write_char(&anyBC)  // expected-warning {{forming 'UnsafeMutablePointer<Int8>' to a variable of type 'any BitwiseCopyable'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    read_uchar(&anyBC)  // expected-warning {{forming 'UnsafePointer<UInt8>' to a variable of type 'any BitwiseCopyable'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    write_uchar(&anyBC) // expected-warning {{forming 'UnsafeMutablePointer<UInt8>' to a variable of type 'any BitwiseCopyable'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}

    let constBCArray: [BitwiseCopyable] = [anyBCArg]
    readBytes(constBCArray) // expected-warning {{forming 'UnsafeRawPointer' to a variable of type '[any BitwiseCopyable]'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    read_char(constBCArray) // expected-warning {{forming 'UnsafePointer<CChar>' (aka 'UnsafePointer<Int8>') to a variable of type '[any BitwiseCopyable]'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    read_uchar(constBCArray) // expected-warning {{forming 'UnsafePointer<UInt8>' to a variable of type '[any BitwiseCopyable]'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}

    var bcArray: [BitwiseCopyable] = [anyBCArg]
    readBytes(&bcArray)     // expected-warning {{forming 'UnsafeRawPointer' to a variable of type '[any BitwiseCopyable]'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    writeBytes(&bcArray)  // expected-warning {{forming 'UnsafeMutableRawPointer' to a variable of type '[any BitwiseCopyable]'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}

    read_char(&bcArray)   // expected-warning {{forming 'UnsafePointer<Int8>' to a variable of type 'Array<any BitwiseCopyable>'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    write_char(&bcArray)  // expected-warning {{forming 'UnsafeMutablePointer<Int8>' to a variable of type 'Array<any BitwiseCopyable>'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    read_uchar(&bcArray)  // expected-warning {{forming 'UnsafePointer<UInt8>' to a variable of type 'Array<any BitwiseCopyable>'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}
    write_uchar(&bcArray) // expected-warning {{forming 'UnsafeMutablePointer<UInt8>' to a variable of type 'Array<any BitwiseCopyable>'; this is likely incorrect because 'any BitwiseCopyable' may contain an object reference.}}

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
        read_char($0.baseAddress!)
        read_uchar($0.baseAddress!)
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
func test_accepted<I: FixedWidthInteger, BC: BitwiseCopyable>(intArg: I, bcArg: BC,
                                                              sarg: String, simdArg: SIMD4<Float>) {
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

    let constIntArray: [I] = [intArg]
    readBytes(constIntArray)
    read_char(constIntArray)
    read_uchar(constIntArray)

    var intArray: [I] = [intArg]
    readBytes(&intArray)
    writeBytes(&intArray)
    read_char(&intArray)
    write_char(&intArray)
    read_uchar(&intArray)
    write_uchar(&intArray)

    var bc: BC = bcArg
    readBytes(&bc)
    writeBytes(&bc)
    read_char(&bc)
    write_char(&bc)
    read_uchar(&bc)
    write_uchar(&bc)

    let constBCArray: [BC] = [bcArg]
    readBytes(constBCArray)
    read_char(constBCArray)
    read_uchar(constBCArray)

    var bcArray: [BC] = [bcArg]
    readBytes(&bcArray)
    writeBytes(&bcArray)
    read_char(&bcArray)
    write_char(&bcArray)
    read_uchar(&bcArray)
    write_uchar(&bcArray)

    let constByteArray: [UInt8] = [0]
    readBytes(constByteArray)
    read_char(constByteArray)
    read_uchar(constByteArray)

    var byteArray: [UInt8] = [0]
    readBytes(&byteArray)
    writeBytes(&byteArray)
    readUInt8(&byteArray)
    writeUInt8(&byteArray)
    write_char(&byteArray)
    read_uchar(byteArray)
    write_uchar(&byteArray)

    let constInt8Array: [Int8] = [0]
    readBytes(constInt8Array)
    read_char(constInt8Array)
    read_uchar(constInt8Array)

    var int8Array: [Int8] = [0]
    readBytes(&int8Array)
    writeBytes(&int8Array)
    read_char(&int8Array)
    write_char(&int8Array)
    read_uchar(&int8Array)
    write_uchar(&int8Array)

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
