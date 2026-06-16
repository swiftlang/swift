// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// This test walks raw metadata/reflection structures by hand. Avoid any pointer
// authentication issues.
// UNSUPPORTED: CPU=arm64e

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// When the base of a dependent member type is a generic parameter with no
// available substitution, the base metadata is null. Make sure
// `createDependentMemberType` handles this gracefully.

import StdlibUnittest

@_silgen_name("swift_getTypeByMangledNameInContext")
func swift_getTypeByMangledNameInContext(
    _ name: UnsafePointer<CChar>,
    _ length: UInt,
    _ context: UnsafeRawPointer?,
    _ args: UnsafePointer<UnsafeRawPointer?>?
) -> UnsafeRawPointer?

protocol P { associatedtype A }
struct ConcreteT: P { typealias A = Int }
struct S<T: P> { var x: T.A }

func relative(_ base: UnsafeRawPointer) -> UnsafeRawPointer {
    let off = base.load(as: Int32.self)
    return base.advanced(by: Int(off))
}

func mangledLength(_ p: UnsafeRawPointer) -> Int {
    let b = p.assumingMemoryBound(to: UInt8.self)
    var i = 0
    while b[i] != 0 {
        let c = b[i]
        if c >= 0x01 && c <= 0x17 { i += 5 }
        else if c >= 0x18 && c <= 0x1f { i += 9 }
        else { i += 1 }
    }
    return i
}

func mangledTypeNameOfFieldX() -> (UnsafeRawPointer, Int) {
    let meta = unsafeBitCast(S<ConcreteT>.self, to: UnsafeRawPointer.self)

    // Nominal type descriptor pointer is the second word of struct metadata.
    let descriptor = meta.load(fromByteOffset: MemoryLayout<Int>.size,
                               as: UnsafeRawPointer.self)

    let fieldDescriptor = relative(descriptor.advanced(by: 16))
    let numFields = fieldDescriptor.load(fromByteOffset: 12, as: UInt32.self)
    precondition(numFields == 1, "expected exactly one stored property")
    let record0 = fieldDescriptor.advanced(by: 16)
    let mangledName = relative(record0.advanced(by: 4))
    return (mangledName, mangledLength(mangledName))
}

let suite = TestSuite("DependentMemberTypeDemangleNullBase")

suite.test("dependent member type with null context does not crash") {
    let (mangledName, len) = mangledTypeNameOfFieldX()

    // Build the type with no generic context: `T` has nothing to resolve to, so
    // the demangler produces a null base for `T.A`.
    let result = mangledName.withMemoryRebound(to: CChar.self, capacity: len) {
        swift_getTypeByMangledNameInContext($0, UInt(len), nil, nil)
    }

    expectNil(result, "a dependent member type with an unresolvable base " +
              "should fail the lookup")
}

runAllTests()
