// RUN: %target-run-simple-swift(-Xlinker /HIGHENTROPYVA:NO -Xlinker /DYNAMICBASE:NO)
// REQUIRES: executable_test, OS=windows-msvc, CPU=x86_64
// UNSUPPORTED: use_os_stdlib, back_deployment_runtime

// Without high-entropy address randomization, Windows can allocate breadcrumbs
// below Int32.max. Such a pointer must not be mistaken for a cached UTF-16 count.

let units = Array(repeating: UInt16(0x00E9), count: 128)
var value = String(decoding: units, as: UTF16.self)
precondition(value.utf16.count == units.count)

// An interior UTF-16 offset populates the full breadcrumbs cache.
let midpoint = value.utf16.index(value.utf16.startIndex, offsetBy: 64)
precondition(value.utf16[midpoint] == units[64])
precondition(value.utf16.count == units.count)

value.append("x")
precondition(value.utf16.count == units.count + 1)
precondition(Array(value.utf16) == units + [0x0078])
