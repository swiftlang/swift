// XFAIL: CPU=powerpc64le
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-experimental-clang-importer-diagnostics -typecheck -verify -I %S/Inputs %s

import cfuncs

func test_complex() {
    complex_parameter(1,2,3,4) // expected-error {{'complex_parameter' is unavailable: type 'Complex' is unavailable in Swift}}
    let _ = complex_retval() // expected-error {{'complex_retval()' is unavailable: type 'Complex' is unavailable in Swift}}
}

func test_atomic() {
    atomic_parameter(1,2,3,4) // expected-error {{'atomic_parameter' is unavailable: type 'Atomic' is unavailable in Swift}}
    let _ = atomic_retval() // expected-error {{'atomic_retval()' is unavailable: type 'Atomic' is unavailable in Swift}}
}
