// Continuous integration for the OS X Platform also runs the tests in the
// iPhone, Apple TV and Apple Watch simulators. We only need to run the
// swift_build_support module unit-tests once per OSX Platform test run, rather
// than once for each supported Apple device.

// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=watchos

// RUN: %empty-directory(%t)
// RUN: env SWIFT_BUILD_ROOT=%t %{python} %utils/swift_build_support/run_tests.py
