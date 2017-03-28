// RUN: %{python} %utils/python_lint.py
// Continuous integration for the OS X Platform also runs the tests in the iPhone, Apple TV and Apple Watch simulators.
// We only need to run python linting once per OSX Platform test run, rather than once for each supported Apple device.
// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos
