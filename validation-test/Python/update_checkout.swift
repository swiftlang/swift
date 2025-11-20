// Continuous integration for the OS X Platform also runs the tests in the
// iPhone, Apple TV and Apple Watch simulators. We only need to run the
// update_checkout module unit-tests once per OSX Platform test run, rather
// than once for each supported Apple device.

// REQUIRES: OS=macosx
// RUN: export SWIFT_BACKTRACE=

// RUN: %{python} %utils/update_checkout/run_tests.py
