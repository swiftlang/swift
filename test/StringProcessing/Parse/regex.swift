// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: swift_in_compiler

_ = '/abc/'

_ = ('/[*/', '/+]/', '/.]/')
// expected-error@-1 {{cannot parse regular expression}}

_ = '/\w+/'
_ = '/\'\\/'
