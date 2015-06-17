// RUN: %target-parse-verify-swift -parse-as-library

//===--- Recovery for extra braces at top level.
//===--- Keep this test the first one in the file.

// Check that we handle multiple consecutive right braces.
} // expected-error{{extraneous '}' at top level}} {{1-2=}}
} // expected-error{{extraneous '}' at top level}} {{1-2=}}

func foo() {}
// Check that we handle multiple consecutive right braces.
} // expected-error{{extraneous '}' at top level}} {{1-2=}}
} // expected-error{{extraneous '}' at top level}} {{1-2=}}
func bar() {}

//===--- Recovery for extra braces at top level.
//===--- Keep this test the last one in the file.

// Check that we handle multiple consecutive right braces.
} // expected-error{{extraneous '}' at top level}} {{1-2=}}
} // expected-error{{extraneous '}' at top level}} {{1-2=}}

