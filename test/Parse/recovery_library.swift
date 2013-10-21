// RUN: %swift %s -parse-as-library -verify

//===--- Recovery for extra braces at top level.
//===--- Keep this test the first one in the file.

} // expected-error{{extraneous '}' at top level}} {{1-2=}}

func foo() {}

} // expected-error{{extraneous '}' at top level}} {{1-2=}}

