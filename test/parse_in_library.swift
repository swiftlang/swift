// RUN: %swift %s -parse-as-library -verify

print(10) // expected-error {{expressions are not allowed at the top level}}
if (true) {  // expected-error {{statments are not allowed at the top level}}
  print(10)
}
