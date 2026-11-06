// RUN: %target-typecheck-verify-swift

let _ = "foo\(#"bar"##)baz"
// expected-error@-1{{too many '#' characters in closing delimiter}}
// expected-cparser-error@-2{{expected ',' separator}}
// expected-cparser-error@-3{{expected a macro identifier}}

let _ = #"\##("invalid")"#
// expected-cparser-error@-1{{too many '#' characters in delimited escape}}
// expected-cparser-error@-2{{invalid escape sequence in literal}}
// expected-astgen-error@-3{{too many '#' characters to start string interpolation}}

let _ = ###"""invalid"######
// expected-cparser-error@-1{{too many '#' characters in closing delimiter}}{{26-29=}} expected-astgen-error@-1{{too many '#' characters in closing delimiter}}{{23-29=}}{{29-29=###}}
// expected-cparser-error@-2{{consecutive statements on a line must be separated by ';'}}
// expected-cparser-error@-3{{expected a macro identifier}}

let _ = ####"invalid"###
// expected-cparser-error@-1{{unterminated string literal}}
// expected-astgen-error@-2{{expected '"####' to end string literal}}

let _ = ###"invalid"######
// expected-cparser-error@-1{{too many '#' characters in closing delimiter}}{{24-27=}} expected-astgen-error@-1{{too many '#' characters in closing delimiter}}{{21-27=}}{{27-27=###}}
// expected-cparser-error@-2{{consecutive statements on a line must be separated by ';'}}
// expected-cparser-error@-3{{expected a macro identifier}}

let _ = ##"""aa
  foobar
  aa"""##
// expected-cparser-error@-3{{multi-line string literal content must begin on a new line}}{{14-14=\n}} expected-astgen-error@-3{{multi-line string literal content must begin on a new line}}{{14-14=\n  }}
// expected-cparser-error@-2{{multi-line string literal closing delimiter must begin on a new line}}{{5-5=\n}} expected-astgen-error@-2{{multi-line string literal closing delimiter must begin on a new line}}{{5-5=\n  }}

let _ = #""" foo "bar" #baz
  """#
// expected-cparser-error@-2{{multi-line string literal content must begin on a new line}}{{13-13=\n}} expected-astgen-error@-2{{multi-line string literal content must begin on a new line}}{{13-13=\n  }}

let _ = ###""" "# "##
  """###
// expected-cparser-error@-2{{multi-line string literal content must begin on a new line}}{{15-15=\n}} expected-astgen-error@-2{{multi-line string literal content must begin on a new line}}{{15-15=\n  }}
