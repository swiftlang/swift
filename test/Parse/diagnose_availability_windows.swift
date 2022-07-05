// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=windows-msvc

// expected-note@+2{{'unavailable()' has been explicitly marked unavailable here}}
@available(Windows, unavailable, message: "unsupported")
func unavailable() {}

// expected-error@+1 {{'unavailable()' is unavailable in Windows: unsupported}}
unavailable()

@available(Windows, introduced: 10.0.17763, deprecated: 10.0.19140)
func introduced_deprecated() {}

// expected-error@+1 {{'introduced_deprecated()' is only available in * 10.0.17763 or newe}}
introduced_deprecated()
// expected-note@-1 {{add 'if #available' version check}}

@available(Windows 10, *)
func windows10() {}

// expected-error@+1 {{'windows10()' is only available in * 10 or newer}}
windows10()
// expected-note@-1 {{add 'if #available' version check}}

func conditional_compilation() {
  if #available(Windows 10, *) {
  }
}
