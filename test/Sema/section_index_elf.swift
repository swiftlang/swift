// RUN: %target-typecheck-verify-swift -parse-as-library
// REQUIRES: OS_BINFMT=elf

// The linker defines __start_/__stop_ bounds only for sections whose name is a
// C identifier, so only those can be listed in the image's section index. The
// warning is for records meant to be found: '@used' variables.

@section("myplugins") @used var ok0: Int = 1
@section("_my_plugins2") @used var ok1: Int = 2
@section("mycode") @used func ok2() {}

@section(".myplugins") @used var bad0: Int = 3 // expected-warning {{section name '.myplugins' is not a C identifier, so the section will not be listed in the image's section index}}
@section("my-plugins") @used var bad1: Int = 4 // expected-warning {{section name 'my-plugins' is not a C identifier, so the section will not be listed in the image's section index}}
@section("2plugins") @used var bad2: Int = 5 // expected-warning {{section name '2plugins' is not a C identifier, so the section will not be listed in the image's section index}}

// Functions are not indexed, and a variable that is not '@used' is not
// expected to be found, so neither warns.
@section("my.code") @used func quiet0() {}
@section(".myplugins") var quiet1: Int = 6
