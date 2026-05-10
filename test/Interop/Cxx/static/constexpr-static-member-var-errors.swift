// RUN: %target-swift-ide-test -print-module -module-to-print=ConstexprStaticMemberVarErrors -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -Xcc -std=c++14 2>&1 | %FileCheck %s

// Check that we properly report the error and don't crash when importing an
// invalid decl.
// When using C++17, this C++ header doesn't trigger any Clang error. This is expected.

// Windows doesn't fail at all here which seems ok (and probably should be the case for other platforms too).
// XFAIL: OS=windows-msvc

// CHECK: error: type 'int' cannot be used prior to '::' because it has no members
// CHECK: {{note: in instantiation of template class 'GetTypeValue<int>' requested here|note: in instantiation of static data member 'GetTypeValue<int>::value' requested here}}
