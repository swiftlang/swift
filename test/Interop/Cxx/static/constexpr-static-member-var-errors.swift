// RUN: %target-swift-ide-test -print-module -module-to-print=ConstexprStaticMemberVarErrors -I %S/Inputs -source-filename=x -enable-cxx-interop 2>&1 | %FileCheck %s

// Check that we properly report the error and don't crash when importing an
// invalid decl.

// CHECK: error: type 'int' cannot be used prior to '::' because it has no members
// CHECK: {{note: in instantiation of template class 'GetTypeValue<int>' requested here|note: in instantiation of static data member 'GetTypeValue<int>::value' requested here}}

// CHECK: error: type 'int' cannot be used prior to '::' because it has no members
// CHECK: note: in instantiation of static data member 'GetTypeValueInline<int>::value' requested here
