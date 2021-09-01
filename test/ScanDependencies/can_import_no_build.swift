// RUN: %target-swift-frontend -c -primary-file %s -Rmodule-interface-rebuild -I %S/Inputs/Swift/

#if canImport(Foo)
print("Can indeed import Foo!")
#else
print("Cannot import Foo!")
#endif

// CHECK-NOT: rebuilding module 'Foo' from interface
