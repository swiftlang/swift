// RUN: %empty-directory(%t)

// Serialize and deserialize a deinit with SourceFile context to make sure we
// don't crash
// RUN: %target-swift-frontend -verify -module-name errors -emit-module -o %t/errors.swiftmodule -experimental-allow-module-with-compiler-errors %s
// RUN: %target-swift-ide-test -print-module -module-to-print=errors -source-filename=x -I %t -allow-compiler-errors

// Also check it wasn't serialized
// RUN: llvm-bcanalyzer -dump %t/errors.swiftmodule | %FileCheck %s
// CHECK-NOT: DESTRUCTOR_DECL

struct Foo {}

@discardableResult // expected-error{{'@discardableResult' attribute cannot be applied to this declaration}}
deinit {} // expected-error{{deinitializers may only be declared within a class, actor, or noncopyable type}}

func foo() -> Foo { return Foo() }

// Make sure @discardableResult isn't added to `foo`, which could be possible
// if the deinit is partially serialized
foo() // expected-warning{{result of call to 'foo()' is unused}}
