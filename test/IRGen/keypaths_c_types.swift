// RUN: %target-swift-frontend -primary-file %s -emit-ir -import-objc-header %S/Inputs/keypaths_c_types.h

// This used to crash while trying to emit a reference to the property
// descriptor for the some_field property.

struct Foo {
  static let somePath: WritableKeyPath<c_union, some_struct>? = \c_union.some_field
}
