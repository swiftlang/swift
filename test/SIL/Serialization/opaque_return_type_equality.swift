// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -swift-version 5 -disable-availability-checking -c %S/Inputs/opaque_return_type_equality_input.swift -emit-module-path %t/opaque_return_type_equality_input.swiftmodule -I %t -O -module-name opaque_return_type_equality_input -o %t/opaque_return_type_equality_input.o
// RUN: %target-swift-frontend -swift-version 5 -disable-availability-checking -c %S/opaque_return_type_equality.swift -emit-module-path %t/opaque_return_type_equality.swiftmodule -I %t -O -module-name opaque_return_type_equality -o %t/opaque_return_type_equality.o

// Check that the SIL type equality check asserts don't fail between the opaque return types and their underlying types.

import opaque_return_type_equality_input

public func build<F: Factory>(f: F) -> any Reducer<WindowData> {
  return f.build()
}

build(f: MyFactory())
