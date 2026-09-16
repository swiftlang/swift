// RUN: not %target-swift-frontend -typecheck -swift-version 5 \
// RUN:   -diagnostic-style llvm %s 2>&1 \
// RUN:   | %FileCheck --strict-whitespace --enable-windows-compatibility %s

@inline(__always) @inline(never) func duplicateAttributeWithArguments() {}

@inlinable @inlinable public func duplicateSimpleAttribute() {}

final final class DuplicateModifier {}

public private var multipleAccessLevels = 0

public private(set) internal(set) var multipleSetterAccessLevels = 0

public private(set) private(set) var duplicateSetterModifier = 0

// CHECK:      error: duplicate attribute
// CHECK-NEXT: {{^}}@inline(__always) @inline(never) func duplicateAttributeWithArguments() {}
// CHECK-NEXT: {{^}}                  ~^~~~~~~~~~~~~{{$}}

// CHECK:      note: attribute already specified here
// CHECK-NEXT: {{^}}@inline(__always) @inline(never) func duplicateAttributeWithArguments() {}
// CHECK-NEXT: {{^}}~^~~~~~~~~~~~~~~~{{$}}

// CHECK:      error: duplicate attribute
// CHECK-NEXT: {{^}}@inlinable @inlinable public func duplicateSimpleAttribute() {}
// CHECK-NEXT: {{^}}           ~^~~~~~~~~{{$}}

// CHECK:      note: attribute already specified here
// CHECK-NEXT: {{^}}@inlinable @inlinable public func duplicateSimpleAttribute() {}
// CHECK-NEXT: {{^}}^~~~~~~~~~{{$}}

// CHECK:      error: duplicate modifier
// CHECK-NEXT: {{^}}final final class DuplicateModifier {}
// CHECK-NEXT: {{^}}      ^~~~~{{$}}

// CHECK:      note: modifier already specified here
// CHECK-NEXT: {{^}}final final class DuplicateModifier {}
// CHECK-NEXT: {{^}}^~~~~{{$}}

// CHECK:      error: multiple incompatible access-level modifiers specified
// CHECK-NEXT: {{^}}public private var multipleAccessLevels = 0
// CHECK-NEXT: {{^}}       ^~~~~~~{{$}}

// CHECK:      note: previous modifier specified here
// CHECK-NEXT: {{^}}public private var multipleAccessLevels = 0
// CHECK-NEXT: {{^}}^~~~~~{{$}}

// CHECK:      error: multiple incompatible access-level modifiers specified
// CHECK-NEXT: {{^}}public private(set) internal(set) var multipleSetterAccessLevels = 0
// CHECK-NEXT: {{^}}                    ^~~~~~~~~~~~~{{$}}

// CHECK:      note: previous modifier specified here
// CHECK-NEXT: {{^}}public private(set) internal(set) var multipleSetterAccessLevels = 0
// CHECK-NEXT: {{^}}       ^~~~~~~~~~~~{{$}}

// CHECK:      error: duplicate modifier
// CHECK-NEXT: {{^}}public private(set) private(set) var duplicateSetterModifier = 0
// CHECK-NEXT: {{^}}                    ^~~~~~~~~~~~{{$}}

// CHECK:      note: modifier already specified here
// CHECK-NEXT: {{^}}public private(set) private(set) var duplicateSetterModifier = 0
// CHECK-NEXT: {{^}}       ^~~~~~~~~~~~{{$}}
