#if MODA_NORMAL
open class ParentClass {
  open func overridableMethodA(param: Int) {}
  open func overridableMethodB(param: Int) {}
  open func overridableMethodC(param: Int) {}
  open func overridableMethodD(param: Int) {}
}
#endif

#if MODA_LOC
open class ParentClass {
#sourceLocation(file: "doesnotexist.swift", line: 10)
  open func overridableMethodA(param: Int) {}
  open func overridableMethodB(param: Int) {}
#sourceLocation(file: "REPLACEDWITHSED", line: 20)
  open func overridableMethodC(param: Int) {}
  open func overridableMethodD(param: Int) {}
#sourceLocation()
}
#endif

#if MODB
import moda

open class SubClass: ParentClass {
  open override func overridableMethodA(param: String) {}
  open override func overridableMethodB(param: String) {}
  open override func overridableMethodC(param: String) {}
  open override func overridableMethodD(param: String) {}
}
#endif

// This test depends on line numbers, hence RUN lines being underneath the
// code.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)
// RUN: sed -e 's|REPLACEDWITHSED|%/t/alternative.swift|g' %s > %t/moda.swift

// RUN: %target-swift-frontend -emit-module -emit-module-source-info -o %t/mods/moda.swiftmodule -D MODA_NORMAL %t/moda.swift

// The diagnostic should have the real location from .swiftsourceinfo
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-EXISTS %s
// CHECK-EXISTS: moda.swift:3:13: note
// CHECK-EXISTS: moda.swift:4:13: note
// CHECK-EXISTS: moda.swift:5:13: note
// CHECK-EXISTS: moda.swift:6:13: note

// Removed the underlying file, so should use the generated source instead
// RUN: mv %t/moda.swift %t/moda.swift-moved
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-GENERATED %s
// CHECK-GENERATED: moda.ParentClass.overridableMethodA:{{.*}}: note:

// Underlying file has changed, so the locations in .swiftsourceinfo may not
// make sense any more. Ignored for now (ie. it is used regardless)
// RUN: echo "// file was modified" > %t/moda.swift
// RUN: cat %t/moda.swift-moved >> %t/moda.swift
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-OUTOFDATE %s
// CHECK-OUTOFDATE-NOT: moda.ParentClass:{{.*}}: note:
// CHECK-OUTOFDATE: moda.swift:{{.*}}: note:

// Underlying file is empty, the locations are now completely invalid (ie. not
// within the buffer). Make sure there's no crash and that we fallback to using
// the generated source.
// RUN: rm %t/moda.swift
// RUN: touch %t/moda.swift
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-EMPTY %s
// CHECK-EMPTY: moda.ParentClass.overridableMethodA:{{.*}}: note:

// The file and line from a location directive should be used whether or not it
// exists - the actual source still comes from the original file, so that's what
// matters in terms of whether generated code is used or not

// RUN: %empty-directory(%t/mods)
// RUN: mv %t/moda.swift-moved %t/moda.swift
// RUN: %target-swift-frontend -emit-module -emit-module-source-info -o %t/mods/moda.swiftmodule -D MODA_LOC %t/moda.swift

// RUN: cp %t/moda.swift %t/alternative.swift
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-DIRECTIVE %s
// CHECK-DIRECTIVE: {{^}}doesnotexist.swift:10:13: note
// CHECK-DIRECTIVE: {{^}}doesnotexist.swift:11:13: note
// CHECK-DIRECTIVE: alternative.swift:20:13: note
// CHECK-DIRECTIVE: alternative.swift:21:13: note

// File in line directive exists, but location does not
// RUN: mv %t/alternative.swift %t/alternative.swift-moved
// RUN: echo "" > %t/alternative.swift
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-DIRECTIVE %s

// Removed the underlying file, so should use the generated source instead
// RUN: mv %t/alternative.swift-moved %t/alternative.swift
// RUN: mv %t/moda.swift %t/moda.swift-moved
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-GENERATED %s
