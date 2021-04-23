#if MODA_NORMAL
open class ParentClass {
  open func overridableMethod(param: Int) {}
}
#endif

#if MODA_LOC
open class ParentClass {
#sourceLocation(file: "REPLACEDWITHSED", line: 10)
  open func overridableMethod(param: Int) {}
#sourceLocation()
}
#endif

#if MODB
import moda

open class SubClass: ParentClass {
  open override func overridableMethod(param: String) {}
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

// Removed the underlying file, so should use the generated source instead
// RUN: mv %t/moda.swift %t/moda.swift-moved
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-GENERATED %s
// CHECK-GENERATED: moda.ParentClass:{{.*}}: note:

// Underlying file has changed, so the locations in .swiftsourceinfo may not
// make sense any more. Ignored for now (ie. it is used regardless)
// RUN: echo "// file was modified" > %t/moda.swift
// RUN: cat %t/moda.swift-moved >> %t/moda.swift
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-EXISTS %s


// The file and line from a location directive should be used whether or not it
// exists - the actual source still comes from the original file, so that's what
// matters in terms of whether generated code is used or not

// RUN: %empty-directory(%t/mods)
// RUN: mv %t/moda.swift-moved %t/moda.swift
// RUN: %target-swift-frontend -emit-module -emit-module-source-info -o %t/mods/moda.swiftmodule -D MODA_LOC %t/moda.swift

// Line directive file exists
// RUN: cp %t/moda.swift %t/alternative.swift
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-DIRECTIVE %s

// File missing
// RUN: mv %t/alternative.swift %t/alternative.swift-moved
// FIXME: Because the presumed location is output and then read back in, the
//        location is invalid and it falls back to the generated. This should be
//        CHECK-DIRECTIVE
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-GENERATED %s
// CHECK-DIRECTIVE: alternative.swift:10:13: note

// File exists but location does not
// RUN: echo "" > %t/alternative.swift
// FIXME: As above, this should be CHECK-DIRECTIVE
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-GENERATED %s

// Removed the underlying file, so should use the generated source instead
// RUN: mv %t/alternative.swift-moved %t/alternative.swift
// RUN: mv %t/moda.swift %t/moda.swift-moved
// FIXME: Should be CHECK-GENERATED but works as, again, it's the presumed
//        location that's output
// RUN: not %target-swift-frontend -typecheck -I %t/mods -D MODB %s 2>&1 | %FileCheck -check-prefix=CHECK-DIRECTIVE %s
