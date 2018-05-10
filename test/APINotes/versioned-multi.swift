// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 3 | %FileCheck -check-prefix=CHECK-SWIFT-3 %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -swift-version 4 | %FileCheck -check-prefix=CHECK-SWIFT-4 %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -swift-version 4.2 | %FileCheck -check-prefix=CHECK-SWIFT-4-2 %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -swift-version 5 | %FileCheck -check-prefix=CHECK-SWIFT-5 %s

// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4_4")
// CHECK-SWIFT-3: var multiVersionedGlobal4: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal4_4: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Notes_4")
// CHECK-SWIFT-3: var multiVersionedGlobal4Notes: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal4Notes_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal4Notes_4")
// CHECK-SWIFT-3: var multiVersionedGlobal4Notes_NEW: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Header_4")
// CHECK-SWIFT-3: var multiVersionedGlobal4Header: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal4Header_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal4Header_4")
// CHECK-SWIFT-3: var multiVersionedGlobal4Header_NEW: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Both_4")
// CHECK-SWIFT-3: var multiVersionedGlobal4Both: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal4Both_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal4Both_4")
// CHECK-SWIFT-3: var multiVersionedGlobal4Both_NEW: Int32

// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal34_3: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 4, renamed: "multiVersionedGlobal34_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34_4: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Notes_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34Notes: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal34Notes_3: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 4, renamed: "multiVersionedGlobal34Notes_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34Notes_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal34Notes_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34Notes_NEW: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Header_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34Header: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal34Header_3: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 4, renamed: "multiVersionedGlobal34Header_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34Header_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal34Header_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34Header_NEW: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Both_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34Both: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal34Both_3: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 4, renamed: "multiVersionedGlobal34Both_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34Both_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal34Both_3")
// CHECK-SWIFT-3: var multiVersionedGlobal34Both_NEW: Int32

// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45_4")
// CHECK-SWIFT-3: var multiVersionedGlobal45: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal45_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45_4")
// CHECK-SWIFT-3: var multiVersionedGlobal45_5: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Notes_4")
// CHECK-SWIFT-3: var multiVersionedGlobal45Notes: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal45Notes_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45Notes_4")
// CHECK-SWIFT-3: var multiVersionedGlobal45Notes_5: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Header_4")
// CHECK-SWIFT-3: var multiVersionedGlobal45Header: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal45Header_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45Header_4")
// CHECK-SWIFT-3: var multiVersionedGlobal45Header_5: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Both_4")
// CHECK-SWIFT-3: var multiVersionedGlobal45Both: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal45Both_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45Both_4")
// CHECK-SWIFT-3: var multiVersionedGlobal45Both_5: Int32

// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal345_3: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 4, renamed: "multiVersionedGlobal345_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345_5: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Notes_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345Notes: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal345Notes_3: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 4, renamed: "multiVersionedGlobal345Notes_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345Notes_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345Notes_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345Notes_5: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Header_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345Header: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal345Header_3: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 4, renamed: "multiVersionedGlobal345Header_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345Header_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345Header_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345Header_5: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Both_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345Both: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal345Both_3: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 4, renamed: "multiVersionedGlobal345Both_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345Both_4: Int32
// CHECK-SWIFT-3: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345Both_3")
// CHECK-SWIFT-3: var multiVersionedGlobal345Both_5: Int32
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34_4_2_not_5")
// CHECK-SWIFT-3: var multiVersionedGlobal34_4_2: Int32
// CHECK-SWIFT-3: var multiVersionedGlobal34_4_2_not_5: Int32


// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4_4")
// CHECK-SWIFT-4: var multiVersionedGlobal4: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal4_4: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Notes_4")
// CHECK-SWIFT-4: var multiVersionedGlobal4Notes: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal4Notes_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal4Notes_4")
// CHECK-SWIFT-4: var multiVersionedGlobal4Notes_NEW: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Header_4")
// CHECK-SWIFT-4: var multiVersionedGlobal4Header: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal4Header_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal4Header_4")
// CHECK-SWIFT-4: var multiVersionedGlobal4Header_NEW: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Both_4")
// CHECK-SWIFT-4: var multiVersionedGlobal4Both: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal4Both_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal4Both_4")
// CHECK-SWIFT-4: var multiVersionedGlobal4Both_NEW: Int32

// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34_3: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal34_4: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Notes_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34Notes: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34Notes_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34Notes_3: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal34Notes_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal34Notes_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34Notes_NEW: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Header_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34Header: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34Header_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34Header_3: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal34Header_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal34Header_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34Header_NEW: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Both_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34Both: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34Both_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34Both_3: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal34Both_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal34Both_4")
// CHECK-SWIFT-4: var multiVersionedGlobal34Both_NEW: Int32

// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45_4")
// CHECK-SWIFT-4: var multiVersionedGlobal45: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal45_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45_4")
// CHECK-SWIFT-4: var multiVersionedGlobal45_5: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Notes_4")
// CHECK-SWIFT-4: var multiVersionedGlobal45Notes: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal45Notes_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45Notes_4")
// CHECK-SWIFT-4: var multiVersionedGlobal45Notes_5: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Header_4")
// CHECK-SWIFT-4: var multiVersionedGlobal45Header: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal45Header_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45Header_4")
// CHECK-SWIFT-4: var multiVersionedGlobal45Header_5: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Both_4")
// CHECK-SWIFT-4: var multiVersionedGlobal45Both: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal45Both_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45Both_4")
// CHECK-SWIFT-4: var multiVersionedGlobal45Both_5: Int32

// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345_3: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal345_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345_5: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Notes_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345Notes: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345Notes_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345Notes_3: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal345Notes_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345Notes_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345Notes_5: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Header_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345Header: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345Header_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345Header_3: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal345Header_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345Header_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345Header_5: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Both_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345Both: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345Both_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345Both_3: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal345Both_4: Int32
// CHECK-SWIFT-4: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345Both_4")
// CHECK-SWIFT-4: var multiVersionedGlobal345Both_5: Int32
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34_4_2_not_5")
// CHECK-SWIFT-4: var multiVersionedGlobal34_4_2: Int32
// CHECK-SWIFT-4: var multiVersionedGlobal34_4_2_not_5: Int32

// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal4: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal4_4: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Notes_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal4Notes: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal4Notes_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal4Notes_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal4Notes_NEW: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Header_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal4Header: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal4Header_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal4Header_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal4Header_NEW: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Both_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal4Both: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal4Both_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal4Both_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal4Both_NEW: Int32

// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34_3: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal34_4: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Notes_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Notes: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34Notes_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Notes_3: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Notes_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal34Notes_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Notes_NEW: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Header_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Header: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34Header_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Header_3: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Header_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal34Header_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Header_NEW: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Both_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Both: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34Both_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Both_3: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Both_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal34Both_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34Both_NEW: Int32

// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal45: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal45_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal45_5: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Notes_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal45Notes: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal45Notes_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45Notes_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal45Notes_5: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Header_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal45Header: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal45Header_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45Header_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal45Header_5: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Both_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal45Both: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal45Both_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal45Both_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal45Both_5: Int32

// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345_3: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal345_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345_5: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Notes_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Notes: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345Notes_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Notes_3: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Notes_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345Notes_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Notes_5: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Header_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Header: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345Header_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Header_3: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Header_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345Header_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Header_5: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Both_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Both: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345Both_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Both_3: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Both_4: Int32
// CHECK-SWIFT-4-2: @available(swift, introduced: 5, renamed: "multiVersionedGlobal345Both_4_2")
// CHECK-SWIFT-4-2: var multiVersionedGlobal345Both_5: Int32
// CHECK-SWIFT-4-2: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34_4_2_not_5")
// CHECK-SWIFT-4-2: var multiVersionedGlobal34_4_2: Int32
// CHECK-SWIFT-4-2: var multiVersionedGlobal34_4_2_not_5: Int32


// CHECK-SWIFT-5: var multiVersionedGlobal4: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal4")
// CHECK-SWIFT-5: var multiVersionedGlobal4_4: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Notes_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal4Notes: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal4Notes_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal4Notes_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal4Notes_NEW: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Header_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal4Header: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal4Header_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal4Header_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal4Header_NEW: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal4Both_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal4Both: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal4Both_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal4Both_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal4Both_NEW: Int32

// CHECK-SWIFT-5: var multiVersionedGlobal34: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34")
// CHECK-SWIFT-5: var multiVersionedGlobal34_3: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal34")
// CHECK-SWIFT-5: var multiVersionedGlobal34_4: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Notes_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal34Notes: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34Notes_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal34Notes_3: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal34Notes_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal34Notes_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal34Notes_NEW: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Header_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal34Header: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34Header_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal34Header_3: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal34Header_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal34Header_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal34Header_NEW: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal34Both_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal34Both: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal34Both_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal34Both_3: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal34Both_NEW")
// CHECK-SWIFT-5: var multiVersionedGlobal34Both_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal34Both_NEW: Int32

// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45_5")
// CHECK-SWIFT-5: var multiVersionedGlobal45: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal45_5")
// CHECK-SWIFT-5: var multiVersionedGlobal45_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal45_5: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Notes_5")
// CHECK-SWIFT-5: var multiVersionedGlobal45Notes: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal45Notes_5")
// CHECK-SWIFT-5: var multiVersionedGlobal45Notes_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal45Notes_5: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Header_5")
// CHECK-SWIFT-5: var multiVersionedGlobal45Header: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal45Header_5")
// CHECK-SWIFT-5: var multiVersionedGlobal45Header_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal45Header_5: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal45Both_5")
// CHECK-SWIFT-5: var multiVersionedGlobal45Both: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal45Both_5")
// CHECK-SWIFT-5: var multiVersionedGlobal45Both_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal45Both_5: Int32

// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345_3: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal345_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal345_5: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Notes_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345Notes: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345Notes_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345Notes_3: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal345Notes_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345Notes_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal345Notes_5: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Header_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345Header: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345Header_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345Header_3: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal345Header_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345Header_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal345Header_5: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "multiVersionedGlobal345Both_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345Both: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 4, renamed: "multiVersionedGlobal345Both_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345Both_3: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal345Both_5")
// CHECK-SWIFT-5: var multiVersionedGlobal345Both_4: Int32
// CHECK-SWIFT-5: var multiVersionedGlobal345Both_5: Int32
// CHECK-SWIFT-5: @available(swift, obsoleted: 5, renamed: "multiVersionedGlobal34_4_2")
// CHECK-SWIFT-5: var multiVersionedGlobal34_4_2_not_5: Int32
