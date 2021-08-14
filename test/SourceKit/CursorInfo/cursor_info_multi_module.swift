#if MOD_PARTIALA
/// Comment from A
public func someFunc() {}
#endif

#if MOD_PARTIALB
public func funcBeforeLoc() {}
/// Comment from B
public func anotherFuncBeforeLoc() {}
#sourceLocation(file: "doesnotexist.swift", line: 10)
public func funcInLoc() {}
/// Comment from #sourceLocation
public func anotherFuncInLoc() {}
#sourceLocation()
public func funcAfterLoc() {}
/// Comment from B
public func anotherFuncAfterLoc() {}
#endif

#if MOD_USE
import somemod

func test() {
  someFunc()
  anotherFuncBeforeLoc()
  anotherFuncInLoc()
  anotherFuncAfterLoc()
}
#endif

// This test depends on line numbers, hence RUN lines being underneath the
// code.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)

// RUN: %target-swift-frontend -emit-module -emit-module-source-info -o %t/mods/somemoda.partial.swiftmodule -D MOD_PARTIALA -module-name somemod %s
// RUN: %target-swift-frontend -emit-module -emit-module-source-info -o %t/mods/somemodb.partial.swiftmodule -D MOD_PARTIALB -module-name somemod %s

// RUN: %target-swift-frontend -emit-module -emit-module-source-info -o %t/mods/somemod.swiftmodule -module-name somemod %t/mods/somemoda.partial.swiftmodule %t/mods/somemodb.partial.swiftmodule

// RUN: %sourcekitd-test -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=24:3 %s -- -D MOD_USE -I %t/mods -target %target-triple %s | %FileCheck --check-prefix=CHECK-NORMAL %s
// CHECK-NORMAL:      source.lang.swift.ref.function.free (3:13-3:21)
// CHECK-NORMAL-NEXT: someFunc()
// CHECK-NORMAL-NEXT: s:7somemod8someFuncyyF
// CHECK-NORMAL:      "docComment": {
// CHECK-NORMAL-NEXT:   "lines": [
// CHECK-NORMAL-NEXT:     {
// CHECK-NORMAL-NEXT:       "range": {
// CHECK-NORMAL-NEXT:         "end": {
// CHECK-NORMAL-NEXT:           "character": 18,
// CHECK-NORMAL-NEXT:           "line": 1
// CHECK-NORMAL-NEXT:         },
// CHECK-NORMAL-NEXT:         "start": {
// CHECK-NORMAL-NEXT:           "character": 4,
// CHECK-NORMAL-NEXT:           "line": 1
// CHECK-NORMAL-NEXT:         }
// CHECK-NORMAL-NEXT:       },
// CHECK-NORMAL-NEXT:       "text": "Comment from A"
// CHECK-NORMAL-NEXT:     }
// CHECK-NORMAL-NEXT:   ]
// CHECK-NORMAL-NEXT: },
// CHECK-NORMAL:      "location": {
// CHECK-NORMAL-NEXT:   "position": {
// CHECK-NORMAL-NEXT:     "character": 12,
// CHECK-NORMAL-NEXT:     "line": 2
// CHECK-NORMAL-NEXT:    },
// CHECK-NORMAL-NEXT:   "uri": "file://{{.*}}cursor_info_multi_module.swift"
// CHECK-NORMAL-NEXT: }

// RUN: %sourcekitd-test -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=25:3 %s -- -D MOD_USE -I %t/mods -target %target-triple %s | %FileCheck --check-prefix=CHECK-BEFORE %s
// CHECK-BEFORE:      source.lang.swift.ref.function.free (9:13-9:33)
// CHECK-BEFORE-NEXT: anotherFuncBeforeLoc()
// CHECK-BEFORE-NEXT: s:7somemod20anotherFuncBeforeLocyyF
// CHECK-BEFORE:      "docComment": {
// CHECK-BEFORE-NEXT:   "lines": [
// CHECK-BEFORE-NEXT:     {
// CHECK-BEFORE-NEXT:       "range": {
// CHECK-BEFORE-NEXT:         "end": {
// CHECK-BEFORE-NEXT:           "character": 18,
// CHECK-BEFORE-NEXT:           "line": 7
// CHECK-BEFORE-NEXT:         },
// CHECK-BEFORE-NEXT:         "start": {
// CHECK-BEFORE-NEXT:           "character": 4,
// CHECK-BEFORE-NEXT:           "line": 7
// CHECK-BEFORE-NEXT:         }
// CHECK-BEFORE-NEXT:       },
// CHECK-BEFORE-NEXT:       "text": "Comment from B"
// CHECK-BEFORE-NEXT:     }
// CHECK-BEFORE-NEXT:   ]
// CHECK-BEFORE-NEXT: },
// CHECK-BEFORE:      "location": {
// CHECK-BEFORE-NEXT:   "position": {
// CHECK-BEFORE-NEXT:     "character": 12,
// CHECK-BEFORE-NEXT:     "line": 8
// CHECK-BEFORE-NEXT:   },
// CHECK-BEFORE-NEXT:   "uri": "file://{{.*}}cursor_info_multi_module.swift"
// CHECK-BEFORE-NEXT: }

// Cursor info ignores #sourceLocation, symbol graph does not
// RUN: %sourcekitd-test -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=26:3 %s -- -D MOD_USE -I %t/mods -target %target-triple %s | %FileCheck --check-prefix=CHECK-IN %s
// CHECK-IN:      source.lang.swift.ref.function.free (13:13-13:29)
// CHECK-IN-NEXT: anotherFuncInLoc()
// CHECK-IN-NEXT: s:7somemod16anotherFuncInLocyyF
// CHECK-IN:      "docComment": {
// CHECK-IN-NEXT:   "lines": [
// CHECK-IN-NEXT:     {
// CHECK-IN-NEXT:       "range": {
// CHECK-IN-NEXT:         "end": {
// CHECK-IN-NEXT:           "character": 32,
// CHECK-IN-NEXT:           "line": 10
// CHECK-IN-NEXT:         },
// CHECK-IN-NEXT:         "start": {
// CHECK-IN-NEXT:           "character": 4,
// CHECK-IN-NEXT:           "line": 10
// CHECK-IN-NEXT:         }
// CHECK-IN-NEXT:       },
// CHECK-IN-NEXT:       "text": "Comment from #sourceLocation"
// CHECK-IN-NEXT:     }
// CHECK-IN-NEXT:   ]
// CHECK-IN-NEXT: },
// CHECK-IN:      "location": {
// CHECK-IN-NEXT:   "position": {
// CHECK-IN-NEXT:     "character": 12,
// CHECK-IN-NEXT:     "line": 11
// CHECK-IN-NEXT:   },
// CHECK-IN-NEXT:   "uri": "file://doesnotexist.swift"
// CHECK-IN-NEXT: }

// RUN: %sourcekitd-test -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=27:3 %s -- -D MOD_USE -I %t/mods -target %target-triple %s | %FileCheck --check-prefix=CHECK-AFTER %s
// CHECK-AFTER:      source.lang.swift.ref.function.free (17:13-17:32)
// CHECK-AFTER-NEXT: anotherFuncAfterLoc()
// CHECK-AFTER-NEXT: s:7somemod19anotherFuncAfterLocyyF
// CHECK-AFTER:      "docComment": {
// CHECK-AFTER-NEXT:   "lines": [
// CHECK-AFTER-NEXT:     {
// CHECK-AFTER-NEXT:       "range": {
// CHECK-AFTER-NEXT:         "end": {
// CHECK-AFTER-NEXT:           "character": 18,
// CHECK-AFTER-NEXT:           "line": 15
// CHECK-AFTER-NEXT:         },
// CHECK-AFTER-NEXT:         "start": {
// CHECK-AFTER-NEXT:           "character": 4,
// CHECK-AFTER-NEXT:           "line": 15
// CHECK-AFTER-NEXT:         }
// CHECK-AFTER-NEXT:       },
// CHECK-AFTER-NEXT:       "text": "Comment from B"
// CHECK-AFTER-NEXT:     }
// CHECK-AFTER-NEXT:   ]
// CHECK-AFTER-NEXT: },
// CHECK-AFTER:      "location": {
// CHECK-AFTER-NEXT:   "position": {
// CHECK-AFTER-NEXT:     "character": 12,
// CHECK-AFTER-NEXT:     "line": 16
// CHECK-AFTER-NEXT:   },
// CHECK-AFTER-NEXT:   "uri": "file://{{.*}}cursor_info_multi_module.swift"
// CHECK-AFTER-NEXT: }
