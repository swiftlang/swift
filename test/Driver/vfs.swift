// https://github.com/apple/swift/issues/55280

// Verifies that the driver passes the -vfsoverlay flag to the frontend.
// RUN: %swiftc_driver -driver-print-jobs -c -vfsoverlay overlay.yaml %s | %FileCheck --check-prefix=CHECK-ONE %s

// CHECK-ONE: bin{{/|\\\\}}swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend{{.*}}-c{{.*}}-vfsoverlay overlay.yaml

// Verifies that multiple occurrences are passed in order.
// RUN: %swiftc_driver -driver-print-jobs -c -vfsoverlay overlay1.yaml -vfsoverlay overlay2.yaml -vfsoverlay overlay3.yaml %s | %FileCheck --check-prefix=CHECK-MULTIPLE %s

// CHECK-MULTIPLE: bin{{/|\\\\}}swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend{{.*}}-c{{.*}}-vfsoverlay overlay1.yaml -vfsoverlay overlay2.yaml -vfsoverlay overlay3.yaml

// Verifies that input paths are not rejected prematurely when -vfsoverlay is present as they may exist on the vfs (which the frontend accounts for) even if they don't exist on the real file system.
// RUN: not %swiftc_driver -driver-print-jobs -c %t/file-not-on-the-real-filesystem.swift
// RUN: %swiftc_driver -driver-print-jobs -c -vfsoverlay overlay1.yaml %t/file-not-on-the-real-filesystem.swift

// RUN: mkdir -p %t
// RUN: cd %t
// RUN: echo "foobar" > main.swift
// RUN: not %swiftc_driver main.swift 2>&1 | %FileCheck --check-prefix=CHECK-ERROR %s
// RUN: echo '{"roots": [],"version": 0}' > %t.yaml
// RUN: not %swiftc_driver -vfsoverlay %t.yaml main.swift 2>&1 | %FileCheck --check-prefix=CHECK-ERROR %s
// RUN: echo '{"version": 0,"roots":[{"type":"directory","name":"%/t","contents":[{"type":"file","name":"vfsname.swift", "external-contents":"main.swift"}]}]}' > %t.yaml
// RUN: not %swiftc_driver -vfsoverlay %t.yaml vfsname.swift -v 2>&1 | %FileCheck --check-prefix=CHECK-ERROR %s
// CHECK-ERROR: {{^}}main.swift:1:1: error:
