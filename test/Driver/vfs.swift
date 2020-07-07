// Verifies that the driver passes the -vfsoverlay flag to the frontend (SR-12834).
// RUN: %swiftc_driver -driver-print-jobs -c -vfsoverlay overlay.yaml %s | %FileCheck --check-prefix=CHECK-ONE %s

// CHECK-ONE: bin{{/|\\\\}}swift{{(-frontend|c)?(\.exe)?"?}} -frontend{{.*}}-c{{.*}}-vfsoverlay overlay.yaml

// Verifies that multiple occurrences are passed in order.
// RUN: %swiftc_driver -driver-print-jobs -c -vfsoverlay overlay1.yaml -vfsoverlay overlay2.yaml -vfsoverlay overlay3.yaml %s | %FileCheck --check-prefix=CHECK-MULTIPLE %s

// CHECK-MULTIPLE: bin{{/|\\\\}}swift{{(-frontend|c)?(\.exe)?"?}} -frontend{{.*}}-c{{.*}}-vfsoverlay overlay1.yaml -vfsoverlay overlay2.yaml -vfsoverlay overlay3.yaml
