// Ensure that an incomplete output-file-map path does not crash the driver,
// but instead outputs a nice diagnostic.
//
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not %swiftc_driver -c %S/../Inputs/empty.swift -output-file-map %t/malformed-output-file-map.json 2>&1 | %FileCheck %s
//
// CHECK: error: Expected quote at end of scalar
// CHECK-NOT: Assertion failed

//--- malformed-output-file-map.json
{
  "/path/to/some/file.swift": {
    "dependencies": "
