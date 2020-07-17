// This file matches test/SILGen/magic_identifier_file_conflicting.swift.
// It should be compiled with -verify.

// We should diagnose cross-file conflicts.
// expected-warning@+2 {{'#sourceLocation' directive produces '#fileID' string of 'Foo/other_file_c.swift', which conflicts with '#fileID' strings produced by other paths in the module}}
// expected-note@+1 {{change file in '#sourceLocation' to 'first/other_file_c.swift'}} {{23-50="first/other_file_c.swift"}}
#sourceLocation(file: "second/other_file_c.swift", line: 1)
#sourceLocation()
