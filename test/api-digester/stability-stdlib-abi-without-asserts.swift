// REQUIRES: OS=macosx
// REQUIRES: swift_stdlib_no_asserts
// RUN: %empty-directory(%t.tmp)
// mkdir %t.tmp/module-cache && mkdir %t.tmp/dummy.sdk
// RUN: %api-digester -diagnose-sdk -module Swift -o %t.tmp/changes.txt -module-cache-path %t.tmp/module-cache -sdk %t.tmp/dummy.sdk -abi -avoid-location -v
// RUN: sed 's|/[*].*[*]/||g' %S/Outputs/stability-stdlib-abi.without.asserts.swift.expected | sed '/^\s*$/d' | sort > %t.tmp/stability-stdlib-abi.swift.expected
// RUN: sed 's|/[*].*[*]/||g' %t.tmp/changes.txt | sed '/^\s*$/d' | sort > %t.tmp/changes.txt.tmp
// RUN: diff -u %t.tmp/stability-stdlib-abi.swift.expected %t.tmp/changes.txt.tmp

// The digester can incorrectly register a generic signature change when
// declarations are shuffled. rdar://problem/46618883
// UNSUPPORTED: swift_evolve
