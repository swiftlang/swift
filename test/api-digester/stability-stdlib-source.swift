// REQUIRES: OS=macosx
// RUN: %empty-directory(%t.tmp)
// mkdir %t.tmp/module-cache && mkdir %t.tmp/dummy.sdk
// RUN: %api-digester -diagnose-sdk -module Swift -o %t.tmp/changes.txt -module-cache-path %t.tmp/module-cache -sdk %t.tmp/dummy.sdk -avoid-location
// RUN: sed 's|/[*].*[*]/||g' %S/Outputs/stability-stdlib-source.swift.expected | sed '/^\s*$/d' | sort > %t.tmp/stability-stdlib-source.swift.expected
// RUN: sed 's|/[*].*[*]/||g' %t.tmp/changes.txt | sed '/^\s*$/d' | sort > %t.tmp/changes.txt.tmp
// RUN: diff -u %t.tmp/stability-stdlib-source.swift.expected %t.tmp/changes.txt.tmp
