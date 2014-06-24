// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift

// RUN: %swift -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -enable-source-import -I=%S/Inputs -sdk "" -enable-access-control -verify
// RUN: %swift -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -enable-source-import -I=%S/Inputs -sdk "" -disable-access-control

// RUN: %swift -emit-module -o %t %S/Inputs/has_accessibility.swift
// RUN: %swift -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -I=%t -sdk "" -enable-access-control -verify

import has_accessibility

println(has_accessibility.x)
println(has_accessibility.y) // expected-error {{module 'has_accessibility' has no member named 'y'}}
println(has_accessibility.z) // expected-error {{module 'has_accessibility' has no member named 'z'}}

println(accessibility.a)
println(accessibility.b)
println(accessibility.c) // expected-error {{module 'accessibility' has no member named 'c'}}

println(x)
println(y) // expected-error {{use of unresolved identifier 'y'}}
println(z) // expected-error {{use of unresolved identifier 'z'}}
println(a)
println(b)
println(c) // expected-error {{use of unresolved identifier 'c'}}

