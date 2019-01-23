// RUN: %target-swift-frontend -O -emit-ir  %s | %FileCheck --check-prefix=CHECK-%target-cpu %s
// RUN: %target-swift-frontend -Osize -emit-ir  %s | %FileCheck --check-prefix=CHECK-%target-cpu %s

// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: CPU=arm64 || CPU=x86_64

// This is an end-to-end test to ensure that the optimizer generates
// optimal code for static String variables.

public struct S {
  // CHECK-x86_64: {{^@"}}[[SMALL:.*smallstr.*pZ]]" ={{.*}} global {{.*}} inttoptr
  // CHECK-arm64: {{^@"}}[[SMALL:.*smallstr.*pZ]]" ={{.*}} global {{.*}} inttoptr
  public static let smallstr = "abc123a"
  // CHECK-arm64: {{^@"}}[[LARGE:.*largestr.*pZ]]" ={{.*}} global {{.*}} inttoptr {{.*}} add
  public static let largestr = "abc123asd3sdj3basfasdf"
  // CHECK-arm64: {{^@"}}[[UNICODE:.*unicodestr.*pZ]]" ={{.*}} global {{.*}} inttoptr {{.*}} add
  public static let unicodestr = "❄️gastroperiodyni"
}

// unsafeMutableAddressor for S.smallstr
// CHECK-arm64: define {{.*smallstr.*}}u"
// CHECK-arm64-NEXT: entry:
// CHECK-arm64-NEXT:   ret {{.*}} @"[[SMALL]]"
// CHECK-arm64-NEXT: }

// CHECK-x86_64: define {{.*smallstr.*}}u"
// CHECK-x86_64-NEXT: entry:
// CHECK-x86_64-NEXT:   ret {{.*}} @"[[SMALL]]"
// CHECK-x86_64-NEXT: }

// getter for S.smallstr
// CHECK-arm64: define {{.*smallstr.*}}gZ"
// CHECK-arm64-NEXT: entry:
// CHECK-arm64-NEXT:   ret {{.*}}
// CHECK-arm64-NEXT: }

// CHECK-x86_64: define {{.*smallstr.*}}gZ"
// CHECK-x86_64-NEXT: entry:
// CHECK-x86_64-NEXT:   ret {{.*}}
// CHECK-x86_64-NEXT: }

// unsafeMutableAddressor for S.largestr
// CHECK-arm64: define {{.*largestr.*}}u"
// CHECK-arm64-NEXT: entry:
// CHECK-arm64-NEXT:   ret {{.*}} @"[[LARGE]]"
// CHECK-arm64-NEXT: }

// getter for S.largestr
// CHECK-arm64: define {{.*largestr.*}}gZ"
// CHECK-arm64-NEXT: entry:
// CHECK-arm64-NEXT:   ret {{.*}}
// CHECK-arm64-NEXT: }

// unsafeMutableAddressor for S.unicodestr
// CHECK-arm64: define {{.*unicodestr.*}}u"
// CHECK-arm64-NEXT: entry:
// CHECK-arm64-NEXT:   ret {{.*}} @"[[UNICODE]]"
// CHECK-arm64-NEXT: }

// getter for S.unicodestr
// CHECK-arm64: define {{.*unicodestr.*}}gZ"
// CHECK-arm64-NEXT: entry:
// CHECK-arm64-NEXT:   ret {{.*}}
// CHECK-arm64-NEXT: }

// CHECK-arm64-LABEL: define {{.*}}get_smallstr
// CHECK-arm64:      entry:
// CHECK-arm64-NEXT:   ret {{.*}}
// CHECK-arm64-NEXT: }

// CHECK-x86_64-LABEL: define {{.*}}get_smallstr
// CHECK-x86_64:      entry:
// CHECK-x86_64-NEXT:   ret {{.*}}
// CHECK-x86_64-NEXT: }
@inline(never)
public func get_smallstr() -> String {
  return S.smallstr
}

// CHECK-arm64-LABEL: define {{.*}}get_largestr
// CHECK-arm64:      entry:
// CHECK-arm64-NEXT:   ret {{.*}}
// CHECK-arm64-NEXT: }
@inline(never)
public func get_largestr() -> String {
  return S.largestr
}

// CHECK-arm64-LABEL: define {{.*}}get_unicodestr
// CHECK-arm64:      entry:
// CHECK-arm64-NEXT:   ret {{.*}}
// CHECK-arm64-NEXT: }
@inline(never)
public func get_unicodestr() -> String {
  return S.unicodestr
}

// Also check if the generated code is correct.

// CHECK-OUTPUT: abc123a
// CHECK-OUTPUT: abc123asd3sdj3basfasdf
// CHECK-OUTPUT: ❄️gastroperiodyni
print(get_smallstr())
print(get_largestr())
print(get_unicodestr())

// Really load the globals from their addresses.
@_optimize(none)
func print_strings_from_addressors() {
  print(S.smallstr)
  print(S.largestr)
  print(S.unicodestr)
}

// CHECK-OUTPUT: abc123a
// CHECK-OUTPUT: abc123asd3sdj3basfasdf
// CHECK-OUTPUT: ❄️gastroperiodyni
print_strings_from_addressors()

