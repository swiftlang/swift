// RUN: %target-typecheck-verify-swift

protocol P1 {}
protocol P2: P1 {}


struct S1<T> {}

extension S1: P2 where T: P1 {}
// expected-error@-1 {{conditional conformance of type 'S1<T>' to protocol 'P2' does not imply conformance to inherited protocol 'P1'}}
// expected-note@-2 {{did you mean to explicitly state the conformance with the same bounds using 'where T: P1'?}} {{1-1=extension S1: P1 where T: P1 {\n    <#witnesses#>\n\}\n\n}} 
// expected-note@-3 {{did you mean to explicitly state the conformance with different bounds?}} {{1-1=extension S1: P1 where <#requirements#> {\n    <#witnesses#>\n\}\n\n}}

protocol P3 {
    associatedtype X
}
struct S2<T, U, V: P3> {}

extension S2: P2 where T: P2, U: P2, V.X: P2 {}
// expected-error@-1 {{conditional conformance of type 'S2<T, U, V>' to protocol 'P2' does not imply conformance to inherited protocol 'P1'}}
// expected-note@-2 {{did you mean to explicitly state the conformance with relaxed bounds using 'where T: P1, U: P1, V.X: P1'?}} {{1-1=extension S2: P1 where T: P1, U: P1, V.X: P1 {\n    <#witnesses#>\n\}\n\n}} 
// expected-note@-3 {{did you mean to explicitly state the conformance with the same bounds using 'where T: P2, U: P2, V.X: P2'?}} {{1-1=extension S2: P1 where T: P2, U: P2, V.X: P2 {\n    <#witnesses#>\n\}\n\n}}
// expected-note@-4 {{did you mean to explicitly state the conformance with different bounds?}} {{1-1=extension S2: P1 where <#requirements#> {\n    <#witnesses#>\n\}\n\n}} 


struct S3<T, U, V: P3> {}

extension S3: P2 where T: P2, U: P2, V.X == Int {}
// expected-error@-1 {{conditional conformance of type 'S3<T, U, V>' to protocol 'P2' does not imply conformance to inherited protocol 'P1'}}
// expected-note@-2 {{did you mean to explicitly state the conformance with the same bounds using 'where T: P2, U: P2, V.X == Int'?}} {{1-1=extension S3: P1 where T: P2, U: P2, V.X == Int {\n    <#witnesses#>\n\}\n\n}} 
// expected-note@-3 {{did you mean to explicitly state the conformance with different bounds?}} {{1-1=extension S3: P1 where <#requirements#> {\n    <#witnesses#>\n\}\n\n}} 


struct S4<T, U, V: P3> {}

extension S4: P2 where T: P2, U: P3, V.X: P2 {}
// expected-error@-1 {{conditional conformance of type 'S4<T, U, V>' to protocol 'P2' does not imply conformance to inherited protocol 'P1'}}
// expected-note@-2 {{did you mean to explicitly state the conformance with the same bounds using 'where T: P2, U: P3, V.X: P2'?}}  {{1-1=extension S4: P1 where T: P2, U: P3, V.X: P2 {\n    <#witnesses#>\n\}\n\n}}
// expected-note@-3 {{did you mean to explicitly state the conformance with different bounds?}} {{1-1=extension S4: P1 where <#requirements#> {\n    <#witnesses#>\n\}\n\n}}

