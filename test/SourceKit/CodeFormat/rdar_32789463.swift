struct $ {
let $: <#Type#>
= foo("foo \($) bar") {
$

// RUN: %sourcekitd-test -req=format -line=1 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "struct $ {"
// CHECK: key.sourcetext: "    let $: <#Type#>"
// CHECK: key.sourcetext: "    = foo(\"foo \\($) bar\") {"
// CHECK: key.sourcetext: "    $"
