public struct S {
  public private(set) var stored = 0
  public internal(set) var computed: Int { get { 0 } set {} }
  internal var internalComputed: Int { get { 0 } set {} }
}

// Cursor info can print a symbol graph for any declaration, but it only shows
// setters that are as accessible as their declaration.

// RUN: %sourcekitd-test -req=cursor -pos=2:27 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=STORED %s
// RUN: %sourcekitd-test -req=cursor -pos=3:28 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=COMPUTED %s
// RUN: %sourcekitd-test -req=cursor -pos=4:16 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=INTERNAL %s

// STORED:      "declarationFragments": [
// STORED:        "spelling": "stored"
// STORED:        "preciseIdentifier": "s:Si",
// STORED-NEXT:   "spelling": "Int"
// STORED-NEXT: },
// STORED-NEXT: {
// STORED-NEXT:   "kind": "text",
// STORED-NEXT:   "spelling": " { get }"
// STORED-NEXT: }
// STORED-NEXT: ],

// COMPUTED:      "declarationFragments": [
// COMPUTED:        "spelling": "computed"
// COMPUTED:        "spelling": " { "
// COMPUTED-NEXT: },
// COMPUTED-NEXT: {
// COMPUTED-NEXT:   "kind": "keyword",
// COMPUTED-NEXT:   "spelling": "get"
// COMPUTED-NEXT: },
// COMPUTED-NEXT: {
// COMPUTED-NEXT:   "kind": "text",
// COMPUTED-NEXT:   "spelling": " }"
// COMPUTED-NEXT: }
// COMPUTED-NEXT: ],

// INTERNAL:      "declarationFragments": [
// INTERNAL:        "spelling": "internalComputed"
// INTERNAL:        "spelling": " { "
// INTERNAL-NEXT: },
// INTERNAL-NEXT: {
// INTERNAL-NEXT:   "kind": "keyword",
// INTERNAL-NEXT:   "spelling": "get"
// INTERNAL-NEXT: },
// INTERNAL-NEXT: {
// INTERNAL-NEXT:   "kind": "text",
// INTERNAL-NEXT:   "spelling": " "
// INTERNAL-NEXT: },
// INTERNAL-NEXT: {
// INTERNAL-NEXT:   "kind": "keyword",
// INTERNAL-NEXT:   "spelling": "set"
// INTERNAL-NEXT: },
// INTERNAL-NEXT: {
// INTERNAL-NEXT:   "kind": "text",
// INTERNAL-NEXT:   "spelling": " }"
// INTERNAL-NEXT: }
// INTERNAL-NEXT: ],
