// REQUIRES: swift_swift_parser
// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=%(line+2):29 | %FileCheck %s --check-prefix=OPTIONAL
// OPTIONAL: func renameShorthandBinding(<base>opt</base>: Int?) {
func renameShorthandBinding(opt: Int?) {
  // RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=%(line+2):10 | %FileCheck %s --check-prefix=OPTIONAL
  // OPTIONAL: if let <base>opt</base> {
  if let opt {
    // RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=%(line+2):9 | %FileCheck %s --check-prefix=OPTIONAL
    // OPTIONAL: _ = <base>opt</base>
    _ = opt
  }
}
