enum StringEnum: String {
  case foo = "foo"
}

enum IntEnum: Int {
  case foo = 12
}

// RUN: %sourcekitd-test -req=cursor -pos=2:14 %s -- %s
// RUN: %sourcekitd-test -req=cursor -pos=6:14 %s -- %s