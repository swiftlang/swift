var description: String {

func test() {}

#if compiler()
#endif

// FIXME: Shouldn't need parse-as-library (https://github.com/swiftlang/swift/issues/84785)
// RUN: %sourcekitd-test -req=conformingmethods -pos=3:6 -repeat-request=2 %s -- %s -parse-as-library
  
