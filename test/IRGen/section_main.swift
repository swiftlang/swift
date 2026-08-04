// RUN: %target-swift-frontend -emit-sil -parse-as-library %s | %FileCheck %s --check-prefix=SYNC
// RUN: %target-swift-frontend -emit-sil -parse-as-library -DASYNC %s | %FileCheck %s --check-prefix=ASYNC

// REQUIRES: concurrency

// The '@section' on the 'main' function of a '@main' type also applies to the
// entry points that the compiler synthesizes for it.
//
// The C entry point is named 'main', except on WebAssembly, whose C ABI calls
// it '__main_argc_argv'.

#if ASYNC
@main
struct Boot {
  @section("__TEXT,boot")
  static func main() async {}
}

// ASYNC: sil hidden [section "__TEXT,boot"] @$s12section_main4BootV0B0yyYaFZ
// ASYNC: sil hidden [section "__TEXT,boot"] @$s12section_main4BootV5$mainyyYaFZ
// ASYNC: sil private [section "__TEXT,boot"] @async_Main
// ASYNC: sil [section "__TEXT,boot"] @{{main|__main_argc_argv}}
#else
@main
struct Boot {
  @section("__TEXT,boot")
  static func main() {}
}

// SYNC: sil hidden [section "__TEXT,boot"] @$s12section_main4BootV0B0yyFZ
// SYNC: sil hidden [section "__TEXT,boot"] @$s12section_main4BootV5$mainyyFZ
// SYNC: sil [section "__TEXT,boot"] @{{main|__main_argc_argv}}
#endif
