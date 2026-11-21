// RUN: %target-swift-frontend -typecheck -verify %s
// RUN: %target-swift-frontend -typecheck -verify -DHAS_MAIN %s

// @main should only suppress top level code parsing if it's in an active
// conditional compilation branch.

#if HAS_MAIN
@main
#endif
struct Entry {
  static func main() {
    print(greeting)
  }
}

let greeting = "hello"

#if !HAS_MAIN
print(greeting)
#endif
