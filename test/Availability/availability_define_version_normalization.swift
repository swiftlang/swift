/// This should not fail when defining 16.0 and using just 16
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm \
// RUN:   %s \
// RUN:   -define-availability "MyLib 16.0: macOS 16.0"

func test() {
    if #available(MyLib 16, *) {
        print("available")
    } else {
        print("unavailable")
    }
}
