// RUN: not --crash %target-swift-ide-test -print-indexed-symbols -include-locals -source-filename %s
for _ in undefined where { let x = 0; return x == 0 }() {}
