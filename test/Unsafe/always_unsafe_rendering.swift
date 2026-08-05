// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Check how the diagnostic chains are actually rendered.
// RUN: not %target-swift-frontend -typecheck %t/test.swift -diagnostic-style=llvm 2>&1 | %PathSanitizingDiff %t/llvm-render.expected

//--- test.swift
@unsafe(always)
struct AlwaysUnsafeType {
  init() { }
}

func generic<T>(_: T) { }

func useGeneric() {
  generic(AlwaysUnsafeType())
}

@unsafe func merelyUnsafeReturningAlwaysUnsafe() -> AlwaysUnsafeType {
  unsafe AlwaysUnsafeType()
}

func useLaundered() {
  _ = merelyUnsafeReturningAlwaysUnsafe()
}

//--- llvm-render.expected
TMP_DIR/test.swift:9:3: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe' [#AlwaysUnsafe]
  generic(AlwaysUnsafeType())
  ^
  unsafe 
TMP_DIR/test.swift:9:11: note: argument #0 in call to global function 'generic' has unsafe type 'AlwaysUnsafeType'
  generic(AlwaysUnsafeType())
          ^~~~~~~~~~~~~~~~~~
TMP_DIR/test.swift:9:3: note: reference to unsafe type 'AlwaysUnsafeType'
  generic(AlwaysUnsafeType())
  ^
TMP_DIR/test.swift:9:11: note: argument 'self' in call to initializer 'init' has unsafe type 'AlwaysUnsafeType.Type'
  generic(AlwaysUnsafeType())
          ^~~~~~~~~~~~~~~~
TMP_DIR/test.swift:9:11: note: reference to unsafe type 'AlwaysUnsafeType'
  generic(AlwaysUnsafeType())
          ^
TMP_DIR/test.swift:9:11: note: reference to initializer 'init()' involves unsafe type 'AlwaysUnsafeType'
  generic(AlwaysUnsafeType())
          ^
TMP_DIR/test.swift:17:7: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe' [#AlwaysUnsafe]
  _ = merelyUnsafeReturningAlwaysUnsafe()
      ^
      unsafe 
TMP_DIR/test.swift:17:7: note: reference to global function 'merelyUnsafeReturningAlwaysUnsafe()' involves unsafe type 'AlwaysUnsafeType'
  _ = merelyUnsafeReturningAlwaysUnsafe()
      ^
