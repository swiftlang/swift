// RUN: %target-typecheck-verify-swift -strict-memory-safety
// RUN: %target-typecheck-verify-swift -Wwarning StrictMemorySafety
// RUN: %target-typecheck-verify-swift -D FILE_DEFAULT

#if FILE_DEFAULT
default @diagnose(StrictMemorySafety, as: error)
#endif

@unsafe public struct UnsafeStruct: Hashable {
  public var string: String
}


@unsafe public enum UnsafeEnum: Hashable {
case something(Int)
}

@safe public struct SafeStruct: Hashable {
  public var us: UnsafeStruct
}

@safe public enum SafeEnum: Hashable {
case something(UnsafeEnum)
}
