// RUN: %target-typecheck-verify-swift -strict-memory-safety
// RUN: %target-typecheck-verify-swift -Wwarning StrictMemorySafety
// RUN: %target-typecheck-verify-swift -D FILE_DEFAULT

#if FILE_DEFAULT
default @diagnose(StrictMemorySafety, as: error)
#endif

@unsafe public struct UnsafeStruct: Codable {
  public var string: String
}


@unsafe public enum UnsafeEnum: Codable {
case something(Int)
}

@safe public struct SafeStruct: Codable {
  public var us: UnsafeStruct
}

@safe public enum SafeEnum: Codable {
case something(UnsafeEnum)
}

@unsafe public class C1: Codable {
  public var string: String = ""
}

@unsafe public class C2: C1 {
  public var otherString: String = ""
}
