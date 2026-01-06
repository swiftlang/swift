// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded
// RUN: %batch-code-completion -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded

func test() {
  #^GLOBAL^#
// GLOBAL: Literal[Integer]/None:              0[#Int#];
// GLOBAL: Literal[Boolean]/None:              true[#Bool#];
// GLOBAL: Literal[Boolean]/None:              false[#Bool#];
// GLOBAL: Literal[Nil]/None:                  nil;
// GLOBAL: Literal[String]/None:               "{#(abc)#}"[#String#];
// GLOBAL: Literal[Array]/None:                [{#(values)#}][#Array<Element>#];
// GLOBAL: Literal[Dictionary]/None:           [{#(key)#}: {#(value)#}][#Dictionary<Key, Value>#];
}
