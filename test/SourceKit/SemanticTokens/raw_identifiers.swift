// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=semantic-tokens %t/main.swift -- %t/main.swift > %t/result.json
// RUN: diff -u %t/tokens.json %t/result.json

//--- main.swift
func `foo`(x: Int) {}
`foo`(x: 0)

func `foo bar baz`() {}
`foo bar baz`()

//--- tokens.json
{
  key.semantic_tokens: [
    {
      key.kind: source.lang.swift.ref.struct,
      key.offset: 14,
      key.length: 3,
      key.is_system: 1
    },
    {
      key.kind: source.lang.swift.ref.function.free,
      key.offset: 22,
      key.length: 5
    },
    {
      key.kind: source.lang.swift.ref.function.free,
      key.offset: 59,
      key.length: 13
    }
  ]
}
