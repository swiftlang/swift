func foo() {
  let _ = NoRawTypeKey.a.stringValue
  let _ = NoRawTypeKey(stringValue: "a")
  let _ = NoRawTypeKey.a.intValue
  let _ = NoRawTypeKey(intValue: 0)

  let _ = StringKey.a.stringValue
  let _ = StringKey(stringValue: "A")
  let _ = StringKey.a.intValue
  let _ = StringKey(intValue: 0)

  let _ = IntKey.a.stringValue
  let _ = IntKey(stringValue: "a")
  let _ = IntKey.a.intValue
  let _ = IntKey(intValue: 3)
}
