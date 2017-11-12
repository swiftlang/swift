// Note that for the test to be effective, each of these structs must only have
// its Equatable or Hashable conformance referenced /once/ in the primary file.
struct FromOtherFile: Hashable {
  let v: String
}
struct AlsoFromOtherFile: Hashable {
  let v: Int
}
struct YetAnotherFromOtherFile: Hashable {
  let v: Float
}

struct OtherFileNonconforming {
  let v: String
}
struct YetOtherFileNonconforming {
  let v: String
}
