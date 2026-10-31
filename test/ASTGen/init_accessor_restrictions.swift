// The names in '@storageRestrictions(initializes: a, b, accesses: c)' continue
// across unlabeled arguments, so 'b' is part of 'initializes'.

// RUN: %target-typecheck-verify-swift -enable-experimental-feature InitAccessors
// RUN: %target-typecheck-verify-swift -enable-experimental-feature InitAccessors \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_InitAccessors

struct S {
  var x: Int
  var y: Int
  var full: (Int, Int)

  var point: (Int, Int) {
    @storageRestrictions(initializes: y, full, accesses: x)
    init(initialValue) {
      self.y = initialValue.1
      self.full = (self.x, self.y)
    }
    get { full }
    set { full = newValue }
  }

  init(x: Int, y: Int) {
    self.x = x
    self.point = (x, y)
  }
}
