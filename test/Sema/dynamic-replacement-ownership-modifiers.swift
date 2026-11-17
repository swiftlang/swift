// RUN: %target-swift-frontend -typecheck -verify %s

struct Butt {
  dynamic init(originalConsumesImplicitly _: String) { }
  dynamic init(originalConsumesExplicitly _: consuming String) { }
  dynamic init(originalBorrowsExplicitly _: borrowing String) { }
}

dynamic func originalBorrowsImplicitly(_: String) { }
dynamic func originalBorrowsExplicitly(_: borrowing String) { }
dynamic func originalConsumesExplicitly(_: consuming String) { }

// OK if replacement matches original exactly.

extension Butt {
  @_dynamicReplacement(for: init(originalConsumesImplicitly:))
  init(originalConsumesImplicitly_replacementConsumesImplicitly _: String) {}
  @_dynamicReplacement(for: init(originalConsumesExplicitly:))
  init(originalConsumesExplicitly_replacementConsumesExplicitly _: consuming String) {}
  @_dynamicReplacement(for: init(originalBorrowsExplicitly:))
  init(originalBorrowsExplicitly_replacementBorrowsExplicitly _: borrowing String) {}
}

@_dynamicReplacement(for: originalBorrowsImplicitly(_:))
func originalBorrowsImplicitly_replacementBorrowsImplicitly(_ x: String) {}
@_dynamicReplacement(for: originalBorrowsExplicitly(_:))
func originalBorrowsExplicitly_replacementBorrowsExplicitly(_ x: borrowing String) {}
@_dynamicReplacement(for: originalConsumesExplicitly(_:))
func originalConsumesExplicitly_replacementConsumesExplicitly(_ x: consuming String) {}

// OK if implicit ownership of original matches explicit ownership of
// replacement, or vice versa.
extension Butt {
  @_dynamicReplacement(for: init(originalConsumesImplicitly:))
  init(originalConsumesImplicitly_replacementConsumesExplicitly _: consuming String) {}
  @_dynamicReplacement(for: init(originalConsumesExplicitly:))
  init(originalConsumesExplicitly_replacementConsumesImplicitly _: String) {}
}

@_dynamicReplacement(for: originalBorrowsImplicitly(_:))
func originalBorrowsImplicitly_replacementBorrowsExplicitly(_ x: borrowing String) {}
@_dynamicReplacement(for: originalBorrowsExplicitly(_:))
func originalBorrowsExplicitly_replacementBorrowsImplicitly(_ x: String) {}

// Error if implicit ownership of original doesn't match the explicit ownership
// of the replacement (or vice versa).
extension Butt {
  // expected-error @+1 {{could not be found}}
  @_dynamicReplacement(for: init(originalConsumesImplicitly:))
  init(originalConsumesImplicitly_replacementBorrowsExplicitly _: borrowing String) {}
  // expected-error @+1 {{could not be found}}
  @_dynamicReplacement(for: init(originalBorrowsExplicitly:))
  init(originalBorrowsExplicitly_replacementConsumesImplicitly _: String) {}
}

// expected-error @+1 {{could not be found}}
@_dynamicReplacement(for: originalBorrowsImplicitly(_:))
func originalBorrowsImplicitly_replacementConsumesExplicitly(_ x: consuming String) {}
// expected-error @+1 {{could not be found}}
@_dynamicReplacement(for: originalConsumesExplicitly(_:))
func originalConsumesExplicitly_replacementBorrowsImplicitly(_ x: String) {}

// Error if explicit ownership of original doesn't match the explicit ownership
// of the replacement.
extension Butt {
  // expected-error @+1 {{could not be found}}
  @_dynamicReplacement(for: init(originalConsumesExplicitly:))
  init(originalConsumesExplicitly_replacementBorrowsExplicitly _: borrowing String) {}
  // expected-error @+1 {{could not be found}}
  @_dynamicReplacement(for: init(originalBorrowsExplicitly:))
  init(originalBorrowsExplicitly_replacementConsumesExplicitly _: consuming String) {}
}

// expected-error @+1 {{could not be found}}
@_dynamicReplacement(for: originalBorrowsExplicitly(_:))
func originalBorrowsExplicitly_replacementConsumesExplicitly(_ x: consuming String) {}
// expected-error @+1 {{could not be found}}
@_dynamicReplacement(for: originalConsumesExplicitly(_:))
func originalConsumesExplicitly_replacementBorrowsExplicitly(_ x: borrowing String) {}
