// RUN: %target-swift-frontend -emit-sil %s -import-objc-header %S/Inputs/enum-new.h -verify
// REQUIRES: OS=macosx

_ = .Red as Color
_ = .Cyan as MoreColor

func test() {
  switch getColor() {
  case .Red, .Blue, .Green: break
  } // no-error

  switch getMoreColor() {
  case .Yellow, .Magenta, .Black, .Cyan: break
  } // no-error

  switch getColorOptions() { // expected-error {{switch must be exhaustive, consider adding a default clause}}
  case ColorOptions.Pastel: break
  case ColorOptions.Swift: break
  }
  
  switch 5 as Int16 { // expected-error {{switch must be exhaustive, consider adding a default clause}}
  case Zero: break // no-error
  }
}
