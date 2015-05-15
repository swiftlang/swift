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

  switch getColorOptions() {
  case ColorOptions.Pastel: break
  case ColorOptions.Swift: break
  } // expected-error {{switch must be exhaustive, consider adding a default clause}}
  
  switch 5 as Int16 {
  case Zero: break // no-error
  } // expected-error {{switch must be exhaustive, consider adding a default clause}}
}

