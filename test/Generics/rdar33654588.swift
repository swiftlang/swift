// RUN: %target-typecheck-verify-swift 2>&1

protocol FP {}

protocol FPSign {
  associatedtype Positive: FPIntDigit
  associatedtype Negative: FPIntDigit
}

protocol FPIntDigit: FP {
  associatedtype Zero:  FPIntDigit, FPPoint, FPExponent
  associatedtype One:   FPIntDigit, FPPoint, FPExponent
  associatedtype Two:   FPIntDigit, FPPoint, FPExponent
  associatedtype Three: FPIntDigit, FPPoint, FPExponent
  associatedtype Four:  FPIntDigit, FPPoint, FPExponent
  associatedtype Five:  FPIntDigit, FPPoint, FPExponent
  associatedtype Six:   FPIntDigit, FPPoint, FPExponent
  associatedtype Seven: FPIntDigit, FPPoint, FPExponent
  associatedtype Eight: FPIntDigit, FPPoint, FPExponent
  associatedtype Nine:  FPIntDigit, FPPoint, FPExponent
}

protocol FPPoint: FP {
  associatedtype Point: FPFractionDigit, FPExponent
}

protocol FPFractionDigit: FP {
  associatedtype Zero:  FPFractionDigit, FPExponent
  associatedtype One:   FPFractionDigit, FPExponent
  associatedtype Two:   FPFractionDigit, FPExponent
  associatedtype Three: FPFractionDigit, FPExponent
  associatedtype Four:  FPFractionDigit, FPExponent
  associatedtype Five:  FPFractionDigit, FPExponent
  associatedtype Six:   FPFractionDigit, FPExponent
  associatedtype Seven: FPFractionDigit, FPExponent
  associatedtype Eight: FPFractionDigit, FPExponent
  associatedtype Nine:  FPFractionDigit, FPExponent
}

protocol FPExponent {
  associatedtype E: FPExponentSign, FPExponentDigit
}

protocol FPExponentSign {
  associatedtype Positive: FPExponentDigit
  associatedtype Negative: FPExponentDigit
}

protocol FPExponentDigit: FP {
  associatedtype Zero:  FPExponentDigit
  associatedtype One:   FPExponentDigit
  associatedtype Two:   FPExponentDigit
  associatedtype Three: FPExponentDigit
  associatedtype Four:  FPExponentDigit
  associatedtype Five:  FPExponentDigit
  associatedtype Six:   FPExponentDigit
  associatedtype Seven: FPExponentDigit
  associatedtype Eight: FPExponentDigit
  associatedtype Nine:  FPExponentDigit
}

