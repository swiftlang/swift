// RUN: %target-typecheck-verify-swift

protocol CustomStringConvertible {
  func print()
}

struct TestFormat {}

protocol FormattedPrintable : CustomStringConvertible { 
  func print(_: TestFormat)
}

struct IsPrintable1 : CustomStringConvertible {
  func print() {}
}

func accept_creates_Printable (_: () -> CustomStringConvertible) {}
func accept_creates_FormattedPrintable (_: () -> FormattedPrintable) {}

func fp_to_p(_ fp: FormattedPrintable) -> CustomStringConvertible { return fp; }
func p_to_fp(_ p: CustomStringConvertible) -> FormattedPrintable { }
func p_to_ip1(_ p: CustomStringConvertible) -> IsPrintable1 { }

func protocolConformance(ac1: @autoclosure () -> CustomStringConvertible,
                         ac2: @autoclosure () -> FormattedPrintable,
                         ip1: @autoclosure () -> IsPrintable1) {
  var f1 : (_ fp : FormattedPrintable) -> CustomStringConvertible = fp_to_p
  var f2 : (_ p : CustomStringConvertible) -> FormattedPrintable = p_to_fp
  let f3 : (_ p : CustomStringConvertible) -> IsPrintable1 = p_to_ip1

  // FIXME: closures make ABI conversions explicit. rdar://problem/19517003
  f1 = { f2($0) } // okay
  f1 = { f3($0) } // okay
  f2 = f1 // expected-error{{cannot assign value of type '(FormattedPrintable) -> CustomStringConvertible' to type '(CustomStringConvertible) -> FormattedPrintable'}}

  accept_creates_Printable(ac1)
  accept_creates_Printable({ ac2() })
  accept_creates_Printable({ ip1() })
  accept_creates_FormattedPrintable(ac1) // expected-error{{cannot convert value of type '() -> CustomStringConvertible' to expected argument type '() -> FormattedPrintable'}}
  accept_creates_FormattedPrintable(ac2)
  accept_creates_FormattedPrintable(ip1) // expected-error{{cannot convert value of type '() -> IsPrintable1' to expected argument type '() -> FormattedPrintable'}}
}

func p_gen_to_fp(_: () -> CustomStringConvertible) -> FormattedPrintable {}
func fp_gen_to_p(_: () -> FormattedPrintable) -> CustomStringConvertible {}

func nonTrivialNested() {
  // FIXME: closures make ABI conversions explicit. rdar://problem/19517003
  var f1 : (_ : () -> CustomStringConvertible) -> CustomStringConvertible = { p_gen_to_fp($0) }
  let f2 : (_ : () -> CustomStringConvertible) -> FormattedPrintable = p_gen_to_fp
  let f3 : (_ : () -> FormattedPrintable) -> CustomStringConvertible = fp_gen_to_p

  f1 = { f2($0) } // okay
  f1 = f3 // expected-error{{cannot assign value of type '(() -> FormattedPrintable) -> CustomStringConvertible' to type '(() -> CustomStringConvertible) -> CustomStringConvertible'}}
  _ = f1
}
