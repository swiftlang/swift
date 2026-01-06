// RUN: %target-typecheck-verify-swift

do {
  struct Mask : OptionSet {
    init(_ rawValue: UInt) {}
    init(rawValue: UInt) {}

    var rawValue: UInt { 0 }

    static var Bingo: Mask { Mask(1) }
  }

  // FIXME: No fix-it. Could be '= Mask(rawValue: 0)' or '= []'.
  let _: Mask = 0
  // expected-error@-1:17 {{cannot convert value of type 'Int' to specified type 'Mask'}}{{none}}
}

do {
  enum E: Int {
    case a
  }

  // TODO: A better fix-it is to remove the init call.
  _ = E(E.a)
  // expected-error@-1:9 {{missing argument label 'rawValue:' in call}}{{9-9=rawValue: }}
  // expected-error@-2:11 {{cannot convert value of type 'E' to expected argument type 'Int'}}{{12-12=.rawValue}}
}

// Fix-its for expectation of implicit RawRepresentable <-> RawValue cast.
do {
  struct Mask : OptionSet {
    init(rawValue: UInt64) {}
    var rawValue: UInt64 { return 0 }
  }

  // RawValue -> RawRepresentable / OptionSet.
  do {
    struct Mask : OptionSet {
      init(rawValue: UInt64) {}
      var rawValue: UInt64 { return 0 }
    }

    func takeMask(_: Mask) {}
    func takeMask_o(_: Mask?) {}

    let int: Int
    let int_o: Int?
    let uint64: UInt64
    let uint64_o: UInt64?

    // FIXME: No fix-it. Could be 'Mask(rawValue: 1)'.
    let _: Mask = 1
    // expected-error@-1:19 {{cannot convert value of type 'Int' to specified type 'Mask'}}{{none}}
    takeMask(1)
    // expected-error@-1:14 {{cannot convert value of type 'Int' to expected argument type 'Mask'}}{{none}}

    // FIXME: No fix-it. Could be 'Mask(rawValue: UInt64(int))'.
    let _: Mask = int
    // expected-error@-1:19 {{cannot convert value of type 'Int' to specified type 'Mask'}}{{none}}
    takeMask(int)
    // expected-error@-1:14 {{cannot convert value of type 'Int' to expected argument type 'Mask'}}{{none}}

    let _: Mask = uint64
    // expected-error@-1:19 {{cannot convert value of type 'UInt64' to specified type 'Mask'}}{{19-19=Mask(rawValue: }}{{25-25=) ?? <#default value#>}}
    takeMask(uint64)
    // expected-error@-1:14 {{cannot convert value of type 'UInt64' to expected argument type 'Mask'}}{{14-14=Mask(rawValue: }}{{20-20=) ?? <#default value#>}}

    // FIXME: No fix-it. Could be 'Mask(rawValue: UInt64(int_o ?? <#default value#>)'.
    let _: Mask = int_o
    // expected-error@-1:19 {{cannot convert value of type 'Int?' to specified type 'Mask'}}{{none}}
    takeMask(int_o)
    // expected-error@-1:14 {{cannot convert value of type 'Int?' to expected argument type 'Mask'}}{{none}}

    // FIXME: No fix-it. Could be 'Mask(rawValue: 1)'.
    let _: Mask? = 1
    // expected-error@-1:20 {{cannot convert value of type 'Int' to specified type 'Mask'}}{{none}}
    takeMask_o(1)
    // expected-error@-1:16 {{cannot convert value of type 'Int' to expected argument type 'Mask'}}{{none}}

    // FIXME: No fix-it. Could be 'Mask(rawValue: UInt64(int))'.
    let _: Mask? = int
    // expected-error@-1:20 {{cannot convert value of type 'Int' to specified type 'Mask'}}{{none}}
    takeMask_o(int)
    // expected-error@-1:16 {{cannot convert value of type 'Int' to expected argument type 'Mask'}}{{none}}

    // FIXME: No fix-it. Could be 'int_o.map { Mask(rawValue: UInt64($0)) }'.
    let _: Mask? = int_o
    // expected-error@-1:20 {{cannot assign value of type 'Int?' to type 'Mask?'}}{{none}}
    // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('Int' and 'Mask') are expected to be equal}}
    takeMask_o(int_o)
    // expected-error@-1:16 {{cannot convert value of type 'Int?' to expected argument type 'Mask?'}}{{none}}
    // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('Int' and 'Mask') are expected to be equal}}

    // FIXME: No fix-it. Could be 'uint64_o.map(Mask(rawValue:))' or 'uint64_o.map { Mask(rawValue: $0) }'.
    let _: Mask? = uint64_o
    // expected-error@-1:20 {{cannot assign value of type 'UInt64?' to type 'Mask?'}}{{none}}
    // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('UInt64' and 'Mask') are expected to be equal}}
    takeMask_o(uint64_o)
    // expected-error@-1:16 {{cannot convert value of type 'UInt64?' to expected argument type 'Mask?'}}{{none}}
    // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('UInt64' and 'Mask') are expected to be equal}}

    // FIXME: No fix-it. Could be '(anything as? Int).map { Mask(rawValue: UInt64($0)) }'.
    let anything: Any
    let _: Mask? = anything as? Int
    // expected-error@-1:29 {{cannot assign value of type 'Int?' to type 'Mask?'}}{{none}}
    // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('Int' and 'Mask') are expected to be equal}}
    takeMask_o(anything as? Int)
    // expected-error@-1:25 {{cannot convert value of type 'Int?' to expected argument type 'Mask?'}}{{none}}
    // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('Int' and 'Mask') are expected to be equal}}
  }

  // Try a nested OptionSet.
  do {
    struct Shell {
      struct Mask : OptionSet {
        init(rawValue: UInt64) {}
        var rawValue: UInt64 { return 0 }
      }
    }

    // FIXME: This is an OptionSet, coelescing in the fix-it is not necessary.
    let _: Shell.Mask = UInt64(0)
    // expected-error@-1:25 {{cannot convert value of type 'UInt64' to specified type 'Shell.Mask'}}{{25-25=Shell.Mask(rawValue: }}{{34-34=) ?? <#default value#>}}
  }

  // Try a non-integral RawValue.
  do {
    struct Mask : RawRepresentable {
      init(rawValue: String) {}
      var rawValue: String { return "" }
    }

    let s: String
    let _: Mask = "\(s)}"
    // expected-error@-1:19 {{cannot convert value of type 'String' to specified type 'Mask'}}{{19-19=Mask(rawValue: }}{{26-26=) ?? <#default value#>}}
  }

  // RawRepresentable / OptionSet -> RawValue.
  do {
    struct Mask : OptionSet {
      init(rawValue: UInt64) {}
      var rawValue: UInt64 { return 0 }
    }

    func takeInt(_: Int) {}
    func takeInt_o(_: Int?) {}
    func takeUInt64(_: UInt64) {}
    func takeUInt64_o(_: UInt64?) {}

    let mask: Mask
    let mask_o: Mask?

    // FIXME: No fix-it. Could be 'Int(mask.rawValue)'.
    let _: Int = mask
    // expected-error@-1:18 {{cannot convert value of type 'Mask' to specified type 'Int'}}{{none}}
    takeInt(mask)
    // expected-error@-1:13 {{cannot convert value of type 'Mask' to expected argument type 'Int'}}{{none}}

    let _: UInt64 = mask
    // expected-error@-1:21 {{cannot convert value of type 'Mask' to specified type 'UInt64'}}{{25-25=.rawValue}}
    takeUInt64(mask)
    // expected-error@-1:16 {{cannot convert value of type 'Mask' to expected argument type 'UInt64'}}{{20-20=.rawValue}}

    let _: UInt64 = mask_o
    // expected-error@-1:21 {{cannot convert value of type 'Mask?' to specified type 'UInt64'}}{{27-27=?.rawValue ?? <#default value#>}}
    takeUInt64(mask_o)
    // expected-error@-1:16 {{cannot convert value of type 'Mask?' to expected argument type 'UInt64'}}{{22-22=?.rawValue ?? <#default value#>}}

    // FIXME: No fix-it. Could be 'mask_o.map { Int($0.rawValue) }'.
    let _: Int? = mask_o
    // expected-error@-1:19 {{cannot assign value of type 'Mask?' to type 'Int?'}}{{none}}
    // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('Mask' and 'Int') are expected to be equal}}
    takeInt_o(mask_o)
    // expected-error@-1:15 {{cannot convert value of type 'Mask?' to expected argument type 'Int?'}}{{none}}
    // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('Mask' and 'Int') are expected to be equal}}

    // FIXME: No fix-it. Could be 'mask_o?.rawValue' or 'mask_o.map { $0.rawValue }'.
    let _: UInt64? = mask_o
    // expected-error@-1:22 {{cannot convert value of type 'Mask?' to specified type 'UInt64?'}}{{28-28=?.rawValue}}
    takeUInt64_o(mask_o)
    // expected-error@-1:18 {{cannot convert value of type 'Mask?' to expected argument type 'UInt64?'}}{{none}}
    // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('Mask' and 'UInt64') are expected to be equal}}
  }
}

do {
  class Class {}
  class SubClass: Class {}
  struct ClassWrapper : RawRepresentable {
    var rawValue: Class
  }

  func takeAnyObject(_: AnyObject) {}
  func takeAnyObjectOpt(_: AnyObject?) {}
  func takeSubClass(_: SubClass) {}

  do {
    let classWrapper: ClassWrapper
    let subClass: SubClass

    takeAnyObject(classWrapper)
    // expected-error@-1:19 {{argument type 'ClassWrapper' expected to be an instance of a class or class-constrained type}}{{none}}
    takeAnyObjectOpt(classWrapper)
    // expected-error@-1:22 {{argument type 'ClassWrapper' expected to be an instance of a class or class-constrained type}}{{none}}

    // FIXME: Bad fix-it, will not compile
    takeSubClass(classWrapper)
    // expected-error@-1:18 {{cannot convert value of type 'ClassWrapper' to expected argument type 'SubClass'}}{{30-30=.rawValue}}

    let iuo: ClassWrapper!
    takeAnyObject(iuo)
    // expected-error@-1:19 {{argument type 'ClassWrapper?' expected to be an instance of a class or class-constrained type}}{{none}}
    takeAnyObjectOpt(iuo)
    // expected-error@-1:22 {{argument type 'ClassWrapper?' expected to be an instance of a class or class-constrained type}}{{none}}

    let _: ClassWrapper = subClass
    // expected-error@-1:27 {{cannot convert value of type 'SubClass' to specified type 'ClassWrapper'}}{{27-27=ClassWrapper(rawValue: }} {{35-35=) ?? <#default value#>}}
    let _: ClassWrapper = classWrapper.rawValue
    // expected-error@-1:40 {{cannot convert value of type 'Class' to specified type 'ClassWrapper'}}{{27-27=ClassWrapper(rawValue: }} {{48-48=) ?? <#default value#>}}

    // FIXME: This note replaces 'as' with 'as!', which is incorrect.
    // expected-note@+3:36 {{did you mean to use 'as!' to force downcast?}}{{36-38=as!}}
    // FIXME: Bad fix-it, will not compile
    // expected-error@+1:36 {{cannot convert value of type 'AnyObject' to specified type 'ClassWrapper'}}{{27-27=ClassWrapper(rawValue: }} {{48-48=) ?? <#default value#>}}
    let _: ClassWrapper = subClass as AnyObject
    // expected-error@-1:36 {{'AnyObject' is not convertible to 'Class'}}{{none}}
  }
}


