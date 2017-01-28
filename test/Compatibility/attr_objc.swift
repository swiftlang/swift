// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s -swift-version 3

class Load1 {
  class func load() { }
  class func alloc() {}
  class func allocWithZone(_: Int) {}
  class func initialize() {}
}

@objc class Load2 {
  class func load() { } // expected-error{{method 'load()' defines Objective-C class method 'load', which is not permitted by Swift}}
  class func alloc() {} // expected-error{{method 'alloc()' defines Objective-C class method 'alloc', which is not permitted by Swift}}
  class func allocWithZone(_: Int) {} // expected-error{{method 'allocWithZone' defines Objective-C class method 'allocWithZone:', which is not permitted by Swift}}
  class func initialize() {} // expected-warning{{method 'initialize()' defines Objective-C class method 'initialize', which is not guaranteed to be invoked by Swift and will be disallowed in future versions}}
}

@objc class Load3 {
  class var load: Load3 {
    get { return Load3() } // expected-error{{getter for 'load' defines Objective-C class method 'load', which is not permitted by Swift}}
    set { }
  }

  @objc(alloc) class var prop: Int { return 0 } // expected-error{{getter for 'prop' defines Objective-C class method 'alloc', which is not permitted by Swift}}
  @objc(allocWithZone:) class func fooWithZone(_: Int) {} // expected-error{{method 'fooWithZone' defines Objective-C class method 'allocWithZone:', which is not permitted by Swift}}
  @objc(initialize) class func barnitialize() {} // expected-warning{{method 'barnitialize()' defines Objective-C class method 'initialize', which is not guaranteed to be invoked by Swift and will be disallowed in future versions}}
}


