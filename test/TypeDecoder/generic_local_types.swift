// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/generic_local_types -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/generic_local_types -type-from-mangled=%t/input | %FileCheck %s

func blackHole(_: Any...) {}

class Generic<T> {
  // Initializer -> closure
  var x1: Int = {
    typealias Alias1 = Int
    let a: Alias1 = 0
    return a
  }()

  // Implicit getter
  var x2: Int {
    typealias Alias2 = Int
    let a: Alias2 = 0
    return a
  }

  // Observers
  var x3: Int = 0 {
    didSet {
      typealias Alias3 = Int
      let a: Alias3 = 0
      blackHole(a)
    }
    willSet {
      typealias Alias4 = Int
      let a: Alias4 = 0
      blackHole(a)
    }
  }

  // Getter and setter
  var x4: Int {
    get {
      typealias Alias5 = Int
      let a: Alias5 = 0
      return a
    }
    set {
      typealias Alias6 = Int
      let a: Alias6 = 0
      blackHole(a)
    }
  }

  // Read and modify
  var x5: Int {
    _read {
      typealias Alias7 = Int
      let a: Alias7 = 0
      yield a
    }
    _modify {
      typealias Alias8 = Int
      var a: Alias8 = 0
      yield &a
    }
  }

  // Subscript implicit getter
  subscript(x x: Int) -> Int {
    typealias Alias9 = Int
    let a: Alias9 = 0
    return a
  }

  // Subscript getter and setter
  subscript(y y: Int) -> Int {
    get {
      typealias Alias10 = Int
      let a: Alias10 = 0
      return a
    }
    set {
      typealias Alias11 = Int
      let a: Alias11 = 0
      blackHole(a)
    }
  }

  // Function -> default argument -> closure
  func method(_: Int = {
    typealias Alias12 = Int
    let a: Alias12 = 0
    return a
  }()) {
    // Function
    typealias Alias13 = Int
    let a: Alias13 = 0

    blackHole(a)

    // Function -> function
    func nested() {
      typealias Alias = Int
      let a: Alias = 0

      blackHole(a)
    }
  }

  // Constructor
  init() {
    typealias Alias14 = Int
    let a: Alias14 = 0

    blackHole(a)
  }

  // Destructor
  deinit {
    typealias Alias15 = Int
    let a: Alias15 = 0

    blackHole(a)
  }
}

// DEMANGLE: $s19generic_local_types7GenericC2x1SivpfiSiyXEfU_6Alias1L_ayx_GD
// DEMANGLE: $s19generic_local_types7GenericC2x2Sivg6Alias2L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC2x3SivW6Alias3L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC2x3Sivw6Alias4L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC2x4Sivg6Alias5L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC2x4Sivs6Alias6L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC2x5Sivr6Alias7L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC2x5SivM6Alias8L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC1xS2i_tcig6Alias9L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC1yS2i_tcig7Alias10L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC1yS2i_tcis7Alias11L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC6methodyySiFfA_SiycfU_7Alias12L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericC6methodyySiF7Alias13L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericCACyxGycfc7Alias14L_ayx__GD
// DEMANGLE: $s19generic_local_types7GenericCfd7Alias15L_ayx__GD

// CHECK: Alias1
// CHECK: Alias2
// CHECK: Alias3
// CHECK: Alias4
// CHECK: Alias5
// CHECK: Alias6
// CHECK: Alias7
// CHECK: Alias8
// CHECK: Alias9
// CHECK: Alias10
// CHECK: Alias11
// CHECK: Alias12
// CHECK: Alias13
// CHECK: Alias14
// CHECK: Alias15