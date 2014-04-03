// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -enable-source-import -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc %s
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -parse-as-library %t/comments.swiftmodule -parse -emit-objc-header-path %t/comments.h
// RUN: sed -n -e '/A000/,$ p' %t/comments.h > %t/comments.h-cleaned
// RUN: diff %t/comments.h-cleaned %S/Inputs/comments-expected-output.h
// RUN: %check-in-clang %t/comments.h

@objc class A000 {}

// CHECK-LABEL: /// Aaa.  A1.  Bbb.
// CHECK-NEXT: @interface A1{{$}}
/// Aaa.  A1.  Bbb.
@objc class A1 {
  ///
  func f0() {}

  /// Aaa.
  func f1() {}

  /** */
  func f2() {}

  /**
   */
  func f3() {}

  /**
   * Aaa.
   */
  func f4() {}

  /// Aaa.  f5.
  ///
  /// :param: first Bbb.
  ///
  /// :param: second Ccc.  Ddd.
  ///   Eee.
  func f5(first: Int, second: Double) {}

  /// Aaa.  f6.
  ///
  /// :param: first Bbb.
  ///
  /// :returns: Ccc.
  ///   Ddd.
  func f6(first: Int) {}

  /// Aaa.  f7.
  ///
  /// :returns: Ccc.
  ///   Ddd.
  ///
  /// :returns: Eee.
  ///   Fff.
  func f7() {}

  /// Aaa.  init().
  init() {}

  /// Aaa.  subscript(i: Int).
  subscript(i: Int) -> Int {
    get {
      return 0
    }
    set {}
  }

  /// Aaa.  v1.
  var v1: Int = 0
}

