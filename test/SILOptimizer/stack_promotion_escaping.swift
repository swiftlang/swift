// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -parse-as-library -O -module-name=test %s -emit-sil | %FileCheck %s

// FIXME: https://bugs.swift.org/browse/SR-2808
// XFAIL: resilient_stdlib

final class Item {}

final public class Escaper {
  var myItem: Item = Item()

  @inline(never)
  func update(items: [Item]) {
    myItem = items[0]
  }

// CHECK-LABEL: sil [noinline] @_T04test7EscaperC15badStuffHappensyyF : $@convention(method) (@guaranteed Escaper) -> () {
// CHECK: %2 = alloc_ref $Item
// CHECK: alloc_ref [stack] [tail_elems $Item * %{{[0-9]+}} : $Builtin.Word] $_ContiguousArrayStorage<Item>
// CHECK: return
  @inline(never)
  public func badStuffHappens() {
    // Check that 'item' is not stack promoted, because it escapes to myItem.
    let item = Item()
    // On the other hand, the array buffer of the array literal should be stack promoted.
    update(items:[item])
  }
}

