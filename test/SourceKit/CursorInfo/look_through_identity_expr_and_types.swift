func lookThroughFunctionConversions() {
  class DispatchQueue {
    class var main: DispatchQueue { get }
    func async(execute work: @escaping @convention(block) () -> Void)
  }

  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):22 %s -- %s | %FileCheck %s --check-prefix=FUNCTION_CONVERSION
  DispatchQueue.main.async {}
}

// FUNCTION_CONVERSION: (DispatchQueue) -> (@escaping @convention(block) () -> ()) -> ()
// FUNCTION_CONVERSION: DYNAMIC

func lookThorughInout() {
  public struct Villager {}
  let villager = Villager()

  var villagers: [Villager] = []
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):13 %s -- %s | %FileCheck %s
  villagers.removeAll(where: { _ in true })
}

// CHECK: RECEIVERS BEGIN
// CHECK: s:Sa
// CHECK: RECEIVERS END

