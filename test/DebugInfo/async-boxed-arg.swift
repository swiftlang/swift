// RUN: %target-swift-frontend %s -emit-ir -g -o - -parse-as-library \
// RUN:    -module-name M -enable-experimental-concurrency | %FileCheck %s
// REQUIRES: concurrency

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Collection {
  public func f() async throws {
    return await try withTaskGroup(of: Element.self) { group in
      var i = self.startIndex
      func doit() async throws {
        await group.spawn { [i] in
          return self[i]
        }
      }
      await try doit()
    }
  }
}

// CHECK: ![[BOXTY:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s5IndexSlQzz_x_SlRzlXXD"
// CHECK: !DILocalVariable(name: "i", arg: 3, {{.*}}type: ![[BOXTY]]
