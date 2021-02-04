// RUN: %target-swift-frontend %s -emit-ir -g -o - -parse-as-library \
// RUN:    -module-name M -enable-experimental-concurrency | %FileCheck %s
// REQUIRES: concurrency

extension Collection {
  public func f() async throws {
    return await try Task.withGroup(resultType: Element.self) { group in
      var i = self.startIndex
      func doit() async throws {
        await group.add { [i] in
          return self[i]
        }
      }
      await try doit()
    }
  }
}

// CHECK: ![[BOXTY:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s5IndexSlQzz_x_SlRzlXXD"
// CHECK: !DILocalVariable(name: "i", arg: 3, {{.*}}type: ![[BOXTY]]
