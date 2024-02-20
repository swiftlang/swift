// RUN: %target-swift-emit-irgen \
// RUN:     -enable-experimental-feature NoncopyableGenerics \
// RUN:     %s \
// RUN: | \
// RUN: %FileCheck %s

// CHECK-LABEL: define{{.*}} void @"$s21moveonly_enum_deinits4ListOwxx"(
// CHECK-SAME:      ptr noalias %object, 
// CHECK-SAME:      ptr %List)
// CHECK:       entry:
// CHECK:         br i1 {{%[^,]+}},
// CHECK-SAME:        label %[[EXIT:[^,]+]],
// CHECK-SAME:        label %[[PAYLOAD:[^,]+]]
// CHECK:       [[PAYLOAD]]:
// CHECK:         br label %[[EXIT]]
// CHECK:       [[EXIT]]:
// CHECK:         ret void
// CHECK:       }

struct Box<T : ~Copyable> : ~Copyable {
    public init(_ l: consuming T) {}
        
    deinit {}
}

enum List: ~Copyable {
    case end
    case more(Int, Box<List>)
}
