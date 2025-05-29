// RUN: %target-swift-emit-irgen                             \
// RUN:     -disable-type-layout                             \
// RUN:     %s                                               \
// RUN: |                                                    \
// RUN: %FileCheck %s

// Check that UMA_Large.deinit is called directly from FIFO_Large.deinit,
// rather than through an outlined release function.
// CHECK-LABEL: define{{.*}} void @"$s21moveonly_enum_deinits10FIFO_LargeVfD"
// CHECK:         call{{.*}} void @"$s21moveonly_enum_deinits9UMA_LargeVfD"
// CHECK:       }
public struct FIFO_Large<T>: ~Copyable {
  public var uma: UMA_Large<T>
  public var i1: Int
  public var i2: Int

  deinit {
    something(self)
  }
}

public struct UMA_Large<T>: ~Copyable {
  public let umbp: UnsafeMutableBufferPointer<T>
  public var allocd: [Bool] = []
  deinit {
    something(self)
  }
}

// Check that UMA_Small.deinit is called directly from FIFO_Small.deinit,
// rather than through an outlined release function.
// CHECK-LABEL: define{{.*}} void @"$s21moveonly_enum_deinits10FIFO_SmallVfD"
// CHECK:         call{{.*}} void @"$s21moveonly_enum_deinits9UMA_SmallVfD"
// CHECK:       }
public struct FIFO_Small<T>: ~Copyable {
  public var uma: UMA_Small<T>
  public var i1: Int
  public var i2: Int

  deinit {
    something(self)
  }
}

public struct UMA_Small<T>: ~Copyable {
  public var allocd: [Bool] = []
  deinit {
    something(self)
  }
}

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

public struct Box<T : ~Copyable> : ~Copyable {
  public init(_ l: consuming T) {}
      
  deinit {}
}

public enum List: ~Copyable {
  case end
  case more(Int, Box<List>)
}

@_silgen_name("something")
func something<T : ~Copyable>(_ t: borrowing T)
