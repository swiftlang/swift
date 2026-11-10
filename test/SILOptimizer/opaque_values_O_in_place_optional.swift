// RUN: %target-swift-frontend -sil-verify-all -enable-sil-opaque-values -enforce-exclusivity=unchecked -parse-as-library -emit-sil -O %s | %FileCheck %s

public enum Terminal<Failure: Error> {
  case finished
  case failed(Failure)
}

public enum YieldResult<Failure: Error> {
  case enqueued
  case dropped(Failure)
  case terminated
}

public struct State<Failure: Error> {
  var terminal: Terminal<Failure>?
}

@inline(never) func resumeFinished() {}
@inline(never) func resumeFailed<E: Error>(_ error: E) {}

public final class Storage<Failure: Error> {
  var state = State<Failure>()

  // Reading and replacing `terminal` work on the field in place.
  // CHECK-LABEL: sil [noinline] @$s{{.*}}7StorageC4nextSiyF : {{.*}} {
  // CHECK-NOT:     alloc_stack {{.*}}$Optional<Terminal<Failure>>
  // CHECK:         [[FIELD:%[^ ]+]] = struct_element_addr {{%[^ ]+}}, #State.terminal
  // CHECK-NOT:     alloc_stack {{.*}}$Optional<Terminal<Failure>>
  // CHECK:         switch_enum_addr [[FIELD]], case #Optional.some!enumelt
  // CHECK-NOT:     alloc_stack {{.*}}$Optional<Terminal<Failure>>
  // CHECK:         init_enum_data_addr [[FIELD]], #Optional.some!enumelt
  // CHECK-NOT:     alloc_stack {{.*}}$Optional<Terminal<Failure>>
  // CHECK-LABEL: } // end sil function '$s{{.*}}7StorageC4nextSiyF'
  @inline(never)
  public func next() -> Int {
    if let terminal = state.terminal {
      state.terminal = .finished
      switch terminal {
      case .finished: return 0
      case .failed: return -1
      }
    }
    return 2
  }

  // `.terminated` is written into the result in place, though `terminal` is
  // still live after the block.
  // CHECK-LABEL: sil [noinline] @$s{{.*}}7StorageC5yield{{.*}} : {{.*}} {
  // CHECK:       bb0(%0 : $*YieldResult<Failure>,
  // CHECK-NOT:     alloc_stack {{.*}}$YieldResult<Failure>
  // CHECK:         inject_enum_addr %0, #YieldResult.terminated!enumelt
  // CHECK-NOT:     alloc_stack {{.*}}$YieldResult<Failure>
  // CHECK-LABEL: } // end sil function '$s{{.*}}7StorageC5yield{{.*}}'
  @inline(never)
  public func yield() -> YieldResult<Failure> {
    var result: YieldResult<Failure>
    if let terminal = state.terminal {
      result = .terminated
      state.terminal = .finished
      switch terminal {
      case .finished: resumeFinished()
      case .failed(let error): resumeFailed(error)
      }
    } else {
      result = .enqueued
    }
    return result
  }
}
