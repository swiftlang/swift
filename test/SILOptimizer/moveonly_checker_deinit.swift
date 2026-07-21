// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature MutateAndConsumeInDeinit %s | %FileCheck %s

// REQUIRES: swift_feature_MutateAndConsumeInDeinit

struct Inner1: ~Copyable {
  mutating func mutate() {}
  consuming func consume() {}
}

struct Inner2: ~Copyable {
  mutating func mutate() {}
  consuming func consume() {}
}

func borrow<T: ~Copyable>(_: borrowing T) {}
func mutate<T: ~Copyable>(_: inout T) {}
func consume<T: ~Copyable>(_: consuming T) {}

struct DoNothingDeinit: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  // CHECK-LABEL: sil{{.*}} @$s{{.*}}15DoNothingDeinitVfD

  // destroy i1:
  // CHECK:         [[I1:%.*]] = struct_element_addr [[SELF:%.*]], #DoNothingDeinit.i1
  // CHECK:         [[I1_ACCESS:%.*]] = begin_access [deinit] [static] [[I1]]
  // CHECK:         [[I1:%.*]] = struct_element_addr [[SELF:%.*]], #DoNothingDeinit.i1
  // CHECK:         destroy_addr [[I1]]
  // CHECK:         end_access [[I1_ACCESS]]

  // destroy i2:
  // CHECK:         [[I2:%.*]] = struct_element_addr [[SELF:%.*]], #DoNothingDeinit.i2
  // CHECK:         [[I2_ACCESS:%.*]] = begin_access [deinit] [static] [[I2]]
  // CHECK:         [[I2:%.*]] = struct_element_addr [[SELF:%.*]], #DoNothingDeinit.i2
  // CHECK:         destroy_addr [[I2]]
  // CHECK:         end_access [[I2_ACCESS]]
  deinit {}
}

struct ChangeOneField: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  // CHECK-LABEL: sil{{.*}} @$s{{.*}}14ChangeOneFieldVfD
  // CHECK:         [[MUTATE:%.*]] = function_ref @$s{{.*}}6mutate
  // CHECK:         apply [[MUTATE]]

  // destroy i1:
  // CHECK:         [[I1:%.*]] = struct_element_addr [[SELF:%.*]], #ChangeOneField.i1
  // CHECK:         [[I1_ACCESS:%.*]] = begin_access [deinit] [static] [[I1]]
  // CHECK:         [[I1:%.*]] = struct_element_addr [[SELF:%.*]], #ChangeOneField.i1
  // CHECK:         destroy_addr [[I1]]
  // CHECK:         end_access [[I1_ACCESS]]

  // destroy i2:
  // CHECK:         [[I2:%.*]] = struct_element_addr [[SELF:%.*]], #ChangeOneField.i2
  // CHECK:         [[I2_ACCESS:%.*]] = begin_access [deinit] [static] [[I2]]
  // CHECK:         [[I2:%.*]] = struct_element_addr [[SELF:%.*]], #ChangeOneField.i2
  // CHECK:         destroy_addr [[I2]]
  // CHECK:         end_access [[I2_ACCESS]]
  deinit {
    mutate(&i1)
  }
}

struct ConsumeOneField: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  // CHECK-LABEL: sil{{.*}} @$s{{.*}}15ConsumeOneFieldVfD
  // CHECK:         [[CONSUME:%.*]] = function_ref @$s{{.*}}7consume
  // CHECK:         apply [[CONSUME]]

  // i1 does not need to be destroyed
  // CHECK:         [[I1:%.*]] = struct_element_addr [[SELF:%.*]], #ConsumeOneField.i1
  // CHECK:         [[I1_ACCESS:%.*]] = begin_access [deinit] [static] [[I1]]
  // CHECK-NOT:     destroy_addr [[I1]]
  // CHECK:         end_access [[I1_ACCESS]]

  // destroy i2:
  // CHECK:         [[I2:%.*]] = struct_element_addr [[SELF:%.*]], #ConsumeOneField.i2
  // CHECK:         [[I2_ACCESS:%.*]] = begin_access [deinit] [static] [[I2]]
  // CHECK:         [[I2:%.*]] = struct_element_addr [[SELF:%.*]], #ConsumeOneField.i2
  // CHECK:         destroy_addr [[I2]]
  // CHECK:         end_access [[I2_ACCESS]]
  deinit {
    consume(i1)
  }
}

struct ConsumeBothFields: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  // CHECK-LABEL: sil{{.*}} @$s{{.*}}17ConsumeBothFieldsVfD
  // CHECK:         [[CONSUME:%.*]] = function_ref @$s{{.*}}7consume
  // CHECK:         apply [[CONSUME]]
  // CHECK:         [[CONSUME:%.*]] = function_ref @$s{{.*}}7consume
  // CHECK:         apply [[CONSUME]]

  // i1 does not need to be destroyed
  // CHECK:         [[I1:%.*]] = struct_element_addr [[SELF:%.*]], #ConsumeBothFields.i1
  // CHECK:         [[I1_ACCESS:%.*]] = begin_access [deinit] [static] [[I1]]
  // CHECK-NOT:     destroy_addr [[I1]]
  // CHECK:         end_access [[I1_ACCESS]]

  // i2 does not need to be destroyed
  // CHECK:         [[I2:%.*]] = struct_element_addr [[SELF:%.*]], #ConsumeBothFields.i2
  // CHECK:         [[I2_ACCESS:%.*]] = begin_access [deinit] [static] [[I2]]
  // CHECK-NOT:     destroy_addr [[I2]]
  // CHECK:         end_access [[I2_ACCESS]]
  deinit {
    consume(i1)
    consume(i2)
  }
}
