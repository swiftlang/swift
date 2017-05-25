// RUN: %target-swift-frontend -swift-version 3 -emit-silgen %s | %FileCheck %s

enum Downgradable {
  case spoon
  case hat
  @_downgrade_exhaustivity_check
  case fork
}

// CHECK-LABEL: sil hidden @_T029downgrade_exhaustivity_swift343testDowngradableOmittedPatternIsUnreachableyAA0E0OSg3pat_tF
func testDowngradableOmittedPatternIsUnreachable(pat : Downgradable?) {
  // CHECK: switch_enum {{%.*}} : $Downgradable, case #Downgradable.spoon!enumelt: [[CASE1:bb[0-9]+]], case #Downgradable.hat!enumelt: [[CASE2:bb[0-9]+]], default [[DEFAULT_CASE:bb[0-9]+]]
  switch pat! {
  // CHECK: [[CASE1]]:
  case .spoon:
    break
  // CHECK: [[CASE2]]:
  case .hat:
    break
  // CHECK: [[DEFAULT_CASE]]:
  // CHECK-NEXT:   unreachable
  }
  
  // CHECK: switch_enum {{%[0-9]+}} : $Downgradable, case #Downgradable.spoon!enumelt: {{bb[0-9]+}}, case #Downgradable.hat!enumelt: {{bb[0-9]+}}, default [[TUPLE_DEFAULT_CASE_1:bb[0-9]+]]
  // CHECK: switch_enum [[Y:%[0-9]+]] : $Downgradable, case #Downgradable.spoon!enumelt: {{bb[0-9]+}}, case #Downgradable.hat!enumelt: {{bb[0-9]+}}, default [[TUPLE_DEFAULT_CASE_2:bb[0-9]+]]
  switch (pat!, pat!) {
  case (.spoon, .spoon):
    break
  case (.spoon, .hat):
    break
  case (.hat, .spoon):
    break
  case (.hat, .hat):
    break
  // CHECK: [[TUPLE_DEFAULT_CASE_2]]:
  // CHECK-NEXT:   unreachable
    
  // CHECK: switch_enum [[Y]] : $Downgradable, case #Downgradable.spoon!enumelt: {{bb[0-9]+}}, case #Downgradable.hat!enumelt: {{bb[0-9]+}}, default [[TUPLE_DEFAULT_CASE_3:bb[0-9]+]]
    
  // CHECK: [[TUPLE_DEFAULT_CASE_3]]:
  // CHECK-NEXT:   unreachable
    
  // CHECK: [[TUPLE_DEFAULT_CASE_1]]:
  // CHECK-NEXT:   unreachable
  }
  
}

