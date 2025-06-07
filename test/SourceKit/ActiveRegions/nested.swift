#if FLAG_1
class Foo {
    #if FLAG_2
    func debugOnly() {
        
    }
    #endif
}
#elseif FLAG_3
class GracefulFallback {
    
}
#else
class Fallback {
    
}
#endif

// RUN: %sourcekitd-test -req=active-regions %s -- -D FLAG_1 -module-name active_regions %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: START IF CONFIGS
// CHECK1-NEXT: 1:1 - active
// CHECK1-NEXT: 3:5 - inactive
// CHECK1-NEXT: 9:1 - inactive
// CHECK1-NEXT: 13:1 - inactive
// CHECK1-NEXT: END IF CONFIGS

// RUN: %sourcekitd-test -req=active-regions %s -- -D FLAG_2 -module-name active_regions %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: START IF CONFIGS
// CHECK2-NEXT: 1:1 - inactive
// CHECK2-NEXT: 9:1 - inactive
// CHECK2-NEXT: 13:1 - active
// CHECK2-NEXT: END IF CONFIGS

// RUN: %sourcekitd-test -req=active-regions %s -- -D FLAG_1 -D FLAG_2 -module-name active_regions %s | %FileCheck -check-prefix=CHECK3 %s
// CHECK3: START IF CONFIGS
// CHECK3-NEXT: 1:1 - active
// CHECK3-NEXT: 3:5 - active
// CHECK3-NEXT: 9:1 - inactive
// CHECK3-NEXT: 13:1 - inactive
// CHECK3-NEXT: END IF CONFIGS

// RUN: %sourcekitd-test -req=active-regions %s -- -D FLAG_3 -module-name active_regions %s | %FileCheck -check-prefix=CHECK4 %s
// CHECK4: START IF CONFIGS
// CHECK4-NEXT: 1:1 - inactive
// CHECK4-NEXT: 9:1 - active
// CHECK4-NEXT: 13:1 - inactive
// CHECK4-NEXT: END IF CONFIGS
