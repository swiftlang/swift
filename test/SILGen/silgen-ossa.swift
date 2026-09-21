// RUN: %target-swift-emit-silgen %s | %FileCheck %s --check-prefix SILGEN
// RUN: %target-swift-emit-silgen-ossa %s | %FileCheck %s --check-prefix OSSA
// RUN: %target-swift-emit-silgen-ossa(mock-sdk: %clang-importer-sdk) %s | %FileCheck %s --check-prefix OSSA

// In -emit-silgen, we don't emit an end_borrow or destroy_value as there is no expression in the body.
// It's raw output from the compiler without having run the verifier on it.

// In -emit-silgen-ossa, we should see those pieces as the follow-up passes make this valid OSSA SIL.

// SILGEN: sil hidden{{.*}}helloySixnRi_zlF : $@convention(thin)
// SILGEN: bb0
// SILGEN-NEXT: alloc_box
// SILGEN-NEXT: begin_borrow
// SILGEN-NEXT: project_box
// SILGEN-NEXT: copy_addr
// SILGEN-NEXT: unreachable
// SILGEN: } // end sil function

// OSSA: sil hidden{{.*}}helloySixnRi_zlF : $@convention(thin)
// OSSA:      copy_addr
// OSSA-NEXT: end_borrow
// OSSA-NEXT: destroy_value [dead_end]
// OSSA-NEXT: unreachable
// OSSA: } // end sil function
func hello<T: ~Copyable>(_ t: consuming T) -> Int {}
