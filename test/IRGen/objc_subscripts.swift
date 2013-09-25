// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s

// CHECK: @_INSTANCE_METHODS_SomeObject = 
// CHECK:   private constant { i32, i32, [5 x { i8*, i8*, i8* }] } 
// CHECK:   { i32 24, i32 5, [5 x { i8*, i8*, i8* }] 
// CHECK:     [
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([26 x i8]* @"\01L_selector_data(objectAtIndexedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast (%CSo10SomeObject* (%CSo10SomeObject*, i8*, i64)* @_TToCSo10SomeObject11__subscriptFT1iSi_S_g to i8*) 
// CHECK:         }, 
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([30 x i8]* @"\01L_selector_data(setObject:atIndexedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast (void (%CSo10SomeObject*, i8*, %CSo10SomeObject*, i64)* @_TToCSo10SomeObject11__subscriptFT1iSi_S_s to i8*) 
// CHECK:         },
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([25 x i8]* @"\01L_selector_data(objectForKeyedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast (i64 (%CSo10SomeObject*, i8*, %CSo10SomeObject*)* @_TToCSo10SomeObject11__subscriptFT1sS__Sig to i8*) 
// CHECK:         }, 
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { 
// CHECK:           i8* getelementptr inbounds ([29 x i8]* @"\01L_selector_data(setObject:forKeyedSubscript:)", i64 0, i64 0), 
// CHECK:           i8* null, 
// CHECK:           i8* bitcast (void (%CSo10SomeObject*, i8*, i64, %CSo10SomeObject*)* @_TToCSo10SomeObject11__subscriptFT1sS__Sis to i8*) 
// CHECK:         },
// CHECK:       { i8*, i8*, i8* } 
// CHECK:         { i8* getelementptr inbounds ([5 x i8]* @"\01L_selector_data(init)", i64 0, i64 0), i8* getelementptr inbounds ([4 x i8]* @0, i64 0, i64 0), i8* bitcast (%CSo10SomeObject* (%CSo10SomeObject*, i8*)* @_TToCSo10SomeObjectcfMS_FT_S_ to i8*) }
// CHECK:    ]
// CHECK:  }, section "__DATA, __objc_const", align 8

class [objc] SomeObject {
  subscript (i : Int) -> SomeObject {
  // CHECK-LABEL: define internal %CSo10SomeObject* @_TToCSo10SomeObject11__subscriptFT1iSi_S_g(%CSo10SomeObject*, i8*, i64) unnamed_addr
  get:
    // CHECK: call %CSo10SomeObject* @_TCSo10SomeObject11__subscriptFT1iSi_S_g
    return self

  // CHECK-LABEL: define internal void @_TToCSo10SomeObject11__subscriptFT1iSi_S_s(%CSo10SomeObject*, i8*, %CSo10SomeObject*, i64) unnamed_addr
  set:
    // CHECK: void @_TCSo10SomeObject11__subscriptFT1iSi_S_s
  }

  subscript (s : SomeObject) -> Int {
  // CHECK-LABEL: define internal i64 @_TToCSo10SomeObject11__subscriptFT1sS__Sig(%CSo10SomeObject*, i8*, %CSo10SomeObject*) unnamed_addr 
  get:
    // CHECK: call i64 @_TCSo10SomeObject11__subscriptFT1sS__Sig
    return 5

  // CHECK-LABEL: define internal void @_TToCSo10SomeObject11__subscriptFT1sS__Sis
  set:
    // CHECK: call void @_TCSo10SomeObject11__subscriptFT1sS__Sis
  }
}

