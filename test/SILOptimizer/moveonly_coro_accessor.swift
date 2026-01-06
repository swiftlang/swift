// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -module-name test -sil-verify-all -verify %s | %FileCheck %s --enable-var-scope

@inline(never) func someFunction() {}

public struct File: ~Copyable {
  var x = 0
  var associatedFiles: ListOfFiles = ListOfFiles()

  func getRawID() -> Int { return x }
}

public class ListOfFiles {
  var next: ListOfFiles? = nil
  public var file: File = File()

// ListOfFiles.file.read
// CHECK-LABEL: sil [transparent] @$s4test11ListOfFilesC4fileAA4FileVvr : $@yield_once @convention(method) (@guaranteed ListOfFiles) -> @yields @guaranteed File {
// CHECK:   bb0([[SELF:%[0-9]+]] : $ListOfFiles):
// CHECK:     [[REF:%[0-9]+]] = ref_element_addr [[SELF]] : $ListOfFiles, #ListOfFiles.file
// CHECK:     [[ACCESS:%[0-9]+]] = begin_access [read] [dynamic] [[REF]] : $*File
// CHECK:     [[FILE:%[0-9]+]] = load [[ACCESS]] : $*File
// CHECK:     yield [[FILE]] : $File, resume bb1, unwind bb2

// CHECK:   bb1:
// CHECK:     end_access [[ACCESS]] : $*File
// CHECK-NOT: destroy
// CHECK:     return

// CHECK:   bb2:
// CHECK:     end_access [[ACCESS]] : $*File
// CHECK-NOT: destroy
// CHECK:     unwind
// CHECK: } // end sil function



// ListOfFiles.file.setter
// CHECK-LABEL: sil [transparent] @$s4test11ListOfFilesC4fileAA4FileVvs : $@convention(method) (@owned File, @guaranteed ListOfFiles) -> () {
// CHECK:  bb0([[NEW_VAL:%.*]] : $File, [[SELF:%.*]] : $ListOfFiles):
// CHECK:    [[NEW_VAL_STACK:%.*]] = alloc_stack $File, let, name "value", argno 1
// CHECK:    store [[NEW_VAL]] to [[NEW_VAL_STACK]] : $*File
// CHECK:    [[NEW_VAL_RELOADED:%.*]] = load [[NEW_VAL_STACK]] : $*File
// >> destroy the element currently in the field
// CHECK:    [[FIELD:%.*]] = ref_element_addr [[SELF]] : $ListOfFiles, #ListOfFiles.file
// >> write the new value in its place
// CHECK:    [[FIELD_ACCESS:%.*]] = begin_access [modify] [dynamic] [[FIELD]] : $*File
// CHECK:    destroy_addr [[FIELD_ACCESS]] : $*File
// CHECK:    store [[NEW_VAL_RELOADED]] to [[FIELD_ACCESS]] : $*File
//
// CHECK:    end_access [[FIELD_ACCESS]] : $*File
// CHECK-NOT: begin_access
// CHECK:    return
// CHECK:  } // end sil function



// ListOfFiles.file.modify
// CHECK:  sil [transparent] @$s4test11ListOfFilesC4fileAA4FileVvM : $@yield_once @convention(method) (@guaranteed ListOfFiles) -> @yields @inout File {
// CHECK:    bb0([[SELF:%.*]] : $ListOfFiles):
// CHECK:    [[FIELD:%.*]] = ref_element_addr [[SELF]] : $ListOfFiles, #ListOfFiles.file
// CHECK:    [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[FIELD]] : $*File
// CHECK:    yield [[ACCESS]] : $*File, resume bb1, unwind bb2
//
// CHECK:    bb1:
//
// CHECK:    end_access [[ACCESS]] : $*File
// CHECK-NOT: begin_access
// CHECK-NOT:    destroy_addr [[FIELD]] : $*File
//
// CHECK:    bb2:
// CHECK-NOT:    destroy_addr [[FIELD]] : $*File
//
// CHECK:    end_access [[ACCESS]] : $*File
// CHECK-NOT: begin_access
// CHECK:    unwind
// CHECK:  } // end sil function

  public var freshFile: File {
    someFunction()
    return File()
  }


// ListOfFiles.freshFile.getter
// CHECK-LABEL:  sil @$s4test11ListOfFilesC9freshFileAA0F0Vvg : $@convention(method) (@guaranteed ListOfFiles) -> @owned File {
// CHECK:  bb0({{.*}} : $ListOfFiles):
// CHECK:    [[SOME_FN:%.*]] = function_ref @$s4test12someFunctionyyF : $@convention(thin) () -> ()
// CHECK:    = apply [[SOME_FN]]() : $@convention(thin) () -> ()
// CHECK:    [[METATYPE:%.*]] = metatype $@thin File.Type
// CHECK:    // function_ref File.init()
// CHECK:    [[INIT:%.*]] = function_ref @$s4test4FileVACycfC : $@convention(method) (@thin File.Type) -> @owned File
// CHECK:    [[NEW_FILE:%.*]] = apply [[INIT]]([[METATYPE]]) : $@convention(method) (@thin File.Type) -> @owned File
// CHECK:    return [[NEW_FILE]] : $File
// CHECK:  } // end sil function


// ListOfFiles.freshFile.read
// CHECK-LABEL:  sil [transparent] @$s4test11ListOfFilesC9freshFileAA0F0Vvr : $@yield_once @convention(method) (@guaranteed ListOfFiles) -> @yields @guaranteed File {
// CHECK:  bb0([[SELF:%.*]] : $ListOfFiles):
// CHECK:    [[GETTER:%.*]] = function_ref @$s4test11ListOfFilesC9freshFileAA0F0Vvg : $@convention(method) (@guaranteed ListOfFiles) -> @owned File
// CHECK:    [[FILE:%.*]] = apply [[GETTER]]([[SELF]]) : $@convention(method) (@guaranteed ListOfFiles) -> @owned File
// CHECK:    yield [[FILE]] : $File, resume bb1, unwind bb2
//
// CHECK:  bb1:
// CHECK:    release_value [[FILE]] : $File
// CHECK:    return {{.*}} : $()
//
// CHECK:  bb2:
// CHECK:    release_value [[FILE]] : $File
// CHECK:    unwind
// CHECK:  } // end sil function
}


// access chain going through move-only and copyable types
func bounceBetweenKinds(_ l: ListOfFiles) -> ListOfFiles {
  return l.file.associatedFiles.file.associatedFiles

// CHECK-LABEL: sil hidden @$s4test18bounceBetweenKindsyAA11ListOfFilesCADF : $@convention(thin) (@guaranteed ListOfFiles) -> @owned ListOfFiles {
// CHECK:  bb0([[ARG:%[0-9]+]] : $ListOfFiles):
// CHECK:    [[REF1:%[0-9]+]] = ref_element_addr [[ARG]] : $ListOfFiles, #ListOfFiles.file
// CHECK:    [[ACCESS1:%[0-9]+]] = begin_access [read] [dynamic] [[REF1]] : $*File
// CHECK:    [[FILE1:%[0-9]+]] = load [[ACCESS1]] : $*File
// CHECK:    [[ASSOC_FILES1:%[0-9]+]] = struct_extract [[FILE1]] : $File, #File.associatedFiles
// CHECK:    strong_retain [[ASSOC_FILES1]] : $ListOfFiles
// CHECK:    end_access [[ACCESS1]] : $*File
// CHECK-NOT: destroy
// CHECK:    [[REF2:%[0-9]+]] = ref_element_addr [[ASSOC_FILES1]] : $ListOfFiles, #ListOfFiles.file
// CHECK:    [[ACCESS2:%[0-9]+]] = begin_access [read] [dynamic] [[REF2]] : $*File
// CHECK:    [[FILE2:%[0-9]+]] = load [[ACCESS2]] : $*File
// CHECK:    [[ASSOC_FILES2:%[0-9]+]] = struct_extract [[FILE2]] : $File, #File.associatedFiles
// CHECK:    strong_retain [[ASSOC_FILES2]] : $ListOfFiles
// CHECK:    end_access [[ACCESS2]] : $*File
// CHECK-NOT: destroy
// CHECK:    strong_release [[ASSOC_FILES1]]
// CHECK-NOT: destroy
// CHECK:    return [[ASSOC_FILES2]]
// CHECK:  } // end sil function '$s4test18bounceBetweenKindsyAA11ListOfFilesCADF'
}

// verify that the VTable doesn't have a 'get' for a move-only type
// CHECK-LABEL: sil_vtable ListOfFiles {
// CHECK:          #ListOfFiles.next!getter: (ListOfFiles) -> () -> ListOfFiles? : @$s4test11ListOfFilesC4nextACSgvg // ListOfFiles.next.getter
// CHECK-NOT:      getter
// CHECK:          #ListOfFiles.file!read
// CHECK-NEXT:     #ListOfFiles.file!setter
// CHECK-NEXT:     #ListOfFiles.file!modify
// CHECK-NEXT:     #ListOfFiles.freshFile!read:
// CHECK-NOT:      getter
// CHECK:       }
