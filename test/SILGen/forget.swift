// RUN: %target-swift-emit-silgen -enable-experimental-move-only -module-name test %s | %FileCheck %s --enable-var-scope
// RUN: %target-swift-emit-sil -enable-experimental-move-only -module-name test -sil-verify-all %s | %FileCheck %s --check-prefix CHECK-SIL --enable-var-scope

func invokedDeinit() {}

@_moveOnly enum MaybeFile {
  case some(File)
  case none

  // NOTE: we can't pattern match on self since
  // that would consume it before we can forget self!
  var test: Int {
    __consuming get {
      _forget self
      return 0
    }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test9MaybeFileOAASivg
  // CHECK:    [[SELF_BOX:%.*]] = alloc_box ${ let MaybeFile }, let, name "self", argno 1
  // CHECK:    project_box
  // CHECK:    store {{.*}} to [init]
  // CHECK:    [[SELF_REF:%.*]] = project_box [[SELF_BOX]] : ${ let MaybeFile }, 0
  // CHECK:    [[SELF_MMC:%.*]] = mark_must_check [assignable_but_not_consumable] [[SELF_REF]] : $*MaybeFile
  // CHECK:    [[SELF_VAL:%.*]] = load [copy] [[SELF_MMC]] : $*MaybeFile
  // CHECK:    switch_enum [[SELF_VAL]] : $MaybeFile, case #MaybeFile.some!enumelt: bb1, case #MaybeFile.none!enumelt: bb2
  //
  // CHECK:  bb1([[FILE:%.*]] : @owned $File):
  // CHECK:    destroy_value [[FILE]] : $File
}

@_moveOnly struct File {
  let fd: Int
  static var nextFD: Int = 0

  init() {
    fd = File.nextFD
    File.nextFD += 1
  }

  __consuming func takeDescriptor() -> Int {
    let id = fd
    _forget self
    return id
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test4FileV14takeDescriptorSiyF
  // CHECK:  [[SELF_BOX:%.*]] = alloc_box ${ let File }, let, name "self", argno 1
  // CHECK:  project_box
  // CHECK:  store {{.*}} to [init]
  // CHECK:  project_box
  // CHECK:  load_borrow {{.*}} : $*File
  // CHECK:  [[SELF_REF:%.*]] = project_box [[SELF_BOX]] : ${ let File }, 0
  // CHECK:  [[SELF_MMC:%.*]] = mark_must_check [assignable_but_not_consumable] [[SELF_REF]] : $*File
  // CHECK:  [[SELF_VAL:%.*]] = load [copy] [[SELF_MMC]] : $*File
  // CHECK:  end_lifetime [[SELF_VAL]] : $File

  deinit {
    invokedDeinit()
  }
}

@_moveOnly struct PointerTree {
  let left: Ptr
  let file: File
  let popularity: Int
  var right: Ptr

  init(doForget: Bool, file: __owned File, ptr: Ptr) throws {
    self.file = file
    self.left = ptr
    self.popularity = 0
    self.right = ptr

    if doForget {
      _forget self
    }
    throw E.err
  }

// CHECK-LABEL: sil hidden [ossa] @$s4test11PointerTreeV8doForget4file3ptrACSb_AA4FileVnAA3PtrCtKcfC
// CHECK:   bb0{{.*}}:
// CHECK:     [[SELF_BOX:%.*]] = alloc_box ${ var PointerTree }, var, name "self"
// CHECK:     [[MUI:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]] : ${ var PointerTree }
// CHECK:     [[SELF:%.*]] = begin_borrow [lexical] [[MUI]] : ${ var PointerTree }
//            .. skip to the conditional test ..
// CHECK:     [[SHOULD_THROW:%.*]] = struct_extract {{.*}} : $Bool, #Bool._value
// CHECK:     cond_br [[SHOULD_THROW]], bb1, bb2
//
// CHECK:   bb1:
// CHECK:     [[SELF_PTR:%.*]] = project_box [[SELF]] : ${ var PointerTree }, 0
// CHECK:     [[ACCESS:%.*]] = begin_access [read] [unknown] [[SELF_PTR]] : $*PointerTree
// CHECK:     [[MMC:%.*]] = mark_must_check [assignable_but_not_consumable] [[ACCESS]] : $*PointerTree
// CHECK:     [[COPIED_SELF:%.*]] = load [copy] [[MMC]] : $*PointerTree
// CHECK:     end_access [[ACCESS]] : $*PointerTree
// CHECK:     ([[LEFT:%.*]], [[FILE:%.*]], {{%.*}}, [[RIGHT:%.*]]) = destructure_struct [[COPIED_SELF]] : $PointerTree
// CHECK:     destroy_value [[LEFT]] : $Ptr
// CHECK:     destroy_value [[FILE]] : $File
// CHECK:     destroy_value [[RIGHT]] : $Ptr
// CHECK:     br bb3
//
// CHECK:   bb2:
// CHECK:     br bb3
//
// CHECK:   bb3:
// CHECK:     end_borrow [[SELF]] : ${ var PointerTree }
// CHECK:     destroy_value [[MUI]] : ${ var PointerTree }
// CHECK:     throw
// CHECK: } // end sil function

// After the mandatory passes have run, check for correct deinitializations within the init.

// CHECK-SIL-LABEL: sil hidden @$s4test11PointerTreeV8doForget4file3ptrACSb_AA4FileVnAA3PtrCtKcfC
// CHECK-SIL:     [[SHOULD_THROW:%.*]] = struct_extract {{.*}} : $Bool, #Bool._value
// CHECK-SIL:     cond_br [[SHOULD_THROW]], bb1, bb2
//
// CHECK-SIL:  bb1:
// CHECK-SIL:     [[ACCESS:%.*]] = begin_access [modify] [static] {{.*}} : $*PointerTree
// CHECK-SIL:     [[SELF_VAL:%.*]] = load [[ACCESS]] : $*PointerTree
// CHECK-SIL:     end_access [[ACCESS]] : $*PointerTree
// CHECK-SIL:     [[LEFT:%.*]] = struct_extract [[SELF_VAL]] : $PointerTree, #PointerTree.left
// CHECK-SIL:     [[FILE:%.*]] = struct_extract [[SELF_VAL]] : $PointerTree, #PointerTree.file
// CHECK-SIL:     [[RIGHT:%.*]] = struct_extract [[SELF_VAL]] : $PointerTree, #PointerTree.right
// CHECK-SIL:     strong_release [[LEFT]] : $Ptr
// CHECK-SIL:     [[FILE_DEINIT:%.*]] = function_ref @$s4test4FileVfD : $@convention(method) (@owned File) -> ()
// CHECK-SIL:     apply [[FILE_DEINIT]]([[FILE]])
// CHECK-SIL:     strong_release [[RIGHT]] : $Ptr
// CHECK-SIL:     br bb3
//
// CHECK-SIL:  bb2:
// CHECK-SIL:     [[TREE_DEINIT:%.*]] = function_ref @$s4test11PointerTreeVfD : $@convention(method) (@owned PointerTree) -> ()
// CHECK-SIL:     [[SELF_VAL:%.*]] = load {{.*}} : $*PointerTree
// CHECK-SIL:     apply [[TREE_DEINIT]]([[SELF_VAL]]) : $@convention(method) (@owned PointerTree) -> ()
// CHECK-SIL:     br bb3
//
// CHECK-SIL:  bb3:
// CHECK-SIL-NOT:  apply
// CHECK-SIL:      throw


  deinit {
    invokedDeinit()
  }
}

final class Wallet {
  var numCards = 0
}

@_moveOnly enum Ticket {
  case empty
  case within(Wallet)

  init(inWallet wallet: Wallet? = nil) {
    self = .within(Wallet())
    if let existingWallet = wallet {
      _forget self
      self = .within(existingWallet)
    }
  }
  // As of now, we allow reinitialization after forget. Not sure if this is intended.
  // CHECK-LABEL: sil hidden [ossa] @$s4test6TicketO8inWalletAcA0D0CSg_tcfC
  // CHECK:    switch_enum {{.*}} : $Optional<Wallet>, case #Optional.some!enumelt: bb2, case #Optional.none!enumelt: bb1
  // CHECK:  bb2(%22 : @owned $Wallet):
  // CHECK:    br bb3
  // >> now we begin the destruction sequence, which involves pattern matching on self to destroy its innards
  // CHECK:  bb3:
  // CHECK:    [[SELF_REF:%.*]] = project_box [[SELF_BOX:%.*]] : ${ var Ticket }, 0
  // CHECK:    [[SELF_ACCESS:%.*]] = begin_access [read] [unknown] %27 : $*Ticket
  // CHECK:    [[SELF_MMC:%.*]] = mark_must_check [assignable_but_not_consumable] %28 : $*Ticket
  // CHECK:    [[SELF_COPY:%.*]] = load [copy] %29 : $*Ticket
  // CHECK:    end_access [[SELF_ACCESS:%.*]] : $*Ticket
  // CHECK:    switch_enum [[SELF_COPY]] : $Ticket, case #Ticket.empty!enumelt: bb4, case #Ticket.within!enumelt: bb5
  // CHECK:  bb4:
  // CHECK:    br bb6
  // CHECK:  bb5([[PREV_SELF_WALLET:%.*]] : @owned $Wallet):
  // CHECK:    destroy_value [[PREV_SELF_WALLET]] : $Wallet
  // CHECK:    br bb6
  // >> from here on we are reinitializing self.
  // CHECK:  bb6:
  // CHECK:    [[SELF_REF2:%.*]] = project_box [[SELF_BOX]] : ${ var Ticket }, 0
  // CHECK:    [[NEW_SELF_VAL:%.*]] = enum $Ticket, #Ticket.within!enumelt, {{.*}} : $Wallet
  // CHECK:    [[SELF_ACCESS2:%.*]] = begin_access [modify] [unknown] [[SELF_REF2]] : $*Ticket
  // CHECK:    [[SELF_MMC2:%.*]] = mark_must_check [assignable_but_not_consumable] [[SELF_ACCESS2]] : $*Ticket
  // CHECK:    assign [[NEW_SELF_VAL]] to [[SELF_MMC2]] : $*Ticket
  // CHECK:    end_access [[SELF_ACCESS2]] : $*Ticket

  deinit {
    print("destroying ticket")
  }
}

enum E: Error { case err }
class Ptr { var whatever: Int = 0 }
