// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 -enable-experimental-concurrency | %FileCheck --enable-var-scope %s
// REQUIRES: concurrency

@propertyWrapper
struct GiftWrapped<T> {
    private var _stored: T
    private var numReads : Int
    init(initially: T) {
        self._stored = initially
    }

    var wrappedValue: T {
        mutating get {
            numReads += 1
            return _stored
        }
        set { _stored = newValue }
    }
}

actor Birb {
    private var _storage : Int = 24

    var feathers : Int {
        _read {
            yield _storage
        }
        _modify {
            yield &_storage
        }
    }
}

actor Cat {
    @GlobalCat var leader : String {
        get { "Tiger" }
    }

    var storedBool : Bool = false

    private(set) var computedSweater : Sweater {
        get { return Sweater(self) }
        set {}
    }

    subscript(_ x : Int) -> Cat {
        get { self }
        set {}
    }

    var friend : Cat = Cat()

    var maybeFriend : Cat?

    @GiftWrapped<Birb>(initially: Birb())
    var bestFriend : Birb
}

struct Sweater : Sendable {
    let owner : Cat
    init (_ owner : Cat) {
        self.owner = owner
    }
}

class CatBox {
    var cat : Cat = Cat()
}

@globalActor
struct GlobalCat {
    static let shared : Cat = Cat()
}

@GlobalCat var globalBool : Bool = false

@GlobalCat var someBirb : Birb {
    get { Birb() }
    set {}
}

// CHECK-LABEL: sil hidden [ossa] @$s4test015accessSweaterOfC03catAA0C0VAA3CatC_tYaF : $@convention(thin) @async (@guaranteed Cat) -> @owned Sweater {
// CHECK:  bb0([[CAT:%[0-9]+]] : @guaranteed $Cat):
// CHECK:    [[PREV_EXEC:%.*]] = builtin "getCurrentExecutor"
// CHECK:    hop_to_executor [[CAT]] : $Cat
// CHECK:    [[CAT_GETTER:%[0-9]+]] = class_method [[CAT]] : $Cat, #Cat.computedSweater!getter : (Cat) -> () -> Sweater, $@convention(method) (@guaranteed Cat) -> @owned Sweater
// CHECK:    [[SWEATER1_REF:%[0-9]+]] = apply [[CAT_GETTER]]([[CAT]]) : $@convention(method) (@guaranteed Cat) -> @owned Sweater
// CHECK:    hop_to_executor [[PREV_EXEC]]
// CHECK:    [[SWEATER1:%[0-9]+]] = begin_borrow [[SWEATER1_REF]] : $Sweater
// CHECK:    [[SWEATER1_OWNER:%[0-9]+]] = struct_extract [[SWEATER1]] : $Sweater, #Sweater.owner
// CHECK:    [[CAT2_REF:%[0-9]+]] = copy_value [[SWEATER1_OWNER]] : $Cat
// CHECK:    end_borrow [[SWEATER1]] : $Sweater
// CHECK:    destroy_value [[SWEATER1_REF]] : $Sweater
// CHECK:    [[CAT2:%[0-9]+]] = begin_borrow [[CAT2_REF]] : $Cat

// CHECK:    [[PREV_EXEC:%.*]] = builtin "getCurrentExecutor"
// CHECK:    hop_to_executor [[CAT2]] : $Cat
// CHECK:    [[CAT2_FOR_LOAD:%[0-9]+]] = begin_borrow [[CAT2_REF]] : $Cat
// CHECK:    [[CAT2_GETTER:%[0-9]+]] = class_method [[CAT2_FOR_LOAD]] : $Cat, #Cat.computedSweater!getter : (Cat) -> () -> Sweater, $@convention(method) (@guaranteed Cat) -> @owned Sweater
// CHECK:    [[SWEATER2_OWNER:%[0-9]+]] = apply [[CAT2_GETTER]]([[CAT2_FOR_LOAD]]) : $@convention(method) (@guaranteed Cat) -> @owned Sweater
// CHECK:    end_borrow [[CAT2_FOR_LOAD]] : $Cat
// CHECK:    end_borrow [[CAT2]] : $Cat
// CHECK:    hop_to_executor [[PREV_EXEC]]

// CHECK:    destroy_value [[CAT2_REF]] : $Cat
// CHECK:    return [[SWEATER2_OWNER]] : $Sweater
// CHECK: } // end sil function '$s4test015accessSweaterOfC03catAA0C0VAA3CatC_tYaF'
func accessSweaterOfSweater(cat : Cat) async -> Sweater {
    // note that Sweater is not an actor!
    return await cat.computedSweater.owner.computedSweater
}

// CHECK-LABEL: sil hidden [ossa] @$s4test26accessGlobalIsolatedMember3catSSAA3CatC_tYaF : $@convention(thin) @async (@guaranteed Cat) -> @owned String {
// CHECK:  bb0([[CAT:%[0-9]+]] : @guaranteed $Cat):
// CHECK:    [[GLOBAL_CAT_SHARED:%[0-9]+]] = function_ref @$s4test9GlobalCatV6sharedAA0C0Cvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:    [[GLOBAL_CAT_RAWPTR:%[0-9]+]] = apply [[GLOBAL_CAT_SHARED]]() : $@convention(thin) () -> Builtin.RawPointer
// CHECK:    [[GLOBAL_CAT_ADDR:%[0-9]+]] = pointer_to_address [[GLOBAL_CAT_RAWPTR]] : $Builtin.RawPointer to [strict] $*Cat
// CHECK:    [[GLOBAL_CAT_REF:%[0-9]+]] = load [copy] [[GLOBAL_CAT_ADDR]] : $*Cat
// CHECK:    [[GLOBAL_CAT:%[0-9]+]] = begin_borrow [[GLOBAL_CAT_REF]] : $Cat

// CHECK:    [[PREV_EXEC:%.*]] = builtin "getCurrentExecutor"
// CHECK:    hop_to_executor [[GLOBAL_CAT]] : $Cat
// CHECK:    [[GETTER:%[0-9]+]] = class_method [[CAT]] : $Cat, #Cat.leader!getter : (Cat) -> () -> String, $@convention(method) (@guaranteed Cat) -> @owned String
// CHECK:    [[THE_STRING:%[0-9]+]] = apply [[GETTER]]([[CAT]]) : $@convention(method) (@guaranteed Cat) -> @owned String
// CHECK:    end_borrow [[GLOBAL_CAT]] : $Cat
// CHECK:    hop_to_executor [[PREV_EXEC]]
// CHECK:    destroy_value [[GLOBAL_CAT_REF]] : $Cat
// CHECK:    return [[THE_STRING]] : $String
// CHECK: } // end sil function '$s4test26accessGlobalIsolatedMember3catSSAA3CatC_tYaF'
func accessGlobalIsolatedMember(cat : Cat) async -> String {
    return await cat.leader
}


actor Dog {

    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC15accessGlobalVarSbyYaF : $@convention(method) @async (@guaranteed Dog) -> Bool {
    // CHECK:   [[GLOBAL_BOOL_ADDR:%[0-9]+]] = global_addr @$s4test10globalBoolSbvp : $*Bool
    // CHECK:   hop_to_executor [[SELF:%[0-9]+]] : $Dog
    // CHECK:   [[SHARED_REF_FN:%[0-9]+]] = function_ref @$s4test9GlobalCatV6sharedAA0C0Cvau : $@convention(thin) () -> Builtin.RawPointer
    // CHECK:   [[SHARED_REF:%[0-9]+]] = apply [[SHARED_REF_FN]]() : $@convention(thin) () -> Builtin.RawPointer
    // CHECK:   [[SHARED_CAT_ADDR:%[0-9]+]] = pointer_to_address [[SHARED_REF]] : $Builtin.RawPointer to [strict] $*Cat
    // CHECK:   [[CAT:%[0-9]+]] = load [copy] [[SHARED_CAT_ADDR]] : $*Cat
    // CHECK:   [[BORROWED_CAT:%[0-9]+]] = begin_borrow [[CAT]] : $Cat

    // CHECK:   hop_to_executor [[BORROWED_CAT]] : $Cat
    // CHECK:   [[GLOBAL_BOOL_ACCESS:%[0-9]+]] = begin_access [read] [dynamic] [[GLOBAL_BOOL_ADDR]] : $*Bool
    // CHECK:   [[THE_BOOL:%[0-9]+]] = load [trivial] [[GLOBAL_BOOL_ACCESS]] : $*Bool
    // CHECK:   end_access [[GLOBAL_BOOL_ACCESS]] : $*Bool
    // CHECK:   end_borrow [[BORROWED_CAT]] : $Cat

    // CHECK:   hop_to_executor [[SELF]] : $Dog
    // CHECK:   destroy_value [[CAT]] : $Cat
    // CHECK:   return [[THE_BOOL]] : $Bool
    // CHECK: } // end sil function '$s4test3DogC15accessGlobalVarSbyYaF'
    func accessGlobalVar() async -> Bool {
        return await globalBool
    }


    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC24accessGlobalComputedPropSiyYaF : $@convention(method) @async (@guaranteed Dog) -> Int {
    // CHECK:  bb0([[SELF:%[0-9]+]] : @guaranteed $Dog):
    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    [[SHARED_REF_FN:%[0-9]+]] = function_ref @$s4test9GlobalCatV6sharedAA0C0Cvau : $@convention(thin) () -> Builtin.RawPointer
    // CHECK:    [[SHARED_REF:%[0-9]+]] = apply [[SHARED_REF_FN]]() : $@convention(thin) () -> Builtin.RawPointer
    // CHECK:    [[SHARED_CAT_ADDR:%[0-9]+]] = pointer_to_address [[SHARED_REF]] : $Builtin.RawPointer to [strict] $*Cat
    // CHECK:    [[CAT:%[0-9]+]] = load [copy] [[SHARED_CAT_ADDR]] : $*Cat
    // CHECK:    [[BORROWED_CAT:%[0-9]+]] = begin_borrow [[CAT]] : $Cat

    // CHECK:    hop_to_executor [[BORROWED_CAT]] : $Cat
    // CHECK:    [[SOMEBIRB_GETTER:%[0-9]+]] = function_ref @$s4test8someBirbAA0C0Cvg : $@convention(thin) () -> @owned Birb
    // CHECK:    [[BIRB:%[0-9]+]] = apply [[SOMEBIRB_GETTER]]() : $@convention(thin) () -> @owned Birb
    // CHECK:    end_borrow [[BORROWED_CAT:%[0-9]+]] : $Cat

    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    [[BORROWED_BIRB:%[0-9]+]] = begin_borrow [[BIRB]] : $Birb
    // CHECK:    hop_to_executor [[BORROWED_BIRB]] : $Birb
    // CHECK:    [[BORROWED_BIRB_FOR_LOAD:%[0-9]+]] = begin_borrow [[BIRB]] : $Birb
    // CHECK:    [[FEATHER_GETTER:%[0-9]+]] = class_method [[BORROWED_BIRB_FOR_LOAD]] : $Birb, #Birb.feathers!getter : (Birb) -> () -> Int, $@convention(method) (@guaranteed Birb) -> Int
    // CHECK:    [[THE_INT:%[0-9]+]] = apply [[FEATHER_GETTER]]([[BORROWED_BIRB_FOR_LOAD]]) : $@convention(method) (@guaranteed Birb) -> Int
    // CHECK:    end_borrow [[BORROWED_BIRB_FOR_LOAD]] : $Birb
    // CHECK:    end_borrow [[BORROWED_BIRB]] : $Birb

    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    destroy_value [[BIRB]] : $Birb
    // CHECK:    destroy_value [[CAT]] : $Cat
    // CHECK:    return [[THE_INT]] : $Int
    // CHECK: } // end sil function '$s4test3DogC24accessGlobalComputedPropSiyYaF'
    func accessGlobalComputedProp() async -> Int {
        return await someBirb.feathers
    }


    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC21accessWrappedProperty3catSiAA3CatC_tYaF : $@convention(method) @async (@guaranteed Cat, @guaranteed Dog) -> Int {
    // CHECK:  bb0([[CAT:%[0-9]+]] : @guaranteed $Cat, [[SELF:%[0-9]+]] : @guaranteed $Dog):
    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    hop_to_executor [[CAT]] : $Cat
    // CHECK:    [[CAT_GETTER:%[0-9]+]] = class_method [[CAT]] : $Cat, #Cat.bestFriend!getter : (Cat) -> () -> Birb, $@convention(method) (@guaranteed Cat) -> @owned Birb
    // CHECK:    [[BIRB_REF:%[0-9]+]] = apply [[CAT_GETTER]]([[CAT]]) : $@convention(method) (@guaranteed Cat) -> @owned Birb
    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    [[BIRB:%[0-9]+]] = begin_borrow [[BIRB_REF]] : $Birb
    // CHECK:    hop_to_executor [[BIRB]] : $Birb
    // CHECK:    [[BIRB_FOR_LOAD:%[0-9]+]] = begin_borrow [[BIRB_REF]] : $Birb
    // CHECK:    [[BIRB_GETTER:%[0-9]+]] = class_method [[BIRB_FOR_LOAD]] : $Birb, #Birb.feathers!getter : (Birb) -> () -> Int, $@convention(method) (@guaranteed Birb) -> Int
    // CHECK:    [[THE_INT:%[0-9]+]] = apply [[BIRB_GETTER]]([[BIRB_FOR_LOAD]]) : $@convention(method) (@guaranteed Birb) -> Int
    // CHECK:    end_borrow [[BIRB_FOR_LOAD]] : $Birb
    // CHECK:    end_borrow [[BIRB]] : $Birb
    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    destroy_value [[BIRB_REF]] : $Birb
    // CHECK:    return [[THE_INT]] : $Int
    // CHECK: } // end sil function '$s4test3DogC21accessWrappedProperty3catSiAA3CatC_tYaF'
    func accessWrappedProperty(cat : Cat) async -> Int {
        return await cat.bestFriend.feathers
    }


    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC16accessFromRValueSbyYaF : $@convention(method) @async (@guaranteed Dog) -> Bool {
    // CHECK:   hop_to_executor [[SELF:%[0-9]+]] : $Dog
    // CHECK:   [[INIT:%[0-9]+]] = function_ref @$s4test3CatCACycfC : $@convention(method) (@thick Cat.Type) -> @owned Cat
    // CHECK:   [[CAT_REF:%[0-9]+]] = apply [[INIT]]({{%[0-9]+}}) : $@convention(method) (@thick Cat.Type) -> @owned Cat
    // CHECK:   [[CAT_BORROW_FOR_HOP:%[0-9]+]] = begin_borrow [[CAT_REF]] : $Cat

    // CHECK:   hop_to_executor [[CAT_BORROW_FOR_HOP]] : $Cat
    // CHECK:   [[CAT_BORROW_FOR_LOAD:%[0-9]+]] = begin_borrow [[CAT_REF]] : $Cat
    // CHECK:   [[GETTER:%[0-9]+]] = class_method [[CAT_BORROW_FOR_LOAD]] : $Cat, #Cat.storedBool!getter : (Cat) -> () -> Bool, $@convention(method) (@guaranteed Cat) -> Bool
    // CHECK:   [[THE_BOOL:%[0-9]+]] = apply [[GETTER]]([[CAT_BORROW_FOR_LOAD]]) : $@convention(method) (@guaranteed Cat) -> Bool
    // CHECK:   end_borrow [[CAT_BORROW_FOR_LOAD]] : $Cat
    // CHECK:   end_borrow [[CAT_BORROW_FOR_HOP]] : $Cat

    // CHECK:   hop_to_executor [[SELF]] : $Dog
    // CHECK:   destroy_value [[CAT_REF]] : $Cat
    // CHECK:   return [[THE_BOOL]] : $Bool
    // CHECK: } // end sil function '$s4test3DogC16accessFromRValueSbyYaF'
    func accessFromRValue() async -> Bool {
        return await Cat().storedBool
    }


    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC23accessFromRValueChainedSbyYaF : $@convention(method) @async (@guaranteed Dog) -> Bool {
    // CHECK:   hop_to_executor [[SELF:%[0-9]+]] : $Dog
    // CHECK:   [[INIT:%[0-9]+]] = function_ref @$s4test3CatCACycfC : $@convention(method) (@thick Cat.Type) -> @owned Cat
    // CHECK:   [[CAT_REF:%[0-9]+]] = apply [[INIT]]({{%[0-9]+}}) : $@convention(method) (@thick Cat.Type) -> @owned Cat
    // CHECK:   [[CAT_BORROW_FOR_HOP:%[0-9]+]] = begin_borrow [[CAT_REF]] : $Cat

    // CHECK:   hop_to_executor [[CAT_BORROW_FOR_HOP]] : $Cat
    // CHECK:   [[CAT_BORROW_FOR_LOAD:%[0-9]+]] = begin_borrow [[CAT_REF]] : $Cat
    // CHECK:   [[FRIEND_GETTER:%[0-9]+]] = class_method [[CAT_BORROW_FOR_LOAD]] : $Cat, #Cat.friend!getter : (Cat) -> () -> Cat, $@convention(method) (@guaranteed Cat) -> @owned Cat
    // CHECK:   [[FRIEND_REF:%[0-9]+]] = apply [[FRIEND_GETTER]]([[CAT_BORROW_FOR_LOAD]]) : $@convention(method) (@guaranteed Cat) -> @owned Cat
    // CHECK:   end_borrow [[CAT_BORROW_FOR_LOAD]] : $Cat
    // CHECK:   end_borrow [[CAT_BORROW_FOR_HOP]] : $Cat

    // CHECK:   hop_to_executor [[SELF]] : $Dog
    // CHECK:   destroy_value [[CAT_REF]] : $Cat
    // CHECK:   [[FRIEND_BORROW_FOR_HOP:%[0-9]+]] = begin_borrow [[FRIEND_REF]] : $Cat

    // CHECK:   hop_to_executor [[FRIEND_BORROW_FOR_HOP]] : $Cat
    // CHECK:   [[FRIEND_BORROW_FOR_LOAD:%[0-9]+]] = begin_borrow [[FRIEND_REF]] : $Cat
    // CHECK:   [[BOOL_GETTER:%[0-9]+]] = class_method [[FRIEND_BORROW_FOR_LOAD]] : $Cat, #Cat.storedBool!getter : (Cat) -> () -> Bool, $@convention(method) (@guaranteed Cat) -> Bool
    // CHECK:   [[THE_BOOL:%[0-9]+]] = apply [[BOOL_GETTER]]([[FRIEND_BORROW_FOR_LOAD]]) : $@convention(method) (@guaranteed Cat) -> Bool
    // CHECK:   end_borrow [[FRIEND_BORROW_FOR_LOAD]] : $Cat
    // CHECK:   end_borrow [[FRIEND_BORROW_FOR_HOP]] : $Cat

    // CHECK:   hop_to_executor [[SELF]] : $Dog
    // CHECK:   destroy_value [[FRIEND_REF]] : $Cat
    // CHECK:   return [[THE_BOOL]] : $Bool
    // CHECK: } // end sil function '$s4test3DogC23accessFromRValueChainedSbyYaF'
    func accessFromRValueChained() async -> Bool {
        return await Cat().friend.storedBool
    }


    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC15accessSubscript3catAA3CatCAG_tYaF : $@convention(method) @async (@guaranteed Cat, @guaranteed Dog) -> @owned Cat {
    // CHECK: bb0([[CAT:%[0-9]+]] : @guaranteed $Cat, [[DOG:%[0-9]+]] : @guaranteed $Dog):
    // CHECK:   hop_to_executor [[DOG]] : $Dog
    // CHECK:   [[INTEGER1:%[0-9]+]] = apply {{%[0-9]+}}({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int

    // CHECK:   hop_to_executor [[CAT]] : $Cat
    // CHECK:   [[SUBSCRIPT_FN:%[0-9]+]] = class_method [[CAT]] : $Cat, #Cat.subscript!getter : (Cat) -> (Int) -> Cat, $@convention(method) (Int, @guaranteed Cat) -> @owned Cat
    // CHECK:   [[OTHER_CAT:%[0-9]+]] = apply [[SUBSCRIPT_FN]]([[INTEGER1]], [[CAT]]) : $@convention(method) (Int, @guaranteed Cat) -> @owned Cat

    // CHECK:   hop_to_executor [[DOG]] : $Dog
    // CHECK:   return [[OTHER_CAT]] : $Cat
    // CHECK: } // end sil function '$s4test3DogC15accessSubscript3catAA3CatCAG_tYaF'
    func accessSubscript(cat : Cat) async -> Cat {
        return await cat[1]
    }

    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC27accessRValueNestedSubscriptAA3CatCyYaF : $@convention(method) @async (@guaranteed Dog) -> @owned Cat {
    // CHECK:   hop_to_executor [[SELF:%[0-9]+]] : $Dog
    // CHECK:   [[RVALUE_CAT_REF:%[0-9]+]] = apply {{%[0-9]+}}({{%[0-9]+}}) : $@convention(method) (@thick Cat.Type) -> @owned Cat
    // CHECK:   [[LIT_ONE:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 1
    // CHECK:   [[INT_ONE:%[0-9]+]] = apply {{%[0-9]+}}([[LIT_ONE]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
    // CHECK:   [[RVALUE_CAT:%[0-9]+]] = begin_borrow [[RVALUE_CAT_REF]] : $Cat

    // CHECK:   hop_to_executor [[RVALUE_CAT]] : $Cat
    // CHECK:   [[RVALUE_CAT_FOR_LOAD:%[0-9]+]] = begin_borrow [[RVALUE_CAT_REF]] : $Cat
    // CHECK:   [[RVALUE_CAT_SUBSCRIPT:%[0-9]+]] = class_method [[RVALUE_CAT_FOR_LOAD]] : $Cat, #Cat.subscript!getter : (Cat) -> (Int) -> Cat, $@convention(method) (Int, @guaranteed Cat) -> @owned Cat
    // CHECK:   [[FIRST_CAT_REF:%[0-9]+]] = apply [[RVALUE_CAT_SUBSCRIPT]]([[INT_ONE]], [[RVALUE_CAT_FOR_LOAD]]) : $@convention(method) (Int, @guaranteed Cat) -> @owned Cat
    // CHECK:   end_borrow [[RVALUE_CAT_FOR_LOAD]] : $Cat
    // CHECK:   end_borrow [[RVALUE_CAT]] : $Cat

    // CHECK:   hop_to_executor [[SELF]] : $Dog
    // CHECK:   destroy_value [[RVALUE_CAT_REF]] : $Cat
    // CHECK:   [[LIT_TWO:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 2
    // CHECK:   [[INT_TWO:%[0-9]+]] = apply {{%[0-9]+}}([[LIT_TWO]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
    // CHECK:   [[FIRST_CAT:%[0-9]+]] = begin_borrow [[FIRST_CAT_REF]] : $Cat

    // CHECK:   hop_to_executor [[FIRST_CAT]] : $Cat
    // CHECK:   [[FIRST_CAT_FOR_LOAD:%[0-9]+]] = begin_borrow [[FIRST_CAT_REF]] : $Cat
    // CHECK:   [[FIRST_CAT_SUBSCRIPT:%[0-9]+]] = class_method [[FIRST_CAT_FOR_LOAD]] : $Cat, #Cat.subscript!getter : (Cat) -> (Int) -> Cat, $@convention(method) (Int, @guaranteed Cat) -> @owned Cat
    // CHECK:   [[SECOND_CAT_REF:%[0-9]+]] = apply [[FIRST_CAT_SUBSCRIPT]]([[INT_TWO]], [[FIRST_CAT_FOR_LOAD]]) : $@convention(method) (Int, @guaranteed Cat) -> @owned Cat
    // CHECK:   end_borrow [[FIRST_CAT_FOR_LOAD]] : $Cat
    // CHECK:   end_borrow [[FIRST_CAT]] : $Cat

    // CHECK:   hop_to_executor [[SELF]] : $Dog
    // CHECK:   destroy_value [[FIRST_CAT_REF]] : $Cat
    // CHECK:   return [[SECOND_CAT_REF]] : $Cat
    // CHECK: } // end sil function '$s4test3DogC27accessRValueNestedSubscriptAA3CatCyYaF'
    func accessRValueNestedSubscript() async -> Cat {
        return await Cat()[1][2]
    }


    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC33accessStoredPropFromRefProjection3boxSbAA6CatBoxC_tYaF : $@convention(method) @async (@guaranteed CatBox, @guaranteed Dog) -> Bool {
    // CHECK:   bb0([[BOX:%[0-9]+]] : @guaranteed $CatBox, [[SELF:%[0-9]+]] : @guaranteed $Dog):
    // CHECK:     hop_to_executor [[SELF]] : $Dog
    // CHECK:     [[BOX_GETTER:%[0-9]+]] = class_method [[BOX]] : $CatBox, #CatBox.cat!getter : (CatBox) -> () -> Cat, $@convention(method) (@guaranteed CatBox) -> @owned Cat
    // CHECK:     [[CAT_REF:%[0-9]+]] = apply [[BOX_GETTER]]([[BOX]]) : $@convention(method) (@guaranteed CatBox) -> @owned Cat
    // CHECK:     [[CAT:%[0-9]+]] = begin_borrow [[CAT_REF]] : $Cat

    // CHECK:     hop_to_executor [[CAT]] : $Cat
    // CHECK:     [[CAT_FOR_LOAD:%[0-9]+]] = begin_borrow [[CAT_REF:%[0-9]+]] : $Cat
    // CHECK:     [[GETTER:%[0-9]+]] = class_method [[CAT_FOR_LOAD]] : $Cat, #Cat.storedBool!getter : (Cat) -> () -> Bool, $@convention(method) (@guaranteed Cat) -> Bool
    // CHECK:     [[THE_BOOL:%[0-9]+]] = apply [[GETTER]]([[CAT_FOR_LOAD]]) : $@convention(method) (@guaranteed Cat) -> Bool
    // CHECK:     end_borrow [[CAT_FOR_LOAD]] : $Cat
    // CHECK:     end_borrow [[CAT]] : $Cat

    // CHECK:     hop_to_executor [[SELF]] : $Dog
    // CHECK:     destroy_value [[CAT_REF]] : $Cat
    // CHECK:    return [[THE_BOOL]] : $Bool
    // CHECK: } // end sil function '$s4test3DogC33accessStoredPropFromRefProjection3boxSbAA6CatBoxC_tYaF'
    func accessStoredPropFromRefProjection(box : CatBox) async -> Bool {
        return await box.cat.storedBool
    }

    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC015accessSweaterOfD03catAA0D0VAA3CatC_tYaF : $@convention(method) @async (@guaranteed Cat, @guaranteed Dog) -> @owned Sweater {
    // CHECK:  bb0([[CAT:%[0-9]+]] : @guaranteed $Cat, [[SELF:%[0-9]+]] : @guaranteed $Dog):
    // CHECK:    hop_to_executor [[SELF]] : $Dog

    // CHECK:    hop_to_executor [[CAT]] : $Cat
    // CHECK:    [[CAT_GETTER:%[0-9]+]] = class_method [[CAT]] : $Cat, #Cat.computedSweater!getter : (Cat) -> () -> Sweater, $@convention(method) (@guaranteed Cat) -> @owned Sweater
    // CHECK:    [[SWEATER1_REF:%[0-9]+]] = apply [[CAT_GETTER]]([[CAT]]) : $@convention(method) (@guaranteed Cat) -> @owned Sweater

    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    [[SWEATER1:%[0-9]+]] = begin_borrow [[SWEATER1_REF]] : $Sweater
    // CHECK:    [[SWEATER1_OWNER:%[0-9]+]] = struct_extract [[SWEATER1]] : $Sweater, #Sweater.owner
    // CHECK:    [[CAT2_REF:%[0-9]+]] = copy_value [[SWEATER1_OWNER]] : $Cat
    // CHECK:    end_borrow [[SWEATER1]] : $Sweater
    // CHECK:    destroy_value [[SWEATER1_REF]] : $Sweater
    // CHECK:    [[CAT2:%[0-9]+]] = begin_borrow [[CAT2_REF]] : $Cat

    // CHECK:    hop_to_executor [[CAT2]] : $Cat
    // CHECK:    [[CAT2_FOR_LOAD:%[0-9]+]] = begin_borrow [[CAT2_REF]] : $Cat
    // CHECK:    [[CAT2_GETTER:%[0-9]+]] = class_method [[CAT2_FOR_LOAD]] : $Cat, #Cat.computedSweater!getter : (Cat) -> () -> Sweater, $@convention(method) (@guaranteed Cat) -> @owned Sweater
    // CHECK:    [[SWEATER2_OWNER:%[0-9]+]] = apply [[CAT2_GETTER]]([[CAT2_FOR_LOAD]]) : $@convention(method) (@guaranteed Cat) -> @owned Sweater
    // CHECK:    end_borrow [[CAT2_FOR_LOAD]] : $Cat
    // CHECK:    end_borrow [[CAT2]] : $Cat

    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    destroy_value [[CAT2_REF]] : $Cat
    // CHECK:    return [[SWEATER2_OWNER]] : $Sweater
    // CHECK: } // end sil function '$s4test3DogC015accessSweaterOfD03catAA0D0VAA3CatC_tYaF'
    func accessSweaterOfSweater(cat : Cat) async -> Sweater {
        // note that Sweater is not an actor!
        return await cat.computedSweater.owner.computedSweater
    }


    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC13accessCatList3catAA0D0CAG_tYaF : $@convention(method) @async (@guaranteed Cat, @guaranteed Dog) -> @owned Cat {
    // CHECK:  bb0([[CAT:%[0-9]+]] : @guaranteed $Cat, [[SELF:%[0-9]+]] : @guaranteed $Dog):
    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    hop_to_executor [[CAT]] : $Cat
    // CHECK:    [[CAT_GETTER:%[0-9]+]] = class_method [[CAT]] : $Cat, #Cat.friend!getter : (Cat) -> () -> Cat, $@convention(method) (@guaranteed Cat) -> @owned Cat
    // CHECK:    [[FRIEND1_REF:%[0-9]+]] = apply [[CAT_GETTER]]([[CAT]]) : $@convention(method) (@guaranteed Cat) -> @owned Cat
    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    [[FRIEND1:%[0-9]+]] = begin_borrow [[FRIEND1_REF]] : $Cat
    // CHECK:    hop_to_executor [[FRIEND1]] : $Cat
    // CHECK:    [[FRIEND1_FOR_LOAD:%[0-9]+]] = begin_borrow [[FRIEND1_REF]] : $Cat
    // CHECK:    [[FRIEND1_GETTER:%[0-9]+]] = class_method [[FRIEND1_FOR_LOAD]] : $Cat, #Cat.friend!getter : (Cat) -> () -> Cat, $@convention(method) (@guaranteed Cat) -> @owned Cat
    // CHECK:    [[FRIEND2_REF:%[0-9]+]] = apply [[FRIEND1_GETTER]]([[FRIEND1_FOR_LOAD]]) : $@convention(method) (@guaranteed Cat) -> @owned Cat
    // CHECK:    end_borrow [[FRIEND1_FOR_LOAD]] : $Cat
    // CHECK:    end_borrow [[FRIEND1]] : $Cat
    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    destroy_value [[FRIEND1_REF]] : $Cat
    // CHECK:    return [[FRIEND2_REF]] : $Cat
    // CHECK: } // end sil function '$s4test3DogC13accessCatList3catAA0D0CAG_tYaF'
    func accessCatList(cat : Cat) async -> Cat {
        return await cat.friend.friend
    }


    // CHECK-LABEL: sil hidden [ossa] @$s4test3DogC21accessOptionalCatList3catAA0E0CSgAG_tYaF : $@convention(method) @async (@guaranteed Cat, @guaranteed Dog) -> @owned Optional<Cat> {
    // CHECK:  bb0([[CAT:%[0-9]+]] : @guaranteed $Cat, [[SELF:%[0-9]+]] : @guaranteed $Dog):
    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    [[FRIEND1_STACK:%[0-9]+]] = alloc_stack $Optional<Cat>

    // CHECK:    hop_to_executor [[CAT]] : $Cat
    // CHECK:    [[MAYBE_GETTER:%[0-9]+]] = class_method [[CAT]] : $Cat, #Cat.maybeFriend!getter : (Cat) -> () -> Cat?, $@convention(method) (@guaranteed Cat) -> @owned Optional<Cat>
    // CHECK:    [[MAYBE_FRIEND:%[0-9]+]] = apply [[MAYBE_GETTER]]([[CAT]]) : $@convention(method) (@guaranteed Cat) -> @owned Optional<Cat>
    // CHECK:    store [[MAYBE_FRIEND]] to [init] [[FRIEND1_STACK]] : $*Optional<Cat>

    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    [[IS_SOME:%[0-9]+]] = select_enum_addr [[FRIEND1_STACK]] : $*Optional<Cat>
    // CHECK:    cond_br [[IS_SOME]], bb1, bb3

    // CHECK:  bb1:
    // CHECK:    [[FRIEND1_ADDR:%[0-9]+]] = unchecked_take_enum_data_addr [[FRIEND1_STACK]] : $*Optional<Cat>, #Optional.some!enumelt
    // CHECK:    [[FRIEND1_REF:%[0-9]+]] = load [copy] [[FRIEND1_ADDR]] : $*Cat
    // CHECK:    destroy_addr [[FRIEND1_STACK]] : $*Optional<Cat>
    // CHECK:    [[FRIEND1:%[0-9]+]] = begin_borrow [[FRIEND1_REF]] : $Cat
    // CHECK:    hop_to_executor [[FRIEND1]] : $Cat
    // CHECK:    [[FRIEND1_FOR_LOAD:%[0-9]+]] = begin_borrow [[FRIEND1_REF]] : $Cat
    // CHECK:    [[FRIEND1_GETTER:%[0-9]+]] = class_method [[FRIEND1_FOR_LOAD]] : $Cat, #Cat.friend!getter : (Cat) -> () -> Cat, $@convention(method) (@guaranteed Cat) -> @owned Cat
    // CHECK:    [[FRIEND2_REF:%[0-9]+]] = apply [[FRIEND1_GETTER]]([[FRIEND1_FOR_LOAD]]) : $@convention(method) (@guaranteed Cat) -> @owned Cat
    // CHECK:    end_borrow [[FRIEND1_FOR_LOAD]] : $Cat
    // CHECK:    end_borrow [[FRIEND1]] : $Cat
    // CHECK:    hop_to_executor [[SELF]] : $Dog
    // CHECK:    destroy_value [[FRIEND1_REF]] : $Cat
    // CHECK:    [[FRIEND2_OPTIONAL:%[0-9]+]] = enum $Optional<Cat>, #Optional.some!enumelt, [[FRIEND2_REF]] : $Cat
    // CHECK:    dealloc_stack [[FRIEND1_STACK]] : $*Optional<Cat>
    // CHECK:    br bb2([[FRIEND2_OPTIONAL]] : $Optional<Cat>)

    // CHECK-NOT:  hop_to_executor
    // CHECK: } // end sil function '$s4test3DogC21accessOptionalCatList3catAA0E0CSgAG_tYaF'
    func accessOptionalCatList(cat : Cat) async -> Cat? {
        return await cat.maybeFriend?.friend
    }
} // END OF DOG ACTOR

