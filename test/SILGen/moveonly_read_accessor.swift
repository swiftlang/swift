// RUN: %target-swift-emit-silgen -enable-experimental-move-only -module-name test %s | %FileCheck %s --enable-var-scope
// RUN: %target-swift-emit-sil -enable-experimental-move-only -sil-verify-all %s

@_moveOnly
public struct Ticket {
  private var id = 0

  func isFrontRow() -> Bool { return false }
}

public final class TicketWrapper {
  let _ticket = Ticket()
  var ticket: Ticket {
    _read { yield _ticket }
  }
// CHECK-LABEL: sil hidden [ossa] @$s4test13TicketWrapperC6ticketAA0B0Vvr : $@yield_once @convention(method) (@guaranteed TicketWrapper) -> @yields @guaranteed Ticket
// CHECK:   [[FIELD_REF:%[0-9]+]] = ref_element_addr {{.*}} : $TicketWrapper, #TicketWrapper._ticket
// CHECK:   [[MMC:%[0-9]+]] = mark_must_check [no_consume_or_assign] [[FIELD_REF]] : $*Ticket
// CHECK:   [[LB:%[0-9]+]] = load_borrow [[MMC]] : $*Ticket
// CHECK:   yield [[LB]] : $Ticket
// CHECK: } // end sil function '$s4test13TicketWrapperC6ticketAA0B0Vvr'
}

public func checkMethodCall(_ wrapper: TicketWrapper) -> Bool {
  return wrapper.ticket.isFrontRow()
}
// CHECK-LABEL: sil [ossa] @$s4test15checkMethodCallySbAA13TicketWrapperCF : $@convention(thin) (@guaranteed TicketWrapper) -> Bool
// CHECK:    [[READ_ACC:%[0-9]+]] = function_ref @$s4test13TicketWrapperC6ticketAA0B0Vvr : $@yield_once @convention(method) (@guaranteed TicketWrapper) -> @yields @guaranteed Ticket
// CHECK:    ([[TICKET:%[0-9]+]], [[APPLY_TOKEN:%[0-9]+]]) = begin_apply [[READ_ACC]]({{.*}}) : $@yield_once @convention(method) (@guaranteed TicketWrapper) -> @yields @guaranteed Ticket
// CHECK:    [[METH:%[0-9]+]] = function_ref @$s4test6TicketV10isFrontRowSbyF : $@convention(method) (@guaranteed Ticket) -> Bool
// CHECK:    [[RV:%[0-9]+]] = apply [[METH]]([[TICKET]]) : $@convention(method) (@guaranteed Ticket) -> Bool
// CHECK:    end_apply [[APPLY_TOKEN]]
// CHECK:    return [[RV]] : $Bool
// CHECK: } // end sil function '$s4test15checkMethodCallySbAA13TicketWrapperCF'
