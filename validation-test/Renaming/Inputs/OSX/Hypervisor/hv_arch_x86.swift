
struct hv_x86_reg_t : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var HV_X86_RIP: hv_x86_reg_t { get }
var HV_X86_RFLAGS: hv_x86_reg_t { get }
var HV_X86_RAX: hv_x86_reg_t { get }
var HV_X86_RCX: hv_x86_reg_t { get }
var HV_X86_RDX: hv_x86_reg_t { get }
var HV_X86_RBX: hv_x86_reg_t { get }
var HV_X86_RSI: hv_x86_reg_t { get }
var HV_X86_RDI: hv_x86_reg_t { get }
var HV_X86_RSP: hv_x86_reg_t { get }
var HV_X86_RBP: hv_x86_reg_t { get }
var HV_X86_R8: hv_x86_reg_t { get }
var HV_X86_R9: hv_x86_reg_t { get }
var HV_X86_R10: hv_x86_reg_t { get }
var HV_X86_R11: hv_x86_reg_t { get }
var HV_X86_R12: hv_x86_reg_t { get }
var HV_X86_R13: hv_x86_reg_t { get }
var HV_X86_R14: hv_x86_reg_t { get }
var HV_X86_R15: hv_x86_reg_t { get }
var HV_X86_CS: hv_x86_reg_t { get }
var HV_X86_SS: hv_x86_reg_t { get }
var HV_X86_DS: hv_x86_reg_t { get }
var HV_X86_ES: hv_x86_reg_t { get }
var HV_X86_FS: hv_x86_reg_t { get }
var HV_X86_GS: hv_x86_reg_t { get }
var HV_X86_IDT_BASE: hv_x86_reg_t { get }
var HV_X86_IDT_LIMIT: hv_x86_reg_t { get }
var HV_X86_GDT_BASE: hv_x86_reg_t { get }
var HV_X86_GDT_LIMIT: hv_x86_reg_t { get }
var HV_X86_LDTR: hv_x86_reg_t { get }
var HV_X86_LDT_BASE: hv_x86_reg_t { get }
var HV_X86_LDT_LIMIT: hv_x86_reg_t { get }
var HV_X86_LDT_AR: hv_x86_reg_t { get }
var HV_X86_TR: hv_x86_reg_t { get }
var HV_X86_TSS_BASE: hv_x86_reg_t { get }
var HV_X86_TSS_LIMIT: hv_x86_reg_t { get }
var HV_X86_TSS_AR: hv_x86_reg_t { get }
var HV_X86_CR0: hv_x86_reg_t { get }
var HV_X86_CR1: hv_x86_reg_t { get }
var HV_X86_CR2: hv_x86_reg_t { get }
var HV_X86_CR3: hv_x86_reg_t { get }
var HV_X86_CR4: hv_x86_reg_t { get }
var HV_X86_DR0: hv_x86_reg_t { get }
var HV_X86_DR1: hv_x86_reg_t { get }
var HV_X86_DR2: hv_x86_reg_t { get }
var HV_X86_DR3: hv_x86_reg_t { get }
var HV_X86_DR4: hv_x86_reg_t { get }
var HV_X86_DR5: hv_x86_reg_t { get }
var HV_X86_DR6: hv_x86_reg_t { get }
var HV_X86_DR7: hv_x86_reg_t { get }
var HV_X86_TPR: hv_x86_reg_t { get }
var HV_X86_XCR0: hv_x86_reg_t { get }
var HV_X86_REGISTERS_MAX: hv_x86_reg_t { get }
