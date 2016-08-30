	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 9
	.globl	_foo
	.p2align	4, 0x90
_foo:                                   ## @foo
	.cfi_startproc
## BB#0:                                ## %entry
	subq	$40, %rsp
Ltmp0:
	.cfi_def_cfa_offset 48
	movl	%edx, %eax
	movq	%rdi, 32(%rsp)          ## 8-byte Spill
	movq	%rsi, 24(%rsp)          ## 8-byte Spill
	movl	%edx, 20(%rsp)          ## 4-byte Spill
	movq	%rax, %rdx
	callq	_memcpy
	movl	20(%rsp), %ecx          ## 4-byte Reload
	movl	%ecx, %edx
	movq	32(%rsp), %rdi          ## 8-byte Reload
	movq	24(%rsp), %rsi          ## 8-byte Reload
	movq	%rax, 8(%rsp)           ## 8-byte Spill
	callq	_memmove
	movq	%rax, (%rsp)            ## 8-byte Spill
	addq	$40, %rsp
	retq
	.cfi_endproc


.subsections_via_symbols
