#!/usr/bin/env python3
# flake8: noqa
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2024 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import argparse
import os
import subprocess
import tempfile
import pathlib


MY_DIR = os.path.dirname(os.path.abspath(__file__))


def shell(cmd):
    return subprocess.check_output(["bash", "-c", cmd])


def shell_print(cmd):
    print(cmd)
    subprocess.check_call(["bash", "-c", cmd])


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--device', choices=sorted([d for d in os.listdir(MY_DIR) if os.path.isdir(MY_DIR + "/" + d)]), required=True)
    parser.add_argument('command', choices=['run', 'demo-run', 'print-c-flags', 'print-swift-flags', 'print-swift-frontend-flags'])
    parser.add_argument('--elf-file')

    args = parser.parse_args()

    if args.device == "arm-qemu-stm32f1":
        target = "armv7em-none-none-eabi"
        qemu_binary = "qemu-system-arm"
        qemu_machine = "stm32vldiscovery"
        mmcu = ""
        entry_point = "vector_table"
        semihosting = True
        qemu_mode = "-kernel"
        always_optimize_for_size = False
    elif args.device == "arm-qemu-stm32f4":
        target = "armv7em-none-none-eabi"
        qemu_binary = "qemu-system-arm"
        qemu_machine = "netduinoplus2"
        mmcu = ""
        entry_point = "vector_table"
        semihosting = True
        qemu_mode = "-kernel"
        always_optimize_for_size = False
    elif args.device == "avr-qemu-atmega2560":
        target = "avr-none-none-elf"
        qemu_binary = "qemu-system-avr"
        qemu_machine = "mega2560"
        mmcu = "-mmcu=atmega2560"
        entry_point = "start"
        semihosting = False
        qemu_mode = "-bios"
        always_optimize_for_size = True
    elif args.device == "riscv32-qemu-virt":
        target = "riscv32-none-none-eabi"
        qemu_binary = "qemu-system-riscv32"
        qemu_machine = "virt"
        mmcu = ""
        entry_point = "start"
        semihosting = False
        qemu_mode = "-bios none -kernel"
        always_optimize_for_size = True
    else:
        assert False

    tmpdir = tempfile.gettempdir()

    cflags = ""
    cflags += " -nostdlib"
    cflags += " -ffunction-sections -fdata-sections"
    cflags += " -ffreestanding"
    cflags += " -O2"
    cflags += " -g"
    cflags += f" {mmcu}"

    supportfile = tmpdir + "/" + target + "-support.o"
    shell(f"clang -target {target} {cflags} -c {MY_DIR}/{args.device}/support.c -o {supportfile}")

    libcfile = tmpdir + "/" + target + "-libc.o"
    shell(f"clang -target {target} {cflags} -c {MY_DIR}/libc.c -o {libcfile}")

    demo_o_file = tmpdir + "/" + target + "-demo.o"
    shell(f"clang -target {target} {cflags} -c {MY_DIR}/demo.c -o {demo_o_file}")

    cflags += f" {supportfile} {libcfile}"

    swift_frontend_flags = ""
    swift_frontend_flags += " -disable-stack-protector -function-sections"
    if mmcu != "": swift_frontend_flags += f" -Xcc {mmcu}"

    swift_flags = ""
    swift_flags += " -Xfrontend -disable-stack-protector -Xfrontend -function-sections -use-ld=lld"
    swift_flags += " -Xclang-linker -nostdlib -Xlinker --gc-sections"
    swift_flags += f" {supportfile} {libcfile}"
    swift_flags += f" -Xlinker -T -Xlinker {MY_DIR}/{args.device}/linkerscript.ld -Xlinker -e -Xlinker {entry_point}"
    if mmcu != "": swift_flags += f" -Xcc {mmcu}"
    if always_optimize_for_size: swift_flags += " -Osize"

    cflags += " -fuse-ld=lld"
    cflags += " -nostdlib"
    cflags += " -Xlinker --gc-sections"
    cflags += f" -Xlinker -T -Xlinker {MY_DIR}/{args.device}/linkerscript.ld"
    cflags += f" -Wl,-e,{entry_point}"

    if args.command == "print-swift-flags":
        print(swift_flags)

    elif args.command == "print-swift-frontend-flags":
        print(swift_frontend_flags)

    elif args.command == "print-c-flags":
        print(cflags)

    elif args.command == "run":
        if args.elf_file is None:
            print("Must specify --elf-file <...>")
            exit(1)

        qemu_command = f"{qemu_binary} -M {qemu_machine} -nographic {qemu_mode} {args.elf_file} {'-semihosting' if semihosting else ''}"
        shell_print(f"expect -c 'spawn {qemu_command}; expect -timeout 5 -re \"HALT\"'")

    elif args.command == "demo-run":
        elffile = tmpdir + "/" + target + ".elf"
        shell(f"clang -target {target} {cflags} {demo_o_file} -o {elffile}")

        qemu_command = f"{qemu_binary} -M {qemu_machine} -nographic {qemu_mode} {elffile} {'-semihosting' if semihosting else ''}"
        shell_print(f"expect -c 'spawn {qemu_command}; expect -timeout 5 -re \"HALT\"'")


if __name__ == "__main__":
    main()
