# Diagnose LSAN Failures in the Compiler

### Create Ubuntu Container 

1. Clone (or pull) swift-docker: https://github.com/apple/swift-docker
2. Build the Ubuntu 18.04 container: `cd swift-ci/master/ubuntu/18.04; docker build .`
3. `docker run -it —cpus <CPUs> —memory <Memory> -v ~/<path to your local sources>:/src-on-host:cached —name lsan-reproducer —cap-add=SYS_PTRACE —security-opt seccomp=unconfined <hash that docker build outputs> bash`
    - The `-cap-add` and `-security-opt` arguments are needed to run LLDB inside the Docker container
4. Copy the sources to inside the Docker container: `cp /src-on-host/* ~`
    - We need to to this because the build needs a case-sensitive file system and your host machine probably has a case-insensitive file system

Build inside the Container

1. `utils/build-script --preset buildbot_incremental_linux,lsan,tools=RDA,stdlib=DA,test=no`
2. This should reproduce the LSAN failure
3. Now, disassemble the failing CMake invocation to a swiftc invocation. I needed to set one environment variable and could the copy the swiftc invocation (but this might change as the build changes)

```
export LD_LIBRARY_PATH=/opt/swift/5.8.1/usr/lib/swift/linux
/home/build-user/build/buildbot_incremental_lsan/swift-linux-x86_64/./bin/swiftc <many arguments>
```

### Symbolicating the LSAN report

For reasons that are not clear to me, LSAN does not symbolicate the report. To get the functions at the reported offsets, perform the following steps (there might be easier steps, please update this document if you know any).

1. Run the swiftc invocation that fails and copy the leak report to somewhere. The leak report should look like the following.
```
==3863==ERROR: LeakSanitizer: detected memory leaks

Direct leak of 120 byte(s) in 3 object(s) allocated from:
 #0 0x55b91c0b59b8 (/home/build-user/build/buildbot_incremental_lsan/swift-linux-x86_64/bin/swift-frontend+0x14d09b8)
 #1 0x55b91d51281c (/home/build-user/build/buildbot_incremental_lsan/swift-linux-x86_64/bin/swift-frontend+0x292d81c)
 #2 0x55b91c1b8700 (/home/build-user/build/buildbot_incremental_lsan/swift-linux-x86_64/bin/swift-frontend+0x15d3700)

SUMMARY: LeakSanitizer: 120 byte(s) leaked in 3 allocation(s).
```
2. `lldb -- <your swiftc invocation above>`
3. Set a breakpoint somewhere in swiftc, eg. at the start of main: `br s -f driver.cpp -l 19`. This should output something like

```
Breakpoint 1: where = swiftc`main + 4 at driver.cpp:20:10, address = 0x00000000014d25d4
```

4. Run swiftc to the breakpoint
5. This should stop you with something like

```
* thread #1, name = 'swiftc', stop reason = breakpoint 1.1
 frame #0: 0x0000555556a265d4 swiftc`main(argc_=168, argv_=0x00007fffffffced8) at driver.cpp:20:10
  17  #include "swift/DriverTool/DriverTool.h"
  18   
  19  int main(int argc_, const char **argv_) {
→ 20   return swift::mainEntry(argc_, argv_);
21  }
```

6. Find the loaded offset of swift-frontend by subtracting the breakpoint address from the frame address. With the example above, that’s `0x0000555556a265d4 - 0x00000000014d25d4 = 0x555555554000`.
7. For the frame that you want to symbolicate,, add the offset you computed above to the stack frame in the LSAN report, eg. to symbolicate frame 1 `0x555555554000 + 0x292d81c = 0x555557E8181C`
8. Look up the address using `image lookup -a <address you computed>`. This should output something like

```
(lldb) image lookup -a 0x555557E8181C
      Address: swiftc[0x000000000292d81c] (swiftc.PT_LOAD[0]..text + 22056284)
      Summary: swiftc`registerFunctionTest(BridgedStringRef, void*) + 28 at SILBridging.cpp:148:3
```

9. Hoorray, you know which function is leaking.

### Making Local Changes Inside the Container

For example, to install vim in the container run

```
docker exec -u 0:0 -it lsan-reproducer bash
$ apt update
$ apt install vim
```
