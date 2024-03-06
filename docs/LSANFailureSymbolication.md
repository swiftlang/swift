# Diagnose LSAN Failures in the Compiler

### Create Ubuntu Container 

1. Clone (or pull) swift-docker: https://github.com/apple/swift-docker
2. Build the Ubuntu 18.04 container: `cd swift-ci/master/ubuntu/18.04; docker build .`
3. `docker run -it --cpus <CPUs> --memory <Memory> -v ~/<path to your local sources>:/src-on-host:cached --name lsan-reproducer --cap-add=SYS_PTRACE --security-opt seccomp=unconfined <hash that docker build outputs> bash`
    - The `-cap-add` and `-security-opt` arguments are needed to run LLDB inside the Docker container
4. Copy the sources to inside the Docker container: `cp -r /src-on-host/* ~`
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
3. Start running swiftc inside lldb by executing `r`
4. Find the loaded offset of swift-frontend by running `image list`
For example, this might output
```
[  0] 0AEA10C1 0x0000555555554000 /home/build-user/build/buildbot_incremental_lsan/swift-linux-x86_64/bin/swift-frontend 
[  1] D52BB67A-BBBB-E429-6E87-FC16144CA7CE-55276DD6 0x00007ffff7ffb000 [vdso] (0x00007ffff7ffb000)
[  2] 9EA8014C-F020-21A2-9E57-AA3E0512E9BB-6E30541D 0x00007ffff7dd3000 /lib/x86_64-linux-gnu/ld-2.27.so
```
The loaded offset is `0x0000555555554000`
5. For the frame that you want to symbolicate,, add the offset you computed above to the stack frame in the LSAN report, eg. to symbolicate frame 1 `0x555555554000 + 0x292d81c = 0x555557E8181C`
6. Look up the address using `image lookup -a <address you computed>`. This should output something like

```
(lldb) image lookup -a 0x555557E8181C
      Address: swiftc[0x000000000292d81c] (swiftc.PT_LOAD[0]..text + 22056284)
      Summary: swiftc`registerFunctionTest(BridgedStringRef, void*) + 28 at SILBridging.cpp:148:3
```

7. Hoorray, you know which function is leaking.

### Making Local Changes Inside the Container

For example, to install vim in the container run

```
docker exec -u 0:0 -it lsan-reproducer bash
$ apt update
$ apt install vim
```
