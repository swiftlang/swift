# Print a function that retains and releases its argument `count` times.
import sys

count = int(sys.argv[1])
print('declare ptr @swift_retain(ptr) nounwind')
print('declare void @swift_release(ptr captures(none))')
print('declare void @user(ptr)')
print('define void @many(ptr %A) {')
print('entry:')
for _ in range(count):
    print('  call ptr @swift_retain(ptr %A)')
print('  call void @user(ptr %A)')
for _ in range(count):
    print('  call void @swift_release(ptr %A)')
print('  ret void')
print('}')
