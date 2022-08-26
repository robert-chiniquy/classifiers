#! /usr/bin/env bash

TARGET="`cargo test --no-run --message-format=json | jq .executable | grep classifiers | xargs echo`"

rust-lldb -- $TARGET

# run a test
# (lldb) r <test name>

# break on a test
# (lldb) b <test name>
# (lldb) run

# next, step over
# (lldb) n

# backtrace
# (lldb) bt

# continue
# (lldb) c
