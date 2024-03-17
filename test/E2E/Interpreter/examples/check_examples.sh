#!/bin/bash

EXE="stack run --"

file_paths=(
    "control.mmm:true10012345678910"
    "fibonacci.mmm:610610"
    "functions.mmm:mainf1f2f3f4f342f5f6Calling f5():f5f5012"
    "main_hello_world.mmm:Hello, World!"
    "short_hello_world.mmm:Hello, World!"
    "print.mmm:Hello, World!1"
    "structs.mmm:2223hello2223"
)

# Function to run program over file and check stdout
check_stdout() {
    file="$1"
    expected_output="$2"

    actual_output=$(${EXE} "$file")

    if [ "$actual_output" = "$expected_output" ]; then
        :
    else
        echo "Output for $file does not match expected output"
        echo "Actual output: $actual_output"
        echo "Expected output: $expected_output"
        exit 1
    fi
}

for item in "${file_paths[@]}"; do
    IFS=':' read -r file expected_output <<< "$item"
    check_stdout "$file" "$expected_output"
done

echo "All tests passed"
