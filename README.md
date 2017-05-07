# CPU-MzISA

A toy CPU simulator.

It doesn't even have branches yet, and the code is littered with debug
statements.

I don't know why you're even here.

## Build

Get [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Now run

    stack build

## Run

    stack exec cpu-exe

Press enter to step the CPU, i.e. execute the next instruction.

This will run the fibonacci program.

## Name

The name was generated with

    cat /dev/urandom | base64 | head -c 5

Amazingly, it included the string "ISA".
