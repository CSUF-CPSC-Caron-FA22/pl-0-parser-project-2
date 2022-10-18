# CPSC 323 Project 2: Ad hoc PL/0 Parser

## Group Members

Please modify this section and list your group members in this format.

Example Name ename@nowhere.com

## Instructions

Please refer to the this [document](https://docs.google.com/document/d/1KFDDcofkGmQhKwELfRZvNcWfIdbz2B-TtWw1dDEwkyY/edit?usp=sharing).

## How to build

Ensure you have a C/C++ compiler, make, and CMake installed. Executed the follow commands in terminal in the repository directory.

```
mkdir build
cd build
cmake ../
make
```

The resulting executables are populated within the build directory, which include `Compiler.out` and `Test.out`.

`Compiler.out` takes one argument, the file path of the file to perform the lexical analysis and parsing on. Because you are graded on the test cases, the `Compiler.out` driver is mainly for testing and debugging example code you will create.

```
./Compiler.out ../test/test_code/test1.p0
```

`Test.out` is executed without arguments.

```
./Test.out
```

Your code must pass all test cases, including withheld test cases, for full credit.
